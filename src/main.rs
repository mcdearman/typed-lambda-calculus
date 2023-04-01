use chumsky::{
    input::Stream, input::ValueInput, prelude::Input, prelude::*, span::SimpleSpan, Parser,
};
use lasso::Spur;
use lasso::ThreadedRodeo;
use logos::Logos;
use once_cell::sync::Lazy;
use rustyline::{
    error::ReadlineError, highlight::MatchingBracketHighlighter,
    validate::MatchingBracketValidator, Completer, Editor, Helper, Highlighter, Hinter, Validator,
};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::fs;
use std::fs::File;
use std::io;
use std::io::Write;
use std::{cell::RefCell, rc::Rc};

// =====================================================================
// =                           Interner                                =
// =====================================================================

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub key: Spur,
}

impl From<Spur> for Ident {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ident({})", unsafe { INTERNER.resolve(&self.key) })
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { INTERNER.resolve(&self.key) })
    }
}

// =====================================================================
// =                            Parser                                 =
// =====================================================================

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[regex(r"\d+", |lex| lex.slice().parse())]
    Int(i64),
    #[regex(r"true|false", |lex| lex.slice().parse())]
    Bool(bool),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| Ident::from(lex.slice()))]
    Ident(Ident),
    #[token("\\")]
    Lambda,
    #[token("->")]
    Arrow,
    #[token("=")]
    Eq,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r"[ \t\n\r]", logos::skip)]
    Whitespace,
    #[error]
    Err,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Ident(name) => write!(f, "{}", name),
            Self::Lambda => write!(f, "\\"),
            Self::Arrow => write!(f, "->"),
            Self::Eq => write!(f, "="),
            Self::Let => write!(f, "let"),
            Self::In => write!(f, "in"),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Err => write!(f, "error"),
            Self::Whitespace => write!(f, "WS"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(Ident),
    Lambda(Ident, Box<Self>),
    Apply(Box<Self>, Box<Self>),
    Let(Ident, Box<Self>, Box<Self>),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Var(name) => write!(f, "{}", name),
            Self::Lambda(param, body) => write!(f, "\\{} -> {}", param, body),
            Self::Apply(fun, arg) => write!(f, "({} {})", fun, arg),
            Self::Let(name, val, body) => write!(f, "let {} = {} in {}", name, val, body),
            Self::Add(l, r) => write!(f, "({} + {})", l, r),
            Self::Sub(l, r) => write!(f, "({} - {})", l, r),
            Self::Mul(l, r) => write!(f, "({} * {})", l, r),
            Self::Div(l, r) => write!(f, "({} / {})", l, r),
        }
    }
}

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>> {
    let ident = select! { Token::Ident(name) => name };

    let expr =
        recursive(|expr| {
            let inline_expr =
                recursive(|inline_expr| {
                    let val = select! {
                        Token::Int(n) => Expr::Int(n),
                        Token::Bool(b) => Expr::Bool(b),
                    };

                    // parse let
                    let let_ = just(Token::Let)
                        .ignore_then(ident)
                        .then_ignore(just(Token::Eq))
                        .then(inline_expr.clone())
                        .then_ignore(just(Token::In))
                        .then(expr.clone())
                        .map(|((name, val), body)| {
                            Expr::Let(Ident::from(name), Box::new(val), Box::new(body))
                        });

                    // parse curry lambda
                    let lambda = just(Token::Lambda).ignore_then(
                        ident
                            .repeated()
                            .foldr(just(Token::Arrow).ignore_then(expr.clone()), |arg, body| {
                                Expr::Lambda(Ident::from(arg), Box::new(body))
                            }),
                    );

                    let atom = choice((
                        val,
                        let_,
                        lambda,
                        ident.map(Expr::Var),
                        expr.clone()
                            .delimited_by(just(Token::LParen), just(Token::RParen)),
                    ));

                    // parse function application
                    let apply = atom.clone().foldl(atom.clone().repeated(), |f, arg| {
                        Expr::Apply(Box::new(f), Box::new(arg))
                    });

                    // parse arithmetic
                    let op = just(Token::Mul).or(just(Token::Div));

                    let product = apply.clone().foldl(
                        op.clone().then(apply.clone()).repeated(),
                        |l, (op, r)| match op {
                            Token::Mul => Expr::Mul(Box::new(l), Box::new(r)),
                            Token::Div => Expr::Div(Box::new(l), Box::new(r)),
                            _ => unreachable!(),
                        },
                    );

                    let op = just(Token::Add).or(just(Token::Sub));
                    let sum = product.clone().foldl(
                        op.clone().then(product.clone()).repeated(),
                        |l, (op, r)| match op {
                            Token::Add => Expr::Add(Box::new(l), Box::new(r)),
                            Token::Sub => Expr::Sub(Box::new(l), Box::new(r)),
                            _ => unreachable!(),
                        },
                    );

                    sum
                });

            inline_expr
        });

    expr
}

// =====================================================================
// =                             Types                                 =
// =====================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Var(usize),
    Lambda(Box<Self>, Box<Self>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(n) => write!(f, "t{}", n),
            Self::Lambda(param, body) => write!(f, "{} -> {}", param, body),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEnv {
    bindings: HashMap<Ident, Type>,
    counter: usize,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            counter: 0,
        }
    }

    pub fn fresh(&mut self) -> Type {
        let ty = Type::Var(self.counter);
        self.counter += 1;
        ty
    }

    pub fn bind(&mut self, name: Ident, ty: Type) {
        self.bindings.insert(name, ty);
    }

    pub fn lookup(&self, name: &Ident) -> Option<Type> {
        self.bindings.get(name).cloned() 
    }
}

pub fn unify(t1: &Type, t2: &Type) -> Result<Type, String> {
    match (t1, t2) {
        (Type::Int, Type::Int) => Ok(Type::Int),
        (Type::Bool, Type::Bool) => Ok(Type::Bool),
        (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
            let p = unify(p1, p2)?;
            let b = unify(b1, b2)?;
            Ok(Type::Lambda(Box::new(p), Box::new(b)))
        }
        _ => Err(format!("cannot unify {:?} and {:?}", t1, t2)),
    }
}

// =====================================================================
// =                             Eval                                  =
// =====================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<Ident, Expr>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            parent: None,
            vars: HashMap::new(),
        }
    }

    pub fn create_child(parent: Rc<RefCell<Self>>) -> Self {
        Self {
            parent: Some(parent),
            vars: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: Ident, val: Expr) {
        self.vars.insert(name, val);
    }

    pub fn lookup(&self, name: &Ident) -> Option<Expr> {
        if let Some(v) = self.vars.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &self.parent {
            parent.as_ref().borrow().lookup(name)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Lambda(Ident, Box<Expr>, Rc<RefCell<Env>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Lambda(param, body, _) => write!(f, "\\{} -> {}", param, body),
        }
    }
}

pub fn eval(env: Rc<RefCell<Env>>, expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Int(i) => Ok(Value::Int(*i)),
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::Lambda(p, b) => {
            let fn_env = Rc::new(RefCell::new(Env::create_child(env.clone())));
            Ok(Value::Lambda(p.clone(), b.clone(), fn_env))
        }
        Expr::Var(name) => {
            if let Some(val) = env.as_ref().borrow().lookup(name) {
                eval(env.clone(), &val)
            } else {
                Err(format!("Unbound variable `{}`", name))
            }
        }
        Expr::Apply(lambda, value) => match eval(env.clone(), lambda)? {
            Value::Lambda(param, body, fn_env) => {
                fn_env
                    .as_ref()
                    .borrow_mut()
                    .define(param.clone(), *value.clone());
                eval(fn_env, &body)
            }
            _ => Err(format!("Expected callable lambda, got `{:?}`", lambda)),
        },
        Expr::Let(name, value, body) => {
            let child_env = Rc::new(RefCell::new(Env::create_child(env.clone())));
            child_env
                .as_ref()
                .borrow_mut()
                .define(name.clone(), *value.clone());
            eval(child_env, body)
        }
        Expr::Add(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Sub(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Mul(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Div(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
    }
}

// =====================================================================
// =                             REPL                                  =
// =====================================================================

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

impl ReplHelper {
    fn new() -> Self {
        Self {
            validator: MatchingBracketValidator::new(),
            highlighter: MatchingBracketHighlighter::new(),
        }
    }
}

pub fn repl() {
    let mut rl = Editor::new().expect("failed to create editor");
    rl.set_helper(Some(ReplHelper::new()));

    println!("Welcome to the Lust REPL!");
    print!("> ");
    io::stdout().flush().expect("failed to flush stdout");
    let mut src = String::new();
    let mut carry = String::new();
    let env = Rc::new(RefCell::new(Env::new()));
    'outer: loop {
        std::io::stdin()
            .read_line(&mut src)
            .expect("failed to read line");
        for expr_str in src.trim_end().split_inclusive(";;") {
            if expr_str.ends_with(";;") {
                let expr_str = carry.clone() + expr_str.trim_end_matches(";;");
                let lex = Token::lexer(&expr_str)
                    .spanned()
                    .map(|(tok, span)| (tok, SimpleSpan::from(span)));
                let tok_stream =
                    Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
                match parser().parse(tok_stream).into_result() {
                    Ok(ast) => match eval(env.clone(), &ast) {
                        Ok(val) => {
                            println!("ast: {:?}", ast);
                            println!("{}", val);
                        }
                        Err(e) => {
                            println!("ast: {:?}", ast);
                            println!("Error: {}", e);
                        }
                    },
                    Err(e) => {
                        println!("Error: {:?}", e);
                        carry.clear();
                        continue 'outer;
                    }
                }
                carry.clear();
            } else {
                carry.push_str(expr_str);
                src.clear();
                print!("- ");
                io::stdout().flush().expect("failed to flush stdout");
                continue 'outer;
            }
        }
        src.clear();
        print!("\n> ");
        io::stdout().flush().expect("failed to flush stdout");
    }
    // loop {
    //     match rl.readline("> ") {
    //         Ok(line) => {
    //             // let tokens: Vec<Token> = Token::lexer(&line).clone().spanned().collect();
    //             // println!("Tokens: {:?}", tokens);

    //             let lex = Token::lexer(&line)
    //                 .spanned()
    //                 .map(|(tok, span)| (tok, SimpleSpan::from(span)));

    //             let tok_stream =
    //                 Stream::from_iter(lex).spanned(SimpleSpan::from(line.len()..line.len()));
    //             let ast = parser().parse(tok_stream).into_result();
    //             match ast {
    //                 Ok(ast) => match eval(env.clone(), &ast) {
    //                     Ok(result) => {
    //                         // println!("AST: {:?}", ast.clone());
    //                         println!("{}", result)
    //                     }
    //                     Err(err) => eprintln!("RuntimeError: {}", err),
    //                 },
    //                 Err(parse_errs) => parse_errs.into_iter().for_each(|err| {
    //                     eprintln!("Parse error: {}", err);
    //                 }),
    //             };
    //         }
    //         Err(ReadlineError::Interrupted) => {
    //             println!("Ctrl-C");
    //             break;
    //         }
    //         Err(ReadlineError::Eof) => {
    //             println!("Ctrl-D");
    //             break;
    //         }
    //         Err(err) => {
    //             println!("Error: {:?}", err);
    //             break;
    //         }
    //     }
    // }
}

// =====================================================================
// =                             Main                                  =
// =====================================================================

fn main() {
    // let src = fs::read_to_string("examples/multiline.tlc").expect("failed to read file");
    // let lex = Token::lexer(&src)
    //     .spanned()
    //     .map(|(tok, span)| (tok, SimpleSpan::from(span)));
    // let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
    // let ast = parser()
    //     .parse(tok_stream)
    //     .into_result()
    //     .expect("failed to parse");
    // println!("{:?}", ast);
    // let val = eval(Rc::new(RefCell::new(Env::new())), &ast).expect("failed to eval");
    // println!("{:?}", val);
    repl();
}
