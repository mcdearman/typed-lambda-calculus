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
use std::{cell::RefCell, rc::Rc};

// =========================
// =       Interner        =
// =========================

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

// =========================
// =        Parser         =
// =========================

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
    #[regex(r"[ \t]", logos::skip)]
    Whitespace,
    #[error]
    Err,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Int(n) => write!(f, "{}", n),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Ident(name) => write!(f, "{}", name),
            Token::Lambda => write!(f, "\\"),
            Token::Arrow => write!(f, "->"),
            Token::Eq => write!(f, "="),
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Err => write!(f, "error"),
            Token::Whitespace => write!(f, "WS"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(Ident),
    Lambda(Ident, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lambda(param, body) => write!(f, "\\{} -> {}", param, body),
            Expr::Apply(fun, arg) => write!(f, "({} {})", fun, arg),
            Expr::Let(name, val, body) => write!(f, "let {} = {} in {}", name, val, body),
            Expr::Add(l, r) => write!(f, "({} + {})", l, r),
            Expr::Sub(l, r) => write!(f, "({} - {})", l, r),
            Expr::Mul(l, r) => write!(f, "({} * {})", l, r),
            Expr::Div(l, r) => write!(f, "({} / {})", l, r),
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

// =========================
// =         Types         =
// =========================

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Var(String),
    Lambda(Box<Self>, Box<Self>),
}

// =========================
// =         Eval          =
// =========================

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

pub fn eval(env: Rc<RefCell<Env>>, expr: &Expr) -> Result<Expr, String> {
    match expr {
        lit @ Expr::Int(_) | lit @ Expr::Bool(_) | lit @ Expr::Lambda(_, _) => Ok(lit.clone()),
        Expr::Var(name) => {
            if let Some(val) = env.as_ref().borrow().lookup(name) {
                Ok(val.clone())
            } else {
                Err(format!("Unbound variable `{}`", name))
            }
        }
        Expr::Apply(lambda, value) => match eval(env.clone(), lambda)? {
            Expr::Lambda(param, body) => {
                let val = eval(env.clone(), &value)?;
                let child_env = Rc::new(RefCell::new(Env::create_child(env.clone())));
                child_env.as_ref().borrow_mut().define(param.clone(), val);
                eval(child_env, &body)
            }
            _ => Err(format!("Expected callable lambda, got `{:?}`", lambda)),
        },
        Expr::Let(name, value, body) => {
            let val = eval(env.clone(), &value)?;
            env.as_ref().borrow_mut().define(name.clone(), val);
            let child_env = Rc::new(RefCell::new(Env::create_child(env.clone())));
            eval(child_env, body)
        }
        Expr::Add(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l + r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Sub(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l - r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Mul(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l * r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Div(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l / r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
    }
}

// =========================
// =         REPL          =
// =========================

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
    let env = Rc::new(RefCell::new(Env::new()));

    println!("Welcome to the Lust REPL!");
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                // let tokens: Vec<Token> = Token::lexer(&line).clone().spanned().collect();
                // println!("Tokens: {:?}", tokens);

                let lex = Token::lexer(&line)
                    .spanned()
                    .map(|(tok, span)| (tok, SimpleSpan::from(span)));

                let tok_stream =
                    Stream::from_iter(lex).spanned(SimpleSpan::from(line.len()..line.len()));
                let ast = parser().parse(tok_stream).into_result();
                match ast {
                    Ok(ast) => match eval(env.clone(), &ast) {
                        Ok(result) => {
                            // println!("AST: {:?}", ast.clone());
                            println!("{}", result)
                        }
                        Err(err) => eprintln!("RuntimeError: {}", err),
                    },
                    Err(parse_errs) => parse_errs.into_iter().for_each(|err| {
                        eprintln!("Parse error: {}", err);
                    }),
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("Ctrl-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Ctrl-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

// =========================
// =         Main          =
// =========================

fn main() {
    repl();
}
