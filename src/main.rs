use chumsky::{
    input::Stream, input::ValueInput, prelude::Input, prelude::*, span::SimpleSpan, Parser,
};
use lasso::Spur;
use lasso::ThreadedRodeo;
use logos::Logos;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io;
use std::io::Write;
use std::ops::Sub;
use std::{cell::RefCell, rc::Rc};

pub mod tests;

// =====================================================================
// =                           Interner                                =
// =====================================================================

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternedString {
    pub key: Spur,
}

impl From<Spur> for InternedString {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for InternedString {
    fn from(name: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ident({})", unsafe { INTERNER.resolve(&self.key) })
    }
}

impl Display for InternedString {
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
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),
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
    Var(InternedString),
    Lambda(InternedString, Box<Self>),
    Apply(Box<Self>, Box<Self>),
    Let(InternedString, Box<Self>, Box<Self>),
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
                            Expr::Let(InternedString::from(name), Box::new(val), Box::new(body))
                        });

                    // parse curry lambda
                    let lambda = just(Token::Lambda).ignore_then(
                        ident
                            .repeated()
                            .foldr(just(Token::Arrow).ignore_then(expr.clone()), |arg, body| {
                                Expr::Lambda(InternedString::from(arg), Box::new(body))
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
    Var(TyVar),
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

type Substitution = HashMap<TyVar, Type>;

struct Scheme {
    vars: Vec<TyVar>,
    ty: Type,
}

struct Context {
    vars: HashMap<InternedString, Scheme>,
}

static mut COUNTER: usize = 0;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(pub usize);

impl TyVar {
    fn fresh() -> Self {
        unsafe {
            COUNTER += 1;
            Self(COUNTER)
        }
    }
}

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self)
    }
}

fn apply_subst(subst: &Substitution, ty: &Type) -> Type {
    match ty {
        Type::Int | Type::Bool => ty.clone(),
        Type::Var(n) => subst.get(n).cloned().unwrap_or_else(|| ty.clone()),
        Type::Lambda(param, body) => Type::Lambda(
            Box::new(apply_subst(subst, param)),
            Box::new(apply_subst(subst, body)),
        ),
    }
}

fn apply_subst_scheme(subst: &Substitution, scheme: &Scheme) -> Scheme {
    let mut subst = subst.clone();
    for var in &scheme.vars {
        subst.remove(var);
    }
    Scheme {
        vars: scheme.vars.clone(),
        ty: apply_subst(&subst, &scheme.ty),
    }
}

fn compose_subst(subst1: &Substitution, subst2: &Substitution) -> Substitution {
    subst1
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .chain(
            subst2
                .iter()
                .map(|(k, v)| (k.clone(), apply_subst(subst1, v))),
        )
        .collect()
}

fn free_vars(ty: &Type) -> Vec<TyVar> {
    match ty {
        Type::Int | Type::Bool => vec![],
        Type::Var(n) => vec![*n],
        Type::Lambda(param, body) => {
            let mut vars = free_vars(param);
            vars.extend(free_vars(body));
            vars
        }
    }
}

fn var_bind(var: TyVar, ty: &Type) -> Result<Substitution, String> {
    if ty == &Type::Var(var) {
        Ok(HashMap::new())
    } else if free_vars(ty).contains(&var) {
        Err(format!("occurs check failed: t{} occurs in {:?}", var, ty))
    } else {
        let mut subst = HashMap::new();
        subst.insert(var, ty.clone());
        Ok(subst)
    }
}

pub fn unify(t1: &Type, t2: &Type) -> Result<Substitution, String> {
    match (t1, t2) {
        (Type::Int, Type::Int) => Ok(HashMap::new()),
        (Type::Bool, Type::Bool) => Ok(HashMap::new()),
        (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
            let s1 = unify(p1, p2)?;
            let s2 = unify(&apply_subst(&s1, &b1), &apply_subst(&s1, &b2))?;
            Ok(compose_subst(&s2, &s1))
        }
        (Type::Var(n), _) | (_, Type::Var(n)) => var_bind(*n, t2),
        _ => Err(format!("cannot unify {:?} and {:?}", t1, t2)),
    }
}

fn infer(ctx: &mut Context, expr: &Expr) -> Result<(Substitution, Type), String> {
    match expr {
        Expr::Int(_) => Ok((HashMap::new(), Type::Int)),
        Expr::Bool(_) => Ok((HashMap::new(), Type::Bool)),
        Expr::Var(name) => match ctx.vars.get(name) {
            Some(scheme) => {
                // let mut subst = HashMap::new();
                // for var in &scheme.vars {
                //     subst.insert(*var, Type::Var(subst.len()));
                // }
                // Ok((subst.clone(), apply_subst(&subst, &scheme.ty)))
                todo!()
            }
            None => Err(format!("unbound variable: {:?}", expr)),
        },
        Expr::Lambda(_, _) => todo!(),
        Expr::Apply(_, _) => todo!(),
        Expr::Let(_, _, _) => todo!(),
        Expr::Add(_, _) => todo!(),
        Expr::Sub(_, _) => todo!(),
        Expr::Mul(_, _) => todo!(),
        Expr::Div(_, _) => todo!(),
    }
}

fn instantiate(scheme: &Scheme) -> Type {
    // let mut subst = HashMap::new();
    // for var in &scheme.vars {
    //     subst.insert(*var, Type::Var(subst.len()));
    // }
    // apply_subst(&subst, &scheme.ty)
    todo!()
}

fn type_inference(ctx: &mut Context, expr: &Expr) -> Result<Type, String> {
    let (subst, ty) = infer(ctx, expr)?;
    Ok(apply_subst(&subst, &ty))
}

// =====================================================================
// =                             Eval                                  =
// =====================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<InternedString, Expr>,
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

    pub fn define(&mut self, name: InternedString, val: Expr) {
        self.vars.insert(name, val);
    }

    pub fn lookup(&self, name: &InternedString) -> Option<Expr> {
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
    Lambda(InternedString, Box<Expr>, Rc<RefCell<Env>>),
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

pub fn repl() {
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
                            eprintln!("Error: {}", e);
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
}

// =====================================================================
// =                             Main                                  =
// =====================================================================

fn main() {
    repl();
}
