use chumsky::{
    input::Stream, input::ValueInput, prelude::Input, prelude::*, span::SimpleSpan, Parser,
};
use lasso::Spur;
use lasso::ThreadedRodeo;
use logos::Logos;
use once_cell::sync::Lazy;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io;
use std::io::Write;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
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

#[derive(Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Var(TyVar),
    Lambda(Box<Self>, Box<Self>),
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(n) => write!(f, "t{}", n),
            Self::Lambda(param, body) => write!(f, "{} -> {}", param, body),
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
struct Scheme {
    vars: Vec<TyVar>,
    ty: Type,
}
impl Scheme {
    fn new(vars: Vec<TyVar>, ty: Type) -> Self {
        Self { vars, ty }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVar(pub usize);

static COUNTER: AtomicUsize = AtomicUsize::new(0);

impl TyVar {
    fn fresh() -> Self {
        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl Debug for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TyVar({})", self)
    }
}

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn apply_subst(subst: Substitution, ty: Type) -> Type {
    match ty {
        Type::Int | Type::Bool => ty.clone(),
        Type::Var(n) => subst.get(&n).cloned().unwrap_or_else(|| ty.clone()),
        Type::Lambda(param, body) => Type::Lambda(
            Box::new(apply_subst(subst.clone(), *param)),
            Box::new(apply_subst(subst.clone(), *body)),
        ),
    }
}

fn apply_subst_scheme(mut subst: Substitution, scheme: Scheme) -> Scheme {
    for var in &scheme.vars {
        subst.remove(var);
    }
    Scheme {
        vars: scheme.vars.clone(),
        ty: apply_subst(subst.clone(), scheme.ty.clone()),
    }
}

// left-biased union
fn map_union(m1: Substitution, m2: Substitution) -> Substitution {
    m2.into_iter().chain(m1.into_iter()).collect()
}

fn compose_subst(s1: Substitution, s2: Substitution) -> Substitution {
    map_union(
        s2.iter()
            .map(|(var, ty)| (var.clone(), apply_subst(s1.clone(), ty.clone())))
            .collect(),
        s1,
    )
}

fn free_vars(ty: Type) -> BTreeSet<TyVar> {
    match ty {
        Type::Var(n) => vec![n.clone()].into_iter().collect(),
        Type::Lambda(param, body) => free_vars(*param.clone())
            .union(&free_vars(*body.clone()))
            .cloned()
            .collect(),
        _ => BTreeSet::new(),
    }
}

fn free_vars_scheme(scheme: Scheme) -> BTreeSet<TyVar> {
    free_vars(scheme.ty.clone())
        .difference(&scheme.vars.iter().cloned().collect())
        .cloned()
        .collect()
}

fn var_bind(var: TyVar, ty: Type) -> Result<Substitution, String> {
    if ty.clone() == Type::Var(var) {
        Ok(HashMap::new())
    } else if free_vars(ty.clone()).contains(&var) {
        Err(format!("occurs check failed: t{} occurs in {:?}", var, ty))
    } else {
        let mut subst = HashMap::new();
        subst.insert(var, ty.clone());
        Ok(subst)
    }
}

pub fn unify(t1: Type, t2: Type) -> Result<Substitution, String> {
    match (t1.clone(), t2.clone()) {
        (Type::Int, Type::Int) => Ok(HashMap::new()),
        (Type::Bool, Type::Bool) => Ok(HashMap::new()),
        (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
            let s1 = unify(*p1.clone(), *p2.clone())?;
            let s2 = unify(
                apply_subst(s1.clone(), *b1.clone()),
                apply_subst(s1.clone(), *b2.clone()),
            )?;
            Ok(compose_subst(s2.clone(), s1.clone()))
        }
        (Type::Var(n), _) | (_, Type::Var(n)) => var_bind(n.clone(), t2),
        _ => Err(format!("cannot unify {:?} and {:?}", t1, t2)),
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Context {
    vars: HashMap<InternedString, Scheme>,
}

fn apply_subst_ctx(subst: Substitution, ctx: Context) -> Context {
    Context {
        vars: ctx
            .vars
            .into_iter()
            .map(|(name, scheme)| (name, apply_subst_scheme(subst.clone(), scheme)))
            .collect(),
    }
}

fn free_vars_ctx(ctx: Context) -> BTreeSet<TyVar> {
    ctx.vars
        .into_iter()
        .map(|(_, scheme)| free_vars_scheme(scheme))
        .fold(BTreeSet::new(), |acc, set| {
            acc.union(&set).cloned().collect()
        })
}

fn generalize(ctx: Context, ty: Type) -> Scheme {
    Scheme {
        vars: free_vars(ty.clone())
            .difference(&free_vars_ctx(ctx))
            .cloned()
            .collect(),
        ty,
    }
}

fn instantiate(scheme: Scheme) -> Type {
    let mut subst = HashMap::new();
    for var in &scheme.vars {
        subst.insert(*var, Type::Var(TyVar::fresh()));
    }
    apply_subst(subst, scheme.ty)
}

fn infer(ctx: Context, expr: Expr) -> Result<(Substitution, Type), String> {
    match expr.clone() {
        Expr::Int(_) => Ok((HashMap::new(), Type::Int)),
        Expr::Bool(_) => Ok((HashMap::new(), Type::Bool)),
        Expr::Var(name) => match ctx.vars.get(&name) {
            Some(scheme) => Ok((HashMap::new(), instantiate(scheme.clone()))),
            None => Err(format!("unbound variable: {:?}", expr)),
        },
        Expr::Lambda(name, body) => {
            let ty_binder = Type::Var(TyVar::fresh());
            let tmp_ctx = Context {
                vars: ctx
                    .vars
                    .into_iter()
                    .chain(vec![(name, Scheme::new(vec![], ty_binder.clone()))].into_iter())
                    .collect(),
            };
            let (s1, t1) = infer(tmp_ctx, *body.clone())?;
            Ok((
                s1.clone(),
                Type::Lambda(Box::new(apply_subst(s1.clone(), ty_binder)), Box::new(t1)),
            ))
        }
        Expr::Apply(fun, arg) => {
            let (s1, t1) = infer(ctx.clone(), *fun.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *arg.clone())?;
            let t3 = Type::Var(TyVar::fresh());
            let s3 = unify(
                apply_subst(s2.clone(), t1),
                Type::Lambda(Box::new(t2), Box::new(t3.clone())),
            )?;
            Ok((
                compose_subst(s3.clone(), s2.clone()),
                apply_subst(s3, t3.clone()),
            ))
        }
        Expr::Let(name, binding, body) => {
            let (s1, t1) = infer(ctx.clone(), *binding.clone())?;
            let scheme = generalize(apply_subst_ctx(s1.clone(), ctx.clone()), t1.clone());
            let tmp_ctx = Context {
                vars: ctx
                    .vars
                    .clone()
                    .into_iter()
                    .chain(vec![(name.clone(), scheme)].into_iter())
                    .collect(),
            };
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), tmp_ctx), *body.clone())?;
            Ok((compose_subst(s2.clone(), s1.clone()), t2.clone()))
        }
        Expr::Add(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            Ok((
                compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
                Type::Int,
            ))
        }
        Expr::Sub(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            Ok((
                compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
                Type::Int,
            ))
        }
        Expr::Mul(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            Ok((
                compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
                Type::Int,
            ))
        }
        Expr::Div(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            Ok((
                compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
                Type::Int,
            ))
        }
    }
}

fn type_inference(ctx: Context, expr: Expr) -> Result<Type, String> {
    println!("type_inference: {:?}", expr);
    let (subst, ty) = infer(ctx, expr)?;
    Ok(apply_subst(subst, ty))
}

fn type_check(expr: Expr) -> Result<Expr, String> {
    type_inference(default_ctx(), expr.clone()).map(|_| expr.clone())
}

fn default_ctx() -> Context {
    let mut ctx = Context {
        vars: HashMap::new(),
    };
    ctx.vars.insert(
        InternedString::from("id"),
        Scheme::new(
            vec![TyVar::fresh()],
            Type::Lambda(
                Box::new(Type::Var(TyVar::fresh())),
                Box::new(Type::Var(TyVar::fresh())),
            ),
        ),
    );
    ctx
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
    type_check(expr.clone()).map_err(|e| format!("Type error: {}", e))?;
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
