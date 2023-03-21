use std::fmt::Display;

use chumsky::{prelude::*, text::keyword};

use crate::intern::Ident;

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

pub fn parser<'a>() -> impl Parser<'a, &'a str, Expr> {
    // parse var
    let ident = text::ident().padded();

    let expr = recursive(|expr| {
        // parse int
        let int = text::int(10)
            .map(|s: &str| Expr::Int(s.parse().unwrap()))
            .padded();

        // parse bool
        let bool = keyword("true")
            .map(|_| Expr::Bool(true))
            .or(keyword("false").map(|_| Expr::Bool(false)))
            .padded();

        // parse lambda
        let lambda = keyword("\\")
            .then(ident)
            .then_ignore(keyword("->"))
            .then(expr.clone())
            .map(|((_, arg), body)| Expr::Lambda(Ident::from(arg), Box::new(body)))
            .padded();

        // parse apply
        let apply = expr
            .clone()
            .then(expr.clone())
            .map(|(fun, arg)| Expr::Apply(Box::new(fun), Box::new(arg)))
            .padded();

        // parse var
        let var = ident.map(|s: &str| Expr::Var(s.into()));

        // parse operators
        let op = |c| just(c).padded();

        let unary = int.or(var.clone()).or(apply.clone());

        // parse product
        let product = unary.clone().foldl(
            choice((
                op('*').to(Expr::Mul as fn(_, _) -> _),
                op('/').to(Expr::Div as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |l, (op, r)| op(Box::new(l), Box::new(r)),
        );

        // parse sum
        let sum = product.clone().foldl(
            choice((
                op('+').to(Expr::Add as fn(_, _) -> _),
                op('-').to(Expr::Sub as fn(_, _) -> _),
            ))
            .then(product)
            .repeated(),
            |l, (op, r)| op(Box::new(l), Box::new(r)),
        );

        int.or(bool).or(lambda).or(apply).or(var).or(sum).padded()
    });

    // parse let
    let bind = recursive(|bind| {
        let let_ = keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(keyword("in"))
            .then(bind.clone())
            .map(|((name, val), body)| Expr::Let(Ident::from(name), Box::new(val), Box::new(body)));

        let_.or(expr).padded()
    });

    bind
}
