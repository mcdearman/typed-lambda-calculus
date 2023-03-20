use std::fmt::Display;

use chumsky::prelude::*;

use crate::intern::Ident;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(Ident),
    Lambda(Ident, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Let(Ident, Box<Expr>, Box<Expr>),
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
        let bool = text::keyword("true")
            .map(|_| Expr::Bool(true))
            .or(text::keyword("false").map(|_| Expr::Bool(false)))
            .padded();

        // parse lambda
        let lambda = text::keyword("\\")
            .then(text::ident())
            .then_ignore(text::keyword("->"))
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
        let var = ident.map(|s: &str| Expr::Var(s.into())).padded();

        int.or(bool).or(lambda).or(apply).or(var).then_ignore(end())
    });

    // parse let
    let bind = recursive(|bind| {
        let let_ = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(text::keyword("in"))
            .then(bind.clone())
            .map(|((name, val), body)| Expr::Let(Ident::from(name), Box::new(val), Box::new(body)))
            .padded();

        let_.or(expr).padded()
    });

    bind
}
