use std::fmt::Display;

use chumsky::prelude::*;
// use logos::Logos;

// #[derive(Logos, Debug, Clone, PartialEq)]
// pub enum Token {
//     #[regex(r"[0-9]+", |lex| lex.slice().parse())]
//     Int(i64),
//     #[regex(r"true|false", |lex| lex.slice().parse())]
//     Bool(bool),
//     #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
//     Var(String),
//     #[token("\\")]
//     Lambda,
//     #[error]
//     Err,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Int(i64),
    Bool(bool),
    Var(&'a str),
    Lambda(&'a str, Box<Expr<'a>>),
    Apply(Box<Expr<'a>>, Box<Expr<'a>>),
    Let(&'a str, Box<Expr<'a>>, Box<Expr<'a>>),
}

impl<'a> Display for Expr<'a> {
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

pub fn parser<'a>() -> impl Parser<'a, &'a str, Expr<'a>> {
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
            .map(|((_, arg), body)| Expr::Lambda(arg, Box::new(body)))
            .padded();

        // parse apply
        let apply = expr
            .clone()
            .then(expr.clone())
            .map(|(fun, arg)| Expr::Apply(Box::new(fun), Box::new(arg)))
            .padded();

        // parse var
        let var = ident.map(|s: &str| Expr::Var(s)).padded();

        int.or(bool).or(lambda).or(apply).then_ignore(end())
    });

    // parse let
    let decl = recursive(|decl| {
        let r#let = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(text::keyword("in"))
            .then(decl.clone())
            .map(|((name, val), body)| Expr::Let(name, Box::new(val), Box::new(body)))
            .padded();

        r#let.or(expr).padded()
    });

    decl
}
