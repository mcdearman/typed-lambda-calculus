use std::fmt::Display;

use chumsky::{input::ValueInput, prelude::*, primitive::select, text::keyword};
use logos::Logos;

use crate::intern::Ident;

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
    #[error]
    Err,
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

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>() -> impl Parser<'a, I, Expr>
{
    let ident = select! { Token::Ident(name) => name };

    let expr = recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Int(n) => Expr::Int(n),
                Token::Bool(b) => Expr::Bool(b),
            };

            let atom = choice((
                val,
                ident.map(Expr::Var),
                expr.clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            ));

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

            // parse function application
            let apply = atom.clone().foldl(atom.clone().repeated(), |f, arg| {
                Expr::Apply(Box::new(f), Box::new(arg))
            });

            apply
        });

        expr
    });

    expr
}
