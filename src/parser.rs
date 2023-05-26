use crate::intern::InternedString;
use chumsky::{input::ValueInput, prelude::*, span::SimpleSpan, Parser};
use logos::Logos;
use std::fmt::Display;

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

    let expr = recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Int(n) => Expr::Int(n),
                Token::Bool(b) => Expr::Bool(b),
            }
            .boxed();

            // parse let
            let let_ = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Eq))
                .then(inline_expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map(|((name, val), body)| {
                    Expr::Let(InternedString::from(name), Box::new(val), Box::new(body))
                })
                .boxed();

            // parse curry lambda
            let lambda = just(Token::Lambda)
                .ignore_then(
                    ident
                        .repeated()
                        .foldr(just(Token::Arrow).ignore_then(expr.clone()), |arg, body| {
                            Expr::Lambda(InternedString::from(arg), Box::new(body))
                        }),
                )
                .boxed();

            let atom = choice((
                val,
                let_,
                lambda,
                ident.map(Expr::Var),
                expr.clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            ))
            .boxed();

            // parse function application
            let apply = atom
                .clone()
                .foldl(atom.clone().repeated(), |f, arg| {
                    Expr::Apply(Box::new(f), Box::new(arg))
                })
                .boxed();

            // parse arithmetic
            let op = just(Token::Mul).or(just(Token::Div)).boxed();

            let product = apply
                .clone()
                .foldl(
                    op.clone().then(apply.clone()).repeated(),
                    |l, (op, r)| match op {
                        Token::Mul => Expr::Mul(Box::new(l), Box::new(r)),
                        Token::Div => Expr::Div(Box::new(l), Box::new(r)),
                        _ => unreachable!(),
                    },
                )
                .boxed();

            let op = just(Token::Add).or(just(Token::Sub)).boxed();
            let sum = product
                .clone()
                .foldl(
                    op.clone().then(product.clone()).repeated(),
                    |l, (op, r)| match op {
                        Token::Add => Expr::Add(Box::new(l), Box::new(r)),
                        Token::Sub => Expr::Sub(Box::new(l), Box::new(r)),
                        _ => unreachable!(),
                    },
                )
                .boxed();

            sum
        });

        inline_expr
    });

    expr
}
