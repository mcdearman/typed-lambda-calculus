use std::collections::HashMap;

use crate::{intern::Ident, parser::Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    parent: Option<Box<Env>>,
    vars: HashMap<Ident, Expr>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            parent: None,
            vars: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: Ident, val: Expr) {
        self.vars.insert(name, val);
    }

    pub fn lookup(&self, name: &Ident) -> Option<&Expr> {
        self.vars
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name)))
    }
}

pub fn eval(mut env: Box<Env>, expr: &Expr) -> Result<Expr, String> {
    match expr {
        lit @ Expr::Int(_) | lit @ Expr::Bool(_) | lit @ Expr::Lambda(_, _) => Ok(lit.clone()),
        Expr::Var(name) => {
            if let Some(val) = env.lookup(name) {
                Ok(val.clone())
            } else {
                Err(format!("Unbound variable: {}", name))
            }
        }
        Expr::Apply(lambda, value) => match lambda.as_ref() {
            Expr::Lambda(param, body) => {
                env.define(param.clone(), *value.clone());
                eval(env, body)
            }
            _ => Err(format!("Expected callable lambda, got {}", lambda)),
        },
        Expr::Let(name, val, body) => {
            env.define(name.clone(), *val.clone());
            eval(env, body)
        }
        Expr::Add(l, r) => match (
            eval(env.clone(), l.as_ref())?,
            eval(env.clone(), r.as_ref())?,
        ) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l + r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Sub(l, r) => match (l.as_ref(), r.as_ref()) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l - r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Mul(l, r) => match (l.as_ref(), r.as_ref()) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l * r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
        Expr::Div(l, r) => match (l.as_ref(), r.as_ref()) {
            (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l / r)),
            _ => Err(format!("Expected two integers, got {} and {}", l, r)),
        },
    }
}
