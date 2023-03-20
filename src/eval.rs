use std::collections::HashMap;

use crate::parser::Expr;

#[derive(Debug, Clone)]
pub struct Env(HashMap<String, Expr>);

impl Env {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, name: String, value: Expr) {
        self.0.insert(name, value);
    }

    pub fn lookup(&self, name: &str) -> Option<&Expr> {
        self.0.get(name)
    }
}

pub fn eval(mut env: &Env, expr: &Expr) -> Result<Expr, String> {
    match expr {
        lit @ Expr::Int(_) | lit @ Expr::Bool(_) => Ok(lit.clone()),
        Expr::Var(name) => env
            .lookup(name)
            .cloned()
            .ok_or(format!("Unbound variable: {}", name)),
        Expr::Lambda(param, body) => todo!(),
        Expr::Apply(_, _) => todo!(),
        Expr::Let(name, val, body) => todo!(),
    }
}
