use std::collections::HashMap;

use crate::parser::Expr;

// #[derive(Debug, Clone)]
// pub struct Env<'a>(HashMap<&'a str, Expr<'a>>);

// impl<'a> Env<'a> {
//     pub fn new() -> Self {
//         Self(HashMap::new())
//     }

//     pub fn insert(&mut self, name: String, value: Expr<'_>) {
//         self.0.insert(name, value.clone());
//     }

//     pub fn lookup(&self, name: &str) -> Option<&Expr> {
//         self.0.get(name)
//     }
// }

pub fn eval<'a>(
    env: &mut Vec<(&'a str, &'a Expr<'a>)>,
    expr: &'a Expr<'a>,
) -> Result<&'a Expr<'a>, String> {
    match expr {
        lit @ Expr::Int(_) | lit @ Expr::Bool(_) => Ok(lit),
        Expr::Var(name) => {
            if let Some((_, val)) = env.iter().rev().find(|(var, _)| var == name) {
                Ok(val.clone())
            } else {
                Err(format!("Unbound variable: {}", name))
            }
        }
        Expr::Lambda(param, body) => todo!(),
        Expr::Apply(_, _) => todo!(),
        Expr::Let(name, val, body) => todo!(),
    }
}
