use crate::{intern::Ident, parser::Expr};

pub fn eval(env: &mut Vec<(Ident, Expr)>, expr: &Expr) -> Result<Expr, String> {
    match expr {
        lit @ Expr::Int(_) | lit @ Expr::Bool(_) => Ok(lit.clone()),
        Expr::Var(name) => {
            if let Some((_, val)) = env.iter().rev().find(|(var, _)| var == name) {
                Ok(val.clone())
            } else {
                Err(format!("Unbound variable: {}", name))
            }
        }
        lambda @ Expr::Lambda(_, _) => Ok(lambda.clone()),
        Expr::Apply(lambda, value) => match lambda.as_ref() {
            Expr::Lambda(param, body) => {
                env.push((param.clone(), *value.clone()));
                eval(env, body)
            }
            _ => Err(format!("Expected callable lambda, got {}", lambda)),
        },
        Expr::Let(name, val, body) => {
            env.push((name.clone(), *val.clone()));
            eval(env, body)
        }
    }
}
