use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{:.2}", n),
            Value::Nil => write!(f, "nil"),
        }
    }
}
