#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Var(String),
    Lambda(Box<Self>, Box<Self>),
}
