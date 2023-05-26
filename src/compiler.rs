use crate::vm::Instr;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct CompilerError(pub String);

impl CompilerError {
    pub fn new(msg: String) -> CompilerError {
        CompilerError(msg)
    }
}

impl From<&str> for CompilerError {
    fn from(msg: &str) -> CompilerError {
        CompilerError::new(msg.to_string())
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

pub struct Compiler {
    code: Vec<Instr>,
    env: HashMap<String, u16>,
    next_var: u16,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: vec![],
            env: HashMap::new(),
            next_var: 0,
        }
    }

    pub fn compile(&mut self) -> Result<Vec<Instr>> {
        todo!()
    }
}
