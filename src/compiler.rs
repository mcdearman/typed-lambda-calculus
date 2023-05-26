use crate::vm::Instr;
use std::collections::HashMap;

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

    pub fn compile(&mut self) -> Vec<Instr> {
        self.code.clone()
    }
}
