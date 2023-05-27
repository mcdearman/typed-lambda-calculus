// use std::collections::HashMap;

// use crate::vm::chunk::Chunk;

// #[derive(Debug, Clone, PartialEq)]
// pub struct CompilerError(pub String);

// impl CompilerError {
//     pub fn new(msg: String) -> CompilerError {
//         CompilerError(msg)
//     }
// }

// impl From<&str> for CompilerError {
//     fn from(msg: &str) -> CompilerError {
//         CompilerError::new(msg.to_string())
//     }
// }

// pub type Result<T> = std::result::Result<T, CompilerError>;

// pub struct Compiler {
//     chunk: Chunk,
//     env: HashMap<String, u16>,
//     next_var: u16,
// }

// impl Compiler {
//     pub fn new() -> Self {
//         Self {
//             chunk: Chunk::new(),
//             env: HashMap::new(),
//             next_var: 0,
//         }
//     }

//     pub fn compile(&mut self) -> Result<Vec<Instr>> {
//         todo!()
//     }
// }
