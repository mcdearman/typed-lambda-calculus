// use std::rc::Rc;

// use super::opcode::Instr;

// #[derive(Debug, Clone, PartialEq)]
// pub enum Value {
//     Int(i64),
//     Bool(bool),
//     Lambda(Rc<ObjClosure>),
// }

pub type Value = i64;

// #[derive(Debug, Clone, PartialEq)]
// pub struct ObjClosure {
//     pub fun: Rc<ObjFunction>,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct ObjFunction {
//     pub arity: u16,
//     pub code: Vec<Instr>,
//     // pub name: String,
// }

// impl ObjFunction {
//     pub fn new() -> Self {
//         Self {
//             arity: 0,
//             code: vec![],
//             // name: String::new(),
//         }
//     }
// }
