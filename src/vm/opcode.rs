#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Const,
    Return,
    Halt,
}

// impl From<u8> for OpCode {
//     fn from(byte: u8) -> Self {
//         match byte {
//             0 => OpCode::Return,
//             _ => panic!("invalid opcode"),
//         }
//     }
// }
