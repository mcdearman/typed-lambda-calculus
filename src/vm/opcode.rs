#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Const,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Return,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Const,
            1 => OpCode::Add,
            2 => OpCode::Sub,
            3 => OpCode::Mul,
            4 => OpCode::Div,
            5 => OpCode::Neg,
            6 => OpCode::Return,
            _ => panic!("invalid opcode"),
        }
    }
}
