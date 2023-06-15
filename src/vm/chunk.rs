use super::{opcode::OpCode, value::Value};
use core::result::Result;
use std::fmt::{Debug, Write};

#[derive(Clone, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn disassemble<W: Write>(&self, out: &mut W) -> Result<(), std::fmt::Error> {
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instr(out, offset)?;
        }
        Ok(())
    }

    fn disassemble_instr<W: Write>(
        &self,
        out: &mut W,
        offset: usize,
    ) -> Result<usize, std::fmt::Error> {
        write!(out, "{:04} ", offset)?;
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            write!(out, "   | ")?;
        } else {
            write!(out, "{:4} ", self.lines[offset])?;
        }
        match OpCode::from(self.code[offset]) {
            OpCode::Const => self.const_instr(out, offset),
            OpCode::Add => self.simple_instr(out, "ADD", offset),
            OpCode::Sub => self.simple_instr(out, "SUB", offset),
            OpCode::Mul => self.simple_instr(out, "MUL", offset),
            OpCode::Div => self.simple_instr(out, "DIV", offset),
            OpCode::Neg => self.simple_instr(out, "NEG", offset),
            OpCode::Return => self.simple_instr(out, "RET", offset),
            _ => {
                write!(out, "Unknown opcode {}", self.code[offset])?;
                Ok(offset + 1)
            }
        }
    }

    fn const_instr<W: Write>(&self, out: &mut W, offset: usize) -> Result<usize, std::fmt::Error> {
        let constant = self.code[offset + 1];
        write!(out, "CONST {:4} ", constant)?;
        write!(out, "'{}'", self.constants[constant as usize])?;
        writeln!(out)?;
        Ok(offset + 2)
    }

    fn simple_instr<W: Write>(
        &self,
        out: &mut W,
        name: &str,
        offset: usize,
    ) -> Result<usize, std::fmt::Error> {
        writeln!(out, "{}", name)?;
        Ok(offset + 1)
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.disassemble(f)
    }
}

mod tests {
    use super::Chunk;
    use super::OpCode;
    use crate::vm::value::Value;

    #[test]
    fn test_ret() {
        let mut chunk = Chunk::new();
        chunk.write(OpCode::Return as u8, 0);
        insta::assert_debug_snapshot!(chunk);
    }

    #[test]
    fn test_const() {
        let mut chunk = Chunk::new();
        let c1 = chunk.add_constant(Value::Number(1.2));
        let c2 = chunk.add_constant(Value::Number(2.3));
        chunk.write(OpCode::Const as u8, 0);
        chunk.write(c1 as u8, 0);
        chunk.write(OpCode::Const as u8, 1);
        chunk.write(c2 as u8, 1);
        insta::assert_debug_snapshot!(chunk);
    }

    #[test]
    fn test_neg() {
        let mut chunk = Chunk::new();
        chunk.write(OpCode::Neg as u8, 0);
        insta::assert_debug_snapshot!(chunk);
    }
}
