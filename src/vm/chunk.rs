use super::value::Value;
use std::fmt::{Debug, Write};

#[derive(Clone, PartialEq)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: vec![],
            constants: vec![],
        }
    }

    pub fn write(&mut self, byte: u8) {
        self.code.push(byte);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn disassemble<W: Write>(&self, out: &mut W) -> core::result::Result<(), std::fmt::Error> {
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
    ) -> core::result::Result<usize, std::fmt::Error> {
        write!(out, "{:04} ", offset)?;
        match self.code[offset] {
            0 => Ok(self.simple_instr("RET", offset)),
            _ => {
                write!(out, "Unknown opcode {}", self.code[offset])?;
                Ok(offset + 1)
            }
        }
    }

    fn simple_instr(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.disassemble(f)
    }
}
