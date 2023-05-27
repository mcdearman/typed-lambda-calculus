use vm::chunk::Chunk;

use crate::vm::opcode::OpCode;

mod compiler;
mod intern;
mod parser;
mod repl;
mod typing;
mod vm;

fn main() {
    let mut c = Chunk::new();
    c.write(OpCode::Return as u8);
    println!("{:?}", c);
}
