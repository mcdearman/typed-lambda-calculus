use crate::{parser::TokenKind, vm::chunk};
use logos::Logos;

pub fn compile<'src>(src: &'src str, chunk: &mut chunk::Chunk) -> bool {
    let lexer = TokenKind::lexer(src);
    todo!()
}
