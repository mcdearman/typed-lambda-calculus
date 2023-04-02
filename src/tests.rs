use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
use logos::Logos;

use crate::{parser, Token};

#[test]
fn test_parse_let() {
    let src = "let x = 5 in x";
    let lex = Token::lexer(&src)
        .spanned()
        .map(|(tok, span)| (tok, SimpleSpan::from(span)));
    let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
    let ast = parser()
        .parse(tok_stream)
        .into_result()
        .expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn test_parse_multiline_let() {
    let src = "let x = \n5 in x";
    let lex = Token::lexer(&src)
        .spanned()
        .map(|(tok, span)| (tok, SimpleSpan::from(span)));
    let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
    let ast = parser()
        .parse(tok_stream)
        .into_result()
        .expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}
