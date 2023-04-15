use std::default;

use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
use logos::Logos;

use crate::{default_ctx, infer, parser, type_inference, Token};

// ============================================================================
//                                  Parser
// ============================================================================

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
    let src = "let x =\n5 in x";
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
fn test_parse_apply() {
    let src = "let f = \\x -> x in f 5";
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
fn test_parse_multiline_apply() {
    let src = "let f =\n\\x -> x in f 5";
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

// ============================================================================
//                               Type Inference
// ============================================================================

#[test]
fn test_infer_let() {
    let src = "let f = \\x -> x in f 5";
    let lex = Token::lexer(&src)
        .spanned()
        .map(|(tok, span)| (tok, SimpleSpan::from(span)));
    let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
    let ast = parser()
        .parse(tok_stream)
        .into_result()
        .expect("failed to parse");
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}
