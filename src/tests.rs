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
fn test_infer_int() {
    let src = "5";
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

#[test]
fn test_infer_bool() {
    let src = "true";
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

#[test]
fn test_infer_add() {
    let src = "5 + 3";
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

#[test]
fn test_infer_lambda_id() {
    let src = "\\x -> x";
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

#[test]
fn test_infer_lambda_add() {
    let src = "\\x -> x + 1";
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

#[test]
fn test_infer_lambda_add_vars() {
    let src = "\\x y -> x + y";
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

#[test]
fn test_infer_apply_lambda_add() {
    let src = "(\\x -> x + 1) 5";
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

#[test]
fn test_infer_lambda_apply() {
    // ('a -> 'b) -> 'a -> 'b
    let src = "\\f x -> f x";
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

#[test]
fn test_infer_partial_apply_lambda_apply() {
    // Int -> Int
    let src = "(\\f x -> f x) (\\x -> x + 1)";
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
