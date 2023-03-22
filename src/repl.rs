use chumsky::{
    input::Stream,
    prelude::{Input, Simple},
    span::SimpleSpan,
    Parser,
};
use logos::Logos;
use rustyline::{
    error::ReadlineError, highlight::MatchingBracketHighlighter,
    validate::MatchingBracketValidator, Completer, DefaultEditor, Editor, Helper, Highlighter,
    Hinter, Validator,
};

use crate::{
    eval::{eval, Env},
    parser::{parser, Token},
};

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

impl ReplHelper {
    fn new() -> Self {
        Self {
            validator: MatchingBracketValidator::new(),
            highlighter: MatchingBracketHighlighter::new(),
        }
    }
}

pub fn repl() {
    let mut rl = Editor::new().expect("failed to create editor");
    rl.set_helper(Some(ReplHelper::new()));
    let env = Box::new(Env::new());

    println!("Welcome to the Lust REPL!");
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let lex = Token::lexer(&line)
                    .spanned()
                    .map(|(tok, span)| (tok, SimpleSpan::from(span)));

                let tok_stream =
                    Stream::from_iter(lex).spanned(SimpleSpan::from(line.len()..line.len()));
                let ast = parser().parse(tok_stream).into_result();
                match ast {
                    Ok(ast) => match eval(env.clone(), &ast) {
                        Ok(result) => {
                            println!("AST: {:?}", ast.clone());
                            println!("{}", result)
                        }
                        Err(err) => eprintln!("RuntimeError: {}", err),
                    },
                    Err(parse_errs) => parse_errs.into_iter().for_each(|err| {
                        eprintln!("Parse error: {}", err);
                    }),
                };
            }
            Err(ReadlineError::Interrupted) => {
                println!("Ctrl-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Ctrl-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
