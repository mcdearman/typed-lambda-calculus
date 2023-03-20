use chumsky::Parser;
use rustyline::{
    error::ReadlineError, highlight::MatchingBracketHighlighter,
    validate::MatchingBracketValidator, Completer, DefaultEditor, Editor, Helper, Highlighter,
    Hinter, Validator,
};

use crate::{eval::eval, parser::parser};

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
    let mut env = Vec::new();

    println!("Welcome to the Lust REPL!");
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                match parser().parse(&line).into_result() {
                    Ok(ast) => match eval(&mut env, &ast) {
                        Ok(result) => println!("{}", result),
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
