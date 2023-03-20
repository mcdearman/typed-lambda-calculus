use chumsky::Parser;

use crate::{eval::eval, parser::parser};

mod eval;
mod parser;
mod typing;

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    match parser().parse(&src).into_result() {
        Ok(ast) => match eval(&mut Vec::new(), &ast) {
            Ok(result) => println!("{}", result),
            Err(err) => eprintln!("RuntimeError: {}", err),
        },
        Err(parse_errs) => parse_errs.into_iter().for_each(|err| {
            eprintln!("Parse error: {}", err);
        }),
    };
}
