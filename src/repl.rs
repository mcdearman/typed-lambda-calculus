use std::{
    cell::RefCell,
    io::{self, Write},
};

use chumsky::span::SimpleSpan;

use crate::parser::Token;

pub fn repl() {
    println!("Welcome to the Lust REPL!");
    print!("> ");
    io::stdout().flush().expect("failed to flush stdout");
    let mut src = String::new();
    let mut carry = String::new();
    let env = Rc::new(RefCell::new(default_env()));
    'outer: loop {
        std::io::stdin()
            .read_line(&mut src)
            .expect("failed to read line");
        for expr_str in src.trim_end().split_inclusive(";;") {
            if expr_str.ends_with(";;") {
                let expr_str = carry.clone() + expr_str.trim_end_matches(";;");
                let lex = Token::lexer(&expr_str)
                    .spanned()
                    .map(|(tok, span)| (tok, SimpleSpan::from(span)));
                let tok_stream =
                    Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
                match parser().parse(tok_stream).into_result() {
                    Ok(expr) => {
                        let ty = match type_inference(default_ctx(), expr.clone()) {
                            Ok(ty) => ty,
                            Err(e) => {
                                // println!("ast: {:?}", expr);
                                eprintln!("Error: {}", e);
                                carry.clear();
                                continue 'outer;
                            }
                        };
                        match eval(env.clone(), &expr) {
                            Ok(v) => {
                                // println!("ast: {:?}", expr);
                                println!("val: {} = {}", ty, v);
                            }
                            Err(e) => {
                                // println!("ast: {:?}", expr);
                                eprintln!("Error: {}", e);
                            }
                        }
                    }
                    Err(e) => {
                        println!("Error: {:?}", e);
                        carry.clear();
                        continue 'outer;
                    }
                }
                carry.clear();
            } else {
                carry.push_str(expr_str);
                src.clear();
                print!("- ");
                io::stdout().flush().expect("failed to flush stdout");
                continue 'outer;
            }
        }
        src.clear();
        print!("\n> ");
        io::stdout().flush().expect("failed to flush stdout");
    }
}
