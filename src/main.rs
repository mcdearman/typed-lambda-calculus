use repl::repl;

mod eval;
mod intern;
mod parser;
mod repl;
mod typing;

fn main() {
    repl();
}
