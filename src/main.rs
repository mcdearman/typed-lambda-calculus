use lasso::Spur;
use lasso::ThreadedRodeo;
use logos::Logos;
use once_cell::sync::Lazy;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io;
use std::io::Write;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::{cell::RefCell, rc::Rc};

mod compiler;
mod intern;
mod parser;
mod repl;
mod typing;
mod vm;

fn main() {
    // repl();
}
