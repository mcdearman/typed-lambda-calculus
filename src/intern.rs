use lasso::Spur;
use lasso::ThreadedRodeo;
use once_cell::sync::Lazy;
use std::fmt::{Debug, Display};

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub key: Spur,
}

impl From<Spur> for Ident {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ident({})", unsafe { INTERNER.resolve(&self.key) })
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { INTERNER.resolve(&self.key) })
    }
}
