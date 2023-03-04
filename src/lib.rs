pub mod ast;
pub mod builtin;
pub mod compile;
mod error;
pub mod lex;
pub mod parse;
pub mod value;
mod vm;

use std::fmt;

pub use error::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ident {
    Name(&'static str),
    Placeholder(usize),
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ident::Name(s) => write!(f, "{s}",),
            Ident::Placeholder(n) => write!(f, "_{n}"),
        }
    }
}

impl From<&'static str> for Ident {
    fn from(s: &'static str) -> Self {
        Ident::Name(s)
    }
}
