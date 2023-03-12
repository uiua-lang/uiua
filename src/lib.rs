mod algorithm;
pub mod array;
mod array_fmt;
pub mod ast;
pub mod compile;
mod error;
pub mod function;
pub mod lex;
mod ops;
pub mod parse;
mod pervade;
pub mod value;
mod vm;

pub use error::*;

pub type Ident = &'static str;
