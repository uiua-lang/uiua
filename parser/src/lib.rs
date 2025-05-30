pub mod ast;
mod complex;
mod defs;
mod error;
mod inputs;
mod lex;
pub mod parse;
mod primitive;
mod signature;
mod split;
mod subscript;

pub use {
    complex::*, defs::*, error::*, inputs::*, lex::*, parse::parse, primitive::*, signature::*,
    split::*, subscript::*,
};

pub type Ident = ecow::EcoString;

pub const WILDCARD_CHAR: char = '\u{100000}';
