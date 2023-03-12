mod algorithm;
pub mod array;
pub mod ast;
pub mod compile;
mod error;
pub mod function;
mod grid_fmt;
pub mod lex;
mod ops;
pub mod parse;
mod pervade;
pub mod value;
mod vm;

use std::rc::Rc;

pub use error::*;

pub type Ident = Rc<str>;
