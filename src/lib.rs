/*!
The Uiua programming language

This currently exists as a library only to reserve the name on crates.io.
The current API should be considered deeply unstable.
*/

#![allow(clippy::single_match, clippy::needless_range_loop)]

mod algorithm;
pub mod array;
pub mod ast;
mod check;
mod compile;
mod cowslice;
mod error;
pub mod format;
pub mod function;
mod grid_fmt;
pub mod lex;
pub mod lsp;
pub mod parse;
pub mod primitive;
#[doc(hidden)]
pub mod profile;
pub mod run;
mod sys;
pub mod value;
#[cfg(feature = "terminal_image")]
mod viuer;

use std::sync::Arc;

pub use {error::*, run::Uiua, sys::*};

pub type Ident = Arc<str>;

#[test]
fn suite() {
    for entry in std::fs::read_dir("tests").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|s| s == "ua") {
            if let Err(e) = Uiua::with_native_sys().load_file(&path) {
                panic!("Test failed in {}:\n{}", path.display(), e.show(true));
            }
        }
    }
}
