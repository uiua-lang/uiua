#![allow(clippy::single_match, clippy::needless_range_loop)]

mod algorithm;
pub mod array;
pub mod ast;
mod check;
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

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    sync::Arc,
};

pub use {error::*, run::Uiua, sys::*};

#[derive(Debug, Clone)]
pub struct Ident(Arc<str>);

impl Ident {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &'a str) -> Self {
        Ident(Arc::from(s))
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(Arc::from(s))
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_lowercase() == other.0.to_lowercase()
    }
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.to_lowercase().partial_cmp(&other.0.to_lowercase())
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.to_lowercase().cmp(&other.0.to_lowercase())
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_lowercase().hash(state)
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.0.to_lowercase() == other.to_lowercase()
    }
}

impl<'a> PartialEq<&'a str> for Ident {
    fn eq(&self, other: &&'a str) -> bool {
        self.0.to_lowercase() == other.to_lowercase()
    }
}
