#![allow(clippy::single_match, clippy::needless_range_loop)]

mod algorithm;
pub mod array;
pub mod ast;
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
    rc::Rc,
};

use array::ArrayValue;

pub use {error::*, run::Uiua, sys::*};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Byte(pub i16);

impl Byte {
    pub fn map(self, f: impl FnOnce(i16) -> i16) -> Self {
        if self.is_fill_value() {
            self
        } else {
            Byte(f(self.0))
        }
    }
    pub fn op(self, other: Self, f: impl FnOnce(i16, i16) -> i16) -> Self {
        if self.is_fill_value() || other.is_fill_value() {
            Byte::fill_value()
        } else {
            Byte(f(self.0, other.0))
        }
    }
    pub fn or(self, default: i16) -> i16 {
        if self.is_fill_value() {
            default
        } else {
            self.0
        }
    }
    pub fn value(self) -> Option<i16> {
        if self.is_fill_value() {
            None
        } else {
            Some(self.0)
        }
    }
}

impl fmt::Debug for Byte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_fill_value() {
            write!(f, "_")
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl From<Byte> for f64 {
    fn from(b: Byte) -> Self {
        if b.is_fill_value() {
            f64::fill_value()
        } else {
            b.0 as f64
        }
    }
}

impl From<bool> for Byte {
    fn from(x: bool) -> Self {
        Byte(x as i16)
    }
}

impl From<u8> for Byte {
    fn from(x: u8) -> Self {
        Byte(x as i16)
    }
}

impl From<f64> for Byte {
    fn from(x: f64) -> Self {
        if x.is_nan() {
            Byte::fill_value()
        } else {
            Byte(x as i16)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident(Rc<str>);

impl Ident {
    pub fn is_functiony(&self) -> bool {
        self.0.starts_with(|c: char| !c.is_lowercase())
    }
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
        Ident(Rc::from(s))
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(Rc::from(s))
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
