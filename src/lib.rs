#![allow(clippy::single_match, clippy::needless_range_loop)]

mod algorithm;
pub mod array;
pub mod ast;
mod cowslice;
mod error;
pub mod format;
pub mod function;
mod grid_fmt;
mod io;
pub mod lex;
pub mod lsp;
pub mod parse;
pub mod primitive;
#[doc(hidden)]
pub mod profile;
pub mod run;
pub mod value;

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
};

pub use {error::*, io::*, run::Uiua};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Byte {
    Value(u8),
    Fill,
}

impl Byte {
    pub fn map(self, f: impl FnOnce(u8) -> u8) -> Self {
        match self {
            Byte::Value(x) => Byte::Value(f(x)),
            Byte::Fill => Byte::Fill,
        }
    }
    pub fn op(self, other: Self, f: impl FnOnce(u8, u8) -> u8) -> Self {
        match (self, other) {
            (Byte::Value(x), Byte::Value(y)) => Byte::Value(f(x, y)),
            _ => Byte::Fill,
        }
    }
    pub fn op_or_value(self, other: Self, f: impl FnOnce(u8, u8) -> u8) -> Self {
        match (self, other) {
            (Byte::Value(x), Byte::Value(y)) => Byte::Value(f(x, y)),
            (Byte::Value(x), Byte::Fill) => Byte::Value(x),
            (Byte::Fill, Byte::Value(y)) => Byte::Value(y),
            _ => Byte::Fill,
        }
    }
    pub fn or(self, default: u8) -> u8 {
        match self {
            Byte::Value(x) => x,
            Byte::Fill => default,
        }
    }
    pub fn map_or<T>(self, default: T, f: impl FnOnce(u8) -> T) -> T {
        match self {
            Byte::Value(x) => f(x),
            Byte::Fill => default,
        }
    }
    pub fn value(self) -> Option<u8> {
        match self {
            Byte::Value(x) => Some(x),
            Byte::Fill => None,
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
        match self {
            Byte::Value(x) => write!(f, "{x}"),
            Byte::Fill => write!(f, " "),
        }
    }
}

impl From<Byte> for f64 {
    fn from(b: Byte) -> Self {
        match b {
            Byte::Value(x) => x as f64,
            Byte::Fill => f64::NAN,
        }
    }
}

impl From<bool> for Byte {
    fn from(x: bool) -> Self {
        Byte::Value(x as u8)
    }
}

impl From<u8> for Byte {
    fn from(x: u8) -> Self {
        Byte::Value(x)
    }
}

impl From<f64> for Byte {
    fn from(x: f64) -> Self {
        if x.is_nan() {
            Byte::Fill
        } else {
            Byte::Value(x as u8)
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
