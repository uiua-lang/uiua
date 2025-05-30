//! The [`Boxed`] type

use std::{
    borrow::{Borrow, BorrowMut},
    fmt,
};

use serde::*;

use crate::value::Value;

/// The element type for box arrays
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
#[serde(from = "BoxedRep", into = "BoxedRep")]
pub struct Boxed(pub Value);

#[derive(Serialize, Deserialize)]
struct BoxedRep {
    b: Value,
}

impl From<Boxed> for BoxedRep {
    fn from(b: Boxed) -> Self {
        Self { b: b.0 }
    }
}

impl From<BoxedRep> for Boxed {
    fn from(b: BoxedRep) -> Self {
        Self(b.b)
    }
}

impl fmt::Debug for Boxed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Boxed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<Value> for Boxed {
    fn from(v: Value) -> Self {
        Self(v)
    }
}

impl AsRef<Value> for Boxed {
    fn as_ref(&self) -> &Value {
        &self.0
    }
}

impl AsMut<Value> for Boxed {
    fn as_mut(&mut self) -> &mut Value {
        &mut self.0
    }
}

impl Borrow<Value> for Boxed {
    fn borrow(&self) -> &Value {
        &self.0
    }
}

impl BorrowMut<Value> for Boxed {
    fn borrow_mut(&mut self) -> &mut Value {
        &mut self.0
    }
}
