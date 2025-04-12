use std::{fmt, hash::Hash};

use serde::*;

use crate::{CodeSpan, Ident, Primitive};

/// A function stack signature
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
#[serde(from = "(usize, usize)", into = "(usize, usize)")]
pub struct Signature {
    /// The number of arguments the function pops off the stack
    pub args: usize,
    /// The number of values the function pushes onto the stack
    pub outputs: usize,
    /// The number of arguments the function pops off the under stack
    pub under_args: usize,
    /// The number of values the function pushes onto the under stack
    pub under_outputs: usize,
}

impl From<(usize, usize)> for Signature {
    fn from((args, outputs): (usize, usize)) -> Self {
        Self::new(args, outputs)
    }
}

impl From<Signature> for (usize, usize) {
    fn from(sig: Signature) -> Self {
        (sig.args, sig.outputs)
    }
}

impl Signature {
    /// Create a new signature with the given number of arguments and outputs
    pub const fn new(args: usize, outputs: usize) -> Self {
        Self {
            args,
            outputs,
            under_args: 0,
            under_outputs: 0,
        }
    }
    /// Set the number of arguments and outputs of the under stack
    pub fn with_under(self, under_args: usize, under_outputs: usize) -> Self {
        Self {
            args: self.args,
            outputs: self.outputs,
            under_args,
            under_outputs,
        }
    }
    /// Check if this signature changes the stack size by the same amount as another signature
    pub fn is_compatible_with(self, other: Self) -> bool {
        self.args as isize - self.outputs as isize == other.args as isize - other.outputs as isize
    }
    /// Check if this [`Signature::is_compatible_with`] another signature and has at least as many arguments
    pub fn is_superset_of(self, other: Self) -> bool {
        self.is_compatible_with(other) && self.args >= other.args
    }
    /// Check if this [`Signature::is_compatible_with`] another signature and has at most as many arguments
    pub fn is_subset_of(self, other: Self) -> bool {
        self.is_compatible_with(other) && self.args <= other.args
    }
    /// Get the signature that has the maximum of the arguments and outputs of this signature and another
    pub fn max_with(self, other: Self) -> Self {
        Self::new(self.args.max(other.args), self.outputs.max(other.outputs))
    }
    /// Compose signatures as if a function with signature `other` was called before a function with signature `self`
    pub fn compose(self, other: Self) -> Self {
        let args = other.args + self.args.saturating_sub(other.outputs);
        let outputs = self.outputs + other.outputs.saturating_sub(self.args);
        let under_args = other.under_args + self.under_args.saturating_sub(other.under_outputs);
        let under_outputs =
            self.under_outputs + other.under_outputs.saturating_sub(self.under_args);
        Self::new(args, outputs).with_under(under_args, under_outputs)
    }
    /// Get the un-inverse of this signature
    pub fn inverse(self) -> Self {
        Self::new(self.outputs, self.args)
    }
    /// The the anti-inverse of this signature
    pub fn anti(self) -> Option<Self> {
        if self.args == 0 {
            return None;
        }
        Some(Signature::new(self.outputs + 1, self.args - 1))
    }
}

impl PartialEq<(usize, usize)> for Signature {
    fn eq(&self, other: &(usize, usize)) -> bool {
        self.args == other.0 && self.outputs == other.1
    }
}

impl fmt::Debug for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "|{}.{}", self.args, self.outputs)
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "|{}.{}", self.args, self.outputs)
    }
}

/// A function that executes Rust code
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DynamicFunction {
    /// An index used to look up the function
    pub(crate) index: usize,
    /// The function's signature
    pub(crate) sig: Signature,
}

impl From<(usize, Signature)> for DynamicFunction {
    fn from((index, sig): (usize, Signature)) -> Self {
        Self { index, sig }
    }
}

impl From<DynamicFunction> for (usize, Signature) {
    fn from(func: DynamicFunction) -> Self {
        (func.index, func.sig)
    }
}

impl DynamicFunction {
    /// Get the function's signature
    pub fn signature(&self) -> Signature {
        self.sig
    }
}

impl fmt::Debug for DynamicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<dynamic#{:x}>", self.index)
    }
}

/// A Uiua function id
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(untagged)]
pub enum FunctionId {
    /// Just a primitive
    Primitive(Primitive),
    /// A named function
    Named(Ident),
    /// A macro expansion
    Macro(Option<Ident>, CodeSpan),
    /// The top-level function
    Main,
    #[doc(hidden)]
    /// Implementation detail
    Unnamed,
}

impl PartialEq<&str> for FunctionId {
    fn eq(&self, other: &&str) -> bool {
        match self {
            FunctionId::Named(name) => &&**name == other,
            _ => false,
        }
    }
}

impl From<Ident> for FunctionId {
    fn from(name: Ident) -> Self {
        Self::Named(name)
    }
}

impl From<Primitive> for FunctionId {
    fn from(op: Primitive) -> Self {
        Self::Primitive(op)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionId::Named(name) => write!(f, "{name}"),
            FunctionId::Primitive(prim) => write!(f, "{prim}"),
            FunctionId::Macro(Some(name), span) => write!(f, "macro expansion of {name} at {span}"),
            FunctionId::Macro(None, span) => write!(f, "macro expansion of at {span}"),
            FunctionId::Main => write!(f, "main"),
            FunctionId::Unnamed => write!(f, "unnamed"),
        }
    }
}
