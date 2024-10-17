use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign, BitAnd, BitOr, BitOrAssign},
};

use serde::*;
use serde_tuple::*;

use crate::{
    lex::CodeSpan,
    primitive::{ImplPrimitive, Primitive},
    Assembly, Ident, Instr, NewFunction,
};

/// A Uiua function
#[derive(Clone, Serialize_tuple, Deserialize_tuple)]
pub struct Function {
    /// The function's id
    pub id: FunctionId,
    signature: Signature,
    pub(crate) slice: FuncSlice,
    hash: u64,
    pub(crate) flags: FunctionFlags,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            id: FunctionId::Unnamed,
            signature: Signature::new(0, 0),
            slice: FuncSlice::default(),
            hash: 0,
            flags: FunctionFlags::default(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub(crate) struct FunctionFlags(u8);

impl BitOrAssign for FunctionFlags {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitOr for FunctionFlags {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitAnd for FunctionFlags {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl FunctionFlags {
    pub const RECURSIVE: Self = Self(1 << 0);
    pub const NO_INLINE: Self = Self(1 << 1);
    pub const TRACK_CALLER: Self = Self(1 << 2);
    pub const NO_PRE_EVAL: Self = Self(1 << 3);
    pub fn recursive(&self) -> bool {
        self.0 & Self::RECURSIVE.0 != 0
    }
    pub fn no_inline(&self) -> bool {
        self.0 & Self::NO_INLINE.0 != 0
    }
    pub fn track_caller(&self) -> bool {
        self.0 & Self::TRACK_CALLER.0 != 0
    }
    pub fn no_pre_eval(&self) -> bool {
        self.0 & Self::NO_PRE_EVAL.0 != 0
    }
}

impl fmt::Debug for FunctionFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut wrote = false;
        for (i, name) in ["RECURSIVE", "NO_INLINE", "TRACK_CALLER", "NO_PRE_EVAL"]
            .into_iter()
            .enumerate()
        {
            if self.0 & (1 << i) != 0 {
                if wrote {
                    write!(f, " | ")?;
                }
                wrote = true;
                write!(f, "{name}")?;
            }
        }
        Ok(())
    }
}

/// A range of compiled instructions
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
#[serde(from = "(usize, usize)", into = "(usize, usize)")]
pub struct FuncSlice {
    pub(crate) start: usize,
    pub(crate) len: usize,
}

impl From<(usize, usize)> for FuncSlice {
    fn from((start, len): (usize, usize)) -> Self {
        Self { start, len }
    }
}

impl From<FuncSlice> for (usize, usize) {
    fn from(slice: FuncSlice) -> Self {
        (slice.start, slice.len)
    }
}

impl FuncSlice {
    /// Get the length of the instructions
    pub fn len(&self) -> usize {
        self.len
    }
    /// Check if the instructions are empty
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
    /// Get the end of the instructions
    pub fn end(&self) -> usize {
        self.start + self.len
    }
}

impl AddAssign<usize> for FuncSlice {
    fn add_assign(&mut self, rhs: usize) {
        self.start += rhs;
    }
}

impl Add<usize> for FuncSlice {
    type Output = Self;
    fn add(mut self, rhs: usize) -> Self::Output {
        self += rhs;
        self
    }
}

/// A function stack signature
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(from = "(usize, usize)", into = "(usize, usize)")]
pub struct Signature {
    /// The number of arguments the function pops off the stack
    pub args: usize,
    /// The number of values the function pushes onto the stack
    pub outputs: usize,
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
        Self { args, outputs }
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
        Self::new(args, outputs)
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
        if self.outputs == 1 {
            write!(f, "|{}", self.args)
        } else {
            write!(f, "{self:?}")
        }
    }
}

/// A function that executes Rust code
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DynamicFunction {
    /// An index used to look up the function
    pub(crate) index: usize,
    /// The function's signature
    pub(crate) signature: Signature,
}

impl From<(usize, Signature)> for DynamicFunction {
    fn from((index, signature): (usize, Signature)) -> Self {
        Self { index, signature }
    }
}

impl From<DynamicFunction> for (usize, Signature) {
    fn from(func: DynamicFunction) -> Self {
        (func.index, func.signature)
    }
}

impl DynamicFunction {
    /// Get the function's signature
    pub fn signature(&self) -> Signature {
        self.signature
    }
}

impl fmt::Debug for DynamicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<dynamic#{:x}>", self.index)
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.slice == other.slice && self.hash == other.hash
    }
}

impl Eq for Function {}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        self.slice.cmp(&other.slice)
    }
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.slice.hash(state);
        self.hash.hash(state);
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl Function {
    /// Create a new function
    pub(crate) fn new(id: FunctionId, signature: Signature, slice: FuncSlice, hash: u64) -> Self {
        Self {
            id,
            slice,
            signature,
            hash,
            flags: FunctionFlags::default(),
        }
    }
    pub(crate) fn with_flags(mut self, flags: FunctionFlags) -> Self {
        self.flags = flags;
        self
    }
    /// Get the [`Signature`] of this function
    pub fn signature(&self) -> Signature {
        self.signature
    }
    /// Whether this function is recursive
    pub fn is_recursive(&self) -> bool {
        self.flags.recursive()
    }
    /// Get the address of function's instructions
    pub fn slice(&self) -> FuncSlice {
        self.slice
    }
    /// Get the function's instructions
    #[track_caller]
    pub fn instrs<'a>(&self, asm: &'a Assembly) -> &'a [Instr] {
        let end = self.slice.end();
        assert!(
            self.slice.start <= asm.instrs.len(),
            "{self} slice start {} out of bounds of {} instrs",
            self.slice.start,
            asm.instrs.len()
        );
        assert!(
            end <= asm.instrs.len(),
            "{self} slice end {} out of bounds of {} instrs",
            end,
            asm.instrs.len()
        );
        &asm.instrs[self.slice.start..end]
    }
    pub(crate) fn new_func(&self, asm: &Assembly) -> NewFunction {
        NewFunction {
            instrs: self.instrs(asm).into(),
            flags: self.flags,
        }
    }
    /// Get a mutable slice of the function's instructions
    pub fn instrs_mut<'a>(&self, asm: &'a mut Assembly) -> &'a mut [Instr] {
        asm.instrs_mut(self.slice)
    }
    /// Try to get a lone primitive from this function
    pub fn as_primitive(&self, asm: &Assembly) -> Option<Primitive> {
        self.as_flipped_primitive(asm)
            .filter(|(_, flipped)| !flipped)
            .map(|(prim, _)| prim)
    }
    /// Try to get a lone implementation primitive from this function
    pub fn as_impl_primitive(&self, asm: &Assembly) -> Option<ImplPrimitive> {
        self.as_flipped_impl_primitive(asm)
            .filter(|(_, flipped)| !flipped)
            .map(|(prim, _)| prim)
    }
    pub(crate) fn as_flipped_primitive(&self, asm: &Assembly) -> Option<(Primitive, bool)> {
        match &self.id {
            FunctionId::Primitive(prim) => Some((*prim, false)),
            _ => instrs_as_flipped_primitive(self.instrs(asm), asm),
        }
    }
    pub(crate) fn as_flipped_impl_primitive(
        &self,
        asm: &Assembly,
    ) -> Option<(ImplPrimitive, bool)> {
        instrs_as_flipped_impl_primitive(self.instrs(asm), asm)
    }
    pub(crate) fn hash(&self) -> u64 {
        self.hash
    }
}

pub(crate) fn instrs_as_flipped_primitive(
    instrs: &[Instr],
    asm: &Assembly,
) -> Option<(Primitive, bool)> {
    use Primitive::*;
    match instrs {
        [Instr::Prim(Flip, _), Instr::Prim(prim, _)] => Some((*prim, true)),
        [Instr::Prim(prim, _)] => Some((*prim, false)),
        [Instr::PushFunc(f), Instr::Call(_)] => f.as_flipped_primitive(asm.as_ref()),
        _ => None,
    }
}
pub(crate) fn instrs_as_flipped_impl_primitive(
    instrs: &[Instr],
    asm: &Assembly,
) -> Option<(ImplPrimitive, bool)> {
    use Primitive::*;
    match instrs {
        [Instr::ImplPrim(prim, _), Instr::Prim(Flip, _)] => Some((*prim, true)),
        [Instr::ImplPrim(prim, _)] => Some((*prim, false)),
        [Instr::PushFunc(f), Instr::Call(_)] => f.as_flipped_impl_primitive(asm.as_ref()),
        _ => None,
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
    /// An anonymous function
    Anonymous(CodeSpan),
    /// A macro expansion
    Macro(Ident, CodeSpan),
    /// The top-level function
    Main,
    #[doc(hidden)]
    /// Implementation detail
    Unnamed,
}

impl FunctionId {
    /// Get the span of the function id, if it has one
    pub fn span(&self) -> Option<&CodeSpan> {
        match self {
            FunctionId::Anonymous(span) => Some(span),
            FunctionId::Macro(_, span) => Some(span),
            _ => None,
        }
    }
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

impl From<CodeSpan> for FunctionId {
    fn from(span: CodeSpan) -> Self {
        Self::Anonymous(span)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionId::Named(name) => write!(f, "{name}"),
            FunctionId::Anonymous(span) => write!(f, "fn from {span}"),
            FunctionId::Primitive(prim) => write!(f, "{prim}"),
            FunctionId::Macro(name, span) => write!(f, "macro expansion of {name} at {span}"),
            FunctionId::Main => write!(f, "main"),
            FunctionId::Unnamed => write!(f, "unnamed"),
        }
    }
}
