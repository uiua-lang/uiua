use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign},
};

use ecow::{EcoString, EcoVec};
use enum_iterator::Sequence;
use serde::*;
use serde_tuple::*;

use crate::{
    lex::CodeSpan,
    primitive::{ImplPrimitive, Primitive},
    value::Value,
    Assembly, Ident,
};

/// A Uiua bytecode instruction
#[derive(Clone)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum Instr {
    /// A comment
    Comment(Ident) = 0,
    /// Push a value onto the stack
    Push(Value),
    /// Push a global value onto the stack
    CallGlobal {
        index: usize,
        call: bool,
        sig: Signature,
    },
    /// Bind a global value
    BindGlobal {
        span: usize,
        index: usize,
    },
    /// Begin an array
    BeginArray,
    /// End an array
    EndArray {
        boxed: bool,
        span: usize,
    },
    /// Execute a primitive
    Prim(Primitive, usize),
    /// Execute an implementation primitive
    ImplPrim(ImplPrimitive, usize),
    /// Call a function
    Call(usize),
    /// Push a function onto the function stack
    PushFunc(Function),
    /// Execute a switch function
    Switch {
        count: usize,
        sig: Signature,
        span: usize,
    },
    /// Do a format string
    Format {
        parts: EcoVec<EcoString>,
        span: usize,
    },
    /// Label an array
    Label {
        label: EcoString,
        span: usize,
    },
    /// Call a dynamic function
    Dynamic(DynamicFunction),
    /// Bind some local values
    PushLocals {
        count: usize,
        span: usize,
    },
    PopLocals,
    /// Get a local value
    GetLocal {
        index: usize,
        span: usize,
    },
    Unpack {
        count: usize,
        span: usize,
        unbox: bool,
    },
    PushTempFunctions(usize),
    PopTempFunctions(usize),
    GetTempFunction {
        offset: usize,
        sig: Signature,
        span: usize,
    },
    TouchStack {
        count: usize,
        span: usize,
    },
    PushTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    PopTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    CopyToTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    CopyFromTemp {
        stack: TempStack,
        offset: usize,
        count: usize,
        span: usize,
    },
    DropTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    SetOutputComment {
        i: usize,
        n: usize,
    },
    PushSig(Signature),
    PopSig,
}

/// A type of temporary stacks
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence, Serialize, Deserialize,
)]
pub enum TempStack {
    /// A stack used to hold values need to undo a function
    Under,
    /// A stack used when inlining some functions
    Inline,
}

impl fmt::Display for TempStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Under => write!(f, "under"),
            Self::Inline => write!(f, "inline"),
        }
    }
}

impl PartialEq for Instr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Push(a), Self::Push(b)) => a == b,
            (Self::BeginArray, Self::BeginArray) => true,
            (Self::EndArray { boxed: a, .. }, Self::EndArray { boxed: b, .. }) => a == b,
            (Self::Prim(a, _), Self::Prim(b, _)) => a == b,
            (Self::ImplPrim(a, _), Self::ImplPrim(b, _)) => a == b,
            (Self::Call(a), Self::Call(b)) => a == b,
            (Self::Format { parts: a, .. }, Self::Format { parts: b, .. }) => a == b,
            (Self::PushFunc(a), Self::PushFunc(b)) => a == b,
            (Self::PushTemp { count: a, .. }, Self::PushTemp { count: b, .. }) => a == b,
            (Self::PopTemp { count: a, .. }, Self::PopTemp { count: b, .. }) => a == b,
            (
                Self::CopyFromTemp {
                    offset: ao,
                    count: ac,
                    ..
                },
                Self::CopyFromTemp {
                    offset: bo,
                    count: bc,
                    ..
                },
            ) => ao == bo && ac == bc,
            (Self::DropTemp { count: a, .. }, Self::DropTemp { count: b, .. }) => a == b,
            (Self::TouchStack { count: a, .. }, Self::TouchStack { count: b, .. }) => a == b,
            _ => false,
        }
    }
}

impl Eq for Instr {}

impl Instr {
    /// Create a new push instruction
    pub fn push(val: impl Into<Value>) -> Self {
        let val = val.into();
        Self::Push(val)
    }
    pub(crate) fn is_temp(&self) -> bool {
        matches!(
            self,
            Self::PushTemp { .. }
                | Self::PopTemp { .. }
                | Self::CopyFromTemp { .. }
                | Self::DropTemp { .. }
        )
    }
    pub(crate) fn is_compile_only(&self) -> bool {
        matches!(self, Self::PushSig(_) | Self::PopSig)
    }
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => {
                if val.element_count() < 50 && val.shape().len() <= 1 {
                    write!(f, "push {val:?}")
                } else {
                    write!(f, "push {} array", val.shape())
                }
            }
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Comment(comment) => write!(f, "# {comment}"),
            Instr::Push(val) => write!(f, "{val:?}"),
            Instr::CallGlobal { index, .. } => write!(f, "<call global {index}>"),
            Instr::BindGlobal { index, .. } => write!(f, "<bind global {index}>"),
            Instr::BeginArray => write!(f, "begin array"),
            Instr::EndArray { boxed: false, .. } => write!(f, "end array"),
            Instr::EndArray { boxed: true, .. } => write!(f, "end box array"),
            Instr::Prim(prim @ Primitive::Over, _) => write!(f, "`{prim}`"),
            Instr::Prim(prim, _) => write!(f, "{prim}"),
            Instr::ImplPrim(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, "call"),
            Instr::PushFunc(func) => write!(f, "push({func})"),
            Instr::Switch { count, .. } => write!(f, "<switch {count}>"),
            Instr::PushTempFunctions(count) => write!(f, "<push {count} functions>"),
            Instr::PopTempFunctions(count) => write!(f, "<pop {count} functions>"),
            Instr::GetTempFunction { offset, .. } => write!(f, "<get function at {offset}>"),
            Instr::Format { parts, .. } => {
                write!(f, "$\"")?;
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, "_")?
                    }
                    write!(f, "{part}")?
                }
                write!(f, "\"")
            }
            Instr::Label { label, .. } => write!(f, "${label}"),
            Instr::Dynamic(df) => write!(f, "{df:?}"),
            Instr::PushLocals { count, .. } => write!(f, "<push {count} locals>"),
            Instr::PopLocals => write!(f, "<pop locals>"),
            Instr::GetLocal { index, .. } => write!(f, "<get local {index}>"),
            Instr::Unpack {
                count,
                unbox: false,
                ..
            } => write!(f, "<unpack {count}>"),
            Instr::Unpack {
                count, unbox: true, ..
            } => write!(f, "<unpack (unbox) {count}>"),
            Instr::TouchStack { count, .. } => write!(f, "<touch {count}>"),
            Instr::PushTemp { stack, count, .. } => write!(f, "<push {stack} {count}>"),
            Instr::PopTemp { stack, count, .. } => write!(f, "<pop {stack} {count}>"),
            Instr::CopyFromTemp {
                stack,
                offset,
                count,
                ..
            } => write!(f, "<copy from {stack} {offset}/{count}>"),
            Instr::CopyToTemp { stack, count, .. } => {
                write!(f, "<copy to {stack} {count}>")
            }
            Instr::DropTemp { stack, count, .. } => write!(f, "<drop {stack} {count}>"),
            Instr::SetOutputComment { i, n, .. } => write!(f, "<set output comment {i}({n})>"),
            Instr::PushSig(sig) => write!(f, "{sig}"),
            Instr::PopSig => write!(f, "-|"),
        }
    }
}

/// A Uiua function
#[derive(Clone, Serialize_tuple, Deserialize_tuple)]
pub struct Function {
    /// The function's id
    pub id: FunctionId,
    signature: Signature,
    pub(crate) slice: FuncSlice,
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
        self.slice == other.slice
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
    pub(crate) fn new(id: FunctionId, signature: Signature, slice: FuncSlice) -> Self {
        Self {
            id,
            slice,
            signature,
        }
    }
    /// Get how many arguments this function pops off the stack and how many it pushes.
    /// Returns `None` if either of these values are dynamic.
    pub fn signature(&self) -> Signature {
        self.signature
    }
    /// Get the address of function's instructions
    pub fn slice(&self) -> FuncSlice {
        self.slice
    }
    /// Get the function's instructions
    pub fn instrs<'a>(&self, env: &'a impl AsRef<Assembly>) -> &'a [Instr] {
        env.as_ref().instrs(self.slice)
    }
    /// Get a mutable slice of the function's instructions
    pub fn instrs_mut<'a>(&self, env: &'a mut impl AsMut<Assembly>) -> &'a mut [Instr] {
        env.as_mut().instrs_mut(self.slice)
    }
    /// Try to get a lone primitive from this function
    pub fn as_primitive(&self, env: &impl AsRef<Assembly>) -> Option<(Primitive, usize)> {
        match self.instrs(env.as_ref()) {
            [Instr::Prim(prim, span)] => Some((*prim, *span)),
            _ => None,
        }
    }
    pub(crate) fn as_flipped_primitive(
        &self,
        env: &impl AsRef<Assembly>,
    ) -> Option<(Primitive, bool)> {
        match &self.id {
            FunctionId::Primitive(prim) => Some((*prim, false)),
            _ => match self.instrs(env.as_ref()) {
                [Instr::Prim(prim, _)] => Some((*prim, false)),
                [Instr::Prim(Primitive::Flip, _), Instr::Prim(prim, _)] => Some((*prim, true)),
                _ => None,
            },
        }
    }
    pub(crate) fn as_flipped_impl_primitive(
        &self,
        env: &impl AsRef<Assembly>,
    ) -> Option<(ImplPrimitive, bool)> {
        match self.instrs(env.as_ref()) {
            [Instr::ImplPrim(prim, _)] => Some((*prim, false)),
            [Instr::Prim(Primitive::Flip, _), Instr::ImplPrim(prim, _)] => Some((*prim, true)),
            _ => None,
        }
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
            FunctionId::Anonymous(span) => write!(f, "fn from {span}"),
            FunctionId::Primitive(prim) => write!(f, "{prim}"),
            FunctionId::Main => write!(f, "main"),
            FunctionId::Unnamed => write!(f, "unnamed"),
        }
    }
}
