use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign, BitAnd, BitOr, BitOrAssign},
};

use ecow::{eco_vec, EcoString, EcoVec};
use enum_iterator::Sequence;
use serde::*;
use serde_tuple::*;

use crate::{
    check::instrs_signature,
    lex::CodeSpan,
    primitive::{ImplPrimitive, Primitive},
    value::Value,
    Assembly, BindingKind, Ident, NewFunction,
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
    /// Call a recursive function
    CallRecursive(usize),
    /// Recur
    Recur(usize),
    /// Push a function onto the function stack
    PushFunc(Function),
    /// Execute a switch
    Switch {
        count: usize,
        sig: Signature,
        span: usize,
        under_cond: bool,
    },
    /// Do a format string
    Format {
        parts: EcoVec<EcoString>,
        span: usize,
    },
    /// Match a format string pattern
    MatchFormatPattern {
        parts: EcoVec<EcoString>,
        span: usize,
    },
    /// Label an array
    Label {
        label: EcoString,
        span: usize,
        remove: bool,
    },
    /// Validate a field type
    ValidateType {
        index: usize,
        name: EcoString,
        type_num: u8,
        span: usize,
    },
    /// Call a dynamic function
    Dynamic(DynamicFunction),
    Unpack {
        count: usize,
        span: usize,
        unbox: bool,
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
    #[serde(rename = "u")]
    Under,
    /// A stack used when inlining some functions
    #[serde(rename = "i")]
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
        // Comparison ignores spans
        match (self, other) {
            (Self::Comment(a), Self::Comment(b)) => a == b,
            (Self::Push(a), Self::Push(b)) => a == b,
            (
                Self::CallGlobal {
                    index: index_a,
                    call: call_a,
                    sig: sig_a,
                },
                Self::CallGlobal {
                    index: index_b,
                    call: call_b,
                    sig: sig_b,
                },
            ) => index_a == index_b && call_a == call_b && sig_a == sig_b,
            (Self::BindGlobal { index: a, .. }, Self::BindGlobal { index: b, .. }) => a == b,
            (Self::BeginArray, Self::BeginArray) => true,
            (Self::EndArray { boxed: a, .. }, Self::EndArray { boxed: b, .. }) => a == b,
            (Self::Prim(a, _), Self::Prim(b, _)) => a == b,
            (Self::ImplPrim(a, _), Self::ImplPrim(b, _)) => a == b,
            (Self::Call(_), Self::Call(_)) => true,
            (Self::CallRecursive(_), Self::CallRecursive(_)) => true,
            (Self::Recur(_), Self::Recur(_)) => true,
            (Self::PushFunc(a), Self::PushFunc(b)) => a == b,
            (
                Self::Switch {
                    count: count_a,
                    sig: sig_a,
                    under_cond: under_cond_a,
                    ..
                },
                Self::Switch {
                    count: count_b,
                    sig: sig_b,
                    under_cond: under_cond_b,
                    ..
                },
            ) => count_a == count_b && sig_a == sig_b && under_cond_a == under_cond_b,
            (Self::Format { parts: a, .. }, Self::Format { parts: b, .. }) => a == b,
            (
                Self::MatchFormatPattern { parts: a, .. },
                Self::MatchFormatPattern { parts: b, .. },
            ) => a == b,
            (Self::Label { label: a, .. }, Self::Label { label: b, .. }) => a == b,
            (
                Self::ValidateType {
                    index: index_a,
                    type_num: type_num_a,
                    name: name_a,
                    ..
                },
                Self::ValidateType {
                    index: index_b,
                    type_num: type_num_b,
                    name: name_b,
                    ..
                },
            ) => index_a == index_b && type_num_a == type_num_b && name_a == name_b,
            (Self::Dynamic(a), Self::Dynamic(b)) => a == b,
            (
                Self::Unpack {
                    count: count_a,
                    unbox: unbox_a,
                    ..
                },
                Self::Unpack {
                    count: count_b,
                    unbox: unbox_b,
                    ..
                },
            ) => count_a == count_b && unbox_a == unbox_b,
            (Self::TouchStack { count: count_a, .. }, Self::TouchStack { count: count_b, .. }) => {
                count_a == count_b
            }
            (
                Self::PushTemp {
                    stack: stack_a,
                    count: count_a,
                    ..
                },
                Self::PushTemp {
                    stack: stack_b,
                    count: count_b,
                    ..
                },
            ) => stack_a == stack_b && count_a == count_b,
            (
                Self::PopTemp {
                    stack: stack_a,
                    count: count_a,
                    ..
                },
                Self::PopTemp {
                    stack: stack_b,
                    count: count_b,
                    ..
                },
            ) => stack_a == stack_b && count_a == count_b,
            (
                Self::CopyToTemp {
                    stack: stack_a,
                    count: count_a,
                    ..
                },
                Self::CopyToTemp {
                    stack: stack_b,
                    count: count_b,
                    ..
                },
            ) => stack_a == stack_b && count_a == count_b,
            (
                Self::SetOutputComment { i: i_a, n: n_a, .. },
                Self::SetOutputComment { i: i_b, n: n_b, .. },
            ) => i_a == i_b && n_a == n_b,
            (Self::PushSig(sig_a), Self::PushSig(sig_b)) => sig_a == sig_b,
            (Self::PopSig, Self::PopSig) => true,
            _ => false,
        }
    }
}

impl Eq for Instr {}

impl Hash for Instr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hashing ignores spans
        match self {
            Instr::Comment(comment) => (0, comment).hash(state),
            Instr::Push(val) => (1, val).hash(state),
            Instr::CallGlobal { index, call, sig } => (2, index, call, sig).hash(state),
            Instr::BindGlobal { index, .. } => (3, index).hash(state),
            Instr::BeginArray => (4).hash(state),
            Instr::EndArray { boxed, .. } => (5, boxed).hash(state),
            Instr::Prim(prim, _) => (6, prim).hash(state),
            Instr::ImplPrim(prim, _) => (7, prim).hash(state),
            Instr::Call(_) => 8.hash(state),
            Instr::CallRecursive(_) => 9.hash(state),
            Instr::Recur(_) => 10.hash(state),
            Instr::PushFunc(func) => (11, func).hash(state),
            Instr::Switch {
                count,
                sig,
                under_cond,
                ..
            } => (12, count, sig, under_cond).hash(state),
            Instr::Format { parts, .. } => (13, parts).hash(state),
            Instr::MatchFormatPattern { parts, .. } => (14, parts).hash(state),
            Instr::Label { label, .. } => (16, label).hash(state),
            Instr::ValidateType {
                index,
                type_num,
                name,
                ..
            } => (17, index, type_num, name).hash(state),
            Instr::Dynamic(df) => (18, df).hash(state),
            Instr::Unpack { count, unbox, .. } => (19, count, unbox).hash(state),
            Instr::TouchStack { count, .. } => (20, count).hash(state),
            Instr::PushTemp { stack, count, .. } => (21, stack, count).hash(state),
            Instr::PopTemp { stack, count, .. } => (22, stack, count).hash(state),
            Instr::CopyToTemp { stack, count, .. } => (23, stack, count).hash(state),
            Instr::SetOutputComment { i, n, .. } => (24, i, n).hash(state),
            Instr::PushSig(sig) => (25, sig).hash(state),
            Instr::PopSig => 26.hash(state),
        }
    }
}

impl Instr {
    /// Create a new push instruction
    pub fn push(val: impl Into<Value>) -> Self {
        let val = val.into();
        Self::Push(val)
    }
    pub(crate) fn push_inline(count: usize, span: usize) -> Self {
        Self::PushTemp {
            stack: TempStack::Inline,
            count,
            span,
        }
    }
    pub(crate) fn pop_inline(count: usize, span: usize) -> Self {
        Self::PopTemp {
            stack: TempStack::Inline,
            count,
            span,
        }
    }
    pub(crate) fn copy_inline(span: usize) -> Self {
        Self::CopyToTemp {
            stack: TempStack::Inline,
            count: 1,
            span,
        }
    }
    pub(crate) fn is_compile_only(&self) -> bool {
        matches!(self, Self::PushSig(_) | Self::PopSig)
    }
    #[allow(dead_code)]
    pub(crate) fn span(&self) -> Option<usize> {
        self.span_impl().copied()
    }
}

macro_rules! instr_span {
    ($name:ident, $self_ty:ty, $out_ty:ty) => {
        impl Instr {
            #[allow(dead_code)]
            pub(crate) fn $name(self: $self_ty) -> Option<$out_ty> {
                Some(match self {
                    Self::BindGlobal { span, .. } => span,
                    Self::EndArray { span, .. } => span,
                    Self::Call(span) => span,
                    Self::Prim(_, span) => span,
                    Self::ImplPrim(_, span) => span,
                    Self::CallRecursive(span) => span,
                    Self::Recur(span) => span,
                    Self::Switch { span, .. } => span,
                    Self::Format { span, .. } => span,
                    Self::MatchFormatPattern { span, .. } => span,
                    Self::PushTemp { span, .. } => span,
                    Self::PopTemp { span, .. } => span,
                    Self::CopyToTemp { span, .. } => span,
                    Self::Label { span, .. } => span,
                    Self::ValidateType { span, .. } => span,
                    Self::Unpack { span, .. } => span,
                    Self::TouchStack { span, .. } => span,
                    _ => return None,
                })
            }
        }
    };
}

instr_span!(span_impl, &Self, &usize);
instr_span!(span_mut, &mut Self, &mut usize);

pub(crate) struct FmtInstrs<'a>(pub &'a [Instr], pub &'a Assembly);
impl<'a> fmt::Debug for FmtInstrs<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, instr) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            match instr {
                Instr::PushFunc(func) => {
                    FmtInstrs(func.instrs(self.1), self.1).fmt(f)?;
                }
                instr => instr.fmt(f)?,
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}
impl<'a> fmt::Display for FmtInstrs<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, instr) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            match instr {
                Instr::PushFunc(func) => FmtInstrs(func.instrs(self.1), self.1).fmt(f),
                instr => instr.fmt(f),
            }?
        }
        Ok(())
    }
}

/// Levels of purity for an operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Purity {
    /// The operation visibly affects the environment
    Mutating,
    /// The operation reads from the environment but does not visibly affect it
    Impure,
    /// The operation is completely pure
    Pure,
}

/// Whether some instructions are pure
pub(crate) fn instrs_are_pure(instrs: &[Instr], asm: &Assembly, min_purity: Purity) -> bool {
    instrs_are_pure_impl(instrs, asm, min_purity, &mut HashSet::new())
}
fn instrs_are_pure_impl<'a>(
    instrs: &'a [Instr],
    asm: &'a Assembly,
    min_purity: Purity,
    visited: &mut HashSet<&'a FunctionId>,
) -> bool {
    'instrs: for (i, instr) in instrs.iter().enumerate() {
        match instr {
            Instr::CallGlobal { index, .. } => {
                if let Some(binding) = asm.bindings.get(*index) {
                    match &binding.kind {
                        BindingKind::Const(Some(_)) => {}
                        BindingKind::Func(f) => {
                            if visited.insert(&f.id)
                                && !instrs_are_pure_impl(f.instrs(asm), asm, min_purity, visited)
                            {
                                return false;
                            }
                        }
                        _ => return false,
                    }
                }
            }
            Instr::BindGlobal { .. } => {
                let prev = &instrs[..i];
                for j in (0..i).rev() {
                    let frag = &prev[j..i];
                    if instrs_signature(frag).is_ok_and(|sig| sig == (0, 1))
                        && instrs_are_pure_impl(frag, asm, min_purity, visited)
                    {
                        continue 'instrs;
                    }
                }
                return false;
            }
            Instr::Prim(prim, _) => {
                if prim.purity() < min_purity {
                    return false;
                }
            }
            Instr::ImplPrim(prim, _) => {
                if prim.purity() < min_purity {
                    return false;
                }
            }
            Instr::PushFunc(f) => {
                if visited.insert(&f.id)
                    && !instrs_are_pure_impl(f.instrs(asm), asm, min_purity, visited)
                {
                    return false;
                }
            }
            Instr::Dynamic(_) => return false,
            Instr::SetOutputComment { .. } => return false,
            _ => {}
        }
    }
    true
}

/// Whether some instructions can be propertly bounded by the runtime execution limit
pub(crate) fn instrs_are_limit_bounded(instrs: &[Instr], asm: &Assembly) -> bool {
    use Primitive::*;
    for instr in instrs {
        match instr {
            Instr::CallGlobal { index, .. } => {
                if let Some(binding) = asm.bindings.get(*index) {
                    match &binding.kind {
                        BindingKind::Const(Some(_)) => {}
                        BindingKind::Func(f) => {
                            if !instrs_are_limit_bounded(f.instrs(asm), asm) {
                                return false;
                            }
                        }
                        _ => return false,
                    }
                }
            }
            Instr::Prim(Send | Recv, _) => return false,
            Instr::Prim(Sys(op), _) if op.purity() <= Purity::Mutating => return false,
            Instr::PushFunc(f) => {
                if f.is_recursive() || !instrs_are_limit_bounded(f.instrs(asm), asm) {
                    return false;
                }
            }
            Instr::Dynamic(_) => return false,
            _ => {}
        }
    }
    true
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
            Instr::CallRecursive(_) => write!(f, "call recursive"),
            Instr::Recur(_) => write!(f, "recur"),
            Instr::PushFunc(func) => write!(f, "push({func})"),
            Instr::Switch { count, .. } => write!(f, "<switch {count}>"),
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
            Instr::MatchFormatPattern { parts, .. } => {
                write!(f, "°$\"")?;
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, "_")?
                    }
                    write!(f, "{part}")?
                }
                write!(f, "\"")
            }
            Instr::Label { label, .. } => write!(f, "${label}"),
            Instr::ValidateType { name, type_num, .. } => {
                write!(f, "<validate {name} as {type_num}>")
            }
            Instr::Dynamic(df) => write!(f, "{df:?}"),
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
            Instr::CopyToTemp { stack, count, .. } => {
                write!(f, "<copy to {stack} {count}>")
            }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
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
    /// Get the inverse of this signature
    pub fn inverse(self) -> Self {
        Self::new(self.outputs, self.args)
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
    Macro(CodeSpan),
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
            FunctionId::Macro(span) => Some(span),
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
            FunctionId::Macro(_) => write!(f, "macro expansion"),
            FunctionId::Main => write!(f, "main"),
            FunctionId::Unnamed => write!(f, "unnamed"),
        }
    }
}
