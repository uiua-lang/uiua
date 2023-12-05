use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    mem::{discriminant, transmute},
    ops::ControlFlow,
    sync::Arc,
};

use ecow::EcoVec;
use enum_iterator::Sequence;

use crate::{
    check::{instrs_signature, SigCheckError},
    lex::CodeSpan,
    optimize::optimize_instrs,
    primitive::{ImplPrimitive, Primitive},
    value::Value,
    Ident, Uiua, UiuaResult,
};

/// A Uiua bytecode instruction
#[derive(Clone)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum Instr {
    /// Push a value onto the stack
    Push(Value) = 0,
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
    /// Call a dynamic function
    Dynamic(DynamicFunction),
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
    PushSig(Signature),
    PopSig,
}

/// A type of temporary stacks
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
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
            (Self::EndArray { .. }, Self::EndArray { .. }) => true,
            (Self::Prim(a, s_span), Self::Prim(b, b_span)) => a == b && s_span == b_span,
            (Self::Call(a), Self::Call(b)) => a == b,
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
            _ => false,
        }
    }
}

impl Eq for Instr {}

impl PartialOrd for Instr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Instr {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Push(a), Self::Push(b)) => a.cmp(b),
            (a, b) => {
                if a == b {
                    Ordering::Equal
                } else {
                    let a: u8 = unsafe { transmute(discriminant(a)) };
                    let b: u8 = unsafe { transmute(discriminant(b)) };
                    a.cmp(&b)
                }
            }
        }
    }
}

impl Hash for Instr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let disc: u8 = unsafe { transmute(discriminant(self)) };
        disc.hash(state);
        match self {
            Instr::Push(val) => val.hash(state),
            Instr::BeginArray => {}
            Instr::EndArray { .. } => {}
            Instr::Prim(p, _) => p.hash(state),
            Instr::ImplPrim(p, _) => p.hash(state),
            Instr::Call(_) => {}
            Instr::PushFunc(f) => f.id.hash(state),
            Instr::Switch { count, sig, .. } => {
                count.hash(state);
                sig.hash(state);
            }
            Instr::Unpack { count, .. } => count.hash(state),
            Instr::PushTempFunctions(count) => count.hash(state),
            Instr::PopTempFunctions(count) => count.hash(state),
            Instr::GetTempFunction { offset, .. } => offset.hash(state),
            Instr::Dynamic(f) => f.id.hash(state),
            Instr::TouchStack { count, .. } => count.hash(state),
            Instr::PushTemp { count, .. } => count.hash(state),
            Instr::PopTemp { count, .. } => count.hash(state),
            Instr::CopyToTemp { count, .. } => count.hash(state),
            Instr::CopyFromTemp { offset, count, .. } => {
                offset.hash(state);
                count.hash(state);
            }
            Instr::DropTemp { count, .. } => count.hash(state),
            Instr::PushSig(_) | Instr::PopSig => {}
        }
    }
}

impl Instr {
    /// Create a new push instruction
    pub fn push(val: impl Into<Value>) -> Self {
        Self::Push(val.into())
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
                    write!(f, "push {} array", val.format_shape())
                }
            }
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => write!(f, "{val:?}"),
            Instr::BeginArray => write!(f, "begin array"),
            Instr::EndArray { .. } => write!(f, "end array"),
            Instr::Prim(prim @ Primitive::Over, _) => write!(f, "`{prim}`"),
            Instr::Prim(prim, _) => write!(f, "{prim}"),
            Instr::ImplPrim(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, "call"),
            Instr::PushFunc(func) => write!(f, "push({func})"),
            Instr::Switch { count, .. } => write!(f, "<switch {count}>"),
            Instr::PushTempFunctions(count) => write!(f, "<push {count} functions>"),
            Instr::PopTempFunctions(count) => write!(f, "<pop {count} functions>"),
            Instr::GetTempFunction { offset, .. } => write!(f, "<get function at {offset}>"),
            Instr::Dynamic(df) => write!(f, "{df:?}"),
            Instr::Unpack { count, .. } => write!(f, "<unpack {count}>"),
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
            Instr::PushSig(sig) => write!(f, "<push {sig}>"),
            Instr::PopSig => write!(f, "<pop sig>"),
        }
    }
}

/// A Uiua function
#[derive(Clone)]
pub struct Function {
    /// The function's id
    pub id: FunctionId,
    /// The function's instructions
    pub instrs: EcoVec<Instr>,
    signature: Signature,
}

/// A function stack signature
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    /// The number of arguments the function pops off the stack
    pub args: usize,
    /// The number of values the function pushes onto the stack
    pub outputs: usize,
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

impl From<(usize, usize)> for Signature {
    fn from((args, outputs): (usize, usize)) -> Self {
        Self::new(args, outputs)
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
#[derive(Clone)]
pub struct DynamicFunction {
    /// An id used for hashing and equality
    pub id: u64,
    /// The function
    pub f: Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync>,
    /// The function's signature
    pub signature: Signature,
}

impl fmt::Debug for DynamicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<dynamic#{:x}>", self.id)
    }
}

impl PartialEq for DynamicFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for DynamicFunction {}

impl PartialOrd for DynamicFunction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DynamicFunction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl Hash for DynamicFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.instrs == other.instrs
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
        self.id
            .cmp(&other.id)
            .then_with(|| self.instrs.cmp(&other.instrs))
    }
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.instrs.hash(state);
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let FunctionId::Named(name) = &self.id {
            return write!(f, "{name}");
        }
        if let Some((prim, _)) = self.as_primitive() {
            return write!(f, "{prim}");
        }
        write!(f, "<function>")
    }
}

impl Function {
    /// Create a new function
    pub fn new<I>(id: FunctionId, instrs: I, signature: Signature) -> Self
    where
        I: IntoIterator<Item = Instr>,
        I::IntoIter: ExactSizeIterator,
    {
        let instrs = optimize_instrs(instrs, true);
        Self {
            id,
            instrs,
            signature,
        }
    }
    /// Create a new function and infer its signature
    pub fn new_inferred(
        id: FunctionId,
        instrs: impl IntoIterator<Item = Instr>,
    ) -> Result<Self, SigCheckError> {
        let instrs: Vec<_> = instrs.into_iter().collect();
        let signature = instrs_signature(&instrs)?;
        let instrs = optimize_instrs(instrs, true);
        Ok(Self {
            id,
            signature,
            instrs,
        })
    }
    /// Get how many arguments this function pops off the stack and how many it pushes.
    /// Returns `None` if either of these values are dynamic.
    pub fn signature(&self) -> Signature {
        self.signature
    }
    /// Try to get a lone primitive from this function
    pub fn as_primitive(&self) -> Option<(Primitive, usize)> {
        match self.instrs.as_slice() {
            [Instr::Prim(prim, span)] => Some((*prim, *span)),
            _ => None,
        }
    }
    pub(crate) fn as_flipped_primitive(&self) -> Option<(Primitive, bool)> {
        match &self.id {
            FunctionId::Primitive(prim) => Some((*prim, false)),
            _ => match self.instrs.as_slice() {
                [Instr::Prim(prim, _)] => Some((*prim, false)),
                [Instr::Prim(Primitive::Flip, _), Instr::Prim(prim, _)] => Some((*prim, true)),
                _ => None,
            },
        }
    }
    /// `invert` this function
    pub fn invert(&self, context: &str, env: &Uiua) -> UiuaResult<Self> {
        self.inverse()
            .ok_or_else(|| env.error(format!("No inverse found{context}")))
    }
    /// `under` this function
    pub fn undered(&self, g_sig: Signature, env: &Uiua) -> UiuaResult<(Self, Self)> {
        self.under(g_sig)
            .ok_or_else(|| env.error("No inverse found"))
    }
    pub(crate) fn recurse_instrs<T>(
        &self,
        mut f: impl FnMut(&Instr) -> ControlFlow<T>,
    ) -> Option<T> {
        recurse_instrs(&self.instrs, &mut f)
    }
}

fn recurse_instrs<T>(instrs: &[Instr], f: &mut impl FnMut(&Instr) -> ControlFlow<T>) -> Option<T> {
    for instr in instrs {
        match instr {
            Instr::PushFunc(func) => {
                if let Some(val) = recurse_instrs(&func.instrs, f) {
                    return Some(val);
                }
            }
            _ => {
                if let ControlFlow::Break(val) = f(instr) {
                    return Some(val);
                }
            }
        }
    }
    None
}

/// A Uiua function id
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    /// A named function
    Named(Ident),
    /// An anonymous function
    Anonymous(CodeSpan),
    /// Just a primitive
    Primitive(Primitive),
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
