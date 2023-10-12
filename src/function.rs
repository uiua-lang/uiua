use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    mem::{discriminant, transmute},
    sync::Arc,
};

use crate::{
    check::instrs_signature, grid_fmt::GridFmt, lex::CodeSpan, primitive::Primitive, value::Value,
    Ident, Uiua, UiuaResult,
};

#[derive(Clone)]
#[repr(u8)]
pub enum Instr {
    Push(Box<Value>) = 0,
    BeginArray,
    EndArray {
        boxed: bool,
        span: usize,
    },
    Prim(Primitive, usize),
    Call(usize),
    Dynamic(DynamicFunction),
    PushTempUnder {
        count: usize,
        span: usize,
    },
    PopTempUnder {
        count: usize,
        span: usize,
    },
    PushTempInline {
        count: usize,
        span: usize,
    },
    PopTempInline {
        count: usize,
        span: usize,
    },
    CopyTempInline {
        offset: usize,
        count: usize,
        span: usize,
    },
    DropTempInline {
        count: usize,
        span: usize,
    },
}

impl PartialEq for Instr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Push(a), Self::Push(b)) => a == b,
            (Self::BeginArray, Self::BeginArray) => true,
            (Self::EndArray { .. }, Self::EndArray { .. }) => true,
            (Self::Prim(a, s_span), Self::Prim(b, b_span)) => a == b && s_span == b_span,
            (Self::Call(a), Self::Call(b)) => a == b,
            (Self::PushTempUnder { count: a, .. }, Self::PushTempUnder { count: b, .. }) => a == b,
            (Self::PopTempUnder { count: a, .. }, Self::PopTempUnder { count: b, .. }) => a == b,
            (Self::PushTempInline { count: a, .. }, Self::PushTempInline { count: b, .. }) => {
                a == b
            }
            (Self::PopTempInline { count: a, .. }, Self::PopTempInline { count: b, .. }) => a == b,
            (
                Self::CopyTempInline {
                    offset: ao,
                    count: ac,
                    ..
                },
                Self::CopyTempInline {
                    offset: bo,
                    count: bc,
                    ..
                },
            ) => ao == bo && ac == bc,
            (Self::DropTempInline { count: a, .. }, Self::DropTempInline { count: b, .. }) => {
                a == b
            }
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
            Instr::BeginArray => 1u8.hash(state),
            Instr::EndArray { .. } => 2u8.hash(state),
            Instr::Prim(p, _) => p.hash(state),
            Instr::Call(_) => {}
            Instr::Dynamic(f) => f.id.hash(state),
            Instr::PushTempUnder { count, .. } => count.hash(state),
            Instr::PopTempUnder { count, .. } => count.hash(state),
            Instr::PushTempInline { count, .. } => count.hash(state),
            Instr::PopTempInline { count, .. } => count.hash(state),
            Instr::CopyTempInline { offset, count, .. } => {
                offset.hash(state);
                count.hash(state);
            }
            Instr::DropTempInline { count, .. } => count.hash(state),
        }
    }
}

impl Instr {
    pub fn push(val: impl Into<Value>) -> Self {
        Self::Push(Box::new(val.into()))
    }
    pub fn as_push(&self) -> Option<&Value> {
        match self {
            Instr::Push(val) => Some(val),
            _ => None,
        }
    }
    pub fn is_temp(&self) -> bool {
        matches!(
            self,
            Self::PushTempUnder { .. }
                | Self::PopTempUnder { .. }
                | Self::PushTempInline { .. }
                | Self::PopTempInline { .. }
                | Self::CopyTempInline { .. }
                | Self::DropTempInline { .. }
        )
    }
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => {
                if val.flat_len() < 50 && val.shape().len() <= 1 {
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
            Instr::BeginArray => write!(f, "]"),
            Instr::EndArray { .. } => write!(f, "["),
            Instr::Prim(prim @ Primitive::Over, _) => write!(f, "`{prim}`"),
            Instr::Prim(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, "!"),
            Instr::Dynamic(df) => write!(f, "{df:?}"),
            Instr::PushTempUnder { count, .. } => write!(f, "<push under {count}>"),
            Instr::PopTempUnder { count, .. } => write!(f, "<pop under {count}>"),
            Instr::PushTempInline { count, .. } => write!(f, "<push inline {count}>"),
            Instr::PopTempInline { count, .. } => write!(f, "<pop inline {count}>"),
            Instr::CopyTempInline { offset, count, .. } => {
                write!(f, "<copy inline {offset}/{count}>")
            }
            Instr::DropTempInline { count, .. } => write!(f, "<drop inline {count}>"),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub id: FunctionId,
    pub instrs: Vec<Instr>,
    signature: Signature,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub args: usize,
    pub outputs: usize,
}

impl Signature {
    pub const fn new(args: usize, outputs: usize) -> Self {
        Self { args, outputs }
    }
    /// Check if this signature changes the stack size by the same amount as another signature.
    pub fn is_compatible_with(self, other: Self) -> bool {
        self.args as isize - self.outputs as isize == other.args as isize - other.outputs as isize
    }
    /// Check if this [`Signature::is_compatible_with`] another signature and has at least as many arguments.
    pub fn is_superset_of(self, other: Self) -> bool {
        self.is_compatible_with(other) && self.args >= other.args
    }
    /// Check if this [`Signature::is_compatible_with`] another signature and has at most as many arguments.
    pub fn is_subset_of(self, other: Self) -> bool {
        self.is_compatible_with(other) && self.args <= other.args
    }
    pub fn max_with(self, other: Self) -> Self {
        Self::new(self.args.max(other.args), self.outputs.max(other.outputs))
    }
    pub fn compose(self, other: Self) -> Self {
        Self::new(
            other.args + self.args.saturating_sub(other.outputs),
            other.outputs.saturating_sub(other.args) + self.outputs,
        )
    }
}

impl PartialEq<(usize, usize)> for Signature {
    fn eq(&self, other: &(usize, usize)) -> bool {
        self.args == other.0 && self.outputs == other.1
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "|{}.{}", self.args, self.outputs)
    }
}

#[derive(Clone)]
pub struct DynamicFunction {
    pub id: u64,
    pub f: Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync>,
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
        write!(f, "{}", self.grid_string())
    }
}

impl Function {
    pub fn new(id: FunctionId, instrs: impl Into<Vec<Instr>>, signature: Signature) -> Self {
        let instrs = instrs.into();
        Self {
            id,
            instrs,
            signature,
        }
    }
    pub fn new_inferred(id: FunctionId, instrs: impl Into<Vec<Instr>>) -> Result<Self, String> {
        let instrs = instrs.into();
        let signature = instrs_signature(&instrs)?;
        Ok(Self {
            id,
            signature,
            instrs,
        })
    }
    pub fn into_inner(f: Arc<Self>) -> Self {
        Arc::try_unwrap(f).unwrap_or_else(|f| (*f).clone())
    }
    pub(crate) fn format_inner(&self) -> Vec<String> {
        if let FunctionId::Named(name) = &self.id {
            return vec![name.as_ref().into()];
        }
        if let Some((prim, _)) = self.as_primitive() {
            return vec![prim.to_string()];
        }
        let mut lines = vec![String::new()];
        for (i, instr) in self.instrs.iter().rev().enumerate() {
            let instr_str = instr.to_string();
            let s = &lines[0];
            let add_space = (s.ends_with(char::is_alphabetic)
                && instr_str.starts_with(char::is_alphabetic))
                || (s.ends_with(|c: char| c.is_ascii_digit())
                    && instr_str.starts_with(|c: char| c.is_ascii_digit()));
            if lines.len() < instr_str.lines().count() {
                lines.resize(instr_str.lines().count(), String::new());
            }
            let max_line_len = lines.iter().map(|s| s.chars().count()).max().unwrap_or(0);
            for line in &mut lines {
                line.extend(std::iter::repeat(' ').take(max_line_len - line.chars().count()));
            }
            if i > 0 && add_space {
                lines[0].push(' ');
            }
            for (line, instr_line) in lines.iter_mut().zip(instr_str.lines()) {
                line.push_str(instr_line);
            }
        }
        lines
    }
    /// Get how many arguments this function pops off the stack and how many it pushes.
    /// Returns `None` if either of these values are dynamic.
    pub fn signature(&self) -> Signature {
        self.signature
    }
    pub fn is_constant(&self) -> bool {
        matches!(&*self.instrs, [Instr::Push(_)])
    }
    pub fn constant(value: impl Into<Value>) -> Self {
        Function::new(
            FunctionId::Constant,
            [Instr::push(value.into())],
            Signature::new(0, 1),
        )
    }
    pub fn as_primitive(&self) -> Option<(Primitive, usize)> {
        match self.instrs.as_slice() {
            [Instr::Prim(prim, span)] => Some((*prim, *span)),
            _ => None,
        }
    }
    pub fn into_unboxed(self) -> Result<Value, Self> {
        if self.is_constant() {
            if let Instr::Push(val) = self.instrs.into_iter().next().unwrap() {
                Ok(*val)
            } else {
                unreachable!();
            }
        } else {
            Err(self)
        }
    }
    pub fn as_boxed(&self) -> Option<&Value> {
        match self.instrs.as_slice() {
            [Instr::Push(val)] => Some(val),
            _ => None,
        }
    }
    pub fn as_boxed_mut(&mut self) -> Option<&mut Value> {
        match self.instrs.as_mut_slice() {
            [Instr::Push(val)] => Some(val),
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
    pub fn compose(a: Arc<Self>, b: Arc<Self>) -> Self {
        let id = a.id.clone().compose(b.id.clone());
        let sig = a.signature.compose(b.signature);
        let mut instrs = b.instrs.clone();
        instrs.extend(a.instrs.iter().cloned());
        Self::new(id, instrs, sig)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(CodeSpan),
    Primitive(Primitive),
    Constant,
    Main,
    Composed(Vec<Self>),
}

impl FunctionId {
    pub fn compose(self, other: Self) -> Self {
        match (self, other) {
            (FunctionId::Composed(mut a), FunctionId::Composed(b)) => {
                a.extend(b);
                FunctionId::Composed(a)
            }
            (FunctionId::Composed(mut a), b) => {
                a.push(b);
                FunctionId::Composed(a)
            }
            (a, FunctionId::Composed(mut b)) => {
                b.insert(0, a);
                FunctionId::Composed(b)
            }
            (a, b) => FunctionId::Composed(vec![a, b]),
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

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionId::Named(name) => write!(f, "`{name}`"),
            FunctionId::Anonymous(span) => write!(f, "fn from {span}"),
            FunctionId::Primitive(prim) => write!(f, "{prim}"),
            FunctionId::Constant => write!(f, "constant"),
            FunctionId::Main => write!(f, "main"),
            FunctionId::Composed(ids) => write!(f, "{ids:?}"),
        }
    }
}
