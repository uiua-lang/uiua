use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    sync::Arc,
};

use parking_lot::Mutex;

use crate::{
    check::instrs_signature, lex::CodeSpan, primitive::Primitive, value::Value, Ident, Uiua,
    UiuaResult,
};

#[derive(Debug, Clone)]
pub enum Instr {
    Push(Box<Value>),
    BeginArray,
    EndArray { constant: bool, span: usize },
    Prim(Primitive, usize),
    Call(usize),
}

impl PartialEq for Instr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Push(a), Self::Push(b)) => a == b,
            (Self::BeginArray, Self::BeginArray) => true,
            (Self::EndArray { .. }, Self::EndArray { .. }) => true,
            (Self::Prim(a, s_span), Self::Prim(b, b_span)) => a == b && s_span == b_span,
            (Self::Call(a), Self::Call(b)) => a == b,
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
            (Self::BeginArray, Self::BeginArray) => Ordering::Equal,
            (Self::EndArray { .. }, Self::EndArray { .. }) => Ordering::Equal,
            (Self::Prim(a, a_span), Self::Prim(b, b_span)) => {
                a.cmp(b).then_with(|| a_span.cmp(b_span))
            }
            (Self::Call(a), Self::Call(b)) => a.cmp(b),
            (Self::Push(_), _) => Ordering::Less,
            (Self::BeginArray, Self::Push(_)) => Ordering::Greater,
            (Self::BeginArray, _) => Ordering::Less,
            (Self::EndArray { .. }, Self::Push(_) | Self::BeginArray) => Ordering::Greater,
            (Self::EndArray { .. }, _) => Ordering::Less,
            (Self::Prim(_, _), Self::Push(_) | Self::BeginArray | Self::EndArray { .. }) => {
                Ordering::Greater
            }
            (Self::Prim(_, _), _) => Ordering::Less,
            (Self::Call(_), _) => Ordering::Greater,
        }
    }
}

impl Hash for Instr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Instr::Push(val) => {
                0u8.hash(state);
                val.hash(state);
            }
            Instr::BeginArray => 1u8.hash(state),
            Instr::EndArray { .. } => 2u8.hash(state),
            Instr::Prim(p, span) => {
                3u8.hash(state);
                p.hash(state);
                span.hash(state);
            }
            Instr::Call(span) => {
                4u8.hash(state);
                span.hash(state);
            }
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
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => write!(f, "{val:?}"),
            Instr::BeginArray => write!(f, "]"),
            Instr::EndArray { .. } => write!(f, "["),
            Instr::Prim(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, "!"),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub id: FunctionId,
    pub instrs: Vec<Instr>,
    pub kind: FunctionKind,
    // Calculating the signature of a function can be expensive, so we cache it.
    signature_cache: Arc<Mutex<Option<Option<Signature>>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub args: usize,
    pub outputs: usize,
}

impl Signature {
    pub fn new(args: impl Into<usize>, outputs: impl Into<usize>) -> Self {
        Self {
            args: args.into(),
            outputs: outputs.into(),
        }
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "|{}.{}", self.args, self.outputs)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionKind {
    Normal,
    Dynamic(DynamicFunctionKind),
}

#[derive(Clone)]
pub struct DynamicFunctionKind {
    pub f: Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync>,
    pub id: u64,
    pub signature: Signature,
}

impl PartialEq for DynamicFunctionKind {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for DynamicFunctionKind {}

impl PartialOrd for DynamicFunctionKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DynamicFunctionKind {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl Hash for DynamicFunctionKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl From<Primitive> for Function {
    fn from(prim: Primitive) -> Self {
        Self::new(
            FunctionId::Primitive(prim),
            [Instr::Prim(prim, 0)],
            FunctionKind::Normal,
        )
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
        self.kind.hash(state);
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
        if let FunctionKind::Dynamic { .. } = self.kind {
            return write!(f, "<dynamic>");
        }
        write!(f, "(")?;
        write!(f, "{}", self.format_inner())?;
        write!(f, ")")?;
        Ok(())
    }
}

impl Function {
    pub fn new(id: FunctionId, instrs: impl Into<Vec<Instr>>, kind: FunctionKind) -> Self {
        Self {
            id,
            instrs: instrs.into(),
            kind,
            signature_cache: Mutex::new(None).into(),
        }
    }
    pub fn set_signature(&self, sig: Signature) {
        *self.signature_cache.lock() = Some(Some(sig));
    }
    pub fn into_inner(f: Arc<Self>) -> Self {
        Arc::try_unwrap(f).unwrap_or_else(|f| (*f).clone())
    }
    pub(crate) fn format_inner(&self) -> String {
        if let FunctionId::Named(name) = &self.id {
            return name.as_str().into();
        }
        if let Some((prim, _)) = self.as_primitive() {
            return prim.to_string();
        }
        if let FunctionKind::Dynamic { .. } = self.kind {
            return "<dynamic>".into();
        }
        let mut s = String::new();
        for (i, instr) in self.instrs.iter().rev().enumerate() {
            if i > 0 {
                s.push(' ');
            }
            s.push_str(&instr.to_string());
        }
        s
    }
    /// Get how many arguments this function pops off the stack and how many it pushes.
    /// Returns `None` if either of these values are dynamic.
    pub fn signature(&self) -> Option<Signature> {
        *self
            .signature_cache
            .lock()
            .get_or_insert_with(|| match self.kind {
                FunctionKind::Normal => instrs_signature(&self.instrs),
                FunctionKind::Dynamic(DynamicFunctionKind { signature, .. }) => Some(signature),
            })
    }
    pub fn is_constant(&self) -> bool {
        matches!(&*self.instrs, [Instr::Push(_)])
    }
    pub fn constant(value: impl Into<Value>) -> Self {
        Function::new(
            FunctionId::Constant,
            [Instr::push(value.into())],
            FunctionKind::Normal,
        )
    }
    pub fn as_primitive(&self) -> Option<(Primitive, usize)> {
        match self.instrs.as_slice() {
            [Instr::Prim(prim, span)] => Some((*prim, *span)),
            _ => None,
        }
    }
    pub fn into_constant(self) -> Result<Value, Self> {
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
    pub fn as_constant(&self) -> Option<&Value> {
        match self.instrs.as_slice() {
            [Instr::Push(val)] => Some(val),
            _ => None,
        }
    }
    pub fn as_constant_mut(&mut self) -> Option<&mut Value> {
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
    pub fn compose(self, other: Self) -> Self {
        match (&self.kind, &other.kind) {
            (FunctionKind::Dynamic { .. }, _) | (_, FunctionKind::Dynamic { .. }) => Function::new(
                self.id.clone().compose(other.id.clone()),
                [
                    Instr::push(other),
                    Instr::Call(0),
                    Instr::push(self),
                    Instr::Call(0),
                ],
                FunctionKind::Normal,
            ),
            _ => Function::new(
                self.id.compose(other.id),
                {
                    let mut instrs = other.instrs;
                    instrs.extend(self.instrs);
                    instrs
                },
                FunctionKind::Normal,
            ),
        }
    }
    pub fn signature_compatible_with(&self, other: &Self) -> Option<bool> {
        match (self.signature(), other.signature()) {
            (Some(a), Some(b)) => {
                Some(a.args as isize - a.outputs as isize == b.args as isize - b.outputs as isize)
            }
            _ => None,
        }
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
            FunctionId::Named(name) => name == other,
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
