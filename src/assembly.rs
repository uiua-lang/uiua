use std::{collections::HashMap, path::PathBuf, sync::Arc};

use dashmap::DashMap;
use ecow::{eco_vec, EcoString, EcoVec};
use serde::*;

use crate::{
    lex::Sp, CodeSpan, DynamicFunction, FuncSlice, Function, Ident, ImplPrimitive, InputSrc, Instr,
    IntoInputSrc, Primitive, Signature, Span, TempStack, Uiua, UiuaResult, Value,
};

/// A compiled Uiua assembly
#[derive(Clone, Serialize, Deserialize)]
pub struct Assembly {
    pub(crate) instrs: EcoVec<Instr>,
    pub(crate) top_slices: Vec<FuncSlice>,
    /// A list of global bindings
    pub bindings: EcoVec<BindingInfo>,
    #[serde(skip)]
    /// A map of references to global bindings
    pub global_references: HashMap<Sp<Ident>, usize>,
    #[serde(skip)]
    pub(crate) dynamic_functions: EcoVec<DynFn>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub(crate) import_inputs: HashMap<PathBuf, EcoString>,
    pub(crate) spans: EcoVec<Span>,
    pub(crate) inputs: Inputs,
}

type DynFn = Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static>;

impl Default for Assembly {
    fn default() -> Self {
        Self {
            instrs: EcoVec::new(),
            top_slices: Vec::new(),
            import_inputs: HashMap::new(),
            spans: eco_vec![Span::Builtin],
            bindings: EcoVec::new(),
            global_references: HashMap::new(),
            dynamic_functions: EcoVec::new(),
            inputs: Inputs::default(),
        }
    }
}

impl From<&Assembly> for Assembly {
    fn from(asm: &Assembly) -> Self {
        asm.clone()
    }
}

impl Assembly {
    /// Get the instructions of a function slice
    pub fn instrs(&self, slice: FuncSlice) -> &[Instr] {
        &self.instrs[slice.address..][..slice.len]
    }
    /// Get the mutable instructions of a function slice
    pub fn instrs_mut(&mut self, slice: FuncSlice) -> &mut [Instr] {
        &mut self.instrs.make_mut()[slice.address..][..slice.len]
    }
    pub(crate) fn bind_function(
        &mut self,
        index: usize,
        function: Function,
        span: usize,
        comment: Option<Arc<str>>,
    ) {
        let span = self.spans[span].clone();
        self.add_global_at(index, Global::Func(function), span.code(), comment);
    }
    pub(crate) fn bind_sig(
        &mut self,
        index: usize,
        sig: Signature,
        span: usize,
        comment: Option<Arc<str>>,
    ) {
        let span = self.spans[span].clone();
        self.add_global_at(index, Global::Sig(sig), span.code(), comment);
    }
    pub(crate) fn bind_const(
        &mut self,
        index: usize,
        value: Value,
        span: usize,
        comment: Option<Arc<str>>,
    ) {
        let span = self.spans[span].clone();
        self.add_global_at(index, Global::Const(value), span.code(), comment);
    }
    pub(crate) fn add_global_at(
        &mut self,
        index: usize,
        global: Global,
        span: Option<CodeSpan>,
        comment: Option<Arc<str>>,
    ) {
        let binding = BindingInfo {
            global,
            span,
            comment,
        };
        if index < self.bindings.len() {
            self.bindings.make_mut()[index] = binding;
        } else {
            while self.bindings.len() < index {
                self.bindings.push(BindingInfo {
                    global: Global::Const(Value::default()),
                    span: None,
                    comment: None,
                });
            }
            self.bindings.push(binding);
        }
    }
}

impl AsRef<Assembly> for Assembly {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl AsMut<Assembly> for Assembly {
    fn as_mut(&mut self) -> &mut Self {
        self
    }
}

/// Information about a binding
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BindingInfo {
    /// The global binding type
    pub global: Global,
    #[serde(skip)]
    #[allow(dead_code)]
    /// The span of the original binding name
    pub span: Option<CodeSpan>,
    #[serde(skip)]
    #[allow(dead_code)]
    /// The comment preceding the binding
    pub comment: Option<Arc<str>>,
}

/// A type of global binding
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Global {
    /// A constant value
    Const(Value),
    /// A function
    Func(Function),
    /// The signature of something that will be bound at runtime
    Sig(Signature),
    /// A module
    #[allow(missing_docs)]
    Module { module: PathBuf },
}

impl Global {
    /// Get the signature of the global
    pub fn signature(&self) -> Option<Signature> {
        match self {
            Self::Const(_) => Some(Signature::new(0, 1)),
            Self::Func(func) => Some(func.signature()),
            Self::Sig(sig) => Some(*sig),
            Self::Module { .. } => None,
        }
    }
}

/// A repository of code strings input to the compiler
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Inputs {
    /// A map of file paths to their string contents
    #[serde(skip_serializing_if = "DashMap::is_empty")]
    pub files: DashMap<PathBuf, EcoString>,
    /// A list of input strings without paths
    #[serde(skip_serializing_if = "EcoVec::is_empty")]
    pub strings: EcoVec<EcoString>,
}

impl Inputs {
    pub(crate) fn add_src(
        &mut self,
        src: impl IntoInputSrc,
        input: impl Into<EcoString>,
    ) -> InputSrc {
        let mut src = src.into_input_src(self.strings.len());
        match &mut src {
            InputSrc::File(path) => {
                self.files.insert(path.to_path_buf(), input.into());
            }
            InputSrc::Str(i) => {
                *i = self.strings.len();
                self.strings.push(input.into())
            }
        }
        src
    }
    /// Get an input string
    pub fn get(&self, src: &InputSrc) -> EcoString {
        match src {
            InputSrc::File(path) => self
                .files
                .get(&**path)
                .unwrap_or_else(|| panic!("File {:?} not found", path))
                .clone(),
            InputSrc::Str(index) => self
                .strings
                .get(*index)
                .unwrap_or_else(|| panic!("String {} not found", index))
                .clone(),
        }
    }
}

impl Serialize for Instr {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        InstrRep::from(self.clone()).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Instr {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        InstrRep::deserialize(deserializer).map(Self::from)
    }
}

#[derive(Serialize, Deserialize)]
enum InstrRep {
    Comment(Ident),
    CallGlobal(usize, bool, Signature),
    BindGlobal(usize, usize),
    BeginArray,
    EndArray(bool, usize),
    Call(usize),
    PushFunc(Function),
    Switch(usize, Signature, usize),
    Format(EcoVec<EcoString>, usize),
    Dynamic(DynamicFunction),
    Unpack(usize, usize, bool),
    PushTempFunctions(usize),
    PopTempFunctions(usize),
    GetTempFunction(usize, Signature, usize),
    TouchStack(usize, usize),
    PushTemp(TempStack, usize, usize),
    PopTemp(TempStack, usize, usize),
    CopyToTemp(TempStack, usize, usize),
    CopyFromTemp(TempStack, usize, usize, usize),
    DropTemp(TempStack, usize, usize),
    PushSig(Signature),
    PopSig,
    SetOutputComment(usize, usize),
    #[serde(untagged)]
    Push(Value),
    #[serde(untagged)]
    Prim(Primitive, usize),
    #[serde(untagged)]
    ImplPrim(ImplPrimitive, usize),
}

impl From<Instr> for InstrRep {
    fn from(value: Instr) -> Self {
        match value {
            Instr::Comment(ident) => Self::Comment(ident),
            Instr::Push(value) => Self::Push(value),
            Instr::CallGlobal { index, call, sig } => Self::CallGlobal(index, call, sig),
            Instr::BindGlobal { span, index } => Self::BindGlobal(span, index),
            Instr::BeginArray => Self::BeginArray,
            Instr::EndArray { boxed, span } => Self::EndArray(boxed, span),
            Instr::Prim(prim, span) => Self::Prim(prim, span),
            Instr::ImplPrim(prim, span) => Self::ImplPrim(prim, span),
            Instr::Call(index) => Self::Call(index),
            Instr::PushFunc(func) => Self::PushFunc(func),
            Instr::Switch { count, sig, span } => Self::Switch(count, sig, span),
            Instr::Format(strings, span) => Self::Format(strings, span),
            Instr::Dynamic(func) => Self::Dynamic(func),
            Instr::Unpack { count, span, unbox } => Self::Unpack(count, span, unbox),
            Instr::PushTempFunctions(count) => Self::PushTempFunctions(count),
            Instr::PopTempFunctions(count) => Self::PopTempFunctions(count),
            Instr::GetTempFunction { offset, sig, span } => {
                Self::GetTempFunction(offset, sig, span)
            }
            Instr::TouchStack { count, span } => Self::TouchStack(count, span),
            Instr::PushTemp { stack, count, span } => Self::PushTemp(stack, count, span),
            Instr::PopTemp { stack, count, span } => Self::PopTemp(stack, count, span),
            Instr::CopyToTemp { stack, count, span } => Self::CopyToTemp(stack, count, span),
            Instr::CopyFromTemp {
                stack,
                offset,
                count,
                span,
            } => Self::CopyFromTemp(stack, offset, count, span),
            Instr::DropTemp { stack, count, span } => Self::DropTemp(stack, count, span),
            Instr::PushSig(sig) => Self::PushSig(sig),
            Instr::PopSig => Self::PopSig,
            Instr::SetOutputComment { i, n } => Self::SetOutputComment(i, n),
        }
    }
}

impl From<InstrRep> for Instr {
    fn from(value: InstrRep) -> Self {
        match value {
            InstrRep::Comment(ident) => Self::Comment(ident),
            InstrRep::Push(value) => Self::Push(value),
            InstrRep::CallGlobal(index, call, sig) => Self::CallGlobal { index, call, sig },
            InstrRep::BindGlobal(span, index) => Self::BindGlobal { span, index },
            InstrRep::BeginArray => Self::BeginArray,
            InstrRep::EndArray(boxed, span) => Self::EndArray { boxed, span },
            InstrRep::Prim(prim, span) => Self::Prim(prim, span),
            InstrRep::ImplPrim(prim, span) => Self::ImplPrim(prim, span),
            InstrRep::Call(index) => Self::Call(index),
            InstrRep::PushFunc(func) => Self::PushFunc(func),
            InstrRep::Switch(count, sig, span) => Self::Switch { count, sig, span },
            InstrRep::Format(strings, span) => Self::Format(strings, span),
            InstrRep::Dynamic(func) => Self::Dynamic(func),
            InstrRep::Unpack(count, span, unbox) => Self::Unpack { count, span, unbox },
            InstrRep::PushTempFunctions(count) => Self::PushTempFunctions(count),
            InstrRep::PopTempFunctions(count) => Self::PopTempFunctions(count),
            InstrRep::GetTempFunction(offset, sig, span) => {
                Self::GetTempFunction { offset, sig, span }
            }
            InstrRep::TouchStack(count, span) => Self::TouchStack { count, span },
            InstrRep::PushTemp(stack, count, span) => Self::PushTemp { stack, count, span },
            InstrRep::PopTemp(stack, count, span) => Self::PopTemp { stack, count, span },
            InstrRep::CopyToTemp(stack, count, span) => Self::CopyToTemp { stack, count, span },
            InstrRep::CopyFromTemp(stack, offset, count, span) => Self::CopyFromTemp {
                stack,
                offset,
                count,
                span,
            },
            InstrRep::DropTemp(stack, count, span) => Self::DropTemp { stack, count, span },
            InstrRep::PushSig(sig) => Self::PushSig(sig),
            InstrRep::PopSig => Self::PopSig,
            InstrRep::SetOutputComment(i, n) => Self::SetOutputComment { i, n },
        }
    }
}
