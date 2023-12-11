use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use dashmap::DashMap;
use ecow::{eco_vec, EcoString, EcoVec};
use serde::*;

use crate::{
    DynamicFunction, FuncSlice, Function, Ident, ImplPrimitive, InputSrc, Instr, IntoInputSrc,
    Primitive, Signature, Span, TempStack, Value,
};

/// A compiled Uiua assembly
#[derive(Clone, Serialize, Deserialize)]
pub struct Assembly {
    pub(crate) instrs: EcoVec<Instr>,
    pub(crate) top_slices: Vec<FuncSlice>,
    pub(crate) globals: EcoVec<Global>,
    pub(crate) import_inputs: HashMap<PathBuf, EcoString>,
    pub(crate) spans: EcoVec<Span>,
    pub(crate) inputs: Inputs,
}

impl Default for Assembly {
    fn default() -> Self {
        Self {
            instrs: EcoVec::new(),
            top_slices: Vec::new(),
            import_inputs: HashMap::new(),
            spans: eco_vec![Span::Builtin],
            globals: EcoVec::new(),
            inputs: Inputs::default(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum Global {
    Const(Value),
    Func(Function),
    Sig(Signature),
}

/// A repository of code strings input to the compiler
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Inputs {
    /// A map of file paths to their string contents
    pub files: DashMap<PathBuf, EcoString>,
    /// A list of input strings without paths
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
                self.files
                    .insert(PathBuf::from(path.as_str()), input.into());
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
                .get(Path::new(path.as_str()))
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
    BindGlobal(Ident, usize, usize),
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
            Instr::BindGlobal { name, span, index } => Self::BindGlobal(name, span, index),
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
        }
    }
}

impl From<InstrRep> for Instr {
    fn from(value: InstrRep) -> Self {
        match value {
            InstrRep::Comment(ident) => Self::Comment(ident),
            InstrRep::Push(value) => Self::Push(value),
            InstrRep::CallGlobal(index, call, sig) => Self::CallGlobal { index, call, sig },
            InstrRep::BindGlobal(name, span, index) => Self::BindGlobal { name, span, index },
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
        }
    }
}
