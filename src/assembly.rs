use std::{path::PathBuf, sync::Arc};

use dashmap::DashMap;
use ecow::{eco_vec, EcoString, EcoVec};
use serde::*;

use crate::{
    CodeSpan, DynamicFunction, FuncSlice, Function, Ident, ImplPrimitive, InputSrc, Instr,
    IntoInputSrc, LocalName, Primitive, Signature, Span, TempStack, Uiua, UiuaResult, Value,
};

/// A compiled Uiua assembly
#[derive(Clone)]
pub struct Assembly {
    pub(crate) instrs: EcoVec<Instr>,
    /// The sections of the instructions that are top-level expressions
    pub(crate) top_slices: Vec<FuncSlice>,
    /// A list of global bindings
    pub bindings: EcoVec<BindingInfo>,
    pub(crate) spans: EcoVec<Span>,
    pub(crate) inputs: Inputs,
    pub(crate) dynamic_functions: EcoVec<DynFn>,
}

type DynFn = Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static>;

impl Default for Assembly {
    fn default() -> Self {
        Self {
            instrs: EcoVec::new(),
            top_slices: Vec::new(),
            spans: eco_vec![Span::Builtin],
            bindings: EcoVec::new(),
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
        &self.instrs[slice.start..slice.start + slice.len]
    }
    /// Get the mutable instructions of a function slice
    pub fn instrs_mut(&mut self, slice: FuncSlice) -> &mut [Instr] {
        &mut self.instrs.make_mut()[slice.start..slice.start + slice.len]
    }
    pub(crate) fn bind_function(
        &mut self,
        local: LocalName,
        function: Function,
        span: usize,
        comment: Option<Arc<str>>,
    ) {
        let span = self.spans[span].clone();
        self.add_global_at(local, BindingKind::Func(function), span.code(), comment);
    }
    pub(crate) fn bind_const(
        &mut self,
        local: LocalName,
        value: Option<Value>,
        span: usize,
        comment: Option<Arc<str>>,
    ) {
        let span = self.spans[span].clone();
        self.add_global_at(local, BindingKind::Const(value), span.code(), comment);
    }
    pub(crate) fn add_global_at(
        &mut self,
        local: LocalName,
        global: BindingKind,
        span: Option<CodeSpan>,
        comment: Option<Arc<str>>,
    ) {
        let binding = BindingInfo {
            public: local.public,
            kind: global,
            span: span.unwrap_or_else(CodeSpan::dummy),
            comment,
        };
        if local.index < self.bindings.len() {
            self.bindings.make_mut()[local.index] = binding;
        } else {
            while self.bindings.len() < local.index {
                self.bindings.push(BindingInfo {
                    kind: BindingKind::Const(None),
                    public: false,
                    span: CodeSpan::dummy(),
                    comment: None,
                });
            }
            self.bindings.push(binding);
        }
    }
    /// Make top-level expressions not run
    pub fn remove_top_level(&mut self) {
        self.top_slices.clear();
    }
    /// Parse a `.uasm` file into an assembly
    pub fn from_uasm(src: &str) -> Result<Self, String> {
        let rest = src;
        let (instrs_src, rest) = rest
            .trim()
            .split_once("TOP SLICES")
            .ok_or("No top slices")?;
        let (top_slices_src, rest) = rest.split_once("BINDINGS").ok_or("No bindings")?;
        let (bindings_src, rest) = rest.trim().split_once("SPANS").ok_or("No spans")?;
        let (spans_src, rest) = rest.trim().split_once("FILES").ok_or("No files")?;
        let (files_src, rest) = rest
            .trim()
            .split_once("STRING INPUTS")
            .ok_or("No string inputs")?;
        let strings_src = rest.trim();

        let mut instrs = EcoVec::new();
        for line in instrs_src.lines().filter(|line| !line.is_empty()) {
            let instr: Instr = serde_json::from_str(line).map_err(|e| e.to_string())?;
            instrs.push(instr);
        }

        let mut top_slices = Vec::new();
        for line in top_slices_src.lines().filter(|line| !line.is_empty()) {
            let (start, len) = line.split_once(' ').ok_or("No start")?;
            let start = start.parse::<usize>().map_err(|e| e.to_string())?;
            let len = len.parse::<usize>().map_err(|e| e.to_string())?;
            top_slices.push(FuncSlice { start, len });
        }

        let mut bindings = EcoVec::new();
        for line in bindings_src.lines().filter(|line| !line.is_empty()) {
            let binding: BindingInfo = serde_json::from_str(line).map_err(|e| e.to_string())?;
            bindings.push(binding);
        }

        let mut spans = EcoVec::new();
        spans.push(Span::Builtin);
        for line in spans_src.lines().filter(|line| !line.is_empty()) {
            let span: Span = serde_json::from_str(line).map_err(|e| e.to_string())?;
            spans.push(span);
        }

        let files = DashMap::new();
        for line in files_src.lines().filter(|line| !line.is_empty()) {
            let (path, src) = line.split_once(": ").ok_or("No path")?;
            let path = PathBuf::from(path);
            let src: EcoString = serde_json::from_str(src).map_err(|e| e.to_string())?;
            files.insert(path, src);
        }

        let mut strings = EcoVec::new();
        for line in strings_src.lines() {
            let src: EcoString = serde_json::from_str(line).map_err(|e| e.to_string())?;
            strings.push(src);
        }

        Ok(Self {
            instrs,
            top_slices,
            bindings,
            spans,
            inputs: Inputs {
                files,
                strings,
                ..Inputs::default()
            },
            dynamic_functions: EcoVec::new(),
        })
    }
    /// Serialize the assembly into a `.uasm` file
    pub fn to_uasm(&self) -> String {
        let mut uasm = String::new();

        for instr in &self.instrs {
            uasm.push_str(&serde_json::to_string(instr).unwrap());
            uasm.push('\n');
        }

        uasm.push_str("\nTOP SLICES\n");
        for slice in &self.top_slices {
            uasm.push_str(&format!("{} {}\n", slice.start, slice.len));
        }

        uasm.push_str("\nBINDINGS\n");
        for binding in &self.bindings {
            uasm.push_str(&serde_json::to_string(binding).unwrap());
            uasm.push('\n');
        }

        uasm.push_str("\nSPANS\n");
        for span in self.spans.iter().skip(1) {
            uasm.push_str(&serde_json::to_string(span).unwrap());
            uasm.push('\n');
        }

        uasm.push_str("\nFILES\n");
        for entry in &self.inputs.files {
            let key = entry.key();
            let value = entry.value();
            uasm.push_str(&format!(
                "{}: {}\n",
                key.to_string_lossy(),
                serde_json::to_string(value).unwrap()
            ));
        }

        uasm.push_str("\nSTRING INPUTS\n");
        for src in &self.inputs.strings {
            uasm.push_str(&serde_json::to_string(src).unwrap());
            uasm.push('\n');
        }

        uasm
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
pub struct BindingInfo {
    /// The binding kind
    pub kind: BindingKind,
    /// Whether the binding is public
    #[serde(default = "tru", skip_serializing_if = "is_true")]
    pub public: bool,
    #[serde(skip, default = "CodeSpan::dummy")]
    #[allow(dead_code)]
    /// The span of the original binding name
    pub span: CodeSpan,
    #[serde(skip)]
    #[allow(dead_code)]
    /// The comment preceding the binding
    pub comment: Option<Arc<str>>,
}

fn is_true(value: &bool) -> bool {
    *value
}

fn tru() -> bool {
    true
}

/// A kind of global binding
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BindingKind {
    /// A constant value
    Const(Option<Value>),
    /// A function
    Func(Function),
    /// A module
    #[allow(missing_docs)]
    Module(PathBuf),
    /// A macro
    Macro,
}

impl BindingKind {
    /// Get the signature of the binding
    pub fn signature(&self) -> Option<Signature> {
        match self {
            Self::Const(_) => Some(Signature::new(0, 1)),
            Self::Func(func) => Some(func.signature()),
            Self::Module { .. } => None,
            Self::Macro => None,
        }
    }
    /// Check if the global is a once-bound constant
    pub fn is_constant(&self) -> bool {
        matches!(self, Self::Const(_))
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
    /// A map of spans to macro strings
    #[serde(skip)]
    pub macros: DashMap<CodeSpan, EcoString>,
}

impl Inputs {
    pub(crate) fn add_src(
        &mut self,
        src: impl IntoInputSrc,
        input: impl Into<EcoString>,
    ) -> InputSrc {
        let src = src.into_input_src(self.strings.len());
        match &src {
            InputSrc::File(path) => {
                self.files.insert(path.to_path_buf(), input.into());
            }
            InputSrc::Str(i) => {
                while self.strings.len() <= *i {
                    self.strings.push(EcoString::default());
                }
                self.strings.make_mut()[*i] = input.into();
            }
            InputSrc::Macro(span) => {
                self.macros.insert((**span).clone(), input.into());
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
            InputSrc::Macro(span) => self
                .macros
                .get(span)
                .unwrap_or_else(|| panic!("Macro at {} not found", span))
                .clone(),
        }
    }
    /// Get an input string and perform an operation on it
    pub fn get_with<T>(&self, src: &InputSrc, f: impl FnOnce(&str) -> T) -> T {
        match src {
            InputSrc::File(path) => {
                if let Some(src) = self.files.get(&**path) {
                    f(&src)
                } else {
                    panic!("File {:?} not found", path)
                }
            }
            InputSrc::Str(index) => {
                if let Some(src) = self.strings.get(*index) {
                    f(src)
                } else {
                    panic!("String {} not found", index)
                }
            }
            InputSrc::Macro(span) => {
                if let Some(src) = self.macros.get(span) {
                    f(src.value())
                } else {
                    panic!("Macro at {} not found", span)
                }
            }
        }
    }
    /// Get an input string and perform an operation on it
    pub fn try_get_with<T>(&self, src: &InputSrc, f: impl FnOnce(&str) -> T) -> Option<T> {
        match src {
            InputSrc::File(path) => self.files.get(&**path).map(|src| f(&src)),
            InputSrc::Str(index) => self.strings.get(*index).map(|src| f(src)),
            InputSrc::Macro(span) => self.macros.get(span).map(|src| f(&src)),
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
    CallGlobal(usize, bool),
    BindGlobal(usize, usize),
    BeginArray(()),
    EndArray(bool, usize),
    Call(usize),
    PushFunc(Function),
    Switch(usize, Signature, usize, bool),
    Format(EcoVec<EcoString>, usize),
    MatchFormatPattern(EcoVec<EcoString>, usize),
    Label(EcoString, usize),
    Dynamic(DynamicFunction),
    PushLocals(usize, usize),
    PopLocals(()),
    GetLocal(usize, usize),
    Unpack(usize, usize, bool),
    TouchStack(usize, usize),
    PushTemp(TempStack, usize, usize),
    PopTemp(TempStack, usize, usize),
    CopyToTemp(TempStack, usize, usize),
    CopyFromTemp(TempStack, usize, usize, usize),
    DropTemp(TempStack, usize, usize),
    PushSig(Signature),
    PopSig(()),
    SetOutputComment(usize, usize),
    NoInline(()),
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
            Instr::CallGlobal { index, call } => Self::CallGlobal(index, call),
            Instr::BindGlobal { span, index } => Self::BindGlobal(span, index),
            Instr::BeginArray => Self::BeginArray(()),
            Instr::EndArray { boxed, span } => Self::EndArray(boxed, span),
            Instr::Prim(prim, span) => Self::Prim(prim, span),
            Instr::ImplPrim(prim, span) => Self::ImplPrim(prim, span),
            Instr::Call(index) => Self::Call(index),
            Instr::PushFunc(func) => Self::PushFunc(func),
            Instr::Switch {
                count,
                sig,
                span,
                under_cond,
            } => Self::Switch(count, sig, span, under_cond),
            Instr::Format { parts, span } => Self::Format(parts, span),
            Instr::MatchFormatPattern { parts, span } => Self::MatchFormatPattern(parts, span),
            Instr::Label { label, span } => Self::Label(label, span),
            Instr::Dynamic(func) => Self::Dynamic(func),
            Instr::PushLocals { count, span } => Self::PushLocals(count, span),
            Instr::PopLocals => Self::PopLocals(()),
            Instr::GetLocal { index, span } => Self::GetLocal(index, span),
            Instr::Unpack { count, span, unbox } => Self::Unpack(count, span, unbox),
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
            Instr::PopSig => Self::PopSig(()),
            Instr::SetOutputComment { i, n } => Self::SetOutputComment(i, n),
            Instr::NoInline => Self::NoInline(()),
        }
    }
}

impl From<InstrRep> for Instr {
    fn from(value: InstrRep) -> Self {
        match value {
            InstrRep::Comment(ident) => Self::Comment(ident),
            InstrRep::Push(value) => Self::Push(value),
            InstrRep::CallGlobal(index, call) => Self::CallGlobal { index, call },
            InstrRep::BindGlobal(span, index) => Self::BindGlobal { span, index },
            InstrRep::BeginArray(()) => Self::BeginArray,
            InstrRep::EndArray(boxed, span) => Self::EndArray { boxed, span },
            InstrRep::Prim(prim, span) => Self::Prim(prim, span),
            InstrRep::ImplPrim(prim, span) => Self::ImplPrim(prim, span),
            InstrRep::Call(index) => Self::Call(index),
            InstrRep::PushFunc(func) => Self::PushFunc(func),
            InstrRep::Switch(count, sig, span, under_cond) => Self::Switch {
                count,
                sig,
                span,
                under_cond,
            },
            InstrRep::Format(parts, span) => Self::Format { parts, span },
            InstrRep::MatchFormatPattern(parts, span) => Self::MatchFormatPattern { parts, span },
            InstrRep::Label(label, span) => Self::Label { label, span },
            InstrRep::Dynamic(func) => Self::Dynamic(func),
            InstrRep::PushLocals(count, span) => Self::PushLocals { count, span },
            InstrRep::PopLocals(()) => Self::PopLocals,
            InstrRep::GetLocal(index, span) => Self::GetLocal { index, span },
            InstrRep::Unpack(count, span, unbox) => Self::Unpack { count, span, unbox },
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
            InstrRep::PopSig(()) => Self::PopSig,
            InstrRep::SetOutputComment(i, n) => Self::SetOutputComment { i, n },
            InstrRep::NoInline(()) => Self::NoInline,
        }
    }
}
