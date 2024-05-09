use std::{fmt, iter::once, path::PathBuf, sync::Arc};

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
        comment: Option<DocComment>,
    ) {
        let span = self.spans[span].clone();
        self.add_global_at(local, BindingKind::Func(function), span.code(), comment);
    }
    pub(crate) fn bind_const(
        &mut self,
        local: LocalName,
        value: Option<Value>,
        span: usize,
        comment: Option<DocComment>,
    ) {
        let span = self.spans[span].clone();
        self.add_global_at(local, BindingKind::Const(value), span.code(), comment);
    }
    pub(crate) fn add_global_at(
        &mut self,
        local: LocalName,
        global: BindingKind,
        span: Option<CodeSpan>,
        comment: Option<DocComment>,
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
            .unwrap_or((rest, ""));
        let strings_src = rest.trim();

        let mut instrs = EcoVec::new();
        for line in instrs_src.lines().filter(|line| !line.trim().is_empty()) {
            let instr: Instr = serde_json::from_str(line)
                .or_else(|e| {
                    let (key, val) = line.split_once(' ').ok_or("No key")?;
                    let json = format!("{{{key:?}: {val}}}");
                    serde_json::from_str(&json).map_err(|_| e.to_string())
                })
                .or_else(|e| {
                    let (key, val) = line.split_once(' ').ok_or("No key")?;
                    let json = format!("[{key:?},{val}]");
                    serde_json::from_str(&json).map_err(|_| e)
                })
                .or_else(|e| serde_json::from_str(&format!("\"{line}\"")).map_err(|_| e))
                .unwrap();
            instrs.push(instr);
        }

        let mut top_slices = Vec::new();
        for line in top_slices_src
            .lines()
            .filter(|line| !line.trim().is_empty())
        {
            let (start, len) = line.split_once(' ').ok_or("No start")?;
            let start = start.parse::<usize>().map_err(|e| e.to_string())?;
            let len = len.parse::<usize>().map_err(|e| e.to_string())?;
            top_slices.push(FuncSlice { start, len });
        }

        let mut bindings = EcoVec::new();
        for line in bindings_src.lines().filter(|line| !line.trim().is_empty()) {
            let (public, line) = if let Some(line) = line.strip_prefix("private ") {
                (false, line)
            } else {
                (true, line)
            };
            let kind: BindingKind = serde_json::from_str(line).or_else(|e| {
                let (key, val) = line.split_once("\" ").ok_or("No key")?;
                let key = format!("{key}\"");
                let json = format!("{{{key:?}: {val:?}}}");
                serde_json::from_str(&json).map_err(|_| e.to_string())
            })?;
            bindings.push(BindingInfo {
                kind,
                public,
                span: CodeSpan::dummy(),
                comment: None,
            });
        }

        let mut spans = EcoVec::new();
        spans.push(Span::Builtin);
        for line in spans_src.lines().filter(|line| !line.trim().is_empty()) {
            let span: Span = serde_json::from_str(line).map_err(|e| e.to_string())?;
            spans.push(span);
        }

        let files = DashMap::new();
        for line in files_src.lines().filter(|line| !line.trim().is_empty()) {
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
            let json = serde_json::to_value(instr).unwrap();
            match &json {
                serde_json::Value::Object(map) => {
                    if map.len() == 1 {
                        let key = map.keys().next().unwrap();
                        let value = map.values().next().unwrap();
                        uasm.push_str(&format!("{} {}\n", key, value));
                        continue;
                    }
                }
                serde_json::Value::Array(arr) => {
                    if arr.len() == 2 {
                        if let serde_json::Value::String(key) = &arr[0] {
                            let value = &arr[1];
                            uasm.push_str(&format!("{} {}\n", key, value));
                            continue;
                        }
                    }
                }
                serde_json::Value::String(s) => {
                    uasm.push_str(s);
                    uasm.push('\n');
                    continue;
                }
                _ => (),
            }
            uasm.push_str(&json.to_string());
            uasm.push('\n');
        }

        uasm.push_str("\nTOP SLICES\n");
        for slice in &self.top_slices {
            uasm.push_str(&format!("{} {}\n", slice.start, slice.len));
        }

        uasm.push_str("\nBINDINGS\n");
        for binding in &self.bindings {
            if !binding.public {
                uasm.push_str("private ");
            }
            if let serde_json::Value::Object(map) = serde_json::to_value(&binding.kind).unwrap() {
                if map.len() == 1 {
                    let key = map.keys().next().unwrap();
                    let value = map.values().next().unwrap();
                    uasm.push_str(&format!("{} {}\n", key, value));
                    continue;
                }
            }
            uasm.push_str(&serde_json::to_string(&binding.kind).unwrap());
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
            uasm.push_str(&format!("{}: {:?}\n", key.display(), value));
        }

        if !self.inputs.strings.is_empty() {
            uasm.push_str("\nSTRING INPUTS\n");
            for src in &self.inputs.strings {
                uasm.push_str(&serde_json::to_string(src).unwrap());
                uasm.push('\n');
            }
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
#[derive(Debug, Clone)]
pub struct BindingInfo {
    /// The binding kind
    pub kind: BindingKind,
    /// Whether the binding is public
    pub public: bool,
    /// The span of the original binding name
    pub span: CodeSpan,
    /// The comment preceding the binding
    pub comment: Option<DocComment>,
}

/// A kind of global binding
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BindingKind {
    /// A constant value
    Const(Option<Value>),
    /// A function
    Func(Function),
    /// A module
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

/// A comment that documents a binding
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct DocComment {
    /// The comment text
    pub text: EcoString,
    /// The signature of the binding
    pub sig: Option<DocCommentSig>,
}

/// A signature in a doc comment
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct DocCommentSig {
    /// The arguments of the signature
    pub args: Vec<DocCommentArg>,
    /// The outputs of the signature
    pub outputs: Option<Vec<DocCommentArg>>,
}

impl DocCommentSig {
    /// Whether the doc comment signature matches a given function signature
    pub fn matches_sig(&self, sig: Signature) -> bool {
        self.args.len() == sig.args
            && (self.outputs.as_ref()).map_or(true, |o| o.len() == sig.outputs)
    }
    pub(crate) fn sig_string(&self) -> String {
        if let Some(outputs) = &self.outputs {
            format!(
                "signature {}",
                Signature::new(self.args.len(), outputs.len())
            )
        } else {
            format!("{} args", self.args.len())
        }
    }
}

impl fmt::Display for DocCommentSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.outputs.is_none() {
            write!(f, "? ")?;
        }
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", arg.name)?;
            if let Some(ty) = &arg.ty {
                write!(f, ":{}", ty)?;
            }
        }
        if let Some(outputs) = &self.outputs {
            write!(f, " --")?;
            for output in outputs {
                write!(f, " {}", output.name)?;
                if let Some(ty) = &output.ty {
                    write!(f, ":{}", ty)?;
                }
            }
        }
        Ok(())
    }
}

/// An argument in a doc comment signature
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct DocCommentArg {
    /// The name of the argument
    pub name: EcoString,
    /// A type descriptor for the argument
    pub ty: Option<EcoString>,
}

impl From<&str> for DocComment {
    fn from(text: &str) -> Self {
        let mut sig = None;
        let sig_line = text.lines().position(|line| {
            line.trim_start().starts_with('?') || line.contains("--") && !line.contains("---")
        });
        let raw_text = if let Some(i) = sig_line {
            let mut sig_text = text.lines().nth(i).unwrap();
            // Trim question mark and whitespace
            sig_text = sig_text.trim().trim_start_matches('?').trim();
            let has_divider = sig_text.contains("--");
            // Split into args and outputs
            let (mut args_text, mut outputs_text) =
                sig_text.split_once("--").unwrap_or((sig_text, ""));
            args_text = args_text.trim();
            outputs_text = outputs_text.trim();
            // Parse args and outputs
            let mut args = Vec::new();
            let mut outputs = Vec::new();
            for (args, text) in once((&mut args, args_text))
                .chain(has_divider.then_some((&mut outputs, outputs_text)))
            {
                // Tokenize text
                let mut tokens = Vec::new();
                for frag in text.split_whitespace() {
                    for (i, token) in frag.split(':').enumerate() {
                        if i > 0 {
                            tokens.push(":");
                        }
                        tokens.push(token);
                    }
                }
                // Parse tokens into args
                let mut curr_arg_name = None;
                let mut tokens = tokens.into_iter().peekable();
                while let Some(token) = tokens.next() {
                    if token == ":" {
                        let ty = tokens.next().unwrap_or_default();
                        args.push(DocCommentArg {
                            name: curr_arg_name.take().unwrap_or_default(),
                            ty: if ty.is_empty() { None } else { Some(ty.into()) },
                        });
                    } else {
                        if let Some(curr) = curr_arg_name.take() {
                            args.push(DocCommentArg {
                                name: curr,
                                ty: None,
                            });
                        }
                        curr_arg_name = Some(token.into());
                    }
                }
                if let Some(curr) = curr_arg_name.take() {
                    args.push(DocCommentArg {
                        name: curr,
                        ty: None,
                    });
                }
            }

            sig = Some(DocCommentSig {
                args,
                outputs: has_divider.then_some(outputs),
            });

            let mut text: EcoString = (text.lines().take(i))
                .chain(once("\n"))
                .chain(text.lines().skip(i + 1))
                .flat_map(|s| s.chars().chain(Some('\n')))
                .collect();
            while text.ends_with('\n') {
                text.pop();
            }
            if text.starts_with('\n') {
                text = text.trim_start_matches('\n').into();
            }
            text
        } else {
            text.into()
        };
        let mut text = EcoString::new();
        for (i, line) in raw_text.lines().enumerate() {
            if i > 0 {
                text.push('\n');
            }
            text.push_str(line.trim());
        }
        DocComment { text, sig }
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
#[serde(rename_all = "snake_case")]
enum InstrRep {
    Comment(Ident),
    CallGlobal(usize, bool),
    BindGlobal(usize, usize),
    BeginArray,
    EndArray(bool, usize),
    Call(usize),
    CallRecursive(usize),
    Recur(usize),
    PushFunc(Function),
    Switch(usize, Signature, usize, bool),
    Format(EcoVec<EcoString>, usize),
    MatchFormatPattern(EcoVec<EcoString>, usize),
    Label(EcoString, usize),
    Dynamic(DynamicFunction),
    Unpack(usize, usize, bool),
    TouchStack(usize, usize),
    PushTemp(TempStack, usize, usize),
    PopTemp(TempStack, usize, usize),
    CopyToTemp(TempStack, usize, usize),
    PushSig(Signature),
    PopSig,
    SetOutputComment(usize, usize),
    NoInline,
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
            Instr::BeginArray => Self::BeginArray,
            Instr::EndArray { boxed, span } => Self::EndArray(boxed, span),
            Instr::Prim(prim, span) => Self::Prim(prim, span),
            Instr::ImplPrim(prim, span) => Self::ImplPrim(prim, span),
            Instr::Call(span) => Self::Call(span),
            Instr::CallRecursive(span) => Self::CallRecursive(span),
            Instr::Recur(span) => Self::Recur(span),
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
            Instr::Unpack { count, span, unbox } => Self::Unpack(count, span, unbox),
            Instr::TouchStack { count, span } => Self::TouchStack(count, span),
            Instr::PushTemp { stack, count, span } => Self::PushTemp(stack, count, span),
            Instr::PopTemp { stack, count, span } => Self::PopTemp(stack, count, span),
            Instr::CopyToTemp { stack, count, span } => Self::CopyToTemp(stack, count, span),
            Instr::PushSig(sig) => Self::PushSig(sig),
            Instr::PopSig => Self::PopSig,
            Instr::SetOutputComment { i, n } => Self::SetOutputComment(i, n),
            Instr::NoInline => Self::NoInline,
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
            InstrRep::BeginArray => Self::BeginArray,
            InstrRep::EndArray(boxed, span) => Self::EndArray { boxed, span },
            InstrRep::Prim(prim, span) => Self::Prim(prim, span),
            InstrRep::ImplPrim(prim, span) => Self::ImplPrim(prim, span),
            InstrRep::Call(span) => Self::Call(span),
            InstrRep::CallRecursive(span) => Self::CallRecursive(span),
            InstrRep::Recur(span) => Self::Recur(span),
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
            InstrRep::Unpack(count, span, unbox) => Self::Unpack { count, span, unbox },
            InstrRep::TouchStack(count, span) => Self::TouchStack { count, span },
            InstrRep::PushTemp(stack, count, span) => Self::PushTemp { stack, count, span },
            InstrRep::PopTemp(stack, count, span) => Self::PopTemp { stack, count, span },
            InstrRep::CopyToTemp(stack, count, span) => Self::CopyToTemp { stack, count, span },
            InstrRep::PushSig(sig) => Self::PushSig(sig),
            InstrRep::PopSig => Self::PopSig,
            InstrRep::SetOutputComment(i, n) => Self::SetOutputComment { i, n },
            InstrRep::NoInline => Self::NoInline,
        }
    }
}
