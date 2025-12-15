use std::{
    collections::BTreeMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Index, IndexMut},
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};

use dashmap::DashMap;
use ecow::{EcoString, EcoVec, eco_vec};
use indexmap::IndexMap;
use serde::*;
use uiua_parser::SUBSCRIPT_DIGITS;

use crate::{
    BindingCounts, CodeSpan, FunctionId, Ident, Inputs, Node, SigNode, Signature, Span, Uiua,
    UiuaResult, Value,
    compile::{LocalIndex, Module},
    is_ident_char,
};

/// A compiled Uiua assembly
#[derive(Clone)]
pub struct Assembly {
    /// The top-level node
    pub root: Node,
    /// Functions
    pub(crate) functions: EcoVec<Node>,
    /// A list of top-level names
    pub exports: Arc<IndexMap<Ident, usize>>,
    /// A list of global bindings
    pub bindings: EcoVec<BindingInfo>,
    pub(crate) spans: EcoVec<Span>,
    /// Inputs used to build the assembly
    pub inputs: Inputs,
    pub(crate) dynamic_functions: EcoVec<DynFn>,
    pub(crate) test_assert_count: usize,
    /// Height of the stack on each line
    pub(crate) line_sigs: BTreeMap<u16, Signature>,
}

/// A Uiua function
///
/// This does not actually contain the function's code.
/// It is a lightweight handle that can be used to look up the function's code in an [`Assembly`].
///
/// It also contains the function's [`FunctionId`] and [`Signature`].
#[derive(Clone)]
pub struct Function {
    /// The function's id
    pub id: FunctionId,
    /// The function's signature
    pub sig: Signature,
    pub(crate) index: usize,
    hash: u64,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ← {}", self.id, self.sig)
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.sig == other.sig && self.hash == other.hash
    }
}

impl Eq for Function {}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Serialize for Function {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (&self.id, &self.sig, &self.index, &self.hash).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Function {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let (id, sig, index, hash) =
            <(FunctionId, Signature, usize, u64)>::deserialize(deserializer)?;
        Ok(Function {
            id,
            sig,
            index,
            hash,
        })
    }
}

impl Assembly {
    /// Get the [`SigNode`] for a function
    pub fn sig_node(&self, f: &Function) -> SigNode {
        SigNode::new(f.sig, self[f].clone())
    }
    /// Add a function to the assembly
    pub fn add_function(&mut self, id: FunctionId, sig: Signature, mut root: Node) -> Function {
        root.optimize_early();
        let mut hasher = DefaultHasher::new();
        root.hash(&mut hasher);
        let hash = hasher.finish();
        self.functions.push(root);
        let index = self.functions.len() - 1;
        Function {
            id,
            sig,
            index,
            hash,
        }
    }
    pub(crate) fn add_binding_at(
        &mut self,
        local: LocalIndex,
        kind: BindingKind,
        span: Option<CodeSpan>,
        meta: BindingMeta,
    ) {
        let binding = BindingInfo {
            public: local.public,
            kind,
            span: span.unwrap_or_else(CodeSpan::dummy),
            meta,
            used: local.public,
        };
        if local.index < self.bindings.len() {
            self.bindings.make_mut()[local.index] = binding;
        } else {
            while self.bindings.len() < local.index {
                self.bindings.push(BindingInfo {
                    kind: BindingKind::Const(None),
                    public: false,
                    span: CodeSpan::dummy(),
                    meta: BindingMeta::default(),
                    used: true,
                });
            }
            self.bindings.push(binding);
        }
    }
    pub(crate) fn bind_const(
        &mut self,
        local: LocalIndex,
        value: Option<Value>,
        span: usize,
        meta: BindingMeta,
    ) {
        let span = self.spans[span].clone();
        self.add_binding_at(local, BindingKind::Const(value), span.code(), meta);
    }
    pub(crate) fn module(&self) -> Module {
        let mut module = Module::default();
        for (name, &index) in &*self.exports {
            let public = self.bindings[index].public;
            (module.names).insert(name.clone(), LocalIndex { index, public });
        }
        module
    }
    /// Parse a `.uasm` file into an assembly
    pub fn from_uasm(src: &str) -> Result<Self, String> {
        let rest = src;
        let (root_src, rest) = rest.split_once("EXPORTS").ok_or("No exports")?;
        let (exports_src, rest) = rest.split_once("BINDINGS").ok_or("No bindings")?;
        let (bindings_src, rest) = rest.trim().split_once("FUNCTIONS").ok_or("No functions")?;
        let (functions_src, rest) = rest.trim().split_once("SPANS").ok_or("No spans")?;
        let (spans_src, rest) = rest.trim().split_once("FILES").ok_or("No files")?;
        let (files_src, rest) = rest
            .trim()
            .split_once("STRING INPUTS")
            .unwrap_or((rest, ""));
        let strings_src = rest.trim();

        let mut root = Node::empty();
        for line in root_src.lines().filter(|line| !line.trim().is_empty()) {
            let node: Node = serde_json::from_str(line).unwrap();
            root.push(node);
        }

        let mut exports = IndexMap::new();
        for line in exports_src.lines().filter(|line| !line.trim().is_empty()) {
            let mut words = line.split_whitespace();
            let name = words.next().ok_or("Missing export name")?;
            let index = words
                .next()
                .ok_or("Missing export index")?
                .parse::<usize>()
                .map_err(|e| format!("Invalid export index: {e}"))?;
            exports.insert(name.into(), index);
        }
        let exports = Arc::new(exports);

        let mut bindings = EcoVec::new();
        for line in bindings_src.lines().filter(|line| !line.trim().is_empty()) {
            let (public, line) = if let Some(line) = line.strip_prefix("private ") {
                (false, line)
            } else {
                (true, line)
            };
            let kind: BindingKind = serde_json::from_str(line).or_else(|e| {
                if let Some((key, val)) = line.split_once(' ') {
                    let json = format!("{{{key:?}: {val}}}");
                    serde_json::from_str(&json).map_err(|_| e.to_string())
                } else {
                    Err("No key".into())
                }
            })?;
            bindings.push(BindingInfo {
                kind,
                public,
                span: CodeSpan::dummy(),
                meta: BindingMeta::default(),
                used: true,
            });
        }

        let mut functions = EcoVec::new();
        for line in functions_src.lines().filter(|line| !line.trim().is_empty()) {
            let func: Node = serde_json::from_str(line).unwrap();
            functions.push(func);
        }

        let mut spans = EcoVec::new();
        spans.push(Span::Builtin);
        for line in spans_src.lines().filter(|line| !line.trim().is_empty()) {
            if line.trim().is_empty() {
                spans.push(Span::Builtin);
            } else {
                let (src_start, end) = line.trim().rsplit_once(' ').ok_or("invalid span")?;
                let (src, start) = src_start.split_once(' ').ok_or("invalid span")?;
                let src = serde_json::from_str(src).map_err(|e| e.to_string())?;
                let start = serde_json::from_str(start).map_err(|e| e.to_string())?;
                let end = serde_json::from_str(end).map_err(|e| e.to_string())?;
                spans.push(Span::Code(CodeSpan { src, start, end }));
            }
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
            root,
            exports,
            bindings,
            functions,
            spans,
            inputs: Inputs {
                files,
                strings,
                ..Inputs::default()
            },
            dynamic_functions: EcoVec::new(),
            test_assert_count: 0,
            line_sigs: BTreeMap::new(),
        })
    }
    /// Serialize the assembly into a `.uasm` file
    pub fn to_uasm(&self) -> String {
        let mut uasm = String::new();
        for node in self.root.iter() {
            uasm.push_str(&serde_json::to_string(node).unwrap());
            uasm.push('\n');
        }

        uasm.push_str("\nEXPORTS\n");
        for (name, index) in &*self.exports {
            uasm.push_str(&format!("{name} {index}\n"));
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
                    uasm.push_str(&format!("{key} {value}\n"));
                    continue;
                }
            }
            uasm.push_str(&serde_json::to_string(&binding.kind).unwrap());
            uasm.push('\n');
        }

        uasm.push_str("\nFUNCTIONS\n");
        for func in &self.functions {
            uasm.push_str(&serde_json::to_string(&func).unwrap());
            uasm.push('\n');
        }

        uasm.push_str("\nSPANS\n");
        for span in self.spans.iter().skip(1) {
            if let Span::Code(span) = span {
                uasm.push_str(&serde_json::to_string(&span.src).unwrap());
                uasm.push(' ');
                uasm.push_str(&serde_json::to_string(&span.start).unwrap());
                uasm.push(' ');
                uasm.push_str(&serde_json::to_string(&span.end).unwrap());
            }
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

impl Index<&Function> for Assembly {
    type Output = Node;
    #[track_caller]
    fn index(&self, func: &Function) -> &Self::Output {
        match self.functions.get(func.index) {
            Some(node) => node,
            None => panic!("{}({:?}) not found in assembly", func.id, func.index),
        }
    }
}

impl IndexMut<&Function> for Assembly {
    #[track_caller]
    fn index_mut(&mut self, func: &Function) -> &mut Self::Output {
        match self.functions.make_mut().get_mut(func.index) {
            Some(node) => node,
            None => panic!("{}({:?}) not found in assembly", func.id, func.index),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
type DynFn = Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static>;
#[cfg(target_arch = "wasm32")]
type DynFn = Arc<dyn Fn(&mut Uiua) -> UiuaResult + 'static>;

impl Default for Assembly {
    fn default() -> Self {
        Self {
            root: Node::default(),
            exports: Arc::new(IndexMap::new()),
            functions: EcoVec::new(),
            spans: eco_vec![Span::Builtin],
            bindings: EcoVec::new(),
            dynamic_functions: EcoVec::new(),
            inputs: Inputs::default(),
            test_assert_count: 0,
            line_sigs: BTreeMap::new(),
        }
    }
}

impl From<&Assembly> for Assembly {
    fn from(asm: &Assembly) -> Self {
        asm.clone()
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
    /// Metadata about the binding
    pub meta: BindingMeta,
    /// Whether the binding was used
    pub used: bool,
}

/// Metadata about a binding
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BindingMeta {
    /// The comment preceding the binding
    pub comment: Option<DocComment>,
    /// The character counts for golfing
    pub counts: Option<BindingCounts>,
    /// The deprecation message
    pub deprecation: Option<EcoString>,
    /// Whether this binding's code was externally provided
    pub external: bool,
}

/// A kind of global binding
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BindingKind {
    /// A constant value
    Const(Option<Value>),
    /// A function
    Func(Function),
    /// An imported module
    Import(PathBuf),
    /// A scoped module
    Module(Module),
    /// A scope being compiled
    Scope(usize),
    /// An index macro
    ///
    /// Contains the number of arguments
    IndexMacro(usize),
    /// A code macro
    CodeMacro(Node),
    /// An error
    Error,
}

impl BindingKind {
    /// Get the signature of the binding
    pub fn sig(&self) -> Option<Signature> {
        match self {
            Self::Const(_) => Some(Signature::new(0, 1)),
            Self::Func(func) => Some(func.sig),
            Self::Import { .. } => None,
            Self::Module(_) => None,
            Self::Scope(_) => None,
            Self::IndexMacro(_) => None,
            Self::CodeMacro(_) => None,
            Self::Error => None,
        }
    }
    /// Check if the binding is a once-bound constant
    pub fn is_constant(&self) -> bool {
        matches!(self, Self::Const(_))
    }
    /// Check if the binding is a module
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Import(_) | Self::Module(_))
    }
    /// Check if the binding is a constant or function
    pub fn has_sig(&self) -> bool {
        match self {
            Self::Const(_) | Self::Func(_) => true,
            Self::Module(m) => m.names.contains_key("Call") || m.names.contains_key("New"),
            _ => false,
        }
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
    /// Whether this is a labelling signature
    pub label: bool,
    /// The arguments of the signature
    pub args: Option<Vec<DocCommentArg>>,
    /// The outputs of the signature
    pub outputs: Option<Vec<DocCommentArg>>,
}

impl DocCommentSig {
    /// Whether the doc comment signature matches a given function signature
    pub fn matches_sig(&self, sig: Signature) -> bool {
        (self.args.as_ref()).is_none_or(|args| args.len() == sig.args())
            && (self.outputs.as_ref()).is_none_or(|o| o.len() == sig.outputs())
    }
    pub(crate) fn sig_string(&self) -> String {
        match (&self.args, &self.outputs) {
            (Some(args), Some(outputs)) => {
                format!("signature {}", Signature::new(args.len(), outputs.len()))
            }
            (Some(args), None) => format!(
                "{} arg{}",
                args.len(),
                if args.len() == 1 { "" } else { "s" }
            ),
            (None, Some(outputs)) => format!(
                "{} output{}",
                outputs.len(),
                if outputs.len() == 1 { "" } else { "s" }
            ),
            (None, None) => "signature".into(),
        }
    }
}

impl fmt::Display for DocCommentSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(outputs) = &self.outputs {
            for output in outputs {
                write!(f, " {}", output.name)?;
                if let Some(ty) = &output.ty {
                    write!(f, ":{ty}")?;
                }
            }
            write!(f, " ")?;
        }
        if self.label {
            write!(f, "$")?;
        } else {
            write!(f, "?")?;
        }
        if let Some(args) = &self.args {
            write!(f, " ")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", arg.name)?;
                if let Some(ty) = &arg.ty {
                    write!(f, ":{ty}")?;
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

fn is_sig_line(s: &str) -> bool {
    if s.chars().filter(|&c| "$?".contains(c)).count() != 1 {
        return false;
    }
    let s = s.trim_end();
    (!s.ends_with(['?', '$']) || s.ends_with(" ?") || s.ends_with(" $"))
        && (s.chars()).all(|c| {
            c.is_whitespace()
                || "?$:".contains(c)
                || is_ident_char(c)
                || SUBSCRIPT_DIGITS.contains(&c)
        })
}

impl FromStr for DocCommentSig {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !is_sig_line(s) {
            return Err(());
        }
        // Split into args and outputs
        let mut label = false;
        let (mut outputs_text, mut args_text) = s
            .split_once('?')
            .or_else(|| s.split_once('$').inspect(|_| label = true))
            .ok_or(())?;
        outputs_text = outputs_text.trim();
        args_text = args_text.trim();
        // Parse args and outputs
        let mut args = Vec::new();
        let mut outputs = Vec::new();
        for (args, text) in [(&mut args, args_text), (&mut outputs, outputs_text)] {
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
        Ok(DocCommentSig {
            label,
            args: (!args.is_empty()).then_some(args),
            outputs: (!outputs.is_empty()).then_some(outputs),
        })
    }
}

impl From<String> for DocComment {
    fn from(text: String) -> Self {
        Self::from(text.as_str())
    }
}

impl From<&str> for DocComment {
    fn from(text: &str) -> Self {
        let mut sig = None;
        let sig_line = text.lines().position(is_sig_line);
        let raw_text = if let Some(i) = sig_line {
            sig = text.lines().nth(i).unwrap().parse().ok();

            let mut text: EcoString = (text.lines().take(i))
                .chain(["\n"])
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

impl fmt::Debug for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct FmtFunctions<'a>(&'a Assembly);
        impl fmt::Debug for FmtFunctions<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_list()
                    .entries(self.0.bindings.iter().filter_map(|b| {
                        if let BindingKind::Func(func) = &b.kind {
                            Some((func, &self.0[func]))
                        } else {
                            None
                        }
                    }))
                    .finish()
            }
        }
        f.debug_struct("Assembly")
            .field("root", &self.root)
            .field("functions", &FmtFunctions(self))
            .finish()
    }
}
