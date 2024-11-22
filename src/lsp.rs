//! Uiua's Language Server Protocol (LSP) implementation
//!
//! Even without the `lsp` feature enabled, this module still provides some useful types and functions for working with Uiua code in an IDE or text editor.

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt,
    path::PathBuf,
    slice,
};

use crate::{
    ast::{Func, InlineMacro, Item, Modifier, ModuleKind, Ref, RefComponent, Word},
    ident_modifier_args, is_custom_glyph,
    lex::{CodeSpan, Sp},
    parse::parse,
    Assembly, BindingInfo, BindingKind, Compiler, DocComment, Ident, InputSrc, Inputs, PreEvalMode,
    Primitive, Purity, SafeSys, Shape, Signature, SysBackend, UiuaError, Value, CONSTANTS,
};

/// Kinds of span in Uiua code, meant to be used in the language server or other IDE tools
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanKind {
    Primitive(Primitive, Option<usize>),
    String,
    Number,
    Comment,
    OutputComment,
    Strand,
    Ident {
        /// The documentation of the identifier
        docs: Option<BindingDocs>,
        /// Whether the identifier is the original binding name
        original: bool,
    },
    Label,
    Signature,
    Whitespace,
    Placeholder(usize),
    Delimiter,
    FuncDelim(Signature, SetInverses),
    MacroDelim(usize),
    ImportSrc(ImportSrc),
    Subscript(Option<Primitive>, Option<usize>),
    Obverse(SetInverses),
}

/// Documentation information for a binding
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingDocs {
    /// The span of the binding name where it was defined
    pub src_span: CodeSpan,
    /// The comment of the binding
    pub comment: Option<DocComment>,
    /// Whether the binding is public
    pub is_public: bool,
    /// The specific binding kind
    pub kind: BindingDocsKind,
    /// An escape code used to type a glyph
    pub escape: Option<String>,
}

/// The kind of a binding
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingDocsKind {
    /// A constant
    Constant(Option<Value>),
    /// A function
    Function {
        /// The signature of the function
        sig: Signature,
        /// Whether the function is invertible
        invertible: bool,
        /// Whether the function is underable
        underable: bool,
        /// Whether the function is pure
        pure: bool,
    },
    /// A modifier
    Modifier(usize),
    /// A module
    Module {
        /// The signature of the module's `New` function
        sig: Option<Signature>,
    },
}

/// Span data extracted from Uiua code
pub struct Spans {
    /// The spans
    pub spans: Vec<Sp<SpanKind>>,
    /// The inputs used to build the spans
    pub inputs: Inputs,
    /// Top-level values for lines
    pub top_level_values: BTreeMap<usize, Vec<Value>>,
}

impl Spans {
    /// Get spans and their kinds from Uiua code
    pub fn from_input(input: &str) -> Self {
        Self::with_backend(input, SafeSys::default())
    }
    /// Get spans and their kinds from Uiua code with a custom backend
    pub fn with_backend(input: &str, backend: impl SysBackend) -> Self {
        let src = InputSrc::Str(0);
        let (items, _, _) = parse(input, src.clone(), &mut Inputs::default());
        let spanner = Spanner::new(src, input, backend);
        let spans = spanner.items_spans(&items);
        let inputs = spanner.asm.inputs;
        let top_level_values = spanner
            .code_meta
            .top_level_values
            .into_iter()
            .map(|(span, vals)| (span.start.line as usize, vals))
            .collect();
        Spans {
            spans,
            inputs,
            top_level_values,
        }
    }
    #[doc(hidden)]
    /// Get spans using the given compiler
    pub fn with_compiler(input: &str, compiler: &Compiler) -> Self {
        let mut compiler = compiler.clone();
        let src = InputSrc::Str(compiler.asm.inputs.strings.len().saturating_sub(1));
        let (items, _, _) = parse(input, src.clone(), &mut compiler.asm.inputs);
        let spanner = Spanner {
            src,
            asm: compiler.asm,
            code_meta: compiler.code_meta,
            errors: Vec::new(),
            diagnostics: Vec::new(),
        };
        let spans = spanner.items_spans(&items);
        let inputs = spanner.asm.inputs;
        let top_level_values = spanner
            .code_meta
            .top_level_values
            .into_iter()
            .map(|(span, vals)| (span.start.line as usize, vals))
            .collect();
        Spans {
            spans,
            inputs,
            top_level_values,
        }
    }
}

/// Code metadata for use in IDE tools
#[derive(Debug, Clone, Default)]
pub struct CodeMeta {
    /// A map of references to global bindings
    pub global_references: HashMap<CodeSpan, usize>,
    /// A map of references to shadowable constants
    pub constant_references: HashSet<Sp<Ident>>,
    /// Spans of functions and their signatures and whether they are explicit
    pub function_sigs: SigDecls,
    /// A map of macro invocations to their expansions
    pub macro_expansions: HashMap<CodeSpan, (Option<Ident>, String)>,
    /// A map of inline macro functions to their number of arguments
    pub inline_macros: HashMap<CodeSpan, usize>,
    /// A map of incomplete ref paths to their module's index
    pub incomplete_refs: HashMap<CodeSpan, usize>,
    /// A map of the spans of top-level lines to values
    pub top_level_values: HashMap<CodeSpan, Vec<Value>>,
    /// A map of strand spans
    pub strands: BTreeMap<CodeSpan, Vec<CodeSpan>>,
    /// A map of inner array spans
    pub array_inner_spans: BTreeMap<CodeSpan, Vec<CodeSpan>>,
    /// A map of array shapes
    pub array_shapes: BTreeMap<CodeSpan, Shape>,
    /// A map of module spans to their source
    pub import_srcs: HashMap<CodeSpan, ImportSrc>,
    /// A map of obverse spans to their set inverses
    pub obverses: HashMap<CodeSpan, SetInverses>,
}

/// Data for the signature of a function
#[derive(Debug, Clone, Copy)]
pub struct SigDecl {
    /// The signature itself
    pub sig: Signature,
    /// Whether the signature is explicitely declared
    pub explicit: bool,
    /// Whether the function is inline
    pub inline: bool,
    /// Inverses
    pub set_inverses: SetInverses,
}

/// Which inverses were set by `obverse`
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SetInverses {
    pub un: bool,
    pub anti: bool,
    pub under: bool,
}

impl SetInverses {
    /// Whether no inverses were set
    pub fn is_empty(&self) -> bool {
        !(self.un || self.anti || self.under)
    }
}

struct FormatSetInverses<'a>(SetInverses, [&'a str; 3]);
impl<'a> fmt::Display for FormatSetInverses<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let FormatSetInverses(set_inverses, names) = self;
        let count =
            set_inverses.un as usize + set_inverses.anti as usize + set_inverses.under as usize;
        if count == 0 {
            return Ok(());
        }
        write!(f, "Sets ")?;
        for (i, (is_set, name)) in [set_inverses.un, set_inverses.anti, set_inverses.under]
            .into_iter()
            .zip(names)
            .enumerate()
        {
            if !is_set {
                continue;
            }
            if i > 0 {
                match count {
                    2 => write!(f, " and ")?,
                    3 if i == 1 => write!(f, ", ")?,
                    3 if i == 2 => write!(f, ", and ")?,
                    _ => {}
                }
            }
            write!(f, "{name}")?;
        }
        write!(f, " inverse{} here", if count == 1 { "" } else { "s" })
    }
}

impl fmt::Display for SetInverses {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        FormatSetInverses(*self, ["° un", "⌝ anti", "⍜ under"]).fmt(f)
    }
}

/// The source of an imported module
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportSrc {
    /// A Git URL
    Git(String),
    /// A file path
    File(PathBuf),
}

pub(crate) type SigDecls = BTreeMap<CodeSpan, SigDecl>;

struct Spanner {
    src: InputSrc,
    asm: Assembly,
    code_meta: CodeMeta,
    #[allow(dead_code)]
    errors: Vec<UiuaError>,
    #[allow(dead_code)]
    diagnostics: Vec<crate::Diagnostic>,
}

impl Spanner {
    fn new(src: InputSrc, input: &str, backend: impl SysBackend) -> Self {
        let mut compiler = Compiler::with_backend(backend);
        compiler.pre_eval_mode(PreEvalMode::Lsp);
        let errors = match compiler.load_str_src(input, src.clone()) {
            Ok(_) => Vec::new(),
            Err(e) => e.into_multi(),
        };
        let diagnostics = compiler.take_diagnostics().into_iter().collect();
        Self {
            src,
            asm: compiler.asm,
            code_meta: compiler.code_meta,
            errors,
            diagnostics,
        }
    }
    fn inputs(&self) -> &Inputs {
        &self.asm.inputs
    }
    fn items_spans(&self, items: &[Item]) -> Vec<Sp<SpanKind>> {
        let mut spans = Vec::new();
        for item in items {
            match item {
                Item::Module(m) => {
                    spans.push(m.value.open_span.clone().sp(SpanKind::Delimiter));
                    match &m.value.kind {
                        ModuleKind::Named(name) => {
                            let binding_docs = self.binding_docs(&name.span);
                            spans.push(name.span.clone().sp(SpanKind::Ident {
                                docs: binding_docs,
                                original: true,
                            }));
                        }
                        ModuleKind::Test => {}
                    }
                    if let Some(line) = &m.value.imports {
                        spans.push(line.tilde_span.clone().sp(SpanKind::Delimiter));
                        for item in &line.items {
                            let binding_docs = self.reference_docs(&item.span);
                            spans.push(item.span.clone().sp(SpanKind::Ident {
                                docs: binding_docs,
                                original: false,
                            }));
                        }
                    }
                    spans.extend(self.items_spans(&m.value.items));
                    if let Some(close_span) = &m.value.close_span {
                        spans.push(close_span.clone().sp(SpanKind::Delimiter));
                    }
                }
                Item::Words(lines) => {
                    for line in lines {
                        spans.extend(self.words_spans(line))
                    }
                }
                Item::Binding(binding) => {
                    let binding_docs = self
                        .binding_docs(&binding.name.span)
                        .or_else(|| self.reference_docs(&binding.name.span));
                    spans.push(binding.name.span.clone().sp(SpanKind::Ident {
                        docs: binding_docs,
                        original: true,
                    }));
                    spans.push(binding.arrow_span.clone().sp(SpanKind::Delimiter));
                    if let Some(sig) = &binding.signature {
                        spans.push(sig.span.clone().sp(SpanKind::Signature));
                    }
                    spans.extend(self.words_spans(&binding.words));
                }
                Item::Data(data) => {
                    spans.push(data.init_span.clone().sp(SpanKind::Delimiter));
                    if let Some(name) = &data.name {
                        spans.push(name.span.clone().sp(SpanKind::Ident {
                            docs: self.binding_docs(&name.span),
                            original: true,
                        }));
                        if let Some(fields) = &data.fields {
                            spans.push(
                                name.span
                                    .clone()
                                    .end_to(&fields.open_span)
                                    .sp(SpanKind::Whitespace),
                            );
                        }
                    }
                    if let Some(fields) = &data.fields {
                        spans.push(fields.open_span.clone().sp(SpanKind::Delimiter));
                        let mut prev: Option<CodeSpan> = None;
                        for field in &fields.fields {
                            if let Some(prev) = prev {
                                let curr = field.span();
                                if prev.end.line == curr.start.line {
                                    spans.push(prev.end_to(&curr).sp(SpanKind::Whitespace))
                                }
                            }
                            spans.push(field.name.span.clone().sp(SpanKind::Ident {
                                docs: self.binding_docs(&field.name.span),
                                original: true,
                            }));
                            if let Some(validator) = &field.validator {
                                spans.push(validator.open_span.clone().sp(SpanKind::Delimiter));
                                spans.extend(self.words_spans(&validator.words));
                                if let Some(close_span) = &validator.close_span {
                                    spans.push(close_span.clone().sp(SpanKind::Delimiter));
                                }
                            }
                            if let Some(init) = &field.init {
                                spans.push(init.arrow_span.clone().sp(SpanKind::Delimiter));
                                spans.extend(self.words_spans(&init.words));
                            }
                            prev = Some(field.span());
                        }
                        if let Some(span) = &fields.close_span {
                            spans.push(span.clone().sp(SpanKind::Delimiter));
                        }
                    }
                    if let Some(words) = &data.func {
                        spans.extend(self.words_spans(words));
                    }
                }
                Item::Import(import) => {
                    if let Some(name) = &import.name {
                        let binding_docs = self.binding_docs(&name.span);
                        spans.push(name.span.clone().sp(SpanKind::Ident {
                            docs: binding_docs,
                            original: false,
                        }));
                    }
                    spans.push(import.tilde_span.clone().sp(SpanKind::Delimiter));
                    spans.push(
                        if let Some(src) = self.code_meta.import_srcs.get(&import.path.span) {
                            (import.path.span.clone()).sp(SpanKind::ImportSrc(src.clone()))
                        } else {
                            import.path.span.clone().sp(SpanKind::String)
                        },
                    );
                    for line in import.lines.iter().flatten() {
                        spans.push(line.tilde_span.clone().sp(SpanKind::Delimiter));
                        for item in &line.items {
                            let binding_docs = self.reference_docs(&item.span);
                            spans.push(item.span.clone().sp(SpanKind::Ident {
                                docs: binding_docs,
                                original: false,
                            }));
                        }
                    }
                }
            }
        }
        spans.sort_by_key(|sp| sp.span.start);
        spans
    }

    fn binding_docs(&self, span: &CodeSpan) -> Option<BindingDocs> {
        for binding in &self.asm.bindings {
            if binding.span != *span {
                continue;
            }
            return Some(self.make_binding_docs(binding));
        }
        None
    }

    fn reference_docs(&self, span: &CodeSpan) -> Option<BindingDocs> {
        // Look in global references
        if let Some(binding) = self
            .code_meta
            .global_references
            .get(span)
            .and_then(|i| self.asm.bindings.get(*i))
        {
            return Some(self.make_binding_docs(binding));
        }
        // Look in constant references
        for name in &self.code_meta.constant_references {
            if name.span != *span {
                continue;
            }
            let Some(constant) = CONSTANTS.iter().find(|c| c.name == name.value) else {
                continue;
            };
            let path = if let InputSrc::File(path) = &self.src {
                Some(&**path)
            } else {
                None
            };
            #[cfg(feature = "native_sys")]
            let sys = &crate::NativeSys;
            #[cfg(not(feature = "native_sys"))]
            let sys = &crate::SafeSys::new();
            let val = constant.value.resolve(path, sys);
            return Some(BindingDocs {
                src_span: span.clone(),
                comment: Some(constant.doc().into()),
                is_public: true,
                kind: BindingDocsKind::Constant(Some(val)),
                escape: None,
            });
        }
        None
    }

    fn make_binding_docs(&self, binfo: &BindingInfo) -> BindingDocs {
        let mut comment = binfo.comment.clone();
        if comment.is_none() {
            let name = binfo.span.as_str(&self.asm.inputs, |s| s.to_string());
            comment = match name.as_str() {
                "🦈" | "🏳️‍⚧️" => Some("Trans rights".into()),
                "🤠" => Some("This town ain't big enough for the ∩ of us".into()),
                "👽" => Some("Ayy, lmao".into()),
                "🐈" | "😺" | "😸" | "😹" | "😻" | "😼" | "😽" | "🙀" | "🐱‍👤" => {
                    Some("Meow".into())
                }
                "🐕" | "🐶" | "🦮" | "🐕‍🦺" => Some("Woof".into()),
                "🐖" | "🐷" | "🐽" /* | "👮" */ => Some("Oink".into()),
                "🐄" | "🐮" => Some("Moo".into()),
                "🐸" => Some("Ribbit".into()),
                "ඞ" => Some("SUS".into()),
                _ => None,
            };
        }
        if comment.is_none() {
            match &binfo.kind {
                BindingKind::Const(None) => comment = Some("constant".into()),
                BindingKind::Import(_) | BindingKind::Module(_) => comment = Some("module".into()),
                BindingKind::IndexMacro(_) | BindingKind::CodeMacro(_) => {
                    comment = Some("macro".into())
                }
                BindingKind::Func(_) => {}
                BindingKind::Const(_) => {}
            }
        }
        let kind = match &binfo.kind {
            BindingKind::Const(val) => BindingDocsKind::Constant(val.clone()),
            BindingKind::Func(f) => BindingDocsKind::Function {
                sig: f.sig,
                invertible: self.asm[f].un_inverse(&self.asm).is_ok(),
                underable: self.asm[f]
                    .under_inverse(Signature::new(1, 1), false, &self.asm)
                    .is_ok(),
                pure: self.asm[f].is_pure(Purity::Pure, &self.asm),
            },
            BindingKind::IndexMacro(args) => BindingDocsKind::Modifier(*args),
            BindingKind::CodeMacro(_) => {
                BindingDocsKind::Modifier(binfo.span.as_str(self.inputs(), ident_modifier_args))
            }
            BindingKind::Import(_) => BindingDocsKind::Module { sig: None },
            BindingKind::Module(m) => {
                let sig = if let Some(local) = m.names.get("Call").or_else(|| m.names.get("New")) {
                    self.asm.bindings[local.index].kind.sig()
                } else {
                    None
                };
                BindingDocsKind::Module { sig }
            }
        };
        let escape = binfo.span.as_str(&self.asm.inputs, |s| {
            is_custom_glyph(s).then(|| {
                let c = s.chars().next().unwrap();
                format!("\\\\{:x}", c as u32)
            })
        });
        BindingDocs {
            src_span: binfo.span.clone(),
            comment,
            is_public: binfo.public,
            kind,
            escape,
        }
    }

    fn words_spans(&self, words: &[Sp<Word>]) -> Vec<Sp<SpanKind>> {
        let mut spans = Vec::new();
        'words: for word in words {
            match &word.value {
                Word::Number(s, _) => {
                    for prim in Primitive::all().filter(|p| p.is_constant()) {
                        if prim.name().starts_with(s) || prim.to_string() == *s {
                            spans.push(word.span.clone().sp(SpanKind::Primitive(prim, None)));
                            continue 'words;
                        }
                    }
                    spans.push(word.span.clone().sp(SpanKind::Number))
                }
                Word::Char(_) | Word::String(_) | Word::FormatString(_) => {
                    spans.push(word.span.clone().sp(SpanKind::String))
                }
                Word::Label(_) => spans.push(word.span.clone().sp(SpanKind::Label)),
                Word::MultilineString(lines) => {
                    spans.extend((lines.iter()).map(|line| line.span.clone().sp(SpanKind::String)))
                }
                Word::MultilineFormatString(lines) => {
                    spans.extend((lines.iter()).map(|line| line.span.clone().sp(SpanKind::String)))
                }
                Word::Ref(r) => spans.extend(self.ref_spans(r)),
                Word::IncompleteRef { path, .. } => spans.extend(self.ref_path_spans(path)),
                Word::Strand(items) => {
                    for i in 0..items.len() {
                        let word = &items[i];
                        if i > 0 {
                            let prev = &items[i - 1];
                            spans.push(
                                CodeSpan {
                                    start: prev.span.end,
                                    end: word.span.start,
                                    src: word.span.src.clone(),
                                }
                                .sp(SpanKind::Strand),
                            );
                        }
                        spans.extend(self.words_spans(slice::from_ref(word)));
                    }
                }
                Word::Array(arr) => {
                    spans.push(word.span.just_start(self.inputs()).sp(SpanKind::Delimiter));
                    if let Some(sig) = &arr.signature {
                        spans.push(sig.span.clone().sp(SpanKind::Signature));
                    }
                    spans.extend(arr.lines.iter().flat_map(|w| self.words_spans(w)));
                    if arr.closed {
                        let end = word.span.just_end(self.inputs());
                        if end.as_str(self.inputs(), |s| s == "]")
                            || end.as_str(self.inputs(), |s| s == "}")
                        {
                            spans.push(end.sp(SpanKind::Delimiter));
                        }
                    }
                }
                Word::Func(func) => spans.extend(self.func_spans(func, &word.span)),
                Word::Pack(pack) => {
                    let mut kind = if let Some(inline) = pack
                        .branches
                        .first()
                        .and_then(|br| self.code_meta.function_sigs.get(&br.span))
                    {
                        SpanKind::FuncDelim(inline.sig, inline.set_inverses)
                    } else {
                        SpanKind::Delimiter
                    };
                    spans.push(word.span.just_start(self.inputs()).sp(kind.clone()));
                    for (i, branch) in pack.branches.iter().enumerate() {
                        let start_span = branch.span.just_start(self.inputs());
                        if i > 0 && start_span.as_str(self.inputs(), |s| s == "|") {
                            kind = if let Some(SigDecl {
                                sig,
                                set_inverses,
                                explicit: false,
                                ..
                            }) = self.code_meta.function_sigs.get(&branch.span)
                            {
                                SpanKind::FuncDelim(*sig, *set_inverses)
                            } else {
                                SpanKind::Delimiter
                            };
                            spans.push(start_span.sp(kind.clone()));
                        }
                        if let Some(sig) = &branch.value.signature {
                            spans.push(sig.span.clone().sp(SpanKind::Signature));
                        }
                        spans.extend(branch.value.lines.iter().flat_map(|w| self.words_spans(w)));
                    }
                    if pack.closed {
                        let end = word.span.just_end(self.inputs());
                        if end.as_str(self.inputs(), |s| s == ")") {
                            spans.push(end.sp(kind));
                        }
                    }
                }
                Word::Primitive(prim) => {
                    spans.push(word.span.clone().sp(SpanKind::Primitive(*prim, None)))
                }
                Word::Modified(m) => {
                    match &m.modifier.value {
                        Modifier::Primitive(Primitive::Obverse) => {
                            spans.push(m.modifier.span.clone().sp(
                                if let Some(set_inverses) =
                                    self.code_meta.obverses.get(&m.modifier.span)
                                {
                                    SpanKind::Obverse(*set_inverses)
                                } else {
                                    SpanKind::Primitive(Primitive::Obverse, None)
                                },
                            ))
                        }
                        Modifier::Primitive(p) => {
                            spans.push((m.modifier.span.clone()).sp(SpanKind::Primitive(*p, None)))
                        }
                        Modifier::Ref(r) => spans.extend(self.ref_spans(r)),
                        Modifier::Macro(mac) => {
                            spans.extend(self.func_spans(&mac.func.value, &mac.func.span));
                            let mac_delim_kind =
                                SpanKind::MacroDelim(ident_modifier_args(&mac.ident.value));
                            if let Some(span) = &mac.caret_span {
                                spans.push(span.clone().sp(mac_delim_kind.clone()));
                            }
                            let ident_span = (mac.ident.span.clone()).sp(mac_delim_kind);
                            spans.push(ident_span);
                        }
                    }
                    spans.extend(self.words_spans(&m.operands));
                }
                Word::Spaces | Word::BreakLine | Word::FlipLine => {
                    spans.push(word.span.clone().sp(SpanKind::Whitespace))
                }
                Word::Comment(_) | Word::SemanticComment(_) => {
                    spans.push(word.span.clone().sp(SpanKind::Comment))
                }
                Word::OutputComment { .. } => {
                    spans.push(word.span.clone().sp(SpanKind::OutputComment))
                }
                Word::Placeholder(op) => {
                    spans.push(word.span.clone().sp(SpanKind::Placeholder(*op)))
                }
                #[allow(clippy::match_single_binding)]
                Word::Subscripted(sub) => match &sub.word.value {
                    Word::Modified(m) => {
                        match &m.modifier.value {
                            Modifier::Primitive(p) => {
                                spans.push(
                                    m.modifier
                                        .span
                                        .clone()
                                        .sp(SpanKind::Primitive(*p, sub.n.value)),
                                );
                                spans.push(sub.n.clone().map(|n| SpanKind::Subscript(Some(*p), n)));
                            }
                            Modifier::Ref(r) => {
                                spans.extend(self.ref_spans(r));
                                spans.push(sub.n.clone().map(|n| SpanKind::Subscript(None, n)));
                            }
                            Modifier::Macro(mac) => {
                                spans.extend(self.func_spans(&mac.func.value, &mac.func.span));
                                let mac_delim_kind =
                                    SpanKind::MacroDelim(ident_modifier_args(&mac.ident.value));
                                if let Some(span) = &mac.caret_span {
                                    spans.push(span.clone().sp(mac_delim_kind.clone()));
                                }
                                let ident_span = (mac.ident.span.clone()).sp(mac_delim_kind);
                                spans.push(ident_span);
                                spans.push(sub.n.clone().map(|n| SpanKind::Subscript(None, n)));
                            }
                        }
                        spans.extend(self.words_spans(&m.operands));
                    }
                    Word::Primitive(p) => {
                        spans
                            .push((sub.word.span.clone()).sp(SpanKind::Primitive(*p, sub.n.value)));
                        spans.push(sub.n.clone().map(|n| SpanKind::Subscript(Some(*p), n)));
                    }
                    _ => {
                        spans.extend(self.words_spans(slice::from_ref(&sub.word)));
                        spans.push(sub.n.clone().map(|n| SpanKind::Subscript(None, n)));
                    }
                },
                Word::InlineMacro(InlineMacro {
                    ident,
                    caret_span,
                    func,
                }) => {
                    spans.extend(self.func_spans(&func.value, &func.span));
                    let mac_delim_kind = SpanKind::MacroDelim(ident_modifier_args(&ident.value));
                    if let Some(span) = caret_span {
                        spans.push(span.clone().sp(mac_delim_kind.clone()));
                    }
                    spans.push(ident.span.clone().sp(mac_delim_kind));
                }
            }
        }
        spans.retain(|sp| !sp.span.as_str(self.inputs(), str::is_empty));
        spans
    }
    fn ref_spans(&self, r: &Ref) -> Vec<Sp<SpanKind>> {
        let mut spans = self.ref_path_spans(&r.path);
        let docs = self.reference_docs(&r.name.span);
        spans.push(r.name.span.clone().sp(SpanKind::Ident {
            docs,
            original: false,
        }));
        spans
    }
    fn ref_path_spans(&self, path: &[RefComponent]) -> Vec<Sp<SpanKind>> {
        let mut spans = Vec::new();
        for comp in path {
            let docs = self.reference_docs(&comp.module.span);
            spans.push(comp.module.span.clone().sp(SpanKind::Ident {
                docs,
                original: false,
            }));
            spans.push(comp.tilde_span.clone().sp(SpanKind::Delimiter));
        }
        spans
    }
    fn func_spans(&self, func: &Func, span: &CodeSpan) -> Vec<Sp<SpanKind>> {
        let mut spans = Vec::new();
        let kind = if let Some(inline) = self.code_meta.function_sigs.get(span) {
            SpanKind::FuncDelim(inline.sig, inline.set_inverses)
        } else if let Some(margs) = self.code_meta.inline_macros.get(span) {
            SpanKind::MacroDelim(*margs)
        } else {
            SpanKind::Delimiter
        };
        spans.push(span.just_start(self.inputs()).sp(kind.clone()));
        if let Some(sig) = &func.signature {
            spans.push(sig.span.clone().sp(SpanKind::Signature));
        }
        spans.extend(func.lines.iter().flat_map(|w| self.words_spans(w)));
        if func.closed {
            let end = span.just_end(self.inputs());
            if end.as_str(self.inputs(), |s| s == ")") || end.as_str(self.inputs(), |s| s == "}") {
                spans.push(end.sp(kind));
            }
        }
        spans
    }
}

#[cfg(feature = "lsp")]
#[doc(hidden)]
pub use server::run_language_server;

#[cfg(feature = "lsp")]
mod server {
    use std::{env::current_dir, path::Path, sync::Arc};

    use dashmap::DashMap;
    use tower_lsp::{
        jsonrpc::{Error, Result},
        lsp_types::{
            request::{GotoDeclarationParams, GotoDeclarationResponse},
            *,
        },
        *,
    };

    use super::*;

    use crate::{
        format::{format_str, FormatConfig},
        is_ident_char,
        lex::{lex, Loc},
        primitive::{PrimClass, PrimDocFragment},
        AsciiToken, Assembly, BindingInfo, NativeSys, PrimDocLine, Span, Token, UiuaErrorKind,
    };

    pub struct LspDoc {
        pub input: String,
        pub spans: Vec<Sp<SpanKind>>,
        pub asm: Assembly,
        pub code_meta: CodeMeta,
        pub errors: Vec<UiuaError>,
        pub diagnostics: Vec<crate::Diagnostic>,
    }

    impl LspDoc {
        fn new(path: &Path, input: String) -> Self {
            let path = path
                .to_string_lossy()
                .strip_prefix("\\\\?\\")
                .map(PathBuf::from)
                .unwrap_or_else(|| path.to_path_buf());
            let path = current_dir()
                .ok()
                .and_then(|curr| pathdiff::diff_paths(&path, curr))
                .unwrap_or(path);
            let src = InputSrc::File(path.into());
            let (items, _, _) = parse(&input, src.clone(), &mut Inputs::default());
            let spanner = Spanner::new(src, &input, NativeSys);
            let spans = spanner.items_spans(&items);
            Self {
                input,
                spans,
                asm: spanner.asm,
                code_meta: spanner.code_meta,
                errors: spanner.errors,
                diagnostics: spanner.diagnostics,
            }
        }
    }

    #[doc(hidden)]
    pub fn run_language_server() {
        #[cfg(feature = "native_sys")]
        crate::sys::native::set_output_enabled(false);

        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(async {
                std::env::set_var("UIUA_NO_FORMAT", "1");

                let stdin = tokio::io::stdin();
                let stdout = tokio::io::stdout();

                let (service, socket) = LspService::new(|client| Backend {
                    client,
                    docs: DashMap::new(),
                });
                Server::new(stdin, stdout, socket)
                    .concurrency_level(1)
                    .serve(service)
                    .await;
            });
    }

    struct Backend {
        client: Client,
        docs: DashMap<Url, Arc<LspDoc>>,
    }

    const UIUA_NUMBER_STT: SemanticTokenType = SemanticTokenType::new("uiua_number");
    const UIUA_STRING_STT: SemanticTokenType = SemanticTokenType::new("uiua_string");
    const STACK_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("stack_function");
    const NOADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("noadic_function");
    const MONADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("monadic_function");
    const DYADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("dyadic_function");
    const TRIADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("triadic_function");
    const TETRADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("tetradic_function");
    const MONADIC_MODIFIER_STT: SemanticTokenType = SemanticTokenType::new("monadic_modifier");
    const DYADIC_MODIFIER_STT: SemanticTokenType = SemanticTokenType::new("dyadic_modifier");
    const TRIADIC_MODIFIER_STT: SemanticTokenType = SemanticTokenType::new("triadic_modifier");
    const MODULE_STT: SemanticTokenType = SemanticTokenType::new("uiua_module");

    const UIUA_SEMANTIC_TOKEN_TYPES: [SemanticTokenType; 13] = [
        SemanticTokenType::COMMENT,
        UIUA_NUMBER_STT,
        UIUA_STRING_STT,
        STACK_FUNCTION_STT,
        NOADIC_FUNCTION_STT,
        MONADIC_FUNCTION_STT,
        DYADIC_FUNCTION_STT,
        TRIADIC_FUNCTION_STT,
        TETRADIC_FUNCTION_STT,
        MONADIC_MODIFIER_STT,
        DYADIC_MODIFIER_STT,
        TRIADIC_MODIFIER_STT,
        MODULE_STT,
    ];

    const NO_STT: SemanticTokenType = SemanticTokenType::new("none");
    const GENERIC_SEMANTIC_TOKEN_TYPES: [SemanticTokenType; 13] = [
        SemanticTokenType::COMMENT,
        SemanticTokenType::NUMBER,
        SemanticTokenType::new("lifetime"),
        NO_STT,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::STRING,
        SemanticTokenType::METHOD,
        NO_STT,
        NO_STT,
        SemanticTokenType::TYPE,
        SemanticTokenType::KEYWORD,
        NO_STT,
        SemanticTokenType::NAMESPACE,
    ];

    #[tower_lsp::async_trait]
    impl LanguageServer for Backend {
        async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
            self.debug("Initializing Uiua language server").await;
            // self.client
            //     .log_message(
            //         MessageType::INFO,
            //         format!("Client capabilities: {:#?}", _params.capabilities),
            //     )
            //     .await;

            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Kind(
                        TextDocumentSyncKind::FULL,
                    )),
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
                    completion_provider: Some(CompletionOptions {
                        trigger_characters: Some(vec!["~".into(), "&".into()]),
                        ..Default::default()
                    }),
                    document_formatting_provider: Some(OneOf::Left(true)),
                    document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
                        first_trigger_character: ' '.to_string(),
                        more_trigger_character: Some(
                            "[{()}]|1234567890~!@#$%^&*_-+=.,<>/?\\\nABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                .chars()
                                .map(|c| c.to_string())
                                .collect(),
                        ),
                    }),
                    semantic_tokens_provider: Some(
                        SemanticTokensServerCapabilities::SemanticTokensOptions(
                            SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: UIUA_SEMANTIC_TOKEN_TYPES
                                        .into_iter()
                                        .chain(GENERIC_SEMANTIC_TOKEN_TYPES)
                                        .collect(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                        ),
                    ),
                    rename_provider: Some(OneOf::Left(true)),
                    definition_provider: Some(OneOf::Left(true)),
                    declaration_provider: Some(DeclarationCapability::Simple(true)),
                    references_provider: Some(OneOf::Left(true)),
                    diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                        DiagnosticOptions {
                            inter_file_dependencies: true,
                            ..Default::default()
                        },
                    )),
                    code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                    inlay_hint_provider: Some(OneOf::Left(true)),
                    inline_value_provider: Some(OneOf::Left(true)),
                    ..Default::default()
                },
                ..Default::default()
            })
        }

        async fn initialized(&self, _: InitializedParams) {
            self.debug("Uiua language server initialized").await;
        }

        async fn did_open(&self, params: DidOpenTextDocumentParams) {
            let path = uri_path(&params.text_document.uri);
            self.docs.insert(
                params.text_document.uri,
                LspDoc::new(&path, params.text_document.text).into(),
            );
        }

        async fn did_change(&self, params: DidChangeTextDocumentParams) {
            let path = uri_path(&params.text_document.uri);
            let doc = LspDoc::new(&path, params.content_changes[0].text.clone());
            self.docs.insert(params.text_document.uri, doc.into());
        }

        async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
            const SET_INVERSES_LINKS: [&str; 3] = [
                "[`° un`](https://uiua.org/docs/un)",
                "[`⌝ anti`](https://uiua.org/docs/anti)",
                "[`⍜ under`](https://uiua.org/docs/under)",
            ];

            let Some(doc) =
                (self.docs).get(&params.text_document_position_params.text_document.uri)
            else {
                return Ok(None);
            };
            let path = uri_path(&params.text_document_position_params.text_document.uri);
            let (line, col) =
                lsp_pos_to_uiua(params.text_document_position_params.position, &doc.input);
            // Hovering a primitive
            for sp in &doc.spans {
                if sp.span.contains_line_col(line, col) && sp.span.src == path {
                    match sp.value {
                        SpanKind::Primitive(prim, _) => {
                            return Ok(Some(Hover {
                                contents: HoverContents::Markup(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: full_prim_doc_markdown(prim),
                                }),
                                range: Some(uiua_span_to_lsp(&sp.span, &doc.asm.inputs)),
                            }));
                        }
                        SpanKind::Obverse(set_inverses) => {
                            let value = if set_inverses.is_empty() {
                                full_prim_doc_markdown(Primitive::Obverse)
                            } else {
                                format!(
                                    "{}\n\n{}",
                                    FormatSetInverses(set_inverses, SET_INVERSES_LINKS),
                                    full_prim_doc_markdown(Primitive::Obverse)
                                )
                            };
                            return Ok(Some(Hover {
                                contents: HoverContents::Markup(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value,
                                }),
                                range: Some(uiua_span_to_lsp(&sp.span, &doc.asm.inputs)),
                            }));
                        }
                        _ => {}
                    }
                }
            }
            // Hovering a binding
            for span_kind in &doc.spans {
                let SpanKind::Ident {
                    docs: Some(docs), ..
                } = &span_kind.value
                else {
                    continue;
                };
                if span_kind.span.contains_line_col(line, col) && span_kind.span.src == path {
                    let docs = span_kind.span.clone().sp(docs);
                    let span = docs.span;
                    let docs = docs.value;
                    let mut value = String::new();
                    value.push_str("```uiua\n");
                    span.as_str(&doc.asm.inputs, |s| value.push_str(s));
                    match docs.kind {
                        BindingDocsKind::Function { sig, .. } => {
                            value.push_str(&format!(" {sig}"));
                        }
                        _ => {}
                    }
                    if !docs.is_public && !matches!(docs.kind, BindingDocsKind::Module { .. }) {
                        value.push_str(" (private)");
                    }
                    match &docs.kind {
                        BindingDocsKind::Constant(Some(val)) => {
                            let s = val.show();
                            value.push('\n');
                            if s.len() < 250 {
                                value.push_str(&s);
                            } else {
                                value.push_str(&val.shape_string())
                            }
                        }
                        _ => {}
                    }
                    value.push_str("\n```");
                    if let Some(escape) = &docs.escape {
                        value.push_str(&format!("\n`{escape}`"));
                    }
                    match docs.kind {
                        BindingDocsKind::Function {
                            invertible,
                            underable,
                            pure,
                            ..
                        } => {
                            if pure || invertible || underable {
                                value.push_str("\n\n");
                                if pure {
                                    value.push_str("pure");
                                }
                                if invertible {
                                    if pure {
                                        value.push_str(" | ");
                                    }
                                    value.push_str("[`° un`](https://uiua.org/docs/un)");
                                }
                                if underable {
                                    if pure || invertible {
                                        value.push_str(" | ");
                                    }
                                    value.push_str("[`⍜ under`](https://uiua.org/docs/under)");
                                }
                            }
                        }
                        _ => {}
                    }
                    if let Some(comment) = &docs.comment {
                        value.push_str("\n\n");
                        if let Some(sig) = &comment.sig {
                            value.push('`');
                            value.push_str(&sig.to_string());
                            value.push_str("`\n\n");
                        }
                        value.push_str(&comment.text);
                    }
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value,
                        }),
                        range: Some(uiua_span_to_lsp(&span, &doc.asm.inputs)),
                    }));
                }
            }
            // Hovering an inline function
            if let Some((span, sig_decl)) = (doc.code_meta.function_sigs.iter())
                .filter(|(span, inline)| {
                    !inline.explicit && span.contains_line_col(line, col) && span.src == path
                })
                .min_by_key(|(span, _)| span.char_count())
            {
                let mut value = format!("```uiua\n{}\n```", sig_decl.sig);
                if !sig_decl.set_inverses.is_empty() {
                    value.push_str("\n\n");
                    value.push_str(
                        &FormatSetInverses(sig_decl.set_inverses, SET_INVERSES_LINKS).to_string(),
                    );
                }
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                    range: Some(uiua_span_to_lsp(span, &doc.asm.inputs)),
                }));
            }
            // Hovering an array
            for (span, arr_meta) in &doc.code_meta.array_shapes {
                if span.contains_line_col(line, col) && span.src == path {
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!("`{}`", arr_meta),
                        }),
                        range: Some(uiua_span_to_lsp(span, &doc.asm.inputs)),
                    }));
                }
            }
            // Hovering a git import
            for (span, src) in &doc.code_meta.import_srcs {
                if span.contains_line_col(line, col) && span.src == path {
                    match src {
                        ImportSrc::Git(url) => {
                            return Ok(Some(Hover {
                                contents: HoverContents::Markup(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: format!("[View Git repository]({url})"),
                                }),
                                range: Some(uiua_span_to_lsp(span, &doc.asm.inputs)),
                            }))
                        }
                        ImportSrc::File(_) => {}
                    };
                }
            }

            Ok(None)
        }

        async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
            fn make_completion(
                doc: &LspDoc,
                name: String,
                span: &CodeSpan,
                binding: &BindingInfo,
            ) -> CompletionItem {
                let kind = match &binding.kind {
                    BindingKind::Const(Some(val)) if val.meta().map_keys.is_some() => {
                        CompletionItemKind::STRUCT
                    }
                    BindingKind::Const(_) => CompletionItemKind::CONSTANT,
                    BindingKind::Func(_) => CompletionItemKind::FUNCTION,
                    BindingKind::IndexMacro(_) | BindingKind::CodeMacro(_) => {
                        CompletionItemKind::FUNCTION
                    }
                    BindingKind::Import(_) | BindingKind::Module(_) => CompletionItemKind::MODULE,
                };
                CompletionItem {
                    label: name.clone(),
                    kind: Some(kind),
                    label_details: Some(CompletionItemLabelDetails {
                        description: (binding.kind.sig())
                            .map(|sig| format!("{:<4}", sig.to_string())),
                        ..Default::default()
                    }),
                    documentation: binding.comment.as_ref().map(|c| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: c.text.to_string(),
                        })
                    }),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range: uiua_span_to_lsp(span, &doc.asm.inputs),
                        new_text: name,
                    })),
                    ..Default::default()
                }
            }

            let doc_uri = &params.text_document_position.text_document.uri;
            let doc = if let Some(doc) = self.doc(doc_uri) {
                doc
            } else {
                return Ok(None);
            };
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position, &doc.input);

            // Find an incomplete ref path at the cursor position
            if let Some((span, index)) =
                doc.code_meta.incomplete_refs.iter().find(|(span, _)| {
                    span.end.line as usize == line && span.end.col as usize == col
                })
            {
                match &doc.asm.bindings[*index].kind {
                    BindingKind::Import(module) => {
                        let mut completions = Vec::new();
                        let mut span = span.clone();
                        span.start = span.end;
                        for binding in self.bindings_in_file(doc_uri, module) {
                            if !binding.public {
                                continue;
                            }
                            let item_name = binding.span.as_str(&doc.asm.inputs, |s| s.to_string());
                            completions.push(make_completion(&doc, item_name, &span, &binding));
                        }
                        return Ok(Some(CompletionResponse::Array(completions)));
                    }
                    BindingKind::Module(module) => {
                        let mut completions = Vec::new();
                        let mut span = span.clone();
                        span.start = span.end;
                        for (name, local) in &module.names {
                            if !local.public {
                                continue;
                            }
                            let item_name = name.to_string();
                            let binfo = doc.asm.bindings.get(local.index).unwrap();
                            completions.push(make_completion(&doc, item_name, &span, binfo));
                        }
                        return Ok(Some(CompletionResponse::Array(completions)));
                    }
                    _ => (),
                }
            }

            // Find the span at the cursor position
            let Some(sp) = (doc.spans.iter()).find(|sp| sp.span.contains_line_col(line, col))
            else {
                return Ok(None);
            };
            // Don't complete identifiers for new bindings
            if let SpanKind::Ident { original: true, .. } = &sp.value {
                return Ok(None);
            }

            let Ok(token) = std::str::from_utf8(&doc.input.as_bytes()[sp.span.byte_range()]) else {
                return Ok(None);
            };

            // Collect primitive completions
            let mut completions: Vec<_> = Primitive::non_deprecated()
                .filter(|p| p.name().starts_with(token))
                .map(|prim| {
                    CompletionItem {
                        label: prim.format().to_string(),
                        label_details: if let Primitive::Sys(op) = prim {
                            Some(CompletionItemLabelDetails {
                                description: Some(format!(
                                    "{} {:<4}",
                                    op.long_name(),
                                    Signature::new(op.args(), op.outputs()).to_string()
                                )),
                                ..Default::default()
                            })
                        } else {
                            prim.sig().map(|sig| CompletionItemLabelDetails {
                                description: Some(format!("{:<4}", sig.to_string())),
                                ..Default::default()
                            })
                        },
                        insert_text: prim.glyph().map(|c| c.to_string()),
                        kind: Some(if prim.is_constant() {
                            CompletionItemKind::CONSTANT
                        } else {
                            CompletionItemKind::FUNCTION
                        }),
                        detail: Some(prim.doc().short_text().to_string()),
                        documentation: Some(Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: full_prim_doc_markdown(prim),
                        })),
                        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                            range: uiua_span_to_lsp(&sp.span, &doc.asm.inputs),
                            new_text: prim
                                .glyph()
                                .map(|c| c.to_string())
                                .unwrap_or_else(|| prim.name().to_string()),
                        })),
                        insert_text_mode: if prim.glyph().is_none() {
                            // Insert a space before the completion if the token is not a glyph
                            Some(InsertTextMode::ADJUST_INDENTATION)
                        } else {
                            None
                        },
                        sort_text: Some(format!(
                            "{} {}",
                            if prim.glyph().is_some() { "0" } else { "1" },
                            prim.name()
                        )),
                        ..Default::default()
                    }
                })
                .collect();

            // Collect binding completions
            for binding in self.bindings_in_file(doc_uri, &uri_path(doc_uri)) {
                let Some(name) = binding.span.try_as_str(&doc.asm.inputs, |s| s.to_string()) else {
                    continue;
                };

                if let BindingKind::Import(module) = &binding.kind {
                    for binding in self.bindings_in_file(doc_uri, module) {
                        if !binding.public {
                            continue;
                        }
                        let item_name = binding.span.as_str(&doc.asm.inputs, |s| s.to_string());
                        if !item_name.to_lowercase().starts_with(&token.to_lowercase()) {
                            continue;
                        }
                        completions.push(make_completion(
                            &doc,
                            format!("{name}~{item_name}"),
                            &sp.span,
                            &binding,
                        ));
                    }
                }

                if !name.to_lowercase().starts_with(&token.to_lowercase()) {
                    continue;
                }
                completions.push(make_completion(&doc, name, &sp.span, &binding));
            }

            // Collect constant completions
            for constant in &CONSTANTS {
                if !constant
                    .name
                    .to_lowercase()
                    .starts_with(&token.to_lowercase())
                {
                    continue;
                }
                completions.push(CompletionItem {
                    label: constant.name.into(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    detail: Some(constant.doc().into()),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range: uiua_span_to_lsp(&sp.span, &doc.asm.inputs),
                        new_text: constant.name.into(),
                    })),
                    ..Default::default()
                });
            }

            Ok(Some(CompletionResponse::Array(completions)))
        }

        async fn formatting(
            &self,
            params: DocumentFormattingParams,
        ) -> Result<Option<Vec<TextEdit>>> {
            let doc = if let Some(doc) = self.doc(&params.text_document.uri) {
                doc
            } else {
                return Ok(None);
            };
            match format_str(&doc.input, &FormatConfig::find().unwrap_or_default()) {
                Ok(formatted) => {
                    let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX));
                    Ok(Some(vec![TextEdit {
                        range,
                        new_text: formatted.output,
                    }]))
                }
                Err(e) => {
                    let mut error = Error::parse_error();
                    error.message = e.to_string().into();
                    Err(error)
                }
            }
        }

        async fn on_type_formatting(
            &self,
            params: DocumentOnTypeFormattingParams,
        ) -> Result<Option<Vec<TextEdit>>> {
            // Skip if disabled
            let config = self
                .client
                .configuration(vec![ConfigurationItem {
                    scope_uri: Some(params.text_document_position.text_document.uri.clone()),
                    section: Some("uiua.format.onTypeFormatting".into()),
                }])
                .await
                .unwrap_or_default();

            let enabled = if let [serde_json::Value::Bool(enabled)] = config.as_slice() {
                *enabled
            } else {
                true
            };
            if !enabled {
                return Ok(None);
            }

            // Get document
            let Some(doc) = self
                .docs
                .get(&params.text_document_position.text_document.uri)
            else {
                return Ok(None);
            };

            // Check if in a string or comment
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position, &doc.input);
            for span in &doc.spans {
                if matches!(
                    span.value,
                    SpanKind::String | SpanKind::Comment | SpanKind::Label
                ) && span.span.contains_line_col(line, col)
                {
                    return Ok(None);
                }
            }

            // Get ident
            let pos = params.text_document_position.position;
            let is_newline = params.ch == "\n";
            let line = if is_newline {
                pos.line.saturating_sub(1)
            } else {
                pos.line
            };
            let Some(line_str) = doc.input.lines().nth(line as usize) else {
                return Ok(None);
            };
            let line16: Vec<u16> = line_str.encode_utf16().collect();
            let col = if is_newline {
                line16.len() as u32
            } else {
                pos.character.saturating_sub(1)
            };
            let before = String::from_utf16(&line16[..col as usize]).unwrap();
            let mut ident = (before.chars().rev())
                .take_while(|&c| is_ident_char(c))
                .collect::<String>();
            ident = ident.chars().rev().collect();

            // Get formatted
            let mut start = col.saturating_sub(ident.encode_utf16().count() as u32);
            let mut formatted = String::new();
            if ident.is_empty() {
                let mut ascii_prims: Vec<_> = Primitive::non_deprecated()
                    .filter_map(|p| p.ascii().map(|a| (p, a.to_string())))
                    .collect();
                ascii_prims.sort_by_key(|(_, a)| a.len());
                ascii_prims.reverse();
                for (prim, ascii) in ascii_prims {
                    if before.ends_with(&ascii) {
                        formatted.push_str(&prim.to_string());
                        start = start.saturating_sub(ascii.encode_utf16().count() as u32);
                        break;
                    }
                }
            } else if let Some(prims) = Primitive::from_format_name_multi(&ident) {
                for (p, _) in prims {
                    formatted.push_str(&p.to_string());
                }
            }
            if formatted.is_empty() {
                return Ok(None);
            }

            // Adjust range
            let mut end = pos;
            if params.ch.chars().all(|c| c.is_whitespace()) {
                formatted.push_str(&params.ch);
            } else {
                end.character = end.character.saturating_sub(1);
            }
            let start = Position {
                line,
                character: start,
            };

            Ok(Some(vec![TextEdit {
                range: Range { start, end },
                new_text: formatted,
            }]))
        }

        async fn semantic_tokens_full(
            &self,
            params: SemanticTokensParams,
        ) -> Result<Option<SemanticTokensResult>> {
            let Some(doc) = self.doc(&params.text_document.uri) else {
                return Ok(None);
            };

            let config = self
                .client
                .configuration(vec![ConfigurationItem {
                    scope_uri: Some(params.text_document.uri.clone()),
                    section: Some("uiua.semanticHighlighting.customTokenTypes".into()),
                }])
                .await
                .unwrap_or_default();
            let custom_highlighting = if let [serde_json::Value::Bool(enabled)] = config.as_slice()
            {
                *enabled
            } else {
                true
            };

            let mut tokens = Vec::new();
            let mut prev_line = 0;
            let mut prev_char = 0;
            let for_prim = |p: Primitive, sub: Option<usize>| {
                let args = p.subscript_sig(sub).map(|sig| sig.args).or(p.args());
                Some(match p.class() {
                    PrimClass::Stack | PrimClass::Debug | PrimClass::Planet
                        if p.modifier_args().is_none() =>
                    {
                        STACK_FUNCTION_STT
                    }
                    PrimClass::Constant => UIUA_NUMBER_STT,
                    _ if p.modifier_args() == Some(1) => MONADIC_MODIFIER_STT,
                    _ if p.modifier_args() == Some(2) => DYADIC_MODIFIER_STT,
                    _ if p.modifier_args() == Some(3) => TRIADIC_MODIFIER_STT,
                    _ if args == Some(0) => NOADIC_FUNCTION_STT,
                    _ if args == Some(1) => MONADIC_FUNCTION_STT,
                    _ if args == Some(2) => DYADIC_FUNCTION_STT,
                    _ if args == Some(3) => TRIADIC_FUNCTION_STT,
                    _ if args == Some(4) => TETRADIC_FUNCTION_STT,
                    _ => return None,
                })
            };
            for sp in &doc.spans {
                let token_type = match &sp.value {
                    SpanKind::String => UIUA_STRING_STT,
                    SpanKind::Number => UIUA_NUMBER_STT,
                    SpanKind::Comment => SemanticTokenType::COMMENT,
                    SpanKind::Primitive(p, sub) => {
                        let Some(stt) = for_prim(*p, *sub) else {
                            continue;
                        };
                        stt
                    }
                    SpanKind::Obverse(_) => for_prim(Primitive::Obverse, None).unwrap(),
                    SpanKind::Ident {
                        docs: Some(docs), ..
                    } => match docs.kind {
                        BindingDocsKind::Constant(_) => continue,
                        BindingDocsKind::Function { sig, .. } => match sig.args {
                            0 => NOADIC_FUNCTION_STT,
                            1 => MONADIC_FUNCTION_STT,
                            2 => DYADIC_FUNCTION_STT,
                            3 => TRIADIC_FUNCTION_STT,
                            4 => TETRADIC_FUNCTION_STT,
                            _ => continue,
                        },
                        BindingDocsKind::Modifier(margs) => match margs {
                            1 => MONADIC_MODIFIER_STT,
                            2 => DYADIC_MODIFIER_STT,
                            3 => TRIADIC_MODIFIER_STT,
                            _ => continue,
                        },
                        BindingDocsKind::Module { .. } => MODULE_STT,
                    },
                    SpanKind::Subscript(Some(prim), n) => {
                        let Some(stt) = for_prim(*prim, *n) else {
                            continue;
                        };
                        stt
                    }
                    _ => continue,
                };
                let mut token_type = UIUA_SEMANTIC_TOKEN_TYPES
                    .iter()
                    .position(|t| t == &token_type)
                    .unwrap() as u32;
                if !custom_highlighting {
                    token_type += UIUA_SEMANTIC_TOKEN_TYPES.len() as u32;
                }
                let span = &sp.span;
                let start = uiua_span_to_lsp(span, &doc.asm.inputs).start;
                let delta_start = if start.line == prev_line {
                    start.character - prev_char
                } else {
                    start.character
                };
                let length = span.as_str(&doc.asm.inputs, |s| s.encode_utf16().count()) as u32;
                let token = SemanticToken {
                    delta_line: start.line - prev_line,
                    delta_start,
                    length,
                    token_type,
                    token_modifiers_bitset: 0,
                };
                tokens.push(token);
                prev_line = start.line;
                prev_char = start.character;
            }
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        }

        async fn code_action(
            &self,
            params: CodeActionParams,
        ) -> Result<Option<CodeActionResponse>> {
            let Some(doc) = self.doc(&params.text_document.uri) else {
                return Ok(None);
            };
            let (line, col) = lsp_pos_to_uiua(params.range.start, &doc.input);
            let path = uri_path(&params.text_document.uri);
            let mut actions = Vec::new();

            // Add explicit signature
            if let Some((span, inline)) = (doc.code_meta.function_sigs.iter())
                .filter(|(span, inline)| {
                    !inline.explicit && span.contains_line_col(line, col) && span.src == path
                })
                .min_by_key(|(span, _)| span.char_count())
            {
                let mut insertion_span = span.just_start(&doc.asm.inputs);
                if span.as_str(&doc.asm.inputs, |s| s.starts_with('(')) {
                    if span.start.line as usize == line && span.start.col as usize == col {
                        // Binding with single inline function
                        insertion_span.end = insertion_span.start;
                    } else {
                        // Inline function
                        insertion_span.start = insertion_span.end;
                    }
                } else {
                    // Binding without single inline function
                    insertion_span.end = insertion_span.start;
                }
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: "Add explicit signature".into(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(
                            [(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: uiua_span_to_lsp(&insertion_span, &doc.asm.inputs),
                                    new_text: format!("{} ", inline.sig),
                                }],
                            )]
                            .into(),
                        ),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
            }

            // Expand macro
            for (span, (name, expanded)) in &doc.code_meta.macro_expansions {
                if !span.contains_line_col(line, col) || span.src != path {
                    continue;
                }
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: if let Some(name) = name {
                        format!("Expand macro {name}")
                    } else {
                        "Expand macro".into()
                    },
                    kind: Some(CodeActionKind::REFACTOR_INLINE),
                    edit: Some(WorkspaceEdit {
                        changes: Some(
                            [(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: uiua_span_to_lsp(span, &doc.asm.inputs),
                                    new_text: expanded.clone(),
                                }],
                            )]
                            .into(),
                        ),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
            }

            // Remove output comment
            for span in &doc.spans {
                if !span.span.contains_line_col(line, col) || span.span.src != path {
                    continue;
                }
                if let SpanKind::OutputComment = &span.value {
                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: "Remove output comment".into(),
                        kind: Some(CodeActionKind::QUICKFIX),
                        edit: Some(WorkspaceEdit {
                            changes: Some(
                                [(
                                    params.text_document.uri.clone(),
                                    vec![TextEdit {
                                        range: uiua_span_to_lsp(&span.span, &doc.asm.inputs),
                                        new_text: "".into(),
                                    }],
                                )]
                                .into(),
                            ),
                            ..Default::default()
                        }),
                        ..Default::default()
                    }));
                }
            }

            // Convert to array syntax
            for (span, parts) in &doc.code_meta.strands {
                if !span.contains_line_col(line, col) || span.src != path {
                    continue;
                }
                let mut new_text = "[".to_string();
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        new_text.push(' ');
                    }
                    part.as_str(&doc.asm.inputs, |s| new_text.push_str(s));
                }
                new_text.push(']');
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: "Convert to array syntax".into(),
                    kind: Some(CodeActionKind::REFACTOR_REWRITE),
                    edit: Some(WorkspaceEdit {
                        changes: Some(
                            [(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: uiua_span_to_lsp(span, &doc.asm.inputs),
                                    new_text,
                                }],
                            )]
                            .into(),
                        ),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
            }

            // Convert to strand syntax
            for (span, parts) in &doc.code_meta.array_inner_spans {
                if !span.contains_line_col(line, col) || span.src != path {
                    continue;
                }
                let mut new_text = String::new();
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        new_text.push('_');
                    }
                    part.as_str(&doc.asm.inputs, |s| new_text.push_str(s));
                }
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: "Convert to strand syntax".into(),
                    kind: Some(CodeActionKind::REFACTOR_REWRITE),
                    edit: Some(WorkspaceEdit {
                        changes: Some(
                            [(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: uiua_span_to_lsp(span, &doc.asm.inputs),
                                    new_text,
                                }],
                            )]
                            .into(),
                        ),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
            }

            // Add experimental
            if !doc.input.contains("# Experimental!") {
                for error in &doc.errors {
                    let UiuaErrorKind::Run { message, .. } = &error.kind else {
                        continue;
                    };
                    let Span::Code(span) = &message.span else {
                        continue;
                    };
                    if span.src != path || !message.value.contains("# Experimental!") {
                        continue;
                    }
                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: "Add # Experimental! comment".into(),
                        kind: Some(CodeActionKind::QUICKFIX),
                        edit: Some(WorkspaceEdit {
                            changes: Some(
                                [(
                                    params.text_document.uri.clone(),
                                    vec![TextEdit {
                                        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                                        new_text: "# Experimental!\n".into(),
                                    }],
                                )]
                                .into(),
                            ),
                            ..Default::default()
                        }),
                        ..Default::default()
                    }));
                }
            }

            Ok(if actions.is_empty() {
                None
            } else {
                Some(actions)
            })
        }

        async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
            let Some(doc) = (self.docs).get(&params.text_document_position.text_document.uri)
            else {
                return Ok(None);
            };
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position, &doc.input);
            let path = uri_path(&params.text_document_position.text_document.uri);
            let mut binding: Option<(&BindingInfo, usize)> = None;
            // Check for span in bindings
            for (i, gb) in doc.asm.bindings.iter().enumerate() {
                if gb.span.contains_line_col(line, col) && gb.span.src == path {
                    binding = Some((gb, i));
                    break;
                }
            }
            // Check for span in binding references
            if binding.is_none() {
                for (name_span, index) in &doc.code_meta.global_references {
                    if name_span.contains_line_col(line, col) && name_span.src == path {
                        binding = Some((&doc.asm.bindings[*index], *index));
                        break;
                    }
                }
            }
            let Some((binding, index)) = binding else {
                return Ok(None);
            };
            // Collect edits
            let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
            changes.insert(
                match &binding.span.src {
                    InputSrc::Str(_) | InputSrc::Macro(_) => {
                        params.text_document_position.text_document.uri.clone()
                    }
                    InputSrc::File(file) => path_to_uri(file)?,
                },
                vec![TextEdit {
                    range: uiua_span_to_lsp(&binding.span, &doc.asm.inputs),
                    new_text: params.new_name.clone(),
                }],
            );
            for entry in &self.docs {
                let uri = entry.key();
                let doc = entry.value();
                for (name_span, idx) in &doc.code_meta.global_references {
                    if *idx == index {
                        let uri = match &name_span.src {
                            InputSrc::Str(_) | InputSrc::Macro(_) => uri.clone(),
                            InputSrc::File(file) => path_to_uri(file)?,
                        };
                        changes.entry(uri).or_default().push(TextEdit {
                            range: uiua_span_to_lsp(name_span, &doc.asm.inputs),
                            new_text: params.new_name.clone(),
                        });
                    }
                }
            }
            Ok(Some(WorkspaceEdit {
                changes: Some(changes.clone()),
                document_changes: None,
                change_annotations: None,
            }))
        }

        async fn goto_definition(
            &self,
            params: GotoDefinitionParams,
        ) -> Result<Option<GotoDefinitionResponse>> {
            let Some(doc) =
                (self.docs).get(&params.text_document_position_params.text_document.uri)
            else {
                return Ok(None);
            };
            let position = params.text_document_position_params.position;
            let (line, col) = lsp_pos_to_uiua(position, &doc.input);
            let path = uri_path(&params.text_document_position_params.text_document.uri);
            // Check global references
            for (name_span, idx) in &doc.code_meta.global_references {
                if name_span.contains_line_col(line, col) && name_span.src == path {
                    let binding = &doc.asm.bindings[*idx];
                    let uri = match &binding.span.src {
                        InputSrc::Str(_) | InputSrc::Macro(_) => {
                            params.text_document_position_params.text_document.uri
                        }
                        InputSrc::File(file) => path_to_uri(file)?,
                    };
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri,
                        range: uiua_span_to_lsp(&binding.span, &doc.asm.inputs),
                    })));
                }
            }
            // Check import sources
            for (span, src) in &doc.code_meta.import_srcs {
                if span.contains_line_col(line, col) && span.src == path {
                    match src {
                        ImportSrc::File(path) => {
                            return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                uri: path_to_uri(path)?,
                                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                            })))
                        }
                        ImportSrc::Git(_) => {}
                    };
                }
            }
            Ok(None)
        }

        async fn goto_declaration(
            &self,
            params: GotoDeclarationParams,
        ) -> Result<Option<GotoDeclarationResponse>> {
            let Some(doc) =
                (self.docs).get(&params.text_document_position_params.text_document.uri)
            else {
                return Ok(None);
            };
            let position = params.text_document_position_params.position;
            let (line, col) = lsp_pos_to_uiua(position, &doc.input);
            let path = uri_path(&params.text_document_position_params.text_document.uri);
            for (name_span, idx) in &doc.code_meta.global_references {
                if name_span.contains_line_col(line, col) && name_span.src == path {
                    let binding = &doc.asm.bindings[*idx];
                    let uri = match &binding.span.src {
                        InputSrc::Str(_) | InputSrc::Macro(_) => {
                            params.text_document_position_params.text_document.uri
                        }
                        InputSrc::File(file) => path_to_uri(file)?,
                    };
                    return Ok(Some(GotoDeclarationResponse::Scalar(Location {
                        uri,
                        range: uiua_span_to_lsp(&binding.span, &doc.asm.inputs),
                    })));
                }
            }
            Ok(None)
        }

        async fn diagnostic(
            &self,
            params: DocumentDiagnosticParams,
        ) -> Result<DocumentDiagnosticReportResult> {
            let mut diagnostics = Vec::new();
            let Some(doc) = self.doc(&params.text_document.uri) else {
                return Ok(DocumentDiagnosticReportResult::Report(
                    DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                        related_documents: None,
                        full_document_diagnostic_report: FullDocumentDiagnosticReport {
                            result_id: None,
                            items: diagnostics,
                        },
                    }),
                ));
            };

            fn path_locs<'a>(span: &'a CodeSpan, path: &Path) -> Option<&'a CodeSpan> {
                match &span.src {
                    InputSrc::File(file) if file.canonicalize().ok().as_deref() == Some(path) => {
                        Some(span)
                    }
                    InputSrc::Macro(span) => path_locs(span, path),
                    _ => None,
                }
            }

            let path = uri_path(&params.text_document.uri);
            let range = |err: &UiuaError, span: &Span| -> Option<Range> {
                let span = match span {
                    Span::Code(span) => path_locs(span, &path)?,
                    Span::Builtin => err.trace.iter().find_map(|frame| match &frame.span {
                        Span::Code(span) => Some(span),
                        _ => None,
                    })?,
                };
                Some(uiua_span_to_lsp(span, &doc.asm.inputs))
            };
            for err in &doc.errors {
                match &err.kind {
                    UiuaErrorKind::Run { message, .. } => {
                        let Some(range) = range(err, &message.span) else {
                            continue;
                        };
                        diagnostics.push(Diagnostic {
                            severity: Some(DiagnosticSeverity::ERROR),
                            range,
                            message: message.value.clone(),
                            ..Default::default()
                        });
                    }
                    UiuaErrorKind::Parse(errors, _) => {
                        for parse_err in errors {
                            let Some(range) = range(err, &Span::Code(parse_err.span.clone()))
                            else {
                                continue;
                            };
                            diagnostics.push(Diagnostic {
                                severity: Some(DiagnosticSeverity::ERROR),
                                range,
                                message: parse_err.value.to_string(),
                                ..Default::default()
                            });
                        }
                    }
                    UiuaErrorKind::Throw(value, span, _) => {
                        let Some(range) = range(err, span) else {
                            continue;
                        };
                        diagnostics.push(Diagnostic {
                            severity: Some(DiagnosticSeverity::ERROR),
                            range,
                            message: value.format(),
                            ..Default::default()
                        })
                    }
                    _ => {}
                }
            }

            for diag in &doc.diagnostics {
                let sev = match diag.kind {
                    crate::DiagnosticKind::Warning => DiagnosticSeverity::WARNING,
                    crate::DiagnosticKind::Advice
                    | crate::DiagnosticKind::Style
                    | crate::DiagnosticKind::Info => DiagnosticSeverity::INFORMATION,
                };
                if let Span::Code(span) = &diag.span {
                    diagnostics.push(Diagnostic {
                        severity: Some(sev),
                        range: uiua_span_to_lsp(span, &doc.asm.inputs),
                        message: diag.message.clone(),
                        ..Default::default()
                    });
                }
            }

            Ok(DocumentDiagnosticReportResult::Report(
                DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        result_id: None,
                        items: diagnostics,
                    },
                }),
            ))
        }

        async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
            let Some(doc) = self.doc(&params.text_document.uri) else {
                return Ok(None);
            };
            let config = self
                .client
                .configuration(
                    [
                        "bindingSignatureHints",
                        "inlineSignatureHints",
                        "inlineHintMinLength",
                        "values",
                    ]
                    .iter()
                    .map(|s| ConfigurationItem {
                        scope_uri: Some(params.text_document.uri.clone()),
                        section: Some(format!("uiua.inlayHints.{s}")),
                    })
                    .collect(),
                )
                .await
                .unwrap_or_default();
            let (binding_sigs, inline_sigs, min_length, show_values) = if let [serde_json::Value::Bool(
                    binding_sigs,
                ), serde_json::Value::Bool(
                    inline_sigs,
                ), serde_json::Value::Number(
                    min_length,
                ), serde_json::Value::Bool(
                    show_values,
                )] = config.as_slice()
            {
                (
                    *binding_sigs,
                    *inline_sigs,
                    min_length.as_u64().unwrap_or(1) as usize,
                    *show_values,
                )
            } else {
                (true, true, 3, true)
            };
            let path = uri_path(&params.text_document.uri);
            // Signature hints
            let mut hints = Vec::new();
            for (span, decl) in &doc.code_meta.function_sigs {
                if span.src != path {
                    continue;
                }
                let is_too_short = || {
                    let mut tokens =
                        span.as_str(&doc.asm.inputs, |s| lex(s, (), &mut Inputs::default()).0);
                    if tokens.first().is_some_and(|t| {
                        matches!(
                            t.value,
                            Token::Simple(AsciiToken::OpenParen | AsciiToken::Bar)
                        )
                    }) {
                        tokens.remove(0);
                    }
                    if tokens
                        .last()
                        .is_some_and(|t| t.value == Token::Simple(AsciiToken::CloseParen))
                    {
                        tokens.pop();
                    }
                    let code_tok_count = tokens
                        .iter()
                        .filter(|tok| {
                            !matches!(tok.value, Token::Spaces | Token::Newline | Token::Comment)
                        })
                        .count();
                    code_tok_count < min_length
                };
                if decl.explicit
                    || !decl.inline && decl.sig == (0, 1)
                    || decl.inline && !inline_sigs
                    || !decl.inline && !binding_sigs
                    || is_too_short()
                {
                    continue;
                }
                let sig = decl.sig.to_string();
                let mut position = uiua_span_to_lsp(span, &doc.asm.inputs).start;
                if decl.inline {
                    if span.as_str(&doc.asm.inputs, |s| s.starts_with(['(', '⟨', '|'])) {
                        position.character += 1;
                    } else if span.before_str(&doc.asm.inputs, |s| s.ends_with(' ')) {
                        position.character = position.character.saturating_sub(1);
                    }
                }
                hints.push(InlayHint {
                    text_edits: Some(vec![TextEdit {
                        range: Range::new(position, position),
                        new_text: format!("{sig} "),
                    }]),
                    position,
                    label: InlayHintLabel::String(sig),
                    kind: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: Some(true),
                    data: None,
                });
            }
            // Values
            if show_values {
                for (span, values) in &doc.code_meta.top_level_values {
                    if span.src != path {
                        continue;
                    }
                    let mut shown: Vec<String> = values.iter().map(Value::show).collect();
                    let mut md = "```uiua\n".to_string();
                    for shown in &shown {
                        md.push_str(shown);
                        md.push('\n');
                    }
                    md.push_str("\n```");
                    let tooltip = InlayHintTooltip::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: md,
                    });
                    let label = if shown.iter().any(|s| s.lines().count() > 1) {
                        let mut shapes = String::new();
                        for (i, val) in values.iter().rev().enumerate() {
                            if i > 0 {
                                shapes.push_str(" | ");
                            }
                            shapes.push_str(&val.shape_string());
                        }
                        InlayHintLabel::String(shapes)
                    } else {
                        shown.reverse();
                        InlayHintLabel::String(shown.join(" "))
                    };
                    hints.push(InlayHint {
                        text_edits: None,
                        position: uiua_span_to_lsp(span, &doc.asm.inputs).end,
                        label,
                        kind: None,
                        tooltip: Some(tooltip),
                        padding_left: Some(true),
                        padding_right: None,
                        data: None,
                    });
                }
            }

            Ok(Some(hints))
        }

        async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
            let Some(doc) = (self.docs).get(&params.text_document_position.text_document.uri)
            else {
                return Ok(None);
            };
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position, &doc.input);
            let path = uri_path(&params.text_document_position.text_document.uri);
            for (i, binfo) in doc.asm.bindings.iter().enumerate() {
                if binfo.span.contains_line_col(line, col) && binfo.span.src == path {
                    let mut locations = Vec::new();
                    for entry in &self.docs {
                        let uri = entry.key();
                        let doc = entry.value();
                        for (name_span, idx) in &doc.code_meta.global_references {
                            if *idx == i {
                                let uri = match &name_span.src {
                                    InputSrc::Str(_) | InputSrc::Macro(_) => uri.clone(),
                                    InputSrc::File(file) => path_to_uri(file)?,
                                };
                                let range = uiua_span_to_lsp(name_span, &doc.asm.inputs);
                                locations.push(Location { uri, range });
                            }
                        }
                    }
                    return Ok(Some(locations));
                }
            }
            Ok(None)
        }

        async fn inline_value(
            &self,
            params: InlineValueParams,
        ) -> Result<Option<Vec<InlineValue>>> {
            let Some(doc) = self.doc(&params.text_document.uri) else {
                return Ok(None);
            };
            let mut inline_values = Vec::new();
            for (span, values) in &doc.code_meta.top_level_values {
                let Some(value) = values.first() else {
                    continue;
                };
                inline_values.push(InlineValue::Text(InlineValueText {
                    range: uiua_span_to_lsp(span, &doc.asm.inputs),
                    text: value.show(),
                }));
            }
            Ok(Some(inline_values))
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
    }

    impl Backend {
        fn doc(&self, uri: &Url) -> Option<Arc<LspDoc>> {
            self.docs.get(uri).map(|doc| Arc::clone(&doc))
        }
        fn bindings_in_file(
            &self,
            doc_uri: &Url,
            path: &Path,
        ) -> impl Iterator<Item = BindingInfo> + '_ {
            let doc_uri = doc_uri.clone();
            let canonical_path = path.canonicalize().ok();
            self.docs.get(&doc_uri).into_iter().flat_map(move |doc| {
                (doc.asm.bindings.iter())
                    .filter(|binfo| {
                        let path = match &binfo.span.src {
                            InputSrc::File(file) => file.to_path_buf(),
                            InputSrc::Str(_) | InputSrc::Macro(_) => uri_path(&doc_uri),
                        };
                        path.canonicalize().ok() == canonical_path
                    })
                    .cloned()
                    .collect::<Vec<_>>()
            })
        }
        async fn debug(&self, message: impl Into<String>) {
            self.client
                .log_message(MessageType::INFO, message.into())
                .await;
        }
    }

    fn path_to_uri(path: &Path) -> Result<Url> {
        Url::from_file_path(
            path.canonicalize()
                .map_err(|e| Error::invalid_params(format!("Invalid file path: {}", e)))?,
        )
        .map_err(|_| Error::invalid_params("Invalid file path"))
    }

    fn uri_path(uri: &Url) -> PathBuf {
        let path = uri.path().replace("/c%3A", "C:");
        let path = PathBuf::from(path);
        path.canonicalize().unwrap_or(path)
    }

    fn lsp_pos_to_uiua(pos: Position, input: &str) -> (usize, usize) {
        let line_no = pos.line as usize;
        let line = input.split('\n').nth(line_no).unwrap();
        let mut lsp_col = pos.character as usize;
        let mut uiua_col = 1;
        for c in line.chars() {
            if lsp_col == 0 {
                break;
            }
            lsp_col -= c.len_utf16();
            uiua_col += 1;
        }
        (line_no + 1, uiua_col)
    }

    fn uiua_loc_to_lsp(loc: Loc, input: &str) -> Position {
        let line_no = loc.line.saturating_sub(1) as usize;
        let line = input.split('\n').nth(line_no).unwrap();
        let line_start = line.as_ptr() as usize - input.as_ptr() as usize;
        let end = loc.byte_pos as usize - line_start;
        let col = line[..end].encode_utf16().count() as u32;
        Position::new(line_no as u32, col)
    }

    fn uiua_locs_to_lsp(start: Loc, end: Loc, input: &str) -> Range {
        Range::new(uiua_loc_to_lsp(start, input), uiua_loc_to_lsp(end, input))
    }

    fn uiua_span_to_lsp(span: &CodeSpan, inputs: &Inputs) -> Range {
        uiua_locs_to_lsp(span.start, span.end, &inputs.get(&span.src))
    }

    fn doc_frag_markdown(md: &mut String, frag: &PrimDocFragment) {
        match frag {
            PrimDocFragment::Text(text) => md.push_str(text),
            PrimDocFragment::Code(text) => md.push_str(&format!("`{}`", text)),
            PrimDocFragment::Emphasis(text) => md.push_str(&format!("*{}*", text)),
            PrimDocFragment::Strong(text) => md.push_str(&format!("**{}**", text)),
            PrimDocFragment::Link { text, url } => md.push_str(&format!("[{}]({})", text, url)),
            PrimDocFragment::Primitive { prim, named } => {
                let text = if *named {
                    format!("`{}`", prim.format())
                } else {
                    prim.to_string()
                };
                md.push_str(&format!(
                    "[{}](https://uiua.org/docs/{})",
                    text,
                    prim.name()
                ))
            }
        }
    }

    fn full_prim_doc_markdown(prim: Primitive) -> String {
        let sig = prim
            .sig()
            .map(|sig| format!(" {}", sig))
            .unwrap_or_default();
        let mut value = format!("```uiua\n{}{}\n```", prim.format(), sig);
        let doc = prim.doc();
        value.push_str("\n\n");
        for frag in &doc.short {
            doc_frag_markdown(&mut value, frag);
        }
        value.push_str("\n\n");
        value.push_str(&format!(
            "[Documentation](https://uiua.org/docs/{})",
            prim.name()
        ));
        value.push_str("\n\n");
        for line in &doc.lines {
            match line {
                PrimDocLine::Text(frags) => {
                    for frag in frags {
                        doc_frag_markdown(&mut value, frag);
                    }
                    value.push('\n');
                }
                PrimDocLine::Example(ex) => {
                    value.push_str(&format!(
                        "\
```uiua
{}
```
> ```
",
                        ex.input()
                    ));
                    match ex.output_strings() {
                        Ok(lines) => {
                            for line in lines.iter().flat_map(|l| l.lines()) {
                                value.push_str(&format!("> {line}\n"));
                            }
                        }
                        Err(err) => value.push_str(&format!("> Error: {err}\n")),
                    }
                    value.push_str("> ```\n\n");
                }
            }
        }
        value
    }
}
