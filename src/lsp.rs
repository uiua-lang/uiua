//! Uiua's Language Server Protocol (LSP) implementation
//!
//! Even without the `lsp` feature enabled, this module still provides some useful types and functions for working with Uiua code in an IDE or text editor.

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    slice,
};

use crate::{
    algorithm::invert::{invert_instrs, under_instrs},
    ast::{Item, Modifier, PlaceholderOp, Ref, RefComponent, Word},
    ident_modifier_args, instrs_are_pure,
    lex::{CodeSpan, Sp},
    parse::parse,
    Assembly, BindingInfo, BindingKind, Compiler, DocComment, Ident, InputSrc, Inputs, PreEvalMode,
    Primitive, Purity, SafeSys, Signature, SysBackend, UiuaError, Value, CONSTANTS,
};

/// Kinds of span in Uiua code, meant to be used in the language server or other IDE tools
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanKind {
    Primitive(Primitive),
    String,
    Number,
    Comment,
    OutputComment,
    Strand,
    Ident(Option<BindingDocs>),
    Label,
    Signature,
    Whitespace,
    Placeholder(PlaceholderOp),
    Delimiter,
    FuncDelim(Signature),
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
    Module,
}

/// Get spans and their kinds from Uiua code
pub fn spans(input: &str) -> (Vec<Sp<SpanKind>>, Inputs) {
    spans_with_backend(input, SafeSys::default())
}

#[doc(hidden)]
/// Used for coloring code in the REPL
pub fn spans_with_compiler(input: &str, compiler: &Compiler) -> (Vec<Sp<SpanKind>>, Inputs) {
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
    (spanner.items_spans(&items), spanner.asm.inputs.clone())
}

/// Get spans and their kinds from Uiua code with a custom backend
pub fn spans_with_backend(input: &str, backend: impl SysBackend) -> (Vec<Sp<SpanKind>>, Inputs) {
    let src = InputSrc::Str(0);
    let (items, _, _) = parse(input, src.clone(), &mut Inputs::default());
    let spanner = Spanner::new(src, input, backend);
    (spanner.items_spans(&items), spanner.asm.inputs)
}

/// Metadata for code for use in IDE tools
#[derive(Debug, Clone, Default)]
pub struct CodeMeta {
    /// A map of references to global bindings
    pub global_references: HashMap<Sp<Ident>, usize>,
    /// A map of references to shadowable constants
    pub constant_references: HashSet<Sp<Ident>>,
    /// Spans of functions and their signatures and whether they are explicit
    pub function_sigs: SigDecls,
    /// A map of macro invocations to their expansions
    pub macro_expansions: HashMap<CodeSpan, (Ident, String)>,
    /// A map of incomplete ref paths to their module's index
    pub incomplete_refs: HashMap<CodeSpan, usize>,
    /// A map of the spans of top-level lines to values
    pub top_level_values: HashMap<CodeSpan, Vec<Value>>,
    /// A map of strand spans
    pub strands: BTreeMap<CodeSpan, Vec<CodeSpan>>,
    /// A map of array spans
    pub arrays: BTreeMap<CodeSpan, Vec<CodeSpan>>,
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
}

pub(crate) type SigDecls = HashMap<CodeSpan, SigDecl>;

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
            Err(UiuaError::Multi(errors)) => errors,
            Err(e) => vec![e],
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
                Item::TestScope(items) => spans.extend(self.items_spans(&items.value)),
                Item::Words(lines) => {
                    for line in lines {
                        spans.extend(self.words_spans(line))
                    }
                }
                Item::Binding(binding) => {
                    let binding_docs = self
                        .binding_docs(&binding.name.span)
                        .or_else(|| self.reference_docs(&binding.name.span));
                    spans.push(binding.name.span.clone().sp(SpanKind::Ident(binding_docs)));
                    spans.push(binding.arrow_span.clone().sp(SpanKind::Delimiter));
                    if let Some(sig) = &binding.signature {
                        spans.push(sig.span.clone().sp(SpanKind::Signature));
                    }
                    spans.extend(self.words_spans(&binding.words));
                }
                Item::Import(import) => {
                    if let Some(name) = &import.name {
                        let binding_docs = self.binding_docs(&name.span);
                        spans.push(name.span.clone().sp(SpanKind::Ident(binding_docs)));
                    }
                    spans.push(import.tilde_span.clone().sp(SpanKind::Delimiter));
                    spans.push(import.path.span.clone().sp(SpanKind::String));
                    for line in import.lines.iter().flatten() {
                        spans.push(line.tilde_span.clone().sp(SpanKind::Delimiter));
                        for item in &line.items {
                            let binding_docs = self.reference_docs(&item.span);
                            spans.push(item.span.clone().sp(SpanKind::Ident(binding_docs)));
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
        for (name, index) in &self.code_meta.global_references {
            let Some(binding) = self.asm.bindings.get(*index) else {
                continue;
            };
            if name.span != *span {
                continue;
            }
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
            let val = constant.value.resolve(path);
            return Some(BindingDocs {
                src_span: span.clone(),
                comment: Some(constant.doc.into()),
                is_public: true,
                kind: BindingDocsKind::Constant(Some(val)),
            });
        }
        None
    }

    fn make_binding_docs(&self, binfo: &BindingInfo) -> BindingDocs {
        let mut comment = binfo.comment.clone();
        if comment.is_none() {
            match &binfo.kind {
                BindingKind::Const(None) => comment = Some("constant".into()),
                BindingKind::Module { .. } => comment = Some("module".into()),
                BindingKind::Macro => comment = Some("macro".into()),
                _ => {}
            }
        }
        let kind = match &binfo.kind {
            BindingKind::Const(val) => BindingDocsKind::Constant(val.clone()),
            BindingKind::Func(f) => BindingDocsKind::Function {
                sig: f.signature(),
                invertible: {
                    let instrs = f.instrs(&self.asm);
                    let mut compiler = Compiler::new().with_assembly(self.asm.clone());
                    invert_instrs(instrs, &mut compiler).is_some()
                },
                underable: {
                    let instrs = f.instrs(&self.asm);
                    let mut compiler = Compiler::new().with_assembly(self.asm.clone());
                    under_instrs(instrs, (1, 1).into(), &mut compiler).is_some()
                },
                pure: instrs_are_pure(f.instrs(&self.asm), &self.asm, Purity::Impure),
            },
            BindingKind::Macro => {
                BindingDocsKind::Modifier(binfo.span.as_str(self.inputs(), ident_modifier_args))
            }
            BindingKind::Module { .. } => BindingDocsKind::Module,
        };
        BindingDocs {
            src_span: binfo.span.clone(),
            comment,
            is_public: binfo.public,
            kind,
        }
    }

    fn words_spans(&self, words: &[Sp<Word>]) -> Vec<Sp<SpanKind>> {
        let mut spans = Vec::new();
        'words: for word in words {
            match &word.value {
                Word::Number(s, _) => {
                    for prim in Primitive::all().filter(|p| p.is_constant()) {
                        if prim.name().starts_with(s) || prim.to_string() == *s {
                            spans.push(word.span.clone().sp(SpanKind::Primitive(prim)));
                            continue 'words;
                        }
                    }
                    spans.push(word.span.clone().sp(SpanKind::Number))
                }
                Word::Char(_)
                | Word::String(_)
                | Word::MultilineString(_)
                | Word::FormatString(_) => spans.push(word.span.clone().sp(SpanKind::String)),
                Word::Label(_) => spans.push(word.span.clone().sp(SpanKind::Label)),
                Word::MultilineFormatString(lines) => {
                    spans.extend((lines.iter()).map(|line| line.span.clone().sp(SpanKind::String)))
                }
                Word::Ref(r) => spans.extend(self.ref_spans(r)),
                Word::IncompleteRef { path, .. } => spans.extend(self.ref_path_spans(path)),
                Word::Strand(items) | Word::Undertied(items) => {
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
                Word::Func(func) => {
                    let kind = if let Some(inline) = self.code_meta.function_sigs.get(&word.span) {
                        SpanKind::FuncDelim(inline.sig)
                    } else {
                        SpanKind::Delimiter
                    };
                    spans.push(word.span.just_start(self.inputs()).sp(kind.clone()));
                    if let Some(sig) = &func.signature {
                        spans.push(sig.span.clone().sp(SpanKind::Signature));
                    }
                    spans.extend(func.lines.iter().flat_map(|w| self.words_spans(w)));
                    if func.closed {
                        let end = word.span.just_end(self.inputs());
                        if end.as_str(self.inputs(), |s| s == ")")
                            || end.as_str(self.inputs(), |s| s == "}")
                        {
                            spans.push(end.sp(kind));
                        }
                    }
                }
                Word::Switch(sw) => {
                    if word.span.as_str(self.inputs(), |s| s.starts_with('?')) {
                        spans.push(word.span.clone().sp(SpanKind::Delimiter));
                        continue;
                    }
                    spans.push(word.span.just_start(self.inputs()).sp(SpanKind::Delimiter));
                    for (i, branch) in sw.branches.iter().enumerate() {
                        let start_span = branch.span.just_start(self.inputs());
                        if i > 0 && start_span.as_str(self.inputs(), |s| s == "|") {
                            let kind = if let Some(SigDecl {
                                sig,
                                explicit: false,
                                ..
                            }) = self.code_meta.function_sigs.get(&branch.span)
                            {
                                SpanKind::FuncDelim(*sig)
                            } else {
                                SpanKind::Delimiter
                            };
                            spans.push(start_span.sp(kind));
                        }
                        if let Some(sig) = &branch.value.signature {
                            spans.push(sig.span.clone().sp(SpanKind::Signature));
                        }
                        spans.extend(branch.value.lines.iter().flat_map(|w| self.words_spans(w)));
                    }
                    if sw.closed {
                        let end = word.span.just_end(self.inputs());
                        if end.as_str(self.inputs(), |s| s == ")") {
                            spans.push(end.sp(SpanKind::Delimiter));
                        }
                    }
                }
                Word::Primitive(prim) => {
                    spans.push(word.span.clone().sp(SpanKind::Primitive(*prim)))
                }
                Word::SemicolonPop => {
                    spans.push(word.span.clone().sp(SpanKind::Primitive(Primitive::Pop)))
                }
                Word::Modified(m) => {
                    let modifier_span = &m.modifier.span;
                    match &m.modifier.value {
                        Modifier::Primitive(p) => {
                            spans.push(modifier_span.clone().sp(SpanKind::Primitive(*p)))
                        }
                        Modifier::Ref(r) => spans.extend(self.ref_spans(r)),
                    }
                    spans.extend(self.words_spans(&m.operands));
                }
                Word::Spaces | Word::BreakLine | Word::UnbreakLine => {
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
            }
        }
        spans.retain(|sp| !sp.span.as_str(self.inputs(), str::is_empty));
        spans
    }
    fn ref_spans(&self, r: &Ref) -> Vec<Sp<SpanKind>> {
        let mut spans = self.ref_path_spans(&r.path);
        let name_docs = self.reference_docs(&r.name.span);
        spans.push(r.name.span.clone().sp(SpanKind::Ident(name_docs)));
        spans
    }
    fn ref_path_spans(&self, path: &[RefComponent]) -> Vec<Sp<SpanKind>> {
        let mut spans = Vec::new();
        for comp in path {
            let module_docs = self.reference_docs(&comp.module.span);
            spans.push(comp.module.span.clone().sp(SpanKind::Ident(module_docs)));
            spans.push(comp.tilde_span.clone().sp(SpanKind::Delimiter));
        }
        spans
    }
}

#[cfg(feature = "lsp")]
#[doc(hidden)]
pub use server::run_language_server;

#[cfg(feature = "lsp")]
mod server {
    use std::{
        env::current_dir,
        path::{Path, PathBuf},
    };

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
        AsciiToken, Assembly, BindingInfo, NativeSys, PrimDocLine, Span, Token,
    };

    pub struct LspDoc {
        pub input: String,
        pub items: Vec<Item>,
        pub spans: Vec<Sp<SpanKind>>,
        pub asm: Assembly,
        pub code_meta: CodeMeta,
        pub errors: Vec<UiuaError>,
        pub diagnostics: Vec<crate::Diagnostic>,
    }

    impl LspDoc {
        fn new(path: impl AsRef<Path>, input: String) -> Self {
            let path = path.as_ref();
            let path = current_dir()
                .ok()
                .and_then(|curr| pathdiff::diff_paths(path, curr))
                .unwrap_or_else(|| path.to_path_buf());
            let src = InputSrc::File(path.into());
            let (items, _, _) = parse(&input, src.clone(), &mut Inputs::default());
            let spanner = Spanner::new(src, &input, NativeSys);
            let spans = spanner.items_spans(&items);
            Self {
                input,
                items,
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
        crate::sys_native::set_output_enabled(false);

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
                Server::new(stdin, stdout, socket).serve(service).await;
            });
    }

    struct Backend {
        client: Client,
        docs: DashMap<Url, LspDoc>,
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
    const MODULE_STT: SemanticTokenType = SemanticTokenType::new("module");

    const SEMANTIC_TOKEN_TYPES: [SemanticTokenType; 13] = [
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
                                    token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
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

        async fn did_open(&self, param: DidOpenTextDocumentParams) {
            let path = uri_path(&param.text_document.uri);
            self.docs.insert(
                param.text_document.uri,
                LspDoc::new(path, param.text_document.text),
            );
        }

        async fn did_change(&self, params: DidChangeTextDocumentParams) {
            let path = uri_path(&params.text_document.uri);
            let doc = LspDoc::new(path, params.content_changes[0].text.clone());
            self.docs.insert(params.text_document.uri, doc);
        }

        async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
            let Some(doc) =
                (self.docs).get(&params.text_document_position_params.text_document.uri)
            else {
                return Ok(None);
            };
            let (line, col) = lsp_pos_to_uiua(params.text_document_position_params.position);
            let mut prim_range = None;
            // Hovering a primitive
            for sp in &doc.spans {
                if sp.span.contains_line_col(line, col) {
                    match sp.value {
                        SpanKind::Primitive(prim) => {
                            prim_range = Some((prim, uiua_span_to_lsp(&sp.span)));
                        }
                        _ => {}
                    }
                }
            }
            // Hovering a binding
            let mut binding_docs: Option<Sp<&BindingDocs>> = None;
            if prim_range.is_none() {
                for span_kind in &doc.spans {
                    if let SpanKind::Ident(Some(docs)) = &span_kind.value {
                        if span_kind.span.contains_line_col(line, col) {
                            binding_docs = Some(span_kind.span.clone().sp(docs));
                            break;
                        }
                    }
                }
            }
            // Hovering an inline function
            let mut inline_function_sig: Option<Sp<Signature>> = None;
            if prim_range.is_none() && binding_docs.is_none() {
                for (span, inline) in &doc.code_meta.function_sigs {
                    if !inline.explicit && span.contains_line_col(line, col) {
                        inline_function_sig = Some(span.clone().sp(inline.sig));
                    }
                }
            }

            Ok(Some(if let Some((prim, range)) = prim_range {
                Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: full_prim_doc_markdown(prim),
                    }),
                    range: Some(range),
                }
            } else if let Some(docs) = binding_docs {
                let span = docs.span;
                let docs = docs.value;
                let mut value = "```uiua\n".to_string();
                span.as_str(&doc.asm.inputs, |s| value.push_str(s));
                match docs.kind {
                    BindingDocsKind::Function { sig, .. } => {
                        value.push_str(&format!(" {sig}"));
                    }
                    _ => {}
                }
                if !docs.is_public && !matches!(docs.kind, BindingDocsKind::Module) {
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
                Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                    range: Some(uiua_span_to_lsp(&span)),
                }
            } else if let Some(sig) = inline_function_sig {
                Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("```uiua\n{}\n```", sig.value),
                    }),
                    range: Some(uiua_span_to_lsp(&sig.span)),
                }
            } else {
                return Ok(None);
            }))
        }

        async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
            fn make_completion(
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
                    BindingKind::Macro => CompletionItemKind::FUNCTION,
                    BindingKind::Module { .. } => CompletionItemKind::MODULE,
                };
                CompletionItem {
                    label: name.clone(),
                    kind: Some(kind),
                    label_details: Some(CompletionItemLabelDetails {
                        description: (binding.kind.signature())
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
                        range: uiua_span_to_lsp(span),
                        new_text: name,
                    })),
                    ..Default::default()
                }
            }

            let doc_uri = &params.text_document_position.text_document.uri;
            let doc = if let Some(doc) = self.docs.get(doc_uri) {
                doc
            } else {
                return Ok(None);
            };
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position);

            // Find an incomplete ref path at the cursor position
            if let Some((span, index)) =
                doc.code_meta.incomplete_refs.iter().find(|(span, _)| {
                    span.end.line as usize == line && span.end.col as usize == col
                })
            {
                if let BindingKind::Module(module) = &doc.asm.bindings[*index].kind {
                    let mut completions = Vec::new();
                    let mut span = span.clone();
                    span.start = span.end;
                    for binding in self.bindings_in_file(doc_uri, module) {
                        if !binding.public {
                            continue;
                        }
                        let item_name = binding.span.as_str(&doc.asm.inputs, |s| s.to_string());
                        completions.push(make_completion(item_name, &span, &binding));
                    }
                    return Ok(Some(CompletionResponse::Array(completions)));
                }
            }

            // Find the span at the cursor position
            let Some(sp) = (doc.spans.iter()).find(|sp| sp.span.contains_line_col(line, col))
            else {
                return Ok(None);
            };

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
                            prim.signature().map(|sig| CompletionItemLabelDetails {
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
                            range: uiua_span_to_lsp(&sp.span),
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

                if let BindingKind::Module(module) = &binding.kind {
                    for binding in self.bindings_in_file(doc_uri, module) {
                        if !binding.public {
                            continue;
                        }
                        let item_name = binding.span.as_str(&doc.asm.inputs, |s| s.to_string());
                        if !item_name.to_lowercase().starts_with(&token.to_lowercase()) {
                            continue;
                        }
                        completions.push(make_completion(
                            format!("{name}~{item_name}"),
                            &sp.span,
                            &binding,
                        ));
                    }
                }

                if !name.to_lowercase().starts_with(&token.to_lowercase()) {
                    continue;
                }
                completions.push(make_completion(name, &sp.span, &binding));
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
                    detail: Some(constant.doc.into()),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range: uiua_span_to_lsp(&sp.span),
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
            let doc = if let Some(doc) = self.docs.get(&params.text_document.uri) {
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
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position);
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
            let is_underscore = params.ch == "_";
            let mut is_undertie = false;
            let line = if is_newline { pos.line - 1 } else { pos.line };
            let Some(line_str) = doc.input.lines().nth(line as usize) else {
                return Ok(None);
            };
            let col = if is_newline {
                line_str.chars().count() as u32
            } else {
                pos.character - 1
            };
            let before = line_str.chars().take(col as usize).collect::<String>();
            let mut ident = (before.chars().rev())
                .take_while(|&c| is_ident_char(c))
                .collect::<String>();
            ident = ident.chars().rev().collect();
            // Get formatted
            let mut start = col - ident.chars().count() as u32;
            let mut formatted = String::new();
            if ident.is_empty() {
                if is_underscore && before.ends_with('_') {
                    formatted.push('‿');
                    start -= 1;
                    is_undertie = true;
                } else {
                    let mut ascii_prims: Vec<_> = Primitive::non_deprecated()
                        .filter_map(|p| p.ascii().map(|a| (p, a.to_string())))
                        .collect();
                    ascii_prims.sort_by_key(|(_, a)| a.len());
                    ascii_prims.reverse();
                    for (prim, ascii) in ascii_prims {
                        if before.ends_with(&ascii) {
                            formatted.push_str(&prim.to_string());
                            start -= ascii.chars().count() as u32;
                            break;
                        }
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
            } else if !is_undertie {
                end.character -= 1;
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
            let Some(doc) = self.docs.get(&params.text_document.uri) else {
                return Ok(None);
            };
            let mut tokens = Vec::new();
            let mut prev_line = 0;
            let mut prev_char = 0;
            for sp in &doc.spans {
                let token_type = match &sp.value {
                    SpanKind::String => UIUA_STRING_STT,
                    SpanKind::Number => UIUA_NUMBER_STT,
                    SpanKind::Comment => SemanticTokenType::COMMENT,
                    SpanKind::Primitive(p) => match p.class() {
                        PrimClass::Stack | PrimClass::Planet if p.modifier_args().is_none() => {
                            STACK_FUNCTION_STT
                        }
                        _ if p.modifier_args() == Some(1) => MONADIC_MODIFIER_STT,
                        _ if p.modifier_args() == Some(2) => DYADIC_MODIFIER_STT,
                        _ if p.modifier_args() == Some(3) => TRIADIC_MODIFIER_STT,
                        _ if p.args() == Some(0) => NOADIC_FUNCTION_STT,
                        _ if p.args() == Some(1) => MONADIC_FUNCTION_STT,
                        _ if p.args() == Some(2) => DYADIC_FUNCTION_STT,
                        _ if p.args() == Some(3) => TRIADIC_FUNCTION_STT,
                        _ if p.args() == Some(4) => TETRADIC_FUNCTION_STT,
                        _ => continue,
                    },
                    SpanKind::Ident(Some(docs)) => match docs.kind {
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
                        BindingDocsKind::Module => MODULE_STT,
                    },
                    _ => continue,
                };
                let token_type = SEMANTIC_TOKEN_TYPES
                    .iter()
                    .position(|t| t == &token_type)
                    .unwrap() as u32;
                let span = &sp.span;
                let start = uiua_loc_to_lsp(span.start);
                let delta_start = if start.line == prev_line {
                    start.character - prev_char
                } else {
                    start.character
                };
                tokens.push(SemanticToken {
                    delta_line: start.line - prev_line,
                    delta_start,
                    length: (span.end.char_pos - span.start.char_pos),
                    token_type,
                    token_modifiers_bitset: 0,
                });
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
            let Some(doc) = self.docs.get(&params.text_document.uri) else {
                return Ok(None);
            };
            let (line, col) = lsp_pos_to_uiua(params.range.start);
            let path = uri_path(&params.text_document.uri);
            let mut actions = Vec::new();

            // Add explicit signature
            for (span, inline) in &doc.code_meta.function_sigs {
                if inline.explicit || !span.contains_line_col(line, col) || span.src != path {
                    continue;
                }
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
                                    range: uiua_span_to_lsp(&insertion_span),
                                    new_text: if inline.sig.outputs == 0 {
                                        format!("|{} ", inline.sig.args)
                                    } else {
                                        format!("{} ", inline.sig)
                                    },
                                }],
                            )]
                            .into(),
                        ),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
                break;
            }

            // Expand macro
            for (span, (name, expanded)) in &doc.code_meta.macro_expansions {
                if !span.contains_line_col(line, col) || span.src != path {
                    continue;
                }
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: format!("Expand macro {name}"),
                    kind: Some(CodeActionKind::REFACTOR_INLINE),
                    edit: Some(WorkspaceEdit {
                        changes: Some(
                            [(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: uiua_span_to_lsp(span),
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
                                        range: uiua_span_to_lsp(&span.span),
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
                                    range: uiua_span_to_lsp(span),
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
            for (span, parts) in &doc.code_meta.arrays {
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
                                    range: uiua_span_to_lsp(span),
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
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position);
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
                for (name, index) in &doc.code_meta.global_references {
                    if name.span.contains_line_col(line, col) && name.span.src == path {
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
                    range: uiua_span_to_lsp(&binding.span),
                    new_text: params.new_name.clone(),
                }],
            );
            for entry in &self.docs {
                let uri = entry.key();
                let doc = entry.value();
                for (name, idx) in &doc.code_meta.global_references {
                    if *idx == index {
                        let uri = match &name.span.src {
                            InputSrc::Str(_) | InputSrc::Macro(_) => uri.clone(),
                            InputSrc::File(file) => path_to_uri(file)?,
                        };
                        changes.entry(uri).or_default().push(TextEdit {
                            range: uiua_span_to_lsp(&name.span),
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
            let (line, col) = lsp_pos_to_uiua(position);
            let path = uri_path(&params.text_document_position_params.text_document.uri);
            for (name, idx) in &doc.code_meta.global_references {
                if name.span.contains_line_col(line, col) && name.span.src == path {
                    let binding = &doc.asm.bindings[*idx];
                    let uri = match &binding.span.src {
                        InputSrc::Str(_) | InputSrc::Macro(_) => {
                            params.text_document_position_params.text_document.uri
                        }
                        InputSrc::File(file) => path_to_uri(file)?,
                    };
                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                        uri,
                        range: uiua_span_to_lsp(&binding.span),
                    })));
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
            let (line, col) = lsp_pos_to_uiua(position);
            let path = uri_path(&params.text_document_position_params.text_document.uri);
            for (name, idx) in &doc.code_meta.global_references {
                if name.span.contains_line_col(line, col) && name.span.src == path {
                    let binding = &doc.asm.bindings[*idx];
                    let uri = match &binding.span.src {
                        InputSrc::Str(_) | InputSrc::Macro(_) => {
                            params.text_document_position_params.text_document.uri
                        }
                        InputSrc::File(file) => path_to_uri(file)?,
                    };
                    return Ok(Some(GotoDeclarationResponse::Scalar(Location {
                        uri,
                        range: uiua_span_to_lsp(&binding.span),
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
            let Some(doc) = self.docs.get(&params.text_document.uri) else {
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
            for mut err in &doc.errors {
                let mut trace = None;
                match err {
                    UiuaError::Fill(e) => err = &**e,
                    UiuaError::Traced { error, trace: tr } => {
                        err = &**error;
                        trace = Some(tr);
                    }
                    _ => (),
                }
                match err {
                    UiuaError::Run(message, _) => {
                        let span = match &message.span {
                            Span::Code(span) => span,
                            Span::Builtin => {
                                if let Some(span) = trace.into_iter().flatten().find_map(|frame| {
                                    match &frame.span {
                                        Span::Code(span) => Some(span),
                                        _ => None,
                                    }
                                }) {
                                    span
                                } else {
                                    continue;
                                }
                            }
                        };
                        diagnostics.push(Diagnostic {
                            severity: Some(DiagnosticSeverity::ERROR),
                            range: uiua_span_to_lsp(span),
                            message: message.value.clone(),
                            ..Default::default()
                        });
                    }
                    UiuaError::Parse(errors, _) => {
                        for err in errors {
                            diagnostics.push(Diagnostic {
                                severity: Some(DiagnosticSeverity::ERROR),
                                range: uiua_span_to_lsp(&err.span),
                                message: err.value.to_string(),
                                ..Default::default()
                            });
                        }
                    }
                    _ => {}
                }
            }

            for diag in &doc.diagnostics {
                let sev = match diag.kind {
                    crate::DiagnosticKind::Warning => DiagnosticSeverity::WARNING,
                    crate::DiagnosticKind::Advice | crate::DiagnosticKind::Style => {
                        DiagnosticSeverity::INFORMATION
                    }
                };
                diagnostics.push(Diagnostic {
                    severity: Some(sev),
                    range: uiua_span_to_lsp(&diag.span),
                    message: diag.message.clone(),
                    ..Default::default()
                });
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
            let Some(doc) = self.docs.get(&params.text_document.uri) else {
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
                let mut position = uiua_loc_to_lsp(span.start);
                if decl.inline {
                    position.character += 1;
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
                        position: uiua_loc_to_lsp(span.end),
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
            let (line, col) = lsp_pos_to_uiua(params.text_document_position.position);
            let path = uri_path(&params.text_document_position.text_document.uri);
            for (i, binfo) in doc.asm.bindings.iter().enumerate() {
                if binfo.span.contains_line_col(line, col) && binfo.span.src == path {
                    let mut locations = Vec::new();
                    for entry in &self.docs {
                        let uri = entry.key();
                        let doc = entry.value();
                        for (name, idx) in &doc.code_meta.global_references {
                            if *idx == i {
                                let uri = match &name.span.src {
                                    InputSrc::Str(_) | InputSrc::Macro(_) => uri.clone(),
                                    InputSrc::File(file) => path_to_uri(file)?,
                                };
                                let range = uiua_span_to_lsp(&name.span);
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
            let Some(doc) = self.docs.get(&params.text_document.uri) else {
                return Ok(None);
            };
            let mut inline_values = Vec::new();
            for (span, values) in &doc.code_meta.top_level_values {
                let Some(value) = values.first() else {
                    continue;
                };
                inline_values.push(InlineValue::Text(InlineValueText {
                    range: uiua_span_to_lsp(span),
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

    fn lsp_pos_to_uiua(pos: Position) -> (usize, usize) {
        (pos.line as usize + 1, pos.character as usize + 1)
    }

    fn uiua_loc_to_lsp(loc: Loc) -> Position {
        Position::new(loc.line as u32 - 1, loc.col as u32 - 1)
    }

    fn uiua_locs_to_lsp(start: Loc, end: Loc) -> Range {
        Range::new(uiua_loc_to_lsp(start), uiua_loc_to_lsp(end))
    }

    fn uiua_span_to_lsp(span: &CodeSpan) -> Range {
        uiua_locs_to_lsp(span.start, span.end)
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
            .signature()
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
                    match ex.output() {
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
