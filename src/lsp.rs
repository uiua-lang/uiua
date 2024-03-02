//! Uiua's Language Server Protocol (LSP) implementation
//!
//! Even without the `lsp` feature enabled, this module still provides some useful types and functions for working with Uiua code in an IDE or text editor.

use std::{
    collections::{HashMap, HashSet},
    slice,
    sync::Arc,
};

use crate::{
    algorithm::invert::{invert_instrs, under_instrs},
    ast::{Item, Modifier, PlaceholderOp, Ref, Word},
    ident_modifier_args,
    lex::{CodeSpan, Loc, Sp},
    parse::parse,
    Assembly, BindingInfo, Compiler, Global, Ident, InputSrc, Inputs, Primitive, SafeSys,
    Signature, SysBackend, UiuaError, CONSTANTS,
};

/// Kinds of span in Uiua code, meant to be used in the language server or other IDE tools
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanKind {
    Primitive(Primitive),
    String,
    Number,
    Comment,
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
    /// The signature of the binding
    pub signature: Option<Signature>,
    /// The number of modifier args for the binding
    pub modifier_args: usize,
    /// The comment of the binding
    pub comment: Option<Arc<str>>,
    /// Whether the binding is invertible and underable
    pub invertible_underable: Option<(bool, bool)>,
    /// Whether the binding is a constant
    pub is_constant: bool,
    /// Whether the binding is a module
    pub is_module: bool,
    /// Whether the binding is public
    pub is_public: bool,
}

/// Get spans and their kinds from Uiua code
pub fn spans(input: &str) -> (Vec<Sp<SpanKind>>, Inputs) {
    spans_with_backend(input, SafeSys::default())
}

/// Get spans and their kinds from Uiua code with a custom backend
pub fn spans_with_backend(input: &str, backend: impl SysBackend) -> (Vec<Sp<SpanKind>>, Inputs) {
    let src = InputSrc::Str(0);
    let (items, _, _) = parse(input, src.clone(), &mut Inputs::default());
    let spanner = Spanner::new(src, input, backend);
    (spanner.items_spans(&items), spanner.asm.inputs)
}

#[derive(Clone, Default)]
pub(crate) struct CodeMeta {
    /// A map of references to global bindings
    pub global_references: HashMap<Sp<Ident>, usize>,
    /// A map of references to shadowable constants
    pub constant_references: HashSet<Sp<Ident>>,
    /// Spans of bare inline functions and their signatures and whether they are explicit
    pub inline_function_sigs: InlineSigs,
    /// A map of macro invoations to their expansions
    pub macro_expansions: HashMap<CodeSpan, (Ident, String)>,
}

#[derive(Clone, Copy)]
pub(crate) struct InlineSig {
    pub sig: Signature,
    pub explicit: bool,
}

pub(crate) type InlineSigs = HashMap<CodeSpan, InlineSig>;

struct Spanner {
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
        let errors = match compiler.load_str_src(input, src) {
            Ok(_) => Vec::new(),
            Err(UiuaError::Multi(errors)) => errors,
            Err(e) => vec![e],
        };
        let diagnostics = compiler.take_diagnostics().into_iter().collect();
        Self {
            asm: compiler.asm,
            code_meta: compiler.code_meta,
            errors,
            diagnostics,
        }
    }
    fn inputs(&self) -> &Inputs {
        &self.asm.inputs
    }
    fn invertible_underable(&self, binding: &BindingInfo) -> Option<(bool, bool)> {
        if let Global::Func(f) = &binding.global {
            let instrs = f.instrs(&self.asm);
            let mut compiler = Compiler::new().with_assembly(self.asm.clone());
            Some((
                invert_instrs(instrs, &mut compiler).is_some(),
                under_instrs(instrs, (1, 1).into(), &mut compiler).is_some(),
            ))
        } else {
            None
        }
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
        for (name, index) in &self.code_meta.global_references {
            let Some(binding) = self.asm.bindings.get(*index) else {
                continue;
            };
            if name.span != *span {
                continue;
            }
            return Some(self.make_binding_docs(binding));
        }
        for name in &self.code_meta.constant_references {
            if name.span != *span {
                continue;
            }
            let Some(constant) = CONSTANTS.iter().find(|c| c.name == name.value) else {
                continue;
            };
            return Some(BindingDocs {
                src_span: span.clone(),
                signature: Some(Signature::new(0, 1)),
                modifier_args: 0,
                comment: Some(constant.doc.into()),
                invertible_underable: None,
                is_constant: true,
                is_module: false,
                is_public: true,
            });
        }
        None
    }

    fn make_binding_docs(&self, binfo: &BindingInfo) -> BindingDocs {
        let modifier_args = binfo.span.as_str(self.inputs(), ident_modifier_args);
        let is_constant = matches!(binfo.global, Global::Const(_));
        let is_module = matches!(binfo.global, Global::Module { .. });
        let comment = binfo.comment.clone().unwrap_or_else(|| {
            if is_constant {
                "constant"
            } else if is_module {
                "module"
            } else if modifier_args > 0 {
                "macro"
            } else {
                "function"
            }
            .into()
        });
        BindingDocs {
            src_span: binfo.span.clone(),
            signature: binfo.global.signature(),
            modifier_args,
            comment: Some(comment),
            invertible_underable: self.invertible_underable(binfo),
            is_constant,
            is_module,
            is_public: binfo.public,
        }
    }

    fn words_spans(&self, words: &[Sp<Word>]) -> Vec<Sp<SpanKind>> {
        let mut spans = Vec::new();
        for word in words {
            match &word.value {
                Word::Number(..) => spans.push(word.span.clone().sp(SpanKind::Number)),
                Word::Char(_) | Word::String(_) | Word::FormatString(_) => {
                    spans.push(word.span.clone().sp(SpanKind::String))
                }
                Word::Label(_) => spans.push(word.span.clone().sp(SpanKind::Label)),
                Word::MultilineString(lines) => {
                    spans.extend((lines.iter()).map(|line| line.span.clone().sp(SpanKind::String)))
                }
                Word::Ref(r) => spans.extend(self.ref_spans(r)),
                Word::Strand(items) => {
                    for (i, word) in items.iter().enumerate() {
                        let item_spans = self.words_spans(slice::from_ref(word));
                        if i > 0 {
                            if let Some(first_item) = item_spans.first() {
                                let end = first_item.span.start;
                                spans.push(
                                    CodeSpan {
                                        start: Loc {
                                            char_pos: end.char_pos - 1,
                                            byte_pos: end.byte_pos - 1,
                                            col: end.col - 1,
                                            ..end
                                        },
                                        end,
                                        ..first_item.span.clone()
                                    }
                                    .sp(SpanKind::Strand),
                                )
                            }
                        }
                        spans.extend(item_spans);
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
                    let kind =
                        if let Some(inline) = self.code_meta.inline_function_sigs.get(&word.span) {
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
                            let kind = if let Some(InlineSig {
                                sig,
                                explicit: false,
                            }) = self.code_meta.inline_function_sigs.get(&branch.span)
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
                Word::Comment(_) | Word::OutputComment { .. } => {
                    spans.push(word.span.clone().sp(SpanKind::Comment))
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
        let mut spans = Vec::new();
        for comp in &r.path {
            let module_docs = self.reference_docs(&comp.module.span);
            spans.push(comp.module.span.clone().sp(SpanKind::Ident(module_docs)));
            spans.push(comp.tilde_span.clone().sp(SpanKind::Delimiter));
        }
        let name_docs = self.reference_docs(&r.name.span);
        spans.push(r.name.span.clone().sp(SpanKind::Ident(name_docs)));
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
        sync::Arc,
    };

    use dashmap::DashMap;
    use tower_lsp::{
        jsonrpc::{Error, Result},
        lsp_types::*,
        *,
    };

    use super::*;

    use crate::{
        format::{format_str, FormatConfig},
        lex::Loc,
        primitive::{PrimClass, PrimDocFragment},
        Assembly, BindingInfo, NativeSys, PrimDocLine, Span,
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
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Client capabilities: {:#?}", _params.capabilities),
                )
                .await;

            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Kind(
                        TextDocumentSyncKind::FULL,
                    )),
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
                    completion_provider: Some(Default::default()),
                    document_formatting_provider: Some(OneOf::Left(true)),
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
                    diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                        DiagnosticOptions {
                            inter_file_dependencies: true,
                            ..Default::default()
                        },
                    )),
                    code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
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
            let doc = if let Some(doc) = self
                .docs
                .get(&params.text_document_position_params.text_document.uri)
            {
                doc
            } else {
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
                for (span, inline) in &doc.code_meta.inline_function_sigs {
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
                if let Some(sig) = docs.signature {
                    value.push_str(&format!(" {sig}"));
                }
                if !docs.is_public && !docs.is_module {
                    value.push_str(" (private)");
                }
                value.push_str("\n```");
                if let Some((invertible, underable)) = docs.invertible_underable {
                    if invertible || underable {
                        value.push_str("\n\n");
                        if invertible {
                            value.push_str("[`° un`](https://uiua.org/docs/un)");
                        }
                        if underable {
                            if invertible {
                                value.push_str(" | ");
                            }
                            value.push_str("[`⍜ under`](https://uiua.org/docs/under)");
                        }
                    }
                }
                if let Some(comment) = &docs.comment {
                    value.push_str("\n\n");
                    value.push_str(comment);
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
            self.catch_crash(|| {
                let doc_uri = &params.text_document_position.text_document.uri;
                let doc = if let Some(doc) = self.docs.get(doc_uri) {
                    doc
                } else {
                    return Ok(None);
                };
                let (line, col) = lsp_pos_to_uiua(params.text_document_position.position);
                let Some(sp) = (doc.spans.iter()).find(|sp| sp.span.contains_line_col(line, col))
                else {
                    return Ok(None);
                };

                let Ok(token) = std::str::from_utf8(&doc.input.as_bytes()[sp.span.byte_range()])
                else {
                    return Ok(None);
                };

                // Collect primitive completions
                let mut completions: Vec<_> = Primitive::non_deprecated()
                    .filter(|p| p.name().starts_with(token))
                    .map(|prim| {
                        CompletionItem {
                            label: prim.format().to_string(),
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
                    let name = binding.span.as_str(&doc.asm.inputs, |s| s.to_string());

                    fn make_completion(
                        name: String,
                        span: &CodeSpan,
                        binding: &BindingInfo,
                    ) -> CompletionItem {
                        let kind = match &binding.global {
                            Global::Const(_) => CompletionItemKind::CONSTANT,
                            Global::Func(_) => CompletionItemKind::FUNCTION,
                            Global::Macro => CompletionItemKind::FUNCTION,
                            Global::Module { .. } => CompletionItemKind::MODULE,
                        };
                        CompletionItem {
                            label: name.clone(),
                            kind: Some(kind),
                            label_details: Some(CompletionItemLabelDetails {
                                detail: binding.global.signature().map(|sig| sig.to_string()),
                                ..Default::default()
                            }),
                            detail: binding.global.signature().map(|sig| sig.to_string()),
                            documentation: binding.comment.as_ref().map(|c| {
                                Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: c.to_string(),
                                })
                            }),
                            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                                range: uiua_span_to_lsp(span),
                                new_text: name,
                            })),
                            ..Default::default()
                        }
                    }

                    if let Global::Module { module } = &binding.global {
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
            })
            .await
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

            match format_str(
                &doc.input,
                &FormatConfig {
                    backend: Arc::new(crate::NativeSys),
                    ..FormatConfig::find().unwrap_or_default()
                },
            ) {
                Ok(formatted) => {
                    let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX));
                    Ok(Some(vec![TextEdit {
                        range,
                        new_text: formatted.output,
                    }]))
                }
                Err(e) => {
                    self.client
                        .log_message(MessageType::LOG, format!("Formatting error: {}", e))
                        .await;
                    let mut error = Error::parse_error();
                    error.message = e.to_string().into();
                    Err(error)
                }
            }
        }

        async fn semantic_tokens_full(
            &self,
            params: SemanticTokensParams,
        ) -> Result<Option<SemanticTokensResult>> {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Semantic tokens {}", params.text_document.uri),
                )
                .await;
            let doc = if let Some(doc) = self.docs.get(&params.text_document.uri) {
                doc
            } else {
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
                        PrimClass::Stack if p.modifier_args().is_none() => STACK_FUNCTION_STT,
                        PrimClass::MonadicPervasive | PrimClass::MonadicArray => {
                            MONADIC_FUNCTION_STT
                        }
                        PrimClass::DyadicPervasive | PrimClass::DyadicArray => DYADIC_FUNCTION_STT,
                        _ if p.modifier_args() == Some(1) => MONADIC_MODIFIER_STT,
                        _ if p.modifier_args() == Some(2) => DYADIC_MODIFIER_STT,
                        _ if p.modifier_args() == Some(3) => TRIADIC_MODIFIER_STT,
                        _ if p.args() == Some(0) => NOADIC_FUNCTION_STT,
                        _ => continue,
                    },
                    SpanKind::Ident(Some(docs)) => {
                        if docs.is_constant {
                            continue;
                        }
                        if docs.is_module {
                            MODULE_STT
                        } else {
                            match docs.modifier_args {
                                1 => MONADIC_MODIFIER_STT,
                                2 => DYADIC_MODIFIER_STT,
                                3 => TRIADIC_MODIFIER_STT,
                                _ => match docs.signature {
                                    Some(sig) if sig.args == 0 => NOADIC_FUNCTION_STT,
                                    Some(sig) if sig.args == 1 => MONADIC_FUNCTION_STT,
                                    Some(sig) if sig.args == 2 => DYADIC_FUNCTION_STT,
                                    Some(sig) if sig.args == 3 => TRIADIC_FUNCTION_STT,
                                    Some(sig) if sig.args == 4 => TETRADIC_FUNCTION_STT,
                                    _ => continue,
                                },
                            }
                        }
                    }
                    _ => continue,
                };
                let token_type = SEMANTIC_TOKEN_TYPES
                    .iter()
                    .position(|t| t == &token_type)
                    .unwrap() as u32;
                let span = &sp.span;
                let start = uiua_loc_to_lsp(span.start);
                let delta_start = if start.character > prev_char {
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
            let mut actions = Vec::new();

            // Add explicit signature
            for (span, inline) in &doc.code_meta.inline_function_sigs {
                if inline.explicit || !span.contains_line_col(line, col) {
                    continue;
                }
                let mut insertion_span = span.just_start(&doc.asm.inputs);
                insertion_span.start = insertion_span.end;
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: "Add explicit signature".into(),
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(
                            [(
                                params.text_document.uri.clone(),
                                vec![TextEdit {
                                    range: uiua_span_to_lsp(&insertion_span),
                                    new_text: format!("{} ", inline.sig),
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
                if !span.contains_line_col(line, col) {
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

            Ok(if actions.is_empty() {
                None
            } else {
                Some(actions)
            })
        }

        async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
            let doc = if let Some(doc) = self
                .docs
                .get(&params.text_document_position.text_document.uri)
            {
                doc
            } else {
                return Ok(None);
            };
            let position = params.text_document_position.position;
            let (line, col) = lsp_pos_to_uiua(position);
            let mut binding: Option<(&BindingInfo, usize)> = None;
            // Check for span in bindings
            for (i, gb) in doc.asm.bindings.iter().enumerate() {
                if gb.span.contains_line_col(line, col) {
                    binding = Some((gb, i));
                    break;
                }
            }
            // Check for span in binding references
            if binding.is_none() {
                for (name, index) in &doc.code_meta.global_references {
                    if name.span.contains_line_col(line, col) {
                        binding = Some((&doc.asm.bindings[*index], *index));
                        break;
                    }
                }
            }
            let Some((binding, index)) = binding else {
                return Ok(None);
            };
            // Collect edits
            let mut edits = vec![TextEdit {
                range: uiua_span_to_lsp(&binding.span),
                new_text: params.new_name.clone(),
            }];
            for (name, idx) in &doc.code_meta.global_references {
                if *idx == index {
                    edits.push(TextEdit {
                        range: uiua_span_to_lsp(&name.span),
                        new_text: params.new_name.clone(),
                    });
                }
            }
            Ok(Some(WorkspaceEdit {
                changes: Some([(params.text_document_position.text_document.uri, edits)].into()),
                document_changes: None,
                change_annotations: None,
            }))
        }

        async fn goto_definition(
            &self,
            params: GotoDefinitionParams,
        ) -> Result<Option<GotoDefinitionResponse>> {
            let current_doc = if let Some(doc) = self
                .docs
                .get(&params.text_document_position_params.text_document.uri)
            {
                doc
            } else {
                return Ok(None);
            };
            let position = params.text_document_position_params.position;
            let (line, col) = lsp_pos_to_uiua(position);
            for (name, idx) in &current_doc.code_meta.global_references {
                if name.span.contains_line_col(line, col) {
                    let binding = &current_doc.asm.bindings[*idx];
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

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
    }

    impl Backend {
        async fn catch_crash<T>(&self, f: impl FnOnce() -> Result<Option<T>>) -> Result<Option<T>> {
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
                Ok(result) => result,
                Err(e) => {
                    self.client
                        .log_message(MessageType::ERROR, format!("Error: {:?}", e))
                        .await;
                    Ok(None)
                }
            }
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
        PathBuf::from(path)
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
