use std::slice;

use crate::{
    ast::{Item, Modifier, Word},
    lex::{CodeSpan, Loc, Sp},
    parse::parse,
    Primitive,
};

/// Kinds of span in Uiua code, meant to be used in the language server or other IDE tools
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanKind {
    Primitive(Primitive),
    String,
    Number,
    Comment,
    Strand,
    Ident,
    Signature,
    Whitespace,
    Placeholder,
    Delimiter,
}

/// Get spans and their kinds from Uiua code
pub fn spans(input: &str) -> Vec<Sp<SpanKind>> {
    let (items, _, _) = parse(input, None);
    items_spans(&items)
}

fn items_spans(items: &[Item]) -> Vec<Sp<SpanKind>> {
    let mut spans = Vec::new();
    for item in items {
        match item {
            Item::TestScope(items) => spans.extend(items_spans(&items.value)),
            Item::Words(words) => spans.extend(words_spans(words)),
            Item::Binding(binding) => {
                spans.push(binding.name.span.clone().sp(SpanKind::Ident));
                spans.push(binding.arrow_span.clone().sp(SpanKind::Delimiter));
                if let Some(sig) = &binding.signature {
                    spans.push(sig.span.clone().sp(SpanKind::Signature));
                }
                spans.extend(words_spans(&binding.words));
            }
            Item::ExtraNewlines(span) => spans.push(span.clone().sp(SpanKind::Whitespace)),
        }
    }
    spans
}

fn words_spans(words: &[Sp<Word>]) -> Vec<Sp<SpanKind>> {
    let mut spans = Vec::new();
    for word in words {
        match &word.value {
            Word::Number(..) => spans.push(word.span.clone().sp(SpanKind::Number)),
            Word::Char(_) | Word::String(_) | Word::FormatString(_) => {
                spans.push(word.span.clone().sp(SpanKind::String))
            }
            Word::MultilineString(lines) => {
                spans.extend((lines.iter()).map(|line| line.span.clone().sp(SpanKind::String)))
            }
            Word::Ident(_) => spans.push(word.span.clone().sp(SpanKind::Ident)),
            Word::Strand(items) => {
                for (i, word) in items.iter().enumerate() {
                    let item_spans = words_spans(slice::from_ref(word));
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
                spans.push(word.span.just_start().sp(SpanKind::Delimiter));
                spans.extend(arr.lines.iter().flat_map(|w| words_spans(w)));
                let end = word.span.just_end();
                if arr.closed && end.as_str() == "]" || end.as_str() == "}" {
                    spans.push(end.sp(SpanKind::Delimiter));
                }
            }
            Word::Func(func) => {
                spans.push(word.span.just_start().sp(SpanKind::Delimiter));
                if let Some(sig) = &func.signature {
                    spans.push(sig.span.clone().sp(SpanKind::Signature));
                }
                spans.extend(func.lines.iter().flat_map(|w| words_spans(w)));
                if func.closed {
                    let end = word.span.just_end();
                    if end.as_str() == ")" || end.as_str() == "}" {
                        spans.push(end.sp(SpanKind::Delimiter));
                    }
                }
            }
            Word::Switch(sw) => {
                spans.push(word.span.just_start().sp(SpanKind::Delimiter));
                for (i, branch) in sw.branches.iter().enumerate() {
                    let start_span = branch.span.just_start();
                    if i > 0 && start_span.as_str() == "|" {
                        spans.push(start_span.sp(SpanKind::Delimiter));
                    }
                    if let Some(sig) = &branch.value.signature {
                        spans.push(sig.span.clone().sp(SpanKind::Signature));
                    }
                    spans.extend(branch.value.lines.iter().flat_map(|w| words_spans(w)));
                }
                if sw.closed {
                    let end = word.span.just_end();
                    if end.as_str() == ")" {
                        spans.push(end.sp(SpanKind::Delimiter));
                    }
                }
            }
            Word::Ocean(prims) => {
                for prim in prims {
                    spans.push(prim.span.clone().sp(SpanKind::Primitive(prim.value)));
                }
            }
            Word::Primitive(prim) => spans.push(word.span.clone().sp(SpanKind::Primitive(*prim))),
            Word::Modified(m) => {
                spans.push(m.modifier.clone().map(|m| match m {
                    Modifier::Primitive(p) => SpanKind::Primitive(p),
                    Modifier::Ident(_) => SpanKind::Ident,
                }));
                spans.extend(words_spans(&m.operands));
            }
            Word::Spaces => spans.push(word.span.clone().sp(SpanKind::Whitespace)),
            Word::Comment(_) => spans.push(word.span.clone().sp(SpanKind::Comment)),
            Word::Placeholder(_) => spans.push(word.span.clone().sp(SpanKind::Placeholder)),
        }
    }
    spans
}

#[cfg(feature = "lsp")]
#[doc(hidden)]
pub use server::run_language_server;

#[cfg(feature = "lsp")]
mod server {
    use std::{collections::BTreeMap, sync::Arc};

    use dashmap::DashMap;
    use tower_lsp::{jsonrpc::Result, lsp_types::*, *};

    use super::*;

    use crate::{
        format::{format_str, FormatConfig},
        lex::Loc,
        primitive::{PrimClass, PrimDocFragment},
        Ident, Uiua,
    };

    pub struct LspDoc {
        pub input: String,
        pub items: Vec<Item>,
        pub spans: Vec<Sp<SpanKind>>,
        pub bindings: BindingsInfo,
    }

    type BindingsInfo = BTreeMap<Sp<Ident>, Arc<BindingInfo>>;

    impl LspDoc {
        fn new(input: String) -> Self {
            let (items, _, _) = parse(&input, None);
            let spans = items_spans(&items);
            let bindings = bindings_info(&items);
            Self {
                input,
                items,
                spans,
                bindings,
            }
        }
    }

    pub struct BindingInfo {
        pub span: CodeSpan,
        pub comment: Option<String>,
    }

    fn bindings_info(items: &[Item]) -> BindingsInfo {
        let mut bindings = BindingsInfo::new();
        let mut scope_bindings = Vec::new();
        let mut last_comment: Option<String> = None;
        for item in items {
            match item {
                Item::TestScope(items) => scope_bindings.push(bindings_info(&items.value)),
                Item::Words(words) => {
                    if let [Sp {
                        value: Word::Comment(comment),
                        ..
                    }] = words.as_slice()
                    {
                        let full = last_comment.get_or_insert_with(String::new);
                        if !full.is_empty() {
                            if comment.trim().is_empty() {
                                full.push('\n');
                                full.push('\n');
                            } else {
                                full.push(' ');
                            }
                        }
                        full.push_str(comment.trim());
                    } else {
                        last_comment = None;
                        for word in words {
                            if let Word::Ident(ident) = &word.value {
                                if let Some((_, info)) =
                                    bindings.iter().rev().find(|(name, _)| name.value == *ident)
                                {
                                    let info = info.clone();
                                    bindings.insert(word.span.clone().sp(ident.clone()), info);
                                }
                            }
                        }
                    }
                }
                Item::Binding(binding) => {
                    let comment = last_comment.take();
                    bindings.insert(
                        binding.name.clone(),
                        BindingInfo {
                            comment,
                            span: binding.name.span.clone(),
                        }
                        .into(),
                    );
                }
                Item::ExtraNewlines(_) => {}
            }
        }
        scope_bindings.push(bindings);
        scope_bindings.into_iter().flatten().collect()
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

    const STACK_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("stack-function");
    const NOADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("noadic-function");
    const MONADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("monadic-function");
    const DYADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("dyadic-function");
    const MONADIC_MODIFIER_STT: SemanticTokenType = SemanticTokenType::new("monadic-modifier");
    const DYADIC_MODIFIER_STT: SemanticTokenType = SemanticTokenType::new("dyadic-modifier");

    const SEMANTIC_TOKEN_TYPES: [SemanticTokenType; 9] = [
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::COMMENT,
        STACK_FUNCTION_STT,
        NOADIC_FUNCTION_STT,
        MONADIC_FUNCTION_STT,
        DYADIC_FUNCTION_STT,
        MONADIC_MODIFIER_STT,
        DYADIC_MODIFIER_STT,
    ];

    #[tower_lsp::async_trait]
    impl LanguageServer for Backend {
        async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
            self.client
                .log_message(MessageType::INFO, "Initializing Uiua language server")
                .await;
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
                    document_formatting_provider: Some(OneOf::Left(true)),
                    semantic_tokens_provider: Some(
                        SemanticTokensServerCapabilities::SemanticTokensOptions(
                            SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                        ),
                    ),
                    ..Default::default()
                },
                ..Default::default()
            })
        }

        async fn initialized(&self, _: InitializedParams) {
            self.client
                .log_message(MessageType::INFO, "Uiua language server initialized")
                .await;
        }

        async fn did_open(&self, param: DidOpenTextDocumentParams) {
            self.docs.insert(
                param.text_document.uri,
                LspDoc::new(param.text_document.text),
            );
        }

        async fn did_change(&self, params: DidChangeTextDocumentParams) {
            self.docs.insert(
                params.text_document.uri,
                LspDoc::new(params.content_changes[0].text.clone()),
            );
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
            let mut binding_range = None;
            for (ident, binding) in &doc.bindings {
                if ident.span.contains_line_col(line, col) {
                    binding_range = Some((ident, binding, uiua_span_to_lsp(&ident.span)));
                }
            }
            Ok(Some(if let Some((prim, range)) = prim_range {
                let mut value: String = prim.name().into();
                if let Some(doc) = prim.doc() {
                    value.push('\n');
                    for frag in doc.short.iter() {
                        match frag {
                            PrimDocFragment::Text(text) => value.push_str(text),
                            PrimDocFragment::Code(text) => value.push_str(&format!("`{}`", text)),
                            PrimDocFragment::Emphasis(text) => {
                                value.push_str(&format!("*{}*", text))
                            }
                            PrimDocFragment::Strong(text) => {
                                value.push_str(&format!("**{}**", text))
                            }
                            PrimDocFragment::Link { text, url } => {
                                value.push_str(&format!("[{}]({})", text, url))
                            }
                            PrimDocFragment::Primitive { prim, named } => {
                                let name = prim.name();
                                value.push_str(&if *named {
                                    if let Some(unicode) = prim.glyph() {
                                        format!("`{unicode} {name}`")
                                    } else {
                                        format!("`{name}`")
                                    }
                                } else if let Some(unicode) = prim.glyph() {
                                    format!("`{unicode}`")
                                } else {
                                    format!("`{name}`")
                                })
                            }
                        }
                    }
                }
                Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                    range: Some(range),
                }
            } else if let Some((ident, binding, range)) = binding_range {
                let mut value: String = ident.value.as_ref().into();
                if let Some(comment) = &binding.comment {
                    value.push('\n');
                    value.push_str(comment);
                }
                Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                    range: Some(range),
                }
            } else {
                return Ok(None);
            }))
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

            let Ok(formatted) = format_str(&doc.input, &FormatConfig::find().unwrap_or_default())
            else {
                return Ok(None);
            };
            let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX));
            Ok(Some(vec![TextEdit {
                range,
                new_text: formatted.output,
            }]))
        }

        async fn inline_value(
            &self,
            params: InlineValueParams,
        ) -> Result<Option<Vec<InlineValue>>> {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Inline value {}", params.text_document.uri),
                )
                .await;
            let doc = if let Some(doc) = self.docs.get(&params.text_document.uri) {
                doc
            } else {
                return Ok(None);
            };
            let mut env = Uiua::with_native_sys();
            Ok(if env.load_str(&doc.input).is_ok() {
                let stack = env.take_stack();
                let mut text = String::new();
                for val in stack {
                    text.push_str(&val.show());
                }
                let range = Range {
                    start: Position::new(0, 0),
                    end: Position::new(u32::MAX, u32::MAX),
                };
                Some(vec![InlineValue::Text(InlineValueText { range, text })])
            } else {
                None
            })
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
                let token_type = match sp.value {
                    SpanKind::String => SemanticTokenType::STRING,
                    SpanKind::Number => SemanticTokenType::NUMBER,
                    SpanKind::Comment => SemanticTokenType::COMMENT,
                    SpanKind::Primitive(p) => match p.class() {
                        PrimClass::Stack if p.modifier_args().is_none() => STACK_FUNCTION_STT,
                        PrimClass::MonadicPervasive | PrimClass::MonadicArray => {
                            MONADIC_FUNCTION_STT
                        }
                        PrimClass::DyadicPervasive | PrimClass::DyadicArray => DYADIC_FUNCTION_STT,
                        _ if p.modifier_args() == Some(1) => MONADIC_MODIFIER_STT,
                        _ if p.modifier_args() == Some(2) => DYADIC_MODIFIER_STT,
                        _ if p.args() == Some(0) => NOADIC_FUNCTION_STT,
                        _ => continue,
                    },
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
                    length: (span.end.char_pos - span.start.char_pos) as u32,
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

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
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
}
