use std::slice;

use crate::{
    ast::{Item, Word},
    lex::{CodeSpan, Loc, Sp},
    parse::parse,
    primitive::Primitive,
};

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
}

pub fn spans(input: &str) -> Vec<Sp<SpanKind>> {
    let (items, _) = parse(input, None);
    items_spans(&items)
}

fn items_spans(items: &[Item]) -> Vec<Sp<SpanKind>> {
    let mut spans = Vec::new();
    for item in items {
        match item {
            Item::Scoped { items, .. } => spans.extend(items_spans(items)),
            Item::Words(words) => spans.extend(words_spans(words)),
            Item::Binding(binding) => {
                if let Some(sig) = &binding.signature {
                    spans.push(sig.span.clone().sp(SpanKind::Signature));
                }
                spans.extend(words_spans(&binding.words));
            }
            Item::Newlines(span) => spans.push(span.clone().sp(SpanKind::Whitespace)),
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
            Word::Array(arr) => spans.extend(arr.lines.iter().flat_map(|w| words_spans(w))),
            Word::Func(func) => {
                if let Some(sig) = &func.signature {
                    spans.push(sig.span.clone().sp(SpanKind::Signature));
                }
                spans.extend(func.lines.iter().flat_map(|w| words_spans(w)));
            }
            Word::Primitive(prim) => spans.push(word.span.clone().sp(SpanKind::Primitive(*prim))),
            Word::Modified(m) => {
                spans.push(m.modifier.clone().map(SpanKind::Primitive));
                spans.extend(words_spans(&m.operands));
            }
            Word::Spaces => spans.push(word.span.clone().sp(SpanKind::Whitespace)),
            Word::Comment(_) => spans.push(word.span.clone().sp(SpanKind::Comment)),
        }
    }
    spans
}

#[cfg(feature = "lsp")]
pub use server::run_server;

#[cfg(feature = "lsp")]
mod server {
    use std::{collections::BTreeMap, sync::Arc};

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
        primitive::PrimDocFragment,
        Ident, Uiua,
    };

    pub struct LspDoc {
        pub input: String,
        pub spans: Vec<Sp<SpanKind>>,
        pub bindings: BindingsInfo,
    }

    type BindingsInfo = BTreeMap<Sp<Ident>, Arc<BindingInfo>>;

    impl LspDoc {
        fn new(input: String) -> Self {
            let (items, _) = parse(&input, None);
            let spans = items_spans(&items);
            let bindings = bindings_info(&items);
            Self {
                input,
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
                Item::Scoped { items, .. } => scope_bindings.push(bindings_info(items)),
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
                Item::Newlines(_) => {}
            }
        }
        scope_bindings.push(bindings);
        scope_bindings.into_iter().flatten().collect()
    }

    pub fn run_server() {
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

    #[tower_lsp::async_trait]
    impl LanguageServer for Backend {
        async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
            self.client
                .log_message(MessageType::INFO, format!("{:#?}", _params.capabilities))
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
                                    token_types: vec![
                                        SemanticTokenType::STRING,
                                        SemanticTokenType::NUMBER,
                                        SemanticTokenType::COMMENT,
                                    ],
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Delta { delta: Some(false) }),
                            },
                        ),
                    ),
                    document_symbol_provider: Some(OneOf::Left(true)),
                    document_highlight_provider: Some(OneOf::Left(true)),
                    inline_value_provider: Some(OneOf::Left(true)),
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
                            if prim.name().is_some() {
                                prim_range = Some((prim, uiua_span_to_lsp(&sp.span)));
                            }
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
                let mut contents = vec![MarkedString::String(prim.name().unwrap().into())];
                if let Some(doc) = prim.doc() {
                    contents.push(MarkedString::String(
                        doc.short
                            .iter()
                            .map(|frag| match frag {
                                PrimDocFragment::Text(text) => text.clone(),
                                PrimDocFragment::Code(code) => code.clone(),
                                PrimDocFragment::Emphasis(text) => text.clone(),
                                PrimDocFragment::Primitive { prim, named } => {
                                    let name = prim.name().unwrap();
                                    if *named {
                                        if let Some(unicode) = prim.unicode() {
                                            format!("{} {}", unicode, name)
                                        } else {
                                            name.into()
                                        }
                                    } else if let Some(unicode) = prim.unicode() {
                                        unicode.into()
                                    } else {
                                        name.into()
                                    }
                                }
                            })
                            .collect(),
                    ))
                }
                Hover {
                    contents: HoverContents::Array(contents),
                    range: Some(range),
                }
            } else if let Some((ident, binding, range)) = binding_range {
                let mut contents = vec![MarkedString::String(ident.value.as_str().into())];
                if let Some(comment) = &binding.comment {
                    contents.push(MarkedString::String(comment.clone()))
                }
                Hover {
                    contents: HoverContents::Array(contents),
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
            let formatted = format_str(
                &doc.input,
                &FormatConfig {
                    multiline_indent: params.options.tab_size as usize,
                    ..Default::default()
                },
            )
            .map_err(|_| Error::parse_error())?;
            let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX));
            Ok(Some(vec![TextEdit {
                range,
                new_text: formatted,
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
            Ok(
                if let Ok(env) = Uiua::with_native_sys().load_str(&doc.input) {
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
                },
            )
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
                    SpanKind::String => 0,
                    SpanKind::Number => 1,
                    SpanKind::Comment => 2,
                    _ => continue,
                };
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

        async fn semantic_tokens_full_delta(
            &self,
            params: SemanticTokensDeltaParams,
        ) -> Result<Option<SemanticTokensFullDeltaResult>> {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Semantic tokens delta {}", params.text_document.uri),
                )
                .await;
            Ok(None)
        }

        async fn semantic_tokens_range(
            &self,
            params: SemanticTokensRangeParams,
        ) -> Result<Option<SemanticTokensRangeResult>> {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Semantic tokens range {}", params.text_document.uri),
                )
                .await;
            Ok(None)
        }

        async fn document_highlight(
            &self,
            params: DocumentHighlightParams,
        ) -> Result<Option<Vec<DocumentHighlight>>> {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!(
                        "Document highlight {}",
                        params.text_document_position_params.text_document.uri
                    ),
                )
                .await;
            Ok(None)
        }

        async fn document_symbol(
            &self,
            params: DocumentSymbolParams,
        ) -> Result<Option<DocumentSymbolResponse>> {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("Document symbol {}", params.text_document.uri),
                )
                .await;
            Ok(None)
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
