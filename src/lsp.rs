#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanKind {
    Primitive(Primitive),
    String,
    Number,
    Comment,
    Strand,
}

pub fn spans(input: &str) -> Vec<Sp<SpanKind>> {
    let (items, _) = parse(input, None);
    items_spans(items)
}

fn items_spans(items: Vec<Item>) -> Vec<Sp<SpanKind>> {
    let mut spans = Vec::new();
    for item in items {
        match item {
            Item::Scoped { items, .. } => spans.extend(items_spans(items)),
            Item::Words(words) => spans.extend(words_spans(words)),
            Item::Binding(binding) => spans.extend(words_spans(binding.words)),
            Item::Newlines => {}
        }
    }
    spans
}

fn words_spans(words: Vec<Sp<Word>>) -> Vec<Sp<SpanKind>> {
    let mut spans = Vec::new();
    for word in words {
        match word.value {
            Word::Number(..) => spans.push(word.span.sp(SpanKind::Number)),
            Word::Char(_) | Word::String(_) | Word::FormatString(_) => {
                spans.push(word.span.sp(SpanKind::String))
            }
            Word::MultilineString(lines) => {
                spans.extend(lines.into_iter().map(|line| line.span.sp(SpanKind::String)))
            }
            Word::Ident(ident) => {
                if let Some(prims) = Primitive::from_format_name_multi(ident.as_str()) {
                    let mut start = word.span.start;
                    spans.extend(prims.iter().map(|(prim, s)| {
                        let mut end = start;
                        end.col += s.chars().count();
                        end.char_pos += s.chars().count();
                        let span = CodeSpan {
                            start,
                            end,
                            ..word.span.clone()
                        };
                        start = end;
                        span.sp(SpanKind::Primitive(*prim))
                    }));
                } else if let Some(prim) = Primitive::from_name(ident.as_str()) {
                    spans.push(word.span.sp(SpanKind::Primitive(prim)));
                }
            }
            Word::Strand(items) => {
                let mut prev_span: Option<CodeSpan> = None;
                for item in words_spans(items) {
                    if let Some(prev) = prev_span {
                        spans.push(
                            CodeSpan {
                                start: prev.end,
                                end: item.span.start,
                                ..word.span.clone()
                            }
                            .sp(SpanKind::Strand),
                        )
                    }
                    prev_span = Some(item.span.clone());
                    spans.push(item);
                }
            }
            Word::Array(items) => spans.extend(items.into_iter().flat_map(words_spans)),
            Word::Func(f) => spans.extend(f.body.into_iter().flat_map(words_spans)),
            Word::Dfn(dfn) => spans.extend(dfn.body.into_iter().flat_map(words_spans)),
            Word::Primitive(prim) => spans.push(word.span.sp(SpanKind::Primitive(prim))),
            Word::Modified(m) => {
                spans.push(m.modifier.map(SpanKind::Primitive));
                spans.extend(words_spans(m.words));
            }
            Word::Spaces => {}
            Word::Comment(_) => spans.push(word.span.sp(SpanKind::Comment)),
        }
    }
    spans
}

#[cfg(feature = "lsp")]
pub use server::run_server;

use crate::{
    ast::{Item, Word},
    lex::{CodeSpan, Sp},
    parse::parse,
    primitive::Primitive,
};

#[cfg(feature = "lsp")]
mod server {
    use super::*;

    use crate::{
        format::{format_str, FormatConfig},
        lex::Loc,
        Uiua,
    };

    use dashmap::DashMap;
    use tower_lsp::{
        jsonrpc::{Error, Result},
        lsp_types::*,
        *,
    };

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
        docs: DashMap<Url, Doc>,
    }

    struct Doc {
        code: String,
        spans: Vec<Sp<SpanKind>>,
    }

    impl Doc {
        fn new(code: String) -> Self {
            let spans = spans(&code);
            Self { code, spans }
        }
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
            self.docs
                .insert(param.text_document.uri, Doc::new(param.text_document.text));
        }

        async fn did_change(&self, params: DidChangeTextDocumentParams) {
            self.docs.insert(
                params.text_document.uri,
                Doc::new(params.content_changes[0].text.clone()),
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
            for sp in &doc.spans {
                if sp.span.contains_line_col(line, col) {
                    match sp.value {
                        SpanKind::Primitive(prim) => {
                            if let Some(name) = prim.name() {
                                return Ok(Some(Hover {
                                    contents: HoverContents::Scalar(MarkedString::String(
                                        name.into(),
                                    )),
                                    range: Some(uiua_span_to_lsp(&sp.span)),
                                }));
                            }
                        }
                        _ => {}
                    }
                }
            }
            Ok(None)
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
                &doc.code,
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
                if let Ok(env) = Uiua::with_native_sys().load_str(&doc.code) {
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
