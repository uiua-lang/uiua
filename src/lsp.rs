#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanKind {
    Primitive(Primitive),
    String,
    Number,
    Comment,
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
            Item::Words(words, comment) => {
                if let Some(comment) = comment {
                    spans.push(comment.span.sp(SpanKind::Comment));
                }
                spans.extend(words_spans(words));
            }
            Item::Binding(binding, comment) => {
                if let Some(comment) = comment {
                    spans.push(comment.span.sp(SpanKind::Comment));
                }
                spans.extend(words_spans(binding.words));
            }
            Item::Newlines => {}
            Item::Comment(comment) => spans.push(comment.span.sp(SpanKind::Comment)),
        }
    }
    spans
}

fn words_spans(words: Vec<Sp<Word>>) -> Vec<Sp<SpanKind>> {
    let mut spans = Vec::new();
    for word in words {
        match word.value {
            Word::Number(_) => spans.push(word.span.sp(SpanKind::Number)),
            Word::Char(_) => spans.push(word.span.sp(SpanKind::String)),
            Word::String(_) => spans.push(word.span.sp(SpanKind::String)),
            Word::Ident(ident) => {
                if let Some(prims) = Primitive::from_format_name_multi(ident.as_str()) {
                    let span = if let Span::Code(span) = word.span {
                        span
                    } else {
                        unreachable!()
                    };
                    let mut start = span.start;
                    spans.extend(prims.iter().map(|(prim, s)| {
                        let mut end = start;
                        end.col += s.chars().count();
                        end.pos += s.chars().count();
                        let span = Span::Code(CodeSpan {
                            start,
                            end,
                            ..span.clone()
                        });
                        start = end;
                        span.sp(SpanKind::Primitive(*prim))
                    }));
                } else if let Some(prim) = Primitive::from_name(ident.as_str()) {
                    spans.push(word.span.sp(SpanKind::Primitive(prim)));
                }
            }
            Word::Strand(items) => spans.extend(words_spans(items)),
            Word::Array(items) => spans.extend(words_spans(items)),
            Word::Func(f) => spans.extend(words_spans(f.body)),
            Word::Dfn(dfn) => spans.extend(words_spans(dfn.body)),
            Word::Primitive(prim) => spans.push(word.span.sp(SpanKind::Primitive(prim))),
            Word::Modified(m) => {
                spans.push(m.modifier.map(SpanKind::Primitive));
                spans.extend(words_spans(m.words));
            }
            Word::Spaces => {}
        }
    }
    spans
}

#[cfg(feature = "lsp")]
pub use server::run_server;

use crate::{
    ast::{Item, Word},
    lex::{CodeSpan, Sp, Span},
    parse::parse,
    primitive::Primitive,
};

#[cfg(feature = "lsp")]
mod server {
    use tower_lsp::{jsonrpc::Result, lsp_types::*, *};

    pub fn run_server() {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(async {
                let stdin = tokio::io::stdin();
                let stdout = tokio::io::stdout();

                let (service, socket) = LspService::new(|client| Backend { client });
                Server::new(stdin, stdout, socket).serve(service).await;
            });
    }

    #[derive(Debug)]
    struct Backend {
        client: Client,
    }

    #[tower_lsp::async_trait]
    impl LanguageServer for Backend {
        async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
                    completion_provider: Some(CompletionOptions::default()),
                    ..Default::default()
                },
                ..Default::default()
            })
        }

        async fn initialized(&self, _: InitializedParams) {
            self.client
                .log_message(MessageType::INFO, "server initialized!")
                .await;
        }

        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
            Ok(Some(CompletionResponse::Array(vec![
                CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
                CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
            ])))
        }

        async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
            Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(
                    "You're hovering!".to_string(),
                )),
                range: None,
            }))
        }
    }
}
