#[cfg(feature = "lsp")]
pub use server::run_server;

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
