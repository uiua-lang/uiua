use std::slice;

use crate::{
    ast::{Item, Modifier, Word},
    lex::{CodeSpan, Loc, Sp},
    parse::parse,
    InputSrc, Inputs, Primitive,
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
pub fn spans(input: &str) -> (Vec<Sp<SpanKind>>, Inputs) {
    let mut inputs = Inputs::default();
    let (items, _, _) = parse(input, InputSrc::Str(0), &mut inputs);
    (Spanner::new(input).items_spans(&items), inputs)
}

struct Spanner {
    inputs: Inputs,
}

impl Spanner {
    fn new(input: &str) -> Self {
        let mut inputs = Inputs::default();
        inputs.strings.push(input.into());
        Self { inputs }
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
                    spans.push(binding.name.span.clone().sp(SpanKind::Ident));
                    spans.push(binding.arrow_span.clone().sp(SpanKind::Delimiter));
                    if let Some(sig) = &binding.signature {
                        spans.push(sig.span.clone().sp(SpanKind::Signature));
                    }
                    spans.extend(self.words_spans(&binding.words));
                }
            }
        }
        spans
    }

    fn words_spans(&self, words: &[Sp<Word>]) -> Vec<Sp<SpanKind>> {
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
                    spans.push(word.span.just_start(&self.inputs).sp(SpanKind::Delimiter));
                    spans.extend(arr.lines.iter().flat_map(|w| self.words_spans(w)));
                    if arr.closed {
                        let end = word.span.just_end(&self.inputs);
                        if end.as_str(&self.inputs, |s| s == "]")
                            || end.as_str(&self.inputs, |s| s == "}")
                        {
                            spans.push(end.sp(SpanKind::Delimiter));
                        }
                    }
                }
                Word::Func(func) => {
                    spans.push(word.span.just_start(&self.inputs).sp(SpanKind::Delimiter));
                    if let Some(sig) = &func.signature {
                        spans.push(sig.span.clone().sp(SpanKind::Signature));
                    }
                    spans.extend(func.lines.iter().flat_map(|w| self.words_spans(w)));
                    if func.closed {
                        let end = word.span.just_end(&self.inputs);
                        if end.as_str(&self.inputs, |s| s == ")")
                            || end.as_str(&self.inputs, |s| s == "}")
                        {
                            spans.push(end.sp(SpanKind::Delimiter));
                        }
                    }
                }
                Word::Switch(sw) => {
                    if word.span.as_str(&self.inputs, |s| s.starts_with('?')) {
                        spans.push(word.span.clone().sp(SpanKind::Delimiter));
                        continue;
                    }
                    spans.push(word.span.just_start(&self.inputs).sp(SpanKind::Delimiter));
                    for (i, branch) in sw.branches.iter().enumerate() {
                        let start_span = branch.span.just_start(&self.inputs);
                        if i > 0 && start_span.as_str(&self.inputs, |s| s == "|") {
                            spans.push(start_span.sp(SpanKind::Delimiter));
                        }
                        if let Some(sig) = &branch.value.signature {
                            spans.push(sig.span.clone().sp(SpanKind::Signature));
                        }
                        spans.extend(branch.value.lines.iter().flat_map(|w| self.words_spans(w)));
                    }
                    if sw.closed {
                        let end = word.span.just_end(&self.inputs);
                        if end.as_str(&self.inputs, |s| s == ")") {
                            spans.push(end.sp(SpanKind::Delimiter));
                        }
                    }
                }
                Word::Primitive(prim) => {
                    spans.push(word.span.clone().sp(SpanKind::Primitive(*prim)))
                }
                Word::Modified(m) => {
                    spans.push(m.modifier.clone().map(|m| match m {
                        Modifier::Primitive(p) => SpanKind::Primitive(p),
                        Modifier::Ident(_) => SpanKind::Ident,
                    }));
                    spans.extend(self.words_spans(&m.operands));
                }
                Word::Spaces | Word::BreakLine | Word::UnbreakLine => {
                    spans.push(word.span.clone().sp(SpanKind::Whitespace))
                }
                Word::Comment(_) | Word::OutputComment { .. } => {
                    spans.push(word.span.clone().sp(SpanKind::Comment))
                }
                Word::Placeholder(_) => spans.push(word.span.clone().sp(SpanKind::Placeholder)),
            }
        }
        spans.retain(|sp| !sp.span.as_str(&self.inputs, str::is_empty));
        spans
    }
}

#[cfg(feature = "lsp")]
#[doc(hidden)]
pub use server::run_language_server;

#[cfg(feature = "lsp")]
mod server {
    use std::sync::Arc;

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
        Assembly, BindingInfo, Compiler, PrimDocLine, Uiua,
    };

    pub struct LspDoc {
        pub input: String,
        pub items: Vec<Item>,
        pub spans: Vec<Sp<SpanKind>>,
        pub asm: Assembly,
    }

    impl LspDoc {
        fn new(input: String) -> Self {
            let (items, _, _) = parse(&input, InputSrc::Str(0), &mut Inputs::default());
            let spanner = Spanner::new(&input);
            let spans = spanner.items_spans(&items);
            let compiler = &mut Compiler::new();
            _ = compiler.load_str(&input);
            let asm = compiler.finish();
            Self {
                input,
                items,
                spans,
                asm,
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

    const STACK_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("stack_function");
    const NOADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("noadic_function");
    const MONADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("monadic_function");
    const DYADIC_FUNCTION_STT: SemanticTokenType = SemanticTokenType::new("dyadic_function");
    const MONADIC_MODIFIER_STT: SemanticTokenType = SemanticTokenType::new("monadic_modifier");
    const DYADIC_MODIFIER_STT: SemanticTokenType = SemanticTokenType::new("dyadic_modifier");

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
                    rename_provider: Some(OneOf::Left(true)),
                    definition_provider: Some(OneOf::Left(true)),
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
            let mut binding_range = None;
            // Hovering the name of the binding itself
            for binding in &doc.asm.bindings {
                if let Some(span) = &binding.span {
                    if span.contains_line_col(line, col) {
                        let ident = span.as_str(&doc.asm.inputs, |s| s.into());
                        binding_range = Some((
                            ident,
                            binding.global.signature(),
                            binding.comment.clone(),
                            uiua_span_to_lsp(span),
                        ));
                        break;
                    }
                }
            }
            // Hovering the name of a binding reference
            if binding_range.is_none() {
                for (name, index) in &doc.asm.global_references {
                    let binding = &doc.asm.bindings[*index];
                    if name.span.contains_line_col(line, col) {
                        binding_range = Some((
                            name.value.to_string(),
                            binding.global.signature(),
                            binding.comment.clone(),
                            uiua_span_to_lsp(&name.span),
                        ));
                        break;
                    }
                }
            }
            Ok(Some(if let Some((prim, range)) = prim_range {
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
                Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                    range: Some(range),
                }
            } else if let Some((ident, signature, comment, range)) = binding_range {
                let mut value = ident;
                if let Some(sig) = signature {
                    value.push_str(&format!(" `{sig}`"));
                }
                if let Some(comment) = comment {
                    value.push_str("\n\n");
                    value.push_str(&comment);
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
            Ok(if env.run_str(&doc.input).is_ok() {
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
                if let Some(span) = &gb.span {
                    if span.contains_line_col(line, col) {
                        binding = Some((gb, i));
                        break;
                    }
                }
            }
            // Check for span in binding references
            if binding.is_none() {
                for (name, index) in &doc.asm.global_references {
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
                range: uiua_span_to_lsp(binding.span.as_ref().unwrap()),
                new_text: params.new_name.clone(),
            }];
            for (name, idx) in &doc.asm.global_references {
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
            let doc = if let Some(doc) = self
                .docs
                .get(&params.text_document_position_params.text_document.uri)
            {
                doc
            } else {
                return Ok(None);
            };
            let position = params.text_document_position_params.position;
            let (line, col) = lsp_pos_to_uiua(position);
            for (name, idx) in &doc.asm.global_references {
                if name.span.contains_line_col(line, col) {
                    let binding = &doc.asm.bindings[*idx];
                    if let Some(span) = &binding.span {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: params.text_document_position_params.text_document.uri,
                            range: uiua_span_to_lsp(span),
                        })));
                    }
                }
            }
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
}
