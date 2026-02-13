//! The Uiua parser

use std::{
    collections::BTreeMap,
    error::Error,
    f64::consts::{PI, TAU},
    fmt,
    mem::{replace, take},
    slice,
};

use ecow::EcoString;

use crate::{
    BindingCounts, Complex, Diagnostic, DiagnosticKind, Ident, Inputs, NumComponent, Primitive,
    Signature,
    ast::*,
    lex::{AsciiToken::*, Token::*, *},
};

/// An error that occurred while parsing
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<EcoString>),
    Unexpected(Token),
    InvalidArgCount(String),
    InvalidOutCount(String),
    ModifierImportName,
    SplitInModifier,
    FlipInModifier,
    RecursionLimit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expectation {
    Term,
    ArgsOutputs,
    ItemName,
    Token(Token),
    CloseModule,
}

impl From<AsciiToken> for Expectation {
    fn from(simple: AsciiToken) -> Self {
        Expectation::Token(simple.into())
    }
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expectation::Term => write!(f, "term"),
            Expectation::ArgsOutputs => write!(f, "arguments and outputs count"),
            Expectation::ItemName => write!(f, "item name"),
            Expectation::Token(Simple(s)) => write!(f, "`{s}`"),
            Expectation::Token(tok) => write!(f, "{tok:?}"),
            Expectation::CloseModule => write!(f, "`---`"),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Lex(e) => write!(f, "{e}"),
            ParseError::Expected(exps, found) => {
                write!(f, "Expected ")?;
                if exps.len() == 2 {
                    write!(f, "{} or {}", exps[0], exps[1])?;
                } else {
                    for (i, exp) in exps.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{exp}")?;
                    }
                }
                if let Some(found) = found {
                    if found == "\n" {
                        write!(f, ", found newline")?;
                    } else {
                        write!(f, ", found {found}")?;
                    }
                }
                Ok(())
            }
            ParseError::Unexpected(token) => write!(f, "Unexpected token {token}"),
            ParseError::InvalidArgCount(n) => write!(f, "Invalid argument count `{n}`"),
            ParseError::InvalidOutCount(n) => write!(f, "Invalid output count `{n}`"),
            ParseError::ModifierImportName => {
                write!(f, "Modifier names may not be used as import names")
            }
            ParseError::SplitInModifier => write!(
                f,
                "Line splitting is not allowed between modifier arguments"
            ),
            ParseError::FlipInModifier => {
                write!(f, "Line flipping is not allowed between modifier arguments")
            }
            ParseError::RecursionLimit => write!(f, "Parsing recursion limit reached"),
        }
    }
}

const STYLE_MAX_WIDTH: usize = 40;
const ADVICE_MAX_WIDTH: usize = 60;
const WARNING_MAX_WIDTH: usize = 80;

impl Error for ParseError {}

/// Parse Uiua code into an AST
pub fn parse(
    input: &str,
    src: impl IntoInputSrc,
    inputs: &mut Inputs,
) -> (Vec<Item>, Vec<Sp<ParseError>>, Vec<Diagnostic>) {
    let (tokens, lex_errors, src) = lex(input, src, inputs);
    fn parse(
        input: &str,
        inputs: &mut Inputs,
        tokens: Vec<Sp<crate::lex::Token>>,
        lex_errors: Vec<Sp<LexError>>,
        src: InputSrc,
    ) -> (Vec<Item>, Vec<Sp<ParseError>>, Vec<Diagnostic>) {
        let errors: Vec<_> = lex_errors
            .into_iter()
            .map(|e| e.map(ParseError::Lex))
            .collect();
        let mut diagnostics = Vec::new();

        // Check for lines that are too long
        if !matches!(src, InputSrc::Macro(_)) {
            for line in tokens.split(|t| matches!(t.value, Newline)) {
                let mut heuristic = 0;
                let mut first = None;
                let mut toks = line.iter().peekable();
                let mut array_depth = 0i32;
                let mut underscore = false;
                while let Some(tok) = toks.next() {
                    first = first.or(Some(&tok.span));
                    let start = heuristic;
                    heuristic += match &tok.value {
                        Spaces | Comment => 0,
                        Simple(OpenBracket | OpenCurly) => {
                            array_depth += 1;
                            1
                        }
                        Simple(CloseBracket | CloseCurly) => {
                            array_depth -= 1;
                            0
                        }
                        Simple(CloseParen) => 0,
                        Simple(Underscore) => {
                            underscore = true;
                            0
                        }
                        MultilineString(_) => {
                            while let Some(MultilineString(_)) = toks.peek().map(|t| &t.value) {
                                toks.next();
                            }
                            1
                        }
                        Number | Str(_) | Char(_) if array_depth > 0 => 0,
                        _ => 1,
                    };
                    if heuristic > start && underscore {
                        underscore = false;
                        heuristic = start;
                    }
                }
                if heuristic <= STYLE_MAX_WIDTH {
                    continue;
                }
                let first = first.unwrap().clone();
                let last = line.last().unwrap().span.clone();
                let span = first.merge(last);
                let (kind, face) = if heuristic > WARNING_MAX_WIDTH {
                    (DiagnosticKind::Warning, '😤')
                } else if heuristic > ADVICE_MAX_WIDTH {
                    (DiagnosticKind::Advice, '😠')
                } else {
                    (DiagnosticKind::Style, '🤨')
                };
                let max = match kind {
                    DiagnosticKind::Style => STYLE_MAX_WIDTH,
                    DiagnosticKind::Advice => ADVICE_MAX_WIDTH,
                    DiagnosticKind::Warning => WARNING_MAX_WIDTH,
                    DiagnosticKind::Info => unreachable!(),
                };
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Split this into multiple lines \
                        (heuristic: {heuristic}/{max}) {face}"
                    ),
                    span,
                    kind,
                    inputs.clone(),
                ));
            }
        }

        // Parse
        let base = 0u8;
        let mut parser = Parser {
            inputs,
            input: input.into(),
            tokens,
            index: 0,
            errors,
            diagnostics,
            next_output_comment: 0,
            start_addr: &base as *const u8 as usize,
        };
        let items = parser.items(ItemsKind::TopLevel);
        if parser.errors.is_empty() && parser.index < parser.tokens.len() {
            parser.errors.push(
                parser
                    .tokens
                    .remove(parser.index)
                    .map(ParseError::Unexpected),
            );
        }
        let mut errors = parser.errors;
        if let Some(error) = errors
            .iter()
            .find(|e| matches!(e.value, ParseError::RecursionLimit))
        {
            let error = error.clone();
            errors = vec![error];
        }
        (items, errors, parser.diagnostics)
    }
    parse(input, inputs, tokens, lex_errors, src)
}

struct Parser<'i> {
    inputs: &'i mut Inputs,
    input: EcoString,
    tokens: Vec<Sp<crate::lex::Token>>,
    index: usize,
    next_output_comment: usize,
    errors: Vec<Sp<ParseError>>,
    diagnostics: Vec<Diagnostic>,
    start_addr: usize,
}

type FunctionContents = (Option<Sp<Signature>>, Vec<Item>, Option<CodeSpan>);

#[derive(Clone, Copy, PartialEq, Eq)]
enum ItemsKind {
    TopLevel,
    Module,
    Function,
}

impl Parser<'_> {
    fn next_token_map<'a, T: 'a>(
        &'a mut self,
        f: impl FnOnce(&'a Token) -> Option<T>,
    ) -> Option<Sp<T>> {
        let token = self.tokens.get(self.index)?;
        if let Some(value) = f(&token.value) {
            self.index += 1;
            Some(token.span.clone().sp(value))
        } else {
            None
        }
    }
    fn exact(&mut self, token: Token) -> Option<CodeSpan> {
        self.next_token_map(|t| (t == &token).then_some(()))
            .map(|t| t.span)
    }
    fn prev_span(&self) -> CodeSpan {
        if let Some(token) = self.tokens.get(self.index.saturating_sub(1)) {
            token.span.clone()
        } else {
            self.tokens.last().unwrap().span.clone()
        }
    }
    fn curr_span(&self) -> CodeSpan {
        if let Some(token) = self.tokens.get(self.index) {
            token.span.clone()
        } else {
            self.tokens.last().unwrap().span.clone()
        }
    }
    fn expected<I: Into<Expectation>>(
        &self,
        expectations: impl IntoIterator<Item = I>,
    ) -> Sp<ParseError> {
        self.curr_span().sp(ParseError::Expected(
            expectations.into_iter().map(Into::into).collect(),
            self.tokens
                .get(self.index)
                .map(|t| self.input[t.span.byte_range()].into()),
        ))
    }
    fn items(&mut self, kind: ItemsKind) -> Vec<Item> {
        let mut items = Vec::new();
        while self.exact(Newline).is_some() {
            self.spaces();
        }
        let mut trailing_newline = false;
        loop {
            match self.item(kind) {
                Some(item) => {
                    trailing_newline = false;
                    items.push(item)
                }
                None => {
                    if self.exact(Newline).is_none() {
                        break;
                    }
                    trailing_newline = true;
                    self.spaces();
                    let mut extra_newlines = false;
                    while self.exact(Newline).is_some() {
                        extra_newlines = true;
                        self.spaces();
                    }
                    if extra_newlines {
                        items.push(Item::Words(Vec::new()));
                    }
                }
            }
        }
        if trailing_newline {
            items.push(Item::Words(Vec::new()));
        }
        items
    }
    fn item(&mut self, kind: ItemsKind) -> Option<Item> {
        if self.too_deep() {
            return None;
        }
        self.spaces();
        let item = if let Some(binding) = self.binding() {
            Item::Binding(binding)
        } else if let Some(import) = self.import() {
            Item::Import(import)
        } else if let Some(module) = self.module(kind == ItemsKind::Module) {
            Item::Module(module)
        } else if let Some(first) = self.data_def(kind != ItemsKind::Function) {
            let mut defs = vec![first];
            while let Some(def) = self.data_def(kind != ItemsKind::Function) {
                defs.push(def);
            }
            Item::Data(defs)
        } else {
            let start = self.index;
            if self.module_delim_hyphens().is_some() {
                self.index = start;
                return None;
            }
            if let Some(words) = self.words() {
                Item::Words(words)
            } else {
                return None;
            }
        };
        Some(item)
    }
    fn module(&mut self, in_module: bool) -> Option<Sp<ScopedModule>> {
        let backup = self.index;
        let (public, open_span) = self.module_open()?.into();
        self.spaces();
        // Name
        let name = self.ident();
        if in_module && name.is_none() {
            self.index = backup;
            return None;
        }
        let kind = match name {
            Some(name) if name.value == "test" => ModuleKind::Test,
            Some(name) => ModuleKind::Named(name),
            None => ModuleKind::Test,
        };
        // Imports
        while self.exact(Spaces).is_some() {}
        let imports = if let Some((tilde_span, public)) = self.tilde() {
            let mut items = Vec::new();
            loop {
                if let Some(ident) = self.ident() {
                    items.push(ident);
                } else if self.spaces().is_some() {
                    continue;
                } else {
                    break;
                }
            }
            if items.is_empty() {
                None
            } else {
                Some(ImportLine {
                    tilde_span,
                    public,
                    items,
                })
            }
        } else {
            None
        };
        // Items
        let items = self.items(ItemsKind::Module);
        let close_span = self.module_close();
        let span = if let Some(end) = close_span.clone() {
            open_span.clone().merge(end)
        } else {
            self.errors.push(self.expected([Expectation::CloseModule]));
            open_span.clone()
        };
        let module = ScopedModule {
            open_span,
            public,
            kind,
            items,
            imports,
            close_span,
        };
        Some(span.sp(module))
    }
    fn comment(&mut self) -> Option<Sp<EcoString>> {
        let span = self.exact(Token::Comment)?;
        let s = &self.input[span.byte_range()];
        let s = s
            .strip_prefix("# ")
            .or_else(|| s.strip_prefix("#"))
            .unwrap_or(s)
            .into();
        Some(span.sp(s))
    }
    fn output_comment(&mut self) -> Option<Sp<Word>> {
        let n = self.next_token_map(Token::as_output_comment)?;
        let i = self.next_output_comment;
        self.next_output_comment += 1;
        Some(n.span.sp(Word::OutputComment { i, n: n.value }))
    }
}

struct BindingInit {
    name: Sp<Ident>,
    arrow_span: CodeSpan,
    public: bool,
    array_macro: bool,
}

impl Parser<'_> {
    fn binding_init(&mut self) -> Option<BindingInit> {
        let start = self.index;
        let name = self.ident()?;
        // Left arrow
        let arrow_span = self.spaces().map(|w| w.span);
        let (glyph_span, public) =
            if let Some(span) = self.exact(Equal.into()).or_else(|| self.exact(LeftArrow)) {
                (span, true)
            } else if let Some(span) = self.exact(LeftStrokeArrow) {
                (span, false)
            } else {
                self.index = start;
                return None;
            };
        let mut arrow_span = if let Some(arrow_span) = arrow_span {
            arrow_span.merge(glyph_span)
        } else {
            glyph_span
        };
        let array_macro = if let Some(span) = self.exact(Placeholder(None)) {
            arrow_span = arrow_span.merge(span);
            true
        } else {
            false
        };
        if let Some(span) = self.spaces().map(|w| w.span) {
            arrow_span = arrow_span.merge(span);
        }
        Some(BindingInit {
            name,
            arrow_span,
            public,
            array_macro,
        })
    }
    fn binding(&mut self) -> Option<Binding> {
        let BindingInit {
            name,
            arrow_span,
            public,
            array_macro,
        } = self.binding_init()?;
        // Bad name advice
        if ["\u{200b}", "\u{200c}", "\u{200d}"]
            .iter()
            .any(|bad_name| &*name.value == *bad_name)
        {
            self.diagnostics.push(Diagnostic::new(
                "Maybe don't".into(),
                name.span.clone(),
                DiagnosticKind::Advice,
                self.inputs.clone(),
            ));
        }
        // Signature
        let words_start = self.index;
        let signature = self.signature(true);
        // Words
        let words = self.words().unwrap_or_default();
        let words_end = self.index;

        fn iter_chars(tokens: &[Sp<Token>], inputs: &Inputs, mut f: impl FnMut(char)) {
            use Primitive::*;
            use Token::*;
            for (i, token) in tokens.iter().enumerate() {
                match &token.value {
                    Token::Newline => {}
                    Token::Spaces => {
                        let prev_prev = (i > 1).then(|| &tokens[i - 2]);
                        let prev = (i > 0).then(|| &tokens[i - 1]);
                        let next = tokens.get(i + 1);
                        let next_next = tokens.get(i + 2);
                        let Some((prev, next)) = prev.zip(next) else {
                            continue;
                        };
                        let count_it = match (&prev.value, &next.value) {
                            (Glyph(Neg), Number) => true,
                            (Number, Glyph(Dup)) => {
                                next_next.is_some_and(|t| matches!(t.value, Number))
                            }
                            (Glyph(Dup), Number) => {
                                prev_prev.is_some_and(|t| matches!(t.value, Number))
                            }
                            (Glyph(Lt), Simple(Equal)) => true,
                            (Glyph(Gt), Simple(Equal)) => true,
                            (Number, Number) => true,
                            (Ident(_), Ident(_)) => true,
                            (Ident(_), Glyph(p)) => p.to_string().starts_with(is_ident_char),
                            _ => false,
                        };
                        if count_it {
                            f(' ');
                        }
                    }
                    Token::Comment | Token::OutputComment(_) => {}
                    _ => token.span.as_str(inputs, |s| s.chars().for_each(&mut f)),
                }
            }
        }
        let char_count = {
            let mut count = 0;
            iter_chars(&self.tokens[words_start..words_end], self.inputs, |_| {
                count += 1
            });
            count
        };
        let sbcs_count = {
            thread_local! {
                static SBCS_CHARS: Vec<char> = {
                    let mut chars = Vec::new();
                    chars.extend(Primitive::non_deprecated().filter_map(|p| p.glyph()));
                    chars.extend(' '..='~');
                    chars.extend(SUBSCRIPT_DIGITS);
                    chars.extend("←↚‼′″‴₋⌞⌟↓".chars());
                    chars.sort_unstable();
                    chars
                };
            }
            SBCS_CHARS.with(|chars| {
                debug_assert!(chars.len() < 256);
                let mut count = 0;
                iter_chars(&self.tokens[words_start..words_end], self.inputs, |c| {
                    count += 1 + if chars.binary_search(&c).is_ok() {
                        0
                    } else {
                        2 * c.len_utf16()
                    };
                });
                count
            })
        };

        self.validate_binding_name(&name);
        Some(Binding {
            name,
            arrow_span,
            public,
            code_macro: array_macro,
            words,
            signature,
            counts: BindingCounts {
                char: char_count,
                sbcs: sbcs_count,
            },
        })
    }
    fn ignore_whitespace(&mut self) -> bool {
        let mut newline = false;
        self.spaces();
        while self.exact(Newline).is_some() {
            newline = true;
            self.spaces();
        }
        newline
    }
    fn data_def(&mut self, allow_variants: bool) -> Option<DataDef> {
        let reset = self.index;
        let mut variant = false;
        let (init_span, public) = (self.exact(Tilde.into()).map(|span| (span, true)))
            .or_else(|| {
                self.exact(DoubleTilde.into())
                    .or_else(|| self.exact(TildeStroke))
                    .map(|span| (span, false))
            })
            .or_else(|| {
                if allow_variants {
                    variant = true;
                    let span = self.exact(Bar.into())?;
                    Some((span, true))
                } else {
                    None
                }
            })?;
        self.spaces();
        let name = self.ident();
        self.spaces();
        let mut boxed = false;
        let open_span = if let Some(span) = self.exact(OpenBracket.into()) {
            Some(span)
        } else if let Some(span) = self.exact(OpenCurly.into()) {
            boxed = true;
            Some(span)
        } else if variant {
            None
        } else {
            self.index = reset;
            return None;
        };
        let fields = if let Some(open_span) = open_span {
            let mut fields = Vec::new();
            while self.exact(Newline).is_some() {
                self.spaces();
            }
            self.spaces();
            let mut trailing_newline = false;
            let post_comments = loop {
                let comments = self.comments();
                let Some(name) = self.ident() else {
                    break comments;
                };
                trailing_newline = false;
                self.spaces();

                // Validator
                let mut validator = None;
                let mut colon = false;
                if let Some(mut open_span) = self
                    .exact(Colon.into())
                    .inspect(|_| colon = true)
                    .or_else(|| self.exact(OpenParen.into()))
                {
                    if let Some(span) = self.spaces().map(|w| w.span) {
                        open_span = open_span.merge(span);
                    }
                    let words = self.words().unwrap_or_else(|| {
                        self.errors.push(self.expected([Expectation::Term]));
                        Vec::new()
                    });
                    let close_span = self.exact(CloseParen.into());
                    validator = Some(FieldValidator {
                        open_span,
                        close_span,
                        words,
                    });
                }

                // Initializer
                let mut init = None;
                let start_arrow_span = self.spaces().map(|w| w.span);
                if let Some(mut arrow_span) =
                    self.exact(Equal.into()).or_else(|| self.exact(LeftArrow))
                {
                    arrow_span = if let Some(start) = start_arrow_span {
                        start.merge(arrow_span)
                    } else {
                        arrow_span
                    };
                    if let Some(span) = self.spaces().map(|w| w.span) {
                        arrow_span = arrow_span.merge(span);
                    }
                    let words = self.words().unwrap_or_else(|| {
                        self.errors.push(self.expected([Expectation::Term]));
                        Vec::new()
                    });
                    init = Some(FieldInit { arrow_span, words })
                };

                let mut eol_comment = None;
                if init.is_none() {
                    eol_comment = self.comment();
                    self.next_token_map(Token::as_semantic_comment); // Eat semantic comment
                }

                trailing_newline |= self.ignore_whitespace();
                let mut bar_span = self.exact(Bar.into());
                if self.exact(Newline).is_some() || self.exact(DoubleSemicolon.into()).is_some() {
                    bar_span = None;
                }
                if bar_span.is_some() {
                    trailing_newline = false;
                }
                fields.push(DataField {
                    comments,
                    name,
                    validator,
                    init,
                    eol_comment,
                    bar_span,
                });
                trailing_newline |= self.ignore_whitespace();
            };
            let close = self.expect_close(if boxed { CloseCurly } else { CloseBracket }.into());
            let close_span = close.value.then_some(close.span);
            self.spaces();
            Some(DataFields {
                boxed,
                open_span,
                fields,
                post_comments,
                trailing_newline,
                close_span,
            })
        } else {
            None
        };

        let func = self.words();
        Some(DataDef {
            init_span,
            public,
            variant,
            name,
            fields,
            func,
        })
    }
    fn validate_binding_name(&mut self, name: &Sp<Ident>) {
        if name
            .value
            .trim_end_matches(['!', '‼', '\'', '′', '″', '‴'])
            .chars()
            .count()
            >= 2
            && name.value.chars().next().unwrap().is_ascii_lowercase()
        {
            let captialized: String = name
                .value
                .chars()
                .next()
                .map(|c| c.to_ascii_uppercase())
                .into_iter()
                .chain(name.value.chars().skip(1))
                .collect();
            self.diagnostics.push(Diagnostic::new(
                format!(
                    "Binding names with 2 or more characters should be TitleCase \
                    to avoid collisions with future builtin functions. Try `{}` instead of `{}`.",
                    captialized, name.value
                ),
                name.span.clone(),
                DiagnosticKind::Advice,
                self.inputs.clone(),
            ));
        }
    }
    fn tilde(&mut self) -> Option<(CodeSpan, bool)> {
        (self.exact(Tilde.into()).map(|s| (s, true))).or_else(|| {
            self.exact(TildeStroke)
                .or_else(|| self.exact(DoubleTilde.into()))
                .map(|s| (s, false))
        })
    }
    fn import(&mut self) -> Option<Import> {
        let reset = self.index;
        // Name
        let name = self.ident();
        self.spaces();
        // Tilde
        let Some((tilde_span, public)) = self.tilde() else {
            self.index = reset;
            return None;
        };
        self.spaces();
        // Path
        let Some(path) = self.next_token_map(Token::as_string) else {
            self.index = reset;
            return None;
        };
        let path = path.map(Into::into);
        self.spaces();
        // Items
        let mut lines: Vec<Option<ImportLine>> = Vec::new();
        let mut line: Option<ImportLine> = None;
        let mut last_tilde_index = self.index;
        let mut line_reset = self.index;
        loop {
            if let Some((line, ident)) = line
                .as_mut()
                .and_then(|line| self.ident().map(|ident| (line, ident)))
            {
                line.items.push(ident);
                continue;
            }
            let Some(token) = self.tokens.get(self.index).cloned() else {
                break;
            };
            let span = token.span;
            let token = token.value;
            match token {
                Token::Ident(ident) if line.is_some() => {
                    let line = line.as_mut().unwrap();
                    let name = span.clone().sp(ident);
                    line.items.push(name);
                }
                Simple(Tilde) if line.is_none() => {
                    last_tilde_index = self.index;
                    line = Some(ImportLine {
                        tilde_span: span.clone(),
                        public: true,
                        items: Vec::new(),
                    })
                }
                TildeStroke | Simple(DoubleTilde) if line.is_none() => {
                    last_tilde_index = self.index;
                    line = Some(ImportLine {
                        tilde_span: span.clone(),
                        public: false,
                        items: Vec::new(),
                    })
                }
                Simple(Tilde) => (self.errors).push(span.sp(ParseError::Unexpected(Simple(Tilde)))),
                Newline => {
                    lines.push(line.take());
                    line_reset = self.index;
                }
                Spaces => {}
                Simple(OpenBracket | OpenCurly) => {
                    self.index = line_reset;
                    line = None;
                    break;
                }
                _ => break,
            }
            self.index += 1;
        }
        if let Some(line) = line {
            if line.items.is_empty() {
                self.index = last_tilde_index;
            } else {
                lines.push(Some(line));
            }
        }
        if let Some(name) = &name {
            self.validate_binding_name(name);
            if name.value.contains(['!', '‼']) {
                self.errors
                    .push(name.span.clone().sp(ParseError::ModifierImportName));
            }
        }
        Some(Import {
            name,
            tilde_span,
            public,
            path,
            lines,
        })
    }
    fn ident(&mut self) -> Option<Sp<Ident>> {
        let mut ident = self.next_token_map(Token::as_ident)?;
        // Add subscript
        while let Some(sub) = self.next_token_map(Token::as_subscript) {
            ident.span.merge_with(sub.span);
            ident.value.push_str(&sub.value.to_string());
        }
        // Add exclams
        while let Some(exclams) = self.next_token_map(|tok| {
            tok.as_ident()
                .filter(|ident| ident.chars().all(|c| "!‼".contains(c)))
        }) {
            ident.span.merge_with(exclams.span);
            ident.value.push_str(&exclams.value);
        }
        Some(ident)
    }
    fn ref_inner(&mut self) -> Option<Result<Ref, Vec<RefComponent>>> {
        let mut checkpoint = self.index;
        let mut name = self.ident()?;
        let mut path = Vec::new();
        while let Some(dot_span) = (self.tokens.get(self.index + 1))
            .is_none_or(|t| t.value != Glyph(Primitive::Dup))
            .then(|| self.exact(Glyph(Primitive::Dup)))
            .flatten()
            .or_else(|| self.exact(Tilde.into()))
        {
            let comp = RefComponent {
                module: name,
                dot_span,
            };
            let Some(next) = self.ident() else {
                self.spaces();
                if self
                    .tokens
                    .get(self.index)
                    .is_none_or(|t| !matches!(t.value, Token::Str(_)))
                {
                    path.push(comp);
                    return Some(Err(path));
                }
                self.index = checkpoint;
                return None;
            };
            checkpoint = self.index;
            path.push(comp);
            name = next;
        }
        Some(Ok(Ref { name, path }))
    }
    fn chained_ref(&mut self) -> Option<Sp<Word>> {
        let first = match self.ref_inner()? {
            Ok(r) => r,
            Err(path) => {
                let span = (path.first().unwrap().module.span.clone())
                    .merge(path.last().unwrap().dot_span.clone());
                return Some(span.sp(Word::IncompleteRef(path)));
            }
        };
        let mut chained = Vec::new();
        while let Some(dot_span) = self
            .exact(DoubleTilde.into())
            .or_else(|| self.exact(AlmostEqual))
            .or_else(|| self.exact(Ellipses))
            .or_else(|| {
                let reset = self.index;
                if let Some((first, second)) = self
                    .exact(Glyph(Primitive::Dup))
                    .zip(self.exact(Glyph(Primitive::Dup)))
                {
                    Some(first.merge(second))
                } else {
                    self.index = reset;
                    None
                }
            })
        {
            let reset = self.index;
            let Some(Ok(item)) = self.ref_inner() else {
                self.index = reset;
                break;
            };
            chained.push(ChainComponent { dot_span, item })
        }
        let mut span = first.span();
        if let Some(comp) = chained.last() {
            span.merge_with(comp.item.name.span.clone());
        }
        Some(span.sp(Word::Ref(first, chained)))
    }
    fn signature(&mut self, error_on_invalid: bool) -> Option<Sp<Signature>> {
        let reset = self.index;
        let start = self.exact(Bar.into())?;
        let inner = self.sig_inner();
        if inner.is_none() {
            if error_on_invalid {
                self.errors.push(self.expected([Expectation::ArgsOutputs]));
            }
            self.index = reset;
        }
        let (args, outs) = inner?;
        let mut end = self.prev_span();
        if let Some(sp) = self.spaces() {
            end = sp.span;
        }
        let span = start.merge(end);
        Some(span.sp(Signature::new(args, outs)))
    }
    fn sig_inner(&mut self) -> Option<(usize, usize)> {
        let range = self.real()?.span.byte_range();
        let s = &self.input[range];
        Some(if let Some((a, o)) = s.split_once('.') {
            let a = match a.parse() {
                Ok(a) => a,
                Err(_) => {
                    self.errors
                        .push(self.prev_span().sp(ParseError::InvalidArgCount(a.into())));
                    1
                }
            };
            let o = match o.parse() {
                Ok(o) => o,
                Err(_) => {
                    self.errors
                        .push(self.prev_span().sp(ParseError::InvalidOutCount(o.into())));
                    1
                }
            };
            (a, o)
        } else {
            let a = match s.parse() {
                Ok(a) => a,
                Err(_) => {
                    self.errors
                        .push(self.prev_span().sp(ParseError::InvalidArgCount(s.into())));
                    1
                }
            };
            (a, 1)
        })
    }
    #[allow(clippy::single_match)]
    fn words(&mut self) -> Option<Vec<Sp<Word>>> {
        let mut words: Vec<Sp<Word>> = Vec::new();
        while let Some(word) = self.word() {
            if let Some(prev) = words.iter().filter(|w| w.value.is_code()).nth_back(0) {
                // Diagnostics
                use Primitive::*;
                let span = || prev.span.clone().merge(word.span.clone());
                match (&prev.value, &word.value) {
                    (Word::Primitive(a), Word::Primitive(b)) => {
                        match (a, b) {
                            // Not comparisons
                            (Not, prim) => {
                                for (a, b) in [(Eq, Ne), (Lt, Ge), (Gt, Le)] {
                                    if *prim == a {
                                        self.diagnostics.push(Diagnostic::new(
                                            format!("Prefer `{b}` over `{Not}{prim}` for clarity"),
                                            span(),
                                            DiagnosticKind::Style,
                                            self.inputs.clone(),
                                        ));
                                    } else if *prim == b {
                                        self.diagnostics.push(Diagnostic::new(
                                            format!("Prefer `{a}` over `{Not}{prim}` for clarity"),
                                            span(),
                                            DiagnosticKind::Style,
                                            self.inputs.clone(),
                                        ));
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }

            words.push(word);
        }
        if words.is_empty() { None } else { Some(words) }
    }
    fn word(&mut self) -> Option<Sp<Word>> {
        self.comment()
            .map(|c| c.map(Word::Comment))
            .or_else(|| self.output_comment())
            .or_else(|| self.strand())
    }
    fn strand(&mut self) -> Option<Sp<Word>> {
        let word = self.modified()?;
        if let Word::Spaces = word.value {
            return Some(word);
        }
        // Collect items
        let mut items = Vec::new();
        while self.exact(Underscore.into()).is_some() {
            let item = match self.modified() {
                Some(mut item) => {
                    if let Word::Spaces = item.value {
                        if items.is_empty() {
                            break;
                        }
                        self.errors.push(self.expected([Expectation::Term]));
                        item = match self.modified() {
                            Some(item) => item,
                            None => {
                                self.errors.push(self.expected([Expectation::Term]));
                                break;
                            }
                        };
                    }
                    item
                }
                None => {
                    self.errors.push(self.expected([Expectation::Term]));
                    break;
                }
            };
            items.push(item);
        }
        // If there is only one item and no underscores, return it
        if items.is_empty() {
            return Some(word);
        }
        // Insert the first word that was parsed
        items.insert(0, word);
        let span = items[0]
            .span
            .clone()
            .merge(items.last().unwrap().span.clone());
        Some(span.sp(Word::Strand(items)))
    }
    fn modified(&mut self) -> Option<Sp<Word>> {
        if self.too_deep() {
            return None;
        }
        let (modifier, mod_span) = if let Some(prim) = Primitive::all()
            .filter(|prim| prim.is_modifier())
            .find_map(|prim| {
                self.exact(prim.into())
                    .or_else(|| prim.ascii().and_then(|simple| self.exact(simple.into())))
                    .map(|span| span.sp(prim))
            }) {
            (Modifier::Primitive(prim.value), prim.span)
        } else {
            let term = self.term()?;
            match term.value {
                Word::Ref(item, chained) => {
                    if item.modifier_args() == 0 {
                        return Some(term.span.sp(Word::Ref(item, chained)));
                    }
                    (Modifier::Ref(item), term.span)
                }
                Word::InlineMacro(mac) => (Modifier::Macro(mac), term.span),
                _ => return Some(term),
            }
        };
        self.spaces();
        let mut subscript = None;
        if let Some(n) = self.next_token_map(Token::as_subscript) {
            subscript = Some(n);
            self.spaces();
        }
        let mut args = Vec::new();
        let margs = modifier.subscript_margs(subscript.as_ref().map(|s| &s.value));
        for i in 0..margs {
            loop {
                args.extend(self.spaces());
                if let Some(span) = self.exact(DoubleSemicolon.into()) {
                    self.errors.push(span.sp(ParseError::SplitInModifier));
                    continue;
                }
                if let Some(span) = self.exact(Semicolon.into()) {
                    self.errors.push(span.sp(ParseError::FlipInModifier));
                    continue;
                }
                break;
            }
            if let Some(arg) = self.strand() {
                // Parse pack syntax
                if let Word::Pack(_) = &arg.value
                    && i == 0
                {
                    args.push(arg);
                    break;
                }
                args.push(arg);
            } else {
                break;
            }
        }

        let span = if let Some(last) = args.last() {
            mod_span.clone().merge(last.span.clone())
        } else {
            mod_span.clone()
        };

        match &modifier {
            Modifier::Primitive(Primitive::Un) => {
                single_word_and(&args, |inverted| {
                    if let Word::Array(arr) = &inverted.value
                        && arr_is_normal_di(arr)
                    {
                        self.diagnostics.pop(); // Pop lower diagnostic
                        self.diagnostics.push(Diagnostic::new(
                            format!(
                                "Prefer `{}{}` ({}{}) over `{}[{}{}]`",
                                Primitive::Un,
                                Primitive::Couple,
                                Primitive::Un.name(),
                                Primitive::Couple.name(),
                                Primitive::Un,
                                Primitive::Dip,
                                Primitive::Identity
                            ),
                            span.clone(),
                            DiagnosticKind::Style,
                            self.inputs.clone(),
                        ));
                    }
                });
            }
            Modifier::Primitive(Primitive::Bracket) => {
                let mut operands = Vec::new();
                if let Some(Sp {
                    value: Word::Pack(pack),
                    ..
                }) = args.first()
                {
                    operands.extend(
                        pack.branches
                            .iter()
                            .map(|branch| Word::Func(branch.value.clone())),
                    );
                } else {
                    operands.extend(
                        args.iter()
                            .map(|arg| &arg.value)
                            .filter(|word| word.is_code())
                            .cloned(),
                    );
                }
            }
            _ => (),
        }

        let mut word = span.sp(Word::Modified(Box::new(Modified {
            modifier: mod_span.sp(modifier),
            operands: args,
            pack_expansion: false,
        })));

        if let Some(n) = subscript {
            let span = word.span.clone().merge(n.span.clone());
            word = span.sp(Word::Subscripted(Box::new(crate::ast::Subscripted {
                script: n,
                word,
            })));
        }

        Some(word)
    }
    fn too_deep(&mut self) -> bool {
        #[cfg(not(target_arch = "wasm32"))]
        const MAX_RECURSION_DEPTH: usize = (512 + 256) * 1024;
        #[cfg(target_arch = "wasm32")]
        const MAX_RECURSION_DEPTH: usize = 512 * 1024;
        let curr = 0u8;
        let curr_addr = &curr as *const u8 as usize;
        let diff = curr_addr.abs_diff(self.start_addr);
        let too_deep = diff > MAX_RECURSION_DEPTH;
        if too_deep {
            self.errors
                .push(self.prev_span().sp(ParseError::RecursionLimit));
        }
        too_deep
    }
    fn term(&mut self) -> Option<Sp<Word>> {
        if self.too_deep() {
            return None;
        }
        let mut word = if let Some(n) = self.num() {
            n.map(|(n, s)| Word::Number(n, s))
        } else if let Some(refer) = self.chained_ref() {
            refer
        } else if let Some(prim) = self.prim() {
            prim.map(Word::Primitive)
        } else if let Some(n) = self.num() {
            n.map(|(n, s)| Word::Number(n, s))
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Word::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Word::String)
        } else if let Some(op) = self.next_token_map(Token::as_placeholder) {
            op.map(Word::Placeholder)
        } else if let Some(label) = self.next_token_map(Token::as_label) {
            if let Some(sub) = self.next_token_map(Token::as_subscript) {
                (label.span.merge(sub.span))
                    .sp(Word::Label(format!("{}{}", label.value, sub.value)))
            } else {
                label.map(Into::into).map(Word::Label)
            }
        } else if let Some(frags) = self.next_token_map(Token::as_format_string) {
            frags.map(Word::FormatString)
        } else if let Some(line) = self.next_token_map(Token::as_multiline_string) {
            let mut span = line.span.clone();
            let mut lines = vec![line];
            while let Some(line) = self.next_token_map(Token::as_multiline_string) {
                span = span.merge(line.span.clone());
                lines.push(line);
            }
            span.sp(Word::MultilineString(lines))
        } else if let Some(line) = self.next_token_map(Token::as_multiline_format_string) {
            let start = line.span.clone();
            let mut end = start.clone();
            let mut lines = vec![line];
            while let Some(line) = self.next_token_map(Token::as_multiline_format_string) {
                end = line.span.clone();
                lines.push(line);
            }
            let span = start.merge(end);
            span.sp(Word::MultilineFormatString(lines))
        } else if let Some(spaces) = self.spaces() {
            spaces
        } else if let Some(word) = self.func_or_array() {
            word
        } else if let Some(span) = self.exact(Semicolon.into()) {
            span.sp(Word::FlipLine)
        } else if let Some(span) = self.exact(DoubleSemicolon.into()) {
            span.sp(Word::BreakLine)
        } else if let Some(sc) = self.next_token_map(Token::as_semantic_comment) {
            sc.map(Word::SemanticComment)
        } else {
            return None;
        };
        if matches!(word.value, Word::Spaces | Word::InlineMacro(_)) {
            return Some(word);
        }
        loop {
            let reset = self.index;
            self.spaces();
            if let Some(n) = self.next_token_map(Token::as_subscript) {
                let span = word.span.clone().merge(n.span.clone());
                word = span.sp(Word::Subscripted(Box::new(crate::ast::Subscripted {
                    script: n,
                    word,
                })));
            } else {
                self.index = reset;
                break;
            }
        }
        Some(word)
    }
    fn num(&mut self) -> Option<Sp<(NumWord, String)>> {
        let reset = self.index;
        // Numerator
        let ((numer, mut s), mut span) = self.numer_or_denom()?.into();
        // Denominator
        if !s.contains(['.', ',', '∞']) {
            let reset = self.index;
            if self.exact(Primitive::Reduce.into()).is_some() {
                if let Some(((denom, ds), dspan)) = self
                    .numer_or_denom()
                    .filter(|n| !n.value.1.contains(['.', '∞']))
                    .map(Into::into)
                {
                    let n = numer.map_with(denom, |n, d| n / d, |n, d| n / d);
                    s.push('/');
                    s.push_str(&ds);
                    if s.contains('¯') {
                        let neg_count = s.chars().filter(|&c| c == '¯').count();
                        if neg_count == 2 {
                            s = s.replace('¯', "");
                        } else if neg_count == 1 && !s.starts_with('¯') {
                            s = s.replace('¯', "");
                            s.insert(0, '¯');
                        }
                    }
                    span.merge_with(dspan);
                    return Some(span.sp((n, s)));
                } else {
                    self.index = reset;
                }
            }
        }
        // Let 1-letter string be identifiers
        if ["r", "i"].contains(&s.as_str()) {
            self.index = reset;
            return None;
        }
        Some(span.sp((numer, s)))
    }
    fn numer_or_denom(&mut self) -> Option<Sp<(NumWord, String)>> {
        fn suffix(s: &str) -> Option<char> {
            s.chars().next_back().filter(|c| "ri".contains(*c))
        }
        let first = self.num_frag(false)?;
        let Some(first_suffix) = suffix(&first.value.1) else {
            return Some(first);
        };
        let mut suffixes = vec![first_suffix];
        let mut frags = vec![first];
        let mut reset = self.index;
        while let Some(frag) = self.num_frag(false) {
            let Some(suffix) = suffix(&frag.value.1) else {
                self.index = reset;
                break;
            };
            if suffixes.contains(&suffix) {
                self.index = reset;
                break;
            }
            frags.push(frag);
            suffixes.push(suffix);
            reset = self.index;
        }
        // Sort fragments
        frags.sort_by_key(|frag| match suffix(&frag.value.1).unwrap() {
            'r' => 0,
            'i' => 1,
            _ => unreachable!(),
        });
        // Merge fragments
        let mut frags = frags.into_iter().map(Into::into);
        let ((mut n, mut s), mut span) = frags.next().unwrap();
        for ((n2, s2), span2) in frags {
            n = n.map_with(n2, |n, n2| n + n2, |n, n2| n + n2);
            s.push_str(&s2);
            span.merge_with(span2);
        }
        // Final suffix
        if !s.contains(['η', 'π', 'τ'])
            && let Some(((n2, s2), span2)) = self.num_frag(true).map(Into::into)
        {
            n = n.map_with(n2, |n, n2| n * n2, Complex::safe_mul);
            s.push_str(&s2);
            span.merge_with(span2);
        }
        Some(span.sp((n, s)))
    }
    fn neg(&mut self) -> Option<CodeSpan> {
        self.exact(Primitive::Neg.into())
            .or_else(|| self.exact(Backtick.into()))
    }
    fn num_frag(&mut self, suffix_mode: bool) -> Option<Sp<(NumWord, String)>> {
        let reset = self.index;
        let mut is_inf = false;
        let (coef, mut s, span) = if suffix_mode {
            (Ok(1.0), String::new(), None)
        } else if let Some((r, span)) = self.real().map(Into::into) {
            let s = &self.input[span.byte_range()];
            let mut s = match &r {
                Ok(_) if s.contains(['e', 'E']) => {
                    if s.contains('`') {
                        s.replace('`', "¯")
                    } else {
                        s.into()
                    }
                }
                Ok(_) if s.contains(',') => {
                    let (whole, frac) = s.split_once('.').map_or((s, ""), |pair| pair);
                    if let Some(sec) = (whole.split(',').skip(1).find(|sec| sec.len() <= 1))
                        .filter(|_| whole.contains(','))
                        .or_else(|| {
                            (frac.rsplit(',').skip(1).find(|sec| sec.len() <= 1))
                                .filter(|_| frac.contains(','))
                        })
                        .or_else(|| {
                            (whole.rsplit(',').next())
                                .filter(|sec| sec.len() <= 2 && whole.contains(','))
                        })
                    {
                        self.diagnostics.push(Diagnostic::new(
                            format!("Separated section `{sec}` is too short"),
                            span.clone(),
                            DiagnosticKind::Style,
                            self.inputs.clone(),
                        ));
                    }
                    s.into()
                }
                Ok(_) if s.contains('.') && s.ends_with('0') => s.into(),
                Ok(n) => n.to_string(),
                Err(_) => s.into(),
            };
            if s.contains(['`', '-']) {
                s = s.replace(['`', '-'], "¯");
            }
            (r, s, Some(span))
        } else if let Some(((n, s), span)) = self.infinity().map(Into::into) {
            is_inf = true;
            (Ok(n), s, Some(span))
        } else if let Some(span) = self.neg() {
            (Ok(-1.0), "¯".into(), Some(span))
        } else {
            (Ok(1.0), String::new(), None)
        };

        let (mut n, mut s, mut span) = if coef == Ok(0.0) {
            // Can't group other stuff with 0
            (coef.into(), s, span.unwrap())
        } else if let Some((sym, mut is_circle)) =
            (!is_inf).then(|| self.symbolic_num(true, &[])).flatten()
        {
            // Real constants
            let ((mut n, sym_s), sym_span) = sym.into();
            let mut used = vec![n.clone()];
            let mut span = match span {
                Some(span) => span.merge(sym_span),
                None => sym_span,
            };
            s.push_str(&sym_s);
            if !suffix_mode {
                while let Some((sym, new_is_circle)) = self.symbolic_num(!is_circle, &used) {
                    let ((new_n, new_s), new_span) = sym.into();
                    is_circle |= new_is_circle;
                    used.push(new_n.clone());
                    s.push_str(&new_s);
                    n = n.map_with(new_n, |n, new| n * new, Complex::safe_mul);
                    span.merge_with(new_span);
                }
            }
            let n = n.map_with(coef.into(), |n, c| c * n, Complex::safe_mul);
            (n, s, span)
        } else if let Some(((n2, s2), span2)) = (!suffix_mode)
            .then(|| self.complex_comp())
            .flatten()
            .map(Into::into)
        {
            // Negative complex
            let n = n2.map_with(coef.into(), |n, c| c * n, Complex::safe_mul);
            s.push_str(&s2);
            let span = match span {
                Some(span) => span.merge(span2),
                None => span2,
            };
            return Some(span.sp((n, s)));
        } else if let Some(span) = span {
            if s == "¯" {
                self.index = reset;
                return None;
            } else {
                // Just the number
                (coef.into(), s, span)
            }
        } else {
            self.index = reset;
            return None;
        };

        // Complex component
        if !suffix_mode && let Some(((n2, s2), span2)) = self.complex_comp().map(Into::into) {
            n = n.map_with(n2, |n, c| n * c, Complex::safe_mul);
            s.push_str(&s2);
            span.merge_with(span2);
        }

        Some(span.sp((n, s)))
    }
    fn symbolic_num(
        &mut self,
        allow_circle: bool,
        used: &[NumWord],
    ) -> Option<(Sp<(NumWord, String)>, bool)> {
        if allow_circle
            && let Some(n_span) = [
                (Primitive::Eta, PI / 2.0),
                (Primitive::Pi, PI),
                (Primitive::Tau, TAU),
            ]
            .into_iter()
            .find_map(|(prim, n)| Some(self.exact(prim.into())?.sp((n.into(), prim.to_string()))))
        {
            return Some((n_span, true));
        }
        let reset = self.index;
        let ident = self.ident()?;
        if let Some(num) = NumComponent::from_format_name(&ident.value).filter(|num| {
            !matches!(num.value(), NumWord::Complex(_)) && !used.contains(&num.value())
        }) {
            Some((ident.map(|s| (num.value(), s.into())), false))
        } else {
            self.index = reset;
            None
        }
    }
    fn complex_comp(&mut self) -> Option<Sp<(NumWord, String)>> {
        let reset = self.index;
        let ident = self.ident()?;
        if let Some(comp) = NumComponent::from_format_name(&ident.value)
            .filter(|num| matches!(num.value(), NumWord::Complex(_)))
        {
            Some(ident.span.sp((comp.value(), comp.name().into())))
        } else {
            self.index = reset;
            None
        }
    }
    fn infinity(&mut self) -> Option<Sp<(f64, String)>> {
        let reset = self.index;
        if let Some(span_a) = self.neg() {
            if let Some(span_b) = self.exact(Primitive::Infinity.into()) {
                Some((span_a.merge(span_b)).sp((f64::NEG_INFINITY, "¯∞".into())))
            } else {
                self.index = reset;
                None
            }
        } else {
            self.exact(Primitive::Infinity.into())
                .map(|span| span.sp((f64::INFINITY, "∞".into())))
        }
    }
    fn real(&mut self) -> Option<Sp<Result<f64, String>>> {
        let span = self.exact(Token::Number)?;
        let mut s = &self.input[span.byte_range()];
        let mut replaced;
        if s.contains('`') {
            replaced = s.replace('`', "-");
            s = &replaced;
        }
        if s.contains('¯') {
            replaced = s.replace('¯', "-");
            s = &replaced;
        }
        if s.contains(',') {
            replaced = s.replace(',', "");
            s = &replaced;
        }
        Some(span.sp(s.parse::<f64>().map_err(|e| e.to_string())))
    }
    fn prim(&mut self) -> Option<Sp<Primitive>> {
        for prim in Primitive::all() {
            let op_span = self
                .exact(prim.into())
                .or_else(|| prim.ascii().and_then(|simple| self.exact(simple.into())));
            if let Some(span) = op_span {
                return Some(span.sp(prim));
            }
        }
        None
    }
    fn func_or_array(&mut self) -> Option<Sp<Word>> {
        let reset = self.index;
        let down_span = self.exact(DownArrow);
        let mut is_array = false;
        let mut boxes = false;
        let mut start = if let Some(start) = self.exact(OpenParen.into()) {
            start
        } else if let Some(start) = self.exact(OpenBracket.into()) {
            is_array = true;
            start
        } else if let Some(start) = self.exact(OpenCurly.into()) {
            is_array = true;
            boxes = true;
            start
        } else {
            self.index = reset;
            return None;
        };
        // Match initial function contents
        let first = self.func_contents();
        // Try to match pack branches
        let mut branches = Vec::new();
        while let Some(start) = self.exact(Bar.into()) {
            let (signature, lines, span) = self.func_contents();
            let span = if let Some(span) = span {
                start.merge(span)
            } else {
                start
            };
            branches.push(span.sp(Func {
                signature,
                lines,
                closed: true,
            }))
        }
        let end = self.expect_close(
            if is_array {
                if boxes { CloseCurly } else { CloseBracket }
            } else {
                CloseParen
            }
            .into(),
        );
        if let Some(last) = branches.last_mut() {
            last.span.merge_with(end.span.clone());
        }
        let (first_sig, first_lines, first_func_span) = first;
        let mut outer_span = start.clone().merge(end.span);
        Some(if branches.is_empty() {
            // Normal func
            let reset = self.index;
            let caret_span = self.exact(Placeholder(None));
            if let Some(ident) = self
                .ident()
                .filter(|ident| !is_array && ident.value.chars().all(|c| "!‼".contains(c)))
            {
                let func = outer_span.clone().sp(Func {
                    signature: first_sig,
                    lines: first_lines,
                    closed: end.value,
                });
                outer_span = outer_span.merge(ident.span.clone());
                outer_span.sp(Word::InlineMacro(InlineMacro {
                    func,
                    caret_span,
                    ident,
                }))
            } else {
                self.index = reset;
                outer_span.sp(if is_array {
                    Word::Array(Arr {
                        down_span,
                        lines: first_lines,
                        boxes,
                        closed: end.value,
                    })
                } else {
                    Word::Func(Func {
                        signature: first_sig,
                        lines: first_lines,
                        closed: end.value,
                    })
                })
            }
        } else {
            // Function pack
            let first_span = if first_lines.len() > 1 {
                if let Some(first_span) = first_lines.iter().find_map(Item::span) {
                    let last_span = first_lines.iter().rev().find_map(Item::span).unwrap();
                    start.start = first_span.start;
                    start.end = last_span.end;
                }
                start
            } else {
                first_func_span.unwrap_or(start)
            };
            let first = first_span.sp(Func {
                signature: first_sig,
                lines: first_lines,
                closed: true,
            });
            branches.insert(0, first);
            outer_span.sp(Word::Pack(FunctionPack {
                down_span,
                is_array: is_array.then_some(boxes),
                branches,
                closed: end.value,
            }))
        })
    }
    fn func_contents(&mut self) -> FunctionContents {
        let mut starts_with_newline = false;
        loop {
            if self.exact(Newline).is_some() {
                starts_with_newline = true;
                continue;
            }
            if self.spaces().is_some() {
                continue;
            }
            break;
        }
        let signature = self.signature(false);
        loop {
            if self.exact(Newline).is_some() {
                starts_with_newline = true;
                continue;
            }
            if self.spaces().is_some() {
                continue;
            }
            break;
        }
        let mut lines = Vec::new();
        if starts_with_newline {
            lines.push(Item::Words(Vec::new()));
        }
        lines.extend(self.items(ItemsKind::Function));
        if lines.is_empty() {
            lines.push(Item::Words(Vec::new()));
        }
        let start = signature
            .as_ref()
            .map(|sig| sig.span.clone())
            .or_else(|| lines.iter().find_map(Item::span));
        let end = lines
            .iter()
            .find_map(Item::span)
            .or_else(|| signature.as_ref().map(|sig| sig.span.clone()));
        let span = start.zip(end).map(|(start, end)| start.merge(end));
        (signature, lines, span)
    }
    fn spaces(&mut self) -> Option<Sp<Word>> {
        self.exact(Spaces).map(|span| span.sp(Word::Spaces))
    }
    fn module_open(&mut self) -> Option<Sp<bool>> {
        (self.exact(OpenModule).map(|span| span.sp(true)))
            .or_else(|| self.exact(OpenPrivateModule).map(|span| span.sp(false)))
            .or_else(|| self.module_delim_hyphens())
    }
    fn module_close(&mut self) -> Option<CodeSpan> {
        self.exact(CloseModule)
            .or_else(|| self.module_delim_hyphens().map(|sp| sp.span))
    }
    fn module_delim_hyphens(&mut self) -> Option<Sp<bool>> {
        let reset = self.index;
        let start = self.exact(Primitive::Sub.into())?;
        if self.exact(Primitive::Sub.into()).is_none() {
            self.index = reset;
            return None;
        }
        let Some(end) = self.exact(Primitive::Sub.into()) else {
            self.index = reset;
            return None;
        };
        let public = self.exact(Tilde.into()).is_none();
        let span = start.merge(end);
        Some(span.sp(public))
    }
    fn expect_close(&mut self, token: Token) -> Sp<bool> {
        if let Some(span) = self.exact(token.clone()) {
            span.sp(true)
        } else {
            self.errors
                .push(self.expected([Expectation::Term, Expectation::Token(token)]));
            self.prev_span().sp(false)
        }
    }
    fn comments(&mut self) -> Option<Comments> {
        let mut lines = Vec::new();
        let mut semantic = BTreeMap::new();
        loop {
            self.ignore_whitespace();
            if let Some(span) = self.exact(Comment) {
                let s = span.as_str(self.inputs, |s| {
                    s.strip_prefix("# ")
                        .or_else(|| s.strip_prefix("#"))
                        .unwrap_or(s)
                        .into()
                });
                lines.push(span.sp(s));
            } else if let Some(sem) = self.next_token_map(Token::as_semantic_comment) {
                semantic.insert(sem.value, sem.span);
            } else {
                break;
            }
        }
        if lines.is_empty() && semantic.is_empty() {
            return None;
        }
        self.ignore_whitespace();
        Some(Comments { lines, semantic })
    }
}

/// Split a list of items
///
/// This processes `;;` tokens, removing them and normalizating the items
pub fn split_items(items: Vec<Item>) -> Vec<Item> {
    items
        .into_iter()
        .flat_map(|item| match item {
            Item::Words(words) => split_words(words).into_iter().map(Item::Words).collect(),
            item => vec![item],
        })
        .collect()
}
/// Split a list of words
///
/// This processes `;;` tokens, removing them and normalizating the words
pub fn split_words(words: Vec<Sp<Word>>) -> Vec<Vec<Sp<Word>>> {
    if !words.iter().any(|w| matches!(w.value, Word::BreakLine)) {
        return vec![words];
    }
    let mut lines = vec![Vec::new()];
    for word in words {
        if matches!(word.value, Word::BreakLine) {
            lines.push(Vec::new());
        } else {
            lines.last_mut().unwrap().push(split_word(word));
        }
    }
    lines.reverse();
    lines
}

/// Flip and/or unsplit a list of items
///
/// This processes `;` tokens, removing them and normalizing the items
pub fn flip_unsplit_items(items: Vec<Item>) -> Vec<Item> {
    flip_unsplit_items_impl(items, false)
}
fn flip_unsplit_items_impl(items: Vec<Item>, in_array: bool) -> Vec<Item> {
    let mut unsplit = Vec::new();
    let mut curr_lines = Vec::new();
    for item in items {
        match item {
            Item::Words(words) => curr_lines.push(words),
            item => {
                unsplit.extend(
                    flip_unsplit_lines_impl(take(&mut curr_lines), in_array)
                        .into_iter()
                        .map(Item::Words),
                );
                unsplit.push(item)
            }
        }
    }
    unsplit.extend(
        flip_unsplit_lines_impl(curr_lines, in_array)
            .into_iter()
            .map(Item::Words),
    );
    unsplit
}

/// Flip and/or unsplit a list of lines
///
/// This processes `;` tokens, removing them and normalizing the words
pub fn flip_unsplit_lines(lines: Vec<Vec<Sp<Word>>>) -> Vec<Vec<Sp<Word>>> {
    flip_unsplit_lines_impl(lines, false)
}
fn flip_unsplit_lines_impl(lines: Vec<Vec<Sp<Word>>>, in_array: bool) -> Vec<Vec<Sp<Word>>> {
    // Unsplit sub-words
    let mut lines = lines
        .into_iter()
        .map(|line| line.into_iter().map(unsplit_word).collect::<Vec<_>>());
    let Some(first) = lines.next() else {
        return Vec::new();
    };
    let mut unsplit = trim_spaces(&first, true)
        .last()
        .filter(|w| matches!(w.value, Word::FlipLine))
        .cloned();

    let trim_spaces = |words: &mut Vec<Sp<Word>>| {
        while (words.first()).is_some_and(|w| matches!(w.value, Word::Spaces)) {
            words.remove(0);
        }
        while words
            .last()
            .is_some_and(|w| matches!(w.value, Word::Spaces))
        {
            words.pop();
        }
    };

    let flip_line = |mut line: Vec<Sp<Word>>| {
        let eol = line.pop_if(|w| w.value.is_end_of_line());
        if line.iter().any(|w| matches!(w.value, Word::FlipLine)) {
            let mut parts = Vec::new();
            while let Some(i) = (line.iter()).rposition(|w| matches!(w.value, Word::FlipLine)) {
                let mut part = line.split_off(i + 1);
                trim_spaces(&mut part);
                parts.push(part);
                let span = line.pop().unwrap().span;
                parts.push(vec![span.sp(Word::Spaces)]);
            }
            trim_spaces(&mut line);
            parts.push(line);
            line = parts.into_iter().flatten().collect();
        }
        line.extend(eol);
        line
    };

    let mut new_lines = vec![flip_line(first)];

    for mut line in lines {
        trim_spaces(&mut line);
        // Check for leading and trailing unbreak lines
        let unsplit_front = (line.first())
            .filter(|w| matches!(w.value, Word::FlipLine))
            .cloned();
        if unsplit_front.is_some() {
            line.remove(0);
        }
        let unsplit_back = (line.last())
            .filter(|w| matches!(w.value, Word::FlipLine))
            .cloned();
        if unsplit_back.is_some() {
            line.pop();
        }
        line = flip_line(line);
        // Reorder lines
        if let Some(word) = unsplit.as_ref().or(unsplit_front.as_ref()) {
            let prev = new_lines.last_mut().unwrap();
            if in_array {
                prev.push(word.span.clone().sp(Word::Spaces));
                prev.extend(line);
            } else {
                let taken_prev = replace(prev, line);
                prev.push(word.span.clone().sp(Word::Spaces));
                prev.extend(taken_prev);
            }
        } else {
            new_lines.push(line);
        }
        unsplit = unsplit_back;
    }
    new_lines
}

fn unsplit_word(word: Sp<Word>) -> Sp<Word> {
    word.map(|word| match word {
        Word::Func(mut func) => {
            func.lines = flip_unsplit_items(func.lines);
            Word::Func(func)
        }
        Word::Array(mut arr) => {
            arr.lines = flip_unsplit_items_impl(arr.lines, true);
            Word::Array(arr)
        }
        Word::Pack(mut pack) => {
            pack.branches = pack
                .branches
                .into_iter()
                .map(|mut br| {
                    br.value.lines = flip_unsplit_items(br.value.lines);
                    br
                })
                .collect();
            Word::Pack(pack)
        }
        Word::Modified(mut m) => {
            m.operands = m.operands.into_iter().map(unsplit_word).collect();
            Word::Modified(m)
        }
        Word::Subscripted(mut sub) => {
            sub.word = unsplit_word(sub.word);
            Word::Subscripted(sub)
        }
        word => word,
    })
}

fn split_word(word: Sp<Word>) -> Sp<Word> {
    word.map(|word| match word {
        Word::Func(mut func) => {
            func.lines = split_items(func.lines);
            Word::Func(func)
        }
        Word::Array(mut arr) => {
            arr.lines = split_items(arr.lines);
            Word::Array(arr)
        }
        Word::Pack(mut pack) => {
            pack.branches = pack
                .branches
                .into_iter()
                .map(|mut br| {
                    br.value.lines = split_items(br.value.lines);
                    br
                })
                .collect();
            Word::Pack(pack)
        }
        Word::Modified(mut m) => {
            m.operands = m.operands.into_iter().map(split_word).collect();
            Word::Modified(m)
        }
        Word::Subscripted(mut sub) => {
            sub.word = split_word(sub.word);
            Word::Subscripted(sub)
        }
        word => word,
    })
}

/// Get the number of modifier arguments implied by an identifier
pub fn ident_modifier_args(ident: &str) -> usize {
    let mut count: usize = 0;
    let mut prefix = ident;
    while let Some((pre, this_count)) = prefix
        .strip_suffix('!')
        .zip(Some(1))
        .or_else(|| prefix.strip_suffix('‼').zip(Some(2)))
    {
        prefix = pre;
        count = count.saturating_add(this_count);
    }
    count
}

/// Get the maximum placeholder index in a list of words,
/// as well as the span of a placeholder shorthand
pub fn max_placeholder(words: &[Sp<Word>]) -> Option<(usize, Option<CodeSpan>)> {
    let mut max: Option<usize> = None;
    let mut shorthand_span = None;
    let mut set = |is: Option<(usize, Option<CodeSpan>)>| {
        if let Some((i, s)) = is {
            let max = max.get_or_insert(0);
            *max = (*max).max(i);
            if shorthand_span.is_none() && s.is_some() {
                shorthand_span = s;
            }
        }
    };
    for word in words {
        match &word.value {
            Word::Placeholder(i) => set(Some((
                i.unwrap_or(0),
                i.is_none().then(|| word.span.clone()),
            ))),
            Word::Strand(items) => set(max_placeholder(items)),
            Word::Array(arr) => {
                for line in arr.word_lines() {
                    set(max_placeholder(line));
                }
            }
            Word::Func(func) => {
                for line in func.word_lines() {
                    set(max_placeholder(line));
                }
            }
            Word::Modified(m) => set(max_placeholder(&m.operands)),
            Word::Pack(pack) => {
                for branch in &pack.branches {
                    for line in branch.value.word_lines() {
                        set(max_placeholder(line));
                    }
                }
            }
            Word::Subscripted(s) => set(max_placeholder(slice::from_ref(&s.word))),
            _ => {}
        }
    }
    max.map(|i| (i, shorthand_span))
}

/// Trim space words
pub fn trim_spaces(words: &[Sp<Word>], trim_end: bool) -> &[Sp<Word>] {
    let mut start = 0;
    for word in words {
        if let Word::Spaces = word.value {
            start += 1;
        } else {
            break;
        }
    }
    let mut end = words.len();
    if trim_end {
        for word in words.iter().rev() {
            if let Word::Spaces = word.value {
                end -= 1;
            } else {
                break;
            }
        }
    }
    if start >= end {
        return &[];
    }
    &words[start..end]
}

fn single_word_and<'a, I>(words: I, mut f: impl FnMut(&Sp<Word>))
where
    I: IntoIterator<Item = &'a Sp<Word>>,
    I::IntoIter: Clone,
{
    let mut iter = words.into_iter();
    if iter.clone().filter(|w| w.value.is_code()).count() == 1 {
        f(iter.find(|w| w.value.is_code()).unwrap())
    }
}

fn arr_is_normal_di(arr: &Arr) -> bool {
    if arr.boxes {
        return false;
    }
    let mut is_di = false;
    single_word_and(
        (arr.lines.iter()).flat_map(|item| item.words_or([].as_slice(), |words| words)),
        |m| {
            if let Word::Modified(m) = &m.value {
                let Modified {
                    modifier, operands, ..
                } = &**m;
                if let Modifier::Primitive(Primitive::Dip) = modifier.value {
                    single_word_and(operands, |f| {
                        is_di = matches!(f.value, Word::Primitive(Primitive::Identity));
                    })
                }
            }
        },
    );
    is_di
}
