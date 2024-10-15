//! The Uiua parser

use std::{error::Error, f64::consts::PI, fmt, mem::replace};

use ecow::EcoString;

use crate::{
    ast::*,
    function::{FunctionId, Signature},
    lex::{AsciiToken::*, Token::*, *},
    Diagnostic, DiagnosticKind, Ident, Inputs, Primitive,
};

/// An error that occurred while parsing
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<EcoString>),
    InvalidNumber(String),
    Unexpected(Token),
    InvalidArgCount(String),
    InvalidOutCount(String),
    AmpersandBindingName,
    ModifierImportName,
    SplitInModifier,
    FlipInModifier,
    LineTooLong(usize),
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
            Expectation::Token(tok) => write!(f, "{:?}", tok),
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
            ParseError::InvalidNumber(s) => write!(f, "Invalid number `{s}`"),
            ParseError::Unexpected(token) => write!(f, "Unexpected token {token}"),
            ParseError::InvalidArgCount(n) => write!(f, "Invalid argument count `{n}`"),
            ParseError::InvalidOutCount(n) => write!(f, "Invalid output count `{n}`"),
            ParseError::AmpersandBindingName => write!(f, "Binding names may not contain `&`"),
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
            ParseError::LineTooLong(width) => write!(
                f,
                "Split line into multiple lines (heuristic: {}/{}) 😏",
                width, ERROR_MAX_WIDTH
            ),
            ParseError::RecursionLimit => write!(f, "Parsing recursion limit reached"),
        }
    }
}

const STYLE_MAX_WIDTH: usize = 40;
const ADVICE_MAX_WIDTH: usize = 53;
const WARNING_MAX_WIDTH: usize = 67;
const ERROR_MAX_WIDTH: usize = 80;

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
        let mut errors: Vec<_> = lex_errors
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
                while let Some(tok) = toks.next() {
                    heuristic += match &tok.value {
                        Spaces | Comment => 0,
                        Simple(CloseBracket | CloseCurly | CloseParen) => 0,
                        Simple(Underscore) => 0,
                        MultilineString(_) => {
                            while let Some(MultilineString(_)) = toks.peek().map(|t| &t.value) {
                                toks.next();
                            }
                            1
                        }
                        _ => {
                            first = first.or(Some(&tok.span));
                            1
                        }
                    };
                }
                if heuristic <= STYLE_MAX_WIDTH {
                    continue;
                }
                let first = first.unwrap().clone();
                let last = line.last().unwrap().span.clone();
                let span = first.merge(last);
                let (kind, face) = if heuristic > ERROR_MAX_WIDTH {
                    errors.push(span.sp(ParseError::LineTooLong(heuristic)));
                    continue;
                } else if heuristic > WARNING_MAX_WIDTH {
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
        let mut parser = Parser {
            inputs,
            input: input.into(),
            tokens,
            index: 0,
            errors,
            diagnostics,
            next_output_comment: 0,
            depth: 0,
        };
        let items = parser.items(false);
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
    depth: usize,
}

type FunctionContents = (Option<Sp<Signature>>, Vec<Vec<Sp<Word>>>, Option<CodeSpan>);

impl<'i> Parser<'i> {
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
    fn try_exact(&mut self, token: Token) -> Option<CodeSpan> {
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
    fn items(&mut self, in_scope: bool) -> Vec<Item> {
        let mut items = Vec::new();
        while self.try_exact(Newline).is_some() {
            self.try_spaces();
        }
        loop {
            match self.try_item(in_scope) {
                Some(item) => items.push(item),
                None => {
                    if self.try_exact(Newline).is_none() {
                        break;
                    }
                    self.try_spaces();
                    let mut extra_newlines = false;
                    while self.try_exact(Newline).is_some() {
                        extra_newlines = true;
                        self.try_spaces();
                    }
                    if extra_newlines {
                        items.push(Item::Words(vec![Vec::new()]));
                    }
                }
            }
        }
        items
    }
    fn try_item(&mut self, in_scope: bool) -> Option<Item> {
        self.try_spaces();
        Some(if let Some(binding) = self.try_binding() {
            Item::Binding(binding)
        } else if let Some(import) = self.try_import() {
            Item::Import(import)
        } else if let Some(module) = self.try_module(in_scope) {
            Item::Module(module)
        } else if let Some(data) = self.try_data_def() {
            Item::Data(data)
        } else {
            let lines = self.multiline_words(true, false);
            if lines.is_empty() {
                return None;
            } else {
                Item::Words(lines)
            }
        })
    }
    fn try_module(&mut self, in_scope: bool) -> Option<Sp<ScopedModule>> {
        let backup = self.index;
        let open_span = self.try_module_open()?;
        self.try_spaces();
        // Name
        let name = self.try_ident();
        if in_scope && name.is_none() {
            self.index = backup;
            return None;
        }
        let kind = match name {
            Some(name) if name.value == "test" => ModuleKind::Test,
            Some(name) => ModuleKind::Named(name),
            None => ModuleKind::Test,
        };
        // Imports
        while self.try_exact(Spaces).is_some() {}
        let imports = if let Some(tilde_span) = self.try_exact(Tilde.into()) {
            let mut items = Vec::new();
            loop {
                if let Some(ident) = self.try_ident() {
                    items.push(ident);
                } else if self.try_spaces().is_some() {
                    continue;
                } else {
                    break;
                }
            }
            if items.is_empty() {
                None
            } else {
                Some(ImportLine { tilde_span, items })
            }
        } else {
            None
        };
        // Items
        let items = self.items(true);
        let close_span = self.try_module_close();
        let span = if let Some(end) = close_span.clone() {
            open_span.clone().merge(end)
        } else {
            self.errors.push(self.expected([Expectation::CloseModule]));
            open_span.clone()
        };
        let module = ScopedModule {
            open_span,
            kind,
            items,
            imports,
            close_span,
        };
        Some(span.sp(module))
    }
    fn comment(&mut self) -> Option<Sp<String>> {
        let span = self.try_exact(Token::Comment)?;
        let s = &self.input[span.byte_range()];
        let s = s.strip_prefix('#').unwrap_or(s).into();
        Some(span.sp(s))
    }
    fn output_comment(&mut self) -> Option<Sp<Word>> {
        let n = self.next_token_map(Token::as_output_comment)?;
        let i = self.next_output_comment;
        self.next_output_comment += 1;
        Some(n.span.sp(Word::OutputComment { i, n: n.value }))
    }
    fn try_binding_init(&mut self) -> Option<(Sp<Ident>, CodeSpan, bool, bool)> {
        let start = self.index;
        let name = self.try_ident()?;
        // Left arrow
        let arrow_span = self.try_spaces().map(|w| w.span);
        let (glyph_span, public) = if let Some(span) = self
            .try_exact(Equal.into())
            .or_else(|| self.try_exact(LeftArrow))
        {
            (span, true)
        } else if let Some(span) = self
            .try_exact(EqualTilde.into())
            .or_else(|| self.try_exact(LeftArrowTilde))
            .or_else(|| self.try_exact(LeftStrokeArrow))
        {
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
        let array_macro = if let Some(span) = self.try_exact(Caret.into()) {
            arrow_span = arrow_span.merge(span);
            true
        } else {
            false
        };
        if let Some(span) = self.try_spaces().map(|w| w.span) {
            arrow_span = arrow_span.merge(span);
        }
        Some((name, arrow_span, public, array_macro))
    }
    fn try_import_init(&mut self) -> Option<(Option<Sp<Ident>>, CodeSpan, Sp<String>)> {
        let start = self.index;
        // Name
        let name = self.try_ident();
        self.try_spaces();
        // Tilde
        let Some(tilde_span) = self.try_exact(Tilde.into()) else {
            self.index = start;
            return None;
        };
        self.try_spaces();
        // Path
        let Some(path) = self.next_token_map(Token::as_string) else {
            self.index = start;
            return None;
        };
        let path = path.map(Into::into);
        self.try_spaces();
        Some((name, tilde_span, path))
    }
    fn try_binding(&mut self) -> Option<Binding> {
        let (name, arrow_span, public, array_macro) = self.try_binding_init()?;
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
        let signature = self.try_signature(true);
        // Words
        let words = self.try_words().unwrap_or_default();
        self.validate_binding_name(&name);
        Some(Binding {
            name,
            arrow_span,
            public,
            code_macro: array_macro,
            words,
            signature,
        })
    }
    fn ignore_whitespace(&mut self) -> bool {
        let mut newline = false;
        self.try_spaces();
        while self.try_exact(Spaces).is_some() {
            newline = true;
            self.try_spaces();
        }
        self.try_spaces();
        newline
    }
    fn try_data_def(&mut self) -> Option<DataDef> {
        let reset = self.index;
        let mut variant = false;
        let init_span = self.try_exact(Tilde.into()).or_else(|| {
            variant = true;
            self.try_exact(Bar.into())
        })?;
        self.try_spaces();
        let name = self.try_ident();
        self.try_spaces();
        let mut boxed = false;
        let open_span = if let Some(span) = self.try_exact(OpenBracket.into()) {
            Some(span)
        } else if let Some(span) = self.try_exact(OpenCurly.into()) {
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
            while self.try_exact(Newline).is_some() {
                self.try_spaces();
            }
            self.try_spaces();
            let mut trailing_newline = false;
            loop {
                let mut comment = None;
                if let Some(comment_span) = self.try_exact(Comment) {
                    let s = comment_span.as_str(self.inputs, |s| s.trim().into());
                    comment = Some(comment_span.sp(s));
                    self.ignore_whitespace();
                }
                let Some(name) = self.try_ident() else {
                    break;
                };

                trailing_newline = false;
                self.try_spaces();
                let mut default = None;
                let start_arrow_span = self.try_spaces().map(|w| w.span);
                if let Some(mut arrow_span) = self
                    .try_exact(Equal.into())
                    .or_else(|| self.try_exact(LeftArrow))
                {
                    arrow_span = if let Some(start) = start_arrow_span {
                        start.merge(arrow_span)
                    } else {
                        arrow_span
                    };
                    if let Some(span) = self.try_spaces().map(|w| w.span) {
                        arrow_span = arrow_span.merge(span);
                    }
                    let words = self.try_words().unwrap_or_else(|| {
                        self.errors.push(self.expected([Expectation::Term]));
                        Vec::new()
                    });
                    default = Some(FieldDefault { arrow_span, words })
                };
                trailing_newline |= self.ignore_whitespace();
                let mut bar_span = self.try_exact(Bar.into());
                if self.try_exact(Newline).is_some()
                    || self.try_exact(DoubleSemicolon.into()).is_some()
                {
                    bar_span = None;
                }
                if bar_span.is_some() {
                    trailing_newline = false;
                }
                fields.push(DataField {
                    comment,
                    name,
                    default,
                    bar_span,
                });
                trailing_newline |= self.ignore_whitespace();
            }
            let close = self.expect_close(if boxed { CloseCurly } else { CloseBracket }.into());
            let close_span = close.value.then_some(close.span);
            self.try_spaces();
            Some(DataFields {
                boxed,
                open_span,
                fields,
                trailing_newline,
                close_span,
            })
        } else {
            None
        };

        let func = self.try_words();
        Some(DataDef {
            init_span,
            variant,
            name,
            fields,
            func,
        })
    }
    fn validate_binding_name(&mut self, name: &Sp<Ident>) {
        if name.value.contains('&') {
            self.errors
                .push(name.span.clone().sp(ParseError::AmpersandBindingName));
        }
        if name.value.trim_end_matches(['!', '‼']).chars().count() >= 2
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
                    to avoid collisions with future builtin functions.\n\
                    Try `{}` instead of `{}`",
                    captialized, name.value
                ),
                name.span.clone(),
                DiagnosticKind::Advice,
                self.inputs.clone(),
            ));
        }
    }
    fn try_import(&mut self) -> Option<Import> {
        let (name, tilde_span, path) = self.try_import_init()?;
        // Items
        let mut lines: Vec<Option<ImportLine>> = Vec::new();
        let mut line: Option<ImportLine> = None;
        self.try_exact(Newline);
        let mut last_tilde_index = self.index;
        while let Some(token) = self.tokens.get(self.index).cloned() {
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
                        items: Vec::new(),
                    })
                }
                Simple(Tilde) => self
                    .errors
                    .push(span.sp(ParseError::Unexpected(Simple(Tilde)))),
                Newline => {
                    lines.push(line.take());
                }
                Spaces => {}
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
            path,
            lines,
        })
    }
    fn try_ident(&mut self) -> Option<Sp<Ident>> {
        self.next_token_map(Token::as_ident)
    }
    fn try_ref(&mut self) -> Option<Sp<Word>> {
        let mut checkpoint = self.index;
        let mut name = self.try_ident()?;
        let start_span = name.span.clone();
        let mut path = Vec::new();
        while let Some(tilde_span) = self.try_exact(Tilde.into()) {
            let comp = RefComponent {
                module: name,
                tilde_span,
            };
            let Some(next) = self.try_ident() else {
                self.try_spaces();
                if self
                    .tokens
                    .get(self.index)
                    .map_or(true, |t| !matches!(t.value, Token::Str(_)))
                {
                    let span = start_span.merge(comp.tilde_span.clone());
                    path.push(comp);
                    return Some(span.sp(Word::IncompleteRef {
                        path,
                        in_macro_arg: false,
                    }));
                }
                self.index = checkpoint;
                return None;
            };
            checkpoint = self.index;
            path.push(comp);
            name = next;
        }
        let span = start_span.merge(name.span.clone());
        Some(span.sp(Word::Ref(Ref {
            name,
            path,
            in_macro_arg: false,
        })))
    }
    fn try_signature(&mut self, error_on_invalid: bool) -> Option<Sp<Signature>> {
        let reset = self.index;
        let start = self.try_exact(Bar.into())?;
        let inner = self.sig_inner();
        if inner.is_none() {
            if error_on_invalid {
                self.errors.push(self.expected([Expectation::ArgsOutputs]));
            }
            self.index = reset;
        }
        let (args, outs) = inner?;
        let mut end = self.prev_span();
        if let Some(sp) = self.try_spaces() {
            end = sp.span;
        }
        let span = start.merge(end);
        Some(span.sp(Signature::new(args, outs)))
    }
    fn sig_inner(&mut self) -> Option<(usize, usize)> {
        let sn = self.try_num()?;
        Some(if let Some((a, o)) = sn.value.0.split_once('.') {
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
            let a = match sn.value.0.parse() {
                Ok(a) => a,
                Err(_) => {
                    self.errors
                        .push(self.prev_span().sp(ParseError::InvalidArgCount(sn.value.0)));
                    1
                }
            };
            (a, 1)
        })
    }
    fn try_words(&mut self) -> Option<Vec<Sp<Word>>> {
        let mut words: Vec<Sp<Word>> = Vec::new();
        while let Some(word) = self.try_word() {
            if let Some(prev) = words.iter().filter(|w| w.value.is_code()).nth_back(0) {
                // Diagnostics
                use Primitive::*;
                let span = || prev.span.clone().merge(word.span.clone());
                match (&prev.value, &word.value) {
                    (Word::Primitive(a), Word::Primitive(b)) => {
                        match (a, b) {
                            (Over, Flip) => self.diagnostics.push(Diagnostic::new(
                                format!("Prefer `{On}{Flip}` over `{Over}{Flip}` for clarity"),
                                span(),
                                DiagnosticKind::Style,
                                self.inputs.clone(),
                            )),
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
        if words.is_empty() {
            None
        } else {
            Some(words)
        }
    }
    fn multiline_words(
        &mut self,
        check_for_bindings: bool,
        extra_newline: bool,
    ) -> Vec<Vec<Sp<Word>>> {
        let mut lines = Vec::new();
        while self.try_spaces().is_some() {}
        let mut newlines: usize = 0;
        loop {
            let curr = self.index;
            if check_for_bindings
                && (self.try_binding_init().is_some()
                    || self.try_import_init().is_some()
                    || self.try_module_delim_hyphens().is_some())
            {
                self.index = curr;
                break;
            }
            if let Some(words) = self.try_words() {
                newlines = 0;
                lines.push(words);
                while self.try_exact(Newline).is_some() {
                    newlines += 1;
                    self.try_spaces();
                }
                if newlines > 1 {
                    lines.push(Vec::new());
                }
            } else {
                break;
            }
        }
        if extra_newline && newlines > 0 {
            lines.push(Vec::new());
        }
        lines
    }
    fn try_word(&mut self) -> Option<Sp<Word>> {
        self.comment()
            .map(|c| c.map(Word::Comment))
            .or_else(|| self.output_comment())
            .or_else(|| self.try_strand())
    }
    fn try_strand(&mut self) -> Option<Sp<Word>> {
        let word = self.try_modified()?;
        if let Word::Spaces = word.value {
            return Some(word);
        }
        // Collect items
        let mut items = Vec::new();
        while self.try_exact(Underscore.into()).is_some() {
            let item = match self.try_modified() {
                Some(mut item) => {
                    if let Word::Spaces = item.value {
                        if items.is_empty() {
                            break;
                        }
                        self.errors.push(self.expected([Expectation::Term]));
                        item = match self.try_modified() {
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
    fn try_modified(&mut self) -> Option<Sp<Word>> {
        if self.depth > 50 {
            self.errors
                .push(self.prev_span().sp(ParseError::RecursionLimit));
            return None;
        }
        let (modifier, mod_span) = if let Some(prim) = Primitive::all()
            .filter(|prim| prim.is_modifier())
            .find_map(|prim| {
                self.try_exact(prim.into())
                    .or_else(|| {
                        prim.ascii()
                            .and_then(|simple| self.try_exact(simple.into()))
                    })
                    .map(|span| span.sp(prim))
            }) {
            (Modifier::Primitive(prim.value), prim.span)
        } else {
            let term = self.try_term()?;
            if let Word::Ref(item) = term.value {
                if item.modifier_args() == 0 {
                    return Some(term.span.sp(Word::Ref(item)));
                }
                (Modifier::Ref(item), term.span)
            } else {
                return Some(term);
            }
        };
        self.try_spaces();
        let mut subscript = None;
        if let Some(n) = self.next_token_map(Token::as_subscript) {
            subscript = Some(n);
            self.try_spaces();
        }
        let mut args = Vec::new();
        self.depth += 1;
        for i in 0..modifier.args() {
            loop {
                args.extend(self.try_spaces());
                if let Some(span) = self.try_exact(DoubleSemicolon.into()) {
                    self.errors.push(span.sp(ParseError::SplitInModifier));
                    continue;
                }
                if let Some(span) = self.try_exact(Semicolon.into()) {
                    self.errors.push(span.sp(ParseError::FlipInModifier));
                    continue;
                }
                break;
            }
            if let Some(arg) = self.try_func().or_else(|| self.try_strand()) {
                // Parse pack syntax
                if let Word::Pack(pack) = &arg.value {
                    if i == 0 && !pack.angled {
                        args.push(arg);
                        break;
                    }
                }
                args.push(arg);
            } else {
                break;
            }
        }
        self.depth -= 1;

        let span = if let Some(last) = args.last() {
            mod_span.clone().merge(last.span.clone())
        } else {
            mod_span.clone()
        };

        match &modifier {
            Modifier::Primitive(Primitive::Un) => {
                single_word_and(&args, |inverted| {
                    if let Word::Array(arr) = &inverted.value {
                        if arr_is_normal_di(arr) {
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
        })));

        if let Some(n) = subscript {
            let span = word.span.clone().merge(n.span.clone());
            word = span.sp(Word::Subscript(Box::new(crate::ast::Subscript { n, word })));
        }

        Some(word)
    }
    fn try_term(&mut self) -> Option<Sp<Word>> {
        let mut word = if let Some(prim) = self.try_prim() {
            prim.map(Word::Primitive)
        } else if let Some(refer) = self.try_ref() {
            refer
        } else if let Some(sn) = self.try_num() {
            sn.map(|(s, n)| Word::Number(s, n))
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Word::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Word::String)
        } else if let Some(op) = self.next_token_map(Token::as_placeholder) {
            op.map(Word::Placeholder)
        } else if let Some(label) = self.next_token_map(Token::as_label) {
            label.map(Into::into).map(Word::Label)
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
        } else if let Some(start) = self.try_exact(OpenBracket.into()) {
            while self.try_exact(Newline).is_some() || self.try_exact(Spaces).is_some() {}
            let signature = self.try_signature(true);
            while self.try_exact(Newline).is_some() {}
            let items = self.multiline_words(false, true);
            let end = self.expect_close(CloseBracket.into());
            let span = start.merge(end.span);
            let arr = Arr {
                signature,
                lines: items,
                boxes: false,
                closed: end.value,
            };
            if arr_is_normal_di(&arr) {
                self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Prefer `{}` ({}) over `[{}{}]`",
                        Primitive::Couple,
                        Primitive::Couple.name(),
                        Primitive::Dip,
                        Primitive::Identity
                    ),
                    span.clone(),
                    DiagnosticKind::Style,
                    self.inputs.clone(),
                ));
            }
            span.sp(Word::Array(arr))
        } else if let Some(start) = self.try_exact(OpenCurly.into()) {
            while self.try_exact(Newline).is_some() || self.try_exact(Spaces).is_some() {}
            let signature = self.try_signature(true);
            while self.try_exact(Newline).is_some() {}
            let items = self.multiline_words(false, true);
            let end = self.expect_close(CloseCurly.into());
            let span = start.merge(end.span);
            span.sp(Word::Array(Arr {
                signature,
                lines: items,
                boxes: true,
                closed: end.value,
            }))
        } else if let Some(spaces) = self.try_spaces() {
            spaces
        } else if let Some(word) = self.try_func() {
            word
        } else if let Some(span) = self.try_exact(Semicolon.into()) {
            span.sp(Word::FlipLine)
        } else if let Some(span) = self.try_exact(DoubleSemicolon.into()) {
            span.sp(Word::BreakLine)
        } else if let Some(sc) = self.next_token_map(Token::as_semantic_comment) {
            sc.map(Word::SemanticComment)
        } else {
            return None;
        };
        loop {
            let reset = self.index;
            self.try_spaces();
            if let Some(n) = self.next_token_map(Token::as_subscript) {
                let span = word.span.clone().merge(n.span.clone());
                word = span.sp(Word::Subscript(Box::new(crate::ast::Subscript { n, word })));
            } else {
                self.index = reset;
                break;
            }
        }
        Some(word)
    }
    fn try_num(&mut self) -> Option<Sp<(String, f64)>> {
        let span = self.try_exact(Token::Number)?;
        let s = self.input[span.byte_range()].to_string();
        fn parse(s: &str) -> Option<f64> {
            let mut s = s.replace(['`', '¯'], "-");
            // Replace pi multiples
            for (name, glyph, mul) in [("eta", 'η', 0.5), ("pi", 'π', 1.0), ("tau", 'τ', 2.0)] {
                if s.contains(glyph) {
                    s = s.replace(glyph, &(PI * mul).to_string());
                } else if s.contains(name) {
                    s = s.replace(name, &(PI * mul).to_string());
                }
            }
            // Replace infinity
            if s.contains('∞') {
                s = s.replace('∞', "inf");
            } else {
                for i in (3..="infinity".len()).rev() {
                    if s.contains(&"infinity"[..i]) {
                        s = s.replace(&"infinity"[..i], "inf");
                        break;
                    }
                }
            }
            s.parse().ok()
        }
        let n: f64 = match parse(&s) {
            Some(n) => n,
            None => {
                if let Some((n, d)) = s.split_once('/').and_then(|(n, d)| parse(n).zip(parse(d))) {
                    n / d
                } else {
                    self.errors
                        .push(self.prev_span().sp(ParseError::InvalidNumber(s.clone())));
                    0.0
                }
            }
        };
        Some(span.sp((s, n)))
    }
    fn try_prim(&mut self) -> Option<Sp<Primitive>> {
        for prim in Primitive::all() {
            let op_span = self.try_exact(prim.into()).or_else(|| {
                prim.ascii()
                    .and_then(|simple| self.try_exact(simple.into()))
            });
            if let Some(span) = op_span {
                return Some(span.sp(prim));
            }
        }
        None
    }
    fn try_func(&mut self) -> Option<Sp<Word>> {
        Some(if let Some(mut start) = self.try_exact(OpenParen.into()) {
            // Match initial function contents
            let first = self.func_contents();
            // Try to match pack branches
            let mut branches = Vec::new();
            while let Some(start) = self.try_exact(Bar.into()) {
                let (signature, lines, span) = self.func_contents();
                let span = if let Some(span) = span {
                    start.merge(span)
                } else {
                    start
                };
                let id = FunctionId::Anonymous(span.clone());
                branches.push(span.sp(Func {
                    id,
                    signature,
                    lines,
                    closed: true,
                }))
            }
            let end = self.expect_close(CloseParen.into());
            if let Some(last) = branches.last_mut() {
                last.span.merge_with(end.span.clone());
            }
            let (first_sig, first_lines, first_func_span) = first;
            let outer_span = start.clone().merge(end.span);
            if branches.is_empty() {
                let id = FunctionId::Anonymous(outer_span.clone());
                outer_span.sp(Word::Func(Func {
                    id,
                    signature: first_sig,
                    lines: first_lines,
                    closed: end.value,
                }))
            } else {
                let first_span = if first_lines.len() > 1 {
                    let code_words = first_lines
                        .iter()
                        .flatten()
                        .filter(|word| word.value.is_code());
                    if let Some(first_word) = code_words.clone().next() {
                        let last_word = code_words.last().unwrap();
                        start.start = first_word.span.start;
                        start.end = last_word.span.end;
                    }
                    start
                } else {
                    first_func_span.unwrap_or(start)
                };
                let first_id = FunctionId::Anonymous(first_span.clone());
                let first = first_span.sp(Func {
                    id: first_id,
                    signature: first_sig,
                    lines: first_lines,
                    closed: true,
                });
                branches.insert(0, first);
                outer_span.sp(Word::Pack(FunctionPack {
                    branches,
                    closed: end.value,
                    angled: false,
                }))
            }
        } else if let Some(start) = self.try_exact(OpenAngle) {
            let first = self.func_contents();
            let mut branches = Vec::new();
            while let Some(start) = self.try_exact(Bar.into()) {
                let (signature, lines, span) = self.func_contents();
                let span = if let Some(span) = span {
                    start.merge(span)
                } else {
                    start
                };
                let id = FunctionId::Anonymous(span.clone());
                branches.push(span.sp(Func {
                    id,
                    signature,
                    lines,
                    closed: true,
                }))
            }
            let end = self.expect_close(CloseAngle);
            if let Some(last) = branches.last_mut() {
                last.span.merge_with(end.span.clone());
            }
            let (first_sig, first_lines, first_span) = first;
            let outer_span = start.clone().merge(end.span);
            let first_span = if let Some(span) = first_span {
                start.merge(span)
            } else {
                start
            };
            let first_id = FunctionId::Anonymous(first_span.clone());
            let first = first_span.sp(Func {
                id: first_id,
                signature: first_sig,
                lines: first_lines,
                closed: true,
            });
            branches.insert(0, first);
            outer_span.sp(Word::Pack(FunctionPack {
                branches,
                closed: end.value,
                angled: true,
            }))
        } else {
            return None;
        })
    }
    fn func_contents(&mut self) -> FunctionContents {
        let mut starts_with_newline = false;
        loop {
            if self.try_exact(Newline).is_some() {
                starts_with_newline = true;
                continue;
            }
            if self.try_spaces().is_some() {
                continue;
            }
            break;
        }
        let signature = self.try_signature(false);
        loop {
            if self.try_exact(Newline).is_some() {
                starts_with_newline = true;
                continue;
            }
            if self.try_spaces().is_some() {
                continue;
            }
            break;
        }
        let mut lines = Vec::new();
        if starts_with_newline {
            lines.push(Vec::new());
        }
        lines.extend(self.multiline_words(false, true));
        if lines.is_empty() {
            lines.push(Vec::new());
        }
        let start = signature
            .as_ref()
            .map(|sig| sig.span.clone())
            .or_else(|| lines.iter().flatten().next().map(|word| word.span.clone()));
        let end = lines
            .iter()
            .flatten()
            .last()
            .map(|word| word.span.clone())
            .or_else(|| signature.as_ref().map(|sig| sig.span.clone()));
        let span = start.zip(end).map(|(start, end)| start.merge(end));
        (signature, lines, span)
    }
    fn try_spaces(&mut self) -> Option<Sp<Word>> {
        self.try_exact(Spaces).map(|span| span.sp(Word::Spaces))
    }
    fn try_module_open(&mut self) -> Option<CodeSpan> {
        self.try_exact(OpenModule)
            .or_else(|| self.try_module_delim_hyphens())
    }
    fn try_module_close(&mut self) -> Option<CodeSpan> {
        self.try_exact(CloseModule)
            .or_else(|| self.try_module_delim_hyphens())
    }
    fn try_module_delim_hyphens(&mut self) -> Option<CodeSpan> {
        let reset = self.index;
        let start = self.try_exact(Primitive::Sub.into())?;
        if self.try_exact(Primitive::Sub.into()).is_none() {
            self.index = reset;
            return None;
        }
        let Some(end) = self.try_exact(Primitive::Sub.into()) else {
            self.index = reset;
            return None;
        };
        Some(start.merge(end))
    }
    fn expect_close(&mut self, token: Token) -> Sp<bool> {
        if let Some(span) = self.try_exact(token.clone()) {
            span.sp(true)
        } else {
            self.errors
                .push(self.expected([Expectation::Term, Expectation::Token(token)]));
            self.prev_span().sp(false)
        }
    }
}

pub(crate) fn split_words(words: Vec<Sp<Word>>) -> Vec<Vec<Sp<Word>>> {
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

/// Flip and/or unsplit a list of lines
pub(crate) fn flip_unsplit_lines(lines: Vec<Vec<Sp<Word>>>) -> Vec<Vec<Sp<Word>>> {
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
        .is_some_and(|w| matches!(w.value, Word::FlipLine));

    let flip_line = |mut line: Vec<Sp<Word>>| {
        if line.iter().any(|w| matches!(w.value, Word::FlipLine)) {
            let mut parts = Vec::new();
            while let Some(i) = (line.iter()).rposition(|w| matches!(w.value, Word::FlipLine)) {
                parts.push(line.split_off(i + 1));
                line.pop();
            }
            parts.push(line);
            line = parts.into_iter().flatten().collect();
        }
        line
    };

    let mut new_lines = vec![flip_line(first)];

    for mut line in lines {
        // Trim spaces
        while (line.first()).is_some_and(|w| matches!(w.value, Word::Spaces)) {
            line.remove(0);
        }
        while line.last().is_some_and(|w| matches!(w.value, Word::Spaces)) {
            line.pop();
        }
        // Check for leading and trailing unbreak lines
        let unsplit_front = (line.first()).is_some_and(|w| matches!(w.value, Word::FlipLine));
        if unsplit_front {
            line.remove(0);
        }
        let unsplit_back = (line.last()).is_some_and(|w| matches!(w.value, Word::FlipLine));
        if unsplit_back {
            line.pop();
        }
        line = flip_line(line);
        // Reorder lines
        if unsplit || unsplit_front {
            let prev = new_lines.last_mut().unwrap();
            if in_array {
                prev.extend(line);
            } else {
                let taken_prev = replace(prev, line);
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
            func.lines = flip_unsplit_lines(func.lines);
            Word::Func(func)
        }
        Word::Array(mut arr) => {
            arr.lines = flip_unsplit_lines_impl(arr.lines, true);
            Word::Array(arr)
        }
        Word::Pack(mut pack) => {
            pack.branches = pack
                .branches
                .into_iter()
                .map(|mut br| {
                    br.value.lines = flip_unsplit_lines(br.value.lines);
                    br
                })
                .collect();
            Word::Pack(pack)
        }
        Word::Modified(mut m) => {
            m.operands = m.operands.into_iter().map(unsplit_word).collect();
            Word::Modified(m)
        }
        Word::Subscript(mut sub) => {
            sub.word = unsplit_word(sub.word);
            Word::Subscript(sub)
        }
        word => word,
    })
}

fn split_word(word: Sp<Word>) -> Sp<Word> {
    word.map(|word| match word {
        Word::Func(mut func) => {
            func.lines = func.lines.into_iter().flat_map(split_words).collect();
            Word::Func(func)
        }
        Word::Array(mut arr) => {
            arr.lines = arr
                .lines
                .into_iter()
                .flat_map(|line| {
                    let mut split_words = split_words(line);
                    split_words.reverse();
                    split_words
                })
                .collect();
            Word::Array(arr)
        }
        Word::Pack(mut pack) => {
            pack.branches = pack
                .branches
                .into_iter()
                .map(|mut br| {
                    br.value.lines = br.value.lines.into_iter().flat_map(split_words).collect();
                    br
                })
                .collect();
            Word::Pack(pack)
        }
        Word::Modified(mut m) => {
            m.operands = m.operands.into_iter().map(split_word).collect();
            Word::Modified(m)
        }
        Word::Subscript(mut sub) => {
            sub.word = split_word(sub.word);
            Word::Subscript(sub)
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

pub(crate) fn count_placeholders(words: &[Sp<Word>]) -> usize {
    let mut count = 0;
    for word in words {
        match &word.value {
            Word::Placeholder(_) => count += 1,
            Word::Strand(items) => count += count_placeholders(items),
            Word::Array(arr) => {
                for line in &arr.lines {
                    count += count_placeholders(line);
                }
            }
            Word::Func(func) => {
                for line in &func.lines {
                    count += count_placeholders(line);
                }
            }
            Word::Modified(m) => count += count_placeholders(&m.operands),
            Word::Pack(pack) => {
                for branch in &pack.branches {
                    for line in &branch.value.lines {
                        count += count_placeholders(line);
                    }
                }
            }
            _ => {}
        }
    }
    count
}

pub(crate) fn trim_spaces(words: &[Sp<Word>], trim_end: bool) -> &[Sp<Word>] {
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
    single_word_and(arr.lines.iter().flatten(), |m| {
        if let Word::Modified(m) = &m.value {
            let Modified { modifier, operands } = &**m;
            if let Modifier::Primitive(Primitive::Dip) = modifier.value {
                single_word_and(operands, |f| {
                    is_di = matches!(f.value, Word::Primitive(Primitive::Identity));
                })
            }
        }
    });
    is_di
}
