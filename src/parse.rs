use std::{
    error::Error,
    fmt,
    mem::{replace, take},
};

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
    FunctionNotAllowed,
    SplitInModifier,
    UnsplitInModifier,
    LineTooLong(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expectation {
    Term,
    ArgOutCount,
    ItemName,
    Simple(AsciiToken),
}

impl From<AsciiToken> for Expectation {
    fn from(simple: AsciiToken) -> Self {
        Expectation::Simple(simple)
    }
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expectation::Term => write!(f, "term"),
            Expectation::ArgOutCount => write!(f, "argument/output count"),
            Expectation::ItemName => write!(f, "item name"),
            Expectation::Simple(s) => write!(f, "`{s}`"),
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
                        write!(f, ", found `{found}`")?;
                    }
                }
                Ok(())
            }
            ParseError::InvalidNumber(s) => write!(f, "Invalid number `{s}`"),
            ParseError::Unexpected(_) => write!(f, "Unexpected token"),
            ParseError::InvalidArgCount(n) => write!(f, "Invalid argument count `{n}`"),
            ParseError::InvalidOutCount(n) => write!(f, "Invalid output count `{n}`"),
            ParseError::AmpersandBindingName => write!(f, "Binding names may not contain `&`"),
            ParseError::ModifierImportName => {
                write!(f, "Modifier names may not be used as import names")
            }
            ParseError::FunctionNotAllowed => write!(
                f,
                "Inline functions are only allowed in modifiers \
                or as the only item in a binding. \
                If you want to visually separate this code, \
                use spaces."
            ),
            ParseError::SplitInModifier => write!(
                f,
                "Line splitting is not allowed between modifier arguments"
            ),
            ParseError::UnsplitInModifier => write!(
                f,
                "Line unsplitting is not allowed between modifier arguments"
            ),
            ParseError::LineTooLong(width) => write!(
                f,
                "Split line into multiple lines (heuristic: {}/{}) 😏",
                width, ERROR_MAX_WIDTH
            ),
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
    let (tokens, lex_errors) = lex(input, src, inputs);
    fn parse(
        input: &str,
        inputs: &mut Inputs,
        tokens: Vec<Sp<crate::lex::Token>>,
        lex_errors: Vec<Sp<LexError>>,
    ) -> (Vec<Item>, Vec<Sp<ParseError>>, Vec<Diagnostic>) {
        let mut errors: Vec<_> = lex_errors
            .into_iter()
            .map(|e| e.map(ParseError::Lex))
            .collect();
        let mut diagnostics = Vec::new();

        // Check for lines that are too long
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

        // Parse
        let mut parser = Parser {
            inputs,
            input: input.into(),
            tokens,
            index: 0,
            errors,
            diagnostics,
            next_output_comment: 0,
        };
        let items = parser.items(true);
        if parser.errors.is_empty() && parser.index < parser.tokens.len() {
            parser.errors.push(
                parser
                    .tokens
                    .remove(parser.index)
                    .map(ParseError::Unexpected),
            );
        }
        (items, parser.errors, parser.diagnostics)
    }
    parse(input, inputs, tokens, lex_errors)
}

struct Parser<'i> {
    inputs: &'i mut Inputs,
    input: EcoString,
    tokens: Vec<Sp<crate::lex::Token>>,
    index: usize,
    next_output_comment: usize,
    errors: Vec<Sp<ParseError>>,
    diagnostics: Vec<Diagnostic>,
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
    fn try_exact(&mut self, token: impl Into<Token>) -> Option<CodeSpan> {
        let token = token.into();
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
    fn expected<I: Into<Expectation>>(
        &self,
        expectations: impl IntoIterator<Item = I>,
    ) -> Sp<ParseError> {
        self.prev_span().sp(ParseError::Expected(
            expectations.into_iter().map(Into::into).collect(),
            self.tokens
                .get(self.index.saturating_sub(1))
                .map(|t| self.input[t.span.byte_range()].into()),
        ))
    }
    #[allow(unused)]
    fn expected_continue<I: Into<Expectation>>(
        &mut self,
        expectations: impl IntoIterator<Item = I>,
    ) {
        let err = self.prev_span().sp(ParseError::Expected(
            expectations.into_iter().map(Into::into).collect(),
            None,
        ));
        self.errors.push(err);
    }
    fn items(&mut self, parse_scopes: bool) -> Vec<Item> {
        let mut items = Vec::new();
        while self.try_exact(Newline).is_some() {
            self.try_spaces();
        }
        loop {
            match self.try_item(parse_scopes) {
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
    fn try_item(&mut self, parse_scopes: bool) -> Option<Item> {
        self.try_spaces();
        Some(if let Some(binding) = self.try_binding() {
            Item::Binding(binding)
        } else if let Some(import) = self.try_import() {
            Item::Import(import)
        } else {
            let lines = self.multiline_words();
            // Convert multiline words into multiple items
            if !lines.is_empty() {
                // Validate words
                for line in &lines {
                    self.validate_words(line, false);
                }
                Item::Words(lines)
            } else if parse_scopes {
                let start = self.try_exact(TripleMinus)?;
                let items = self.items(false);
                let span = if let Some(end) = self.try_exact(TripleMinus) {
                    start.merge(end)
                } else {
                    self.errors.push(self.expected([TripleMinus]));
                    start
                };
                Item::TestScope(span.sp(items))
            } else {
                return None;
            }
        })
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
    fn try_binding_init(&mut self) -> Option<(Sp<Ident>, CodeSpan)> {
        let start = self.index;
        let name = self.try_ident()?;
        // Left arrow
        let mut arrow_span = self.try_spaces().map(|w| w.span);
        if let Some(span) = self.try_exact(Equal).or_else(|| self.try_exact(LeftArrow)) {
            arrow_span = Some(if let Some(arrow_span) = arrow_span {
                arrow_span.merge(span)
            } else {
                span
            });
        } else {
            self.index = start;
            return None;
        }
        let mut arrow_span = arrow_span.unwrap();
        if let Some(span) = self.try_spaces().map(|w| w.span) {
            arrow_span = arrow_span.merge(span);
        }
        Some((name, arrow_span))
    }
    fn try_binding(&mut self) -> Option<Binding> {
        let (name, arrow_span) = self.try_binding_init()?;
        // Bad name advice
        if ["\u{200b}", "\u{200c}", "\u{200d}"]
            .iter()
            .any(|bad_name| &*name.value == *bad_name)
        {
            self.diagnostics.push(Diagnostic::new(
                "Maybe don't",
                name.span.clone(),
                DiagnosticKind::Advice,
                self.inputs.clone(),
            ));
        }
        // Signature
        let signature = self.try_signature(Bar);
        // Words
        let words = self.try_words().unwrap_or_default();
        // Validate words
        if let (1, Some(Word::Func(func))) = (
            words.iter().filter(|w| w.value.is_code()).count(),
            &words.iter().find(|w| w.value.is_code()).map(|w| &w.value),
        ) {
            for line in &func.lines {
                self.validate_words(line, false);
            }
        } else {
            self.validate_words(&words, false);
        }
        self.validate_binding_name(&name);
        Some(Binding {
            name,
            arrow_span,
            words,
            signature,
        })
    }
    fn validate_binding_name(&mut self, name: &Sp<Ident>) {
        if name.value.contains('&') {
            self.errors
                .push(name.span.clone().sp(ParseError::AmpersandBindingName));
        }
        if name.value.trim_end_matches('!').chars().count() >= 2
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
        let start = self.index;
        // Name
        let name = self.try_ident();
        self.try_spaces();
        // Tilde
        let Some(tilde_span) = self.try_exact(Tilde) else {
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
        // Items
        let mut items: Vec<Vec<_>> = Vec::new();
        let mut line = Vec::new();
        let mut item_tilde_span = None;
        self.try_exact(Newline);
        while let Some(token) = self.tokens.get(self.index).cloned() {
            let span = token.span;
            let token = token.value;
            match token {
                Token::Ident if item_tilde_span.is_some() => {
                    let ident = Ident::from(&self.input[span.byte_range()]);
                    let name = span.clone().sp(ident);
                    let tilde_span = item_tilde_span.take().unwrap();
                    line.push(ImportItem { name, tilde_span });
                }
                Simple(Tilde) if item_tilde_span.is_none() => item_tilde_span = Some(span.clone()),
                Simple(Tilde) => self
                    .errors
                    .push(span.sp(ParseError::Unexpected(Simple(Tilde)))),
                Newline => {
                    if !(line.is_empty() && items.last().is_some_and(|line| line.is_empty())) {
                        items.push(take(&mut line));
                    }
                }
                Spaces => {}
                _ => break,
            }
            self.index += 1;
        }
        if !line.is_empty() {
            items.push(line);
        }
        if let Some(name) = &name {
            self.validate_binding_name(name);
            if name.value.contains('!') {
                self.errors
                    .push(name.span.clone().sp(ParseError::ModifierImportName));
            }
        }
        Some(Import {
            name,
            tilde_span,
            path,
            items,
        })
    }
    fn try_ident(&mut self) -> Option<Sp<Ident>> {
        let span = self.try_exact(Token::Ident)?;
        let s: Ident = self.input[span.byte_range()].into();
        Some(span.sp(s))
    }
    fn try_modifier_ident(&mut self) -> Option<Sp<Ident>> {
        let start = self.index;
        let ident = self.try_ident()?;
        if ident_modifier_args(&ident.value) == 0 {
            self.index = start;
            return None;
        }
        Some(ident)
    }
    fn try_signature(&mut self, initial_token: AsciiToken) -> Option<Sp<Signature>> {
        let start = self.try_exact(initial_token)?;
        self.try_spaces();
        let (args, outs) = self.sig_inner();
        let mut end = self.prev_span();
        if let Some(sp) = self.try_spaces() {
            end = sp.span;
        }
        let span = start.merge(end);
        Some(span.sp(Signature::new(args, outs)))
    }
    fn sig_inner(&mut self) -> (usize, usize) {
        if let Some(sn) = self.try_num() {
            if let Some((a, o)) = sn.value.0.split_once('.') {
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
            }
        } else {
            self.errors.push(self.expected([Expectation::ArgOutCount]));
            (1usize, 1usize)
        }
    }
    fn try_words(&mut self) -> Option<Vec<Sp<Word>>> {
        let mut words: Vec<Sp<Word>> = Vec::new();
        while let Some(word) = self.try_word() {
            if let Some(prev) = words.last() {
                // Diagnostics
                use Primitive::*;
                let span = || prev.span.clone().merge(word.span.clone());
                match (&prev.value, &word.value) {
                    (Word::Primitive(a), Word::Primitive(b)) => {
                        match (a, b) {
                            (Flip, Over) => self.diagnostics.push(Diagnostic::new(
                                format!("Prefer `{Dip}{Dup}` over `{Flip}{Over}` for clarity"),
                                span(),
                                DiagnosticKind::Style,
                                self.inputs.clone(),
                            )),
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
    fn multiline_words(&mut self) -> Vec<Vec<Sp<Word>>> {
        let mut lines = Vec::new();
        while self.try_spaces().is_some() {}
        loop {
            let curr = self.index;
            if self.try_binding_init().is_some() {
                self.index = curr;
                break;
            }
            if let Some(words) = self.try_words() {
                lines.push(words);
                let mut newlines = 0;
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
        lines
    }
    fn try_word(&mut self) -> Option<Sp<Word>> {
        self.comment()
            .map(|c| c.map(Word::Comment))
            .or_else(|| self.output_comment())
            .or_else(|| self.try_strand())
            .or_else(|| self.try_placeholder())
    }
    fn try_strand(&mut self) -> Option<Sp<Word>> {
        let word = self.try_modified()?;
        if let Word::Spaces = word.value {
            return Some(word);
        }
        // Collect items
        let mut items = Vec::new();
        while self.try_exact(Underscore).is_some() {
            let item = match self.try_term() {
                Some(mut item) => {
                    if let Word::Spaces = item.value {
                        if items.is_empty() {
                            break;
                        }
                        self.errors.push(self.expected([Expectation::Term]));
                        item = match self.try_term() {
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
        let (modifier, mod_span) = if let Some(prim) = Primitive::all()
            .filter(|prim| prim.is_modifier())
            .find_map(|prim| {
                self.try_exact(prim)
                    .or_else(|| prim.ascii().and_then(|simple| self.try_exact(simple)))
                    .map(|span| span.sp(prim))
            }) {
            (Modifier::Primitive(prim.value), prim.span)
        } else if let Some(ident) = self.try_modifier_ident() {
            (Modifier::Ident(ident.value), ident.span)
        } else {
            let term = self.try_term()?;
            if let Word::ModuleItem(item) = term.value {
                if ident_modifier_args(&item.name.value) == 0 {
                    return Some(term.span.sp(Word::ModuleItem(item)));
                }
                (Modifier::ModuleItem(item), term.span)
            } else {
                return Some(term);
            }
        };
        let mut args = Vec::new();
        self.try_spaces();
        for i in 0..modifier.args() {
            loop {
                args.extend(self.try_spaces());
                if let Some(span) = self.try_exact(Quote) {
                    self.errors.push(span.sp(ParseError::SplitInModifier));
                    continue;
                }
                if let Some(span) = self.try_exact(Quote2) {
                    self.errors.push(span.sp(ParseError::UnsplitInModifier));
                    continue;
                }
                break;
            }
            if let Some(arg) = self
                .try_func()
                .or_else(|| self.try_strand())
                .or_else(|| self.try_placeholder())
            {
                // Parse switch function syntax
                if let Word::Switch(sw) = &arg.value {
                    if i == 0 && sw.branches.len() >= modifier.args() {
                        args.push(arg);
                        break;
                    }
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
                    value: Word::Switch(sw),
                    ..
                }) = args.first()
                {
                    operands.extend(
                        sw.branches
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
                if operands.len() == 2 && operands[0] == operands[1] {
                    self.diagnostics.push(Diagnostic::new(
                        format!(
                            "{}'s functions are the same, so it \
                                can be replaced with {}.",
                            Primitive::Bracket.format(),
                            Primitive::Both.format(),
                        ),
                        span.clone(),
                        DiagnosticKind::Advice,
                        self.inputs.clone(),
                    ));
                }
            }
            _ => (),
        }

        Some(span.sp(Word::Modified(Box::new(Modified {
            modifier: mod_span.sp(modifier),
            operands: args,
        }))))
    }
    fn try_placeholder(&mut self) -> Option<Sp<Word>> {
        let sig = self.try_signature(Caret)?;
        Some(sig.map(Word::Placeholder))
    }
    fn try_term(&mut self) -> Option<Sp<Word>> {
        Some(if let Some(prim) = self.try_prim() {
            prim.map(Word::Primitive)
        } else if let Some(ident) = self.try_ident() {
            if let Some(tilde_span) = self.try_exact(Tilde) {
                let name = self.try_ident();
                let finished = name.is_some();
                if !finished {
                    self.errors.push(self.expected([Expectation::ItemName]));
                }
                let name = name.unwrap_or_else(|| tilde_span.clone().sp(Ident::default()));
                let span = ident.span.clone().merge(name.span.clone());
                span.sp(Word::ModuleItem(ModuleItem {
                    module: ident,
                    tilde_span,
                    name,
                    finished,
                }))
            } else {
                ident.map(Word::Ident)
            }
        } else if let Some(sn) = self.try_num() {
            sn.map(|(s, n)| Word::Number(s, n))
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Word::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Word::String)
        } else if let Some(label) = self.next_token_map(Token::as_label) {
            label.map(Into::into).map(Word::Label)
        } else if let Some(frags) = self.next_token_map(Token::as_format_string) {
            frags.map(Word::FormatString)
        } else if let Some(line) = self.next_token_map(Token::as_multiline_string) {
            let start = line.span.clone();
            let mut end = start.clone();
            let mut lines = vec![line];
            while let Some(line) = self.next_token_map(Token::as_multiline_string) {
                end = line.span.clone();
                lines.push(line);
            }
            let span = start.merge(end);
            span.sp(Word::MultilineString(lines))
        } else if let Some(start) = self.try_exact(OpenBracket) {
            while self.try_exact(Newline).is_some() {}
            let items = self.multiline_words();
            let end = self.expect_close(CloseBracket);
            let span = start.merge(end.span);
            let arr = Arr {
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
        } else if let Some(start) = self.try_exact(OpenCurly) {
            while self.try_exact(Newline).is_some() {}
            let items = self.multiline_words();
            let end = self.expect_close(CloseCurly);
            let span = start.merge(end.span);
            span.sp(Word::Array(Arr {
                lines: items,
                boxes: true,
                closed: end.value,
            }))
        } else if let Some(spaces) = self.try_spaces() {
            spaces
        } else if let Some(word) = self.try_func() {
            word
        } else if let Some(span) = self.try_exact(Quote) {
            span.sp(Word::BreakLine)
        } else if let Some(span) = self.try_exact(Quote2) {
            span.sp(Word::UnbreakLine)
        } else if let Some(span) = self.try_exact(Semicolon) {
            self.diagnostics.push(Diagnostic::new(
                format!(
                    "`;` for {} is deprecated and will be removed in the future",
                    Primitive::Pop.format(),
                ),
                span.clone(),
                DiagnosticKind::Style,
                self.inputs.clone(),
            ));
            span.sp(Word::Primitive(Primitive::Pop))
        } else {
            return None;
        })
    }
    fn try_num(&mut self) -> Option<Sp<(String, f64)>> {
        let span = self.try_exact(Token::Number)?;
        let s = self.input[span.byte_range()].to_string();
        fn parse(s: &str) -> Option<f64> {
            let parseable = s.replace(['`', '¯'], "-");
            parseable.parse().ok()
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
            let op_span = self
                .try_exact(prim)
                .or_else(|| prim.ascii().and_then(|simple| self.try_exact(simple)));
            if let Some(span) = op_span {
                return Some(span.sp(prim));
            }
        }
        None
    }
    fn try_func(&mut self) -> Option<Sp<Word>> {
        Some(if let Some(start) = self.try_exact(OpenParen) {
            let first = self.func_contents();
            let mut branches = Vec::new();
            while let Some(start) = self.try_exact(Bar) {
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
            let end = self.expect_close(CloseParen);
            let (first_sig, first_lines, first_span) = first;
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
                let first_span = first_span.unwrap_or(start);
                let first_id = FunctionId::Anonymous(first_span.clone());
                let first = first_span.sp(Func {
                    id: first_id,
                    signature: first_sig,
                    lines: first_lines,
                    closed: true,
                });
                branches.insert(0, first);
                outer_span.sp(Word::Switch(Switch {
                    branches,
                    closed: end.value,
                }))
            }
        } else {
            return None;
        })
    }
    fn func_contents(&mut self) -> FunctionContents {
        let mut any_newlines = false;
        loop {
            if self.try_exact(Newline).is_some() {
                any_newlines = true;
                continue;
            }
            if self.try_spaces().is_some() {
                continue;
            }
            break;
        }
        let signature = self.try_signature(Bar);
        loop {
            if self.try_exact(Newline).is_some() {
                any_newlines = true;
                continue;
            }
            if self.try_spaces().is_some() {
                continue;
            }
            break;
        }
        let mut lines = self.multiline_words();
        any_newlines |= lines.len() > 1;
        if any_newlines && !lines.last().is_some_and(|line| line.is_empty()) {
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
    fn expect_close(&mut self, ascii: AsciiToken) -> Sp<bool> {
        if let Some(span) = self.try_exact(ascii) {
            span.sp(true)
        } else {
            self.errors
                .push(self.expected([Expectation::Term, Expectation::Simple(ascii)]));
            self.prev_span().sp(false)
        }
    }
    fn validate_words(&mut self, words: &[Sp<Word>], allow_func: bool) {
        for word in words {
            match &word.value {
                Word::Strand(items) => self.validate_words(items, false),
                Word::Array(arr) => {
                    for line in &arr.lines {
                        self.validate_words(line, false);
                    }
                }
                Word::Func(func) => {
                    if !allow_func && func.closed {
                        self.errors
                            .push(word.span.clone().sp(ParseError::FunctionNotAllowed));
                    }
                    for line in &func.lines {
                        self.validate_words(line, false);
                    }
                }
                Word::Switch(sw) => {
                    for branch in &sw.branches {
                        for line in &branch.value.lines {
                            self.validate_words(line, false);
                        }
                    }
                }
                Word::Modified(m) => self.validate_words(&m.operands, true),
                _ => {}
            }
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

pub(crate) fn unsplit_words(lines: impl IntoIterator<Item = Vec<Sp<Word>>>) -> Vec<Vec<Sp<Word>>> {
    unsplit_words_impl(lines, false)
}
fn unsplit_words_impl(
    lines: impl IntoIterator<Item = Vec<Sp<Word>>>,
    in_array: bool,
) -> Vec<Vec<Sp<Word>>> {
    let mut lines = lines
        .into_iter()
        .map(|line| line.into_iter().map(unsplit_word).collect::<Vec<_>>());
    let Some(mut first) = lines.next() else {
        return Vec::new();
    };
    let mut unsplit = trim_spaces(&first, true)
        .last()
        .is_some_and(|w| matches!(w.value, Word::UnbreakLine));
    first.retain(|w| !matches!(w.value, Word::UnbreakLine));
    let mut new_lines = vec![first];
    for mut line in lines {
        let trimmed = trim_spaces(&line, true);
        let unsplit_front = trimmed
            .first()
            .is_some_and(|w| matches!(w.value, Word::UnbreakLine));
        let unsplit_back = trimmed
            .last()
            .is_some_and(|w| matches!(w.value, Word::UnbreakLine));
        line.retain(|w| !matches!(w.value, Word::UnbreakLine));
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
            func.lines = unsplit_words(func.lines);
            Word::Func(func)
        }
        Word::Array(mut arr) => {
            arr.lines = unsplit_words_impl(arr.lines, true);
            Word::Array(arr)
        }
        Word::Switch(mut sw) => {
            sw.branches = sw
                .branches
                .into_iter()
                .map(|mut br| {
                    br.value.lines = unsplit_words(br.value.lines);
                    br
                })
                .collect();
            Word::Switch(sw)
        }
        Word::Modified(mut m) => {
            m.operands = m.operands.into_iter().map(unsplit_word).collect();
            Word::Modified(m)
        }
        word => word,
    })
}

fn split_word(word: Sp<Word>) -> Sp<Word> {
    word.map(|word| match word {
        Word::Func(mut func) => {
            func.lines = func.lines.into_iter().flat_map(split_words).collect();
            if func.lines.len() > 1 && !func.lines.last().unwrap().is_empty() {
                func.lines.push(Vec::new());
            }
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
        Word::Switch(mut sw) => {
            sw.branches = sw
                .branches
                .into_iter()
                .map(|mut br| {
                    br.value.lines = br.value.lines.into_iter().flat_map(split_words).collect();
                    br
                })
                .collect();
            Word::Switch(sw)
        }
        Word::Modified(mut m) => {
            m.operands = m.operands.into_iter().map(split_word).collect();
            Word::Modified(m)
        }
        word => word,
    })
}

/// Get the number of modifier arguments implied by an identifier
pub fn ident_modifier_args(ident: &str) -> usize {
    let mut count: usize = 0;
    let mut prefix = ident;
    while let Some(pre) = prefix.strip_suffix('!') {
        prefix = pre;
        count = count.saturating_add(1);
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
            Word::Switch(sw) => {
                for branch in &sw.branches {
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
