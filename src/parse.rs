use std::{error::Error, fmt, iter::once, path::Path};

use crate::{
    ast::*,
    function::{FunctionId, Signature},
    lex::{AsciiToken::*, Token::*, *},
    primitive::Primitive,
    Diagnostic, DiagnosticKind, Ident,
};

#[derive(Debug, Clone)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<Box<Sp<Token>>>),
    InvalidNumber(String),
    Unexpected(Token),
    InvalidArgCount(String),
    InvalidOutCount(String),
    AmpersandBindingName,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expectation {
    Term,
    ArgOutCount,
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
                    if let Token::Simple(ascii) = found.value {
                        if exps.iter().any(|exp| exp == &Expectation::Simple(ascii)) {
                            return Ok(());
                        }
                    }
                    let found = found.span.as_str();
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
        }
    }
}

impl Error for ParseError {}

pub type ParseResult<T = ()> = Result<T, Sp<ParseError>>;

pub fn parse(
    input: &str,
    path: Option<&Path>,
) -> (Vec<Item>, Vec<Sp<ParseError>>, Vec<Diagnostic>) {
    let (tokens, lex_errors) = lex(input, path);
    let errors = lex_errors
        .into_iter()
        .map(|e| e.map(ParseError::Lex))
        .collect();
    let mut parser = Parser {
        tokens,
        index: 0,
        errors,
        diagnostics: Vec::new(),
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

struct Parser {
    tokens: Vec<Sp<crate::lex::Token>>,
    index: usize,
    errors: Vec<Sp<ParseError>>,
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
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
                .cloned()
                .map(Box::new),
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
        loop {
            match self.try_item(parse_scopes) {
                Some(item) => items.push(item),
                None => {
                    if self.try_exact(Newline).is_none() {
                        break;
                    }
                    let mut newline_span: Option<CodeSpan> = None;
                    while let Some(span) = self.try_exact(Newline) {
                        newline_span = Some(if let Some(prev) = newline_span.take() {
                            prev.merge(span)
                        } else {
                            span
                        });
                    }
                    items.extend(newline_span.map(Item::ExtraNewlines));
                }
            }
        }
        items
    }
    fn try_item(&mut self, parse_scopes: bool) -> Option<Item> {
        self.try_spaces();
        Some(if let Some(binding) = self.try_binding() {
            Item::Binding(binding)
        } else if let Some(words) = self.try_words() {
            Item::Words(words)
        } else if parse_scopes && self.try_exact(TripleMinus).is_some() {
            let items = self.items(false);
            if self.try_exact(TripleMinus).is_none() {
                self.errors.push(self.expected([TripleMinus]));
            }
            Item::Scoped { items, test: false }
        } else if parse_scopes && self.try_exact(TripleTilde).is_some() {
            let items = self.items(false);
            if self.try_exact(TripleTilde).is_none() {
                self.errors.push(self.expected([TripleTilde]));
            }
            Item::Scoped { items, test: true }
        } else {
            return None;
        })
    }
    fn comment(&mut self) -> Option<Sp<String>> {
        let span = self.try_exact(Token::Comment)?;
        let s = span.as_str();
        let s = s.strip_prefix('#').unwrap_or(s).into();
        Some(span.sp(s))
    }
    fn try_binding(&mut self) -> Option<Binding> {
        let start = self.index;
        Some(if let Some(ident) = self.try_ident() {
            if ident.value.contains('&') {
                self.errors
                    .push(self.prev_span().sp(ParseError::AmpersandBindingName));
            }
            self.try_spaces();
            if self.try_exact(Equal).is_none() && self.try_exact(LeftArrow).is_none() {
                self.index = start;
                return None;
            }
            self.try_spaces();
            let sig = self.try_signature();
            let words = self.try_words().unwrap_or_default();
            // Check for uncapitalized binding names
            if ident.value.chars().count() >= 3
                && ident.value.chars().next().unwrap().is_ascii_lowercase()
            {
                let captialized: String = ident
                    .value
                    .chars()
                    .next()
                    .map(|c| c.to_ascii_uppercase())
                    .into_iter()
                    .chain(ident.value.chars().skip(1))
                    .collect();
                self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Binding names with 3 or more characters should be TitleCase \
                        to avoid collisions with future builtin functions.\n\
                        Try `{}` instead of `{}`",
                        captialized, ident.value
                    ),
                    ident.span.clone(),
                    DiagnosticKind::Advice,
                ));
            }
            Binding {
                name: ident,
                words,
                signature: sig,
            }
        } else {
            return None;
        })
    }
    fn try_ident(&mut self) -> Option<Sp<Ident>> {
        let span = self.try_exact(Token::Ident)?;
        let s: Ident = span.as_str().into();
        Some(span.sp(s))
    }
    fn try_signature(&mut self) -> Option<Sp<Signature>> {
        let start = self.try_exact(Bar)?;
        self.try_spaces();
        let (args, outs) = if let Some(sn) = self.try_num() {
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
        };
        let end = self.prev_span();
        self.try_spaces();
        let span = start.merge(end);
        Some(span.sp(Signature::new(args, outs)))
    }
    fn try_words(&mut self) -> Option<Vec<Sp<Word>>> {
        let mut words: Vec<Sp<Word>> = Vec::new();
        while let Some(word) = self.try_word() {
            if let Some(prev) = words.last() {
                // Style diagnostics
                use Primitive::*;
                let span = || prev.span.clone().merge(word.span.clone());
                if let (Word::Primitive(a), Word::Primitive(b)) = (&prev.value, &word.value) {
                    match (a, b) {
                        (Flip, Over) => self.diagnostics.push(Diagnostic::new(
                            format!("Prefer `{Dip}{Dup}` over `{Flip}{Over}` for clarity"),
                            span(),
                            DiagnosticKind::Style,
                        )),
                        // Not comparisons
                        (Not, prim) => {
                            for (a, b) in [(Eq, Ne), (Lt, Ge), (Gt, Le)] {
                                if *prim == a {
                                    self.diagnostics.push(Diagnostic::new(
                                        format!("Prefer `{b}` over `{Not}{prim}` for clarity"),
                                        span(),
                                        DiagnosticKind::Style,
                                    ));
                                } else if *prim == b {
                                    self.diagnostics.push(Diagnostic::new(
                                        format!("Prefer `{a}` over `{Not}{prim}` for clarity"),
                                        span(),
                                        DiagnosticKind::Style,
                                    ));
                                }
                            }
                        }
                        _ => {}
                    }
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
        while self.try_exact(Newline).is_some() || self.try_spaces().is_some() {}
        while let Some(words) = self.try_words() {
            lines.push(words);
            let mut newlines = 0;
            while self.try_exact(Newline).is_some() {
                newlines += 1;
                self.try_spaces();
            }
            if newlines > 1 {
                lines.push(Vec::new());
            }
        }
        if lines.last().is_some_and(|line| line.is_empty()) {
            lines.pop();
        }
        lines
    }
    fn try_word(&mut self) -> Option<Sp<Word>> {
        self.comment()
            .map(|c| c.map(Word::Comment))
            .or_else(|| self.try_strand())
    }
    fn try_strand(&mut self) -> Option<Sp<Word>> {
        let word = self.try_modified()?;
        if let Word::Spaces = word.value {
            return Some(word);
        }
        // Collect items
        let mut items = Vec::new();
        let mut singleton = false;
        while self.try_exact(Underscore).is_some() {
            let item = match self.try_modified() {
                Some(mut item) => {
                    if let Word::Spaces = item.value {
                        if items.is_empty() {
                            singleton = true;
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
                None if items.is_empty() => {
                    singleton = true;
                    break;
                }
                None => {
                    self.errors.push(self.expected([Expectation::Term]));
                    break;
                }
            };
            items.push(item);
        }
        // If there is only one item and no underscores, return it
        if items.is_empty() && !singleton {
            return Some(word);
        }
        // Insert the first word that was parsed
        items.insert(0, word);
        // Create identitys
        for item in &mut items {
            if let Word::Func(func) = &item.value {
                if func.lines.is_empty() && func.signature.is_none() {
                    item.value = Word::Primitive(Primitive::Identity);
                }
            }
        }
        let span = items[0]
            .span
            .clone()
            .merge(items.last().unwrap().span.clone());
        Some(span.sp(Word::Strand(items)))
    }
    fn try_modified(&mut self) -> Option<Sp<Word>> {
        let Some((modifier, margs)) = Primitive::all()
            .filter_map(|prim| prim.modifier_args().map(|margs| (prim, margs)))
            .find_map(|(prim, margs)| {
                self.try_exact(prim)
                    .or_else(|| prim.ascii().and_then(|simple| self.try_exact(simple)))
                    .map(|span| (span.sp(prim), margs))
            })
        else {
            return self.try_term();
        };
        let mut args = Vec::new();
        self.try_spaces();
        let mut terminated = false;
        for _ in 0..margs {
            if self.try_exact(Bar).is_some() {
                terminated = true;
                break;
            }
            args.extend(self.try_spaces());
            if let Some(arg) = self.try_strand() {
                args.push(arg);
            } else {
                break;
            }
        }

        // Style diagnostic for bind
        if let Primitive::Bind = modifier.value {
            for arg in &args {
                if let Word::Modified(m) = &arg.value {
                    if let Primitive::Bind = m.modifier.value {
                        let span = modifier.span.clone().merge(m.modifier.span.clone());
                        self.diagnostics.push(Diagnostic::new(
                            format!("Do not chain `bind {}`", Primitive::Bind),
                            span,
                            DiagnosticKind::Style,
                        ));
                    }
                }
            }
        }

        Some(if args.is_empty() {
            modifier.map(Word::Primitive)
        } else {
            for arg in &mut args {
                if let Word::Func(func) = &arg.value {
                    if func.lines.is_empty() && func.signature.is_none() {
                        arg.value = Word::Primitive(Primitive::Identity);
                    }
                }
            }
            let span = modifier
                .span
                .clone()
                .merge(args.last().unwrap().span.clone());
            span.sp(Word::Modified(Box::new(Modified {
                modifier,
                operands: args,
                terminated,
            })))
        })
    }
    fn try_term(&mut self) -> Option<Sp<Word>> {
        Some(if let Some(prim) = self.try_prim() {
            if prim.value.is_ocean() {
                let mut ocean_parts = Vec::new();
                while let Some(part) = self.try_ocean() {
                    ocean_parts.push(part);
                }
                if ocean_parts.is_empty() {
                    prim.map(Word::Primitive)
                } else {
                    let span = prim
                        .span
                        .clone()
                        .merge(ocean_parts.last().unwrap().span.clone());
                    span.sp(Word::Ocean(once(prim).chain(ocean_parts).collect()))
                }
            } else {
                prim.map(Word::Primitive)
            }
        } else if let Some(ident) = self.try_ident() {
            ident.map(Word::Ident)
        } else if let Some(sn) = self.try_num() {
            sn.map(|(s, n)| Word::Number(s, n))
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Word::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Word::String)
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
        } else if let Some(expr) = self.try_func() {
            expr
        } else if let Some(start) = self.try_exact(OpenBracket) {
            let items = self.multiline_words();
            let end = self.expect_close(CloseBracket);
            let span = start.merge(end);
            span.sp(Word::Array(Arr {
                lines: items,
                constant: false,
            }))
        } else if let Some(start) = self.try_exact(OpenCurly) {
            let items = self.multiline_words();
            let end = self.expect_close(CloseCurly);
            let span = start.merge(end);
            span.sp(Word::Array(Arr {
                lines: items,
                constant: true,
            }))
        } else if let Some(spaces) = self.try_spaces() {
            spaces
        } else {
            return None;
        })
    }
    fn try_num(&mut self) -> Option<Sp<(String, f64)>> {
        let span = self.try_exact(Token::Number)?;
        let s = span.as_str().to_string();
        let parseable = s.replace(['`', '¯'], "-");
        let n: f64 = match parseable.parse() {
            Ok(n) => n,
            Err(_) => {
                self.errors
                    .push(self.prev_span().sp(ParseError::InvalidNumber(s.clone())));
                0.0
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
    fn try_ocean(&mut self) -> Option<Sp<Primitive>> {
        for prim in Primitive::all().filter(Primitive::is_ocean) {
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
            while self.try_exact(Newline).is_some() || self.try_spaces().is_some() {}
            let signature = self.try_signature();
            let body = self.multiline_words();
            let end = self.expect_close(CloseParen);
            let span = start.merge(end);
            span.clone().sp(Word::Func(Func {
                id: FunctionId::Anonymous(span),
                signature,
                lines: body,
            }))
        } else {
            return None;
        })
    }
    fn try_spaces(&mut self) -> Option<Sp<Word>> {
        self.try_exact(Spaces).map(|span| span.sp(Word::Spaces))
    }
    fn expect_close(&mut self, ascii: AsciiToken) -> CodeSpan {
        if let Some(span) = self.try_exact(ascii) {
            span
        } else {
            self.errors
                .push(self.expected([Expectation::Term, Expectation::Simple(ascii)]));
            self.prev_span()
        }
    }
}
