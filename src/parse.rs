use std::{error::Error, fmt, path::Path};

use crate::{
    ast::*,
    function::FunctionId,
    lex::{Simple::*, *},
    primitive::Primitive,
    Ident,
};

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<Box<Sp<Token>>>),
    InvalidNumber(String),
}

#[derive(Debug)]
pub enum Expectation {
    Term,
    Eof,
    Simple(Simple),
}

impl From<Simple> for Expectation {
    fn from(simple: Simple) -> Self {
        Expectation::Simple(simple)
    }
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expectation::Term => write!(f, "term"),
            Expectation::Eof => write!(f, "end of file"),
            Expectation::Simple(s) => write!(f, "`{s}`"),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Lex(e) => write!(f, "{e}"),
            ParseError::Expected(exps, found) => {
                write!(f, "expected ")?;
                for (i, exp) in exps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{exp}")?;
                }
                if let Some(found) = found {
                    write!(f, ", found `{}`", found.span.as_str())?;
                }
                Ok(())
            }
            ParseError::InvalidNumber(s) => write!(f, "invalid number `{s}`"),
        }
    }
}

impl Error for ParseError {}

pub type ParseResult<T = ()> = Result<T, Sp<ParseError>>;

pub fn parse(input: &str, path: Option<&Path>) -> (Vec<Item>, Vec<Sp<ParseError>>) {
    let (tokens, lex_errors) = lex(input, path);
    let errors = lex_errors
        .into_iter()
        .map(|e| e.map(ParseError::Lex))
        .collect();
    let mut parser = Parser {
        tokens,
        index: 0,
        errors,
    };
    let items = parser.items(true);
    if parser.errors.is_empty() && parser.index < parser.tokens.len() {
        parser.errors.push(parser.expected([Expectation::Eof]));
    }
    (items, parser.errors)
}

struct Parser {
    tokens: Vec<Sp<crate::lex::Token>>,
    index: usize,
    errors: Vec<Sp<ParseError>>,
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
    fn last_span(&self) -> CodeSpan {
        if let Some(token) = self.tokens.get(self.index) {
            token.span.clone()
        } else {
            let mut span = self.tokens.last().unwrap().span.clone();
            span.start = span.end;
            if self.tokens.len() > span.end.char_pos {
                span.end.char_pos += 1;
                span.end.col += 1;
            }
            span
        }
    }
    fn expected<I: Into<Expectation>>(
        &self,
        expectations: impl IntoIterator<Item = I>,
    ) -> Sp<ParseError> {
        self.last_span().sp(ParseError::Expected(
            expectations.into_iter().map(Into::into).collect(),
            self.tokens.get(self.index).cloned().map(Box::new),
        ))
    }
    #[allow(unused)]
    fn expected_continue<I: Into<Expectation>>(
        &mut self,
        expectations: impl IntoIterator<Item = I>,
    ) {
        let err = self.last_span().sp(ParseError::Expected(
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
                    let mut newline_item = false;
                    while self.try_exact(Newline).is_some() {
                        newline_item = true;
                    }
                    if newline_item {
                        items.push(Item::Newlines);
                    }
                }
            }
        }
        items
    }
    fn try_item(&mut self, parse_scopes: bool) -> Option<Item> {
        self.try_exact(Spaces);
        Some(if let Some(binding) = self.try_binding() {
            Item::Binding(binding, self.comment())
        } else if let Some(words) = self.try_words() {
            Item::Words(words, self.comment())
        } else if let Some(comment) = self.comment() {
            Item::Comment(comment)
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
        let s = span.as_str().trim_start_matches(['#', ' ']).into();
        Some(span.sp(s))
    }
    fn try_binding(&mut self) -> Option<Binding> {
        let start = self.index;
        Some(if let Some(ident) = self.try_ident() {
            self.try_exact(Spaces);
            if self.try_exact(Equal).is_none() && self.try_exact(LeftArrow).is_none() {
                self.index = start;
                return None;
            }
            self.try_exact(Spaces);
            let words = self.try_words().unwrap_or_default();
            Binding { name: ident, words }
        } else {
            return None;
        })
    }
    fn try_ident(&mut self) -> Option<Sp<Ident>> {
        let span = self.try_exact(Token::Ident)?;
        let s = span.as_str().into();
        Some(span.sp(s))
    }
    fn try_words(&mut self) -> Option<Vec<Sp<Word>>> {
        let mut words = Vec::new();
        while let Some(word) = self.try_word() {
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
        while self.try_exact(Newline).is_some() || self.try_exact(Spaces).is_some() {}
        while let Some(words) = self.try_words() {
            lines.push(words);
            let mut newlines = 0;
            while self.try_exact(Newline).is_some() {
                newlines += 1;
                while self.try_exact(Spaces).is_some() {}
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
        self.try_strand()
    }
    fn try_strand(&mut self) -> Option<Sp<Word>> {
        let Some(word) = self.try_modified() else {
            return None;
        };
        let mut items = Vec::new();
        while let Some(uspan) = self.try_exact(Underscore) {
            let item = match self.try_modified() {
                Some(item) => {
                    if let Word::Spaces = item.value {
                        self.errors.push(self.expected([Expectation::Term]));
                    }
                    item
                }
                None => {
                    self.errors.push(self.expected([Expectation::Term]));
                    uspan.sp(Word::Primitive(Primitive::Noop))
                }
            };
            items.push(item);
        }
        if items.is_empty() {
            return Some(word);
        }
        items.insert(0, word);
        for item in &mut items {
            if let Word::Func(func) = &item.value {
                if func.body.is_empty() {
                    item.value = Word::Primitive(Primitive::Noop);
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
            }) else {
            return self.try_term();
        };
        let mut args = Vec::new();
        for _ in 0..margs {
            self.try_exact(Spaces);
            if let Some(arg) = self.try_modified() {
                args.push(arg);
            } else {
                break;
            }
        }
        Some(if args.is_empty() {
            modifier.map(Word::Primitive)
        } else {
            let span = modifier
                .span
                .clone()
                .merge(args.last().unwrap().span.clone());
            span.sp(Word::Modified(Box::new(Modified {
                modifier,
                words: args,
            })))
        })
    }
    fn try_term(&mut self) -> Option<Sp<Word>> {
        Some(if let Some(prim) = self.try_prim() {
            prim.map(Word::Primitive)
        } else if let Some(ident) = self.try_ident() {
            ident.map(Word::Ident)
        } else if let Some(span) = self.try_exact(Token::Number) {
            let s = span.as_str().to_string();
            let parseable = s.replace(['`', '¯'], "-");
            let n: f64 = match parseable.parse() {
                Ok(n) => n,
                Err(_) => {
                    self.errors
                        .push(self.last_span().sp(ParseError::InvalidNumber(s.clone())));
                    0.0
                }
            };
            span.sp(Word::Number(s, n))
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Word::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Word::String)
        } else if let Some(frags) = self.next_token_map(Token::as_format_string) {
            frags.map(Into::into).map(Word::FormatString)
        } else if let Some(expr) = self.try_func() {
            expr
        } else if let Some(expr) = self.try_dfn() {
            expr
        } else if let Some(start) = self.try_exact(OpenBracket) {
            let items = self.multiline_words();
            let end = self.expect_close(CloseBracket);
            let span = start.merge(end);
            span.sp(Word::Array(items))
        } else if let Some(span) = self.try_exact(Spaces) {
            span.sp(Word::Spaces)
        } else {
            return None;
        })
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
        let Some(start) = self.try_exact(OpenParen) else {
            return None;
        };
        let body = self.multiline_words();
        let end = self.expect_close(CloseParen);
        let span = start.merge(end);
        Some(span.clone().sp(if body.is_empty() {
            Word::Primitive(Primitive::Noop)
        } else {
            Word::Func(Func {
                id: FunctionId::Anonymous(span),
                body,
            })
        }))
    }
    fn try_dfn(&mut self) -> Option<Sp<Word>> {
        let Some(start) = self.try_exact(OpenCurly) else {
            return None;
        };
        let body = self.multiline_words();
        let end = self.expect_close(CloseCurly);
        let span = start.merge(end);
        Some(span.clone().sp(Word::Dfn(Func {
            id: FunctionId::Anonymous(span),
            body,
        })))
    }
    fn expect_close(&mut self, simple: Simple) -> CodeSpan {
        if let Some(span) = self.try_exact(simple) {
            span
        } else {
            self.errors
                .push(self.expected([Expectation::Term, Expectation::Simple(simple)]));
            self.last_span()
        }
    }
}
