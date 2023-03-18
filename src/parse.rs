use std::{error::Error, fmt, fs, path::Path};

use crate::{
    ast::*,
    function::{FunctionId, Selector},
    lex::{Simple::*, *},
    ops::Primitive,
    Ident, UiuaError, UiuaResult,
};

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<Box<Sp<Token>>>),
    BlockMustEndWithWord,
}

#[derive(Debug)]
pub enum Expectation {
    Ident,
    Words,
    Eof,
    Term,
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
            Expectation::Ident => write!(f, "identifier"),
            Expectation::Words => write!(f, "words"),
            Expectation::Eof => write!(f, "end of file"),
            Expectation::Term => write!(f, "term"),
            Expectation::Simple(s) => write!(f, "{s}"),
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
                    write!(f, "`{exp}`")?;
                }
                if let Some(found) = found {
                    write!(f, ", found `{}`", found.value)?;
                }
                Ok(())
            }
            ParseError::BlockMustEndWithWord => {
                write!(f, "block must end with an words")
            }
        }
    }
}

impl Error for ParseError {}

pub type ParseResult<T = ()> = Result<T, Sp<ParseError>>;

pub fn parse(input: &str, path: &Path) -> (Vec<Item>, Vec<Sp<ParseError>>) {
    let tokens = match lex(input, path) {
        Ok(tokens) => tokens,
        Err(e) => return (Vec::new(), vec![e.map(ParseError::Lex)]),
    };
    let mut items = Vec::new();
    let mut parser = Parser {
        tokens,
        index: 0,
        errors: Vec::new(),
    };
    loop {
        match parser.try_item() {
            Ok(Some(item)) => items.push(item),
            Ok(None) => {
                if parser.try_exact(Newline).is_none() {
                    break;
                }
                let mut newline_item = false;
                while parser.try_exact(Newline).is_some() {
                    newline_item = true;
                }
                if newline_item {
                    items.push(Item::Newlines);
                }
            }
            Err(e) => {
                parser.errors.push(e);
                break;
            }
        }
    }
    if parser.index != parser.tokens.len() {
        parser.errors.push(parser.expected([Expectation::Eof]));
    }
    (items, parser.errors)
}

pub fn format_items(items: Vec<Item>) -> String {
    let mut state = FormatState::default();
    for item in items {
        item.format(&mut state);
    }
    let mut s = state.string;
    s = s.trim_end().into();
    s.push('\n');
    s
}

pub fn format(input: &str, path: &Path) -> Result<String, Vec<Sp<ParseError>>> {
    let (items, errors) = parse(input, path);
    if errors.is_empty() {
        Ok(format_items(items))
    } else {
        Err(errors)
    }
}

pub fn format_file<P: AsRef<Path>>(path: P) -> UiuaResult<String> {
    let path = path.as_ref();
    let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.to_path_buf(), e))?;
    let formatted = format(&input, path)?;
    if formatted == input {
        return Ok(formatted);
    }
    fs::write(path, &formatted).map_err(|e| UiuaError::Format(path.to_path_buf(), e))?;
    Ok(formatted)
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
    fn try_exact(&mut self, token: impl Into<Token>) -> Option<Span> {
        let token = token.into();
        self.next_token_map(|t| (t == &token).then_some(()))
            .map(|t| t.span)
    }
    fn last_span(&self) -> Span {
        if let Some(token) = self.tokens.get(self.index) {
            token.span.clone()
        } else {
            let mut span = self.tokens.last().unwrap().span.clone();
            if let Span::Code(span) = &mut span {
                span.start = span.end;
                if self.tokens.len() > span.end.pos {
                    span.end.pos += 1;
                    span.end.col += 1;
                }
            }
            span
        }
    }
    fn expect<T>(&mut self, val: T) -> ParseResult<Span>
    where
        T: Copy + Into<Token> + Into<Expectation>,
    {
        self.try_exact(val).ok_or_else(|| self.expected([val]))
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
    fn try_item(&mut self) -> ParseResult<Option<Item>> {
        Ok(Some(if let Some(ident) = self.try_ident() {
            if self.try_exact(Equal).is_some() {
                let mut words = self.words()?;
                if words.len() == 1 {
                    if let Word::Func(func) = &mut words[0].value {
                        func.id = FunctionId::Named(ident.value.clone());
                    }
                }
                Item::Binding(Binding { name: ident, words })
            } else {
                let mut words = self.try_words()?.unwrap_or_default();
                words.insert(0, ident.map(Word::Ident));
                Item::Words(words)
            }
        } else if let Some(words) = self.try_words()? {
            Item::Words(words)
        } else if let Some(comment) = self.next_token_map(Token::as_comment) {
            Item::Comment(comment.value.into())
        } else {
            return Ok(None);
        }))
    }
    fn try_ident(&mut self) -> Option<Sp<Ident>> {
        self.next_token_map(|token| token.as_ident().cloned())
    }
    fn try_selector(&mut self) -> Option<Sp<Selector>> {
        self.next_token_map(|token| token.as_selector().cloned())
    }
    fn try_words(&mut self) -> ParseResult<Option<Vec<Sp<Word>>>> {
        let mut words = Vec::new();
        while let Some(word) = self.try_word()? {
            words.push(word);
        }
        Ok(if words.is_empty() { None } else { Some(words) })
    }
    fn words(&mut self) -> ParseResult<Vec<Sp<Word>>> {
        self.try_words()?
            .ok_or_else(|| self.expected([Expectation::Words]))
    }
    fn try_word(&mut self) -> ParseResult<Option<Sp<Word>>> {
        self.try_strand()
    }
    fn try_strand(&mut self) -> ParseResult<Option<Sp<Word>>> {
        let Some(mut word) = self.try_modified()? else {
            return Ok(None);
        };
        let mut items = Vec::new();
        while self.try_exact(Underscore).is_some() {
            let item = self
                .try_modified()?
                .ok_or_else(|| self.expected([Expectation::Term]))?;
            items.push(item);
        }
        if let Some(last) = items.last() {
            let span = word.span.clone().merge(last.span.clone());
            items.insert(0, word);
            word = span.sp(Word::Strand(items));
        }
        Ok(Some(word))
    }
    fn try_modified(&mut self) -> ParseResult<Option<Sp<Word>>> {
        for prim in Primitive::ALL.into_iter().filter(Primitive::is_modifier) {
            let op_span = self
                .try_exact(prim)
                .or_else(|| prim.name().ascii.and_then(|simple| self.try_exact(simple)));
            if let Some(mspan) = op_span {
                let inner = self.try_modified()?;
                return Ok(Some(if let Some(word) = inner {
                    let span = mspan.clone().merge(word.span.clone());
                    span.sp(Word::Modified(Box::new(Modified {
                        modifier: mspan.sp(prim),
                        word,
                    })))
                } else {
                    mspan.sp(Word::Primitive(prim))
                }));
            }
        }
        self.try_term()
    }
    fn try_term(&mut self) -> ParseResult<Option<Sp<Word>>> {
        Ok(Some(if let Some(prim) = self.try_op()? {
            prim.map(Word::Primitive)
        } else if let Some(ident) = self.try_ident() {
            ident.map(Word::Ident)
        } else if let Some(r) = self.next_token_map(Token::as_number) {
            r.map(Into::into).map(Word::Real)
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Word::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Word::String)
        } else if let Some(expr) = self.try_parened()? {
            expr
        } else if let Some(start) = self.try_exact(OpenBracket) {
            let items = self.try_words()?.unwrap_or_default();
            let end = self.expect(CloseBracket)?;
            let span = start.merge(end);
            span.sp(Word::Array(items))
        } else if let Some(sel) = self.try_selector() {
            sel.map(Word::Selector)
        } else {
            return Ok(None);
        }))
    }
    fn try_op(&mut self) -> ParseResult<Option<Sp<Primitive>>> {
        for prim in Primitive::ALL {
            let op_span = self
                .try_exact(prim)
                .or_else(|| prim.name().ascii.and_then(|simple| self.try_exact(simple)))
                .or_else(|| {
                    prim.name()
                        .ident
                        .and_then(|ident| {
                            self.next_token_map(|token| token.as_ident().filter(|i| **i == ident))
                        })
                        .map(|s| s.span)
                });
            if let Some(span) = op_span {
                return Ok(Some(span.sp(prim)));
            }
        }
        if let Some(n) = self.next_token_map(Token::as_colons) {
            return Ok(Some(n.map(Primitive::AdicFork)));
        }
        Ok(None)
    }
    fn try_parened(&mut self) -> ParseResult<Option<Sp<Word>>> {
        let Some(start) = self.try_exact(OpenParen) else {
            return Ok(None);
        };
        let mut groups = Vec::new();
        let end = loop {
            let words = self.try_words()?.unwrap_or_default();
            groups.push(words);
            if let Some(span) = self.try_exact(CloseParen) {
                break span;
            } else if self.try_exact(Bar).is_none() {
                return Err(
                    self.expected([Expectation::Simple(CloseParen), Expectation::Simple(Bar)])
                );
            }
        };
        let span = start.clone().merge(end);
        Ok(Some(span.clone().sp(if groups.is_empty() {
            unreachable!("At least one group is always pushed")
        } else if groups.len() <= 1 {
            let words = groups.into_iter().next().unwrap();
            if words.is_empty() {
                Word::Primitive(Primitive::Nop)
            } else {
                Word::Func(Func {
                    id: FunctionId::Anonymous(span),
                    body: words,
                })
            }
        } else {
            let mut last_span = start;
            let mut funcs = Vec::new();
            for words in groups {
                let span = if let Some((first, last)) = words.first().zip(words.last()) {
                    last_span = last.span.clone();
                    first.span.clone().merge(last.span.clone())
                } else {
                    last_span.clone()
                };
                funcs.push(Func {
                    id: FunctionId::Anonymous(span),
                    body: words,
                });
            }
            Word::FuncArray(funcs)
        })))
    }
}
