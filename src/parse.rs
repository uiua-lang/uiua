use std::{error::Error, fmt, path::Path};

use crate::{
    ast::*,
    function::FunctionId,
    lex::{Simple::*, *},
    ops::{Op2, Primitive},
    Ident,
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
    Block,
    Type,
    Words,
    Pattern,
    Eof,
    FunctionBody,
    Parameter,
    Term,
    Simple(Simple),
    Keyword(Keyword),
}

impl From<Simple> for Expectation {
    fn from(simple: Simple) -> Self {
        Expectation::Simple(simple)
    }
}

impl From<Keyword> for Expectation {
    fn from(keyword: Keyword) -> Self {
        Expectation::Keyword(keyword)
    }
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expectation::Ident => write!(f, "identifier"),
            Expectation::Block => write!(f, "block"),
            Expectation::Type => write!(f, "type"),
            Expectation::Words => write!(f, "words"),
            Expectation::Pattern => write!(f, "pattern"),
            Expectation::Eof => write!(f, "end of file"),
            Expectation::FunctionBody => write!(f, "function body"),
            Expectation::Parameter => write!(f, "parameter"),
            Expectation::Term => write!(f, "term"),
            Expectation::Simple(s) => write!(f, "{s}"),
            Expectation::Keyword(k) => write!(f, "{k}"),
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

struct Parser {
    tokens: Vec<Sp<crate::lex::Token>>,
    index: usize,
    errors: Vec<Sp<ParseError>>,
}

#[allow(unused)]
const PARENS: (Simple, Simple) = (OpenParen, CloseParen);
const BRACKETS: (Simple, Simple) = (OpenBracket, CloseBracket);
#[allow(unused)]
const CURLIES: (Simple, Simple) = (OpenCurly, CloseCurly);

impl Parser {
    fn next_token_map<'a, T: 'a>(
        &'a mut self,
        f: impl FnOnce(&'a Token) -> Option<T>,
    ) -> Option<Sp<T>> {
        while let Some(token) = self.tokens.get(self.index) {
            if matches!(token.value, Token::Comment(_)) {
                self.index += 1;
            } else if let Some(value) = f(&token.value) {
                self.index += 1;
                return Some(token.span.clone().sp(value));
            } else {
                return None;
            }
        }
        None
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
        let item = if let Some(r#let) = self.try_let()? {
            Item::Let(r#let)
        } else if let Some(r#const) = self.try_const()? {
            Item::Const(r#const)
        } else if self.try_exact(Keyword::Do).is_some() {
            Item::Words(self.words()?)
        } else {
            return Ok(None);
        };
        Ok(Some(item))
    }
    fn try_let(&mut self) -> ParseResult<Option<Let>> {
        // Let
        if self.try_exact(Keyword::Let).is_none() {
            return Ok(None);
        };
        // Name
        let name = self.ident()?;
        self.expect(Equal)?;
        // Words
        let mut words = self.words()?;
        if words.len() == 1 {
            if let Word::Func(func) = &mut words[0].value {
                func.id = FunctionId::Named(name.value.clone());
            }
        }
        Ok(Some(Let { name, words }))
    }
    fn try_const(&mut self) -> ParseResult<Option<Const>> {
        // Const
        if self.try_exact(Keyword::Const).is_none() {
            return Ok(None);
        };
        // Pattern
        let name = self.ident()?;
        self.expect(Equal)?;
        // Words
        let words = self.words()?;
        Ok(Some(Const { name, words }))
    }
    fn try_ident(&mut self) -> Option<Sp<Ident>> {
        self.next_token_map(|token| token.as_ident().cloned())
    }
    fn ident(&mut self) -> ParseResult<Sp<Ident>> {
        self.try_ident()
            .ok_or_else(|| self.expected([Expectation::Ident]))
    }
    fn try_surrounded_list<T>(
        &mut self,
        (open, close): (Simple, Simple),
        item: impl Fn(&mut Self) -> ParseResult<Option<T>>,
    ) -> ParseResult<Option<Sp<Vec<T>>>> {
        let Some(start) = self.try_exact(open) else {
            return Ok(None);
        };
        self.try_exact(Newline);
        let mut items = Vec::new();
        while let Some(item) = item(self)? {
            items.push(item);
            if self.try_exact(Comma).is_none() {
                break;
            }
            self.try_exact(Newline);
        }
        let end = self.expect(close)?;
        let span = start.merge(end);
        Ok(Some(span.sp(items)))
    }
}

static BIN_OPS: &[(Simple, Primitive)] = &[
    (Equal, Primitive::Op2(Op2::Eq)),
    (NotEqual, Primitive::Op2(Op2::Ne)),
    (Less, Primitive::Op2(Op2::Lt)),
    (LessEqual, Primitive::Op2(Op2::Le)),
    (Greater, Primitive::Op2(Op2::Gt)),
    (GreaterEqual, Primitive::Op2(Op2::Ge)),
    (Plus, Primitive::Op2(Op2::Add)),
    (Minus, Primitive::Op2(Op2::Sub)),
    (Star, Primitive::Op2(Op2::Mul)),
    (Percent, Primitive::Op2(Op2::Div)),
    (Slash, Primitive::Fork),
];

impl Parser {
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
        let Some(mut word) = self.try_term()? else {
            return Ok(None);
        };
        let mut items = Vec::new();
        while self.try_exact(Underscore).is_some() {
            let item = self
                .try_term()?
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
    fn try_term(&mut self) -> ParseResult<Option<Sp<Word>>> {
        Ok(Some(if let Some(ident) = self.try_ident() {
            ident.map(Word::Ident)
        } else if let Some(r) = self.next_token_map(Token::as_number) {
            r.map(Into::into).map(Word::Real)
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Word::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Word::String)
        } else if let Some(start) = self.try_exact(OpenParen) {
            let mut body = Vec::new();
            while let Some(item) = self.try_word()? {
                body.push(item);
            }
            let end = self.expect(CloseParen)?;
            let span = start.merge(end);
            span.clone().sp(Word::Func(Func {
                id: FunctionId::Anonymous(span),
                body,
            }))
        } else if let Some(items) = self.try_surrounded_list(BRACKETS, Self::try_word)? {
            items.map(Word::Array)
        } else if let Some(prim) = self.try_op()? {
            prim.map(Word::Primitive)
        } else {
            return Ok(None);
        }))
    }
    fn try_op(&mut self) -> ParseResult<Option<Sp<Primitive>>> {
        for (simple, prim) in BIN_OPS {
            if let Some(span) = self.try_exact(*simple) {
                return Ok(Some(span.sp(*prim)));
            }
        }
        Ok(None)
    }
}
