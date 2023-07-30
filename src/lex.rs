use std::{
    cmp::Ordering,
    error::Error,
    fmt,
    hash::{Hash, Hasher},
    path::Path,
    sync::Arc,
};

use crate::{primitive::Primitive, Ident, UiuaError};

pub fn lex(input: &str, file: Option<&Path>) -> LexResult<Vec<Sp<Token>>> {
    Lexer {
        input_chars: input.chars().collect(),
        loc: Loc {
            pos: 0,
            line: 1,
            col: 1,
        },
        file: file.map(Into::into),
        input: input.into(),
        tokens: Vec::new(),
    }
    .run()
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedChar(char),
    ExpectedCharacter(Option<char>),
    InvalidEscape(char),
    ExpectedNumber,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar(c) => write!(f, "unexpected char {c:?}"),
            LexError::ExpectedCharacter(Some(c)) => write!(f, "expected {c:?}"),
            LexError::ExpectedCharacter(None) => write!(f, "expected character"),
            LexError::InvalidEscape(c) => write!(f, "invalid escape character {c:?}"),
            LexError::ExpectedNumber => write!(f, "expected number"),
        }
    }
}

impl Error for LexError {}

pub type LexResult<T = ()> = Result<T, Sp<LexError>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self {
            pos: 0,
            line: 1,
            col: 1,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Span {
    Code(CodeSpan),
    Builtin,
}

#[derive(Clone)]
pub struct CodeSpan {
    pub start: Loc,
    pub end: Loc,
    pub file: Option<Arc<Path>>,
    pub input: Arc<str>,
}

impl fmt::Debug for CodeSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for CodeSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(file) = &self.file {
            write!(f, "{}:{}", file.to_string_lossy(), self.start)
        } else {
            write!(f, "{}", self.start)
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Span::Code(span) => write!(f, "{span}"),
            Span::Builtin => write!(f, "<builtin>"),
        }
    }
}

impl Span {
    pub fn merge(self, end: Self) -> Self {
        match (self, end) {
            (Span::Code(a), Span::Code(b)) => Span::Code(a.merge(b)),
            (Span::Builtin, Span::Builtin) => Span::Builtin,
            _ => panic!("cannot merge builtin and code span"),
        }
    }
    pub(crate) const fn sp<T>(self, value: T) -> Sp<T> {
        Sp { value, span: self }
    }
    pub fn error(&self, msg: impl Into<String>) -> UiuaError {
        self.clone().sp(msg.into()).into()
    }
}

impl CodeSpan {
    pub fn merge(self, end: Self) -> Self {
        CodeSpan {
            start: self.start.min(end.start),
            end: self.end.max(end.end),
            ..self
        }
    }
}

impl PartialEq for CodeSpan {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end && self.file == other.file
    }
}

impl Eq for CodeSpan {}

impl PartialOrd for CodeSpan {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CodeSpan {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start
            .cmp(&other.start)
            .then(self.end.cmp(&other.end))
            .then(self.file.cmp(&other.file))
    }
}

impl Hash for CodeSpan {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
        self.file.hash(state);
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sp<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Sp<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Sp<U> {
        Sp {
            value: f(self.value),
            span: self.span,
        }
    }
    pub fn map_into<U>(self) -> Sp<U>
    where
        T: Into<U>,
    {
        self.map(Into::into)
    }
    pub fn as_ref(&self) -> Sp<&T> {
        Sp {
            value: &self.value,
            span: self.span.clone(),
        }
    }
    pub fn filter_map<U>(self, f: impl FnOnce(T) -> Option<U>) -> Option<Sp<U>> {
        f(self.value).map(|value| Sp {
            value,
            span: self.span,
        })
    }
}

impl<T: Clone> Sp<&T> {
    pub fn cloned(self) -> Sp<T> {
        Sp {
            value: self.value.clone(),
            span: self.span,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: ", self.span)?;
        self.value.fmt(f)
    }
}

impl<T: fmt::Display> fmt::Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span, self.value)
    }
}

impl<T: Error> Error for Sp<T> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Comment(String),
    Ident(Ident),
    Number(String),
    Char(char),
    Str(String),
    Simple(Simple),
    Glyph(Primitive),
}

impl Token {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Token::Ident(ident) => Some(ident),
            _ => None,
        }
    }
    pub fn as_number(&self) -> Option<&str> {
        match self {
            Token::Number(real) => Some(real),
            _ => None,
        }
    }
    pub fn as_char(&self) -> Option<char> {
        match self {
            Token::Char(char) => Some(*char),
            _ => None,
        }
    }
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Token::Str(string) => Some(string),
            _ => None,
        }
    }
    pub fn as_glyph(&self) -> Option<Primitive> {
        match self {
            Token::Glyph(glyph) => Some(*glyph),
            _ => None,
        }
    }
    pub fn as_comment(&self) -> Option<&str> {
        match self {
            Token::Comment(comment) => Some(comment),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Comment(comment) => write!(f, "// {comment}"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Number(real) => write!(f, "{real}"),
            Token::Char(char) => write!(f, "{char:?}"),
            Token::Str(s) => write!(f, "{s:?}"),
            Token::Simple(simple) => write!(f, "{simple}"),
            Token::Glyph(glyph) => write!(f, "{glyph}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Simple {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Underscore,
    Bang,
    Star,
    Percent,
    Equal,
    BangEqual,
    LessEqual,
    GreaterEqual,
    Backtick,
    LeftArrow,
    Newline,
    Spaces,
    TripleMinus,
    TripleTilde,
}

impl fmt::Display for Simple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Simple::OpenParen => write!(f, "("),
            Simple::CloseParen => write!(f, ")"),
            Simple::OpenCurly => write!(f, "{{"),
            Simple::CloseCurly => write!(f, "}}"),
            Simple::OpenBracket => write!(f, "["),
            Simple::CloseBracket => write!(f, "]"),
            Simple::Underscore => write!(f, "_"),
            Simple::Bang => write!(f, "!"),
            Simple::Star => write!(f, "*"),
            Simple::Percent => write!(f, "%"),
            Simple::Equal => write!(f, "="),
            Simple::BangEqual => write!(f, "!="),
            Simple::LessEqual => write!(f, "<="),
            Simple::GreaterEqual => write!(f, ">="),
            Simple::Backtick => write!(f, "`"),
            Simple::LeftArrow => write!(f, "←"),
            Simple::Newline => write!(f, "\\n"),
            Simple::Spaces => write!(f, " "),
            Simple::TripleMinus => write!(f, "---"),
            Simple::TripleTilde => write!(f, "~~~"),
        }
    }
}

impl From<Simple> for Token {
    fn from(s: Simple) -> Self {
        Self::Simple(s)
    }
}

impl From<Primitive> for Token {
    fn from(p: Primitive) -> Self {
        Self::Glyph(p)
    }
}

struct Lexer {
    input_chars: Vec<char>,
    loc: Loc,
    file: Option<Arc<Path>>,
    input: Arc<str>,
    tokens: Vec<Sp<Token>>,
}

impl Lexer {
    fn peek_char(&self) -> Option<char> {
        self.input_chars.get(self.loc.pos).copied()
    }
    fn update_loc(&mut self, c: char) {
        match c {
            '\n' => {
                self.loc.line += 1;
                self.loc.col = 1;
            }
            '\r' => {}
            _ => self.loc.col += 1,
        }
        self.loc.pos += 1;
    }
    fn next_char_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let c = *self.input_chars.get(self.loc.pos)?;
        if !f(c) {
            return None;
        }
        self.update_loc(c);
        Some(c)
    }
    fn next_char_exact(&mut self, c: char) -> bool {
        self.next_char_if(|c2| c2 == c).is_some()
    }
    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }
    fn next_chars_exact(&mut self, s: &str) -> bool {
        let start = self.loc;
        for c in s.chars() {
            if !self.next_char_exact(c) {
                self.loc = start;
                return false;
            }
        }
        true
    }
    fn make_span(&self, start: Loc, end: Loc) -> Span {
        Span::Code(CodeSpan {
            start,
            end,
            file: self.file.clone(),
            input: self.input.clone(),
        })
    }
    fn end_span(&self, start: Loc) -> Span {
        self.make_span(start, self.loc)
    }
    fn end(&mut self, token: impl Into<Token>, start: Loc) {
        self.tokens.push(Sp {
            value: token.into(),
            span: self.end_span(start),
        })
    }
    fn run(mut self) -> LexResult<Vec<Sp<Token>>> {
        use {self::Simple::*, Token::*};
        loop {
            let start = self.loc;
            let Some(c) = self.next_char() else {
                break;
            };
            match c {
                '(' => self.end(OpenParen, start),
                ')' => self.end(CloseParen, start),
                '{' => self.end(OpenCurly, start),
                '}' => self.end(CloseCurly, start),
                '[' => self.end(OpenBracket, start),
                ']' => self.end(CloseBracket, start),
                '_' => self.end(Underscore, start),
                '`' => {
                    let number = self.number('-');
                    if number.len() > 1 {
                        self.end(Number(number), start)
                    } else {
                        self.end(Backtick, start)
                    }
                }
                '¯' if self.peek_char().filter(char::is_ascii_digit).is_some() => {
                    let number = self.number('-');
                    self.end(Number(number), start)
                }
                '*' => self.end(Star, start),
                '%' => self.end(Percent, start),
                '=' => self.end(Equal, start),
                '<' if self.next_char_exact('=') => self.end(LessEqual, start),
                '>' if self.next_char_exact('=') => self.end(GreaterEqual, start),
                '!' if self.next_char_exact('=') => self.end(BangEqual, start),
                '←' => self.end(LeftArrow, start),
                // Scope delimiters
                '-' if self.next_chars_exact("--") => self.end(TripleMinus, start),
                '~' if self.next_chars_exact("~~") => self.end(TripleTilde, start),
                // Comments
                '#' => {
                    let mut comment = String::new();
                    while let Some(c) = self.next_char_if(|c| c != '\n') {
                        comment.push(c);
                    }
                    if comment.starts_with(' ') {
                        comment.remove(0);
                    }
                    self.end(Comment(comment), start)
                }
                // Characters
                '\'' => {
                    let mut escaped = false;
                    let char = match self.character(&mut escaped, '\'') {
                        Ok(Some(c)) => c,
                        Ok(None) => {
                            return Err(self.end_span(start).sp(LexError::ExpectedCharacter(None)))
                        }
                        Err(e) => return Err(self.end_span(start).sp(LexError::InvalidEscape(e))),
                    };
                    if !self.next_char_exact('\'') {
                        return Err(self
                            .end_span(start)
                            .sp(LexError::ExpectedCharacter(Some('\''))));
                    }
                    self.end(Char(char), start)
                }
                // Strings
                '"' => {
                    let mut string = String::new();
                    let mut escaped = false;
                    loop {
                        match self.character(&mut escaped, '"') {
                            Ok(Some(c)) => string.push(c),
                            Ok(None) => break,
                            Err(e) => {
                                return Err(self.end_span(start).sp(LexError::InvalidEscape(e)))
                            }
                        }
                    }
                    if !self.next_char_exact('"') {
                        return Err(self
                            .end_span(start)
                            .sp(LexError::ExpectedCharacter(Some('"'))));
                    }
                    self.end(Str(string), start)
                }
                // Identifiers and selectors
                c if is_custom_glyph(c) => self.end(Ident(c.to_string().into()), start),
                c if is_basically_alphabetic(c) => {
                    let mut ident = String::new();
                    ident.push(c);
                    while let Some(c) = self.next_char_if(is_basically_alphabetic) {
                        ident.push(c);
                    }
                    if let Some(prims) = Primitive::from_multiname(&ident) {
                        let mut start = start;
                        for (prim, frag) in prims {
                            let end = Loc {
                                col: start.col + frag.chars().count(),
                                ..start
                            };
                            self.tokens.push(Sp {
                                value: Glyph(prim),
                                span: self.make_span(start, end),
                            });
                            start = end;
                        }
                    } else {
                        self.end(Ident(ident.into()), start)
                    }
                }
                // Numbers
                c if c.is_ascii_digit() => {
                    let number = self.number(c);
                    self.end(Number(number), start)
                }
                // Newlines
                '\n' => self.end(Newline, start),
                ' ' | '\t' => {
                    while self.next_char_exact(' ') || self.next_char_exact('\t') {}
                    self.end(Spaces, start)
                }
                c if c.is_whitespace() => continue,
                c => {
                    if let Some(prim) = Primitive::from_unicode(c) {
                        self.end(Glyph(prim), start)
                    } else {
                        return Err(self.end_span(start).sp(LexError::UnexpectedChar(c)));
                    }
                }
            };
        }
        Ok(self.tokens)
    }
    fn number(&mut self, init: char) -> String {
        // Whole part
        let mut number = String::from(init);
        while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
            number.push(c);
        }
        // Fractional part
        let before_dot = self.loc;
        if self.next_char_exact('.') {
            number.push('.');
            let mut has_decimal = false;
            while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
                number.push(c);
                has_decimal = true;
            }
            if !has_decimal {
                self.loc = before_dot;
                number.pop();
            }
        }
        // Exponent
        let loc_before_e = self.loc;
        let number_before_e = number.len();
        if let Some(e) = self.next_char_if(|c| c == 'e' || c == 'E') {
            number.push(e);
            if self
                .next_char_if(|c| c == '-' || c == '`' || c == '¯')
                .is_some()
            {
                number.push('-');
            }
            let mut got_digit = false;
            while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
                number.push(c);
                got_digit = true;
            }
            if !got_digit {
                self.loc = loc_before_e;
                number.truncate(number_before_e);
            }
        }
        number
    }
    fn character(&mut self, escaped: &mut bool, escape_char: char) -> Result<Option<char>, char> {
        let Some(c) = self.next_char_if(|c| c != escape_char || *escaped) else {
            return Ok(None);
        };
        Ok(Some(if *escaped {
            *escaped = false;
            match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                '{' => '{',
                '}' => '}',
                c => return Err(c),
            }
        } else if c == '\\' {
            *escaped = true;
            return self.character(escaped, escape_char);
        } else {
            c
        }))
    }
}

pub fn is_basically_alphabetic(c: char) -> bool {
    c.is_alphabetic() && c != 'ⁿ'
}

pub fn is_custom_glyph(c: char) -> bool {
    c as u32 > 127 && !is_basically_alphabetic(c) && Primitive::from_unicode(c).is_none()
}
