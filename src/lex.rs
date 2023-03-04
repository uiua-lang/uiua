use std::{error::Error, fmt, path::Path, sync::Arc};

use enum_iterator::{all, Sequence};

use crate::{Ident, RuntimeError};

pub fn lex(input: &str, file: &Path) -> LexResult<Vec<Sp<Token>>> {
    let mut lexer = Lexer::new(input, file);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next_token()? {
        tokens.push(token);
    }

    Ok(tokens)
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedChar(char),
    Bang,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar(c) => write!(f, "unexpected char {c:?}"),
            LexError::Bang => {
                write!(f, " unexpected char '!', maybe you meant 'not'?")
            }
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
pub struct Span {
    pub start: Loc,
    pub end: Loc,
    pub file: Option<Arc<Path>>,
    pub input: Arc<str>,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file = if let Some(file) = &self.file {
            file.to_string_lossy()
        } else {
            "<builtin>".into()
        };
        write!(f, "{}:{}", file, self.start)
    }
}

impl Span {
    pub fn merge(self, end: Self) -> Self {
        Self {
            start: self.start.min(end.start),
            end: self.end.max(end.end),
            ..self
        }
    }
    pub(crate) const fn sp<T>(self, value: T) -> Sp<T> {
        Sp { value, span: self }
    }
    pub fn builtin() -> Self {
        Self {
            start: Loc::default(),
            end: Loc::default(),
            file: None,
            input: "".into(),
        }
    }
    pub fn error(&self, msg: impl Into<String>) -> RuntimeError {
        self.clone().sp(msg.into())
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
    DocComment(String),
    Ident(Ident),
    Int(String),
    Real(String),
    Keyword(Keyword),
    Simple(Simple),
}

impl Token {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Token::Ident(ident) => Some(ident),
            _ => None,
        }
    }
    pub fn as_int(&self) -> Option<&str> {
        match self {
            Token::Int(int) => Some(int),
            _ => None,
        }
    }
    pub fn as_real(&self) -> Option<&str> {
        match self {
            Token::Real(real) => Some(real),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Comment(comment) => write!(f, "// {comment}"),
            Token::DocComment(comment) => write!(f, "/// {comment}"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Int(int) => write!(f, "{int}"),
            Token::Real(real) => write!(f, "{real}"),
            Token::Keyword(keyword) => write!(f, "{keyword}"),
            Token::Simple(simple) => write!(f, "{simple}"),
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
    Comma,
    Period,
    Colon,
    SemiColon,
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl fmt::Display for Simple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Simple::OpenParen => "(",
                Simple::CloseParen => ")",
                Simple::OpenCurly => "{",
                Simple::CloseCurly => "}",
                Simple::OpenBracket => "[",
                Simple::CloseBracket => "]",
                Simple::Underscore => "_",
                Simple::Comma => ",",
                Simple::Period => ".",
                Simple::Colon => ":",
                Simple::SemiColon => ";",
                Simple::Plus => "+",
                Simple::Minus => "-",
                Simple::Star => "*",
                Simple::Slash => "/",
                Simple::Equal => "=",
                Simple::NotEqual => "!=",
                Simple::Less => "<",
                Simple::LessEqual => "<=",
                Simple::Greater => ">",
                Simple::GreaterEqual => ">=",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Sequence)]
pub enum Keyword {
    Let,
    Fn,
    If,
    Then,
    Else,
    And,
    Or,
    True,
    False,
    Not,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format!("{self:?}").to_lowercase())
    }
}

impl From<Keyword> for Token {
    fn from(k: Keyword) -> Self {
        Self::Keyword(k)
    }
}

impl From<Simple> for Token {
    fn from(s: Simple) -> Self {
        Self::Simple(s)
    }
}

struct Lexer {
    input_chars: Vec<char>,
    loc: Loc,
    file: Arc<Path>,
    input: Arc<str>,
}

impl Lexer {
    fn new(input: &str, file: &Path) -> Self {
        Self {
            input_chars: input.chars().collect(),
            loc: Loc {
                pos: 0,
                line: 1,
                col: 1,
            },
            file: file.into(),
            input: input.into(),
        }
    }
    fn next_char_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let c = *self.input_chars.get(self.loc.pos)?;
        if !f(c) {
            return None;
        }
        match c {
            '\n' => {
                self.loc.line += 1;
                self.loc.col = 1;
            }
            '\r' => {}
            _ => self.loc.col += 1,
        }
        self.loc.pos += 1;
        Some(c)
    }
    fn next_char_exact(&mut self, c: char) -> bool {
        self.next_char_if(|c2| c2 == c).is_some()
    }
    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }
    fn end_span(&self, start: Loc) -> Span {
        Span {
            start,
            end: self.loc,
            file: Some(self.file.clone()),
            input: self.input.clone(),
        }
    }
    fn end(&self, token: impl Into<Token>, start: Loc) -> LexResult<Option<Sp<Token>>> {
        Ok(Some(Sp {
            value: token.into(),
            span: self.end_span(start),
        }))
    }
    fn switch_next<T, U, const N: usize>(
        &mut self,
        a: T,
        others: [(char, U); N],
        start: Loc,
    ) -> LexResult<Option<Sp<Token>>>
    where
        T: Into<Token>,
        U: Into<Token>,
    {
        let token = others
            .into_iter()
            .find(|(c, _)| self.next_char_exact(*c))
            .map(|(_, t)| t.into())
            .unwrap_or(a.into());
        self.end(token, start)
    }
    fn next_token(&mut self) -> LexResult<Option<Sp<Token>>> {
        use {self::Simple::*, Token::*};
        loop {
            let start = self.loc;
            let Some(c) = self.next_char() else {
                break Ok(None);
            };
            match c {
                '(' => return self.end(OpenParen, start),
                ')' => return self.end(CloseParen, start),
                '{' => return self.end(OpenCurly, start),
                '}' => return self.end(CloseCurly, start),
                '[' => return self.end(OpenBracket, start),
                ']' => return self.end(CloseBracket, start),
                '.' => return self.end(Period, start),
                ':' => return self.end(Colon, start),
                ';' => return self.end(SemiColon, start),
                ',' => return self.end(Comma, start),
                '+' => return self.end(Plus, start),
                '-' => return self.end(Minus, start),
                '*' => return self.end(Star, start),
                '=' => return self.end(Equal, start),
                '<' => return self.switch_next(Less, [('=', LessEqual)], start),
                '>' => return self.switch_next(Greater, [('=', GreaterEqual)], start),
                '!' => {
                    return if self.next_char_exact('=') {
                        self.end(NotEqual, start)
                    } else {
                        Err(self.end_span(start).sp(LexError::Bang))
                    }
                }
                // Division and comments
                '/' => {
                    if self.next_char_exact('/') {
                        // Comments
                        let token = if self.next_char_exact('/') {
                            DocComment
                        } else {
                            Comment
                        };
                        let mut comment = String::new();
                        while let Some(c) = self.next_char_if(|c| c != '\n') {
                            comment.push(c);
                        }
                        let end = self.loc;
                        self.next_char();
                        return Ok(Some(Sp {
                            value: token(comment),
                            span: Span {
                                start,
                                end,
                                file: Some(self.file.clone()),
                                input: self.input.clone(),
                            },
                        }));
                    } else {
                        // Division
                        return self.end(Slash, start);
                    }
                }
                // Identifiers, keywords, and underscore
                c if is_ident_start(c) => {
                    let mut ident = String::new();
                    ident.push(c);
                    while let Some(c) = self.next_char_if(is_ident) {
                        ident.push(c);
                    }
                    let token = if let Some(keyword) =
                        all::<self::Keyword>().find(|k| format!("{k:?}").to_lowercase() == ident)
                    {
                        Keyword(keyword)
                    } else if ident == "_" {
                        Simple(Underscore)
                    } else {
                        Ident(ascend::static_str(&ident).into())
                    };
                    return self.end(token, start);
                }
                // Numbers
                c if c.is_ascii_digit() => {
                    // Whole part
                    let mut number = String::from(c);
                    while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
                        number.push(c);
                    }
                    // Fractional part
                    let before_dot = self.loc;
                    if self.next_char_exact('.') {
                        if self.next_char_exact('.') {
                            self.loc = before_dot;
                        } else {
                            number.push('.');
                            while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
                                number.push(c);
                            }
                        }
                    }
                    // Exponent
                    if let Some(e) = self.next_char_if(|c| c == 'e' || c == 'E') {
                        number.push(e);
                        if let Some(sign) = self.next_char_if(|c| c == '-' || c == '+') {
                            number.push(sign);
                        }
                        while let Some(c) = self.next_char_if(|c| c.is_ascii_digit()) {
                            number.push(c);
                        }
                    }
                    let token = if number.contains(['.', 'e', 'E']) {
                        Real
                    } else {
                        Int
                    }(number);
                    return self.end(token, start);
                }
                c if c.is_whitespace() => {}
                c => return Err(self.end_span(start).sp(LexError::UnexpectedChar(c))),
            }
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
