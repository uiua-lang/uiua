use std::{
    cmp::Ordering,
    error::Error,
    fmt,
    hash::{Hash, Hasher},
    path::Path,
    sync::Arc,
};

use crate::{primitive::Primitive, UiuaError};

pub fn lex(input: &str, file: Option<&Path>) -> (Vec<Sp<Token>>, Vec<Sp<LexError>>) {
    Lexer {
        input_chars: input.chars().collect(),
        loc: Loc {
            char_pos: 0,
            byte_pos: 0,
            line: 1,
            col: 1,
        },
        file: file.map(Into::into),
        input: input.into(),
        tokens: Vec::new(),
        errors: Vec::new(),
    }
    .run()
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedChar(char),
    ExpectedCharacter(Option<char>),
    InvalidEscape(char),
    ExpectedNumber,
    InvalidDfnArg(char),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar(c) => write!(f, "unexpected char {c:?}"),
            LexError::ExpectedCharacter(Some(c)) => write!(f, "expected {c:?}"),
            LexError::ExpectedCharacter(None) => write!(f, "expected character"),
            LexError::InvalidEscape(c) => write!(f, "invalid escape character {c:?}"),
            LexError::ExpectedNumber => write!(f, "expected number"),
            LexError::InvalidDfnArg(c) => write!(f, "invalid dfn arg {c:?}"),
        }
    }
}

impl Error for LexError {}

pub type LexResult<T = ()> = Result<T, Sp<LexError>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
    pub char_pos: usize,
    pub byte_pos: usize,
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
            char_pos: 0,
            byte_pos: 0,
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

impl From<CodeSpan> for Span {
    fn from(span: CodeSpan) -> Self {
        Self::Code(span)
    }
}

impl Span {
    pub fn sp<T>(self, value: T) -> Sp<T, Self> {
        Sp { value, span: self }
    }
    pub fn error(&self, msg: impl Into<String>) -> UiuaError {
        self.clone().sp(msg.into()).into()
    }
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

impl CodeSpan {
    pub(crate) const fn sp<T>(self, value: T) -> Sp<T> {
        Sp { value, span: self }
    }
    pub fn merge(self, end: Self) -> Self {
        CodeSpan {
            start: self.start.min(end.start),
            end: self.end.max(end.end),
            ..self
        }
    }
    pub fn as_str(&self) -> &str {
        &self.input[self.start.byte_pos..self.end.byte_pos]
    }
    pub fn contains_line_col(&self, line: usize, col: usize) -> bool {
        if self.start.line == self.end.line {
            self.start.line == line && (self.start.col..=self.end.col).contains(&col)
        } else {
            (self.start.line..=self.end.line).contains(&line)
                && (self.start.line < line || col >= self.start.col)
                && (self.end.line > line || col <= self.end.col)
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
pub struct Sp<T, S = CodeSpan> {
    pub value: T,
    pub span: S,
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

impl<T: fmt::Debug, S: fmt::Display> fmt::Debug for Sp<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: ", self.span)?;
        self.value.fmt(f)
    }
}

impl<T: fmt::Display, S: fmt::Display> fmt::Display for Sp<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span, self.value)
    }
}

impl<T: Error> Error for Sp<T> {}

impl<T> From<Sp<T>> for Sp<T, Span> {
    fn from(value: Sp<T>) -> Self {
        Self {
            value: value.value,
            span: Span::Code(value.span),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Comment,
    Ident,
    Number,
    Char(char),
    Str(String),
    FormatStr(Vec<String>),
    MultilineString(Vec<String>),
    Simple(AsciiToken),
    Glyph(Primitive),
    DfnArg(DfnArg),
    LeftArrow,
    Newline,
    Spaces,
}

impl Token {
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
    pub fn as_format_string(&self) -> Option<Vec<String>> {
        match self {
            Token::FormatStr(frags) => Some(frags.clone()),
            _ => None,
        }
    }
    pub fn as_multiline_string(&self) -> Option<Vec<String>> {
        match self {
            Token::MultilineString(parts) => Some(parts.clone()),
            _ => None,
        }
    }
    pub fn as_glyph(&self) -> Option<Primitive> {
        match self {
            Token::Glyph(glyph) => Some(*glyph),
            _ => None,
        }
    }
    pub fn as_dfn_arg(&self) -> Option<DfnArg> {
        match self {
            Token::DfnArg(arg) => Some(*arg),
            _ => None,
        }
    }
}

/// The symbol used to denote a dfn arg
///
/// It is the combining ring below character.
pub const DFN_ARG_SYMBOL: char = '\u{325}';

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DfnArg(pub u8);

impl fmt::Display for DfnArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 >= 26 {
            return write!(f, "invalid dfn arg ({})", self.0);
        }
        write!(f, "{}{DFN_ARG_SYMBOL}", (b'a' + self.0) as char)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsciiToken {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Underscore,
    Colon,
    Star,
    Percent,
    Equal,
    BangEqual,
    LessEqual,
    GreaterEqual,
    Backtick,
    TripleMinus,
    TripleTilde,
}

impl fmt::Display for AsciiToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsciiToken::OpenParen => write!(f, "("),
            AsciiToken::CloseParen => write!(f, ")"),
            AsciiToken::OpenCurly => write!(f, "{{"),
            AsciiToken::CloseCurly => write!(f, "}}"),
            AsciiToken::OpenBracket => write!(f, "["),
            AsciiToken::CloseBracket => write!(f, "]"),
            AsciiToken::Underscore => write!(f, "_"),
            AsciiToken::Colon => write!(f, ":"),
            AsciiToken::Star => write!(f, "*"),
            AsciiToken::Percent => write!(f, "%"),
            AsciiToken::Equal => write!(f, "="),
            AsciiToken::BangEqual => write!(f, "!="),
            AsciiToken::LessEqual => write!(f, "<="),
            AsciiToken::GreaterEqual => write!(f, ">="),
            AsciiToken::Backtick => write!(f, "`"),
            AsciiToken::TripleMinus => write!(f, "---"),
            AsciiToken::TripleTilde => write!(f, "~~~"),
        }
    }
}

impl From<AsciiToken> for Token {
    fn from(s: AsciiToken) -> Self {
        Self::Simple(s)
    }
}

impl From<Primitive> for Token {
    fn from(p: Primitive) -> Self {
        Self::Glyph(p)
    }
}

impl From<DfnArg> for Token {
    fn from(d: DfnArg) -> Self {
        Self::DfnArg(d)
    }
}

struct Lexer {
    input_chars: Vec<char>,
    loc: Loc,
    file: Option<Arc<Path>>,
    input: Arc<str>,
    tokens: Vec<Sp<Token>>,
    errors: Vec<Sp<LexError>>,
}

impl Lexer {
    fn peek_char(&self) -> Option<char> {
        self.input_chars.get(self.loc.char_pos).copied()
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
        self.loc.char_pos += 1;
        self.loc.byte_pos += c.len_utf8();
    }
    fn next_char_if(&mut self, f: impl Fn(char) -> bool) -> Option<char> {
        let c = *self.input_chars.get(self.loc.char_pos)?;
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
    fn make_span(&self, start: Loc, end: Loc) -> CodeSpan {
        CodeSpan {
            start,
            end,
            file: self.file.clone(),
            input: self.input.clone(),
        }
    }
    fn end_span(&self, start: Loc) -> CodeSpan {
        assert!(self.loc.char_pos > start.char_pos, "empty span");
        self.make_span(start, self.loc)
    }
    fn end(&mut self, token: impl Into<Token>, start: Loc) {
        self.tokens.push(Sp {
            value: token.into(),
            span: self.end_span(start),
        })
    }
    fn run(mut self) -> (Vec<Sp<Token>>, Vec<Sp<LexError>>) {
        use {self::AsciiToken::*, Token::*};
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
                ':' => self.end(Colon, start),
                '`' => {
                    if self.number('-') {
                        self.end(Number, start)
                    } else {
                        self.end(Backtick, start)
                    }
                }
                '¯' if self.peek_char().filter(char::is_ascii_digit).is_some() => {
                    self.number('-');
                    self.end(Number, start)
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
                '~' => {
                    if let Some(c) = self.next_char() {
                        if c.is_ascii_lowercase() {
                            self.end(self::DfnArg(c as u8 - b'a'), start);
                        } else {
                            self.errors.push(
                                self.end_span(start)
                                    .sp(LexError::ExpectedCharacter(Some(c))),
                            );
                        }
                    } else {
                        self.errors
                            .push(self.end_span(start).sp(LexError::ExpectedCharacter(None)));
                    }
                }
                // Comments
                '#' => {
                    let mut comment = String::new();
                    while let Some(c) = self.next_char_if(|c| c != '\n') {
                        comment.push(c);
                    }
                    if comment.starts_with(' ') {
                        comment.remove(0);
                    }
                    self.end(Comment, start)
                }
                // Characters
                '@' => {
                    let mut escaped = false;
                    let char = match self.character(&mut escaped, None) {
                        Ok(Some(c)) => c,
                        Ok(None) => {
                            self.errors
                                .push(self.end_span(start).sp(LexError::ExpectedCharacter(None)));
                            continue;
                        }
                        Err(e) => {
                            self.errors
                                .push(self.end_span(start).sp(LexError::InvalidEscape(e)));
                            continue;
                        }
                    };
                    self.end(Char(char), start)
                }
                // Strings
                '"' | '$' => {
                    let format = c == '$';
                    if format && self.next_char_exact(' ') {
                        // Multiline strings
                        let mut start = start;
                        loop {
                            let inner = self.parse_string_contents(start, None);
                            let string = parse_format_fragments(&inner);
                            self.end(MultilineString(string), start);
                            let checkpoint = self.loc;
                            while self.next_char_exact('\r') {}
                            if self.next_char_exact('\n') {
                                while self
                                    .next_char_if(|c| c.is_whitespace() && c != '\n')
                                    .is_some()
                                {}
                                start = self.loc;
                                if self.next_chars_exact("$ ") {
                                    continue;
                                }
                            }
                            self.loc = checkpoint;
                            break;
                        }
                        continue;
                    }
                    if format && !self.next_char_exact('"') {
                        self.errors.push(
                            self.end_span(start)
                                .sp(LexError::ExpectedCharacter(Some('"'))),
                        );
                    }
                    // Single-line strings
                    let inner = self.parse_string_contents(start, Some('"'));
                    if !self.next_char_exact('"') {
                        self.errors.push(
                            self.end_span(start)
                                .sp(LexError::ExpectedCharacter(Some('"'))),
                        );
                    }
                    if format {
                        let frags = parse_format_fragments(&inner);
                        self.end(FormatStr(frags), start)
                    } else {
                        self.end(Str(inner), start)
                    }
                }
                // Identifiers and selectors
                c if is_custom_glyph(c) => self.end(Ident, start),
                c if is_basically_alphabetic(c) => {
                    let mut ident = String::new();
                    ident.push(c);
                    let mut dfn_arg = None;
                    // Collect characters
                    while let Some(c) = self.next_char_if(is_basically_alphabetic) {
                        ident.push(c);
                    }
                    // Try to parse dfn arg
                    if self.next_char_exact(DFN_ARG_SYMBOL) {
                        if let Some(c) = ident.pop() {
                            if c.is_ascii_lowercase() {
                                dfn_arg = Some(c as u8 - b'a');
                            } else {
                                self.errors
                                    .push(self.end_span(start).sp(LexError::InvalidDfnArg(c)));
                            }
                        } else {
                            self.errors
                                .push(self.end_span(start).sp(LexError::ExpectedCharacter(None)));
                        }
                    }
                    if dfn_arg.is_some() {
                        self.loc.byte_pos -= 1 + DFN_ARG_SYMBOL.len_utf8();
                        self.loc.char_pos -= 2;
                        self.loc.col -= 2;
                    }
                    if !ident.is_empty() {
                        // Try to parse as primitives
                        if let Some(prims) = Primitive::from_format_name_multi(&ident) {
                            let mut start = start;
                            for (prim, frag) in prims {
                                let end = Loc {
                                    col: start.col + frag.chars().count(),
                                    char_pos: start.char_pos + frag.chars().count(),
                                    byte_pos: start.byte_pos + frag.len(),
                                    ..start
                                };
                                self.tokens.push(Sp {
                                    value: Glyph(prim),
                                    span: self.make_span(start, end),
                                });
                                start = end;
                            }
                        } else {
                            // Lone ident
                            self.end(Ident, start)
                        }
                    }
                    if let Some(dfn_arg) = dfn_arg {
                        let start = self.loc;
                        self.loc.byte_pos += 1 + DFN_ARG_SYMBOL.len_utf8();
                        self.loc.char_pos += 2;
                        self.loc.col += 2;
                        self.end(self::DfnArg(dfn_arg), start);
                    }
                }
                // Numbers
                c if c.is_ascii_digit() => {
                    self.number(c);
                    self.end(Number, start)
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
                    } else if !self
                        .errors
                        .iter()
                        .last()
                        .map_or(true, |e| matches!(e.value, LexError::UnexpectedChar(_)))
                    {
                        self.errors
                            .push(self.end_span(start).sp(LexError::UnexpectedChar(c)));
                    }
                }
            };
        }
        (self.tokens, self.errors)
    }
    fn number(&mut self, init: char) -> bool {
        // Whole part
        let mut got_digit = false;
        while self.next_char_if(|c| c.is_ascii_digit()).is_some() {
            got_digit = true;
        }
        if !init.is_ascii_digit() && !got_digit {
            return false;
        }
        // Fractional part
        let before_dot = self.loc;
        if self.next_char_exact('.') {
            let mut has_decimal = false;
            while self.next_char_if(|c| c.is_ascii_digit()).is_some() {
                has_decimal = true;
            }
            if !has_decimal {
                self.loc = before_dot;
            }
        }
        // Exponent
        let loc_before_e = self.loc;
        if self.next_char_if(|c| c == 'e' || c == 'E').is_some() {
            self.next_char_if(|c| c == '-' || c == '`' || c == '¯');
            let mut got_digit = false;
            while self.next_char_if(|c| c.is_ascii_digit()).is_some() {
                got_digit = true;
            }
            if !got_digit {
                self.loc = loc_before_e;
            }
        }
        true
    }
    fn character(
        &mut self,
        escaped: &mut bool,
        escape_char: Option<char>,
    ) -> Result<Option<char>, char> {
        let Some(c) = self.next_char_if(|c| !"\r\n".contains(c) && (Some(c) != escape_char || *escaped)) else {
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
                '_' => char::MAX,
                c => return Err(c),
            }
        } else if c == '\\' {
            *escaped = true;
            return self.character(escaped, escape_char);
        } else {
            c
        }))
    }
    fn parse_string_contents(&mut self, start: Loc, escape_char: Option<char>) -> String {
        let mut string = String::new();
        let mut escaped = false;
        loop {
            match self.character(&mut escaped, escape_char) {
                Ok(Some(c)) => string.push(c),
                Ok(None) => break,
                Err(e) => {
                    self.errors
                        .push(self.end_span(start).sp(LexError::InvalidEscape(e)));
                }
            }
        }
        string
    }
}

fn parse_format_fragments(s: &str) -> Vec<String> {
    let mut frags: Vec<String> = Vec::new();
    let mut curr = String::new();
    for c in s.chars() {
        match c {
            '_' => {
                frags.push(curr);
                curr = String::new();
            }
            char::MAX => curr.push('_'),
            c => curr.push(c),
        }
    }
    frags.push(curr);
    frags
}

pub fn is_basically_alphabetic(c: char) -> bool {
    c.is_alphabetic() && !"ⁿₙηπτ".contains(c)
}

pub fn is_custom_glyph(c: char) -> bool {
    c as u32 > 127 && !is_basically_alphabetic(c) && Primitive::from_unicode(c).is_none()
}
