use std::{error::Error, fmt, path::Path};

use crate::{
    ast::*,
    function::FunctionId,
    lex::{Simple::*, *},
    Ident,
};

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<Box<Sp<Token>>>),
    BlockMustEndWithExpr,
}

#[derive(Debug)]
pub enum Expectation {
    Ident,
    Block,
    Type,
    Expr,
    Pattern,
    Eof,
    FunctionBody,
    Parameter,
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
            Expectation::Expr => write!(f, "expression"),
            Expectation::Pattern => write!(f, "pattern"),
            Expectation::Eof => write!(f, "end of file"),
            Expectation::FunctionBody => write!(f, "function body"),
            Expectation::Parameter => write!(f, "parameter"),
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
            ParseError::BlockMustEndWithExpr => {
                write!(f, "block must end with an expression")
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
        let item = if let Some(item) = self.try_let_or_fucntion_def()? {
            item
        } else if let Some(r#const) = self.try_const()? {
            Item::Const(r#const)
        } else if self.try_exact(Keyword::Do).is_some() {
            Item::Expr(self.expr()?)
        } else {
            return Ok(None);
        };
        Ok(Some(item))
    }
    fn block(&mut self) -> ParseResult<Block> {
        let mut items = Vec::new();
        while let Some(item) = self.try_item()? {
            items.push(item);
            self.expect(Newline)?;
        }
        let expr = self.expr()?;
        Ok(Block { items, expr })
    }
    fn try_let_or_fucntion_def(&mut self) -> ParseResult<Option<Item>> {
        // Let
        if self.try_exact(Keyword::Let).is_none() {
            return Ok(None);
        };
        // Pattern or function name and params
        Ok(Some(if let Some(ident) = self.try_ident() {
            let mut params = Vec::new();
            while let Some(param) = self.try_param() {
                params.push(param);
            }
            if params.is_empty() {
                Item::Let(self.finish_binding(ident.map(Pattern::Ident))?)
            } else {
                self.expect(Equal)?;
                let body = self.block()?;
                let id = FunctionId::Named(ident.value);
                Item::FunctionDef(FunctionDef {
                    name: ident,
                    func: Func { id, params, body },
                })
            }
        } else if let Some(items) = self.try_surrounded_list(PARENS, Self::try_pattern)? {
            Item::Let(self.finish_binding(items.map(Pattern::List))?)
        } else {
            return Ok(None);
        }))
    }
    fn finish_binding(&mut self, pattern: Sp<Pattern>) -> ParseResult<Let> {
        self.expect(Equal)?;
        let expr = self.expr()?;
        Ok(Let { pattern, expr })
    }
    fn try_const(&mut self) -> ParseResult<Option<Const>> {
        // Const
        if self.try_exact(Keyword::Const).is_none() {
            return Ok(None);
        };
        // Pattern
        let name = self.ident()?;
        // Expression
        self.expect(Equal)?;
        let expr = self.expr()?;
        Ok(Some(Const { name, expr }))
    }
    fn try_pattern(&mut self) -> ParseResult<Option<Sp<Pattern>>> {
        Ok(Some(if let Some(ident) = self.try_ident() {
            ident.map(Pattern::Ident)
        } else if let Some(items) = self.try_surrounded_list(PARENS, Self::try_pattern)? {
            items.map(Pattern::List)
        } else {
            return Ok(None);
        }))
    }
    fn try_ident(&mut self) -> Option<Sp<Ident>> {
        self.next_token_map(|token| token.as_ident().copied())
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

struct BinExprDef<'a> {
    ops: &'a [(Token, BinOp)],
    associativity: Associativity,
    child: Option<&'a Self>,
}

enum Associativity {
    Left,
    Right,
}

static TOP_BIN_EXPR: BinExprDef = BinExprDef {
    ops: &[(Token::Simple(BackPipe), BinOp::BackPipe)],
    associativity: Associativity::Right,
    child: Some(&BinExprDef {
        ops: &[(Token::Simple(Pipe), BinOp::Pipe)],
        associativity: Associativity::Left,
        child: Some(&BinExprDef {
            ops: &[
                (Token::Simple(LessGreater), BinOp::Slf),
                (Token::Simple(GreaterLess), BinOp::Flip),
            ],
            associativity: Associativity::Left,
            child: Some(&BinExprDef {
                ops: &[
                    (Token::Simple(Slash), BinOp::LeftLeaf),
                    (Token::Simple(DoubleSlash), BinOp::LeftTree),
                ],
                associativity: Associativity::Right,
                child: Some(&BinExprDef {
                    ops: &[
                        (Token::Simple(BackSlash), BinOp::RightLeaf),
                        (Token::Simple(DoubleBackSlash), BinOp::RightTree),
                    ],
                    associativity: Associativity::Left,
                    child: Some(&BinExprDef {
                        ops: &[
                            (Token::Simple(Period), BinOp::Compose),
                            (Token::Simple(Period3), BinOp::BlackBird),
                        ],
                        associativity: Associativity::Left,
                        child: Some(&BinExprDef {
                            associativity: Associativity::Left,
                            ops: &[
                                (Token::Simple(Equal), BinOp::Eq),
                                (Token::Simple(NotEqual), BinOp::Ne),
                                (Token::Simple(Less), BinOp::Lt),
                                (Token::Simple(LessEqual), BinOp::Le),
                                (Token::Simple(Greater), BinOp::Gt),
                                (Token::Simple(GreaterEqual), BinOp::Ge),
                            ],
                            child: Some(&BinExprDef {
                                associativity: Associativity::Left,
                                ops: &[
                                    (Token::Simple(Plus), BinOp::Add),
                                    (Token::Simple(Minus), BinOp::Sub),
                                ],
                                child: Some(&BinExprDef {
                                    associativity: Associativity::Left,
                                    ops: &[
                                        (Token::Simple(Star), BinOp::Mul),
                                        (Token::Simple(Percent), BinOp::Div),
                                    ],
                                    child: Some(&BinExprDef {
                                        associativity: Associativity::Left,
                                        ops: &[
                                            (Token::Simple(BarMinus), BinOp::Right),
                                            (Token::Simple(MinusBar), BinOp::Left),
                                        ],
                                        child: None,
                                    }),
                                }),
                            }),
                        }),
                    }),
                }),
            }),
        }),
    }),
};

impl Parser {
    fn expr(&mut self) -> ParseResult<Sp<Expr>> {
        self.expect_expr(Self::try_expr)
    }
    fn expect_expr(
        &mut self,
        f: impl FnOnce(&mut Self) -> ParseResult<Option<Sp<Expr>>>,
    ) -> ParseResult<Sp<Expr>> {
        f(self)?.ok_or_else(|| self.expected([Expectation::Expr]))
    }
    fn try_expr(&mut self) -> ParseResult<Option<Sp<Expr>>> {
        self.try_bin_expr_def(&TOP_BIN_EXPR)
    }
    fn try_bin_expr_def(&mut self, def: &BinExprDef) -> ParseResult<Option<Sp<Expr>>> {
        let right_leaf = |this: &mut Self| {
            if let Some(child) = def.child {
                match def.associativity {
                    Associativity::Left => this.try_bin_expr_def(child),
                    Associativity::Right => this.try_bin_expr_def(def),
                }
            } else {
                this.try_call()
            }
        };
        let mut expr_span = None;
        let left = if let Some(child) = def.child {
            self.try_bin_expr_def(child)?
        } else {
            self.try_call()?
        };
        let mut expr = if let Some(expr) = left {
            expr_span = Some(expr.span);
            expr.value
        } else {
            Expr::Placeholder
        };
        // Repeatedly try to parse the next operator and right operand at this precedence level
        'rhs: loop {
            // Try each operator
            for (token, op) in def.ops {
                if let Some(op_span) = self.try_exact(token.clone()) {
                    // Span the op
                    let op = op_span.clone().sp(*op);
                    // Set the left span if it was a placeholder
                    let left_span = expr_span.unwrap_or_else(|| op_span.clone());
                    // Get the right side
                    let mut must_break = false;
                    let right = if let Some(right) = right_leaf(self)? {
                        right
                    } else {
                        must_break = true;
                        op_span.sp(Expr::Placeholder)
                    };
                    // Span the new whole expression
                    expr_span = Some(left_span.clone().merge(right.span.clone()));
                    // Set the new expression
                    expr = Expr::Bin(Box::new(BinExpr {
                        op,
                        left: left_span.sp(expr),
                        right,
                    }));
                    if must_break {
                        break 'rhs;
                    } else {
                        continue 'rhs;
                    }
                }
            }
            break;
        }
        Ok(expr_span.map(|span| span.sp(expr)))
    }
    fn try_call(&mut self) -> ParseResult<Option<Sp<Expr>>> {
        let Some(mut expr) = self.try_term()? else {
            return Ok(None);
        };
        while let Some(arg) = self.try_term()? {
            let span = expr.span.clone().merge(arg.span.clone());
            let call = span.sp(Expr::Call(Box::new(CallExpr { func: expr, arg })));
            expr = call;
        }
        Ok(Some(expr))
    }
    fn try_term(&mut self) -> ParseResult<Option<Sp<Expr>>> {
        Ok(Some(if let Some(ident) = self.try_ident() {
            ident.map(Expr::Ident)
        } else if let Some(r) = self.next_token_map(Token::as_number) {
            r.map(Into::into).map(Expr::Real)
        } else if let Some(c) = self.next_token_map(Token::as_char) {
            c.map(Into::into).map(Expr::Char)
        } else if let Some(s) = self.next_token_map(Token::as_string) {
            s.map(Into::into).map(Expr::String)
        } else if let Some(s) = self.next_token_map(Token::as_format_string) {
            s.map(Into::into).map(Expr::FormatString)
        } else if let Some(start) = self.try_exact(OpenParen) {
            let inner = self.try_expr()?;
            let end = self.expect(CloseParen)?;
            let span = start.merge(end);
            span.sp(if let Some(expr) = inner {
                Expr::Parened(expr.into())
            } else {
                Expr::Unit
            })
        } else if let Some(items) = self.try_surrounded_list(BRACKETS, Self::try_expr)? {
            items.map(Expr::Array)
        } else if let Some(items) = self.try_surrounded_list(CURLIES, Self::try_expr)? {
            items.map(Expr::List)
        } else if let Some(func) = self.try_func()? {
            func.map(Box::new).map(Expr::Func)
        } else {
            return Ok(None);
        }))
    }
    fn try_func(&mut self) -> ParseResult<Option<Sp<Func>>> {
        let mut params = Vec::new();
        let mut start = None;
        while let Some(span) = self.try_exact(Bar) {
            start.get_or_insert(span);
            params.push(self.expect_param()?);
        }
        let Some(start) = start else {
            return Ok(None);
        };
        self.try_exact(Newline);
        // Body
        let body = self.block()?;
        let span = start.clone().merge(body.expr.span.clone());
        let id = FunctionId::Anonymous(start);
        Ok(Some(span.sp(Func { id, params, body })))
    }
    fn try_param(&mut self) -> Option<Sp<Ident>> {
        self.try_ident()
    }
    fn expect_param(&mut self) -> ParseResult<Sp<Ident>> {
        self.try_param()
            .ok_or_else(|| self.expected([Expectation::Parameter]))
    }
}
