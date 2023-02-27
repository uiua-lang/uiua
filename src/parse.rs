use std::{error::Error, fmt, path::Path};

use crate::{
    ast::*,
    lex::{Simple::*, *},
};

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    Expected(Vec<Expectation>, Option<Box<Sp<Token>>>),
}

#[derive(Debug)]
pub enum Expectation {
    Ident,
    Block,
    Type,
    Expr(&'static str),
    Simple(Simple),
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expectation::Ident => write!(f, "identifier"),
            Expectation::Block => write!(f, "block"),
            Expectation::Type => write!(f, "type"),
            Expectation::Expr(kind) => write!(f, "{kind} expression"),
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
                    write!(f, "{exp}")?;
                }
                if let Some(found) = found {
                    write!(f, ", found {found:?}")?;
                }
                Ok(())
            }
        }
    }
}

impl Error for ParseError {}

pub type ParseResult<T = ()> = Result<T, Sp<ParseError>>;

pub fn parse(input: &str, path: &Path) -> Result<Vec<Item>, Vec<Sp<ParseError>>> {
    let tokens = lex(input, path).map_err(|e| vec![e.map(ParseError::Lex)])?;
    let mut items = Vec::new();
    let mut parser = Parser {
        tokens,
        index: 0,
        errors: Vec::new(),
    };
    while let Some(item) = parser.try_item().map_err(|e| vec![e])? {
        items.push(item);
    }
    if parser.errors.is_empty() {
        Ok(items)
    } else {
        Err(parser.errors)
    }
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
            span.start = span.end;
            if self.tokens.len() > span.end.pos {
                span.end.pos += 1;
                span.end.col += 1;
            }
            span
        }
    }
    fn expect(&mut self, simple: Simple) -> ParseResult<Span> {
        self.try_exact(simple)
            .ok_or_else(|| self.expected([Expectation::Simple(simple)]))
    }
    fn expected(&self, expectations: impl IntoIterator<Item = Expectation>) -> Sp<ParseError> {
        self.last_span().sp(ParseError::Expected(
            expectations.into_iter().collect(),
            self.tokens.get(self.index).cloned().map(Box::new),
        ))
    }
    fn try_item(&mut self) -> ParseResult<Option<Item>> {
        Ok(if let Some(def) = self.try_function_def()? {
            Some(Item::FunctionDef(def))
        } else if let Some(expr) = self.try_expr()? {
            self.expect(SemiColon)?;
            Some(Item::Expr(expr))
        } else {
            None
        })
    }
    pub fn try_function_def(&mut self) -> ParseResult<Option<FunctionDef>> {
        // Documentation comments
        let mut doc: Option<Sp<String>> = None;
        while let Some(line) = self.next_token_map(|token| {
            if let Token::DocComment(line) = token {
                Some(line)
            } else {
                None
            }
        }) {
            if let Some(doc) = &mut doc {
                doc.value.push_str(line.value);
                doc.span.end = line.span.end;
            } else {
                doc = Some(line.cloned());
            }
        }
        // Keyword
        if self.try_exact(Keyword::Fn).is_none() {
            return Ok(None);
        };
        // Name
        let name = self.ident()?;
        // Parameters
        self.expect(OpenParen)?;
        let mut params = Vec::new();
        while let Some(param) = self.try_param()? {
            params.push(param);
            if self.try_exact(Comma).is_none() {
                break;
            }
        }
        self.expect(CloseParen)?;
        // Body
        let body = self.block()?;
        Ok(Some(FunctionDef {
            doc,
            name,
            params,
            body,
        }))
    }
    fn try_block(&mut self) -> ParseResult<Option<Block>> {
        if self.try_exact(OpenCurly).is_none() {
            return Ok(None);
        }
        let mut exprs = Vec::new();
        while let Some(expr) = self.try_expr()? {
            exprs.push(expr);
        }
        self.expect(CloseCurly)?;
        Ok(Some(Block { exprs }))
    }
    fn block(&mut self) -> ParseResult<Block> {
        self.try_block()?
            .ok_or_else(|| self.expected([Expectation::Block]))
    }
    fn try_param(&mut self) -> ParseResult<Option<Param>> {
        let Some(name) = self.try_ident() else {
            return Ok(None);
        };
        self.expect(Colon)?;
        let ty = self.ty()?;
        Ok(Some(Param { name, ty }))
    }
    fn try_ty(&mut self) -> ParseResult<Option<Sp<Type>>> {
        Ok(if let Some(ident) = self.try_ident() {
            // Named type
            Some(ident.map(Type::Ident))
        } else if self.try_exact(OpenBracket).is_some() {
            // Array
            let inner = self.ty()?;
            self.expect(CloseBracket)?;
            Some(inner.map(Box::new).map(Type::Array))
        } else if let Some(start) = self.try_exact(OpenParen) {
            // Tuple
            let mut tys = Vec::new();
            while let Some(ty) = self.try_ty()? {
                tys.push(ty);
                if self.try_exact(Comma).is_none() {
                    break;
                }
            }
            let end = self.expect(CloseParen)?;
            let span = start.merge(end);
            Some(span.sp(Type::Tuple(tys)))
        } else {
            None
        })
    }
    fn ty(&mut self) -> ParseResult<Sp<Type>> {
        self.try_ty()?
            .ok_or_else(|| self.expected([Expectation::Type]))
    }
    fn try_ident(&mut self) -> Option<Sp<String>> {
        self.next_token_map(|token| {
            if let Token::Ident(ident) = token {
                Some(ident.clone())
            } else {
                None
            }
        })
    }
    fn ident(&mut self) -> ParseResult<Sp<String>> {
        self.try_ident()
            .ok_or_else(|| self.expected([Expectation::Ident]))
    }
}

struct BinExprDef<'a> {
    kind: &'static str,
    ops: &'a [(Token, BinOp)],
    child: Option<&'a Self>,
}

static BIN_EXPR_OR: BinExprDef = BinExprDef {
    kind: "or",
    ops: &[(Token::Keyword(Keyword::Or), BinOp::Or)],
    child: Some(&BinExprDef {
        kind: "and",
        ops: &[(Token::Keyword(Keyword::And), BinOp::And)],
        child: Some(&BinExprDef {
            kind: "comparison",
            ops: &[
                (Token::Simple(Equal), BinOp::Eq),
                (Token::Simple(NotEqual), BinOp::Ne),
                (Token::Simple(Less), BinOp::Lt),
                (Token::Simple(LessEqual), BinOp::Le),
                (Token::Simple(Greater), BinOp::Gt),
                (Token::Simple(GreaterEqual), BinOp::Ge),
            ],
            child: Some(&BinExprDef {
                kind: "add-sub",
                ops: &[
                    (Token::Simple(Plus), BinOp::Add),
                    (Token::Simple(Minus), BinOp::Sub),
                ],
                child: Some(&BinExprDef {
                    kind: "mul-div",
                    ops: &[
                        (Token::Simple(Star), BinOp::Mul),
                        (Token::Simple(Slash), BinOp::Div),
                    ],
                    child: None,
                }),
            }),
        }),
    }),
};

impl Parser {
    fn try_expr(&mut self) -> ParseResult<Option<Sp<Expr>>> {
        let Some(expr) = self.try_expr_impl()? else {
            return Ok(None);
        };
        Ok(Some(if self.try_exact(Equals).is_some() {
            let rhs = self.expr("any", Self::try_expr_impl)?;
            let lhs = expr;
            let start = lhs.span.clone();
            let end = rhs.span.clone();
            let span = start.merge(end);
            let lvalue = if let Some(pattern) = lhs.as_ref().filter_map(Expr::as_pattern) {
                pattern.map(LValue::Pattern)
            } else {
                lhs.map(LValue::Expr)
            };
            span.sp(Expr::Assign(Box::new(Assignment { lvalue, expr: rhs })))
        } else {
            expr
        }))
    }
    fn expr(
        &mut self,
        kind: &'static str,
        f: impl FnOnce(&mut Self) -> ParseResult<Option<Sp<Expr>>>,
    ) -> ParseResult<Sp<Expr>> {
        f(self)?.ok_or_else(|| self.expected([Expectation::Expr(kind)]))
    }
    fn try_expr_impl(&mut self) -> ParseResult<Option<Sp<Expr>>> {
        self.try_bin_expr_def(&BIN_EXPR_OR)
    }
    fn try_bin_expr_def(&mut self, def: &BinExprDef) -> ParseResult<Option<Sp<Expr>>> {
        let leaf = |this: &mut Self| {
            if let Some(child) = def.child {
                this.try_bin_expr_def(child)
            } else {
                this.try_term()
            }
        };
        let mut lhs = self.expr(def.kind, leaf)?;
        'rhs: loop {
            for (token, op) in def.ops {
                if let Some(op_span) = self.try_exact(token.clone()) {
                    let rhs = self.expr(def.kind, leaf)?;
                    let start = lhs.span.clone();
                    let end = rhs.span.clone();
                    let span = start.merge(end);
                    let op = op_span.sp(*op);
                    lhs = span.sp(Expr::Bin(Box::new(BinExpr { lhs, op, rhs })));
                    continue 'rhs;
                }
            }
            return Ok(Some(lhs));
        }
    }
    fn try_term(&mut self) -> ParseResult<Option<Sp<Expr>>> {
        Ok(Some(if let Some(ident) = self.try_ident() {
            println!("ident: {ident}");
            ident.map(Expr::Ident)
        } else {
            return Ok(None);
        }))
    }
}
