use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt,
    path::Path,
};

use crate::{
    ast::{self, BinOp, LogicalOp},
    lex::{Ident, Sp, Span},
    parse::{parse, ParseError},
};

#[derive(Debug)]
pub enum CheckError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
    UnknownBinding(Ident),
}

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CheckError::Parse(e) => write!(f, "{e}"),
            CheckError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            CheckError::InvalidReal(s) => write!(f, "invalid real: {s}"),
            CheckError::UnknownBinding(s) => write!(f, "unknown binding: {s}"),
        }
    }
}

pub type CheckResult<T> = Result<T, Sp<CheckError>>;

#[derive(Debug, Clone)]
pub enum Item {
    Expr(Sp<Expr>),
    Binding(Binding),
    FunctionDef(FunctionDef),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: Ident,
    pub func: Function,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: FunctionId,
    pub params: Vec<Sp<Ident>>,
    pub body: Block,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Function {}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(Span),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub bindings: Vec<Binding>,
    pub expr: Sp<Expr>,
}

impl From<Sp<Expr>> for Block {
    fn from(expr: Sp<Expr>) -> Self {
        Block {
            bindings: Vec::new(),
            expr,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub pattern: Sp<Pattern>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(Ident),
    List(Vec<Sp<Pattern>>),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Ident(s) => write!(f, "{s}"),
            Pattern::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item.value)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Ident(Ident),
    Bool(bool),
    Nat(u64),
    Int(i64),
    Real(f64),
    If(Box<IfExpr>),
    List(Vec<Sp<Expr>>),
    Binary(Box<BinExpr>),
    Call(Box<CallExpr>),
    Logic(Box<LogicalExpr>),
    Function(Box<Function>),
}

impl From<IfExpr> for Expr {
    fn from(if_expr: IfExpr) -> Self {
        Expr::If(Box::new(if_expr))
    }
}

impl From<CallExpr> for Expr {
    fn from(call_expr: CallExpr) -> Self {
        Expr::Call(Box::new(call_expr))
    }
}

impl From<LogicalExpr> for Expr {
    fn from(logical_expr: LogicalExpr) -> Self {
        Expr::Logic(Box::new(logical_expr))
    }
}

impl From<BinExpr> for Expr {
    fn from(bin_expr: BinExpr) -> Self {
        Expr::Binary(Box::new(bin_expr))
    }
}

impl From<Function> for Expr {
    fn from(func: Function) -> Self {
        Expr::Function(Box::new(func))
    }
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: Sp<Expr>,
    pub if_true: Block,
    pub if_false: Block,
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    pub left: Sp<Expr>,
    pub op: Sp<BinOp>,
    pub right: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Sp<Expr>,
    pub args: Vec<Sp<Expr>>,
}

#[derive(Debug, Clone)]
pub struct LogicalExpr {
    pub left: Sp<Expr>,
    pub op: LogicalOp,
    pub right: Sp<Expr>,
}

pub struct Checker {
    scopes: Vec<Scope>,
    pub(crate) functions: HashMap<FunctionId, Function>,
    errors: Vec<Sp<CheckError>>,
}

#[derive(Debug, Default)]
pub(crate) struct Scope {
    pub bindings: HashSet<Ident>,
}

impl Default for Checker {
    fn default() -> Self {
        Checker {
            scopes: vec![Scope::default()],
            functions: HashMap::new(),
            errors: Vec::new(),
        }
    }
}

impl Checker {
    pub fn load(&mut self, input: &str, path: &Path) -> Result<Vec<Item>, Vec<Sp<CheckError>>> {
        let (ast_items, errors) = parse(input, path);
        let mut errors: Vec<Sp<CheckError>> =
            (errors.into_iter().map(|e| e.map(CheckError::Parse))).collect();
        let mut items = Vec::new();
        for item in ast_items {
            match self.item(item) {
                Ok(item) => items.push(item),
                Err(e) => errors.push(e),
            }
            errors.append(&mut self.errors);
        }
        if errors.is_empty() {
            Ok(items)
        } else {
            Err(errors)
        }
    }
    pub(crate) fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    fn item(&mut self, item: ast::Item) -> CheckResult<Item> {
        Ok(match item {
            ast::Item::Expr(expr) => Item::Expr(self.expr(expr)?),
            ast::Item::Binding(binding) => Item::Binding(self.binding(binding)?),
            ast::Item::FunctionDef(def) => Item::FunctionDef(self.function_def(def)?),
        })
    }
    fn function_def(&mut self, def: ast::FunctionDef) -> CheckResult<FunctionDef> {
        self.scope_mut().bindings.insert(def.name.value);
        self.scopes.push(Scope::default());
        let params: Vec<_> = def.params.into_iter().map(|p| self.param(p)).collect();
        for param in &params {
            self.scope_mut().bindings.insert(param.value);
        }
        let body = self.block(def.body)?;
        self.scopes.pop().unwrap();
        let id = FunctionId::Named(def.name.value);
        let func = Function { id, params, body };
        self.functions.insert(func.id.clone(), func.clone());
        Ok(FunctionDef {
            name: def.name.value,
            func,
        })
    }
    fn block(&mut self, body: ast::Block) -> CheckResult<Block> {
        self.scopes.push(Scope::default());
        let bindings = body
            .bindings
            .into_iter()
            .map(|b| self.binding(b))
            .collect::<CheckResult<_>>()?;
        let expr = self.expr(body.expr)?;
        self.scopes.pop().unwrap();
        Ok(Block { bindings, expr })
    }
    fn binding(&mut self, binding: ast::Binding) -> CheckResult<Binding> {
        let expr = self.expr(binding.expr)?;
        let pattern = self.pattern(binding.pattern)?;
        Ok(Binding { pattern, expr })
    }
    fn param(&mut self, param: Sp<Ident>) -> Sp<Ident> {
        self.scope_mut().bindings.insert(param.value);
        param
    }
    fn pattern(&mut self, pattern: Sp<ast::Pattern>) -> CheckResult<Sp<Pattern>> {
        Ok(pattern.span.sp(match pattern.value {
            ast::Pattern::Ident(name) => {
                self.scope_mut().bindings.insert(name);
                Pattern::Ident(name)
            }
            ast::Pattern::Tuple(patterns) => Pattern::List(
                patterns
                    .into_iter()
                    .map(|p| self.pattern(p))
                    .collect::<CheckResult<_>>()?,
            ),
        }))
    }
    fn expr(&mut self, expr: Sp<ast::Expr>) -> CheckResult<Sp<Expr>> {
        Ok(expr.span.clone().sp(match expr.value {
            ast::Expr::Unit => Expr::Unit,
            ast::Expr::If(if_expr) => self.if_expr(*if_expr)?,
            ast::Expr::Call(call) => self.call_expr(*call)?,
            ast::Expr::Bin(bin_expr) => self.bin_expr(*bin_expr)?,
            ast::Expr::Logic(log_expr) => self.logic_expr(*log_expr)?,
            ast::Expr::Bool(b) => Expr::Bool(b),
            ast::Expr::Integer(i) => Expr::Int(
                i.parse()
                    .map_err(|_| expr.span.sp(CheckError::InvalidInteger(i)))?,
            ),
            ast::Expr::Real(r) => Expr::Real(
                r.parse()
                    .map_err(|_| expr.span.sp(CheckError::InvalidReal(r)))?,
            ),
            ast::Expr::Ident(name) => {
                if !self
                    .scopes
                    .iter()
                    .rev()
                    .any(|scope| scope.bindings.contains(&name))
                {
                    return Err(expr.span.sp(CheckError::UnknownBinding(name)));
                }
                Expr::Ident(name)
            }
            ast::Expr::List(items) => Expr::List(
                items
                    .into_iter()
                    .map(|item| self.expr(item))
                    .collect::<CheckResult<_>>()?,
            ),
            ast::Expr::Parened(inner) => return self.expr(expr.span.sp(*inner)),
        }))
    }
    fn if_expr(&mut self, if_expr: ast::IfExpr) -> CheckResult<Expr> {
        let cond = self.expr(if_expr.cond)?;
        let if_true = self.block(if_expr.if_true)?;
        let if_false = self.block(if_expr.if_false)?;
        Ok(Expr::from(IfExpr {
            cond,
            if_true,
            if_false,
        }))
    }
    fn call_expr(&mut self, call: ast::CallExpr) -> CheckResult<Expr> {
        let func = self.expr(call.func)?;
        let args: Vec<_> = call
            .args
            .into_iter()
            .map(|arg| self.expr(arg))
            .collect::<CheckResult<_>>()?;
        Ok(Expr::from(CallExpr { func, args }))
    }
    fn bin_expr(&mut self, bin_expr: ast::BinExpr) -> CheckResult<Expr> {
        let left = self.expr(bin_expr.left)?;
        let right = self.expr(bin_expr.right)?;
        Ok(Expr::from(BinExpr {
            op: bin_expr.op,
            left,
            right,
        }))
    }
    fn logic_expr(&mut self, log_expr: ast::LogicalExpr) -> CheckResult<Expr> {
        let left = self.expr(log_expr.left)?;
        let right = self.expr(log_expr.right)?;
        Ok(Expr::from(LogicalExpr {
            op: log_expr.op.value,
            left,
            right,
        }))
    }
}
