use std::{
    collections::{HashMap, HashSet},
    fmt,
    path::Path,
};

use crate::{
    ast::{self, LogicalOp},
    lex::Sp,
    parse::{parse, ParseError},
};

#[derive(Debug)]
pub enum CheckError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
    UnknownBinding(String),
    WrongNumberOfArgs(usize, usize),
}

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CheckError::Parse(e) => write!(f, "{e}"),
            CheckError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            CheckError::InvalidReal(s) => write!(f, "invalid real: {s}"),
            CheckError::UnknownBinding(s) => write!(f, "unknown binding: {s}"),
            CheckError::WrongNumberOfArgs(expected, actual) => {
                write!(
                    f,
                    "wrong number of arguments: expected {expected}, got {actual}"
                )
            }
        }
    }
}

pub type CheckResult<T> = Result<T, Sp<CheckError>>;

#[derive(Debug, Clone)]
pub enum Item {
    Expr(Expr),
    Binding(Binding),
    FunctionDef(FunctionDef),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub func: Function,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub bindings: Vec<Binding>,
    pub expr: Expr,
}

impl From<Expr> for Block {
    fn from(expr: Expr) -> Self {
        Block {
            bindings: Vec::new(),
            expr,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub pattern: Pattern,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(String),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Ident(String),
    Bool(bool),
    Nat(u64),
    Int(i64),
    Real(f64),
    If(Box<IfExpr>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
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

impl From<Function> for Expr {
    fn from(func: Function) -> Self {
        Expr::Function(Box::new(func))
    }
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: Expr,
    pub if_true: Block,
    pub if_false: Block,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct LogicalExpr {
    pub left: Expr,
    pub op: LogicalOp,
    pub right: Expr,
}

pub struct Checker {
    scopes: Vec<Scope>,
    pub(crate) functions: HashMap<String, Function>,
    errors: Vec<Sp<CheckError>>,
}

#[derive(Default)]
pub(crate) struct Scope {
    pub bindings: HashSet<String>,
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
        for item in &ast_items {
            println!("{item:#?}");
        }
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
        self.scopes.push(Scope::default());
        let params: Vec<String> = def
            .params
            .into_iter()
            .map(|p| self.param(p.value))
            .collect();
        let body = self.block(def.body)?;
        let func = Function { params, body };
        self.functions.insert(def.name.value.clone(), func.clone());
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
    fn param(&mut self, param: String) -> String {
        self.scope_mut().bindings.insert(param.clone());
        param
    }
    fn pattern(&mut self, pattern: Sp<ast::Pattern>) -> CheckResult<Pattern> {
        Ok(match pattern.value {
            ast::Pattern::Ident(name) => {
                self.scope_mut().bindings.insert(name.clone());
                Pattern::Ident(name)
            }
            ast::Pattern::Tuple(patterns) => Pattern::Tuple(
                patterns
                    .into_iter()
                    .map(|p| self.pattern(p))
                    .collect::<CheckResult<_>>()?,
            ),
        })
    }
    fn expr(&mut self, expr: Sp<ast::Expr>) -> CheckResult<Expr> {
        Ok(match expr.value {
            ast::Expr::Unit => Expr::Unit,
            ast::Expr::If(if_expr) => self.if_expr(*if_expr)?,
            ast::Expr::Call(call) => self.call_expr(*call)?,
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
                if !self.scope_mut().bindings.contains(&name) {
                    return Err(expr.span.sp(CheckError::UnknownBinding(name)));
                }
                Expr::Ident(name)
            }
            ast::Expr::Tuple(items) => Expr::Tuple(
                items
                    .into_iter()
                    .map(|item| self.expr(item))
                    .collect::<CheckResult<_>>()?,
            ),
            ast::Expr::List(items) => Expr::List(
                items
                    .into_iter()
                    .map(|item| self.expr(item))
                    .collect::<CheckResult<_>>()?,
            ),
            ast::Expr::Parened(inner) => self.expr(expr.span.sp(*inner))?,
        })
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
        let func = call.func.span.sp(match call.func.value {
            ast::CallKind::Normal(func) => func,
            ast::CallKind::Binary(op) => ast::Expr::Ident(op.name().into()),
        });
        let func = self.expr(func)?;
        let args: Vec<Expr> = call
            .args
            .into_iter()
            .map(|arg| self.expr(arg))
            .collect::<CheckResult<_>>()?;
        Ok(Expr::from(CallExpr { func, args }))
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
