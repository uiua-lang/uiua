use std::{
    collections::HashMap,
    error::Error,
    fmt, fs, io,
    path::{Path, PathBuf},
};

use crate::{
    ast::{BinOp, LogicalOp},
    check::*,
    lex::{Ident, Sp, Span},
    value::Value,
};

#[derive(Debug)]
pub enum RuntimeError {
    Load(PathBuf, io::Error),
    Check(Vec<Sp<CheckError>>),
    Run(Sp<String>),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Load(path, e) => {
                write!(f, "failed to load {}: {e}", path.to_string_lossy())
            }
            RuntimeError::Check(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            RuntimeError::Run(e) => write!(f, "{e}"),
        }
    }
}

impl From<Sp<String>> for RuntimeError {
    fn from(value: Sp<String>) -> Self {
        Self::Run(value)
    }
}

impl From<Vec<Sp<CheckError>>> for RuntimeError {
    fn from(errors: Vec<Sp<CheckError>>) -> Self {
        Self::Check(errors)
    }
}

impl Error for RuntimeError {}

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

pub struct Interpretter {
    scopes: Vec<Scope>,
    checker: Checker,
}

#[derive(Debug, Default)]
struct Scope {
    bindings: HashMap<Ident, Value>,
}

impl Default for Interpretter {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            checker: Checker::default(),
        }
    }
}

impl Interpretter {
    pub fn new() -> Self {
        Self::default()
    }
    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> RuntimeResult {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| RuntimeError::Load(path.into(), e))?;
        let items = self.checker.load(&input, path)?;
        for item in items {
            self.item(&item)?;
        }
        Ok(())
    }
    pub fn item(&mut self, item: &Item) -> RuntimeResult {
        match item {
            Item::Binding(binding) => self.binding(binding)?,
            Item::Expr(expr) => {
                let val = self.expr(expr)?;
                println!("{}: {:?}", expr.span, val);
            }
            Item::FunctionDef(function_def) => self.function_def(function_def)?,
        }
        Ok(())
    }
    pub fn function_def(&mut self, def: &FunctionDef) -> RuntimeResult {
        let value = Value::Function(def.func.clone());
        self.scope_mut().bindings.insert(def.name.clone(), value);
        Ok(())
    }
    pub fn binding(&mut self, binding: &Binding) -> RuntimeResult {
        let value = self.expr(&binding.expr)?;
        self.pattern(&binding.pattern, value, false)?;
        Ok(())
    }
    pub fn block(&mut self, block: &Block) -> RuntimeResult<Value> {
        for binding in &block.bindings {
            self.binding(binding)?;
        }
        self.expr(&block.expr)
    }
    pub fn pattern(
        &mut self,
        pattern: &Sp<Pattern>,
        value: Value,
        fallible: bool,
    ) -> RuntimeResult<bool> {
        Ok(match (&pattern.value, value) {
            (Pattern::Ident(name), value) => {
                self.scope_mut().bindings.insert(name.clone(), value);
                true
            }
            (Pattern::List(items), Value::List(list)) => {
                if items.len() == list.len() {
                    for (item, value) in items.iter().zip(list) {
                        if !self.pattern(item, value, fallible)? {
                            return Ok(false);
                        }
                    }
                    true
                } else if fallible {
                    false
                } else {
                    return Err(pattern
                        .span
                        .clone()
                        .sp(format!(
                            "list pattern {} does not match list of length {}",
                            pattern.value,
                            list.len()
                        ))
                        .into());
                }
            }
            (pat, value) => {
                if fallible {
                    false
                } else {
                    return Err(pattern
                        .span
                        .clone()
                        .sp(format!("{pat} does not match {}", value.ty()))
                        .into());
                }
            }
        })
    }
    pub fn expr(&mut self, expr: &Sp<Expr>) -> RuntimeResult<Value> {
        Ok(match &expr.value {
            Expr::Unit => Value::Unit,
            Expr::Ident(name) => self
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.bindings.get(name))
                .unwrap()
                .clone(),
            Expr::Bool(b) => Value::Bool(*b),
            Expr::Nat(n) => Value::Nat(*n),
            Expr::Int(i) => Value::Int(*i),
            Expr::Real(r) => Value::Real(*r),
            Expr::Call(call) => self.call_expr(call, &expr.span)?,
            Expr::Logic(logic) => self.logic_expr(logic)?,
            Expr::If(if_expr) => self.if_expr(if_expr)?,
            Expr::List(list) => Value::List(
                list.iter()
                    .map(|expr| self.expr(expr))
                    .collect::<RuntimeResult<_>>()?,
            ),
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr)?,
            Expr::Function(_) => todo!(),
        })
    }
    pub fn call_expr(&mut self, call: &CallExpr, span: &Span) -> RuntimeResult<Value> {
        let func = self.expr(&call.func)?;
        let func = match func {
            Value::Function(func) => func,
            value => {
                return Err(span
                    .clone()
                    .sp(format!("cannot call {}", value.ty()))
                    .into())
            }
        };
        self.scopes.push(Scope::default());
        for (param, arg) in func.params.iter().zip(&call.args) {
            let val = self.expr(arg)?;
            self.scope_mut().bindings.insert(param.value.clone(), val);
        }
        let result = self.block(&func.body)?;
        self.scopes.pop().unwrap();
        Ok(result)
    }
    pub fn if_expr(&mut self, if_expr: &IfExpr) -> RuntimeResult<Value> {
        let cond = self.expr(&if_expr.cond)?;
        if cond.is_truthy() {
            self.block(&if_expr.if_true)
        } else {
            self.block(&if_expr.if_false)
        }
    }
    pub fn logic_expr(&mut self, logic: &LogicalExpr) -> RuntimeResult<Value> {
        let left = self.expr(&logic.left)?;
        let mut right = || self.expr(&logic.right);
        Ok(match logic.op {
            LogicalOp::Or => {
                if left.is_truthy() {
                    left
                } else {
                    right()?
                }
            }
            LogicalOp::And => {
                if left.is_truthy() {
                    right()?
                } else {
                    left
                }
            }
        })
    }
    pub fn bin_expr(&mut self, bin_expr: &BinExpr) -> RuntimeResult<Value> {
        let left = self.expr(&bin_expr.left)?;
        let right = self.expr(&bin_expr.right)?;
        let span = &bin_expr.op.span;
        Ok(match bin_expr.op.value {
            BinOp::Add => left.add(right, span)?,
            BinOp::Sub => left.sub(right, span)?,
            BinOp::Mul => left.mul(right, span)?,
            BinOp::Div => left.div(right, span)?,
            BinOp::Eq => Value::Bool(left == right),
            BinOp::Ne => Value::Bool(left != right),
            BinOp::Lt => Value::Bool(left < right),
            BinOp::Gt => Value::Bool(left > right),
            BinOp::Le => Value::Bool(left <= right),
            BinOp::Ge => Value::Bool(left >= right),
            BinOp::RangeEx => todo!(),
        })
    }
}
