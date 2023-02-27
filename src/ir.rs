use std::{
    collections::HashMap,
    error::Error,
    fmt,
    mem::take,
    ops::{Add, Div, Mul, Sub},
    path::Path,
};

use crate::{
    ast::{self, BinOp, Block, Expr, Item},
    builtin::builtin_types,
    lex::{Sp, Span},
    parse::{parse, ParseError},
};

#[derive(Debug, Clone)]
pub struct Ir {
    pub types: HashMap<String, Type>,
    pub values: HashMap<String, Value>,
}

impl Default for Ir {
    fn default() -> Self {
        Self {
            types: builtin_types(),
            values: HashMap::new(),
        }
    }
}

impl Ir {
    pub fn compile(&mut self, input: &str, path: &Path) -> Result<(), Vec<Sp<InterpretError>>> {
        let (items, errors) = parse(input, path);
        let mut errors: Vec<Sp<InterpretError>> = errors
            .into_iter()
            .map(|e| e.map(InterpretError::Parse))
            .collect();
        let mut interpretter = Interpretter {
            scopes: vec![Scope {
                types: take(&mut self.types),
                values: take(&mut self.values),
                kind: ScopeKind::Normal,
            }],
        };
        for item in items {
            if let Err(e) = interpretter.item(&item) {
                errors.push(e);
            }
        }
        let scope = interpretter.scopes.pop().unwrap();
        self.types = scope.types;
        self.values = scope.values;
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Nat,
    Int,
    Real,
    Array(Option<Box<Type>>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Type,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Nat => write!(f, "nat"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Array(Some(ty)) => write!(f, "[{ty}]"),
            Type::Array(None) => write!(f, "[_]"),
            Type::Tuple(tys) => {
                write!(f, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ")")
            }
            Type::Function(params, ret_ty) => {
                write!(f, "(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{param}")?;
                }
                write!(f, ")")?;
                if **ret_ty != Type::Unit {
                    write!(f, " -> {ret_ty}")?;
                }
                Ok(())
            }
            Type::Type => write!(f, "type"),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum Value {
    #[default]
    Unit,
    Bool(bool),
    Nat(u64),
    Int(i64),
    Real(f64),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Function(Function),
    Type(Type),
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Nat(_) => Type::Nat,
            Value::Int(_) => Type::Int,
            Value::Real(_) => Type::Real,
            Value::Array(items) => Type::Array(items.first().map(Value::ty).map(Box::new)),
            Value::Tuple(_) => Type::Tuple(vec![]),
            Value::Function(func) => Type::Function(
                func.params.iter().map(|param| param.ty.clone()).collect(),
                Box::new(func.ret_ty.clone()),
            ),
            Value::Type(_) => Type::Type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub doc: Option<String>,
    pub params: Vec<Param>,
    pub ret_ty: Type,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub enum InterpretError {
    Parse(ParseError),
    UnknownValue(String),
    UnknownType(String),
    InvalidInteger(String),
    InvalidReal(String),
    InvalidLValue,
    WrongNumberOfArguments(Option<String>, usize, usize),
    CallNonFunction(Type),
    InvalidBinOp(BinOp, Type, Type),
}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpretError::Parse(error) => write!(f, "{error}"),
            InterpretError::UnknownValue(name) => write!(f, "unknown value `{name}`"),
            InterpretError::UnknownType(name) => write!(f, "unknown type `{name}`"),
            InterpretError::InvalidInteger(value) => write!(f, "invalid integer `{value}`"),
            InterpretError::InvalidReal(value) => write!(f, "invalid real `{value}`"),
            InterpretError::InvalidLValue => write!(f, "invalid lvalue"),
            InterpretError::WrongNumberOfArguments(Some(name), expected, actual) => write!(
                f,
                "wrong number of arguments for `{name}`: expected {expected}, got {actual}"
            ),
            InterpretError::WrongNumberOfArguments(None, expected, actual) => write!(
                f,
                "wrong number of arguments: expected {expected}, got {actual}"
            ),
            InterpretError::CallNonFunction(ty) => write!(f, "attempted to call a {ty}"),
            InterpretError::InvalidBinOp(op, lhs, rhs) => {
                write!(f, "cannot {op:?} {lhs} and {rhs}")
            }
        }
    }
}

impl Error for InterpretError {}

pub type InterpretResult<T = ()> = Result<T, Sp<InterpretError>>;

struct Interpretter {
    scopes: Vec<Scope>,
}

struct Scope {
    values: HashMap<String, Value>,
    types: HashMap<String, Type>,
    kind: ScopeKind,
}

enum ScopeKind {
    Normal,
    Function,
    Loop,
}

impl Interpretter {
    fn find_value(&self, name: &str) -> Option<Value> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.values.get(name))
            .cloned()
    }
    fn find_value_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.values.get_mut(name))
    }
    fn find_type(&self, name: &str) -> Option<Type> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.types.get(name))
            .cloned()
    }
    fn in_new_scope<T>(&mut self, kind: ScopeKind, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.push(Scope {
            values: HashMap::new(),
            types: HashMap::new(),
            kind,
        });
        let result = f(self);
        self.scopes.pop();
        result
    }
    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    fn item(&mut self, item: &Item) -> InterpretResult<Value> {
        Ok(match item {
            Item::FunctionDef(def) => {
                let func = Function {
                    name: Some(def.name.value.clone()),
                    doc: def.doc.as_ref().map(|doc| doc.value.clone()),
                    params: def
                        .params
                        .iter()
                        .map(|param| -> InterpretResult<_> {
                            Ok(Param {
                                name: param.name.value.clone(),
                                ty: self.ty(&param.ty)?,
                            })
                        })
                        .collect::<InterpretResult<_>>()?,
                    ret_ty: def
                        .ret_ty
                        .as_ref()
                        .map(|ty| self.ty(ty))
                        .transpose()?
                        .unwrap_or(Type::Unit),
                    body: def.body.clone(),
                };
                self.scope_mut()
                    .values
                    .insert(def.name.value.clone(), Value::Function(func));
                Value::Unit
            }
            Item::Binding(binding) => {
                let value = self.expr(&binding.expr)?;
                match &binding.pattern.value {
                    ast::Pattern::Ident(name) => {
                        self.scope_mut().values.insert(name.clone(), value);
                    }
                    ast::Pattern::Tuple(_) => todo!(),
                }
                Value::Unit
            }
            Item::Expr(expr, ended) => {
                let value = self.expr(expr)?;
                if *ended {
                    Value::Unit
                } else {
                    value
                }
            }
        })
    }
    fn ty(&mut self, ty: &Sp<ast::Type>) -> InterpretResult<Type> {
        Ok(match &ty.value {
            ast::Type::Ident(ident) => self.find_type(ident).ok_or_else(|| {
                ty.span
                    .clone()
                    .sp(InterpretError::UnknownType(ident.clone()))
            })?,
            ast::Type::Array(inner) => Type::Array(Some(Box::new(
                self.ty(&ty.span.clone().sp((**inner).clone()))?,
            ))),
            ast::Type::Tuple(tys) if tys.is_empty() => Type::Unit,
            ast::Type::Tuple(tys) => Type::Tuple(
                tys.iter()
                    .map(|ty| self.ty(ty))
                    .collect::<InterpretResult<_>>()?,
            ),
        })
    }
    fn expr(&mut self, expr: &Sp<Expr>) -> InterpretResult<Value> {
        let span = |e: InterpretError| expr.span.clone().sp(e);
        Ok(match &expr.value {
            Expr::Struct(_) => todo!(),
            Expr::Enum(_) => todo!(),
            Expr::Ident(name) => self
                .find_value(name)
                .ok_or_else(|| span(InterpretError::UnknownValue(name.clone())))?,
            Expr::Tuple(exprs) if exprs.is_empty() => Value::Unit,
            Expr::Tuple(exprs) => Value::Tuple(
                exprs
                    .iter()
                    .map(|expr| self.expr(expr))
                    .collect::<InterpretResult<_>>()?,
            ),
            Expr::Array(exprs) => Value::Array(
                exprs
                    .iter()
                    .map(|expr| self.expr(expr))
                    .collect::<InterpretResult<_>>()?,
            ),
            Expr::Integer(i) => Value::Int(
                i.parse()
                    .map_err(|_| span(InterpretError::InvalidInteger(i.clone())))?,
            ),
            Expr::Real(f) => Value::Real(
                f.parse()
                    .map_err(|_| span(InterpretError::InvalidReal(f.clone())))?,
            ),
            Expr::Bool(b) => Value::Bool(*b),
            Expr::Bin(bin) => {
                let mut value = self.expr(&bin.lhs)?;
                for (op, rhs) in &bin.rhs {
                    let rhs = self.expr(rhs)?;
                    value = eval_bin_op(op, value, rhs)?;
                }
                value
            }
            Expr::Un(_) => todo!(),
            Expr::Assign(assignment) => {
                let value = self.expr(&assignment.expr)?;
                let lvalue_span = &assignment.lvalue.span;
                match &assignment.lvalue.value {
                    ast::Expr::Ident(name) => {
                        *self.find_value_mut(name).ok_or_else(|| {
                            lvalue_span
                                .clone()
                                .sp(InterpretError::UnknownValue(name.clone()))
                        })? = value;
                    }
                    _ => return Err(lvalue_span.clone().sp(InterpretError::InvalidLValue)),
                }
                Value::Unit
            }
            Expr::Block(block) => self.block(block)?,
            Expr::Call(call) => {
                let func = self.expr(&call.expr)?;
                if let Value::Function(func) = func {
                    let args: Vec<Value> = call
                        .args
                        .iter()
                        .map(|arg| self.expr(arg))
                        .collect::<InterpretResult<_>>()?;
                    self.call_impl(&func, args, expr.span.clone())?
                } else {
                    return Err(span(InterpretError::CallNonFunction(func.ty())));
                }
            }
            Expr::Access(_) => todo!(),
            Expr::For(_) => todo!(),
            Expr::While(_) => todo!(),
            Expr::IfElse(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Break => todo!(),
            Expr::Continue => todo!(),
        })
    }
    fn call_impl(
        &mut self,
        func: &Function,
        args: Vec<Value>,
        span: Span,
    ) -> InterpretResult<Value> {
        self.in_new_scope(ScopeKind::Function, |this| {
            if func.params.len() != args.len() {
                return Err(span.sp(InterpretError::WrongNumberOfArguments(
                    func.name.clone(),
                    func.params.len(),
                    args.len(),
                )));
            }
            for (param, arg) in func.params.iter().zip(args) {
                this.scope_mut().values.insert(param.name.clone(), arg);
            }
            this.block(&func.body)
        })
    }
    fn block(&mut self, block: &Block) -> InterpretResult<Value> {
        self.in_new_scope(ScopeKind::Normal, |this| {
            let mut value = Value::Unit;
            for item in &block.items {
                value = this.item(item)?;
            }
            Ok(value)
        })
    }
}

fn eval_bin_op(op: &Sp<BinOp>, a: Value, b: Value) -> InterpretResult<Value> {
    macro_rules! math {
        ($f:expr) => {
            match (a, b) {
                (Value::Nat(a), Value::Nat(b)) => Value::Nat($f(a, b)),
                (Value::Int(a), Value::Int(b)) => Value::Int($f(a, b)),
                (Value::Real(a), Value::Real(b)) => Value::Real($f(a, b)),
                (a, b) => {
                    return Err(op.span.clone().sp(InterpretError::InvalidBinOp(
                        op.value,
                        a.ty(),
                        b.ty(),
                    )))
                }
            }
        };
    }
    Ok(match op.value {
        BinOp::Add => math!(Add::add),
        BinOp::Sub => math!(Sub::sub),
        BinOp::Mul => math!(Mul::mul),
        BinOp::Div => math!(Div::div),
        _ => todo!(),
    })
}
