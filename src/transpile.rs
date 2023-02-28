use std::{error::Error, fmt, path::Path};

use crate::{
    ast::*,
    lex::Sp,
    parse::{parse, ParseError},
};

#[derive(Debug)]
pub enum TranspileError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
}

impl fmt::Display for TranspileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TranspileError::Parse(e) => write!(f, "{e}"),
            TranspileError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            TranspileError::InvalidReal(s) => write!(f, "invalid real: {s}"),
        }
    }
}

impl Error for TranspileError {}

pub type TranspileResult = Result<(), Sp<TranspileError>>;

#[derive(Debug, Default)]
pub struct Transpiler {
    pub code: String,
    indentation: usize,
}

impl Transpiler {
    pub fn transpile(&mut self, input: &str, path: &Path) -> Result<(), Vec<Sp<TranspileError>>> {
        let (items, errors) = parse(input, path);
        let mut errors: Vec<_> = errors
            .into_iter()
            .map(|e| e.map(TranspileError::Parse))
            .collect();
        for item in items {
            if let Err(e) = self.item(item) {
                errors.push(e);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
    fn item(&mut self, item: Item) -> TranspileResult {
        match item {
            Item::FunctionDef(def) => self.function_def(def),
            Item::Expr(expr, _) => self.expr(expr),
            Item::Binding(binding) => self.binding(binding),
        }
    }
    fn add(&mut self, s: impl Into<String>) {
        if self.code.ends_with('\n') {
            for _ in 0..self.indentation * 4 {
                self.code.push(' ');
            }
        }
        self.code.push_str(&s.into());
    }
    fn line(&mut self, s: impl Into<String>) {
        self.add(s);
        self.code.push('\n');
    }
    fn ensure_line(&mut self) {
        let ends_with_newline = self
            .code
            .chars()
            .rev()
            .find(|c| *c != ' ')
            .map_or(true, |c| c == '\n');
        if !ends_with_newline {
            self.code.push('\n');
        }
    }
    fn function_def(&mut self, def: FunctionDef) -> TranspileResult {
        self.add(format!("function {}(", def.name.value));
        for (i, param) in def.params.into_iter().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.add(param.name.value);
        }
        self.line(")");
        self.indentation += 1;
        for binding in def.bindings {
            self.binding(binding)?;
        }
        self.add("return ");
        self.expr(def.ret)?;
        self.ensure_line();
        self.indentation -= 1;
        self.line("end");
        Ok(())
    }
    fn binding(&mut self, binding: Binding) -> TranspileResult {
        match binding.pattern.value {
            Pattern::Ident(ident) => {
                self.add(format!("local {ident} = "));
                self.expr(binding.expr)?;
                self.ensure_line();
            }
            Pattern::Tuple(_) => todo!(),
        }
        Ok(())
    }
    fn expr(&mut self, expr: Sp<Expr>) -> TranspileResult {
        match expr.value {
            Expr::Struct(_) => todo!(),
            Expr::Enum(_) => todo!(),
            Expr::Ident(ident) => self.add(ident),
            Expr::Tuple(items) => {
                self.add("{");
                for (i, item) in items.into_iter().enumerate() {
                    if i > 0 {
                        self.add(", ");
                    }
                    self.expr(item)?;
                }
                self.add("}");
            }
            Expr::Array(_) => todo!(),
            Expr::Integer(i) => self.add(
                i.parse::<u64>()
                    .map_err(|_| expr.span.sp(TranspileError::InvalidInteger(i)))?
                    .to_string(),
            ),
            Expr::Real(r) => self.add(
                r.parse::<f64>()
                    .map_err(|_| expr.span.sp(TranspileError::InvalidReal(r)))?
                    .to_string(),
            ),
            Expr::Bool(b) => self.add(b.to_string()),
            Expr::Bin(bin) => self.bin_expr(*bin)?,
            Expr::Un(un) => self.un_expr(*un)?,
            Expr::If(if_expr) => self.if_expr(*if_expr)?,
            Expr::Call(call) => self.call(*call)?,
            Expr::Parened(inner) => {
                self.expr(expr.span.sp(*inner))?;
            }
        }
        Ok(())
    }
    fn call(&mut self, call: CallExpr) -> TranspileResult {
        self.expr(call.func)?;
        self.add("(");
        for (i, arg) in call.args.into_iter().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.expr(arg)?;
        }
        self.add(")");
        Ok(())
    }
    fn bin_expr(&mut self, bin: BinExpr) -> TranspileResult {
        self.expr(bin.lhs)?;
        for (op, rhs) in bin.rhs {
            self.add(format!(
                " {} ",
                match op.value {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "/",
                    BinOp::Eq => "==",
                    BinOp::Ne => "~=",
                    BinOp::Lt => "<",
                    BinOp::Le => "<=",
                    BinOp::Gt => ">",
                    BinOp::Ge => ">=",
                    BinOp::And => "and",
                    BinOp::Or => "or",
                    BinOp::RangeEx => todo!(),
                }
            ));
            self.expr(rhs)?;
        }
        Ok(())
    }
    fn un_expr(&mut self, un: UnExpr) -> TranspileResult {
        self.add(match un.op.value {
            UnOp::Neg => "-",
            UnOp::Not => "not ",
        });
        self.expr(un.expr)?;
        Ok(())
    }
    fn if_expr(&mut self, if_expr: IfExpr) -> TranspileResult {
        self.expr(if_expr.cond)?;
        self.add(" and ");
        self.expr(if_expr.if_true)?;
        self.add(" or ");
        self.expr(if_expr.if_false)?;
        Ok(())
    }
}
