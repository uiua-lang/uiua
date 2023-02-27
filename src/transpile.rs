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

#[derive(Debug)]
pub struct Transpiler {
    pub code: String,
    indentation: usize,
    ret_stack: Vec<RetType>,
}

#[derive(Debug, Clone)]
enum RetType {
    None,
    Return,
    Assign(Sp<Expr>, Option<Sp<BinOp>>),
}

impl Default for Transpiler {
    fn default() -> Self {
        Self {
            code: String::new(),
            indentation: 0,
            ret_stack: vec![RetType::None],
        }
    }
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
        if !self.code.ends_with('\n') {
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
        self.block(def.body, RetType::Return)?;
        self.ensure_line();
        self.line("end");
        Ok(())
    }
    fn binding(&mut self, binding: Binding) -> TranspileResult {
        match binding.pattern.value {
            Pattern::Ident(ident) => {
                if let Expr::Block(block) = binding.expr.value {
                    self.line(format!("local {ident}"));
                    self.line("do");
                    self.block(
                        block,
                        RetType::Assign(binding.pattern.span.sp(Expr::Ident(ident)), None),
                    )?;
                    self.ensure_line();
                    self.line("end");
                } else {
                    self.add(format!("local {ident} = "));
                    self.expr(binding.expr)?;
                    self.ensure_line();
                }
            }
            Pattern::Tuple(_) => todo!(),
        }
        self.line("");
        Ok(())
    }
    fn block(&mut self, block: Block, ret_ty: RetType) -> TranspileResult {
        self.indentation += 1;
        self.ret_stack.push(ret_ty);
        let item_count = block.items.len();
        for (i, item) in block.items.into_iter().enumerate() {
            match item {
                Item::Expr(expr, ended)
                    if i == item_count - 1 && !ended && expr.value.returns_value() =>
                {
                    match self.ret_stack.last().unwrap() {
                        RetType::None => {}
                        RetType::Return => self.add("return "),
                        RetType::Assign(lvalue, op) => {
                            self.assignment_impl(lvalue.clone(), op.clone())?
                        }
                    }
                    self.expr(expr)?;
                    self.ensure_line();
                }
                item => self.item(item)?,
            }
        }
        self.indentation -= 1;
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
            Expr::Un(_) => todo!(),
            Expr::Assign(assign) => self.assignment(*assign)?,
            Expr::Block(block) => {
                self.line("do");
                let ret_ty = self.ret_stack.last().unwrap().clone();
                self.block(block, ret_ty)?;
                self.ensure_line();
                self.line("end");
            }
            Expr::Call(call) => self.call(*call)?,
            Expr::Access(_) => todo!(),
            Expr::For(_) => todo!(),
            Expr::While(_) => todo!(),
            Expr::IfElse(if_else) => self.if_else(*if_else)?,
            Expr::Return(expr) => {
                self.add("return ");
                if let Some(expr) = expr {
                    self.expr(*expr)?;
                }
                self.ensure_line();
            }
            Expr::Break => todo!(),
            Expr::Continue => todo!(),
        }
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
    fn assignment(&mut self, assign: Assignment) -> TranspileResult {
        if let Expr::Block(block) = assign.expr.value {
            self.line("do");
            self.block(block, RetType::Assign(assign.lvalue, assign.op))?;
            self.ensure_line();
            self.line("end");
        } else {
            self.assignment_impl(assign.lvalue, assign.op)?;
            self.expr(assign.expr)?;
            self.ensure_line();
        }
        Ok(())
    }
    fn assignment_impl(&mut self, lvalue: Sp<Expr>, op: Option<Sp<BinOp>>) -> TranspileResult {
        self.expr(lvalue)?;
        if let Some(op) = op {
            let op = match op.value {
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
            };
            self.add(format!(" {op} "));
        } else {
            self.add(" = ");
        }
        Ok(())
    }
    fn call(&mut self, call: Call) -> TranspileResult {
        self.expr(call.expr)?;
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
    fn if_else(&mut self, if_else: IfElse) -> TranspileResult {
        self.add("if ");
        self.expr(if_else.first.cond)?;
        self.line(" then");
        let ret_ty = self.ret_stack.last().unwrap().clone();
        self.block(if_else.first.then, ret_ty)?;
        self.ensure_line();
        self.line("end");
        Ok(())
    }
}
