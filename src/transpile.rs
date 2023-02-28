use std::{collections::HashMap, mem::take, path::Path};

use crate::{
    ast::{BinOp, UnOp},
    check::*,
    lex::Sp,
};

pub struct Transpiler {
    pub(crate) checker: Checker,
    pub(crate) code: String,
    indentation: usize,
    pub(crate) function_replacements: HashMap<String, String>,
}

impl Default for Transpiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Transpiler {
    pub(crate) fn new() -> Self {
        Self {
            checker: Checker::default(),
            code: String::new(),
            indentation: 0,
            function_replacements: HashMap::new(),
        }
    }
    pub fn transpile(&mut self, input: &str, path: &Path) -> Result<(), Vec<Sp<CheckError>>> {
        let (items, errors) = self.checker.load(input, path);
        for item in items {
            self.item(item);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
    fn item(&mut self, item: Item) {
        match item {
            Item::FunctionDef(def) => self.function_def(def),
            Item::Expr(expr) => self.expr(expr.expr),
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
    fn function_def(&mut self, def: FunctionDef) {
        self.add(format!("function {}(", def.name));
        for (i, param) in def.params.into_iter().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.add(param.name);
        }
        self.line(")");
        self.indentation += 1;
        for binding in def.bindings {
            self.binding(binding);
        }
        self.add("return ");
        self.expr(def.expr.expr);
        self.ensure_line();
        self.indentation -= 1;
        self.line("end");
    }
    fn binding(&mut self, binding: Binding) {
        match binding.pattern {
            Pattern::Ident(ident) => {
                self.add(format!("local {ident} = "));
                self.expr(binding.expr.expr);
                self.ensure_line();
            }
            Pattern::Tuple(items) => {
                // Initial expression binding
                self.add("local ");
                let mut groups = Vec::new();
                for (i, pattern) in items.into_iter().enumerate() {
                    if i > 0 {
                        self.add(", ");
                    }
                    match pattern {
                        Pattern::Ident(ident) => self.add(ident),
                        Pattern::Tuple(items) => {
                            let name = format!("tuple_{}", i);
                            self.add(name.clone());
                            groups.push((name, items));
                        }
                    }
                }
                self.add(" = ");
                self.add("unpack(");
                self.expr(binding.expr.expr);
                self.add(")");
                self.ensure_line();
                // Subpattern bindings
                while !groups.is_empty() {
                    for (name, patterns) in take(&mut groups) {
                        self.add("local ");
                        for (i, pattern) in patterns.into_iter().enumerate() {
                            if i > 0 {
                                self.add(", ");
                            }
                            match pattern {
                                Pattern::Ident(ident) => self.add(ident),
                                Pattern::Tuple(items) => {
                                    let name = format!("tuple_{}_{}", name, i);
                                    self.add(name.clone());
                                    groups.push((name, items));
                                }
                            }
                        }
                        self.add(" = ");
                        self.add("unpack(");
                        self.add(name);
                        self.add(")");
                        self.ensure_line();
                    }
                }
            }
        }
    }
    fn expr(&mut self, expr: Expr) {
        match expr {
            Expr::Unit => self.add("nil"),
            Expr::Ident(ident) => self.add(ident),
            Expr::Tuple(items) => {
                self.add("{");
                for (i, item) in items.into_iter().enumerate() {
                    if i > 0 {
                        self.add(", ");
                    }
                    self.expr(item);
                }
                self.add("}");
            }
            Expr::List(_) => todo!(),
            Expr::Nat(n) => self.add(n.to_string()),
            Expr::Int(i) => self.add(i.to_string()),
            Expr::Real(r) => self.add(r.to_string()),
            Expr::Bool(b) => self.add(b.to_string()),
            Expr::If(if_expr) => self.if_expr(*if_expr),
            Expr::Call(call) => self.call(*call),
        }
    }
    fn call(&mut self, call: CallExpr) {
        match call.func {
            CallKind::Normal(func) => {
                self.expr(func);
                self.add("(");
                for (i, arg) in call.args.into_iter().enumerate() {
                    if i > 0 {
                        self.add(", ");
                    }
                    self.expr(arg);
                }
                self.add(")");
            }
            CallKind::Binary(op) => {
                if call.args.len() != 2 {
                    todo!("implement partial binary operator application");
                }
                let [left, right]: [Expr; 2] =
                    call.args.try_into().unwrap_or_else(|_| unreachable!());
                self.bin_expr(left, op, right);
            }
            CallKind::Unary(op) => {
                if call.args.len() != 1 {
                    todo!("implement partial unary operator application");
                }
                let expr = call.args.into_iter().next().unwrap();
                self.un_expr(op, expr);
            }
        }
    }
    fn if_expr(&mut self, if_expr: IfExpr) {
        self.expr(if_expr.cond);
        self.add(" and ");
        self.expr(if_expr.if_true);
        self.add(" or ");
        self.expr(if_expr.if_false);
    }
    fn bin_expr(&mut self, left: Expr, op: BinOp, right: Expr) {
        self.expr(left);
        self.add(format!(
            " {} ",
            match op {
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
        self.expr(right);
    }
    fn un_expr(&mut self, op: UnOp, expr: Expr) {
        self.add(match op {
            UnOp::Neg => "-",
            UnOp::Not => "not ",
        });
        self.expr(expr);
    }
}
