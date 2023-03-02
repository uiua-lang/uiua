#![allow(dead_code, unused_variables)]

use std::{mem::take, path::Path};

use crate::{check::*, lex::Sp};

#[derive(Default)]
pub struct Transpiler {
    code: String,
    indent: usize,
    checker: Checker,
}

impl Transpiler {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add(&mut self, code: impl AsRef<str>) {
        if self.code.ends_with('\n') {
            self.code.push_str(&"    ".repeat(self.indent));
        }
        self.code.push_str(code.as_ref());
    }
    pub fn line(&mut self, code: impl AsRef<str>) {
        self.add(code);
        self.code.push('\n');
    }
    pub fn load(&mut self, input: &str, path: &Path) -> Result<String, Vec<Sp<CheckError>>> {
        let items = self.checker.load(input, path)?;
        for item in items {
            self.item(item);
        }
        Ok(take(&mut self.code))
    }
    fn item(&mut self, item: Item) {
        match item {
            Item::Expr(expr) => self.expr(expr.value),
            Item::Binding(_) => todo!(),
            Item::FunctionDef(_) => todo!(),
        }
    }
    fn function_def(&mut self, def: FunctionDef) {
        self.function(Some(def.name), def.func);
    }
    fn function(&mut self, name: Option<String>, func: Function) {
        self.add("function");
        if let Some(name) = name {
            self.add(" ");
            self.add(name);
        }
        self.add("(");
        for (i, param) in func.params.into_iter().enumerate() {
            if i > 0 {
                self.add(", ");
            }
            self.add(param.value);
        }
        self.add(") ");
        self.block(func.body);
        self.add("end");
    }
    fn binding(&mut self, binding: Binding) {}
    fn expr(&mut self, expr: Expr) {}
    fn block(&mut self, block: Block) {}
}
