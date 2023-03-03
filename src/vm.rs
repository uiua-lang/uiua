use std::{collections::HashMap, fmt, fs, path::Path};

use crate::{
    ast::BinOp,
    check::*,
    interpret::{UiuaError, UiuaResult},
    lex::{Ident, Sp, Span},
};

type Value = crate::value::Value<usize>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    Comment(String),
    Push(Value),
    Copy(usize),
    Call(usize, Span),
    Return,
    Jump(usize),
    JumpIf(usize, bool),
    BinOp(BinOp, Span),
    DestructureList(usize, Span),
    Dud,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Comment(s) => write!(f, "\n// {}", s),
            instr => write!(f, "{instr:?}"),
        }
    }
}

#[derive(Default)]
pub struct Vm {
    instrs: Vec<Instr>,
    stack: Vec<Value>,
    call_stack: Vec<StackFrame>,
}

struct StackFrame {
    ret: usize,
    stack_size: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn run_file(&mut self, path: impl AsRef<Path>) -> UiuaResult {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        let mut compiler = Compiler::new();
        compiler.load(&input, path)?;
        let (instrs, start) = compiler.finish();
        self.instrs = instrs;
        for (i, instr) in self.instrs.iter().enumerate() {
            println!("{i:>3} {instr}");
        }
        println!();
        self.run(start)
    }
    fn run(&mut self, mut pc: usize) -> UiuaResult {
        println!("Running...");
        while pc < self.instrs.len() {
            let instr = &self.instrs[pc];
            // println!("{instr}");
            match instr {
                Instr::Comment(_) => {}
                Instr::Push(v) => self.stack.push(v.clone()),
                Instr::Copy(n) => self.stack.push(self.stack[self.stack.len() - *n].clone()),
                Instr::Call(arg_count, _) => {
                    let index = match self.stack.pop().unwrap() {
                        Value::Function(func) => func,
                        val => {
                            let message = format!("cannot call {}", val.ty());
                            dbg!(val);
                            return Err(Span::default().sp(message).into());
                        }
                    };
                    self.call_stack.push(StackFrame {
                        ret: pc + 1,
                        stack_size: self.stack.len() - arg_count,
                    });
                    pc = index;
                    continue;
                }
                Instr::Return => {
                    if let Some(frame) = self.call_stack.pop() {
                        let value = self.stack.pop().unwrap();
                        pc = frame.ret;
                        self.stack.truncate(frame.stack_size);
                        self.stack.push(value);
                        continue;
                    } else {
                        break;
                    }
                }
                Instr::Jump(to) => {
                    pc = *to;
                    continue;
                }
                Instr::JumpIf(to, cond) => {
                    let val = self.stack.pop().unwrap();
                    if val.is_truthy() == *cond {
                        pc = *to;
                        continue;
                    }
                }
                Instr::BinOp(op, span) => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    let result = left.bin_op(right, *op, span)?;
                    self.stack.push(result);
                }
                Instr::DestructureList(n, span) => {
                    let list = match self.stack.pop().unwrap() {
                        Value::List(list) if *n == list.len() => list,
                        Value::List(list) => {
                            let message =
                                format!("cannot destructure list of {} as list of {n}", list.len());
                            return Err(span.clone().sp(message).into());
                        }
                        val => {
                            let message =
                                format!("cannot destructure {} as list of {}", val.ty(), n);
                            return Err(span.clone().sp(message).into());
                        }
                    };
                    for val in list.into_iter().rev() {
                        self.stack.push(val);
                    }
                }
                Instr::Dud => {
                    panic!("unresolved instruction")
                }
            }
            // println!("{:?}", self.stack);
            pc += 1;
        }
        println!("\nstack:");
        for val in &self.stack {
            println!("{:?}", val);
        }
        Ok(())
    }
}

pub struct Compiler {
    function_instrs: Vec<Instr>,
    global_instrs: Vec<Instr>,
    checker: Checker,
    scopes: Vec<Scope>,
    height: usize,
}

#[derive(Default)]
struct Scope {
    functions: HashMap<FunctionId, usize>,
    bindings: HashMap<Ident, usize>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            function_instrs: Vec::new(),
            global_instrs: vec![Instr::Comment("BEGIN".into())],
            checker: Checker::new(),
            scopes: vec![Scope::default()],
            height: 0,
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn load(&mut self, input: &str, path: &Path) -> UiuaResult {
        let items = self.checker.load(input, path)?;
        for item in items {
            self.item(item)?;
        }
        Ok(())
    }
    pub fn finish(mut self) -> (Vec<Instr>, usize) {
        let start = self.function_instrs.len();
        self.function_instrs.append(&mut self.global_instrs);
        (self.function_instrs, start)
    }
    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    fn push_scope(&mut self) -> usize {
        self.scopes.push(Scope::default());
        self.height
    }
    fn pop_scope(&mut self, height: usize) {
        self.scopes.pop().unwrap();
        self.height = height;
    }
    fn bind(&mut self, ident: Ident) {
        let height = self.height;
        self.scope_mut().bindings.insert(ident, height);
    }
    fn instrs(&self) -> &[Instr] {
        if self.scopes.len() <= 1 {
            &self.global_instrs
        } else {
            &self.function_instrs
        }
    }
    fn instrs_mut(&mut self) -> &mut Vec<Instr> {
        if self.scopes.len() <= 1 {
            &mut self.global_instrs
        } else {
            &mut self.function_instrs
        }
    }
    fn push_instr(&mut self, instr: Instr) {
        match &instr {
            Instr::Copy(_) => self.height += 1,
            Instr::Push(_) => self.height += 1,
            Instr::BinOp(..) => self.height -= 1,
            Instr::Call(..) => self.height -= 1,
            Instr::JumpIf(..) => self.height -= 1,
            _ => {}
        }
        self.instrs_mut().push(instr);
    }
    fn item(&mut self, item: Item) -> UiuaResult {
        match item {
            Item::Expr(expr) => self.expr(expr),
            Item::Binding(binding) => self.binding(binding),
            Item::FunctionDef(def) => self.function_def(def),
        }
    }
    fn function_def(&mut self, def: FunctionDef) -> UiuaResult {
        let index = self.function_instrs.len();
        self.scope_mut()
            .functions
            .insert(def.func.id.clone(), index);
        self.function(def.func)?;
        Ok(())
    }
    fn function(&mut self, func: Function) -> UiuaResult {
        let height = self.push_scope();
        self.push_instr(Instr::Comment(match func.id {
            FunctionId::Named(name) => format!("fn {name}"),
            FunctionId::Anonymous(span) => format!("fn at {span}"),
        }));
        for param in func.params {
            self.bind(param.value);
            self.height += 1;
        }
        self.block(func.body)?;
        self.push_instr(Instr::Return);
        self.pop_scope(height);
        Ok(())
    }
    fn binding(&mut self, binding: Binding) -> UiuaResult {
        self.expr(binding.expr)?;
        self.pattern(binding.pattern)?;
        Ok(())
    }
    fn pattern(&mut self, pattern: Sp<Pattern>) -> UiuaResult {
        match pattern.value {
            Pattern::Ident(ident) => self.bind(ident),
            Pattern::List(patterns) => {
                let len = patterns.len();
                for pattern in patterns {
                    self.pattern(pattern)?;
                }
                self.push_instr(Instr::DestructureList(len, pattern.span.clone()));
            }
        }
        Ok(())
    }
    fn expr(&mut self, expr: Sp<Expr>) -> UiuaResult {
        self.expr_maybe_function(expr, false)
    }
    fn expr_maybe_function(&mut self, expr: Sp<Expr>, function: bool) -> UiuaResult {
        match expr.value {
            Expr::Unit => self.push_instr(Instr::Push(Value::Unit)),
            Expr::Bool(b) => self.push_instr(Instr::Push(Value::Bool(b))),
            Expr::Nat(n) => self.push_instr(Instr::Push(Value::Nat(n))),
            Expr::Int(i) => self.push_instr(Instr::Push(Value::Int(i))),
            Expr::Real(r) => self.push_instr(Instr::Push(Value::Real(r))),
            Expr::Ident(ident) => {
                if function {
                    let index = *self
                        .scopes
                        .iter()
                        .rev()
                        .find_map(|scope| scope.functions.get(&FunctionId::Named(ident)))
                        .unwrap();
                    self.push_instr(Instr::Push(Value::Function(index)));
                } else {
                    let index = *self
                        .scopes
                        .iter()
                        .rev()
                        .find_map(|scope| scope.bindings.get(&ident))
                        .unwrap();
                    let curr = self.height;
                    let diff = curr - index;
                    self.push_instr(Instr::Copy(diff));
                }
            }
            Expr::Binary(bin) => {
                self.expr(bin.left)?;
                self.expr(bin.right)?;
                self.push_instr(Instr::BinOp(bin.op.value, bin.op.span.clone()));
            }
            Expr::Call(call) => {
                let args_len = call.args.len();
                for arg in call.args {
                    self.expr(arg)?;
                }
                let call_span = call.func.span.clone();
                self.expr_maybe_function(call.func, true)?;
                self.push_instr(Instr::Call(args_len, call_span));
            }
            Expr::If(if_expr) => self.if_expr(*if_expr)?,
            _ => todo!(),
        }
        Ok(())
    }
    fn block(&mut self, block: Block) -> UiuaResult {
        let height = self.push_scope();
        for binding in block.bindings {
            self.binding(binding)?;
        }
        self.expr(block.expr)?;
        self.pop_scope(height);
        Ok(())
    }
    fn if_expr(&mut self, if_expr: IfExpr) -> UiuaResult {
        self.expr(if_expr.cond)?;
        let jump_to_else_spot = self.instrs().len();
        self.push_instr(Instr::Dud);
        self.height -= 1;
        self.block(if_expr.if_true)?;
        let jump_to_end_spot = self.instrs().len();
        self.push_instr(Instr::Dud);
        self.instrs_mut()[jump_to_else_spot] = Instr::JumpIf(self.instrs().len(), false);
        self.block(if_expr.if_false)?;
        self.instrs_mut()[jump_to_end_spot] = Instr::Jump(self.instrs().len());
        Ok(())
    }
}
