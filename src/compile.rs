use std::{collections::HashMap, fmt, fs, path::Path};

use crate::{
    ast::*,
    lex::{Ident, Sp},
    parse::{parse, ParseError},
    value::{Function, Value},
    vm::{run_instrs, Instr},
    UiuaError, UiuaResult,
};

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
    UnknownBinding(Ident),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::Parse(e) => write!(f, "{e}"),
            CompileError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            CompileError::InvalidReal(s) => write!(f, "invalid real: {s}"),
            CompileError::UnknownBinding(s) => write!(f, "unknown binding: {s}"),
        }
    }
}

pub type CompileResult<T = ()> = Result<T, Sp<CompileError>>;

pub struct Compiler {
    function_instrs: Vec<Instr>,
    global_instrs: Vec<Instr>,
    function_info: HashMap<Function, FunctionInfo>,
    scopes: Vec<Scope>,
    height: usize,
    in_function: bool,
    errors: Vec<Sp<CompileError>>,
}

#[derive(Default)]
struct Scope {
    functions: HashMap<FunctionId, Function>,
    bindings: HashMap<Ident, ScopeBind>,
}

enum ScopeBind {
    Function(FunctionId),
    Local(usize),
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            function_instrs: Vec::new(),
            global_instrs: vec![Instr::Comment("BEGIN".into())],
            function_info: HashMap::new(),
            scopes: vec![Scope::default()],
            height: 0,
            in_function: false,
            errors: Vec::new(),
        }
    }
}

pub struct Assembly {
    pub(crate) instrs: Vec<Instr>,
    pub(crate) start: usize,
    function_info: HashMap<Function, FunctionInfo>,
    global_functions: HashMap<Ident, Function>,
}

impl Assembly {
    pub fn lookup_function(&self, name: Ident) -> Option<Function> {
        self.global_functions.get(&name).copied()
    }
    #[track_caller]
    pub fn function_info(&self, function: Function) -> &FunctionInfo {
        if let Some(info) = self.function_info.get(&function) {
            info
        } else {
            panic!("function was compiled in a different assembly")
        }
    }
    pub fn run(&self) -> UiuaResult {
        run_instrs(&self.instrs, self.start, Vec::new())
    }
    pub fn run_function<A: IntoIterator<Item = Value>>(
        &self,
        function: Function,
        args: A,
    ) -> UiuaResult {
        let info = self.function_info(function);
        let args: Vec<Value> = args.into_iter().collect();
        if args.len() != info.args {
            return Err(UiuaError::NotEnoughArgsOnEntry(info.args, args.len()));
        }
        run_instrs(&self.instrs, function.0, args)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub id: FunctionId,
    pub args: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        self.load(&input, path)?;
        Ok(())
    }
    pub fn load(&mut self, input: &str, path: &Path) -> Result<(), Vec<Sp<CompileError>>> {
        let (items, errors) = parse(input, path);
        let mut errors: Vec<Sp<CompileError>> = errors
            .into_iter()
            .map(|e| e.map(CompileError::Parse))
            .collect();
        let function_start = self.function_instrs.len();
        let global_start = self.global_instrs.len();
        for item in items {
            if let Err(e) = self.item(item) {
                errors.push(e);
            }
        }
        errors.append(&mut self.errors);
        if errors.is_empty() {
            Ok(())
        } else {
            self.function_instrs.truncate(function_start);
            self.global_instrs.truncate(global_start);
            Err(errors)
        }
    }
    pub fn finish(mut self) -> Assembly {
        let start = self.function_instrs.len();
        self.function_instrs.append(&mut self.global_instrs);
        let mut global_functions = HashMap::new();
        for (id, index) in self.scopes.pop().unwrap().functions {
            if let FunctionId::Named(name) = id {
                global_functions.insert(name, index);
            }
        }
        for (i, instr) in self.function_instrs.iter().enumerate() {
            println!("{i:>3} {instr}");
        }
        Assembly {
            instrs: self.function_instrs,
            start,
            function_info: self.function_info,
            global_functions,
        }
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
    /// Bind the value on the top of the stack to the given identifier
    fn bind_local(&mut self, ident: Ident) {
        let height = self.height - 1;
        self.scope_mut()
            .bindings
            .insert(ident, ScopeBind::Local(height));
    }
    fn bind_function(&mut self, ident: Ident, id: FunctionId) {
        self.scope_mut()
            .bindings
            .insert(ident, ScopeBind::Function(id));
    }
    fn instrs(&self) -> &[Instr] {
        if self.in_function {
            &self.function_instrs
        } else {
            &self.global_instrs
        }
    }
    fn instrs_mut(&mut self) -> &mut Vec<Instr> {
        if self.in_function {
            &mut self.function_instrs
        } else {
            &mut self.global_instrs
        }
    }
    fn push_instr(&mut self, instr: Instr) {
        match &instr {
            Instr::Copy(_) => self.height += 1,
            Instr::Push(_) => self.height += 1,
            Instr::BinOp(..) => self.height -= 1,
            Instr::Call(..) => self.height -= 1,
            _ => {}
        }
        self.instrs_mut().push(instr);
    }
    fn push_spot(&mut self) -> usize {
        let spot = self.instrs().len();
        self.push_instr(Instr::Dud);
        spot
    }
    fn item(&mut self, item: Item) -> CompileResult {
        match item {
            Item::Expr(expr) => self.expr(expr),
            Item::Binding(binding) => self.binding(binding),
            Item::FunctionDef(def) => self.function_def(def),
        }
    }
    fn function_def(&mut self, def: FunctionDef) -> CompileResult {
        self.bind_function(def.name.value, def.func.id.clone());
        self.func(def.func)?;
        Ok(())
    }
    fn func(&mut self, func: Func) -> CompileResult {
        self.in_function = true;
        let function = Function(self.function_instrs.len());
        self.scope_mut().functions.insert(func.id.clone(), function);
        self.function_info.insert(
            function,
            FunctionInfo {
                id: func.id.clone(),
                args: func.params.len(),
            },
        );
        let height = self.push_scope();
        self.push_instr(Instr::Comment(match func.id {
            FunctionId::Named(name) => format!("fn {name}"),
            FunctionId::Anonymous(span) => format!("fn at {span}"),
        }));
        for param in func.params.into_iter().rev() {
            self.height += 1;
            self.bind_local(param.value);
        }
        self.block(func.body)?;
        self.push_instr(Instr::Return);
        self.pop_scope(height);
        self.in_function = false;
        Ok(())
    }
    fn binding(&mut self, binding: Binding) -> CompileResult {
        self.expr(binding.expr)?;
        self.pattern(binding.pattern)?;
        Ok(())
    }
    fn pattern(&mut self, pattern: Sp<Pattern>) -> CompileResult {
        match pattern.value {
            Pattern::Ident(ident) => self.bind_local(ident),
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
    fn expr(&mut self, expr: Sp<Expr>) -> CompileResult {
        match expr.value {
            Expr::Unit => self.push_instr(Instr::Push(Value::unit())),
            Expr::Bool(b) => self.push_instr(Instr::Push(Value::bool(b))),
            Expr::Int(s) => {
                let i: i64 = s
                    .parse()
                    .map_err(|_| expr.span.sp(CompileError::InvalidInteger(s)))?;
                self.push_instr(Instr::Push(Value::int(i)));
            }
            Expr::Real(s) => {
                let r: f64 = s
                    .parse()
                    .map_err(|_| expr.span.sp(CompileError::InvalidReal(s)))?;
                self.push_instr(Instr::Push(Value::real(r)))
            }
            Expr::Ident(ident) => {
                let bind = self
                    .scopes
                    .iter()
                    .rev()
                    .find_map(|scope| scope.bindings.get(&ident))
                    .unwrap_or_else(|| panic!("unbound variable `{ident}`"));
                match bind {
                    ScopeBind::Local(index) => {
                        let curr = self.height;
                        let diff = curr - *index;
                        self.push_instr(Instr::Copy(diff));
                    }
                    ScopeBind::Function(id) => {
                        let function = *self
                            .scopes
                            .iter()
                            .rev()
                            .find_map(|scope| scope.functions.get(id))
                            .unwrap_or_else(|| panic!("unbound function `{id:?}`"));
                        self.push_instr(Instr::Push(Value::function(function)));
                    }
                }
            }
            Expr::Bin(bin) => {
                self.expr(bin.left)?;
                self.expr(bin.right)?;
                self.push_instr(Instr::BinOp(bin.op.value, bin.op.span.clone()));
            }
            Expr::Call(call) => self.call(*call)?,
            Expr::If(if_expr) => self.if_expr(*if_expr)?,
            Expr::Logic(log_expr) => self.logic_expr(*log_expr)?,
            Expr::List(_) => todo!(),
            Expr::Parened(inner) => self.expr(expr.span.sp(*inner))?,
        }
        Ok(())
    }
    fn call(&mut self, call: CallExpr) -> CompileResult {
        let args_len = call.args.len();
        for arg in call.args.into_iter().rev() {
            self.expr(arg)?;
        }
        let call_span = call.func.span.clone();
        self.expr(call.func)?;
        self.push_instr(Instr::Call(args_len, call_span));
        Ok(())
    }
    fn block(&mut self, block: Block) -> CompileResult {
        let height = self.push_scope();
        for binding in block.bindings {
            self.binding(binding)?;
        }
        self.expr(block.expr)?;
        self.pop_scope(height);
        Ok(())
    }
    fn if_expr(&mut self, if_expr: IfExpr) -> CompileResult {
        self.expr(if_expr.cond)?;
        let jump_to_else_spot = self.push_spot();
        self.height -= 1;
        self.block(if_expr.if_true)?;
        let jump_to_end_spot = self.push_spot();
        self.instrs_mut()[jump_to_else_spot] = Instr::PopJumpIf(
            self.instrs().len() as isize - jump_to_else_spot as isize,
            false,
        );
        self.block(if_expr.if_false)?;
        self.instrs_mut()[jump_to_end_spot] =
            Instr::Jump(self.instrs().len() as isize - jump_to_end_spot as isize);
        Ok(())
    }
    fn logic_expr(&mut self, log_expr: LogicExpr) -> CompileResult {
        self.expr(log_expr.left)?;
        let jump_spot = self.push_spot();
        self.height -= 1;
        self.expr(log_expr.right)?;
        let jump_cond = match log_expr.op.value {
            LogicOp::And => false,
            LogicOp::Or => true,
        };
        self.instrs_mut()[jump_spot] =
            Instr::JumpIfElsePop(self.instrs().len() as isize - jump_spot as isize, jump_cond);
        Ok(())
    }
}
