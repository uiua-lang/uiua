use std::{collections::HashMap, fmt, fs, path::Path};

use nohash_hasher::BuildNoHashHasher;

use crate::{
    ast::*,
    lex::{Ident, Sp},
    parse::{parse, ParseError},
    value::{Function, Value},
    vm::{run_assembly, Instr},
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
    in_progress_functions: Vec<Vec<Instr>>,
    function_info: HashMap<Function, FunctionInfo, BuildNoHashHasher<Function>>,
    scopes: Vec<Scope>,
    height: usize,
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
            in_progress_functions: Vec::new(),
            function_info: HashMap::default(),
            scopes: vec![Scope::default()],
            height: 0,
            errors: Vec::new(),
        }
    }
}

pub struct Assembly {
    pub(crate) instrs: Vec<Instr>,
    pub(crate) start: usize,
    function_info: HashMap<Function, FunctionInfo, BuildNoHashHasher<Function>>,
}

impl Assembly {
    #[track_caller]
    pub fn function_info(&self, function: Function) -> &FunctionInfo {
        if let Some(info) = self.function_info.get(&function) {
            info
        } else {
            panic!("function was compiled in a different assembly")
        }
    }
    pub fn run(&self) -> UiuaResult {
        run_assembly(self)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub id: FunctionId,
    pub params: usize,
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
        for (i, instr) in self.function_instrs.iter().enumerate() {
            println!("{i:>3} {instr}");
        }
        Assembly {
            instrs: self.function_instrs,
            start,
            function_info: self.function_info,
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
    fn instrs(&self) -> &[Instr] {
        self.in_progress_functions
            .last()
            .unwrap_or(&self.global_instrs)
    }
    fn instrs_mut(&mut self) -> &mut Vec<Instr> {
        self.in_progress_functions
            .last_mut()
            .unwrap_or(&mut self.global_instrs)
    }
    fn push_instr(&mut self, instr: Instr) {
        match &instr {
            Instr::Copy(_) => self.height += 1,
            Instr::Push(_) => self.height += 1,
            Instr::PushUnresolvedFunction(_) => self.height += 1,
            Instr::BinOp(..) => self.height -= 1,
            Instr::Call(args, _) => self.height -= *args,
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
        self.scope_mut()
            .bindings
            .insert(def.name.value, ScopeBind::Function(def.func.id.clone()));
        self.func(def.func, false)?;
        Ok(())
    }
    fn func(&mut self, func: Func, push: bool) -> CompileResult {
        // Initialize the function's instruction list
        self.in_progress_functions.push(Vec::new());
        // Push the function's scope
        let height = self.push_scope();
        // Push the function's name as a comment
        self.push_instr(Instr::Comment(match &func.id {
            FunctionId::Named(name) => format!("fn {name}"),
            FunctionId::Anonymous(span) => format!("fn at {span}"),
        }));
        // Push and bind the function's parameters
        let params = func.params.len();
        for param in func.params.into_iter().rev() {
            self.height += 1;
            self.bind_local(param.value);
        }
        // Compile the function's body
        self.block(func.body)?;
        self.push_instr(Instr::Return);
        // Pop the function's scope
        self.pop_scope(height);
        //Determine the function's index
        let function = Function(self.function_instrs.len());
        // Resolve function references
        for instrs in &mut self.in_progress_functions {
            for instr in instrs {
                if matches!(instr, Instr::PushUnresolvedFunction(id) if *id == func.id) {
                    *instr = Instr::Push(function.into());
                }
            }
        }
        // Add the function's instructions to the global function list
        let instrs = self.in_progress_functions.pop().unwrap();
        self.function_instrs.extend(instrs);
        self.scope_mut().functions.insert(func.id.clone(), function);
        // Add to the function info map
        self.function_info.insert(
            function,
            FunctionInfo {
                id: func.id,
                params,
            },
        );
        // Push the function if necessary
        if push {
            self.push_instr(Instr::Push(function.into()));
        }
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
            Expr::Bool(b) => self.push_instr(Instr::Push(b.into())),
            Expr::Int(s) => {
                let i: i64 = s
                    .parse()
                    .map_err(|_| expr.span.sp(CompileError::InvalidInteger(s)))?;
                self.push_instr(Instr::Push(i.into()));
            }
            Expr::Real(s) => {
                let r: f64 = s
                    .parse()
                    .map_err(|_| expr.span.sp(CompileError::InvalidReal(s)))?;
                self.push_instr(Instr::Push(r.into()))
            }
            Expr::Ident(ident) => {
                let bind = self
                    .scopes
                    .iter()
                    .rev()
                    .find_map(|scope| scope.bindings.get(&ident))
                    .ok_or_else(|| expr.span.sp(CompileError::UnknownBinding(ident)))?;
                match bind {
                    ScopeBind::Local(index) => {
                        let curr = self.height;
                        let diff = curr - *index;
                        self.push_instr(Instr::Copy(diff));
                    }
                    ScopeBind::Function(id) => {
                        if let Some(function) = self
                            .scopes
                            .iter()
                            .rev()
                            .find_map(|scope| scope.functions.get(id))
                            .cloned()
                        {
                            self.push_instr(Instr::Push(function.into()));
                        } else {
                            self.push_instr(Instr::PushUnresolvedFunction(id.clone()));
                        }
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
            Expr::Func(func) => self.func(*func, true)?,
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
