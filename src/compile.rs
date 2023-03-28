use std::{collections::HashMap, fmt, fs, path::Path};

use crate::{
    ast::*,
    function::{Function, FunctionId, Instr},
    io::{IoBackend, StdIo},
    lex::{Sp, Span},
    parse::{parse, ParseError},
    primitive::Primitive,
    value::Value,
    vm::{dprintln, Vm},
    Env, Ident, RuntimeError, UiuaError, UiuaResult,
};

pub struct Assembly {
    pub(crate) instrs: Vec<Instr>,
    pub(crate) spans: Vec<Span>,
}

impl Assembly {
    pub fn load_file<P: AsRef<Path>>(path: P) -> UiuaResult<Self> {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        Self::load(&input, path)
    }
    pub fn load<P: AsRef<Path>>(input: &str, path: P) -> UiuaResult<Self> {
        let mut compiler = Compiler::default();
        compiler.load_impl(input, Some(path.as_ref()))?;
        Ok(compiler.assembly)
    }
    pub fn load_str(input: &str) -> UiuaResult<Self> {
        let mut compiler = Compiler::default();
        compiler.load_impl(input, None)?;
        Ok(compiler.assembly)
    }
    pub fn run_with_backend<B: IoBackend>(&self, backend: B) -> UiuaResult<(Vec<Value>, B)> {
        let mut vm = Vm::new(backend);
        self.run_with_vm(&mut vm)?;
        Ok((vm.stack, vm.io))
    }
    pub fn run(&self) -> UiuaResult<Vec<Value>> {
        let mut vm = Vm::<StdIo>::default();
        self.run_with_vm(&mut vm)?;
        Ok(vm.stack)
    }
    fn run_with_vm<B: IoBackend>(&self, vm: &mut Vm<B>) -> UiuaResult {
        for (i, instr) in self.instrs.iter().enumerate() {
            dprintln!("{i:>3}: {instr}");
        }
        vm.run_assembly(self)?;
        dprintln!("stack:");
        for val in &vm.stack {
            dprintln!("  {val:?}");
        }
        Ok(())
    }
    pub(crate) fn error(&self, span: usize, msg: impl Into<String>) -> RuntimeError {
        self.spans[span].error(msg.into())
    }
    pub fn env(&self) -> Env {
        Env {
            assembly: self,
            span: 0,
        }
    }
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidNumber(String),
    UnknownBinding(Ident),
    ConstEval(UiuaError),
    RefOutsideContext(usize),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::Parse(e) => write!(f, "{e}"),
            CompileError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            CompileError::InvalidNumber(s) => write!(f, "invalid real: {s}"),
            CompileError::UnknownBinding(s) => write!(f, "unknown binding: {s}"),
            CompileError::ConstEval(e) => write!(f, "{e}"),
            CompileError::RefOutsideContext(n) => write!(
                f,
                "`{}` is referenced outside a context",
                (*n as u8 + b'a') as char
            ),
        }
    }
}

impl From<ParseError> for CompileError {
    fn from(e: ParseError) -> Self {
        CompileError::Parse(e)
    }
}

impl From<UiuaError> for CompileError {
    fn from(e: UiuaError) -> Self {
        CompileError::ConstEval(e)
    }
}

pub(crate) struct Compiler {
    /// Instructions for functions that are currently being compiled
    in_progress_functions: Vec<InProgressFunction>,
    /// Values and functions that are bound
    bindings: HashMap<Ident, Bound>,
    /// Errors that don't stop compilation
    pub(crate) errors: Vec<Sp<CompileError>>,
    /// The partially compiled assembly
    assembly: Assembly,
}

#[derive(Debug, Clone)]
enum Bound {
    /// A primitive
    Primitive(Primitive),
    /// A global variable is referenced by its index in the global array.
    Global(usize, bool),
    /// A dud binding used when a binding lookup fails
    Error,
}

impl Default for Compiler {
    fn default() -> Self {
        let mut bindings = HashMap::new();
        // Initialize primitives
        for prim in Primitive::ALL {
            if let Some(name) = prim.name() {
                bindings.insert(name.into(), Bound::Primitive(prim));
            }
        }

        Self {
            in_progress_functions: Vec::new(),
            bindings,
            errors: Vec::new(),
            assembly: Assembly {
                instrs: Vec::new(),
                spans: vec![Span::Builtin],
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub id: FunctionId,
    pub params: usize,
}

struct InProgressFunction {
    instrs: Vec<Instr>,
    refs: Vec<(usize, Span)>,
}

impl Compiler {
    fn load_impl(&mut self, input: &str, path: Option<&Path>) -> UiuaResult {
        let (items, errors) = parse(input, path);
        let mut errors: Vec<Sp<CompileError>> = errors
            .into_iter()
            .map(|e| e.map(CompileError::Parse))
            .collect();
        let global_start = self.assembly.instrs.len();
        for item in items {
            self.item(item);
        }
        errors.append(&mut self.errors);
        if errors.is_empty() {
            Ok(())
        } else {
            self.assembly.instrs.truncate(global_start);
            Err(errors.into())
        }
    }
    fn push_instr(&mut self, instr: Instr) {
        if let Some(ipf) = self.in_progress_functions.last_mut() {
            ipf.instrs.push(instr);
        } else {
            self.assembly.instrs.push(instr);
        }
    }
    fn push_call_span(&mut self, span: Span) -> usize {
        let spot = self.assembly.spans.len();
        self.assembly.spans.push(span);
        spot
    }
    pub(crate) fn item(&mut self, item: Item) {
        match item {
            Item::Words(words) => self.words(words, true),
            Item::Binding(binding) => self.binding(binding),
            Item::Comment(_) | Item::Newlines => {}
        }
    }
    fn binding(&mut self, binding: Binding) {
        let function = if binding.name.value.is_capitalized() {
            self.func(Func {
                id: FunctionId::Named(binding.name.value.clone()),
                body: binding.words,
            });
            true
        } else {
            self.words(binding.words, true);
            false
        };
        let index = self
            .bindings
            .values()
            .filter(|b| matches!(b, Bound::Global(..)))
            .count();
        self.bindings
            .insert(binding.name.value, Bound::Global(index, function));
        let span = self.push_call_span(binding.name.span);
        self.push_instr(Instr::BindGlobal(span));
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) {
        for word in words.into_iter().rev() {
            self.word(word, call);
        }
    }
    fn word(&mut self, word: Sp<Word>, call: bool) {
        match word.value {
            Word::Number(s) => {
                let f: f64 = match s.parse() {
                    Ok(f) => f,
                    Err(_) => {
                        self.errors
                            .push(word.span.sp(CompileError::InvalidNumber(s)));
                        0.0
                    }
                };
                self.push_instr(Instr::Push(f.into()));
            }
            Word::Char(c) => self.push_instr(Instr::Push(c.into())),
            Word::String(s) => self.push_instr(Instr::Push(s.into())),
            Word::Ident(ident) => self.ident(ident, word.span, call),
            Word::Array(items) => {
                self.push_instr(Instr::BeginArray);
                self.words(items, true);
                let span = self.push_call_span(word.span);
                self.push_instr(Instr::EndArray(true, span));
            }
            Word::Strand(items) => {
                self.push_instr(Instr::BeginArray);
                self.words(items, false);
                let span = self.push_call_span(word.span);
                self.push_instr(Instr::EndArray(false, span));
            }
            Word::Func(func) => self.func(func),
            Word::RefFunc(func) => self.ref_func(func, word.span),
            Word::Primitive(prim) => self.primitive(prim, word.span, call),
            Word::Modified(m) => self.modified(*m, call),
        }
    }
    fn ident(&mut self, ident: Ident, span: Span, call: bool) {
        let bound = match self.bindings.get(&ident) {
            Some(bind) => bind,
            None => {
                let name = ident.as_str();
                if let Some(prim) = Primitive::from_name(name) {
                    return self.primitive(prim, span, call);
                }
                if let Some(ipf) = self.in_progress_functions.last_mut() {
                    if name.len() == 1 {
                        let c = name.chars().next().unwrap();
                        if c.is_ascii_lowercase() {
                            let n = (c as u8 - b'a') as usize;
                            ipf.refs.push((n, span.clone()));
                            let span = self.push_call_span(span);
                            self.push_instr(Instr::CopyRef(n, span));
                            return;
                        }
                    }
                }
                self.errors
                    .push(span.clone().sp(CompileError::UnknownBinding(ident.clone())));
                &Bound::Error
            }
        };
        match bound.clone() {
            Bound::Global(index, _) => self.push_instr(Instr::CopyGlobal(index)),
            Bound::Primitive(prim) => return self.primitive(prim, span, call),
            Bound::Error => self.push_instr(Instr::Push(Value::default())),
        }
        if call {
            let span = self.push_call_span(span);
            self.push_instr(Instr::Call(span));
        }
    }
    fn func_outer(
        &mut self,
        id: FunctionId,
        refs_span: Option<Span>,
        inner: impl FnOnce(&mut Self),
    ) {
        // Initialize the function's instruction list
        self.in_progress_functions.push(InProgressFunction {
            instrs: Vec::new(),
            refs: Vec::new(),
        });
        // Compile the function's body
        inner(self);
        // Add the function's instructions to the global function list
        let ipf = self.in_progress_functions.pop().unwrap();
        // Push the function
        self.push_instr(Instr::Push(
            Function {
                id,
                instrs: ipf.instrs,
            }
            .into(),
        ));
        // Call as reference if necessary
        if let Some(span) = refs_span {
            let max_ref = ipf.refs.iter().map(|(n, _)| *n).max().unwrap_or(0) + 1;
            let span = self.push_call_span(span);
            self.push_instr(Instr::CallRef(max_ref, span));
        } else if !ipf.refs.is_empty() {
            for (n, span) in ipf.refs {
                self.errors
                    .push(span.sp(CompileError::RefOutsideContext(n)));
            }
        }
    }
    fn func(&mut self, func: Func) {
        if func.body.len() == 1 {
            if let Word::Ident(ident) = &func.body[0].value {
                if let Some(Bound::Global(i, true)) = self.bindings.get(ident) {
                    self.push_instr(Instr::CopyGlobal(*i));
                    return;
                }
            }
        }
        self.func_outer(func.id, None, |this| this.words(func.body, true));
    }
    fn ref_func(&mut self, func: Func, span: Span) {
        self.func_outer(func.id, Some(span), |this| this.words(func.body, true));
    }
    fn primitive(&mut self, prim: Primitive, span: Span, call: bool) {
        let span = self.push_call_span(span);
        let instr = Instr::Primitive(prim, span);
        if call {
            self.push_instr(instr);
        } else {
            self.push_instr(Instr::Push(
                Function {
                    id: FunctionId::Primitive(prim),
                    instrs: vec![instr],
                }
                .into(),
            ));
        }
    }
    fn modified(&mut self, modified: Modified, call: bool) {
        let span = modified.modifier.span.clone();
        let id = FunctionId::Anonymous(
            modified
                .modifier
                .span
                .clone()
                .merge(modified.words.last().unwrap().span.clone()),
        );
        self.func_outer(id, None, |this| {
            for word in modified.words.into_iter().rev() {
                this.word(word, false);
            }
            this.primitive(modified.modifier.value, modified.modifier.span, true);
        });
        if call {
            let span = self.push_call_span(span);
            self.push_instr(Instr::Call(span));
        }
    }
}
