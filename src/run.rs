use std::{collections::HashMap, fs, mem::take, path::Path, rc::Rc};

use crate::{
    array::Array,
    ast::*,
    function::{Function, FunctionId, Instr},
    lex::{Sp, Span},
    parse::parse,
    primitive::Primitive,
    value::Value,
    Ident, IoBackend, StdIo, UiuaError, UiuaResult,
};

pub struct Uiua<'io> {
    spans: Vec<Span>,
    stack: Vec<Value>,
    antistack: Vec<Value>,
    array_stack: Vec<usize>,
    ref_stack: Vec<Vec<Value>>,
    globals: Vec<Value>,
    global_names: HashMap<Ident, usize>,
    new_functions: Vec<Vec<Instr>>,
    new_refs: Vec<Vec<u8>>,
    call_stack: Vec<StackFrame>,
    pub(crate) io: &'io dyn IoBackend,
}

struct StackFrame {
    function: Rc<Function>,
    call_span: usize,
    pc: usize,
    spans: Vec<usize>,
}

impl<'io> Default for Uiua<'io> {
    fn default() -> Self {
        Uiua {
            spans: vec![Span::Builtin],
            stack: Vec::new(),
            antistack: Vec::new(),
            array_stack: Vec::new(),
            ref_stack: Vec::new(),
            globals: Vec::new(),
            global_names: HashMap::new(),
            new_functions: Vec::new(),
            new_refs: Vec::new(),
            call_stack: Vec::new(),
            io: &StdIo,
        }
    }
}

impl<'io> Uiua<'io> {
    pub fn with_stdio() -> Self {
        Default::default()
    }
    pub fn with_backend(io: &'io dyn IoBackend) -> Self {
        Uiua {
            io,
            ..Default::default()
        }
    }
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<&mut Self> {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        self.load_impl(&input, Some(path))
    }
    pub fn load_str(&mut self, input: &str) -> UiuaResult<&mut Self> {
        self.load_impl(input, None)
    }
    fn load_impl(&mut self, input: &str, path: Option<&Path>) -> UiuaResult<&mut Self> {
        let (items, errors) = parse(input, path);
        if !errors.is_empty() {
            return Err(errors.into());
        }
        for item in items {
            self.item(item)?;
        }
        Ok(self)
    }
    fn item(&mut self, item: Item) -> UiuaResult {
        match item {
            Item::Words(words) => {
                let instrs = self.compile_words(words)?;
                self.exec_global_instrs(instrs)?;
            }
            Item::Binding(binding) => self.binding(binding)?,
            Item::Newlines => {}
            Item::Comment(_) => {}
        }
        Ok(())
    }
    fn push_span(&mut self, span: Span) -> usize {
        let idx = self.spans.len();
        self.spans.push(span);
        idx
    }
    fn binding(&mut self, binding: Binding) -> UiuaResult {
        let instrs = self.compile_words(binding.words)?;
        let val = if binding.name.value.is_capitalized() {
            let func = Function {
                id: FunctionId::Named(binding.name.value.clone()),
                instrs,
            };
            Value::from(func)
        } else {
            self.exec_global_instrs(instrs)?;
            self.stack.pop().unwrap_or_default()
        };
        let idx = self.globals.len();
        self.globals.push(val);
        self.global_names.insert(binding.name.value, idx);
        Ok(())
    }
    fn compile_words(&mut self, words: Vec<Sp<Word>>) -> UiuaResult<Vec<Instr>> {
        self.new_functions.push(Vec::new());
        self.words(words)?;
        Ok(self.new_functions.pop().unwrap())
    }
    fn words(&mut self, words: Vec<Sp<Word>>) -> UiuaResult {
        for word in words.into_iter().rev() {
            self.word(word)?;
        }
        Ok(())
    }
    fn push_instr(&mut self, instr: Instr) {
        let instrs = self.new_functions.last_mut().unwrap();
        match (instrs.last_mut(), instr) {
            (Some(Instr::Primitive(last, _)), Instr::Primitive(new, new_span)) => {
                match (&last, new) {
                    (Primitive::Reverse, Primitive::First) => *last = Primitive::Last,
                    (Primitive::Reverse, Primitive::Last) => *last = Primitive::First,
                    (a, b)
                        if a.args() == a.outputs()
                            && b.args() == b.outputs()
                            && a.inverse() == Some(b) =>
                    {
                        instrs.pop();
                    }
                    _ => instrs.push(Instr::Primitive(new, new_span)),
                }
            }
            (_, Instr::Primitive(Primitive::Noop, _)) => {}
            (_, instr) => instrs.push(instr),
        }
    }
    fn word(&mut self, word: Sp<Word>) -> UiuaResult {
        match word.value {
            Word::Number(n) => {
                let n: f64 = n
                    .parse()
                    .map_err(|e| word.span.sp(format!("invalid number {n:?}: {e}")))?;
                self.push_instr(Instr::Push(n.into()));
            }
            Word::Char(c) => self.push_instr(Instr::Push(c.into())),
            Word::String(s) => self.push_instr(Instr::Push(s.into())),
            Word::Ident(ident) => self.ident(ident, word.span)?,
            Word::Strand(items) => {
                self.push_instr(Instr::BeginArray);
                self.words(items)?;
                let span = self.push_span(word.span);
                self.push_instr(Instr::EndArray(false, span));
            }
            Word::Array(items) => {
                self.push_instr(Instr::BeginArray);
                self.words(items)?;
                let span = self.push_span(word.span);
                self.push_instr(Instr::EndArray(true, span));
            }
            Word::Func(func) => self.func(func, word.span)?,
            Word::RefFunc(func) => self.ref_func(func, word.span)?,
            Word::Primitive(p) => {
                let span = self.push_span(word.span);
                self.push_instr(Instr::Primitive(p, span));
            }
            Word::Modified(m) => self.modified(*m, word.span)?,
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: Span) -> UiuaResult {
        if let Some(idx) = self.global_names.get(&ident) {
            self.push_instr(Instr::CopyGlobal(*idx));
        } else if let Some(prim) = Primitive::from_name(ident.as_str()) {
            let span = self.push_span(span);
            self.push_instr(Instr::Primitive(prim, span));
        } else {
            if let Some(refs) = self.new_refs.last_mut() {
                if ident.as_str().len() == 1 {
                    let c = ident.as_str().chars().next().unwrap();
                    if c.is_ascii_lowercase() {
                        let idx = c as u8 - b'a';
                        refs.push(idx);
                        self.push_instr(Instr::CopyRef(idx as usize));
                        return Ok(());
                    }
                }
            }
            return Err(span.sp(format!("unknown identifier {}", ident)).into());
        }
        Ok(())
    }
    fn func(&mut self, func: Func, _span: Span) -> UiuaResult {
        let instrs = self.compile_words(func.body)?;
        let func = Function {
            id: func.id,
            instrs,
        };
        self.push_instr(Instr::Push(func.into()));
        Ok(())
    }
    fn ref_func(&mut self, func: Func, span: Span) -> UiuaResult {
        self.new_refs.push(Vec::new());
        let instrs = self.compile_words(func.body)?;
        let refs = self.new_refs.pop().unwrap();
        let func = Function {
            id: func.id,
            instrs,
        };
        self.push_instr(Instr::Push(func.into()));
        let span = self.push_span(span);
        let ref_size = refs.into_iter().max().unwrap_or(0) + 1;
        self.push_instr(Instr::CallRef(ref_size as usize, span));
        Ok(())
    }
    fn modified(&mut self, _modified: Modified, _span: Span) -> UiuaResult {
        todo!("modified")
    }
    fn exec_global_instrs(&mut self, instrs: Vec<Instr>) -> UiuaResult {
        let func = Function {
            id: FunctionId::Anonymous(Span::Builtin),
            instrs,
        };
        self.exec(StackFrame {
            function: Rc::new(func),
            call_span: 0,
            spans: Vec::new(),
            pc: 0,
        })
    }
    fn exec(&mut self, frame: StackFrame) -> UiuaResult {
        let ret_height = self.call_stack.len();
        self.call_stack.push(frame);
        'outer: while self.call_stack.len() > ret_height {
            let frame = self.call_stack.last_mut().unwrap();
            // println!("frame: {:?}", frame.function.instrs);
            while let Some(instr) = frame.function.instrs.get(frame.pc) {
                // println!("{:?}", self.stack);
                // println!("  {:?}", instr);
                match instr {
                    Instr::Push(val) => self.stack.push(val.clone()),
                    Instr::BeginArray => self.array_stack.push(self.stack.len()),
                    Instr::EndArray(normalize, span) => {
                        let start = self.array_stack.pop().unwrap();
                        if start > self.stack.len() {
                            return Err(self.spans[*span]
                                .clone()
                                .sp("array removed elements".into())
                                .into());
                        }
                        let mut array = Array::from_iter(self.stack.drain(start..));
                        if *normalize {
                            if let Some((a, b)) = array.normalize() {
                                return Err(self.spans[*span]
                                    .clone()
                                    .sp(format!(
                                        "array items have different shapes: {a:?} and {b:?}"
                                    ))
                                    .into());
                            }
                        } else {
                            array.normalize_type();
                        }
                        self.stack.push(array.into());
                    }
                    Instr::CopyGlobal(idx) => self.stack.push(self.globals[*idx].clone()),
                    &Instr::Primitive(prim, span) => {
                        frame.spans.push(span);
                        prim.run(self)?;
                        self.call_stack.last_mut().unwrap().spans.pop();
                        self.call_stack.last_mut().unwrap().pc += 1;
                        continue 'outer;
                    }
                    Instr::Call(span) => {
                        let value = self.stack.pop().unwrap();
                        if value.is_function() {
                            let function = value.into_function();
                            let new_frame = StackFrame {
                                function,
                                call_span: *span,
                                spans: Vec::new(),
                                pc: 0,
                            };
                            frame.pc += 1;
                            self.call_stack.push(new_frame);
                            continue 'outer;
                        } else {
                            self.stack.push(value);
                        }
                    }
                    &Instr::CallRef(n, span) => {
                        let f = self.pop("ref function")?;
                        if self.stack.len() < n {
                            return Err(self.spans[span]
                                .clone()
                                .sp(format!("not enough arguments for reference of {n} values"))
                                .into());
                        }
                        let refs = self.stack.drain(self.stack.len() - n..).collect();
                        self.ref_stack.push(refs);
                        self.stack.push(f);
                        self.call()?;
                        self.call_stack.last_mut().unwrap().pc += 1;
                        continue 'outer;
                    }
                    Instr::CopyRef(n) => {
                        let value = self.ref_stack.last().unwrap()[*n].clone();
                        self.stack.push(value);
                    }
                }
                frame.pc += 1;
            }
            self.call_stack.pop();
        }
        Ok(())
    }
    pub fn call(&mut self) -> UiuaResult {
        let call_span = self.span_index();
        let value = self.pop("called function")?;
        println!("call: {value:?}");
        if value.is_function() {
            let function = value.into_function();
            let new_frame = StackFrame {
                function,
                call_span,
                spans: Vec::new(),
                pc: 0,
            };
            self.exec(new_frame)
        } else {
            self.stack.push(value);
            Ok(())
        }
    }
    fn span_index(&self) -> usize {
        self.call_stack.last().map_or(0, |frame| frame.call_span)
    }
    pub fn span(&self) -> &Span {
        &self.spans[self.span_index()]
    }
    pub fn error(&self, message: impl ToString) -> UiuaError {
        UiuaError::Run(self.span().clone().sp(message.to_string()))
    }
    pub fn pop(&mut self, arg: impl StackArg) -> UiuaResult<Value> {
        self.stack.pop().ok_or_else(|| {
            self.error(format!(
                "Stack was empty when evaluating {}",
                arg.arg_name()
            ))
        })
    }
    pub fn antipop(&mut self, arg: impl StackArg) -> UiuaResult<Value> {
        self.antistack.pop().ok_or_else(|| {
            self.error(format!(
                "Antistack was empty when evaluating {}",
                arg.arg_name()
            ))
        })
    }
    pub fn pop_result(&mut self) -> UiuaResult<Value> {
        self.pop("result")
    }
    pub fn push(&mut self, val: impl Into<Value>) {
        self.stack.push(val.into());
    }
    pub fn antipush(&mut self, val: impl Into<Value>) {
        self.antistack.push(val.into());
    }
    pub fn take_stack(&mut self) -> Vec<Value> {
        take(&mut self.stack)
    }
    pub(crate) fn monadic<V: Into<Value>>(&mut self, f: fn(&Value) -> V) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(&value));
        Ok(())
    }
    pub(crate) fn monadic_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(&value, self)?);
        Ok(())
    }
    pub(crate) fn monadic_mut(&mut self, f: fn(&mut Value)) -> UiuaResult {
        let mut a = self.pop(1)?;
        f(&mut a);
        self.push(a);
        Ok(())
    }
    pub(crate) fn monadic_mut_env(&mut self, f: fn(&mut Value, &Self) -> UiuaResult) -> UiuaResult {
        let mut a = self.pop(1)?;
        f(&mut a, self)?;
        self.push(a);
        Ok(())
    }
    pub(crate) fn dyadic<V: Into<Value>>(&mut self, f: fn(&Value, &Value) -> V) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, &b));
        Ok(())
    }
    pub(crate) fn dyadic_mut(&mut self, f: fn(&mut Value, Value)) -> UiuaResult {
        let mut a = self.pop(1)?;
        let b = self.pop(2)?;
        f(&mut a, b);
        self.push(a);
        Ok(())
    }
    pub(crate) fn dyadic_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        let value = f(&a, &b, self)?.into();
        self.push(value);
        Ok(())
    }
    pub(crate) fn dyadic_mut_env(
        &mut self,
        f: fn(&mut Value, Value, &Self) -> UiuaResult,
    ) -> UiuaResult {
        let mut a = self.pop(1)?;
        let b = self.pop(2)?;
        f(&mut a, b, self)?;
        self.push(a);
        Ok(())
    }
    pub(crate) fn stack_size(&self) -> usize {
        self.stack.len()
    }
    pub(crate) fn antistack_size(&self) -> usize {
        self.antistack.len()
    }
    pub(crate) fn truncate_stack(&mut self, size: usize) {
        self.stack.truncate(size);
    }
    pub(crate) fn truncate_antistack(&mut self, size: usize) {
        self.antistack.truncate(size);
    }
}

pub trait StackArg {
    fn arg_name(&self) -> String;
}

impl StackArg for usize {
    fn arg_name(&self) -> String {
        format!("argument {self}")
    }
}

impl StackArg for u8 {
    fn arg_name(&self) -> String {
        format!("argument {self}")
    }
}

impl StackArg for i32 {
    fn arg_name(&self) -> String {
        format!("argument {self}")
    }
}

impl<'a> StackArg for &'a str {
    fn arg_name(&self) -> String {
        self.to_string()
    }
}
