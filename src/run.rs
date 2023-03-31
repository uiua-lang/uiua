use std::{
    collections::HashMap,
    fs, io,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    ast::*,
    function::{Function, FunctionId, Instr},
    lex::{Sp, Span},
    parse::{parse, ParseError},
    primitive::Primitive,
    value::Value,
    Ident,
};

#[derive(Debug)]
pub enum UiuaError {
    Load(PathBuf, io::Error),
    Format(PathBuf, io::Error),
    Parse(Vec<Sp<ParseError>>),
    Run(Sp<String>),
    Traced {
        error: Box<Self>,
        trace: Vec<TraceFrame>,
    },
}

pub type UiuaResult<T = ()> = Result<T, UiuaError>;

impl From<Sp<String>> for UiuaError {
    fn from(value: Sp<String>) -> Self {
        Self::Run(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraceFrame {
    pub id: FunctionId,
    pub span: Span,
}

pub struct Uiua {
    spans: Vec<Span>,
    stack: Vec<Value>,
    antistack: Vec<Value>,
    array_stack: Vec<usize>,
    globals: Vec<Value>,
    global_names: HashMap<Ident, usize>,
    new_functions: Vec<Vec<Instr>>,
    call_stack: Vec<StackFrame>,
}

struct StackFrame {
    function: Rc<Function>,
    call_span: usize,
    pc: usize,
}

impl Default for Uiua {
    fn default() -> Self {
        Uiua {
            spans: vec![Span::Builtin],
            stack: Vec::new(),
            antistack: Vec::new(),
            array_stack: Vec::new(),
            globals: Vec::new(),
            global_names: HashMap::new(),
            new_functions: Vec::new(),
            call_stack: Vec::new(),
        }
    }
}

impl Uiua {
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        self.load_impl(&input, Some(path))
    }
    pub fn load_str(&mut self, input: &str) -> UiuaResult {
        self.load_impl(input, None)
    }
    fn load_impl(&mut self, input: &str, path: Option<&Path>) -> UiuaResult {
        let (items, errors) = parse(input, path);
        if !errors.is_empty() {
            return Err(UiuaError::Parse(errors));
        }
        for item in items {
            self.item(item)?;
        }
        Ok(())
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
        for word in words {
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
            Word::Ident(_) => todo!(),
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
            Word::Func(_) => todo!(),
            Word::RefFunc(_) => todo!(),
            Word::Primitive(p) => {
                let span = self.push_span(word.span);
                self.push_instr(Instr::Primitive(p, span));
            }
            Word::Modified(_) => todo!(),
        }
        Ok(())
    }
    fn exec_global_instrs(&mut self, instrs: Vec<Instr>) -> UiuaResult {
        let func = Function {
            id: FunctionId::Anonymous(Span::Builtin),
            instrs,
        };
        self.call_stack.push(StackFrame {
            function: Rc::new(func),
            call_span: 0,
            pc: 0,
        });
        self.exec()
    }
    fn exec(&mut self) -> UiuaResult {
        'outer: while let Some(frame) = self.call_stack.last_mut() {
            while let Some(instr) = frame.function.instrs.get(frame.pc) {
                match instr {
                    Instr::Push(val) => self.stack.push(val.clone()),
                    Instr::BeginArray => self.array_stack.push(self.stack.len()),
                    Instr::EndArray(_, _) => todo!(),
                    Instr::CopyGlobal(idx) => self.stack.push(self.globals[*idx].clone()),
                    Instr::BindGlobal(_) => todo!(),
                    Instr::Primitive(_, _) => todo!(),
                    Instr::Call(span) => {
                        let value = self.stack.pop().unwrap();
                        if value.is_function() {
                            let function = value.into_function();
                            let new_frame = StackFrame {
                                function,
                                call_span: *span,
                                pc: 0,
                            };
                            frame.pc += 1;
                            self.call_stack.push(new_frame);
                            continue 'outer;
                        } else {
                            self.stack.push(value);
                        }
                    }
                    Instr::CallRef(_, _) => todo!(),
                    Instr::CopyRef(_, _) => todo!(),
                }
                frame.pc += 1;
            }
            self.call_stack.pop();
        }
        Ok(())
    }
    pub fn error(&self, message: impl ToString) -> UiuaError {
        let span = self.spans[self.call_stack.last().unwrap().call_span].clone();
        UiuaError::Run(span.sp(message.to_string()))
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
    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }
    pub fn antipush(&mut self, val: Value) {
        self.antistack.push(val);
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
