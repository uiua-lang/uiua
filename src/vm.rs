use std::{
    fmt,
    mem::{replace, swap},
};

use crate::{
    array::Array,
    compile::Assembly,
    function::Function,
    io::{IoBackend, StdIo},
    value::{Type, Value},
    RuntimeError, RuntimeResult, TraceFrame, UiuaError, UiuaResult,
};

pub struct Env<'a> {
    pub span: usize,
    pub assembly: &'a Assembly,
}

impl<'a> Env<'a> {
    pub fn error(&self, message: impl Into<String>) -> RuntimeError {
        self.assembly.spans[self.span].error(message)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instr {
    Comment(String),
    Push(Value),
    Constant(usize),
    BeginArray,
    EndArray(bool, usize),
    BindGlobal,
    CopyGlobal(usize),
    CopyRef(usize, usize),
    Call(usize),
    Return,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Comment(s) => write!(f, "\n// {}", s),
            instr => write!(f, "{instr:?}"),
        }
    }
}

struct StackFrame {
    stack_size: usize,
    function: Function,
    ret: usize,
    call_span: usize,
}

macro_rules! dprintln {
    ($($arg:tt)*) => {
        if cfg!(feature = "debug") {
            println!($($arg)*);
        }
    };
}
pub(crate) use dprintln;

#[derive(Default)]
pub struct Vm<B = StdIo> {
    call_stack: Vec<StackFrame>,
    array_stack: Vec<usize>,
    reference_stack: Vec<ReferenceFrame>,
    pub globals: Vec<Value>,
    pub stack: Vec<Value>,
    pc: usize,
    just_called: Option<Function>,
    pub io: B,
}

struct ReferenceFrame {
    bottom: usize,
    size: usize,
    values: Option<Vec<Value>>,
}

pub struct RestorePoint {
    stack: Vec<Value>,
    pc: usize,
    array_stack_size: usize,
    context_stack_size: usize,
}

impl<B: IoBackend> Vm<B> {
    pub fn run_assembly(&mut self, assembly: &Assembly) -> UiuaResult {
        if let Err(error) = self.run_assembly_inner(assembly, None) {
            let mut trace = Vec::new();
            for frame in self.call_stack.iter().rev() {
                let id = assembly.function_id(frame.function);
                trace.push(TraceFrame {
                    id: id.clone(),
                    span: assembly.spans[frame.call_span].clone(),
                });
            }
            Err(UiuaError::Run {
                error: error.into(),
                trace,
            })
        } else {
            Ok(())
        }
    }
    pub(crate) fn restore_point(&self) -> RestorePoint {
        RestorePoint {
            stack: self.stack.clone(),
            pc: self.pc,
            array_stack_size: self.array_stack.len(),
            context_stack_size: self.reference_stack.len(),
        }
    }
    pub(crate) fn restore(&mut self, point: RestorePoint) -> Vec<Value> {
        assert!(self.call_stack.is_empty());
        assert!(self.array_stack.is_empty());
        self.pc = point.pc;
        self.just_called = None;
        self.array_stack.truncate(point.array_stack_size);
        self.reference_stack.truncate(point.context_stack_size);
        replace(&mut self.stack, point.stack)
    }
    fn run_assembly_inner(
        &mut self,
        assembly: &Assembly,
        return_depth: Option<usize>,
    ) -> RuntimeResult {
        if return_depth.is_none() {
            dprintln!("\nRunning...");
            if self.pc == 0 {
                self.pc = assembly.start;
            }
        }
        #[cfg(feature = "profile")]
        let mut i = 0;
        while self.pc < assembly.instrs.len() {
            let pc = &mut self.pc;
            let stack = &mut self.stack;
            #[cfg(feature = "profile")]
            if i % 100_000 == 0 {
                puffin::GlobalProfiler::lock().new_frame();
            }
            #[cfg(feature = "profile")]
            {
                i += 1;
            }
            #[cfg(feature = "profile")]
            puffin::profile_scope!("instr loop");
            let instr = &assembly.instrs[*pc];
            dprintln!("{pc:>3} {instr}");
            match instr {
                Instr::Comment(_) => {}
                Instr::Push(v) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("push");
                    stack.push(v.clone())
                }
                Instr::Constant(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("constant");
                    stack.push(assembly.constants[*n].clone())
                }
                Instr::BeginArray => self.array_stack.push(stack.len()),
                Instr::EndArray(normalize, span) => {
                    let bottom = self.array_stack.pop().expect("nothing in array stack");
                    if bottom > stack.len() {
                        return Err(assembly.spans[*span]
                            .error("array construction ended with a smaller stack"));
                    }
                    let array: Array = stack.drain(bottom..).rev().collect();
                    stack.push(array.normalized(*normalize as usize).into());
                }
                Instr::BindGlobal => self.globals.push(stack.pop().unwrap()),
                Instr::CopyGlobal(n) => stack.push(self.globals[*n].clone()),
                &Instr::CopyRef(n, span) => {
                    let name = || (n as u8 + b'a') as char;
                    let reference = self.reference_stack.last_mut().ok_or_else(|| {
                        assembly.spans[span].error(format!("no reference set for `{}`", name()))
                    })?;
                    if n >= reference.size {
                        return Err(assembly.spans[span].error(format!(
                            "reference `{}` is out of bounds of {} reference",
                            name(),
                            reference.size
                        )));
                    }
                    if reference.values.is_none() {
                        if reference.size > stack.len()
                            || reference.bottom - reference.size > stack.len()
                        {
                            return Err(assembly.spans[span]
                                .error(format!("reference `{}` was invalidated", name())));
                        }
                        reference.values = Some(
                            stack
                                .drain(reference.bottom - reference.size..reference.bottom)
                                .rev()
                                .collect(),
                        );
                    }
                    let value = reference.values.as_ref().unwrap()[n].clone();
                    stack.push(value);
                }
                &Instr::Call(span) => {
                    self.call(assembly, span)?;
                }
                Instr::Return => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("return");
                    if let Some(frame) = self.call_stack.pop() {
                        let value = stack.pop();
                        stack.truncate(frame.stack_size);
                        stack.extend(value);
                        if return_depth.map_or(false, |d| d == self.call_stack.len()) {
                            *pc = 0;
                            return Ok(());
                        } else {
                            *pc = frame.ret;
                            continue;
                        }
                    } else {
                        *pc = 0;
                        return Ok(());
                    }
                }
            }
            dprintln!("  {:?}", self.stack);
            self.pc = self.pc.overflowing_add(1).0;
        }
        self.pc -= 1;
        Ok(())
    }
    fn call(&mut self, assembly: &Assembly, span: usize) -> RuntimeResult<bool> {
        #[cfg(feature = "profile")]
        puffin::profile_scope!("call");
        let value = self.stack.pop().unwrap();
        let function = match value.ty() {
            Type::Function => value.function(),
            _ => {
                self.stack.push(value);
                return Ok(false);
            }
        };
        // Call
        let pc = self.pc;
        let mut env = CallEnv {
            vm: self,
            assembly,
            span,
        };
        let call_started = match function {
            Function::Code(start) => {
                self.call_stack.push(StackFrame {
                    stack_size: self.stack.len(),
                    function,
                    ret: self.pc + 1,
                    call_span: span,
                });
                self.pc = start as usize;
                true
            }
            Function::Primitive(prim) => {
                prim.run(&mut env)?;
                self.pc = pc;
                false
            }
        };
        self.just_called = Some(function);
        Ok(call_started)
    }
}

pub(crate) struct CallEnv<'a, B> {
    pub vm: &'a mut Vm<B>,
    pub assembly: &'a Assembly,
    pub span: usize,
}

impl<'a, B: IoBackend> CallEnv<'a, B> {
    pub fn env<'b>(&self) -> Env<'b>
    where
        'a: 'b,
    {
        Env {
            assembly: self.assembly,
            span: self.span,
        }
    }
    pub fn push(&mut self, value: impl Into<Value>) {
        self.vm.stack.push(value.into());
    }
    pub fn stack_size(&self) -> usize {
        self.vm.stack.len()
    }
    pub fn truncate(&mut self, size: usize) {
        self.vm.stack.truncate(size);
    }
    pub fn pop(&mut self, arg: impl StackArg) -> RuntimeResult<Value> {
        self.vm.stack.pop().ok_or_else(|| {
            self.error(format!(
                "Stack was empty when evaluating {}",
                arg.arg_name()
            ))
        })
    }
    pub fn pop_n(&mut self, n: usize) -> RuntimeResult<Vec<Value>> {
        if self.vm.stack.len() < n {
            return Err(self.error("stack is empty"));
        }
        Ok(self.vm.stack.drain(self.vm.stack.len() - n..).collect())
    }
    pub fn top_mut(&mut self, arg: impl StackArg) -> RuntimeResult<&mut Value> {
        if let Some(value) = self.vm.stack.last_mut() {
            Ok(value)
        } else {
            Err(self.assembly.spans[self.span].error(format!(
                "Stack was empty when evaluating argument {}",
                arg.arg_name()
            )))
        }
    }
    pub fn call(&mut self) -> RuntimeResult {
        let return_depth = self.vm.call_stack.len();
        let call_started = self.vm.call(self.assembly, self.span)?;
        if call_started {
            self.vm.pc = self.vm.pc.overflowing_add(1).0;
            self.vm
                .run_assembly_inner(self.assembly, Some(return_depth))?;
        }
        Ok(())
    }
    pub fn error(&mut self, msg: impl Into<String>) -> RuntimeError {
        self.assembly.error(self.span, msg.into())
    }
    pub fn monadic<V: Into<Value>>(&mut self, f: fn(&Value) -> V) -> RuntimeResult {
        let value = self.pop(1)?;
        self.push(f(&value));
        Ok(())
    }
    pub fn monadic_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Env) -> RuntimeResult<V>,
    ) -> RuntimeResult {
        let env = self.env();
        let value = self.top_mut(1)?;
        *value = f(value, &env)?.into();
        Ok(())
    }
    pub fn monadic_mut(&mut self, f: fn(&mut Value)) -> RuntimeResult {
        f(self.top_mut(1)?);
        Ok(())
    }
    pub fn monadic_mut_env(&mut self, f: fn(&mut Value, &Env) -> RuntimeResult) -> RuntimeResult {
        let env = self.env();
        f(self.top_mut(1)?, &env)
    }
    pub fn dyadic<V: Into<Value>>(&mut self, f: fn(&Value, &Value) -> V) -> RuntimeResult {
        let mut b = self.pop(1)?;
        let a = self.top_mut(2)?;
        swap(a, &mut b);
        *a = f(a, &b).into();
        Ok(())
    }
    pub fn dyadic_mut(&mut self, f: fn(&mut Value, Value)) -> RuntimeResult {
        let mut b = self.pop(1)?;
        let a = self.top_mut(2)?;
        swap(a, &mut b);
        f(a, b);
        Ok(())
    }
    pub fn dyadic_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Value, &Env) -> RuntimeResult<V>,
    ) -> RuntimeResult {
        let env = self.env();
        let mut b = self.pop(1)?;
        let a = self.top_mut(2)?;
        swap(a, &mut b);
        *a = f(a, &b, &env)?.into();
        Ok(())
    }
    pub fn dyadic_mut_env(
        &mut self,
        f: fn(&mut Value, Value, &Env) -> RuntimeResult,
    ) -> RuntimeResult {
        let env = self.env();
        let mut b = self.pop(1)?;
        let a = self.top_mut(2)?;
        swap(a, &mut b);
        f(a, b, &env)
    }
    pub fn with_reference(
        &mut self,
        size: usize,
        f: impl FnOnce(&mut Self) -> RuntimeResult,
    ) -> RuntimeResult {
        let bottom = self.vm.stack.len();
        if bottom < size {
            return Err(self.error(format!("This reference needs at least {size} values, but there are only {bottom} on the stack")));
        }
        self.vm.reference_stack.push(ReferenceFrame {
            bottom,
            size,
            values: None,
        });
        let res = f(self);
        self.vm
            .reference_stack
            .pop()
            .expect("popped empty reference stack");
        res
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
