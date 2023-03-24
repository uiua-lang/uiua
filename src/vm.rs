use std::{mem::swap, rc::Rc, vec};

use crate::{
    array::Array,
    compile::Assembly,
    function::{Function, Instr},
    io::{IoBackend, StdIo},
    lex::Span,
    value::{Type, Value},
    RuntimeError, RuntimeResult, TraceFrame, UiuaError, UiuaResult,
};

pub struct Env<'a> {
    pub(crate) span: usize,
    pub assembly: &'a Assembly,
}

impl<'a> Env<'a> {
    pub fn error(&self, message: impl Into<String>) -> RuntimeError {
        self.span().error(message)
    }
    pub fn span(&self) -> &Span {
        &self.assembly.spans[self.span]
    }
}

#[derive(Clone)]
struct StackFrame {
    function: Rc<Function>,
    call_span: usize,
    pc: usize,
}

macro_rules! dprintln {
    ($($arg:tt)*) => {
        if cfg!(feature = "debug") {
            println!($($arg)*);
        }
    };
}
pub(crate) use dprintln;

pub struct Vm<B = StdIo> {
    instrs: vec::IntoIter<Instr>,
    call_stack: Vec<StackFrame>,
    ref_stack: Vec<Vec<Value>>,
    array_stack: Vec<usize>,
    pub globals: Vec<Value>,
    pub stack: Vec<Value>,
    pub io: B,
}

impl<B: Default + IoBackend> Default for Vm<B> {
    fn default() -> Self {
        Self::new(B::default())
    }
}

impl<B: IoBackend> Vm<B> {
    pub fn new(io: B) -> Self {
        Self {
            instrs: Vec::new().into_iter(),
            call_stack: Vec::new(),
            ref_stack: Vec::new(),
            array_stack: Vec::new(),
            globals: Vec::new(),
            stack: Vec::new(),
            io,
        }
    }
    pub fn run_assembly(&mut self, assembly: &Assembly) -> UiuaResult {
        self.instrs = assembly.instrs.clone().into_iter();
        if let Err(error) = self.run_assembly_inner(assembly, None) {
            let mut trace = Vec::new();
            for frame in self.call_stack.iter().rev() {
                trace.push(TraceFrame {
                    id: frame.function.id.clone(),
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
    fn run_assembly_inner(
        &mut self,
        assembly: &Assembly,
        return_depth: Option<usize>,
    ) -> RuntimeResult {
        if return_depth.is_none() {
            dprintln!("Running...");
        }
        loop {
            if let Some(frame) = self.call_stack.last_mut() {
                if let Some(instr) = frame.function.instrs.get(frame.pc).cloned() {
                    dprintln!("  {instr}");
                    frame.pc += 1;
                    self.instr(assembly, instr)?;
                } else {
                    self.call_stack.pop();
                    if return_depth == Some(self.call_stack.len()) {
                        break;
                    }
                }
            } else if let Some(instr) = self.instrs.next() {
                dprintln!("  {instr}");
                self.instr(assembly, instr)?;
            } else {
                break;
            }

            dprintln!("{:?}", self.stack);
        }
        Ok(())
    }
    fn instr(&mut self, assembly: &Assembly, instr: Instr) -> RuntimeResult {
        match instr {
            Instr::Push(v) => self.stack.push(v),
            Instr::BeginArray => self.array_stack.push(self.stack.len()),
            Instr::EndArray(normalize, span) => {
                let bottom = self.array_stack.pop().expect("nothing in array stack");
                if bottom > self.stack.len() {
                    return Err(
                        assembly.spans[span].error("array construction ended with a smaller stack")
                    );
                }
                let array: Array = self.stack.drain(bottom..).rev().collect();
                self.stack.push(array.normalized(normalize as usize).into());
            }
            Instr::BindGlobal(span) => self.globals.push(
                self.stack
                    .pop()
                    .ok_or_else(|| assembly.spans[span].error("stack empty when binding global"))?,
            ),
            Instr::CopyGlobal(n) => self.stack.push(self.globals[n].clone()),
            Instr::Call(span) => {
                self.call(span)?;
            }
            Instr::Primitive(prim, span) => {
                let mut env = CallEnv {
                    vm: self,
                    assembly,
                    span,
                };
                prim.run(&mut env)?;
            }
            Instr::CallRef(n, span) => {
                let f = self.stack.pop().expect("stack empty when calling ref");
                if self.stack.len() < n {
                    return Err(assembly.spans[span].error(format!(
                        "reference requires {n} values, but only {} are on the stack",
                        self.stack.len()
                    )));
                }
                let mut values = self.stack.split_off(self.stack.len() - n);
                values.reverse();
                self.ref_stack.push(values);
                self.stack.push(f);
                let res = self.call_complete(assembly, span);
                self.ref_stack.pop().expect("ref stack empty");
                res?
            }
            Instr::CopyRef(n, span) => {
                let name = || (n as u8 + b'a') as char;
                let Some(values) = self.ref_stack.last() else {
                    return Err(assembly.spans[span].error(format!("`{}` has no reference context", name())));
                };
                if n >= values.len() {
                    return Err(assembly.spans[span].error(format!(
                        "reference `{}` requires {n} values, but the current context only has {}",
                        name(),
                        values.len()
                    )));
                }
                self.stack.push(values[n].clone());
            }
        }
        Ok(())
    }
    fn call(&mut self, span: usize) -> RuntimeResult<bool> {
        let value = self.stack.pop().unwrap();
        let function = match value.ty() {
            Type::Function => value.into_function(),
            _ => {
                self.stack.push(value);
                return Ok(false);
            }
        };
        // Call
        self.call_stack.push(StackFrame {
            function,
            call_span: span,
            pc: 0,
        });
        Ok(true)
    }
    pub fn call_complete(&mut self, assembly: &Assembly, span: usize) -> RuntimeResult {
        let return_depth = self.call_stack.len();
        let call_started = self.call(span)?;
        if call_started {
            self.run_assembly_inner(assembly, Some(return_depth))?;
        }
        Ok(())
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
        self.vm.call_complete(self.assembly, self.span)
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
