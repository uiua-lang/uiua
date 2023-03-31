use std::{mem::swap, rc::Rc, vec};

use crate::{
    array::Array,
    compile::Assembly,
    function::{Function, Instr},
    io::IoBackend,
    lex::Span,
    primitive::Primitive,
    value::{Type, Value},
    RuntimeError, RuntimeResult, TraceFrame, UiuaError, UiuaResult,
};

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

pub struct Vm<'io> {
    instrs: vec::IntoIter<Instr>,
    call_stack: Vec<StackFrame>,
    ref_stack: Vec<Vec<Value>>,
    array_stack: Vec<usize>,
    pub globals: Vec<Value>,
    pub stack: Vec<Value>,
    pub antistack: Vec<Value>,
    pub io: &'io mut dyn IoBackend,
}

impl<'io> Vm<'io> {
    pub fn new(io: &'io mut dyn IoBackend) -> Self {
        Self {
            instrs: Vec::new().into_iter(),
            call_stack: Vec::new(),
            ref_stack: Vec::new(),
            array_stack: Vec::new(),
            globals: Vec::new(),
            stack: Vec::new(),
            antistack: Vec::new(),
            io,
        }
    }
    pub fn run_assembly(&mut self, assembly: &mut Assembly) -> UiuaResult {
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
        assembly: &mut Assembly,
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
    fn instr(&mut self, assembly: &mut Assembly, instr: Instr) -> RuntimeResult {
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
                self.stack.push(array.into());
                if normalize {
                    let mut env = Env {
                        vm: self,
                        assembly,
                        span,
                    };
                    Primitive::Normalize.run(&mut env)?;
                }
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
                let mut env = Env {
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
    pub fn call_complete(&mut self, assembly: &mut Assembly, span: usize) -> RuntimeResult {
        let return_depth = self.call_stack.len();
        let call_started = self.call(span)?;
        if call_started {
            self.run_assembly_inner(assembly, Some(return_depth))?;
        }
        Ok(())
    }
}

pub struct Env<'vm, 'io, 'a> {
    pub(crate) vm: &'vm mut Vm<'io>,
    pub assembly: &'a mut Assembly,
    pub span: usize,
}

impl<'vm, 'io, 'a> Env<'vm, 'io, 'a> {
    pub fn push(&mut self, value: impl Into<Value>) {
        self.vm.stack.push(value.into());
    }
    pub fn antipush(&mut self, value: impl Into<Value>) {
        self.vm.antistack.push(value.into());
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
    pub fn antipop(&mut self, arg: impl StackArg) -> RuntimeResult<Value> {
        self.vm.antistack.pop().ok_or_else(|| {
            self.error(format!(
                "Antistack was empty when evaluating {}",
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
    pub fn error(&self, msg: impl Into<String>) -> RuntimeError {
        self.assembly.error(self.span, msg.into())
    }
    pub fn monadic<V: Into<Value>>(&mut self, f: fn(&Value) -> V) -> RuntimeResult {
        let value = self.pop(1)?;
        self.push(f(&value));
        Ok(())
    }
    pub fn monadic_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Self) -> RuntimeResult<V>,
    ) -> RuntimeResult {
        let value = self.pop(1)?;
        self.push(f(&value, self)?);
        Ok(())
    }
    pub fn monadic_mut(&mut self, f: fn(&mut Value)) -> RuntimeResult {
        f(self.top_mut(1)?);
        Ok(())
    }
    pub fn monadic_mut_env(&mut self, f: fn(&mut Value, &Self) -> RuntimeResult) -> RuntimeResult {
        let mut a = self.pop(1)?;
        f(&mut a, self)?;
        self.push(a);
        Ok(())
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
        f: fn(&Value, &Value, &Self) -> RuntimeResult<V>,
    ) -> RuntimeResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        let value = f(&a, &b, self)?.into();
        self.push(value);
        Ok(())
    }
    pub fn dyadic_mut_env(
        &mut self,
        f: fn(&mut Value, Value, &Self) -> RuntimeResult,
    ) -> RuntimeResult {
        let mut a = self.pop(1)?;
        let b = self.pop(2)?;
        f(&mut a, b, self)?;
        self.push(a);
        Ok(())
    }
    pub fn span(&self) -> &Span {
        &self.assembly.spans[self.span]
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
