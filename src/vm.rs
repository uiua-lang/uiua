use std::{collections::VecDeque, fmt, mem::swap};

use crate::{
    array::Array,
    compile::Assembly,
    function::{FunctionId, Instr},
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
pub(crate) enum VmInstr {
    BindGlobal,
    Function(Instr),
}

impl fmt::Display for VmInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VmInstr::BindGlobal => write!(f, "BindGlobal"),
            VmInstr::Function(instr) => write!(f, "{instr}"),
        }
    }
}

impl From<Instr> for VmInstr {
    fn from(instr: Instr) -> Self {
        Self::Function(instr)
    }
}

struct StackFrame {
    call_span: usize,
    id: FunctionId,
    instrs: VecDeque<Instr>,
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
    instrs: VecDeque<VmInstr>,
    call_stack: Vec<StackFrame>,
    array_stack: Vec<usize>,
    pub globals: Vec<Value>,
    pub stack: Vec<Value>,
    pub io: B,
}

impl<B: IoBackend> Vm<B> {
    pub fn run_assembly(&mut self, assembly: &Assembly) -> UiuaResult {
        self.instrs = assembly.instrs.iter().cloned().map(Into::into).collect();
        if let Err(error) = self.run_assembly_inner(assembly, None) {
            let mut trace = Vec::new();
            for frame in self.call_stack.iter().rev() {
                trace.push(TraceFrame {
                    id: frame.id.clone(),
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
        #[cfg(feature = "profile")]
        let mut i = 0;
        loop {
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
            if let Some(frame) = self.call_stack.last_mut() {
                if let Some(instr) = frame.instrs.pop_front() {
                    dprintln!("  {instr}");
                    self.instr(assembly, &instr)?;
                } else {
                    self.call_stack.pop();
                    if return_depth == Some(self.call_stack.len()) {
                        break;
                    }
                }
            } else if let Some(instr) = self.instrs.pop_front() {
                dprintln!("  {instr}");
                match instr {
                    VmInstr::BindGlobal => self.globals.push(self.stack.pop().unwrap()),
                    VmInstr::Function(instr) => self.instr(assembly, &instr)?,
                }
            } else {
                break;
            }

            dprintln!("{:?}", self.stack);
        }
        Ok(())
    }
    fn instr(&mut self, assembly: &Assembly, instr: &Instr) -> RuntimeResult {
        match instr {
            Instr::Push(v) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("push");
                self.stack.push(v.clone())
            }
            Instr::BeginArray => self.array_stack.push(self.stack.len()),
            Instr::EndArray(normalize, span) => {
                let bottom = self.array_stack.pop().expect("nothing in array stack");
                if bottom > self.stack.len() {
                    return Err(assembly.spans[*span]
                        .error("array construction ended with a smaller stack"));
                }
                let array: Array = self.stack.drain(bottom..).rev().collect();
                self.stack
                    .push(array.normalized(*normalize as usize).into());
            }
            Instr::CopyGlobal(n) => self.stack.push(self.globals[*n].clone()),
            &Instr::Call(span) => {
                self.call(span)?;
            }
            &Instr::Primitive(prim, span) => {
                let mut env = CallEnv {
                    vm: self,
                    assembly,
                    span,
                };
                prim.run(&mut env)?;
            }
        }
        Ok(())
    }
    fn call(&mut self, span: usize) -> RuntimeResult<bool> {
        #[cfg(feature = "profile")]
        puffin::profile_scope!("call");
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
            call_span: span,
            id: function.id,
            instrs: function.instrs.into(),
        });
        Ok(true)
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
        let call_started = self.vm.call(self.span)?;
        if call_started {
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
        _size: usize,
        _f: impl FnOnce(&mut Self) -> RuntimeResult,
    ) -> RuntimeResult {
        todo!("references")
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
