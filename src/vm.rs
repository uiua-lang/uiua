use std::{fmt, mem::swap};

use crate::{
    array::Array,
    compile::Assembly,
    function::Function,
    value::{RawType, Value},
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
    EndArray,
    BindGlobal,
    CopyGlobal(usize),
    Call(usize),
    Return,
}

fn _keep_instr_small(_: std::convert::Infallible) {
    let _: [u8; 32] = unsafe { std::mem::transmute(Instr::Return) };
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
pub struct Vm {
    call_stack: Vec<StackFrame>,
    array_stack: Vec<usize>,
    pub stack: Vec<Value>,
    pc: usize,
    just_called: Option<Function>,
    globals: Vec<Value>,
}

impl Vm {
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
                Instr::EndArray => {
                    let bottom = self.array_stack.pop().expect("nothing in array stack");
                    let array: Array = stack.drain(bottom..).rev().collect();
                    stack.push(array.normalized(1).into());
                }
                Instr::BindGlobal => self.globals.push(stack.pop().unwrap()),
                Instr::CopyGlobal(n) => stack.push(self.globals[*n].clone()),
                &Instr::Call(span) => {
                    self.call(assembly, span)?;
                }
                Instr::Return => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("return");
                    if let Some(frame) = self.call_stack.pop() {
                        let value = stack.pop().unwrap();
                        stack.truncate(frame.stack_size);
                        stack.push(value);
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
        let function = match value.raw_ty() {
            RawType::Function => value.function(),
            _ => {
                self.stack.pop();
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
            Function::Selector(sel) => {
                let mut items = Vec::with_capacity(sel.outputs() as usize);
                for _ in 0..sel.outputs() {
                    items.push(env.pop()?);
                }
                let bottom = env.vm.stack.len();
                for i in sel.indices() {
                    env.push(items[i as usize].clone());
                }
                self.stack[bottom..].reverse();
                false
            }
        };
        self.just_called = Some(function);
        Ok(call_started)
    }
}

pub(crate) struct CallEnv<'a> {
    pub vm: &'a mut Vm,
    pub assembly: &'a Assembly,
    pub span: usize,
}

impl<'a> CallEnv<'a> {
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
    #[track_caller]
    pub fn pop(&mut self) -> RuntimeResult<Value> {
        self.vm
            .stack
            .pop()
            .ok_or_else(|| self.error("stack is empty"))
    }
    pub fn pop_n(&mut self, n: usize) -> RuntimeResult<Vec<Value>> {
        if self.vm.stack.len() < n {
            return Err(self.error("stack is empty"));
        }
        Ok(self.vm.stack.drain(self.vm.stack.len() - n..).collect())
    }
    #[track_caller]
    pub fn top_mut(&mut self) -> RuntimeResult<&mut Value> {
        if let Some(value) = self.vm.stack.last_mut() {
            Ok(value)
        } else {
            Err(self.assembly.spans[self.span].error("stack is empty"))
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
        let value = self.pop()?;
        self.push(f(&value));
        Ok(())
    }
    pub fn monadic_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Env) -> RuntimeResult<V>,
    ) -> RuntimeResult {
        let env = self.env();
        let value = self.top_mut()?;
        *value = f(value, &env)?.into();
        Ok(())
    }
    pub fn monadic_mut(&mut self, f: fn(&mut Value)) -> RuntimeResult {
        f(self.top_mut()?);
        Ok(())
    }
    pub fn monadic_mut_env(&mut self, f: fn(&mut Value, &Env) -> RuntimeResult) -> RuntimeResult {
        let env = self.env();
        f(self.top_mut()?, &env)
    }
    pub fn dyadic<V: Into<Value>>(&mut self, f: fn(&Value, &Value) -> V) -> RuntimeResult {
        let mut b = self.pop()?;
        let a = self.top_mut()?;
        swap(a, &mut b);
        *a = f(a, &b).into();
        Ok(())
    }
    pub fn dyadic_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Value, &Env) -> RuntimeResult<V>,
    ) -> RuntimeResult {
        let env = self.env();
        let mut b = self.pop()?;
        let a = self.top_mut()?;
        swap(a, &mut b);
        *a = f(a, &b, &env)?.into();
        Ok(())
    }
    pub fn dyadic_mut_env(
        &mut self,
        f: fn(&mut Value, &mut Value, &Env) -> RuntimeResult,
    ) -> RuntimeResult {
        let env = self.env();
        let mut b = self.pop()?;
        let a = self.top_mut()?;
        swap(a, &mut b);
        f(a, &mut b, &env)
    }
}
