use std::{fmt, mem::swap};

use crate::{
    array::Array,
    compile::Assembly,
    function::{Function, Primitive},
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
    List(usize),
    Array(usize),
    /// Copy the nth value from the top of the stack
    CopyRel(usize),
    /// Copy the nth value from the bottom of the stack
    CopyAbs(usize),
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
    pub stack: Vec<Value>,
    pc: usize,
    just_called: Option<Function>,
}

impl Vm {
    pub fn run_assembly(&mut self, assembly: &Assembly) -> UiuaResult {
        if let Err(error) = self.run_assembly_inner(assembly, 0) {
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
    fn run_assembly_inner(&mut self, assembly: &Assembly, return_depth: usize) -> RuntimeResult {
        if return_depth == 0 {
            dprintln!("\nRunning...");
        }
        if self.pc == 0 {
            self.pc = assembly.start;
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
                Instr::List(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("list");
                    let array: Array = stack.drain(stack.len() - *n..).collect();
                    stack.push(array.normalized(0).into());
                }
                Instr::Array(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("array");
                    let array: Array = stack.drain(stack.len() - *n..).collect();
                    stack.push(array.normalized(1).into());
                }
                Instr::CopyRel(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("copy");
                    stack.push(stack[stack.len() - *n].clone())
                }
                Instr::CopyAbs(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("copy");
                    stack.push(stack[*n].clone())
                }
                &Instr::Call(span) => {
                    self.call_impl(assembly, span)?;
                }
                Instr::Return => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("return");
                    if let Some(frame) = self.call_stack.pop() {
                        let value = stack.pop().unwrap();
                        stack.truncate(frame.stack_size);
                        stack.push(value);
                        if return_depth > 0 && self.call_stack.len() == return_depth {
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
    pub fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(value.into());
    }
    #[track_caller]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().expect("nothing to pop")
    }
    #[track_caller]
    pub fn top_mut(&mut self) -> &mut Value {
        self.stack.last_mut().expect("stack is empty")
    }
    pub fn call(&mut self, assembly: &Assembly, span: usize) -> RuntimeResult {
        let return_depth = self.call_stack.len();
        let call_started = self.call_impl(assembly, span)?;
        if call_started {
            self.pc = self.pc.overflowing_add(1).0;
            self.run_assembly_inner(assembly, return_depth)?;
        }
        Ok(())
    }
    fn call_impl(&mut self, assembly: &Assembly, span: usize) -> RuntimeResult<bool> {
        #[cfg(feature = "profile")]
        puffin::profile_scope!("call");
        let value = self.stack.pop().unwrap();
        let function = if let RawType::Function = value.raw_ty() {
            value.function()
        } else {
            self.stack.pop();
            self.stack.push(value);
            return Ok(false);
        };
        // Call
        let call_started = match function {
            Function::Code(start) => {
                self.call_stack.push(StackFrame {
                    stack_size: self.stack.len(),
                    function,
                    ret: self.pc + 1,
                    call_span: span,
                });
                self.pc = (start as usize).overflowing_sub(1).0;
                true
            }
            Function::Primitive(prim) => {
                match prim {
                    Primitive::Op1(op1) => {
                        let val = self.stack.last_mut().unwrap();
                        let env = Env { assembly, span: 0 };
                        val.op1(op1, &env)?;
                    }
                    Primitive::Op2(op2) => {
                        let (left, right) = {
                            let mut iter = self.stack.iter_mut().rev();
                            let right = iter.next().unwrap();
                            let left = iter.next().unwrap();
                            (left, right)
                        };
                        swap(left, right);
                        let env = Env { assembly, span };
                        left.op2(right, op2, &env)?;
                        self.stack.pop();
                    }
                    Primitive::HigherOp(hop) => {
                        let env = Env { assembly, span };
                        hop.run(self, &env)?;
                    }
                }
                false
            }
        };
        self.just_called = Some(function);
        Ok(call_started)
    }
}
