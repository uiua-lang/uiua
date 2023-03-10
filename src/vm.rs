use std::{fmt, mem::swap};

use crate::{
    array::Array,
    compile::Assembly,
    function::{Function, FunctionId, Partial},
    lex::Span,
    ops::{HigherOp, Op1, Op2},
    value::{RawType, Type, Value},
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
#[allow(unused)]
pub(crate) enum Instr {
    Comment(String),
    AssertType(Type),
    Push(Value),
    Constant(usize),
    List(usize),
    Array(usize),
    /// Copy the nth value from the top of the stack
    CopyRel(usize),
    /// Copy the nth value from the bottom of the stack
    CopyAbs(usize),
    Swap,
    Move(usize),
    Rotate(usize),
    Pop(usize),
    Call(usize),
    Return,
    Jump(isize),
    PopJumpIf(isize, bool),
    JumpIfElsePop(isize, bool),
    Op1(Op1),
    Op2(Op2, usize),
    HigherOp(HigherOp),
    DestructureList(usize, Box<Span>),
    PushUnresolvedFunction(Box<FunctionId>),
    Dud,
}

fn _keep_instr_small(_: std::convert::Infallible) {
    let _: [u8; 32] = unsafe { std::mem::transmute(Instr::Dud) };
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
        dprintln!("\nRunning...");
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
                Instr::AssertType(ty) => {
                    let value = stack.last().unwrap();
                    let val_ty = value.ty();
                    if val_ty != *ty {
                        return Err(assembly.spans[0]
                            .clone()
                            .sp(format!("Value expected to be {ty} was {val_ty} instead")));
                    }
                }
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
                    stack.push(array.into());
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
                Instr::Swap => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("swap");
                    let len = stack.len();
                    stack.swap(len - 1, len - 2);
                }
                Instr::Move(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("move");
                    let val = stack.remove(stack.len() - *n);
                    stack.push(val);
                }
                Instr::Rotate(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("rotate");
                    let val = stack.pop().unwrap();
                    stack.insert(stack.len() + 1 - *n, val);
                }
                Instr::Pop(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("pop");
                    stack.truncate(stack.len() - *n);
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
                Instr::Jump(delta) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("jump");
                    *pc = pc.wrapping_add_signed(*delta);
                    continue;
                }
                Instr::PopJumpIf(delta, cond) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("pop_jump_if");
                    let val = stack.pop().unwrap();
                    if val.is_truthy() == *cond {
                        *pc = pc.wrapping_add_signed(*delta);
                        continue;
                    }
                }
                Instr::JumpIfElsePop(delta, cond) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("jump_if_else_pop");
                    let val = stack.last().unwrap();
                    if val.is_truthy() == *cond {
                        *pc = pc.wrapping_add_signed(*delta);
                        continue;
                    } else {
                        stack.pop();
                    }
                }
                Instr::Op1(op) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("op1");
                    let val = stack.last_mut().unwrap();
                    let env = Env { assembly, span: 0 };
                    val.op1(*op, &env)?;
                }
                Instr::Op2(op, span) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("op2");
                    let (left, right) = {
                        #[cfg(feature = "profile")]
                        puffin::profile_scope!("op2_args");
                        let mut iter = stack.iter_mut().rev();
                        let right = iter.next().unwrap();
                        let left = iter.next().unwrap();
                        (left, right)
                    };
                    swap(left, right);
                    let env = Env {
                        assembly,
                        span: *span,
                    };
                    left.op2(right, *op, &env)?;
                    stack.pop();
                }
                Instr::HigherOp(hop) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("algorithm");
                    let env = Env { assembly, span: 0 };
                    hop.run(self, &env)?;
                }
                Instr::DestructureList(_n, _span) => {
                    // let list = match stack.pop().unwrap() {
                    //     Value::List(list) if *n == list.len() => list,
                    //     Value::List(list) => {
                    //         let message =
                    //             format!("cannot destructure list of {} as list of {n}", list.len());
                    //         return Err(span.sp(message).into());
                    //     }
                    //     val => {
                    //         let message =
                    //             format!("cannot destructure {} as list of {}", val.ty(), n);
                    //         return Err(span.sp(message).into());
                    //     }
                    // };
                    // for val in list.into_iter().rev() {
                    //     stack.push(val);
                    // }
                }
                Instr::PushUnresolvedFunction(id) => {
                    panic!("unresolved function: {id:?}")
                }
                Instr::Dud => {
                    panic!("unresolved instruction")
                }
            }
            dprintln!("{:?}", self.stack);
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
    pub fn call(&mut self, args: usize, assembly: &Assembly, span: usize) -> RuntimeResult {
        let return_depth = self.call_stack.len();
        let mut call_started = false;
        for _ in 0..args {
            call_started = self.call_impl(assembly, span)?;
        }
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
        let (function, partial_count, span) = match value.raw_ty() {
            RawType::Function => (value.function(), 0, span),
            RawType::Partial => {
                let partial = value.partial();
                let arg_count = partial.args.len();
                for arg in partial.args.iter() {
                    self.stack.push(arg.clone());
                }
                (partial.function, arg_count, partial.span)
            }
            _ => {
                let message = if let Some(function) = self.just_called.take() {
                    let id = assembly.function_id(function);
                    format!("Too many arguments to {}: expected {}", id, function.params)
                } else {
                    format!("Cannot call {}", value.ty())
                };
                return Err(assembly.spans[span].error(message));
            }
        };
        // Handle partial arguments
        let arg_count = partial_count + 1;
        if partial_count > 0 {
            dprintln!("{:?}", self.stack);
        }
        // Call
        if arg_count < function.params as usize {
            let partial = Partial {
                function,
                args: self.stack.drain(self.stack.len() - arg_count..).collect(),
                span,
            };
            self.stack.push(partial.into());
            Ok(false)
        } else {
            self.call_stack.push(StackFrame {
                stack_size: self.stack.len() - arg_count,
                function,
                ret: self.pc + 1,
                call_span: span,
            });
            self.pc = (function.start as usize).overflowing_sub(1).0;
            self.just_called = Some(function);
            Ok(true)
        }
    }
}
