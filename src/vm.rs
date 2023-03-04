use std::{cmp::Ordering, fmt, mem::swap};

use crate::{
    ast::{BinOp, FunctionId},
    builtin::{BuiltinOp1, BuiltinOp2},
    compile::Assembly,
    lex::Span,
    value::{Function, Partial, Value},
    RuntimeResult, TraceFrame, UiuaError, UiuaResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instr {
    Comment(String),
    Push(Value),
    /// Copy the nth value from the top of the stack
    CopyRel(usize),
    /// Copy the nth value from the bottom of the stack
    CopyAbs(usize),
    Call {
        args: usize,
        span: usize,
    },
    Return,
    Jump(isize),
    PopJumpIf(isize, bool),
    JumpIfElsePop(isize, bool),
    BinOp(BinOp, Span),
    BuiltinOp1(BuiltinOp1, Span),
    BuiltinOp2(BuiltinOp2, Span),
    DestructureList(usize, Span),
    PushUnresolvedFunction(FunctionId),
    Dud,
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

const DBG: bool = false;
macro_rules! dprintln {
    ($($arg:tt)*) => {
        if DBG {
            println!($($arg)*);
        }
    };
}

pub(crate) fn run_assembly(assembly: &Assembly) -> UiuaResult {
    let mut call_stack = Vec::new();
    if let Err(error) = run_assembly_inner(assembly, &mut call_stack) {
        let mut trace = Vec::new();
        for frame in call_stack.into_iter().rev() {
            let info = assembly.function_info(frame.function);
            trace.push(TraceFrame {
                id: info.id.clone(),
                span: assembly.call_spans[frame.call_span].clone(),
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
fn run_assembly_inner(assembly: &Assembly, call_stack: &mut Vec<StackFrame>) -> RuntimeResult {
    let mut stack = Vec::new();
    dprintln!("Running...");
    let mut pc = assembly.start;
    #[cfg(feature = "profile")]
    let mut i = 0;
    while pc < assembly.instrs.len() {
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
        let instr = &assembly.instrs[pc];
        dprintln!("{pc:>3} {instr}");
        match instr {
            Instr::Comment(_) => {}
            Instr::Push(v) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("push");
                stack.push(v.clone())
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
            Instr::Call {
                args: call_arg_count,
                span,
            } => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("call");
                let (function, args) = match Function::try_from(stack.pop().unwrap()) {
                    Ok(func) => (func, Vec::new()),
                    Err(val) => match Partial::try_from(val) {
                        Ok(partial) => (partial.function, partial.args),
                        Err(val) => {
                            let message = format!("cannot call {}", val.ty());
                            return Err(assembly.call_spans[*span].clone().sp(message));
                        }
                    },
                };
                let partial_count = args.len();
                let arg_count = call_arg_count + partial_count;
                for arg in args {
                    stack.push(arg);
                }
                if partial_count > 0 {
                    dprintln!("{stack:?}");
                }
                let info = assembly.function_info(function);
                match arg_count.cmp(&info.params) {
                    Ordering::Less => {
                        let partial = Partial {
                            function,
                            args: stack.drain(stack.len() - arg_count..).collect(),
                        };
                        stack.push(partial.into());
                    }
                    Ordering::Equal => {
                        call_stack.push(StackFrame {
                            stack_size: stack.len() - arg_count,
                            function,
                            ret: pc + 1,
                            call_span: *span,
                        });
                        pc = function.0;
                        continue;
                    }
                    Ordering::Greater => {
                        let message = format!(
                            "too many arguments: expected {}, got {}",
                            info.params, arg_count
                        );
                        return Err(assembly.call_spans[*span].clone().sp(message));
                    }
                }
            }
            Instr::Return => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("return");
                if let Some(frame) = call_stack.pop() {
                    let value = stack.pop().unwrap();
                    pc = frame.ret;
                    stack.truncate(frame.stack_size);
                    stack.push(value);
                    dprintln!("{:?}", stack);
                    continue;
                } else {
                    break;
                }
            }
            Instr::Jump(delta) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("jump");
                pc = pc.wrapping_add_signed(*delta);
                continue;
            }
            Instr::PopJumpIf(delta, cond) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("jump_if");
                let val = stack.pop().unwrap();
                if val.is_truthy() == *cond {
                    pc = pc.wrapping_add_signed(*delta);
                    continue;
                }
            }
            Instr::JumpIfElsePop(delta, cond) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("jump_if_else_pop");
                let val = stack.last().unwrap();
                if val.is_truthy() == *cond {
                    pc = pc.wrapping_add_signed(*delta);
                    continue;
                } else {
                    stack.pop();
                }
            }
            Instr::BinOp(op, span) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("bin_op");
                let (left, right) = {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("bin_args");
                    let right = stack.pop().unwrap();
                    let left = stack.last_mut().unwrap();
                    (left, right)
                };
                left.bin_op(right, *op, span)?;
            }
            Instr::BuiltinOp1(op, span) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("builtin_op1");
                let val = stack.last_mut().unwrap();
                val.op1(*op, span)?;
            }
            Instr::BuiltinOp2(op, span) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("builtin_op2");
                let (left, mut right) = {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("builtin_op2_args");
                    let right = stack.pop().unwrap();
                    let left = stack.last_mut().unwrap();
                    (left, right)
                };
                swap(left, &mut right);
                left.op2(right, *op, span)?;
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
        dprintln!("{:?}", stack);
        pc += 1;
    }
    println!("\nstack:");
    for val in &stack {
        println!("{:?}", val);
    }
    Ok(())
}
