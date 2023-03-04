use std::{cmp::Ordering, fmt};

use crate::{
    ast::{BinOp, FunctionId},
    compile::Assembly,
    lex::Span,
    value::{Function, Partial, Value},
    UiuaResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instr {
    Comment(String),
    Push(Value),
    /// Copy the nth value from the top of the stack
    CopyRel(usize),
    Call(usize, Span),
    Return,
    Jump(isize),
    PopJumpIf(isize, bool),
    JumpIfElsePop(isize, bool),
    BinOp(BinOp, Span),
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
    ret: usize,
    stack_size: usize,
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
    let mut stack = Vec::new();
    let mut call_stack = Vec::new();
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
            Instr::Call(call_arg_count, span) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("call");

                let (func, args) = match Function::try_from(stack.pop().unwrap()) {
                    Ok(func) => (func, Vec::new()),
                    Err(val) => match Partial::try_from(val) {
                        Ok(partial) => (partial.func, partial.args),
                        Err(val) => {
                            let message = format!("cannot call {}", val.ty());
                            return Err(span.clone().sp(message).into());
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
                let info = assembly.function_info(func);
                match arg_count.cmp(&info.params) {
                    Ordering::Less => {
                        let partial = Partial {
                            func,
                            args: stack.drain(stack.len() - arg_count..).collect(),
                        };
                        stack.push(partial.into());
                    }
                    Ordering::Equal => {
                        call_stack.push(StackFrame {
                            ret: pc + 1,
                            stack_size: stack.len() - arg_count,
                        });
                        pc = func.0;
                        continue;
                    }
                    Ordering::Greater => {
                        let message = format!(
                            "too many arguments: expected {}, got {}",
                            info.params, arg_count
                        );
                        return Err(span.clone().sp(message).into());
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
            Instr::DestructureList(_n, _span) => {
                // let list = match stack.pop().unwrap() {
                //     Value::List(list) if *n == list.len() => list,
                //     Value::List(list) => {
                //         let message =
                //             format!("cannot destructure list of {} as list of {n}", list.len());
                //         return Err(span.clone().sp(message).into());
                //     }
                //     val => {
                //         let message =
                //             format!("cannot destructure {} as list of {}", val.ty(), n);
                //         return Err(span.clone().sp(message).into());
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
