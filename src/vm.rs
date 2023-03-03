use std::fmt;

use crate::{
    ast::BinOp,
    lex::Span,
    value::{Function, Value},
    UiuaResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instr {
    Comment(String),
    Push(Value),
    Copy(usize),
    Call(usize, Span),
    Return,
    Jump(isize),
    PopJumpIf(isize, bool),
    JumpIfElsePop(isize, bool),
    BinOp(BinOp, Span),
    DestructureList(usize, Span),
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

pub(crate) fn run_instrs(instrs: &[Instr], start: usize, args: Vec<Value>) -> UiuaResult {
    let mut stack = args;
    stack.reverse();
    let mut call_stack = Vec::new();
    dprintln!("Running...");
    let mut pc = start;
    #[cfg(feature = "profile")]
    let mut i = 0;
    while pc < instrs.len() {
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
        let instr = &instrs[pc];
        dprintln!("{pc:>3} {instr}");
        match instr {
            Instr::Comment(_) => {}
            Instr::Push(v) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("push");
                stack.push(v.clone())
            }
            Instr::Copy(n) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("copy");
                stack.push(stack[stack.len() - *n].clone())
            }
            Instr::Call(arg_count, _) => {
                #[cfg(feature = "profile")]
                puffin::profile_scope!("call");
                let func = stack.pop().unwrap();
                let Function(index) = match func.as_function() {
                    Some(func) => func,
                    _ => {
                        let message = format!("cannot call {}", func.ty());
                        return Err(Span::default().sp(message).into());
                    }
                };
                call_stack.push(StackFrame {
                    ret: pc + 1,
                    stack_size: stack.len() - arg_count,
                });
                pc = index;
                continue;
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
