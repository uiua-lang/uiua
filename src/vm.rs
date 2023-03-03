use std::{fmt, fs, path::Path};

use crate::{
    ast::BinOp,
    compile::{Assembly, Compiler},
    lex::Span,
    value::{init_tables, Value},
    UiuaError, UiuaResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instr {
    Comment(String),
    Push(Value),
    Copy(usize),
    Call(usize, Span),
    Return,
    Jump(isize),
    JumpIf(isize, bool),
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

#[derive(Default)]
pub struct Vm {
    instrs: Vec<Instr>,
    stack: Vec<Value>,
    call_stack: Vec<StackFrame>,
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

impl Vm {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn run_file(&mut self, path: impl AsRef<Path>) -> UiuaResult {
        init_tables();
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        let mut compiler = Compiler::new();
        compiler.load(&input, path)?;
        let assembly = compiler.finish();
        self.instrs = assembly.instrs;
        for (i, instr) in self.instrs.iter().enumerate() {
            println!("{i:>3} {instr}");
        }
        println!();
        self.run(assembly.start)
    }
    pub fn run_assembly(&mut self, assembly: Assembly) -> UiuaResult {
        self.instrs = assembly.instrs;
        self.run(assembly.start)
    }
    fn run(&mut self, mut pc: usize) -> UiuaResult {
        println!("Running...");
        #[cfg(feature = "profile")]
        let mut i = 0;
        while pc < self.instrs.len() {
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
            let instr = &self.instrs[pc];
            dprintln!("{pc:>3} {instr}");
            match instr {
                Instr::Comment(_) => {}
                Instr::Push(v) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("push");
                    self.stack.push(v.clone())
                }
                Instr::Copy(n) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("copy");
                    self.stack.push(self.stack[self.stack.len() - *n].clone())
                }
                Instr::Call(arg_count, _) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("call");
                    let func = self.stack.pop().unwrap();
                    let index = match func.as_function() {
                        Some(func) => func,
                        _ => {
                            let message = format!("cannot call {}", func.ty());
                            return Err(Span::default().sp(message).into());
                        }
                    };
                    self.call_stack.push(StackFrame {
                        ret: pc + 1,
                        stack_size: self.stack.len() - arg_count,
                    });
                    pc = index;
                    continue;
                }
                Instr::Return => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("return");
                    if let Some(frame) = self.call_stack.pop() {
                        let value = self.stack.pop().unwrap();
                        pc = frame.ret;
                        self.stack.truncate(frame.stack_size);
                        self.stack.push(value);
                        dprintln!("{:?}", self.stack);
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
                Instr::JumpIf(delta, cond) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("jump_if");
                    let val = self.stack.pop().unwrap();
                    if val.is_truthy() == *cond {
                        pc = pc.wrapping_add_signed(*delta);
                        continue;
                    }
                }
                Instr::JumpIfElsePop(delta, cond) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("jump_if_else_pop");
                    let val = self.stack.last().unwrap();
                    if val.is_truthy() == *cond {
                        pc = pc.wrapping_add_signed(*delta);
                        continue;
                    } else {
                        self.stack.pop();
                    }
                }
                Instr::BinOp(op, span) => {
                    #[cfg(feature = "profile")]
                    puffin::profile_scope!("bin_op");
                    let (left, right) = {
                        #[cfg(feature = "profile")]
                        puffin::profile_scope!("bin_args");
                        let right = self.stack.pop().unwrap();
                        let left = self.stack.last_mut().unwrap();
                        (left, right)
                    };
                    left.bin_op(right, *op, span)?;
                }
                Instr::DestructureList(_n, _span) => {
                    // let list = match self.stack.pop().unwrap() {
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
                    //     self.stack.push(val);
                    // }
                }
                Instr::Dud => {
                    panic!("unresolved instruction")
                }
            }
            dprintln!("{:?}", self.stack);
            pc += 1;
        }
        println!("\nstack:");
        for val in &self.stack {
            println!("{:?}", val);
        }
        Ok(())
    }
}
