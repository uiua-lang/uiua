use crate::{
    array::Array,
    function::{Function, Instr},
    primitive::Primitive,
};

/// Count the number of arguments and the stack Δ of a function.
pub(crate) fn instrs_args_outputs(instrs: &[Instr]) -> Option<(usize, usize)> {
    const START_HEIGHT: usize = 16;
    let mut env = VirtualEnv {
        stack: vec![BasicValue::Other; START_HEIGHT],
        array_stack: Vec::new(),
        min_height: START_HEIGHT,
    };
    if let Err(_e) = env.instrs(instrs) {
        // println!("instrs: {:?}", instrs);
        // println!("unable to count a/o: {}", _e);
        return None;
    }
    let args = START_HEIGHT.saturating_sub(env.min_height);
    let outputs = env.stack.len() - env.min_height;
    // println!("instrs: {:?}", instrs);
    // println!("args/outputs: {}/{}", args, outputs);
    Some((args, outputs))
}

/// An environment that emulates the runtime but only keeps track of the stack.
struct VirtualEnv<'a> {
    stack: Vec<BasicValue<'a>>,
    array_stack: Vec<usize>,
    min_height: usize,
}

#[derive(Clone, Copy)]
enum BasicValue<'a> {
    Func(&'a Function),
    Num(f64),
    Arr(usize),
    Other,
}

impl<'a> VirtualEnv<'a> {
    pub fn instrs(&mut self, instrs: &'a [Instr]) -> Result<(), String> {
        for instr in instrs {
            self.instr(instr)?;
        }
        Ok(())
    }
    fn instr(&mut self, instr: &'a Instr) -> Result<(), String> {
        use Primitive::*;
        match instr {
            Instr::Push(val) => {
                let val = if let Some(f) = val.as_func_array().and_then(Array::as_scalar) {
                    BasicValue::Func(f)
                } else if let Some(n) = val.as_num_array().and_then(Array::as_scalar) {
                    BasicValue::Num(*n)
                } else {
                    BasicValue::Other
                };
                self.stack.push(val);
            }
            Instr::DfnVal(_) => self.stack.push(BasicValue::Other),
            Instr::BeginArray => self.array_stack.push(self.stack.len()),
            Instr::EndArray(_) => {
                let len = self
                    .array_stack
                    .pop()
                    .ok_or("EndArray without BeginArray")?;
                self.stack.truncate(len);
                self.stack.push(BasicValue::Arr(len));
            }
            Instr::Prim(prim, _) => match prim {
                Reduce | Scan => self.handle_mod(prim, 2, 1, 1)?,
                Fold => self.handle_mod(prim, 2, 1, 2)?,
                Each | Rows => self.handle_variadic_mod(prim)?,
                Distribute | Table | Cross => self.handle_mod(prim, 2, 1, 2)?,
                Spawn => {
                    if let Some(BasicValue::Num(n)) = self.stack.pop() {
                        if n.fract() == 0.0 && n >= 0.0 {
                            self.handle_mod(prim, 1, 1, n as usize)?
                        } else {
                            return Err("Spawn without a natural number".into());
                        }
                    } else {
                        return Err("Spawn without a number".into());
                    }
                }
                Repeat => {
                    let f = self.pop()?;
                    let n = self.pop()?;
                    if let BasicValue::Num(n) = n {
                        if n.fract() == 0.0 && n >= 0.0 {
                            let n = n as usize;
                            if let BasicValue::Func(f) = f {
                                let (f_args, f_outputs) = f.args_outputs().ok_or_else(|| {
                                    format!("Repeat's function {f:?} had indeterminate a/o")
                                })?;
                                let m_args = f_outputs * n;
                                self.stack.push(BasicValue::Func(f));
                                self.handle_mod(prim, f_args, f_outputs, m_args)?
                            } else {
                                self.handle_mod(prim, 0, 1, n)?
                            }
                        } else {
                            return Err("Repeat without a natural number".into());
                        }
                    } else {
                        return Err("Repeat without a number".into());
                    }
                }
                Dup => {
                    let val = self.pop()?;
                    self.set_min_height();
                    self.stack.push(val);
                    self.stack.push(val);
                }
                Flip => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.set_min_height();
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Pop => {
                    self.pop()?;
                    self.set_min_height();
                }
                Over => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.set_min_height();
                    self.stack.push(b);
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Call => self.handle_call(true)?,
                Recur => return Err("Recur present".into()),
                _ => {
                    let args = prim.args().ok_or("Prim had indeterminate args")?;
                    for _ in 0..args {
                        self.pop()?;
                    }
                    self.set_min_height();
                    let outputs = prim.outputs().ok_or("Prim had indeterminate outputs")?;
                    for _ in 0..outputs {
                        self.stack.push(BasicValue::Other);
                    }
                }
            },
            Instr::Call(_) => self.handle_call(false)?,
        }
        self.set_min_height();
        Ok(())
    }
    fn pop(&mut self) -> Result<BasicValue<'a>, String> {
        Ok(self.stack.pop().ok_or("function is too complex")?)
    }
    fn set_min_height(&mut self) {
        self.min_height = self.min_height.min(self.stack.len());
        if let Some(h) = self.array_stack.last_mut() {
            *h = (*h).min(self.stack.len());
        }
    }
    fn handle_call(&mut self, explicit: bool) -> Result<(), String> {
        match self.pop()? {
            BasicValue::Func(f) => {
                let (a, o) = f
                    .args_outputs()
                    .ok_or_else(|| format!("Call's function {f:?} had indeterminate a/o"))?;
                for _ in 0..a {
                    self.pop()?;
                }
                self.set_min_height();
                for _ in 0..o {
                    self.stack.push(BasicValue::Other);
                }
            }
            val if explicit => self.stack.push(val),
            _ => return Err("Call without function".into()),
        }
        Ok(())
    }
    fn handle_mod(
        &mut self,
        prim: &Primitive,
        f_args: usize,
        f_outputs: usize,
        m_args: usize,
    ) -> Result<(), String> {
        if let BasicValue::Func(f) = self.pop()? {
            let (a, o) = f
                .args_outputs()
                .ok_or_else(|| format!("{prim}'s function {f:?} had indeterminate a/o"))?;
            if a != f_args {
                return Err(format!(
                    "{prim}'s function {f:?} had {a} args, expected {f_args}"
                ));
            }
            if o != f_outputs {
                return Err(format!(
                    "{prim}'s function {f:?} had {o} outputs, expected {f_outputs}"
                ));
            }
            for _ in 0..m_args {
                self.pop()?;
            }
            self.set_min_height();
            self.stack.push(BasicValue::Other);
            Ok(())
        } else {
            Err(format!("{prim} without function"))
        }
    }
    fn handle_variadic_mod(&mut self, prim: &Primitive) -> Result<(), String> {
        if let BasicValue::Func(f) = self.pop()? {
            let (a, o) = f
                .args_outputs()
                .ok_or_else(|| format!("{prim}'s function {f:?} had indeterminate a/o"))?;
            if o != 1 {
                return Err(format!("{prim}'s function {f:?} did not return 1 value",));
            }
            for _ in 0..a {
                self.pop()?;
            }
            self.set_min_height();
            self.stack.push(BasicValue::Other);
            Ok(())
        } else {
            Err(format!("{prim} without function"))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::value::Value;

    use super::*;
    use Instr::*;
    use Primitive::*;
    fn push<T>(val: T) -> Instr
    where
        T: Into<Value>,
    {
        Push(val.into().into())
    }
    #[test]
    fn instrs_args_outputs() {
        let check = super::instrs_args_outputs;

        assert_eq!(Some((0, 0)), check(&[]));
        assert_eq!(Some((0, 0)), check(&[Prim(Noop, 0)]));

        assert_eq!(Some((0, 1)), check(&[push(10), push(2), Prim(Pow, 0)]));
        assert_eq!(
            Some((1, 1)),
            check(&[push(10), push(2), Prim(Pow, 0), Prim(Add, 0)])
        );
        assert_eq!(Some((1, 1)), check(&[push(1), Prim(Add, 0)]));

        assert_eq!(
            Some((0, 1)),
            check(&[BeginArray, push(3), push(2), push(1), EndArray(0)])
        );
        assert_eq!(
            Some((1, 1)),
            check(&[
                BeginArray,
                push(3),
                push(2),
                push(1),
                EndArray(0),
                Prim(Add, 0)
            ])
        );
        assert_eq!(
            Some((0, 1)),
            check(&[
                BeginArray,
                push(3),
                push(2),
                push(1),
                EndArray(0),
                push(Add),
                Prim(Reduce, 0)
            ])
        );
    }
}
