use crate::{
    array::Array,
    function::{Function, Instr},
    primitive::Primitive,
};

/// Count the number of arguments and the stack Δ of a function.
pub(crate) fn instrs_stack_delta(instrs: &[Instr]) -> Option<(usize, isize)> {
    const START_HEIGHT: usize = 16;
    let mut env = VirtualEnv {
        stack: vec![BasicValue::Other; START_HEIGHT],
        array_stack: Vec::new(),
        min_height: START_HEIGHT,
    };
    if let Err(_e) = env.instrs(instrs) {
        // println!("instrs: {:?}", instrs);
        // println!("unable to count a/Δ: {}", _e);
        return None;
    }
    let args = START_HEIGHT.saturating_sub(env.min_height);
    let delta = env.stack.len() as isize - START_HEIGHT as isize;
    // println!("instrs: {:?}", instrs);
    // println!("args/Δ: {}/{}", args, delta);
    Some((args, delta))
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
    Other,
}

impl<'a> VirtualEnv<'a> {
    pub fn instrs(&mut self, instrs: &'a [Instr]) -> Result<(), String> {
        use Primitive::*;
        for instr in instrs {
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
                    self.stack.truncate(
                        self.array_stack
                            .pop()
                            .ok_or("EndArray without BeginArray")?,
                    );
                    self.stack.push(BasicValue::Other);
                }
                Instr::Prim(prim, _) => match prim {
                    Reduce | Scan => self.handle_mod(prim, 2, -1, 1)?,
                    Fold => self.handle_mod(prim, 2, -1, 2)?,
                    Each | Rows => self.handle_mod(prim, 1, 0, 1)?,
                    Distribute | Plow | Table | Cross => self.handle_mod(prim, 2, -1, 2)?,
                    Spawn => {
                        if let Some(BasicValue::Num(n)) = self.stack.pop() {
                            if n.fract() == 0.0 && n >= 0.0 {
                                self.handle_mod(prim, 1, 0, n as usize)?
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
                                    let (f_args, f_delta) = f.args_delta().ok_or_else(|| {
                                        format!("Repeat's function {f:?} had indeterminate a/Δ")
                                    })?;
                                    let m_args = -f_delta.min(0) as usize * n;
                                    self.stack.push(BasicValue::Func(f));
                                    self.handle_mod(prim, f_args, f_delta, m_args)?
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
                        self.stack.push(val);
                        self.stack.push(val);
                    }
                    Flip => {
                        let a = self.pop()?;
                        let b = self.pop()?;
                        self.stack.push(a);
                        self.stack.push(b);
                    }
                    Pop => {
                        self.pop()?;
                    }
                    Over => {
                        let a = self.pop()?;
                        let b = self.pop()?;
                        self.stack.push(b);
                        self.stack.push(a);
                        self.stack.push(b);
                    }
                    Call => self.handle_call()?,
                    _ => {
                        let args = prim.args().ok_or("Prim had indeterminate args")?;
                        for _ in 0..args {
                            self.pop()?;
                        }
                        self.set_min_height();
                        let delta = prim.delta().ok_or("Prim had indeterminate delta")?;
                        for _ in 0..delta + args as i8 {
                            self.stack.push(BasicValue::Other);
                        }
                    }
                },
                Instr::Call(_) => self.handle_call()?,
            }
            self.set_min_height();
        }
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
    fn handle_call(&mut self) -> Result<(), String> {
        if let BasicValue::Func(f) = self.pop()? {
            let (a, d) = f
                .args_delta()
                .ok_or_else(|| format!("Call's function {f:?} had indeterminate a/Δ"))?;
            for _ in 0..a {
                self.pop()?;
            }
            self.set_min_height();
            for _ in 0..(d + a as isize).max(0) {
                self.stack.push(BasicValue::Other);
            }
            Ok(())
        } else {
            Err("Call without a function".into())
        }
    }
    fn handle_mod(
        &mut self,
        prim: &Primitive,
        f_args: usize,
        f_delta: isize,
        m_args: usize,
    ) -> Result<(), String> {
        if let BasicValue::Func(f) = self.pop()? {
            let (a, d) = f
                .args_delta()
                .ok_or_else(|| format!("{prim}'s function {f:?} had indeterminate a/Δ"))?;
            if a != f_args {
                return Err(format!(
                    "{prim}'s function {f:?} had {a} args, expected {f_args}"
                ));
            }
            if d != f_delta {
                return Err(format!(
                    "{prim}'s function {f:?} had {d} delta, expected {f_delta}"
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
        let check = super::instrs_stack_delta;

        assert_eq!(Some((0, 0)), check(&[]));
        assert_eq!(Some((0, 0)), check(&[Prim(Noop, 0)]));

        assert_eq!(Some((0, 1)), check(&[push(10), push(2), Prim(Pow, 0)]));
        assert_eq!(
            Some((1, 0)),
            check(&[push(10), push(2), Prim(Pow, 0), Prim(Add, 0)])
        );
        assert_eq!(Some((1, 0)), check(&[push(1), Prim(Add, 0)]));

        assert_eq!(
            Some((0, 1)),
            check(&[BeginArray, push(3), push(2), push(1), EndArray(0)])
        );
        assert_eq!(
            Some((1, 0)),
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
