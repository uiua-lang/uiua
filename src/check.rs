use std::cmp::Ordering;

use crate::{
    array::Array,
    function::{Function, Instr, Signature},
    primitive::Primitive,
    value::Value,
};

/// Count the number of arguments and the stack Δ of a function.
pub(crate) fn instrs_signature(instrs: &[Instr]) -> Result<Signature, String> {
    if let [Instr::Prim(prim, _)] = instrs {
        if let Some((args, outputs)) = prim.args().zip(prim.outputs()) {
            return Ok(Signature {
                args: args as usize,
                outputs: outputs as usize,
            });
        }
    }
    // println!("Checking {:?}", instrs);
    const START_HEIGHT: usize = 16;
    let mut env = VirtualEnv {
        stack: vec![BasicValue::Unknown; START_HEIGHT],
        array_stack: Vec::new(),
        min_height: START_HEIGHT,
    };
    env.instrs(instrs)?;
    let args = START_HEIGHT.saturating_sub(env.min_height);
    let outputs = env.stack.len() - env.min_height;
    // println!("Checked {:?} -> {}/{}", instrs, args, outputs);
    Ok(Signature { args, outputs })
}

/// An environment that emulates the runtime but only keeps track of the stack.
struct VirtualEnv<'a> {
    stack: Vec<BasicValue<'a>>,
    array_stack: Vec<usize>,
    min_height: usize,
}

#[derive(Debug, Clone)]
enum BasicValue<'a> {
    Func(&'a Function),
    Num(f64),
    Arr(Vec<Self>),
    Other,
    Unknown,
}

impl<'a> BasicValue<'a> {
    fn signature(&self) -> Signature {
        match self {
            BasicValue::Func(f) => f.signature(),
            BasicValue::Num(_) => Signature {
                args: 0,
                outputs: 1,
            },
            BasicValue::Arr(_) => Signature {
                args: 0,
                outputs: 1,
            },
            BasicValue::Other | BasicValue::Unknown => Signature {
                args: 0,
                outputs: 1,
            },
        }
    }
    fn from_val(value: &'a Value) -> Self {
        if let Some(f) = value.as_func_array().and_then(Array::as_scalar) {
            BasicValue::Func(f)
        } else if let Some(n) = value.as_num_array().and_then(Array::as_scalar) {
            BasicValue::Num(*n)
        } else if value.rank() == 1 {
            BasicValue::Arr(match value {
                Value::Num(n) => n.data.iter().map(|n| BasicValue::Num(*n)).collect(),
                Value::Byte(b) => b.data.iter().map(|b| BasicValue::Num(*b as f64)).collect(),
                Value::Char(c) => c.data.iter().map(|_| BasicValue::Other).collect(),
                Value::Func(f) => f.data.iter().map(|f| BasicValue::Func(f)).collect(),
            })
        } else {
            BasicValue::Other
        }
    }
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
            Instr::Push(val) => self.stack.push(BasicValue::from_val(val)),
            Instr::BeginArray => self.array_stack.push(self.stack.len()),
            Instr::EndArray { .. } => {
                let bottom = self
                    .array_stack
                    .pop()
                    .ok_or("EndArray without BeginArray")?;
                let mut items: Vec<_> = self.stack.drain(bottom..).collect();
                items.reverse();
                self.stack.push(BasicValue::Arr(items));
            }
            Instr::Call(_) => self.handle_call(false)?,
            Instr::Prim(prim, _) => match prim {
                Reduce | Scan => self.handle_mod(prim, Some(2), Some(1), 1, None)?,
                Fold => self.handle_mod(prim, Some(2), Some(1), 2, None)?,
                Each | Rows => self.handle_variadic_mod(prim)?,
                Distribute | Table | Cross => self.handle_mod(prim, Some(2), Some(1), 2, None)?,
                Group | Partition => {
                    if let BasicValue::Func(f) = self.pop()? {
                        let sig = f.signature();
                        if sig.outputs == 0 && sig.args != 0 {
                            return Err(format!("{prim}'s function {f} has no outputs"));
                        }
                        self.pop()?; // Pop the array
                        self.set_min_height();
                        match sig.args {
                            0 => {}
                            1 | 2 => {
                                if sig.outputs != 1 {
                                    return Err(format!(
                                        "{prim}'s function {f} has {} outputs, expected 1",
                                        sig.outputs
                                    ));
                                }
                            }
                            n => {
                                return Err(format!(
                                    "{prim}'s function {f} has {n} args, expected 0, 1, or 2"
                                ))
                            }
                        }
                    } else {
                        return Err(format!("{prim} with non-function"));
                    }
                }
                Spawn => self.handle_mod(prim, None, None, 1, Some(1))?,
                Repeat => {
                    let f = self.pop()?;
                    let n = self.pop()?;
                    if let BasicValue::Num(n) = n {
                        // If n is a known natural number, then the function can have any signature.
                        if n.fract() == 0.0 && n >= 0.0 {
                            let n = n as usize;
                            if n > 0 {
                                if let BasicValue::Func(f) = f {
                                    let sig = f.signature();
                                    let (args, outputs) = match sig.args.cmp(&sig.outputs) {
                                        Ordering::Equal => (sig.args, sig.outputs),
                                        Ordering::Less => (
                                            (n - 1) * (sig.args - sig.outputs) + sig.args,
                                            sig.outputs,
                                        ),
                                        Ordering::Greater => {
                                            (sig.args, (n - 1) * (sig.outputs - sig.args) + 1)
                                        }
                                    };
                                    for _ in 0..args {
                                        self.pop()?;
                                    }
                                    self.set_min_height();
                                    for _ in 0..outputs {
                                        self.stack.push(BasicValue::Other);
                                    }
                                } else {
                                    self.handle_mod(prim, Some(0), Some(1), n, None)?
                                }
                            }
                        } else {
                            return Err("repeat without a natural number".into());
                        }
                    } else if let BasicValue::Func(f) = f {
                        // If n is unknown, then the function must be compatible with |1.1
                        let sig = f.signature();
                        if f.signature().is_compatible_with(Signature::new(1, 1)) {
                            for _ in 0..sig.args {
                                self.pop()?;
                            }
                            self.set_min_height();
                            for _ in 0..sig.outputs {
                                self.stack.push(BasicValue::Other);
                            }
                        } else {
                            return Err(format!(
                                "repeat with no number and a function with signature {sig}"
                            ));
                        }
                    } else {
                        return Err("repeat without a number".into());
                    }
                }
                Fork => {
                    let f = self.pop()?;
                    let g = self.pop()?;
                    self.pop()?;
                    self.pop()?;
                    self.set_min_height();
                    let f_out = f.signature().outputs.max(1);
                    let g_out = g.signature().outputs.max(1);
                    for _ in 0..f_out + g_out {
                        self.stack.push(BasicValue::Other);
                    }
                }
                Trident => {
                    let f = self.pop()?;
                    let g = self.pop()?;
                    let h = self.pop()?;
                    self.pop()?;
                    self.pop()?;
                    self.pop()?;
                    self.set_min_height();
                    let f_out = f.signature().outputs.max(1);
                    let g_out = g.signature().outputs.max(1);
                    let h_out = h.signature().outputs.max(1);
                    for _ in 0..f_out + g_out + h_out {
                        self.stack.push(BasicValue::Other);
                    }
                }
                Level => {
                    let arg_count = match self.pop()? {
                        BasicValue::Arr(items) => items.len(),
                        _ => 1,
                    };
                    let f = self.pop()?;
                    for _ in 0..arg_count {
                        self.pop()?;
                    }
                    self.set_min_height();
                    for _ in 0..f.signature().outputs {
                        self.stack.push(BasicValue::Other);
                    }
                }
                Try => {
                    let f = self.pop()?;
                    let handler = self.pop()?;
                    let f_sig = f.signature();
                    let handler_sig = handler.signature();
                    if !f_sig.is_compatible_with(handler_sig) {
                        return Err(format!(
                            "try's functions have incompatible signatures {f_sig} and {handler_sig}"
                        ));
                    }
                    let sig = f_sig.max_with(handler_sig);
                    for _ in 0..sig.args {
                        self.pop()?;
                    }
                    self.set_min_height();
                    for _ in 0..sig.outputs {
                        self.stack.push(BasicValue::Other);
                    }
                }
                Invert => {
                    if let BasicValue::Func(f) = self.pop()? {
                        if let Some(inverted) = f.inverse() {
                            let sig = inverted.signature();
                            for _ in 0..sig.args {
                                self.pop()?;
                            }
                            self.set_min_height();
                            for _ in 0..sig.outputs {
                                self.stack.push(BasicValue::Other);
                            }
                        } else {
                            // We could return an error here,
                            // but the "no inverse found" error is more useful.
                        }
                    } else {
                        return Err("invert with non-function".into());
                    }
                }
                Under => {
                    let f = self.pop()?;
                    let g = self.pop()?;
                    if let (BasicValue::Func(f), BasicValue::Func(_)) = (f, g) {
                        if let Some((before, after)) = f.clone().under() {
                            let before_sig = before.signature();
                            let after_sig = after.signature();
                            self.handle_sig(before_sig)?;
                            self.handle_sig(Signature::new(1, 1))?;
                            self.handle_sig(after_sig)?;
                        } else {
                            // We could return an error here,
                            // but the "no inverse found" error is more useful.
                        }
                    } else {
                        return Err("under with non-function".into());
                    }
                }
                Fill => {
                    self.pop()?;
                    let f = self.pop()?;
                    self.handle_sig(f.signature())?;
                }
                Dup => {
                    let val = self.pop()?;
                    self.set_min_height();
                    self.stack.push(val.clone());
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
                    self.stack.push(b.clone());
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Roll => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let c = self.pop()?;
                    self.set_min_height();
                    self.stack.push(a);
                    self.stack.push(c);
                    self.stack.push(b);
                }
                Unroll => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let c = self.pop()?;
                    self.set_min_height();
                    self.stack.push(b);
                    self.stack.push(a);
                    self.stack.push(c);
                }
                Restack => {
                    let ns = match self.pop()? {
                        BasicValue::Arr(items) => {
                            let mut ns = Vec::with_capacity(items.len());
                            for item in items {
                                if let BasicValue::Num(n) = item {
                                    ns.push(n);
                                } else {
                                    return Err("restack with an unknown index".into());
                                }
                            }
                            ns
                        }
                        BasicValue::Num(n) => vec![n],
                        _ => return Err("restack without an array".into()),
                    };
                    if ns.is_empty() {
                        self.set_min_height();
                    } else {
                        let mut indices = Vec::with_capacity(ns.len());
                        for n in ns {
                            if n.fract() == 0.0 && n >= 0.0 {
                                indices.push(n as usize);
                            } else {
                                return Err("restack with a non-natural index".into());
                            }
                        }
                        let max_index = *indices.iter().max().unwrap();
                        let mut values = Vec::with_capacity(max_index + 1);
                        for _ in 0..=max_index {
                            values.push(self.pop()?);
                        }
                        self.set_min_height();
                        for index in indices.into_iter().rev() {
                            self.stack.push(values[index].clone());
                        }
                    }
                }
                Call => self.handle_call(true)?,
                Recur => return Err("recur present".into()),
                prim => {
                    let args = prim
                        .args()
                        .ok_or_else(|| format!("{prim} has indeterminate args"))?;
                    for _ in 0..args {
                        self.pop()?;
                    }
                    self.set_min_height();
                    let outputs = prim
                        .outputs()
                        .ok_or_else(|| format!("{prim} has indeterminate outputs"))?;
                    for _ in 0..outputs {
                        self.stack.push(BasicValue::Other);
                    }
                }
            },
        }
        self.set_min_height();
        // println!("{instr:?} -> {}/{}", self.min_height, self.stack.len());
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
    fn handle_sig(&mut self, sig: Signature) -> Result<(), String> {
        for _ in 0..sig.args {
            self.pop()?;
        }
        self.set_min_height();
        for _ in 0..sig.outputs {
            self.stack.push(BasicValue::Other);
        }
        Ok(())
    }
    fn handle_call(&mut self, explicit: bool) -> Result<(), String> {
        match self.pop()? {
            BasicValue::Func(f) => {
                let sig = f.signature();
                for _ in 0..sig.args {
                    self.pop()?;
                }
                self.set_min_height();
                for _ in 0..sig.outputs {
                    self.stack.push(BasicValue::Other);
                }
            }
            BasicValue::Arr(items) if explicit => {
                let mut fs = Vec::new();
                for item in items {
                    if let BasicValue::Func(f) = item {
                        fs.push(f);
                    } else {
                        return Err("call with non-function array".into());
                    }
                }
                let mut max_args = 0;
                let mut max_outputs = 0;
                for f in &fs {
                    let sig = f.signature();
                    max_args = max_args.max(sig.args);
                    max_outputs = max_outputs.max(sig.outputs);
                }
                for win in fs.windows(2) {
                    if !win[0].signature().is_compatible_with(win[1].signature()) {
                        return Err("call with incompatible functions".into());
                    }
                }
                self.pop()?; // Pop the index
                for _ in 0..max_args {
                    self.pop()?;
                }
                self.set_min_height();
                for _ in 0..max_outputs {
                    self.stack.push(BasicValue::Other);
                }
            }
            BasicValue::Unknown => return Err("call with unknown function".into()),
            val => self.stack.push(val),
        }
        Ok(())
    }
    fn handle_mod(
        &mut self,
        prim: &Primitive,
        f_args: Option<usize>,
        f_outputs: Option<usize>,
        m_args: usize,
        m_outputs: Option<usize>,
    ) -> Result<(), String> {
        if let BasicValue::Func(f) = self.pop()? {
            let sig = f.signature();
            if let Some(f_args) = f_args {
                if sig.args != f_args {
                    return Err(format!(
                        "{prim}'s function {f:?} has {} args, expected {}",
                        sig.args, f_args
                    ));
                }
            }
            if let Some(f_outputs) = f_outputs {
                if sig.outputs != f_outputs {
                    return Err(format!(
                        "{prim}'s function {f:?} has {} outputs, expected {}",
                        sig.outputs, f_outputs
                    ));
                }
            }
            for _ in 0..m_args {
                self.pop()?;
            }
            self.set_min_height();
            let outputs = m_outputs.unwrap_or(f_outputs.unwrap_or(sig.outputs));
            for _ in 0..outputs {
                self.stack.push(BasicValue::Other);
            }
            Ok(())
        } else {
            Err(format!("{prim} without function"))
        }
    }
    fn handle_variadic_mod(&mut self, prim: &Primitive) -> Result<(), String> {
        if let BasicValue::Func(f) = self.pop()? {
            let sig = f.signature();
            if sig.outputs != 1 {
                return Err(format!("{prim}'s function {f:?} did not return 1 value",));
            }
            for _ in 0..sig.args {
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
    fn instrs_signature() {
        let check = super::instrs_signature;
        fn sig(a: usize, o: usize) -> Signature {
            Signature {
                args: a,
                outputs: o,
            }
        }
        assert_eq!(Ok(sig(0, 0)), check(&[]));
        assert_eq!(Ok(sig(0, 0)), check(&[Prim(Noop, 0)]));

        assert_eq!(Ok(sig(0, 1)), check(&[push(10), push(2), Prim(Pow, 0)]));
        assert_eq!(
            Ok(sig(1, 1)),
            check(&[push(10), push(2), Prim(Pow, 0), Prim(Add, 0)])
        );
        assert_eq!(Ok(sig(1, 1)), check(&[push(1), Prim(Add, 0)]));

        assert_eq!(
            Ok(sig(0, 1)),
            check(&[
                BeginArray,
                push(3),
                push(2),
                push(1),
                EndArray {
                    span: 0,
                    constant: false
                }
            ])
        );
        assert_eq!(
            Ok(sig(1, 1)),
            check(&[
                BeginArray,
                push(3),
                push(2),
                push(1),
                EndArray {
                    span: 0,
                    constant: false
                },
                Prim(Add, 0)
            ])
        );
    }
}
