use std::{borrow::Cow, cmp::Ordering, fmt};

use crate::{
    array::Array,
    function::{Function, FunctionId, Instr, Signature},
    primitive::Primitive,
    value::Value,
};

/// Count the number of arguments and the stack Δ of a function.
pub(crate) fn instrs_signature(instrs: &[Instr]) -> Result<Signature, String> {
    if let [Instr::Prim(prim, _)] = instrs {
        if let Some((args, outputs)) = prim.args().zip(prim.outputs()) {
            return Ok(Signature {
                args: args as usize + prim.modifier_args().unwrap_or(0) as usize,
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
    Func(Cow<'a, Function>),
    Num(f64),
    Arr(Vec<Self>),
    Other,
    Unknown,
    DifferentSignatures,
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
            BasicValue::Other | BasicValue::Unknown | BasicValue::DifferentSignatures => {
                Signature {
                    args: 0,
                    outputs: 1,
                }
            }
        }
    }
    fn from_val(value: &'a Value) -> Self {
        if let Some(f) = value.as_func_array().and_then(Array::as_scalar) {
            BasicValue::Func(Cow::Borrowed(f))
        } else if let Some(n) = value.as_num_array().and_then(Array::as_scalar) {
            BasicValue::Num(*n)
        } else if let Some(n) = value.as_byte_array().and_then(Array::as_scalar) {
            BasicValue::Num(*n as f64)
        } else if value.rank() == 1 {
            BasicValue::Arr(match value {
                Value::Num(n) => n.data.iter().map(|n| BasicValue::Num(*n)).collect(),
                Value::Byte(b) => b.data.iter().map(|b| BasicValue::Num(*b as f64)).collect(),
                Value::Char(c) => c.data.iter().map(|_| BasicValue::Other).collect(),
                Value::Func(f) => f
                    .data
                    .iter()
                    .map(|f| BasicValue::Func(Cow::Borrowed(f)))
                    .collect(),
            })
        } else {
            BasicValue::Other
        }
    }
    fn expect_function<T: fmt::Display>(
        &self,
        err: impl FnOnce() -> T,
    ) -> Result<Signature, String> {
        match self {
            BasicValue::Func(f) => Ok(f.signature()),
            BasicValue::Unknown | BasicValue::Other => {
                Err(format!("{} with unknown function", err()))
            }
            _ => Err(format!("{} with non-function", err())),
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
            Instr::Call(_) => self.handle_call()?,
            Instr::PushTempInline { count, .. } | Instr::PushTempUnder { count, .. } => {
                self.handle_args_outputs(*count, 0)?
            }
            Instr::PopTempInline { count, .. }
            | Instr::PopTempUnder { count, .. }
            | Instr::CopyTempInline { count, .. } => self.handle_args_outputs(0, *count)?,
            Instr::Dynamic(f) => self.handle_sig(f.signature)?,
            Instr::DropTempInline { .. } => {}
            Instr::Prim(prim, _) => match prim {
                Reduce | Scan => {
                    let sig = self.pop()?.expect_function(|| prim)?;
                    let outputs = match (sig.args, sig.outputs) {
                        (0, _) => return Err(format!("{prim}'s function has no args")),
                        (1, 0) => 0,
                        (1, _) if *prim == Reduce && !self.array_stack.is_empty() => 1,
                        (1, _) => return Err(format!("{prim}'s function's signature is {sig}")),
                        (2, 1) => 1,
                        _ => return Err(format!("{prim}'s function's signature is {sig}")),
                    };
                    self.handle_args_outputs(1, outputs)?;
                }
                Each | Rows => self.handle_variadic_mod(prim)?,
                Table | Cross => self.handle_mod(prim, Some(2), Some(1), 2, None)?,
                Distribute => {
                    let sig = self.pop()?.expect_function(|| prim)?;
                    self.handle_sig(sig)?
                }
                Group | Partition => {
                    let sig = self.pop()?.expect_function(|| prim)?;
                    let (args, outputs) = match sig.args {
                        0 => (2, 0),
                        1 => (2, 1),
                        2 => (3, 1),
                        _ => {
                            return Err(format!(
                                "{prim}'s function must take at most 2 arguments, \
                                    but its signature is {sig}",
                            ))
                        }
                    };
                    self.handle_args_outputs(args, outputs)?;
                }
                Spawn => self.handle_mod(prim, None, None, 1, Some(1))?,
                Repeat => {
                    let f = self.pop()?;
                    let n = self.pop()?;
                    // Break anywhere but the end of the function prevents signature checking.
                    if let BasicValue::Func(f) = &f {
                        if instrs_contain_break_at_non_end(&f.instrs) {
                            return Err("repeat with break not at the end".into());
                        }
                    }
                    if let BasicValue::Num(n) = n {
                        // If n is a known natural number, then the function can have any signature.
                        if n.fract() == 0.0 && n >= 0.0 {
                            let n = n as usize;
                            if n > 0 {
                                if let BasicValue::Func(f) = f {
                                    let sig = f.signature();
                                    let (args, outputs) = match sig.args.cmp(&sig.outputs) {
                                        Ordering::Equal => (sig.args, sig.outputs),
                                        Ordering::Less => {
                                            (sig.args, n * (sig.outputs - sig.args) + sig.args)
                                        }
                                        Ordering::Greater => (
                                            (n - 1) * (sig.args - sig.outputs) + sig.args,
                                            sig.outputs,
                                        ),
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
                            // If we are creating an array, then the function just has to have more outputs than args.
                            let creating_array =
                                sig.args < sig.outputs && !self.array_stack.is_empty();
                            if creating_array {
                                for _ in 0..sig.args {
                                    self.pop()?;
                                }
                                self.set_min_height();
                            } else {
                                return Err(format!(
                                    "repeat with no number and a function with signature {sig}"
                                ));
                            };
                        }
                    } else {
                        return Err("repeat without a number".into());
                    }
                }
                Fold => {
                    let sig = self.pop()?.expect_function(|| prim)?;
                    if sig.args.saturating_sub(sig.outputs) != 1 {
                        return Err(format!(
                            "fold's function's signature {sig} does \
                            not have 1 more argument than output"
                        ));
                    }
                    self.handle_sig(sig)?
                }
                Bind => {
                    let f = self.pop()?;
                    let g = self.pop()?;
                    for val in [g, f] {
                        for _ in 0..val.signature().args {
                            self.pop()?;
                        }
                        self.set_min_height();
                        for _ in 0..val.signature().outputs {
                            self.stack.push(BasicValue::Other);
                        }
                    }
                }
                Both => {
                    let sig = self.pop()?.expect_function(|| prim)?;
                    let args = sig.args * 2;
                    let outputs = sig.outputs * 2;
                    self.handle_args_outputs(args, outputs)?;
                }
                Fork => {
                    let f_sig = self.pop()?.expect_function(|| prim)?;
                    let g_sig = self.pop()?.expect_function(|| prim)?;
                    let args = f_sig.args.max(g_sig.args);
                    let outputs = f_sig.outputs + g_sig.outputs;
                    self.handle_args_outputs(args, outputs)?;
                }
                Bracket => {
                    let f_sig = self.pop()?.expect_function(|| prim)?;
                    let g_sig = self.pop()?.expect_function(|| prim)?;
                    let args = f_sig.args + g_sig.args;
                    let outputs = f_sig.outputs + g_sig.outputs;
                    self.handle_args_outputs(args, outputs)?;
                }
                If => {
                    let if_true = self.pop()?;
                    let if_false = self.pop()?;
                    let _cond = self.pop()?;
                    let if_true_sig = if_true.signature();
                    let if_false_sig = if_false.signature();
                    if if_true_sig.outputs != if_false_sig.outputs {
                        return Err(format!(
                            "if's branches with signatures {} and {} \
                            have a different numbers of outputs",
                            if_true_sig, if_false_sig
                        ));
                    }
                    let args = if_true_sig.args.max(if_false_sig.args);
                    let outputs = if_true_sig.outputs;
                    self.handle_args_outputs(args, outputs)?;
                }
                Level => {
                    let _ranks = self.pop()?;
                    let f = self.pop()?;
                    let f_sig = f.signature();
                    self.handle_sig(f_sig)?;
                }
                Try => {
                    let f = self.pop()?;
                    let handler = self.pop()?;
                    let f_sig = f.signature();
                    let target_handler_sig = Signature::new(f_sig.args + 1, f_sig.outputs);
                    let handler_sig = handler.signature();
                    if !handler_sig.is_subset_of(target_handler_sig) {
                        return Err(format!(
                            "try's functions have incompatible signatures {f_sig} and {handler_sig}. \
                            The error handler should take one more argement than the function."
                        ));
                    }
                    self.handle_sig(f_sig)?;
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
                    self.set_min_height();
                    if let BasicValue::Func(f) = f {
                        if let Some((before, after)) = f.into_owned().under() {
                            let before_sig = before.signature();
                            let after_sig = after.signature();
                            self.handle_sig(before_sig)?;
                            self.handle_sig(g.signature())?;
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
                Dip => {
                    let f = self.pop()?;
                    let x = self.pop()?;
                    self.set_min_height();
                    self.handle_sig(f.signature())?;
                    self.stack.push(x);
                }
                Gap => {
                    let f = self.pop()?;
                    self.pop()?;
                    self.set_min_height();
                    self.handle_sig(f.signature())?;
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
                Join => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.set_min_height();
                    match (a, b) {
                        (BasicValue::Arr(mut a), BasicValue::Arr(b)) => {
                            a.extend(b);
                            self.stack.push(BasicValue::Arr(a));
                        }
                        (BasicValue::Arr(mut a), b) => {
                            a.push(b);
                            self.stack.push(BasicValue::Arr(a));
                        }
                        (a, BasicValue::Arr(mut b)) => {
                            b.insert(0, a);
                            self.stack.push(BasicValue::Arr(b));
                        }
                        (a, b) => {
                            self.stack.push(BasicValue::Arr(vec![a, b]));
                        }
                    }
                }
                Pick => {
                    let _index = self.pop()?;
                    let arr = self.pop()?;
                    self.set_min_height();
                    match arr {
                        BasicValue::Arr(arr) if !arr.is_empty() => {
                            let mut items = arr.iter();
                            let mut sig = items.next().unwrap().signature();
                            let mut function = None;
                            for item in items {
                                if item.signature().is_compatible_with(sig) {
                                    sig = sig.max_with(item.signature());
                                    if let BasicValue::Func(f) = item {
                                        if function.is_none() {
                                            function = Some(f);
                                        }
                                    }
                                } else {
                                    self.stack.push(BasicValue::DifferentSignatures);
                                    return Ok(());
                                }
                            }
                            let (id, instrs) = if let Some(f) = function {
                                (f.id.clone(), f.instrs.as_slice())
                            } else {
                                (FunctionId::Constant, Default::default())
                            };
                            let f = Function::new(id, instrs, sig);
                            self.stack.push(BasicValue::Func(Cow::Owned(f)));
                        }
                        _ => self.stack.push(BasicValue::Other),
                    }
                }
                Call => self.handle_call()?,
                Recur => return Err("recur present".into()),
                prim => {
                    let array_args = prim
                        .args()
                        .ok_or_else(|| format!("{prim} has indeterminate args"))?;
                    let function_args = prim.modifier_args().unwrap_or(0);
                    let args = array_args + function_args;
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
        // println!("{instr:?} -> {}/{}", self.min_height, self.stack.len());
        Ok(())
    }
    // Simulate popping a value. Errors if the stack is empty, which means the function is too complex.
    fn pop(&mut self) -> Result<BasicValue<'a>, String> {
        Ok(self.stack.pop().ok_or("function is too complex")?)
    }
    /// Set the current stack height as a potential minimum.
    /// At the end of checking, the minimum stack height is a component in calculating the signature.
    fn set_min_height(&mut self) {
        self.min_height = self.min_height.min(self.stack.len());
        if let Some(h) = self.array_stack.last_mut() {
            *h = (*h).min(self.stack.len());
        }
    }
    fn handle_args_outputs(&mut self, args: usize, outputs: usize) -> Result<(), String> {
        for _ in 0..args {
            self.pop()?;
        }
        self.set_min_height();
        for _ in 0..outputs {
            self.stack.push(BasicValue::Other);
        }
        Ok(())
    }
    fn handle_sig(&mut self, sig: Signature) -> Result<(), String> {
        self.handle_args_outputs(sig.args, sig.outputs)
    }
    fn handle_call(&mut self) -> Result<(), String> {
        match self.pop()? {
            BasicValue::Func(f) => self.handle_sig(f.signature())?,
            BasicValue::Unknown => return Err("call with unknown function".into()),
            BasicValue::DifferentSignatures => {
                return Err("call could potentially have different signatures".into())
            }
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

fn instrs_contain_break_at_non_end(instrs: &[Instr]) -> bool {
    for (i, instr) in instrs.iter().enumerate() {
        match instr {
            Instr::Prim(Primitive::Break, _) => {
                if i != instrs.len() - 1 {
                    return true;
                }
            }
            Instr::Push(val) => {
                if let Some(f) = val.as_func_array() {
                    for f in &f.data {
                        if instrs_contain_break_at_non_end(&f.instrs) {
                            return true;
                        }
                    }
                }
            }
            _ => (),
        }
    }
    false
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
        assert_eq!(Ok(sig(1, 1)), check(&[Prim(Identity, 0)]));

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
                    boxed: false
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
                    boxed: false
                },
                Prim(Add, 0)
            ])
        );
    }
}
