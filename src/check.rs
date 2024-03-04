use std::{borrow::Cow, cmp::Ordering, fmt};

use enum_iterator::Sequence;

use crate::{
    array::Array,
    function::{Function, Instr, Signature},
    value::Value,
    ImplPrimitive, Primitive, TempStack,
};

const START_HEIGHT: usize = 16;

/// Count the number of arguments and outputs of a function.
pub(crate) fn instrs_signature(instrs: &[Instr]) -> Result<Signature, SigCheckError> {
    if let [Instr::Prim(prim, _)] = instrs {
        if let Some((args, outputs)) = prim.args().zip(prim.outputs()) {
            return Ok(Signature {
                args: args + prim.modifier_args().unwrap_or(0),
                outputs,
            });
        }
    }
    let env = VirtualEnv::from_instrs(instrs)?;
    Ok(env.sig())
}

pub(crate) fn instrs_temp_signatures(
    instrs: &[Instr],
) -> Result<[Signature; TempStack::CARDINALITY], SigCheckError> {
    let env = VirtualEnv::from_instrs(instrs)?;
    Ok(env.temp_signatures())
}

/// An environment that emulates the runtime but only keeps track of the stack.
struct VirtualEnv<'a> {
    stack: Vec<BasicValue>,
    temp_stacks: [Vec<BasicValue>; TempStack::CARDINALITY],
    function_stack: Vec<Cow<'a, Function>>,
    array_stack: Vec<usize>,
    min_height: usize,
    temp_min_heights: [usize; TempStack::CARDINALITY],
    popped: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SigCheckError {
    pub message: String,
    pub ambiguous: bool,
}

impl SigCheckError {
    pub fn ambiguous(self) -> Self {
        Self {
            ambiguous: true,
            ..self
        }
    }
}

impl<'a> From<&'a str> for SigCheckError {
    fn from(s: &'a str) -> Self {
        Self {
            message: s.to_string(),
            ambiguous: false,
        }
    }
}

impl From<String> for SigCheckError {
    fn from(s: String) -> Self {
        Self {
            message: s,
            ambiguous: false,
        }
    }
}

impl fmt::Display for SigCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

#[derive(Debug, Clone)]
enum BasicValue {
    Num(f64),
    Arr(Vec<Self>),
    Other,
    Unknown(usize),
}

impl BasicValue {
    fn from_val(value: &Value) -> Self {
        if let Some(n) = value.as_num_array().and_then(Array::as_scalar) {
            BasicValue::Num(*n)
        } else if let Some(n) = value.as_byte_array().and_then(Array::as_scalar) {
            BasicValue::Num(*n as f64)
        } else if value.rank() == 1 {
            BasicValue::Arr(match value {
                Value::Num(n) => n.data.iter().map(|n| BasicValue::Num(*n)).collect(),
                #[cfg(feature = "bytes")]
                Value::Byte(b) => b.data.iter().map(|b| BasicValue::Num(*b as f64)).collect(),
                Value::Complex(c) => c.data.iter().map(|_| BasicValue::Other).collect(),
                Value::Char(c) => c.data.iter().map(|_| BasicValue::Other).collect(),
                Value::Box(b) => b.data.iter().map(|_| BasicValue::Other).collect(),
            })
        } else {
            BasicValue::Other
        }
    }
}

impl FromIterator<f64> for BasicValue {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = f64>,
    {
        BasicValue::Arr(iter.into_iter().map(BasicValue::Num).collect())
    }
}

fn derive_sig(min_height: usize, final_height: usize) -> Signature {
    Signature {
        args: START_HEIGHT.saturating_sub(min_height),
        outputs: final_height - min_height,
    }
}

impl<'a> VirtualEnv<'a> {
    fn from_instrs(instrs: &'a [Instr]) -> Result<Self, SigCheckError> {
        let mut temp_stacks = <[_; TempStack::CARDINALITY]>::default();
        for stack in temp_stacks.iter_mut() {
            *stack = vec![BasicValue::Other; START_HEIGHT];
        }
        let mut env = VirtualEnv {
            stack: (0..START_HEIGHT).rev().map(BasicValue::Unknown).collect(),
            temp_stacks,
            function_stack: Vec::new(),
            array_stack: Vec::new(),
            min_height: START_HEIGHT,
            temp_min_heights: [START_HEIGHT; TempStack::CARDINALITY],
            popped: Vec::new(),
        };
        env.instrs(instrs)?;
        Ok(env)
    }
    fn sig(&self) -> Signature {
        derive_sig(self.min_height, self.stack.len())
    }
    fn temp_signatures(&self) -> [Signature; TempStack::CARDINALITY] {
        let mut sigs = [Signature::new(0, 0); TempStack::CARDINALITY];
        for ((sig, min_height), stack) in sigs
            .iter_mut()
            .zip(&self.temp_min_heights)
            .zip(&self.temp_stacks)
        {
            *sig = derive_sig(*min_height, stack.len());
        }
        sigs
    }
    fn instrs(&mut self, instrs: &'a [Instr]) -> Result<(), SigCheckError> {
        let mut i = 0;
        while i < instrs.len() {
            match &instrs[i] {
                Instr::PushSig(sig) => {
                    let mut depth = 0;
                    i += 1;
                    while i < instrs.len() {
                        match &instrs[i] {
                            Instr::PushSig(_) => depth += 1,
                            Instr::PopSig => {
                                if depth == 0 {
                                    break;
                                } else {
                                    depth -= 1;
                                }
                            }
                            _ => {}
                        }
                        i += 1;
                    }
                    self.handle_sig(*sig)?;
                }
                Instr::PopSig => {
                    return Err(SigCheckError::from(
                        "PopSig without PushSig. \
                        It is a bug in the interpreter for you to see this message.",
                    ))
                }
                instr => self.instr(instr)?,
            }
            i += 1;
        }
        Ok(())
    }
    fn instr(&mut self, instr: &'a Instr) -> Result<(), SigCheckError> {
        use Primitive::*;
        match instr {
            Instr::Comment(_) => {}
            Instr::Push(val) => self.stack.push(BasicValue::from_val(val)),
            Instr::CallGlobal { call, .. } => {
                if *call {
                    self.handle_args_outputs(0, 1)?;
                }
            }
            Instr::BindGlobal { .. } => {
                self.pop()?;
            }
            Instr::BeginArray => self.array_stack.push(self.stack.len()),
            Instr::EndArray { .. } => {
                let bottom = self
                    .array_stack
                    .pop()
                    .ok_or("EndArray without BeginArray")?;
                let mut items: Vec<_> = self.stack.drain(bottom..).collect();
                self.set_min_height();
                items.reverse();
                self.stack.push(BasicValue::Arr(items));
            }
            Instr::Call(_) => {
                let sig = self.pop_func()?.signature();
                self.handle_sig(sig)?
            }
            Instr::PushLocals { count, .. } => self.handle_args_outputs(*count, 0)?,
            Instr::PopLocals => {}
            Instr::GetLocal { .. } => self.stack.push(BasicValue::Other),
            Instr::PushTemp { count, stack, .. } => {
                for _ in 0..*count {
                    let val = self.pop()?;
                    self.temp_stacks[*stack as usize].push(val);
                }
                self.set_min_height();
            }
            Instr::CopyToTemp { count, stack, .. } => {
                let mut vals = Vec::with_capacity(*count);
                for _ in 0..*count {
                    vals.push(self.pop()?);
                }
                self.set_min_height();
                for val in vals {
                    self.temp_stacks[*stack as usize].push(val.clone());
                    self.stack.push(val);
                }
            }
            Instr::PopTemp { count, stack, .. } => {
                for _ in 0..*count {
                    let val = self.pop_temp(*stack)?;
                    self.stack.push(val);
                }
                self.set_min_height();
            }
            Instr::CopyFromTemp {
                count,
                stack,
                offset,
                ..
            } => {
                for val in self.temp_stacks[*stack as usize]
                    .iter()
                    .rev()
                    .skip(*offset)
                    .take(*count)
                {
                    self.stack.push(val.clone());
                }
                self.set_min_height();
            }
            Instr::Label { .. } => self.handle_args_outputs(1, 1)?,
            Instr::PushFunc(f) => self.function_stack.push(Cow::Borrowed(f)),
            &Instr::Switch { count, sig, .. } => {
                for _ in 0..count {
                    self.pop_func()?;
                }
                self.handle_args_outputs(sig.args + 1, sig.outputs)?;
            }
            Instr::Format { parts, .. } => {
                self.handle_args_outputs(parts.len().saturating_sub(1), 1)?
            }
            Instr::Dynamic(f) => self.handle_sig(f.signature)?,
            Instr::Unpack { count, .. } => self.handle_args_outputs(1, *count)?,
            Instr::TouchStack { count, .. } => self.handle_args_outputs(*count, *count)?,
            Instr::DropTemp { count, stack, .. } => {
                for _ in 0..*count {
                    self.pop_temp(*stack)?;
                }
                self.set_min_height();
            }
            Instr::Prim(prim, _) => match prim {
                Reduce => {
                    let sig = self.pop_func()?.signature();
                    let outputs = match (sig.args, sig.outputs) {
                        (0, _) => return Err(format!("{prim}'s function has no args").into()),
                        (1, 0) => 0,
                        (1, _) => {
                            return Err(SigCheckError::from(format!(
                                "{prim}'s function's signature is {sig}"
                            ))
                            .ambiguous())
                        }
                        (_, 1) => 1,
                        _ => return Err(format!("{prim}'s function's signature is {sig}").into()),
                    };
                    self.handle_args_outputs(1, outputs)?;
                }
                Scan => {
                    let _sig = self.pop_func()?.signature();
                    self.handle_args_outputs(1, 1)?;
                }
                Each | Rows | Inventory => {
                    let sig = self.pop_func()?.signature();
                    self.handle_sig(sig)?
                }
                Table | Cross => {
                    let sig = self.pop_func()?.signature();
                    self.handle_sig(sig)?;
                }
                Group | Partition => {
                    let sig = self.pop_func()?.signature();
                    self.handle_args_outputs(2, sig.outputs)?;
                }
                Spawn | Pool => {
                    let sig = self.pop_func()?.signature();
                    self.handle_args_outputs(sig.args, 1)?;
                }
                Repeat => {
                    let f = self.pop_func()?;
                    let sig = f.signature();
                    let n = self.pop()?;
                    if let BasicValue::Num(n) = n {
                        // If n is a known natural number, then the function can have any signature.
                        if n.fract() == 0.0 && n >= 0.0 {
                            let n = n as usize;
                            if n > 0 {
                                let (args, outputs) = match sig.args.cmp(&sig.outputs) {
                                    Ordering::Equal => (sig.args, sig.outputs),
                                    Ordering::Less => {
                                        (sig.args, n * (sig.outputs - sig.args) + sig.args)
                                    }
                                    Ordering::Greater => {
                                        ((n - 1) * (sig.args - sig.outputs) + sig.args, sig.outputs)
                                    }
                                };
                                for _ in 0..args {
                                    self.pop()?;
                                }
                                self.set_min_height();
                                for _ in 0..outputs {
                                    self.stack.push(BasicValue::Other);
                                }
                            }
                        } else if n.is_infinite() {
                            if sig.args != sig.outputs {
                                return Err(SigCheckError::from(format!(
                                    "repeat with infinity and a function with signature {sig}"
                                )));
                            }
                            self.handle_sig(sig)?;
                        } else {
                            return Err("repeat without a natural number or infinity".into());
                        }
                    } else {
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
                                self.handle_sig(sig)?;
                            } else {
                                return Err(SigCheckError::from(format!(
                                    "repeat with no number and a function with signature {sig}"
                                ))
                                .ambiguous());
                            };
                        }
                    }
                }
                Do => {
                    let f = self.pop_func()?;
                    let g = self.pop_func()?;
                    let f_sig = f.signature();
                    let g_sig = g.signature();
                    let copy_count = g_sig.args.saturating_sub(g_sig.outputs.saturating_sub(1));
                    let g_sub_sig =
                        Signature::new(g_sig.args, (g_sig.outputs + copy_count).saturating_sub(1));
                    let comp_sig = f_sig.compose(g_sub_sig);
                    self.handle_args_outputs(
                        comp_sig.args,
                        comp_sig.outputs + g_sub_sig.outputs.saturating_sub(g_sig.args),
                    )?;
                }
                All => {
                    let f_sig = self.pop_func()?.signature();
                    let g_sig = self.pop_func()?.signature();
                    let lower_arg_count = g_sig.outputs / f_sig.args.saturating_sub(1).max(1);
                    let args = lower_arg_count;
                    let outputs = f_sig.outputs * lower_arg_count;
                    self.handle_args_outputs(args, outputs)?;
                }
                Fold => {
                    let f = self.pop_func()?;
                    self.handle_sig(f.signature())?;
                }
                Try => {
                    let f_sig = self.pop_func()?.signature();
                    let _handler_sig = self.pop_func()?.signature();
                    self.handle_sig(f_sig)?;
                }
                Fill => {
                    let fill = self.pop_func()?;
                    self.handle_sig(fill.signature())?;
                    let _fill_value = self.pop()?;
                    let f = self.pop_func()?;
                    self.handle_sig(f.signature())?;
                }
                Content | Memo | Comptime => {
                    let f = self.pop_func()?;
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
                    if let BasicValue::Unknown(i) = self.pop()? {
                        self.popped.push(i);
                    }
                    self.set_min_height();
                }
                By => {
                    let sig = self.pop_func()?.signature();
                    self.handle_args_outputs(sig.args, sig.outputs + 1)?;
                }
                Over => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.set_min_height();
                    self.stack.push(b.clone());
                    self.stack.push(a);
                    self.stack.push(b);
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
                Rectify => {
                    let f = self.pop_func()?;
                    self.handle_sig(f.signature())?;
                }
                SetInverse => {
                    let f = self.pop_func()?;
                    let _inv = self.pop_func()?;
                    self.handle_sig(f.signature())?;
                }
                SetUnder => {
                    let f = self.pop_func()?;
                    let _before = self.pop_func()?;
                    let _after = self.pop_func()?;
                    self.handle_sig(f.signature())?;
                }
                This => {
                    let f = self.pop_func()?;
                    self.handle_sig(f.signature())?;
                }
                Recur => return Err(SigCheckError::from("recur present").ambiguous()),
                Dump => {
                    self.pop_func()?;
                }
                prim => {
                    let args = prim
                        .args()
                        .ok_or_else(|| format!("{prim} has indeterminate args"))?;
                    for _ in 0..prim.modifier_args().unwrap_or(0) {
                        self.pop_func()?;
                    }
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
            Instr::ImplPrim(prim @ ImplPrimitive::ReduceContent, _) => {
                let sig = self.pop_func()?.signature();
                let outputs = match (sig.args, sig.outputs) {
                    (0, _) => return Err(format!("{prim}'s function has no args").into()),
                    (1, 0) => 0,
                    (1, _) => {
                        return Err(SigCheckError::from(format!(
                            "{prim}'s function's signature is {sig}"
                        ))
                        .ambiguous())
                    }
                    (_, 1) => 1,
                    _ => return Err(format!("{prim}'s function's signature is {sig}").into()),
                };
                self.handle_args_outputs(1, outputs)?;
            }
            Instr::ImplPrim(prim, _) => {
                let args = prim.args();
                for _ in 0..prim.modifier_args().unwrap_or(0) {
                    self.pop_func()?;
                }
                for _ in 0..args {
                    self.pop()?;
                }
                self.set_min_height();
                let outputs = prim.outputs();
                for _ in 0..outputs {
                    self.stack.push(BasicValue::Other);
                }
            }
            Instr::PushSig(_) | Instr::PopSig => {
                panic!("PushSig and PopSig should have been handled higher up")
            }
            Instr::SetOutputComment { .. } => {}
            Instr::NoInline => {}
        }
        // println!("{instr:?} -> {}/{}", self.min_height, self.stack.len());
        Ok(())
    }
    // Simulate popping a value. Errors if the stack is empty, which means the function has too many args.
    fn pop(&mut self) -> Result<BasicValue, String> {
        Ok(self.stack.pop().ok_or("function has too many args")?)
    }
    fn pop_temp(&mut self, stack: TempStack) -> Result<BasicValue, String> {
        Ok(self.temp_stacks[stack as usize]
            .pop()
            .ok_or("function has too many args")?)
    }
    fn pop_func(&mut self) -> Result<Cow<'a, Function>, String> {
        self.function_stack
            .pop()
            .ok_or_else(|| "expected function. This is an interpreter bug".into())
    }
    /// Set the current stack height as a potential minimum.
    /// At the end of checking, the minimum stack height is a component in calculating the signature.
    fn set_min_height(&mut self) {
        self.min_height = self.min_height.min(self.stack.len());
        if let Some(h) = self.array_stack.last_mut() {
            *h = (*h).min(self.stack.len());
        }
        for (min_height, stack) in self.temp_min_heights.iter_mut().zip(&self.temp_stacks) {
            *min_height = (*min_height).min(stack.len());
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
        Push(val.into())
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
