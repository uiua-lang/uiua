//! Signature checker implementation

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
};

use enum_iterator::Sequence;

use crate::{
    array::Array,
    function::{Instr, Signature},
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

/// The the signature of some instructions, but only
/// if the temp stack signatures are `|0.0`
pub(crate) fn instrs_clean_signature(instrs: &[Instr]) -> Option<Signature> {
    let sig = instrs_all_signatures(instrs).ok()?;
    if sig.functions_left != 0 || sig.temps.iter().any(|&sig| sig != (0, 0)) || sig.array_stack != 0
    {
        return None;
    }
    Some(sig.stack)
}

pub(crate) fn instrs_temp_signatures(
    instrs: &[Instr],
) -> Result<[Signature; TempStack::CARDINALITY], SigCheckError> {
    let env = VirtualEnv::from_instrs(instrs)?;
    Ok(env.temp_signatures())
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct AllSignatures {
    pub stack: Signature,
    pub temps: [Signature; TempStack::CARDINALITY],
    pub functions_left: usize,
    pub array_stack: usize,
}

pub(crate) fn instrs_all_signatures(instrs: &[Instr]) -> Result<AllSignatures, SigCheckError> {
    type AllSigsCache = HashMap<u64, AllSignatures>;
    thread_local! {
        static CACHE: RefCell<AllSigsCache> = RefCell::new(AllSigsCache::new());
    }
    let mut hasher = DefaultHasher::new();
    instrs.hash(&mut hasher);
    let hash = hasher.finish();
    CACHE.with(|cache| {
        if let Some(sigs) = cache.borrow().get(&hash) {
            return Ok(*sigs);
        }
        let env = VirtualEnv::from_instrs(instrs)?;
        let sigs = AllSignatures {
            stack: env.sig(),
            temps: env.temp_signatures(),
            functions_left: env.function_stack.len(),
            array_stack: env.array_stack.len(),
        };
        cache.borrow_mut().insert(hash, sigs);
        Ok(sigs)
    })
}

pub(crate) fn naive_under_sig(f: Signature, g: Signature) -> Signature {
    let f_inv = if f.outputs > 1 {
        f.inverse()
    } else {
        Signature::new(f.args.min(1), f.outputs)
    };
    let mut curr = 0i32;
    let mut min = 0i32;
    curr -= f.args as i32;
    min = min.min(curr);
    curr += f.outputs as i32;
    curr -= g.args as i32;
    min = min.min(curr);
    curr += g.outputs as i32;
    curr -= f_inv.args as i32;
    min = min.min(curr);
    curr += f_inv.outputs as i32;
    Signature::new(min.unsigned_abs() as usize, (curr - min) as usize)
}

/// An environment that emulates the runtime but only keeps track of the stack.
struct VirtualEnv {
    stack: Vec<BasicValue>,
    temp_stacks: [Vec<BasicValue>; TempStack::CARDINALITY],
    function_stack: Vec<Signature>,
    array_stack: Vec<usize>,
    min_height: usize,
    temp_min_heights: [usize; TempStack::CARDINALITY],
    popped: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SigCheckError {
    pub message: String,
    pub kind: SigCheckErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SigCheckErrorKind {
    Incorrect,
    Ambiguous,
    LoopOverreach,
    LoopVariable { sig: Signature, inf: bool },
}

impl SigCheckError {
    pub fn ambiguous(self) -> Self {
        Self {
            kind: SigCheckErrorKind::Ambiguous,
            ..self
        }
    }
    pub fn loop_overreach(self) -> Self {
        Self {
            kind: SigCheckErrorKind::LoopOverreach,
            ..self
        }
    }
    pub fn loop_variable(self, sig: Signature, inf: bool) -> Self {
        Self {
            kind: SigCheckErrorKind::LoopVariable { sig, inf },
            ..self
        }
    }
}

impl<'a> From<&'a str> for SigCheckError {
    fn from(s: &'a str) -> Self {
        Self {
            message: s.to_string(),
            kind: SigCheckErrorKind::Incorrect,
        }
    }
}

impl From<String> for SigCheckError {
    fn from(s: String) -> Self {
        Self {
            message: s,
            kind: SigCheckErrorKind::Incorrect,
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

impl VirtualEnv {
    fn from_instrs(instrs: &[Instr]) -> Result<Self, SigCheckError> {
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
    fn instrs(&mut self, instrs: &[Instr]) -> Result<(), SigCheckError> {
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
    fn instr(&mut self, instr: &Instr) -> Result<(), SigCheckError> {
        use Primitive::*;
        match instr {
            Instr::Comment(_) => {}
            Instr::Push(val) => self.stack.push(BasicValue::from_val(val)),
            Instr::CallGlobal { call, sig, .. } => {
                if *call {
                    self.handle_sig(*sig)?;
                } else {
                    self.function_stack.push(*sig);
                }
            }
            Instr::BindGlobal { .. } => {
                self.pop()?;
            }
            Instr::BeginArray => self.array_stack.push(self.stack.len()),
            Instr::EndArray { .. } => {
                let bottom = (self.array_stack.pop()).ok_or("EndArray without BeginArray")?;
                let mut items: Vec<_> = self.stack.drain(bottom.min(self.stack.len())..).collect();
                self.set_min_height();
                items.reverse();
                self.stack.push(BasicValue::Arr(items));
            }
            Instr::ImplPrim(ImplPrimitive::EndRandArray, _) => {
                let _len = self.pop()?;
                let bottom = (self.array_stack.pop()).ok_or("EndRandArray without BeginArray")?;
                self.stack.drain(bottom.min(self.stack.len())..);
                self.set_min_height();
                self.stack.push(BasicValue::Other);
            }
            Instr::Call(_) | Instr::CallRecursive(_) => {
                let sig = self.pop_func()?;
                self.handle_sig(sig)?
            }
            Instr::Recur(_) => return Err(SigCheckError::from("recur present").ambiguous()),
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
            Instr::Label { .. } => self.handle_args_outputs(1, 1)?,
            Instr::ValidateType { .. } => self.handle_args_outputs(1, 1)?,
            Instr::PushFunc(f) => self.function_stack.push(f.signature()),
            &Instr::Switch {
                count,
                sig,
                under_cond,
                ..
            } => {
                for _ in 0..count {
                    self.pop_func()?;
                }
                let cond = self.pop()?;
                if under_cond {
                    self.temp_stacks[TempStack::Under as usize].push(cond);
                }
                self.handle_args_outputs(sig.args, sig.outputs)?;
            }
            Instr::Format { parts, .. } => {
                self.handle_args_outputs(parts.len().saturating_sub(1), 1)?
            }
            Instr::MatchFormatPattern { parts, .. } => {
                self.handle_args_outputs(1, parts.len().saturating_sub(1))?
            }
            Instr::Dynamic(f) => self.handle_sig(f.signature)?,
            Instr::Unpack { count, .. } => self.handle_args_outputs(1, *count)?,
            Instr::TouchStack { count, .. } => self.handle_args_outputs(*count, *count)?,
            Instr::Prim(Astar, _) | Instr::ImplPrim(ImplPrimitive::AstarFirst, _) => {
                let _start = self.pop()?;
                let neighbors = self.pop_func()?;
                let heuristic = self.pop_func()?;
                let is_goal = self.pop_func()?;
                let args = neighbors
                    .args
                    .max(heuristic.args)
                    .max(is_goal.args)
                    .saturating_sub(1);
                self.handle_args_outputs(args, 2)?;
            }
            Instr::Prim(prim, _) => match prim {
                Reduce | Scan => {
                    let sig = self.pop_func()?;
                    let args = sig.args.saturating_sub(sig.outputs);
                    self.handle_args_outputs(args, sig.outputs)?;
                }
                Each | Rows | Inventory => {
                    let sig = self.pop_func()?;
                    self.handle_sig(sig)?
                }
                Table | Tuples | Triangle => {
                    let sig = self.pop_func()?;
                    self.handle_sig(sig)?;
                }
                Group | Partition => {
                    let sig = self.pop_func()?;
                    self.handle_args_outputs(2, sig.outputs)?;
                }
                Spawn | Pool => {
                    let sig = self.pop_func()?;
                    self.handle_args_outputs(sig.args, 1)?;
                }
                Repeat => {
                    let f = self.pop_func()?;
                    let n = self.pop()?;
                    self.repeat(f, n)?;
                }
                Do => {
                    let body = self.pop_func()?;
                    let cond = self.pop_func()?;
                    let copy_count = cond.args.saturating_sub(cond.outputs.saturating_sub(1));
                    let cond_sub_sig =
                        Signature::new(cond.args, (cond.outputs + copy_count).saturating_sub(1));
                    let comp_sig = body.compose(cond_sub_sig);
                    if comp_sig.args < comp_sig.outputs && self.array_stack.is_empty() {
                        return Err(SigCheckError::from(format!(
                            "do with a function with signature {comp_sig}"
                        ))
                        .loop_variable(comp_sig, false));
                    }
                    self.handle_args_outputs(
                        comp_sig.args,
                        comp_sig.outputs + cond_sub_sig.outputs.saturating_sub(cond.args),
                    )?;
                }
                Un => {
                    let sig = self.pop_func()?;
                    self.handle_args_outputs(sig.outputs, sig.args)?;
                }
                Anti => {
                    let sig = self.pop_func()?;
                    self.handle_args_outputs(sig.args, sig.outputs)?;
                }
                Under => {
                    let f = self.pop_func()?;
                    let g = self.pop_func()?;
                    self.handle_sig(naive_under_sig(f, g))?;
                }
                Fold => {
                    let f = self.pop_func()?;
                    self.handle_sig(f)?;
                }
                Try => {
                    let f_sig = self.pop_func()?;
                    let _handler_sig = self.pop_func()?;
                    self.handle_sig(f_sig)?;
                }
                Case => {
                    let f_sig = self.pop_func()?;
                    self.handle_sig(f_sig)?;
                }
                Fill => {
                    let fill_sig = self.pop_func()?;
                    if fill_sig.outputs > 0 {
                        self.handle_sig(fill_sig)?;
                    }
                    self.handle_args_outputs(fill_sig.outputs, 0)?;
                    let f = self.pop_func()?;
                    self.handle_sig(f)?;
                }
                Content | Memo | Comptime => {
                    let f = self.pop_func()?;
                    self.handle_sig(f)?;
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
                Over => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.set_min_height();
                    self.stack.push(b.clone());
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Around => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.set_min_height();
                    self.stack.push(a.clone());
                    self.stack.push(b);
                    self.stack.push(a);
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
                SetInverse => {
                    let f = self.pop_func()?;
                    let _inv = self.pop_func()?;
                    self.handle_sig(f)?;
                }
                SetUnder => {
                    let f = self.pop_func()?;
                    let _before = self.pop_func()?;
                    let _after = self.pop_func()?;
                    self.handle_sig(f)?;
                }
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
                    let outputs = prim
                        .outputs()
                        .ok_or_else(|| format!("{prim} has indeterminate outputs"))?;
                    self.handle_args_outputs(args, outputs)?;
                }
            },
            Instr::ImplPrim(prim, _) => match prim {
                ImplPrimitive::ReduceContent | ImplPrimitive::ReduceDepth(_) => {
                    let sig = self.pop_func()?;
                    let args = sig.args.saturating_sub(sig.outputs);
                    self.handle_args_outputs(args, sig.outputs)?;
                }
                ImplPrimitive::RepeatWithInverse => {
                    let f = self.pop_func()?;
                    let inv = self.pop_func()?;
                    if f.inverse() != inv {
                        return Err(SigCheckError::from(
                            "repeat inverse does not have inverse signature",
                        )
                        .ambiguous());
                    }
                    let n = self.pop()?;
                    self.repeat(f, n)?;
                }
                ImplPrimitive::UnFill => {
                    let fill_sig = self.pop_func()?;
                    if fill_sig.outputs > 0 {
                        self.handle_sig(fill_sig)?;
                    }
                    self.handle_args_outputs(fill_sig.outputs, 0)?;
                    let f = self.pop_func()?;
                    self.handle_sig(f)?;
                }
                prim => {
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
            },
            Instr::PushSig(_) | Instr::PopSig => {
                panic!("PushSig and PopSig should have been handled higher up")
            }
            Instr::SetOutputComment { .. } => {}
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
    fn pop_func(&mut self) -> Result<Signature, String> {
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
    fn repeat(&mut self, sig: Signature, n: BasicValue) -> Result<(), SigCheckError> {
        if let BasicValue::Num(n) = n {
            // If n is a known natural number, then the function can have any signature.
            let sig = if n >= 0.0 { sig } else { sig.inverse() };
            if n.fract() == 0.0 {
                let n = n.abs() as usize;
                if n > 0 {
                    let (args, outputs) = match sig.args.cmp(&sig.outputs) {
                        Ordering::Equal => (sig.args, sig.outputs),
                        Ordering::Less => (sig.args, n * (sig.outputs - sig.args) + sig.args),
                        Ordering::Greater => {
                            ((n - 1) * (sig.args - sig.outputs) + sig.args, sig.outputs)
                        }
                    };
                    self.handle_args_outputs(args, outputs)?;
                }
            } else if n.is_infinite() {
                match sig.args.cmp(&sig.outputs) {
                    Ordering::Greater => {
                        return Err(SigCheckError::from(format!(
                            "repeat with infinity and a function with signature {sig}"
                        ))
                        .loop_overreach());
                    }
                    Ordering::Less if self.array_stack.is_empty() => {
                        return Err(SigCheckError::from(format!(
                            "repeat with infinity and a function with signature {sig}"
                        ))
                        .loop_variable(sig, true));
                    }
                    _ => self.handle_sig(sig)?,
                }
            } else {
                return Err("repeat without an integer or infinity".into());
            }
        } else {
            // If n is unknown, then what we do depends on the signature
            match sig.args.cmp(&sig.outputs) {
                Ordering::Equal => self.handle_sig(sig)?,
                Ordering::Greater => {
                    return Err(SigCheckError::from(format!(
                        "repeat with no number and a function with signature {sig}"
                    ))
                    .loop_overreach());
                }
                Ordering::Less if self.array_stack.is_empty() => {
                    return Err(SigCheckError::from(format!(
                        "repeat with no number and a function with signature {sig}"
                    ))
                    .loop_variable(sig, false));
                }
                Ordering::Less => self.handle_sig(sig)?,
            }
        }
        Ok(())
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
