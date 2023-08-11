use std::{cmp::Ordering, fmt, sync::Arc};

use crate::{
    array::Array, lex::CodeSpan, primitive::Primitive, value::Value, Ident, Uiua, UiuaResult,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    Push(Box<Value>),
    BeginArray,
    EndArray(usize),
    Prim(Primitive, usize),
    Call(usize),
    DfnVal(usize),
}

impl Instr {
    pub fn push(val: impl Into<Value>) -> Self {
        Self::Push(Box::new(val.into()))
    }
    pub fn as_push(&self) -> Option<&Value> {
        match self {
            Instr::Push(val) => Some(val),
            _ => None,
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => write!(f, "{val}"),
            Instr::BeginArray => write!(f, "]"),
            Instr::EndArray(_) => write!(f, "["),
            Instr::Prim(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, ":"),
            Instr::DfnVal(n) => write!(f, "{}", (*n as u8 + b'a') as char),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub id: FunctionId,
    pub instrs: Vec<Instr>,
    pub kind: FunctionKind,
}

#[derive(Clone)]
pub enum FunctionKind {
    Normal,
    Dfn(u8),
    Dynamic {
        f: Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync>,
        inputs: u8,
        outputs: u8,
    },
}

impl From<Primitive> for Function {
    fn from(prim: Primitive) -> Self {
        Self {
            id: FunctionId::Primitive(prim),
            instrs: vec![Instr::Prim(prim, 0)],
            kind: FunctionKind::Normal,
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.instrs == other.instrs
    }
}

impl Eq for Function {}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id
            .cmp(&other.id)
            .then_with(|| self.instrs.cmp(&other.instrs))
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let FunctionId::Named(name) = &self.id {
            return write!(f, "{name}");
        }
        if let Some(prim) = self.as_primitive() {
            return write!(f, "{prim}");
        }
        if let FunctionKind::Dynamic { .. } = self.kind {
            return write!(f, "<dynamic>");
        }
        if let FunctionKind::Dfn(_) = self.kind {
            write!(f, "{{")?;
        } else {
            write!(f, "(")?;
        }
        for instr in self.instrs.iter().rev() {
            instr.fmt(f)?;
        }
        if let FunctionKind::Dfn(_) = self.kind {
            write!(f, "}}")?;
        } else {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl Function {
    /// Get how many arguments this function takes and by how much it changes the height of the stack.
    /// Returns `None` if either of these values are dynamic.
    pub fn args_delta(&self) -> Option<(usize, isize)> {
        if let FunctionKind::Dynamic {
            inputs, outputs, ..
        } = &self.kind
        {
            Some((*inputs as usize, *outputs as isize - *inputs as isize))
        } else {
            instrs_stack_delta(&self.instrs)
        }
    }
    pub fn constant(value: impl Into<Value>) -> Self {
        Function {
            id: FunctionId::Constant,
            instrs: vec![Instr::push(value.into())],
            kind: FunctionKind::Normal,
        }
    }
    pub fn as_primitive(&self) -> Option<Primitive> {
        match &self.id {
            FunctionId::Primitive(prim) => Some(*prim),
            _ => None,
        }
    }
    pub(crate) fn as_flipped_primitive(&self) -> Option<(Primitive, bool)> {
        match &self.id {
            FunctionId::Primitive(prim) => Some((*prim, false)),
            _ => match self.instrs.as_slice() {
                [Instr::Prim(Primitive::Flip, _), Instr::Prim(prim, _)] => Some((*prim, true)),
                _ => None,
            },
        }
    }
    pub fn compose(self, other: Self) -> Self {
        match (&self.kind, &other.kind) {
            (FunctionKind::Dynamic { .. }, _) | (_, FunctionKind::Dynamic { .. }) => Function {
                id: self.id.clone().compose(other.id.clone()),
                instrs: vec![
                    Instr::push(other),
                    Instr::Call(0),
                    Instr::push(self),
                    Instr::Call(0),
                ],
                kind: FunctionKind::Normal,
            },
            _ => Function {
                id: self.id.compose(other.id),
                instrs: {
                    let mut instrs = other.instrs;
                    instrs.extend(self.instrs);
                    instrs
                },
                kind: FunctionKind::Normal,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(CodeSpan),
    Primitive(Primitive),
    Constant,
    Main,
    Composed(Vec<Self>),
}

impl FunctionId {
    pub fn compose(self, other: Self) -> Self {
        match (self, other) {
            (FunctionId::Composed(mut a), FunctionId::Composed(b)) => {
                a.extend(b);
                FunctionId::Composed(a)
            }
            (FunctionId::Composed(mut a), b) => {
                a.push(b);
                FunctionId::Composed(a)
            }
            (a, FunctionId::Composed(mut b)) => {
                b.insert(0, a);
                FunctionId::Composed(b)
            }
            (a, b) => FunctionId::Composed(vec![a, b]),
        }
    }
}

impl From<Ident> for FunctionId {
    fn from(name: Ident) -> Self {
        Self::Named(name)
    }
}

impl From<Primitive> for FunctionId {
    fn from(op: Primitive) -> Self {
        Self::Primitive(op)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionId::Named(name) => write!(f, "`{name}`"),
            FunctionId::Anonymous(span) => write!(f, "fn from {span}"),
            FunctionId::Primitive(prim) => write!(f, "{prim}"),
            FunctionId::Constant => write!(f, "constant"),
            FunctionId::Main => write!(f, "main"),
            FunctionId::Composed(ids) => write!(f, "{ids:?}"),
        }
    }
}

/// Count the number of arguments and outputs of list of instructions.
pub(crate) fn instrs_stack_delta(instrs: &[Instr]) -> Option<(usize, isize)> {
    const START_HEIGHT: usize = 16;
    let mut env = VirtualEnv {
        stack: vec![None; START_HEIGHT],
        array_stack: Vec::new(),
        min_height: START_HEIGHT,
    };
    if let Err(_e) = env.instrs(instrs) {
        // println!("instrs: {:?}", instrs);
        // println!("unable to count a/o: {}", _e);
        return None;
    }
    let args = START_HEIGHT.saturating_sub(env.min_height);
    let delta = env.stack.len() as isize - START_HEIGHT as isize;
    // println!("instrs: {:?}", instrs);
    // println!("args/delta: {}/{}", args, delta);
    Some((args, delta))
}

struct VirtualEnv<'a> {
    stack: Vec<Option<&'a Function>>,
    array_stack: Vec<usize>,
    min_height: usize,
}

impl<'a> VirtualEnv<'a> {
    pub fn instrs(&mut self, instrs: &'a [Instr]) -> Result<(), String> {
        use Primitive::*;
        for instr in instrs {
            match instr {
                Instr::Push(val) => {
                    self.stack
                        .push(val.as_func_array().and_then(Array::as_scalar).map(|f| &**f));
                }
                Instr::DfnVal(_) => self.stack.push(None),
                Instr::BeginArray => self.array_stack.push(self.stack.len()),
                Instr::EndArray(_) => {
                    self.stack.truncate(
                        self.array_stack
                            .pop()
                            .ok_or("EndArray without BeginArray")?,
                    );
                    self.stack.push(None);
                }
                Instr::Prim(prim, _) => {
                    let mod_ad = match prim {
                        Reduce | Scan => Some((2, -1, 1)),
                        Fold => Some((2, -1, 2)),
                        Each | Rows => Some((1, 0, 1)),
                        Zip | Bridge | Distribute | Plow | Table | Cross => Some((2, -1, 2)),
                        _ => None,
                    };
                    if let Some((f_args, f_delta, m_args)) = mod_ad {
                        if let Some(f) = self.pop()? {
                            let (a, d) = f.args_delta().ok_or_else(|| {
                                format!("{prim}'s function {f:?} had indeterminate a/o")
                            })?;
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
                            self.stack.push(None);
                        } else {
                            return Err(format!("{prim} without function"));
                        }
                    } else if let Some((..)) = prim.modifier_args() {
                        return Err(format!("{prim}'s signature is not yet supported"));
                    } else {
                        let args = prim.args().ok_or("Prim had indeterminate args")?;
                        for _ in 0..args {
                            self.pop()?;
                        }
                        self.set_min_height();
                        let delta = prim.delta().ok_or("Prim had indeterminate delta")?;
                        for _ in 0..delta + args as i8 {
                            self.stack.push(None);
                        }
                    }
                }
                Instr::Call(_) => return Err("Call in instrs".into()),
            }
            self.set_min_height();
        }
        Ok(())
    }
    fn pop(&mut self) -> Result<Option<&'a Function>, String> {
        Ok(self.stack.pop().ok_or("function is too complex")?)
    }
    fn set_min_height(&mut self) {
        self.min_height = self.min_height.min(self.stack.len());
        if let Some(h) = self.array_stack.last_mut() {
            *h = (*h).min(self.stack.len());
        }
    }
}

#[cfg(test)]
mod test {
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
