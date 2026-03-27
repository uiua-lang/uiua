#![allow(dead_code)]

use std::ops::BitOr;

use crate::{
    Assembly, Boxed, Exec, HasStack, ImplPrimitive, Node, Primitive, Shape, SigNode, Value,
    invert::InversionError,
};

pub struct TypeEnv<'a> {
    asm: &'a Assembly,
    stack: Vec<TypeVal>,
}

impl<'a> HasStack for TypeEnv<'a> {
    type Item = TypeVal;
    type Error = TypeError;
    fn stack(&self) -> &Vec<Self::Item> {
        &self.stack
    }
    fn stack_mut(&mut self) -> &mut Vec<Self::Item> {
        &mut self.stack
    }
    fn underflow_error<A: uiua_stack::StackArg>(&self, arg: A) -> Self::Error {
        TypeError::Underflow(arg.underflow_message())
    }
}

#[derive(Debug)]
pub enum TypeError {
    Unsupported,
    Underflow(String),
    Inversion(InversionError),
    Generic(String),
}

impl From<&str> for TypeError {
    fn from(s: &str) -> Self {
        TypeError::Generic(s.into())
    }
}

impl From<String> for TypeError {
    fn from(s: String) -> Self {
        TypeError::Generic(s.into())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TypeVal {
    Num(f64),
    NumList(Vec<f64>),
    Val(Value),
    Type(Type),
}

impl TypeVal {
    pub fn ty(self) -> Type {
        match self {
            TypeVal::Num(_) => Type {
                scalar: Scalar::Num,
                shape: DynShape::default(),
            },
            TypeVal::NumList(list) => Type {
                scalar: Scalar::Num,
                shape: DynShape::from(list.len()),
            },
            TypeVal::Val(val) => Type::from(&val),
            TypeVal::Type(ty) => ty.clone(),
        }
    }
}

impl From<Value> for TypeVal {
    fn from(value: Value) -> Self {
        TypeVal::Val(value)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub struct Type {
    pub scalar: Scalar,
    pub shape: DynShape,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Scalar {
    #[default]
    Any,
    Num,
    Char,
    Box(Option<Box<Type>>),
    Complex,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DynShape {
    dims: Vec<Dim>,
    suffix: Option<Vec<Dim>>,
}

impl From<&Shape> for DynShape {
    fn from(shape: &Shape) -> Self {
        DynShape {
            dims: shape.iter().map(|&d| Dim::Static(d)).collect(),
            suffix: None,
        }
    }
}

impl From<usize> for DynShape {
    fn from(n: usize) -> Self {
        DynShape {
            dims: vec![Dim::Static(n)],
            suffix: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Dim {
    Static(usize),
    Dyn,
}

impl BitOr for Scalar {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use Scalar::*;
        match (self, rhs) {
            (Box(a), Box(b)) => Box(a.zip(b).map(|(a, b)| (*a | *b).into())),
            (a, b) if a == b => a,
            _ => Any,
        }
    }
}

impl BitOr for DynShape {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use Dim::*;
        let dim_count = self.dims.len().max(rhs.dims.len());
        let mut dims = Vec::with_capacity(dim_count);
        for i in 0..dim_count {
            let a = self.dims.get(i).unwrap_or(&Dyn);
            let b = rhs.dims.get(i).unwrap_or(&Dyn);
            dims.push(match (a, b) {
                (Static(a), Static(b)) if a == b => Static(*a),
                _ => Dyn,
            });
        }
        let suffix = self.suffix.zip(rhs.suffix).map(|(a, b)| {
            let dim_count = a.len().max(b.len());
            let mut dims = Vec::with_capacity(dim_count);
            for i in 0..dim_count {
                let a = (a.len().checked_sub(i))
                    .and_then(|i| a.get(i))
                    .unwrap_or(&Dyn);
                let b = (b.len().checked_sub(i))
                    .and_then(|i| b.get(i))
                    .unwrap_or(&Dyn);
                dims.push(match (a, b) {
                    (Static(a), Static(b)) if a == b => Static(*a),
                    _ => Dyn,
                });
            }
            dims
        });
        DynShape { dims, suffix }
    }
}

impl BitOr for Dim {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use Dim::*;
        match (self, rhs) {
            (Dyn, _) | (_, Dyn) => Dyn,
            (Static(a), Static(b)) if a == b => Static(a),
            (Static(_), Static(_)) => Dyn,
        }
    }
}

impl BitOr for TypeVal {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        if self == rhs {
            return self;
        }
        TypeVal::Type(self.ty() | rhs.ty())
    }
}

impl From<&Value> for Type {
    fn from(val: &Value) -> Self {
        let scalar = match val {
            Value::Num(_) | Value::Byte(_) => Scalar::Num,
            Value::Char(_) => Scalar::Char,
            Value::Complex(_) => Scalar::Complex,
            Value::Box(arr) => Scalar::Box(
                arr.data
                    .iter()
                    .map(|Boxed(v)| Type::from(v))
                    .reduce(BitOr::bitor)
                    .map(Box::new),
            ),
        }
        .into();
        let shape = DynShape::from(&val.shape);
        Type { scalar, shape }
    }
}

impl BitOr for Type {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        let scalar = self.scalar | rhs.scalar;
        let shape = self.shape | rhs.shape;
        Type { scalar, shape }
    }
}

impl<'a> Exec<SigNode> for TypeEnv<'a> {
    type Output = ();
    fn exec(&mut self, f: SigNode) -> Result<Self::Output, Self::Error> {
        self.node(&f.node)
    }
}

impl<'a> Exec<&SigNode> for TypeEnv<'a> {
    type Output = ();
    fn exec(&mut self, f: &SigNode) -> Result<Self::Output, Self::Error> {
        self.node(&f.node)
    }
}

impl<'a> TypeEnv<'a> {
    fn node(&mut self, node: &Node) -> Result<(), TypeError> {
        use {ImplPrimitive::*, Node::*, Primitive::*, Scalar::*};
        match node {
            Run(nodes) => {
                for node in nodes {
                    self.node(node)?;
                }
            }
            Call(f, _) => self.node(&self.asm[f])?,
            Push(val) => self.push(TypeVal::Val(val.clone())),
            Prim(prim, _) => match prim {
                Identity => _ = self.require_height(1)?,
                Pop => {
                    self.pop(1)?;
                }
                Dup => self.dup()?,
                Flip => self.flip()?,
                Add => {
                    let a = self.pop(1)?.ty();
                    let b = self.pop(2)?.ty();
                    todo!()
                }
                _ => return Err(TypeError::Unsupported),
            },
            ImplPrim(prim, _) => match prim {
                Over => self.over()?,
                _ => return Err(TypeError::Unsupported),
            },
            Mod(prim, ops, _) => match prim {
                Fork => self.fork(ops.clone())?,
                Bracket => self.bracket(ops.clone())?,
                Dip => self.dip(monad(ops))?,
                _ => return Err(TypeError::Unsupported),
            },
            ImplMod(prim, ops, _) => match prim {
                &DipN(n) => self.dip_n(n, monad(ops))?,
                _ => return Err(TypeError::Unsupported),
            },
            CustomInverse(cust, _) => match &cust.normal {
                Ok(node) => self.exec(node)?,
                Err(e) => return Err(TypeError::Inversion(e.clone())),
            },
            _ => return Err(TypeError::Unsupported),
        }
        Ok(())
    }
}

fn monad(ops: &[SigNode]) -> &SigNode {
    let [f] = get_ops(ops);
    f
}

fn get_ops<const N: usize>(ops: &[SigNode]) -> &[SigNode; N] {
    ops.try_into().unwrap()
}
