#![allow(dead_code)]

use std::{fmt, mem::take, ops::BitOr};

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
        TypeError::Generic(s)
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
        match value {
            Value::Num(arr) if arr.rank() == 0 => TypeVal::Num(arr.data[0]),
            Value::Byte(arr) if arr.rank() == 0 => TypeVal::Num(arr.data[0] as f64),
            Value::Num(arr) if arr.rank() == 1 => TypeVal::NumList(arr.data.into()),
            Value::Byte(arr) if arr.rank() == 1 => {
                TypeVal::NumList(arr.data.into_iter().map(Into::into).collect())
            }
            val => TypeVal::Val(val),
        }
    }
}

impl From<Type> for TypeVal {
    fn from(ty: Type) -> Self {
        TypeVal::Type(ty)
    }
}

impl From<Scalar> for TypeVal {
    fn from(scalar: Scalar) -> Self {
        TypeVal::Type(scalar.into())
    }
}

impl From<usize> for TypeVal {
    fn from(n: usize) -> Self {
        TypeVal::Num(n as f64)
    }
}

impl From<Dim> for TypeVal {
    fn from(dim: Dim) -> Self {
        match dim {
            Dim::Static(n) => n.into(),
            Dim::Dyn => Scalar::Num.into(),
        }
    }
}

impl From<DynShape> for TypeVal {
    fn from(shape: DynShape) -> Self {
        if shape.suffix.is_some() {
            TypeVal::Type(Scalar::Num.shaped(Dim::Dyn))
        } else {
            let mut list = Vec::new();
            for dim in shape.dims {
                match dim {
                    Dim::Static(n) => list.push(n as f64),
                    Dim::Dyn => return TypeVal::Type(Scalar::Num.shaped(Dim::Dyn)),
                }
            }
            TypeVal::NumList(list)
        }
    }
}

impl From<Scalar> for Type {
    fn from(scalar: Scalar) -> Self {
        Type {
            scalar,
            shape: DynShape::default(),
        }
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
impl Scalar {
    #[allow(non_upper_case_globals)]
    pub const Byte: Self = Self::Num;
    pub fn scalar(self) -> Type {
        self.shaped([0usize; 0])
    }
    pub fn shaped(self, shape: impl Into<DynShape>) -> Type {
        Type {
            scalar: self,
            shape: shape.into(),
        }
    }
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

impl From<Dim> for DynShape {
    fn from(dim: Dim) -> Self {
        DynShape {
            dims: vec![dim],
            suffix: None,
        }
    }
}

impl<const N: usize> From<[Dim; N]> for DynShape {
    fn from(dims: [Dim; N]) -> Self {
        DynShape {
            dims: dims.into(),
            suffix: None,
        }
    }
}

impl<const N: usize> From<[usize; N]> for DynShape {
    fn from(dims: [usize; N]) -> Self {
        dims.map(Dim::Static).into()
    }
}

impl From<usize> for DynShape {
    fn from(n: usize) -> Self {
        Dim::Static(n).into()
    }
}

impl DynShape {
    pub fn row_count(&self) -> Dim {
        self.dims.first().copied().unwrap_or(Dim::Static(1))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Dim {
    Static(usize),
    Dyn,
}

impl fmt::Display for Dim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Dim::Static(n) => write!(f, "{n}"),
            Dim::Dyn => write!(f, "*"),
        }
    }
}

impl fmt::Display for DynShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, dim) in self.dims.iter().enumerate() {
            if i > 0 {
                write!(f, "×")?;
            }
            write!(f, "{dim}")?;
        }
        if let Some(suffix) = &self.suffix {
            write!(f, "×…")?;
            for dim in suffix {
                write!(f, "×")?;
                write!(f, "{dim}")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.shape, self.scalar)
    }
}

impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scalar::Any => write!(f, "*"),
            Scalar::Num => write!(f, "ℝ"),
            Scalar::Char => write!(f, "@"),
            Scalar::Box(None) => write!(f, "□"),
            Scalar::Box(Some(inner)) => write!(f, "□{inner}"),
            Scalar::Complex => write!(f, "ℂ"),
        }
    }
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
        };
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
        use {crate::algorithm::pervade::*, ImplPrimitive::*, Node::*, Primitive::*};
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
                Pop => _ = self.pop(1)?,
                Dup => self.dup()?,
                Flip => self.flip()?,
                Neg => self.monadic_pervasive(Scalar::scalar_neg, scalar_neg::num)?,
                Not => self.monadic_pervasive(Scalar::not, not::num)?,
                Abs => self.monadic_pervasive(Scalar::scalar_abs, scalar_abs::num)?,
                Sign => self.monadic_pervasive(Scalar::sign, sign::num)?,
                Reciprocal => self.monadic_pervasive(Scalar::recip, recip::num)?,
                Sqrt => self.monadic_pervasive(Scalar::sqrt, sqrt::num)?,
                Exp => self.monadic_pervasive(Scalar::exp, exp::num)?,
                Sin => self.monadic_pervasive(Scalar::sin, sin::num)?,
                Floor => self.monadic_pervasive(Scalar::floor, floor::num)?,
                Ceil => self.monadic_pervasive(Scalar::ceil, ceil::num)?,
                Round => self.monadic_pervasive(Scalar::round, round::num)?,
                Len => self.monadic(
                    |ty| Ok(ty.shape.row_count()),
                    |_| Ok(1),
                    |list| Ok(list.len()),
                )?,
                Shape => self.monadic(
                    |ty| Ok(ty.shape),
                    |_| Ok(Scalar::Num.scalar()),
                    |list| Ok(Scalar::Num.shaped([list.len()])),
                )?,
                _ => return Err(TypeError::Unsupported),
            },
            ImplPrim(prim, _) => match prim {
                Over => self.over()?,
                Ln => self.monadic_pervasive(Scalar::ln, ln::num)?,
                Cos => self.monadic_pervasive(Scalar::cos, cos::num)?,
                Asin => self.monadic_pervasive(Scalar::asin, asin::num)?,
                Acos => self.monadic_pervasive(Scalar::acos, acos::num)?,
                Exp2 => self.monadic_pervasive(Scalar::exp2, exp2::num)?,
                Exp10 => self.monadic_pervasive(Scalar::exp10, exp10::num)?,
                Log2 => self.monadic_pervasive(Scalar::log2, log2::num)?,
                Log10 => self.monadic_pervasive(Scalar::log10, log10::num)?,
                SquareAbs => self.monadic_pervasive(Scalar::square_abs, square_abs::num)?,
                NegAbs => self.monadic_pervasive(Scalar::neg_abs, neg_abs::num)?,
                UnComplex => {
                    let x = self.copy_top()?;
                    self.monadic_pervasive(Scalar::complex_re, complex_re::generic)?;
                    self.push(x);
                    self.monadic_pervasive(Scalar::complex_im, complex_im::num)?;
                }
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
    fn monadic_pervasive(
        &mut self,
        f: impl Fn(Scalar) -> Result<Scalar, String>,
        f64: impl Fn(f64) -> f64,
    ) -> Result<(), TypeError> {
        self.monadic(
            |mut ty| {
                Ok(Type {
                    scalar: f(take(&mut ty.scalar))?,
                    ..ty
                })
            },
            |n| Ok(TypeVal::Num(f64(n))),
            |mut ns| {
                for n in &mut ns {
                    *n = f64(*n);
                }
                Ok(TypeVal::NumList(ns))
            },
        )
    }
    fn monadic<T: Into<TypeVal>, N: Into<TypeVal>, L: Into<TypeVal>>(
        &mut self,
        f: impl Fn(Type) -> Result<T, String>,
        num: impl Fn(f64) -> Result<N, String>,
        list: impl Fn(Vec<f64>) -> Result<L, String>,
    ) -> Result<(), TypeError> {
        let x = self.pop(1)?;
        self.push(match x {
            TypeVal::Num(n) => num(n)?.into(),
            TypeVal::NumList(ns) => list(ns)?.into(),
            tv => f(tv.ty())?.into(),
        });
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
