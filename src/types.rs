#![allow(dead_code)]

use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    iter::repeat_n,
    mem::{discriminant, swap, take},
    ops::BitOr,
};

use ecow::{EcoVec, eco_vec};
use serde::*;

use crate::{
    ArrayCmp, ArrayValue, Assembly, Boxed, Complex, Exec, HasStack, ImplPrimitive, Node, Primitive,
    Shape, SigNode, SubSide, Value, invert::InversionError,
};

pub fn typecheck(
    sn: &SigNode,
    asm: &Assembly,
) -> Result<(Vec<TypeVal>, Vec<TypeVal>), (TypeError, usize)> {
    let mut env = TypeEnv {
        asm,
        stack: vec![TypeVal::default(); sn.sig.args()],
        under_stack: Vec::new(),
        call_stack: Vec::new(),
        arg_types: vec![TypeVal::default(); sn.sig.args()],
    };
    env.sig_node(sn)
        .map(|()| {
            env.stack.reverse();
            env.arg_types.reverse();
            (take(&mut env.arg_types), take(&mut env.stack))
        })
        .map_err(|e| (e, env.call_stack.pop().unwrap_or(0)))
}

pub struct TypeEnv<'a> {
    asm: &'a Assembly,
    stack: Vec<TypeVal>,
    under_stack: Vec<TypeVal>,
    call_stack: Vec<usize>,
    arg_types: Vec<TypeVal>,
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
    Unsupported(Option<String>),
    Underflow(String),
    Inversion(InversionError),
    DyadicPervasiveShapes(DynShape, DynShape),
    RowsShapes(DynShape, DynShape),
    TypeMismatch(Scalar, Scalar),
    ShapeMismatch(Option<SubSide>, Shape, DynShape),
    Generic(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::Unsupported(None) => {
                write!(f, "type checking is unsupported on this operation")
            }
            TypeError::Unsupported(Some(thing)) => {
                write!(f, "type checking is unsupported for {thing}")
            }
            TypeError::Underflow(message) => write!(f, "{message}"),
            TypeError::Inversion(e) => write!(f, "{e}"),
            TypeError::DyadicPervasiveShapes(a, b) => {
                write!(f, "shapes {a} and {b} are not compatible")
            }
            TypeError::RowsShapes(a, b) => write!(f, "shapes {a} and {b} are not compatible"),
            TypeError::TypeMismatch(expected, found) => {
                write!(f, "expected {expected} but found {found}")
            }
            TypeError::ShapeMismatch(None, expected, found) => {
                write!(f, "expected shape {expected} but found {found}")
            }
            TypeError::ShapeMismatch(Some(SubSide::Left), expected, found) => write!(
                f,
                "expected shape to start with {expected} but found {found}"
            ),
            TypeError::ShapeMismatch(Some(SubSide::Right), expected, found) => {
                write!(f, "expected shape to end with {expected} but found {found}")
            }
            TypeError::Generic(e) => write!(f, "{e}"),
        }
    }
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

#[derive(Debug, Clone, PartialOrd, Serialize, Deserialize)]
pub enum TypeVal {
    Num(f64),
    NumList(EcoVec<f64>),
    Val(Value),
    Type(Type),
}

impl TypeVal {
    pub fn ty(self) -> Type {
        match self {
            TypeVal::Num(_) => Scalar::Num.scalar(),
            TypeVal::NumList(list) => Scalar::Num.shaped(list.len()),
            TypeVal::Val(val) => Type::from(&val),
            TypeVal::Type(ty) => ty.clone(),
        }
    }
    pub fn row_count(&self) -> Dim {
        match self {
            TypeVal::Num(_) => Dim::Static(1),
            TypeVal::NumList(list) => Dim::Static(list.len()),
            TypeVal::Val(value) => Dim::Static(value.row_count()),
            TypeVal::Type(ty) => ty.shape.row_count(),
        }
    }
    pub fn shape(&self) -> Cow<DynShape> {
        match self {
            TypeVal::Num(_) => Cow::Owned(DynShape::scalar()),
            TypeVal::NumList(items) => Cow::Owned([items.len()].into()),
            TypeVal::Val(value) => Cow::Owned((&value.shape).into()),
            TypeVal::Type(ty) => Cow::Borrowed(&ty.shape),
        }
    }
    pub fn into_row(self) -> Self {
        match self {
            TypeVal::Num(_) => self,
            TypeVal::NumList(list) if list.len() == 1 => TypeVal::Num(list[0]),
            TypeVal::Val(mut val) if val.row_count() == 1 => {
                val.shape.undo_fix();
                TypeVal::Val(val)
            }
            TypeVal::NumList(_) | TypeVal::Val(_) => TypeVal::Type(self.ty().into_row()),
            TypeVal::Type(ty) => TypeVal::Type(ty.into_row()),
        }
    }
    pub fn prepend_dim(&mut self, dim: Dim) {
        match (&mut *self, dim) {
            (TypeVal::Num(n), Dim::Static(1)) => *self = TypeVal::NumList(eco_vec![*n]),
            (TypeVal::NumList(list), Dim::Static(1)) => {
                let mut val: Value = take(list).into();
                val.fix();
                *self = TypeVal::Val(val)
            }
            (TypeVal::Val(val), Dim::Static(1)) => val.fix(),
            (TypeVal::Type(ty), _) => ty.shape.dims.insert(0, dim),
            (_, Dim::Dyn | Dim::Static(_)) => {
                *self = take(self).ty().into();
                self.prepend_dim(dim);
            }
        }
    }
    pub fn boxed(self) -> Self {
        match self {
            TypeVal::Val(val) => TypeVal::Val(val.boxed_if(true)),
            tv => Scalar::Box(Some(tv.ty().into())).into(),
        }
    }
    pub fn unboxed(self) -> Self {
        match self {
            TypeVal::Val(val) => TypeVal::Val(val.unboxed()),
            TypeVal::Type(Type {
                scalar: Scalar::Box(inner),
                shape,
            }) if shape.is_scalar() => inner.map_or_else(TypeVal::default, |ty| (*ty).into()),
            tv => tv,
        }
    }
    pub fn type_id(&self) -> Option<u8> {
        Some(match self {
            TypeVal::Num(_) | TypeVal::NumList(_) => f64::TYPE_ID,
            TypeVal::Val(val) => val.type_id(),
            TypeVal::Type(ty) => match ty.scalar {
                Scalar::Any => return None,
                Scalar::Num => f64::TYPE_ID,
                Scalar::Char => char::TYPE_ID,
                Scalar::Box(_) => Boxed::TYPE_ID,
                Scalar::Complex => Complex::TYPE_ID,
            },
        })
    }
    pub fn as_nat(&self) -> Option<usize> {
        let n = match self {
            TypeVal::Num(n) => *n,
            TypeVal::Val(Value::Num(arr)) if arr.rank() == 0 => arr.data[0],
            TypeVal::Val(Value::Byte(arr)) if arr.rank() == 0 => arr.data[0] as f64,
            _ => return None,
        };
        if n.fract() == 0.0 && n >= 0.0 {
            Some(n as usize)
        } else {
            None
        }
    }
    pub fn as_nat_list(&self) -> Option<EcoVec<usize>> {
        Some(match self {
            TypeVal::NumList(nums) if nums.iter().all(|&n| n >= 0.0 && n.fract() == 0.0) => {
                nums.iter().map(|&n| n as usize).collect()
            }
            TypeVal::Val(Value::Num(arr))
                if arr.rank() == 1 && arr.data.iter().all(|&n| n >= 0.0 && n.fract() == 0.0) =>
            {
                arr.data.iter().map(|&n| n as usize).collect()
            }
            TypeVal::Val(Value::Byte(arr)) if arr.rank() == 1 => {
                arr.data.iter().map(|&f| f as usize).collect()
            }
            _ => return None,
        })
    }
    pub fn is_any(&self) -> bool {
        matches!(self, TypeVal::Type(ty) if ty.is_any())
    }
}

impl Default for TypeVal {
    fn default() -> Self {
        TypeVal::Type(Type::default())
    }
}

impl PartialEq for TypeVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => a.array_eq(b),
            (Self::NumList(a), Self::NumList(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.array_eq(b))
            }
            (Self::Val(a), Self::Val(b)) => a == b,
            (Self::Type(a), Self::Type(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for TypeVal {}

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
        (n as f64).into()
    }
}

impl From<u8> for TypeVal {
    fn from(n: u8) -> Self {
        (n as f64).into()
    }
}

impl From<f64> for TypeVal {
    fn from(n: f64) -> Self {
        TypeVal::Num(n)
    }
}

impl From<Complex> for TypeVal {
    fn from(_: Complex) -> Self {
        Scalar::Complex.into()
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
            let mut list = EcoVec::new();
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
            shape: DynShape::scalar(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub struct Type {
    pub scalar: Scalar,
    pub shape: DynShape,
}

impl Type {
    pub fn is_any(&self) -> bool {
        self.scalar.is_any() && self.shape.is_any()
    }
    pub fn into_row(self) -> Self {
        Type {
            scalar: self.scalar,
            shape: self.shape.into_row(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Serialize, Deserialize)]
pub enum Scalar {
    #[default]
    Any,
    Num,
    Char,
    Box(Option<Box<Type>>),
    Complex,
}
impl Scalar {
    pub fn is_any(&self) -> bool {
        matches!(self, Scalar::Any)
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DynShape {
    dims: Vec<Dim>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
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
    pub fn scalar() -> Self {
        DynShape {
            dims: Vec::new(),
            suffix: None,
        }
    }
    pub fn any() -> Self {
        DynShape {
            dims: Vec::new(),
            suffix: Some(Vec::new()),
        }
    }
    pub fn row_count(&self) -> Dim {
        self.dims.first().copied().unwrap_or(Dim::Static(1))
    }
    pub fn is_any(&self) -> bool {
        self.dims.is_empty() && self.suffix.as_ref().is_some_and(|s| s.is_empty())
    }
    pub fn is_scalar(&self) -> bool {
        self.dims.is_empty() && self.suffix.is_none()
    }
    pub fn into_row(self) -> DynShape {
        let mut row = self;
        if row.dims.is_empty() {
            if let Some(suff) = &mut row.suffix
                && !suff.is_empty()
            {
                suff.remove(0);
            }
        } else {
            row.dims.remove(0);
        }
        row
    }
}

impl Default for DynShape {
    fn default() -> Self {
        Self::any()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dim {
    Static(usize),
    Dyn,
}

impl From<usize> for Dim {
    fn from(n: usize) -> Self {
        Dim::Static(n)
    }
}

impl Dim {
    pub const MIN: Self = Dim::Static(1);
    pub fn row_compatible(self, other: Self) -> bool {
        match (self, other) {
            (Dim::Dyn | Dim::Static(1), _) | (_, Dim::Dyn | Dim::Static(1)) => true,
            (Dim::Static(a), Dim::Static(b)) => a == b,
        }
    }
}

impl Ord for Dim {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Dim::Static(1), Dim::Dyn) => Ordering::Less,
            (Dim::Static(_), Dim::Dyn) => Ordering::Greater,
            (Dim::Dyn, Dim::Static(1)) => Ordering::Greater,
            (Dim::Dyn, Dim::Static(_)) => Ordering::Less,
            (Dim::Dyn, Dim::Dyn) => Ordering::Equal,
            (Dim::Static(a), Dim::Static(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for Dim {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq<usize> for Dim {
    fn eq(&self, other: &usize) -> bool {
        *self == Dim::Static(*other)
    }
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
        if self.is_any() {
            return write!(f, "…");
        }
        for (i, dim) in self.dims.iter().enumerate() {
            if i > 0 {
                write!(f, "×")?;
            }
            write!(f, "{dim}")?;
        }
        if let Some(suffix) = &self.suffix {
            if !self.dims.is_empty() {
                write!(f, "×…")?;
            }
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
        if self.is_any() {
            write!(f, "…*")
        } else if self.shape.is_scalar() {
            write!(f, "{}", self.scalar)
        } else {
            write!(f, "[{} {}]", self.shape, self.scalar)
        }
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
        let scalar = Scalar::from(val);
        let shape = DynShape::from(&val.shape);
        Type { scalar, shape }
    }
}

impl From<&Value> for Scalar {
    fn from(val: &Value) -> Self {
        match val {
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
    fn exec(&mut self, sn: SigNode) -> Result<Self::Output, Self::Error> {
        self.sig_node(&sn)
    }
}

impl<'a> Exec<&SigNode> for TypeEnv<'a> {
    type Output = ();
    fn exec(&mut self, sn: &SigNode) -> Result<Self::Output, Self::Error> {
        self.sig_node(sn)
    }
}

impl<'a> TypeEnv<'a> {
    fn update_arg_types(&mut self) {
        if self.stack.len() != self.arg_types.len() {
            return;
        }
        for (tv, arg) in self.stack.iter().zip(&mut self.arg_types) {
            if let TypeVal::Type(arg_ty) = arg {
                if let TypeVal::Type(ty) = tv {
                    if arg_ty.scalar.is_any() {
                        arg_ty.scalar = ty.scalar.clone();
                    }
                    if arg_ty.shape.is_any() {
                        arg_ty.shape = ty.shape.clone();
                    }
                } else if arg_ty.is_any() {
                    *arg = tv.clone();
                }
            }
        }
    }
    fn sig_node(&mut self, sn: &SigNode) -> Result<(), TypeError> {
        let stack_height = self.stack_len();
        match self.node(&sn.node) {
            Ok(()) => Ok(()),
            Err(TypeError::Unsupported(e)) => {
                // Replace types with any
                let Some(min_height) = stack_height.checked_sub(sn.sig.args()) else {
                    return Err(TypeError::Unsupported(e));
                };
                while self.stack_len() > min_height {
                    self.stack.pop();
                }
                while self.stack_len() < min_height + sn.sig.outputs() {
                    self.push(TypeVal::default())
                }
                Ok(())
            }
            Err(e) => Err(e),
        }
    }
    fn node(&mut self, node: &Node) -> Result<(), TypeError> {
        let span = node.span();
        self.call_stack.extend(span);
        let res = self.node_impl(node);
        if let Ok(()) | Err(TypeError::Unsupported(_)) = res
            && span.is_some()
        {
            self.call_stack.pop();
        }
        res
    }
    fn node_impl(&mut self, node: &Node) -> Result<(), TypeError> {
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
                Add => self.dyadic_pervasive(Scalar::add, add::num_num)?,
                Sub => self.dyadic_pervasive(Scalar::sub, sub::num_num)?,
                Mul => self.dyadic_pervasive(Scalar::mul, mul::num_num)?,
                Div => self.dyadic_pervasive(Scalar::div, div::num_num)?,
                Modulo => self.dyadic_pervasive(Scalar::modulo, modulo::num_num)?,
                Or => self.dyadic_pervasive(Scalar::or, or::num_num)?,
                Pow => self.dyadic_pervasive(Scalar::scalar_pow, scalar_pow::num_num)?,
                Atan => self.dyadic_pervasive(Scalar::atan2, atan2::num_num)?,
                Min => self.dyadic_pervasive(Scalar::min, min::num_num)?,
                Max => self.dyadic_pervasive(Scalar::max, max::num_num)?,
                Complex => self.dyadic_pervasive(Scalar::complex, complex::num_num)?,
                Eq => self.dyadic_pervasive(Scalar::is_eq, is_eq::num_num)?,
                Ne => self.dyadic_pervasive(Scalar::is_ne, is_ne::num_num)?,
                Lt => self.dyadic_pervasive(Scalar::other_is_lt, other_is_lt::num_num)?,
                Le => self.dyadic_pervasive(Scalar::other_is_le, other_is_le::num_num)?,
                Gt => self.dyadic_pervasive(Scalar::other_is_gt, other_is_gt::num_num)?,
                Ge => self.dyadic_pervasive(Scalar::other_is_ge, other_is_ge::num_num)?,
                Len => self.monadic(
                    |ty| Ok(ty.shape.row_count()),
                    |_| Ok(1u8),
                    |list| Ok(list.len()),
                )?,
                Shape => self.monadic(
                    |ty| Ok(ty.shape),
                    |_| Ok(Scalar::Num.scalar()),
                    |list| Ok(Scalar::Num.shaped([list.len()])),
                )?,
                Fix => self.top_mut(1)?.prepend_dim(1.into()),
                Box => {
                    let x = self.pop(1)?;
                    self.push(x.boxed());
                }
                First | Last => {
                    let x = self.pop(1)?;
                    self.push(x.into_row());
                }
                _ => return Err(TypeError::Unsupported(Some(prim.format().to_string()))),
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
                SetSign => self.dyadic_pervasive(Scalar::set_sign, set_sign::num_num)?,
                Root => self.dyadic_pervasive(Scalar::root, root::num_num)?,
                Log => self.dyadic_pervasive(Scalar::log, log::num_num)?,
                AbsComplex => self.dyadic_pervasive(Scalar::abs_complex, abs_complex::num)?,
                UnComplex => {
                    let x = self.copy_top()?;
                    self.monadic_pervasive(Scalar::complex_re, complex_re::generic)?;
                    self.push(x);
                    self.monadic_pervasive(Scalar::complex_im, complex_im::num)?;
                }
                UnBox => {
                    let x = self.pop(1)?;
                    self.push(x.unboxed());
                }
                UnCouple => self.unpack(2, false, Some(Couple))?,
                _ => return Err(TypeError::Unsupported(Some(format!("{prim:?}")))),
            },
            Mod(prim, ops, _) => match prim {
                Fork => self.fork(ops.clone())?,
                Bracket => self.bracket(ops.clone())?,
                Dip => self.dip(monad(ops))?,
                Both => self.both(monad(ops))?,
                By => self.by(monad(ops))?,
                On => self.on(monad(ops))?,
                With => self.with(monad(ops))?,
                Off => self.off(monad(ops))?,
                Below => self.below(monad(ops))?,
                Above => self.above(monad(ops))?,
                Rows => {
                    let f = monad(ops);
                    let args = self.pop_n(f.sig.args())?;
                    let mut row_count = Dim::MIN;
                    let mut all_scalar = true;
                    for (i, a) in args.iter().enumerate() {
                        let ash = a.shape();
                        for b in &args[i + 1..] {
                            let bsh = b.shape();
                            if !ash.row_count().row_compatible(bsh.row_count()) {
                                return Err(TypeError::RowsShapes(
                                    ash.into_owned(),
                                    bsh.into_owned(),
                                ));
                            }
                        }
                        row_count = row_count.max(ash.row_count());
                        all_scalar &= ash.is_scalar();
                    }
                    self.push_all(args.into_iter().map(TypeVal::into_row));
                    let outputs = f.sig.outputs();
                    self.exec(f)?;
                    if !all_scalar {
                        for output in self.top_n_mut(outputs)? {
                            output.prepend_dim(row_count);
                        }
                    }
                }
                _ => return Err(TypeError::Unsupported(Some(prim.format().to_string()))),
            },
            ImplMod(prim, ops, _) => match *prim {
                DipN(n) => self.dip_n(n, monad(ops))?,
                BothImpl(sub) if sub.num.is_some() && sub.side.is_none() => {
                    self.both_n(sub.num.unwrap() as usize, monad(ops))?
                }
                ValidateImpl(sub) => {
                    let side = sub.map(|sub| sub.side);
                    let f = monad(ops);
                    self.exec(f)?;
                    let outputs = self.pop_n(f.sig.outputs())?;
                    let val = self.top_mut("validated value")?;
                    let mut ty = val.clone().ty();
                    for mat in outputs {
                        if let Some(type_id) = mat.as_nat() {
                            // Type checking
                            let expected = match type_id as u8 {
                                f64::TYPE_ID => Scalar::Num,
                                char::TYPE_ID => Scalar::Char,
                                Boxed::TYPE_ID => Scalar::Box(None),
                                crate::Complex::TYPE_ID => Scalar::Complex,
                                type_id => return Err(format!("Invalid type id {type_id}").into()),
                            };
                            if let Scalar::Any = ty.scalar {
                                ty.scalar = expected;
                                *val = ty.clone().into();
                            } else if discriminant(&expected) != discriminant(&ty.scalar) {
                                return Err(TypeError::TypeMismatch(expected, ty.scalar));
                            }
                        } else if let Some(shape) = mat.as_nat_list() {
                            // Shape checking
                            let shape = crate::Shape::from(shape.as_slice());
                            let dims = &ty.shape.dims;
                            let mismatch = !ty.shape.is_any()
                                && match side {
                                    Some(SubSide::Left) => {
                                        dims.iter().zip(&shape).all(|(a, b)| a == b)
                                    }
                                    Some(SubSide::Right) => if let Some(suf) = &ty.shape.suffix {
                                        suf
                                    } else {
                                        dims
                                    }
                                    .iter()
                                    .rev()
                                    .zip(shape.iter().rev())
                                    .all(|(a, b)| a == b),
                                    None => {
                                        if let Some(suf) = &ty.shape.suffix {
                                            dims.len() + suf.len() > shape.len()
                                                || !dims.iter().eq(shape.iter().take(dims.len()))
                                                || !suf
                                                    .iter()
                                                    .rev()
                                                    .eq(shape[dims.len()..].iter().rev())
                                        } else {
                                            !dims.iter().eq(&shape)
                                        }
                                    }
                                };
                            if mismatch {
                                return Err(TypeError::ShapeMismatch(side, shape, ty.shape));
                            } else if ty.shape.is_any() {
                                ty.shape = (&shape).into();
                                if let Some(side) = side {
                                    match side {
                                        SubSide::Left => ty.shape.suffix = Some(Vec::new()),
                                        SubSide::Right => swap(
                                            &mut ty.shape.dims,
                                            ty.shape.suffix.get_or_insert_default(),
                                        ),
                                    }
                                }
                                *val = ty.clone().into();
                            }
                        }
                    }
                    self.update_arg_types();
                }
                _ => return Err(TypeError::Unsupported(Some(format!("{prim:?}")))),
            },
            CustomInverse(cust, _) => match &cust.normal {
                Ok(node) => self.exec(node)?,
                Err(e) => return Err(TypeError::Inversion(e.clone())),
            },
            &PushUnder(n, _) => {
                let vals = self.pop_n(n)?;
                self.under_stack.extend(vals)
            }
            &CopyToUnder(n, _) => {
                let vals = self.copy_n(n)?;
                self.under_stack.extend(vals)
            }
            &PopUnder(n, _) => self.stack.extend(self.under_stack.pop_n(n)?),
            NoInline(node) => self.node(node)?,
            TrackCaller(sn) => self.exec(&**sn)?,
            SetOutputComment { .. } => {}
            &Unpack {
                count, unbox, prim, ..
            } => self.unpack(count, unbox, prim)?,
            _ => return Err(TypeError::Unsupported(None)),
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
                for n in ns.make_mut() {
                    *n = f64(*n);
                }
                Ok(TypeVal::NumList(ns))
            },
        )
    }
    fn dyadic_pervasive<N: Into<TypeVal>>(
        &mut self,
        f: impl Fn(Scalar, Scalar) -> Result<Scalar, TypeError>,
        f64: impl Fn(f64, f64) -> N,
    ) -> Result<(), TypeError> {
        self.dyadic(
            |a, b| {
                Ok(Type {
                    scalar: f(a.scalar, b.scalar)?,
                    shape: pervade_dyn_shapes(a.shape, b.shape)?,
                })
            },
            |a, b| Ok(f64(a, b)),
        )
    }
    fn monadic<T: Into<TypeVal>, N: Into<TypeVal>, L: Into<TypeVal>>(
        &mut self,
        f: impl Fn(Type) -> Result<T, TypeError>,
        num: impl Fn(f64) -> Result<N, TypeError>,
        list: impl Fn(EcoVec<f64>) -> Result<L, TypeError>,
    ) -> Result<(), TypeError> {
        let x = self.pop(1)?;
        self.push(match x {
            TypeVal::Num(n) => num(n)?.into(),
            TypeVal::NumList(ns) => list(ns)?.into(),
            tv => f(tv.ty())?.into(),
        });
        Ok(())
    }
    fn dyadic<T: Into<TypeVal>, N: Into<TypeVal>>(
        &mut self,
        f: impl Fn(Type, Type) -> Result<T, TypeError>,
        num: impl Fn(f64, f64) -> Result<N, TypeError>,
    ) -> Result<(), TypeError> {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(match (a, b) {
            (TypeVal::Num(a), TypeVal::Num(b)) => num(a, b)?.into(),
            (a, b) => f(a.ty(), b.ty())?.into(),
        });
        Ok(())
    }
    fn unpack(&mut self, n: usize, unbox: bool, prim: Option<Primitive>) -> Result<(), TypeError> {
        let x = self.pop(1)?;
        if x.row_count() != n {
            return Err(if let Some(prim) = prim {
                format!(
                    "Cannot {} {} array of shape {} into {n} rows",
                    Primitive::Un.format(),
                    prim.format(),
                    x.shape()
                )
            } else {
                format!("Cannot unpack array of shape {} into {n} rows", x.shape())
            }
            .into());
        }
        match x {
            TypeVal::Num(n) => self.push(n),
            TypeVal::NumList(list) => self.push_all(list.into_iter().map(Into::into)),
            TypeVal::Type(ty) => {
                let mut tv = TypeVal::from(ty.into_row());
                if unbox {
                    tv = tv.unboxed();
                }
                self.push_all(repeat_n(tv, n))
            }
            TypeVal::Val(mut val) if n == 1 => {
                val.undo_fix();
                self.push(val.unboxed_if(unbox))
            }
            TypeVal::Val(val) => self.push_all(val.into_rows().map(|v| v.unboxed_if(unbox).into())),
        };
        Ok(())
    }
}

pub(crate) fn pervade_dyn_shapes(a: DynShape, b: DynShape) -> Result<DynShape, TypeError> {
    if a.is_any() {
        return Ok(b);
    }
    if b.is_any() {
        return Ok(a);
    }
    let mut shape = DynShape::scalar();
    for i in 0..a.dims.len().max(b.dims.len()) {
        // TODO: Handle fills
        let new_dim = match (a.dims.get(i).copied(), b.dims.get(i).copied()) {
            (None, None) => unreachable!(),
            (Some(d), None | Some(Dim::Dyn)) | (None | Some(Dim::Dyn), Some(d)) => d,
            (Some(d), Some(Dim::Static(1))) | (Some(Dim::Static(1)), Some(d)) => d,
            (Some(a), Some(b)) if a == b => a,
            (Some(_), Some(_)) => {
                return Err(TypeError::DyadicPervasiveShapes(a, b));
            }
        };
        shape.dims.push(new_dim);
    }
    shape.suffix = a.suffix.xor(b.suffix);
    Ok(shape)
}

fn monad(ops: &[SigNode]) -> &SigNode {
    let [f] = get_ops(ops);
    f
}

fn get_ops<const N: usize>(ops: &[SigNode]) -> &[SigNode; N] {
    ops.try_into().unwrap()
}
