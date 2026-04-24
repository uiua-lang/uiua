#![allow(dead_code)]

use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    iter::repeat_n,
    mem::{discriminant, replace, swap, take},
    ops::{BitOr, Div, Mul},
};

use ecow::{EcoVec, eco_vec};
use serde::*;

use crate::{
    Array, ArrayCmp, ArrayValue, Assembly, Boxed, Complex, Exec, HasStack, ImplPrimitive, Node,
    PrimClass, Primitive, Shape, SigNode, StackArg, SubSide, SysOp, Value, grid_fmt::GridFmt,
};

pub type TypeSig = (Vec<TypeVal>, Vec<TypeVal>);

pub fn typecheck(sn: &SigNode, asm: &Assembly) -> Result<TypeSig, (TypeError, usize)> {
    let mut env = TypeEnv {
        asm,
        stack: vec![TypeVal::default(); sn.sig.args()],
        under_stack: Vec::new(),
        call_stack: Vec::new(),
        arg_types: vec![TypeVal::default(); sn.sig.args()],
        fill_stack: Vec::new(),
        stashed_fills: Vec::new(),
        can_set_arg_types: true,
    };
    env.exec(sn)
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
    fill_stack: Vec<TypeVal>,
    stashed_fills: Vec<TypeVal>,
    can_set_arg_types: bool,
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
    fn underflow_error<A: StackArg>(&self, arg: A) -> Self::Error {
        TypeError::Underflow(arg.underflow_message())
    }
}

#[derive(Debug)]
pub enum TypeError {
    Unsupported(Option<String>),
    Underflow(String),
    DyadicPervasiveShapes(DynShape, DynShape),
    RowsShapes(DynShape, DynShape),
    TypeMismatch(Scalar, Scalar),
    ShapeMismatch(DynShape, DynShape),
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
            TypeError::DyadicPervasiveShapes(a, b) => {
                write!(f, "shapes {a} and {b} are not compatible")
            }
            TypeError::RowsShapes(a, b) => write!(f, "shapes {a} and {b} are not compatible"),
            TypeError::TypeMismatch(expected, found) => {
                write!(f, "expected {expected} but found {found}")
            }
            TypeError::ShapeMismatch(expected, found) => {
                write!(f, "expected shape {expected} but found {found}")
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

pub type TypeResult<T = ()> = Result<T, TypeError>;

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
            TypeVal::Num(_) => Scalar::Num.scalar_type(),
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
    pub fn scalar(&self) -> Scalar {
        match self {
            TypeVal::Num(_) | TypeVal::NumList(_) => Scalar::Num,
            TypeVal::Val(val) => Scalar::from(val),
            TypeVal::Type(ty) => ty.scalar.clone(),
        }
    }
    pub fn set_scalar(&mut self, scalar: Scalar) {
        if scalar.is_any() {
            return;
        }
        match (self, scalar) {
            (TypeVal::Num(_) | TypeVal::NumList(_), Scalar::Num) => {}
            (TypeVal::Val(val), scalar) if Type::from(&*val).scalar == scalar => {}
            (tv, scalar) => {
                let mut ty = take(tv).ty();
                ty.scalar = scalar;
                *tv = ty.into();
            }
        }
    }
    pub fn set_shape(&mut self, shape: DynShape) {
        let curr = self.shape();
        if *curr == shape {
            return;
        }
        let mut ty = take(self).ty();
        ty.shape = shape;
        *self = ty.into();
    }
    pub fn rank(&self) -> usize {
        match self {
            TypeVal::Num(_) => 0,
            TypeVal::NumList(_) => 1,
            TypeVal::Val(val) => val.rank(),
            TypeVal::Type(ty) => ty.shape.rank(),
        }
    }
    pub fn leading_rank(&self) -> usize {
        match self {
            TypeVal::Num(_) => 0,
            TypeVal::NumList(_) => 1,
            TypeVal::Val(val) => val.rank(),
            TypeVal::Type(ty) => ty.shape.dims.len(),
        }
    }
    pub fn suffix_rank(&self) -> Option<usize> {
        match self {
            TypeVal::Type(ty) => ty.shape.suffix.as_ref().map(|suf| suf.len()),
            _ => None,
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
    pub fn as_dim(&self) -> Option<Dim> {
        let n = match self {
            TypeVal::Num(n) => *n,
            TypeVal::Val(Value::Num(arr)) if arr.rank() == 0 => arr.data[0],
            TypeVal::Val(Value::Byte(arr)) if arr.rank() == 0 => arr.data[0] as f64,
            _ => return None,
        };
        if n.is_infinite() {
            Some(Dim::Dyn)
        } else if n.fract() == 0.0 && n >= 0.0 {
            Some(Dim::Static(n as usize))
        } else {
            None
        }
    }
    pub fn as_dims(&self) -> Option<Vec<Dim>> {
        Some(match self {
            TypeVal::NumList(nums)
                if (nums.iter()).all(|&n| n == f64::INFINITY || n >= 0.0 && n.fract() == 0.0) =>
            {
                nums.iter().copied().map(Into::into).collect()
            }
            TypeVal::Val(Value::Num(arr))
                if arr.rank() == 1
                    && (arr.data.iter())
                        .all(|&n| n == f64::INFINITY || n >= 0.0 && n.fract() == 0.0) =>
            {
                arr.data.iter().copied().map(Into::into).collect()
            }
            TypeVal::Val(Value::Byte(arr)) if arr.rank() == 1 => {
                arr.data.iter().map(|&f| Dim::Static(f as usize)).collect()
            }
            _ => return None,
        })
    }
    pub fn is_any(&self) -> bool {
        matches!(self, TypeVal::Type(ty) if ty.is_any())
    }
    pub fn reshape_scalar(&mut self, dim: Dim, suffix: bool) {
        match (dim, suffix) {
            (Dim::Static(n), false) => match self {
                TypeVal::Num(x) => *self = eco_vec![*x; n].into(),
                TypeVal::NumList(list) => {
                    let mut val: Value = Value::from(take(list));
                    val.reshape_scalar(Ok(n as isize), false, &()).unwrap();
                    *self = val.into()
                }
                TypeVal::Val(val) => val.reshape_scalar(Ok(n as isize), false, &()).unwrap(),
                TypeVal::Type(ty) => ty.shape.dims.insert(0, dim),
            },
            _ => {
                let mut ty = take(self).ty();
                if suffix {
                    ty.shape.suffix.get_or_insert_default()
                } else {
                    &mut ty.shape.dims
                }
                .insert(0, dim);
                *self = ty.into();
            }
        }
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

impl From<EcoVec<f64>> for TypeVal {
    fn from(list: EcoVec<f64>) -> Self {
        TypeVal::NumList(list)
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub struct Type {
    pub scalar: Scalar,
    pub shape: DynShape,
}

impl Type {
    pub fn new(scalar: impl Into<Scalar>, shape: impl Into<DynShape>) -> Self {
        Type {
            scalar: scalar.into(),
            shape: shape.into(),
        }
    }
    pub fn listy() -> Type {
        DynShape::prefix([Dim::Dyn]).any_scalar()
    }
    pub fn list() -> Type {
        DynShape::from(Dim::Dyn).any_scalar()
    }
    pub fn string() -> Type {
        Scalar::Char.shaped(Dim::Dyn)
    }
    pub fn is_any(&self) -> bool {
        self.scalar.is_any() && self.shape.is_any()
    }
    pub fn into_row(self) -> Self {
        Type {
            scalar: self.scalar,
            shape: self.shape.into_row(),
        }
    }
    pub fn is_string(&self) -> bool {
        self.shape.rank() <= 1 && self.scalar.compatible_with(&Scalar::Char)
            || self.shape.rank() == 0 && self.scalar.compatible_with(&Scalar::Box(None))
    }
    pub fn boxed(self) -> Self {
        Scalar::Box(Some(self.into())).scalar_type()
    }
    pub fn box_list(self) -> Self {
        Scalar::Box(Some(self.into())).shaped(Dim::Dyn)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub enum Scalar {
    #[default]
    Any,
    Num,
    Char,
    Box(Option<Box<Type>>),
    Complex,
    Stream,
}
impl Scalar {
    pub fn is_any(&self) -> bool {
        matches!(self, Scalar::Any)
    }
    pub fn is_box(&self) -> bool {
        matches!(self, Scalar::Box(_))
    }
    pub fn scalar_type(self) -> Type {
        self.shaped([0usize; 0])
    }
    pub fn any_shape(self) -> Type {
        self.shaped(DynShape::any())
    }
    pub fn maybe_scalar_type(self) -> Option<Type> {
        if self.is_any() {
            None
        } else {
            Some(self.scalar_type())
        }
    }
    pub fn shaped(self, shape: impl Into<DynShape>) -> Type {
        Type {
            scalar: self,
            shape: shape.into(),
        }
    }
    pub fn compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Scalar::Any, _)
            | (_, Scalar::Any)
            | (Scalar::Num, Scalar::Num)
            | (Scalar::Char, Scalar::Char)
            | (Scalar::Complex, Scalar::Complex)
            | (Scalar::Box(_), Scalar::Box(_))
            | (Scalar::Stream | Scalar::Box(_), Scalar::Stream | Scalar::Box(_)) => true,
            _ => false,
        }
    }
    pub fn compatible_with_boxes(&self, other: &Self) -> bool {
        match (self, other) {
            (Scalar::Box(Some(a)), Scalar::Box(Some(b))) => {
                a.scalar.compatible_with_boxes(&b.scalar) && a.shape.compatible_with(&b.shape)
            }
            _ => self.compatible_with(other),
        }
    }
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Scalar::Box(Some(a)), Scalar::Box(Some(b))) => {
                if a.scalar.compatible_with_boxes(&b.scalar) && a.shape.compatible_with(&b.shape) {
                    Scalar::Box(Some(a.max(b)))
                } else {
                    Scalar::Box(None)
                }
            }
            (a, b) if a == b => a,
            _ => Scalar::Any,
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

impl From<Vec<Dim>> for DynShape {
    fn from(dims: Vec<Dim>) -> Self {
        DynShape { dims, suffix: None }
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
    pub fn prefix(prefix: impl Into<Vec<Dim>>) -> Self {
        DynShape {
            dims: prefix.into(),
            suffix: Some(Vec::new()),
        }
    }
    pub fn any_scalar(self) -> Type {
        Type {
            scalar: Scalar::Any,
            shape: self,
        }
    }
    pub fn row_count(&self) -> Dim {
        self.dims.first().copied().unwrap_or_else(|| {
            if self.suffix.is_some() {
                Dim::Dyn
            } else {
                Dim::Static(1)
            }
        })
    }
    pub fn rank(&self) -> usize {
        self.dims.len() + self.suffix.as_ref().map_or(0, |s| s.len())
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
    pub fn compatible_with(&self, other: &Self) -> bool {
        self.dims.len() == other.dims.len()
            && self
                .dims
                .iter()
                .zip(&other.dims)
                .all(|(a, b)| Dim::row_compatible(*a, *b))
    }
}

impl Default for DynShape {
    fn default() -> Self {
        Self::any()
    }
}

impl FromIterator<Dim> for DynShape {
    fn from_iter<T: IntoIterator<Item = Dim>>(iter: T) -> Self {
        iter.into_iter().collect::<Vec<_>>().into()
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

impl From<f64> for Dim {
    fn from(n: f64) -> Self {
        if n == f64::INFINITY {
            Dim::Dyn
        } else {
            Dim::Static(n as usize)
        }
    }
}

impl Mul for Dim {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Dim::Static(a), Dim::Static(b)) => Dim::Static(a * b),
            _ => Dim::Dyn,
        }
    }
}

impl Div for Dim {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Dim::Static(_), Dim::Static(0)) => Dim::Dyn,
            (Dim::Static(a), Dim::Static(b)) => Dim::Static(a / b),
            _ => Dim::Dyn,
        }
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
    pub fn cmp_defined(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Dim::Static(_), Dim::Dyn) => Ordering::Greater,
            (Dim::Dyn, Dim::Static(_)) => Ordering::Less,
            (Dim::Dyn, Dim::Dyn) => Ordering::Equal,
            (Dim::Static(a), Dim::Static(b)) => a.cmp(b),
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
            Dim::Dyn => write!(f, "_"),
        }
    }
}

impl DynShape {
    fn fmt_inner(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            if self.dims.is_empty() {
                write!(f, "…")?;
            } else {
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

impl fmt::Display for DynShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        self.fmt_inner(f)?;
        write!(f, "]")
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_any() {
            write!(f, "…*")
        } else if self.scalar.is_any() {
            write!(f, "{}", self.shape)
        } else if self.shape.is_scalar() {
            write!(f, "{}", self.scalar)
        } else if self.scalar == Scalar::Char
            && self.shape.dims == [Dim::Dyn]
            && self.shape.suffix.is_none()
        {
            write!(f, "str")
        } else {
            write!(f, "[")?;
            self.shape.fmt_inner(f)?;
            write!(f, " {}]", self.scalar)
        }
    }
}

impl fmt::Display for TypeVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeVal::Num(n) => write!(f, "{}", n.grid_string(false)),
            TypeVal::NumList(list) => {
                write!(f, "[")?;
                for (i, n) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", n.grid_string(false))?;
                }
                write!(f, "]")
            }
            TypeVal::Val(val) => {
                let s = val.grid_string(false);
                if s.contains("\n") {
                    write!(f, "{s}")
                } else {
                    Type::from(val).fmt(f)
                }
            }
            TypeVal::Type(ty) => ty.fmt(f),
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
            Scalar::Stream => write!(f, "stream"),
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

impl<'a> Exec<&SigNode> for TypeEnv<'a> {
    type Output = ();
    fn exec(&mut self, sn: &SigNode) -> Result<Self::Output, Self::Error> {
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
}

impl<'a> TypeEnv<'a> {
    fn type_hint(&mut self, tys: impl IntoIterator<Item = Type>) {
        for (tv, ty) in self.stack.iter_mut().rev().zip(tys) {
            if tv.scalar().is_any() {
                tv.set_scalar(ty.scalar);
            }
            if tv.shape().is_any() {
                tv.set_shape(ty.shape);
            }
        }
        self.update_arg_types();
    }
    fn update_arg_types(&mut self) {
        if !self.can_set_arg_types || self.stack.len() > self.arg_types.len() {
            return;
        }
        for (tv, arg) in self.stack.iter().rev().zip(self.arg_types.iter_mut().rev()) {
            if let TypeVal::Type(arg_ty) = arg {
                if let TypeVal::Type(ty) = tv {
                    if arg_ty.scalar.is_any() {
                        arg_ty.scalar = ty.scalar.clone();
                    }
                    if arg_ty.shape.is_any() {
                        arg_ty.shape = ty.shape.clone();
                    }
                    if (arg_ty.shape.suffix.as_ref()).is_some_and(|suf| suf.is_empty())
                        && ty.shape.suffix.is_some()
                    {
                        arg_ty.shape.suffix = ty.shape.suffix.clone();
                    }
                    if arg_ty.shape.dims.is_empty() && arg_ty.shape.suffix.is_some() {
                        arg_ty.shape.dims = ty.shape.dims.clone();
                    }
                } else if arg_ty.is_any() {
                    *arg = tv.clone();
                }
            }
        }
    }
    fn exec_no_fill(&mut self, sn: &SigNode) -> TypeResult {
        let fill_stack = take(&mut self.fill_stack);
        let len = fill_stack.len();
        self.stashed_fills.extend(fill_stack);
        self.exec(sn)?;
        self.fill_stack = self.stashed_fills.split_off(self.stashed_fills.len() - len);
        Ok(())
    }
    fn node(&mut self, node: &Node) -> TypeResult {
        let span = node.span();
        self.call_stack.extend(span);
        let res = self.node_impl(node);
        if let Ok(()) | Err(TypeError::Unsupported(_)) = res
            && span.is_some()
        {
            self.call_stack.pop();
        }

        fn node_allows_more_arg_types(node: &Node) -> bool {
            use {ImplPrimitive::*, Node::*};
            match node {
                Run(nodes) => nodes.iter().all(node_allows_more_arg_types),
                Push(_) => true,
                Prim(prim, _) => prim.class() == PrimClass::Arguments,
                Mod(prim, args, _) => {
                    prim.class() == PrimClass::Arguments
                        && args.iter().all(|sn| node_allows_more_arg_types(&sn.node))
                }
                ImplPrim(Over, _) => true,
                ImplMod(DipN(_) | BothImpl(_), args, _) => {
                    args.iter().all(|sn| node_allows_more_arg_types(&sn.node))
                }
                ImplMod(ValidateImpl(_), ..) => true,
                _ => false,
            }
        }

        self.can_set_arg_types = self.can_set_arg_types && node_allows_more_arg_types(node);
        res
    }
    fn node_impl(&mut self, node: &Node) -> TypeResult {
        use self::Type;
        use {crate::algorithm::pervade::*, ImplPrimitive::*, Node::*, Primitive::*};

        fn unsupported() -> Result<TypeVal, TypeError> {
            Err(TypeError::Unsupported(None))
        }

        match node {
            Run(nodes) => {
                for node in nodes {
                    self.node(node)?;
                }
            }
            Call(f, _) => {
                let sn = SigNode::new(f.sig, self.asm[f].clone());
                self.exec_no_fill(&sn)?
            }
            Push(val) => self.push(val.clone()),
            Prim(prim, _) => match prim {
                Identity => _ = self.require_height(1)?,
                Pop => _ = self.pop(1)?,
                Dup => self.dup()?,
                Flip => self.flip()?,
                Neg => self.monadic_pervasive(Scalar::scalar_neg, scalar_neg::num)?,
                Not => self.monadic_pervasive_hint(Scalar::Num, Scalar::not, not::num)?,
                Abs => self.monadic_pervasive(Scalar::scalar_abs, scalar_abs::num)?,
                Sign => self.monadic_pervasive(Scalar::sign, sign::num)?,
                Reciprocal => {
                    self.monadic_pervasive_hint(Scalar::Num, Scalar::recip, recip::num)?
                }
                Sqrt => self.monadic_pervasive_hint(Scalar::Num, Scalar::sqrt, sqrt::num)?,
                Exp => self.monadic_pervasive_hint(Scalar::Num, Scalar::exp, exp::num)?,
                Sin => self.monadic_pervasive_hint(Scalar::Num, Scalar::sin, sin::num)?,
                Floor => self.monadic_pervasive_hint(Scalar::Num, Scalar::floor, floor::num)?,
                Ceil => self.monadic_pervasive_hint(Scalar::Num, Scalar::ceil, ceil::num)?,
                Round => self.monadic_pervasive_hint(Scalar::Num, Scalar::round, round::num)?,
                Add => self.dyadic_pervasive_hint(Scalar::Num, Scalar::add, add::num_num)?,
                Sub => self.dyadic_pervasive_hint(Scalar::Num, Scalar::sub, sub::num_num)?,
                Mul => self.dyadic_pervasive_hint(Scalar::Num, Scalar::mul, mul::num_num)?,
                Div => self.dyadic_pervasive_hint(Scalar::Num, Scalar::div, div::num_num)?,
                Modulo => {
                    self.dyadic_pervasive_hint(Scalar::Num, Scalar::modulo, modulo::num_num)?
                }
                Or => self.dyadic_pervasive_hint(Scalar::Num, Scalar::or, or::num_num)?,
                Pow => self.dyadic_pervasive_hint(
                    Scalar::Num,
                    Scalar::scalar_pow,
                    scalar_pow::num_num,
                )?,
                Atan => self.dyadic_pervasive_hint(Scalar::Num, Scalar::atan2, atan2::num_num)?,
                Min => self.dyadic_pervasive(Scalar::min, min::num_num)?,
                Max => self.dyadic_pervasive(Scalar::max, max::num_num)?,
                Complex => {
                    self.dyadic_pervasive_hint(Scalar::Num, Scalar::complex, complex::num_num)?
                }
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
                    |_| None,
                )?,
                Shape => self.monadic(
                    |ty| Ok(ty.shape),
                    |_| Ok(Scalar::Num.scalar_type()),
                    |list| Ok(Scalar::Num.shaped([list.len()])),
                    |_| None,
                )?,
                Fix => self.top_mut(1)?.prepend_dim(1.into()),
                Box => {
                    let x = self.pop(1)?;
                    self.push(x.boxed());
                }
                First | Last => {
                    self.type_hint([Type::listy()]);
                    let x = self.pop(1)?;
                    self.push(x.into_row());
                }
                Transpose => self.monadic(
                    |mut ty| {
                        if let Some(suf) = &mut ty.shape.suffix {
                            if ty.shape.dims.is_empty() {
                                let mid = suf.len().min(1);
                                suf.rotate_left(mid);
                            } else {
                                suf.push(ty.shape.dims.remove(0));
                            }
                        } else {
                            let mid = ty.shape.dims.len().min(1);
                            ty.shape.dims.rotate_left(mid);
                        }
                        Ok(ty)
                    },
                    Ok,
                    Ok,
                    |mut val| {
                        val.transpose();
                        Some(val)
                    },
                )?,
                Couple => self.pack(2, false, true, Some(Couple))?,
                Range => self.monadic(
                    |ty| {
                        if !ty.scalar.compatible_with(&Scalar::Num) {
                            return Err(format!("Cannot create range from {}", ty.scalar).into());
                        }
                        let shape = ty.shape;
                        Ok(Scalar::Num.shaped(if shape.rank() > 1 {
                            return Err(format!(
                                "Cannot create range from array with shape {shape}"
                            )
                            .into());
                        } else if shape.suffix.is_some() {
                            DynShape {
                                dims: vec![Dim::Dyn],
                                suffix: Some(Vec::new()),
                            }
                        } else {
                            match *shape.dims {
                                [] => Dim::Dyn.into(),
                                [Dim::Static(n)] => {
                                    let mut dims = vec![Dim::Dyn; n];
                                    dims.push(Dim::Static(n));
                                    dims.into()
                                }
                                [Dim::Dyn] => DynShape {
                                    dims: vec![Dim::Dyn],
                                    suffix: Some(Vec::new()),
                                },
                                _ => unreachable!(),
                            }
                        }))
                    },
                    |n| Ok(Type::new(Scalar::Num, n.abs() as usize)),
                    |list| {
                        let len = list.len();
                        Ok(Type::new(
                            Scalar::Num,
                            list.into_iter()
                                .map(|n| n.abs() as usize)
                                .chain([len])
                                .map(Dim::Static)
                                .collect::<Vec<_>>(),
                        ))
                    },
                    |_| None,
                )?,
                Reverse => {
                    self.type_hint([Type::listy()]);
                    self.monadic(
                        Ok,
                        Ok,
                        |mut list| {
                            list.make_mut().reverse();
                            Ok(list)
                        },
                        |mut val| {
                            val.reverse();
                            Some(val)
                        },
                    )?
                }
                Deshape => self.monadic(
                    |mut ty| {
                        ty.shape.dims = vec![if ty.shape.suffix.take().is_some() {
                            Dim::Dyn
                        } else {
                            (ty.shape.dims.into_iter()).fold(Dim::Static(1), Dim::mul)
                        }];
                        Ok(ty)
                    },
                    |n| Ok(Value::from([n])),
                    |list| Ok(Value::from(list)),
                    |mut val| {
                        val.deshape();
                        Some(val)
                    },
                )?,
                Reshape => self.dyadic(
                    |sh, mut ty| {
                        if sh.shape.suffix.is_some() {
                            return Err(TypeError::Unsupported(None));
                        }
                        if sh.shape.dims.len() > 1 {
                            return Err(format!(
                                "{} must be rank 0 or 1, but it is rank {}",
                                Reshape.format(),
                                sh.shape.dims.len()
                            )
                            .into());
                        }
                        match *sh.shape.dims.as_slice() {
                            [] => ty.shape.dims.insert(0, Dim::Dyn),
                            [Dim::Dyn] => ty.shape = DynShape::any(),
                            [Dim::Static(n)] => ty.shape = vec![Dim::Dyn; n].into(),
                            _ => unreachable!(),
                        }
                        Ok(ty)
                    },
                    |n, mut ty| {
                        ty.shape.dims.insert(0, Dim::Static(n.abs() as usize));
                        Ok(ty)
                    },
                    |list, mut ty| {
                        let has_suffix = ty.shape.suffix.take().is_some();
                        let inf_count = list.iter().filter(|&&n| n == f64::INFINITY).count();
                        if inf_count > 1 {
                            return Err(format!(
                                "{} list can have at most one ∞, but it has {inf_count}",
                                Reshape.format()
                            )
                            .into());
                        }
                        if inf_count == 1 {
                            if has_suffix {
                                ty.shape = DynShape::any();
                            } else {
                                let elem_count =
                                    ty.shape.dims.into_iter().fold(Dim::Static(1), Dim::mul);
                                let target_count = Dim::Static(
                                    list.iter()
                                        .filter(|&&n| n != f64::INFINITY)
                                        .product::<f64>()
                                        .abs() as usize,
                                );
                                ty.shape.dims = list
                                    .into_iter()
                                    .map(|n| {
                                        if n == f64::INFINITY {
                                            elem_count / target_count
                                        } else {
                                            Dim::Static(n.abs() as usize)
                                        }
                                    })
                                    .collect();
                            }
                        } else {
                            ty.shape.dims =
                                list.into_iter().map(|n| Dim::Static(n as usize)).collect();
                        }
                        Ok(ty)
                    },
                    |n, x| {
                        Ok(if n.abs() <= 1000.0 {
                            Value::from(eco_vec![x; n.abs() as usize]).into()
                        } else {
                            TypeVal::Type(Type::new(Scalar::Num, n.abs() as usize))
                        })
                    },
                )?,
                Sort => {
                    self.type_hint([Type::listy()]);
                    self.monadic(
                        Ok,
                        Ok,
                        |mut list| {
                            list.make_mut().sort_unstable_by(f64::array_cmp);
                            Ok(list)
                        },
                        |mut val| {
                            val.sort_up();
                            Some(val)
                        },
                    )?
                }
                Rise => {
                    self.type_hint([Type::listy()]);
                    self.monadic(
                        |mut ty| {
                            ty.scalar = Scalar::Num;
                            if ty.shape.suffix.take().is_some() && ty.shape.dims.is_empty() {
                                ty.shape = DynShape::any();
                            } else {
                                ty.shape.dims.truncate(1);
                            }
                            Ok(ty)
                        },
                        |_| Ok(Value::from(0)),
                        |list| Ok(Type::new(Scalar::Num, list.len())),
                        |val| Some(Value::from(val.rise())),
                    )?
                }
                Fall => {
                    self.type_hint([Type::listy()]);
                    self.monadic(
                        |mut ty| {
                            ty.scalar = Scalar::Num;
                            if ty.shape.suffix.take().is_some() && ty.shape.dims.is_empty() {
                                ty.shape = DynShape::any();
                            } else {
                                ty.shape.dims.truncate(1);
                            }
                            Ok(ty)
                        },
                        |_| Ok(Value::from(0)),
                        |list| Ok(Type::new(Scalar::Num, list.len())),
                        |val| Some(Value::from(val.fall())),
                    )?
                }
                Match => {
                    self.pop_n(2)?;
                    self.push(Scalar::Num);
                }
                Select => self.dyadic(
                    |indices, mut ty| {
                        if !indices.scalar.compatible_with(&Scalar::Num) {
                            return Err(format!(
                                "Cannot {} with {} indices",
                                Select.format(),
                                indices.scalar
                            )
                            .into());
                        }
                        ty = ty.into_row();
                        let mut idx_shape = indices.shape;
                        if let Some(suf) = &mut idx_shape.suffix {
                            if ty.shape.suffix.is_some() {
                                ty.shape.dims = idx_shape.dims;
                            } else {
                                suf.extend(ty.shape.dims);
                                ty.shape = idx_shape;
                            }
                        } else {
                            idx_shape.dims.extend(ty.shape.dims);
                            ty.shape.dims = idx_shape.dims;
                        }
                        Ok(ty)
                    },
                    |index, ty| {
                        if let Dim::Static(n) = ty.shape.row_count()
                            && (index >= 0.0 && index as usize >= n
                                || index < 0.0 && index.abs() as usize > n)
                        {
                            return Err(
                                format!("Index {index} is out of bounds of length {n}").into()
                            );
                        }
                        Ok(ty.into_row())
                    },
                    |indices, mut ty| {
                        if let Dim::Static(n) = ty.shape.row_count()
                            && let Some(i) = indices.iter().find(|&&i| {
                                i >= 0.0 && i as usize >= n || i < 0.0 && i.abs() as usize > n
                            })
                        {
                            return Err(format!("Index {i} is out of bounds of length {n}").into());
                        }
                        ty = ty.into_row();
                        ty.shape.dims.insert(0, Dim::Static(indices.len()));
                        Ok(ty)
                    },
                    |index, n| {
                        if index != 0.0 && index != 1.0 {
                            Err(format!("Index {index} is out of bounds of length 1").into())
                        } else {
                            Ok(n)
                        }
                    },
                )?,
                Take => {
                    self.dyadic(
                        |amnt, mut ty| {
                            if !amnt.scalar.compatible_with(&Scalar::Num) {
                                return Err(format!(
                                    "Cannot {} with {} amount",
                                    Take.format(),
                                    amnt.scalar
                                )
                                .into());
                            }
                            if amnt.shape.dims.len() > 1 {
                                return Err(format!(
                                    "Cannot {} amount of shape {}",
                                    Take.format(),
                                    amnt.shape
                                )
                                .into());
                            }
                            if amnt.shape.suffix.is_some() {
                                return Err(TypeError::Unsupported(None));
                            }
                            if amnt.shape.dims.is_empty() {
                                return Ok(ty);
                            }
                            let n = amnt.shape.dims[0];
                            match (n, ty.shape.row_count()) {
                                (Dim::Static(n), Dim::Static(r)) if n > r => {
                                    return Err(format!(
                                        "Cannot take {n} rows from array with shape {}",
                                        ty.shape
                                    )
                                    .into());
                                }
                                (Dim::Static(n), _) => ty.shape.dims[0] = Dim::Static(n),
                                _ => ty.shape.dims[0] = Dim::Dyn,
                            }
                            Ok(ty)
                        },
                        |n, mut ty| {
                            if !n.is_infinite() {
                                ty = ty.into_row();
                                ty.shape.dims.insert(0, Dim::Static(n.abs() as usize));
                            }
                            Ok(ty)
                        },
                        |list, mut ty| {
                            if list.iter().any(|n| n.is_infinite()) {
                                ty.shape = DynShape::any();
                            } else {
                                for _ in 0..list.len() {
                                    ty = ty.into_row();
                                }
                                for n in list.into_iter().rev() {
                                    ty.shape.dims.insert(0, Dim::Static(n.abs() as usize));
                                }
                            }
                            Ok(ty)
                        },
                        |n, x| {
                            Ok::<TypeVal, _>(match n {
                                0.0 => Value::default().into(),
                                1.0 => Value::from([x]).into(),
                                n => Type::new(Scalar::Num, [n.abs() as usize]).into(),
                            })
                        },
                    )?;
                    self.update_arg_types();
                }
                Rand => self.push(Scalar::Num.scalar_type()),
                Parse => {
                    fn parse(ty: Type) -> TypeResult<Type> {
                        let mut shape = ty.shape;
                        match ty.scalar {
                            Scalar::Char | Scalar::Any => {
                                if let Some(suf) = &mut shape.suffix {
                                    suf.pop();
                                } else {
                                    shape.dims.pop();
                                }
                            }
                            Scalar::Box(None) => {}
                            Scalar::Box(Some(boxed)) => {
                                let ty = parse(*boxed)?;
                                if ty.shape.suffix.is_some() {
                                    shape.suffix = Some(Vec::new());
                                } else {
                                    shape.dims.extend(ty.shape.dims);
                                }
                            }
                            scalar => {
                                return Err(format!("Cannot {} {scalar}", Parse.format()).into());
                            }
                        }
                        Ok(Type::new(Scalar::Num, shape))
                    }
                    let ty = self.pop(1)?.ty();
                    self.push(parse(ty)?);
                }
                Args => {}
                // TODO (descending priority):
                // - Input type suggestion
                // - pick, drop
                // - keep, rotate, where
                // - bits, base, memberof, indexin
                // - classify, occurences, deduplicate, find, mask, orient
                // - map functions
                Sys(SysOp::FReadAllStr | SysOp::FReadAllBytes) => {
                    self.type_hint([Type::new(Scalar::Char, Dim::Dyn)]);
                    let path = self.pop(1)?.ty();
                    if !path.is_string() {
                        return Err(format!(
                            "{}'s path must be a string, but it is {path}",
                            prim.format()
                        )
                        .into());
                    }
                    let scalar = if *prim == Sys(SysOp::FReadAllBytes) {
                        Scalar::Num
                    } else {
                        Scalar::Char
                    };
                    self.push(scalar.shaped(Dim::Dyn));
                }
                Sys(SysOp::ReadBytes | SysOp::ReadStr | SysOp::Seek) => {
                    self.type_hint([
                        Scalar::Num.scalar_type(),
                        Scalar::Stream.scalar_type().boxed(),
                    ]);
                    let _count = self.pop(1)?;
                    let _handle = self.pop(2)?;
                    let scalar = if *prim == Sys(SysOp::ReadBytes) {
                        Scalar::Num
                    } else {
                        Scalar::Char
                    };
                    self.push(scalar.shaped(Dim::Dyn));
                }
                Sys(SysOp::Write) => {
                    self.type_hint([Scalar::Stream.scalar_type().boxed(), Type::list()]);
                    let _handle = self.pop(1)?;
                    let _data = self.pop(2)?;
                }
                Sys(SysOp::Close) => {
                    self.type_hint([Scalar::Stream.scalar_type().boxed()]);
                    let _handle = self.pop(1)?;
                }
                Sys(SysOp::FOpen | SysOp::FCreate | SysOp::TcpConnect) => {
                    self.type_hint([Type::string()]);
                    let _path = self.pop("path")?;
                    self.push(Scalar::Stream.scalar_type().boxed());
                }
                Sys(SysOp::FDelete | SysOp::FTrash | SysOp::FMakeDir | SysOp::Invoke) => {
                    self.type_hint([Type::string()]);
                    let _path = self.pop("path")?;
                }
                Sys(SysOp::FListDir) => {
                    self.type_hint([Type::string()]);
                    let _path = self.pop("path")?;
                    self.push(Type::string().box_list());
                }
                Sys(SysOp::FIsFile | SysOp::FExists) => {
                    self.type_hint([Type::string()]);
                    let _path = self.pop("path")?;
                    self.push(Scalar::Num);
                }
                Sys(SysOp::FWriteAll) => {
                    self.type_hint([Type::string(), Type::list()]);
                    let _path = self.pop(1)?;
                    let _data = self.pop(2)?;
                }
                Sys(SysOp::RawMode | SysOp::Exit) => {
                    self.type_hint([Scalar::Num.scalar_type()]);
                    let _on = self.pop(1)?;
                }
                Sys(SysOp::Var) => {
                    self.type_hint([Type::string()]);
                    let _name = self.pop(1)?;
                    self.push(Type::string());
                }
                Sys(SysOp::EnvArgs) => self.push(Type::string().box_list()),
                Sys(
                    SysOp::Prin | SysOp::Print | SysOp::PrinErr | SysOp::PrintErr | SysOp::Show,
                ) => _ = self.pop(1)?,
                Sys(SysOp::TermSize) => self.push(Scalar::Num.shaped(2)),
                Sys(SysOp::RunInherit) => {
                    let _args = self.pop(1);
                    self.push(Scalar::Num);
                }
                Sys(SysOp::RunCapture) => {
                    let _args = self.pop(1);
                    self.push(Type::string());
                    self.push(Type::string());
                    self.push(Scalar::Num);
                }
                Sys(SysOp::RunStream) => {
                    let _args = self.pop(1);
                    for _ in 0..3 {
                        self.push(Scalar::Stream.scalar_type().boxed());
                    }
                }
                _ => return Err(TypeError::Unsupported(Some(prim.format().to_string()))),
            },
            ImplPrim(prim, _) => match prim {
                Over => self.over()?,
                Ln => self.monadic_pervasive_hint(Scalar::Num, Scalar::ln, ln::num)?,
                Cos => self.monadic_pervasive_hint(Scalar::Num, Scalar::cos, cos::num)?,
                Asin => self.monadic_pervasive_hint(Scalar::Num, Scalar::asin, asin::num)?,
                Acos => self.monadic_pervasive_hint(Scalar::Num, Scalar::acos, acos::num)?,
                Exp2 => self.monadic_pervasive_hint(Scalar::Num, Scalar::exp2, exp2::num)?,
                Exp10 => self.monadic_pervasive_hint(Scalar::Num, Scalar::exp10, exp10::num)?,
                Log2 => self.monadic_pervasive_hint(Scalar::Num, Scalar::log2, log2::num)?,
                Log10 => self.monadic_pervasive_hint(Scalar::Num, Scalar::log10, log10::num)?,
                SquareAbs => {
                    self.monadic_pervasive_hint(Scalar::Num, Scalar::square_abs, square_abs::num)?
                }
                NegAbs => self.monadic_pervasive(Scalar::neg_abs, neg_abs::num)?,
                SetSign => self.dyadic_pervasive(Scalar::set_sign, set_sign::num_num)?,
                Root => self.dyadic_pervasive_hint(Scalar::Num, Scalar::root, root::num_num)?,
                Log => self.dyadic_pervasive_hint(Scalar::Num, Scalar::log, log::num_num)?,
                AbsComplex => {
                    self.dyadic_pervasive_hint(Scalar::Num, Scalar::abs_complex, abs_complex::num)?
                }
                UnComplex => {
                    self.type_hint([Scalar::Complex.any_shape()]);
                    let x = self.copy_top()?;
                    self.monadic_pervasive(Scalar::complex_re, complex_re::generic)?;
                    self.push(x);
                    self.monadic_pervasive(Scalar::complex_im, complex_im::num)?;
                }
                UnBox => {
                    self.type_hint([Scalar::Box(None).any_shape()]);
                    let x = self.pop(1)?;
                    self.push(x.unboxed());
                }
                UnCouple => self.unpack(2, false, Some(Couple))?,
                &TransposeN(amnt) => self.monadic(
                    |mut ty| {
                        let abs_amnt = amnt.unsigned_abs() as usize;
                        if let Some(suf) = &mut ty.shape.suffix {
                            if amnt >= 0 {
                                if ty.shape.dims.is_empty() {
                                    let mid = suf.len().min(abs_amnt);
                                    suf.rotate_left(mid);
                                } else {
                                    suf.push(ty.shape.dims.remove(0));
                                }
                            } else if let Some(dim) = suf.pop() {
                                ty.shape.dims.insert(0, dim);
                            } else {
                                ty.shape = DynShape::any();
                            }
                        } else {
                            let mid = ty.shape.dims.len().min(abs_amnt);
                            if amnt >= 0 {
                                ty.shape.dims.rotate_left(mid);
                            } else {
                                ty.shape.dims.rotate_right(mid);
                            }
                        }
                        Ok(ty)
                    },
                    Ok,
                    Ok,
                    |mut val| {
                        val.transpose_depth(0, amnt);
                        Some(val)
                    },
                )?,
                Retropose => self.monadic(
                    |mut ty| {
                        ty.shape.dims.reverse();
                        if let Some(suf) = &mut ty.shape.suffix {
                            suf.reverse();
                            swap(&mut ty.shape.dims, suf);
                        }
                        Ok(ty)
                    },
                    Ok,
                    Ok,
                    |mut val| {
                        val.retropose_depth(0);
                        Some(val)
                    },
                )?,
                SortDown => self.monadic(
                    Ok,
                    Ok,
                    |mut list| {
                        list.make_mut().sort_unstable_by(f64::array_cmp);
                        list.make_mut().reverse();
                        Ok(list)
                    },
                    |mut val| {
                        val.sort_down();
                        Some(val)
                    },
                )?,
                RandomRow => {
                    let x = self.pop(1)?;
                    self.push(x.into_row());
                }
                _ => return Err(TypeError::Unsupported(Some(format!("{prim:?}")))),
            },
            Mod(prim, ops, _) => match prim {
                Fork => self.fork(ops)?,
                Bracket => self.bracket(ops)?,
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
                    self.exec_no_fill(f)?;
                    if !all_scalar {
                        for output in self.top_n_mut(outputs)? {
                            output.prepend_dim(row_count);
                        }
                    }
                }
                Reduce => self.reduce(monad(ops))?,
                Table => self.table(monad(ops))?,
                Group | Partition => {
                    let f = monad(ops);
                    self.type_hint([Type::new(Scalar::Num, DynShape::any())]);
                    let markers = self.pop(1)?;
                    if markers.rank() > 1 {
                        return Err(TypeError::Unsupported(None));
                    }
                    if !markers.scalar().compatible_with(&Scalar::Num) {
                        return Err(format!(
                            "{}'s first argument must be numbers, not {}",
                            prim.format(),
                            markers.scalar()
                        )
                        .into());
                    }
                    let partitioned = self.pop_n(f.sig.args())?;
                    self.push_all(partitioned.into_iter().map(|tv| {
                        let mut ty = tv.ty().into_row();
                        ty.shape.dims.insert(0, Dim::Dyn);
                        ty.into()
                    }));
                    self.exec_no_fill(f)?;
                    for tv in self.top_n_mut(f.sig.outputs())? {
                        let mut shape = tv.shape().into_owned();
                        shape.dims.insert(0, Dim::Dyn);
                        tv.set_shape(shape);
                    }
                }
                Fill => {
                    let [f, g] = dyad(ops);
                    self.exec(f)?;
                    if f.sig.outputs() == 0 {
                        self.exec_no_fill(g)?;
                    } else {
                        let mut fills = self.pop_n(f.sig.outputs())?;
                        self.fill_stack.push(fills.remove(0));
                        self.exec(g)?;
                        self.fill_stack.pop();
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
                    let mut did_type = false;
                    for mat in outputs {
                        if !did_type && let Some(type_id) = mat.as_nat() {
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
                            did_type = true;
                        } else if let Some(dims) =
                            mat.as_dims().or_else(|| mat.as_dim().map(|d| vec![d]))
                        {
                            // Shape checking
                            let val_dims = &ty.shape.dims;
                            let mismatch = !ty.shape.is_any()
                                && match side {
                                    Some(SubSide::Left) => {
                                        val_dims.len() < dims.len()
                                            || val_dims.iter().zip(&dims).any(|(a, b)| a != b)
                                    }
                                    Some(SubSide::Right) => {
                                        let val_dims = if let Some(suf) = &ty.shape.suffix {
                                            suf
                                        } else {
                                            val_dims
                                        };
                                        ty.shape.rank() < dims.len()
                                            || (val_dims.iter().rev())
                                                .zip(dims.iter().rev())
                                                .any(|(a, b)| a != b)
                                    }
                                    None => {
                                        if let Some(suf) = &ty.shape.suffix {
                                            val_dims.len() + suf.len() > dims.len()
                                                || !(val_dims.iter())
                                                    .eq(dims.iter().take(val_dims.len()))
                                                || !(suf.iter().rev())
                                                    .eq(dims[val_dims.len()..].iter().rev())
                                        } else {
                                            !val_dims.iter().eq(&dims)
                                        }
                                    }
                                };
                            let shape = match side {
                                None => DynShape { dims, suffix: None },
                                Some(SubSide::Left) => DynShape {
                                    dims,
                                    suffix: Some(Vec::new()),
                                },
                                Some(SubSide::Right) => DynShape {
                                    dims: Vec::new(),
                                    suffix: Some(dims),
                                },
                            };
                            if mismatch {
                                return Err(TypeError::ShapeMismatch(shape, ty.shape));
                            } else if ty.shape.is_any() {
                                ty.shape = shape;
                            } else {
                                if ty.shape.suffix == Some(Vec::new()) && shape.suffix.is_some() {
                                    ty.shape.suffix = shape.suffix;
                                }
                                if ty.shape.dims.is_empty() && ty.shape.suffix.is_some() {
                                    ty.shape.dims = shape.dims
                                }
                            }
                            *val = ty.clone().into();
                        }
                    }
                    self.update_arg_types();
                }
                FixMatchRanks => {
                    let f = monad(ops);
                    let n = f.sig.args();
                    let max_rank = self.top_n(n)?.map(|tv| tv.rank()).max().unwrap_or(0);
                    for tv in self.top_n_mut(n)? {
                        if tv.rank() == 0 {
                            continue;
                        }
                        while tv.rank() < max_rank {
                            tv.prepend_dim(1.into());
                        }
                    }
                    self.exec(f)?;
                }
                ReduceTable => {
                    let [f, g] = dyad(ops);
                    self.table(g)?;
                    self.reduce(f)?;
                }
                _ => return Err(TypeError::Unsupported(Some(format!("{prim:?}")))),
            },
            CustomInverse(cust, _) => match &cust.normal {
                Ok(node) => self.exec(node)?,
                Err(_) => return Err(TypeError::Unsupported(None)),
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
            Label(..) | RemoveLabel(..) | SetOutputComment { .. } => {}
            &Unpack {
                count, unbox, prim, ..
            } => self.unpack(count, unbox, prim)?,
            Array {
                len,
                inner,
                boxed,
                allow_ext,
                prim,
                ..
            } => {
                self.node(inner)?;
                self.pack(*len, *boxed, *allow_ext, *prim)?;
            }
            Format(parts, _) => {
                self.pop_n(parts.len().saturating_sub(1))?;
                self.push(self::Type::new(Scalar::Char, [Dim::Dyn]));
            }
            _ => return Err(TypeError::Unsupported(None)),
        }
        Ok(())
    }
    fn table(&mut self, f: &SigNode) -> TypeResult {
        let args = self.pop_n(f.sig.args())?;
        let shape_prefix: Vec<_> = args.iter().map(TypeVal::row_count).collect();
        self.push_all(args.into_iter().map(TypeVal::into_row));
        self.exec_no_fill(f)?;
        for tv in self.top_n_mut(f.sig.outputs())? {
            let mut shape = tv.shape().into_owned();
            let suffix = replace(&mut shape.dims, shape_prefix.clone());
            shape.dims.extend(suffix);
            tv.set_shape(shape);
        }
        Ok(())
    }
    fn reduce(&mut self, f: &SigNode) -> TypeResult {
        if f.sig.args() != 2 {
            return Err(TypeError::Unsupported(None));
        }
        let mut xs = self.pop(1)?;
        if xs.row_count() == 2 {
            self.push(xs);
            self.unpack(2, false, None)?;
            return self.exec_no_fill(f);
        }
        if let Some((prim, _)) = f.node.as_flipped_primitive()
            && prim.class() == PrimClass::DyadicPervasive
        {
            let mut shape = xs.shape().into_owned();
            if shape.dims.is_empty() {
                if shape.suffix.is_some() {
                    shape = DynShape::any();
                }
            } else {
                shape.dims.remove(0);
            }
            xs.set_shape(shape);
            self.push(xs);
        } else if let Some(Node::Prim(Primitive::Join, _)) = f.node.last() {
            let mut shape = xs.shape().into_owned();
            if shape.dims.len() <= 1 {
                if shape.suffix.is_some() {
                    shape = DynShape::any();
                }
            } else {
                let first = shape.dims.remove(0);
                match (&mut shape.dims[0], first) {
                    (Dim::Static(a), Dim::Static(b)) => *a *= b,
                    (a @ Dim::Static(_), Dim::Dyn) => *a = Dim::Dyn,
                    _ => {}
                }
            }
            xs.set_shape(shape);
            self.push(xs);
        } else {
            return Err(TypeError::Unsupported(None));
        }
        Ok(())
    }
    fn monadic_pervasive_hint(
        &mut self,
        hint: Scalar,
        f: impl Fn(Scalar) -> Result<Scalar, String>,
        f64: impl Fn(f64) -> f64,
    ) -> TypeResult {
        self.type_hint([hint.any_shape()]);
        self.monadic_pervasive(f, f64)
    }
    fn monadic_pervasive(
        &mut self,
        f: impl Fn(Scalar) -> Result<Scalar, String>,
        f64: impl Fn(f64) -> f64,
    ) -> TypeResult {
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
            |_| None,
        )
    }
    fn dyadic_pervasive_hint<N: Into<TypeVal>>(
        &mut self,
        hint: Scalar,
        f: impl Fn(Scalar, Scalar, bool, bool) -> Result<Scalar, TypeError>,
        f64: impl Fn(f64, f64) -> N,
    ) -> TypeResult {
        let hint = hint.any_shape();
        self.type_hint([hint.clone(), hint]);
        self.dyadic_pervasive(f, f64)
    }
    fn dyadic_pervasive<N: Into<TypeVal>>(
        &mut self,
        f: impl Fn(Scalar, Scalar, bool, bool) -> Result<Scalar, TypeError>,
        f64: impl Fn(f64, f64) -> N,
    ) -> TypeResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(match (a, b) {
            (TypeVal::Num(a), TypeVal::Num(b)) => f64(a, b).into(),
            (a, b) => {
                let (a, b) = (a.ty(), b.ty());
                let a_fill = self.fill_for(&a);
                let b_fill = self.fill_for(&b);
                Type {
                    scalar: f(a.scalar, b.scalar, a_fill, b_fill)?,
                    shape: pervade_dyn_shapes(a.shape, b.shape, a_fill, b_fill)?,
                }
                .into()
            }
        });
        Ok(())
    }
    fn fill_for(&self, ty: &Type) -> bool {
        self.fill_stack
            .iter()
            .any(|f| f.scalar().compatible_with(&ty.scalar))
    }
    fn monadic<T: Into<TypeVal>, N: Into<TypeVal>, L: Into<TypeVal>>(
        &mut self,
        f: impl Fn(Type) -> Result<T, TypeError>,
        num: impl Fn(f64) -> Result<N, TypeError>,
        list: impl Fn(EcoVec<f64>) -> Result<L, TypeError>,
        val: impl Fn(Value) -> Option<Value>,
    ) -> TypeResult {
        let x = self.pop(1)?;
        self.push(match x {
            TypeVal::Num(n) => num(n)?.into(),
            TypeVal::NumList(ns) => list(ns)?.into(),
            TypeVal::Val(Value::Num(arr)) if arr.rank() == 0 => num(arr.data[0])?.into(),
            TypeVal::Val(Value::Byte(arr)) if arr.rank() == 0 => num(arr.data[0] as f64)?.into(),
            TypeVal::Val(Value::Num(arr)) if arr.rank() == 1 => list(arr.data.into())?.into(),
            TypeVal::Val(Value::Byte(arr)) if arr.rank() == 1 => {
                list(arr.data.into_iter().map(Into::into).collect())?.into()
            }
            TypeVal::Val(v) => {
                let ty = Type::from(&v);
                if let Some(v) = val(v) {
                    TypeVal::Val(v)
                } else {
                    f(ty)?.into()
                }
            }
            TypeVal::Type(ty) => f(ty)?.into(),
        });
        Ok(())
    }
    fn dyadic<T, N, L, NN>(
        &mut self,
        f: impl Fn(Type, Type) -> Result<T, TypeError>,
        num: impl Fn(f64, Type) -> Result<N, TypeError>,
        list: impl Fn(EcoVec<f64>, Type) -> Result<L, TypeError>,
        num_num: impl Fn(f64, f64) -> Result<NN, TypeError>,
    ) -> TypeResult
    where
        T: Into<TypeVal>,
        N: Into<TypeVal>,
        L: Into<TypeVal>,
        NN: Into<TypeVal>,
    {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(match (a, b) {
            (TypeVal::Num(a), TypeVal::Num(b)) => num_num(a, b)?.into(),
            (TypeVal::Num(a), b) => num(a, b.ty())?.into(),
            (TypeVal::NumList(a), b) => list(a, b.ty())?.into(),
            (a, b) => f(a.ty(), b.ty())?.into(),
        });
        Ok(())
    }
    fn pack(&mut self, n: usize, bx: bool, allow_ext: bool, prim: Option<Primitive>) -> TypeResult {
        if n == 0 {
            self.push(if bx {
                Array::<Boxed>::default().into()
            } else {
                Value::default()
            });
            return Ok(());
        }
        let mut tvs = self.pop_n(n)?;
        if bx {
            // Box
            for tv in &mut tvs {
                *tv = take(tv).boxed();
            }
        } else {
            // Coerce anys and boxes
            for i in 0..tvs.len().saturating_sub(1) {
                let [a, b] = tvs.get_disjoint_mut([i, i + 1]).unwrap();
                if a.scalar().is_any() {
                    a.set_scalar(b.scalar())
                }
                if b.scalar().is_any() {
                    b.set_scalar(a.scalar())
                }
                if a.scalar().is_box() && !b.scalar().is_box() {
                    b.set_scalar(Scalar::Box(b.scalar().maybe_scalar_type().map(Box::new)));
                } else if !a.scalar().is_box() && b.scalar().is_box() {
                    a.set_scalar(Scalar::Box(a.scalar().maybe_scalar_type().map(Box::new)));
                }
            }
        }
        // Combining scalars
        if let Some(v) = tvs.iter().try_fold(EcoVec::new(), |mut v, tv| {
            if let &TypeVal::Num(n) = tv {
                v.push(n);
                Some(v)
            } else {
                None
            }
        }) {
            self.push(v);
            return Ok(());
        }
        // Extend
        if allow_ext {
            let max_rank = tvs.iter().map(TypeVal::rank).max().unwrap();
            let max_shape = tvs.iter().find(|tv| tv.rank() == max_rank).unwrap().shape();
            let dims: Vec<(Dim, bool)> = (max_shape.dims.iter().map(|&d| (d, false)))
                .chain((max_shape.suffix.as_ref().into_iter().flatten()).map(|&d| (d, true)))
                .collect();
            for tv in &mut tvs {
                while tv.rank() < max_rank {
                    let (dim, suffix) = dims[max_rank - tv.rank() - 1];
                    tv.reshape_scalar(dim, suffix);
                }
            }
        }
        for win in tvs.windows(2) {
            let [a, b] = win else { unreachable!() };
            if !a.shape().compatible_with(&b.shape()) {
                return Err(if let Some(prim) = prim {
                    format!(
                        "Cannot {} arrays with shapes {} and {}",
                        prim.format(),
                        a.shape(),
                        b.shape()
                    )
                } else {
                    format!(
                        "Cannot combine arrays with shapes {} and {}",
                        a.shape(),
                        b.shape()
                    )
                }
                .into());
            }
            if !a.scalar().compatible_with(&b.scalar()) {
                return Err(if let Some(prim) = prim {
                    format!(
                        "Cannot {} {} array with {} array",
                        prim.format(),
                        a.scalar(),
                        b.scalar()
                    )
                } else {
                    format!(
                        "Cannot combine {} array with {} array",
                        a.scalar(),
                        b.scalar()
                    )
                }
                .into());
            }
        }
        let scalar = tvs
            .iter()
            .map(|tv| tv.scalar())
            .reduce(Scalar::union)
            .unwrap();
        // Figure out shape
        let mut dims = Vec::new();
        for i in 0.. {
            let Some(d) = tvs
                .iter()
                .filter_map(|tv| tv.shape().dims.get(i).copied())
                .max_by(|a, b| a.cmp_defined(b))
            else {
                break;
            };
            dims.push(d)
        }
        let mut suffix = None;
        if tvs.iter().any(|tv| tv.suffix_rank().is_some()) {
            let suffix = suffix.get_or_insert_with(Vec::new);
            for i in 0.. {
                let Some(d) = tvs
                    .iter()
                    .filter_map(|tv| tv.shape().into_owned().suffix)
                    .filter(|suf| suf.len() > i)
                    .filter_map(|suf| suf.get(suf.len() - 1 - i).copied())
                    .max_by(|a, b| a.cmp_defined(b))
                else {
                    break;
                };
                suffix.insert(0, d);
            }
        }
        dims.insert(0, Dim::Static(n));
        let shape = DynShape { dims, suffix };
        self.push(Type { scalar, shape });
        Ok(())
    }
    fn unpack(&mut self, n: usize, unbox: bool, prim: Option<Primitive>) -> TypeResult {
        self.type_hint([DynShape::prefix([Dim::Static(n)]).any_scalar()]);
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

pub(crate) fn pervade_dyn_shapes(
    ash: DynShape,
    bsh: DynShape,
    a_fill: bool,
    b_fill: bool,
) -> Result<DynShape, TypeError> {
    let mut shape = DynShape::scalar();
    for i in 0..ash.dims.len().max(bsh.dims.len()) {
        // TODO: Handle fills
        let new_dim = match (ash.dims.get(i).copied(), bsh.dims.get(i).copied()) {
            (None, None) => unreachable!(),
            (Some(d), None | Some(Dim::Dyn)) | (None | Some(Dim::Dyn), Some(d)) => d,
            (Some(d), Some(Dim::Static(1))) | (Some(Dim::Static(1)), Some(d)) => d,
            (Some(a), Some(b)) => match (a.cmp(&b), a_fill, b_fill) {
                (Ordering::Equal, ..) => a,
                (Ordering::Less, true, _) => b,
                (Ordering::Greater, _, true) => a,
                _ => return Err(TypeError::DyadicPervasiveShapes(ash, bsh)),
            },
        };
        shape.dims.push(new_dim);
    }
    shape.suffix = match (ash.suffix, bsh.suffix) {
        (None, None) => None,
        (Some(suf), None) | (None, Some(suf)) => Some(suf),
        (Some(a), Some(b)) if a.is_empty() => Some(b),
        (Some(a), Some(b)) if b.is_empty() => Some(a),
        (Some(_), Some(_)) => Some(Vec::new()),
    };
    Ok(shape)
}

fn monad(ops: &[SigNode]) -> &SigNode {
    let [f] = get_ops(ops);
    f
}

fn dyad(ops: &[SigNode]) -> &[SigNode; 2] {
    get_ops(ops)
}

fn get_ops<const N: usize>(ops: &[SigNode]) -> &[SigNode; N] {
    ops.try_into().unwrap()
}
