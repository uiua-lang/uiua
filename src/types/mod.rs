mod scalar;
mod shape;
mod ty;
mod val;

use std::{
    borrow::Cow,
    cmp::{self, Ordering},
    fmt,
    iter::repeat_n,
    mem::{discriminant, replace, swap, take},
    ops::{BitOr, Div, Mul},
    slice,
};

use ecow::{EcoString, EcoVec, eco_vec};
use serde::*;

use crate::{
    Array, ArrayCmp, ArrayValue, Assembly, Boxed, Complex, Context, Exec, HasStack, ImplPrimitive,
    Node, PrimClass, Primitive, Shape, SigNode, StackArg, SubSide, SysOp, Value, grid_fmt::GridFmt,
};

pub use {scalar::*, shape::*, ty::*, val::*};

pub type TypeSig = (Vec<TypeVal>, Vec<TypeVal>);

/// Typecheck a node
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

pub fn validate(spec: Type, ch: &mut Type, side: Option<SubSide>) -> TypeResult {
    check_scalar(spec.scalar, ch)?;
    check_shape(spec.shape, ch, side)?;
    Ok(())
}

fn check_scalar(expected: Scalar, ch: &mut Type) -> TypeResult {
    if let Scalar::Any = ch.scalar {
        ch.scalar = expected;
    } else {
        if let Scalar::Box(ScalarBox::Def(Some(name), fields)) = &expected {
            ch.as_mut_fields(Some(name), fields.len());
        }
        if !expected.superset_of(&ch.scalar) {
            return Err(TypeError::ScalarMismatch(expected, ch.scalar.clone()));
        }
    }
    Ok(())
}

fn check_shape(shape: DynShape, ch: &mut Type, side: Option<SubSide>) -> TypeResult {
    let is_any = shape.is_any();
    let dims = shape.dims;
    let ch_dims = &ch.shape.dims;
    let mismatch = !ch.shape.is_any()
        && match side {
            Some(SubSide::Left) => {
                ch_dims.len() < dims.len() && ch.shape.suffix.is_none()
                    || (ch_dims.iter().zip(&dims)).any(|(a, b)| !a.compatible(*b))
            }
            Some(SubSide::Right) => {
                let val_dims = if let Some(suf) = &ch.shape.suffix {
                    suf
                } else {
                    ch_dims
                };
                ch.shape.rank() < dims.len()
                    || (val_dims.iter().rev())
                        .zip(dims.iter().rev())
                        .any(|(a, b)| !a.compatible(*b))
            }
            None => {
                if let Some(suf) = &ch.shape.suffix {
                    ch_dims.len() + suf.len() > dims.len()
                        || !(ch_dims.iter()).eq(dims.iter().take(ch_dims.len()))
                        || !(suf.iter().rev()).eq(dims[ch_dims.len()..].iter().rev())
                } else {
                    ch_dims.len() != dims.len()
                        || ch_dims.iter().zip(&dims).any(|(a, b)| !a.compatible(*b))
                }
            }
        };

    let shape = match side {
        None => DynShape {
            dims,
            suffix: shape.suffix,
        },
        Some(SubSide::Left) => DynShape {
            dims,
            suffix: Some(Vec::new()),
        },
        Some(SubSide::Right) => DynShape {
            dims: Vec::new(),
            suffix: Some(dims),
        },
    };
    if mismatch && !is_any {
        return Err(TypeError::ShapeMismatch(shape, ch.shape.clone()));
    } else {
        ch.shape.merge_from(shape);
    }
    Ok(())
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
    ScalarMismatch(Scalar, Scalar),
    ShapeMismatch(DynShape, DynShape),
    InvalidSpec,
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
            TypeError::ScalarMismatch(expected, found) => {
                write!(f, "expected {expected:?} but found {found:?}")
            }
            TypeError::ShapeMismatch(expected, found) => {
                write!(f, "expected shape {expected:?} but found shape {found:?}")
            }
            TypeError::InvalidSpec => write!(f, "invalid type specification"),
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

fn value_as_scalar_spec(val: &Value) -> Option<Scalar> {
    match val {
        Value::Complex(arr)
            if arr.data.as_slice() == [Complex::I] || arr.data.as_slice() == [Complex::ONE] =>
        {
            Some(Scalar::Complex)
        }
        Value::Char(arr) if arr.rank() == 0 => Some(match arr.data[0] {
            '*' => Scalar::Any,
            'ℝ' => Scalar::Num,
            'ℤ' => Scalar::Int,
            'ℕ' => Scalar::Nat,
            '𝔹' => Scalar::Bool,
            '𝕌' => Scalar::Char,
            '@' => Scalar::Ascii,
            '□' => Scalar::Box(ScalarBox::Any),
            'ℂ' => Scalar::Complex,
            _ => return None,
        }),
        Value::Box(_) if val.rank() == 1 => {
            Some(Scalar::Box(ScalarBox::All(Type::from_spec(val)?.into())))
        }
        _ => None,
    }
}

fn num_as_dim(n: f64) -> Option<Dim> {
    if n == f64::INFINITY {
        Some(Dim::Dyn)
    } else if n >= 00.0 && n.fract() == 0.0 {
        Some(Dim::Static(n as usize))
    } else {
        None
    }
}

fn value_as_dim(val: &Value) -> Option<Dim> {
    match val {
        Value::Num(arr) if arr.rank() == 0 => num_as_dim(arr.data[0]),
        Value::Byte(arr) if arr.rank() == 0 => Some(Dim::Static(arr.data[0] as usize)),
        _ => None,
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
            if ty.scalar.superset_of(&ty.scalar) {
                tv.set_scalar(ty.scalar);
            }
            if tv.shape().is_any() {
                tv.set_shape(ty.shape);
            }
        }
        self.update_arg_types();
    }
    fn update_arg_types(&mut self) {
        if !self.can_set_arg_types {
            return;
        }

        fn match_types(arg_ty: &mut Type, stack_ty: Type) {
            match (&mut arg_ty.scalar, stack_ty.scalar) {
                (
                    Scalar::Box(ScalarBox::All(arg_inner)),
                    Scalar::Box(ScalarBox::All(stack_inner)),
                ) => match_types(arg_inner, *stack_inner),
                (
                    Scalar::Box(ScalarBox::Def(arg_name, arg_fields)),
                    Scalar::Box(ScalarBox::Def(stack_name, stack_fields)),
                ) => {
                    if arg_name.is_none() {
                        *arg_name = stack_name;
                    }
                    for (arg, stack) in arg_fields.iter_mut().zip(stack_fields) {
                        match_types(arg, stack);
                    }
                }
                (Scalar::Box(arg_sb @ ScalarBox::Any), Scalar::Box(stack_sb)) => *arg_sb = stack_sb,
                (_, stack_scalar) => {
                    if arg_ty.scalar.superset_of(&stack_scalar) {
                        arg_ty.scalar = stack_scalar;
                    }
                }
            }
            arg_ty.shape.merge_from(stack_ty.shape);
        }

        let arg_start = self.arg_types.len().saturating_sub(self.stack.len());
        let stack_start = self.stack.len().saturating_sub(self.arg_types.len());
        for (tv, arg) in (self.stack.iter().rev().skip(stack_start))
            .zip(self.arg_types.iter_mut().rev().skip(arg_start))
        {
            if let TypeVal::Type(arg_ty) = arg {
                if let TypeVal::Type(ty) = tv {
                    match_types(arg_ty, ty.clone());
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
                ImplPrim(Over | ValidateImpl(..), _) => true,
                ImplMod(DipN(_) | BothImpl(_), args, _) => {
                    args.iter().all(|sn| node_allows_more_arg_types(&sn.node))
                }
                Label(..) | RemoveLabel(..) => true,
                _ => false,
            }
        }

        self.can_set_arg_types = self.can_set_arg_types && node_allows_more_arg_types(node);
        res
    }
    fn node_impl(&mut self, node: &Node) -> TypeResult {
        use self::Type;
        use {crate::algorithm::pervade::*, ImplPrimitive::*, Node::*, Primitive::*};

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
                Cos => self.monadic_pervasive_hint(Scalar::Num, Scalar::cos, cos::num)?,
                Tan => self.monadic_pervasive_hint(Scalar::Num, Scalar::tan, tan::num)?,
                SinH => self.monadic_pervasive_hint(Scalar::Num, Scalar::sinh, sinh::num)?,
                CosH => self.monadic_pervasive_hint(Scalar::Num, Scalar::cosh, cosh::num)?,
                TanH => self.monadic_pervasive_hint(Scalar::Num, Scalar::tanh, tanh::num)?,
                Floor => self.monadic_pervasive_hint(Scalar::Num, Scalar::floor, floor::num)?,
                Ceil => self.monadic_pervasive_hint(Scalar::Num, Scalar::ceil, ceil::num)?,
                Round => self.monadic_pervasive_hint(Scalar::Num, Scalar::round, round::num)?,
                Add => self.dyadic_pervasive_hint2(
                    Scalar::Num,
                    Scalar::Int,
                    Scalar::add,
                    add::num_num,
                )?,
                Sub => self.dyadic_pervasive_hint2(
                    Scalar::Num,
                    Scalar::Char,
                    Scalar::sub,
                    sub::num_num,
                )?,
                Mul => self.dyadic_pervasive_hint2(
                    Scalar::Num,
                    Scalar::Int,
                    Scalar::mul,
                    mul::num_num,
                )?,
                Div => self.dyadic_pervasive_hint2(
                    Scalar::Num,
                    Scalar::Int,
                    Scalar::div,
                    div::num_num,
                )?,
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
                    |_| Ok(Scalar::Nat.scalar_type()),
                    |list| Ok(Scalar::Nat.shaped([list.len()])),
                    |_| None,
                )?,
                Fix => self.top_mut(1)?.prepend_dim(1.into()),
                Box => {
                    let x = self.pop(1)?;
                    self.push(x.boxed());
                }
                First => {
                    self.type_hint([Type::listy()]);
                    let x = self.pop(1)?;
                    self.push(x.into_first_row());
                }
                Last => {
                    self.type_hint([Type::listy()]);
                    let x = self.pop(1)?;
                    self.push(x.into_last_row());
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
                        if !Scalar::Int.superset_of(&ty.scalar) {
                            return Err(format!("Cannot create range from {}", ty.scalar).into());
                        }
                        let shape = ty.shape;
                        Ok(Scalar::Int.shaped(if shape.rank() > 1 {
                            return Err(format!(
                                "Cannot create range from array with shape {shape}"
                            )
                            .into());
                        } else if shape.suffix.is_some_and(|suf| !suf.is_empty()) {
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
                    |n| Ok(Type::new(Scalar::Int, n.abs() as usize)),
                    |list| {
                        let len = list.len();
                        Ok(Type::new(
                            Scalar::Int,
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
                        let dims = if let Some(suf) = sh.shape.suffix {
                            if !sh.shape.dims.is_empty() {
                                return Err(TypeError::Unsupported(None));
                            }
                            suf
                        } else {
                            sh.shape.dims
                        };
                        if dims.len() > 1 {
                            return Err(format!(
                                "{} must be rank 0 or 1, but it is rank {}",
                                Reshape.format(),
                                dims.len()
                            )
                            .into());
                        }
                        match *dims.as_slice() {
                            [] => ty.shape.dims.insert(0, Dim::Dyn),
                            [Dim::Dyn] => ty.shape = DynShape::ANY,
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
                                ty.shape = DynShape::ANY;
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
                            ty.scalar = Scalar::Nat;
                            if ty.shape.suffix.take().is_some() && ty.shape.dims.is_empty() {
                                ty.shape = DynShape::ANY;
                            } else {
                                ty.shape.dims.truncate(1);
                            }
                            Ok(ty)
                        },
                        |_| Ok(Value::from(0)),
                        |list| Ok(Type::new(Scalar::Nat, list.len())),
                        |val| Some(Value::from(val.rise())),
                    )?
                }
                Fall => {
                    self.type_hint([Type::listy()]);
                    self.monadic(
                        |mut ty| {
                            ty.scalar = Scalar::Nat;
                            if ty.shape.suffix.take().is_some() && ty.shape.dims.is_empty() {
                                ty.shape = DynShape::ANY;
                            } else {
                                ty.shape.dims.truncate(1);
                            }
                            Ok(ty)
                        },
                        |_| Ok(Value::from(0)),
                        |list| Ok(Type::new(Scalar::Nat, list.len())),
                        |val| Some(Value::from(val.fall())),
                    )?
                }
                Match => {
                    self.pop_n(2)?;
                    self.push(Scalar::Bool);
                }
                Select => {
                    self.type_hint([Scalar::Int.any_shape()]);
                    self.dyadic(
                        |indices, mut ty| {
                            if !Scalar::Num.superset_of(&indices.scalar) {
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
                                return Err(format!(
                                    "Index {index} is out of bounds of length {n}"
                                )
                                .into());
                            }
                            if let Scalar::Box(ScalarBox::Def(_, fields)) = &ty.scalar
                                && ty.shape.rank() == 1
                            {
                                return Ok(if index >= 0.0 {
                                    ty.into_nth_row(index as usize)
                                } else {
                                    let n = fields.len().saturating_sub(index.abs() as usize);
                                    ty.into_nth_row(n)
                                });
                            }
                            Ok(ty.into_row())
                        },
                        |indices, mut ty| {
                            if let Dim::Static(n) = ty.shape.row_count()
                                && let Some(i) = indices.iter().find(|&&i| {
                                    i >= 0.0 && i as usize >= n || i < 0.0 && i.abs() as usize > n
                                })
                            {
                                return Err(
                                    format!("Index {i} is out of bounds of length {n}").into()
                                );
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
                    )?
                }
                Pick => {
                    self.type_hint([Scalar::Int.any_shape()]);
                    self.dyadic(
                        |_, mut ty| {
                            ty.shape = DynShape::ANY;
                            Ok(ty)
                        },
                        |index, ty| {
                            if let Dim::Static(n) = ty.shape.row_count()
                                && (index >= 0.0 && index as usize >= n
                                    || index < 0.0 && index.abs() as usize > n)
                            {
                                return Err(format!(
                                    "Index {index} is out of bounds of length {n}"
                                )
                                .into());
                            }
                            if let Scalar::Box(ScalarBox::Def(_, fields)) = &ty.scalar
                                && ty.shape.rank() == 1
                            {
                                return Ok(if index >= 0.0 {
                                    ty.into_nth_row(index as usize)
                                } else {
                                    let n = fields.len().saturating_sub(index.abs() as usize);
                                    ty.into_nth_row(n)
                                });
                            }
                            Ok(ty.into_row())
                        },
                        |_, mut ty| {
                            ty.shape = DynShape::ANY;
                            Ok(ty)
                        },
                        |index, n| {
                            if index != 0.0 && index != 1.0 {
                                Err(format!("Index {index} is out of bounds of length 1").into())
                            } else {
                                Ok(n)
                            }
                        },
                    )?
                }
                Keep => {
                    self.type_hint([Scalar::Num.any_shape()]);
                    if let Ok(mask) = self.top(1)
                        && mask.shape().is_any()
                    {
                        self.type_hint([Scalar::Int.shaped([Dim::Dyn])]);
                    }
                    self.dyadic(
                        |mask, mut ty| {
                            if mask.shape.rank() > 1 {
                                return Err(format!(
                                    "Cannot {} with array of shape {}",
                                    Keep.format(),
                                    mask.shape
                                )
                                .into());
                            }
                            match ty.shape.dims.first_mut() {
                                Some(d) => *d = Dim::Dyn,
                                None if ty.shape.suffix.is_none() => {
                                    ty.shape.dims.insert(0, Dim::Dyn)
                                }
                                None => {}
                            }
                            Ok(ty)
                        },
                        |n, mut ty| {
                            if n.is_infinite() {
                                return Err(
                                    format!("Cannot {} an infinite amount", Keep.format()).into()
                                );
                            }
                            match ty.shape.dims.first_mut() {
                                Some(d @ Dim::Dyn) if n == 0.0 => *d = Dim::Static(0),
                                Some(Dim::Dyn) => {}
                                Some(Dim::Static(d)) => *d = (n.abs() * *d as f64).round() as usize,
                                None if ty.shape.suffix.is_none() => {
                                    ty.shape.dims.insert(0, Dim::Static(n.abs() as usize))
                                }
                                None => {}
                            }
                            Ok(ty)
                        },
                        |list, mut ty| {
                            match ty.shape.dims.first_mut() {
                                Some(d) if list.is_empty() => *d = Dim::Static(0),
                                Some(d) => *d = Dim::Dyn,
                                None if ty.shape.suffix.is_none() => {
                                    ty.shape.dims.insert(0, Dim::Dyn)
                                }
                                None => {}
                            }
                            Ok(ty)
                        },
                        |n, f| {
                            if n.is_infinite() {
                                return Err(
                                    format!("Cannot {} an infinite amount", Keep.format()).into()
                                );
                            }
                            let n = n.abs() as usize;
                            Ok(if n > 10 {
                                Scalar::from([f].as_slice()).shaped(n).into()
                            } else {
                                TypeVal::NumList(eco_vec![f;n])
                            })
                        },
                    )?;
                }
                Take => {
                    self.type_hint([Scalar::Int.any_shape()]);
                    let has_fill = self.second_filled();
                    self.dyadic(
                        |amnt, mut ty| {
                            if !Scalar::Num.superset_of(&amnt.scalar) {
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
                                let n = n.abs() as usize;
                                if !has_fill
                                    && let Dim::Static(r) = ty.shape.row_count()
                                    && n > r
                                {
                                    return Err(format!(
                                        "Cannot {} {n} rows from an array with \
                                            shape {} outside a fill context",
                                        Take.format(),
                                        ty.shape
                                    )
                                    .into());
                                }
                                ty = ty.into_row();
                                ty.shape.dims.insert(0, Dim::Static(n));
                            }
                            Ok(ty)
                        },
                        |list, mut ty| {
                            if list.iter().any(|n| n.is_infinite()) {
                                ty.shape = DynShape::ANY;
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
                            Scalar::Box(sb) => {
                                if let Some(ty) = sb.into_inner() {
                                    let ty = parse(ty)?;
                                    if ty.shape.suffix.is_some() {
                                        shape.suffix = Some(Vec::new());
                                    } else {
                                        shape.dims.extend(ty.shape.dims);
                                    }
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
                // - drop
                // - rotate, where
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
                ASin => self.monadic_pervasive_hint(Scalar::Num, Scalar::asin, asin::num)?,
                ACos => self.monadic_pervasive_hint(Scalar::Num, Scalar::acos, acos::num)?,
                ATan => self.monadic_pervasive_hint(Scalar::Num, Scalar::atan, atan::num)?,
                ASinH => self.monadic_pervasive_hint(Scalar::Num, Scalar::asinh, asinh::num)?,
                ACosH => self.monadic_pervasive_hint(Scalar::Num, Scalar::acosh, acosh::num)?,
                ATanH => self.monadic_pervasive_hint(Scalar::Num, Scalar::atanh, atanh::num)?,
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
                    self.type_hint([Scalar::Box(ScalarBox::Any).any_shape()]);
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
                                ty.shape = DynShape::ANY;
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
                Dual => self.monadic_pervasive_hint(Scalar::Complex, Scalar::dual, dual::com)?,
                UnDual => {
                    self.monadic_pervasive_hint(Scalar::Complex, Scalar::undual, undual::com)?
                }
                Conj => self.monadic_pervasive_hint(Scalar::Complex, Scalar::conj, conj::com)?,
                NegConj => {
                    self.monadic_pervasive_hint(Scalar::Complex, Scalar::negconj, negconj::com)?
                }
                MvImpl(mode) => {
                    self.type_hint([Scalar::Num.into()]);
                    let x = self.pop(1)?;
                    if !Scalar::Num.superset_of(&x.scalar()) {
                        return Err(
                            format!("Cannot make multivector from {} array", x.scalar()).into()
                        );
                    }
                    let mut ty = x.ty();
                    let n = (ty.shape.suffix.as_ref())
                        .and_then(|s| s.last())
                        .or_else(|| ty.shape.dims.last())
                        .copied();
                    ty.scalar = if mode.side == Some(SubSide::Left)
                        && (mode.dims == Some(2)
                            || mode.dims.is_none() && n == Some(Dim::Static(2)))
                    {
                        Scalar::Complex
                    } else {
                        Scalar::Multivector
                    };
                    if let Some(suf) = &mut ty.shape.suffix {
                        suf.pop();
                    } else {
                        ty.shape.dims.pop();
                    }
                    self.push(ty);
                }
                &ValidateImpl(side) => {
                    let spec = self.pop(1)?.spec().ok_or(TypeError::InvalidSpec)?;
                    let val = self.top_mut("validated value")?;
                    let mut ch = val.clone().ty();
                    validate(spec, &mut ch, side)?;
                    val.set_scalar(ch.scalar);
                    val.set_shape(ch.shape);
                    self.update_arg_types();
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
                    self.type_hint([Type::new(Scalar::Num, DynShape::ANY)]);
                    let markers = self.pop(1)?;
                    if markers.rank() > 1 {
                        return Err(TypeError::Unsupported(None));
                    }
                    if !Scalar::Num.superset_of(&markers.scalar()) {
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
                    shape = DynShape::ANY;
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
                    shape = DynShape::ANY;
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
    fn monadic_pervasive_hint<N: Into<TypeVal>>(
        &mut self,
        hint: Scalar,
        f: impl Fn(Scalar) -> Result<Scalar, String>,
        f64: impl Fn(f64) -> N,
    ) -> TypeResult {
        self.type_hint([hint.any_shape()]);
        self.monadic_pervasive(f, f64)
    }
    fn monadic_pervasive<N: Into<TypeVal>>(
        &mut self,
        f: impl Fn(Scalar) -> Result<Scalar, String>,
        f64: impl Fn(f64) -> N,
    ) -> TypeResult {
        self.monadic(
            |mut ty| {
                Ok(Type {
                    scalar: f(take(&mut ty.scalar))?,
                    ..ty
                })
            },
            |n| Ok(f64(n).into()),
            |mut ns| {
                if ns.is_empty() {
                    return Ok(TypeVal::NumList(ns));
                }
                Ok(match f64(ns[0]).into() {
                    TypeVal::Num(_) => {
                        for n in ns.make_mut() {
                            *n = if let TypeVal::Num(n) = f64(*n).into() {
                                n
                            } else {
                                unreachable!()
                            };
                        }
                        TypeVal::NumList(ns)
                    }
                    mut tv => {
                        tv.prepend_dim(Dim::Static(ns.len()));
                        tv
                    }
                })
            },
            |_| None,
        )
    }
    fn dyadic_pervasive_hint2<N: Into<TypeVal>>(
        &mut self,
        num_hint: Scalar,
        char_hint: Scalar,
        f: impl Fn(Scalar, Scalar, bool, bool) -> Result<Scalar, TypeError>,
        f64: impl Fn(f64, f64) -> N,
    ) -> TypeResult {
        if let Ok(a) = self.pop(1) {
            if a.is_any() {
                self.push(a);
                let num_hint = num_hint.any_shape();
                self.type_hint([num_hint.clone(), num_hint]);
            } else {
                self.type_hint([if Scalar::Char.superset_of(&a.scalar()) {
                    char_hint
                } else {
                    num_hint
                }
                .any_shape()]);
                self.push(a);
            }
        }
        self.dyadic_pervasive(f, f64)
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
            .any(|f| f.scalar().superset_of(&ty.scalar))
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
                let ty = Type::of_val(&v);
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
    fn second_filled(&self) -> bool {
        self.stack.len() >= 2 && self.fill_for(&self.stack[self.stack.len() - 2].clone().ty())
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
                    b.set_scalar(Scalar::Box(
                        b.scalar()
                            .maybe_scalar_type()
                            .map(Box::new)
                            .map(ScalarBox::All)
                            .unwrap_or_default(),
                    ));
                } else if !a.scalar().is_box() && b.scalar().is_box() {
                    a.set_scalar(Scalar::Box(
                        a.scalar()
                            .maybe_scalar_type()
                            .map(Box::new)
                            .map(ScalarBox::All)
                            .unwrap_or_default(),
                    ));
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
        self.type_hint([if unbox {
            DynShape::from(Dim::Static(n)).with_scalar(Scalar::Box(ScalarBox::Any))
        } else {
            DynShape::prefix([Dim::Static(n)]).with_scalar(Scalar::Any)
        }]);
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
    let mut shape = DynShape::SCALAR;
    for i in 0..ash.dims.len().max(bsh.dims.len()) {
        let new_dim = match (ash.dims.get(i).copied(), bsh.dims.get(i).copied()) {
            (None, None) => unreachable!(),
            (None, Some(Dim::Static(1))) | (Some(Dim::Static(1)), None) => Dim::Dyn,
            (Some(d), Some(Dim::Static(1)) | None) | (Some(Dim::Static(1)) | None, Some(d)) => d,
            (Some(d), Some(Dim::Dyn)) | (Some(Dim::Dyn), Some(d)) => d,
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
