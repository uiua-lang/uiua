use std::{array, cmp::Ordering, mem::take};

use crate::{
    cowslice::CowSlice, Array, Assembly, Boxed, Complex, ImplPrimitive, Node, PersistentMeta,
    Primitive, Shape, SigNode, Uiua, Value,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum ScalarType {
    Real,
    Complex,
    Char,
    Box(Option<Box<Ty>>),
}

impl Value {
    fn scalar_ty(&self) -> ScalarType {
        match self {
            Value::Num(_) => ScalarType::Real,
            Value::Byte(_) => ScalarType::Real,
            Value::Complex(_) => ScalarType::Complex,
            Value::Char(_) => ScalarType::Char,
            Value::Box(arr) => ScalarType::Box(if arr.data.is_empty() {
                None
            } else if (arr.data.windows(2)).all(|w| w[0].0.row_ty() == w[1].0.row_ty()) {
                Some(Box::new(arr.data[0].0.ty()))
            } else {
                None
            }),
        }
    }
    fn ty(&self) -> Ty {
        Ty {
            scalar: self.scalar_ty(),
            shape: self.shape.clone(),
            int: match self {
                Value::Num(n) if self.rank() == 0 && n.data[0].fract() == 0.0 => {
                    Some(n.data[0] as isize)
                }
                Value::Byte(n) if self.rank() == 0 => Some(n.data[0] as isize),
                _ => None,
            },
        }
    }
    fn row_ty(&self) -> Ty {
        Ty {
            scalar: self.scalar_ty(),
            shape: self.shape.row(),
            int: match self {
                Value::Num(n) if self.rank() == 0 && n.data[0].fract() == 0.0 => {
                    Some(n.data[0] as isize)
                }
                Value::Byte(n) if self.rank() == 0 => Some(n.data[0] as isize),
                _ => None,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Ty {
    scalar: ScalarType,
    shape: Shape,
    int: Option<isize>,
}

impl Ty {
    fn new(scalar: ScalarType, shape: impl Into<Shape>) -> Self {
        Self {
            scalar,
            shape: shape.into(),
            int: None,
        }
    }
    fn unboxed(self) -> Ty {
        if let ScalarType::Box(ty) = self.scalar {
            if let Some(ty) = ty {
                *ty
            } else {
                Ty::new(ScalarType::Real, [0])
            }
        } else {
            self
        }
    }
}

enum TypeError {
    StackUnderflow,
    NotSupported,
    Other,
}

fn make_val(mut ty: Ty) -> Value {
    ty.shape.prepend(0);
    match ty.scalar {
        ScalarType::Real => Array::<u8>::new(ty.shape, CowSlice::default()).into(),
        ScalarType::Complex => Array::<Complex>::new(ty.shape, CowSlice::default()).into(),
        ScalarType::Char => Array::<char>::new(ty.shape, CowSlice::default()).into(),
        ScalarType::Box(_) => Array::<Boxed>::new(ty.shape, CowSlice::default()).into(),
    }
}

pub(crate) fn push_empty_rows_value<'a, I>(
    f: &SigNode,
    args: I,
    inventory: bool,
    per_meta: &mut PersistentMeta,
    env: &mut Uiua,
) -> bool
where
    I: IntoIterator<Item = &'a Value>,
    I::IntoIter: DoubleEndedIterator,
{
    if inventory {
        let per_meta = take(per_meta);
        for _ in 0..f.sig.outputs().saturating_sub(1) {
            let mut arr = Array::<Boxed>::new([0], CowSlice::default());
            arr.meta.set_per_meta(per_meta.clone());
            env.push(arr);
        }
        if f.sig.outputs() > 0 {
            let mut arr = Array::<Boxed>::new([0], CowSlice::default());
            arr.meta.set_per_meta(per_meta);
            env.push(arr);
        }
        return true;
    }
    let mut stack = Vec::new();
    for arg in args.into_iter().rev() {
        stack.push(arg.row_ty())
    }
    let mut rt = TypeRt {
        stack,
        under_stack: Vec::new(),
        asm: &env.asm,
    };
    match rt.node(&f.node) {
        Ok(()) => {
            let per_meta = take(per_meta);
            let count = rt.stack.len();
            let mut tys = rt.stack.into_iter().rev();
            for ty in tys.by_ref().take(count.saturating_sub(1)) {
                let mut val = make_val(ty);
                val.meta.set_per_meta(per_meta.clone());
                env.push(val);
            }
            if let Some(ty) = tys.next() {
                let mut val = make_val(ty);
                val.meta.set_per_meta(per_meta);
                env.push(val);
            }
            true
        }
        Err(_) => false,
    }
}

struct TypeRt<'a> {
    stack: Vec<Ty>,
    under_stack: Vec<Ty>,
    asm: &'a Assembly,
}

impl TypeRt<'_> {
    #[allow(clippy::collapsible_match)]
    fn node(&mut self, node: &Node) -> Result<(), TypeError> {
        use Primitive::*;
        match node {
            Node::Run(nodes) => nodes.iter().try_for_each(|node| self.node(node))?,
            Node::Push(val) => self.stack.push(val.row_ty()),
            Node::Call(f, _) => self.node(&self.asm[f])?,
            Node::Prim(prim, _) => match prim {
                Dup => {
                    let val = self.pop()?;
                    self.stack.push(val.clone());
                    self.stack.push(val);
                }
                Flip => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Not | Sign | Neg | Abs | Sqrt | Floor | Ceil | Round => {
                    let x = self.pop()?;
                    self.stack.push(x);
                }
                Add | Sub | Mul | Div | Pow | Modulo | Log => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let shape = if a.shape.len() > b.shape.len() {
                        a.shape
                    } else {
                        b.shape
                    };
                    let scalar = a.scalar.max(b.scalar);
                    self.stack.push(Ty::new(scalar, shape));
                }
                Couple => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let scalar = a.scalar.max(b.scalar);
                    let mut shape = if a.shape.len() > b.shape.len() {
                        a.shape
                    } else {
                        b.shape
                    };
                    shape.prepend(2);
                    self.stack.push(Ty::new(scalar, shape));
                }
                Join => {
                    let mut a = self.pop()?;
                    let mut b = self.pop()?;
                    let scalar = a.scalar.max(b.scalar);
                    let shape = match a.shape.len().cmp(&b.shape.len()) {
                        Ordering::Equal => {
                            *a.shape.row_count_mut() += b.shape.row_count();
                            a.shape
                        }
                        Ordering::Less => {
                            *b.shape.row_count_mut() += 1;
                            b.shape
                        }
                        Ordering::Greater => {
                            *a.shape.row_count_mut() += 1;
                            a.shape
                        }
                    };
                    self.stack.push(Ty::new(scalar, shape));
                }
                Match | Has => {
                    self.pop()?;
                    self.pop()?;
                    self.stack.push(Ty::new(ScalarType::Real, []));
                }
                Get => {
                    let _key = self.pop()?;
                    let val = self.pop()?;
                    self.stack.push(val);
                }
                Parse => {
                    let x = self.pop()?;
                    let mut shape = x.shape;
                    if !matches!(x.scalar, ScalarType::Box(_)) {
                        shape.pop();
                    }
                    self.stack.push(Ty::new(ScalarType::Real, shape));
                }
                Box => {
                    let x = self.pop()?;
                    let boxed = Ty::new(ScalarType::Box(Some(x.into())), []);
                    self.stack.push(boxed);
                }
                First | Last => {
                    let mut x = self.pop()?;
                    x.shape.make_row();
                    self.stack.push(x);
                }
                Identity => {}
                // Select => {
                //     let index = self.pop()?;
                //     let from = self.pop()?;
                //     let mut shape = index.shape.clone();
                //     shape.extend(from.shape.iter().copied().skip(1));
                //     self.stack.push(Ty::new(from.scalar, shape));
                // }
                // Pick => {
                //     let index = self.pop()?;
                //     let mut from = self.pop()?;
                //     if let Some((last, outer)) = index.shape.split_last() {
                //         let shape = Shape::from_iter(
                //             (outer.iter().copied()).chain(from.shape.iter().copied().skip(*last)),
                //         );
                //         self.stack.push(Ty::new(from.scalar, shape));
                //     } else {
                //         if !from.shape.is_empty() {
                //             from.shape.remove(0);
                //         }
                //         self.stack.push(from);
                //     }
                // }
                Take => {
                    let n = self.pop()?;
                    let mut x = self.pop()?;
                    if let Some(n) = n.int {
                        *x.shape.row_count_mut() = n.unsigned_abs();
                        self.stack.push(x);
                    } else {
                        return Err(TypeError::NotSupported);
                    }
                }
                Drop => {
                    let n = self.pop()?;
                    let mut x = self.pop()?;
                    if let Some(n) = n.int {
                        let len = x.shape.row_count_mut();
                        *len = len.saturating_sub(n.unsigned_abs());
                    } else {
                        return Err(TypeError::NotSupported);
                    }
                }
                prim if prim.outputs() == Some(0) => {
                    if let Some(args) = prim.args() {
                        for _ in 0..args {
                            self.pop()?;
                        }
                    }
                }
                _ => return Err(TypeError::NotSupported),
            },
            Node::ImplPrim(prim, _) => match prim {
                ImplPrimitive::UnBox => {
                    let x = self.pop()?;
                    self.stack.push(x.unboxed());
                }
                ImplPrimitive::Over => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b.clone());
                    self.stack.push(a);
                    self.stack.push(b);
                }
                _ => return Err(TypeError::NotSupported),
            },
            Node::Mod(prim, args, _) => match prim {
                Dip => {
                    let [f] = get_args(args)?;
                    let x = self.pop()?;
                    self.node(&f.node)?;
                    self.stack.push(x);
                }
                _ => return Err(TypeError::NotSupported),
            },
            Node::NoInline(inner) | Node::TrackCaller(inner) => self.node(inner)?,
            &Node::PushUnder(n, _) => {
                for _ in 0..n {
                    let value = self.pop()?;
                    self.under_stack.push(value);
                }
            }
            &Node::CopyToUnder(n, _) => {
                for _ in 0..n {
                    let value = self.pop()?;
                    self.under_stack.push(value);
                }
                for ty in self.under_stack.iter().rev().take(n) {
                    self.stack.push(ty.clone());
                }
            }
            &Node::PopUnder(n, _) => {
                for _ in 0..n {
                    let value = self.pop_under()?;
                    self.stack.push(value);
                }
            }
            &Node::Unpack { count, unbox, .. } => {
                let mut x = self.pop()?;
                x.shape.make_row();
                if unbox {
                    x = x.unboxed();
                }
                for _ in 0..count {
                    self.stack.push(x.clone());
                }
            }
            _ => return Err(TypeError::NotSupported),
        }
        Ok(())
    }
    fn pop(&mut self) -> Result<Ty, TypeError> {
        self.stack.pop().ok_or(TypeError::StackUnderflow)
    }
    fn pop_under(&mut self) -> Result<Ty, TypeError> {
        self.under_stack.pop().ok_or(TypeError::StackUnderflow)
    }
}

fn get_args<const N: usize>(args: &[SigNode]) -> Result<[&SigNode; N], TypeError> {
    if args.len() != N {
        return Err(TypeError::Other);
    }
    Ok(array::from_fn(|i| &args[i]))
}
