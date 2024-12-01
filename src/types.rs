use std::{cmp::Ordering, mem::take};

use crate::{
    cowslice::CowSlice, Array, Assembly, Boxed, Complex, ImplPrimitive, Node, PersistentMeta,
    Primitive, Shape, SigNode, Uiua, Value,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum ScalarType {
    Real,
    Complex,
    Char,
    Box(Option<Box<Type>>),
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
    fn ty(&self) -> Type {
        Type {
            scalar: self.scalar_ty(),
            shape: self.shape().clone(),
        }
    }
    fn row_ty(&self) -> Type {
        Type {
            scalar: self.scalar_ty(),
            shape: self.shape().row(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Type {
    scalar: ScalarType,
    shape: Shape,
}

impl Type {
    fn new(scalar: ScalarType, shape: impl Into<Shape>) -> Self {
        Self {
            scalar,
            shape: shape.into(),
        }
    }
}

enum TypeError {
    StackUnderflow,
    NotSupported,
}

fn make_val(mut ty: Type) -> Value {
    ty.shape.insert(0, 0);
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
        for _ in 0..f.sig.outputs.saturating_sub(1) {
            let mut arr = Array::<Boxed>::new([0], CowSlice::default());
            arr.set_per_meta(per_meta.clone());
            env.push(arr);
        }
        if f.sig.outputs > 0 {
            let mut arr = Array::<Boxed>::new([0], CowSlice::default());
            arr.set_per_meta(per_meta);
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
        _under_stack: Vec::new(),
        array_stack: Vec::new(),
        asm: &env.asm,
    };
    match rt.node(&f.node) {
        Ok(()) => {
            let per_meta = take(per_meta);
            let count = rt.stack.len();
            let mut tys = rt.stack.into_iter().rev();
            for ty in tys.by_ref().take(count.saturating_sub(1)) {
                let mut val = make_val(ty);
                val.set_per_meta(per_meta.clone());
                env.push(val);
            }
            if let Some(ty) = tys.next() {
                let mut val = make_val(ty);
                val.set_per_meta(per_meta);
                env.push(val);
            }
            true
        }
        Err(_) => false,
    }
}

struct TypeRt<'a> {
    stack: Vec<Type>,
    _under_stack: Vec<Type>,
    array_stack: Vec<usize>,
    asm: &'a Assembly,
}

impl<'a> TypeRt<'a> {
    #[allow(clippy::collapsible_match)]
    fn node(&mut self, node: &Node) -> Result<(), TypeError> {
        use Primitive as P;
        match node {
            Node::Push(val) => self.stack.push(val.row_ty()),
            Node::Call(f, _) => self.node(&self.asm[f])?,
            Node::Prim(prim, _) => match prim {
                P::Dup => {
                    let val = self.pop()?;
                    self.stack.push(val.clone());
                    self.stack.push(val);
                }
                P::Flip => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(a);
                    self.stack.push(b);
                }
                P::Over => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b.clone());
                    self.stack.push(a);
                    self.stack.push(b);
                }
                P::Around => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(a.clone());
                    self.stack.push(b);
                    self.stack.push(a);
                }
                P::Not | P::Sign | P::Neg | P::Abs | P::Sqrt | P::Floor | P::Ceil | P::Round => {
                    let x = self.pop()?;
                    self.stack.push(x);
                }
                P::Add | P::Sub | P::Mul | P::Div | P::Pow | P::Modulus | P::Log => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let shape = if a.shape.len() > b.shape.len() {
                        a.shape
                    } else {
                        b.shape
                    };
                    let scalar = a.scalar.max(b.scalar);
                    self.stack.push(Type::new(scalar, shape));
                }
                P::Couple => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let scalar = a.scalar.max(b.scalar);
                    let mut shape = if a.shape.len() > b.shape.len() {
                        a.shape
                    } else {
                        b.shape
                    };
                    shape.insert(0, 2);
                    self.stack.push(Type::new(scalar, shape));
                }
                P::Join => {
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
                    self.stack.push(Type::new(scalar, shape));
                }
                P::Match | P::Has => {
                    self.pop()?;
                    self.pop()?;
                    self.stack.push(Type::new(ScalarType::Real, []));
                }
                P::Get => {
                    let _key = self.pop()?;
                    let val = self.pop()?;
                    self.stack.push(val);
                }
                P::Parse => {
                    let x = self.pop()?;
                    let mut shape = x.shape;
                    if !matches!(x.scalar, ScalarType::Box(_)) {
                        shape.pop();
                    }
                    self.stack.push(Type::new(ScalarType::Real, shape));
                }
                P::Box => {
                    let x = self.pop()?;
                    let boxed = Type::new(ScalarType::Box(Some(x.into())), []);
                    self.stack.push(boxed);
                }
                P::Identity => {}
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
                    if x.shape.len() == 0 {
                        if let ScalarType::Box(ty) = x.scalar {
                            self.stack.push(if let Some(ty) = ty {
                                *ty
                            } else {
                                Type::new(ScalarType::Real, [0])
                            });
                            return Ok(());
                        }
                    }
                    self.stack.push(x);
                }
                _ => return Err(TypeError::NotSupported),
            },
            Node::NoInline(inner) | Node::TrackCaller(inner) => self.node(inner)?,
            _ => return Err(TypeError::NotSupported),
        }
        Ok(())
    }
    fn pop(&mut self) -> Result<Type, TypeError> {
        let ty = self.stack.pop().ok_or(TypeError::StackUnderflow)?;
        for height in &mut self.array_stack {
            *height = (*height).min(self.stack.len());
        }
        Ok(ty)
    }
}
