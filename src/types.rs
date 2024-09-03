use std::{cmp::Ordering, mem::take};

use enum_iterator::Sequence;

use crate::{
    cowslice::CowSlice, Array, Assembly, Boxed, Complex, Function, ImplPrimitive, Instr,
    PersistentMeta, Primitive, Shape, TempStack, Uiua, Value,
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
            #[cfg(feature = "bytes")]
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
    FunctionStackUnderflow,
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
    f: &Function,
    args: I,
    inv: bool,
    per_meta: &mut PersistentMeta,
    env: &mut Uiua,
) -> bool
where
    I: IntoIterator<Item = &'a Value>,
    I::IntoIter: DoubleEndedIterator,
{
    if inv {
        let per_meta = take(per_meta);
        for _ in 0..f.signature().outputs.saturating_sub(1) {
            let mut arr = Array::<Boxed>::new([0], CowSlice::default());
            arr.set_per_meta(per_meta.clone());
            env.push(arr);
        }
        if f.signature().outputs > 0 {
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
        temp_stacks: Default::default(),
        array_stack: Vec::new(),
        function_stack: Vec::new(),
    };
    match rt.instrs(f.instrs(&env.asm), &env.asm) {
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
    temp_stacks: [Vec<Type>; TempStack::CARDINALITY],
    array_stack: Vec<usize>,
    function_stack: Vec<&'a Function>,
}

impl<'a> TypeRt<'a> {
    fn instrs(&mut self, instrs: &'a [Instr], asm: &'a Assembly) -> Result<(), TypeError> {
        for instr in instrs {
            self.instr(instr, asm)?;
        }
        Ok(())
    }
    #[allow(clippy::collapsible_match)]
    fn instr(&mut self, instr: &'a Instr, asm: &'a Assembly) -> Result<(), TypeError> {
        use Primitive as P;
        match instr {
            Instr::Push(val) => self.stack.push(val.row_ty()),
            Instr::PushFunc(f) => self.function_stack.push(f),
            Instr::Call(_) => {
                let f = self.pop_func()?;
                self.instrs(f.instrs(asm), asm)?;
            }
            Instr::PushTemp { stack, count, .. } => {
                for _ in 0..*count {
                    let val = self.pop()?;
                    self.temp_stacks[*stack as usize].push(val);
                }
            }
            Instr::CopyToTemp { stack, count, .. } => {
                let mut vals = Vec::with_capacity(*count);
                for _ in 0..*count {
                    vals.push(self.pop()?);
                }
                for val in vals {
                    self.temp_stacks[*stack as usize].push(val.clone());
                    self.stack.push(val);
                }
            }
            Instr::PopTemp { stack, count, .. } => {
                for _ in 0..*count {
                    self.stack.push(
                        self.temp_stacks[*stack as usize]
                            .pop()
                            .ok_or(TypeError::StackUnderflow)?,
                    );
                }
            }
            Instr::Prim(prim, _) => match prim {
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
                    self.stack.push(a.clone());
                    self.stack.push(b);
                    self.stack.push(a);
                }
                P::Not | P::Sign | P::Neg | P::Abs | P::Sqrt | P::Floor | P::Ceil | P::Round => {
                    let x = self.pop()?;
                    self.stack.push(x);
                }
                P::Add | P::Sub | P::Mul | P::Div | P::Pow | P::Mod | P::Log => {
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
                prim if prim.outputs() == Some(0) => {
                    if let Some(args) = prim.args() {
                        for _ in 0..args {
                            self.pop()?;
                        }
                    }
                }
                _ => return Err(TypeError::NotSupported),
            },
            Instr::ImplPrim(prim, _) => match prim {
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
    fn pop_func(&mut self) -> Result<&'a Function, TypeError> {
        self.function_stack
            .pop()
            .ok_or(TypeError::FunctionStackUnderflow)
    }
}
