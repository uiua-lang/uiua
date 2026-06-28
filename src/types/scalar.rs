use super::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub enum ScalarBox {
    #[default]
    Any,
    All(Box<Type>),
    Def(Option<EcoString>, Vec<Type>),
}
impl ScalarBox {
    pub fn into_inner(self) -> Option<Type> {
        match self {
            ScalarBox::Any => None,
            ScalarBox::All(ty) => Some(*ty),
            ScalarBox::Def(_, items) => items.into_iter().next(),
        }
    }
    pub fn as_first(&self) -> Option<&Type> {
        match self {
            ScalarBox::Any => None,
            ScalarBox::All(ty) => Some(ty),
            ScalarBox::Def(_, items) => items.iter().next(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub enum Scalar {
    Bool,
    Nat,
    Int,
    Num,
    Complex,
    Multivector,
    Ascii,
    Char,
    Stream,
    Box(ScalarBox),
    #[default]
    Any,
}
impl Scalar {
    pub fn is_any(&self) -> bool {
        matches!(self, Scalar::Any)
    }
    pub fn is_box(&self) -> bool {
        matches!(self, Scalar::Box(_))
    }
    pub fn scalar_type(self) -> Type {
        self.shaped(DynShape::SCALAR)
    }
    pub fn any_shape(self) -> Type {
        self.shaped(DynShape::ANY)
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
    pub fn superset_of(&self, sub: &Self) -> bool {
        match (self, sub) {
            (Scalar::Any, _) => true,
            #[cfg(feature = "ga")]
            (
                Scalar::Multivector,
                Scalar::Complex | Scalar::Num | Scalar::Int | Scalar::Nat | Scalar::Bool,
            ) => true,
            (Scalar::Complex, Scalar::Num | Scalar::Int | Scalar::Nat | Scalar::Bool) => true,
            (Scalar::Num, Scalar::Int | Scalar::Nat | Scalar::Bool) => true,
            (Scalar::Int, Scalar::Nat | Scalar::Bool) => true,
            (Scalar::Nat, Scalar::Bool) => true,
            (Scalar::Char, Scalar::Ascii) => true,
            (a, b) => discriminant(a) == discriminant(b),
        }
    }
    pub fn compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Scalar::Any, _)
            | (_, Scalar::Any)
            | (Scalar::Box(_), Scalar::Box(_))
            | (Scalar::Stream | Scalar::Box(_), Scalar::Stream | Scalar::Box(_)) => true,
            _ => self.superset_of(other) || other.superset_of(self),
        }
    }
    pub fn compatible_with_boxes(&self, other: &Self) -> bool {
        match (self, other) {
            (Scalar::Box(ScalarBox::All(a)), Scalar::Box(ScalarBox::All(b))) => {
                a.scalar.compatible_with_boxes(&b.scalar) && a.shape.compatible_with(&b.shape)
            }
            _ => self.compatible_with(other),
        }
    }
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Scalar::Box(ScalarBox::All(a)), Scalar::Box(ScalarBox::All(b))) => {
                if a.scalar.compatible_with_boxes(&b.scalar) && a.shape.compatible_with(&b.shape) {
                    Scalar::Box(ScalarBox::All(a.max(b)))
                } else {
                    Scalar::Box(ScalarBox::Any)
                }
            }
            (a, b) if a == b => a,
            (a, b) if a.superset_of(&b) => a,
            (a, b) if b.superset_of(&a) => b,
            _ => Scalar::Any,
        }
    }
    pub fn unrefine(&mut self) {
        match self {
            Scalar::Int | Scalar::Nat | Scalar::Bool => *self = Scalar::Num,
            Scalar::Ascii => *self = Scalar::Char,
            Scalar::Box(ScalarBox::All(ty)) => ty.scalar.unrefine(),
            Scalar::Box(ScalarBox::Def(_, fields)) => {
                fields.iter_mut().for_each(|t| t.scalar.unrefine())
            }
            _ => {}
        }
    }
    pub fn of_val(val: &Value) -> Self {
        match val {
            Value::Num(arr) => arr.data.as_slice().into(),
            Value::Byte(arr) => {
                for &n in &arr.data {
                    if n > 1 {
                        return Scalar::Nat;
                    }
                }
                Scalar::Bool
            }
            Value::Char(arr) => {
                for c in &arr.data {
                    if !c.is_ascii() {
                        return Scalar::Char;
                    }
                }
                Scalar::Ascii
            }
            Value::Complex(_) => Scalar::Complex,
            Value::Box(arr) => Scalar::Box(
                (arr.data.iter())
                    .map(|Boxed(v)| Type::of_val(v))
                    .reduce(BitOr::bitor)
                    .map(Box::new)
                    .map_or(ScalarBox::Any, ScalarBox::All),
            ),
            #[cfg(feature = "ga")]
            Value::Mv(_) => Scalar::Multivector,
        }
    }
}

impl From<&[f64]> for Scalar {
    fn from(nums: &[f64]) -> Self {
        let mut max = Scalar::Bool;
        for &n in nums {
            if n.fract() != 0.0 {
                return Scalar::Num;
            }
            if n < 0.0 {
                max = cmp::max(max, Scalar::Int);
            }
            if n > 1.0 {
                max = cmp::max(max, Scalar::Nat);
            }
        }
        max
    }
}

impl fmt::Debug for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scalar::Box(ScalarBox::Def(..)) => write!(f, "{self}"),
            _ => write!(f, "array of {self}"),
        }
    }
}

impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scalar::Any => write!(f, "*"),
            Scalar::Num => write!(f, "ℝ"),
            Scalar::Int => write!(f, "ℤ"),
            Scalar::Nat => write!(f, "ℕ"),
            Scalar::Bool => write!(f, "𝔹"),
            Scalar::Ascii => write!(f, "@"),
            Scalar::Char => write!(f, "𝕌"),
            Scalar::Box(ScalarBox::Any) => write!(f, "□"),
            Scalar::Box(ScalarBox::All(inner)) => write!(f, "□{inner}"),
            Scalar::Box(ScalarBox::Def(Some(name), _)) => write!(f, "{name}"),
            Scalar::Box(ScalarBox::Def(None, fields)) => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{field}")?;
                }
                write!(f, "}}")
            }
            Scalar::Complex => write!(f, "ℂ"),
            Scalar::Stream => write!(f, "stream"),
            #[cfg(feature = "ga")]
            Scalar::Multivector => write!(f, "𝕍"),
        }
    }
}
