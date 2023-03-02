use std::fmt;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum Type {
    #[default]
    Unit,
    Bool,
    Nat,
    Int,
    Real,
    Function(Box<FunctionType>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Unknown,
    UnknownInt,
}

impl Type {
    pub fn matches(&mut self, other: &mut Self) -> bool {
        match (self, other) {
            (a @ Type::Unknown, b) => {
                *a = b.clone();
                true
            }
            (a, b @ Type::Unknown) => {
                *b = a.clone();
                true
            }
            (a @ Type::UnknownInt, b @ (Type::Nat | Type::Int)) => {
                *a = b.clone();
                true
            }
            (a @ (Type::Nat | Type::Int), b @ Type::UnknownInt) => {
                *b = a.clone();
                true
            }
            (Type::List(a), Type::List(b)) => a.matches(b),
            (Type::Tuple(a), Type::Tuple(b)) => a.iter_mut().zip(b).all(|(a, b)| a.matches(b)),
            (Type::Function(a), Type::Function(b)) => a.matches(b),
            (a, b) => a == b,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Nat => write!(f, "nat"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Function(func) => write!(f, "{func}"),
            Type::List(ty) => write!(f, "[{ty}]"),
            Type::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Type::Unknown => write!(f, "_"),
            Type::UnknownInt => write!(f, "{{integer}}"),
        }
    }
}

impl From<FunctionType> for Type {
    fn from(func: FunctionType) -> Self {
        Self::Function(Box::new(func))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct FunctionType {
    pub name: Option<String>,
    pub params: Vec<Type>,
    pub ret: Type,
}

impl FunctionType {
    pub fn new<T: Into<Type>>(
        name: Option<String>,
        params: impl IntoIterator<Item = T>,
        ret: T,
    ) -> Self {
        Self {
            name,
            params: params.into_iter().map(Into::into).collect(),
            ret: ret.into(),
        }
    }
    pub fn matches(&mut self, other: &mut Self) -> bool {
        if self != other {
            return false;
        }
        for (a, b) in self.params.iter_mut().zip(&mut other.params) {
            if !a.matches(b) {
                return false;
            }
        }
        self.ret.matches(&mut other.ret)
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{param}")?;
        }
        write!(f, ")")?;
        if self.ret != Type::Unit {
            write!(f, " -> {}", self.ret)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeSet {
    Any,
    Concrete(Type),
    Or(Vec<Self>),
    And(Vec<Self>),
}

impl From<Type> for TypeSet {
    fn from(ty: Type) -> Self {
        Self::Concrete(ty)
    }
}

impl TypeSet {
    pub fn or(self, other: impl Into<Self>) -> Self {
        match self {
            Self::Any => Self::Any,
            Self::Or(mut or) => {
                or.push(other.into());
                Self::Or(or)
            }
            _ => Self::Or(vec![self, other.into()]),
        }
    }
    pub fn and(self, other: impl Into<Self>) -> Self {
        match self {
            Self::Any => other.into(),
            Self::And(mut and) => {
                and.push(other.into());
                Self::And(and)
            }
            _ => Self::And(vec![self, other.into()]),
        }
    }
}
