use std::fmt;

#[derive(Debug, Clone, Default)]
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
    UnknownFunction(Vec<FunctionType>),
}

impl Type {
    pub fn matches(&mut self, other: &mut Self) -> bool {
        if self != other {
            return false;
        }
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
            (Type::UnknownFunction(fs), Type::Function(f))
            | (Type::Function(f), Type::UnknownFunction(fs)) => {
                fs.retain_mut(|f2| {
                    let mut f2_clone = f2.clone();
                    let matches = f2_clone.matches(f);
                    if matches {
                        *f2 = f2_clone;
                    }
                    matches
                });
                !fs.is_empty()
            }
            _ => true,
        }
    }
    pub fn pick_default(&mut self) -> bool {
        match self {
            Type::Unknown => false,
            Type::UnknownInt => {
                *self = Type::Int;
                true
            }
            Type::UnknownFunction(_) => false,
            _ => true,
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
            Type::UnknownFunction(_) => write!(f, "{{function}}"),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::UnknownInt, Type::Nat | Type::Int)
            | (Type::Nat | Type::Int, Type::UnknownInt) => true,
            (Type::UnknownFunction(fs), Type::Function(f))
            | (Type::Function(f), Type::UnknownFunction(fs)) => fs.iter().any(|f2| **f == *f2),
            (Type::Unit, Type::Unit) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Nat, Type::Nat) => true,
            (Type::Int, Type::Int) => true,
            (Type::Real, Type::Real) => true,
            (Type::Function(a), Type::Function(b)) => a == b,
            (Type::List(a), Type::List(b)) => a == b,
            (Type::Tuple(a), Type::Tuple(b)) => a == b,
            _ => false,
        }
    }
}

impl From<FunctionType> for Type {
    fn from(func: FunctionType) -> Self {
        Self::Function(Box::new(func))
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub ret: Type,
}

impl FunctionType {
    pub fn new(params: impl IntoIterator<Item = Type>, ret: Type) -> Self {
        Self {
            params: params.into_iter().collect(),
            ret,
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
