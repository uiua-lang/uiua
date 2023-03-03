use std::{cmp::Ordering, fmt, ops::*};

use once_cell::sync::Lazy;

use crate::{ast::BinOp, lex::Span, UiuaResult};

pub struct Value {
    pub ty: Type,
    pub data: ValueData,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "{}", unsafe { self.data.bool }),
            Type::Nat => write!(f, "{}", unsafe { self.data.nat }),
            Type::Int => write!(f, "{}", unsafe { self.data.int }),
            Type::Real => write!(f, "{}", unsafe { self.data.real }),
            Type::Function => write!(f, "function"),
        }
    }
}

pub union ValueData {
    pub unit: (),
    pub bool: bool,
    pub nat: u64,
    pub int: i64,
    pub real: f64,
    pub function: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Unit,
    Bool,
    Nat,
    Int,
    Real,
    Function,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Nat => write!(f, "nat"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Function => write!(f, "function"),
        }
    }
}

impl Value {
    pub fn unit() -> Self {
        Self {
            ty: Type::Unit,
            data: ValueData { unit: () },
        }
    }
    pub fn bool(b: bool) -> Self {
        Self {
            ty: Type::Bool,
            data: ValueData { bool: b },
        }
    }
    pub fn nat(n: u64) -> Self {
        Self {
            ty: Type::Nat,
            data: ValueData { nat: n },
        }
    }
    pub fn int(n: i64) -> Self {
        Self {
            ty: Type::Int,
            data: ValueData { int: n },
        }
    }
    pub fn real(n: f64) -> Self {
        Self {
            ty: Type::Real,
            data: ValueData { real: n },
        }
    }
    pub fn function(n: usize) -> Self {
        Self {
            ty: Type::Function,
            data: ValueData { function: n },
        }
    }
    pub fn ty(&self) -> Type {
        self.ty
    }
    pub fn is_truthy(&self) -> bool {
        !(self.ty == Type::Unit || (self.ty == Type::Bool && unsafe { !self.data.bool }))
    }
    pub fn as_function(&self) -> Option<usize> {
        if self.ty == Type::Function {
            Some(unsafe { self.data.function })
        } else {
            None
        }
    }
}

static CLONE_TABLE: [fn(&ValueData) -> ValueData; 6] = [
    |_| ValueData { unit: () },
    |data| ValueData {
        bool: unsafe { data.bool },
    },
    |data| ValueData {
        nat: unsafe { data.nat },
    },
    |data| ValueData {
        int: unsafe { data.int },
    },
    |data| ValueData {
        real: unsafe { data.real },
    },
    |data| ValueData {
        function: unsafe { data.function },
    },
];

impl Clone for Value {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty,
            data: CLONE_TABLE[self.ty as usize](&self.data),
        }
    }
}

impl Value {
    pub fn bin_op(&mut self, other: Self, op: BinOp, span: &Span) -> UiuaResult {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        match op {
            BinOp::Add => self.add_assign(other, span)?,
            BinOp::Sub => self.sub_assign(other, span)?,
            BinOp::Mul => self.mul_assign(other, span)?,
            BinOp::Div => self.div_assign(other, span)?,
            BinOp::Eq => *self = Value::bool(*self == other),
            BinOp::Ne => *self = Value::bool(*self != other),
            BinOp::Lt => *self = Value::bool(*self < other),
            BinOp::Gt => *self = Value::bool(*self > other),
            BinOp::Le => *self = Value::bool(*self <= other),
            BinOp::Ge => *self = Value::bool(*self >= other),
            BinOp::RangeEx => todo!(),
        }
        Ok(())
    }
}

const ALL_TYPES: [Type; 6] = [
    Type::Unit,
    Type::Bool,
    Type::Nat,
    Type::Int,
    Type::Real,
    Type::Function,
];

type MathFn = fn(&mut Value, Value, span: &Span) -> UiuaResult;

macro_rules! value_bin_op {
    ($table:ident, $method:ident, $verb:literal) => {
        static $table: Lazy<Vec<MathFn>> = Lazy::new(|| {
            let mut fs: Vec<MathFn> = Vec::new();
            for a in ALL_TYPES {
                for b in ALL_TYPES {
                    unsafe {
                        fs.push(match (a, b) {
                            (Type::Unit, Type::Unit) => |_, _, _| Ok(()),
                            (Type::Nat, Type::Nat) => |a, b, _| {
                                a.data.nat.$method(b.data.nat);
                                Ok(())
                            },
                            (Type::Int, Type::Int) => |a, b, _| {
                                a.data.int.$method(b.data.int);
                                Ok(())
                            },
                            (Type::Real, Type::Real) => |a, b, _| {
                                a.data.real.$method(b.data.real);
                                Ok(())
                            },
                            _ => |a, b, span| -> UiuaResult {
                                Err(span
                                    .clone()
                                    .sp(format!("cannot {} {} and {}", $verb, a.ty, b.ty))
                                    .into())
                            },
                        });
                    }
                }
            }
            fs
        });
        impl Value {
            pub fn $method(&mut self, other: Self, span: &Span) -> UiuaResult {
                #[cfg(feature = "profile")]
                puffin::profile_function!();
                $table[self.ty as usize * ALL_TYPES.len() + other.ty as usize](self, other, span)
            }
        }
    };
}

value_bin_op!(ADD_TABLE, add_assign, "add");
value_bin_op!(SUB_TABLE, sub_assign, "subtract");
value_bin_op!(MUL_TABLE, mul_assign, "multiply");
value_bin_op!(DIV_TABLE, div_assign, "divide");

type EqFn = fn(&Value, &Value) -> bool;

static EQ_TABLE: Lazy<Vec<EqFn>> = Lazy::new(|| {
    let mut fs: Vec<EqFn> = Vec::new();
    for a in ALL_TYPES {
        for b in ALL_TYPES {
            unsafe {
                fs.push(match (a, b) {
                    (Type::Unit, Type::Unit) => |_, _| true,
                    (Type::Bool, Type::Bool) => |a, b| a.data.bool == b.data.bool,
                    (Type::Nat, Type::Nat) => |a, b| a.data.nat == b.data.nat,
                    (Type::Int, Type::Int) => |a, b| a.data.int == b.data.int,
                    (Type::Real, Type::Real) => |a, b| {
                        a.data.real.is_nan() && b.data.real.is_nan() || a.data.real == b.data.real
                    },
                    _ => |_, _| false,
                });
            }
        }
    }
    fs
});

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        EQ_TABLE[self.ty as usize * ALL_TYPES.len() + other.ty as usize](self, other)
    }
}

impl Eq for Value {}

type CmpFn = fn(&Value, &Value) -> Ordering;

static CMP_TABLE: Lazy<Vec<CmpFn>> = Lazy::new(|| {
    let mut fs: Vec<CmpFn> = Vec::new();
    for a in ALL_TYPES {
        for b in ALL_TYPES {
            unsafe {
                fs.push(match (a, b) {
                    (Type::Unit, Type::Unit) => |_, _| Ordering::Equal,
                    (Type::Bool, Type::Bool) => |a, b| a.data.bool.cmp(&b.data.bool),
                    (Type::Nat, Type::Nat) => |a, b| a.data.nat.cmp(&b.data.nat),
                    (Type::Int, Type::Int) => |a, b| a.data.int.cmp(&b.data.int),
                    (Type::Real, Type::Real) => {
                        |a, b| match (a.data.real.is_nan(), b.data.real.is_nan()) {
                            (true, true) => Ordering::Equal,
                            (true, false) => Ordering::Greater,
                            (false, true) => Ordering::Less,
                            (false, false) => a.data.real.partial_cmp(&b.data.real).unwrap(),
                        }
                    }
                    _ => |a, b| a.ty.cmp(&b.ty),
                });
            }
        }
    }
    fs
});

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        CMP_TABLE[self.ty as usize * ALL_TYPES.len() + other.ty as usize](self, other)
    }
}
