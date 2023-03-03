use std::{cmp::Ordering, fmt, ops::*, sync::Once};

use crate::{ast::BinOp, lex::Span, UiuaResult};

pub struct Value {
    ty: Type,
    data: ValueData,
}

fn _keep_value_small(_: std::convert::Infallible) {
    let _: [u8; 16] = unsafe { std::mem::transmute(Value::unit()) };
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

union ValueData {
    unit: (),
    bool: bool,
    nat: u64,
    int: i64,
    real: f64,
    function: Function,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Function(pub(crate) usize);

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
    pub fn nat(nat: u64) -> Self {
        Self {
            ty: Type::Nat,
            data: ValueData { nat },
        }
    }
    pub fn int(int: i64) -> Self {
        Self {
            ty: Type::Int,
            data: ValueData { int },
        }
    }
    pub fn real(real: f64) -> Self {
        Self {
            ty: Type::Real,
            data: ValueData { real },
        }
    }
    pub fn function(function: Function) -> Self {
        Self {
            ty: Type::Function,
            data: ValueData { function },
        }
    }
    pub fn ty(&self) -> Type {
        self.ty
    }
    pub fn is_truthy(&self) -> bool {
        !(self.ty == Type::Unit || (self.ty == Type::Bool && unsafe { !self.data.bool }))
    }
    pub fn as_function(&self) -> Option<Function> {
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

const TYPES_COUNT: usize = 6;
const ALL_TYPES: [Type; TYPES_COUNT] = [
    Type::Unit,
    Type::Bool,
    Type::Nat,
    Type::Int,
    Type::Real,
    Type::Function,
];

type MathFn = fn(&mut Value, Value, span: &Span) -> UiuaResult;

macro_rules! value_bin_op {
    ($table:ident,$init:ident, $method:ident, $verb:literal) => {
        static mut $table: [MathFn; TYPES_COUNT * TYPES_COUNT] = [|a, b, span| -> UiuaResult {
            Err(span
                .clone()
                .sp(format!("cannot {} {} and {}", $verb, a.ty, b.ty))
                .into())
        }; TYPES_COUNT * TYPES_COUNT];
        unsafe fn $init() {
            for a in ALL_TYPES {
                for b in ALL_TYPES {
                    let f = &mut $table[a as usize * TYPES_COUNT + b as usize];
                    match (a, b) {
                        (Type::Unit, Type::Unit) => *f = |_, _, _| Ok(()),
                        (Type::Nat, Type::Nat) => {
                            *f = |a, b, _| {
                                a.data.nat.$method(b.data.nat);
                                Ok(())
                            }
                        }
                        (Type::Int, Type::Int) => {
                            *f = |a, b, _| {
                                a.data.int.$method(b.data.int);
                                Ok(())
                            }
                        }
                        (Type::Real, Type::Real) => {
                            *f = |a, b, _| {
                                a.data.real.$method(b.data.real);
                                Ok(())
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        impl Value {
            pub fn $method(&mut self, other: Self, span: &Span) -> UiuaResult {
                #[cfg(feature = "profile")]
                puffin::profile_function!();
                unsafe {
                    $table[self.ty as usize * TYPES_COUNT + other.ty as usize](self, other, span)
                }
            }
        }
    };
}

pub(crate) fn init_tables() {
    static ONCE: Once = Once::new();
    unsafe {
        ONCE.call_once(|| {
            init_add_table();
            init_sub_table();
            init_mul_table();
            init_div_table();
            init_eq_table();
            init_cmp_table();
        });
    }
}

value_bin_op!(ADD_TABLE, init_add_table, add_assign, "add");
value_bin_op!(SUB_TABLE, init_sub_table, sub_assign, "subtract");
value_bin_op!(MUL_TABLE, init_mul_table, mul_assign, "multiply");
value_bin_op!(DIV_TABLE, init_div_table, div_assign, "divide");

type EqFn = fn(&Value, &Value) -> bool;

static mut EQ_TABLE: [EqFn; TYPES_COUNT * TYPES_COUNT] = [|_, _| false; TYPES_COUNT * TYPES_COUNT];
unsafe fn init_eq_table() {
    for a in ALL_TYPES {
        for b in ALL_TYPES {
            let f = &mut EQ_TABLE[a as usize * TYPES_COUNT + b as usize];
            match (a, b) {
                (Type::Unit, Type::Unit) => *f = |_, _| true,
                (Type::Bool, Type::Bool) => *f = |a, b| a.data.bool == b.data.bool,
                (Type::Nat, Type::Nat) => *f = |a, b| a.data.nat == b.data.nat,
                (Type::Int, Type::Int) => *f = |a, b| a.data.int == b.data.int,
                (Type::Real, Type::Real) => {
                    *f = |a, b| {
                        a.data.real.is_nan() && b.data.real.is_nan() || a.data.real == b.data.real
                    }
                }
                _ => {}
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        unsafe { EQ_TABLE[self.ty as usize * TYPES_COUNT + other.ty as usize](self, other) }
    }
}

impl Eq for Value {}

type CmpFn = fn(&Value, &Value) -> Ordering;

static mut CMP_TABLE: [CmpFn; TYPES_COUNT * TYPES_COUNT] =
    [|_, _| Ordering::Equal; TYPES_COUNT * TYPES_COUNT];

unsafe fn init_cmp_table() {
    for a in ALL_TYPES {
        for b in ALL_TYPES {
            let f = &mut CMP_TABLE[a as usize * TYPES_COUNT + b as usize];
            match (a, b) {
                (Type::Unit, Type::Unit) => *f = |_, _| Ordering::Equal,
                (Type::Bool, Type::Bool) => *f = |a, b| a.data.bool.cmp(&b.data.bool),
                (Type::Nat, Type::Nat) => *f = |a, b| a.data.nat.cmp(&b.data.nat),
                (Type::Int, Type::Int) => *f = |a, b| a.data.int.cmp(&b.data.int),
                (Type::Real, Type::Real) => {
                    *f = |a, b| match (a.data.real.is_nan(), b.data.real.is_nan()) {
                        (true, true) => Ordering::Equal,
                        (true, false) => Ordering::Greater,
                        (false, true) => Ordering::Less,
                        (false, false) => a.data.real.partial_cmp(&b.data.real).unwrap(),
                    }
                }
                _ => {}
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        unsafe { CMP_TABLE[self.ty as usize * TYPES_COUNT + other.ty as usize](self, other) }
    }
}
