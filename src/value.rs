use std::{cmp::Ordering, fmt, mem::ManuallyDrop, ops::*};

use im::Vector;

use crate::{ast::BinOp, lex::Span, RuntimeResult};

pub struct Value {
    pub(crate) ty: Type,
    pub(crate) data: ValueData,
}

fn _keep_value_small(_: std::convert::Infallible) {
    let _: [u8; 16] = unsafe { std::mem::transmute(Value::unit()) };
}

impl Default for Value {
    fn default() -> Self {
        Self::unit()
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "{}", unsafe { self.data.bool }),
            Type::Int => write!(f, "{}", unsafe { self.data.int }),
            Type::Real => write!(f, "{:?}", unsafe { self.data.real }),
            Type::Function => write!(f, "function({})", unsafe { self.data.function.0 }),
            Type::Partial => write!(f, "partial({})", unsafe { self.data.partial.args.len() }),
            Type::List => f
                .debug_list()
                .entries(unsafe { self.data.list.0.iter() })
                .finish(),
        }
    }
}

pub(crate) union ValueData {
    pub unit: (),
    pub bool: bool,
    pub int: i64,
    pub real: f64,
    pub function: Function,
    pub partial: ManuallyDrop<Box<Partial>>,
    pub list: ManuallyDrop<Box<List>>,
}

macro_rules! ty {
    ($($ty:ident),* $(,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Type {
            $($ty),*
        }

        impl Type {
            pub const ARITY: usize = 0 $(+ { let _ = stringify!($ty); 1 })*;
            pub const ALL: [Self; Self::ARITY] = [$(Self::$ty),*];
        }
    };
}

ty!(Unit, Bool, Int, Real, Function, Partial, List);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Function => write!(f, "function"),
            Type::Partial => write!(f, "partial"),
            Type::List => write!(f, "list"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Function(pub(crate) usize);

impl nohash_hasher::IsEnabled for Function {}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Partial {
    pub(crate) function: Function,
    pub(crate) args: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
#[repr(transparent)]
pub struct List(pub Vector<Value>);

impl Value {
    pub const fn unit() -> Self {
        Self {
            ty: Type::Unit,
            data: ValueData { unit: () },
        }
    }
    #[inline(always)]
    pub const fn is_unit(&self) -> bool {
        matches!(self.ty, Type::Unit)
    }
    #[inline(always)]
    pub const fn ty(&self) -> Type {
        self.ty
    }
    pub const fn is_truthy(&self) -> bool {
        !(self.is_unit() || (matches!(self.ty, Type::Bool) && unsafe { !self.data.bool }))
    }
    pub(crate) unsafe fn list_mut(&mut self) -> &mut List {
        &mut self.data.list
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self {
            ty: Type::Bool,
            data: ValueData { bool: b },
        }
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Self {
            ty: Type::Int,
            data: ValueData { int: i },
        }
    }
}

impl From<f64> for Value {
    fn from(r: f64) -> Self {
        Self {
            ty: Type::Real,
            data: ValueData { real: r },
        }
    }
}

impl From<Function> for Value {
    fn from(f: Function) -> Self {
        Self {
            ty: Type::Function,
            data: ValueData { function: f },
        }
    }
}

impl From<Partial> for Value {
    fn from(p: Partial) -> Self {
        Self {
            ty: Type::Partial,
            data: ValueData {
                partial: ManuallyDrop::new(Box::new(p)),
            },
        }
    }
}

impl From<List> for Value {
    fn from(l: List) -> Self {
        Self {
            ty: Type::List,
            data: ValueData {
                list: ManuallyDrop::new(Box::new(l)),
            },
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::Bool {
            Ok(unsafe { value.data.bool })
        } else {
            Err(value)
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::Int {
            Ok(unsafe { value.data.int })
        } else {
            Err(value)
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::Real {
            Ok(unsafe { value.data.real })
        } else {
            Err(value)
        }
    }
}

impl TryFrom<Value> for Function {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::Function {
            Ok(unsafe { value.data.function })
        } else {
            Err(value)
        }
    }
}

impl TryFrom<Value> for Partial {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::Partial {
            Ok(unsafe { (**value.data.partial).clone() })
        } else {
            Err(value)
        }
    }
}

impl TryFrom<Value> for List {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::List {
            Ok(unsafe { (**value.data.list).clone() })
        } else {
            Err(value)
        }
    }
}

static DROP_TABLE: [fn(&mut ValueData); Type::ARITY] = [
    |_| {},
    |_| {},
    |_| {},
    |_| {},
    |_| {},
    |data| unsafe {
        ManuallyDrop::drop(&mut data.partial);
    },
    |data| unsafe {
        ManuallyDrop::drop(&mut data.list);
    },
];

impl Drop for Value {
    fn drop(&mut self) {
        DROP_TABLE[self.ty as usize](&mut self.data);
    }
}

static CLONE_TABLE: [fn(&ValueData) -> ValueData; Type::ARITY] = [
    |_| ValueData { unit: () },
    |data| ValueData {
        bool: unsafe { data.bool },
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
    |data| ValueData {
        partial: ManuallyDrop::new(unsafe { (*data.partial).clone() }),
    },
    |data| ValueData {
        list: ManuallyDrop::new(unsafe { (*data.list).clone() }),
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
    pub fn bin_op(&mut self, other: Self, op: BinOp, span: &Span) -> RuntimeResult {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        match op {
            BinOp::Add => self.add_assign(other, span)?,
            BinOp::Sub => self.sub_assign(other, span)?,
            BinOp::Mul => self.mul_assign(other, span)?,
            BinOp::Div => self.div_assign(other, span)?,
            BinOp::Eq => *self = (*self == other).into(),
            BinOp::Ne => *self = (*self != other).into(),
            BinOp::Lt => *self = (*self < other).into(),
            BinOp::Gt => *self = (*self > other).into(),
            BinOp::Le => *self = (*self <= other).into(),
            BinOp::Ge => *self = (*self >= other).into(),
        }
        Ok(())
    }
}

macro_rules! type_line {
    ($f:expr) => {
        [
            $f($crate::value::Type::Unit),
            $f($crate::value::Type::Bool),
            $f($crate::value::Type::Int),
            $f($crate::value::Type::Real),
            $f($crate::value::Type::Function),
            $f($crate::value::Type::Partial),
            $f($crate::value::Type::List),
        ]
    };
}
pub(crate) use type_line;

macro_rules! type_side {
    ($a:expr, $f:expr) => {
        [
            $f($a, $crate::value::Type::Unit),
            $f($a, $crate::value::Type::Bool),
            $f($a, $crate::value::Type::Int),
            $f($a, $crate::value::Type::Real),
            $f($a, $crate::value::Type::Function),
            $f($a, $crate::value::Type::Partial),
            $f($a, $crate::value::Type::List),
        ]
    };
}
pub(crate) use type_side;

macro_rules! type_square {
    ($f:expr) => {
        [
            $crate::value::type_side!($crate::value::Type::Unit, $f),
            $crate::value::type_side!($crate::value::Type::Bool, $f),
            $crate::value::type_side!($crate::value::Type::Int, $f),
            $crate::value::type_side!($crate::value::Type::Real, $f),
            $crate::value::type_side!($crate::value::Type::Function, $f),
            $crate::value::type_side!($crate::value::Type::Partial, $f),
            $crate::value::type_side!($crate::value::Type::List, $f),
        ]
    };
}
pub(crate) use type_square;

type EqFn = unsafe fn(&Value, &Value) -> bool;

static mut EQ_TABLE: [[EqFn; Type::ARITY]; Type::ARITY] = type_square!(eq_fn);
const fn eq_fn(a: Type, b: Type) -> EqFn {
    unsafe {
        match (a, b) {
            (Type::Unit, Type::Unit) => |_, _| true,
            (Type::Bool, Type::Bool) => |a, b| a.data.bool == b.data.bool,
            (Type::Int, Type::Int) => |a, b| a.data.int == b.data.int,
            (Type::Real, Type::Real) => {
                |a, b| a.data.real.is_nan() && b.data.real.is_nan() || a.data.real == b.data.real
            }
            (Type::Function, Type::Function) => |a, b| a.data.function == b.data.function,
            (Type::Partial, Type::Partial) => |a, b| a.data.partial == b.data.partial,
            (Type::List, Type::List) => |a, b| a.data.list == b.data.list,
            (Type::Int, Type::Real) => |a, b| a.data.int as f64 == b.data.real,
            (Type::Real, Type::Int) => |a, b| a.data.real == b.data.int as f64,
            _ => |_, _| false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        unsafe { EQ_TABLE[self.ty as usize][other.ty as usize](self, other) }
    }
}

impl Eq for Value {}

type CmpFn = fn(&Value, &Value) -> Ordering;

static mut CMP_TABLE: [[CmpFn; Type::ARITY]; Type::ARITY] = type_square!(cmp_fn);

const fn cmp_fn(a: Type, b: Type) -> CmpFn {
    unsafe {
        match (a, b) {
            (Type::Unit, Type::Unit) => |_, _| Ordering::Equal,
            (Type::Bool, Type::Bool) => |a, b| a.data.bool.cmp(&b.data.bool),
            (Type::Int, Type::Int) => |a, b| a.data.int.cmp(&b.data.int),
            (Type::Real, Type::Real) => |a, b| match (a.data.real.is_nan(), b.data.real.is_nan()) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                (false, false) => a.data.real.partial_cmp(&b.data.real).unwrap(),
            },
            (Type::Function, Type::Function) => |a, b| a.data.function.cmp(&b.data.function),
            (Type::Partial, Type::Partial) => |a, b| a.data.partial.cmp(&b.data.partial),
            (Type::List, Type::List) => |a, b| a.data.list.cmp(&b.data.list),
            (Type::Int, Type::Real) => |a, b| {
                if b.data.real.is_nan() {
                    Ordering::Less
                } else {
                    (a.data.int as f64).partial_cmp(&b.data.real).unwrap()
                }
            },
            (Type::Real, Type::Int) => |a, b| {
                if a.data.real.is_nan() {
                    Ordering::Greater
                } else {
                    a.data.real.partial_cmp(&(b.data.int as f64)).unwrap()
                }
            },
            _ => |a, b| a.ty.cmp(&b.ty),
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
        unsafe { CMP_TABLE[self.ty as usize][other.ty as usize](self, other) }
    }
}

type MathFn = fn(*mut Value, Value, span: &Span) -> RuntimeResult;

macro_rules! value_bin_op {
    ($table:ident, $fn_name:ident, $method:ident, $verb:literal) => {
        static mut $table: [[MathFn; Type::ARITY]; Type::ARITY] = type_square!($fn_name);
        const fn $fn_name(a: Type, b: Type) -> MathFn {
            unsafe {
                match (a, b) {
                    (Type::Unit, Type::Unit) => |_, _, _| Ok(()),
                    (Type::Int, Type::Int) => |a, b, _| {
                        (*a).data.int.$method(b.data.int);
                        Ok(())
                    },
                    (Type::Real, Type::Real) => |a, b, _| {
                        (*a).data.real.$method(b.data.real);
                        Ok(())
                    },
                    (Type::Real, Type::Int) => |a, b, _| {
                        (*a).data.real.$method(b.data.int as f64);
                        Ok(())
                    },
                    (Type::Int, Type::Real) => |a, b, _| {
                        *a = ((*a).data.int as f64).into();
                        (*a).data.real.$method(b.data.real);
                        Ok(())
                    },
                    _ => |a, b, span| {
                        Err(span
                            .clone()
                            .sp(format!("cannot {} {} and {}", $verb, (*a).ty, b.ty))
                            .into())
                    },
                }
            }
        }
        impl Value {
            pub fn $method(&mut self, other: Self, span: &Span) -> RuntimeResult {
                #[cfg(feature = "profile")]
                puffin::profile_function!();
                unsafe { $table[self.ty as usize][other.ty as usize](self, other, span) }
            }
        }
    };
}

value_bin_op!(ADD_TABLE, add_fn, add_assign, "add");
value_bin_op!(SUB_TABLE, sub_fn, sub_assign, "subtract");
value_bin_op!(MUL_TABLE, mul_fn, mul_assign, "multiply");
value_bin_op!(DIV_TABLE, div_fn, div_assign, "divide");
