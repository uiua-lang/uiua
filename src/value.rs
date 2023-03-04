use std::{cmp::Ordering, fmt, mem::ManuallyDrop, ops::*};

use im::Vector;

use crate::{array::Array, ast::BinOp, builtin::ValueFn2, lex::Span, RuntimeResult};

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
                .debug_set()
                .entries(unsafe { &*self.data.list }.0.iter())
                .finish(),
            Type::Array => unsafe { (*self.data.array).fmt(f) },
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
    pub array: ManuallyDrop<Box<Array>>,
}

macro_rules! ty {
    ($($ty:ident),* $(,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u8)]
        pub enum Type {
            $($ty),*
        }

        impl Type {
            pub const ARITY: usize = 0 $(+ { let _ = stringify!($ty); 1 })*;
            pub const ALL: [Self; Self::ARITY] = [$(Self::$ty),*];
        }
    };
}

ty!(Unit, Bool, Int, Real, Function, Partial, List, Array);

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
            Type::Array => write!(f, "array"),
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
    pub(crate) unsafe fn array_mut(&mut self) -> &mut Array {
        &mut self.data.array
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

impl From<Array> for Value {
    fn from(a: Array) -> Self {
        Self {
            ty: Type::Array,
            data: ValueData {
                array: ManuallyDrop::new(Box::new(a)),
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

impl TryFrom<Value> for Array {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::Array {
            Ok(unsafe { (**value.data.array).clone() })
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
    |data| unsafe {
        ManuallyDrop::drop(&mut data.array);
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
    |data| ValueData {
        array: ManuallyDrop::new(unsafe { (*data.array).clone() }),
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
            BinOp::Add => self.add(other, span)?,
            BinOp::Sub => self.sub(other, span)?,
            BinOp::Mul => self.mul(other, span)?,
            BinOp::Div => self.div(other, span)?,
            BinOp::Eq => self.is_eq(other, span)?,
            BinOp::Ne => self.is_ne(other, span)?,
            BinOp::Lt => self.is_lt(other, span)?,
            BinOp::Le => self.is_le(other, span)?,
            BinOp::Gt => self.is_gt(other, span)?,
            BinOp::Ge => self.is_ge(other, span)?,
        }
        Ok(())
    }
}

macro_rules! type_line {
    ($f:expr $(,$a:expr)?) => {
        [
            $f($($a,)? $crate::value::Type::Unit),
            $f($($a,)? $crate::value::Type::Bool),
            $f($($a,)? $crate::value::Type::Int),
            $f($($a,)? $crate::value::Type::Real),
            $f($($a,)? $crate::value::Type::Function),
            $f($($a,)? $crate::value::Type::Partial),
            $f($($a,)? $crate::value::Type::List),
            $f($($a,)? $crate::value::Type::Array),
        ]
    };
}
pub(crate) use type_line;

macro_rules! type_square {
    ($f:expr) => {
        [
            $crate::value::type_line!($f, $crate::value::Type::Unit),
            $crate::value::type_line!($f, $crate::value::Type::Bool),
            $crate::value::type_line!($f, $crate::value::Type::Int),
            $crate::value::type_line!($f, $crate::value::Type::Real),
            $crate::value::type_line!($f, $crate::value::Type::Function),
            $crate::value::type_line!($f, $crate::value::Type::Partial),
            $crate::value::type_line!($f, $crate::value::Type::List),
            $crate::value::type_line!($f, $crate::value::Type::Array),
        ]
    };
}
pub(crate) use type_square;

type EqFn = unsafe fn(&Value, &Value) -> bool;
static mut SIMPLE_EQ_TABLE: [[EqFn; Type::ARITY]; Type::ARITY] = type_square!(simple_eq_fn);
const fn simple_eq_fn(a: Type, b: Type) -> EqFn {
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
            (Type::Array, Type::Array) => |a, b| a.data.array == b.data.array,
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
        unsafe { SIMPLE_EQ_TABLE[self.ty as usize][other.ty as usize](self, other) }
    }
}

impl Eq for Value {}

type SimpleCmpFn = fn(&Value, &Value) -> Ordering;
static mut CMP_TABLE: [[SimpleCmpFn; Type::ARITY]; Type::ARITY] = type_square!(simple_cmp_fn);
const fn simple_cmp_fn(a: Type, b: Type) -> SimpleCmpFn {
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
            (Type::Array, Type::Array) => |a, b| a.data.array.cmp(&b.data.array),
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

macro_rules! value_math_op {
    ($table:ident, $fn_name:ident, $trait:ident, $method:ident, $verb:literal) => {
        static mut $table: [[ValueFn2; Type::ARITY]; Type::ARITY] = type_square!($fn_name);
        const fn $fn_name(a: Type, b: Type) -> ValueFn2 {
            unsafe {
                match (a, b) {
                    (Type::Unit, Type::Unit) => |_, _, _| Ok(()),
                    (Type::Int, Type::Int) => |a, b, _| {
                        *a = $trait::$method((*a).data.int, b.data.int).into();
                        Ok(())
                    },
                    (Type::Real, Type::Real) => |a, b, _| {
                        *a = $trait::$method((*a).data.real, b.data.real).into();
                        Ok(())
                    },
                    (Type::Real, Type::Int) => |a, b, _| {
                        *a = $trait::$method((*a).data.real, b.data.int as f64).into();
                        Ok(())
                    },
                    (Type::Int, Type::Real) => |a, b, _| {
                        *a = ((*a).data.int as f64).into();
                        *a = $trait::$method((*a).data.real, b.data.real).into();
                        Ok(())
                    },
                    (Type::Array, Type::Array) => |a, b, span| {
                        if (*a).array_mut().len() != b.data.array.len() {
                            return Err(span.error(format!(
                                "Cannot {} arrays of different lengths: {} and {}",
                                $verb,
                                (*a).array_mut().len(),
                                b.data.array.len()
                            )));
                        }
                        for (a, b) in (*a)
                            .array_mut()
                            .iter_mut()
                            .zip(b.data.array.iter().cloned())
                        {
                            a.$method(b, span)?;
                        }
                        Ok(())
                    },
                    (Type::Array, _) => |a, b, span| {
                        for a in (*a).array_mut().iter_mut() {
                            a.$method(b.clone(), span)?;
                        }
                        Ok(())
                    },
                    (_, Type::Array) => |a, mut b, span| {
                        for b in b.array_mut().iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$method(b.clone(), span)?;
                            *b = a_clone;
                        }
                        *a = b;
                        Ok(())
                    },
                    _ => |a, b, span| {
                        Err(span
                            .clone()
                            .sp(format!("Cannot {} {} and {}", $verb, (*a).ty, b.ty))
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

value_math_op!(ADD_TABLE, add_fn, Add, add, "add");
value_math_op!(SUB_TABLE, sub_fn, Sub, sub, "subtract");
value_math_op!(MUL_TABLE, mul_fn, Mul, mul, "multiply");
value_math_op!(DIV_TABLE, div_fn, Div, div, "divide");

fn real_ordering(a: f64, b: f64) -> Ordering {
    match (a.is_nan(), b.is_nan()) {
        (true, true) => Ordering::Equal,
        (false, true) => Ordering::Less,
        (true, false) => Ordering::Greater,
        (false, false) => a.partial_cmp(&b).unwrap(),
    }
}

fn is_eq(ordering: Ordering) -> bool {
    ordering == Ordering::Equal
}
fn is_ne(ordering: Ordering) -> bool {
    ordering != Ordering::Equal
}
fn is_lt(ordering: Ordering) -> bool {
    ordering == Ordering::Less
}
fn is_le(ordering: Ordering) -> bool {
    ordering != Ordering::Greater
}
fn is_gt(ordering: Ordering) -> bool {
    ordering == Ordering::Greater
}
fn is_ge(ordering: Ordering) -> bool {
    ordering != Ordering::Less
}

macro_rules! value_cmp_op {
    ($table:ident, $fn_name:ident, $method:ident) => {
        static mut $table: [[ValueFn2; Type::ARITY]; Type::ARITY] = type_square!($fn_name);
        const fn $fn_name(a: Type, b: Type) -> ValueFn2 {
            unsafe {
                match (a, b) {
                    (Type::Unit, Type::Unit) => |a, _, _| {
                        *a = $method(Ordering::Equal).into();
                        Ok(())
                    },
                    (Type::Bool, Type::Bool) => |a, b, _| {
                        *a = $method((*a).data.bool.cmp(&b.data.bool)).into();
                        Ok(())
                    },
                    (Type::Int, Type::Int) => |a, b, _| {
                        *a = $method((*a).data.int.cmp(&b.data.int)).into();
                        Ok(())
                    },
                    (Type::Real, Type::Real) => |a, b, _| {
                        *a = $method(real_ordering((*a).data.real, b.data.real)).into();
                        Ok(())
                    },
                    (Type::Real, Type::Int) => |a, b, _| {
                        *a = $method(real_ordering((*a).data.real, b.data.int as f64)).into();
                        Ok(())
                    },
                    (Type::Int, Type::Real) => |a, b, _| {
                        *a = ((*a).data.int as f64).into();
                        *a = $method(real_ordering((*a).data.real, b.data.real)).into();
                        Ok(())
                    },
                    (Type::Function, Type::Function) => |a, b, _| {
                        *a = $method((*a).data.function.cmp(&b.data.function)).into();
                        Ok(())
                    },
                    (Type::Partial, Type::Partial) => |a, b, _| {
                        *a = $method((*a).data.partial.cmp(&b.data.partial)).into();
                        Ok(())
                    },
                    (Type::Array, Type::Array) => |a, b, span| {
                        if (*a).array_mut().len() != b.data.array.len() {
                            return Err(span.error(format!(
                                "Cannot compare arrays of different lengths: {} and {}",
                                (*a).array_mut().len(),
                                b.data.array.len()
                            )));
                        }
                        for (a, b) in (*a)
                            .array_mut()
                            .iter_mut()
                            .zip(b.data.array.iter().cloned())
                        {
                            a.$method(b, span)?;
                        }
                        Ok(())
                    },
                    (Type::Array, _) => |a, b, span| {
                        for a in (*a).array_mut().iter_mut() {
                            a.$method(b.clone(), span)?;
                        }
                        Ok(())
                    },
                    (_, Type::Array) => |a, mut b, span| {
                        for b in b.array_mut().iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$method(b.clone(), span)?;
                            *b = a_clone;
                        }
                        *a = b;
                        Ok(())
                    },
                    _ => |a, b, _| {
                        *a = $method((*a).ty.cmp(&b.ty)).into();
                        Ok(())
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

value_cmp_op!(EQ_TABLE, eq_fn, is_eq);
value_cmp_op!(NE_TABLE, ne_fn, is_ne);
value_cmp_op!(LT_TABLE, lt_fn, is_lt);
value_cmp_op!(LE_TABLE, le_fn, is_le);
value_cmp_op!(GT_TABLE, gt_fn, is_gt);
value_cmp_op!(GE_TABLE, ge_fn, is_ge);
