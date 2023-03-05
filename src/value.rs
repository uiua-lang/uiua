use std::{cmp::Ordering, fmt, mem::ManuallyDrop, ops::*, ptr, sync::Arc};

use crate::{
    array::Array,
    ast::BinOp,
    builtin::{Env, ValueFn2},
    list::List,
    RuntimeResult,
};

pub struct Value {
    pub(crate) ty: Type,
    pub(crate) data: ValueData,
}

fn _keep_value_small(_: std::convert::Infallible) {
    let _: [u8; 16] = unsafe { std::mem::transmute(Value::unit()) };
}

impl Default for Value {
    #[inline(always)]
    fn default() -> Self {
        Self::unit()
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "{}", unsafe { self.data.bool }),
            Type::Byte => write!(f, "{}", unsafe { self.data.byte }),
            Type::Int => write!(f, "{}", unsafe { self.data.int }),
            Type::Real => write!(f, "{:?}", unsafe { self.data.real }),
            Type::Char => write!(f, "{:?}", unsafe { self.data.char }),
            Type::Function => write!(f, "function({})", unsafe { self.data.function.0 }),
            Type::Partial => write!(f, "partial({})", unsafe { self.data.partial.args.len() }),
            Type::String => write!(f, "{:?}", unsafe { &*self.data.string }),
            Type::List => f
                .debug_set()
                .entries(unsafe { &*self.data.list }.iter())
                .finish(),
            Type::Array => unsafe { (*self.data.array).fmt(f) },
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "{}", unsafe { self.data.bool }),
            Type::Byte => write!(f, "{}", unsafe { self.data.byte }),
            Type::Int => write!(f, "{}", unsafe { self.data.int }),
            Type::Real => write!(f, "{}", unsafe { self.data.real }),
            Type::Char => write!(f, "{}", unsafe { self.data.char }),
            Type::Function => write!(f, "function({})", unsafe { self.data.function.0 }),
            Type::Partial => write!(f, "partial({})", unsafe { self.data.partial.args.len() }),
            Type::String => write!(f, "{}", unsafe { &*self.data.string }),
            Type::List => f
                .debug_set()
                .entries(unsafe { &*self.data.list }.iter())
                .finish(),
            Type::Array => write!(f, "{:?}", unsafe { &*self.data.array }),
        }
    }
}

pub(crate) union ValueData {
    pub unit: (),
    pub bool: bool,
    pub byte: u8,
    pub int: i64,
    pub real: f64,
    pub char: char,
    pub function: Function,
    pub partial: ManuallyDrop<Box<Partial>>,
    pub string: ManuallyDrop<Arc<String>>,
    pub list: ManuallyDrop<Box<List>>,
    pub array: ManuallyDrop<Box<Array>>,
}

macro_rules! ty {
    ($($ty:ident),* $(,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(usize)]
        pub enum Type {
            $($ty),*
        }

        impl Type {
            pub const ARITY: usize = 0 $(+ { let _ = stringify!($ty); 1 })*;
            pub const ALL: [Self; Self::ARITY] = [$(Self::$ty),*];
        }
    };
}

ty!(Unit, Bool, Byte, Int, Real, Char, Function, Partial, String, List, Array);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Byte => write!(f, "byte"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Char => write!(f, "char"),
            Type::Function => write!(f, "function"),
            Type::Partial => write!(f, "partial"),
            Type::String => write!(f, "string"),
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

impl Value {
    #[inline(always)]
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
    pub(crate) unsafe fn string_mut(&mut self) -> &mut String {
        Arc::make_mut(&mut *self.data.string)
    }
    #[inline(always)]
    pub(crate) unsafe fn list_mut(&mut self) -> &mut List {
        &mut self.data.list
    }
    #[inline(always)]
    pub(crate) unsafe fn array_mut(&mut self) -> &mut Array {
        &mut self.data.array
    }
}

static DROP_TABLE: [fn(&mut ValueData); Type::ARITY] = [
    |_| {},
    |_| {},
    |_| {},
    |_| {},
    |_| {},
    |_| {},
    |_| {},
    |data| unsafe {
        ManuallyDrop::drop(&mut data.partial);
    },
    |data| unsafe {
        ManuallyDrop::drop(&mut data.string);
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
        byte: unsafe { data.byte },
    },
    |data| ValueData {
        int: unsafe { data.int },
    },
    |data| ValueData {
        real: unsafe { data.real },
    },
    |data| ValueData {
        char: unsafe { data.char },
    },
    |data| ValueData {
        function: unsafe { data.function },
    },
    |data| ValueData {
        partial: ManuallyDrop::new(unsafe { (*data.partial).clone() }),
    },
    |data| ValueData {
        string: ManuallyDrop::new(unsafe { (*data.string).clone() }),
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
    pub(crate) fn bin_op(&mut self, other: &mut Self, op: BinOp, env: Env) -> RuntimeResult {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        match op {
            BinOp::Add => self.add(other, env)?,
            BinOp::Sub => self.sub(other, env)?,
            BinOp::Mul => self.mul(other, env)?,
            BinOp::Div => self.div(other, env)?,
            BinOp::Eq => self.is_eq(other, env)?,
            BinOp::Ne => self.is_ne(other, env)?,
            BinOp::Lt => self.is_lt(other, env)?,
            BinOp::Le => self.is_le(other, env)?,
            BinOp::Gt => self.is_gt(other, env)?,
            BinOp::Ge => self.is_ge(other, env)?,
        }
        Ok(())
    }
}

macro_rules! type_line {
    ($f:expr $(,$a:expr)?) => {
        [
            $f($($a,)? $crate::value::Type::Unit),
            $f($($a,)? $crate::value::Type::Bool),
            $f($($a,)? $crate::value::Type::Byte),
            $f($($a,)? $crate::value::Type::Int),
            $f($($a,)? $crate::value::Type::Real),
            $f($($a,)? $crate::value::Type::Char),
            $f($($a,)? $crate::value::Type::Function),
            $f($($a,)? $crate::value::Type::Partial),
            $f($($a,)? $crate::value::Type::String),
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
            $crate::value::type_line!($f, $crate::value::Type::Byte),
            $crate::value::type_line!($f, $crate::value::Type::Int),
            $crate::value::type_line!($f, $crate::value::Type::Real),
            $crate::value::type_line!($f, $crate::value::Type::Char),
            $crate::value::type_line!($f, $crate::value::Type::Function),
            $crate::value::type_line!($f, $crate::value::Type::Partial),
            $crate::value::type_line!($f, $crate::value::Type::String),
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
            (Type::Byte, Type::Byte) => |a, b| a.data.byte == b.data.byte,
            (Type::Int, Type::Int) => |a, b| a.data.int == b.data.int,
            (Type::Real, Type::Real) => {
                |a, b| a.data.real.is_nan() && b.data.real.is_nan() || a.data.real == b.data.real
            }
            (Type::Char, Type::Char) => |a, b| a.data.char == b.data.char,
            (Type::Function, Type::Function) => |a, b| a.data.function == b.data.function,
            (Type::Partial, Type::Partial) => |a, b| a.data.partial == b.data.partial,
            (Type::String, Type::String) => |a, b| a.data.string == b.data.string,
            (Type::List, Type::List) => |a, b| a.data.list == b.data.list,
            (Type::Array, Type::Array) => |a, b| a.data.array == b.data.array,
            (Type::Int, Type::Byte) => |a, b| a.data.int == b.data.byte as i64,
            (Type::Byte, Type::Int) => |a, b| a.data.byte as i64 == b.data.int,
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
            (Type::Char, Type::Char) => |a, b| a.data.char.cmp(&b.data.char),
            (Type::Function, Type::Function) => |a, b| a.data.function.cmp(&b.data.function),
            (Type::Partial, Type::Partial) => |a, b| a.data.partial.cmp(&b.data.partial),
            (Type::String, Type::String) => |a, b| a.data.string.cmp(&b.data.string),
            (Type::List, Type::List) => |a, b| a.data.list.cmp(&b.data.list),
            (Type::Array, Type::Array) => |a, b| a.data.array.cmp(&b.data.array),
            (Type::Int, Type::Byte) => |a, b| a.data.int.cmp(&(b.data.byte as i64)),
            (Type::Byte, Type::Int) => |a, b| (a.data.byte as i64).cmp(&b.data.int),
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
                        *a = $trait::$method((*a).data.int, (*b).data.int).into();
                        Ok(())
                    },
                    (Type::Real, Type::Real) => |a, b, _| {
                        *a = $trait::$method((*a).data.real, (*b).data.real).into();
                        Ok(())
                    },
                    (Type::Real, Type::Int) => |a, b, _| {
                        *a = $trait::$method((*a).data.real, (*b).data.int as f64).into();
                        Ok(())
                    },
                    (Type::Int, Type::Real) => |a, b, _| {
                        *a = ((*a).data.int as f64).into();
                        *a = $trait::$method((*a).data.real, (*b).data.real).into();
                        Ok(())
                    },
                    (Type::Array, Type::Array) => |a, b, env| {
                        if (*a).array_mut().len() != (*b).data.array.len() {
                            return Err(env.error(format!(
                                "Cannot {} arrays of different lengths: {} and {}",
                                $verb,
                                (*a).array_mut().len(),
                                (*b).data.array.len()
                            )));
                        }
                        for (a, b) in (*a).array_mut().iter_mut().zip((*b).array_mut().iter_mut()) {
                            a.$method(&mut *b, env)?;
                        }
                        Ok(())
                    },
                    (Type::Array, _) => |a, b, env| {
                        for a in (*a).array_mut().iter_mut() {
                            a.$method(&mut *b, env)?;
                        }
                        Ok(())
                    },
                    (_, Type::Array) => |a, b, env| {
                        for b in (*b).array_mut().iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$method(b, env)?;
                            *b = a_clone;
                        }
                        ptr::swap(a, b);
                        Ok(())
                    },
                    _ => |a, b, env| {
                        Err(env.error(format!("Cannot {} {} and {}", $verb, (*a).ty, (*b).ty)))
                    },
                }
            }
        }
        impl Value {
            pub(crate) fn $method(&mut self, other: &mut Self, env: Env) -> RuntimeResult {
                #[cfg(feature = "profile")]
                puffin::profile_function!();
                unsafe { $table[self.ty as usize][other.ty as usize](self, other, env) }
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
                        *a = $method((*a).data.bool.cmp(&(*b).data.bool)).into();
                        Ok(())
                    },
                    (Type::Int, Type::Int) => |a, b, _| {
                        *a = $method((*a).data.int.cmp(&(*b).data.int)).into();
                        Ok(())
                    },
                    (Type::Real, Type::Real) => |a, b, _| {
                        *a = $method(real_ordering((*a).data.real, (*b).data.real)).into();
                        Ok(())
                    },
                    (Type::Real, Type::Int) => |a, b, _| {
                        *a = $method(real_ordering((*a).data.real, (*b).data.int as f64)).into();
                        Ok(())
                    },
                    (Type::Int, Type::Real) => |a, b, _| {
                        *a = ((*a).data.int as f64).into();
                        *a = $method(real_ordering((*a).data.real, (*b).data.real)).into();
                        Ok(())
                    },
                    (Type::Function, Type::Function) => |a, b, _| {
                        *a = $method((*a).data.function.cmp(&(*b).data.function)).into();
                        Ok(())
                    },
                    (Type::Partial, Type::Partial) => |a, b, _| {
                        *a = $method((*a).data.partial.cmp(&(*b).data.partial)).into();
                        Ok(())
                    },
                    (Type::Array, Type::Array) => |a, b, env| {
                        if (*a).array_mut().len() != (*b).data.array.len() {
                            return Err(env.error(format!(
                                "Cannot compare arrays of different lengths: {} and {}",
                                (*a).array_mut().len(),
                                (*b).data.array.len()
                            )));
                        }
                        for (a, b) in (*a).array_mut().iter_mut().zip((*b).array_mut().iter_mut()) {
                            a.$method(&mut *b, env)?;
                        }
                        Ok(())
                    },
                    (Type::Array, _) => |a, b, env| {
                        for a in (*a).array_mut().iter_mut() {
                            a.$method(&mut *b, env)?;
                        }
                        Ok(())
                    },
                    (_, Type::Array) => |a, b, env| {
                        for b in (*b).array_mut().iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$method(b, env)?;
                            *b = a_clone;
                        }
                        ptr::swap(a, b);
                        Ok(())
                    },
                    _ => |a, b, _| {
                        *a = $method((*a).ty.cmp(&(*b).ty)).into();
                        Ok(())
                    },
                }
            }
        }
        impl Value {
            pub(crate) fn $method(&mut self, other: &mut Self, env: Env) -> RuntimeResult {
                #[cfg(feature = "profile")]
                puffin::profile_function!();
                unsafe { $table[self.ty as usize][other.ty as usize](self, other, env) }
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

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self {
            ty: Type::Bool,
            data: ValueData { bool: b },
        }
    }
}

impl From<u8> for Value {
    fn from(i: u8) -> Self {
        Self {
            ty: Type::Byte,
            data: ValueData { byte: i },
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

impl From<char> for Value {
    fn from(c: char) -> Self {
        Self {
            ty: Type::Char,
            data: ValueData { char: c },
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

impl From<Arc<String>> for Value {
    fn from(s: Arc<String>) -> Self {
        Self {
            ty: Type::String,
            data: ValueData {
                string: ManuallyDrop::new(s),
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

impl TryFrom<Value> for Arc<String> {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if value.ty == Type::String {
            Ok(unsafe { (*value.data.string).clone() })
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
