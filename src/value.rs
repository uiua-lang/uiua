use std::{cmp::Ordering, fmt, mem::forget, rc::Rc};

use nanbox::NanBox;

use crate::{
    array::Array,
    function::{Function, Partial},
    pervade,
    vm::Env,
    RuntimeResult,
};

pub struct Value(NanBox);

fn _value_is_small() {
    let _: u64 = unsafe { std::mem::transmute(Value::from(0.0)) };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Num,
    Char,
    Function,
    Array,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Num => write!(f, "number"),
            Type::Char => write!(f, "character"),
            Type::Function => write!(f, "function"),
            Type::Array => write!(f, "array"),
        }
    }
}

type PartialRef = *mut Partial;
type ArrayRef = *const Array;
const NUM_TAG: u8 = 0;
const CHAR_TAG: u8 = 1;
const FUNCTION_TAG: u8 = 2;
const PARTIAL_TAG: u8 = 3;
const ARRAY_TAG: u8 = 4;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RawType {
    Num,
    Char,
    Function,
    Partial,
    Array,
}

static RAW_TYPES: [RawType; 5] = {
    let mut types = [RawType::Num; 5];
    types[NUM_TAG as usize] = RawType::Num;
    types[CHAR_TAG as usize] = RawType::Char;
    types[FUNCTION_TAG as usize] = RawType::Function;
    types[PARTIAL_TAG as usize] = RawType::Partial;
    types[ARRAY_TAG as usize] = RawType::Array;
    types
};

impl RawType {
    pub fn ty(&self) -> Type {
        match self {
            RawType::Num => Type::Num,
            RawType::Char => Type::Char,
            RawType::Function => Type::Function,
            RawType::Partial => Type::Function,
            RawType::Array => Type::Array,
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        0.0.into()
    }
}

impl Value {
    pub fn raw_ty(&self) -> RawType {
        RAW_TYPES[self.0.tag() as usize]
    }
    pub fn ty(&self) -> Type {
        self.raw_ty().ty()
    }
    pub fn is_num(&self) -> bool {
        self.0.tag() == NUM_TAG as u32
    }
    pub fn is_nat(&self) -> bool {
        self.is_num() && {
            let n = self.number();
            n >= 0.0 && n.trunc() == n
        }
    }
    pub fn is_char(&self) -> bool {
        self.0.tag() == CHAR_TAG as u32
    }
    pub fn is_function(&self) -> bool {
        self.0.tag() == FUNCTION_TAG as u32
    }
    pub fn is_partial(&self) -> bool {
        self.0.tag() == PARTIAL_TAG as u32
    }
    pub fn is_array(&self) -> bool {
        self.0.tag() == ARRAY_TAG as u32
    }
    pub fn params(&self) -> u8 {
        match self.raw_ty() {
            RawType::Num => 1,
            RawType::Char => 1,
            RawType::Function => self.function().params,
            RawType::Partial => self.partial().function.params - self.partial().args.len() as u8,
            RawType::Array => 1,
        }
    }
    pub fn number(&self) -> f64 {
        assert!(self.is_num());
        unsafe { self.0.unpack::<f64>() }
    }
    pub fn char(&self) -> char {
        assert!(self.is_char());
        unsafe { self.0.unpack::<char>() }
    }
    pub fn function(&self) -> Function {
        assert!(self.is_function());
        unsafe { self.0.unpack::<Function>() }
    }
    pub fn partial(&self) -> &Partial {
        assert!(self.is_partial());
        unsafe { &*self.0.unpack::<PartialRef>() }
    }
    pub fn partial_mut(&mut self) -> &mut Partial {
        assert!(self.is_partial());
        unsafe { &mut *self.0.unpack::<PartialRef>() }
    }
    pub fn array(&self) -> &Array {
        assert!(self.is_array());
        unsafe { &*self.0.unpack::<ArrayRef>() }
    }
    pub fn array_mut(&mut self) -> &mut Array {
        assert!(self.is_array());
        // Conjure the rc
        let mut rc = unsafe { Rc::from_raw(self.0.unpack::<ArrayRef>()) };
        // Ensure its reference is unique
        Rc::make_mut(&mut rc);
        // Get the pointer
        let ptr = Rc::as_ptr(&rc) as *mut Array;
        // Return the rc to the aether
        self.0 = new_array_nanbox(rc);
        // This should be safe because the mutable pointer is unique and the rc was forgotten
        unsafe { &mut *ptr }
    }
    pub fn into_array(self) -> Array {
        assert!(self.is_array());
        // Conjure the rc
        let rc = unsafe { Rc::from_raw(self.0.unpack::<ArrayRef>()) };
        let array = match Rc::try_unwrap(rc) {
            Ok(array) => {
                // The rc is consumed and can rest
                array
            }
            Err(rc) => {
                // Clone the array and let the rc rest
                let array = (*rc).clone();
                drop(rc);
                array
            }
        };
        // The rc is already dropped, so the destructor shouldn't be run again
        forget(self);
        array
    }
}

macro_rules! value_un_impl {
    ($name:ident $(,($rt:ident, $get:ident, $f:ident))* $(,)?) => {
        impl Value {
            #[allow(unreachable_patterns)]
            pub fn $name(&self, env: &Env) -> RuntimeResult<Self> {
                Ok(match self.raw_ty() {
                    $(RawType::$rt => pervade::$name::$f(&self.$get()).into(),)*
                    RawType::Array => self.array().$name(env)?.into(),
                    ty => return Err(pervade::$name::error(ty.ty(), env)),
                })
            }
        }
    };
}

value_un_impl!(not, (Num, number, num));
value_un_impl!(neg, (Num, number, num));
value_un_impl!(abs, (Num, number, num));
value_un_impl!(sqrt, (Num, number, num));
value_un_impl!(sin, (Num, number, num));
value_un_impl!(cos, (Num, number, num));
value_un_impl!(floor, (Num, number, num));
value_un_impl!(ceil, (Num, number, num));
value_un_impl!(round, (Num, number, num));

macro_rules! value_bin_impl {
    ($name:ident
        $(,($a_ty:ident, $af:ident, $b_ty:ident, $bf:ident, $ab:ident))*
        $(,|$a_fb:ident, $b_fb:ident| $fallback:expr)?
    $(,)?) => {
        impl Value {
            #[allow(unreachable_patterns)]
            pub fn $name(&self, other: &Self, env: &Env) -> RuntimeResult<Self> {
                Ok(match (self.raw_ty(), other.raw_ty()) {
                    $((RawType::$a_ty, RawType::$b_ty) => {
                        Value::from(pervade::$name::$ab(&self.$af(), &other.$bf()))
                    })*
                    (RawType::Array, RawType::Array) => {
                        Value::from(self.array().$name(other.array(), env)?)
                    }
                    $((RawType::Array, RawType::$b_ty) => {
                        Value::from(self.array().$name(&Array::from(other.$bf().clone()), env)?)
                    }),*
                    $((RawType::$a_ty, RawType::Array) => {
                        Value::from(Array::$name(&Array::from(self.$af().clone()), other.array(), env)?)
                    }),*
                    $(($a_fb, $b_fb) => $fallback,)?
                    (a, b) => return Err(pervade::$name::error(a.ty(), b.ty(), env))
                })
            }
        }
    };
}

value_bin_impl!(
    add,
    (Num, number, Num, number, num_num),
    (Num, number, Char, char, num_char),
    (Char, char, Num, number, char_num),
);

value_bin_impl!(
    sub,
    (Num, number, Num, number, num_num),
    (Char, char, Num, number, char_num),
    (Char, char, Char, char, char_char),
);

value_bin_impl!(mul, (Num, number, Num, number, num_num));
value_bin_impl!(div, (Num, number, Num, number, num_num));
value_bin_impl!(modulus, (Num, number, Num, number, num_num));
value_bin_impl!(pow, (Num, number, Num, number, num_num));
value_bin_impl!(atan2, (Num, number, Num, number, num_num));

value_bin_impl!(
    min,
    (Num, number, Num, number, num_num),
    (Char, char, Char, char, char_char),
    (Char, char, Num, number, char_num),
    (Num, number, Char, char, num_char),
);

value_bin_impl!(
    max,
    (Num, number, Num, number, num_num),
    (Char, char, Char, char, char_char),
    (Char, char, Num, number, char_num),
    (Num, number, Char, char, num_char),
);

macro_rules! cmp_impls {
    ($($name:ident),*) => {
        $(
            value_bin_impl!(
                $name,
                (Num, number, Num, number, num_num),
                (Char, char, Char, char, generic),
                (Function, function, Function, function, generic),
                (Partial, partial, Partial, partial, generic),
                |a, b| pervade::$name::is(a.ty().cmp(&b.ty())).into()
            );
        )*
    };
}

cmp_impls!(is_eq, is_ne, is_lt, is_le, is_gt, is_ge);

impl Drop for Value {
    fn drop(&mut self) {
        match self.raw_ty() {
            RawType::Partial => unsafe {
                drop(Box::from_raw(self.0.unpack::<PartialRef>()));
            },
            RawType::Array => unsafe {
                // Conjure the rc
                let rc = Rc::from_raw(self.0.unpack::<ArrayRef>());
                // It may now rest in peace and decrease the refcount
                drop(rc);
            },
            _ => {}
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self.raw_ty() {
            RawType::Partial => Self(unsafe {
                NanBox::new::<PartialRef>(
                    PARTIAL_TAG,
                    Box::into_raw(Box::new(self.partial().clone())),
                )
            }),
            RawType::Array => Self(unsafe {
                // Conjure the rc
                let rc = Rc::from_raw(self.0.unpack::<ArrayRef>());
                // Clone it to increase the refcount
                let clone = rc.clone();
                // Return the original rc to the aether
                forget(rc);
                // Use the clone to create a new NanBox
                new_array_nanbox(clone)
            }),
            _ => Self(self.0),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self.raw_ty(), other.raw_ty()) {
            (RawType::Num, RawType::Num) => {
                let a = self.number();
                let b = other.number();
                a == b || a.is_nan() && b.is_nan()
            }
            (RawType::Char, RawType::Char) => self.char() == other.char(),
            (RawType::Function, RawType::Function) => self.function() == other.function(),
            (RawType::Partial, RawType::Partial) => self.partial() == other.partial(),
            (RawType::Array, RawType::Array) => self.array() == other.array(),
            _ => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.raw_ty(), other.raw_ty()) {
            (RawType::Num, RawType::Num) => {
                let a = self.number();
                let b = other.number();
                a.partial_cmp(&b)
                    .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
            }
            (RawType::Char, RawType::Char) => self.char().cmp(&other.char()),
            (RawType::Function, RawType::Function) => self.function().cmp(&other.function()),
            (RawType::Partial, RawType::Partial) => self.partial().cmp(other.partial()),
            (RawType::Array, RawType::Array) => self.array().cmp(other.array()),
            (a, b) => a.cmp(&b),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.raw_ty() {
            RawType::Num => write!(f, "{:?}", self.number()),
            RawType::Char => write!(f, "{:?}", self.char()),
            RawType::Function => write!(f, "{:?}", self.function()),
            RawType::Partial => write!(f, "{:?}", self.partial()),
            RawType::Array => write!(f, "{:?}", self.array()),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.raw_ty() {
            RawType::Num => write!(f, "{}", self.number()),
            RawType::Char => write!(f, "{}", self.char()),
            RawType::Function => write!(f, "{}", self.function()),
            RawType::Partial => write!(f, "{}", self.partial()),
            RawType::Array => write!(f, "{}", self.array()),
        }
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::from(Array::from(s))
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Self(unsafe { NanBox::new(NUM_TAG, n) })
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self {
        Self(unsafe { NanBox::new(CHAR_TAG, c) })
    }
}

impl From<Function> for Value {
    fn from(f: Function) -> Self {
        Self(unsafe { NanBox::new(FUNCTION_TAG, f) })
    }
}

impl From<Partial> for Value {
    fn from(p: Partial) -> Self {
        Self(unsafe { NanBox::new::<PartialRef>(PARTIAL_TAG, Box::into_raw(Box::new(p))) })
    }
}

impl From<Array> for Value {
    fn from(a: Array) -> Self {
        // Create a new rc
        let rc = Rc::new(a);
        // Cast it into the aether
        Self(new_array_nanbox(rc))
    }
}

fn new_array_nanbox(rc: Rc<Array>) -> NanBox {
    unsafe { NanBox::new::<ArrayRef>(ARRAY_TAG, Rc::into_raw(rc)) }
}

#[test]
fn value_memory_test() {
    use crate::{compile::Compiler, UiuaError};
    let mut compiler = Compiler::new();
    let code = "
let xs = range [3, 4]
do show xs
do show <| each (fold (+) 0) xs
";
    if let Err(e) = compiler.load(code, "test.uiua") {
        eprintln!("{}", UiuaError::from(e));
        return;
    }
    let assembly = compiler.finish();
    if let Err(e) = assembly.run() {
        eprintln!("{e}");
    }
}
