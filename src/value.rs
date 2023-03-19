use std::{cmp::Ordering, fmt, mem::forget, rc::Rc};

use nanbox::NanBox;

use crate::{array::Array, function::Function, grid_fmt::GridFmt, pervade, vm::Env, RuntimeResult};

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

type ArrayRef = *const Array;
const NUM_TAG: u8 = 0;
const CHAR_TAG: u8 = 1;
const FUNCTION_TAG: u8 = 2;
const ARRAY_TAG: u8 = 3;

static TYPES: [Type; 4] = {
    let mut types = [Type::Num; 4];
    types[NUM_TAG as usize] = Type::Num;
    types[CHAR_TAG as usize] = Type::Char;
    types[FUNCTION_TAG as usize] = Type::Function;
    types[ARRAY_TAG as usize] = Type::Array;
    types
};

impl Default for Value {
    fn default() -> Self {
        0.0.into()
    }
}

impl Value {
    pub fn show(&self) -> String {
        self.grid_string()
    }
    pub fn ty(&self) -> Type {
        TYPES[self.0.tag() as usize]
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
    pub fn as_nat(&self) -> Option<u64> {
        if self.is_nat() {
            Some(self.number() as u64)
        } else {
            None
        }
    }
    pub fn is_char(&self) -> bool {
        self.0.tag() == CHAR_TAG as u32
    }
    pub fn is_function(&self) -> bool {
        self.0.tag() == FUNCTION_TAG as u32
    }
    pub fn is_array(&self) -> bool {
        self.0.tag() == ARRAY_TAG as u32
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
                Ok(match self.ty() {
                    $(Type::$rt => pervade::$name::$f(&self.$get()).into(),)*
                    Type::Array => self.array().$name(env)?.into(),
                    ty => return Err(pervade::$name::error(ty, env)),
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
value_un_impl!(asin, (Num, number, num));
value_un_impl!(acos, (Num, number, num));
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
                Ok(match (self.ty(), other.ty()) {
                    $((Type::$a_ty, Type::$b_ty) => {
                        Value::from(pervade::$name::$ab(&self.$af(), &other.$bf()))
                    })*
                    (Type::Array, Type::Array) => {
                        Value::from(self.array().$name(other.array(), env)?)
                    }
                    $((Type::Array, Type::$b_ty) => {
                        Value::from(self.array().$name(&Array::from(other.$bf().clone()), env)?)
                    }),*
                    $((Type::$a_ty, Type::Array) => {
                        Value::from(Array::$name(&Array::from(self.$af().clone()), other.array(), env)?)
                    }),*
                    $(($a_fb, $b_fb) => $fallback,)?
                    (a, b) => return Err(pervade::$name::error(a, b, env))
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
value_bin_impl!(root, (Num, number, Num, number, num_num));
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
                |a, b| pervade::$name::is(a.cmp(&b)).into()
            );
        )*
    };
}

cmp_impls!(is_eq, is_ne, is_lt, is_le, is_gt, is_ge);

impl Drop for Value {
    fn drop(&mut self) {
        match self.ty() {
            Type::Array => unsafe {
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
        match self.ty() {
            Type::Array => Self(unsafe {
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
        match (self.ty(), other.ty()) {
            (Type::Num, Type::Num) => {
                let a = self.number();
                let b = other.number();
                a == b || a.is_nan() && b.is_nan()
            }
            (Type::Char, Type::Char) => self.char() == other.char(),
            (Type::Function, Type::Function) => self.function() == other.function(),
            (Type::Array, Type::Array) => self.array() == other.array(),
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
        match (self.ty(), other.ty()) {
            (Type::Num, Type::Num) => {
                let a = self.number();
                let b = other.number();
                a.partial_cmp(&b)
                    .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
            }
            (Type::Char, Type::Char) => self.char().cmp(&other.char()),
            (Type::Function, Type::Function) => self.function().cmp(&other.function()),
            (Type::Array, Type::Array) => self.array().cmp(other.array()),
            (a, b) => a.cmp(&b),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty() {
            Type::Num => write!(f, "{:?}", self.number()),
            Type::Char => write!(f, "{:?}", self.char()),
            Type::Function => write!(f, "{:?}", self.function()),
            Type::Array => write!(f, "{:?}", self.array()),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty() {
            Type::Num => write!(f, "{}", self.number()),
            Type::Char => write!(f, "{}", self.char()),
            Type::Function => write!(f, "{}", self.function()),
            Type::Array => write!(f, "{}", self.array()),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::from(b as u8 as f64)
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
    use crate::compile::Compiler;
    let mut compiler = Compiler::new();
    let code = "
xs = range 3_4
show xs
show ≡/+xs
";
    compiler.load(code, "test.uiua").unwrap();
    let assembly = compiler.finish();
    assembly.run().unwrap();
}
