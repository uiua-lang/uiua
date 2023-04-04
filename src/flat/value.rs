use std::mem::take;

use crate::{function::Function, Uiua, UiuaResult};

use super::{array::*, pervade::*};

pub enum Value {
    Num(Array<f64>),
    Byte(Array<u8>),
    Char(Array<char>),
    Func(Array<Function>),
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Num(_) => "number",
            Self::Byte(_) => "byte",
            Self::Char(_) => "char",
            Self::Func(_) => "function",
        }
    }
    pub fn shape(&self) -> &[usize] {
        match self {
            Self::Num(array) => array.shape(),
            Self::Byte(array) => array.shape(),
            Self::Char(array) => array.shape(),
            Self::Func(array) => array.shape(),
        }
    }
}

macro_rules! value_from {
    ($ty:ty, $variant:ident) => {
        impl From<$ty> for Value {
            fn from(item: $ty) -> Self {
                Self::$variant(Array::from(item))
            }
        }
        impl From<Array<$ty>> for Value {
            fn from(array: Array<$ty>) -> Self {
                Self::$variant(array)
            }
        }
        impl From<Vec<$ty>> for Value {
            fn from(vec: Vec<$ty>) -> Self {
                Self::$variant(Array::from(vec))
            }
        }
        impl From<(Vec<usize>, Vec<$ty>)> for Value {
            fn from((shape, data): (Vec<usize>, Vec<$ty>)) -> Self {
                Self::$variant(Array::new(shape, data))
            }
        }
        impl FromIterator<$ty> for Value {
            fn from_iter<I: IntoIterator<Item = $ty>>(iter: I) -> Self {
                Self::$variant(Array::from_iter(iter))
            }
        }
    };
}

value_from!(f64, Num);
value_from!(u8, Byte);
value_from!(char, Char);
value_from!(Function, Func);

macro_rules! value_un_impl {
    ($name:ident, $(($variant:ident, $f:ident)),* $(,)?) => {
        impl Value {
            pub fn $name(&mut self, env: &Uiua) -> UiuaResult {
                Ok(match self {
                    $(Self::$variant(array) => {
                        let (shape, data) = take(array).into_pair();
                        *self = (shape, data.into_iter().map($name::$f).collect::<Vec<_>>()).into();
                    },)*
                    val => return Err($name::error(val.type_name(), env))
                })
            }
        }
    }
}

macro_rules! value_un_impl_all {
    ($($name:ident),* $(,)?) => {
        $(value_un_impl!($name, (Num, num), (Byte, byte));)*
    }
}

value_un_impl_all!(neg, not, abs, sign, sqrt, sin, cos, asin, acos, floor, ceil, round);

macro_rules! value_bin_impl {
    ($name:ident, $(($va:ident, $vb:ident, $f:ident)),* $(,)?) => {
        impl Value {
            pub fn $name(&mut self, other: &Self, env: &Uiua) -> UiuaResult {
                Ok(match (&mut *self, other) {
                    $((Value::$va(a), Value::$vb(b)) => {
                        *self = bin_pervade(a, &b, env, $name::$f)?.into()
                    },)*
                    (a, b) => return Err($name::error(a.type_name(), b.type_name(), env)),
                })
            }
        }
    };
}

value_bin_impl!(
    add,
    (Num, Num, num_num),
    (Num, Char, num_char),
    (Char, Num, char_num),
    (Byte, Byte, byte_byte),
    (Byte, Char, byte_char),
    (Char, Byte, char_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);

value_bin_impl!(
    sub,
    (Num, Num, num_num),
    (Num, Char, num_char),
    (Char, Char, char_char),
    (Byte, Byte, byte_byte),
    (Byte, Char, byte_char),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);

value_bin_impl!(
    mul,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);
value_bin_impl!(
    div,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);
value_bin_impl!(
    modulus,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);
value_bin_impl!(
    pow,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);
value_bin_impl!(
    log,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);
value_bin_impl!(atan2, (Num, Num, num_num));

value_bin_impl!(
    min,
    (Num, Num, num_num),
    (Char, Char, char_char),
    (Char, Num, char_num),
    (Num, Char, num_char),
    (Byte, Byte, byte_byte),
    (Char, Byte, char_byte),
    (Byte, Char, byte_char),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);

value_bin_impl!(
    max,
    (Num, Num, num_num),
    (Char, Char, char_char),
    (Char, Num, char_num),
    (Num, Char, num_char),
    (Byte, Byte, byte_byte),
    (Char, Byte, char_byte),
    (Byte, Char, byte_char),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);
