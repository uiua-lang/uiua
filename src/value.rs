use std::{cmp::Ordering, fmt, mem::take};

use crate::{algorithm::pervade::*, array::*, function::Function, Uiua, UiuaResult};

#[derive(Clone)]
pub enum Value {
    Num(Array<f64>),
    Byte(Array<u8>),
    Char(Array<char>),
    Func(Array<Function>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(array) => array.fmt(f),
            Self::Byte(array) => array.fmt(f),
            Self::Char(array) => array.fmt(f),
            Self::Func(array) => array.fmt(f),
        }
    }
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
    pub fn generic_mut<T>(
        &mut self,
        n: impl FnOnce(&mut Array<f64>) -> T,
        b: impl FnOnce(&mut Array<u8>) -> T,
        c: impl FnOnce(&mut Array<char>) -> T,
        f: impl FnOnce(&mut Array<Function>) -> T,
    ) -> T {
        match self {
            Self::Num(array) => n(array),
            Self::Byte(array) => b(array),
            Self::Char(array) => c(array),
            Self::Func(array) => f(array),
        }
    }
    pub fn into_generic<T>(
        self,
        n: impl FnOnce(Array<f64>) -> T,
        b: impl FnOnce(Array<u8>) -> T,
        c: impl FnOnce(Array<char>) -> T,
        f: impl FnOnce(Array<Function>) -> T,
    ) -> T {
        match self {
            Self::Num(array) => n(array),
            Self::Byte(array) => b(array),
            Self::Char(array) => c(array),
            Self::Func(array) => f(array),
        }
    }
    pub fn as_indices(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<Vec<isize>> {
        self.as_number_list(env, requirement, |f| f % 1.0 == 0.0, |f| f as isize)
    }
    pub fn as_naturals(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<Vec<usize>> {
        self.as_number_list(
            env,
            requirement,
            |f| f % 1.0 == 0.0 && f >= 0.0,
            |f| f as usize,
        )
    }
    fn as_number_list<T>(
        &self,
        env: &Uiua,
        requirement: &'static str,
        test: fn(f64) -> bool,
        convert: fn(f64) -> T,
    ) -> UiuaResult<Vec<T>> {
        Ok(match self {
            Value::Num(nums) => {
                if nums.rank() > 1 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", nums.rank()))
                    );
                }
                let mut result = Vec::with_capacity(nums.len());
                for &num in nums.data() {
                    if !test(num) {
                        return Err(env.error(requirement));
                    }
                    result.push(convert(num));
                }
                result
            }
            Value::Byte(bytes) => {
                if bytes.rank() > 1 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", bytes.rank()))
                    );
                }
                let mut result = Vec::with_capacity(bytes.len());
                for &byte in bytes.data() {
                    let num = byte as f64;
                    if !test(num) {
                        return Err(env.error(requirement));
                    }
                    result.push(convert(num));
                }
                result
            }
            value => {
                return Err(env.error(format!(
                    "{requirement}, but its type is {}",
                    value.type_name()
                )))
            }
        })
    }
    pub fn as_string(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<String> {
        if let Value::Char(chars) = self {
            if chars.rank() > 1 {
                return Err(env.error(format!("{requirement}, but its rank is {}", chars.rank())));
            }
            Ok(chars.data().iter().collect())
        } else {
            Err(env.error(format!(
                "{requirement}, but its type is {}",
                self.type_name()
            )))
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

impl FromIterator<usize> for Value {
    fn from_iter<I: IntoIterator<Item = usize>>(iter: I) -> Self {
        iter.into_iter().map(|i| i as f64).collect()
    }
}

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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Byte(a), Value::Byte(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Func(a), Value::Func(b)) => a == b,
            (Value::Num(a), Value::Byte(b)) => a.eq(b),
            (Value::Byte(a), Value::Num(b)) => b.eq(a),
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
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.cmp(b),
            (Value::Byte(a), Value::Byte(b)) => a.cmp(b),
            (Value::Char(a), Value::Char(b)) => a.cmp(b),
            (Value::Func(a), Value::Func(b)) => a.cmp(b),
            (Value::Num(a), Value::Byte(b)) => a.cmp(b),
            (Value::Byte(a), Value::Num(b)) => b.cmp(a).reverse(),
            (Value::Num(_), _) => Ordering::Less,
            (_, Value::Num(_)) => Ordering::Greater,
            (Value::Byte(_), _) => Ordering::Less,
            (_, Value::Byte(_)) => Ordering::Greater,
            (Value::Char(_), _) => Ordering::Less,
            (_, Value::Char(_)) => Ordering::Greater,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(n) => n.fmt(f),
            Value::Byte(b) => b.fmt(f),
            Value::Char(c) => c.fmt(f),
            Value::Func(func) => func.fmt(f),
        }
    }
}
