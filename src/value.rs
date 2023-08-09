use std::{cmp::Ordering, fmt, sync::Arc};

use crate::{
    algorithm::pervade::*, array::*, function::Function, grid_fmt::GridFmt, primitive::Primitive,
    Byte, Uiua, UiuaResult,
};

#[derive(Clone)]
pub enum Value {
    Num(Array<f64>),
    Byte(Array<Byte>),
    Char(Array<char>),
    Func(Array<Arc<Function>>),
}

unsafe fn _value_size() {
    #[cfg(target_pointer_width = "64")]
    let _: [u8; 56] = std::mem::transmute(Value::from(0));
    #[cfg(target_pointer_width = "32")]
    let _: [u8; 32] = std::mem::transmute(Value::from(0));
}

impl Default for Value {
    fn default() -> Self {
        Array::<Byte>::default().into()
    }
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
    pub fn from_row_values(
        values: impl IntoIterator<Item = Value>,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let mut row_values = values.into_iter();
        let Some(mut value) = row_values.next() else {
            return Ok(Value::default());
        };
        let mut count = 1;
        for row in row_values {
            count += 1;
            value = if count == 2 {
                value.couple(row, env)?
            } else {
                value.join_impl(row, false, env)?
            };
        }
        if count == 1 {
            value.shape_mut().insert(0, 1);
        }
        Ok(value)
    }
    pub fn into_rows(self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Self::Num(array) => Box::new(array.into_rows().map(Value::from)),
            Self::Byte(array) => Box::new(array.into_rows().map(Value::from)),
            Self::Char(array) => Box::new(array.into_rows().map(Value::from)),
            Self::Func(array) => Box::new(array.into_rows().map(Value::from)),
        }
    }
    pub fn into_rows_rev(self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Self::Num(array) => Box::new(array.into_rows_rev().map(Value::from)),
            Self::Byte(array) => Box::new(array.into_rows_rev().map(Value::from)),
            Self::Char(array) => Box::new(array.into_rows_rev().map(Value::from)),
            Self::Func(array) => Box::new(array.into_rows_rev().map(Value::from)),
        }
    }
    pub fn into_flat_values(self) -> Box<dyn Iterator<Item = Self>> {
        match self {
            Self::Num(array) => Box::new(array.data.into_iter().map(Value::from)),
            Self::Byte(array) => Box::new(array.data.into_iter().map(Value::from)),
            Self::Char(array) => Box::new(array.data.into_iter().map(Value::from)),
            Self::Func(array) => Box::new(array.data.into_iter().map(Value::from)),
        }
    }
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Num(_) => "number",
            Self::Byte(_) => "byte",
            Self::Char(_) => "character",
            Self::Func(_) => "function",
        }
    }
    pub fn shape(&self) -> &[usize] {
        self.generic_ref(Array::shape, Array::shape, Array::shape, Array::shape)
    }
    pub fn row_count(&self) -> usize {
        self.generic_ref(
            Array::row_count,
            Array::row_count,
            Array::row_count,
            Array::row_count,
        )
    }
    pub fn row_len(&self) -> usize {
        self.generic_ref(
            Array::row_len,
            Array::row_len,
            Array::row_len,
            Array::row_len,
        )
    }
    pub fn flat_len(&self) -> usize {
        self.generic_ref(
            Array::flat_len,
            Array::flat_len,
            Array::flat_len,
            Array::flat_len,
        )
    }
    pub(crate) fn first_dim_zero(&self) -> Self {
        match self {
            Self::Num(array) => array.first_dim_zero().into(),
            Self::Byte(array) => array.first_dim_zero().into(),
            Self::Char(array) => array.first_dim_zero().into(),
            Self::Func(array) => array.first_dim_zero().into(),
        }
    }
    pub fn rank(&self) -> usize {
        self.shape().len()
    }
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.generic_ref(Array::len, Array::len, Array::len, Array::len)
    }
    pub fn shape_mut(&mut self) -> &mut Vec<usize> {
        self.generic_mut(
            |arr| &mut arr.shape,
            |arr| &mut arr.shape,
            |arr| &mut arr.shape,
            |arr| &mut arr.shape,
        )
    }
    pub(crate) fn validate_shape(&self) {
        self.generic_ref(
            Array::validate_shape,
            Array::validate_shape,
            Array::validate_shape,
            Array::validate_shape,
        )
    }
    pub fn truncate(&mut self) {
        self.generic_mut(
            Array::truncate,
            Array::truncate,
            Array::truncate,
            Array::truncate,
        )
    }
    pub fn generic_ref<'a, T: 'a>(
        &'a self,
        n: impl FnOnce(&'a Array<f64>) -> T,
        b: impl FnOnce(&'a Array<Byte>) -> T,
        c: impl FnOnce(&'a Array<char>) -> T,
        f: impl FnOnce(&'a Array<Arc<Function>>) -> T,
    ) -> T {
        match self {
            Self::Num(array) => n(array),
            Self::Byte(array) => b(array),
            Self::Char(array) => c(array),
            Self::Func(array) => f(array),
        }
    }
    pub fn generic_mut<'a, T: 'a>(
        &'a mut self,
        n: impl FnOnce(&'a mut Array<f64>) -> T,
        b: impl FnOnce(&'a mut Array<Byte>) -> T,
        c: impl FnOnce(&'a mut Array<char>) -> T,
        f: impl FnOnce(&'a mut Array<Arc<Function>>) -> T,
    ) -> T {
        match self {
            Self::Num(array) => n(array),
            Self::Byte(array) => b(array),
            Self::Char(array) => c(array),
            Self::Func(array) => f(array),
        }
    }
    pub fn show(&self) -> String {
        match self {
            Self::Num(array) => array.grid_string(),
            Self::Byte(array) => array.grid_string(),
            Self::Char(array) => array.grid_string(),
            Self::Func(array) => array.grid_string(),
        }
    }
    pub fn as_primitive(&self) -> Option<Primitive> {
        if let Value::Func(fs) = self {
            if fs.rank() == 0 {
                return fs.data[0].as_primitive();
            }
        }
        None
    }
    pub(crate) fn as_flipped_primitive(&self) -> Option<(Primitive, bool)> {
        if let Value::Func(fs) = self {
            if fs.rank() == 0 {
                return fs.data[0].as_flipped_primitive();
            }
        }
        None
    }
    pub fn as_indices(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<Vec<isize>> {
        self.as_number_list(env, requirement, |f| f % 1.0 == 0.0, |f| f as isize)
    }
    pub fn as_nat(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<usize> {
        Ok(match self {
            Value::Num(nums) => {
                if nums.rank() > 0 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", nums.rank()))
                    );
                }
                let num = nums.data[0];
                if num < 0.0 {
                    return Err(env.error(format!("{requirement}, but it is negative")));
                }
                if num.fract().abs() > f64::EPSILON {
                    return Err(env.error(format!("{requirement}, but it has a fractional part")));
                }
                num as usize
            }
            Value::Byte(bytes) => {
                if bytes.rank() > 0 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", bytes.rank()))
                    );
                }
                if let Some(b) = bytes.data[0].value() {
                    if b >= 0 {
                        b as usize
                    } else {
                        return Err(env.error(format!("{requirement}, but it is negative")));
                    }
                } else {
                    return Err(env.error(format!("{requirement}, but it is a fill byte")));
                }
            }
            value => {
                return Err(env.error(format!("{requirement}, but it is {}", value.type_name())))
            }
        })
    }
    pub fn as_int(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<isize> {
        Ok(match self {
            Value::Num(nums) => {
                if nums.rank() > 0 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", nums.rank()))
                    );
                }
                let num = nums.data[0];
                if num.fract().abs() > f64::EPSILON {
                    return Err(env.error(format!("{requirement}, but it has a fractional part")));
                }
                num as isize
            }
            Value::Byte(bytes) => {
                if bytes.rank() > 0 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", bytes.rank()))
                    );
                }
                if let Some(b) = bytes.data[0].value() {
                    b as isize
                } else {
                    return Err(env.error(format!("{requirement}, but it is a fill byte")));
                }
            }
            value => {
                return Err(env.error(format!("{requirement}, but it is {}", value.type_name())))
            }
        })
    }
    pub fn as_num(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<f64> {
        Ok(match self {
            Value::Num(nums) => {
                if nums.rank() > 0 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", nums.rank()))
                    );
                }
                nums.data[0]
            }
            Value::Byte(bytes) => {
                if bytes.rank() > 0 {
                    return Err(
                        env.error(format!("{requirement}, but its rank is {}", bytes.rank()))
                    );
                }
                if let Some(b) = bytes.data[0].value() {
                    b as f64
                } else {
                    return Err(env.error(format!("{requirement}, but it is a fill byte")));
                }
            }
            value => {
                return Err(env.error(format!("{requirement}, but it is {}", value.type_name())))
            }
        })
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
                let mut result = Vec::with_capacity(nums.row_count());
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
                let mut result = Vec::with_capacity(bytes.row_count());
                for &byte in bytes.data() {
                    let num = byte.value().map(|b| b as f64).unwrap_or(f64::fill_value());
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
    pub fn into_bytes(self, env: &Uiua, requirement: &'static str) -> UiuaResult<Vec<u8>> {
        Ok(match self {
            Value::Byte(a) => {
                if a.rank() != 1 {
                    return Err(env.error(format!("{requirement}, but its rank is {}", a.rank())));
                }
                a.data
                    .into_iter()
                    .filter_map(|b| b.value().map(|b| b as u8))
                    .collect()
            }
            Value::Num(a) => {
                if a.rank() != 1 {
                    return Err(env.error(format!("{requirement}, but its rank is {}", a.rank())));
                }
                a.data.into_iter().map(|f| f as u8).collect()
            }
            Value::Char(a) => {
                if a.rank() != 1 {
                    return Err(env.error(format!("{requirement}, but its rank is {}", a.rank())));
                }
                a.data.into_iter().collect::<String>().into_bytes()
            }
            value => {
                return Err(env.error(format!(
                    "{requirement}, but its type is {}",
                    value.type_name()
                )))
            }
        })
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
                Self::$variant((shape, data).into())
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
value_from!(Byte, Byte);
value_from!(char, Char);
value_from!(Arc<Function>, Func);

impl FromIterator<usize> for Value {
    fn from_iter<I: IntoIterator<Item = usize>>(iter: I) -> Self {
        iter.into_iter().map(|i| i as f64).collect()
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::from(Byte(b as i16))
    }
}

impl From<usize> for Value {
    fn from(i: usize) -> Self {
        Value::from(i as f64)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        s.chars().collect()
    }
}

impl From<Function> for Value {
    fn from(f: Function) -> Self {
        Arc::new(f).into()
    }
}

impl From<Primitive> for Value {
    fn from(prim: Primitive) -> Self {
        Function::from(prim).into()
    }
}

macro_rules! value_un_impl {
    ($name:ident, $(($variant:ident, $f:ident)),* $(,)?) => {
        impl Value {
            pub fn $name(self, env: &Uiua) -> UiuaResult<Self> {
                Ok(match self {
                    $(Self::$variant(array) => {
                        (array.shape, array.data.into_iter().map($name::$f).collect::<Vec<_>>()).into()
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

value_un_impl_all!(neg, not, abs, sign, sqrt, sin, cos, tan, asin, acos, floor, ceil, round);

macro_rules! value_bin_impl {
    ($name:ident, $(($va:ident, $vb:ident, $f:ident)),* $(,)?) => {
        impl Value {
            pub fn $name(&self, other: &Self, env: &Uiua) -> UiuaResult<Self> {
                Ok(match (self, other) {
                    $((Value::$va(a), Value::$vb(b)) => {
                        bin_pervade(a, b, env, $name::$f)?.into()
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
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);

value_bin_impl!(
    max,
    (Num, Num, num_num),
    (Char, Char, char_char),
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
);

macro_rules! cmp_impls {
    ($($name:ident),*) => {
        $(
            value_bin_impl!(
                $name,
                // Value comparable
                (Num, Num, num_num),
                (Byte, Byte, generic),
                (Char, Char, generic),
                (Num, Byte, num_byte),
                (Byte, Num, byte_num),
                // Type comparable
                (Num, Char, always_less),
                (Num, Func, always_less),
                (Byte, Char, always_less),
                (Byte, Func, always_less),
                (Char, Num, always_greater),
                (Char, Byte, always_greater),
                (Char, Func, always_less),
            );
        )*
    };
}

cmp_impls!(is_eq, is_ne, is_lt, is_le, is_gt, is_ge);

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Byte(a), Value::Byte(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Func(a), Value::Func(b)) => a == b,
            (Value::Num(a), Value::Byte(b)) => a.val_eq(b),
            (Value::Byte(a), Value::Num(b)) => b.val_eq(a),
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
            (Value::Num(a), Value::Num(b)) => a.val_cmp(b),
            (Value::Byte(a), Value::Byte(b)) => a.val_cmp(b),
            (Value::Char(a), Value::Char(b)) => a.val_cmp(b),
            (Value::Func(a), Value::Func(b)) => a.val_cmp(b),
            (Value::Num(a), Value::Byte(b)) => a.val_cmp(b),
            (Value::Byte(a), Value::Num(b)) => b.val_cmp(a).reverse(),
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
