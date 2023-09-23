use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    mem::take,
    sync::Arc,
};

use crate::{
    algorithm::pervade::*,
    array::*,
    function::{Function, Signature},
    grid_fmt::GridFmt,
    primitive::Primitive,
    Uiua, UiuaResult,
};

#[derive(Clone)]
pub enum Value {
    Num(Array<f64>),
    Byte(Array<u8>),
    Char(Array<char>),
    Func(Array<Arc<Function>>),
}

impl Default for Value {
    fn default() -> Self {
        Array::<u8>::default().into()
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
    pub fn signature(&self) -> Signature {
        if let Some(f) = self.as_func_array().and_then(Array::as_scalar) {
            f.signature()
        } else {
            Signature::new(0, 1)
        }
    }
    pub fn as_num_array(&self) -> Option<&Array<f64>> {
        match self {
            Self::Num(array) => Some(array),
            _ => None,
        }
    }
    pub fn as_byte_array(&self) -> Option<&Array<u8>> {
        match self {
            Self::Byte(array) => Some(array),
            _ => None,
        }
    }
    pub fn as_char_array(&self) -> Option<&Array<char>> {
        match self {
            Self::Char(array) => Some(array),
            _ => None,
        }
    }
    pub fn as_func_array(&self) -> Option<&Array<Arc<Function>>> {
        match self {
            Self::Func(array) => Some(array),
            _ => None,
        }
    }
    pub fn into_func_array(self) -> Result<Array<Arc<Function>>, Self> {
        match self {
            Self::Func(array) => Ok(array),
            _ => Err(self),
        }
    }
    pub fn as_function(&self) -> Option<&Arc<Function>> {
        self.as_func_array().and_then(Array::as_scalar)
    }
    pub fn into_function(self) -> Result<Arc<Function>, Self> {
        match self.into_func_array() {
            Ok(array) => array.into_scalar().map_err(Into::into),
            Err(value) => Err(value),
        }
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
    pub fn shape_prefixes_match(&self, other: &Self) -> bool {
        self.shape().iter().zip(other.shape()).all(|(a, b)| a == b)
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
    pub fn format_shape(&self) -> FormatShape {
        self.generic_ref(
            Array::format_shape,
            Array::format_shape,
            Array::format_shape,
            Array::format_shape,
        )
    }
    pub fn rank(&self) -> usize {
        self.shape().len()
    }
    pub fn shape_mut(&mut self) -> &mut Shape {
        match self {
            Self::Num(array) => &mut array.shape,
            Self::Byte(array) => &mut array.shape,
            Self::Char(array) => &mut array.shape,
            Self::Func(array) => &mut array.shape,
        }
    }
    pub(crate) fn validate_shape(&self) {
        self.generic_ref(
            Array::validate_shape,
            Array::validate_shape,
            Array::validate_shape,
            Array::validate_shape,
        )
    }
    pub fn row(&self, i: usize) -> Self {
        self.generic_ref(
            |arr| arr.row(i).into(),
            |arr| arr.row(i).into(),
            |arr| arr.row(i).into(),
            |arr| arr.row(i).into(),
        )
    }
    pub fn generic_into<T>(
        self,
        n: impl FnOnce(Array<f64>) -> T,
        b: impl FnOnce(Array<u8>) -> T,
        c: impl FnOnce(Array<char>) -> T,
        f: impl FnOnce(Array<Arc<Function>>) -> T,
    ) -> T {
        match self {
            Self::Num(array) => n(array),
            Self::Byte(array) => b(array),
            Self::Char(array) => c(array),
            Self::Func(array) => match array.into_constant() {
                Ok(value) => value.generic_into(n, b, c, f),
                Err(array) => f(array),
            },
        }
    }
    pub fn generic_ref<'a, T: 'a>(
        &'a self,
        n: impl FnOnce(&'a Array<f64>) -> T,
        b: impl FnOnce(&'a Array<u8>) -> T,
        c: impl FnOnce(&'a Array<char>) -> T,
        f: impl FnOnce(&'a Array<Arc<Function>>) -> T,
    ) -> T {
        match self {
            Self::Num(array) => n(array),
            Self::Byte(array) => b(array),
            Self::Char(array) => c(array),
            Self::Func(array) => {
                if let Some(value) = array.as_constant() {
                    value.generic_ref(n, b, c, f)
                } else {
                    f(array)
                }
            }
        }
    }
    pub fn generic_ref_env<'a, T: 'a>(
        &'a self,
        n: impl FnOnce(&'a Array<f64>, &Uiua) -> UiuaResult<T>,
        b: impl FnOnce(&'a Array<u8>, &Uiua) -> UiuaResult<T>,
        c: impl FnOnce(&'a Array<char>, &Uiua) -> UiuaResult<T>,
        f: impl FnOnce(&'a Array<Arc<Function>>, &Uiua) -> UiuaResult<T>,
        env: &Uiua,
    ) -> UiuaResult<T> {
        self.generic_ref(|a| n(a, env), |a| b(a, env), |a| c(a, env), |a| f(a, env))
    }
    pub fn generic_mut<T>(
        &mut self,
        n: impl FnOnce(&mut Array<f64>) -> T,
        b: impl FnOnce(&mut Array<u8>) -> T,
        c: impl FnOnce(&mut Array<char>) -> T,
        f: impl FnOnce(&mut Array<Arc<Function>>) -> T,
    ) -> T {
        match self {
            Self::Num(array) => n(array),
            Self::Byte(array) => b(array),
            Self::Char(array) => c(array),
            Self::Func(array) => {
                if let Some(value) = array.as_constant_mut() {
                    value.generic_mut(n, b, c, f)
                } else {
                    f(array)
                }
            }
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
    pub fn as_primitive(&self) -> Option<(Primitive, usize)> {
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
                bytes.data[0] as usize
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
                bytes.data[0] as isize
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
                bytes.data[0] as f64
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
    pub(crate) fn as_number_list<T>(
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
                    let num = byte as f64;
                    if !test(num) {
                        return Err(env.error(requirement));
                    }
                    result.push(convert(num));
                }
                result
            }
            value => {
                return Err(env.error(format!("{requirement}, but it is {}s", value.type_name())))
            }
        })
    }
    pub(crate) fn as_number_array<T: Clone>(
        &self,
        env: &Uiua,
        requirement: &'static str,
        test_shape: fn(&[usize]) -> bool,
        test_num: fn(f64) -> bool,
        convert_num: fn(f64) -> T,
    ) -> UiuaResult<Array<T>> {
        Ok(match self {
            Value::Num(nums) => {
                if !test_shape(self.shape()) {
                    return Err(env.error(format!(
                        "{requirement}, but its shape is {}",
                        nums.format_shape()
                    )));
                }
                let mut result = Vec::with_capacity(nums.flat_len());
                for &num in nums.data() {
                    if !test_num(num) {
                        return Err(env.error(requirement));
                    }
                    result.push(convert_num(num));
                }
                Array::new(self.shape(), result)
            }
            Value::Byte(bytes) => {
                if !test_shape(self.shape()) {
                    return Err(env.error(format!(
                        "{requirement}, but its shape is {}",
                        bytes.format_shape()
                    )));
                }
                let mut result = Vec::with_capacity(bytes.flat_len());
                for &byte in bytes.data() {
                    let num = byte as f64;
                    if !test_num(num) {
                        return Err(env.error(requirement));
                    }
                    result.push(convert_num(num));
                }
                Array::new(self.shape(), result)
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
                a.data.into()
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
    /// Turn a number array into a byte array if no information is lost.
    pub fn compress(&mut self) {
        if let Value::Num(nums) = self {
            if nums
                .data
                .iter()
                .all(|n| n.fract() == 0.0 && *n <= u8::MAX as f64 && *n >= 0.0)
            {
                let mut bytes = Vec::with_capacity(nums.flat_len());
                for n in take(&mut nums.data) {
                    bytes.push(n as u8);
                }
                *self = (take(&mut nums.shape), bytes).into();
            }
        }
    }
    pub fn coerce_to_function(self) -> Array<Arc<Function>> {
        match self {
            Value::Num(arr) => arr.convert_with(|n| Arc::new(Function::constant(n))),
            Value::Byte(arr) => arr.convert_with(|n| Arc::new(Function::constant(n))),
            Value::Char(arr) => arr.convert_with(|n| Arc::new(Function::constant(n))),
            Value::Func(arr) => arr,
        }
    }
    pub fn coerce_as_function(&self) -> Cow<Array<Arc<Function>>> {
        match self {
            Value::Num(arr) => {
                Cow::Owned(arr.convert_ref_with(|n| Arc::new(Function::constant(n))))
            }
            Value::Byte(arr) => {
                Cow::Owned(arr.convert_ref_with(|n| Arc::new(Function::constant(n))))
            }
            Value::Char(arr) => {
                Cow::Owned(arr.convert_ref_with(|n| Arc::new(Function::constant(n))))
            }
            Value::Func(arr) => Cow::Borrowed(arr),
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
        impl From<(Shape, Vec<$ty>)> for Value {
            fn from((shape, data): (Shape, Vec<$ty>)) -> Self {
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
value_from!(Arc<Function>, Func);

impl FromIterator<usize> for Value {
    fn from_iter<I: IntoIterator<Item = usize>>(iter: I) -> Self {
        iter.into_iter().map(|i| i as f64).collect()
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::from(b as u8)
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

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Value::from(i as f64)
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
                    Value::Func(mut array) => {
                        let mut new_data = Vec::with_capacity(array.flat_len());
                        for f in array.data {
                            match Function::into_inner(f).into_constant() {
                                Ok(value) => new_data.push(Arc::new(Function::constant(value.$name(env)?))),
                                Err(_) => return Err($name::error("function", env)),
                            }
                        }
                        array.data = new_data.into();
                        array.into()
                    }
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

macro_rules! val_retry {
    (Byte, $env:expr) => {
        $env.num_fill().is_some()
    };
    ($variant:ident, $env:expr) => {
        false
    };
}

macro_rules! value_bin_impl {
    ($name:ident, $(($va:ident, $vb:ident, $f:ident $(, $retry:ident)?)),* $(,)?) => {
        impl Value {
            #[allow(unreachable_patterns)]
            pub fn $name(&self, other: &Self, env: &Uiua) -> UiuaResult<Self> {
                Ok(match (self, other) {
                    $((Value::$va(a), Value::$vb(b)) => {
                        let res = bin_pervade(a, b, env, InfalliblePervasiveFn::new($name::$f));
                        match res {
                            Ok(arr) => arr.into(),
                            #[allow(unreachable_code, unused_variables)]
                            Err(e) if e.is_fill() && (val_retry!($va, env) || val_retry!($vb, env)) => {
                                $(return bin_pervade(&a.convert_ref(), &b.convert_ref(), env, InfalliblePervasiveFn::new($name::$retry)).map(Into::into);)?
                                return Err(e);
                            }
                            Err(e) => return Err(e),
                        }
                    },)*
                    (Value::Func(a), b) => {
                        match a.as_constant() {
                            Some(a) => Value::$name(a, b, env)?,
                            None => {
                                let b = b.coerce_as_function();
                                bin_pervade(a, &b, env, FalliblePerasiveFn::new(|a: Arc<Function>, b: Arc<Function>, env: &Uiua| {
                                    let a = a.as_constant().ok_or_else(|| env.error("First argument is not a constant function"))?;
                                    let b = b.as_constant().ok_or_else(|| env.error("Second argument is not a constant function"))?;
                                    Ok(Arc::new(Function::constant(Value::$name(a, b, env)?)))
                                }))?.into()
                            }
                        }
                    },
                    (a, Value::Func(b)) => {
                        match b.as_constant() {
                            Some(b) => Value::$name(a, b, env)?,
                            None => {
                                let a = a.coerce_as_function();
                                bin_pervade(&a, b, env, FalliblePerasiveFn::new(|a: Arc<Function>, b: Arc<Function>, env: &Uiua| {
                                    let a = a.as_constant().ok_or_else(|| env.error("First argument is not a constant function"))?;
                                    let b = b.as_constant().ok_or_else(|| env.error("Second argument is not a constant function"))?;
                                    Ok(Arc::new(Function::constant(Value::$name(a, b, env)?)))
                                }))?.into()
                            }
                        }
                    },
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
    (Byte, Byte, byte_byte, num_num),
    (Byte, Char, byte_char),
    (Char, Byte, char_byte),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);

value_bin_impl!(
    sub,
    (Num, Num, num_num),
    (Num, Char, num_char),
    (Char, Char, char_char),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Char, byte_char),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);

value_bin_impl!(
    mul,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);
value_bin_impl!(
    div,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);
value_bin_impl!(
    modulus,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);
value_bin_impl!(
    pow,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);
value_bin_impl!(
    log,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);
value_bin_impl!(atan2, (Num, Num, num_num));

value_bin_impl!(
    min,
    (Num, Num, num_num),
    (Char, Char, char_char),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);

value_bin_impl!(
    max,
    (Num, Num, num_num),
    (Char, Char, char_char),
    (Byte, Byte, byte_byte, num_num),
    (Byte, Num, byte_num, num_num),
    (Num, Byte, num_byte, num_num),
);

macro_rules! cmp_impls {
    ($($name:ident),*) => {
        $(
            value_bin_impl!(
                $name,
                // Value comparable
                (Num, Num, num_num),
                (Byte, Byte, generic, num_num),
                (Char, Char, generic),
                (Num, Byte, num_byte, num_num),
                (Byte, Num, byte_num, num_num),
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
            (Value::Num(a), Value::Byte(b)) => a == b,
            (Value::Byte(a), Value::Num(b)) => a == b,
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
            (Value::Num(a), Value::Byte(b)) => a.partial_cmp(b).unwrap(),
            (Value::Byte(a), Value::Num(b)) => a.partial_cmp(b).unwrap(),
            (Value::Num(_), _) => Ordering::Less,
            (_, Value::Num(_)) => Ordering::Greater,
            (Value::Byte(_), _) => Ordering::Less,
            (_, Value::Byte(_)) => Ordering::Greater,
            (Value::Char(_), _) => Ordering::Less,
            (_, Value::Char(_)) => Ordering::Greater,
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Num(arr) => {
                0u8.hash(state);
                arr.hash(state);
            }
            Value::Byte(arr) => {
                1u8.hash(state);
                arr.hash(state);
            }
            Value::Char(arr) => {
                2u8.hash(state);
                arr.hash(state);
            }
            Value::Func(arr) => {
                3u8.hash(state);
                arr.hash(state);
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(n) => n.fmt(f),
            Value::Byte(b) => b.fmt(f),
            Value::Char(c) => c.fmt(f),
            Value::Func(func) => {
                if let Some(val) = func.as_constant() {
                    val.fmt(f)
                } else {
                    func.fmt(f)
                }
            }
        }
    }
}
