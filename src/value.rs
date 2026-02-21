use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    iter::once,
    mem::{size_of, take},
    ops::{Deref, DerefMut},
};

use ecow::EcoVec;
use serde::*;

use crate::{
    Boxed, Complex, Shape, Uiua, UiuaResult,
    algorithm::{ErrorContext, FillContext, pervade::*},
    array::*,
    cowslice::CowSlice,
    grid_fmt::GridFmt,
};

/// A generic array value
///
/// This enum is used to represent all possible array types.
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
#[repr(C)]
pub enum Value {
    /// Byte array used for some boolean operations and for I/O
    Byte(Array<u8>),
    /// Common number array
    Num(Array<f64>),
    /// Complex number array
    Complex(Array<Complex>),
    /// Common character array
    Char(Array<char>),
    /// Common box array
    Box(Array<Boxed>),
}

impl Default for Value {
    fn default() -> Self {
        Array::<u8>::default().into()
    }
}

const _: () = {
    assert!(
        size_of::<Value>() <= 64,
        "Value is bigger than 64 bytes. This is really really bad for performance!"
    );
};

/// A combination of [`ExactSizeIterator`] and [`DoubleEndedIterator`]
pub trait ExactDoubleIterator: ExactSizeIterator + DoubleEndedIterator {}
impl<T: ExactSizeIterator + DoubleEndedIterator> ExactDoubleIterator for T {}

/// Operate on a value as an array
#[macro_export]
macro_rules! val_as_arr {
    ($input:expr, |$arr:ident| $body:expr) => {
        match $input {
            Value::Num($arr) => $body,
            Value::Byte($arr) => $body,
            Value::Complex($arr) => $body,
            Value::Char($arr) => $body,
            Value::Box($arr) => $body,
        }
    };
    ($input:expr, $f:path) => {
        match $input {
            Value::Num(arr) => $f(arr),
            Value::Byte(arr) => $f(arr),
            Value::Complex(arr) => $f(arr),
            Value::Char(arr) => $f(arr),
            Value::Box(arr) => $f(arr),
        }
    };
    ($input:expr, $env:expr, $f:path) => {
        match $input {
            Value::Num(arr) => $f(arr, $env),
            Value::Byte(arr) => $f(arr, $env),
            Value::Complex(arr) => $f(arr, $env),
            Value::Char(arr) => $f(arr, $env),
            Value::Box(arr) => $f(arr, $env),
        }
    };
}

impl Value {
    /// A NULL pointer value for use in `&ffi`
    pub(crate) fn null() -> Self {
        let mut arr = Array::<u8>::default();
        arr.meta.pointer = Some(MetaPtr::null());
        Value::from(arr)
    }
    #[allow(dead_code)]
    pub(crate) fn ffi_pointer(ptr: &MetaPtr) -> Self {
        let mut arr = Array::<u8>::default();
        arr.meta.pointer = Some(ptr.clone());
        Value::from(arr)
    }
    pub(crate) fn builder(capacity: usize) -> ValueBuilder {
        ValueBuilder::with_capacity(capacity)
    }
    pub(crate) fn type_id(&self) -> u8 {
        match self {
            Self::Num(_) => f64::TYPE_ID,
            Self::Byte(_) => u8::TYPE_ID,
            Self::Complex(_) => Complex::TYPE_ID,
            Self::Char(_) => char::TYPE_ID,
            Self::Box(_) => Boxed::TYPE_ID,
        }
    }
    /// Get a reference to a possible number array
    pub fn as_num_array(&self) -> Option<&Array<f64>> {
        match self {
            Self::Num(array) => Some(array),
            _ => None,
        }
    }
    /// Get a reference to a possible byte array
    pub fn as_byte_array(&self) -> Option<&Array<u8>> {
        match self {
            Self::Byte(array) => Some(array),
            _ => None,
        }
    }
    /// Get a reference to a possible character array
    pub fn as_char_array(&self) -> Option<&Array<char>> {
        match self {
            Self::Char(array) => Some(array),
            _ => None,
        }
    }
    /// Get a reference to a possible box array
    pub fn as_box_array(&self) -> Option<&Array<Boxed>> {
        match self {
            Self::Box(array) => Some(array),
            _ => None,
        }
    }
    /// Get a reference to a possible scalar box
    pub fn as_box(&self) -> Option<&Boxed> {
        self.as_box_array().and_then(Array::as_scalar)
    }
    /// Get an iterator over the rows of the value
    pub fn rows(&self) -> Box<dyn ExactSizeIterator<Item = Self> + '_> {
        if self.shape.first() == Some(&1) {
            let mut row = self.clone();
            row.undo_fix();
            Box::new(once(row))
        } else {
            val_as_arr!(self, |array| Box::new(array.rows().map(Value::from)))
        }
    }
    pub(crate) fn depth_rows(&self, depth: usize) -> Box<dyn ExactSizeIterator<Item = Self> + '_> {
        val_as_arr!(self, |a| Box::new(a.depth_rows(depth).map(Value::from)))
    }
    pub(crate) fn row_shaped_slice(&self, index: usize, row_shape: Shape) -> Self {
        val_as_arr!(self, |a| a.row_shaped_slice(index, row_shape).into())
    }
    /// Get an iterator over the rows of the value that have the given shape
    pub fn row_shaped_slices(
        &self,
        row_shape: Shape,
    ) -> Box<dyn ExactSizeIterator<Item = Self> + '_> {
        val_as_arr!(self, |array| Box::new(
            array.row_shaped_slices(row_shape).map(Value::from)
        ))
    }
    /// Get an iterator over the rows of the value that have the given shape
    pub fn into_row_shaped_slices(
        self,
        row_shape: Shape,
    ) -> Box<dyn DoubleEndedIterator<Item = Self>> {
        val_as_arr!(self, |array| Box::new(
            array.into_row_shaped_slices(row_shape).map(Value::from)
        ))
    }
    /// Consume the value and get an iterator over its rows
    pub fn into_rows(mut self) -> Box<dyn ExactDoubleIterator<Item = Self>> {
        if self.shape.first() == Some(&1) {
            self.undo_fix();
            Box::new(once(self))
        } else {
            val_as_arr!(self, |array| Box::new(array.into_rows().map(Value::from)))
        }
    }
    /// Get an iterator over the elements of the value
    pub fn elements(&self) -> Box<dyn ExactSizeIterator<Item = Self> + '_> {
        val_as_arr!(self, |array| Box::new(
            array.data.iter().cloned().map(Value::from)
        ))
    }
    /// Cosume the value and get an iterator over its elements
    pub fn into_elements(self) -> Box<dyn Iterator<Item = Self>> {
        val_as_arr!(self, |array| Box::new(
            array.data.into_iter().map(Value::from)
        ))
    }
    /// Get the value's type name
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Num(_) => "number",
            Self::Byte(_) => "number",
            Self::Complex(_) => "complex",
            Self::Char(_) => "character",
            Self::Box(_) => "box",
        }
    }
    /// Get a plural form of the value's type name
    pub fn type_name_plural(&self) -> &'static str {
        match self {
            Self::Num(_) => "numbers",
            Self::Byte(_) => "numbers",
            Self::Complex(_) => "complexes",
            Self::Char(_) => "characters",
            Self::Box(_) => "boxes",
        }
    }
    /// Get the number of rows
    #[inline(always)]
    pub fn row_count(&self) -> usize {
        self.shape.row_count()
    }
    /// Get the number of element in each row
    #[inline(always)]
    pub fn row_len(&self) -> usize {
        self.shape.row_len()
    }
    pub(crate) fn proxy_scalar(&self, env: &Uiua) -> Self {
        match self {
            Self::Num(_) => (env.scalar_fill().map(|fv| fv.value))
                .unwrap_or_else(|_| f64::proxy())
                .into(),
            Self::Byte(_) => (env.scalar_fill().map(|fv| fv.value))
                .unwrap_or_else(|_| u8::proxy())
                .into(),
            Self::Complex(_) => (env.scalar_fill().map(|fv| fv.value))
                .unwrap_or_else(|_| Complex::proxy())
                .into(),
            Self::Char(_) => (env.scalar_fill().map(|fv| fv.value))
                .unwrap_or_else(|_| char::proxy())
                .into(),
            Self::Box(_) => (env.scalar_fill().map(|fv| fv.value))
                .unwrap_or_else(|_| Boxed::proxy())
                .into(),
        }
    }
    pub(crate) fn proxy_row(&self, env: &Uiua) -> Self {
        if self.rank() == 0 {
            return self.proxy_scalar(env);
        }
        let shape: Shape = self.shape[1..].into();
        let elem_count = shape.iter().product();
        match self {
            Self::Num(_) => Array::new(
                shape,
                CowSlice::from_elem(
                    env.scalar_fill()
                        .map(|fv| fv.value)
                        .unwrap_or_else(|_| f64::proxy()),
                    elem_count,
                ),
            )
            .into(),
            Self::Byte(_) => Array::new(
                shape,
                CowSlice::from_elem(
                    env.scalar_fill()
                        .map(|fv| fv.value)
                        .unwrap_or_else(|_| u8::proxy()),
                    elem_count,
                ),
            )
            .into(),
            Self::Complex(_) => Array::new(
                shape,
                CowSlice::from_elem(
                    env.scalar_fill()
                        .map(|fv| fv.value)
                        .unwrap_or_else(|_| Complex::proxy()),
                    elem_count,
                ),
            )
            .into(),
            Self::Char(_) => Array::new(
                shape,
                CowSlice::from_elem(
                    env.scalar_fill()
                        .map(|fv| fv.value)
                        .unwrap_or_else(|_| char::proxy()),
                    elem_count,
                ),
            )
            .into(),
            Self::Box(_) => Array::new(
                shape,
                CowSlice::from_elem(
                    env.scalar_fill()
                        .map(|fv| fv.value)
                        .unwrap_or_else(|_| Boxed::proxy()),
                    elem_count,
                ),
            )
            .into(),
        }
    }
    pub(crate) fn fill(&mut self, env: &Uiua) -> Result<Value, &'static str> {
        self.match_fill(env);
        match self {
            Value::Num(_) => env.array_fill::<f64>().map(|fv| fv.value).map(Into::into),
            Value::Byte(_) => env.array_fill::<u8>().map(|fv| fv.value).map(Into::into),
            Value::Complex(_) => env
                .array_fill::<Complex>()
                .map(|fv| fv.value)
                .map(Into::into),
            Value::Char(_) => env.array_fill::<char>().map(|fv| fv.value).map(Into::into),
            Value::Box(_) => env.array_fill::<Boxed>().map(|fv| fv.value).map(Into::into),
        }
    }
    pub(crate) fn first_dim_zero(&self) -> Self {
        val_as_arr!(self, |array| array.first_dim_zero().into())
    }
    /// Get the rank
    #[inline(always)]
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    pub(crate) fn pop_row(&mut self) -> Option<Self> {
        val_as_arr!(self, |array| array.pop_row().map(Value::from))
    }
    pub(crate) fn elem_size(&self) -> usize {
        match self {
            Self::Num(_) => size_of::<f64>(),
            Self::Byte(_) => size_of::<u8>(),
            Self::Complex(_) => size_of::<Complex>(),
            Self::Char(_) => size_of::<char>(),
            Self::Box(_) => size_of::<Boxed>(),
        }
    }
    /// Or with reversed sorted flags
    pub fn or_sorted_flags_rev(&mut self, mut flags: ArrayFlags) {
        if let Value::Box(_) = self {
            self.meta.take_sorted_flags();
            return;
        }
        flags &= ArrayFlags::SORTEDNESS;
        if flags == ArrayFlags::NONE {
            return;
        }
        flags.reverse_sorted();
        self.meta.flags |= flags;
        if (self.meta.is_sorted_up() || self.meta.is_sorted_down())
            && let Value::Num(arr) = self
            && arr.data.iter().any(|n| n.is_nan())
        {
            arr.meta.take_sorted_flags();
        }
    }
}

/// A representation of a [`Value`] that allows direct access to the
/// inner [`Array`]'s shape and metadata, regardless of element type
#[repr(C)]
pub struct ValueRepr {
    /// Safety: Do not access this field!
    __discriminant: u8,
    /// The value's shape
    pub shape: Shape,
    /// Safety: Do not access this field!
    __data: CowSlice<f64>,
    /// The value's metadata
    pub meta: ArrayMeta,
}

#[cfg(test)]
#[test]
fn value_repr_correctness() {
    assert_eq!(size_of::<Value>(), size_of::<ValueRepr>());
    assert_eq!(align_of::<Value>(), align_of::<ValueRepr>());
}

impl Deref for Value {
    type Target = ValueRepr;
    fn deref(&self) -> &Self::Target {
        // Safety: The layout of `Value` should always match that of `Value`
        unsafe { &*(self as *const Self as *const ValueRepr) }
    }
}

impl DerefMut for Value {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: The layout of `Value` should always match that of `Value`
        unsafe { &mut *(self as *mut Self as *mut ValueRepr) }
    }
}

impl Value {
    /// Get the value's metadata
    pub fn meta(&self) -> &ArrayMeta {
        &self.meta
    }
    /// Get a mutable reference to the value's metadata
    pub fn meta_mut(&mut self) -> &mut ArrayMeta {
        &mut self.meta
    }
    /// Add a 1-length dimension to the front of the array's shape
    pub fn fix(&mut self) {
        self.fix_depth(0);
    }
    pub(crate) fn fix_depth(&mut self, depth: usize) {
        let depth = depth.min(self.rank());
        self.shape.fix_depth(depth);
        if depth == 0
            && let Some(keys) = self.meta.map_keys_mut()
        {
            keys.fix();
        }
    }
    /// Set the sortedness flags according to the value's data
    pub fn derive_sortedness(&mut self) {
        val_as_arr!(self, |array| array.derive_sortedness());
    }
    /// Remove a 1-length dimension from the front of the array's shape
    pub fn unfix(&mut self, env: &Uiua) -> UiuaResult {
        if let Some(keys) = self.meta.map_keys_mut() {
            keys.unfix();
        }
        self.shape.unfix().map_err(|e| env.error(e))?;
        self.meta.take_sorted_flags();
        self.validate();
        Ok(())
    }
    /// Collapse the top two dimensions of the array's shape
    pub fn undo_fix(&mut self) {
        if let Some(keys) = self.meta.map_keys_mut()
            && !keys.unfix()
        {
            self.meta.take_map_keys();
        }
        _ = self.shape.unfix();
        self.meta.take_sorted_flags();
        self.validate();
    }
    /// Ensure the value's invariants are upheld. Only runs in debug mode.
    #[track_caller]
    pub(crate) fn validate(&self) {
        val_as_arr!(self, |arr| arr.validate());
    }
    /// Get the row at the given index
    #[track_caller]
    pub fn row(&self, i: usize) -> Self {
        val_as_arr!(self, |arr| arr.row(i).into())
    }
    #[track_caller]
    pub(crate) fn depth_row(&self, depth: usize, i: usize) -> Self {
        val_as_arr!(self, |arr| arr.depth_row(depth, i).into())
    }
    #[track_caller]
    /// Create an value that is a slice of this values's rows
    ///
    /// Generally doesn't allocate
    ///
    /// - `start` must be <= `end`
    /// - `start` must be < `self.row_count()`
    /// - `end` must be <= `self.row_count()`
    pub(crate) fn slice_rows(&self, start: usize, end: usize) -> Self {
        val_as_arr!(self, |arr| arr.slice_rows(start, end).into())
    }
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn generic_bin_into<T, E>(
        self,
        other: Self,
        n: impl FnOnce(Array<f64>, Array<f64>) -> Result<T, E>,
        _b: impl FnOnce(Array<u8>, Array<u8>) -> Result<T, E>,
        _co: impl FnOnce(Array<Complex>, Array<Complex>) -> Result<T, E>,
        ch: impl FnOnce(Array<char>, Array<char>) -> Result<T, E>,
        f: impl FnOnce(Array<Boxed>, Array<Boxed>) -> Result<T, E>,
        err: impl FnOnce(Self, Self) -> E,
    ) -> Result<T, E> {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => n(a, b),
            (Self::Byte(a), Self::Byte(b)) => _b(a, b),
            (Self::Byte(a), Self::Num(b)) => n(a.convert(), b),
            (Self::Num(a), Self::Byte(b)) => n(a, b.convert()),
            (Self::Complex(a), Self::Complex(b)) => _co(a, b),
            (Self::Complex(a), Self::Num(b)) => _co(a, b.convert()),
            (Self::Num(a), Self::Complex(b)) => _co(a.convert(), b),
            (Self::Complex(a), Self::Byte(b)) => _co(a, b.convert()),
            (Self::Byte(a), Self::Complex(b)) => _co(a.convert(), b),
            (Self::Char(a), Self::Char(b)) => ch(a, b),
            (Self::Box(a), Self::Box(b)) => f(a, b),
            (Self::Box(a), b) => f(a, b.coerce_to_boxes()),
            (a, Self::Box(b)) => f(a.coerce_to_boxes(), b),
            (a, b) => Err(err(a, b)),
        }
    }
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn generic_bin_ref<T, E>(
        &self,
        other: &Self,
        n: impl FnOnce(&Array<f64>, &Array<f64>) -> Result<T, E>,
        _b: impl FnOnce(&Array<u8>, &Array<u8>) -> Result<T, E>,
        _co: impl FnOnce(&Array<Complex>, &Array<Complex>) -> Result<T, E>,
        ch: impl FnOnce(&Array<char>, &Array<char>) -> Result<T, E>,
        f: impl FnOnce(&Array<Boxed>, &Array<Boxed>) -> Result<T, E>,
        err: impl FnOnce(&Self, &Self) -> E,
    ) -> Result<T, E> {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => n(a, b),
            (Self::Byte(a), Self::Byte(b)) => _b(a, b),
            (Self::Byte(a), Self::Num(b)) => n(&a.convert_ref(), b),
            (Self::Num(a), Self::Byte(b)) => n(a, &b.convert_ref()),
            (Self::Complex(a), Self::Complex(b)) => _co(a, b),
            (Self::Complex(a), Self::Num(b)) => _co(a, &b.convert_ref()),
            (Self::Num(a), Self::Complex(b)) => _co(&a.convert_ref(), b),
            (Self::Complex(a), Self::Byte(b)) => _co(a, &b.convert_ref()),
            (Self::Byte(a), Self::Complex(b)) => _co(&a.convert_ref(), b),
            (Self::Char(a), Self::Char(b)) => ch(a, b),
            (Self::Box(a), Self::Box(b)) => f(a, b),
            (Self::Box(a), b) => f(a, &b.coerce_as_boxes()),
            (a, Self::Box(b)) => f(&a.coerce_as_boxes(), b),
            (a, b) => Err(err(a, b)),
        }
    }
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn generic_bin_mut<T, E>(
        &mut self,
        other: Self,
        n: impl FnOnce(&mut Array<f64>, Array<f64>) -> Result<T, E>,
        _b: impl FnOnce(&mut Array<u8>, Array<u8>) -> Result<T, E>,
        _co: impl FnOnce(&mut Array<Complex>, Array<Complex>) -> Result<T, E>,
        ch: impl FnOnce(&mut Array<char>, Array<char>) -> Result<T, E>,
        f: impl FnOnce(&mut Array<Boxed>, Array<Boxed>) -> Result<T, E>,
        err: impl FnOnce(&Self, &Self) -> E,
    ) -> Result<T, E> {
        match (&mut *self, other) {
            (Self::Num(a), Self::Num(b)) => n(a, b),
            (Self::Byte(a), Self::Byte(b)) => _b(a, b),
            (Self::Byte(a), Self::Num(b)) => {
                let mut a_num = a.convert_ref();
                let res = n(&mut a_num, b);
                *self = a_num.into();
                res
            }
            (Self::Num(a), Self::Byte(b)) => n(a, b.convert_ref()),
            (Self::Complex(a), Self::Complex(b)) => _co(a, b),
            (Self::Complex(a), Self::Num(b)) => _co(a, b.convert_ref()),
            (Self::Num(a), Self::Complex(b)) => {
                let mut a_comp = a.convert_ref();
                let res = _co(&mut a_comp, b);
                *self = a_comp.into();
                res
            }
            (Self::Complex(a), Self::Byte(b)) => _co(a, b.convert_ref()),
            (Self::Byte(a), Self::Complex(b)) => {
                let mut a_comp = a.convert_ref();
                let res = _co(&mut a_comp, b);
                *self = a_comp.into();
                res
            }
            (Self::Char(a), Self::Char(b)) => ch(a, b),
            (Self::Box(a), b) => f(a, b.coerce_to_boxes()),
            (a, Self::Box(b)) => {
                let mut a_box = take(a).coerce_to_boxes();
                let res = f(&mut a_box, b);
                *self = a_box.into();
                res
            }
            (a, b) => Err(err(a, &b)),
        }
    }
    /// Ensure that the capacity is at least `min`
    pub(crate) fn reserve_min(&mut self, min: usize) {
        val_as_arr!(self, |arr| arr.data.reserve_min(min))
    }
    /// Get the pretty-printed string representation of the value that appears in output
    pub fn show(&self) -> String {
        self.grid_string(true)
    }
    /// Get the pretty-printed string representation of the value that appears when formatted
    pub fn format(&self) -> String {
        fn recur(val: &Value, qoute: bool) -> String {
            if val.is_map() {
                let mut s = "{".to_string();
                for (i, (k, v)) in val.map_kv().into_iter().enumerate() {
                    if i > 0 {
                        s.push(' ');
                    }
                    s.push_str(&recur(&k, true));
                    s.push('→');
                    s.push_str(&recur(&v, true));
                }
                s.push('}');
                return s;
            }
            match val {
                Value::Num(arr) if arr.rank() == 0 => arr.data[0].to_string(),
                Value::Byte(arr) if arr.rank() == 0 => arr.data[0].to_string(),
                Value::Complex(arr) if arr.rank() == 0 => arr.data[0].to_string(),
                Value::Char(arr) if arr.rank() < 2 => {
                    let mut s: String = arr.data.iter().collect();
                    if qoute {
                        s = format!("{s:?}");
                    }
                    s
                }
                Value::Box(arr) if arr.rank() == 0 => recur(&arr.data[0].0, qoute),
                value if value.rank() > 0 => {
                    let mut s = "[".to_string();
                    for (i, row) in value.rows().enumerate() {
                        if i > 0 {
                            s.push(' ');
                        }
                        s.push_str(&recur(&row, true));
                    }
                    s.push(']');
                    s
                }
                value => value.to_string(),
            }
        }
        recur(self, false)
    }
}

pub(crate) trait ScalarNum: Copy {
    fn from_u8(u: u8) -> Result<Self, FromU8Error>;
    fn from_f64(f: f64) -> Result<Self, FromF64Error>;
}

pub(crate) enum FromU8Error {
    NonBoolean,
}

pub(crate) enum FromF64Error {
    NaN,
    Infinite,
    TooHigh,
    TooLow,
    NonInteger,
    NonBoolean,
}

impl fmt::Display for FromU8Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FromU8Error::NonBoolean => write!(f, "not a boolean"),
        }
    }
}

impl fmt::Display for FromF64Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FromF64Error::NaN => write!(f, "present"),
            FromF64Error::Infinite => write!(f, "infinite"),
            FromF64Error::TooHigh => write!(f, "too high"),
            FromF64Error::TooLow => write!(f, "too low"),
            FromF64Error::NonInteger => write!(f, "not an integer"),
            FromF64Error::NonBoolean => write!(f, "not a boolean"),
        }
    }
}

impl ScalarNum for usize {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(u as usize)
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        if f.is_nan() {
            Err(FromF64Error::NaN)
        } else if f.is_infinite() {
            Err(FromF64Error::Infinite)
        } else if f.fract() != 0.0 {
            Err(FromF64Error::NonInteger)
        } else if f > usize::MAX as f64 {
            Err(FromF64Error::TooHigh)
        } else if f < 0.0 {
            Err(FromF64Error::TooLow)
        } else {
            Ok(f as usize)
        }
    }
}

impl ScalarNum for isize {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(u as isize)
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        if f.is_nan() {
            Err(FromF64Error::NaN)
        } else if f.is_infinite() {
            Err(FromF64Error::Infinite)
        } else if f.fract() != 0.0 {
            Err(FromF64Error::NonInteger)
        } else if f > isize::MAX as f64 {
            Err(FromF64Error::TooHigh)
        } else if f < isize::MIN as f64 {
            Err(FromF64Error::TooLow)
        } else {
            Ok(f as isize)
        }
    }
}

impl ScalarNum for i64 {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(u as i64)
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        if f.is_nan() {
            Err(FromF64Error::NaN)
        } else if f.is_infinite() {
            Err(FromF64Error::Infinite)
        } else if f > i64::MAX as f64 {
            Err(FromF64Error::TooHigh)
        } else if f < i64::MIN as f64 {
            Err(FromF64Error::TooLow)
        } else if f.fract() != 0.0 {
            Err(FromF64Error::NonInteger)
        } else {
            Ok(f as i64)
        }
    }
}

impl ScalarNum for Result<isize, bool> {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(Ok(u as isize))
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        if f.is_nan() {
            Err(FromF64Error::NaN)
        } else if f.is_infinite() {
            Ok(Err(f.is_sign_negative()))
        } else if f > isize::MAX as f64 {
            Err(FromF64Error::TooHigh)
        } else if f < isize::MIN as f64 {
            Err(FromF64Error::TooLow)
        } else if f.fract() != 0.0 {
            Err(FromF64Error::NonInteger)
        } else {
            Ok(Ok(f as isize))
        }
    }
}

impl ScalarNum for Option<isize> {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(Some(u as isize))
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        Result::<isize, bool>::from_f64(f).map(Result::ok)
    }
}

impl ScalarNum for Option<usize> {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(Some(u as usize))
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        if f == f64::INFINITY {
            Ok(None)
        } else {
            usize::from_f64(f).map(Some)
        }
    }
}

impl ScalarNum for u8 {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(u)
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        if f.is_nan() {
            Err(FromF64Error::NaN)
        } else if f.is_infinite() {
            Err(FromF64Error::Infinite)
        } else if f > u8::MAX as f64 {
            Err(FromF64Error::TooHigh)
        } else if f < 0.0 {
            Err(FromF64Error::TooLow)
        } else if f.fract() != 0.0 {
            Err(FromF64Error::NonInteger)
        } else {
            Ok(f as u8)
        }
    }
}

impl ScalarNum for u16 {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(u as u16)
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        if f.is_nan() {
            Err(FromF64Error::NaN)
        } else if f.is_infinite() {
            Err(FromF64Error::Infinite)
        } else if f > u16::MAX as f64 {
            Err(FromF64Error::TooHigh)
        } else if f < 0.0 {
            Err(FromF64Error::TooLow)
        } else if f.fract() != 0.0 {
            Err(FromF64Error::NonInteger)
        } else {
            Ok(f as u16)
        }
    }
}

impl ScalarNum for bool {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        match u {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(FromU8Error::NonBoolean),
        }
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        match f {
            0.0 => Ok(false),
            1.0 => Ok(true),
            _ => Err(FromF64Error::NonBoolean),
        }
    }
}

impl ScalarNum for f64 {
    fn from_u8(u: u8) -> Result<Self, FromU8Error> {
        Ok(u as f64)
    }
    fn from_f64(f: f64) -> Result<Self, FromF64Error> {
        Ok(f)
    }
}

impl Value {
    /// Attempt to convert the array to a list of integers
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_ints<C: ErrorContext>(
        &self,
        ctx: &C,
        requirement: impl Into<Option<&'static str>>,
    ) -> Result<Vec<isize>, C::Error> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be array of integers");
        self.as_number_list(ctx, requirement)
    }
    pub(crate) fn as_ints_or_infs(
        &self,
        env: &Uiua,
        requirement: &'static str,
    ) -> UiuaResult<Vec<Result<isize, bool>>> {
        self.as_number_list(env, requirement)
    }
    /// Attempt to convert the array to a single boolean
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_bool(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<bool> {
        let requirement = requirement.into().unwrap_or("Expected value to be boolean");
        self.as_number(env, requirement)
    }
    /// Attempt to convert the array to a single natural number
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_nat(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<usize> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be a natural number");
        self.as_number(env, requirement)
    }
    pub(crate) fn as_nat_or_inf(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Option<usize>> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be a natural number or infinity");
        self.as_number(env, requirement)
    }
    /// Attempt to convert the array to a single integer
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_int<C: ErrorContext>(
        &self,
        ctx: &C,
        requirement: impl Into<Option<&'static str>>,
    ) -> Result<isize, C::Error> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be an integer");
        self.as_number(ctx, requirement)
    }
    pub(crate) fn as_int_or_inf(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Result<isize, bool>> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be an integer or infinity");
        self.as_number(env, requirement)
    }
    /// Attempt to convert the array to a single number
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_num(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<f64> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be a number");
        self.as_number(env, requirement)
    }
    /// Attempt to convert the array to a list of numbers
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_nums<'a, C: ErrorContext>(
        &'a self,
        ctx: &C,
        requirement: impl Into<Option<&'static str>>,
    ) -> Result<Cow<'a, [f64]>, C::Error> {
        if self.rank() <= 1
            && let Value::Num(arr) = self
        {
            return Ok(Cow::Borrowed(arr.data.as_slice()));
        }
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be array of numbers");
        self.as_number_list(ctx, requirement).map(Cow::Owned)
    }
    /// Attempt to convert the array to a list of natural numbers
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_nats<C: ErrorContext>(
        &self,
        ctx: &C,
        requirement: impl Into<Option<&'static str>>,
    ) -> Result<Vec<usize>, C::Error> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be array of natural numbers");
        if let Value::Num(arr) = self
            && let Some(&(mut n)) =
                (arr.data.iter()).find(|&&n| n > usize::MAX as f64 && n.fract() == 0.0)
        {
            let power = n.log10().floor() as i32;
            n /= 10f64.powi(power);
            return Err(ctx.error(format!("{requirement}, but {n}e{power} is too large")));
        }
        self.as_number_list(ctx, requirement)
    }
    /// Attempt to convert the array to a list of bytes
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_bytes<'a>(
        &'a self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Cow<'a, [u8]>> {
        if self.rank() <= 1
            && let Value::Byte(arr) = self
        {
            return Ok(Cow::Borrowed(arr.data.as_slice()));
        }
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be array of bytes");
        self.as_number_list(env, requirement).map(Cow::Owned)
    }
    /// Attempt to convert the array to a list of u16s
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_u16s(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Vec<u16>> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be array of unsigned 16-bit integers");
        self.as_number_list(env, requirement)
    }
    /// Attempt to convert the array to a list of booleans
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_bools(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Vec<bool>> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be array of booleans");
        self.as_number_list(env, requirement)
    }
    /// Attempt to convert the array to a list of integers or infinity
    ///
    /// `None` represents infinity.
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_rank_list(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Vec<Option<isize>>> {
        let requirement = requirement
            .into()
            .unwrap_or("Elements of rank list must be integers or infinity");
        self.as_number_list(env, requirement)
    }
    fn as_number<T, C>(&self, ctx: &C, requirement: &'static str) -> Result<T, C::Error>
    where
        T: ScalarNum,
        C: ErrorContext,
    {
        if self.rank() != 0 {
            return Err(ctx.error(format!("{requirement}, but its rank is {}", self.rank())));
        }
        Ok(match self {
            Value::Num(nums) => {
                let n = nums.data[0];
                T::from_f64(n).map_err(|e| {
                    ctx.error(format!(
                        "{requirement}, but {} is {e}",
                        n.grid_string(false)
                    ))
                })?
            }
            Value::Byte(bytes) => {
                let n = bytes.data[0];
                T::from_u8(n).map_err(|e| {
                    ctx.error(format!(
                        "{requirement}, but {} is {e}",
                        n.grid_string(false)
                    ))
                })?
            }
            value => {
                return Err(ctx.error(format!("{requirement}, but it is a {}", value.type_name())));
            }
        })
    }
    pub(crate) fn as_number_list<T, C>(
        &self,
        ctx: &C,
        requirement: &'static str,
    ) -> Result<Vec<T>, C::Error>
    where
        T: ScalarNum,
        C: ErrorContext,
    {
        Ok(match self {
            Value::Num(nums) => {
                if nums.rank() > 1 {
                    return Err(
                        ctx.error(format!("{requirement}, but its rank is {}", nums.rank()))
                    );
                }
                let mut result = Vec::with_capacity(nums.row_count());
                for &num in &nums.data {
                    result.push(T::from_f64(num).map_err(|e| {
                        ctx.error(format!(
                            "{requirement}, but {} is {e}",
                            num.grid_string(false)
                        ))
                    })?);
                }
                result
            }
            Value::Byte(bytes) => {
                if bytes.rank() > 1 {
                    return Err(
                        ctx.error(format!("{requirement}, but its rank is {}", bytes.rank()))
                    );
                }
                let mut result = Vec::with_capacity(bytes.row_count());
                for &byte in &bytes.data {
                    result.push(
                        T::from_u8(byte)
                            .map_err(|e| ctx.error(format!("{requirement}, but {byte} is {e}")))?,
                    );
                }
                result
            }
            value => {
                return Err(ctx.error(format!(
                    "{requirement}, but it is {}",
                    value.type_name_plural()
                )));
            }
        })
    }
    pub(crate) fn as_integer_array(
        &self,
        env: &Uiua,
        requirement: &'static str,
    ) -> UiuaResult<Array<isize>> {
        self.as_number_array(env, requirement)
    }
    pub(crate) fn as_natural_array(
        &self,
        env: &Uiua,
        requirement: &'static str,
    ) -> UiuaResult<Array<usize>> {
        if let Value::Num(arr) = self
            && let Some(&(mut n)) =
                (arr.data.iter()).find(|&&n| n > usize::MAX as f64 && n.fract() == 0.0)
        {
            let power = n.log10().floor() as i32;
            n /= 10f64.powi(power);
            return Err(env.error(format!(
                "{requirement}, but {}e{power} is too large",
                n.grid_string(false)
            )));
        }
        self.as_number_array(env, requirement)
    }
    pub(crate) fn as_number_array<T: ScalarNum>(
        &self,
        env: &Uiua,
        requirement: &'static str,
    ) -> UiuaResult<Array<T>> {
        Ok(match self {
            Value::Num(nums) => {
                let mut result = EcoVec::with_capacity(nums.element_count());
                for &num in &nums.data {
                    result.push(T::from_f64(num).map_err(|e| {
                        env.error(format!(
                            "{requirement}, but {} is {e}",
                            num.grid_string(false)
                        ))
                    })?);
                }
                Array::new(self.shape.clone(), result)
            }
            Value::Byte(bytes) => {
                let mut result = EcoVec::with_capacity(bytes.element_count());
                for &byte in &bytes.data {
                    result.push(
                        T::from_u8(byte)
                            .map_err(|e| env.error(format!("{requirement}, but {byte} is {e}")))?,
                    );
                }
                Array::new(self.shape.clone(), result)
            }
            value => {
                return Err(env.error(format!(
                    "{requirement}, but its type is {}",
                    value.type_name()
                )));
            }
        })
    }
    /// Attempt to convert the array to a string
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_string(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<String> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be a string");
        match (self, self.rank()) {
            (Value::Char(chars), 0 | 1) => Ok(chars.data.iter().collect()),
            (Value::Char(_), n) => Err(env.error(format!("{requirement}, but its rank is {n}"))),
            (Value::Box(boxes), 0) => boxes.data[0].0.as_string(env, requirement),
            (val, _) => Err(env.error(format!(
                "{requirement}, but it is {}",
                val.type_name_plural()
            ))),
        }
    }
    /// Attempt to convert the array to a string
    pub fn as_string_opt(&self) -> Option<String> {
        match (self, self.rank()) {
            (Value::Char(chars), 0 | 1) => Some(chars.data.iter().collect()),
            (Value::Box(boxes), 0) => boxes.data[0].0.as_string_opt(),
            _ => None,
        }
    }
    /// Attempt to convert the array to a list of strings
    ///
    /// A rank-1 character array is treated as a single string.
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_strings(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Vec<String>> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be a string or list of strings");

        match (self, self.rank()) {
            (Value::Char(chars), 0 | 1) => Ok(vec![chars.data.iter().collect()]),
            (Value::Char(chars), 2) => {
                let mut strings = Vec::with_capacity(self.row_count());
                for row in chars.row_slices() {
                    strings.push(row.iter().copied().collect());
                }
                Ok(strings)
            }
            (Value::Char(_), n) => Err(env.error(format!(
                "{requirement}, but it is a rank-{n} character array"
            ))),
            (Value::Box(boxes), 0) => boxes.data[0].0.as_strings(env, requirement),
            (Value::Box(boxes), 1) => {
                let mut strings = Vec::with_capacity(boxes.row_count());
                for Boxed(val) in &boxes.data {
                    strings.push(val.as_string(env, requirement)?);
                }
                Ok(strings)
            }
            (Value::Box(_), n) => {
                Err(env.error(format!("{requirement}, but it is a rank-{n} box array")))
            }
            (val, _) => Err(env.error(format!(
                "{requirement}, but it is {}",
                val.type_name_plural()
            ))),
        }
    }
    /// Attempt to convert the array to a list of bytes
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn into_bytes(
        self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Vec<u8>> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be a list of bytes");
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
                )));
            }
        })
    }
    /// Remove all top-level layers of boxing
    pub fn unpack(&mut self) {
        if let Value::Box(arr) = self {
            *self = match take(arr).into_unboxed() {
                Ok(value) => value.unpacked(),
                Err(arr) => Value::Box(arr),
            };
        }
    }
    /// Remove all top-level layers of boxing
    pub fn unpacked(self) -> Self {
        match self {
            Self::Box(arr) => match arr.into_unboxed() {
                Ok(value) => value.unpacked(),
                Err(arr) => Self::Box(arr),
            },
            value => value,
        }
    }
    pub(crate) fn unpacked_ref(&self) -> &Self {
        match self {
            Self::Box(arr) => match arr.as_unboxed() {
                Some(value) => value.unpacked_ref(),
                None => self,
            },
            value => value,
        }
    }
    /// Apply a function to the highest-level unboxed value
    pub fn map_boxed(self, f: impl FnOnce(Self) -> Self) -> Self {
        match self {
            Value::Box(boxed) => match boxed.into_scalar() {
                Ok(scalar) => Boxed(scalar.0.map_boxed(f)).into(),
                Err(boxed) => f(Value::Box(boxed)),
            },
            val => f(val),
        }
    }
    /// Apply a function to the highest-level unboxed value
    pub fn try_map_boxed(self, f: impl FnOnce(Self) -> UiuaResult<Self>) -> UiuaResult<Self> {
        match self {
            Value::Box(boxed) => match boxed.into_scalar() {
                Ok(scalar) => scalar.0.try_map_boxed(f).map(Boxed).map(Value::from),
                Err(boxed) => f(Value::Box(boxed)),
            },
            val => f(val),
        }
    }
    /// Remove a single layer of boxing
    pub fn unbox(&mut self) {
        if let Value::Box(boxed) = self
            && boxed.rank() == 0
        {
            *self = take(&mut boxed.data.as_mut_slice()[0].0);
        }
    }
    /// Remove a single layer of boxing
    pub fn unboxed(self) -> Self {
        match self {
            Value::Box(boxed) => match boxed.into_scalar() {
                Ok(scalar) => scalar.0,
                Err(boxed) => Value::Box(boxed),
            },
            val => val,
        }
    }
    /// Remove a single layer of boxing if the condition is met
    pub fn unboxed_if(self, unbox: bool) -> Self {
        if unbox { self.unboxed() } else { self }
    }
    /// Box the value if the condition is met
    pub fn boxed_if(self, do_box: bool) -> Self {
        if do_box { Boxed(self).into() } else { self }
    }
    /// Turn the value into a scalar box if it is not one already
    pub fn box_if_not(&mut self) {
        match &mut *self {
            Value::Box(arr) if arr.rank() == 0 => {}
            val => *self = Value::Box(Array::from(Boxed(take(val)))),
        }
    }
    /// Turn the value into a scalar box
    pub fn box_it(&mut self) {
        *self = Value::Box(Array::from(Boxed(take(self))))
    }
    /// Turn the value into a scalar box if it is not one already
    pub fn boxed_if_not(self) -> Boxed {
        match self {
            Value::Box(arr) if arr.rank() == 0 => arr.data.into_iter().next().unwrap(),
            val => Boxed(val),
        }
    }
    /// Turn a number array into a byte array if no information is lost.
    ///
    /// Also sets the boolean flag if the array contains only 0s and 1s.
    pub fn try_shrink(&mut self) {
        match self {
            Value::Num(nums) => {
                let mut compress = true;
                let mut boolean = true;
                for &n in &nums.data {
                    if n.fract() != 0.0 || n.is_sign_negative() || n > u8::MAX as f64 {
                        compress = false;
                        boolean = false;
                        break;
                    }
                    if n > 1.0 {
                        boolean = false;
                    }
                }
                if compress {
                    let meta = take(&mut nums.meta);
                    let mut bytes = EcoVec::with_capacity(nums.element_count());
                    for n in take(&mut nums.data) {
                        bytes.push(n as u8);
                    }
                    let mut arr = Array::new(take(&mut nums.shape), bytes);
                    arr.meta = meta;
                    if boolean {
                        arr.meta.flags.set(ArrayFlags::BOOLEAN, true);
                    }
                    *self = arr.into();
                }
            }
            Value::Byte(bytes) => {
                let mut boolean = true;
                for &b in &bytes.data {
                    if b > 1 {
                        boolean = false;
                        break;
                    }
                }
                if boolean {
                    bytes.meta.flags.set(ArrayFlags::BOOLEAN, true);
                }
            }
            _ => {}
        }
    }
    /// Convert to a box array by boxing every element
    pub fn coerce_to_boxes(self) -> Array<Boxed> {
        if self.rank() == 0 && !matches!(self, Value::Box(_)) {
            return Boxed(self).into();
        }
        match self {
            Value::Num(arr) => arr.convert_with(|v| Boxed(Value::from(v))),
            Value::Byte(arr) => arr.convert_with(|v| Boxed(Value::from(v))),
            Value::Complex(arr) => arr.convert_with(|v| Boxed(Value::from(v))),
            Value::Char(arr) => arr.convert_with(|v| Boxed(Value::from(v))),
            Value::Box(arr) => arr,
        }
    }
    /// Convert to a box array by boxing every element
    pub fn coerce_as_boxes(&self) -> Cow<Array<Boxed>> {
        if self.rank() == 0 {
            return match self {
                Value::Box(arr) => Cow::Borrowed(arr),
                val => Cow::Owned(Boxed(val.clone()).into()),
            };
        }
        match self {
            Value::Num(arr) => Cow::Owned(arr.convert_ref_with(|v| Boxed(Value::from(v)))),
            Value::Byte(arr) => Cow::Owned(arr.convert_ref_with(|v| Boxed(Value::from(v)))),
            Value::Complex(arr) => Cow::Owned(arr.convert_ref_with(|v| Boxed(Value::from(v)))),
            Value::Char(arr) => Cow::Owned(arr.convert_ref_with(|v| Boxed(Value::from(v)))),
            Value::Box(arr) => Cow::Borrowed(arr),
        }
    }
    /// Propagate a value's label across an operation
    pub fn keep_label(mut self, f: impl FnOnce(Self) -> UiuaResult<Self>) -> UiuaResult<Self> {
        let label = self.meta.take_label();
        let mut result = f(self)?;
        if let Some(label) = label {
            result.meta.label = Some(label);
        }
        Ok(result)
    }
    /// Propagate values' labels across an operation
    pub fn keep_labels(
        mut self,
        mut other: Self,
        f: impl FnOnce(Self, Self) -> UiuaResult<Self>,
    ) -> UiuaResult<Self> {
        let label = self.meta.take_label();
        let other_label = other.meta.take_label();
        let mut result = f(self, other)?;
        match (label, other_label) {
            (Some(label), None) | (None, Some(label)) => {
                result.meta.label = Some(label);
            }
            (Some(a), Some(b)) if a == b => {
                result.meta.label = Some(a);
            }
            _ => {}
        }
        Ok(result)
    }
    /// Propagate a value's map keys across an operation
    pub fn keep_map_key(mut self, f: impl FnOnce(Self) -> UiuaResult<Self>) -> UiuaResult<Self> {
        let keys = self.meta.take_map_keys();
        let mut result = f(self)?;
        if let Some(keys) = keys {
            result.meta.map_keys = Some(keys);
        }
        Ok(result)
    }
    /// Propagate values' map keys across an operation
    pub fn keep_map_keys(
        mut self,
        mut other: Self,
        f: impl FnOnce(Self, Self) -> UiuaResult<Self>,
    ) -> UiuaResult<Self> {
        let keys = self.meta.take_map_keys();
        let other_keys = other.meta.take_map_keys();
        let mut result = f(self, other)?;
        if let Some(keys) = keys.xor(other_keys) {
            result.meta.map_keys = Some(keys);
        }
        Ok(result)
    }
    /// Propagate a value's uncorruptable metadata across an operation
    pub fn keep_meta(self, f: impl FnOnce(Self) -> UiuaResult<Self>) -> UiuaResult<Self> {
        self.keep_label(|val| val.keep_map_key(f))
    }
    /// Propagate values' uncorruptable metadata across an operation
    pub fn keep_metas(
        self,
        other: Self,
        f: impl FnOnce(Self, Self) -> UiuaResult<Self>,
    ) -> UiuaResult<Self> {
        self.keep_labels(other, |a, b| a.keep_map_keys(b, f))
    }
    pub(crate) fn match_fill<C: FillContext>(&mut self, ctx: &C) {
        if let Value::Byte(arr) = self {
            if arr.meta.flags.is_boolean() && ctx.scalar_fill::<f64>().is_ok() {
                arr.meta.flags.remove(ArrayFlags::BOOLEAN);
            }
            if ctx.number_only_fill() {
                let shape = take(&mut arr.shape);
                let meta = take(&mut arr.meta);
                let data: EcoVec<f64> = take(&mut arr.data).into_iter().map(|b| b as f64).collect();
                let mut array = Array::new(shape, data);
                array.meta = meta;
                *self = array.into();
            }
        }
    }
    pub(crate) fn has_wildcard(&self) -> bool {
        val_as_arr!(self, |arr| arr.data.iter().any(ArrayValue::has_wildcard))
    }
    pub(crate) fn box_nesting(&self) -> usize {
        let Value::Box(arr) = self else {
            return 0;
        };
        (arr.data.iter())
            .map(|Boxed(v)| v.box_nesting())
            .max()
            .unwrap_or(0)
            + 1
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
        impl From<EcoVec<$ty>> for Value {
            fn from(vec: EcoVec<$ty>) -> Self {
                Self::$variant(Array::from(vec))
            }
        }
        impl<const N: usize> From<[$ty; N]> for Value {
            fn from(array: [$ty; N]) -> Self {
                Self::$variant(Array::from_iter(array))
            }
        }
        impl<const M: usize, const N: usize> From<[[$ty; N]; M]> for Value {
            fn from(array: [[$ty; N]; M]) -> Self {
                let data: EcoVec<$ty> = array.into_iter().flatten().collect();
                Self::$variant(Array::new([M, N], data))
            }
        }
        impl From<CowSlice<$ty>> for Value {
            fn from(vec: CowSlice<$ty>) -> Self {
                Self::$variant(Array::from(vec))
            }
        }
        impl From<(Shape, EcoVec<$ty>)> for Value {
            fn from((shape, data): (Shape, EcoVec<$ty>)) -> Self {
                Self::$variant(Array::new(shape, data))
            }
        }
        impl From<(Shape, CowSlice<$ty>)> for Value {
            fn from((shape, data): (Shape, CowSlice<$ty>)) -> Self {
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
value_from!(Boxed, Box);
value_from!(Complex, Complex);

impl FromIterator<usize> for Value {
    fn from_iter<I: IntoIterator<Item = usize>>(iter: I) -> Self {
        iter.into_iter().map(|i| i as f64).collect()
    }
}

impl FromIterator<String> for Value {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        iter.into_iter().map(|s| Boxed(s.into())).collect()
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::from(Array::<u8>::from(b))
    }
}

impl From<usize> for Value {
    fn from(i: usize) -> Self {
        Value::from(i as f64)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::from(i as f64)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        s.chars().collect()
    }
}

impl<'a> From<&'a str> for Value {
    fn from(s: &'a str) -> Self {
        s.chars().collect()
    }
}

impl<'a> From<&'a [&str]> for Value {
    fn from(s: &'a [&str]) -> Self {
        Value::from(Array::<Boxed>::from_row_arrays_infallible(
            s.iter().map(|&s| Boxed(Value::from(s)).into()),
        ))
    }
}

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Value::from(i as f64)
    }
}

impl From<Vec<u8>> for Value {
    fn from(vec: Vec<u8>) -> Self {
        let mut data = CowSlice::new();
        data.extend_from_vec(vec);
        Self::from(data)
    }
}

impl<const M: usize, const N: usize> From<[[i32; N]; M]> for Value {
    fn from(array: [[i32; N]; M]) -> Self {
        let data: EcoVec<f64> = array.into_iter().flatten().map(|n| n as f64).collect();
        Self::Num(Array::new([M, N], data))
    }
}

impl<const N: usize> From<[i32; N]> for Value {
    fn from(array: [i32; N]) -> Self {
        let data: EcoVec<f64> = array.into_iter().map(|n| n as f64).collect();
        Self::Num(Array::new(N, data))
    }
}

macro_rules! value_mon_impl {
    (
        $name:ident,
        $(
            $([$(|$meta:ident| $pred:expr,)* $in_place:ident, $f:ident])?
            $(($make_new:ident, $f2:ident))?
        ),*
        $(|$sorted_val:ident, $sorted_flags:ident| $sorted_body:expr)?
    ) => {
        impl Value {
            #[doc(hidden)]
            #[allow(unused_mut, clippy::redundant_closure_call)]
            pub fn $name(mut self, env: &Uiua) -> UiuaResult<Self> {
                let _sorted_flags = self.meta.take_sorted_flags();
                let mut val: Value = self.keep_meta(|val| Ok(match val {
                    $($(Self::$in_place(mut array) $(if (|$meta: &ArrayMeta| $pred)(&array.meta))* => {
                        for val in &mut array.data {
                            *val = $name::$f(*val);
                        }
                        array.into()
                    },)*)*
                    $($(Self::$make_new(array) => {
                        let mut new = EcoVec::with_capacity(array.element_count());
                        for val in array.data {
                            new.push($name::$f2(val));
                        }
                        (array.shape, new).into()
                    },)*)*
                    Value::Box(mut array) => {
                        let mut new_data = EcoVec::with_capacity(array.element_count());
                        for b in array.data {
                            new_data.push(Boxed(b.0.$name(env)?));
                        }
                        array.data = new_data.into();
                        array.into()
                    }
                    #[allow(unreachable_patterns)]
                    val => return Err($name::error(val.type_name(), env))
                }))?;
                $((|$sorted_val: &mut Value, $sorted_flags| $sorted_body)(&mut val, _sorted_flags);)?
                val.validate();
                Ok(val)
            }
        }
    }
}

value_mon_impl!(
    scalar_neg,
    [Num, num],
    (Byte, byte),
    [Complex, com],
    [Char, char]
);
value_mon_impl!(
    not,
    [Num, num],
    [|meta| meta.flags.is_boolean(), Byte, bool],
    (Byte, byte),
    [Complex, com],
    |val, flags| val.or_sorted_flags_rev(flags)
);
value_mon_impl!(
    scalar_abs,
    [Num, num],
    (Byte, byte),
    (Complex, com),
    [Char, char]
);
value_mon_impl!(
    sign,
    [Num, num],
    [Byte, byte],
    [Complex, com],
    (Char, char),
    |val, flags| if val.rank() < 2 && !matches!(val, Value::Complex(_)) {
        val.meta.or_sorted_flags(flags)
    }
);
value_mon_impl!(recip, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(
    sqrt,
    [Num, num],
    [|meta| meta.flags.is_boolean(), Byte, bool],
    (Byte, byte),
    [Complex, com]
);
value_mon_impl!(exp, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(ln, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(sin, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(cos, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(asin, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(acos, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(
    floor,
    [Num, num],
    [Byte, byte],
    [Complex, com],
    |val, flags| if val.rank() < 2 {
        val.meta.or_sorted_flags(flags)
    }
);
value_mon_impl!(
    ceil,
    [Num, num],
    [Byte, byte],
    [Complex, com],
    |val, flags| if val.rank() < 2 {
        val.meta.or_sorted_flags(flags)
    }
);
value_mon_impl!(
    round,
    [Num, num],
    [Byte, byte],
    [Complex, com],
    |val, flags| if val.rank() < 2 {
        val.meta.or_sorted_flags(flags)
    }
);
value_mon_impl!(complex_re, [Num, generic], [Byte, generic], (Complex, com));
value_mon_impl!(complex_im, [Num, num], [Byte, byte], (Complex, com));
value_mon_impl!(exp2, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(exp10, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(log2, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(log10, [Num, num], (Byte, byte), [Complex, com]);
value_mon_impl!(square_abs, [Num, num], (Byte, byte), (Complex, com));
value_mon_impl!(
    neg_abs,
    [Num, num],
    (Byte, byte),
    (Complex, com),
    (Char, char)
);

impl Value {
    /// Get the `absolute value` of a value
    pub fn abs(self, env: &Uiua) -> UiuaResult<Self> {
        match self {
            Value::Char(mut chars) if chars.rank() == 1 && env.scalar_fill::<char>().is_ok() => {
                chars.data = (chars.data.into_iter())
                    .flat_map(|c| c.to_uppercase())
                    .collect();
                chars.shape = chars.data.len().into();
                Ok(chars.into())
            }
            Value::Char(mut chars) if chars.rank() > 1 && env.scalar_fill::<char>().is_ok() => {
                let meta = chars.meta.get_mut().map(take);
                let mut rows = Vec::new();
                for row in chars.row_shaped_slices(Shape::from(*chars.shape.last().unwrap())) {
                    rows.push(Array::<char>::from_iter(
                        row.data.iter().flat_map(|c| c.to_uppercase()),
                    ));
                }
                let mut arr = Array::from_row_arrays(rows, env)?;
                let last = arr.shape.pop().unwrap();
                arr.shape = chars.shape;
                *arr.shape.last_mut().unwrap() = last;
                arr.meta = meta.unwrap_or_default();
                Ok(arr.into())
            }
            value => value.scalar_abs(env),
        }
    }
    /// `negate` a value
    pub fn neg(mut self, env: &Uiua) -> UiuaResult<Self> {
        let sorted_flags = self.meta.take_sorted_flags();
        let mut val = match self {
            Value::Char(mut chars) if chars.rank() == 1 && env.scalar_fill::<char>().is_ok() => {
                let mut new_data = EcoVec::with_capacity(chars.data.len());
                for c in chars.data {
                    if c.is_uppercase() {
                        new_data.extend(c.to_lowercase());
                    } else {
                        new_data.extend(c.to_uppercase());
                    }
                }
                chars.data = new_data.into();
                chars.shape = chars.data.len().into();
                chars.into()
            }
            Value::Char(chars) if chars.rank() > 1 && env.scalar_fill::<char>().is_ok() => {
                let mut rows = Vec::new();
                for row in chars.row_shaped_slices(Shape::from(*chars.shape.last().unwrap())) {
                    let mut new_data = EcoVec::with_capacity(row.data.len());
                    for c in row.data {
                        if c.is_uppercase() {
                            new_data.extend(c.to_lowercase());
                        } else {
                            new_data.extend(c.to_uppercase());
                        }
                    }
                    rows.push(Array::from(new_data));
                }
                let mut arr = Array::from_row_arrays(rows, env)?;
                let last = arr.shape.pop().unwrap();
                arr.shape = chars.shape;
                *arr.shape.last_mut().unwrap() = last;
                arr.into()
            }
            value => value.scalar_neg(env)?,
        };
        val.or_sorted_flags_rev(sorted_flags);
        Ok(val)
    }
    /// Raise a value to a power
    pub fn pow(self, base: Self, env: &Uiua) -> UiuaResult<Self> {
        if let Ok(pow) = self.as_int(env, None) {
            match pow {
                1 => return Ok(base),
                2 => return base.clone().mul(base, env),
                -1 => return base.div(Value::from(1), env),
                _ => {}
            }
        }
        self.scalar_pow(base, env)
    }
}

fn optimize_types(a: Value, b: Value) -> (Value, Value) {
    match (a, b) {
        (Value::Num(a), Value::Byte(b)) if a.element_count() > b.element_count() => {
            (a.into(), b.convert::<f64>().into())
        }
        (Value::Byte(a), Value::Num(b)) if a.element_count() < b.element_count() => {
            (a.convert::<f64>().into(), b.into())
        }
        (a, b) => (a, b),
    }
}

/// Macro to generate a dyadic pervasive function on [`Value`]s.
macro_rules! value_dy_impl {
    (
        $name:ident,
        $(
            $(($na:ident, $nb:ident, $f1:ident))*
            $([$(|$meta:ident| $pred:expr,)* $ip:ident, $f2:ident $(, $reset_value_flags:literal)?])*
        ),*
        $({
            get_pre: |$get_pre_a:ident, $get_pre_b:ident, $get_pre_left:ident| $get_pre_body:expr,
            handle_pre: |$pre_a:ident: $pre_ty:ty, $pre_b:ident, $handle_val:ident| $handle_body:expr,
        })?
    ) => {
        impl Value {
            #[doc(hidden)]
            #[allow(unreachable_patterns, unused_assignments, unused_mut, clippy::wrong_self_convention)]
            pub fn $name(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
                let (mut a, mut b) = optimize_types(self, other);
                let mut handle_pre: Option<&dyn Fn(&mut Value)> = None;
                a.match_fill(env);
                b.match_fill(env);
                $(
                    let get_pre = |$get_pre_a: &mut Value, $get_pre_b: &Value, $get_pre_left: bool| $get_pre_body;
                    let pre_a = get_pre(&mut a, &b, false);
                    let pre_b = get_pre(&mut b, &a, true);
                    let f = move |val: &mut Value| {
                        (|$pre_a: $pre_ty, $pre_b: $pre_ty, $handle_val: &mut Value| $handle_body)(pre_a, pre_b, val)
                    };
                    if env.fill().value_for(&a).is_none() && env.fill().value_for(&b).is_none() {
                        handle_pre = Some(&f);
                    }
                )?
                a.meta.take_sorted_flags();
                b.meta.take_sorted_flags();
                let mut val = a.keep_metas(b, |a, b| { Ok(match (a, b) {
                    $($((Value::$ip(mut a), Value::$ip(mut b)) $(if {
                        let f = |$meta: &ArrayMeta| $pred;
                        f(&a.meta) && f(&b.meta)
                    })* => {
                        bin_pervade_mut(a, &mut b, true, env, $name::$f2)?;
                        let mut val: Value = b.into();
                        $(if $reset_value_flags {
                            val.meta.take_value_flags();
                        })*
                        if let Some(handle_pre) = handle_pre {
                            handle_pre(&mut val);
                        }
                        val
                    },)*)*
                    $($((Value::$na(a), Value::$nb(b)) => {
                        let mut val: Value = bin_pervade(a, b, env, InfalliblePervasiveFn::new($name::$f1))?.into();
                        val.meta.take_value_flags();
                        if let Some(handle_pre) = handle_pre {
                            handle_pre(&mut val);
                        }
                        val
                    },)*)*
                    (Value::Box(a), Value::Box(b)) => {
                        let mut val: Value = bin_pervade(a, b, env, FalliblePerasiveFn::new(|a: Boxed, b: Boxed, env: &Uiua| {
                            Ok(Boxed(Value::$name(a.0, b.0, env)?))
                        }))?.into();
                        val.meta.take_value_flags();
                        if let Some(handle_pre) = handle_pre {
                            handle_pre(&mut val);
                        }
                        val
                    }
                    (Value::Box(a), b) => {
                        let b = b.coerce_as_boxes().into_owned();
                        let mut val: Value = bin_pervade(a, b, env, FalliblePerasiveFn::new(|a: Boxed, b: Boxed, env: &Uiua| {
                            Ok(Boxed(Value::$name(a.0, b.0, env)?))
                        }))?.into();
                        val.meta.take_value_flags();
                        if let Some(handle_pre) = handle_pre {
                            handle_pre(&mut val);
                        }
                        val
                    },
                    (a, Value::Box(b)) => {
                        let a = a.coerce_as_boxes().into_owned();
                        let mut val: Value = bin_pervade(a, b, env, FalliblePerasiveFn::new(|a: Boxed, b: Boxed, env: &Uiua| {
                            Ok(Boxed(Value::$name(a.0, b.0, env)?))
                        }))?.into();
                        val.meta.take_value_flags();
                        if let Some(handle_pre) = handle_pre {
                            handle_pre(&mut val);
                        }
                        val
                    },
                    (a, b) => return Err($name::error(a.type_name(), b.type_name(), env)),
                })})?;
                val.validate();
                Ok(val)
            }
        }
    };
}

/// Macro to generate a dyadic pervasive math function on [`Value`]s.
macro_rules! value_dy_math_impl {
    // The generated function will maintain the sortedness of
    // the result if one of the inputs is a scalar number.
    // The $left parameter determines whether the scalar is the left argument.
    ($name:ident $(,($($tt:tt)*))? , maintain_scalar_sortedness$(($left:expr))?) => {
        value_dy_math_impl!(
            $name
            $(,($($tt)*))?,
            pre {
                get_pre: |a, b, _left| {
                    if b.shape != [] || b.type_id() != f64::TYPE_ID {
                        return None;
                    }
                    let mut flags = a.meta.take_sorted_flags();
                    $(if _left != $left {
                        flags.reverse_sorted();
                    })?
                    Some(flags)
                },
                handle_pre: |a: Option<ArrayFlags>, b, val| {
                    if let Some(flags) = a.or(b) {
                        val.meta.or_sorted_flags(flags);
                    }
                },
            }
        );
    };
    // The generated function will maintain the sortedness of
    // the result if both of the inputs have the same sortedness.
    ($name:ident $(,($($tt:tt)*))? , maintain_both_sortedness) => {
        value_dy_math_impl!(
            $name
            $(,($($tt)*))?,
            pre {
                get_pre: |a, b, _left| {
                    if a.type_id() != f64::TYPE_ID || b.type_id() != f64::TYPE_ID {
                        return None;
                    }
                    let a_flags = a.meta.take_sorted_flags();
                    Some(if b.shape == [] {
                        a_flags
                    } else {
                        a_flags & (b.meta.flags & ArrayFlags::SORTEDNESS)
                    })
                },
                handle_pre: |a: Option<ArrayFlags>, b, val| {
                    if let Some(flags) = a.or(b) {
                        val.meta.or_sorted_flags(flags);
                    }
                },
            }
        );
    };
    // The generated function will maintain the sortedness of
    // the result if one of the inputs is a scalar number,
    // reversing the sortedness if the number is negative.
    // The $left parameter determines whether the scalar is the left argument.
    ($name:ident $(,($($tt:tt)*))? , signed_scalar_sortedness$(($left:expr))?) => {
        value_dy_math_impl!(
            $name
            $(,($($tt)*))?,
            pre {
                get_pre: |a, b, _left| {
                    let negative = match b {
                        Value::Num(arr) if arr.shape == [] => {
                            if arr.data[0].is_infinite() || arr.data[0].is_nan() {
                                return None;
                            }
                            arr.data[0] < 0.0
                        },
                        Value::Byte(arr) if arr.shape == [] => false,
                        _ => return None,
                    };
                    let mut flags = a.meta.take_sorted_flags();
                    if negative {
                        flags.reverse_sorted();
                    }
                    $(if _left != $left {
                        flags.reverse_sorted();
                    })?
                    Some(flags)
                },
                handle_pre: |a: Option<ArrayFlags>, b, val| {
                    if let Some(flags) = a.or(b) {
                        val.meta.or_sorted_flags(flags);
                    }
                    if val.meta.is_sorted_up() || val.meta.is_sorted_down() {
                        if let Value::Num(arr) =  val {
                            if arr.data.iter().any(|&n| n.is_nan()) {
                                arr.meta.take_sorted_flags();
                            }
                        }
                    }
                },
            }
        );
    };
    ($name:ident $(,($($tt:tt)*))? $(,pre {$($after:tt)*})?) => {
        value_dy_impl!(
            $name,
            $($($tt)*)?
            [Num, num_num],
            (Byte, Byte, byte_byte),
            (Byte, Num, byte_num),
            (Num, Byte, num_byte),
            [Complex, com_x],
            (Complex, Num, com_x),
            (Num, Complex, x_com),
            (Complex, Byte, com_x),
            (Byte, Complex, x_com),
            $({$($after)*})?
        );
    };
}

value_dy_math_impl!(
    add,
    (
        (Num, Char, num_char),
        (Char, Num, char_num),
        (Byte, Char, byte_char),
        (Char, Byte, char_byte),
        [|meta| meta.flags.is_boolean(), Byte, bool_bool, true],
    ),
    maintain_both_sortedness
);
value_dy_math_impl!(
    sub,
    (
        (Num, Char, num_char),
        (Char, Char, char_char),
        (Byte, Char, byte_char),
    ),
    maintain_scalar_sortedness(true)
);
value_dy_math_impl!(
    mul,
    (
        (Num, Char, num_char),
        (Char, Num, char_num),
        (Byte, Char, byte_char),
        (Char, Byte, char_byte),
        [|meta| meta.flags.is_boolean(), Byte, bool_bool],
    ),
    signed_scalar_sortedness
);
value_dy_math_impl!(
    set_sign,
    (
        (Num, Char, num_char),
        (Char, Num, char_num),
        (Byte, Char, byte_char),
        (Char, Byte, char_byte),
    )
);
value_dy_math_impl!(
    div,
    ((Num, Char, num_char), (Byte, Char, byte_char)),
    signed_scalar_sortedness(true)
);
value_dy_math_impl!(modulo, ((Complex, Complex, com_com)));
value_dy_math_impl!(or, ([|meta| meta.flags.is_boolean(), Byte, bool_bool]));
value_dy_math_impl!(scalar_pow);
value_dy_math_impl!(root);
value_dy_math_impl!(log);
value_dy_math_impl!(atan2);
value_dy_math_impl!(
    min,
    (
        [Char, generic],
        (Box, Box, generic),
        [|meta| meta.flags.is_boolean(), Byte, bool_bool],
    ),
    maintain_both_sortedness
);
value_dy_math_impl!(
    max,
    (
        [Char, generic],
        (Box, Box, generic),
        [|meta| meta.flags.is_boolean(), Byte, bool_bool],
    ),
    maintain_both_sortedness
);

value_dy_impl!(
    complex,
    (Num, Num, num_num),
    (Byte, Byte, byte_byte),
    (Byte, Num, byte_num),
    (Num, Byte, num_byte),
    [Complex, com_x],
    (Complex, Num, com_x),
    (Num, Complex, x_com),
    (Complex, Byte, com_x),
    (Byte, Complex, x_com),
);

value_dy_impl!(
    abs_complex,
    [Num, num],
    (Byte, Byte, num),
    (Byte, Num, num),
    (Num, Byte, num),
    (Complex, Complex, com),
    (Complex, Num, com),
    (Num, Complex, com),
    (Complex, Byte, com),
    (Byte, Complex, com),
);

macro_rules! eq_impls {
    ($($name:ident),*) => {
        $(
            value_dy_impl!(
                $name,
                // Value comparable
                [Num, same_type],
                (Complex, Complex, com_x),
                (Box, Box, generic),
                [Byte, same_type],
                (Char, Char, generic),
                (Num, Byte, num_byte),
                (Byte, Num, byte_num),
                (Complex, Num, com_x),
                (Num, Complex, x_com),
                (Complex, Byte, com_x),
                (Byte, Complex, x_com),
                // Type comparable
                (Num, Char, always_less),
                (Byte, Char, always_less),
                (Complex, Char, always_less),
                (Char, Num, always_greater),
                (Char, Byte, always_greater),
                (Char, Complex, always_greater),
            );
        )*
    };
}

macro_rules! cmp_impls {
    ($($name:ident),*) => {
        $(
            value_dy_impl!(
                $name,
                // Value comparable
                [Num, same_type],
                [Complex, com_x],
                (Box, Box, generic),
                [Byte, same_type],
                (Char, Char, generic),
                (Num, Byte, num_byte),
                (Byte, Num, byte_num),
                (Complex, Num, com_x),
                (Num, Complex, x_com),
                (Complex, Byte, com_x),
                (Byte, Complex, x_com),
                // Type comparable
                (Num, Char, always_less),
                (Byte, Char, always_less),
                (Complex, Char, always_less),
                (Char, Num, always_greater),
                (Char, Byte, always_greater),
                (Char, Complex, always_greater),
            );
        )*
    };
}

eq_impls!(is_eq, is_ne);
cmp_impls!(other_is_lt, other_is_le, other_is_gt, other_is_ge);

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if let Some(a) = &self.meta.pointer {
            return other.meta.pointer.as_ref().is_some_and(|b| a == b);
        }
        if other.meta.pointer.is_some() {
            return false;
        }
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Byte(a), Value::Byte(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Complex(a), Value::Complex(b)) => a == b,
            (Value::Box(a), Value::Box(b)) => a == b,
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
        let type_order = self.type_id().cmp(&other.type_id());
        if type_order != Ordering::Equal {
            return type_order;
        }
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.cmp(b),
            (Value::Byte(a), Value::Byte(b)) => a.cmp(b),
            (Value::Complex(a), Value::Complex(b)) => a.cmp(b),
            (Value::Char(a), Value::Char(b)) => a.cmp(b),
            (Value::Box(a), Value::Box(b)) => a.cmp(b),
            (Value::Num(a), Value::Byte(b)) => a.partial_cmp(b).unwrap(),
            (Value::Byte(a), Value::Num(b)) => a.partial_cmp(b).unwrap(),
            (Value::Num(_), _) => Ordering::Less,
            (_, Value::Num(_)) => Ordering::Greater,
            (Value::Byte(_), _) => Ordering::Less,
            (_, Value::Byte(_)) => Ordering::Greater,
            (Value::Complex(_), _) => Ordering::Less,
            (_, Value::Complex(_)) => Ordering::Greater,
            (Value::Char(_), _) => Ordering::Less,
            (_, Value::Char(_)) => Ordering::Greater,
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        val_as_arr!(self, |arr| arr.hash(hasher))
    }
}

#[derive(Clone)]
/// Hash values including things that don't affect their actual value
pub(crate) struct AestheticHash<V = Value>(pub V);

impl PartialEq for AestheticHash<Value> {
    fn eq(&self, other: &Self) -> bool {
        AestheticHash(&self.0) == AestheticHash(&other.0)
    }
}
impl Eq for AestheticHash<Value> {}
impl Hash for AestheticHash<Value> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        AestheticHash(&self.0).hash(hasher)
    }
}

impl PartialEq for AestheticHash<&Value> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
            && self.0.meta.label == other.0.meta.label
            && match (&self.0, &other.0) {
                (Value::Num(a), Value::Num(b)) => (a.data.iter().zip(&b.data))
                    .all(|(a, b)| a.is_sign_positive() == b.is_sign_positive()),
                _ => true,
            }
    }
}
impl Eq for AestheticHash<&Value> {}
impl Hash for AestheticHash<&Value> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.meta.label.hash(hasher);
        match self.0 {
            Value::Num(arr) => {
                arr.hash(hasher);
                let signs_as_bytes: Vec<u8> = arr
                    .data
                    .iter()
                    .map(|n| u8::from(n.is_sign_positive()))
                    .collect();
                hasher.write(&signs_as_bytes);
            }
            Value::Box(arr) => {
                if let Some(keys) = &arr.meta.map_keys {
                    keys.hash(hasher);
                }
                Boxed::TYPE_ID.hash(hasher);
                if let Some(scalar) = arr.as_scalar()
                    && let Some(value) = scalar.nested_value()
                {
                    value.hash(hasher);
                    return;
                }
                arr.shape.hash(hasher);
                (arr.data.iter()).for_each(|x| AestheticHash(&x.0).hash(hasher));
            }
            val => val_as_arr!(val, |arr| arr.hash(hasher)),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        val_as_arr!(self, |arr| arr.fmt(f))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Char(c) if c.rank() < 2 => c.fmt(f),
            Value::Box(arr) if arr.rank() == 0 => arr.fmt(f),
            value => value.grid_string(true).fmt(f),
        }
    }
}

impl PartialEq<i32> for Value {
    fn eq(&self, other: &i32) -> bool {
        if self.rank() > 0 {
            return false;
        }
        match self {
            Value::Num(arr) => arr.data[0] == (*other as f64),
            Value::Byte(arr) => arr.data[0] as i32 == *other,
            _ => false,
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct ValueBuilder {
    value: Option<Value>,
    rows: usize,
    capacity: usize,
}

impl ValueBuilder {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            value: None,
            rows: 0,
            capacity,
        }
    }
    pub fn add_row<C: FillContext>(&mut self, mut row: Value, ctx: &C) -> Result<(), C::Error> {
        if let Some(value) = &mut self.value {
            value.append(row, false, ctx)?;
        } else {
            row.reserve_min(self.capacity);
            row.shape.prepend(1);
            self.value = Some(row);
        }
        self.rows += 1;
        Ok(())
    }
    pub fn finish(self) -> Value {
        self.value.unwrap_or_default()
    }
}
