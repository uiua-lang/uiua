//! Algorithms for performing operations on arrays

use std::{
    cmp::Ordering,
    convert::Infallible,
    hash::{Hash, Hasher},
};

#[cfg(feature = "bytes")]
use crate::UiuaResult;
use crate::{
    array::{Array, ArrayValue, Shape},
    value::Value,
    Uiua, UiuaError,
};

mod dyadic;
pub mod fork;
pub(crate) mod invert;
pub mod loops;
mod monadic;
pub mod pervade;
pub mod reduce;
pub mod table;
pub mod zip;

fn max_shape(a: &[usize], b: &[usize]) -> Shape {
    let shape_len = a.len().max(b.len());
    let mut new_shape = Shape::with_capacity(shape_len);
    for _ in 0..shape_len {
        new_shape.push(0);
    }
    for i in 0..new_shape.len() {
        let j = new_shape.len() - i - 1;
        if a.len() > i {
            new_shape[j] = a[a.len() - i - 1];
        }
        if b.len() > i {
            new_shape[j] = new_shape[j].max(b[b.len() - i - 1]);
        }
    }
    new_shape
}

pub trait FillContext {
    type Error;
    fn error(&self, msg: impl ToString) -> Self::Error;
    fn pack_boxes(&self) -> bool;
    fn fill<T: ArrayValue>(&self) -> Option<T>;
    fn fill_error(error: Self::Error) -> Self::Error;
    fn is_fill_error(error: &Self::Error) -> bool;
}

impl FillContext for Uiua {
    type Error = UiuaError;
    fn error(&self, msg: impl ToString) -> Self::Error {
        self.error(msg)
    }
    fn pack_boxes(&self) -> bool {
        self.pack_boxes()
    }
    fn fill<T: ArrayValue>(&self) -> Option<T> {
        T::get_fill(self)
    }
    fn fill_error(error: Self::Error) -> Self::Error {
        error.fill()
    }
    fn is_fill_error(error: &Self::Error) -> bool {
        error.is_fill()
    }
}

impl FillContext for () {
    type Error = Infallible;
    fn error(&self, msg: impl ToString) -> Self::Error {
        panic!("{}", msg.to_string())
    }
    fn pack_boxes(&self) -> bool {
        false
    }
    fn fill<T: ArrayValue>(&self) -> Option<T> {
        None
    }
    fn fill_error(error: Self::Error) -> Self::Error {
        error
    }
    fn is_fill_error(error: &Self::Error) -> bool {
        match *error {}
    }
}

pub(crate) fn shape_prefixes_match(a: &[usize], b: &[usize]) -> bool {
    a.iter().zip(b.iter()).all(|(a, b)| a == b)
}

#[allow(dead_code)]
pub(crate) fn fill_value_shapes<C>(a: &mut Value, b: &mut Value, ctx: &C) -> Result<(), C::Error>
where
    C: FillContext,
{
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => fill_array_shapes(a, b, ctx),
        #[cfg(feature = "bytes")]
        (Value::Num(a), Value::Byte(b)) => fill_array_shapes(a, b, ctx),
        (Value::Num(a), Value::Char(b)) => fill_array_shapes(a, b, ctx),
        (Value::Num(a), Value::Box(b)) => fill_array_shapes(a, b, ctx),
        #[cfg(feature = "bytes")]
        (Value::Byte(a), Value::Num(b)) => fill_array_shapes(a, b, ctx),
        #[cfg(feature = "bytes")]
        (Value::Byte(a), Value::Byte(b)) => fill_array_shapes(a, b, ctx),
        #[cfg(feature = "bytes")]
        (Value::Byte(a), Value::Char(b)) => fill_array_shapes(a, b, ctx),
        #[cfg(feature = "bytes")]
        (Value::Byte(a), Value::Box(b)) => fill_array_shapes(a, b, ctx),
        (Value::Char(a), Value::Num(b)) => fill_array_shapes(a, b, ctx),
        #[cfg(feature = "bytes")]
        (Value::Char(a), Value::Byte(b)) => fill_array_shapes(a, b, ctx),
        (Value::Char(a), Value::Char(b)) => fill_array_shapes(a, b, ctx),
        (Value::Char(a), Value::Box(b)) => fill_array_shapes(a, b, ctx),
        (Value::Box(a), Value::Num(b)) => fill_array_shapes(a, b, ctx),
        #[cfg(feature = "bytes")]
        (Value::Box(a), Value::Byte(b)) => fill_array_shapes(a, b, ctx),
        (Value::Box(a), Value::Char(b)) => fill_array_shapes(a, b, ctx),
        (Value::Box(a), Value::Box(b)) => fill_array_shapes(a, b, ctx),
    }
}

pub(crate) fn fill_array_shapes<A, B, C>(
    a: &mut Array<A>,
    b: &mut Array<B>,
    ctx: &C,
) -> Result<(), C::Error>
where
    A: ArrayValue,
    B: ArrayValue,
    C: FillContext,
{
    if !shape_prefixes_match(&a.shape, &b.shape) {
        // Fill in missing rows
        match a.row_count().cmp(&b.row_count()) {
            Ordering::Less => {
                if let Some(fill) = ctx.fill() {
                    let mut target_shape = a.shape().to_vec();
                    target_shape[0] = b.row_count();
                    a.fill_to_shape(&target_shape, fill);
                }
            }
            Ordering::Greater => {
                if let Some(fill) = ctx.fill() {
                    let mut target_shape = b.shape().to_vec();
                    target_shape[0] = a.row_count();
                    b.fill_to_shape(&target_shape, fill);
                }
            }
            Ordering::Equal => {}
        }
        // Fill in missing dimensions
        if !shape_prefixes_match(&a.shape, &b.shape) {
            match a.rank().cmp(&b.rank()) {
                Ordering::Less => {
                    if let Some(fill) = ctx.fill() {
                        let mut target_shape = a.shape.clone();
                        target_shape.insert(0, b.row_count());
                        a.fill_to_shape(&target_shape, fill);
                    }
                }
                Ordering::Greater => {
                    if let Some(fill) = ctx.fill() {
                        let mut target_shape = b.shape.clone();
                        target_shape.insert(0, a.row_count());
                        b.fill_to_shape(&target_shape, fill);
                    }
                }
                Ordering::Equal => {
                    let target_shape = max_shape(a.shape(), b.shape());
                    if a.shape() != &*target_shape {
                        if let Some(fill) = ctx.fill() {
                            a.fill_to_shape(&target_shape, fill);
                        }
                    }
                    if b.shape() != &*target_shape {
                        if let Some(fill) = ctx.fill() {
                            b.fill_to_shape(&target_shape, fill);
                        }
                    }
                }
            }
            if !shape_prefixes_match(&a.shape, &b.shape) {
                return Err(C::fill_error(ctx.error(format!(
                    "Shapes {} and {} do not match",
                    a.format_shape(),
                    b.format_shape()
                ))));
            }
        }
    }
    Ok(())
}

/// If a function fails on a byte array because no fill byte is defined,
/// convert the byte array to a number array and try again.
#[cfg(feature = "bytes")]
fn op_bytes_retry_fill<T>(
    bytes: Array<u8>,
    on_bytes: impl FnOnce(Array<u8>) -> UiuaResult<T>,
    on_nums: impl FnOnce(Array<f64>) -> UiuaResult<T>,
) -> UiuaResult<T> {
    match on_bytes(bytes.clone()) {
        Ok(res) => Ok(res),
        Err(err) => {
            if err.is_fill() {
                on_nums(bytes.convert())
            } else {
                Err(err)
            }
        }
    }
}

/// If a function fails on a byte array because no fill byte is defined,
/// convert the byte array to a number array and try again.
#[cfg(feature = "bytes")]
fn op_bytes_ref_retry_fill<T>(
    bytes: &Array<u8>,
    on_bytes: impl FnOnce(&Array<u8>) -> UiuaResult<T>,
    on_nums: impl FnOnce(&Array<f64>) -> UiuaResult<T>,
) -> UiuaResult<T> {
    match on_bytes(bytes) {
        Ok(res) => Ok(res),
        Err(err) => {
            if err.is_fill() {
                on_nums(&bytes.clone().convert())
            } else {
                Err(err)
            }
        }
    }
}

/// If a function fails on 2 byte arrays because no fill byte is defined,
/// convert the byte arrays to number arrays and try again.
#[cfg(feature = "bytes")]
fn op2_bytes_retry_fill<T, C: FillContext>(
    a: Array<u8>,
    b: Array<u8>,
    on_bytes: impl FnOnce(Array<u8>, Array<u8>) -> Result<T, C::Error>,
    on_nums: impl FnOnce(Array<f64>, Array<f64>) -> Result<T, C::Error>,
) -> Result<T, C::Error> {
    match on_bytes(a.clone(), b.clone()) {
        Ok(res) => Ok(res),
        Err(err) => {
            if C::is_fill_error(&err) {
                on_nums(a.convert(), b.convert())
            } else {
                Err(err)
            }
        }
    }
}

struct ArrayCmpSlice<'a, T>(&'a [T]);

impl<'a, T: ArrayValue> PartialEq for ArrayCmpSlice<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len() && self.0.iter().zip(other.0).all(|(a, b)| a.array_eq(b))
    }
}

impl<'a, T: ArrayValue> Eq for ArrayCmpSlice<'a, T> {}

impl<'a, T: ArrayValue> PartialOrd for ArrayCmpSlice<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a, T: ArrayValue> Ord for ArrayCmpSlice<'a, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0
            .iter()
            .zip(other.0)
            .map(|(a, b)| a.array_cmp(b))
            .find(|&o| o != Ordering::Equal)
            .unwrap_or(Ordering::Equal)
    }
}

impl<'a, T: ArrayValue> Hash for ArrayCmpSlice<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elem in self.0 {
            elem.array_hash(state);
        }
    }
}
