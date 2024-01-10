//! Algorithms for performing operations on arrays

use std::{
    cmp::Ordering,
    convert::Infallible,
    hash::{Hash, Hasher},
};

use tinyvec::TinyVec;

use crate::{
    Array, ArrayValue, CodeSpan, Function, Inputs, Shape, Signature, Span, Uiua, UiuaError,
    UiuaResult, Value,
};

mod dyadic;
pub(crate) mod invert;
pub mod loops;
pub(crate) mod map;
mod monadic;
pub mod pervade;
pub mod reduce;
pub mod table;
pub mod zip;

type MultiOutput<T> = TinyVec<[T; 1]>;
fn multi_output<T: Clone + Default>(n: usize, val: T) -> MultiOutput<T> {
    let mut vec = TinyVec::with_capacity(n);
    if n == 0 {
        return vec;
    }
    for _ in 0..n - 1 {
        vec.push(val.clone());
    }
    vec.push(val);
    vec
}

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

pub trait ErrorContext {
    type Error;
    fn error(&self, msg: impl ToString) -> Self::Error;
}

impl ErrorContext for Uiua {
    type Error = UiuaError;
    fn error(&self, msg: impl ToString) -> Self::Error {
        self.error(msg)
    }
}

impl ErrorContext for (&CodeSpan, &Inputs) {
    type Error = UiuaError;
    fn error(&self, msg: impl ToString) -> Self::Error {
        UiuaError::Run(
            Span::Code(self.0.clone()).sp(msg.to_string()),
            self.1.clone().into(),
        )
    }
}

impl ErrorContext for () {
    type Error = Infallible;
    fn error(&self, msg: impl ToString) -> Self::Error {
        panic!("{}", msg.to_string())
    }
}

pub trait FillContext: ErrorContext {
    fn unpack_boxes(&self) -> bool;
    fn fill<T: ArrayValue>(&self) -> Result<T, &'static str>;
    fn fill_error(error: Self::Error) -> Self::Error;
    fn is_fill_error(error: &Self::Error) -> bool;
}

impl FillContext for Uiua {
    fn unpack_boxes(&self) -> bool {
        self.unpack_boxes()
    }
    fn fill<T: ArrayValue>(&self) -> Result<T, &'static str> {
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
    fn unpack_boxes(&self) -> bool {
        false
    }
    fn fill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        Err(". No fill is set.")
    }
    fn fill_error(error: Self::Error) -> Self::Error {
        error
    }
    fn is_fill_error(error: &Self::Error) -> bool {
        match *error {}
    }
}

impl FillContext for (&CodeSpan, &Inputs) {
    fn unpack_boxes(&self) -> bool {
        false
    }
    fn fill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        Err(". No fill is set.")
    }
    fn fill_error(error: Self::Error) -> Self::Error {
        error
    }
    fn is_fill_error(error: &Self::Error) -> bool {
        error.is_fill()
    }
}

pub(crate) fn shape_prefixes_match(a: &[usize], b: &[usize]) -> bool {
    a.iter().zip(b).all(|(a, b)| a == b)
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
    if shape_prefixes_match(&a.shape, &b.shape) {
        return Ok(());
    }
    if a.row_count() == 1 && ctx.fill::<A>().is_err() {
        let fixes = (a.shape.iter())
            .take_while(|&&dim| dim == 1)
            .count()
            .min(a.shape.len());
        if (b.shape.iter().rev())
            .zip(a.shape[fixes..].iter().rev())
            .all(|(b, a)| b == a)
        {
            a.shape.drain(..fixes);
            for &dim in b.shape.iter().take(fixes).rev() {
                a.reshape_scalar(dim);
            }
        }
    }
    if b.row_count() == 1 && ctx.fill::<B>().is_err() {
        let fixes = (b.shape.iter())
            .take_while(|&&dim| dim == 1)
            .count()
            .min(b.shape.len());
        if (a.shape.iter().rev())
            .zip(b.shape[fixes..].iter().rev())
            .all(|(a, b)| a == b)
        {
            b.shape.drain(..fixes);
            for &dim in a.shape.iter().take(fixes).rev() {
                b.reshape_scalar(dim);
            }
        }
    }
    if shape_prefixes_match(&a.shape, &b.shape) {
        return Ok(());
    }
    let mut fill_error = None;
    // Fill in missing rows
    match a.row_count().cmp(&b.row_count()) {
        Ordering::Less => match ctx.fill() {
            Ok(fill) => {
                let mut target_shape = a.shape().to_vec();
                target_shape[0] = b.row_count();
                a.fill_to_shape(&target_shape, fill);
            }
            Err(e) => fill_error = Some(e),
        },
        Ordering::Greater => match ctx.fill() {
            Ok(fill) => {
                let mut target_shape = b.shape().to_vec();
                target_shape[0] = a.row_count();
                b.fill_to_shape(&target_shape, fill);
            }
            Err(e) => fill_error = Some(e),
        },
        Ordering::Equal => fill_error = Some(""),
    }
    if shape_prefixes_match(&a.shape, &b.shape) {
        return Ok(());
    }
    // Fill in missing dimensions
    match a.rank().cmp(&b.rank()) {
        Ordering::Less => match ctx.fill() {
            Ok(fill) => {
                let mut target_shape = a.shape.clone();
                target_shape.insert(0, b.row_count());
                a.fill_to_shape(&target_shape, fill);
                fill_error = None;
            }
            Err(e) => fill_error = Some(e),
        },
        Ordering::Greater => match ctx.fill() {
            Ok(fill) => {
                let mut target_shape = b.shape.clone();
                target_shape.insert(0, a.row_count());
                b.fill_to_shape(&target_shape, fill);
                fill_error = None;
            }
            Err(e) => fill_error = Some(e),
        },
        Ordering::Equal => {
            let target_shape = max_shape(a.shape(), b.shape());
            if a.shape() != *target_shape {
                match ctx.fill() {
                    Ok(fill) => {
                        a.fill_to_shape(&target_shape, fill);
                        fill_error = None;
                    }
                    Err(e) => fill_error = Some(e),
                }
            }
            if b.shape() != *target_shape {
                match ctx.fill() {
                    Ok(fill) => {
                        b.fill_to_shape(&target_shape, fill);
                        fill_error = None;
                    }
                    Err(e) => fill_error = Some(e),
                }
            }
        }
    }
    if !shape_prefixes_match(&a.shape, &b.shape) && fill_error.is_none() {
        fill_error = Some(". A ⬚ fill attempt failed.");
    }
    if let Some(e) = fill_error {
        return Err(C::fill_error(ctx.error(format!(
            "Shapes {} and {} do not match{e}",
            a.shape(),
            b.shape(),
        ))));
    }

    Ok(())
}

pub fn all(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    let g = env.pop_function()?;
    let f_sig = f.signature();
    let g_sig = g.signature();
    // Call g
    env.call(g)?;
    // Determine arg counts
    let lower_arg_count = g_sig.outputs / f_sig.args.saturating_sub(1).max(1);
    let upper_arg_count = f_sig.args.saturating_sub(1) * lower_arg_count;
    let mut lower_args = Vec::with_capacity(lower_arg_count);
    let mut upper_args = Vec::with_capacity(upper_arg_count);
    for i in 0..upper_arg_count {
        upper_args.push(env.pop(i + 1)?);
    }
    for i in 0..lower_arg_count {
        lower_args.push(env.pop(upper_arg_count + i + 1)?);
    }
    let mut lower_args = lower_args.into_iter().rev();
    let mut upper_args = upper_args.into_iter().rev();
    // Call f
    for _ in 0..lower_arg_count {
        env.push(lower_args.next().unwrap());
        for _ in 0..f_sig.args.saturating_sub(1) {
            env.push(upper_args.next().unwrap());
        }
        env.call(f.clone())?;
    }
    Ok(())
}

pub fn switch(count: usize, sig: Signature, env: &mut Uiua) -> UiuaResult {
    // Get selector
    let selector = env
        .pop("switch index")?
        .as_natural_array(env, "Switch index must be an array of naturals")?;
    if let Some(i) = selector.data.iter().find(|&&i| i >= count) {
        return Err(env.error(format!(
            "Switch index {i} is out of bounds for switch of size {count}"
        )));
    }
    // Switch
    if selector.rank() == 0 {
        // Scalar
        let i = selector.data[0];
        // Get function
        let Some(f) = env
            .rt
            .function_stack
            .drain(env.rt.function_stack.len() - count..)
            .nth(i)
        else {
            return Err(env.error(
                "Function stack was empty when getting switch function. \
                This is a bug in the interpreter.",
            ));
        };
        // Discard unused arguments
        let discard_start = env.rt.stack.len().saturating_sub(sig.args);
        if discard_start > env.rt.stack.len() {
            return Err(env.error("Stack was empty when discarding excess switch arguments."));
        }
        let discard_end =
            discard_start + sig.args - f.signature().args - (sig.outputs - f.signature().outputs);
        if discard_end > env.rt.stack.len() {
            return Err(env.error("Stack was empty when discarding excess switch arguments."));
        }
        env.rt.stack.drain(discard_start..discard_end);
        env.call(f)
    } else {
        // Array
        // Collect arguments
        let mut args_rows: Vec<_> = Vec::with_capacity(sig.args);
        for i in 0..sig.args {
            let arg = env.pop(i + 1)?;
            if !arg.shape().starts_with(selector.shape()) {
                return Err(env.error(format!(
                    "The function's select's shape {} is not compatible \
                    with the argument {}'s shape {}",
                    selector.shape(),
                    i + 1,
                    arg.shape(),
                )));
            }
            let row_shape = Shape::from(&arg.shape()[selector.rank()..]);
            args_rows.push(arg.into_row_shaped_slices(row_shape));
        }
        args_rows.reverse();
        // Collect functions
        let functions: Vec<(Function, usize)> = env
            .rt
            .function_stack
            .drain(env.rt.function_stack.len() - count..)
            .map(|f| {
                let args = if f.signature().outputs < sig.outputs {
                    f.signature().args + sig.outputs - f.signature().outputs
                } else {
                    f.signature().args
                };
                (f, args)
            })
            .collect();
        let mut outputs = multi_output(sig.outputs, Vec::new());
        for elem in selector.data {
            let (f, args) = &functions[elem];
            for (i, arg) in args_rows.iter_mut().rev().enumerate().rev() {
                let arg = arg.next().unwrap();
                if i < *args {
                    env.push(arg);
                }
            }
            env.call(f.clone())?;
            for i in 0..sig.outputs {
                outputs[i].push(env.pop("switch output")?);
            }
        }
        for output in outputs.into_iter().rev() {
            let mut new_value = Value::from_row_values(output, env)?;
            let mut new_shape = selector.shape.clone();
            new_shape.extend_from_slice(&new_value.shape()[1..]);
            *new_value.shape_mut() = new_shape;
            env.push(new_value);
        }
        Ok(())
    }
}

/// If a function fails on a byte array because no fill byte is defined,
/// convert the byte array to a number array and try again.
fn op_bytes_retry_fill<T>(
    bytes: Array<u8>,
    on_bytes: impl FnOnce(Array<u8>) -> UiuaResult<T>,
    on_nums: impl FnOnce(Array<f64>) -> UiuaResult<T>,
) -> UiuaResult<T> {
    match on_bytes(bytes.clone()) {
        Ok(res) => Ok(res),
        Err(err) if err.is_fill() => on_nums(bytes.convert()),
        Err(err) => Err(err),
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
        Err(err) if err.is_fill() => on_nums(&bytes.clone().convert()),
        Err(err) => Err(err),
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
        Err(err) if C::is_fill_error(&err) => on_nums(a.convert(), b.convert()),
        Err(err) => Err(err),
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
