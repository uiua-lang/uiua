//! Algorithms for performing operations on arrays

use std::{
    cmp::Ordering,
    convert::Infallible,
    fmt,
    hash::{Hash, Hasher},
    iter,
    mem::size_of,
    option,
};

use tinyvec::TinyVec;

use crate::{
    Array, ArrayValue, CodeSpan, ExactDoubleIterator, Function, Inputs, PersistentMeta, Shape,
    Signature, Span, TempStack, Uiua, UiuaError, UiuaResult, Value,
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

#[derive(Debug)]
pub struct SizeError(f64);

impl fmt::Display for SizeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Array of {} elements would be too large", self.0)
    }
}

impl std::error::Error for SizeError {}

pub fn validate_size<T>(
    sizes: impl IntoIterator<Item = usize> + Clone,
    env: &Uiua,
) -> UiuaResult<usize> {
    validate_size_impl(size_of::<T>(), sizes).map_err(|e| env.error(e))
}

pub(crate) fn validate_size_impl(
    elem_size: usize,
    sizes: impl IntoIterator<Item = usize>,
) -> Result<usize, SizeError> {
    let mut elements = 1.0;
    for size in sizes {
        if size == 0 {
            return Ok(0);
        }
        elements *= size as f64;
    }
    let size = elements * elem_size as f64;
    let max_mega = if cfg!(target_arch = "wasm32") {
        256
    } else {
        4096
    };
    if size > (max_mega * 1024usize.pow(2)) as f64 {
        return Err(SizeError(elements));
    }
    Ok(elements as usize)
}

pub trait ErrorContext {
    type Error: FillError;
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

pub struct IgnoreError;
impl ErrorContext for IgnoreError {
    type Error = ();
    fn error(&self, _: impl ToString) -> Self::Error {}
}

pub trait FillError: fmt::Debug {
    fn is_fill(&self) -> bool;
}

impl FillError for () {
    fn is_fill(&self) -> bool {
        false
    }
}

impl FillError for UiuaError {
    fn is_fill(&self) -> bool {
        UiuaError::is_fill(self)
    }
}

impl FillError for Infallible {
    fn is_fill(&self) -> bool {
        match *self {}
    }
}

pub trait FillContext: ErrorContext {
    fn scalar_fill<T: ArrayValue>(&self) -> Result<T, &'static str>;
    fn array_fill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str>;
    fn fill_error(error: Self::Error) -> Self::Error;
    fn is_fill_error(error: &Self::Error) -> bool;
}

impl FillContext for Uiua {
    fn scalar_fill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        T::get_scalar_fill(self)
    }
    fn array_fill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
        T::get_array_fill(self)
    }
    fn fill_error(error: Self::Error) -> Self::Error {
        error.fill()
    }
    fn is_fill_error(error: &Self::Error) -> bool {
        error.is_fill()
    }
}

impl FillContext for () {
    fn scalar_fill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        Err(". No fill is set.")
    }
    fn array_fill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
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
    fn scalar_fill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        Err(". No fill is set.")
    }
    fn array_fill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
        Err(". No fill is set.")
    }
    fn fill_error(error: Self::Error) -> Self::Error {
        error.fill()
    }
    fn is_fill_error(error: &Self::Error) -> bool {
        error.is_fill()
    }
}

pub(crate) fn shape_prefixes_match(a: &[usize], b: &[usize]) -> bool {
    a.iter().zip(b).all(|(a, b)| a == b)
}

fn fill_value_shape<C>(
    val: &mut Value,
    target: &Shape,
    expand_fixed: bool,
    ctx: &C,
) -> Result<(), &'static str>
where
    C: FillContext,
{
    #[derive(Debug)]
    struct StaticFillError(&'static str);
    impl FillError for StaticFillError {
        fn is_fill(&self) -> bool {
            true
        }
    }

    match val {
        Value::Num(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
        Value::Byte(arr) => {
            *val = op_bytes_retry_fill(
                arr.clone(),
                |mut arr| -> Result<Value, StaticFillError> {
                    fill_array_shape(&mut arr, target, expand_fixed, ctx)
                        .map_err(StaticFillError)?;
                    Ok(arr.into())
                },
                |mut arr| -> Result<Value, StaticFillError> {
                    fill_array_shape(&mut arr, target, expand_fixed, ctx)
                        .map_err(StaticFillError)?;
                    Ok(arr.into())
                },
            )
            .map_err(|StaticFillError(e)| e)?;
            Ok(())
        }
        Value::Complex(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
        Value::Char(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
        Value::Box(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
    }
}

fn fill_array_shape<T, C>(
    arr: &mut Array<T>,
    target: &Shape,
    expand_fixed: bool,
    ctx: &C,
) -> Result<(), &'static str>
where
    T: ArrayValue,
    C: FillContext,
{
    if shape_prefixes_match(&arr.shape, target) {
        return Ok(());
    }
    if expand_fixed && arr.row_count() == 1 && ctx.scalar_fill::<T>().is_err() {
        let fixes = (arr.shape.iter())
            .take_while(|&&dim| dim == 1)
            .count()
            .min(arr.shape.len());
        let same_under_fixes = (target.iter().skip(fixes))
            .zip(arr.shape[fixes..].iter())
            .all(|(b, a)| b == a);
        if same_under_fixes {
            arr.shape.drain(..fixes);
            if target.len() >= fixes {
                for &dim in target.iter().take(fixes).rev() {
                    arr.reshape_scalar_integer(dim);
                }
            } else if arr.shape() == target {
                for &dim in target.iter().cycle().take(fixes) {
                    arr.reshape_scalar_integer(dim);
                }
            }
        }
        if shape_prefixes_match(&arr.shape, target) {
            return Ok(());
        }
    }
    // Fill in missing rows
    let target_row_count = target.first().copied().unwrap_or(1);
    let mut res = Ok(());
    match arr.row_count().cmp(&target_row_count) {
        Ordering::Less => match ctx.scalar_fill() {
            Ok(fill) => {
                let mut target_shape = arr.shape().to_vec();
                target_shape[0] = target_row_count;
                arr.fill_to_shape(&target_shape, fill);
            }
            Err(e) => res = Err(e),
        },
        Ordering::Greater => {}
        Ordering::Equal => res = Err(""),
    }
    if shape_prefixes_match(&arr.shape, target) {
        return Ok(());
    }
    // Fill in missing dimensions
    match arr.rank().cmp(&target.len()) {
        Ordering::Less => match ctx.scalar_fill() {
            Ok(fill) => {
                let mut target_shape = arr.shape.clone();
                target_shape.insert(0, target_row_count);
                arr.fill_to_shape(&target_shape, fill);
                res = Ok(());
            }
            Err(e) => res = Err(e),
        },
        Ordering::Greater => {}
        Ordering::Equal => {
            let target_shape = max_shape(arr.shape(), target);
            if arr.shape() != *target_shape {
                match ctx.scalar_fill() {
                    Ok(fill) => {
                        arr.fill_to_shape(&target_shape, fill);
                        res = Ok(());
                    }
                    Err(e) => res = Err(e),
                }
            }
        }
    }
    if !shape_prefixes_match(&arr.shape, target) && res.is_ok() {
        res = Err("");
    }
    res
}

pub(crate) fn fill_value_shapes<C>(
    a: &mut Value,
    b: &mut Value,
    expand_fixed: bool,
    ctx: &C,
) -> Result<(), C::Error>
where
    C: FillContext,
{
    let a_err = fill_value_shape(a, b.shape(), expand_fixed, ctx).err();
    let b_err = fill_value_shape(b, a.shape(), expand_fixed, ctx).err();

    if shape_prefixes_match(a.shape(), b.shape())
        || !expand_fixed && (a.shape().starts_with(&[1]) || b.shape().starts_with(&[1]))
    {
        Ok(())
    } else if let Some(e) = a_err.or(b_err) {
        Err(C::fill_error(ctx.error(format!(
            "Shapes {} and {} do not match{e}",
            a.shape(),
            b.shape(),
        ))))
    } else {
        Err(C::fill_error(ctx.error(format!(
            "Shapes {} and {} do not match",
            a.shape(),
            b.shape(),
        ))))
    }
}

pub(crate) fn fill_array_shapes<A, B, C>(
    a: &mut Array<A>,
    b: &mut Array<B>,
    a_depth: usize,
    b_depth: usize,
    ctx: &C,
) -> Result<(), C::Error>
where
    A: ArrayValue,
    B: ArrayValue,
    C: FillContext,
{
    let a_depth = a_depth.min(a.rank());
    let b_depth = b_depth.min(b.rank());
    match (a_depth, b_depth) {
        (0, 0) => {
            if shape_prefixes_match(&a.shape, &b.shape) {
                return Ok(());
            }
            let a_shape = a.shape().clone();
            let b_shape = b.shape().clone();
            let a_err = fill_array_shape(a, b.shape(), true, ctx).err();
            let b_err = fill_array_shape(b, &a_shape, true, ctx).err();
            if shape_prefixes_match(&a.shape, &b.shape) {
                Ok(())
            } else if let Some(e) = a_err.or(b_err) {
                Err(C::fill_error(ctx.error(format!(
                    "Shapes {a_shape} and {b_shape} do not match{e}"
                ))))
            } else {
                Err(C::fill_error(ctx.error(format!(
                    "Shapes {a_shape} and {b_shape} do not match"
                ))))
            }
        }
        (_, _) => {
            if a.row_count() != b.row_count() {
                return Err(C::fill_error(ctx.error(format!(
                    "Shapes {} and {} do not match",
                    a.shape(),
                    b.shape(),
                ))));
            }
            if !shape_prefixes_match(&a.shape[a_depth..], &b.shape[b_depth..]) {
                let mut new_a_rows = Vec::with_capacity(a.row_count());
                let mut new_b_rows = Vec::with_capacity(b.row_count());
                for (mut a_row, mut b_row) in a.rows().zip(b.rows()) {
                    fill_array_shapes(&mut a_row, &mut b_row, a_depth - 1, b_depth - 1, ctx)?;
                    new_a_rows.push(a_row);
                    new_b_rows.push(b_row);
                }
                *a = Array::from_row_arrays_infallible(new_a_rows);
                *b = Array::from_row_arrays_infallible(new_b_rows);
            }
            Ok(())
        }
    }
}

pub fn switch(
    count: usize,
    sig: Signature,
    copy_condition_under: bool,
    env: &mut Uiua,
) -> UiuaResult {
    // Get selector
    let selector = env.pop("switch index")?;
    let copied_selector = if copy_condition_under {
        Some(selector.clone())
    } else {
        None
    };
    let selector = selector.as_natural_array(env, "Switch index must be an array of naturals")?;
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
        // `saturating_sub` and `max` handle incorrect explicit signatures
        let discard_end = (discard_start + sig.args + f.signature().outputs)
            .saturating_sub(f.signature().args + sig.outputs)
            .max(discard_start);
        if discard_end > env.rt.stack.len() {
            return Err(env.error("Stack was empty when discarding excess switch arguments."));
        }
        env.rt.stack.drain(discard_start..discard_end);
        env.call(f)?;
    } else {
        // Array
        // Collect arguments
        let mut args_rows: Vec<_> = Vec::with_capacity(sig.args);
        for i in 0..sig.args {
            let arg = env.pop(i + 1)?;
            if !arg.shape().starts_with(selector.shape()) {
                return Err(env.error(format!(
                    "The selector's shape {} is not compatible \
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
        // Switch with each selector element
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
        // Collect output
        for output in outputs.into_iter().rev() {
            let mut new_value = Value::from_row_values(output, env)?;
            let mut new_shape = selector.shape.clone();
            new_shape.extend_from_slice(&new_value.shape()[1..]);
            *new_value.shape_mut() = new_shape;
            env.push(new_value);
        }
    }
    if let Some(selector) = copied_selector {
        env.push_temp(TempStack::Under, selector);
    }
    Ok(())
}

pub fn try_(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    let handler = env.pop_function()?;
    let f_sig = f.signature();
    let handler_sig = handler.signature();
    if env.stack_height() < f_sig.args {
        for i in 0..f_sig.args {
            env.pop(i + 1)?;
        }
    }
    let backup = env.clone_stack_top(f_sig.args.min(handler_sig.args))?;
    if let Err(mut err) = env.call_clean_stack(f) {
        err = err.handle_case()?;
        if handler_sig.args > f_sig.args {
            (env.rt.backend).save_error_color(err.message(), err.report().to_string());
            env.push(err.value());
        }
        for val in backup {
            env.push(val);
        }
        env.call(handler)?;
    }
    Ok(())
}

pub fn case(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    env.call(f).map_err(|e| UiuaError::Case(e.into()))
}

/// If a function fails on a byte array because no fill byte is defined,
/// convert the byte array to a number array and try again.
fn op_bytes_retry_fill<T, E: FillError>(
    bytes: Array<u8>,
    on_bytes: impl FnOnce(Array<u8>) -> Result<T, E>,
    on_nums: impl FnOnce(Array<f64>) -> Result<T, E>,
) -> Result<T, E> {
    match on_bytes(bytes.clone()) {
        Ok(res) => Ok(res),
        Err(err) if err.is_fill() => on_nums(bytes.convert()),
        Err(err) => Err(err),
    }
}

/// If a function fails on a byte array because no fill byte is defined,
/// convert the byte array to a number array and try again.
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
fn op2_bytes_retry_fill<T, C: FillContext>(
    a: Array<u8>,
    b: Array<u8>,
    ctx: &C,
    on_bytes: impl FnOnce(Array<u8>, Array<u8>) -> Result<T, C::Error>,
    on_nums: impl FnOnce(Array<f64>, Array<f64>) -> Result<T, C::Error>,
) -> Result<T, C::Error> {
    if ctx.scalar_fill::<f64>().is_ok() {
        match on_bytes(a.clone(), b.clone()) {
            Ok(res) => Ok(res),
            Err(err) if C::is_fill_error(&err) => on_nums(a.convert(), b.convert()),
            Err(err) => Err(err),
        }
    } else {
        on_bytes(a, b)
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

type FixedRows = Vec<
    Result<iter::Chain<Box<dyn ExactDoubleIterator<Item = Value>>, option::IntoIter<Value>>, Value>,
>;

struct FixedRowsData {
    rows: FixedRows,
    row_count: usize,
    is_empty: bool,
    all_scalar: bool,
    per_meta: PersistentMeta,
}

fn fixed_rows(
    prim: impl fmt::Display,
    outputs: usize,
    mut args: Vec<Value>,
    env: &Uiua,
) -> UiuaResult<FixedRowsData> {
    for a in 0..args.len() {
        let a_can_fill = args[a].length_is_fillable(env);
        for b in a + 1..args.len() {
            let b_can_fill = args[b].length_is_fillable(env);
            let mut err = None;
            if a_can_fill {
                let b_row_count = args[b].row_count();
                err = args[a].fill_length_to(b_row_count, env).err();
            }
            if err.is_none() && b_can_fill {
                let a_row_count = args[a].row_count();
                err = args[b].fill_length_to(a_row_count, env).err();
            }
            if err.is_none()
                && args[a].row_count() != args[b].row_count()
                && args[a].row_count() != 1
                && args[b].row_count() != 1
            {
                err = Some("");
            }
            if let Some(e) = err {
                return Err(env.error(format!(
                    "Cannot {prim} arrays with different number of rows, shapes {} and {}{e}",
                    args[a].shape(),
                    args[b].shape(),
                )));
            }
        }
    }
    let mut row_count = 0;
    let mut all_scalar = true;
    let mut all_1 = true;
    let is_empty = outputs > 0 && args.iter().any(|v| v.row_count() == 0);
    let mut per_meta = Vec::new();
    let fixed_rows: FixedRows = args
        .into_iter()
        .map(|mut v| {
            all_scalar = all_scalar && v.rank() == 0;
            if v.row_count() == 1 {
                v.undo_fix();
                Err(v)
            } else {
                let proxy = is_empty.then(|| v.proxy_row(env));
                row_count = row_count.max(v.row_count());
                all_1 = false;
                per_meta.push(v.take_per_meta());
                Ok(v.into_rows().chain(proxy))
            }
        })
        .collect();
    if all_1 {
        row_count = 1;
    }
    let per_meta = PersistentMeta::xor_all(per_meta);
    let row_count = row_count + is_empty as usize;
    Ok(FixedRowsData {
        rows: fixed_rows,
        row_count,
        is_empty,
        all_scalar,
        per_meta,
    })
}

#[cfg(not(feature = "fft"))]
pub fn fft(env: &mut Uiua) -> UiuaResult {
    Err(env.error("FFT is not available in this environment"))
}

#[cfg(not(feature = "fft"))]
pub fn unfft(env: &mut Uiua) -> UiuaResult {
    Err(env.error("FFT is not available in this environment"))
}

#[cfg(feature = "fft")]
pub fn fft(env: &mut Uiua) -> UiuaResult {
    fft_impl(env, rustfft::FftPlanner::plan_fft_forward)
}

#[cfg(feature = "fft")]
pub fn unfft(env: &mut Uiua) -> UiuaResult {
    fft_impl(env, rustfft::FftPlanner::plan_fft_inverse)
}

#[cfg(feature = "fft")]
fn fft_impl(
    env: &mut Uiua,
    plan: fn(&mut rustfft::FftPlanner<f64>, usize) -> std::sync::Arc<dyn rustfft::Fft<f64>>,
) -> UiuaResult {
    use std::mem::transmute;

    use rustfft::{num_complex::Complex64, FftPlanner};

    use crate::Complex;

    let mut arr: Array<Complex> = match env.pop(1)? {
        Value::Num(arr) => arr.convert(),
        Value::Byte(arr) => arr.convert(),
        Value::Complex(arr) => arr,
        val => {
            return Err(env.error(format!("Cannot perform FFT on a {} array", val.type_name())));
        }
    };
    if arr.rank() == 0 {
        env.push(0);
        return Ok(());
    }
    let list_row_len: usize = arr.shape[arr.rank() - 1..].iter().product();
    if list_row_len == 0 {
        env.push(arr);
        return Ok(());
    }
    let mut planner = FftPlanner::new();
    let scaling_factor = 1.0 / (list_row_len as f64).sqrt();
    for row in arr.data.as_mut_slice().chunks_exact_mut(list_row_len) {
        let fft = plan(&mut planner, row.len());
        // SAFETY: Uiua's `Complex` and `num_complex::Complex64` have the same memory layout
        let slice: &mut [Complex64] = unsafe { transmute::<&mut [Complex], &mut [Complex64]>(row) };
        fft.process(slice);
        for c in row {
            *c = *c * scaling_factor;
        }
    }
    env.push(arr);
    Ok(())
}

#[cfg(not(feature = "pathfinding"))]
pub fn astar(env: &mut Uiua) -> UiuaResult {
    Err(env.error("A* pathfinding is not available in this environment"))
}

#[cfg(not(feature = "pathfinding"))]
pub fn astar_first(env: &mut Uiua) -> UiuaResult {
    Err(env.error("A* pathfinding is not available in this environment"))
}

#[cfg(feature = "pathfinding")]
pub fn astar(env: &mut Uiua) -> UiuaResult {
    use ecow::EcoVec;

    use crate::Boxed;

    let (solution, cost) = astar_impl(env)?;
    let mut paths = EcoVec::new();
    for path in solution {
        paths.push(Boxed(Value::from_row_values(path, env)?));
    }
    env.push(cost);
    env.push(paths);
    Ok(())
}

#[cfg(feature = "pathfinding")]
pub fn astar_first(env: &mut Uiua) -> UiuaResult {
    use crate::Boxed;

    let (solution, cost) = astar_impl(env)?;
    if let Some(path) = solution.into_iter().next() {
        env.push(cost);
        env.push(Boxed(Value::from_row_values(path, env)?));
        Ok(())
    } else {
        Err(env.error("No path found"))
    }
}

#[cfg(feature = "pathfinding")]
fn astar_impl(
    env: &mut Uiua,
) -> UiuaResult<(pathfinding::directed::astar::AstarSolution<Value>, f64)> {
    use std::{cell::RefCell, rc::Rc};

    let start = env.pop("start")?;
    let neighbors = env.pop_function()?;
    let heuristic = env.pop_function()?;
    let is_goal = env.pop_function()?;
    let nei_sig = neighbors.signature();
    let heu_sig = heuristic.signature();
    let isg_sig = is_goal.signature();
    for (name, f, req_out) in &[
        ("neighbors", &neighbors, [1, 2].as_slice()),
        ("heuristic", &heuristic, &[1]),
        ("goal", &is_goal, &[1]),
    ] {
        let sig = f.signature();
        if !req_out.contains(&sig.outputs) {
            let count = if req_out.len() == 1 {
                "1"
            } else {
                "either 1 or 2"
            };
            return Err(env.error_maybe_span(
                f.id.span(),
                format!(
                    "A* {name} function must return {count} outputs \
                    but its signature is {sig}",
                ),
            ));
        }
    }
    let arg_count = nei_sig.args.max(heu_sig.args).max(isg_sig.args) - 1;
    let mut args = Vec::with_capacity(arg_count);
    for i in 0..arg_count {
        args.push(env.pop(i + 1)?);
    }
    const COST_MUL: f64 = 1e6;
    let error: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));
    let error_clone = error.clone();
    let do_error = move |msg| {
        *error_clone.borrow_mut() = Some(msg);
    };
    let path = {
        let env = Rc::new(RefCell::new(&mut *env));
        pathfinding::directed::astar::astar_bag(
            &start,
            |n| {
                let res = (|| {
                    let mut env = env.borrow_mut();
                    for arg in args.iter().take(nei_sig.args.saturating_sub(1)).rev() {
                        env.push(arg.clone());
                    }
                    if nei_sig.args > 0 {
                        env.push(n.clone());
                    }
                    env.call(neighbors.clone())?;
                    let (nodes, costs) = if nei_sig.outputs == 2 {
                        let costs = env
                            .pop("neighbors costs")?
                            .as_nums(&env, "Costs must be a list of numbers")?;
                        let nodes = env.pop("neighbors nodes")?;
                        if costs.len() != nodes.row_count() {
                            return Err(env.error_maybe_span(
                                neighbors.id.span(),
                                format!(
                                    "Number of nodes {} does not match number of costs {}",
                                    nodes.row_count(),
                                    costs.len(),
                                ),
                            ));
                        }
                        let mut icosts = Vec::with_capacity(costs.len());
                        for cost in costs {
                            if cost < 0.0 {
                                return Err(env.error_maybe_span(
                                    neighbors.id.span(),
                                    "Negative costs are not allowed in A*",
                                ));
                            }
                            icosts.push((cost * COST_MUL).round() as u64);
                        }
                        (nodes, icosts)
                    } else {
                        let nodes = env.pop("neighbors nodes")?;
                        let costs = vec![COST_MUL as u64; nodes.row_count()];
                        (nodes, costs)
                    };
                    Ok(nodes.into_rows().zip(costs).collect::<Vec<_>>())
                })();
                match res {
                    Ok(res) => res,
                    Err(e) => {
                        do_error(e.message());
                        Vec::new()
                    }
                }
            },
            |n| {
                let res = (|| {
                    let mut env = env.borrow_mut();
                    for arg in args.iter().take(heu_sig.args.saturating_sub(1)).rev() {
                        env.push(arg.clone());
                    }
                    if heu_sig.args > 0 {
                        env.push(n.clone());
                    }
                    env.call(heuristic.clone())?;
                    let h = env
                        .pop("heuristic")?
                        .as_num(&env, "Heuristic must be a number")?;
                    if h < 0.0 {
                        return Err(env.error_maybe_span(
                            heuristic.id.span(),
                            "Negative heuristic values are not allowed in A*",
                        ));
                    }
                    Ok((h * COST_MUL).round() as u64)
                })();
                match res {
                    Ok(res) => res,
                    Err(e) => {
                        do_error(e.message());
                        0
                    }
                }
            },
            |n| {
                let res = (|| -> UiuaResult<bool> {
                    let mut env = env.borrow_mut();
                    for arg in args.iter().take(isg_sig.args.saturating_sub(1)).rev() {
                        env.push(arg.clone());
                    }
                    if isg_sig.args > 0 {
                        env.push(n.clone());
                    }
                    env.call(is_goal.clone())?;
                    let is_goal = env
                        .pop("is_goal")?
                        .as_bool(&env, "A& goal function must return a boolean")?;
                    Ok(is_goal)
                })();
                match res {
                    Ok(res) => res,
                    Err(e) => {
                        do_error(e.message());
                        false
                    }
                }
            },
        )
    };
    drop(do_error);
    if let Some(msg) = Rc::try_unwrap(error).unwrap().into_inner() {
        return Err(env.error(msg.clone()));
    }
    if let Some((solution, cost)) = path {
        Ok((solution, cost as f64 / COST_MUL))
    } else {
        Err(env.error("No path found"))
    }
}
