//! Algorithms for performing operations on arrays

use std::{
    cmp::Ordering,
    collections::{BTreeSet, BinaryHeap, HashMap},
    convert::Infallible,
    fmt,
    hash::{Hash, Hasher},
    iter,
    mem::{size_of, take},
    ops::Deref,
    option,
};

use ecow::{EcoString, EcoVec};
use tinyvec::TinyVec;

use crate::{
    cowslice::ecovec_extend_cowslice, Array, ArrayCmp, ArrayValue, Boxed, CodeSpan, Complex,
    ExactDoubleIterator, Inputs, Ops, PersistentMeta, Primitive, Shape, SigNode, Signature, Span,
    Uiua, UiuaError, UiuaErrorKind, UiuaResult, Value,
};

mod dyadic;
pub mod encode;
pub mod loops;
pub mod map;
mod monadic;
pub mod permute;
pub mod pervade;
pub mod reduce;
pub mod stencil;
pub mod table;
pub mod zip;

pub(crate) fn get_ops<const N: usize>(
    ops: EcoVec<SigNode>,
    env: &Uiua,
) -> UiuaResult<[SigNode; N]> {
    ops.try_into().map_err(|ops: EcoVec<SigNode>| {
        env.error(if ops.len() < N {
            #[cfg(debug_assertions)]
            panic!("Not enough operands");
            #[cfg(not(debug_assertions))]
            "Not enough operands. This is a bug in the interpreter."
        } else {
            "Too many operands.  This is a bug in the interpreter."
        })
    })
}

pub trait Indexable: IntoIterator + Deref<Target = [Self::Item]> {}

impl<T> Indexable for T where T: IntoIterator + Deref<Target = [T::Item]> {}

type MultiOutput<T> = TinyVec<[T; 1]>;
pub(crate) fn multi_output<T: Clone + Default>(n: usize, val: T) -> MultiOutput<T> {
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

#[derive(Debug)]
pub enum FillShapeError {
    Size(SizeError),
    Shape(&'static str),
}

impl From<SizeError> for FillShapeError {
    fn from(e: SizeError) -> Self {
        Self::Size(e)
    }
}

impl fmt::Display for FillShapeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Size(e) => e.fmt(f),
            Self::Shape(e) => write!(f, "{e}"),
        }
    }
}

pub fn validate_size<T>(sizes: impl IntoIterator<Item = usize>, env: &Uiua) -> UiuaResult<usize> {
    validate_size_of::<T>(sizes).map_err(|e| env.error(e))
}

pub fn validate_size_of<T>(sizes: impl IntoIterator<Item = usize>) -> Result<usize, SizeError> {
    validate_size_impl(size_of::<T>(), sizes)
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
    let max_mega = if cfg!(target_pointer_width = "32") {
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
        UiuaErrorKind::Run {
            message: Span::Code(self.0.clone()).sp(msg.to_string()),
            info: Vec::new(),
            inputs: self.1.clone().into(),
        }
        .into()
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
        self.is_fill
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
    fn scalar_unfill<T: ArrayValue>(&self) -> Result<T, &'static str>;
    fn array_unfill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str>;
    fn either_array_fill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
        self.array_fill::<T>().or_else(|_| self.array_unfill::<T>())
    }
    fn fill_error(error: Self::Error) -> Self::Error;
    fn is_fill_error(error: &Self::Error) -> bool;
    fn number_only_fill(&self) -> bool {
        self.array_fill::<f64>().is_ok() && self.array_fill::<u8>().is_err()
    }
    fn is_scalar_filled(&self, val: &Value) -> bool {
        match val {
            Value::Num(_) => self.scalar_fill::<f64>().is_ok(),
            Value::Byte(_) => self.scalar_fill::<u8>().is_ok(),
            Value::Complex(_) => self.scalar_fill::<Complex>().is_ok(),
            Value::Char(_) => self.scalar_fill::<char>().is_ok(),
            Value::Box(_) => self.scalar_fill::<Boxed>().is_ok(),
        }
    }
}

impl FillContext for Uiua {
    fn scalar_fill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        T::get_scalar_fill(&self.fill())
    }
    fn array_fill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
        T::get_array_fill(&self.fill())
    }
    fn scalar_unfill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        T::get_scalar_fill(&self.unfill())
    }
    fn array_unfill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
        T::get_array_fill(&self.unfill())
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
    fn scalar_unfill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        Err(". No unfill is set.")
    }
    fn array_unfill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
        Err(". No unfill is set.")
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
    fn scalar_unfill<T: ArrayValue>(&self) -> Result<T, &'static str> {
        Err(". No unfill is set.")
    }
    fn array_unfill<T: ArrayValue>(&self) -> Result<Array<T>, &'static str> {
        Err(". No unfill is set.")
    }
    fn fill_error(error: Self::Error) -> Self::Error {
        error.fill()
    }
    fn is_fill_error(error: &Self::Error) -> bool {
        error.is_fill
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
) -> Result<(), FillShapeError>
where
    C: FillContext,
{
    val.match_fill(ctx);
    match val {
        Value::Num(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
        Value::Byte(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
        Value::Complex(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
        Value::Char(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
        Value::Box(arr) => fill_array_shape(arr, target, expand_fixed, ctx),
    }
}

/// The error is a tuple of the size of an array that would be too large and the error message
fn fill_array_shape<T, C>(
    arr: &mut Array<T>,
    target: &Shape,
    expand_fixed: bool,
    ctx: &C,
) -> Result<(), FillShapeError>
where
    T: ArrayValue,
    C: FillContext,
{
    if shape_prefixes_match(&arr.shape, target) {
        return Ok(());
    }
    if expand_fixed && arr.row_count() == 1 && ctx.scalar_fill::<T>().is_err() {
        let mut fixes = (arr.shape.iter()).take_while(|&&dim| dim == 1).count();
        if fixes == arr.rank() {
            fixes = (fixes - 1).max(1)
        }
        let same_under_fixes = (target.iter().skip(fixes))
            .zip(arr.shape[fixes..].iter())
            .all(|(b, a)| b == a);
        if same_under_fixes {
            arr.shape.drain(..fixes);
            if target.len() >= fixes {
                for &dim in target.iter().take(fixes).rev() {
                    arr.reshape_scalar_integer(dim)?;
                }
            } else if arr.shape() == target {
                for &dim in target.iter().cycle().take(fixes) {
                    arr.reshape_scalar_integer(dim)?;
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
            Err(e) => res = Err(FillShapeError::Shape(e)),
        },
        Ordering::Greater => {}
        Ordering::Equal => res = Err(FillShapeError::Shape("")),
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
            Err(e) => res = Err(FillShapeError::Shape(e)),
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
                    Err(e) => res = Err(FillShapeError::Shape(e)),
                }
            }
        }
    }
    if !shape_prefixes_match(&arr.shape, target) && res.is_ok() {
        res = Err(FillShapeError::Shape(""));
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
    } else {
        Err(C::fill_error(ctx.error(match (a_err, b_err) {
            (Some(FillShapeError::Size(e)), _) | (_, Some(FillShapeError::Size(e))) => {
                e.to_string()
            }
            (Some(e), _) | (_, Some(e)) => {
                format!("Shapes {} and {} do not match{e}", a.shape(), b.shape())
            }
            (None, None) => {
                format!("Shapes {} and {} do not match", a.shape(), b.shape())
            }
        })))
    }
}

pub fn switch(
    branches: Ops,
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
    // Switch
    if selector.rank() == 0 {
        // Scalar
        let selector =
            selector.as_natural_array(env, "Switch index must be an array of naturals")?;
        if let Some(i) = selector.data.iter().find(|&&i| i >= branches.len()) {
            return Err(env.error(format!(
                "Switch index {i} is out of bounds for switch of size {}",
                branches.len()
            )));
        }
        let i = selector.data[0];
        // Get function
        let Some(f) = branches.into_iter().nth(i) else {
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
        let discard_end = (discard_start + sig.args + f.sig.outputs)
            .saturating_sub(f.sig.args + sig.outputs)
            .max(discard_start);
        if discard_end > env.rt.stack.len() {
            return Err(env.error("Stack was empty when discarding excess switch arguments."));
        }
        env.rt.stack.drain(discard_start..discard_end);
        env.exec(f)?;
    } else {
        // Array
        // Collect arguments
        let mut args = Vec::with_capacity(sig.args + 1);
        let mut new_shape = selector.shape().clone();
        args.push(selector);
        for i in 0..sig.args {
            let arg = env.pop(i + 1)?;
            args.push(arg);
        }
        args[1..].reverse();
        let FixedRowsData {
            mut rows,
            row_count,
            is_empty,
            all_scalar,
            ..
        } = fixed_rows("switch", sig.outputs, args, env)?;
        // Collect functions
        let args: Vec<usize> = branches
            .iter()
            .map(|sn| {
                if sn.sig.outputs < sig.outputs {
                    sn.sig.args + sig.outputs - sn.sig.outputs
                } else {
                    sn.sig.args
                }
            })
            .collect();

        // Switch with each selector element
        let mut outputs = multi_output(sig.outputs, Vec::new());
        let mut rows_to_sel = Vec::with_capacity(sig.args);
        for _ in 0..row_count {
            let selector = match &mut rows[0] {
                Ok(selector) => selector.next().unwrap(),
                Err(selector) => selector.clone(),
            }
            .as_natural_array(env, "Switch index must be an array of naturals")?;
            if let Some(i) = selector.data.iter().find(|&&i| i >= branches.len()) {
                return Err(env.error(format!(
                    "Switch index {i} is out of bounds for switch of size {}",
                    branches.len()
                )));
            }
            // println!("selector: {} {:?}", selector.shape, selector.data);
            rows_to_sel.clear();
            for row in rows[1..].iter_mut() {
                let row = match row {
                    Ok(row) => row.next().unwrap(),
                    Err(row) => row.clone(),
                };
                // println!("row: {:?}", row);
                if selector.rank() > row.rank() || is_empty {
                    rows_to_sel.push(Err(row));
                } else {
                    let row_shape = row.shape()[selector.rank()..].into();
                    rows_to_sel.push(Ok(row.into_row_shaped_slices(row_shape)));
                }
            }
            for sel_row_slice in selector.row_slices() {
                for &elem in sel_row_slice {
                    // println!("  elem: {}", elem);
                    let node = &branches[elem];
                    let arg_count = args[elem];
                    for (i, row) in rows_to_sel.iter_mut().rev().enumerate().rev() {
                        let row = match row {
                            Ok(row) => row.next().unwrap(),
                            Err(row) => row.clone(),
                        };
                        // println!("  row: {:?}", row);
                        if i < arg_count {
                            env.push(row);
                        }
                    }
                    env.exec(node.clone())?;
                    for i in 0..sig.outputs {
                        outputs[i].push(env.pop("switch output")?);
                    }
                }
            }
        }
        // Collect output
        if is_empty {
            new_shape[0] = 0;
        }
        for output in outputs.into_iter().rev() {
            let mut new_shape = new_shape.clone();
            let mut new_value = Value::from_row_values(output, env)?;
            if all_scalar {
                new_value.undo_fix();
            } else if is_empty {
                new_value.pop_row();
            }
            new_shape.extend_from_slice(&new_value.shape()[1..]);
            *new_value.shape_mut() = new_shape;
            new_value.validate_shape();
            env.push(new_value);
        }
    }
    if let Some(selector) = copied_selector {
        env.push_under(selector);
    }
    Ok(())
}

pub fn try_(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f, handler] = get_ops(ops, env)?;
    let f_sig = f.sig;
    let handler_sig = handler.sig;
    if env.stack_height() < f_sig.args {
        for i in 0..f_sig.args {
            env.pop(i + 1)?;
        }
    }
    let backup = env.clone_stack_top(f_sig.args.min(handler_sig.args))?;
    if let Err(mut err) = env.exec_clean_stack(f) {
        if err.is_case {
            err.is_case = false;
            return Err(err);
        }
        if handler_sig.args > f_sig.args {
            (env.rt.backend).save_error_color(err.to_string(), err.report().to_string());
            env.push(err.value());
        }
        for val in backup {
            env.push(val);
        }
        env.exec(handler)?;
    }
    Ok(())
}

pub fn format(parts: &[EcoString], env: &mut Uiua) -> UiuaResult {
    fn format_val(chars: &mut EcoVec<char>, val: Value) {
        match val {
            Value::Char(arr) if arr.rank() <= 1 => {
                if chars.is_empty() {
                    *chars = arr.data.into();
                } else {
                    ecovec_extend_cowslice(chars, arr.data);
                }
            }
            Value::Box(arr) if arr.rank() == 0 => format_val(chars, arr.into_scalar().unwrap().0),
            val => chars.extend(val.format().chars()),
        }
    }

    let mut chars = EcoVec::new();
    for (i, part) in parts.iter().enumerate() {
        if i > 0 {
            let value = env.pop(("format argument", i))?;
            format_val(&mut chars, value);
        }
        chars.extend(part.chars());
    }
    env.push(chars);
    Ok(())
}

#[repr(transparent)]
#[derive(Debug)]
struct ArrayCmpSlice<'a, T>(&'a [T]);

impl<T: ArrayValue> PartialEq for ArrayCmpSlice<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len() && self.0.iter().zip(other.0).all(|(a, b)| a.array_eq(b))
    }
}

impl<T: ArrayValue> Eq for ArrayCmpSlice<'_, T> {}

impl<T: ArrayValue> PartialOrd for ArrayCmpSlice<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ArrayValue> Ord for ArrayCmpSlice<'_, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0
            .iter()
            .zip(other.0)
            .map(|(a, b)| a.array_cmp(b))
            .find(|&o| o != Ordering::Equal)
            .unwrap_or(Ordering::Equal)
    }
}

impl<T: ArrayValue> Hash for ArrayCmpSlice<'_, T> {
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
    args: Vec<Value>,
    env: &Uiua,
) -> UiuaResult<FixedRowsData> {
    for a in 0..args.len() {
        let a_row_count = args[a].row_count();
        for b in a + 1..args.len() {
            let b_row_count = args[b].row_count();
            if a_row_count != b_row_count && !(a_row_count == 1 || b_row_count == 1) {
                return Err(env.error(format!(
                    "Cannot {prim} arrays with different number of rows, shapes {} and {}",
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
            *c *= scaling_factor;
        }
    }
    env.push(arr);
    Ok(())
}

pub fn path(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    env: &mut Uiua,
) -> UiuaResult {
    path_impl(neighbors, is_goal, heuristic, PathMode::All, env)
}

pub fn path_first(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    env: &mut Uiua,
) -> UiuaResult {
    path_impl(neighbors, is_goal, heuristic, PathMode::First, env)
}

pub fn path_pop(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    env: &mut Uiua,
) -> UiuaResult {
    path_impl(neighbors, is_goal, heuristic, PathMode::CostOnly, env)
}

enum PathMode {
    All,
    First,
    CostOnly,
}

fn path_impl(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    mode: PathMode,
    env: &mut Uiua,
) -> UiuaResult {
    let start = env.pop("start")?;
    let nei_sig = neighbors.sig;
    let heu_sig = heuristic
        .as_ref()
        .map(|h| h.sig)
        .unwrap_or_else(|| Signature::new(0, 1));
    let isg_sig = is_goal.sig;
    for (name, sig, req_out) in &[
        ("neighbors", nei_sig, [1, 2].as_slice()),
        ("goal", isg_sig, &[1]),
        ("heuristic", heu_sig, &[1]),
    ] {
        if !req_out.contains(&sig.outputs) {
            let count = if req_out.len() == 1 {
                "1"
            } else {
                "either 1 or 2"
            };
            return Err(env.error(format!(
                "{} {name} function must return {count} outputs \
                but its signature is {sig}",
                Primitive::Path.format()
            )));
        }
    }
    let arg_count = nei_sig.args.max(heu_sig.args).max(isg_sig.args) - 1;
    let mut args = Vec::with_capacity(arg_count);
    for i in 0..arg_count {
        args.push(env.pop(i + 1)?);
    }

    struct NodeCost {
        node: usize,
        cost: f64,
    }
    impl PartialEq for NodeCost {
        fn eq(&self, other: &Self) -> bool {
            self.cost.array_eq(&other.cost)
        }
    }
    impl Eq for NodeCost {}
    impl PartialOrd for NodeCost {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }
    impl Ord for NodeCost {
        fn cmp(&self, other: &Self) -> Ordering {
            self.cost.array_cmp(&other.cost).reverse()
        }
    }

    struct PathEnv<'a> {
        env: &'a mut Uiua,
        neighbors: SigNode,
        is_goal: SigNode,
        heuristic: Option<SigNode>,
        args: Vec<Value>,
    }

    impl PathEnv<'_> {
        fn heuristic(&mut self, node: &Value) -> UiuaResult<f64> {
            Ok(if let Some(heuristic) = &self.heuristic {
                let heu_args = heuristic.sig.args;
                for arg in (self.args.iter()).take(heu_args.saturating_sub(1)).rev() {
                    self.env.push(arg.clone());
                }
                if heu_args > 0 {
                    self.env.push(node.clone());
                }
                self.env.exec(heuristic.clone())?;
                let h = (self.env)
                    .pop("heuristic")?
                    .as_num(self.env, "Heuristic must be a number")?;
                if h < 0.0 {
                    return Err(self
                        .env
                        .error("Negative heuristic values are not allowed in A*"));
                }
                h
            } else {
                0.0
            })
        }
        fn neighbors(&mut self, node: &Value) -> UiuaResult<Vec<(Value, f64)>> {
            let nei_args = self.neighbors.sig.args;
            for arg in (self.args.iter()).take(nei_args.saturating_sub(1)).rev() {
                self.env.push(arg.clone());
            }
            if nei_args > 0 {
                self.env.push(node.clone());
            }
            self.env.exec(self.neighbors.clone())?;
            let (nodes, costs) = if self.neighbors.sig.outputs == 2 {
                let costs = (self.env.pop("neighbors costs")?)
                    .as_nums(self.env, "Costs must be a list of numbers")?;
                let nodes = self.env.pop("neighbors nodes")?;
                if costs.len() != nodes.row_count() {
                    return Err(self.env.error(format!(
                        "Number of nodes {} does not match number of costs {}",
                        nodes.row_count(),
                        costs.len(),
                    )));
                }
                if costs.iter().any(|&c| c < 0.0) {
                    return Err(self.env.error("Negative costs are not allowed in A*"));
                }
                (nodes, costs)
            } else {
                let nodes = self.env.pop("neighbors nodes")?;
                let costs = vec![1.0; nodes.row_count()];
                (nodes, costs)
            };
            Ok(nodes.into_rows().zip(costs).collect())
        }
        fn is_goal(&mut self, node: &Value) -> UiuaResult<bool> {
            let isg_args = self.is_goal.sig.args;
            for arg in (self.args.iter()).take(isg_args.saturating_sub(1)).rev() {
                self.env.push(arg.clone());
            }
            if isg_args > 0 {
                self.env.push(node.clone());
            }
            self.env.exec(self.is_goal.clone())?;
            let is_goal = (self.env.pop("is_goal")?)
                .as_bool(self.env, "path goal function must return a boolean")?;
            Ok(is_goal)
        }
    }

    let mut env = PathEnv {
        env,
        neighbors,
        heuristic,
        is_goal,
        args,
    };

    // Initialize state
    let mut to_see = BinaryHeap::new();
    let mut backing = vec![start.clone()];
    let mut indices: HashMap<Value, usize> = [(start, 0)].into();
    to_see.push(NodeCost { node: 0, cost: 0.0 });

    let mut came_from: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut full_cost: HashMap<usize, f64> = [(0, 0.0)].into();

    let mut shortest_cost = f64::INFINITY;
    let mut ends = BTreeSet::new();

    // Main pathing loop
    while let Some(NodeCost { node: curr, .. }) = to_see.pop() {
        env.env.respect_execution_limit()?;
        let curr_cost = full_cost[&curr];
        // Early exit if found a shorter path
        if curr_cost > shortest_cost || ends.contains(&curr) {
            continue;
        }
        // Check if reached a goal
        if env.is_goal(&backing[curr])? {
            ends.insert(curr);
            shortest_cost = curr_cost;
            if let PathMode::All = mode {
                continue;
            } else {
                break;
            }
        }
        // Check neighbors
        for (nei, nei_cost) in env.neighbors(&backing[curr])? {
            let nei = if let Some(index) = indices.get(&nei) {
                *index
            } else {
                let index = backing.len();
                indices.insert(nei.clone(), index);
                backing.push(nei);
                index
            };
            let tentative_full_cost = curr_cost + nei_cost;
            let neighbor_g_score = full_cost.get(&nei).copied().unwrap_or(f64::INFINITY);
            // Mark parents
            if tentative_full_cost <= neighbor_g_score {
                if let Some(parents) = came_from.get_mut(&nei) {
                    parents.push(curr);
                } else {
                    came_from.insert(nei, vec![curr]);
                }
                // Add to to see
                if tentative_full_cost < neighbor_g_score {
                    full_cost.insert(nei, tentative_full_cost);
                    to_see.push(NodeCost {
                        cost: tentative_full_cost + env.heuristic(&backing[nei])?,
                        node: nei,
                    });
                }
            }
        }
    }

    let env = env.env;
    env.push(shortest_cost);

    let mut paths = EcoVec::new();

    let make_path = |path: Vec<usize>| {
        if let Some(&[a, b]) = path
            .windows(2)
            .find(|w| backing[w[0]].shape() != backing[w[1]].shape())
        {
            return Err(env.error(format!(
                "Cannot make path from nodes with incompatible shapes {} and {}",
                backing[a].shape(),
                backing[b].shape()
            )));
        }
        let path: Vec<_> = path.into_iter().map(|i| backing[i].clone()).collect();
        Value::from_row_values(path, env)
    };

    match mode {
        PathMode::All => {
            for end in ends {
                let mut currs = vec![vec![end]];
                let mut these_paths = Vec::new();
                while !currs.is_empty() {
                    env.respect_execution_limit()?;
                    let mut new_paths = Vec::new();
                    currs.retain_mut(|path| {
                        let parents = came_from
                            .get(path.last().unwrap())
                            .map(|p| p.as_slice())
                            .unwrap_or(&[]);
                        match parents {
                            [] => {
                                these_paths.push(take(path));
                                false
                            }
                            &[parent] => {
                                path.push(parent);
                                true
                            }
                            &[parent, ref rest @ ..] => {
                                for &parent in rest {
                                    let mut path = path.clone();
                                    path.push(parent);
                                    new_paths.push(path);
                                }
                                path.push(parent);
                                true
                            }
                        }
                    });
                    currs.extend(new_paths);
                }
                for mut path in these_paths {
                    path.reverse();
                    paths.push(Boxed(make_path(path)?));
                }
            }
            env.push(paths);
        }
        PathMode::First => {
            let mut curr = ends
                .into_iter()
                .next()
                .ok_or_else(|| env.error("No path found"))?;
            let mut path = vec![curr];
            while let Some(from) = came_from.get(&curr) {
                path.push(from[0]);
                curr = from[0];
            }
            path.reverse();
            env.push(Boxed(make_path(path)?));
        }
        PathMode::CostOnly => {}
    }
    Ok(())
}
