//! Algorithms for reducing modifiers

use std::{collections::VecDeque, convert::identity, iter::repeat};

use ecow::{eco_vec, EcoVec};

use crate::{
    algorithm::{loops::flip, pervade::*},
    check::instrs_signature,
    cowslice::cowslice,
    Array, ArrayValue, Complex, FillKind, Function, ImplPrimitive, Instr, Primitive, Shape,
    Signature, Uiua, UiuaResult, Value,
};

pub fn reduce(depth: usize, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let xs = env.pop(1)?;
    match (f.as_flipped_primitive(&env.asm), xs) {
        (Some((Primitive::Join, false)), mut xs)
            if env.value_fill(FillKind::Shape).is_none()
                && env.value_fill(FillKind::Default).is_none() =>
        {
            let depth = depth.min(xs.rank());
            if xs.rank() - depth < 2 {
                env.push(xs);
                return Ok(());
            }
            let shape = xs.shape();
            let mut new_shape = Shape::with_capacity(xs.rank() - 1);
            new_shape.extend_from_slice(&shape[..depth]);
            new_shape.push(shape[depth] * shape[depth + 1]);
            new_shape.extend_from_slice(&shape[depth + 2..]);
            *xs.shape_mut() = new_shape;
            env.push(xs);
        }
        (Some((prim, flipped)), Value::Num(nums)) => {
            if let Err(nums) = reduce_nums(prim, flipped, nums, depth, env) {
                return generic_reduce(f, Value::Num(nums), depth, env);
            }
        }

        (Some((prim, flipped)), Value::Complex(nums)) => {
            if let Err(nums) = reduce_coms(prim, flipped, nums, depth, env) {
                return generic_reduce(f, Value::Complex(nums), depth, env);
            }
        }
        (Some((prim, flipped)), Value::Byte(bytes)) => {
            let fill = env.num_fill(FillKind::Default).ok();
            env.push::<Value>(match prim {
                Primitive::Add => {
                    fast_reduce_different(bytes, 0.0, fill, depth, add::num_num, add::num_byte)
                        .into()
                }
                Primitive::Sub if flipped => fast_reduce_different(
                    bytes,
                    0.0,
                    fill,
                    depth,
                    flip(sub::num_num),
                    flip(sub::byte_num),
                )
                .into(),
                Primitive::Sub => {
                    fast_reduce_different(bytes, 0.0, fill, depth, sub::num_num, sub::num_byte)
                        .into()
                }
                Primitive::Mul => {
                    fast_reduce_different(bytes, 1.0, fill, depth, mul::num_num, mul::num_byte)
                        .into()
                }
                Primitive::Div if flipped => fast_reduce_different(
                    bytes,
                    1.0,
                    fill,
                    depth,
                    flip(div::num_num),
                    flip(div::byte_num),
                )
                .into(),
                Primitive::Div => {
                    fast_reduce_different(bytes, 1.0, fill, depth, div::num_num, div::num_byte)
                        .into()
                }
                Primitive::Mod if flipped => fast_reduce_different(
                    bytes,
                    1.0,
                    fill,
                    depth,
                    flip(modulus::num_num),
                    flip(modulus::byte_num),
                )
                .into(),
                Primitive::Mod => fast_reduce_different(
                    bytes,
                    1.0,
                    fill,
                    depth,
                    modulus::num_num,
                    modulus::num_byte,
                )
                .into(),
                Primitive::Atan if flipped => fast_reduce_different(
                    bytes,
                    0.0,
                    fill,
                    depth,
                    flip(atan2::num_num),
                    flip(atan2::byte_num),
                )
                .into(),
                Primitive::Atan => {
                    fast_reduce_different(bytes, 0.0, fill, depth, atan2::num_num, atan2::num_byte)
                        .into()
                }
                Primitive::Max => {
                    let byte_fill = env.byte_fill(FillKind::Default).ok();
                    if bytes.row_count() == 0 || fill.is_some() && byte_fill.is_none() {
                        fast_reduce_different(
                            bytes,
                            f64::NEG_INFINITY,
                            fill,
                            depth,
                            max::num_num,
                            max::num_byte,
                        )
                        .into()
                    } else {
                        fast_reduce(bytes, 0, byte_fill, depth, max::byte_byte).into()
                    }
                }
                Primitive::Min => {
                    let byte_fill = env.byte_fill(FillKind::Default).ok();
                    if bytes.row_count() == 0 || fill.is_some() && byte_fill.is_none() {
                        fast_reduce_different(
                            bytes,
                            f64::INFINITY,
                            fill,
                            depth,
                            min::num_num,
                            min::num_byte,
                        )
                        .into()
                    } else {
                        fast_reduce(bytes, 0, byte_fill, depth, min::byte_byte).into()
                    }
                }
                _ => return generic_reduce(f, Value::Byte(bytes), depth, env),
            })
        }
        (_, xs) => {
            if env.value_fill(FillKind::Default).is_none() {
                if xs.row_count() == 0 {
                    let val = reduce_identity(f.instrs(env), xs).ok_or_else(|| {
                        env.error(format!(
                            "Cannot {} empty array. Function has no identity value.",
                            Primitive::Reduce.format()
                        ))
                    })?;
                    env.push(val);
                    return Ok(());
                }
                if xs.row_count() == 1 {
                    let val = reduce_one(f.instrs(env), xs);
                    env.push(val);
                    return Ok(());
                }
            }
            generic_reduce(f, xs, depth, env)?
        }
    }
    Ok(())
}

fn trim_instrs(mut instrs: &[Instr]) -> &[Instr] {
    use ImplPrimitive::*;
    use Primitive::*;
    let trim = |instr: &Instr| {
        matches!(
            instr,
            Instr::Prim(Stack | Trace, _)
                | Instr::ImplPrim(UnStack | UnTrace | BothTrace | UnBothTrace, _)
        )
    };
    while instrs.first().is_some_and(trim) {
        instrs = &instrs[1..];
    }
    while instrs.last().is_some_and(trim) {
        instrs = &instrs[..instrs.len() - 1];
    }
    instrs
}

fn reduce_identity(instrs: &[Instr], mut val: Value) -> Option<Value> {
    use Primitive::*;
    let instrs = trim_instrs(instrs);
    let mut shape = val.shape().clone();
    shape.make_row();
    let len: usize = shape.iter().product();
    let (first, tail) = instrs.split_first()?;
    let (last, init) = instrs.split_last()?;
    let init_sig = || instrs_signature(init).is_ok_and(|sig| sig.args == sig.outputs);
    let tail_sig = || instrs_signature(tail).is_ok_and(|sig| sig.args == 1 && sig.outputs == 1);
    Some(match first {
        Instr::Prim(Join, _) if tail_sig() => {
            if val.rank() < 2 {
                val.shape_mut()[0] = 0;
            } else {
                let first = val.shape_mut().remove(0);
                val.shape_mut()[0] *= first;
            }
            val
        }
        _ => match last {
            Instr::Prim(Add | Sub, _) if init_sig() => Array::new(shape, eco_vec![0u8; len]).into(),
            Instr::Prim(Mul | Div | Mod, _) if init_sig() => {
                Array::new(shape, eco_vec![1u8; len]).into()
            }
            Instr::Prim(Max, _) if init_sig() => {
                Array::new(shape, eco_vec![f64::NEG_INFINITY; len]).into()
            }
            Instr::Prim(Min, _) if init_sig() => {
                Array::new(shape, eco_vec![f64::INFINITY; len]).into()
            }
            Instr::Prim(Atan, _) if init_sig() => Array::new(shape, eco_vec![0.0; len]).into(),
            Instr::Prim(Join, _) if init_sig() => {
                if val.rank() < 2 {
                    val.shape_mut()[0] = 0;
                } else {
                    let first = val.shape_mut().remove(0);
                    val.shape_mut()[0] *= first;
                }
                val
            }
            Instr::Format { parts, .. } if parts.len() == 3 && init_sig() => {
                EcoVec::<char>::new().into()
            }
            _ => return None,
        },
    })
}

fn reduce_one(instrs: &[Instr], val: Value) -> Value {
    use Primitive::*;
    let instrs = trim_instrs(instrs);
    let row = val.row(0);
    let Some((first, tail)) = instrs.split_first() else {
        return val;
    };
    let (last, init) = instrs.split_last().unwrap();
    let init_sig = || instrs_signature(init).is_ok_and(|sig| sig.args == sig.outputs);
    let tail_sig = || instrs_signature(tail).is_ok_and(|sig| sig.args == 1 && sig.outputs == 1);
    match first {
        Instr::Prim(Join, _) if tail_sig() => val,
        _ => match last {
            Instr::Prim(Join, _) if init_sig() => {
                if val.rank() < 2 {
                    val
                } else {
                    row
                }
            }
            Instr::Format { parts, .. } if init_sig() && parts.len() == 3 => row.format().into(),
            _ => row,
        },
    }
}

macro_rules! reduce_math {
    ($fname:ident, $ty:ident, $f:ident, $fill:ident) => {
        #[allow(clippy::result_large_err)]
        fn $fname(
            prim: Primitive,
            flipped: bool,
            xs: Array<$ty>,
            depth: usize,
            env: &mut Uiua,
        ) -> Result<(), Array<$ty>>
        where
            $ty: From<f64>,
        {
            let fill = env.$fill(FillKind::Default).ok();
            env.push(match prim {
                Primitive::Add => fast_reduce(xs, 0.0.into(), fill, depth, add::$f),
                Primitive::Sub if flipped => {
                    fast_reduce(xs, 0.0.into(), fill, depth, flip(sub::$f))
                }
                Primitive::Sub => fast_reduce(xs, 0.0.into(), fill, depth, sub::$f),
                Primitive::Mul => fast_reduce(xs, 1.0.into(), fill, depth, mul::$f),
                Primitive::Div if flipped => {
                    fast_reduce(xs, 1.0.into(), fill, depth, flip(div::$f))
                }
                Primitive::Div => fast_reduce(xs, 1.0.into(), fill, depth, div::$f),
                Primitive::Mod if flipped => {
                    fast_reduce(xs, 1.0.into(), fill, depth, flip(modulus::$f))
                }
                Primitive::Mod => fast_reduce(xs, 1.0.into(), fill, depth, modulus::$f),
                Primitive::Atan if flipped => {
                    fast_reduce(xs, 0.0.into(), fill, depth, flip(atan2::$f))
                }
                Primitive::Atan => fast_reduce(xs, 0.0.into(), fill, depth, atan2::$f),
                Primitive::Max => fast_reduce(xs, f64::NEG_INFINITY.into(), fill, depth, max::$f),
                Primitive::Min => fast_reduce(xs, f64::INFINITY.into(), fill, depth, min::$f),
                _ => return Err(xs),
            });
            Ok(())
        }
    };
}

reduce_math!(reduce_nums, f64, num_num, num_fill);
reduce_math!(reduce_coms, Complex, com_x, complex_fill);

fn fast_reduce_different<T, U>(
    arr: Array<T>,
    identity: U,
    default: Option<U>,
    mut depth: usize,
    fuu: impl Fn(U, U) -> U,
    fut: impl Fn(U, T) -> U,
) -> Array<U>
where
    T: ArrayValue + Copy + Into<U>,
    U: ArrayValue + Copy,
{
    depth = depth.min(arr.rank());
    if depth == 0 && arr.rank() == 1 {
        return if let Some(default) = default {
            arr.data.into_iter().fold(default, fut).into()
        } else if arr.row_count() == 0 {
            identity.into()
        } else {
            let first = arr.data[0].into();
            arr.data.into_iter().skip(1).fold(first, fut).into()
        };
    }
    fast_reduce(arr.convert(), identity, default, depth, fuu)
}

fn fast_reduce<T>(
    mut arr: Array<T>,
    identity: T,
    default: Option<T>,
    mut depth: usize,
    f: impl Fn(T, T) -> T,
) -> Array<T>
where
    T: ArrayValue + Copy,
{
    depth = depth.min(arr.rank());
    if depth == 0 && arr.rank() == 1 {
        return if let Some(default) = default {
            arr.data.into_iter().fold(default, f).into()
        } else if arr.row_count() == 0 {
            identity.into()
        } else {
            let first = arr.data[0];
            arr.data.into_iter().skip(1).fold(first, f).into()
        };
    }
    match (arr.rank(), depth) {
        (r, d) if r == d => arr,
        (1, 0) => {
            let data = arr.data.as_mut_slice();
            let reduced = default.into_iter().chain(data.iter().copied()).reduce(f);
            if let Some(reduced) = reduced {
                if data.is_empty() {
                    arr.data.extend(Some(reduced));
                } else {
                    data[0] = reduced;
                    arr.data.truncate(1);
                }
            } else {
                arr.data.extend(Some(identity));
            }
            arr.shape = Shape::default();
            arr.validate_shape();
            arr
        }
        (_, 0) => {
            let row_len = arr.row_len();
            if row_len == 0 {
                arr.shape.remove(0);
                return Array::new(arr.shape, EcoVec::new());
            }
            let row_count = arr.row_count();
            if row_count == 0 {
                arr.shape.remove(0);
                let data = cowslice![identity; row_len];
                return Array::new(arr.shape, data);
            }
            let sliced = arr.data.as_mut_slice();
            let (acc, rest) = sliced.split_at_mut(row_len);
            if let Some(default) = default {
                for acc in &mut *acc {
                    *acc = f(default, *acc);
                }
            }
            rest.chunks_exact(row_len).fold(acc, |acc, row| {
                for (a, b) in acc.iter_mut().zip(row) {
                    *a = f(*a, *b);
                }
                acc
            });
            arr.data.truncate(row_len);
            arr.shape.remove(0);
            arr.validate_shape();
            arr
        }
        (_, depth) => {
            let chunk_count: usize = arr.shape[..depth].iter().product();
            let chunk_len: usize = arr.shape[depth..].iter().product();
            let chunk_row_len: usize = arr.shape[depth + 1..].iter().product();
            let data_slice = arr.data.as_mut_slice();
            if chunk_len == 0 {
                let val = default.unwrap_or(identity);
                arr.data = repeat(val).take(chunk_count * chunk_row_len).collect();
            } else {
                for c in 0..chunk_count {
                    let chunk_start = c * chunk_len;
                    let chunk = &mut data_slice[chunk_start..][..chunk_len];
                    let (acc, rest) = chunk.split_at_mut(chunk_row_len);
                    if let Some(default) = default {
                        for acc in &mut *acc {
                            *acc = f(default, *acc);
                        }
                    }
                    rest.chunks_exact_mut(chunk_row_len).fold(acc, |acc, row| {
                        for (a, b) in acc.iter_mut().zip(row) {
                            *a = f(*a, *b);
                        }
                        acc
                    });
                    data_slice
                        .copy_within(chunk_start..chunk_start + chunk_row_len, c * chunk_row_len);
                }
                arr.data.truncate(chunk_count * chunk_row_len);
            }
            arr.shape.remove(depth);
            arr.validate_shape();
            arr
        }
    }
}

fn generic_reduce(f: Function, xs: Value, depth: usize, env: &mut Uiua) -> UiuaResult {
    generic_reduce_impl(f, xs, depth, identity, env)
}

pub fn reduce_content(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    let xs = env.pop(1)?;
    if let (1, Some((Primitive::Join, false))) = (xs.rank(), f.as_flipped_primitive(&env.asm)) {
        if xs.row_count() == 0 {
            env.push(match xs {
                Value::Box(_) => Value::default(),
                value => value,
            });
            return Ok(());
        }
        let mut rows = xs.into_rows().map(Value::unboxed);
        let mut acc = rows.next().unwrap();
        if acc.rank() == 0 {
            acc.shape_mut().insert(0, 1);
        }
        for row in rows {
            acc = acc.join(row, env)?;
        }
        env.push(acc);
        return Ok(());
    }
    generic_reduce_impl(f, xs, 0, Value::unboxed, env)
}

fn generic_reduce_impl(
    f: Function,
    xs: Value,
    depth: usize,
    process: impl Fn(Value) -> Value + Copy,
    env: &mut Uiua,
) -> UiuaResult {
    let sig = f.signature();
    if let (0 | 1, 1) = (sig.args, sig.outputs) {
        // Backwards compatibility for deprecated reduce behavior
        for row in xs.into_rows() {
            env.push(process(row));
            env.call(f.clone())?;
        }
    } else {
        let val = generic_reduce_inner(f, xs, depth, process, env)?;
        env.push(val);
    }
    Ok(())
}

fn generic_reduce_inner(
    f: Function,
    xs: Value,
    depth: usize,
    process: impl Fn(Value) -> Value + Copy,
    env: &mut Uiua,
) -> UiuaResult<Value> {
    let sig = f.signature();
    if sig != (2, 1) {
        return Err(env.error(format!(
            "{}'s function's signature must be |2.1, but it is {sig}",
            Primitive::Reduce.format(),
        )));
    }
    match (sig.args, sig.outputs) {
        (2, 1) => {
            let mut rows = xs.into_rows();
            if depth > 0 {
                let mut new_rows = Vec::with_capacity(rows.len());
                for row in rows {
                    new_rows.push(generic_reduce_inner(
                        f.clone(),
                        row,
                        depth - 1,
                        process,
                        env,
                    )?);
                }
                let val = Value::from_row_values(new_rows, env)?;
                Ok(val)
            } else {
                let mut acc = (env.value_fill(FillKind::Default).cloned())
                    .or_else(|| rows.next())
                    .ok_or_else(|| {
                        env.error(format!("Cannot {} empty array", Primitive::Reduce.format()))
                    })?;
                acc = process(acc);
                acc = env.without_fill(|env| -> UiuaResult<Value> {
                    for row in rows {
                        env.push(process(row));
                        env.push(acc);
                        env.call(f.clone())?;
                        acc = env.pop("reduced function result")?;
                    }
                    Ok(acc)
                })?;
                Ok(acc)
            }
        }
        _ => Err(env.error(format!(
            "{}'s function's signature must be |2.1, but it is {sig}",
            Primitive::Reduce.format(),
        ))),
    }
}

pub fn scan(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let xs = env.pop(1)?;
    if xs.rank() == 0 {
        return Err(env.error(format!("Cannot {} rank 0 array", Primitive::Scan.format())));
    }
    match (f.as_flipped_primitive(&env.asm), xs) {
        (Some((prim, flipped)), Value::Num(nums)) => {
            let fill = env.num_fill(FillKind::Default).ok();
            let arr = match prim {
                Primitive::Eq => fast_scan(nums, fill, |a, b| is_eq::num_num(a, b) as f64),
                Primitive::Ne => fast_scan(nums, fill, |a, b| is_ne::num_num(a, b) as f64),
                Primitive::Add => fast_scan(nums, fill, add::num_num),
                Primitive::Sub if flipped => fast_scan(nums, fill, flip(sub::num_num)),
                Primitive::Sub => fast_scan(nums, fill, sub::num_num),
                Primitive::Mul => fast_scan(nums, fill, mul::num_num),
                Primitive::Div if flipped => fast_scan(nums, fill, flip(div::num_num)),
                Primitive::Div => fast_scan(nums, fill, div::num_num),
                Primitive::Mod if flipped => fast_scan(nums, fill, flip(modulus::num_num)),
                Primitive::Mod => fast_scan(nums, fill, modulus::num_num),
                Primitive::Atan if flipped => fast_scan(nums, fill, flip(atan2::num_num)),
                Primitive::Atan => fast_scan(nums, fill, atan2::num_num),
                Primitive::Max => fast_scan(nums, fill, max::num_num),
                Primitive::Min => fast_scan(nums, fill, min::num_num),
                _ => return generic_scan(f, Value::Num(nums), env),
            };
            env.push(arr);
            Ok(())
        }
        (Some((prim, flipped)), Value::Byte(bytes)) => {
            let byte_fill = env.byte_fill(FillKind::Default).ok();
            let num_fill = env.num_fill(FillKind::Default).ok();
            if byte_fill.is_none() && num_fill.is_some() {
                env.push_func(f);
                env.push(Value::Num(bytes.convert()));
                return scan(env);
            }
            match prim {
                Primitive::Eq => env.push(fast_scan(bytes, byte_fill, is_eq::generic)),
                Primitive::Ne => env.push(fast_scan(bytes, byte_fill, is_ne::generic)),
                Primitive::Add => {
                    env.push(fast_scan::<f64>(bytes.convert(), num_fill, add::num_num))
                }
                Primitive::Sub if flipped => env.push(fast_scan::<f64>(
                    bytes.convert(),
                    num_fill,
                    flip(sub::num_num),
                )),
                Primitive::Sub => {
                    env.push(fast_scan::<f64>(bytes.convert(), num_fill, sub::num_num))
                }
                Primitive::Mul => {
                    env.push(fast_scan::<f64>(bytes.convert(), num_fill, mul::num_num))
                }
                Primitive::Div if flipped => env.push(fast_scan::<f64>(
                    bytes.convert(),
                    num_fill,
                    flip(div::num_num),
                )),
                Primitive::Div => {
                    env.push(fast_scan::<f64>(bytes.convert(), num_fill, div::num_num))
                }
                Primitive::Mod if flipped => env.push(fast_scan::<f64>(
                    bytes.convert(),
                    num_fill,
                    flip(modulus::num_num),
                )),
                Primitive::Mod => env.push(fast_scan::<f64>(
                    bytes.convert(),
                    num_fill,
                    modulus::num_num,
                )),
                Primitive::Atan if flipped => env.push(fast_scan::<f64>(
                    bytes.convert(),
                    num_fill,
                    flip(atan2::num_num),
                )),
                Primitive::Atan => {
                    env.push(fast_scan::<f64>(bytes.convert(), num_fill, atan2::num_num))
                }
                Primitive::Max => env.push(fast_scan(bytes, byte_fill, u8::max)),
                Primitive::Min => env.push(fast_scan(bytes, byte_fill, u8::min)),
                _ => return generic_scan(f, Value::Byte(bytes), env),
            }
            Ok(())
        }
        (_, xs) => generic_scan(f, xs, env),
    }
}

fn fast_scan<T>(mut arr: Array<T>, default: Option<T>, f: impl Fn(T, T) -> T) -> Array<T>
where
    T: ArrayValue + Copy,
{
    match arr.shape.len() {
        0 => unreachable!("fast_scan called on unit array, should have been guarded against"),
        1 => {
            if arr.row_count() == 0 {
                return arr;
            }
            if let Some(default) = default {
                arr.data.extend_from_array([default]);
                arr.data.as_mut_slice().rotate_right(1);
                arr.shape[0] += 1;
            }
            let mut acc = arr.data[0];
            for val in arr.data.as_mut_slice().iter_mut().skip(1) {
                acc = f(acc, *val);
                *val = acc;
            }
            arr
        }
        _ => {
            let row_len: usize = arr.row_len();
            if arr.row_count() == 0 {
                return arr;
            }
            if let Some(default) = default {
                arr.data.extend_from_vec(vec![default; row_len]);
                arr.data.as_mut_slice().rotate_right(row_len);
                arr.shape[0] += 1;
            }
            let shape = arr.shape.clone();
            let mut new_data = EcoVec::with_capacity(arr.data.len());
            let mut rows = arr.into_rows();
            new_data.extend(rows.next().unwrap().data);
            for row in rows {
                let start = new_data.len() - row_len;
                for (i, r) in row.data.into_iter().enumerate() {
                    new_data.push(f(new_data[start + i], r));
                }
            }
            Array::new(shape, new_data)
        }
    }
}

fn generic_scan(f: Function, xs: Value, env: &mut Uiua) -> UiuaResult {
    let sig = f.signature();
    if sig != (2, 1) {
        return Err(env.error(format!(
            "{}'s function's signature must be |2.1, but it is {sig}",
            Primitive::Scan.format(),
        )));
    }
    if xs.row_count() == 0 {
        env.push(xs.first_dim_zero());
        return Ok(());
    }
    let row_count = xs.row_count();
    let mut rows = xs.into_rows();
    let mut acc = env
        .value_fill(FillKind::Default)
        .cloned()
        .unwrap_or_else(|| rows.next().unwrap());
    let mut scanned = Vec::with_capacity(row_count);
    scanned.push(acc.clone());
    env.without_fill(|env| -> UiuaResult {
        for row in rows.by_ref() {
            env.push(row);
            env.push(acc.clone());
            env.call(f.clone())?;
            acc = env.pop("scanned function result")?;
            scanned.push(acc.clone());
        }
        Ok(())
    })?;
    let val = Value::from_row_values(scanned, env)?;
    env.push(val);
    Ok(())
}

pub fn unscan(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    let mut xs = env.pop(1)?;
    if xs.rank() == 0 {
        return Err(env.error(format!("Cannot {} rank 0 array", ImplPrimitive::UnScan,)));
    }
    let sig = f.signature();
    if sig != (2, 1) {
        return Err(env.error(format!(
            "{} unscan's function's signature must be |2.1, but it is {sig}",
            ImplPrimitive::UnScan,
        )));
    }
    if xs.row_count() == 0 {
        env.push(xs.first_dim_zero());
        return Ok(());
    }

    match xs {
        Value::Num(nums) => match f.as_flipped_primitive(&env.asm) {
            Some((Primitive::Sub, false)) => {
                env.push(fast_invscan(nums, sub::num_num));
                return Ok(());
            }
            Some((Primitive::Div, false)) => {
                env.push(fast_invscan(nums, div::num_num));
                return Ok(());
            }
            _ => xs = Value::Num(nums),
        },
        Value::Byte(bytes) => match f.as_flipped_primitive(&env.asm) {
            Some((Primitive::Sub, false)) => {
                env.push(fast_invscan(bytes.convert(), sub::num_num));
                return Ok(());
            }
            Some((Primitive::Div, false)) => {
                env.push(fast_invscan(bytes.convert(), div::num_num));
                return Ok(());
            }
            _ => xs = Value::Byte(bytes),
        },
        val => xs = val,
    }

    let mut unscanned = Vec::with_capacity(xs.row_count());
    let mut rows = xs.into_rows();
    let mut curr = rows.next().unwrap();
    unscanned.push(curr.clone());
    env.without_fill(|env| -> UiuaResult {
        for row in rows {
            env.push(row.clone());
            env.push(curr);
            env.call(f.clone())?;
            unscanned.push(env.pop("unscanned function result")?);
            curr = row;
        }
        Ok(())
    })?;
    env.push(Value::from_row_values(unscanned, env)?);
    Ok(())
}

fn fast_invscan<T>(mut arr: Array<T>, f: impl Fn(T, T) -> T) -> Array<T>
where
    T: ArrayValue + Copy,
{
    match arr.shape.len() {
        0 => unreachable!("fast_invscan called on unit array, should have been guarded against"),
        1 => {
            if arr.row_count() == 0 {
                return arr;
            }
            let mut acc = arr.data[0];
            for val in arr.data.as_mut_slice().iter_mut().skip(1) {
                let temp = *val;
                *val = f(acc, *val);
                acc = temp;
            }
            arr
        }
        _ => {
            if arr.row_count() == 0 {
                return arr;
            }
            let row_len: usize = arr.row_len();
            let (acc, rest) = arr.data.as_mut_slice().split_at_mut(row_len);
            let mut acc = acc.to_vec();
            let mut temp = acc.clone();
            for row_slice in rest.chunks_exact_mut(row_len) {
                temp.copy_from_slice(row_slice);
                for (a, b) in acc.iter_mut().zip(row_slice) {
                    *b = f(*a, *b);
                }
                acc.copy_from_slice(&temp);
            }
            arr
        }
    }
}

pub fn fold(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    if sig.args <= sig.outputs {
        return Err(env.error(format!(
            "{}'s function must take more values than it returns, \
            but its signature is {}",
            Primitive::Fold.format(),
            sig
        )));
    }
    let iterable_count = sig.args - sig.outputs;
    let mut arrays = Vec::with_capacity(iterable_count);
    for i in 0..iterable_count {
        let mut val = env.pop(("iterated array", i + 1))?;
        arrays.push(if val.row_count() == 1 {
            val.unfix();
            Err(val)
        } else {
            Ok(val.into_rows())
        });
    }
    if env.stack_height() < sig.outputs {
        for i in 0..sig.outputs {
            env.pop(("accumulator", i + 1))?;
        }
    }
    for i in 0..iterable_count {
        for j in i + 1..iterable_count {
            if let (Ok(a), Ok(b)) = (&arrays[i], &arrays[j]) {
                if a.len() != b.len() {
                    return Err(env.error(format!(
                        "Cannot {} arrays of different lengths: {} and {}",
                        Primitive::Fold.format(),
                        a.len(),
                        b.len()
                    )));
                }
            }
        }
    }
    let mut row_count = arrays
        .iter()
        .filter_map(|arr| arr.as_ref().ok())
        .map(|arr| arr.len())
        .max()
        .unwrap_or(0);
    if row_count == 0 && arrays.iter().all(Result::is_err) {
        row_count = 1;
    }
    for _ in 0..row_count {
        for array in arrays.iter_mut().rev() {
            env.push(match array {
                Ok(arr) => arr.next().unwrap(),
                Err(arr) => arr.clone(),
            });
        }
        env.call(f.clone())?;
    }
    Ok(())
}

pub fn adjacent(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    let n = env.pop(1)?;
    let xs = env.pop(2)?;
    if n.rank() != 0 {
        return adjacent_fallback(f, n, xs, env);
    }
    let n = n.as_int(env, "Window size must be an integer or list of integers")?;
    let n_abs = n.unsigned_abs();
    if n_abs == 0 {
        return Err(env.error("Window size cannot be zero"));
    }
    let n = n_abs;
    match (f.as_flipped_primitive(&env.asm), xs) {
        (Some((prim, flipped)), Value::Num(nums)) => env.push(match prim {
            Primitive::Add => fast_adjacent(nums, n, env, add::num_num),
            Primitive::Sub if flipped => fast_adjacent(nums, n, env, flip(sub::num_num)),
            Primitive::Sub => fast_adjacent(nums, n, env, sub::num_num),
            Primitive::Mul => fast_adjacent(nums, n, env, mul::num_num),
            Primitive::Div if flipped => fast_adjacent(nums, n, env, flip(div::num_num)),
            Primitive::Div => fast_adjacent(nums, n, env, div::num_num),
            Primitive::Mod if flipped => fast_adjacent(nums, n, env, flip(modulus::num_num)),
            Primitive::Mod => fast_adjacent(nums, n, env, modulus::num_num),
            Primitive::Atan if flipped => fast_adjacent(nums, n, env, flip(atan2::num_num)),
            Primitive::Atan => fast_adjacent(nums, n, env, atan2::num_num),
            Primitive::Max => fast_adjacent(nums, n, env, max::num_num),
            Primitive::Min => fast_adjacent(nums, n, env, min::num_num),
            _ => return generic_adjacent(f, Value::Num(nums), n, env),
        }?),
        (Some((prim, flipped)), Value::Byte(bytes)) => env.push::<Value>(match prim {
            Primitive::Add => fast_adjacent(bytes.convert(), n, env, add::num_num)?.into(),
            Primitive::Sub if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(sub::num_num))?.into()
            }
            Primitive::Sub => fast_adjacent(bytes.convert(), n, env, sub::num_num)?.into(),
            Primitive::Mul => fast_adjacent(bytes.convert(), n, env, mul::num_num)?.into(),
            Primitive::Div if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(div::num_num))?.into()
            }
            Primitive::Div => fast_adjacent(bytes.convert(), n, env, div::num_num)?.into(),
            Primitive::Mod if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(modulus::num_num))?.into()
            }
            Primitive::Mod => fast_adjacent(bytes.convert(), n, env, modulus::num_num)?.into(),
            Primitive::Atan if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(atan2::num_num))?.into()
            }
            Primitive::Atan => fast_adjacent(bytes.convert(), n, env, atan2::num_num)?.into(),
            Primitive::Max => fast_adjacent(bytes, n, env, max::byte_byte)?.into(),
            Primitive::Min => fast_adjacent(bytes, n, env, min::byte_byte)?.into(),
            _ => return generic_adjacent(f, Value::Byte(bytes), n, env),
        }),
        (_, xs) => generic_adjacent(f, xs, n, env)?,
    }
    Ok(())
}

fn adjacent_fallback(f: Function, n: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    let windows = n.windows(&xs, env)?;
    let mut new_rows = Vec::with_capacity(windows.row_count());
    for window in windows.into_rows() {
        env.push(window);
        env.push_func(f.clone());
        reduce(0, env)?;
        new_rows.push(env.pop("adjacent function result")?);
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}

fn fast_adjacent<T>(
    mut arr: Array<T>,
    n: usize,
    env: &Uiua,
    f: impl Fn(T, T) -> T,
) -> UiuaResult<Array<T>>
where
    T: Copy,
{
    match arr.rank() {
        0 => Err(env.error("Cannot get adjacency of scalar")),
        1 => {
            if arr.row_count() < n {
                return Ok(Array::new([0], EcoVec::new()));
            }
            let data = arr.data.as_mut_slice();
            for i in 0..data.len() - (n - 1) {
                let start = i;
                for j in 1..n {
                    data[start] = f(data[start], data[start + j]);
                }
            }
            arr.data.truncate(arr.data.len() - (n - 1));
            arr.shape[0] -= n - 1;
            arr.validate_shape();
            Ok(arr)
        }
        _ => {
            let row_len = arr.row_len();
            let row_count = arr.row_count();
            if row_count < n {
                let mut shape = arr.shape;
                shape[0] = 0;
                return Ok(Array::new(shape, EcoVec::new()));
            }
            let data = arr.data.as_mut_slice();
            for i in 0..row_count - (n - 1) {
                let start = i * row_len;
                for j in 1..n {
                    let next = (i + j) * row_len;
                    for k in 0..row_len {
                        data[start + k] = f(data[start + k], data[next + k]);
                    }
                }
            }
            arr.data.truncate(arr.data.len() - (n - 1) * row_len);
            arr.shape[0] -= n - 1;
            arr.validate_shape();
            Ok(arr)
        }
    }
}

fn generic_adjacent(f: Function, xs: Value, n: usize, env: &mut Uiua) -> UiuaResult {
    let sig = f.signature();
    if sig != (2, 1) {
        return Err(env.error(format!(
            "Adjacent's function's signature must be {}, but it is {}",
            Signature::new(2, 1),
            sig
        )));
    }
    if xs.row_count() < n {
        env.push(xs.first_dim_zero());
        return Ok(());
    }
    let win_count = xs.row_count() - (n - 1);
    let mut rows = xs.into_rows();
    let mut window = VecDeque::with_capacity(n);
    let mut new_rows = Vec::with_capacity(win_count);
    window.extend(rows.by_ref().take(n));
    for _ in 0..win_count {
        let mut acc = window.pop_front().unwrap();
        for row in &window {
            env.push(row.clone());
            env.push(acc);
            env.call(f.clone())?;
            acc = env.pop("adjacent function result")?;
        }
        new_rows.push(acc);
        window.extend(rows.next());
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}
