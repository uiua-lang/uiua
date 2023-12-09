//! Algorithms for reducing modifiers

use ecow::EcoVec;

use crate::{
    algorithm::{loops::flip, pervade::*},
    array::{Array, ArrayValue, Shape},
    cowslice::cowslice,
    function::Function,
    value::Value,
    Primitive, Uiua, UiuaResult,
};

pub fn reduce(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let xs = env.pop(1)?;

    match (f.as_flipped_primitive(), xs) {
        (Some((Primitive::Join, false)), mut xs) if !env.unpack_boxes() => {
            if xs.rank() < 2 {
                env.push(xs);
                return Ok(());
            }
            let shape = xs.shape();
            let mut new_shape = Shape::with_capacity(xs.rank() - 1);
            new_shape.push(shape[0] * shape[1]);
            new_shape.extend_from_slice(&shape[2..]);
            *xs.shape_mut() = new_shape;
            env.push(xs);
        }
        (Some((prim, flipped)), Value::Num(nums)) => {
            if let Err(nums) = reduce_nums(prim, flipped, nums, env) {
                return generic_reduce(f, Value::Num(nums), env);
            }
        }

        (Some((prim, flipped)), Value::Complex(nums)) => {
            if let Err(nums) = reduce_coms(prim, flipped, nums, env) {
                return generic_reduce(f, Value::Complex(nums), env);
            }
        }
        #[cfg(feature = "bytes")]
        (Some((prim, flipped)), Value::Byte(bytes)) => env.push(match prim {
            Primitive::Add => fast_reduce(bytes.convert(), 0.0, add::num_num),
            Primitive::Sub if flipped => fast_reduce(bytes.convert(), 0.0, flip(sub::num_num)),
            Primitive::Sub => fast_reduce(bytes.convert(), 0.0, sub::num_num),
            Primitive::Mul => fast_reduce(bytes.convert(), 1.0, mul::num_num),
            Primitive::Div if flipped => fast_reduce(bytes.convert(), 1.0, flip(div::num_num)),
            Primitive::Div => fast_reduce(bytes.convert(), 1.0, div::num_num),
            Primitive::Mod if flipped => fast_reduce(bytes.convert(), 1.0, flip(modulus::num_num)),
            Primitive::Mod => fast_reduce(bytes.convert(), 1.0, modulus::num_num),
            Primitive::Atan if flipped => fast_reduce(bytes.convert(), 0.0, flip(atan2::num_num)),
            Primitive::Atan => fast_reduce(bytes.convert(), 0.0, atan2::num_num),
            Primitive::Max => fast_reduce(bytes.convert(), f64::NEG_INFINITY, max::num_num),
            Primitive::Min => fast_reduce(bytes.convert(), f64::INFINITY, min::num_num),
            _ => return generic_reduce(f, Value::Byte(bytes), env),
        }),
        (_, xs) => generic_reduce(f, xs, env)?,
    }
    Ok(())
}

macro_rules! reduce_math {
    ($fname:ident, $ty:ty, $f:ident) => {
        #[allow(clippy::result_large_err)]
        fn $fname(
            prim: Primitive,
            flipped: bool,
            xs: Array<$ty>,
            env: &mut Uiua,
        ) -> Result<(), Array<$ty>>
        where
            $ty: From<f64>,
        {
            env.push(match prim {
                Primitive::Add => fast_reduce(xs, 0.0.into(), add::$f),
                Primitive::Sub if flipped => fast_reduce(xs, 0.0.into(), flip(sub::$f)),
                Primitive::Sub => fast_reduce(xs, 0.0.into(), sub::$f),
                Primitive::Mul => fast_reduce(xs, 1.0.into(), mul::$f),
                Primitive::Div if flipped => fast_reduce(xs, 1.0.into(), flip(div::$f)),
                Primitive::Div => fast_reduce(xs, 1.0.into(), div::$f),
                Primitive::Mod if flipped => fast_reduce(xs, 1.0.into(), flip(modulus::$f)),
                Primitive::Mod => fast_reduce(xs, 1.0.into(), modulus::$f),
                Primitive::Atan if flipped => fast_reduce(xs, 0.0.into(), flip(atan2::$f)),
                Primitive::Atan => fast_reduce(xs, 0.0.into(), atan2::$f),
                Primitive::Max => fast_reduce(xs, f64::NEG_INFINITY.into(), max::$f),
                Primitive::Min => fast_reduce(xs, f64::INFINITY.into(), min::$f),
                _ => return Err(xs),
            });
            Ok(())
        }
    };
}

reduce_math!(reduce_nums, f64, num_num);

reduce_math!(reduce_coms, crate::Complex, com_x);

pub fn fast_reduce<T>(mut arr: Array<T>, identity: T, f: impl Fn(T, T) -> T) -> Array<T>
where
    T: ArrayValue + Copy,
{
    match arr.shape.len() {
        0 => arr,
        1 => {
            let data = arr.data.as_mut_slice();
            let reduced = data.iter().copied().reduce(f);
            if let Some(reduced) = reduced {
                data[0] = reduced;
                arr.data.truncate(1);
            } else {
                arr.data.extend(Some(identity));
            }
            arr.shape = Shape::default();
            arr
        }
        _ => {
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
            rest.chunks_exact(row_len).fold(acc, |acc, row| {
                for (a, b) in acc.iter_mut().zip(row) {
                    *a = f(*a, *b);
                }
                acc
            });
            arr.data.truncate(row_len);
            arr.shape.remove(0);
            arr
        }
    }
}

fn generic_reduce(f: Function, xs: Value, env: &mut Uiua) -> UiuaResult {
    let sig = f.signature();
    match (sig.args, sig.outputs) {
        (0 | 1, 1) => {
            for row in xs.into_rows() {
                env.push(row);
                env.call(f.clone())?;
            }
        }
        (2, 1) => {
            let mut rows = xs.into_rows();
            let mut acc = rows.next().ok_or_else(|| {
                env.error(format!("Cannot {} empty array", Primitive::Reduce.format()))
            })?;
            if env.unpack_boxes() {
                acc.unpack();
            }
            for row in rows {
                env.push(row);
                env.push(acc);
                env.call(f.clone())?;
                acc = env.pop("reduced function result")?;
            }
            env.push(acc);
        }
        _ => {
            return Err(env.error(format!(
                "{}'s function's signature must be |2.1, but it is {sig}",
                Primitive::Reduce.format(),
            )))
        }
    }
    Ok(())
}

pub fn scan(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let xs = env.pop(1)?;
    if xs.rank() == 0 {
        return Err(env.error(format!("Cannot {} rank 0 array", Primitive::Scan.format())));
    }
    match (f.as_flipped_primitive(), xs) {
        (Some((prim, flipped)), Value::Num(nums)) => {
            let arr = match prim {
                Primitive::Eq => fast_scan(nums, |a, b| is_eq::num_num(a, b) as f64),
                Primitive::Ne => fast_scan(nums, |a, b| is_ne::num_num(a, b) as f64),
                Primitive::Add => fast_scan(nums, add::num_num),
                Primitive::Sub if flipped => fast_scan(nums, flip(sub::num_num)),
                Primitive::Sub => fast_scan(nums, sub::num_num),
                Primitive::Mul => fast_scan(nums, mul::num_num),
                Primitive::Div if flipped => fast_scan(nums, flip(div::num_num)),
                Primitive::Div => fast_scan(nums, div::num_num),
                Primitive::Mod if flipped => fast_scan(nums, flip(modulus::num_num)),
                Primitive::Mod => fast_scan(nums, modulus::num_num),
                Primitive::Atan if flipped => fast_scan(nums, flip(atan2::num_num)),
                Primitive::Atan => fast_scan(nums, atan2::num_num),
                Primitive::Max => fast_scan(nums, max::num_num),
                Primitive::Min => fast_scan(nums, min::num_num),
                _ => return generic_scan(f, Value::Num(nums), env),
            };
            env.push(arr);
            Ok(())
        }
        #[cfg(feature = "bytes")]
        (Some((prim, flipped)), Value::Byte(bytes)) => {
            match prim {
                Primitive::Eq => env.push(fast_scan(bytes, is_eq::generic)),
                Primitive::Ne => env.push(fast_scan(bytes, is_ne::generic)),
                Primitive::Add => env.push(fast_scan::<f64>(bytes.convert(), add::num_num)),
                Primitive::Sub if flipped => {
                    env.push(fast_scan::<f64>(bytes.convert(), flip(sub::num_num)))
                }
                Primitive::Sub => env.push(fast_scan::<f64>(bytes.convert(), sub::num_num)),
                Primitive::Mul => env.push(fast_scan::<f64>(bytes.convert(), mul::num_num)),
                Primitive::Div if flipped => {
                    env.push(fast_scan::<f64>(bytes.convert(), flip(div::num_num)))
                }
                Primitive::Div => env.push(fast_scan::<f64>(bytes.convert(), div::num_num)),
                Primitive::Mod if flipped => {
                    env.push(fast_scan::<f64>(bytes.convert(), flip(modulus::num_num)))
                }
                Primitive::Mod => env.push(fast_scan::<f64>(bytes.convert(), modulus::num_num)),
                Primitive::Atan if flipped => {
                    env.push(fast_scan::<f64>(bytes.convert(), flip(atan2::num_num)))
                }
                Primitive::Atan => env.push(fast_scan::<f64>(bytes.convert(), atan2::num_num)),
                Primitive::Max => env.push(fast_scan(bytes, u8::max)),
                Primitive::Min => env.push(fast_scan(bytes, u8::min)),
                _ => return generic_scan(f, Value::Byte(bytes), env),
            }
            Ok(())
        }
        (_, xs) => generic_scan(f, xs, env),
    }
}

fn fast_scan<T>(mut arr: Array<T>, f: impl Fn(T, T) -> T) -> Array<T>
where
    T: ArrayValue + Copy,
{
    match arr.shape.len() {
        0 => unreachable!("fast_scan called on unit array, should have been guarded against"),
        1 => {
            if arr.row_count() == 0 {
                return arr;
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
    let mut acc = rows.next().unwrap();
    let mut scanned = Vec::with_capacity(row_count);
    scanned.push(acc.clone());
    for row in rows.by_ref() {
        env.push(row);
        env.push(acc.clone());
        env.call(f.clone())?;
        acc = env.pop("scanned function result")?;
        scanned.push(acc.clone());
    }
    let val = if rows.len() == 0 {
        Value::from_row_values(scanned, env)?
    } else {
        let rows: Vec<Value> = scanned.into_iter().chain(rows).collect();
        Value::from_row_values(rows, env)?
    };
    env.push(val);
    Ok(())
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
        let val = env.pop(i + 1)?;
        arrays.push(if val.row_count() == 1 {
            Err(val.into_rows().next().unwrap())
        } else {
            Ok(val.into_rows())
        });
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
    let row_count = arrays
        .iter()
        .map(|arr| match arr {
            Ok(arr) => arr.len(),
            Err(_) => 1,
        })
        .max()
        .unwrap_or(0);
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
