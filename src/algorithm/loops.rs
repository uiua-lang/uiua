use std::ops::{Add, Mul};

use crate::{
    array::{Array, ArrayType},
    primitive::Primitive,
    value::Value,
    Uiua, UiuaResult,
};

pub fn fold(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let acc = env.pop(2)?;
    let xs = env.pop(3)?;
    generic_fold(f, xs, Some(acc), env)
}

pub fn reduce(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    if let (Some(prim), true) = (f.as_primitive(), xs.is_array()) {
        let (shape, numbers) = match xs.array().ty() {
            ArrayType::Num => xs.into_array().into_shape_numbers(),
            ArrayType::Byte => {
                let (shape, bytes) = xs.into_array().into_shape_bytes();
                (shape, bytes.into_iter().map(|b| b as f64).collect())
            }
            _ => return generic_fold(f, xs, None, env),
        };
        match prim {
            Primitive::Add => env.push(reduce_nums(&shape, numbers, 0.0, Add::add)),
            Primitive::Mul => env.push(reduce_nums(&shape, numbers, 1.0, Mul::mul)),
            Primitive::Max => env.push(reduce_nums(&shape, numbers, f64::NEG_INFINITY, f64::max)),
            Primitive::Min => env.push(reduce_nums(&shape, numbers, f64::INFINITY, f64::min)),
            _ => generic_fold(f, Array::from((shape, numbers)).into(), None, env)?,
        }
        Ok(())
    } else {
        generic_fold(f, xs, None, env)
    }
}

fn reduce_nums(
    shape: &[usize],
    mut numbers: Vec<f64>,
    identity: f64,
    f: impl Fn(f64, f64) -> f64,
) -> Value {
    match shape.len() {
        0 => numbers[0].into(),
        1 => numbers.iter().copied().reduce(f).unwrap_or(identity).into(),
        _ => {
            let row_size: usize = shape[1..].iter().product();
            for i in 1..shape[0] {
                let start = i * row_size;
                for j in 0..row_size {
                    numbers[j] = f(numbers[j], numbers[start + j]);
                }
            }
            numbers.truncate(row_size);
            Array::from((shape[1..].to_vec(), numbers)).into()
        }
    }
}

fn generic_fold(f: Value, xs: Value, init: Option<Value>, env: &mut Uiua) -> UiuaResult {
    if !xs.is_array() {
        env.push(xs);
        return Ok(());
    }
    let mut cells = xs.into_array().into_values().into_iter().rev();
    let mut acc = init
        .or_else(|| cells.next())
        .or_else(|| f.as_primitive().and_then(|p| p.reduce_identity()))
        .ok_or_else(|| env.error("Cannot reduce empty array"))?;
    for cell in cells {
        env.push(acc);
        env.push(cell);
        env.push(f.clone());
        if env.call_catch_break()? {
            return Ok(());
        }
        acc = env.pop("reduced function result")?;
    }
    env.push(acc);
    Ok(())
}
