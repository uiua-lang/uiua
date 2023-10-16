//! Algorithms for tabling modifiers

use std::ops::{Add, Div, Mul, Sub};

use ecow::EcoVec;
use tinyvec::tiny_vec;

use crate::{
    array::{Array, ArrayValue, Shape},
    primitive::Primitive,
    run::{ArrayArg, FunctionArg},
    value::Value,
    Uiua, UiuaResult,
};

use super::loops::flip;

fn bin_bool<T: ArrayValue>(f: impl Fn(T, T) -> bool + Copy) -> impl Fn(T, T) -> u8 {
    move |x, y| f(x, y) as u8
}

pub fn table(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(FunctionArg(1))?;
    let xs = env.pop(ArrayArg(1))?;
    let ys = env.pop(ArrayArg(2))?;
    match (f.as_flipped_primitive(), xs, ys) {
        (Some((prim, flipped)), Value::Num(xs), Value::Num(ys)) => {
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        (Some((prim, flipped)), Value::Num(xs), Value::Byte(ys)) => {
            let ys = ys.convert();
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        (Some((prim, flipped)), Value::Byte(xs), Value::Num(ys)) => {
            let xs = xs.convert();
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        (Some((prim, flipped)), Value::Byte(xs), Value::Byte(ys)) => match prim {
            Primitive::Eq => env.push(fast_table(xs, ys, bin_bool(|x, y| x == y))),
            Primitive::Ne => env.push(fast_table(xs, ys, bin_bool(|x, y| x != y))),
            Primitive::Lt if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x < y))),
            Primitive::Lt => env.push(fast_table(xs, ys, bin_bool(|x, y| y < x))),
            Primitive::Gt if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x > y))),
            Primitive::Gt => env.push(fast_table(xs, ys, bin_bool(|x, y| y > x))),
            Primitive::Le if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x <= y))),
            Primitive::Le => env.push(fast_table(xs, ys, bin_bool(|x, y| y <= x))),
            Primitive::Ge if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x >= y))),
            Primitive::Ge => env.push(fast_table(xs, ys, bin_bool(|x, y| y >= x))),
            Primitive::Add => env.push(fast_table(xs, ys, |a, b| f64::from(a) + f64::from(b))),
            Primitive::Sub if flipped => {
                env.push(fast_table(xs, ys, |a, b| f64::from(a) - f64::from(b)))
            }
            Primitive::Sub => env.push(fast_table(xs, ys, |a, b| f64::from(b) - f64::from(a))),
            Primitive::Mul => env.push(fast_table(xs, ys, |a, b| f64::from(a) * f64::from(b))),
            Primitive::Div if flipped => {
                env.push(fast_table(xs, ys, |a, b| f64::from(a) / f64::from(b)))
            }
            Primitive::Div => env.push(fast_table(xs, ys, |a, b| f64::from(b) / f64::from(a))),
            Primitive::Min => env.push(fast_table(xs, ys, u8::min)),
            Primitive::Max => env.push(fast_table(xs, ys, u8::max)),
            Primitive::Join | Primitive::Couple => env.push(fast_table_join_or_couple(xs, ys)),
            _ => generic_table(f, Value::Byte(xs), Value::Byte(ys), env)?,
        },
        (_, xs, ys) => generic_table(f, xs, ys, env)?,
    }
    Ok(())
}

#[allow(clippy::result_large_err)]
fn table_nums(
    prim: Primitive,
    flipped: bool,
    xs: Array<f64>,
    ys: Array<f64>,
    env: &mut Uiua,
) -> Result<(), (Array<f64>, Array<f64>)> {
    match prim {
        Primitive::Eq => env.push(fast_table(xs, ys, bin_bool(|x, y| x == y))),
        Primitive::Ne => env.push(fast_table(xs, ys, bin_bool(|x, y| x != y))),
        Primitive::Lt if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x < y))),
        Primitive::Lt => env.push(fast_table(xs, ys, bin_bool(|x, y| y < x))),
        Primitive::Gt if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x > y))),
        Primitive::Gt => env.push(fast_table(xs, ys, bin_bool(|x, y| y > x))),
        Primitive::Le if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x <= y))),
        Primitive::Le => env.push(fast_table(xs, ys, bin_bool(|x, y| y <= x))),
        Primitive::Ge if flipped => env.push(fast_table(xs, ys, bin_bool(|x, y| x >= y))),
        Primitive::Ge => env.push(fast_table(xs, ys, bin_bool(|x, y| y >= x))),
        Primitive::Add => env.push(fast_table(xs, ys, Add::add)),
        Primitive::Sub if flipped => env.push(fast_table(xs, ys, Sub::sub)),
        Primitive::Sub => env.push(fast_table(xs, ys, flip(Sub::sub))),
        Primitive::Mul => env.push(fast_table(xs, ys, Mul::mul)),
        Primitive::Div if flipped => env.push(fast_table(xs, ys, Div::div)),
        Primitive::Div => env.push(fast_table(xs, ys, flip(Div::div))),
        Primitive::Min => env.push(fast_table(xs, ys, f64::min)),
        Primitive::Max => env.push(fast_table(xs, ys, f64::max)),
        Primitive::Join | Primitive::Couple => env.push(fast_table_join_or_couple(xs, ys)),
        _ => return Err((xs, ys)),
    }
    Ok(())
}

fn fast_table<A: ArrayValue, B: ArrayValue, C: ArrayValue>(
    a: Array<A>,
    b: Array<B>,
    f: impl Fn(A, B) -> C,
) -> Array<C> {
    let mut new_data = EcoVec::with_capacity(a.data.len() * b.data.len());
    for x in a.data {
        for y in b.data.iter().cloned() {
            new_data.push(f(x.clone(), y));
        }
    }
    let mut new_shape = a.shape;
    new_shape.extend_from_slice(&b.shape);
    Array::new(new_shape, new_data)
}

fn fast_table_join_or_couple<T: ArrayValue>(a: Array<T>, b: Array<T>) -> Array<T> {
    let mut new_data = EcoVec::with_capacity(a.data.len() * b.data.len() * 2);
    for x in a.data {
        for y in b.data.iter().cloned() {
            new_data.push(x.clone());
            new_data.push(y);
        }
    }
    let mut new_shape = a.shape;
    new_shape.extend_from_slice(&b.shape);
    new_shape.push(2);
    Array::new(new_shape, new_data)
}

fn generic_table(f: Value, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let sig = f.signature();
    if sig != (2, 1) {
        return Err(env.error(format!(
            "Table's function's signature must be |2.1, but it is {sig}"
        )));
    }
    let mut new_shape = Shape::from(xs.shape());
    new_shape.extend_from_slice(ys.shape());
    let mut items = Value::builder(xs.flat_len() * ys.flat_len());
    let y_values = ys.into_flat_values().collect::<Vec<_>>();
    for x in xs.into_flat_values() {
        for y in y_values.iter().cloned() {
            env.push(y);
            env.push(x.clone());
            env.call_error_on_break(f.clone(), "break is not allowed in table")?;
            let item = env.pop("tabled function result")?;
            item.validate_shape();
            items.add_row(item, &env)?;
        }
    }
    let mut tabled = items.finish();
    new_shape.extend_from_slice(&tabled.shape()[1..]);
    *tabled.shape_mut() = new_shape;
    tabled.validate_shape();
    env.push(tabled);
    Ok(())
}

pub fn cross(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(FunctionArg(1))?;
    let xs = env.pop(ArrayArg(1))?;
    let ys = env.pop(ArrayArg(2))?;
    let sig = f.signature();
    if sig != (2, 1) {
        return Err(env.error(format!(
            "Cross's function's signature must be |2.1, but it is {sig}"
        )));
    }
    let mut new_shape = tiny_vec![xs.row_count(), ys.row_count()];
    let mut items = Value::builder(xs.row_count() * ys.row_count());
    let y_rows = ys.into_rows().collect::<Vec<_>>();
    for x_row in xs.into_rows() {
        for y_row in y_rows.iter().cloned() {
            env.push(y_row);
            env.push(x_row.clone());
            env.call_error_on_break(f.clone(), "break is not allowed in cross")?;
            let item = env.pop("crossed function result")?;
            item.validate_shape();
            items.add_row(item, &env)?;
        }
    }
    let mut crossed = items.finish();
    new_shape.extend_from_slice(&crossed.shape()[1..]);
    *crossed.shape_mut() = new_shape;
    crossed.validate_shape();
    env.push(crossed);
    Ok(())
}
