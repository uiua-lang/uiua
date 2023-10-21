//! Algorithms for tabling modifiers

use std::{mem::take, sync::Arc};

use ecow::EcoVec;
use tinyvec::tiny_vec;

use crate::{
    algorithm::{
        loops::{rank_list, rank_to_depth},
        pervade::*,
    },
    array::{Array, ArrayValue, Shape},
    function::Function,
    primitive::Primitive,
    run::ArrayArg,
    value::Value,
    Uiua, UiuaResult,
};

use super::loops::flip;

pub fn table(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
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
            Primitive::Eq => env.push(fast_table(xs, ys, is_eq::generic)),
            Primitive::Ne => env.push(fast_table(xs, ys, is_ne::generic)),
            Primitive::Lt if flipped => env.push(fast_table(xs, ys, flip(is_lt::generic))),
            Primitive::Lt => env.push(fast_table(xs, ys, is_lt::generic)),
            Primitive::Gt if flipped => env.push(fast_table(xs, ys, flip(is_gt::generic))),
            Primitive::Gt => env.push(fast_table(xs, ys, is_gt::generic)),
            Primitive::Le if flipped => env.push(fast_table(xs, ys, flip(is_le::generic))),
            Primitive::Le => env.push(fast_table(xs, ys, is_le::generic)),
            Primitive::Ge if flipped => env.push(fast_table(xs, ys, flip(is_ge::generic))),
            Primitive::Ge => env.push(fast_table(xs, ys, is_ge::generic)),
            Primitive::Add => env.push(fast_table(xs, ys, add::byte_byte)),
            Primitive::Sub if flipped => env.push(fast_table(xs, ys, flip(sub::byte_byte))),
            Primitive::Sub => env.push(fast_table(xs, ys, sub::byte_byte)),
            Primitive::Mul => env.push(fast_table(xs, ys, mul::byte_byte)),
            Primitive::Div if flipped => env.push(fast_table(xs, ys, flip(div::byte_byte))),
            Primitive::Div => env.push(fast_table(xs, ys, flip(div::byte_byte))),
            Primitive::Min => env.push(fast_table(xs, ys, min::byte_byte)),
            Primitive::Max => env.push(fast_table(xs, ys, max::byte_byte)),
            Primitive::Join | Primitive::Couple => {
                env.push(fast_table_join_or_couple(xs, ys, flipped))
            }
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
        Primitive::Eq => env.push(fast_table(xs, ys, is_eq::num_num)),
        Primitive::Ne => env.push(fast_table(xs, ys, is_ne::num_num)),
        Primitive::Lt if flipped => env.push(fast_table(xs, ys, flip(is_lt::num_num))),
        Primitive::Lt => env.push(fast_table(xs, ys, is_lt::num_num)),
        Primitive::Gt if flipped => env.push(fast_table(xs, ys, flip(is_gt::num_num))),
        Primitive::Gt => env.push(fast_table(xs, ys, is_gt::num_num)),
        Primitive::Le if flipped => env.push(fast_table(xs, ys, flip(is_le::num_num))),
        Primitive::Le => env.push(fast_table(xs, ys, is_le::num_num)),
        Primitive::Ge if flipped => env.push(fast_table(xs, ys, flip(is_ge::num_num))),
        Primitive::Ge => env.push(fast_table(xs, ys, is_ge::num_num)),
        Primitive::Add => env.push(fast_table(xs, ys, add::num_num)),
        Primitive::Sub if flipped => env.push(fast_table(xs, ys, flip(sub::num_num))),
        Primitive::Sub => env.push(fast_table(xs, ys, sub::num_num)),
        Primitive::Mul => env.push(fast_table(xs, ys, mul::num_num)),
        Primitive::Div if flipped => env.push(fast_table(xs, ys, flip(div::num_num))),
        Primitive::Div => env.push(fast_table(xs, ys, flip(div::num_num))),
        Primitive::Min => env.push(fast_table(xs, ys, min::num_num)),
        Primitive::Max => env.push(fast_table(xs, ys, max::num_num)),
        Primitive::Join | Primitive::Couple => env.push(fast_table_join_or_couple(xs, ys, flipped)),
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

fn fast_table_join_or_couple<T: ArrayValue>(a: Array<T>, b: Array<T>, flipped: bool) -> Array<T> {
    let mut new_data = EcoVec::with_capacity(a.data.len() * b.data.len() * 2);
    if flipped {
        for x in a.data {
            for y in b.data.iter().cloned() {
                new_data.push(y);
                new_data.push(x.clone());
            }
        }
    } else {
        for x in a.data {
            for y in b.data.iter().cloned() {
                new_data.push(x.clone());
                new_data.push(y);
            }
        }
    }
    let mut new_shape = a.shape;
    new_shape.extend_from_slice(&b.shape);
    new_shape.push(2);
    Array::new(new_shape, new_data)
}

fn generic_table(f: Arc<Function>, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
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
            items.add_row(item, env)?;
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
    let f = env.pop_function()?;
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
            items.add_row(item, env)?;
        }
    }
    let mut crossed = items.finish();
    new_shape.extend_from_slice(&crossed.shape()[1..]);
    *crossed.shape_mut() = new_shape;
    crossed.validate_shape();
    env.push(crossed);
    Ok(())
}

pub fn combinate(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let ns = rank_list("Combinate", env)?;
    if ns.len() < 2 {
        return Err(env.error(format!(
            "Combinate's rank list must have at least 2 elements, but it has {}",
            ns.len()
        )));
    }
    let f = env.pop_function()?;
    let f_sig = f.signature();
    if f_sig.outputs != 1 {
        return Err(env.error(format!(
            "Combinate's function must return 1 value, but it returns {}",
            f_sig.outputs
        )));
    }
    if f_sig.args != ns.len() {
        return Err(env.error(format!(
            "Combinate's rank list has {} elements, but its function takes {} arguments",
            ns.len(),
            f_sig.args
        )));
    }
    let mut args = Vec::with_capacity(ns.len());
    for i in 0..ns.len() {
        let arg = env.pop(ArrayArg(i + 1))?;
        args.push(arg);
    }
    let ns: Vec<usize> = ns
        .into_iter()
        .zip(&args)
        .map(|(n, arg)| rank_to_depth(n, arg.rank()))
        .collect();
    let res = multi_combinate_recursive(f, &mut args, &ns, 0, env)?;
    env.push(res);
    Ok(())
}

fn multi_combinate_recursive(
    f: Arc<Function>,
    args: &mut [Value],
    ns: &[usize],
    curr: usize,
    env: &mut Uiua,
) -> UiuaResult<Value> {
    if ns.iter().all(|&n| n == 0) {
        for arg in args.iter().rev() {
            env.push(arg.clone());
        }
        env.call_error_on_break(f, "break is not allowed in combinate")?;
        Ok(env.pop("combinate's function result")?)
    } else if ns[curr] == 0 {
        multi_combinate_recursive(f, args, ns, curr + 1, env)
    } else {
        let curr_arg = take(&mut args[curr]);
        let mut res = Value::builder(curr_arg.row_count());
        let mut dec_ns = ns.to_vec();
        dec_ns[curr] -= 1;
        for row in curr_arg.rows() {
            args[curr] = row;
            let item = multi_combinate_recursive(f.clone(), args, &dec_ns, curr, env)?;
            res.add_row(item, env)?;
        }
        args[curr] = curr_arg;
        Ok(res.finish())
    }
}
