//! Algorithms for tabling modifiers

use std::mem::take;

use ecow::EcoVec;
use tinyvec::tiny_vec;

use crate::{
    algorithm::{
        loops::{rank_list, rank_to_depth},
        pervade::*,
    },
    array::{Array, ArrayValue, Shape},
    function::Function,
    value::Value,
    Primitive, Uiua, UiuaResult,
};

use super::{loops::flip, multi_output};

pub fn table(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let xs = env.pop(1)?;
    let ys = env.pop(2)?;
    match (f.as_flipped_primitive(), xs, ys) {
        (Some((prim, flipped)), Value::Num(xs), Value::Num(ys)) => {
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        #[cfg(feature = "bytes")]
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
            Primitive::Div => env.push(fast_table(xs, ys, div::byte_byte)),
            Primitive::Mod if flipped => env.push(fast_table(xs, ys, flip(modulus::byte_byte))),
            Primitive::Mod => env.push(fast_table(xs, ys, modulus::byte_byte)),
            Primitive::Atan if flipped => env.push(fast_table::<f64, f64, _>(
                xs.convert(),
                ys.convert(),
                flip(atan2::num_num),
            )),
            Primitive::Atan => env.push(fast_table::<f64, f64, _>(
                xs.convert(),
                ys.convert(),
                atan2::num_num,
            )),
            Primitive::Complex if flipped => env.push(fast_table(xs, ys, flip(complex::byte_byte))),
            Primitive::Complex => env.push(fast_table(xs, ys, complex::byte_byte)),
            Primitive::Min => env.push(fast_table(xs, ys, min::byte_byte)),
            Primitive::Max => env.push(fast_table(xs, ys, max::byte_byte)),
            Primitive::Join | Primitive::Couple => {
                env.push(fast_table_join_or_couple(xs, ys, flipped))
            }
            _ => generic_table(f, Value::Byte(xs), Value::Byte(ys), env)?,
        },

        (Some((prim, flipped)), Value::Complex(xs), Value::Complex(ys)) => {
            if let Err((xs, ys)) = table_coms(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Complex(xs), Value::Complex(ys), env);
            }
        }
        #[cfg(feature = "bytes")]
        (Some((prim, flipped)), Value::Num(xs), Value::Byte(ys)) => {
            let ys = ys.convert();
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        #[cfg(feature = "bytes")]
        (Some((prim, flipped)), Value::Byte(xs), Value::Num(ys)) => {
            let xs = xs.convert();
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }

        (Some((prim, flipped)), Value::Num(xs), Value::Complex(ys)) => {
            let xs = xs.convert();
            if let Err((xs, ys)) = table_coms(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Complex(xs), Value::Complex(ys), env);
            }
        }

        (Some((prim, flipped)), Value::Complex(xs), Value::Num(ys)) => {
            let ys = ys.convert();
            if let Err((xs, ys)) = table_coms(prim, flipped, xs, ys, env) {
                return generic_table(f, Value::Complex(xs), Value::Complex(ys), env);
            }
        }
        // Boxes
        (Some((Primitive::Join | Primitive::Couple, flipped)), Value::Box(xs), ys) => {
            env.push(fast_table_join_or_couple(xs, ys.coerce_to_boxes(), flipped))
        }
        (Some((Primitive::Join | Primitive::Couple, flipped)), xs, Value::Box(ys)) => {
            env.push(fast_table_join_or_couple(xs.coerce_to_boxes(), ys, flipped))
        }
        // Chars
        (
            Some((Primitive::Join | Primitive::Couple, flipped)),
            Value::Char(xs),
            Value::Char(ys),
        ) => env.push(fast_table_join_or_couple(xs, ys, flipped)),
        (_, xs, ys) => generic_table(f, xs, ys, env)?,
    }
    Ok(())
}

macro_rules! table_math {
    ($fname:ident, $ty:ty, $f:ident) => {
        #[allow(clippy::result_large_err)]
        fn $fname(
            prim: Primitive,
            flipped: bool,
            xs: Array<$ty>,
            ys: Array<$ty>,
            env: &mut Uiua,
        ) -> Result<(), (Array<$ty>, Array<$ty>)> {
            match prim {
                Primitive::Eq => env.push(fast_table(xs, ys, is_eq::$f)),
                Primitive::Ne => env.push(fast_table(xs, ys, is_ne::$f)),
                Primitive::Lt if flipped => env.push(fast_table(xs, ys, flip(is_lt::$f))),
                Primitive::Lt => env.push(fast_table(xs, ys, is_lt::$f)),
                Primitive::Gt if flipped => env.push(fast_table(xs, ys, flip(is_gt::$f))),
                Primitive::Gt => env.push(fast_table(xs, ys, is_gt::$f)),
                Primitive::Le if flipped => env.push(fast_table(xs, ys, flip(is_le::$f))),
                Primitive::Le => env.push(fast_table(xs, ys, is_le::$f)),
                Primitive::Ge if flipped => env.push(fast_table(xs, ys, flip(is_ge::$f))),
                Primitive::Ge => env.push(fast_table(xs, ys, is_ge::$f)),
                Primitive::Add => env.push(fast_table(xs, ys, add::$f)),
                Primitive::Sub if flipped => env.push(fast_table(xs, ys, flip(sub::$f))),
                Primitive::Sub => env.push(fast_table(xs, ys, sub::$f)),
                Primitive::Mul => env.push(fast_table(xs, ys, mul::$f)),
                Primitive::Div if flipped => env.push(fast_table(xs, ys, flip(div::$f))),
                Primitive::Div => env.push(fast_table(xs, ys, div::$f)),
                Primitive::Mod if flipped => env.push(fast_table(xs, ys, flip(modulus::$f))),
                Primitive::Mod => env.push(fast_table(xs, ys, modulus::$f)),
                Primitive::Atan if flipped => env.push(fast_table(xs, ys, flip(atan2::$f))),
                Primitive::Atan => env.push(fast_table(xs, ys, atan2::$f)),

                Primitive::Complex if flipped => env.push(fast_table(xs, ys, flip(complex::$f))),

                Primitive::Complex => env.push(fast_table(xs, ys, complex::$f)),
                Primitive::Min => env.push(fast_table(xs, ys, min::$f)),
                Primitive::Max => env.push(fast_table(xs, ys, max::$f)),
                Primitive::Join | Primitive::Couple => {
                    env.push(fast_table_join_or_couple(xs, ys, flipped))
                }
                _ => return Err((xs, ys)),
            }
            Ok(())
        }
    };
}

table_math!(table_nums, f64, num_num);

table_math!(table_coms, crate::Complex, com_x);

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

fn generic_table(f: Function, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let sig = f.signature();
    if sig.args != 2 {
        return Err(env.error(format!(
            "Table's function must take 2 arguments, but its signature is {sig}",
        )));
    }
    let mut new_shape = Shape::from(xs.shape());
    new_shape.extend_from_slice(ys.shape());
    let outputs = sig.outputs;
    let mut items = multi_output(
        outputs,
        Value::builder(xs.element_count() * ys.element_count()),
    );
    let y_values = ys.into_elements().collect::<Vec<_>>();
    for x in xs.into_elements() {
        for y in y_values.iter().cloned() {
            env.push(y);
            env.push(x.clone());
            env.call(f.clone())?;
            for i in 0..outputs {
                items[i].add_row(env.pop("tabled function result")?, env)?;
            }
        }
    }
    for items in items.into_iter().rev() {
        let mut tabled = items.finish();
        let mut new_shape = new_shape.clone();
        new_shape.extend_from_slice(&tabled.shape()[1..]);
        *tabled.shape_mut() = new_shape;
        tabled.validate_shape();
        env.push(tabled);
    }
    Ok(())
}

pub fn cross(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    match sig.args {
        0 | 1 => {
            return Err(env.error(format!(
                "Cross's function must take at least 2 arguments, but its signature is {sig}",
            )))
        }
        2 => {
            let xs = env.pop(1)?;
            let ys = env.pop(2)?;
            if sig.args != 2 {
                return Err(env.error(format!(
                    "Cross's function must take 2 arguments, but its signature is {sig}",
                )));
            }
            let new_shape = tiny_vec![xs.row_count(), ys.row_count()];
            let outputs = sig.outputs;
            let mut items = multi_output(outputs, Value::builder(xs.row_count() * ys.row_count()));
            let y_rows = ys.into_rows().collect::<Vec<_>>();
            for x_row in xs.into_rows() {
                for y_row in y_rows.iter().cloned() {
                    env.push(y_row);
                    env.push(x_row.clone());
                    env.call(f.clone())?;
                    for i in 0..outputs {
                        items[i].add_row(env.pop("crossed function result")?, env)?;
                    }
                }
            }
            for items in items.into_iter().rev() {
                let mut crossed = items.finish();
                let mut new_shape = new_shape.clone();
                new_shape.extend_from_slice(&crossed.shape()[1..]);
                *crossed.shape_mut() = new_shape;
                crossed.validate_shape();
                env.push(crossed);
            }
        }
        n => {
            let xs = env.pop(1)?;
            let ys = env.pop(2)?;
            let zs = env.pop(3)?;
            let mut others = Vec::with_capacity(n - 3);
            for i in 3..n {
                others.push(env.pop(i + 1)?);
            }
            let mut new_shape = Shape::with_capacity(n);
            new_shape.push(xs.row_count());
            new_shape.push(ys.row_count());
            new_shape.push(zs.row_count());
            for arg in &others {
                new_shape.push(arg.row_count());
            }
            let outputs = sig.outputs;
            let other_rows_product = others.iter().map(|a| a.row_count()).product::<usize>();
            let mut items = multi_output(
                outputs,
                Value::builder(
                    xs.row_count() * ys.row_count() * zs.row_count() * other_rows_product,
                ),
            );
            for x_row in xs.into_rows() {
                for y_row in ys.rows() {
                    for z_row in zs.rows() {
                        for mut i in 0..other_rows_product {
                            for arg in others.iter().rev() {
                                let j = i % arg.row_count();
                                env.push(arg.row(j));
                                i /= arg.row_count();
                            }
                            env.push(z_row.clone());
                            env.push(y_row.clone());
                            env.push(x_row.clone());
                            env.call(f.clone())?;
                            for i in 0..outputs {
                                items[i].add_row(env.pop("crossed function result")?, env)?;
                            }
                        }
                    }
                }
            }
            for items in items.into_iter().rev() {
                let mut crossed = items.finish();
                let mut new_shape = new_shape.clone();
                new_shape.extend_from_slice(&crossed.shape()[1..]);
                *crossed.shape_mut() = new_shape;
                crossed.validate_shape();
                env.push(crossed);
            }
        }
    }
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
        let arg = env.pop(i + 1)?;
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
    f: Function,
    args: &mut [Value],
    ns: &[usize],
    curr: usize,
    env: &mut Uiua,
) -> UiuaResult<Value> {
    if ns.iter().all(|&n| n == 0) {
        for arg in args.iter().rev() {
            env.push(arg.clone());
        }
        env.call(f)?;
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
