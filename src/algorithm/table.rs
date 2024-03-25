//! Algorithms for tabling modifiers

use ecow::eco_vec;

use crate::{
    algorithm::{pervade::*, zip::rows1},
    function::Function,
    random,
    value::Value,
    Array, ArrayValue, ImplPrimitive, Primitive, Shape, Uiua, UiuaResult,
};

use super::{loops::flip, multi_output, validate_size};

pub fn table(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    match sig.args {
        0 => env.call(f),
        1 => rows1(f, env.pop(1)?, env),
        n => {
            let xs = env.pop(1)?;
            let ys = env.pop(2)?;
            if n == 2 && xs.rank() <= 1 && ys.rank() <= 1 {
                table_list(f, xs, ys, env)
            } else {
                generic_table(f, xs, ys, env)
            }
        }
    }
}

fn generic_table(f: Function, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let sig = f.signature();
    match sig.args {
        2 => {
            let new_shape = Shape::from([xs.row_count(), ys.row_count()]);
            let outputs = sig.outputs;
            let mut items = multi_output(outputs, Value::builder(xs.row_count() * ys.row_count()));
            let y_rows = ys.into_rows().collect::<Vec<_>>();
            env.without_fill(|env| -> UiuaResult {
                for x_row in xs.into_rows() {
                    for y_row in y_rows.iter().cloned() {
                        env.push(y_row);
                        env.push(x_row.clone());
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            items[i].add_row(env.pop("tabled function result")?, env)?;
                        }
                    }
                }
                Ok(())
            })?;
            for items in items.into_iter().rev() {
                let mut tabled = items.finish();
                let mut new_shape = new_shape.clone();
                new_shape.extend_from_slice(&tabled.shape()[1..]);
                *tabled.shape_mut() = new_shape;
                tabled.validate_shape();
                env.push(tabled);
            }
        }
        n => {
            let zs = env.pop(3)?;
            let mut others = Vec::with_capacity(n - 3);
            for i in 3..n {
                others.push(env.pop(i + 1)?);
            }
            let mut new_shape = Shape::with_capacity(n);
            for arg in [&xs, &ys, &zs].into_iter().chain(&others) {
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
            env.without_fill(|env| -> UiuaResult {
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
                Ok(())
            })?;
            for items in items.into_iter().rev() {
                let mut tabled = items.finish();
                let mut new_shape = new_shape.clone();
                new_shape.extend_from_slice(&tabled.shape()[1..]);
                *tabled.shape_mut() = new_shape;
                tabled.validate_shape();
                env.push(tabled);
            }
        }
    }
    Ok(())
}

pub fn table_list(f: Function, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    match (f.as_flipped_primitive(&env.asm), xs, ys) {
        (Some((prim, flipped)), Value::Num(xs), Value::Num(ys)) => {
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env)? {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        (Some((prim, flipped)), Value::Byte(xs), Value::Byte(ys)) => match prim {
            Primitive::Eq => env.push(fast_table_list(xs, ys, is_eq::generic, env)?),
            Primitive::Ne => env.push(fast_table_list(xs, ys, is_ne::generic, env)?),
            Primitive::Lt if flipped => {
                env.push(fast_table_list(xs, ys, flip(is_lt::generic), env)?)
            }
            Primitive::Lt => env.push(fast_table_list(xs, ys, is_lt::generic, env)?),
            Primitive::Gt if flipped => {
                env.push(fast_table_list(xs, ys, flip(is_gt::generic), env)?)
            }
            Primitive::Gt => env.push(fast_table_list(xs, ys, is_gt::generic, env)?),
            Primitive::Le if flipped => {
                env.push(fast_table_list(xs, ys, flip(is_le::generic), env)?)
            }
            Primitive::Le => env.push(fast_table_list(xs, ys, is_le::generic, env)?),
            Primitive::Ge if flipped => {
                env.push(fast_table_list(xs, ys, flip(is_ge::generic), env)?)
            }
            Primitive::Ge => env.push(fast_table_list(xs, ys, is_ge::generic, env)?),
            Primitive::Add => env.push(fast_table_list(xs, ys, add::byte_byte, env)?),
            Primitive::Sub if flipped => {
                env.push(fast_table_list(xs, ys, flip(sub::byte_byte), env)?)
            }
            Primitive::Sub => env.push(fast_table_list(xs, ys, sub::byte_byte, env)?),
            Primitive::Mul => env.push(fast_table_list(xs, ys, mul::byte_byte, env)?),
            Primitive::Div if flipped => {
                env.push(fast_table_list(xs, ys, flip(div::byte_byte), env)?)
            }
            Primitive::Div => env.push(fast_table_list(xs, ys, div::byte_byte, env)?),
            Primitive::Mod if flipped => {
                env.push(fast_table_list(xs, ys, flip(modulus::byte_byte), env)?)
            }
            Primitive::Mod => env.push(fast_table_list(xs, ys, modulus::byte_byte, env)?),
            Primitive::Atan if flipped => env.push(fast_table_list::<f64, f64, _>(
                xs.convert(),
                ys.convert(),
                flip(atan2::num_num),
                env,
            )?),
            Primitive::Atan => env.push(fast_table_list::<f64, f64, _>(
                xs.convert(),
                ys.convert(),
                atan2::num_num,
                env,
            )?),
            Primitive::Complex if flipped => {
                env.push(fast_table_list(xs, ys, flip(complex::byte_byte), env)?)
            }
            Primitive::Complex => env.push(fast_table_list(xs, ys, complex::byte_byte, env)?),
            Primitive::Min => env.push(fast_table_list(xs, ys, min::byte_byte, env)?),
            Primitive::Max => env.push(fast_table_list(xs, ys, max::byte_byte, env)?),
            Primitive::Join | Primitive::Couple => {
                env.push(fast_table_list_join_or_couple(xs, ys, flipped, env)?)
            }
            _ => generic_table(f, Value::Byte(xs), Value::Byte(ys), env)?,
        },

        (Some((prim, flipped)), Value::Complex(xs), Value::Complex(ys)) => {
            if let Err((xs, ys)) = table_coms(prim, flipped, xs, ys, env)? {
                return generic_table(f, Value::Complex(xs), Value::Complex(ys), env);
            }
        }
        (Some((prim, flipped)), Value::Num(xs), Value::Byte(ys)) => {
            let ys = ys.convert();
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env)? {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        (Some((prim, flipped)), Value::Byte(xs), Value::Num(ys)) => {
            let xs = xs.convert();
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env)? {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }

        (Some((prim, flipped)), Value::Num(xs), Value::Complex(ys)) => {
            let xs = xs.convert();
            if let Err((xs, ys)) = table_coms(prim, flipped, xs, ys, env)? {
                return generic_table(f, Value::Complex(xs), Value::Complex(ys), env);
            }
        }

        (Some((prim, flipped)), Value::Complex(xs), Value::Num(ys)) => {
            let ys = ys.convert();
            if let Err((xs, ys)) = table_coms(prim, flipped, xs, ys, env)? {
                return generic_table(f, Value::Complex(xs), Value::Complex(ys), env);
            }
        }
        // Boxes
        (Some((Primitive::Join | Primitive::Couple, flipped)), Value::Box(xs), ys) => env.push(
            fast_table_list_join_or_couple(xs, ys.coerce_to_boxes(), flipped, env)?,
        ),
        (Some((Primitive::Join | Primitive::Couple, flipped)), xs, Value::Box(ys)) => env.push(
            fast_table_list_join_or_couple(xs.coerce_to_boxes(), ys, flipped, env)?,
        ),
        // Chars
        (
            Some((Primitive::Join | Primitive::Couple, flipped)),
            Value::Char(xs),
            Value::Char(ys),
        ) => env.push(fast_table_list_join_or_couple(xs, ys, flipped, env)?),
        (_, xs, ys) => match f.as_flipped_impl_primitive(&env.asm) {
            // Random
            Some((ImplPrimitive::ReplaceRand2, _)) => {
                let shape = [xs.row_count(), ys.row_count()];
                let mut data = eco_vec![0.0; xs.row_count() * ys.row_count()];
                for n in data.make_mut() {
                    *n = random();
                }
                env.push(Array::new(shape, data));
            }
            _ => generic_table(f, xs, ys, env)?,
        },
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
        ) -> UiuaResult<Result<(), (Array<$ty>, Array<$ty>)>> {
            match prim {
                Primitive::Eq => env.push(fast_table_list(xs, ys, is_eq::$f, env)?),
                Primitive::Ne => env.push(fast_table_list(xs, ys, is_ne::$f, env)?),
                Primitive::Lt if flipped => {
                    env.push(fast_table_list(xs, ys, flip(is_lt::$f), env)?)
                }
                Primitive::Lt => env.push(fast_table_list(xs, ys, is_lt::$f, env)?),
                Primitive::Gt if flipped => {
                    env.push(fast_table_list(xs, ys, flip(is_gt::$f), env)?)
                }
                Primitive::Gt => env.push(fast_table_list(xs, ys, is_gt::$f, env)?),
                Primitive::Le if flipped => {
                    env.push(fast_table_list(xs, ys, flip(is_le::$f), env)?)
                }
                Primitive::Le => env.push(fast_table_list(xs, ys, is_le::$f, env)?),
                Primitive::Ge if flipped => {
                    env.push(fast_table_list(xs, ys, flip(is_ge::$f), env)?)
                }
                Primitive::Ge => env.push(fast_table_list(xs, ys, is_ge::$f, env)?),
                Primitive::Add => env.push(fast_table_list(xs, ys, add::$f, env)?),
                Primitive::Sub if flipped => env.push(fast_table_list(xs, ys, flip(sub::$f), env)?),
                Primitive::Sub => env.push(fast_table_list(xs, ys, sub::$f, env)?),
                Primitive::Mul => env.push(fast_table_list(xs, ys, mul::$f, env)?),
                Primitive::Div if flipped => env.push(fast_table_list(xs, ys, flip(div::$f), env)?),
                Primitive::Div => env.push(fast_table_list(xs, ys, div::$f, env)?),
                Primitive::Mod if flipped => {
                    env.push(fast_table_list(xs, ys, flip(modulus::$f), env)?)
                }
                Primitive::Mod => env.push(fast_table_list(xs, ys, modulus::$f, env)?),
                Primitive::Atan if flipped => {
                    env.push(fast_table_list(xs, ys, flip(atan2::$f), env)?)
                }
                Primitive::Atan => env.push(fast_table_list(xs, ys, atan2::$f, env)?),

                Primitive::Complex if flipped => {
                    env.push(fast_table_list(xs, ys, flip(complex::$f), env)?)
                }

                Primitive::Complex => env.push(fast_table_list(xs, ys, complex::$f, env)?),
                Primitive::Min => env.push(fast_table_list(xs, ys, min::$f, env)?),
                Primitive::Max => env.push(fast_table_list(xs, ys, max::$f, env)?),
                Primitive::Join | Primitive::Couple => {
                    env.push(fast_table_list_join_or_couple(xs, ys, flipped, env)?)
                }
                _ => return Ok(Err((xs, ys))),
            }
            Ok(Ok(()))
        }
    };
}

table_math!(table_nums, f64, num_num);

table_math!(table_coms, crate::Complex, com_x);

fn fast_table_list<A: ArrayValue, B: ArrayValue, C: ArrayValue + Default>(
    a: Array<A>,
    b: Array<B>,
    f: impl Fn(A, B) -> C,
    env: &Uiua,
) -> UiuaResult<Array<C>> {
    let elem_count = a.data.len() * b.data.len();
    validate_size::<C>(elem_count, env)?;
    let mut new_data = eco_vec![C::default(); elem_count];
    let data_slice = new_data.make_mut();
    let mut i = 0;
    for x in a.data {
        for y in b.data.iter().cloned() {
            data_slice[i] = f(x.clone(), y);
            i += 1;
        }
    }
    let mut new_shape = a.shape;
    new_shape.extend_from_slice(&b.shape);
    Ok(Array::new(new_shape, new_data))
}

fn fast_table_list_join_or_couple<T: ArrayValue + Default>(
    a: Array<T>,
    b: Array<T>,
    flipped: bool,
    env: &Uiua,
) -> UiuaResult<Array<T>> {
    let elem_count = a.data.len() * b.data.len() * 2;
    validate_size::<T>(elem_count, env)?;
    let mut new_data = eco_vec![T::default(); elem_count];
    let data_slice = new_data.make_mut();
    let mut i = 0;
    if flipped {
        for x in a.data {
            for y in b.data.iter().cloned() {
                data_slice[i] = y;
                i += 1;
                data_slice[i] = x.clone();
                i += 1;
            }
        }
    } else {
        for x in a.data {
            for y in b.data.iter().cloned() {
                data_slice[i] = x.clone();
                i += 1;
                data_slice[i] = y;
                i += 1;
            }
        }
    }
    let mut new_shape = a.shape;
    new_shape.extend_from_slice(&b.shape);
    new_shape.push(2);
    Ok(Array::new(new_shape, new_data))
}
