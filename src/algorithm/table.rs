//! Algorithms for tabling modifiers

use ecow::eco_vec;

use crate::{
    Array, ArrayValue, Complex, ImplPrimitive, Node, Ops, Primitive, Shape, SigNode, Uiua,
    UiuaResult,
    algorithm::{FillContext, get_ops, pervade::*, zip::rows1},
    random,
    value::Value,
};

use super::{loops::flip, multi_output, reduce::reduce_impl, validate_size};

pub fn table(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    table_impl(f, env)
}

pub fn table_sub(f: SigNode, sub: i32, env: &mut Uiua) -> UiuaResult {
    let sig = f.sig;
    let inputs = env.top_n_mut(sig.args())?;
    let shapes: Vec<Shape> = inputs.iter().map(|v| v.shape.clone()).rev().collect();
    for val in inputs {
        if sub != -1 {
            val.deshape_sub(sub + 1, 0, false, &())?;
        }
    }
    table_impl(f, env)?;
    let outputs = env.top_n_mut(sig.outputs())?;
    let shape_prefix: Shape = if sub >= 0 {
        (shapes.into_iter())
            .flat_map(|s| {
                let take = s.len().saturating_sub(sub.unsigned_abs() as usize);
                s.into_iter().take(take)
            })
            .collect()
    } else {
        (shapes.into_iter())
            .flat_map(|s| s.into_iter().take(sub.unsigned_abs() as usize))
            .collect()
    };
    for val in outputs {
        let mut shape = shape_prefix.clone();
        shape.extend(val.shape[sig.args()..].iter().copied());
        val.shape = shape;
        val.validate();
    }
    Ok(())
}

pub(crate) fn table_impl(f: SigNode, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    match f.sig.args() {
        0 => env.exec(f),
        1 => rows1(f, env.pop(1)?, 0, false, env),
        n => {
            let xs = env.pop(1)?;
            let ys = env.pop(2)?;
            if n == 2 && (xs.rank() <= 1 || ys.rank() <= 1 || xs.shape[1..] == ys.shape[1..]) {
                table_list(f, xs, ys, env)
            } else {
                if let [
                    Node::Prim(Primitive::Mul, _),
                    Node::Mod(Primitive::Reduce, args, _),
                ] = f.node.as_slice()
                    && let [sn] = args.as_slice()
                    && let Some((Primitive::Add, _)) = sn.node.as_flipped_primitive()
                {
                    // Matrix mul
                    match (&xs, &ys) {
                        (Value::Num(a), Value::Num(b)) => {
                            return a.matrix_mul(b, env).map(|val| env.push(val));
                        }
                        (Value::Num(a), Value::Byte(b)) => {
                            return a.matrix_mul(&b.convert_ref(), env).map(|val| env.push(val));
                        }
                        (Value::Byte(a), Value::Num(b)) => {
                            return a.convert_ref().matrix_mul(b, env).map(|val| env.push(val));
                        }
                        (Value::Byte(a), Value::Byte(b)) => {
                            return a
                                .convert_ref()
                                .matrix_mul(&b.convert_ref(), env)
                                .map(|val| env.push(val));
                        }
                        _ => {}
                    }
                }
                generic_table(f, xs, ys, env)
            }
        }
    }
}

fn generic_table(f: SigNode, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let sig = f.sig;
    match sig.args() {
        2 => {
            let x_scalar = xs.rank() == 0;
            let y_scalar = ys.rank() == 0;
            validate_size::<f64>([sig.outputs(), xs.row_count(), ys.row_count()], env)?;
            let new_shape = Shape::from([xs.row_count(), ys.row_count()]);
            let outputs = sig.outputs();
            let mut items = multi_output(outputs, Value::builder(xs.row_count() * ys.row_count()));
            let y_rows = ys.into_rows().collect::<Vec<_>>();
            env.without_fill(|env| -> UiuaResult {
                for x_row in xs.into_rows() {
                    for y_row in y_rows.iter().cloned() {
                        env.push(y_row);
                        env.push(x_row.clone());
                        env.exec(f.clone())?;
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
                if y_scalar {
                    new_shape.remove(1);
                }
                if x_scalar {
                    new_shape.remove(0);
                }
                new_shape.extend_from_slice(&tabled.shape[1..]);
                tabled.shape = new_shape;
                tabled.validate();
                env.push(tabled);
            }
        }
        n => {
            let zs = env.pop(3)?;
            let mut others = Vec::with_capacity(n - 3);
            for i in 3..n {
                others.push(env.pop(i + 1)?);
            }
            validate_size::<f64>(
                [
                    sig.outputs(),
                    xs.row_count(),
                    ys.row_count(),
                    zs.row_count(),
                    others.iter().map(|a| a.row_count()).product::<usize>(),
                ],
                env,
            )?;
            let mut new_shape = Shape::with_capacity(n);
            for arg in [&xs, &ys, &zs].into_iter().chain(&others) {
                if arg.rank() > 0 {
                    new_shape.push(arg.row_count());
                }
            }
            let outputs = sig.outputs();
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
                                env.exec(f.clone())?;
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
                new_shape.extend_from_slice(&tabled.shape[1..]);
                tabled.shape = new_shape;
                tabled.validate();
                env.push(tabled);
            }
        }
    }
    Ok(())
}

pub fn table_list(f: SigNode, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    validate_size::<f64>([f.sig.outputs(), xs.row_count(), ys.row_count()], env)?;
    match (f.node.as_flipped_primitive(), xs, ys) {
        (Some((prim, flipped)), Value::Num(xs), Value::Num(ys)) => {
            if let Err((xs, ys)) = table_nums(prim, flipped, xs, ys, env)? {
                return generic_table(f, Value::Num(xs), Value::Num(ys), env);
            }
        }
        (Some((prim, flipped)), Value::Byte(xs), Value::Byte(ys)) => match prim {
            Primitive::Eq => env.push(fast_table_list(xs, ys, is_eq::generic, env)?),
            Primitive::Ne => env.push(fast_table_list(xs, ys, is_ne::generic, env)?),
            #[cfg(feature = "opt")]
            Primitive::Lt if flipped => {
                env.push(fast_table_list(xs, ys, flip(other_is_lt::generic), env)?)
            }
            Primitive::Lt if !flipped => {
                env.push(fast_table_list(xs, ys, other_is_lt::generic, env)?)
            }
            #[cfg(feature = "opt")]
            Primitive::Gt if flipped => {
                env.push(fast_table_list(xs, ys, flip(other_is_gt::generic), env)?)
            }
            Primitive::Gt if !flipped => {
                env.push(fast_table_list(xs, ys, other_is_gt::generic, env)?)
            }
            #[cfg(feature = "opt")]
            Primitive::Le if flipped => {
                env.push(fast_table_list(xs, ys, flip(other_is_le::generic), env)?)
            }
            Primitive::Le if !flipped => {
                env.push(fast_table_list(xs, ys, other_is_le::generic, env)?)
            }
            #[cfg(feature = "opt")]
            Primitive::Ge if flipped => {
                env.push(fast_table_list(xs, ys, flip(other_is_ge::generic), env)?)
            }
            Primitive::Ge if !flipped => {
                env.push(fast_table_list(xs, ys, other_is_ge::generic, env)?)
            }
            Primitive::Add => env.push(fast_table_list(xs, ys, add::byte_byte, env)?),
            #[cfg(feature = "opt")]
            Primitive::Sub if flipped => {
                env.push(fast_table_list(xs, ys, flip(sub::byte_byte), env)?)
            }
            Primitive::Sub if !flipped => env.push(fast_table_list(xs, ys, sub::byte_byte, env)?),
            Primitive::Mul => env.push(fast_table_list(xs, ys, mul::byte_byte, env)?),
            #[cfg(feature = "opt")]
            Primitive::Div if flipped => {
                env.push(fast_table_list(xs, ys, flip(div::byte_byte), env)?)
            }
            Primitive::Div if !flipped => env.push(fast_table_list(xs, ys, div::byte_byte, env)?),
            #[cfg(feature = "opt")]
            Primitive::Modulo if flipped => {
                env.push(fast_table_list(xs, ys, flip(modulo::byte_byte), env)?)
            }
            Primitive::Modulo if !flipped => {
                env.push(fast_table_list(xs, ys, modulo::byte_byte, env)?)
            }
            #[cfg(feature = "opt")]
            Primitive::Atan if flipped => env.push(fast_table_list::<f64, _>(
                xs.convert(),
                ys.convert(),
                flip(atan2::num_num),
                env,
            )?),
            Primitive::Atan if !flipped => env.push(fast_table_list::<f64, _>(
                xs.convert(),
                ys.convert(),
                atan2::num_num,
                env,
            )?),
            Primitive::Complex if flipped => {
                env.push(fast_table_list(xs, ys, flip(complex::byte_byte), env)?)
            }
            Primitive::Complex if !flipped => {
                env.push(fast_table_list(xs, ys, complex::byte_byte, env)?)
            }
            Primitive::Min => env.push(fast_table_list(xs, ys, min::byte_byte, env)?),
            Primitive::Max => env.push(fast_table_list(xs, ys, max::byte_byte, env)?),
            Primitive::Join | Primitive::Couple if xs.rank() <= 1 && ys.rank() <= 1 => {
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
        // Chars
        (Some((Primitive::Eq, _)), Value::Char(xs), Value::Char(ys)) => {
            env.push(fast_table_list(xs, ys, is_eq::generic, env)?)
        }
        (Some((Primitive::Ne, _)), Value::Char(xs), Value::Char(ys)) => {
            env.push(fast_table_list(xs, ys, is_ne::generic, env)?)
        }
        // Boxes
        (Some((Primitive::Eq, _)), Value::Box(xs), Value::Box(ys)) => {
            env.push(fast_table_list(xs, ys, is_eq::generic, env)?)
        }
        (Some((Primitive::Ne, _)), Value::Box(xs), Value::Box(ys)) => {
            env.push(fast_table_list(xs, ys, is_ne::generic, env)?)
        }
        (Some((Primitive::Join | Primitive::Couple, flipped)), Value::Box(xs), ys) => env.push(
            fast_table_list_join_or_couple(xs, ys.coerce_to_boxes(), flipped, env)?,
        ),
        (Some((Primitive::Join | Primitive::Couple, flipped)), xs, Value::Box(ys)) => env.push(
            fast_table_list_join_or_couple(xs.coerce_to_boxes(), ys, flipped, env)?,
        ),
        (
            Some((Primitive::Join | Primitive::Couple, flipped)),
            Value::Char(xs),
            Value::Char(ys),
        ) => env.push(fast_table_list_join_or_couple(xs, ys, flipped, env)?),
        (_, xs, ys) => match f.node.as_flipped_impl_primitive() {
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
    ($fname:ident, $ty:ty, $f:ident $(,#[$attr:meta])?) => {
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
                #[cfg(feature = "opt")]
                Primitive::Lt if flipped => {
                    env.push(fast_table_list(xs, ys, flip(other_is_lt::$f), env)?)
                }
                $(#[$attr])*
                Primitive::Lt if !flipped=> env.push(fast_table_list(xs, ys, other_is_lt::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Gt if flipped => {
                    env.push(fast_table_list(xs, ys, flip(other_is_gt::$f), env)?)
                }
                $(#[$attr])*
                Primitive::Gt if !flipped => env.push(fast_table_list(xs, ys, other_is_gt::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Le if flipped => {
                    env.push(fast_table_list(xs, ys, flip(other_is_le::$f), env)?)
                }
                $(#[$attr])*
                Primitive::Le if !flipped => env.push(fast_table_list(xs, ys, other_is_le::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Ge if flipped => {
                    env.push(fast_table_list(xs, ys, flip(other_is_ge::$f), env)?)
                }
                $(#[$attr])*
                Primitive::Ge => env.push(fast_table_list(xs, ys, other_is_ge::$f, env)?),
                Primitive::Add => env.push(fast_table_list(xs, ys, add::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Sub if flipped => env.push(fast_table_list(xs, ys, flip(sub::$f), env)?),
                Primitive::Sub if !flipped => env.push(fast_table_list(xs, ys, sub::$f, env)?),
                Primitive::Mul => env.push(fast_table_list(xs, ys, mul::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Div if flipped => env.push(fast_table_list(xs, ys, flip(div::$f), env)?),
                Primitive::Div if !flipped => env.push(fast_table_list(xs, ys, div::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Modulo if flipped => {
                    env.push(fast_table_list(xs, ys, flip(modulo::$f), env)?)
                }
                $(#[$attr])*
                Primitive::Modulo if !flipped => env.push(fast_table_list(xs, ys, modulo::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Atan if flipped => {
                    env.push(fast_table_list(xs, ys, flip(atan2::$f), env)?)
                }
                $(#[$attr])*
                Primitive::Atan => env.push(fast_table_list(xs, ys, atan2::$f, env)?),
                #[cfg(feature = "opt")]
                Primitive::Complex if flipped => {
                    env.push(fast_table_list(xs, ys, flip(complex::$f), env)?)
                }
                $(#[$attr])*
                Primitive::Complex if !flipped => env.push(fast_table_list(xs, ys, complex::$f, env)?),
                Primitive::Min => env.push(fast_table_list(xs, ys, min::$f, env)?),
                Primitive::Max => env.push(fast_table_list(xs, ys, max::$f, env)?),
                Primitive::Join | Primitive::Couple if xs.rank() <= 1 && ys.rank() <= 1  => {
                    env.push(fast_table_list_join_or_couple(xs, ys, flipped, env)?)
                }
                _ => return Ok(Err((xs, ys))),
            }
            Ok(Ok(()))
        }
    };
}

table_math!(table_nums, f64, num_num);
table_math!(table_coms, crate::Complex, com_x, #[cfg(feature = "opt")]);

fn fast_table_list<T: ArrayValue, U: ArrayValue + Default>(
    a: Array<T>,
    b: Array<T>,
    f: impl Fn(T, T) -> U,
    env: &Uiua,
) -> UiuaResult<Array<U>> {
    match (a.rank(), b.rank()) {
        (0..=1, 0..=1) => fast_table_list_inner(a, b, f, env),
        (0..=1, _) => fast_table_left(a, b, f, env),
        (_, 0..=1) => fast_table_right(a, b, f, env),
        _ => fast_table_same(a, b, f, env),
    }
}

fn fast_table_list_inner<T: ArrayValue, U: ArrayValue + Default>(
    a: Array<T>,
    b: Array<T>,
    f: impl Fn(T, T) -> U,
    env: &Uiua,
) -> UiuaResult<Array<U>> {
    let elem_count = validate_size::<U>([a.data.len(), b.data.len()], env)?;
    let mut new_data = eco_vec![U::default(); elem_count];
    let data_slice = new_data.make_mut();
    let mut i = 0;
    for a in a.data {
        for b in &b.data {
            data_slice[i] = f(a.clone(), b.clone());
            i += 1;
        }
    }
    let mut new_shape = a.shape;
    new_shape.extend_from_slice(&b.shape);
    Ok(Array::new(new_shape, new_data))
}

fn fast_table_left<T: ArrayValue, U: ArrayValue + Default>(
    a: Array<T>,
    b: Array<T>,
    f: impl Fn(T, T) -> U,
    env: &Uiua,
) -> UiuaResult<Array<U>> {
    let elem_count = validate_size::<U>([a.data.len(), b.data.len()], env)?;
    let mut new_data = eco_vec![U::default(); elem_count];
    let data_slice = new_data.make_mut();
    let mut i = 0;
    let b_row_len = b.row_len();
    if b_row_len > 0 {
        for a in a.data {
            for b in b.data.chunks_exact(b_row_len).flatten() {
                data_slice[i] = f(a.clone(), b.clone());
                i += 1;
            }
        }
    }
    let mut new_shape = a.shape;
    new_shape.extend_from_slice(&b.shape);
    Ok(Array::new(new_shape, new_data))
}

fn fast_table_right<T: ArrayValue, U: ArrayValue + Default>(
    a: Array<T>,
    b: Array<T>,
    f: impl Fn(T, T) -> U,
    env: &Uiua,
) -> UiuaResult<Array<U>> {
    let elem_count = validate_size::<U>([a.data.len(), b.data.len()], env)?;
    let mut new_data = eco_vec![U::default(); elem_count];
    let data_slice = new_data.make_mut();
    let mut i = 0;
    let a_row_len = a.row_len();
    if a_row_len > 0 {
        for a in a.data.chunks_exact(a_row_len) {
            for b in &b.data {
                for a in a {
                    data_slice[i] = f(a.clone(), b.clone());
                    i += 1;
                }
            }
        }
    }
    let new_shape = Shape::from_iter(
        (a.shape.iter().take(1))
            .chain(b.shape.iter().take(1))
            .chain(a.shape.iter().skip(1))
            .copied(),
    );
    Ok(Array::new(new_shape, new_data))
}

fn fast_table_same<T: ArrayValue, U: ArrayValue + Default>(
    a: Array<T>,
    b: Array<T>,
    f: impl Fn(T, T) -> U,
    env: &Uiua,
) -> UiuaResult<Array<U>> {
    assert_eq!(a.shape[1..], b.shape[1..]);
    let elem_count = validate_size::<U>([a.row_count(), b.data.len()], env)?;
    let mut new_data = eco_vec![U::default(); elem_count];
    let data_slice = new_data.make_mut();
    let mut i = 0;
    let (a_row_len, b_row_len) = (a.row_len(), b.row_len());
    if a_row_len > 0 && b_row_len > 0 {
        for a in a.data.chunks_exact(a_row_len) {
            for b in b.data.chunks_exact(b_row_len) {
                for (a, b) in a.iter().zip(b) {
                    data_slice[i] = f(a.clone(), b.clone());
                    i += 1;
                }
            }
        }
    }
    let new_shape = Shape::from_iter(
        (a.shape.iter().take(1))
            .chain(b.shape.iter().take(1))
            .chain(a.shape.iter().skip(1))
            .copied(),
    );
    Ok(Array::new(new_shape, new_data))
}

fn fast_table_list_join_or_couple<T: ArrayValue + Default>(
    a: Array<T>,
    b: Array<T>,
    flipped: bool,
    env: &Uiua,
) -> UiuaResult<Array<T>> {
    let elem_count = validate_size::<T>([a.data.len(), b.data.len(), 2], env)?;
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

pub fn reduce_table(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f, g] = get_ops(ops, env)?;
    if let Some((Primitive::Join, _)) = f.node.as_flipped_primitive() {
        table_impl(g, env)?;
        return reduce_impl(f, 0, env);
    }
    let xs = env.pop(1)?;
    let ys = env.pop(2)?;
    if xs.rank() == 1 && ys.rank() == 1 {
        let prims = (f.node.as_flipped_primitive()).zip(g.node.as_flipped_primitive());
        match (prims, xs, ys) {
            (Some(((fp, f_flip), (gp, g_flip))), Value::Num(xs), Value::Num(ys)) => {
                if let Err((xs, ys)) = reduce_table_nums(fp, gp, f_flip, g_flip, xs, ys, env)? {
                    return generic_reduce_table(f, g, Value::Num(xs), Value::Num(ys), env);
                }
            }
            (Some(((fp, f_flip), (gp, g_flip))), Value::Complex(xs), Value::Complex(ys)) => {
                if let Err((xs, ys)) = reduce_coms(fp, gp, f_flip, g_flip, xs, ys, env)? {
                    return generic_reduce_table(f, g, Value::Complex(xs), Value::Complex(ys), env);
                }
            }
            (Some(((fp, f_flip), (gp, g_flip))), Value::Byte(xs), Value::Num(ys)) => {
                let xs = xs.convert();
                if let Err((xs, ys)) = reduce_table_nums(fp, gp, f_flip, g_flip, xs, ys, env)? {
                    return generic_reduce_table(f, g, Value::Num(xs), Value::Num(ys), env);
                }
            }
            (Some(((fp, f_flip), (gp, g_flip))), Value::Num(xs), Value::Byte(ys)) => {
                let ys = ys.convert();
                if let Err((xs, ys)) = reduce_table_nums(fp, gp, f_flip, g_flip, xs, ys, env)? {
                    return generic_reduce_table(f, g, Value::Num(xs), Value::Num(ys), env);
                }
            }
            (Some(((fp, false), (gp, false))), Value::Byte(xs), Value::Byte(ys)) => {
                if let Err((xs, ys)) = reduce_table_bytes(fp, gp, xs, ys, env) {
                    return generic_reduce_table(f, g, Value::Byte(xs), Value::Byte(ys), env);
                }
            }
            (_, xs, ys) => generic_reduce_table(f, g, xs, ys, env)?,
        }
    } else {
        generic_reduce_table(f, g, xs, ys, env)?;
    }
    Ok(())
}

fn reduce_table_bytes(
    fp: Primitive,
    gp: Primitive,
    xs: Array<u8>,
    ys: Array<u8>,
    env: &mut Uiua,
) -> Result<(), (Array<u8>, Array<u8>)> {
    macro_rules! all_gs {
        ($xs:expr, $ys:expr, $ff:expr, $ff_complex:expr, $iden:expr, $ciden:expr, $fill:expr, $arith:ident, $cmp:ident) => {{
            let fill = $fill.map(Into::into);
            match gp {
                Primitive::Add => env.push(frtl($xs, $ys, $ff, add::$arith, $iden, fill)),
                Primitive::Sub => env.push(frtl($xs, $ys, $ff, sub::$arith, $iden, fill)),
                Primitive::Mul => env.push(frtl($xs, $ys, $ff, mul::$arith, $iden, fill)),
                Primitive::Div => env.push(frtl($xs, $ys, $ff, div::$arith, $iden, fill)),
                Primitive::Modulo => env.push(frtl($xs, $ys, $ff, modulo::$arith, $iden, fill)),
                #[cfg(feature = "opt")]
                Primitive::Atan => env.push(frtl($xs, $ys, $ff, atan2::$arith, $iden, fill)),
                Primitive::Eq => env.push(frtl($xs, $ys, $ff, to(is_eq::$cmp), $iden, fill)),
                Primitive::Ne => env.push(frtl($xs, $ys, $ff, to(is_ne::$cmp), $iden, fill)),
                Primitive::Lt => env.push(frtl($xs, $ys, $ff, to(other_is_lt::$cmp), $iden, fill)),
                Primitive::Gt => env.push(frtl($xs, $ys, $ff, to(other_is_gt::$cmp), $iden, fill)),
                Primitive::Le => env.push(frtl($xs, $ys, $ff, to(other_is_le::$cmp), $iden, fill)),
                Primitive::Ge => env.push(frtl($xs, $ys, $ff, to(other_is_ge::$cmp), $iden, fill)),
                Primitive::Min => env.push(frtl($xs, $ys, $ff, min::$arith, $iden, fill)),
                Primitive::Max => env.push(frtl($xs, $ys, $ff, max::$arith, $iden, fill)),
                Primitive::Complex => env.push(frtl(
                    $xs,
                    $ys,
                    $ff_complex,
                    complex::$arith,
                    Complex::new($iden, $ciden),
                    env.scalar_fill::<Complex>().ok().map(|fv| fv.value),
                )),
                Primitive::Couple | Primitive::Join => env.push(frtljc($xs, $ys, $ff, $iden, fill)),
                _ => return Err((xs, ys)),
            }
        }};
    }
    let fill = env.scalar_fill::<f64>().ok().map(|fv| fv.value);
    match fp {
        Primitive::Add => {
            all_gs!(
                xs,
                ys,
                to_left(add::num_num),
                add::com_x,
                0.0,
                0.0,
                fill,
                byte_byte,
                generic
            )
        }
        Primitive::Mul => {
            all_gs!(
                xs,
                ys,
                to_left(mul::num_num),
                mul::com_x,
                1.0,
                0.0,
                fill,
                byte_byte,
                generic
            )
        }
        Primitive::Min => {
            let byte_fill = env.scalar_fill::<u8>().ok().map(|fv| fv.value);
            if xs.row_count() == 0 || fill.is_some() && byte_fill.is_none() {
                all_gs!(
                    xs.convert(),
                    ys.convert(),
                    min::num_num,
                    min::com_x,
                    f64::INFINITY,
                    f64::INFINITY,
                    fill,
                    num_num,
                    num_num
                )
            } else {
                all_gs!(
                    xs,
                    ys,
                    to_left(min::num_num),
                    min::com_x,
                    f64::INFINITY,
                    f64::INFINITY,
                    byte_fill,
                    byte_byte,
                    generic
                )
            }
        }
        Primitive::Max => {
            let byte_fill = env.scalar_fill::<u8>().ok().map(|fv| fv.value);
            if xs.row_count() == 0 || fill.is_some() && byte_fill.is_none() {
                all_gs!(
                    xs.convert(),
                    ys.convert(),
                    max::num_num,
                    max::com_x,
                    f64::NEG_INFINITY,
                    f64::NEG_INFINITY,
                    fill,
                    num_num,
                    num_num
                )
            } else {
                all_gs!(
                    xs,
                    ys,
                    to_left(max::num_num),
                    max::com_x,
                    f64::NEG_INFINITY,
                    f64::NEG_INFINITY,
                    byte_fill,
                    byte_byte,
                    generic
                )
            }
        }
        Primitive::Ne if xs.row_count() > 0 => {
            all_gs!(
                xs.convert(),
                ys.convert(),
                to(is_ne::num_num),
                to(is_ne::com_x),
                0.0,
                0.0,
                fill,
                num_num,
                num_num
            )
        }
        _ => return Err((xs, ys)),
    }
    Ok(())
}

fn generic_reduce_table(
    f: SigNode,
    g: SigNode,
    xs: Value,
    ys: Value,
    env: &mut Uiua,
) -> UiuaResult {
    if xs.rank() == 0 || ys.rank() == 0 || env.value_fill().is_some() {
        env.push(ys);
        env.push(xs);
        table_impl(g, env)?;
        return reduce_impl(f, 0, env);
    }

    let mut xs = xs.into_rows();
    let mut acc = xs
        .next()
        .ok_or_else(|| env.error("Cannot reduce empty array"))?;
    let mut g_rows = Value::builder(ys.row_count());
    for y in ys.rows() {
        env.push(y);
        env.push(acc.clone());
        env.exec(g.clone())?;
        g_rows.add_row(env.pop("reduced function result")?, env)?;
    }
    acc = g_rows.finish();
    for x in xs {
        g_rows = Value::builder(ys.row_count());
        for y in ys.rows() {
            env.push(y);
            env.push(x.clone());
            env.exec(g.clone())?;
            g_rows.add_row(env.pop("reduced function result")?, env)?;
        }
        env.push(g_rows.finish());
        env.push(acc);
        env.exec(f.clone())?;
        acc = env.pop("reduced function result")?;
    }
    env.push(acc);
    Ok(())
}

fn to<T, U>(f: impl Fn(T, T) -> U) -> impl Fn(T, T) -> T
where
    U: Into<T>,
{
    move |a, b| f(a, b).into()
}

fn to_left<T, U>(f: impl Fn(T, T) -> T) -> impl Fn(T, U) -> T
where
    U: Into<T>,
{
    move |a, b| f(a, b.into())
}

macro_rules! reduce_table_math {
    ($fname:ident, $ty:ty, $f:ident) => {
        #[allow(clippy::result_large_err)]
        fn $fname(
            f_prim: Primitive,
            g_prim: Primitive,
            f_flipped: bool,
            g_flipped: bool,
            xs: Array<$ty>,
            ys: Array<$ty>,
            env: &mut Uiua,
        ) -> UiuaResult<Result<(), (Array<$ty>, Array<$ty>)>> {
            if f_flipped || g_flipped {
                return Ok(Err((xs, ys)));
            }
            let fill = env.scalar_fill::<$ty>().ok().map(|fv| fv.value);
            macro_rules! all_gs {
                ($ff:expr, $ff_complex:expr, $iden:expr, $ciden:expr) => {
                    match g_prim {
                        Primitive::Add => env.push(frtl(xs, ys, $ff, add::$f, $iden.into(), fill)),
                        Primitive::Sub => env.push(frtl(xs, ys, $ff, sub::$f, $iden.into(), fill)),
                        Primitive::Mul => env.push(frtl(xs, ys, $ff, mul::$f, $iden.into(), fill)),
                        Primitive::Div => env.push(frtl(xs, ys, $ff, div::$f, $iden.into(), fill)),
                        Primitive::Modulo => {
                            env.push(frtl(xs, ys, $ff, modulo::$f, $iden.into(), fill))
                        }
                        #[cfg(feature = "opt")]
                        Primitive::Atan => {
                            env.push(frtl(xs, ys, $ff, atan2::$f, $iden.into(), fill))
                        }
                        Primitive::Eq => {
                            env.push(frtl(xs, ys, $ff, to(is_eq::$f), $iden.into(), fill))
                        }
                        Primitive::Ne => {
                            env.push(frtl(xs, ys, $ff, to(is_ne::$f), $iden.into(), fill))
                        }
                        Primitive::Lt => {
                            env.push(frtl(xs, ys, $ff, to(other_is_lt::$f), $iden.into(), fill))
                        }
                        Primitive::Gt => {
                            env.push(frtl(xs, ys, $ff, to(other_is_gt::$f), $iden.into(), fill))
                        }
                        Primitive::Le => {
                            env.push(frtl(xs, ys, $ff, to(other_is_le::$f), $iden.into(), fill))
                        }
                        Primitive::Ge => {
                            env.push(frtl(xs, ys, $ff, to(other_is_ge::$f), $iden.into(), fill))
                        }
                        Primitive::Min => env.push(frtl(xs, ys, $ff, min::$f, $iden.into(), fill)),
                        Primitive::Max => env.push(frtl(xs, ys, $ff, max::$f, $iden.into(), fill)),
                        Primitive::Complex => env.push(frtl(
                            xs,
                            ys,
                            $ff_complex,
                            complex::$f,
                            Complex::new($iden, $ciden),
                            env.scalar_fill::<Complex>().ok().map(|fv| fv.value),
                        )),
                        Primitive::Couple | Primitive::Join => {
                            env.push(frtljc(xs, ys, $ff, $iden.into(), fill))
                        }
                        _ => return Ok(Err((xs, ys))),
                    }
                };
            }
            match f_prim {
                Primitive::Add => all_gs!(add::$f, add::com_x, 0.0, 0.0),
                Primitive::Mul => all_gs!(mul::$f, mul::com_x, 1.0, 0.0),
                Primitive::Min => all_gs!(min::$f, min::com_x, f64::INFINITY, f64::INFINITY),
                Primitive::Max => {
                    all_gs!(max::$f, max::com_x, f64::NEG_INFINITY, f64::NEG_INFINITY)
                }
                Primitive::Ne if xs.row_count() > 0 => {
                    all_gs!(to(is_ne::$f), to(is_ne::com_x), 0.0, 0.0)
                }
                _ => return Ok(Err((xs, ys))),
            }
            Ok(Ok(()))
        }
    };
}

reduce_table_math!(reduce_table_nums, f64, num_num);
reduce_table_math!(reduce_coms, Complex, com_x);

/// Fast reduce table list
fn frtl<T, G, F>(
    a: Array<T>,
    b: Array<T>,
    f: impl Fn(F, G) -> F,
    g: impl Fn(T, T) -> G,
    identity: F,
    default: Option<F>,
) -> Array<F>
where
    T: ArrayValue + Copy,
    G: ArrayValue,
    F: ArrayValue + Copy,
{
    let mut acc = eco_vec![default.unwrap_or(identity); b.shape.elements()];
    let acc_slice = acc.make_mut();
    for a in a.data {
        for (&b, c) in b.data.iter().zip(&mut *acc_slice) {
            *c = f(*c, g(a, b));
        }
    }
    Array::new(b.shape, acc)
}

/// Fast reduce table list join or couple
fn frtljc<T, F>(
    a: Array<T>,
    b: Array<T>,
    f: impl Fn(F, T) -> F,
    identity: F,
    default: Option<F>,
) -> Array<F>
where
    T: ArrayValue + Copy,
    F: ArrayValue + Copy,
{
    let mut acc = eco_vec![default.unwrap_or(identity); b.shape.elements() * 2];
    let acc_slice = acc.make_mut();
    for a in a.data {
        let mut i = 0;
        for b in b.data.iter().cloned() {
            acc_slice[i] = f(acc_slice[i], a);
            i += 1;
            acc_slice[i] = f(acc_slice[i], b);
            i += 1;
        }
    }
    let mut new_shape = b.shape.clone();
    new_shape.push(2);
    Array::new(new_shape, acc)
}
