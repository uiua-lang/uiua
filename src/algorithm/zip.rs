//! Algorithms for zipping modifiers

use std::{iter::repeat, slice};

use ecow::{eco_vec, EcoVec};

use crate::{
    algorithm::pervade::bin_pervade_generic, function::Function, random, value::Value, Array,
    ArrayValue, Boxed, Complex, ImplPrimitive, Instr, PersistentMeta, Primitive, Shape, Uiua,
    UiuaResult,
};

use super::{fill_value_shapes, multi_output, FillContext, MultiOutput};

type ValueUnFn = Box<dyn Fn(Value, usize, &mut Uiua) -> UiuaResult<Value>>;
type ValueBinFn = Box<dyn Fn(Value, Value, usize, usize, &mut Uiua) -> UiuaResult<Value>>;

fn spanned_mon_fn(
    span: usize,
    f: impl Fn(Value, usize, &Uiua) -> UiuaResult<Value> + 'static,
) -> ValueUnFn {
    Box::new(move |v, d, env| env.with_span(span, |env| f(v, d, env)))
}

fn prim_mon_fast_fn(prim: Primitive, span: usize) -> Option<ValueUnFn> {
    use Primitive::*;
    Some(match prim {
        Not => spanned_mon_fn(span, |v, _, env| Value::not(v, env)),
        Sign => spanned_mon_fn(span, |v, _, env| Value::sign(v, env)),
        Neg => spanned_mon_fn(span, |v, _, env| Value::neg(v, env)),
        Abs => spanned_mon_fn(span, |v, _, env| Value::abs(v, env)),
        Sqrt => spanned_mon_fn(span, |v, _, env| Value::sqrt(v, env)),
        Floor => spanned_mon_fn(span, |v, _, env| Value::floor(v, env)),
        Ceil => spanned_mon_fn(span, |v, _, env| Value::ceil(v, env)),
        Round => spanned_mon_fn(span, |v, _, env| Value::round(v, env)),
        Deshape => spanned_mon_fn(span, |mut v, d, _| {
            Value::deshape_depth(&mut v, d);
            Ok(v)
        }),
        Transpose => spanned_mon_fn(span, |mut v, d, _| {
            Value::transpose_depth(&mut v, d, 1);
            Ok(v)
        }),
        Reverse => spanned_mon_fn(span, |mut v, d, _| {
            Value::reverse_depth(&mut v, d);
            Ok(v)
        }),
        Fix => spanned_mon_fn(span, |mut v, d, _| {
            v.fix_depth(d);
            Ok(v)
        }),
        _ => return None,
    })
}

fn impl_prim_mon_fast_fn(prim: ImplPrimitive, span: usize) -> Option<ValueUnFn> {
    use ImplPrimitive::*;
    Some(match prim {
        TransposeN(n) => spanned_mon_fn(span, move |mut v, d, _| {
            Value::transpose_depth(&mut v, d, n);
            Ok(v)
        }),
        ReplaceRand => spanned_mon_fn(span, |v, d, _| {
            let shape = &v.shape()[..d.min(v.rank())];
            let elem_count: usize = shape.iter().product();
            let mut data = eco_vec![0.0; elem_count];
            for n in data.make_mut() {
                *n = random();
            }
            Ok(Array::new(shape, data).into())
        }),
        _ => return None,
    })
}

fn f_mon_fast_fn(f: &Function, env: &Uiua) -> Option<(ValueUnFn, usize)> {
    use Primitive::*;
    match f.instrs(env) {
        &[Instr::Prim(prim, span)] => {
            let f = prim_mon_fast_fn(prim, span)?;
            return Some((f, 0));
        }
        &[Instr::ImplPrim(prim, span)] => {
            let f = impl_prim_mon_fast_fn(prim, span)?;
            return Some((f, 0));
        }
        [Instr::PushFunc(f), Instr::Prim(Rows, _)] => {
            let (f, d) = f_mon_fast_fn(f, env)?;
            return Some((f, d + 1));
        }
        _ => (),
    }
    None
}

fn spanned_dy_fn(
    span: usize,
    f: impl Fn(Value, Value, usize, usize, &Uiua) -> UiuaResult<Value> + 'static,
) -> ValueBinFn {
    Box::new(move |a, b, ad, bd, env| env.with_span(span, |env| f(a, b, ad, bd, env)))
}

fn prim_dy_fast_fn(prim: Primitive, span: usize) -> Option<ValueBinFn> {
    use std::boxed::Box;
    use Primitive::*;
    Some(match prim {
        Add => spanned_dy_fn(span, Value::add),
        Sub => spanned_dy_fn(span, Value::sub),
        Mul => spanned_dy_fn(span, Value::mul),
        Div => spanned_dy_fn(span, Value::div),
        Pow => spanned_dy_fn(span, Value::pow),
        Mod => spanned_dy_fn(span, Value::modulus),
        Log => spanned_dy_fn(span, Value::log),
        Eq => spanned_dy_fn(span, Value::is_eq),
        Ne => spanned_dy_fn(span, Value::is_ne),
        Lt => spanned_dy_fn(span, Value::is_lt),
        Gt => spanned_dy_fn(span, Value::is_gt),
        Le => spanned_dy_fn(span, Value::is_le),
        Ge => spanned_dy_fn(span, Value::is_ge),
        Complex => spanned_dy_fn(span, Value::complex),
        Max => spanned_dy_fn(span, Value::max),
        Min => spanned_dy_fn(span, Value::min),
        Atan => spanned_dy_fn(span, Value::atan2),
        Rotate => Box::new(move |a, b, ad, bd, env| {
            env.with_span(span, |env| a.rotate_depth(b, ad, bd, env))
        }),
        _ => return None,
    })
}

pub(crate) fn f_dy_fast_fn(instrs: &[Instr], env: &Uiua) -> Option<(ValueBinFn, usize, usize)> {
    use std::boxed::Box;
    use Primitive::*;

    fn nest_dy_fast<F>(
        (f, ad1, bd1): (F, usize, usize),
        ad2: usize,
        bd2: usize,
    ) -> Option<(F, usize, usize)> {
        if (ad1 as isize - ad2 as isize).signum() != (bd1 as isize - bd2 as isize).signum() {
            return None;
        }
        Some((f, ad1 + ad2, bd1 + bd2))
    }

    match instrs {
        &[Instr::Prim(prim, span)] => {
            let f = prim_dy_fast_fn(prim, span)?;
            return Some((f, 0, 0));
        }
        [Instr::PushFunc(f), Instr::Prim(Rows, _)] => {
            return nest_dy_fast(f_dy_fast_fn(f.instrs(env), env)?, 1, 1)
        }
        [Instr::Prim(Flip, _), rest @ ..] => {
            let (f, a, b) = f_dy_fast_fn(rest, env)?;
            let f = Box::new(move |a, b, ad, bd, env: &mut Uiua| f(b, a, bd, ad, env));
            return Some((f, a, b));
        }
        _ => (),
    }
    None
}

pub fn each(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    match sig.args {
        0 => Ok(()),
        1 => each1(f, env.pop(1)?, env),
        2 => each2(f, env.pop(1)?, env.pop(2)?, env),
        n => {
            let mut args = Vec::with_capacity(n);
            for i in 0..n {
                args.push(env.pop(i + 1)?);
            }
            eachn(f, args, env)
        }
    }
}

fn each1(f: Function, mut xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, ..)) = f_mon_fast_fn(&f, env) {
        let maybe_through_boxes = matches!(&xs, Value::Box(..));
        if !maybe_through_boxes {
            let rank = xs.rank();
            let val = f(xs, rank, env)?;
            env.push(val);
            return Ok(());
        }
    }
    let outputs = f.signature().outputs;
    let mut new_values = multi_output(outputs, Vec::with_capacity(xs.element_count()));
    let new_shape = xs.shape().clone();
    let is_empty = outputs > 0 && xs.row_count() == 0;
    let per_meta = xs.take_per_meta();
    env.without_fill(|env| -> UiuaResult {
        if is_empty {
            env.push(xs.proxy_scalar(env));
            _ = env.call_maintain_sig(f);
            for i in 0..outputs {
                new_values[i].push(env.pop("each's function result")?);
            }
        } else {
            for val in xs.into_elements() {
                env.push(val);
                env.call(f.clone())?;
                for i in 0..outputs {
                    new_values[i].push(env.pop("each's function result")?);
                }
            }
        }
        Ok(())
    })?;
    for new_values in new_values.into_iter().rev() {
        let mut new_shape = new_shape.clone();
        let mut eached = Value::from_row_values(new_values, env)?;
        if is_empty {
            eached.pop_row();
        }
        new_shape.extend_from_slice(&eached.shape()[1..]);
        *eached.shape_mut() = new_shape;
        eached.set_per_meta(per_meta.clone());
        env.push(eached);
    }
    Ok(())
}

fn each2(f: Function, mut xs: Value, mut ys: Value, env: &mut Uiua) -> UiuaResult {
    fill_value_shapes(&mut xs, &mut ys, true, env)?;
    if let Some((f, ..)) = f_dy_fast_fn(f.instrs(env), env) {
        let xrank = xs.rank();
        let yrank = ys.rank();
        let val = f(xs, ys, xrank, yrank, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let mut xs_shape = xs.shape().to_vec();
        let mut ys_shape = ys.shape().to_vec();
        let is_empty = outputs > 0 && (xs.row_count() == 0 || ys.row_count() == 0);
        let per_meta = xs.take_per_meta().xor(ys.take_per_meta());
        let (new_shape, new_values) = env.without_fill(|env| {
            if is_empty {
                if let Some(r) = xs_shape.first_mut() {
                    *r = 1;
                }
                if let Some(r) = ys_shape.first_mut() {
                    *r = 1;
                }
                bin_pervade_generic(
                    &xs_shape,
                    slice::from_ref(&xs.proxy_scalar(env)),
                    &ys_shape,
                    slice::from_ref(&ys.proxy_scalar(env)),
                    env,
                    |x, y, env| {
                        env.push(y);
                        env.push(x);
                        if env.call_maintain_sig(f.clone()).is_ok() {
                            (0..outputs)
                                .map(|_| env.pop("each's function result"))
                                .collect::<Result<MultiOutput<_>, _>>()
                        } else {
                            Ok(multi_output(outputs, Value::default()))
                        }
                    },
                )
            } else {
                let xs_values: Vec<_> = xs.into_elements().collect();
                let ys_values: Vec<_> = ys.into_elements().collect();
                bin_pervade_generic(
                    &xs_shape,
                    xs_values,
                    &ys_shape,
                    ys_values,
                    env,
                    |x, y, env| {
                        env.push(y);
                        env.push(x);
                        env.call(f.clone())?;
                        (0..outputs)
                            .map(|_| env.pop("each's function result"))
                            .collect::<Result<MultiOutput<_>, _>>()
                    },
                )
            }
        })?;
        let mut transposed = multi_output(outputs, Vec::with_capacity(new_values.len()));
        for values in new_values {
            for (i, value) in values.into_iter().enumerate() {
                transposed[i].push(value);
            }
        }
        for new_values in transposed {
            let mut new_shape = new_shape.clone();
            let mut eached = Value::from_row_values(new_values, env)?;
            if is_empty {
                eached.pop_row();
                if let Some(r) = new_shape.first_mut() {
                    *r -= 1;
                }
            }
            new_shape.extend_from_slice(&eached.shape()[1..]);
            *eached.shape_mut() = new_shape;
            eached.set_per_meta(per_meta.clone());
            env.push(eached);
        }
    }
    Ok(())
}

fn eachn(f: Function, mut args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
    for a in 0..args.len() - 1 {
        let (a, b) = args.split_at_mut(a + 1);
        let a = a.last_mut().unwrap();
        for b in b {
            fill_value_shapes(a, b, true, env)?;
        }
    }
    let outputs = f.signature().outputs;
    let is_empty = outputs > 0 && args.iter().any(|v| v.row_count() == 0);
    let elem_count = args.iter().map(Value::element_count).max().unwrap() + is_empty as usize;
    let mut new_values = multi_output(outputs, Vec::with_capacity(elem_count));
    let new_shape = args
        .iter()
        .map(Value::shape)
        .max_by_key(|s| s.len())
        .unwrap()
        .clone();
    let per_meta = PersistentMeta::xor_all(args.iter_mut().map(|v| v.take_per_meta()));
    env.without_fill(|env| -> UiuaResult {
        if is_empty {
            for arg in args.into_iter().rev() {
                env.push(arg.proxy_scalar(env));
            }
            _ = env.call_maintain_sig(f);
            for i in 0..outputs {
                new_values[i].push(env.pop("each's function result")?);
            }
        } else {
            let mut arg_elems: Vec<_> = args
                .into_iter()
                .map(|val| {
                    let repetitions = elem_count / val.element_count();
                    val.into_elements()
                        .flat_map(move |elem| repeat(elem).take(repetitions))
                })
                .collect();
            for _ in 0..elem_count {
                for arg in arg_elems.iter_mut().rev() {
                    env.push(arg.next().unwrap());
                }
                env.call(f.clone())?;
                for i in 0..outputs {
                    new_values[i].push(env.pop("each's function result")?);
                }
            }
        }
        Ok(())
    })?;
    for new_values in new_values.into_iter().rev() {
        let mut new_shape = new_shape.clone();
        let mut eached = Value::from_row_values(new_values, env)?;
        if is_empty {
            eached.pop_row();
        }
        new_shape.extend_from_slice(&eached.shape()[1..]);
        *eached.shape_mut() = new_shape;
        eached.set_per_meta(per_meta.clone());
        env.push(eached);
    }
    Ok(())
}

pub fn rows(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    match sig.args {
        0 => Ok(()),
        1 => rows1(f, env.pop(1)?, env),
        2 => rows2(f, env.pop(1)?, env.pop(2)?, env),
        n => {
            let mut args = Vec::with_capacity(n);
            for i in 0..n {
                args.push(env.pop(i + 1)?);
            }
            rowsn(f, args, env)
        }
    }
}

pub fn rows1(f: Function, mut xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, d)) = f_mon_fast_fn(&f, env) {
        let maybe_through_boxes = matches!(&xs, Value::Box(arr) if arr.rank() <= d + 1);
        if !maybe_through_boxes {
            let val = f(xs, d + 1, env)?;
            env.push(val);
            return Ok(());
        }
    }
    let outputs = f.signature().outputs;
    let is_empty = outputs > 0 && xs.row_count() == 0;
    let mut new_rows = multi_output(
        outputs,
        Vec::with_capacity(xs.row_count() + is_empty as usize),
    );
    let per_meta = xs.take_per_meta();
    env.without_fill(|env| -> UiuaResult {
        if is_empty {
            env.push(xs.proxy_row(env));
            _ = env.call_maintain_sig(f);
            for i in 0..outputs {
                new_rows[i].push(env.pop("rows' function result")?);
            }
        } else {
            for row in xs.into_rows() {
                env.push(row);
                env.call(f.clone())?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("rows' function result")?);
                }
            }
        }
        Ok(())
    })?;
    for new_rows in new_rows.into_iter().rev() {
        let mut val = Value::from_row_values(new_rows, env)?;
        if is_empty {
            val.pop_row();
        }
        val.set_per_meta(per_meta.clone());
        env.push(val);
    }

    Ok(())
}

fn rows2(f: Function, mut xs: Value, mut ys: Value, env: &mut Uiua) -> UiuaResult {
    let outputs = f.signature().outputs;
    let both_scalar = xs.rank() == 0 && ys.rank() == 0;
    match (xs.row_count(), ys.row_count()) {
        (_, 1) if !ys.length_is_fillable(env) => {
            ys.shape_mut().make_row();
            let is_empty = outputs > 0 && xs.row_count() == 0;
            let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
            let per_meta = xs.take_per_meta();
            env.without_fill(|env| -> UiuaResult {
                if is_empty {
                    env.push(ys.proxy_row(env));
                    env.push(xs.proxy_row(env));
                    _ = env.call_maintain_sig(f);
                    for i in 0..outputs {
                        new_rows[i].push(env.pop("rows's function result")?);
                    }
                } else {
                    for x in xs.into_rows() {
                        env.push(ys.clone());
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?);
                        }
                    }
                }
                Ok(())
            })?;
            for new_rows in new_rows.into_iter().rev() {
                let mut val = Value::from_row_values(new_rows, env)?;
                if both_scalar {
                    val.shape_mut().make_row();
                } else if is_empty {
                    val.pop_row();
                }
                val.set_per_meta(per_meta.clone());
                env.push(val);
            }
            Ok(())
        }
        (1, _) if !xs.length_is_fillable(env) => {
            xs.shape_mut().make_row();
            let is_empty = outputs > 0 && ys.row_count() == 0;
            let mut new_rows = multi_output(outputs, Vec::with_capacity(ys.row_count()));
            let per_meta = ys.take_per_meta();
            env.without_fill(|env| -> UiuaResult {
                if is_empty {
                    env.push(ys.proxy_row(env));
                    env.push(xs.proxy_row(env));
                    _ = env.call_maintain_sig(f);
                    for i in 0..outputs {
                        new_rows[i].push(env.pop("rows's function result")?);
                    }
                } else {
                    for y in ys.into_rows() {
                        env.push(y);
                        env.push(xs.clone());
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?);
                        }
                    }
                }
                Ok(())
            })?;
            for new_rows in new_rows.into_iter().rev() {
                let mut val = Value::from_row_values(new_rows, env)?;
                if both_scalar {
                    val.shape_mut().make_row();
                } else if is_empty {
                    val.pop_row();
                }
                val.set_per_meta(per_meta.clone());
                env.push(val);
            }
            Ok(())
        }
        (a, b) => {
            if a != b {
                if let Err(e) = xs
                    .fill_length_to(ys.row_count(), env)
                    .and_then(|()| ys.fill_length_to(xs.row_count(), env))
                {
                    return Err(env.error(format!(
                        "Cannot {} arrays with different number of rows {a} and {b}{e}",
                        Primitive::Rows.format(),
                    )));
                }
            }
            if let Some((f, a, b)) = f_dy_fast_fn(f.instrs(env), env) {
                let val = f(xs, ys, a + 1, b + 1, env)?;
                env.push(val);
                return Ok(());
            }
            let is_empty = outputs > 0 && (xs.row_count() == 0 || ys.row_count() == 0);
            let mut new_rows = multi_output(
                outputs,
                Vec::with_capacity(xs.row_count() + is_empty as usize),
            );
            let per_meta = xs.take_per_meta().xor(ys.take_per_meta());
            env.without_fill(|env| -> UiuaResult {
                if is_empty {
                    env.push(ys.proxy_row(env));
                    env.push(xs.proxy_row(env));
                    _ = env.call_maintain_sig(f);
                    for i in 0..outputs {
                        new_rows[i].push(env.pop("rows's function result")?);
                    }
                } else {
                    for (x, y) in xs.into_rows().zip(ys.into_rows()) {
                        env.push(y);
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?);
                        }
                    }
                }
                Ok(())
            })?;
            for new_rows in new_rows.into_iter().rev() {
                let mut val = Value::from_row_values(new_rows, env)?;
                if both_scalar {
                    val.shape_mut().make_row();
                } else if is_empty {
                    val.pop_row();
                }
                val.set_per_meta(per_meta.clone());
                env.push(val);
            }
            Ok(())
        }
    }
}

fn rowsn(f: Function, mut args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
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
                    "Cannot {} arrays with different number of rows, shapes {} and {}{e}",
                    Primitive::Rows.format(),
                    args[a].shape(),
                    args[b].shape(),
                )));
            }
        }
    }
    let mut row_count = 0;
    let mut all_scalar = true;
    let mut all_1 = true;
    let outputs = f.signature().outputs;
    let is_empty = outputs > 0 && args.iter().any(|v| v.row_count() == 0);
    let mut per_meta = Vec::new();
    let mut arg_elems: Vec<_> = args
        .into_iter()
        .map(|mut v| {
            all_scalar = all_scalar && v.rank() == 0;
            if v.row_count() == 1 {
                v.shape_mut().make_row();
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
    let mut new_values = multi_output(outputs, Vec::new());
    let per_meta = PersistentMeta::xor_all(per_meta);
    env.without_fill(|env| -> UiuaResult {
        for _ in 0..row_count + is_empty as usize {
            for arg in arg_elems.iter_mut().rev() {
                match arg {
                    Ok(rows) => env.push(rows.next().unwrap()),
                    Err(row) => env.push(row.clone()),
                }
            }
            env.call(f.clone())?;
            for i in 0..outputs {
                new_values[i].push(env.pop("rows's function result")?);
            }
        }
        Ok(())
    })?;
    for new_values in new_values.into_iter().rev() {
        let mut rowsed = Value::from_row_values(new_values, env)?;
        if all_scalar {
            rowsed.shape_mut().make_row();
        } else if is_empty {
            rowsed.pop_row();
        }
        rowsed.validate_shape();
        rowsed.set_per_meta(per_meta.clone());
        env.push(rowsed);
    }
    Ok(())
}

pub fn inventory(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    match sig.args {
        1 => inventory1(f, env.pop(1)?, env),
        2 => invertory2(f, env.pop(1)?, env.pop(2)?, env),
        n => {
            let mut args = Vec::with_capacity(n);
            for i in 0..n {
                args.push(env.pop(i + 1)?);
            }
            inventoryn(f, args, env)
        }
    }
}

fn inventory1(f: Function, xs: Value, env: &mut Uiua) -> UiuaResult {
    let outputs = f.signature().outputs;
    let mut new_values = multi_output(outputs, Vec::with_capacity(xs.element_count()));
    let shape = env.without_fill(|env| -> UiuaResult<Shape> {
        Ok(match xs {
            Value::Box(xs) => {
                let shape = xs.shape().clone();
                for Boxed(x) in xs.data.into_iter() {
                    env.push(x);
                    env.call(f.clone())?;
                    for i in 0..outputs {
                        new_values[i].push(Boxed(env.pop("inventory's function result")?));
                    }
                }
                shape
            }
            value => {
                let shape = value.row_count().into();
                for val in value.into_rows() {
                    env.push(val);
                    env.call(f.clone())?;
                    for i in 0..outputs {
                        new_values[i].push(Boxed(env.pop("inventory's function result")?));
                    }
                }
                shape
            }
        })
    })?;
    for new_values in new_values.into_iter().rev() {
        let new_arr = Array::new(shape.clone(), new_values.into_iter().collect::<EcoVec<_>>());
        env.push(new_arr);
    }
    Ok(())
}

fn invertory2(f: Function, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let outputs = f.signature().outputs;
    let mut new_values = multi_output(outputs, Vec::with_capacity(xs.element_count()));
    let both_scalar = xs.rank() == 0 && ys.rank() == 0;
    fn finish(
        new_values: MultiOutput<Vec<Boxed>>,
        new_shape: Shape,
        both_scalar: bool,
        env: &mut Uiua,
    ) {
        for new_values in new_values.into_iter().rev() {
            let mut new_arr = Array::new(
                new_shape.clone(),
                new_values.into_iter().collect::<EcoVec<_>>(),
            );
            if both_scalar {
                new_arr.shape.make_row();
            }
            env.push(new_arr);
        }
    }
    match (xs, ys) {
        // Both box arrays
        (Value::Box(mut xs), Value::Box(mut ys)) => match (xs.row_count(), ys.row_count()) {
            (a, b) if a == b && xs.shape() == ys.shape() => {
                let shape = xs.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    for (Boxed(x), Boxed(y)) in xs.data.into_iter().zip(ys.data.into_iter()) {
                        env.push(y);
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (_, 1) => {
                let shape = xs.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    ys.shape.make_row();
                    for Boxed(x) in xs.data.into_iter() {
                        env.push(ys.clone());
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (1, _) => {
                let shape = ys.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    xs.shape.make_row();
                    for Boxed(y) in ys.data.into_iter() {
                        env.push(y);
                        env.push(xs.clone());
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            _ => Err(env.error(format!(
                "Cannot {} box arrays with shapes {} and {}",
                Primitive::Inventory.format(),
                xs.shape(),
                ys.shape()
            ))),
        },
        // Left box array
        (Value::Box(xs), ys) if ys.shape().starts_with(xs.shape()) => {
            let shape = xs.shape().clone();
            let ys_row_shape = ys.shape()[xs.rank()..].into();
            env.without_fill(|env| -> UiuaResult {
                for (Boxed(x), y) in
                    (xs.data.into_iter()).zip(ys.into_row_shaped_slices(ys_row_shape))
                {
                    env.push(y);
                    env.push(x);
                    env.call(f.clone())?;
                    for i in 0..outputs {
                        new_values[i].push(Boxed(env.pop("inventory's function result")?));
                    }
                }
                Ok(())
            })?;
            finish(new_values, shape, both_scalar, env);
            Ok(())
        }
        (Value::Box(mut xs), mut ys) if xs.rank() <= 1 => match (xs.row_count(), ys.row_count()) {
            (a, b) if a == b => {
                let shape = xs.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    for (Boxed(x), y) in xs.data.into_iter().zip(ys.into_rows()) {
                        env.push(y);
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (_, 1) => {
                let shape = xs.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    ys.shape_mut().make_row();
                    ys.unbox();
                    for Boxed(x) in xs.data.into_iter() {
                        env.push(ys.clone());
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (1, _) => {
                let shape = ys.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    xs.shape.make_row();
                    for y in ys.into_rows() {
                        env.push(y);
                        env.push(xs.clone());
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (a, b) => Err(env.error(format!(
                "Cannot {} box and non-box arrays with different number of rows {a} and {b}",
                Primitive::Inventory.format()
            ))),
        },
        // Right box array
        (xs, Value::Box(ys)) if xs.shape().starts_with(ys.shape()) => {
            let shape = ys.shape().clone();
            let xs_row_shape = xs.shape()[ys.rank()..].into();
            env.without_fill(|env| -> UiuaResult {
                for (x, Boxed(y)) in xs
                    .into_row_shaped_slices(xs_row_shape)
                    .zip(ys.data.into_iter())
                {
                    env.push(y);
                    env.push(x);
                    env.call(f.clone())?;
                    for i in 0..outputs {
                        new_values[i].push(Boxed(env.pop("inventory's function result")?));
                    }
                }
                Ok(())
            })?;
            finish(new_values, shape, both_scalar, env);
            Ok(())
        }
        (mut xs, Value::Box(mut ys)) if ys.rank() <= 1 => match (xs.row_count(), ys.row_count()) {
            (a, b) if a == b => {
                let shape = ys.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    for (x, Boxed(y)) in xs.into_rows().zip(ys.data.into_iter()) {
                        env.push(y);
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (_, 1) => {
                let shape = xs.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    ys.shape.make_row();
                    for x in xs.into_rows() {
                        env.push(ys.clone());
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (1, _) => {
                let shape = ys.shape().clone();
                env.without_fill(|env| -> UiuaResult {
                    xs.shape_mut().make_row();
                    xs.unbox();
                    for Boxed(y) in ys.data.into_iter() {
                        env.push(y);
                        env.push(xs.clone());
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_values[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                finish(new_values, shape, both_scalar, env);
                Ok(())
            }
            (a, b) => Err(env.error(format!(
                "Cannot {} box and non-box arrays with different number of rows {a} and {b}",
                Primitive::Inventory.format()
            ))),
        },
        // Both non-box arrays
        (mut xs, mut ys) => match (xs.row_count(), ys.row_count()) {
            (a, b) if a == b => {
                let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
                env.without_fill(|env| -> UiuaResult {
                    for (x, y) in xs.into_rows().zip(ys.into_rows()) {
                        env.push(y);
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                for new_rows in new_rows.into_iter().rev() {
                    let mut arr = Array::from_iter(new_rows);
                    if both_scalar {
                        arr.shape.make_row();
                    }
                    env.push(arr);
                }
                Ok(())
            }
            (_, 1) => {
                ys.shape_mut().make_row();
                ys.unbox();
                let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
                env.without_fill(|env| -> UiuaResult {
                    for x in xs.into_rows() {
                        env.push(ys.clone());
                        env.push(x);
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                for new_rows in new_rows.into_iter().rev() {
                    let mut arr = Array::from_iter(new_rows);
                    if both_scalar {
                        arr.shape.make_row();
                    }
                    env.push(arr);
                }
                Ok(())
            }
            (1, _) => {
                xs.shape_mut().make_row();
                xs.unbox();
                let mut new_rows = multi_output(outputs, Vec::with_capacity(ys.row_count()));
                env.without_fill(|env| -> UiuaResult {
                    for y in ys.into_rows() {
                        env.push(y);
                        env.push(xs.clone());
                        env.call(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(Boxed(env.pop("inventory's function result")?));
                        }
                    }
                    Ok(())
                })?;
                for new_rows in new_rows.into_iter().rev() {
                    let mut arr = Array::from_iter(new_rows);
                    if both_scalar {
                        arr.shape.make_row();
                    }
                    env.push(arr);
                }
                Ok(())
            }
            (a, b) => Err(env.error(format!(
                "Cannot {} arrays with different number of rows {a} and {b}",
                Primitive::Inventory.format(),
            ))),
        },
    }
}

fn inventoryn(f: Function, mut args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
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
                    "Cannot {} arrays with different number of rows, shapes {} and {}{e}",
                    Primitive::Inventory.format(),
                    args[a].shape(),
                    args[b].shape(),
                )));
            }
        }
    }
    let mut row_count = 0;
    let mut all_scalar = true;
    let mut all_1 = true;
    let outputs = f.signature().outputs;
    let is_empty = outputs > 0 && args.iter().any(|v| v.row_count() == 0);
    let mut arg_elems: Vec<_> = args
        .into_iter()
        .map(|mut v| {
            all_scalar = all_scalar && v.rank() == 0;
            if v.row_count() == 1 {
                if v.rank() == 0 {
                    v.unbox();
                } else {
                    v.shape_mut().make_row();
                }
                Err(v)
            } else {
                let proxy = is_empty.then(|| v.proxy_row(env));
                row_count = row_count.max(v.row_count());
                all_1 = false;
                Ok(v.into_rows().map(Value::unboxed).chain(proxy))
            }
        })
        .collect();
    if all_1 {
        row_count = 1;
    }
    let mut new_values = multi_output(outputs, Vec::new());
    env.without_fill(|env| -> UiuaResult {
        for _ in 0..row_count + is_empty as usize {
            for arg in arg_elems.iter_mut().rev() {
                match arg {
                    Ok(rows) => env.push(rows.next().unwrap()),
                    Err(row) => env.push(row.clone()),
                }
            }
            env.call(f.clone())?;
            for i in 0..outputs {
                new_values[i].push(Boxed(env.pop("inventory's function result")?));
            }
        }
        Ok(())
    })?;
    for new_values in new_values.into_iter().rev() {
        let mut arr = Array::<Boxed>::from_iter(new_values);
        if all_scalar {
            arr.shape.make_row();
        } else if is_empty {
            arr.pop_row();
        }
        arr.validate_shape();
        env.push(arr);
    }
    Ok(())
}

impl Value {
    pub(crate) fn length_is_fillable<C>(&self, ctx: &C) -> bool
    where
        C: FillContext,
    {
        if self.rank() == 0 {
            return false;
        }
        match self {
            Value::Num(_) => ctx.scalar_fill::<f64>().is_ok(),
            #[cfg(feature = "bytes")]
            Value::Byte(_) => ctx.scalar_fill::<u8>().is_ok(),
            Value::Complex(_) => ctx.scalar_fill::<Complex>().is_ok(),
            Value::Char(_) => ctx.scalar_fill::<char>().is_ok(),
            Value::Box(_) => ctx.scalar_fill::<Boxed>().is_ok(),
        }
    }
    pub(crate) fn fill_length_to<C>(&mut self, len: usize, ctx: &C) -> Result<(), &'static str>
    where
        C: FillContext,
    {
        match self {
            Value::Num(arr) => arr.fill_length_to(len, ctx),
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => arr.fill_length_to(len, ctx),
            Value::Complex(arr) => arr.fill_length_to(len, ctx),
            Value::Char(arr) => arr.fill_length_to(len, ctx),
            Value::Box(arr) => arr.fill_length_to(len, ctx),
        }
    }
}

impl<T: ArrayValue> Array<T> {
    pub(crate) fn fill_length_to<C>(&mut self, len: usize, ctx: &C) -> Result<(), &'static str>
    where
        C: FillContext,
    {
        if self.rank() == 0 || self.row_count() >= len {
            return Ok(());
        }
        let fill = ctx.scalar_fill::<T>()?;
        let more_elems = (len - self.row_count()) * self.row_len();
        self.data.reserve(more_elems);
        self.data.extend(repeat(fill).take(more_elems));
        self.shape[0] = len;
        Ok(())
    }
}
