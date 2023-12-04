//! Algorithms for zipping modifiers

use crate::{
    algorithm::pervade::bin_pervade_generic,
    array::{FormatShape, Shape},
    function::Function,
    value::Value,
    ImplPrimitive, Instr, Primitive, Uiua, UiuaResult,
};

use super::{multi_output, MultiOutput};

type ValueUnFn = Box<dyn Fn(Value, usize, &mut Uiua) -> UiuaResult<Value>>;
type ValueBinFn = Box<dyn Fn(Value, Value, usize, usize, &mut Uiua) -> UiuaResult<Value>>;

fn spanned_un_fn(
    span: usize,
    f: impl Fn(Value, usize, &Uiua) -> UiuaResult<Value> + 'static,
) -> ValueUnFn {
    Box::new(move |v, d, env| env.with_span(span, |env| f(v, d, env)))
}

fn prim_un_fast_fn(prim: Primitive, span: usize) -> Option<ValueUnFn> {
    use Primitive::*;
    Some(match prim {
        Not => spanned_un_fn(span, |v, _, env| Value::not(v, env)),
        Sign => spanned_un_fn(span, |v, _, env| Value::sign(v, env)),
        Neg => spanned_un_fn(span, |v, _, env| Value::neg(v, env)),
        Abs => spanned_un_fn(span, |v, _, env| Value::abs(v, env)),
        Sqrt => spanned_un_fn(span, |v, _, env| Value::sqrt(v, env)),
        Floor => spanned_un_fn(span, |v, _, env| Value::floor(v, env)),
        Ceil => spanned_un_fn(span, |v, _, env| Value::ceil(v, env)),
        Round => spanned_un_fn(span, |v, _, env| Value::round(v, env)),
        Deshape => spanned_un_fn(span, |mut v, d, _| {
            Value::deshape_depth(&mut v, d);
            Ok(v)
        }),
        Transpose => spanned_un_fn(span, |mut v, d, _| {
            Value::transpose_depth(&mut v, d);
            Ok(v)
        }),
        Reverse => spanned_un_fn(span, |mut v, d, _| {
            Value::reverse_depth(&mut v, d);
            Ok(v)
        }),
        _ => return None,
    })
}

fn impl_prim_un_fast_fn(prim: ImplPrimitive, span: usize) -> Option<ValueUnFn> {
    use ImplPrimitive::*;
    Some(match prim {
        InvTranspose => spanned_un_fn(span, |mut v, d, _| {
            Value::inv_transpose_depth(&mut v, d);
            Ok(v)
        }),
        _ => return None,
    })
}

fn instrs_un_fast_fn(instrs: &[Instr]) -> Option<(ValueUnFn, usize)> {
    use Primitive::*;
    match instrs {
        &[Instr::Prim(prim, span)] => {
            let f = prim_un_fast_fn(prim, span)?;
            return Some((f, 0));
        }
        &[Instr::ImplPrim(prim, span)] => {
            let f = impl_prim_un_fast_fn(prim, span)?;
            return Some((f, 0));
        }
        [Instr::PushFunc(f), Instr::Prim(Rows, _)] => {
            let (f, d) = instrs_un_fast_fn(&f.instrs)?;
            return Some((f, d + 1));
        }
        _ => (),
    }
    None
}

fn spanned_bin_fn(
    span: usize,
    f: impl Fn(Value, Value, usize, usize, &Uiua) -> UiuaResult<Value> + 'static,
) -> ValueBinFn {
    Box::new(move |a, b, ad, bd, env| env.with_span(span, |env| f(a, b, ad, bd, env)))
}

fn prim_bin_fast_fn(prim: Primitive, span: usize) -> Option<ValueBinFn> {
    use std::boxed::Box;
    use Primitive::*;
    Some(match prim {
        Add => spanned_bin_fn(span, Value::add),
        Sub => spanned_bin_fn(span, Value::sub),
        Mul => spanned_bin_fn(span, Value::mul),
        Div => spanned_bin_fn(span, Value::div),
        Pow => spanned_bin_fn(span, Value::pow),
        Mod => spanned_bin_fn(span, Value::modulus),
        Log => spanned_bin_fn(span, Value::log),
        Eq => spanned_bin_fn(span, Value::is_eq),
        Ne => spanned_bin_fn(span, Value::is_ne),
        Lt => spanned_bin_fn(span, Value::is_lt),
        Gt => spanned_bin_fn(span, Value::is_gt),
        Le => spanned_bin_fn(span, Value::is_le),
        Ge => spanned_bin_fn(span, Value::is_ge),
        Complex => spanned_bin_fn(span, Value::complex),
        Max => spanned_bin_fn(span, Value::max),
        Min => spanned_bin_fn(span, Value::min),
        Atan => spanned_bin_fn(span, Value::atan2),
        Rotate => Box::new(move |a, b, ad, bd, env| {
            env.with_span(span, |env| a.rotate_depth(b, ad, bd, env))
        }),
        _ => return None,
    })
}

pub(crate) fn instrs_bin_fast_fn(instrs: &[Instr]) -> Option<(ValueBinFn, usize, usize)> {
    use std::boxed::Box;
    use Primitive::*;

    fn nest_bin_fast<F>(
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
            let f = prim_bin_fast_fn(prim, span)?;
            return Some((f, 0, 0));
        }
        [Instr::PushFunc(f), Instr::Prim(Rows, _)] => {
            return nest_bin_fast(instrs_bin_fast_fn(&f.instrs)?, 1, 1)
        }
        [Instr::Prim(Flip, _), rest @ ..] => {
            let (f, a, b) = instrs_bin_fast_fn(rest)?;
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
        0 => Err(env.error("Each's function must take at least 1 argument")),
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

fn each1(f: Function, xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, ..)) = instrs_un_fast_fn(&f.instrs) {
        let rank = xs.rank();
        let val = f(xs, rank, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let mut new_values = multi_output(outputs, Vec::with_capacity(xs.element_count()));
        let new_shape = Shape::from(xs.shape());
        let is_empty = outputs > 0 && xs.row_count() == 0;
        let prototype = is_empty.then(|| xs.prototype_scalar(env));
        let mut old_values = xs.into_elements();
        for val in old_values.by_ref().chain(prototype) {
            env.push(val);
            env.call(f.clone())?;
            for i in 0..outputs {
                new_values[i].push(env.pop("each's function result")?);
            }
        }
        for new_values in new_values.into_iter().rev() {
            let mut new_shape = new_shape.clone();
            let mut eached = Value::from_row_values(new_values, env)?;
            if is_empty {
                eached.pop_row();
            }
            new_shape.extend_from_slice(&eached.shape()[1..]);
            *eached.shape_mut() = new_shape;
            env.push(eached);
        }
    }
    Ok(())
}

fn each2(f: Function, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    if !xs.shape().iter().zip(ys.shape()).all(|(a, b)| a == b) {
        let min_rank = xs.rank().min(ys.rank());
        return Err(env.error(format!(
            "Cannot each arrays with shapes {} and {} because their \
            shape prefixes {} and {} are different",
            xs.format_shape(),
            ys.format_shape(),
            FormatShape(&xs.shape()[..min_rank]),
            FormatShape(&ys.shape()[..min_rank])
        )));
    }
    if let Some((f, ..)) = instrs_bin_fast_fn(&f.instrs) {
        let xrank = xs.rank();
        let yrank = ys.rank();
        let val = f(xs, ys, xrank, yrank, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let mut xs_shape = xs.shape().to_vec();
        let mut ys_shape = ys.shape().to_vec();
        let is_empty = outputs > 0 && xs.row_count() == 0 || ys.row_count() == 0;
        let xs_proto = is_empty.then(|| xs.prototype_scalar(env));
        let ys_proto = is_empty.then(|| ys.prototype_scalar(env));
        if is_empty {
            if let Some(r) = xs_shape.first_mut() {
                *r += 1;
            }
            if let Some(r) = ys_shape.first_mut() {
                *r += 1;
            }
        }
        let xs_values: Vec<_> = xs.into_elements().chain(xs_proto).collect();
        let ys_values: Vec<_> = ys.into_elements().chain(ys_proto).collect();
        let (new_shape, new_values) = bin_pervade_generic(
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
        )?;
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
            env.push(eached);
        }
    }
    Ok(())
}

fn eachn(f: Function, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
    for win in args.windows(2) {
        if win[0].shape() != win[1].shape() {
            return Err(env.error(format!(
                "The shapes in each of 3 or more arrays must all match, but shapes {} and {} cannot be eached together. \
                If you want more flexibility, use rows.",
                win[0].format_shape(),
                win[1].format_shape()
            )));
        }
    }
    let outputs = f.signature().outputs;
    let is_empty = outputs > 0 && args.iter().any(|v| v.row_count() == 0);
    let prototypes: Vec<_> = args
        .iter()
        .map(|v| is_empty.then(|| v.prototype_scalar(env)))
        .collect();
    let elem_count = args[0].element_count() + is_empty as usize;
    let mut new_values = multi_output(outputs, Vec::with_capacity(elem_count));
    let new_shape = Shape::from(args[0].shape());
    let mut arg_elems: Vec<_> = args
        .into_iter()
        .zip(prototypes)
        .map(|(v, proto)| v.into_elements().chain(proto))
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
    for new_values in new_values.into_iter().rev() {
        let mut new_shape = new_shape.clone();
        let mut eached = Value::from_row_values(new_values, env)?;
        if is_empty {
            eached.pop_row();
        }
        new_shape.extend_from_slice(&eached.shape()[1..]);
        *eached.shape_mut() = new_shape;
        env.push(eached);
    }
    Ok(())
}

pub fn rows(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    match sig.args {
        0 => Err(env.error("Rows' function must take at least 1 argument")),
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

fn rows1(f: Function, xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, d)) = instrs_un_fast_fn(&f.instrs) {
        let val = f(xs, d + 1, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let is_empty = outputs > 0 && xs.row_count() == 0;
        let prototype = is_empty.then(|| xs.prototype_row(env));
        let mut new_rows =
            multi_output(outputs, Value::builder(xs.row_count() + is_empty as usize));
        for row in xs.into_rows().chain(prototype) {
            env.push(row);
            env.call(f.clone())?;
            for i in 0..outputs {
                new_rows[i].add_row(env.pop("rows' function result")?, env)?;
            }
        }
        for new_rows in new_rows.into_iter().rev() {
            let mut val = new_rows.finish();
            if is_empty {
                val.pop_row();
            }
            env.push(val);
        }
    }
    Ok(())
}

fn rows2(f: Function, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let outputs = f.signature().outputs;
    match (xs.row_count(), ys.row_count()) {
        (a, b) if a == b => {
            if let Some((f, a, b)) = instrs_bin_fast_fn(&f.instrs) {
                let val = f(xs, ys, a + 1, b + 1, env)?;
                env.push(val);
            } else {
                let is_empty = outputs > 0 && xs.row_count() == 0 || ys.row_count() == 0;
                let xs_proto = is_empty.then(|| xs.prototype_row(env));
                let ys_proto = is_empty.then(|| ys.prototype_row(env));
                let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
                let x_rows = xs.into_rows().chain(xs_proto);
                let y_rows = ys.into_rows().chain(ys_proto);
                for (x, y) in x_rows.into_iter().zip(y_rows) {
                    env.push(y);
                    env.push(x);
                    env.call(f.clone())?;
                    for i in 0..outputs {
                        new_rows[i].push(env.pop("rows's function result")?);
                    }
                }
                for new_rows in new_rows.into_iter().rev() {
                    let mut val = Value::from_row_values(new_rows, env)?;
                    if is_empty {
                        val.pop_row();
                    }
                    env.push(val);
                }
            }
            Ok(())
        }
        (_, 1) => {
            let ys = ys.into_rows().next().unwrap();
            let is_empty = outputs > 0 && xs.row_count() == 0;
            let prototype = is_empty.then(|| xs.prototype_row(env));
            let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
            for x in xs.into_rows().chain(prototype) {
                env.push(ys.clone());
                env.push(x);
                env.call(f.clone())?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("rows's function result")?);
                }
            }
            for new_rows in new_rows.into_iter().rev() {
                let mut val = Value::from_row_values(new_rows, env)?;
                if is_empty {
                    val.pop_row();
                }
                env.push(val);
            }
            Ok(())
        }
        (1, _) => {
            let xs = xs.into_rows().next().unwrap();
            let is_empty = outputs > 0 && ys.row_count() == 0;
            let prototype = is_empty.then(|| ys.prototype_row(env));
            let mut new_rows = multi_output(outputs, Vec::with_capacity(ys.row_count()));
            for y in ys.into_rows().chain(prototype) {
                env.push(y);
                env.push(xs.clone());
                env.call(f.clone())?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("rows's function result")?);
                }
            }
            for new_rows in new_rows.into_iter().rev() {
                let mut val = Value::from_row_values(new_rows, env)?;
                if is_empty {
                    val.pop_row();
                }
                env.push(val);
            }
            Ok(())
        }
        (a, b) => Err(env.error(format!(
            "Cannot rows arrays with different number of rows {a} and {b}",
        ))),
    }
}

fn rowsn(f: Function, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
    for a in 0..args.len() {
        for b in a + 1..args.len() {
            if !(args[a].row_count() == 1 || args[b].row_count() == 1)
                && args[a].row_count() != args[b].row_count()
            {
                return Err(env.error(format!(
                    "Cannot rows arrays with different number of rows {} and {}",
                    args[a].row_count(),
                    args[b].row_count()
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
        .map(|v| {
            all_scalar = all_scalar && v.rank() == 0;
            if v.row_count() == 1 {
                Err(v.into_rows().next().unwrap())
            } else {
                let proto = is_empty.then(|| v.prototype_row(env));
                row_count = row_count.max(v.row_count());
                all_1 = false;
                Ok(v.into_rows().chain(proto))
            }
        })
        .collect();
    if all_1 {
        row_count = 1;
    }
    let mut new_values = multi_output(outputs, Vec::new());
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
    for new_values in new_values.into_iter().rev() {
        let mut eached = Value::from_row_values(new_values, env)?;
        if all_scalar {
            eached.shape_mut().remove(0);
        } else if is_empty {
            eached.pop_row();
        }
        eached.validate_shape();
        env.push(eached);
    }
    Ok(())
}
