//! Algorithms for zipping modifiers

use std::sync::Arc;

use crate::{
    algorithm::{
        loops::{rank_list, rank_to_depth},
        pervade::bin_pervade_generic,
    },
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
        Rotate => spanned_bin_fn(span, |a, b, ad, bd, env| a.rotate_depth(b, ad, bd, env)),
        _ => return None,
    })
}

fn instrs_bin_fast_fn(instrs: &[Instr]) -> Option<(ValueBinFn, usize, usize)> {
    use std::boxed::Box;
    use Primitive::*;
    match instrs {
        &[Instr::Prim(prim, span)] => {
            let f = prim_bin_fast_fn(prim, span)?;
            return Some((f, 0, 0));
        }
        [Instr::PushFunc(f), Instr::Prim(Rows, _)] => {
            let (f, a, b) = instrs_bin_fast_fn(&f.instrs)?;
            return Some((f, a + 1, b + 1));
        }
        [Instr::PushFunc(f), Instr::Prim(Distribute, _)] => {
            let (f, a, b) = instrs_bin_fast_fn(&f.instrs)?;
            return Some((f, a, b + 1));
        }
        [Instr::PushFunc(f), Instr::Prim(Tribute, _)] => {
            let (f, a, b) = instrs_bin_fast_fn(&f.instrs)?;
            return Some((f, a + 1, b));
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

fn each1(f: Arc<Function>, xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, ..)) = instrs_un_fast_fn(&f.instrs) {
        let rank = xs.rank();
        let val = f(xs, rank, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let mut new_values = multi_output(outputs, Vec::with_capacity(xs.element_count()));
        let new_shape = Shape::from(xs.shape());
        let mut old_values = xs.into_elements();
        for val in old_values.by_ref() {
            env.push(val);
            let broke = env.call_catch_break(f.clone())?;
            for i in 0..outputs {
                new_values[i].push(env.pop("each's function result")?);
            }
            if broke {
                for row in old_values {
                    for i in 0..outputs {
                        new_values[i].push(row.clone());
                    }
                }
                break;
            }
        }
        for new_values in new_values.into_iter().rev() {
            let mut new_shape = new_shape.clone();
            let mut eached = Value::from_row_values(new_values, env)?;
            new_shape.extend_from_slice(&eached.shape()[1..]);
            *eached.shape_mut() = new_shape;
            env.push(eached);
        }
    }
    Ok(())
}

fn each2(f: Arc<Function>, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, ..)) = instrs_bin_fast_fn(&f.instrs) {
        let xrank = xs.rank();
        let yrank = ys.rank();
        let val = f(xs, ys, xrank, yrank, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let xs_shape = xs.shape().to_vec();
        let ys_shape = ys.shape().to_vec();
        let xs_values: Vec<_> = xs.into_elements().collect();
        let ys_values: Vec<_> = ys.into_elements().collect();
        let (new_shape, new_values) = bin_pervade_generic(
            &xs_shape,
            xs_values,
            &ys_shape,
            ys_values,
            env,
            |x, y, env| {
                env.push(y);
                env.push(x);
                env.call_error_on_break(f.clone(), "break is not allowed in multi-argument each")?;
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
            new_shape.extend_from_slice(&eached.shape()[1..]);
            *eached.shape_mut() = new_shape;
            env.push(eached);
        }
    }
    Ok(())
}

fn eachn(f: Arc<Function>, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
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
    let elem_count = args[0].element_count();
    let mut arg_elems: Vec<_> = args.into_iter().map(|v| v.into_elements()).collect();
    let mut new_values = Vec::new();
    for _ in 0..elem_count {
        for arg in arg_elems.iter_mut().rev() {
            env.push(arg.next().unwrap());
        }
        env.call_error_on_break(f.clone(), "break is not allowed in multi-argument each")?;
        new_values.push(env.pop("each's function result")?);
    }
    let eached = Value::from_row_values(new_values, env)?;
    env.push(eached);
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

fn rows1(f: Arc<Function>, xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, d)) = instrs_un_fast_fn(&f.instrs) {
        let val = f(xs, d + 1, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let mut new_rows = multi_output(outputs, Value::builder(xs.row_count()));
        let mut old_rows = xs.into_rows();
        for row in old_rows.by_ref() {
            env.push(row);
            let broke = env.call_catch_break(f.clone())?;
            for i in 0..outputs {
                new_rows[i].add_row(env.pop("rows' function result")?, env)?;
            }
            if broke {
                for row in old_rows {
                    for i in 0..outputs {
                        new_rows[i].add_row(row.clone(), env)?;
                    }
                }
                break;
            }
        }
        for new_rows in new_rows.into_iter().rev() {
            env.push(new_rows.finish());
        }
    }
    Ok(())
}

fn rows2(f: Arc<Function>, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    if xs.row_count() != ys.row_count() {
        return Err(env.error(format!(
            "Cannot rows arrays with different number of rows {} and {}",
            xs.row_count(),
            ys.row_count()
        )));
    }
    if let Some((f, a, b)) = instrs_bin_fast_fn(&f.instrs) {
        let val = f(xs, ys, a + 1, b + 1, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
        let x_rows = xs.into_rows();
        let y_rows = ys.into_rows();
        for (x, y) in x_rows.into_iter().zip(y_rows) {
            env.push(y);
            env.push(x);
            env.call_error_on_break(f.clone(), "break is not allowed in multi-argument rows")?;
            for i in 0..outputs {
                new_rows[i].push(env.pop("rows's function result")?);
            }
        }
        for new_rows in new_rows.into_iter().rev() {
            env.push(Value::from_row_values(new_rows, env)?);
        }
    }
    Ok(())
}

fn rowsn(f: Arc<Function>, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
    for win in args.windows(2) {
        if win[0].row_count() != win[1].row_count() {
            return Err(env.error(format!(
                "The number of rows in each of 3 or more arrays must all match, \
                but arrays with {} and {} rows were found.",
                win[0].row_count(),
                win[1].row_count()
            )));
        }
    }
    let row_count = args[0].row_count();
    let mut arg_elems: Vec<_> = args.into_iter().map(|v| v.into_rows()).collect();
    let outputs = f.signature().outputs;
    let mut new_values = multi_output(outputs, Vec::new());
    for _ in 0..row_count {
        for arg in arg_elems.iter_mut().rev() {
            env.push(arg.next().unwrap());
        }
        env.call_error_on_break(f.clone(), "break is not allowed in multi-argument each")?;
        for i in 0..outputs {
            new_values[i].push(env.pop("rows's function result")?);
        }
    }
    for new_values in new_values.into_iter().rev() {
        let eached = Value::from_row_values(new_values, env)?;
        env.push(eached);
    }
    Ok(())
}

pub fn distribute(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    let outputs = sig.outputs;
    match sig.args {
        n @ (0 | 1) => {
            return Err(env.error(format!(
                "Distribute's function must take at least 2 arguments, \
                but it takes {n}"
            )))
        }
        2 => {
            let a = env.pop(1)?;
            let xs = env.pop(2)?;
            distribute2(f, a, xs, env)?;
        }
        3 => {
            let a = env.pop(1)?;
            let b = env.pop(2)?;
            let xs = env.pop(3)?;
            if xs.row_count() == 0 {
                for _ in 0..outputs {
                    env.push(xs.clone());
                }
                return Ok(());
            }
            let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
            for x in xs.into_rows() {
                env.push(x);
                env.push(b.clone());
                env.push(a.clone());
                env.call_error_on_break(f.clone(), "break is not allowed in distribute")?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("distribute's function result")?);
                }
            }
            for new_rows in new_rows.into_iter().rev() {
                env.push(Value::from_row_values(new_rows, env)?);
            }
        }
        n => {
            let mut args = Vec::with_capacity(n - 1);
            for i in 0..n - 1 {
                args.push(env.pop(i + 1)?);
            }
            let xs = env.pop(n)?;
            if xs.row_count() == 0 {
                for _ in 0..outputs {
                    env.push(xs.clone());
                }
                return Ok(());
            }
            let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
            for x in xs.into_rows() {
                env.push(x);
                for arg in args.iter().rev() {
                    env.push(arg.clone());
                }
                env.call_error_on_break(f.clone(), "break is not allowed in level")?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("distribute's function result")?);
                }
            }
            for new_rows in new_rows.into_iter().rev() {
                env.push(Value::from_row_values(new_rows, env)?);
            }
        }
    }
    Ok(())
}

fn distribute2(f: Arc<Function>, a: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, xd, yd)) = instrs_bin_fast_fn(&f.instrs) {
        let val = f(a, xs, xd, yd + 1, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        if xs.row_count() == 0 {
            for _ in 0..outputs {
                env.push(xs.clone());
            }
            return Ok(());
        }
        let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
        for x in xs.into_rows() {
            env.push(x);
            env.push(a.clone());
            env.call_error_on_break(f.clone(), "break is not allowed in distribute")?;
            for i in 0..outputs {
                new_rows[i].push(env.pop("distribute's function result")?);
            }
        }
        for new_rows in new_rows.into_iter().rev() {
            env.push(Value::from_row_values(new_rows, env)?);
        }
    }
    Ok(())
}

pub fn tribute(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    let outputs = sig.outputs;
    match sig.args {
        n @ (0 | 1) => {
            return Err(env.error(format!(
                "Tribute's function must take at least 2 arguments, \
                but it takes {n}"
            )))
        }
        2 => {
            let xs = env.pop(1)?;
            let a = env.pop(2)?;
            tribute2(f, xs, a, env)?;
        }
        3 => {
            let xs = env.pop(1)?;
            let a = env.pop(2)?;
            let b = env.pop(3)?;
            if xs.row_count() == 0 {
                for _ in 0..outputs {
                    env.push(xs.clone());
                }
                return Ok(());
            }
            let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
            for x in xs.into_rows() {
                env.push(b.clone());
                env.push(a.clone());
                env.push(x);
                env.call_error_on_break(f.clone(), "break is not allowed in tribute")?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("tribute's function result")?);
                }
            }
            for new_rows in new_rows.into_iter().rev() {
                env.push(Value::from_row_values(new_rows, env)?);
            }
        }
        n => {
            let mut args = Vec::with_capacity(n - 1);
            let xs = env.pop(1)?;
            for i in 0..n - 1 {
                args.push(env.pop(i + 2)?);
            }
            if xs.row_count() == 0 {
                for _ in 0..outputs {
                    env.push(xs.clone());
                }
                return Ok(());
            }
            let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
            for x in xs.into_rows() {
                for arg in args.iter().rev() {
                    env.push(arg.clone());
                }
                env.push(x);
                env.call_error_on_break(f.clone(), "break is not allowed in tribute")?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("tribute's function result")?);
                }
            }
            for new_rows in new_rows.into_iter().rev() {
                env.push(Value::from_row_values(new_rows, env)?);
            }
        }
    }
    Ok(())
}

fn tribute2(f: Arc<Function>, xs: Value, a: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, xd, yd)) = instrs_bin_fast_fn(&f.instrs) {
        let val = f(xs, a, xd + 1, yd, env)?;
        env.push(val);
    } else {
        let outputs = f.signature().outputs;
        if xs.row_count() == 0 {
            for _ in 0..outputs {
                env.push(xs.clone());
            }
            return Ok(());
        }
        let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
        for x in xs.into_rows() {
            env.push(a.clone());
            env.push(x);
            env.call_error_on_break(f.clone(), "break is not allowed in tribute")?;
            for i in 0..outputs {
                new_rows[i].push(env.pop("tribute's function result")?);
            }
        }
        for new_rows in new_rows.into_iter().rev() {
            env.push(Value::from_row_values(new_rows, env)?);
        }
    }
    Ok(())
}

pub fn level(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let ns = rank_list("Level", env)?;
    let f = env.pop_function()?;
    let f_sig = f.signature();
    if f_sig.outputs != 1 {
        return Err(env.error(format!(
            "Level's function must return 1 value, but it returns {}",
            f_sig.outputs
        )));
    }
    if f_sig.args != ns.len() {
        return Err(env.error(format!(
            "Level's rank list has {} elements, but its function takes {} arguments",
            ns.len(),
            f_sig.args
        )));
    }
    match ns.as_slice() {
        [] => return Ok(()),
        &[n] => {
            let xs = env.pop(1)?;
            let n = rank_to_depth(n, xs.rank());
            match n {
                0 => {
                    env.push(xs);
                    return env.call(f);
                }
                1 => return rows1(f, xs, env),
                n if n == xs.rank() => return each1(f, xs, env),
                _ => {}
            }
            monadic_level(f, xs, n, env)?;
        }
        &[xn, yn] => {
            let xs = env.pop(1)?;
            let ys = env.pop(2)?;
            let xn = rank_to_depth(xn, xs.rank());
            let yn = rank_to_depth(yn, ys.rank());
            match (xn, yn) {
                (0, 0) => {
                    env.push(ys);
                    env.push(xs);
                    return env.call(f);
                }
                (1, 1) => return rows2(f, xs, ys, env),
                (0, 1) => return distribute2(f, xs, ys, env),
                (1, 0) => return tribute2(f, xs, ys, env),
                (a, b) if a == xs.rank() && b == ys.rank() => return each2(f, xs, ys, env),
                _ => {}
            }
            if let Some((f, a, b)) = instrs_bin_fast_fn(&f.instrs) {
                let value = f(xs, ys, xn + a, yn + b, env)?;
                env.push(value);
            } else {
                dyadic_level(f, xs, ys, xn, yn, env)?;
            }
        }
        is => {
            let mut args = Vec::with_capacity(is.len());
            for i in 0..is.len() {
                let arg = env.pop(i + 1)?;
                args.push(arg);
            }
            let mut ns: Vec<usize> = Vec::with_capacity(is.len());
            for (i, arg) in args.iter().enumerate() {
                ns.push(rank_to_depth(is[i], arg.rank()));
            }
            let res = multi_level_recursive(f, args, &ns, env)?;
            env.push(res);
        }
    }
    Ok(())
}

fn monadic_level(f: Arc<Function>, value: Value, mut n: usize, env: &mut Uiua) -> UiuaResult {
    if let Some((f, d)) = instrs_un_fast_fn(&f.instrs) {
        let val = f(value, d + n, env)?;
        env.push(val);
    } else if n == 0 {
        env.push(value);
        env.call(f)?;
    } else {
        n = n.min(value.rank());
        let row_shape = Shape::from(&value.shape()[n..]);
        let mut new_shape = Shape::from(&value.shape()[..n]);
        let mut new_rows = Value::builder(new_shape.iter().product());
        for value in value.row_shaped_slices(row_shape) {
            env.push(value);
            env.call_error_on_break(f.clone(), "break is not allowed in level")?;
            let row = env.pop("level's function result")?;
            new_rows.add_row(row, env)?;
        }
        let mut new_value = new_rows.finish();
        new_shape.extend_from_slice(&new_value.shape()[1..]);
        *new_value.shape_mut() = new_shape;
        new_value.validate_shape();
        env.push(new_value);
    }
    Ok(())
}

fn dyadic_level(
    f: Arc<Function>,
    xs: Value,
    ys: Value,
    mut xn: usize,
    mut yn: usize,
    env: &mut Uiua,
) -> UiuaResult {
    xn = xn.min(xs.rank());
    yn = yn.min(ys.rank());
    let xs_prefix = &xs.shape()[..xn];
    let ys_prefix = &ys.shape()[..yn];
    if !xs_prefix.iter().zip(ys_prefix).all(|(a, b)| a == b) {
        return Err(env.error(format!(
            "Cannot level with ranks {} and {} arrays with shapes {} and {} \
            because shape prefixes {} and {} are not compatible",
            xs.rank() - xn,
            ys.rank() - yn,
            xs.format_shape(),
            ys.format_shape(),
            FormatShape(xs_prefix),
            FormatShape(ys_prefix)
        )));
    }
    let xs_row_shape = Shape::from(&xs.shape()[xn..]);
    let ys_row_shape = Shape::from(&ys.shape()[yn..]);
    let mut new_rows = Vec::new();
    if xn == yn {
        for (x, y) in xs
            .row_shaped_slices(xs_row_shape)
            .zip(ys.row_shaped_slices(ys_row_shape))
        {
            env.push(y);
            env.push(x);
            env.call_error_on_break(f.clone(), "break is not allowed in level")?;
            let row = env.pop("level's function result")?;
            new_rows.push(row);
        }
        let mut new_value = Value::from_row_values(new_rows, env)?;
        let mut new_shape = Shape::from_iter(if xs.shape().len() > ys.shape().len() {
            xs.shape()[..xn].iter().copied()
        } else {
            ys.shape()[..yn].iter().copied()
        });
        new_shape.extend_from_slice(&new_value.shape()[1..]);
        *new_value.shape_mut() = new_shape;
        new_value.validate_shape();
        env.push(new_value);
    } else {
        for x in xs.row_shaped_slices(xs_row_shape) {
            for y in ys.row_shaped_slices(ys_row_shape.clone()) {
                env.push(y);
                env.push(x.clone());
                env.call_error_on_break(f.clone(), "break is not allowed in level")?;
                let row = env.pop("level's function result")?;
                new_rows.push(row);
            }
        }
        let mut new_value = Value::from_row_values(new_rows, env)?;
        let mut new_shape =
            Shape::from_iter(xs.shape()[..xn].iter().chain(&ys.shape()[..yn]).copied());
        new_shape.extend_from_slice(&new_value.shape()[1..]);
        *new_value.shape_mut() = new_shape;
        new_value.validate_shape();
        env.push(new_value);
    }
    Ok(())
}

fn multi_level_recursive(
    f: Arc<Function>,
    args: Vec<Value>,
    ns: &[usize],
    env: &mut Uiua,
) -> UiuaResult<Value> {
    if ns.iter().all(|&n| n == 0) {
        for arg in args.into_iter().rev() {
            env.push(arg);
        }
        env.call_error_on_break(f, "break is not allowed in level")?;
        Ok(env.pop("level's function result")?)
    } else {
        let (&n_with_max_row_count, arg_with_max_row_count) = ns
            .iter()
            .zip(&args)
            .max_by_key(|&(&n, v)| if n == 0 { 1 } else { v.shape()[0] })
            .unwrap();
        for (n, arg) in ns.iter().zip(&args) {
            if !arg.shape()[..*n]
                .iter()
                .zip(&arg_with_max_row_count.shape()[..n_with_max_row_count])
                .all(|(a, b)| a == b)
            {
                return Err(env.error(format!(
                    "Cannot level with ranks {} and {} arrays with shapes {} and {}",
                    arg_with_max_row_count.rank() - n_with_max_row_count,
                    arg.rank() - n,
                    arg_with_max_row_count.format_shape(),
                    arg.format_shape()
                )));
            }
        }
        let row_count = if n_with_max_row_count == 0 {
            1
        } else {
            arg_with_max_row_count.shape()[0]
        };
        let mut rows = Vec::with_capacity(row_count);
        let mut row_args = args.clone();
        let dec_ns: Vec<usize> = ns.iter().map(|n| n.saturating_sub(1)).collect();
        for i in 0..row_count {
            for (j, (arg, n)) in args.iter().zip(ns).enumerate() {
                row_args[j] = if *n == 0 { arg.clone() } else { arg.row(i) };
            }
            let row = multi_level_recursive(f.clone(), row_args.clone(), &dec_ns, env)?;
            rows.push(row);
        }
        Value::from_row_values(rows, env)
    }
}
