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
    Uiua, UiuaResult,
};

use super::{multi_output, MultiOutput};

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
    Ok(())
}

fn each2(f: Arc<Function>, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
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
    if sig.outputs != 1 {
        return Err(env.error(format!(
            "Distribute's function must return 1 value, but it returns {}",
            sig.outputs
        )));
    }
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
                env.push(xs);
                return Ok(());
            }
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for x in xs.into_rows() {
                env.push(x);
                env.push(b.clone());
                env.push(a.clone());
                env.call_error_on_break(f.clone(), "break is not allowed in distribute")?;
                new_rows.push(env.pop("distribute's function result")?);
            }
            env.push(Value::from_row_values(new_rows, env)?);
        }
        n => {
            let mut args = Vec::with_capacity(n - 1);
            for i in 0..n - 1 {
                args.push(env.pop(i + 1)?);
            }
            let xs = env.pop(n)?;
            if xs.row_count() == 0 {
                env.push(xs);
                return Ok(());
            }
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for x in xs.into_rows() {
                env.push(x);
                for arg in args.iter().rev() {
                    env.push(arg.clone());
                }
                env.call_error_on_break(f.clone(), "break is not allowed in level")?;
                new_rows.push(env.pop("level's function result")?);
            }
            env.push(Value::from_row_values(new_rows, env)?);
        }
    }
    Ok(())
}

fn distribute2(f: Arc<Function>, a: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    if xs.row_count() == 0 {
        env.push(xs);
        return Ok(());
    }
    let mut new_rows = Vec::with_capacity(xs.row_count());
    for x in xs.into_rows() {
        env.push(x);
        env.push(a.clone());
        env.call_error_on_break(f.clone(), "break is not allowed in distribute")?;
        new_rows.push(env.pop("distribute's function result")?);
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}

pub fn tribute(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    if sig.outputs != 1 {
        return Err(env.error(format!(
            "Tribute's function must return 1 value, but it returns {}",
            sig.outputs
        )));
    }
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
                env.push(xs);
                return Ok(());
            }
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for x in xs.into_rows() {
                env.push(b.clone());
                env.push(a.clone());
                env.push(x);
                env.call_error_on_break(f.clone(), "break is not allowed in tribute")?;
                new_rows.push(env.pop("tribute's function result")?);
            }
            env.push(Value::from_row_values(new_rows, env)?);
        }
        n => {
            let mut args = Vec::with_capacity(n - 1);
            let xs = env.pop(1)?;
            for i in 0..n - 1 {
                args.push(env.pop(i + 2)?);
            }
            if xs.row_count() == 0 {
                env.push(xs);
                return Ok(());
            }
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for x in xs.into_rows() {
                for arg in args.iter().rev() {
                    env.push(arg.clone());
                }
                env.push(x);
                env.call_error_on_break(f.clone(), "break is not allowed in tribute")?;
                new_rows.push(env.pop("tribute's function result")?);
            }
            env.push(Value::from_row_values(new_rows, env)?);
        }
    }
    Ok(())
}

fn tribute2(f: Arc<Function>, xs: Value, a: Value, env: &mut Uiua) -> UiuaResult {
    if xs.row_count() == 0 {
        env.push(xs);
        return Ok(());
    }
    let mut new_rows = Vec::with_capacity(xs.row_count());
    for x in xs.into_rows() {
        env.push(a.clone());
        env.push(x);
        env.call_error_on_break(f.clone(), "break is not allowed in tribute")?;
        new_rows.push(env.pop("tribute's function result")?);
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}

pub fn level(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let ns = rank_list("Level", env)?;
    if let Some((end, init)) = ns.split_last() {
        if end.is_some_and(|n| n == -1) && !init.is_empty() && init.iter().all(Option::is_none) {
            return distribute(env);
        }
    }
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
            if xs.rank() == 0 {
                env.push(xs);
                return Ok(());
            }
            match n {
                Some(0) => return each1(f, xs, env),
                Some(-1) => return rows1(f, xs, env),
                None => {
                    env.push(xs);
                    return env.call(f);
                }
                Some(_) => {}
            }
            let n = rank_to_depth(n, xs.rank());
            let res = monadic_level_recursive(f, xs, n, env)?;
            env.push(res);
        }
        &[xn, yn] => {
            let xs = env.pop(1)?;
            let ys = env.pop(2)?;
            if xs.rank() == 0 && ys.rank() == 0 {
                env.push(xs);
                env.push(ys);
                return Ok(());
            }
            match (xn, yn) {
                (Some(0), Some(0)) => return each2(f, xs, ys, env),
                (Some(-1), Some(-1)) => return rows2(f, xs, ys, env),
                (None, Some(-1)) => return distribute2(f, xs, ys, env),
                (Some(-1), None) => return tribute2(f, xs, ys, env),
                (None, None) => {
                    env.push(ys);
                    env.push(xs);
                    return env.call(f);
                }
                _ => {}
            }
            let xn = rank_to_depth(xn, xs.rank());
            let yn = rank_to_depth(yn, ys.rank());
            let res = dyadic_level_recursive(f, xs, ys, xn, yn, env)?;
            env.push(res);
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

fn monadic_level_recursive(
    f: Arc<Function>,
    value: Value,
    n: usize,
    env: &mut Uiua,
) -> UiuaResult<Value> {
    if n == 0 {
        env.push(value);
        env.call(f)?;
        Ok(env.pop("level's function result")?)
    } else {
        let mut rows = Vec::with_capacity(value.row_count());
        for row in value.into_rows() {
            rows.push(monadic_level_recursive(f.clone(), row, n - 1, env)?);
        }
        Value::from_row_values(rows, env)
    }
}

fn dyadic_level_recursive(
    f: Arc<Function>,
    xs: Value,
    ys: Value,
    xn: usize,
    yn: usize,
    env: &mut Uiua,
) -> UiuaResult<Value> {
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
    Ok(match (xn, yn) {
        (0, 0) => {
            env.push(ys);
            env.push(xs);
            env.call(f)?;
            env.pop("level's function result")?
        }
        (0, yn) => {
            let mut new_rows = Vec::with_capacity(ys.row_count());
            for y in ys.into_rows() {
                new_rows.push(dyadic_level_recursive(
                    f.clone(),
                    xs.clone(),
                    y,
                    xn,
                    yn - 1,
                    env,
                )?);
            }
            Value::from_row_values(new_rows, env)?
        }
        (xn, 0) => {
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for x in xs.into_rows() {
                new_rows.push(dyadic_level_recursive(
                    f.clone(),
                    x,
                    ys.clone(),
                    xn - 1,
                    yn,
                    env,
                )?);
            }
            Value::from_row_values(new_rows, env)?
        }
        (xn, yn) => {
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for (x, y) in xs.into_rows().zip(ys.into_rows()) {
                new_rows.push(dyadic_level_recursive(
                    f.clone(),
                    x,
                    y,
                    xn - 1,
                    yn - 1,
                    env,
                )?);
            }
            Value::from_row_values(new_rows, env)?
        }
    })
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
