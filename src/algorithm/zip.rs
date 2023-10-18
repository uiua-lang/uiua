//! Algorithms for zipping modifiers

use crate::{
    algorithm::{
        loops::{rank_list, rank_to_depth},
        pervade::bin_pervade_generic,
    },
    array::{FormatShape, Shape},
    run::{ArrayArg, FunctionArg},
    value::Value,
    Uiua, UiuaResult,
};

pub fn each(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(FunctionArg(1))?;
    let sig = f.signature();
    let output = match sig.outputs {
        0 => false,
        1 => true,
        n => {
            return Err(env.error(format!(
                "Each's function must return 0 or 1 values, but it returns {}",
                n
            )))
        }
    };
    match sig.args {
        0 => Ok(()),
        1 => {
            let xs = env.pop(ArrayArg(1))?;
            if output {
                each1_1(f, xs, env)
            } else {
                each1_0(f, xs, env)
            }
        }
        2 => {
            let xs = env.pop(ArrayArg(1))?;
            let ys = env.pop(ArrayArg(2))?;
            if output {
                each2_1(f, xs, ys, env)
            } else {
                each2_0(f, xs, ys, env)
            }
        }
        n => {
            let mut args = Vec::with_capacity(n);
            for i in 0..n {
                args.push(env.pop(ArrayArg(i + 1))?);
            }
            if output {
                eachn_1(f, args, env)
            } else {
                eachn_0(f, args, env)
            }
        }
    }
}

fn each1_1(f: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    let mut new_values = Vec::with_capacity(xs.flat_len());
    let mut new_shape = Shape::from(xs.shape());
    let mut old_values = xs.into_flat_values();
    for val in old_values.by_ref() {
        env.push(val);
        let broke = env.call_catch_break(f.clone())?;
        new_values.push(env.pop("each's function result")?);
        if broke {
            for row in old_values {
                new_values.push(row);
            }
            break;
        }
    }
    let mut eached = Value::from_row_values(new_values, env)?;
    new_shape.extend_from_slice(&eached.shape()[1..]);
    *eached.shape_mut() = new_shape;
    env.push(eached);
    Ok(())
}

fn each1_0(f: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    let values = xs.into_flat_values();
    for val in values {
        env.push(val);
        if env.call_catch_break(f.clone())? {
            break;
        }
    }
    Ok(())
}

fn each2_1(f: Value, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let xs_shape = xs.shape().to_vec();
    let ys_shape = ys.shape().to_vec();
    let xs_values: Vec<_> = xs.into_flat_values().collect();
    let ys_values: Vec<_> = ys.into_flat_values().collect();
    let (mut shape, values) = bin_pervade_generic(
        &xs_shape,
        xs_values,
        &ys_shape,
        ys_values,
        env,
        |x, y, env| {
            env.push(y);
            env.push(x);
            env.call_error_on_break(f.clone(), "break is not allowed in multi-argument each")?;
            env.pop("each's function result")
        },
    )?;
    let mut eached = Value::from_row_values(values, env)?;
    shape.extend_from_slice(&eached.shape()[1..]);
    *eached.shape_mut() = shape;
    env.push(eached);
    Ok(())
}

fn each2_0(f: Value, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    let xs_shape = xs.shape().to_vec();
    let ys_shape = ys.shape().to_vec();
    let xs_values: Vec<_> = xs.into_flat_values().collect();
    let ys_values: Vec<_> = ys.into_flat_values().collect();
    bin_pervade_generic(
        &xs_shape,
        xs_values,
        &ys_shape,
        ys_values,
        env,
        |x, y, env| {
            env.push(y);
            env.push(x);
            env.call_error_on_break(f.clone(), "break is not allowed multi-argument in each")?;
            Ok(())
        },
    )?;
    Ok(())
}

fn eachn_1(f: Value, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
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
    let elem_count = args[0].flat_len();
    let mut arg_elems: Vec<_> = args.into_iter().map(|v| v.into_flat_values()).collect();
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

fn eachn_0(f: Value, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
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
    let elem_count = args[0].flat_len();
    let mut arg_elems: Vec<_> = args.into_iter().map(|v| v.into_flat_values()).collect();
    for _ in 0..elem_count {
        for arg in arg_elems.iter_mut().rev() {
            env.push(arg.next().unwrap());
        }
        env.call_error_on_break(f.clone(), "break is not allowed in multi-argument each")?;
    }
    Ok(())
}

pub fn rows(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(FunctionArg(1))?;
    let sig = f.signature();
    let output = match sig.outputs {
        0 => false,
        1 => true,
        n => {
            return Err(env.error(format!(
                "Rows's function must return 0 or 1 values, but it returns {}",
                n
            )))
        }
    };
    match sig.args {
        0 => Ok(()),
        1 => {
            let xs = env.pop(ArrayArg(1))?;
            if output {
                rows1_1(f, xs, env)
            } else {
                rows1_0(f, xs, env)
            }
        }
        2 => {
            let xs = env.pop(ArrayArg(1))?;
            let ys = env.pop(ArrayArg(2))?;
            if output {
                rows2_1(f, xs, ys, env)
            } else {
                rows2_0(f, xs, ys, env)
            }
        }
        n => {
            let mut args = Vec::with_capacity(n);
            for i in 0..n {
                args.push(env.pop(ArrayArg(i + 1))?);
            }
            if output {
                rowsn_1(f, args, env)
            } else {
                rowsn_0(f, args, env)
            }
        }
    }
}

fn rows1_1(f: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    let mut new_rows = Value::builder(xs.row_count());
    let mut old_rows = xs.into_rows();
    for row in old_rows.by_ref() {
        env.push(row);
        let broke = env.call_catch_break(f.clone())?;
        new_rows.add_row(env.pop("rows' function result")?, &env)?;
        if broke {
            for row in old_rows {
                new_rows.add_row(row, &env)?;
            }
            break;
        }
    }
    env.push(new_rows.finish());
    Ok(())
}

fn rows1_0(f: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    for row in xs.into_rows() {
        env.push(row);
        let broke = env.call_catch_break(f.clone())?;
        if broke {
            break;
        }
    }
    Ok(())
}

fn rows2_1(f: Value, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    if xs.row_count() != ys.row_count() {
        return Err(env.error(format!(
            "Cannot rows arrays with different number of rows {} and {}",
            xs.row_count(),
            ys.row_count()
        )));
    }
    let mut new_rows = Vec::with_capacity(xs.row_count());
    let x_rows = xs.into_rows();
    let y_rows = ys.into_rows();
    for (x, y) in x_rows.into_iter().zip(y_rows) {
        env.push(y);
        env.push(x);
        env.call_error_on_break(f.clone(), "break is not allowed in multi-argument rows")?;
        new_rows.push(env.pop("rows's function result")?);
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}

fn rows2_0(f: Value, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    if xs.row_count() != ys.row_count() {
        return Err(env.error(format!(
            "Cannot rows arrays with different number of rows {} and {}",
            xs.row_count(),
            ys.row_count()
        )));
    }
    let x_rows = xs.into_rows();
    let y_rows = ys.into_rows();
    for (x, y) in x_rows.into_iter().zip(y_rows) {
        env.push(y);
        env.push(x);
        env.call_error_on_break(f.clone(), "break is not allowed in multi-argument rows")?;
    }
    Ok(())
}

fn rowsn_1(f: Value, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
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
    let mut new_values = Vec::new();
    for _ in 0..row_count {
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

fn rowsn_0(f: Value, args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
    let row_count = args[0].row_count();
    let mut arg_elems: Vec<_> = args.into_iter().map(|v| v.into_rows()).collect();
    for _ in 0..row_count {
        for arg in arg_elems.iter_mut().rev() {
            env.push(arg.next().unwrap());
        }
        env.call_error_on_break(f.clone(), "break is not allowed in multi-argument each")?;
    }
    Ok(())
}

pub fn distribute(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(FunctionArg(1))?;
    let sig = f.signature();
    if sig.outputs != 1 {
        return Err(env.error(format!(
            "Level's function must return 1 value, but it returns {}",
            sig.outputs
        )));
    }
    match sig.args {
        n @ (0 | 1) => {
            return Err(env.error(format!(
                "Level's function must take at least 2 arguments, \
                but it takes {n}"
            )))
        }
        2 => {
            let a = env.pop(ArrayArg(1))?;
            let xs = env.pop(ArrayArg(2))?;
            if xs.row_count() == 0 {
                env.push(xs);
                return Ok(());
            }
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for x in xs.into_rows() {
                env.push(x);
                env.push(a.clone());
                env.call_error_on_break(f.clone(), "break is not allowed in level")?;
                new_rows.push(env.pop("level's function result")?);
            }
            env.push(Value::from_row_values(new_rows, env)?);
        }
        3 => {
            let a = env.pop(ArrayArg(1))?;
            let b = env.pop(ArrayArg(2))?;
            let xs = env.pop(ArrayArg(3))?;
            if xs.row_count() == 0 {
                env.push(xs);
                return Ok(());
            }
            let mut new_rows = Vec::with_capacity(xs.row_count());
            for x in xs.into_rows() {
                env.push(x);
                env.push(b.clone());
                env.push(a.clone());
                env.call_error_on_break(f.clone(), "break is not allowed in level")?;
                new_rows.push(env.pop("level's function result")?);
            }
            env.push(Value::from_row_values(new_rows, env)?);
        }
        n => {
            let mut args = Vec::with_capacity(n - 1);
            for i in 0..n - 1 {
                args.push(env.pop(ArrayArg(i + 1))?);
            }
            let xs = env.pop(ArrayArg(n))?;
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

pub fn level(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let ns = rank_list("Level", env)?;
    if let Some((end, init)) = ns.split_last() {
        if end.is_some_and(|n| n == -1) && !init.is_empty() && init.iter().all(Option::is_none) {
            return distribute(env);
        }
    }
    let f = env.pop(FunctionArg(2))?;
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
            let xs = env.pop(ArrayArg(1))?;
            if xs.rank() == 0 {
                env.push(xs);
                return Ok(());
            }
            match n {
                Some(0) => return each1_1(f, xs, env),
                Some(-1) => return rows1_1(f, xs, env),
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
            let xs = env.pop(ArrayArg(1))?;
            let ys = env.pop(ArrayArg(2))?;
            if xs.rank() == 0 && ys.rank() == 0 {
                env.push(xs);
                env.push(ys);
                return Ok(());
            }
            match (xn, yn) {
                (Some(0), Some(0)) => return each2_1(f, xs, ys, env),
                (Some(-1), Some(-1)) => return rows2_1(f, xs, ys, env),
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
                let arg = env.pop(ArrayArg(i + 1))?;
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

fn monadic_level_recursive(f: Value, value: Value, n: usize, env: &mut Uiua) -> UiuaResult<Value> {
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
    f: Value,
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
    f: Value,
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
