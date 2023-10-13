//! Algorithms for forking modifiers

use crate::{
    run::{ArrayArg, FunctionArg},
    value::Value,
    Uiua, UiuaResult,
};

pub fn restack(env: &mut Uiua) -> UiuaResult {
    let indices = env
        .pop(1)?
        .as_naturals(env, "Restack indices must be a list of natural numbers")?;
    if indices.is_empty() {
        return Ok(());
    }
    let max_index = *indices.iter().max().unwrap();
    let mut values = Vec::with_capacity(max_index + 1);
    for i in 0..=max_index {
        values.push(env.pop(i + 2)?);
    }
    for index in indices.into_iter().rev() {
        env.push(values[index].clone());
    }
    Ok(())
}

pub fn both(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(FunctionArg(1))?;
    match f.signature().args {
        0 => {
            env.call(f.clone())?;
            env.call(f)?;
        }
        1 => {
            let a = env.pop(ArrayArg(1))?;
            let b = env.pop(ArrayArg(2))?;
            env.push(b);
            env.call(f.clone())?;
            env.push(a);
            env.call(f)?;
        }
        n => {
            let mut a = Vec::with_capacity(n);
            let mut b = Vec::with_capacity(n);
            for i in 0..n {
                a.push(env.pop(ArrayArg(i + 1))?);
            }
            for i in 0..n {
                b.push(env.pop(ArrayArg(n + i + 1))?);
            }
            for b in b.into_iter().rev() {
                env.push(b);
            }
            env.call(f.clone())?;
            for a in a.into_iter().rev() {
                env.push(a);
            }
            env.call(f)?;
        }
    }

    Ok(())
}

pub fn fork(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(FunctionArg(1))?;
    let g = env.pop(FunctionArg(2))?;
    let arg_count = f.signature().args.max(g.signature().args);
    let mut args = Vec::with_capacity(arg_count);
    for i in 0..arg_count {
        args.push(env.pop(ArrayArg(i + 1))?);
    }
    for arg in args.iter().take(g.signature().args).rev() {
        env.push(arg.clone());
    }
    env.call(g)?;
    for arg in args.into_iter().take(f.signature().args).rev() {
        env.push(arg);
    }
    env.call(f)?;
    Ok(())
}

pub fn bracket(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(FunctionArg(1))?;
    let g = env.pop(FunctionArg(2))?;
    let f_sig = f.signature();
    let mut f_args = Vec::with_capacity(f_sig.args);
    for i in 0..f_sig.args {
        f_args.push(env.pop(ArrayArg(i + 1))?);
    }
    env.call(g)?;
    for arg in f_args.into_iter().rev() {
        env.push(arg);
    }
    env.call(f)?;
    Ok(())
}

pub fn iff(env: &mut Uiua) -> UiuaResult {
    let if_true = env.pop(FunctionArg(1))?;
    let if_false = env.pop(FunctionArg(2))?;
    let condition = env.pop(ArrayArg(1))?;
    if let Ok(condition) = condition.as_nat(env, "") {
        if condition > 1 {
            return Err(env.error(format!(
                "If's condition must be 0 or 1, but it is {}",
                condition
            )));
        }
        let if_true_sig = if_true.signature();
        let if_false_sig = if_false.signature();
        if if_true_sig.args == if_false_sig.args {
            if condition == 1 {
                env.call(if_true)?;
            } else {
                env.call(if_false)?;
            }
        } else {
            let arg_count = if_true_sig.args.max(if_false_sig.args);
            let mut args = Vec::with_capacity(arg_count);
            for i in 0..arg_count {
                args.push(env.pop(ArrayArg(i + 1))?);
            }
            if condition == 1 {
                for arg in args.into_iter().take(if_true_sig.args).rev() {
                    env.push(arg);
                }
                env.call(if_true)?;
            } else {
                for arg in args.into_iter().take(if_false_sig.args).rev() {
                    env.push(arg);
                }
                env.call(if_false)?;
            }
        }
    } else {
        let condition = condition.as_naturals(
            env,
            "If's condition must be a natural number or list of natural numbers",
        )?;
        if !condition.iter().all(|&x| x <= 1) {
            return Err(env.error(format!(
                "If's condition must be all 0s or 1s, but it is {:?}",
                condition
            )));
        }
        let if_true_sig = if_true.signature();
        let if_false_sig = if_false.signature();
        if if_true_sig.outputs != 1 {
            return Err(env.error(format!(
                "If's true branch must return 1 value, but it returns {}",
                if_true_sig.outputs
            )));
        }
        if if_false_sig.outputs != 1 {
            return Err(env.error(format!(
                "If's false branch must return 1 value, but it returns {}",
                if_false_sig.outputs
            )));
        }
        let arg_count = if_true_sig.args.max(if_false_sig.args);
        match arg_count {
            1 => {
                let xs = env.pop(ArrayArg(2))?;
                if xs.row_count() != condition.len() {
                    return Err(env.error(format!(
                        "If's condition must have the same number of rows as its argument, \
                        but it has {} rows and its argument has {} rows",
                        condition.len(),
                        xs.row_count()
                    )));
                }
                let mut new_rows = Vec::with_capacity(condition.len());
                for (con, x) in condition.into_iter().zip(xs.into_rows()) {
                    env.push(x);
                    if con == 1 {
                        env.call(if_true.clone())?;
                        new_rows.push(env.pop("if's true branch result")?);
                    } else {
                        env.call(if_false.clone())?;
                        new_rows.push(env.pop("if's false branch result")?);
                    }
                }
                env.push(Value::from_row_values(new_rows, env)?);
            }
            2 => {
                let a = env.pop(ArrayArg(2))?;
                let b = env.pop(ArrayArg(3))?;
                if a.row_count() != condition.len() || b.row_count() != condition.len() {
                    return Err(env.error(format!(
                        "If's condition must have the same number of rows as its arguments, \
                        but it has {} rows and its arguments have {} and {} rows",
                        condition.len(),
                        a.row_count(),
                        b.row_count()
                    )));
                }
                let mut new_rows = Vec::with_capacity(condition.len());
                for (con, (a, b)) in condition.into_iter().zip(a.into_rows().zip(b.into_rows())) {
                    if con == 1 {
                        if if_true_sig.args == 2 {
                            env.push(b);
                        }
                        env.push(a);
                        env.call(if_true.clone())?;
                        new_rows.push(env.pop("if's true branch result")?);
                    } else {
                        if if_false_sig.args == 2 {
                            env.push(b);
                        }
                        env.push(a);
                        env.call(if_false.clone())?;
                        new_rows.push(env.pop("if's false branch result")?);
                    }
                }
                env.push(Value::from_row_values(new_rows, env)?);
            }
            n => {
                return Err(env.error(format!(
                    "The maximum of iterating if's function's arguments must be 1 or 2, \
                    but it is {n}"
                )));
            }
        }
    }

    Ok(())
}
