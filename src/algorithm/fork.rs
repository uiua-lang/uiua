//! Algorithms for forking modifiers

use crate::{
    run::{ArrayArg, FunctionArg},
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
    let condition = env
        .pop(ArrayArg(1))?
        .as_nat(env, "If's condition must be a natural number")?;
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
            for arg in args.into_iter().take(if_true_sig.args) {
                env.push(arg);
            }
            env.call(if_true)?;
        } else {
            for arg in args.into_iter().take(if_false_sig.args) {
                env.push(arg);
            }
            env.call(if_false)?;
        }
    }
    Ok(())
}
