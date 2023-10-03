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
    let a = env.pop(ArrayArg(1))?;
    let b = env.pop(ArrayArg(2))?;

    match g.signature().args {
        0 | 1 => {
            env.push(b.clone());
            env.call(g)?;
        }
        2 => {
            env.push(b.clone());
            env.push(a.clone());
            env.call(g)?;
        }
        n => {
            return Err(env.error(format!(
                "Fork's functions may not take more than 2 arguments, \
                but function 2 one takes {n}"
            )))
        }
    }

    match f.signature().args {
        0 | 1 => {
            env.push(a);
            env.call(f)?;
        }
        2 => {
            env.push(b);
            env.push(a);
            env.call(f)?;
        }
        n => {
            return Err(env.error(format!(
                "Fork's functions may not take more than 2 arguments, \
                but function 1 one takes {n}"
            )))
        }
    }

    Ok(())
}

pub fn trident(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(FunctionArg(1))?;
    let g = env.pop(FunctionArg(2))?;
    let h = env.pop(FunctionArg(3))?;
    let a = env.pop(ArrayArg(1))?;
    let b = env.pop(ArrayArg(2))?;
    let c = env.pop(ArrayArg(3))?;

    match h.signature().args {
        0 | 1 => {
            env.push(c.clone());
            env.call(h)?;
        }
        2 => {
            env.push(c.clone());
            env.push(b.clone());
            env.call(h)?;
        }
        3 => {
            env.push(c.clone());
            env.push(b.clone());
            env.push(a.clone());
            env.call(h)?;
        }
        n => {
            return Err(env.error(format!(
                "Trident's functions may not take more than 3 arguments, \
                but the third function {h} takes {n}"
            )))
        }
    }

    match g.signature().args {
        0 | 1 => {
            env.push(b.clone());
            env.call(g)?;
        }
        2 => {
            env.push(c.clone());
            env.push(a.clone());
            env.call(g)?;
        }
        3 => {
            env.push(c.clone());
            env.push(b.clone());
            env.push(a.clone());
            env.call(g)?;
        }
        n => {
            return Err(env.error(format!(
                "Trident's functions may not take more than 3 arguments, \
                but the second function {g} takes {n}"
            )))
        }
    }

    match f.signature().args {
        0 | 1 => {
            env.push(a);
            env.call(f)?;
        }
        2 => {
            env.push(b);
            env.push(a);
            env.call(f)?;
        }
        3 => {
            env.push(c);
            env.push(b);
            env.push(a);
            env.call(f)?;
        }
        n => {
            return Err(env.error(format!(
                "Trident's functions may not take more than 3 arguments, \
                but the first function {f} takes {n}"
            )))
        }
    }

    Ok(())
}

pub fn lives(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(FunctionArg(1))?;
    let g = env.pop(FunctionArg(2))?;
    let f = f
        .into_func_array()
        .map_err(|val| {
            env.error(format!(
                "Lives' first function must be a function, but is has type {}",
                val.type_name()
            ))
        })
        .and_then(|f| {
            f.into_scalar().map_err(|arr| {
                env.error(format!(
                    "Lives' first function must be a scalar, but is has rank {}",
                    arr.rank()
                ))
            })
        })?;
    let g = g
        .into_func_array()
        .map_err(|val| {
            env.error(format!(
                "Lives' second function must be a function, but is has type {}",
                val.type_name()
            ))
        })
        .and_then(|f| {
            f.into_scalar().map_err(|arr| {
                env.error(format!(
                    "Lives' second function must be a scalar, but is has rank {}",
                    arr.rank()
                ))
            })
        })?;
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
