use crate::{
    run::{ArrayArg, FunctionArg},
    Uiua, UiuaResult,
};

pub fn fork(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(FunctionArg(1))?;
    let g = env.pop(FunctionArg(2))?;
    let a = env.pop(ArrayArg(1))?;
    let b = env.pop(ArrayArg(2))?;

    let f_args = f.signature().map(|sig| sig.args).unwrap_or(2);
    let g_args = g.signature().map(|sig| sig.args).unwrap_or(2);

    match g_args {
        0 | 1 => {
            env.push(b.clone());
            env.push(g);
            env.call()?;
        }
        2 => {
            env.push(b.clone());
            env.push(a.clone());
            env.push(g);
            env.call()?;
        }
        n => {
            return Err(env.error(format!(
                "Fork's functions may not take more than 2 arguments, \
                but function 2 one takes {n}"
            )))
        }
    }

    match f_args {
        0 | 1 => {
            env.push(a);
            env.push(f);
            env.call()?;
        }
        2 => {
            env.push(b);
            env.push(a);
            env.push(f);
            env.call()?;
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

    let f_args = f.signature().map(|sig| sig.args).unwrap_or(3);
    let g_args = g.signature().map(|sig| sig.args).unwrap_or(3);
    let h_args = h.signature().map(|sig| sig.args).unwrap_or(3);

    match h_args {
        0 | 1 => {
            env.push(c.clone());
            env.push(h);
            env.call()?;
        }
        2 => {
            env.push(a.clone());
            env.push(c.clone());
            env.push(h);
            env.call()?;
        }
        3 => {
            env.push(b.clone());
            env.push(a.clone());
            env.push(c.clone());
            env.push(h);
            env.call()?;
        }
        n => {
            return Err(env.error(format!(
                "Trident's functions may not take more than 3 arguments, \
                but function 3 takes {n}"
            )))
        }
    }

    match g_args {
        0 | 1 => {
            env.push(b.clone());
            env.push(g);
            env.call()?;
        }
        2 => {
            env.push(c.clone());
            env.push(b.clone());
            env.push(g);
            env.call()?;
        }
        3 => {
            env.push(a.clone());
            env.push(c.clone());
            env.push(b.clone());
            env.push(g);
            env.call()?;
        }
        n => {
            return Err(env.error(format!(
                "Trident's functions may not take more than 3 arguments, \
                but function 2 takes {n}"
            )))
        }
    }

    match f_args {
        0 | 1 => {
            env.push(a);
            env.push(f);
            env.call()?;
        }
        2 => {
            env.push(b);
            env.push(a);
            env.push(f);
            env.call()?;
        }
        3 => {
            env.push(c);
            env.push(b);
            env.push(a);
            env.push(f);
            env.call()?;
        }
        n => {
            return Err(env.error(format!(
                "Trident's functions may not take more than 3 arguments, \
                but function 1 takes {n}"
            )))
        }
    }

    Ok(())
}
