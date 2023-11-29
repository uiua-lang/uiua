//! Algorithms for forking modifiers

use std::sync::Arc;

use crate::{value::Value, Function, Shape, Signature, Uiua, UiuaResult};

use super::multi_output;

pub fn both(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    match f.signature().args {
        0 => {
            env.call(f.clone())?;
            env.call(f)?;
        }
        1 => {
            let a = env.pop(1)?;
            let b = env.pop(2)?;
            env.push(b);
            env.call(f.clone())?;
            env.push(a);
            env.call(f)?;
        }
        n => {
            let mut a = Vec::with_capacity(n);
            let mut b = Vec::with_capacity(n);
            for i in 0..n {
                a.push(env.pop(i + 1)?);
            }
            for i in 0..n {
                b.push(env.pop(n + i + 1)?);
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
    let f = env.pop_function()?;
    let g = env.pop_function()?;
    let arg_count = f.signature().args.max(g.signature().args);
    let mut args = Vec::with_capacity(arg_count);
    for i in 0..arg_count {
        args.push(env.pop(i + 1)?);
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
    let f = env.pop_function()?;
    let g = env.pop_function()?;
    let f_sig = f.signature();
    let mut f_args = Vec::with_capacity(f_sig.args);
    for i in 0..f_sig.args {
        f_args.push(env.pop(i + 1)?);
    }
    env.call(g)?;
    for arg in f_args.into_iter().rev() {
        env.push(arg);
    }
    env.call(f)?;
    Ok(())
}

pub fn all(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    let g = env.pop_function()?;
    let f_sig = f.signature();
    let g_sig = g.signature();
    // Call g
    env.call(g)?;
    // Determine arg counts
    let lower_arg_count = g_sig.outputs / f_sig.args.saturating_sub(1).max(1);
    let upper_arg_count = f_sig.args.saturating_sub(1) * lower_arg_count;
    let mut lower_args = Vec::with_capacity(lower_arg_count);
    let mut upper_args = Vec::with_capacity(upper_arg_count);
    for i in 0..upper_arg_count {
        upper_args.push(env.pop(lower_arg_count + i + 1)?);
    }
    for i in 0..lower_arg_count {
        lower_args.push(env.pop(i + 1)?);
    }
    let mut lower_args = lower_args.into_iter().rev();
    let mut upper_args = upper_args.into_iter().rev();
    // Call f
    for _ in 0..lower_arg_count {
        env.push(lower_args.next().unwrap());
        for _ in 0..f_sig.args.saturating_sub(1) {
            env.push(upper_args.next().unwrap());
        }
        env.call(f.clone())?;
    }
    Ok(())
}

pub fn switch(count: usize, sig: Signature, env: &mut Uiua) -> UiuaResult {
    // Get selector
    let selector = env
        .pop("switch index")?
        .as_natural_array(env, "Switch index must be an array of naturals")?;
    if let Some(i) = selector.data.iter().find(|&&i| i >= count) {
        return Err(env.error(format!(
            "Switch index {i} is out of bounds for switch of size {count}"
        )));
    }
    // Switch
    if selector.rank() == 0 {
        // Scalar
        let i = selector.data[0];
        // Get function
        let Some(f) = env
            .function_stack
            .drain(env.function_stack.len() - count..)
            .nth(i)
        else {
            return Err(env.error(
                "Function stack was empty when getting switch function. \
                This is a bug in the interpreter.",
            ));
        };
        // Discard unused arguments
        let discard_start = env.stack.len().saturating_sub(sig.args);
        if discard_start > env.stack.len() {
            return Err(env.error("Stack was empty when discarding excess switch arguments."));
        }
        let discard_end =
            discard_start + sig.args - f.signature().args - (sig.outputs - f.signature().outputs);
        if discard_end > env.stack.len() {
            return Err(env.error("Stack was empty when discarding excess switch arguments."));
        }
        env.stack.drain(discard_start..discard_end);
        env.call(f)
    } else {
        // Array
        // Collect arguments
        let mut args_rows: Vec<_> = Vec::with_capacity(sig.args);
        for i in 0..sig.args {
            let arg = env.pop(i + 1)?;
            if !arg.shape().starts_with(selector.shape()) {
                return Err(env.error(format!(
                    "The function's select's shape {} is not compatible \
                    with the argument {}'s shape {}",
                    selector.format_shape(),
                    i + 1,
                    arg.format_shape(),
                )));
            }
            let row_shape = Shape::from(&arg.shape()[selector.rank()..]);
            args_rows.push(arg.into_row_shaped_slices(row_shape));
        }
        args_rows.reverse();
        // Collect functions
        let functions: Vec<Arc<Function>> = env
            .function_stack
            .drain(env.function_stack.len() - count..)
            .collect();
        let mut outputs = multi_output(sig.outputs, Vec::new());
        for elem in selector.data {
            let f = &functions[elem];
            for (i, arg) in args_rows.iter_mut().rev().enumerate().rev() {
                let arg = arg.next().unwrap();
                if i < f.signature().args {
                    env.push(arg);
                }
            }
            env.call(f.clone())?;
            for i in 0..sig.outputs {
                outputs[i].push(env.pop("switch output")?);
            }
        }
        for output in outputs.into_iter().rev() {
            let mut new_value = Value::from_row_values(output, env)?;
            let mut new_shape = selector.shape.clone();
            new_shape.extend_from_slice(&new_value.shape()[1..]);
            *new_value.shape_mut() = new_shape;
            env.push(new_value);
        }
        Ok(())
    }
}
