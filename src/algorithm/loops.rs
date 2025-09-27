//! Algorithms for looping modifiers

use std::mem::size_of;

use crate::{
    algorithm::{fixed_rows, get_ops, pervade::pervade_dim, FixedRowsData},
    array::Array,
    value::Value,
    Ops, Primitive, Shape, SigNode, Signature, Uiua, UiuaResult,
};

use super::{multi_output, validate_size_impl};

pub fn flip<A, B, C>(f: impl Fn(A, B) -> C + Copy) -> impl Fn(B, A) -> C + Copy {
    move |b, a| f(a, b)
}

pub fn repeat(ops: Ops, with_inverse: bool, count_convergence: bool, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let (f, inv) = if with_inverse {
        let [f, inv] = get_ops(ops, env)?;
        (f, Some(inv))
    } else {
        let [f] = get_ops(ops, env)?;
        (f, None)
    };
    if count_convergence {
        let count = repeat_impl(f, inv, f64::INFINITY, env)?;
        env.push(count as f64);
        return Ok(());
    }
    let n = env.pop("repetition count")?;
    env.require_height(f.sig.args())?;
    if n.rank() == 0 {
        // Scalar repeat
        let n = rep_count(n, env)?;
        repeat_impl(f, inv, n.data[0], env)?;
        Ok(())
    } else {
        // Array
        let sig = f.sig;
        if sig.args() != sig.outputs() {
            return Err(env.error(format!(
                "{} with a non-scalar repetition count \
                must use a function with the same number \
                of arguments and outputs, but its signature \
                is {sig}",
                Primitive::Repeat.format()
            )));
        }
        // Collect arguments
        let mut args = Vec::with_capacity(sig.args() + 1);
        let mut new_shape = n.shape.clone();
        let mut true_shape = Shape::SCALAR;
        let n_shape = n.shape.clone();
        args.push(n);
        for i in 0..sig.args() {
            let arg = env.pop(i + 1)?;
            if arg.rank() > 0
                && !n_shape.is_empty()
                && !(arg.shape.iter().skip(1))
                    .zip(n_shape.iter().skip(1))
                    .all(|(a, b)| *a == 1 || *b == 1 || a == b)
            {
                return Err(env.error(format!(
                    "Cannot {} with counts of shape {n_shape} \
                    when argument {} has shape {}",
                    Primitive::Repeat.format(),
                    i + 1,
                    arg.shape
                )));
            }
            for (a, &b) in new_shape.iter_mut().zip(&arg.shape) {
                true_shape.push(pervade_dim(*a, b));
                *a = (*a).max(b);
            }
            args.push(arg);
        }
        args[1..].reverse();
        rep_recur(f, inv, args, env)
    }
}

fn rep_recur(f: SigNode, inv: Option<SigNode>, mut args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
    if args[0].rank() == 0 {
        // Scalar repeat
        let n = rep_count(args.remove(0), env)?;
        for arg in args {
            env.push(arg);
        }
        repeat_impl(f, inv, n.data[0], env)?;
        return Ok(());
    }
    let FixedRowsData {
        mut rows,
        row_count,
        is_empty,
        ..
    } = fixed_rows(Primitive::Repeat.format(), f.sig.outputs(), args, env)?;
    let mut new_values = multi_output(f.sig.outputs(), Vec::new());
    for _ in 0..row_count {
        let args: Vec<_> = rows
            .iter_mut()
            .map(|arg| match arg {
                Ok(rows) => rows.next().unwrap(),
                Err(row) => row.clone(),
            })
            .collect();
        rep_recur(f.clone(), inv.clone(), args, env)?;
        for i in 0..f.sig.outputs() {
            new_values[i].push(env.pop("rows's function result")?);
        }
    }
    for new_values in new_values.into_iter().rev() {
        let mut rowsed = Value::from_row_values(new_values, env)?;
        if is_empty {
            rowsed.pop_row();
        }
        rowsed.validate();
        env.push(rowsed);
    }
    Ok(())
}

fn rep_count(value: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    Ok(match value {
        Value::Num(n) => n,
        Value::Byte(n) => n.convert(),
        val => {
            return Err(env.error(format!(
                "Repetitions must be a scalar or list of \
                natural numbers or infinity, \
                but it is {}",
                val.type_name_plural()
            )))
        }
    })
}

fn repeat_impl(f: SigNode, inv: Option<SigNode>, n: f64, env: &mut Uiua) -> UiuaResult<u64> {
    let sig = f.sig;
    let (f, n) = if n >= 0.0 {
        (f, n)
    } else {
        let f = inv.ok_or_else(|| env.error("No inverse found"))?;
        (f, -n)
    };
    let excess_count = sig.outputs().saturating_sub(sig.args());
    let preserve_count = sig.args().saturating_sub(sig.outputs());
    let preserved = env.copy_n_down(preserve_count, f.sig.args())?;
    let mut excess_rows = vec![Vec::new(); excess_count];
    let mut convergence_count = 0;
    env.without_fill(|env| -> UiuaResult {
        if n.is_infinite() {
            // Converging repeat
            if sig.args() == 0 {
                return Err(env.error(format!(
                    "Converging {}'s function must have at least 1 argument",
                    Primitive::Repeat.format()
                )));
            }
            let mut prev = env.pop(1)?;
            env.push(prev.clone());
            loop {
                if preserve_count > 0 {
                    env.insert_stack(sig.outputs(), preserved.iter().cloned())?;
                }
                env.exec(f.clone())?;
                for (i, row) in env
                    .remove_n(excess_count, sig.args() + excess_count)?
                    .enumerate()
                {
                    excess_rows[i].push(row);
                }
                let next = env.pop("converging function result")?;
                let converged = next == prev;
                if converged {
                    env.push(next);
                    break;
                } else {
                    env.push(next.clone());
                    prev = next;
                }
                convergence_count += 1;
            }
        } else {
            // Normal repeat
            if n.fract() != 0.0 {
                return Err(env.error("Repetitions must be an integer or infinity"));
            }
            let n = n as usize;
            if sig.outputs() > sig.args() {
                let delta = sig.outputs() - sig.args();
                if validate_size_impl(size_of::<Value>(), [n, delta]).is_err() {
                    return Err(env.error(format!(
                        "{} would create too many values on the stack",
                        Primitive::Repeat.format()
                    )));
                }
            }
            for _ in 0..n {
                if preserve_count > 0 {
                    env.insert_stack(sig.outputs(), preserved.iter().cloned())?;
                }
                env.exec(f.clone())?;
                for (i, row) in env
                    .remove_n(excess_count, sig.args() + excess_count)?
                    .enumerate()
                {
                    excess_rows[i].push(row);
                }
            }
        }
        Ok(())
    })?;
    // Remove preserved/excess values
    if excess_count > 0 {
        _ = env.remove_n(sig.args(), sig.args())?;
    } else if preserve_count > 0 {
        _ = env.remove_n(preserve_count, sig.args())?;
    }
    // Collect excess values
    for rows in excess_rows.into_iter().rev() {
        env.respect_execution_limit()?;
        let new_val = Value::from_row_values(rows, env)?;
        env.push(new_val);
    }
    Ok(convergence_count)
}

pub fn do_(ops: Ops, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [body, cond] = get_ops(ops, env)?;
    let cond_sig_err = if cond.sig.outputs() == 0 {
        Some(env.error(format!(
            "Do's condition function must return at least 1 value, \
            but its signature is {}",
            cond.sig
        )))
    } else {
        None
    };
    let copy_count = (cond.sig.args()).saturating_sub(cond.sig.outputs().saturating_sub(1));
    let cond_sub_sig = Signature::new(
        cond.sig.args(),
        (cond.sig.outputs() + copy_count).saturating_sub(1),
    );
    let comp_sig = body.sig.compose(cond_sub_sig);
    let excess_count = comp_sig.outputs().saturating_sub(comp_sig.args());
    let mut excess_rows = vec![Vec::new(); excess_count];
    let preserve_count = comp_sig.args().saturating_sub(comp_sig.outputs());
    let preserved = env.copy_n_down(preserve_count, comp_sig.args())?;
    loop {
        // Make sure there are enough values
        if env.stack().len() < copy_count {
            // Pop until it fails
            for i in 0..copy_count {
                env.pop(i + 1)?;
            }
        }
        // Copy necessary condition args
        env.dup_values(copy_count, copy_count)?;
        // Call condition
        env.exec(cond.clone())?;
        // Break if condition is false
        if let Some(err) = cond_sig_err {
            return Err(err);
        }
        let cond = (env.pop("do condition")?).as_bool(env, "Do condition must be a boolean")?;
        if !cond {
            break;
        }
        // Call body
        if preserve_count > 0 {
            env.insert_stack(comp_sig.outputs(), preserved.iter().cloned())?;
        }
        env.exec(body.clone())?;
        for (i, row) in env
            .remove_n(excess_count, comp_sig.args() + excess_count)?
            .enumerate()
        {
            excess_rows[i].push(row);
        }
    }
    // Remove preserved/excess values
    if excess_count > 0 {
        _ = env.remove_n(comp_sig.args(), comp_sig.args())?;
    } else if preserve_count > 0 {
        _ = env.remove_n(preserve_count, comp_sig.args())?;
    }
    // Collect excess values
    for rows in excess_rows.into_iter().rev() {
        let new_val = Value::from_row_values(rows, env)?;
        env.push(new_val);
    }
    Ok(())
}
