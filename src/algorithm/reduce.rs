//! Algorithms for reducing modifiers

use core::f64;
use std::{convert::identity, mem::take};

use ecow::{EcoVec, eco_vec};

use crate::{
    Array, ArrayValue, Complex, ImplPrimitive, Node, Ops, Primitive, Shape, SigNode, Signature,
    Uiua, UiuaResult, Value,
    algorithm::{get_ops, loops::flip, pervade::*, validate_size},
    check::{nodes_clean_sig, nodes_sig},
    cowslice::cowslice,
};

use super::{FillContext, FixedRowsData, fixed_rows};

pub fn reduce(ops: Ops, depth: usize, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [f] = get_ops(ops, env)?;
    reduce_impl(f, depth, env)
}

pub(crate) fn reduce_impl(f: SigNode, depth: usize, env: &mut Uiua) -> UiuaResult {
    if f.sig.args() < 2 {
        return Err(env.error(format!(
            "{}'s function must have at least 2 arguments, \
            but its signature is {}",
            Primitive::Reduce.format(),
            f.sig
        )));
    }
    let xs = env.pop(1)?;
    match (f.node.as_flipped_primitive(), xs) {
        (Some((Primitive::Join, false)), mut xs)
            if env.value_fill().is_none() && env.value_fill().is_none() =>
        {
            let depth = depth.min(xs.rank());
            if xs.rank() - depth < 2 {
                env.push(xs);
                return Ok(());
            }
            let shape = &xs.shape;
            let mut new_shape = Shape::with_capacity(xs.rank() - 1);
            new_shape.extend_from_slice(&shape[..depth]);
            new_shape.push(shape[depth] * shape[depth + 1]);
            new_shape.extend_from_slice(&shape[depth + 2..]);
            xs.shape = new_shape;
            xs.meta.take_sorted_flags();
            xs.validate();
            env.push(xs);
        }
        (Some((prim, flipped)), Value::Num(nums)) => {
            if let Err(nums) = reduce_nums(prim, flipped, nums, depth, env)? {
                return generic_reduce(f, Value::Num(nums), depth, env);
            }
        }
        (Some((prim, flipped)), Value::Complex(nums)) => {
            if let Err(nums) = reduce_coms(prim, flipped, nums, depth, env)? {
                return generic_reduce(f, Value::Complex(nums), depth, env);
            }
        }
        (Some((prim, _flipped)), Value::Byte(bytes)) => {
            let fill = env.scalar_fill::<f64>().ok().map(|fv| fv.value);
            if fill.is_none() && env.value_fill().is_some() {
                return generic_reduce(f, Value::Byte(bytes), depth, env);
            }
            let mut val: Value = match prim {
                Primitive::Add => fast_reduce_different(
                    bytes,
                    0.0,
                    fill,
                    depth,
                    add::num_num,
                    add::num_byte,
                    env,
                )?
                .into(),
                #[cfg(feature = "opt")]
                Primitive::Sub if _flipped => fast_reduce_different(
                    bytes,
                    0.0,
                    fill,
                    depth,
                    flip(sub::num_num),
                    flip(sub::byte_num),
                    env,
                )?
                .into(),
                #[cfg(feature = "opt")]
                Primitive::Sub => fast_reduce_different(
                    bytes,
                    0.0,
                    fill,
                    depth,
                    sub::num_num,
                    sub::num_byte,
                    env,
                )?
                .into(),
                Primitive::Mul if bytes.meta.flags.is_boolean() => {
                    let byte_fill = env.scalar_fill::<u8>().ok().map(|fv| fv.value);
                    if bytes.row_count() == 0 || fill.is_some() && byte_fill.is_none() {
                        fast_reduce_different(
                            bytes,
                            1.0,
                            fill,
                            depth,
                            mul::num_num,
                            mul::num_byte,
                            env,
                        )?
                        .into()
                    } else {
                        fast_reduce(bytes, 1, byte_fill, depth, mul::bool_bool, env)?.into()
                    }
                }
                Primitive::Mul => fast_reduce_different(
                    bytes,
                    1.0,
                    fill,
                    depth,
                    mul::num_num,
                    mul::num_byte,
                    env,
                )?
                .into(),
                Primitive::Or => {
                    let byte_fill = env.scalar_fill::<u8>().ok().map(|fv| fv.value);
                    if fill.is_some() && byte_fill.is_none() {
                        fast_reduce_different(
                            bytes,
                            0.0,
                            fill,
                            depth,
                            or::num_num,
                            or::num_byte,
                            env,
                        )?
                        .into()
                    } else if bytes.meta.flags.is_boolean() {
                        fast_reduce(bytes, 0, byte_fill, depth, or::bool_bool, env)?.into()
                    } else {
                        fast_reduce(bytes, 0, byte_fill, depth, or::byte_byte, env)?.into()
                    }
                }
                Primitive::Min => {
                    if bytes.rank() == 1 {
                        if bytes.meta.is_sorted_up() {
                            env.push(
                                (bytes.data.first().copied().map(f64::from))
                                    .unwrap_or(f64::INFINITY)
                                    .min(fill.unwrap_or(f64::INFINITY)),
                            );
                            return Ok(());
                        }
                        if bytes.meta.is_sorted_down() {
                            env.push(
                                (bytes.data.last().copied().map(f64::from))
                                    .unwrap_or(f64::INFINITY)
                                    .min(fill.unwrap_or(f64::INFINITY)),
                            );
                            return Ok(());
                        }
                    }
                    let byte_fill = env.scalar_fill::<u8>().ok().map(|fv| fv.value);
                    if bytes.row_count() == 0 || fill.is_some() && byte_fill.is_none() {
                        fast_reduce_different(
                            bytes,
                            f64::INFINITY,
                            fill,
                            depth,
                            min::num_num,
                            min::num_byte,
                            env,
                        )?
                        .into()
                    } else {
                        fast_reduce(bytes, 0, byte_fill, depth, min::byte_byte, env)?.into()
                    }
                }
                Primitive::Max => {
                    if bytes.rank() == 1 {
                        if bytes.meta.is_sorted_up() {
                            env.push(
                                (bytes.data.last().copied().map(f64::from))
                                    .unwrap_or(f64::NEG_INFINITY)
                                    .max(fill.unwrap_or(f64::NEG_INFINITY)),
                            );
                            return Ok(());
                        }
                        if bytes.meta.is_sorted_down() {
                            env.push(
                                (bytes.data.first().copied().map(f64::from))
                                    .unwrap_or(f64::NEG_INFINITY)
                                    .max(fill.unwrap_or(f64::NEG_INFINITY)),
                            );
                            return Ok(());
                        }
                    }
                    let byte_fill = env.scalar_fill::<u8>().ok().map(|fv| fv.value);
                    if bytes.row_count() == 0 || fill.is_some() && byte_fill.is_none() {
                        fast_reduce_different(
                            bytes,
                            f64::NEG_INFINITY,
                            fill,
                            depth,
                            max::num_num,
                            max::num_byte,
                            env,
                        )?
                        .into()
                    } else {
                        fast_reduce(bytes, 0, byte_fill, depth, max::byte_byte, env)?.into()
                    }
                }
                _ => return generic_reduce(f, Value::Byte(bytes), depth, env),
            };
            val.meta.take_sorted_flags();
            val.validate();
            env.push(val);
        }
        (_, xs) if f.sig == (2, 1) => {
            if depth == 0 && env.value_fill().is_none() {
                if xs.row_count() == 0 {
                    let val = reduce_identity(&f.node, xs, env)?.ok_or_else(|| {
                        env.error(format!(
                            "Cannot {} empty array. Function has no identity value.",
                            Primitive::Reduce.format()
                        ))
                    })?;
                    env.push(val);
                    return Ok(());
                }
                if xs.row_count() == 1 {
                    let val = reduce_singleton(&f.node, xs, identity);
                    env.push(val);
                    return Ok(());
                }
            }
            generic_reduce(f, xs, depth, env)?
        }
        (_, xs) => generic_reduce(f, xs, depth, env)?,
    }
    Ok(())
}

fn trim_node(node: &Node) -> &[Node] {
    let mut nodes = node.as_slice();
    use ImplPrimitive::*;
    use Primitive::*;
    let trim = |node: &Node| {
        matches!(
            node,
            Node::Prim(Args, _) | Node::ImplPrim(UnStack | StackN { .. }, _)
        )
    };
    while nodes.first().is_some_and(trim) {
        nodes = &nodes[1..];
    }
    while nodes.last().is_some_and(trim) {
        nodes = &nodes[..nodes.len() - 1];
    }
    nodes
}

fn reduce_identity(node: &Node, val: Value, env: &Uiua) -> UiuaResult<Option<Value>> {
    reduce_identity_impl(node, val, false, env)
}

fn reduce_identity_impl(
    node: &Node,
    mut val: Value,
    unbox: bool,
    env: &Uiua,
) -> UiuaResult<Option<Value>> {
    use Primitive::*;
    let nodes = trim_node(node);
    let mut shape = val.shape.clone();
    shape.make_row();
    let len: usize = shape.iter().product();
    let Some((first, tail)) = nodes.split_first() else {
        return Ok(None);
    };
    let Some((last, init)) = nodes.split_last() else {
        return Ok(None);
    };
    let init_sig = || nodes_sig(init).is_ok_and(|sig| sig.args() == sig.outputs());
    let tail_sig = || nodes_sig(tail).is_ok_and(|sig| sig.args() >= 1 && sig.outputs() == 1);
    Ok(Some(match first {
        Node::Prim(Join, _) if tail_sig() => {
            if val.rank() < 2 {
                val.shape[0] = 0;
                if unbox {
                    Array::<u8>::new(take(&mut val.shape), []).into()
                } else {
                    val
                }
            } else {
                let first = val.shape.remove(0);
                val.shape[0] *= first;
                val
            }
        }
        _ => match last {
            Node::Prim(Add | Sub | Or | Complex, _) if init_sig() => {
                let len = validate_size::<u8>([len], env)?;
                Array::new(shape, eco_vec![0u8; len]).into()
            }
            Node::Prim(Mul | Div | Pow, _) if init_sig() => {
                let len = validate_size::<u8>([len], env)?;
                Array::new(shape, eco_vec![1u8; len]).into()
            }
            Node::Prim(Max, _) if init_sig() => {
                let len = validate_size::<f64>([len], env)?;
                Array::new(shape, eco_vec![f64::NEG_INFINITY; len]).into()
            }
            Node::Prim(Modulo | Min, _) if init_sig() => {
                let len = validate_size::<f64>([len], env)?;
                Array::new(shape, eco_vec![f64::INFINITY; len]).into()
            }
            Node::Prim(Join, _) if init_sig() => {
                if val.rank() < 2 {
                    val.shape[0] = 0;
                    if unbox {
                        Array::<u8>::new(take(&mut val.shape), []).into()
                    } else {
                        val
                    }
                } else {
                    let first = val.shape.remove(0);
                    val.shape[0] *= first;
                    val
                }
            }
            Node::Format(parts, _) if parts.len() == 3 && init_sig() => {
                EcoVec::<char>::new().into()
            }
            _ => return Ok(None),
        },
    }))
}

fn reduce_singleton(node: &Node, val: Value, process: impl Fn(Value) -> Value) -> Value {
    use {Node::*, Primitive::*};
    if val.rank() == 0 {
        return val;
    }
    let row = process(val.row(0));
    // dbg!(node, &val, &row);
    let nodes = trim_node(node);
    match nodes {
        [init @ .., Prim(Join, _), Prim(Join, _)] if net_0_args(3, init) => {
            at_least_rank(1, row.unboxed_if(any_nodes_are_unboxes(init)))
        }
        [init @ .., Prim(Join, _)] if net_0_args(2, init) => {
            at_least_rank(1, row.unboxed_if(any_nodes_are_unboxes(init)))
        }
        [init @ .., Format(parts, _)] if net_0_args(parts.len().saturating_sub(1), init) => {
            row.format().into()
        }
        [Prim(Join, _), rest @ ..] if net_0_args(1, rest) => {
            at_least_rank(1, row.unboxed_if(any_nodes_are_unboxes(rest)))
        }
        nodes => row.unboxed_if(any_nodes_are_unboxes(nodes)),
    }
}

fn net_0_args(max: usize, nodes: &[Node]) -> bool {
    nodes_clean_sig(nodes).is_some_and(|sig| sig.args() <= max && sig.args() == sig.outputs())
}

fn at_least_rank(rank: usize, mut val: Value) -> Value {
    while val.rank() < rank {
        val.fix();
    }
    val
}

fn any_nodes_are_unboxes(nodes: &[Node]) -> bool {
    use {ImplPrimitive::*, Node::*, Primitive::*};
    nodes.iter().any(|node| match node {
        Run(nodes) => any_nodes_are_unboxes(nodes),
        ImplPrim(UnBox, _) => true,
        Mod(Dip | Both, args, _) if args.len() == 1 => any_nodes_are_unboxes(&args[0].node),
        _ => false,
    })
}

macro_rules! reduce_math {
    ($fname:ident, $ty:ident, $f:ident) => {
        #[allow(clippy::result_large_err)]
        fn $fname(
            prim: Primitive,
            _flipped: bool,
            xs: Array<$ty>,
            depth: usize,
            env: &mut Uiua,
        ) -> UiuaResult<Result<(), Array<$ty>>>
        where
            $ty: From<f64>,
        {
            let fill = env.scalar_fill::<$ty>().ok().map(|fv| fv.value);
            if fill.is_none() && env.value_fill().is_some() {
                return Ok(Err(xs));
            }
            const TID: u8 = <$ty>::TYPE_ID;
            env.push(match prim {
                Primitive::Add => fast_reduce(xs, 0.0.into(), fill, depth, add::$f, env)?,
                #[cfg(feature = "opt")]
                Primitive::Sub if _flipped => {
                    fast_reduce(xs, 0.0.into(), fill, depth, flip(sub::$f), env)?
                }
                #[cfg(feature = "opt")]
                Primitive::Sub => fast_reduce(xs, 0.0.into(), fill, depth, sub::$f, env)?,
                Primitive::Mul => fast_reduce(xs, 1.0.into(), fill, depth, mul::$f, env)?,
                Primitive::Or => fast_reduce(xs, 0.0.into(), fill, depth, or::$f, env)?,
                Primitive::Min if TID == 0 && xs.rank() == 1 && xs.meta.is_sorted_up() => {
                    let mut min = (xs.data.iter().find(|x| x.is_sortable()).copied())
                        .unwrap_or(f64::NEG_INFINITY.into());
                    if let Some(fill) = fill {
                        min = min.min(fill);
                    }
                    min.into()
                }
                Primitive::Min if TID == 0 && xs.rank() == 1 && xs.meta.is_sorted_down() => {
                    let mut min = (xs.data.iter().rfind(|&&x| x.is_sortable()).copied())
                        .unwrap_or(f64::NEG_INFINITY.into());
                    if let Some(fill) = fill {
                        min = min.min(fill);
                    }
                    min.into()
                }
                Primitive::Max if TID == 0 && xs.rank() == 1 && xs.meta.is_sorted_up() => {
                    let mut max = (xs.data.iter().rfind(|&&x| x.is_sortable()).copied())
                        .unwrap_or(f64::NEG_INFINITY.into());
                    if let Some(fill) = fill {
                        max = max.max(fill);
                    }
                    max.into()
                }
                Primitive::Max if TID == 0 && xs.rank() == 1 && xs.meta.is_sorted_down() => {
                    let mut max = (xs.data.iter().find(|x| x.is_sortable()).copied())
                        .unwrap_or(f64::NEG_INFINITY.into());
                    if let Some(fill) = fill {
                        max = max.max(fill);
                    }
                    max.into()
                }
                Primitive::Min => fast_reduce(xs, f64::INFINITY.into(), fill, depth, min::$f, env)?,
                Primitive::Max => {
                    fast_reduce(xs, f64::NEG_INFINITY.into(), fill, depth, max::$f, env)?
                }
                _ => return Ok(Err(xs)),
            });
            Ok(Ok(()))
        }
    };
}

reduce_math!(reduce_nums, f64, num_num);
reduce_math!(reduce_coms, Complex, com_x);

fn fast_reduce_different<T, U>(
    arr: Array<T>,
    identity: U,
    default: Option<U>,
    mut depth: usize,
    fuu: impl Fn(U, U) -> U,
    fut: impl Fn(U, T) -> U,
    env: &Uiua,
) -> UiuaResult<Array<U>>
where
    T: ArrayValue + Copy + Into<U>,
    U: ArrayValue + Copy,
{
    depth = depth.min(arr.rank());
    if depth == 0 && arr.rank() == 1 {
        return Ok(if let Some(default) = default {
            arr.data.into_iter().fold(default, fut).into()
        } else if arr.row_count() == 0 {
            identity.into()
        } else {
            let first = arr.data[0].into();
            arr.data[1..].iter().copied().fold(first, fut).into()
        });
    }
    fast_reduce(arr.convert(), identity, default, depth, fuu, env)
}

fn fast_reduce<T>(
    mut arr: Array<T>,
    identity: T,
    default: Option<T>,
    mut depth: usize,
    f: impl Fn(T, T) -> T,
    env: &Uiua,
) -> UiuaResult<Array<T>>
where
    T: ArrayValue + Copy,
{
    depth = depth.min(arr.rank());
    if depth == 0 && arr.rank() == 1 {
        return Ok(if let Some(default) = default {
            arr.data.iter().copied().fold(default, f).into()
        } else if arr.row_count() == 0 {
            identity.into()
        } else {
            let first = arr.data[0];
            arr.data[1..].iter().copied().fold(first, f).into()
        });
    }
    let mut arr = match (arr.rank(), depth) {
        (r, d) if r == d => arr,
        (1, 0) => {
            let data = arr.data.as_mut_slice();
            let reduced = default.into_iter().chain(data.iter().copied()).reduce(f);
            if let Some(reduced) = reduced {
                if data.is_empty() {
                    arr.data.extend(Some(reduced));
                } else {
                    data[0] = reduced;
                    arr.data.truncate(1);
                }
            } else {
                arr.data.extend(Some(identity));
            }
            arr.shape = Shape::default();
            arr
        }
        (_, 0) => {
            let row_len = arr.row_len();
            if row_len == 0 {
                arr.shape.remove(0);
                return Ok(Array::new(arr.shape, EcoVec::new()));
            }
            validate_size::<T>([row_len], env)?;
            let row_count = arr.row_count();
            arr.shape.remove(0);
            if row_count == 0 {
                let data = cowslice![default.unwrap_or(identity); row_len];
                return Ok(Array::new(arr.shape, data));
            }
            let sliced = arr.data.as_mut_slice();
            let (acc, rest) = sliced.split_at_mut(row_len);
            if let Some(default) = default {
                for acc in &mut *acc {
                    *acc = f(default, *acc);
                }
            }
            rest.chunks_exact(row_len).fold(acc, |acc, row| {
                for (a, b) in acc.iter_mut().zip(row) {
                    *a = f(*a, *b);
                }
                acc
            });
            arr.data.truncate(row_len);
            arr
        }
        (_, depth) => {
            let chunk_count: usize = arr.shape[..depth].iter().product();
            let chunk_len: usize = arr.shape[depth..].iter().product();
            let chunk_row_len: usize = arr.shape[depth + 1..].iter().product();
            let data_slice = arr.data.as_mut_slice();
            if chunk_len == 0 {
                let val = default.unwrap_or(identity);
                arr.data = cowslice![val; chunk_count * chunk_row_len];
            } else {
                for c in 0..chunk_count {
                    let chunk_start = c * chunk_len;
                    let chunk = &mut data_slice[chunk_start..][..chunk_len];
                    let (acc, rest) = chunk.split_at_mut(chunk_row_len);
                    if let Some(default) = default {
                        for acc in &mut *acc {
                            *acc = f(default, *acc);
                        }
                    }
                    rest.chunks_exact_mut(chunk_row_len).fold(acc, |acc, row| {
                        for (a, b) in acc.iter_mut().zip(row) {
                            *a = f(*a, *b);
                        }
                        acc
                    });
                    data_slice
                        .copy_within(chunk_start..chunk_start + chunk_row_len, c * chunk_row_len);
                }
                arr.data.truncate(chunk_count * chunk_row_len);
            }
            arr.shape.remove(depth);
            arr
        }
    };
    arr.meta.take_sorted_flags();
    arr.validate();
    Ok(arr)
}

fn generic_reduce(f: SigNode, xs: Value, depth: usize, env: &mut Uiua) -> UiuaResult {
    env.push(xs);
    let mut val = generic_reduce_inner(f, depth, identity, env)?;
    val.meta.take_sorted_flags();
    val.validate();
    env.push(val);
    Ok(())
}

pub fn reduce_content(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    let xs = env.pop(1)?;
    if let (1, Some(Primitive::Join)) = (xs.rank(), f.node.as_primitive()) {
        let (mut acc, rows) = if let Some(fv) = env.value_fill() {
            (fv.value.clone(), xs.into_rows().map(Value::unboxed))
        } else {
            match xs.row_count() {
                0 => {
                    env.push(match xs {
                        Value::Box(_) => Value::default(),
                        value => value,
                    });
                    return Ok(());
                }
                1 => {
                    let val = reduce_singleton(&f.node, xs, Value::unboxed);
                    env.push(val);
                    return Ok(());
                }
                _ => {}
            }
            let mut rows = xs.into_rows().map(Value::unboxed);
            (rows.next().unwrap(), rows)
        };
        if acc.rank() == 0 {
            acc.shape.prepend(1);
        }
        for row in rows {
            acc = acc.join(row, true, env)?;
        }
        env.push(acc);
        return Ok(());
    } else if xs.row_count() == 0 {
        let val = reduce_identity_impl(&f.node, xs, true, env)?.ok_or_else(|| {
            env.error(format!(
                "Cannot {} empty array. Function has no identity value.",
                Primitive::Reduce.format()
            ))
        })?;
        env.push(val);
        return Ok(());
    }
    env.push(xs);
    let val = generic_reduce_inner(f, 0, Value::unboxed, env)?;
    env.push(val);
    Ok(())
}

fn generic_reduce_inner(
    f: SigNode,
    depth: usize,
    process: impl Fn(Value) -> Value + Copy,
    env: &mut Uiua,
) -> UiuaResult<Value> {
    let sig = f.sig;
    if sig.outputs() != 1 {
        return Err(env.error(format!(
            "{}'s function must have exactly 1 output, \
            but its signature is {sig}",
            Primitive::Reduce.format(),
        )));
    }
    if sig.args() <= 1 {
        return Err(env.error(format!(
            "{}'s function must have at least 2 arguments, \
            but its signature is {sig}",
            Primitive::Reduce.format(),
        )));
    }
    let n = sig.args();
    let mut repeated = Vec::with_capacity(n - 2);
    for i in 0..n - 2 {
        repeated.push(process(env.pop(i + 1)?));
    }
    let xs = env.pop(n - 1)?;
    let value_fill = env.value_fill();
    if depth == 0 && value_fill.is_none() {
        if xs.row_count() == 0 {
            return reduce_identity(&f.node, xs, env)?.ok_or_else(|| {
                env.error(format!(
                    "Cannot {} empty array. Function has no identity value.",
                    Primitive::Reduce.format()
                ))
            });
        }
        if xs.row_count() == 1 {
            return Ok(reduce_singleton(&f.node, xs, process));
        }
    }
    if sig.args() == 2 {
        if depth == 0 {
            let mut rows = xs.into_rows();
            let mut acc = value_fill
                .map(|fv| fv.value.clone())
                .or_else(|| rows.next())
                .ok_or_else(|| {
                    env.error(format!("Cannot {} empty array", Primitive::Reduce.format()))
                })?;
            acc = process(acc);
            env.without_fill(|env| -> UiuaResult<Value> {
                for row in rows {
                    env.push(process(row));
                    env.push(acc);
                    env.exec(f.clone())?;
                    acc = env.pop("reduced function result")?;
                }
                Ok(acc)
            })
        } else {
            let mut new_rows = Vec::with_capacity(xs.row_count());

            // Handle empty arrays
            if xs.row_count() == 0
                && let Some(mut xs) = reduce_identity(&f.node, xs.clone(), env)?
                && xs.shape.elements() == 0
            {
                xs.shape.prepend(0);
                return Ok(xs);
            }

            // Normal case
            env.without_fill(|env| -> UiuaResult {
                for row in xs.into_rows() {
                    env.push(row);
                    let val = generic_reduce_inner(f.clone(), depth - 1, process, env)?;
                    new_rows.push(val);
                }
                Ok(())
            })?;
            Value::from_row_values(new_rows, env)
        }
    } else if depth == 0 {
        let mut rows = xs.into_rows();
        let mut acc = value_fill
            .map(|fv| fv.value.clone())
            .or_else(|| rows.next())
            .ok_or_else(|| {
                env.error(format!("Cannot {} empty array", Primitive::Reduce.format()))
            })?;
        acc = process(acc);
        env.without_fill(|env| {
            for row in rows {
                env.push(process(row));
                for val in repeated.iter().rev() {
                    env.push(val.clone());
                }
                env.push(acc);
                env.exec(f.clone())?;
                acc = env.pop("reduced function result")?;
            }
            Ok(acc)
        })
    } else {
        let mut new_values = Vec::with_capacity(xs.row_count());
        let mut args = repeated;
        args.push(xs);
        let FixedRowsData {
            row_count,
            mut rows,
            all_scalar,
            is_empty,
            per_meta,
            ..
        } = fixed_rows(Primitive::Rows.format(), 1, args, env)?;
        env.without_fill(|env| -> UiuaResult {
            for _ in 0..row_count {
                for arg in rows.iter_mut().rev() {
                    match arg {
                        Ok(rows) => env.push(rows.next().unwrap()),
                        Err(row) => env.push(row.clone()),
                    }
                }
                let val = generic_reduce_inner(f.clone(), depth - 1, process, env)?;
                new_values.push(val);
            }
            Ok(())
        })?;
        let mut rowsed = Value::from_row_values(new_values, env)?;
        if all_scalar {
            rowsed.undo_fix();
        } else if is_empty {
            rowsed.pop_row();
        }
        rowsed.validate();
        rowsed.meta.set_per_meta(per_meta.clone());
        Ok(rowsed)
    }
}

pub fn scan(ops: Ops, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [f] = get_ops(ops, env)?;
    let xs = env.pop(1)?;
    if xs.rank() == 0 && f.sig.args() <= 2 {
        return Err(env.error(format!("Cannot {} rank 0 array", Primitive::Scan.format())));
    }
    if env.value_fill().is_some() {
        return generic_scan(f, xs, env);
    }
    match (f.node.as_flipped_primitive(), xs) {
        (Some((prim, flipped)), Value::Num(nums)) => {
            let mut sorted_up = false;
            let mut sorted_down = false;
            let mut arr = match prim {
                Primitive::Eq => fast_scan(nums, |a, b| is_eq::num_num(a, b) as f64),
                Primitive::Ne => fast_scan(nums, |a, b| is_ne::num_num(a, b) as f64),
                Primitive::Lt => fast_scan(nums, |a, b| other_is_lt::num_num(a, b) as f64),
                Primitive::Le => fast_scan(nums, |a, b| other_is_le::num_num(a, b) as f64),
                Primitive::Gt => fast_scan(nums, |a, b| other_is_gt::num_num(a, b) as f64),
                Primitive::Ge => fast_scan(nums, |a, b| other_is_ge::num_num(a, b) as f64),
                Primitive::Add => fast_scan(nums, add::num_num),
                Primitive::Sub if flipped => fast_scan(nums, flip(sub::num_num)),
                Primitive::Sub => fast_scan(nums, sub::num_num),
                Primitive::Mul => fast_scan(nums, mul::num_num),
                Primitive::Div if flipped => fast_scan(nums, flip(div::num_num)),
                Primitive::Div => fast_scan(nums, div::num_num),
                Primitive::Modulo if flipped => fast_scan(nums, flip(modulo::num_num)),
                Primitive::Modulo => fast_scan(nums, modulo::num_num),
                Primitive::Atan if flipped => fast_scan(nums, flip(atan2::num_num)),
                Primitive::Atan => fast_scan(nums, atan2::num_num),
                Primitive::Min => {
                    sorted_down = true;
                    if nums.rank() == 1 && nums.meta.is_sorted_down() {
                        nums
                    } else {
                        fast_scan(nums, min::num_num)
                    }
                }
                Primitive::Max => {
                    sorted_up = true;
                    if nums.rank() == 1 && nums.meta.is_sorted_up() {
                        nums
                    } else {
                        fast_scan(nums, max::num_num)
                    }
                }
                _ => return generic_scan(f, Value::Num(nums), env),
            };
            arr.meta.mark_sorted_up(sorted_up);
            arr.meta.mark_sorted_down(sorted_down);
            arr.validate();
            env.push(arr);
            Ok(())
        }
        (Some((prim, flipped)), Value::Byte(bytes)) => {
            let mut sorted_up = false;
            let mut sorted_down = false;
            let mut val: Value = match prim {
                Primitive::Eq => fast_scan(bytes, is_eq::generic).into(),
                Primitive::Ne => fast_scan(bytes, is_ne::generic).into(),
                Primitive::Lt => fast_scan(bytes, other_is_lt::generic).into(),
                Primitive::Le => fast_scan(bytes, other_is_le::generic).into(),
                Primitive::Gt => fast_scan(bytes, other_is_gt::generic).into(),
                Primitive::Ge => fast_scan(bytes, other_is_ge::generic).into(),
                Primitive::Add => fast_scan::<f64>(bytes.convert(), add::num_num).into(),
                Primitive::Sub if flipped => {
                    fast_scan::<f64>(bytes.convert(), flip(sub::num_num)).into()
                }
                Primitive::Sub => fast_scan::<f64>(bytes.convert(), sub::num_num).into(),
                Primitive::Mul => fast_scan::<f64>(bytes.convert(), mul::num_num).into(),
                Primitive::Div if flipped => {
                    fast_scan::<f64>(bytes.convert(), flip(div::num_num)).into()
                }
                Primitive::Div => fast_scan::<f64>(bytes.convert(), div::num_num).into(),
                Primitive::Modulo if flipped => {
                    fast_scan::<f64>(bytes.convert(), flip(modulo::num_num)).into()
                }
                Primitive::Modulo => fast_scan::<f64>(bytes.convert(), modulo::num_num).into(),
                Primitive::Atan if flipped => {
                    fast_scan::<f64>(bytes.convert(), flip(atan2::num_num)).into()
                }
                Primitive::Atan => fast_scan::<f64>(bytes.convert(), atan2::num_num).into(),
                Primitive::Min => {
                    sorted_down = true;
                    if bytes.rank() == 1 && bytes.meta.is_sorted_down() {
                        bytes
                    } else {
                        fast_scan(bytes, u8::min)
                    }
                    .into()
                }
                Primitive::Max => {
                    sorted_up = true;
                    if bytes.rank() == 1 && bytes.meta.is_sorted_up() {
                        bytes
                    } else {
                        fast_scan(bytes, u8::max)
                    }
                    .into()
                }
                _ => return generic_scan(f, Value::Byte(bytes), env),
            };
            val.meta.mark_sorted_up(sorted_up);
            val.meta.mark_sorted_down(sorted_down);
            val.validate();
            env.push(val);
            Ok(())
        }
        (_, xs) => generic_scan(f, xs, env),
    }
}

fn fast_scan<T>(mut arr: Array<T>, f: impl Fn(T, T) -> T) -> Array<T>
where
    T: ArrayValue + Copy,
{
    match arr.shape.len() {
        0 => unreachable!("fast_scan called on unit array, should have been guarded against"),
        1 => {
            if arr.row_count() == 0 {
                return arr;
            }
            let mut acc = arr.data[0];
            for val in arr.data.as_mut_slice()[1..].iter_mut() {
                acc = f(acc, *val);
                *val = acc;
            }
            arr
        }
        _ => {
            let row_len: usize = arr.row_len();
            if arr.row_count() == 0 {
                return arr;
            }
            let shape = arr.shape.clone();
            let mut new_data = EcoVec::with_capacity(arr.data.len());
            let mut rows = arr.into_rows();
            new_data.extend(rows.next().unwrap().data);
            for row in rows {
                let start = new_data.len() - row_len;
                for (i, r) in row.data.into_iter().enumerate() {
                    new_data.push(f(new_data[start + i], r));
                }
            }
            Array::new(shape, new_data)
        }
    }
}

fn generic_scan(f: SigNode, xs: Value, env: &mut Uiua) -> UiuaResult {
    let sig = f.sig;
    if sig.outputs() != 1 {
        return Err(env.error(format!(
            "{}'s function must have 1 output, \
            but its signature is {sig}",
            Primitive::Scan.format(),
        )));
    }
    match sig.args() {
        0 | 1 => Err(env.error(format!(
            "{}'s function must have at least 2 arguments, \
            but its signature is {sig}",
            Primitive::Scan.format(),
        ))),
        2 => {
            if xs.row_count() == 0 {
                env.push(xs.first_dim_zero());
                return Ok(());
            }
            let row_count = xs.row_count();
            let mut rows = xs.into_rows();
            let mut scanned = Vec::with_capacity(row_count);
            let mut acc = if let Some(fill) = env.value_fill() {
                fill.value.clone()
            } else {
                let first = rows.next().unwrap();
                scanned.push(first.clone());
                first
            };
            env.without_fill(|env| -> UiuaResult {
                for row in rows.by_ref() {
                    env.push(row);
                    env.push(acc.clone());
                    env.exec(f.clone())?;
                    acc = env.pop("scanned function result")?;
                    scanned.push(acc.clone());
                }
                Ok(())
            })?;
            let val = Value::from_row_values(scanned, env)?;
            env.push(val);
            Ok(())
        }
        n => {
            let mut repeated = Vec::with_capacity(n - 1);
            repeated.push(xs);
            for i in 0..n - 2 {
                repeated.push(env.pop(i + 2)?);
            }
            let xs = repeated.pop().unwrap();
            if xs.row_count() == 0 {
                let val = reduce_identity(&f.node, xs.clone(), env)?
                    .map(|v| v.first_dim_zero())
                    .unwrap_or(xs);
                env.push(val);
                return Ok(());
            }
            let mut scanned = Vec::with_capacity(xs.row_count());
            let mut rows = xs.into_rows();
            let mut acc = rows.next().unwrap();
            scanned.push(acc.clone());
            env.without_fill(|env| -> UiuaResult {
                for row in rows {
                    env.push(row);
                    for val in repeated.iter().rev() {
                        env.push(val.clone());
                    }
                    env.push(acc);
                    env.exec(f.clone())?;
                    acc = env.pop("reduced function result")?;
                    scanned.push(acc.clone());
                }
                Ok(())
            })?;
            let val = Value::from_row_values(scanned, env)?;
            env.push(val);
            Ok(())
        }
    }
}

pub fn unscan(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    let mut xs = env.pop(1)?;
    if xs.rank() == 0 {
        return Err(env.error(format!(
            "Cannot {}{} rank 0 array",
            Primitive::Un,
            Primitive::Scan
        )));
    }
    let sig = f.sig;
    if sig != (2, 1) {
        return Err(env.error(format!(
            "{}{}'s function's signature must be |2.1, but it is {sig}",
            Primitive::Un,
            Primitive::Scan
        )));
    }
    if xs.row_count() == 0 {
        env.push(xs.first_dim_zero());
        return Ok(());
    }

    match xs {
        Value::Num(nums) => match f.node.as_flipped_primitive() {
            Some((Primitive::Sub, false)) => {
                env.push(fast_invscan(nums, sub::num_num));
                return Ok(());
            }
            Some((Primitive::Div, false)) => {
                env.push(fast_invscan(nums, div::num_num));
                return Ok(());
            }
            _ => xs = Value::Num(nums),
        },
        Value::Byte(bytes) => match f.node.as_flipped_primitive() {
            Some((Primitive::Sub, false)) => {
                env.push(fast_invscan(bytes.convert(), sub::num_num));
                return Ok(());
            }
            Some((Primitive::Div, false)) => {
                env.push(fast_invscan(bytes.convert(), div::num_num));
                return Ok(());
            }
            _ => xs = Value::Byte(bytes),
        },
        val => xs = val,
    }

    let mut unscanned = Vec::with_capacity(xs.row_count());
    let mut rows = xs.into_rows();
    let mut curr = rows.next().unwrap();
    unscanned.push(curr.clone());
    env.without_fill(|env| -> UiuaResult {
        for row in rows {
            env.push(row.clone());
            env.push(curr);
            env.exec(f.clone())?;
            unscanned.push(env.pop("unscanned function result")?);
            curr = row;
        }
        Ok(())
    })?;
    env.push(Value::from_row_values(unscanned, env)?);
    Ok(())
}

fn fast_invscan<T>(mut arr: Array<T>, f: impl Fn(T, T) -> T) -> Array<T>
where
    T: ArrayValue + Copy,
{
    arr.meta.take_sorted_flags();
    match arr.shape.len() {
        0 => unreachable!("fast_invscan called on unit array, should have been guarded against"),
        1 => {
            if arr.row_count() == 0 {
                return arr;
            }
            let mut acc = arr.data[0];
            for val in arr.data.as_mut_slice()[1..].iter_mut() {
                let temp = *val;
                *val = f(acc, *val);
                acc = temp;
            }
            arr
        }
        _ => {
            if arr.row_count() == 0 {
                return arr;
            }
            let row_len: usize = arr.row_len();
            let (acc, rest) = arr.data.as_mut_slice().split_at_mut(row_len);
            let mut acc = acc.to_vec();
            let mut temp = acc.clone();
            for row_slice in rest.chunks_exact_mut(row_len) {
                temp.copy_from_slice(row_slice);
                for (a, b) in acc.iter_mut().zip(row_slice) {
                    *b = f(*a, *b);
                }
                acc.copy_from_slice(&temp);
            }
            arr
        }
    }
}

struct FoldState<T> {
    arrays: Vec<Result<T, Value>>,
    acc_count: usize,
    excess_count: usize,
    row_count: usize,
}

fn prepare_fold(
    sig: Signature,
    env: &mut Uiua,
) -> UiuaResult<FoldState<impl Iterator<Item = Value> + 'static>> {
    let (iterable_count, acc_count, excess_count) = if sig.args() > sig.outputs() {
        (sig.args() - sig.outputs(), sig.outputs(), 0)
    } else {
        let iter = sig.args().min(1);
        let acc = sig.args().saturating_sub(iter);
        let collect = sig.outputs() - acc;
        (iter, acc, collect)
    };
    let mut arrays = Vec::with_capacity(iterable_count);
    for i in 0..iterable_count {
        let mut val = env.pop(("iterated array", i + 1))?;
        arrays.push(if val.row_count() == 1 {
            val.undo_fix();
            Err(val)
        } else {
            Ok(val.into_rows())
        });
    }
    if env.stack_height() < acc_count {
        for i in 0..acc_count {
            env.pop(("accumulator", i + 1))?;
        }
    }
    for i in 0..iterable_count {
        for j in i + 1..iterable_count {
            if let (Ok(a), Ok(b)) = (&arrays[i], &arrays[j])
                && a.len() != b.len()
            {
                return Err(env.error(format!(
                    "Cannot {} arrays of different lengths: {} and {}",
                    Primitive::Fold.format(),
                    a.len(),
                    b.len()
                )));
            }
        }
    }
    let mut row_count = arrays
        .iter()
        .filter_map(|arr| arr.as_ref().ok())
        .map(|arr| arr.len())
        .max()
        .unwrap_or(0);
    if row_count == 0 && arrays.iter().all(Result::is_err) {
        row_count = 1;
    }
    Ok(FoldState {
        arrays,
        acc_count,
        excess_count,
        row_count,
    })
}

pub fn fold(ops: Ops, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [f] = get_ops(ops, env)?;
    let FoldState {
        mut arrays,
        excess_count,
        acc_count,
        row_count,
    } = prepare_fold(f.sig, env)?;
    let mut excess_rows = vec![Vec::new(); excess_count];
    for _ in 0..row_count {
        for array in arrays.iter_mut().rev() {
            env.push(match array {
                Ok(arr) => arr.next().unwrap(),
                Err(arr) => arr.clone(),
            });
        }
        env.exec(f.clone())?;
        if excess_count > 0 {
            for (i, row) in env
                .remove_n(excess_count, acc_count + excess_count)?
                .enumerate()
            {
                excess_rows[i].push(row);
            }
        }
    }
    // Remove preserved/excess values
    if excess_count > 0 {
        _ = env.remove_n(acc_count, acc_count)?;
    }
    // Collect excess values
    for rows in excess_rows.into_iter().rev() {
        let new_val = Value::from_row_values(rows, env)?;
        env.push(new_val);
    }
    Ok(())
}

pub fn fold_while(ops: Ops, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [f, g] = get_ops(ops, env)?;

    let copy_count = (g.sig.args()).saturating_sub(g.sig.outputs().saturating_sub(1));
    let cond_sub_sig = Signature::new(
        g.sig.args(),
        (g.sig.outputs() + copy_count).saturating_sub(1),
    );
    let comp_sig = f.sig.compose(cond_sub_sig);

    let FoldState {
        mut arrays,
        excess_count,
        acc_count,
        row_count,
    } = prepare_fold(comp_sig, env)?;
    let mut excess_rows = vec![Vec::new(); excess_count];
    let mut copies = Vec::with_capacity(copy_count);
    let mut g_args = Vec::with_capacity(g.sig.args());
    let iterable_count = arrays.len();
    dbg!(copy_count, excess_count, acc_count, row_count);
    for _ in 0..row_count {
        let mut arr_iter = arrays.iter_mut().map(|array| match array {
            Ok(arr) => arr.next().unwrap(),
            Err(arr) => arr.clone(),
        });
        for i in 0..g.sig.args() {
            if i < copy_count {
                &mut copies
            } else {
                &mut g_args
            }
            .push(if let Some(arr) = arr_iter.next() {
                arr
            } else {
                env.copy_nth(i - iterable_count)?
            })
        }
        for arg in g_args.drain(..).rev().chain(copies.iter().cloned().rev()) {
            env.push(arg);
        }
        println!("stack before condition: {:?}", env.stack());
        env.exec(g.clone())?;
        let condition = env
            .pop("condition")?
            .as_bool(env, "Condition must be a boolean")?;
        println!("stack after condition: {:?}", env.stack());
        if !condition {
            env.pop_n(g.sig.outputs().saturating_sub(1))?;
            break;
        }
        for arr in arr_iter
            .rev()
            .chain(copies.drain(..).take(iterable_count).rev())
        {
            env.push(arr);
        }
        env.exec(f.clone())?;
        if excess_count > 0 {
            for (i, row) in env
                .remove_n(excess_count, acc_count + excess_count)?
                .enumerate()
            {
                excess_rows[i].push(row);
            }
        }
    }
    // Remove preserved/excess values
    if excess_count > 0 {
        _ = env.remove_n(acc_count, acc_count)?;
    }
    // Collect excess values
    for rows in excess_rows.into_iter().rev() {
        let new_val = Value::from_row_values(rows, env)?;
        env.push(new_val);
    }
    Ok(())
}
