//! Algorithms for zipping modifiers

use std::{cell::RefCell, collections::HashMap, iter::repeat, mem::swap, rc::Rc};

use ecow::eco_vec;

use crate::{
    algorithm::{pervade::bin_pervade_values, reduce},
    cowslice::CowSlice,
    get_ops, random,
    types::push_empty_rows_value,
    val_as_arr,
    value::Value,
    Array, ArrayValue, Boxed, ImplPrimitive, Node, Ops, PersistentMeta, Primitive, Shape, SigNode,
    Uiua, UiuaResult,
};

use super::{fill_value_shapes, fixed_rows, multi_output, FixedRowsData, MultiOutput};

pub(crate) type ValueMonFn = Rc<dyn Fn(Value, usize, &mut Uiua) -> UiuaResult<Value>>;
type ValueMon2Fn = Box<dyn Fn(Value, usize, &mut Uiua) -> UiuaResult<(Value, Value)>>;
type ValueDyFn = Box<dyn Fn(Value, Value, usize, usize, &mut Uiua) -> UiuaResult<Value>>;

fn mon_fn(f: impl Fn(Value, usize, &mut Uiua) -> UiuaResult<Value> + 'static) -> ValueMonFn {
    Rc::new(move |v, d, env| f(v, d, env))
}

fn spanned_mon_fn(
    span: usize,
    f: impl Fn(Value, usize, &Uiua) -> UiuaResult<Value> + 'static,
) -> ValueMonFn {
    mon_fn(move |v, d, env| env.with_span(span, |env| f(v, d, env)))
}

fn spanned_mon2_fn(
    span: usize,
    f: impl Fn(Value, usize, &Uiua) -> UiuaResult<(Value, Value)> + 'static,
) -> ValueMon2Fn {
    Box::new(move |v, d, env| env.with_span(span, |env| f(v, d, env)))
}

fn prim_mon_fast_fn(prim: Primitive, span: usize) -> Option<ValueMonFn> {
    use Primitive::*;
    Some(match prim {
        Not => spanned_mon_fn(span, |v, _, env| Value::not(v, env)),
        Sign => spanned_mon_fn(span, |v, _, env| Value::sign(v, env)),
        Neg => spanned_mon_fn(span, |v, _, env| Value::neg(v, env)),
        Abs => spanned_mon_fn(span, |v, _, env| Value::abs(v, env)),
        Sqrt => spanned_mon_fn(span, |v, _, env| Value::sqrt(v, env)),
        Floor => spanned_mon_fn(span, |v, _, env| Value::floor(v, env)),
        Ceil => spanned_mon_fn(span, |v, _, env| Value::ceil(v, env)),
        Round => spanned_mon_fn(span, |v, _, env| Value::round(v, env)),
        Deshape => spanned_mon_fn(span, |mut v, d, _| {
            Value::deshape_depth(&mut v, d);
            Ok(v)
        }),
        Transpose => spanned_mon_fn(span, |mut v, d, _| {
            Value::transpose_depth(&mut v, d, 1);
            Ok(v)
        }),
        Reverse => spanned_mon_fn(span, |mut v, d, _| {
            Value::reverse_depth(&mut v, d);
            Ok(v)
        }),
        Classify => spanned_mon_fn(span, |v, d, _| Ok(v.classify_depth(d))),
        Fix => spanned_mon_fn(span, |mut v, d, _| {
            v.fix_depth(d);
            Ok(v)
        }),
        Box => spanned_mon_fn(span, |v, d, _| Ok(v.box_depth(d).into())),
        First => spanned_mon_fn(span, |v, d, env| v.first_depth(d, env)),
        Last => spanned_mon_fn(span, |v, d, env| v.last_depth(d, env)),
        Sort => spanned_mon_fn(span, |mut v, d, _| {
            v.sort_up_depth(d);
            Ok(v)
        }),
        _ => return None,
    })
}

fn impl_prim_mon_fast_fn(prim: ImplPrimitive, span: usize) -> Option<ValueMonFn> {
    use ImplPrimitive::*;
    Some(match prim {
        TransposeN(n) => spanned_mon_fn(span, move |mut v, d, _| {
            Value::transpose_depth(&mut v, d, n);
            Ok(v)
        }),
        DeshapeSub(i) => spanned_mon_fn(span, move |mut v, d, env| {
            Value::deshape_sub(&mut v, i, d, true, env)?;
            Ok(v)
        }),
        ReplaceRand => spanned_mon_fn(span, |v, d, _| {
            let shape = &v.shape()[..d.min(v.rank())];
            let elem_count: usize = shape.iter().product();
            let mut data = eco_vec![0.0; elem_count];
            for n in data.make_mut() {
                *n = random();
            }
            Ok(Array::new(shape, data).into())
        }),
        SortDown => spanned_mon_fn(span, |mut v, d, _| {
            v.sort_down_depth(d);
            Ok(v)
        }),
        _ => return None,
    })
}

fn prim_mon2_fast_fn(prim: Primitive, span: usize) -> Option<ValueMon2Fn> {
    use Primitive::*;
    Some(match prim {
        Dup => spanned_mon2_fn(span, |v, _, _| Ok((v.clone(), v))),
        _ => return None,
    })
}

fn impl_prim_mon2_fast_fn(prim: ImplPrimitive, span: usize) -> Option<ValueMon2Fn> {
    use ImplPrimitive::*;
    Some(match prim {
        UnCouple => spanned_mon2_fn(span, |v, d, env| v.uncouple_depth(d, env)),
        UnJoin => spanned_mon2_fn(span, |v, d, env| v.unjoin_depth(d, false, env)),
        UnJoinEnd => spanned_mon2_fn(span, |v, d, env| v.unjoin_depth(d, true, env)),
        _ => return None,
    })
}

pub(crate) fn f_mon_fast_fn(node: &Node, env: &Uiua) -> Option<(ValueMonFn, usize)> {
    thread_local! {
        static CACHE: RefCell<HashMap<Node, Option<(ValueMonFn, usize)>>>
        = RefCell::new(HashMap::new());
    }
    CACHE.with(|cache| {
        if !cache.borrow().contains_key(node) {
            let f_and_depth = f_mon_fast_fn_impl(node, false, env);
            cache.borrow_mut().insert(node.clone(), f_and_depth);
        }
        cache.borrow()[node].clone()
    })
}

fn f_mon_fast_fn_impl(nodes: &[Node], deep: bool, env: &Uiua) -> Option<(ValueMonFn, usize)> {
    use Primitive::*;
    Some(match nodes {
        &[Node::Prim(prim, span)] => {
            let f = prim_mon_fast_fn(prim, span)?;
            (f, 0)
        }
        &[Node::ImplPrim(prim, span)] => {
            let f = impl_prim_mon_fast_fn(prim, span)?;
            (f, 0)
        }
        [Node::CustomInverse(cust, _)] => {
            return if let Ok(f) = cust.normal.as_ref() {
                f_mon_fast_fn_impl(f.node.as_slice(), deep, env)
            } else {
                None
            }
        }
        [Node::Mod(Rows, args, _)] => {
            let (f, d) = f_mon_fast_fn(&args[0].node, env)?;
            (f, d + 1)
        }
        [Node::Mod(Reduce, args, _)] if args[0].sig == (2, 1) => {
            let args = args.clone();
            let f = Rc::new(move |val, depth, env: &mut Uiua| -> UiuaResult<Value> {
                env.push(val);
                reduce::reduce(args.clone(), depth, env)?;
                let val = env.pop("reduced function result")?;
                Ok(val)
            });
            (f, 0)
        }
        [Node::Prim(Pop, _), Node::Push(repl)] => {
            let replacement = repl.clone();
            (
                Rc::new(move |val, depth, _| Ok(val.replace_depth(replacement.clone(), depth))),
                0,
            )
        }
        nodes if !deep => {
            let mut start = 0;
            let mut depth = 0;
            let mut func: Option<ValueMonFn> = None;
            'outer: loop {
                for len in [1, 2] {
                    let end = start + len;
                    if end > nodes.len() {
                        break 'outer;
                    }
                    let nodes = &nodes[start..end];
                    let Some((f, d)) = f_mon_fast_fn_impl(nodes, true, env) else {
                        continue;
                    };
                    depth += d;
                    func = Some(if let Some(func) = func {
                        mon_fn(
                            move |val: Value, depth: usize, env: &mut Uiua| -> UiuaResult<Value> {
                                f(func(val, depth, env)?, depth, env)
                            },
                        )
                    } else {
                        f
                    });
                    start = end;
                    continue 'outer;
                }
                return None;
            }
            let func = func?;
            (func, depth)
        }
        _ => return None,
    })
}

fn f_mon2_fast_fn(nodes: &[Node], env: &Uiua) -> Option<(ValueMon2Fn, usize)> {
    f_mon2_fast_fn_impl(nodes, env)
}
fn f_mon2_fast_fn_impl(nodes: &[Node], env: &Uiua) -> Option<(ValueMon2Fn, usize)> {
    use Primitive::*;
    Some(match nodes {
        &[Node::Prim(prim, span)] => {
            let f = prim_mon2_fast_fn(prim, span)?;
            (f, 0)
        }
        &[Node::ImplPrim(prim, span)] => {
            let f = impl_prim_mon2_fast_fn(prim, span)?;
            (f, 0)
        }
        [Node::CustomInverse(cust, _)] => {
            return if let Ok(f) = cust.normal.as_ref() {
                f_mon2_fast_fn_impl(f.node.as_slice(), env)
            } else {
                None
            }
        }
        [Node::Mod(Rows, args, _)] => {
            let (f, d) = f_mon2_fast_fn(args[0].node.as_slice(), env)?;
            (f, d + 1)
        }
        // By monadic
        [Node::Mod(By, args, _)] if args[0].sig == (1, 1) => {
            let get_second = f_mon_fast_fn_impl(args[0].node.as_slice(), false, env)?;
            let f = std::boxed::Box::new(move |val: Value, depth: usize, env: &mut Uiua| {
                Ok((get_second.0(val.clone(), depth, env)?, val))
            });
            (f, 0)
        }
        // On monadic
        [Node::Mod(On, args, _)] if args[0].sig == (1, 1) => {
            let get_second = f_mon_fast_fn_impl(args[0].node.as_slice(), false, env)?;
            let f = std::boxed::Box::new(move |val: Value, depth: usize, env: &mut Uiua| {
                Ok((val.clone(), get_second.0(val, depth, env)?))
            });
            (f, 0)
        }
        // By constant
        [Node::Prim(Identity, _), Node::Push(repl)] => {
            let repl = repl.clone();
            let f = std::boxed::Box::new(move |val: Value, depth: usize, _: &mut Uiua| {
                let replaced = val.replace_depth(repl.clone(), depth);
                Ok((replaced, val))
            });
            (f, 0)
        }
        // On constant
        [Node::Push(repl), Node::Prim(Flip, _)] => {
            let repl = repl.clone();
            let f = std::boxed::Box::new(move |val: Value, depth: usize, _: &mut Uiua| {
                let replaced = val.replace_depth(repl.clone(), depth);
                Ok((val, replaced))
            });
            (f, 0)
        }
        _ => return None,
    })
}

impl Value {
    fn replace_depth(&self, mut replacement: Value, depth: usize) -> Value {
        let depth = self.rank().min(depth);
        let prefix = &self.shape()[..depth];
        val_as_arr!(&mut replacement, |arr| arr
            .repeat_shape(Shape::from(prefix)));
        replacement
    }
}

impl<T: ArrayValue> Array<T> {
    fn repeat_shape(&mut self, mut shape_prefix: Shape) {
        let count = shape_prefix.elements();
        swap(&mut self.shape, &mut shape_prefix);
        self.shape.extend(shape_prefix);
        match count {
            0 => {
                self.data = CowSlice::default();
            }
            1 => {}
            n => {
                let mut new_data = self.data.clone();
                for _ in 1..n {
                    new_data.extend_from_slice(&self.data);
                }
                self.data = new_data;
            }
        }
        self.validate();
    }
}

fn spanned_dy_fn(
    span: usize,
    f: impl Fn(Value, Value, &Uiua) -> UiuaResult<Value> + 'static,
) -> ValueDyFn {
    Box::new(move |a, b, _, _, env| env.with_span(span, |env| f(a, b, env)))
}

fn prim_dy_fast_fn(
    prim: Primitive,
    span: usize,
    _a_filled: bool,
    b_filled: bool,
) -> Option<ValueDyFn> {
    use std::boxed::Box;
    use Primitive::*;
    Some(match prim {
        Add => spanned_dy_fn(span, Value::add),
        Sub => spanned_dy_fn(span, Value::sub),
        Mul => spanned_dy_fn(span, Value::mul),
        Div => spanned_dy_fn(span, Value::div),
        Pow => spanned_dy_fn(span, Value::pow),
        Modulus => spanned_dy_fn(span, Value::modulus),
        Log => spanned_dy_fn(span, Value::log),
        Eq => spanned_dy_fn(span, Value::is_eq),
        Ne => spanned_dy_fn(span, Value::is_ne),
        Lt => spanned_dy_fn(span, Value::other_is_lt),
        Gt => spanned_dy_fn(span, Value::other_is_gt),
        Le => spanned_dy_fn(span, Value::other_is_le),
        Ge => spanned_dy_fn(span, Value::other_is_ge),
        Complex => spanned_dy_fn(span, Value::complex),
        Max => spanned_dy_fn(span, Value::max),
        Min => spanned_dy_fn(span, Value::min),
        Atan => spanned_dy_fn(span, Value::atan2),
        Rotate if !b_filled => Box::new(move |a, mut b, ad, bd, env| {
            env.with_span(span, |env| {
                a.rotate_depth(&mut b, ad, bd, env)?;
                Ok(b)
            })
        }),
        _ => return None,
    })
}

pub(crate) fn f_dy_fast_fn(
    nodes: &[Node],
    a_filled: bool,
    b_filled: bool,
) -> Option<(ValueDyFn, usize, usize)> {
    use std::boxed::Box;
    use Primitive::*;

    fn nest_dy_fast<F>(
        (f, ad1, bd1): (F, usize, usize),
        ad2: usize,
        bd2: usize,
    ) -> Option<(F, usize, usize)> {
        if (ad1 as isize - ad2 as isize).signum() != (bd1 as isize - bd2 as isize).signum() {
            return None;
        }
        Some((f, ad1 + ad2, bd1 + bd2))
    }

    match nodes {
        &[Node::Prim(prim, span)] => {
            let f = prim_dy_fast_fn(prim, span, a_filled, b_filled)?;
            return Some((f, 0, 0));
        }
        [Node::Mod(Rows, args, _)] => {
            return nest_dy_fast(
                f_dy_fast_fn(args[0].node.as_slice(), a_filled, b_filled)?,
                1,
                1,
            )
        }
        [Node::Prim(Flip, _), rest @ ..] => {
            let (f, a, b) = f_dy_fast_fn(rest, a_filled, b_filled)?;
            let f = Box::new(move |a, b, ad, bd, env: &mut Uiua| f(b, a, bd, ad, env));
            return Some((f, a, b));
        }
        _ => (),
    }
    None
}

pub fn each(ops: Ops, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [f] = get_ops(ops, env)?;
    let sig = f.sig;
    match sig.args() {
        0 => env.without_fill(|env| env.exec(f)),
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

fn each1(f: SigNode, mut xs: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, ..)) = f_mon_fast_fn(&f.node, env) {
        let maybe_through_boxes = matches!(&xs, Value::Box(..));
        if !maybe_through_boxes {
            let rank = xs.rank();
            let val = f(xs, rank, env)?;
            env.push(val);
            return Ok(());
        }
    }
    let outputs = f.sig.outputs();
    let mut new_values = multi_output(outputs, Vec::with_capacity(xs.element_count()));
    let new_shape = xs.shape().clone();
    let is_empty = outputs > 0 && xs.row_count() == 0;
    let per_meta = xs.take_per_meta();
    env.without_fill(|env| -> UiuaResult {
        if is_empty {
            env.push(xs.proxy_scalar(env));
            _ = env.exec_maintain_sig(f);
            for i in 0..outputs {
                new_values[i].push(env.pop("each's function result")?);
            }
        } else {
            for val in xs.into_elements() {
                env.push(val);
                env.exec(f.clone())?;
                for i in 0..outputs {
                    new_values[i].push(env.pop("each's function result")?);
                }
            }
        }
        Ok(())
    })?;
    for new_values in new_values.into_iter().rev() {
        let mut new_shape = new_shape.clone();
        let mut eached = Value::from_row_values(new_values, env)?;
        if is_empty {
            eached.pop_row();
        }
        new_shape.extend_from_slice(eached.shape().row_slice());
        *eached.shape_mut() = new_shape;
        eached.validate();
        eached.set_per_meta(per_meta.clone());
        env.push(eached);
    }
    Ok(())
}

fn each2(f: SigNode, mut xs: Value, mut ys: Value, env: &mut Uiua) -> UiuaResult {
    if let Some((f, ..)) = f_dy_fast_fn(
        f.node.as_slice(),
        env.fill().value_for(&xs).is_some(),
        env.fill().value_for(&ys).is_some(),
    ) {
        let xrank = xs.rank();
        let yrank = ys.rank();
        let val = f(xs, ys, xrank, yrank, env)?;
        env.push(val);
    } else {
        let outputs = f.sig.outputs();
        let mut xs_shape = xs.shape().to_vec();
        let mut ys_shape = ys.shape().to_vec();
        let is_empty = outputs > 0 && (xs.row_count() == 0 || ys.row_count() == 0);
        let per_meta = xs.take_per_meta().xor(ys.take_per_meta());
        let xs_fill = xs.fill(env);
        let ys_fill = ys.fill(env);
        let new_values = env.without_fill(|env| {
            if is_empty {
                if let Some(r) = xs_shape.first_mut() {
                    *r = 1;
                }
                if let Some(r) = ys_shape.first_mut() {
                    *r = 1;
                }
                bin_pervade_values(
                    xs.proxy_row(env),
                    ys.proxy_row(env),
                    xs_fill,
                    ys_fill,
                    outputs,
                    env,
                    |x, y, env| {
                        env.push(y);
                        env.push(x);
                        if env.exec_maintain_sig(f.clone()).is_ok() {
                            (0..outputs)
                                .map(|_| env.pop("each's function result"))
                                .collect::<Result<MultiOutput<_>, _>>()
                        } else {
                            Ok(multi_output(outputs, Value::default()))
                        }
                    },
                )
            } else {
                bin_pervade_values(xs, ys, xs_fill, ys_fill, outputs, env, |x, y, env| {
                    env.push(y);
                    env.push(x);
                    env.exec(f.clone())?;
                    (0..outputs)
                        .map(|_| env.pop("each's function result"))
                        .collect::<Result<MultiOutput<_>, _>>()
                })
            }
        })?;
        for mut eached in new_values.into_iter().rev() {
            eached.set_per_meta(per_meta.clone());
            env.push(eached);
        }
    }
    Ok(())
}

fn eachn(f: SigNode, mut args: Vec<Value>, env: &mut Uiua) -> UiuaResult {
    for a in 0..args.len() - 1 {
        let (a, b) = args.split_at_mut(a + 1);
        let a = a.last_mut().unwrap();
        for b in b {
            fill_value_shapes(a, b, true, env)?;
        }
    }
    let outputs = f.sig.outputs();
    let is_empty = args.iter().any(|v| v.element_count() == 0);
    let elem_count = args.iter().map(Value::element_count).max().unwrap() + is_empty as usize;
    let mut new_values = multi_output(outputs, Vec::with_capacity(elem_count));
    let new_shape = args
        .iter()
        .map(Value::shape)
        .max_by_key(|s| s.len())
        .unwrap()
        .clone();
    let per_meta = PersistentMeta::xor_all(args.iter_mut().map(|v| v.take_per_meta()));
    env.without_fill(|env| -> UiuaResult {
        if is_empty {
            for arg in args.into_iter().rev() {
                env.push(arg.proxy_scalar(env));
            }
            _ = env.exec_maintain_sig(f);
            for i in 0..outputs {
                new_values[i].push(env.pop("each's function result")?);
            }
        } else {
            let mut arg_elems: Vec<_> = args
                .into_iter()
                .map(|val| {
                    let repetitions = elem_count / val.element_count().max(1);
                    val.into_elements()
                        .flat_map(move |elem| repeat(elem).take(repetitions))
                })
                .collect();
            for _ in 0..elem_count {
                for arg in arg_elems.iter_mut().rev() {
                    env.push(arg.next().unwrap());
                }
                env.exec(f.clone())?;
                for i in 0..outputs {
                    new_values[i].push(env.pop("each's function result")?);
                }
            }
        }
        Ok(())
    })?;
    for new_values in new_values.into_iter().rev() {
        let mut new_shape = new_shape.clone();
        let mut eached = Value::from_row_values(new_values, env)?;
        if is_empty {
            eached.pop_row();
        }
        new_shape.extend_from_slice(&eached.shape().row());
        *eached.shape_mut() = new_shape;
        eached.set_per_meta(per_meta.clone());
        env.push(eached);
    }
    Ok(())
}

pub fn rows(f: SigNode, inv: bool, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    match f.sig.args() {
        0 => env.without_fill(|env| env.exec(f)),
        1 => rows1(f, env.pop(1)?, inv, env),
        2 => rows2(f, env.pop(1)?, env.pop(2)?, inv, env),
        n => {
            let mut args = Vec::with_capacity(n);
            for i in 0..n {
                args.push(env.pop(i + 1)?);
            }
            rowsn(f, args, inv, env)
        }
    }
}

fn collect_outputs(
    outputs: MultiOutput<Vec<Value>>,
    is_scalar: bool,
    is_empty: bool,
    per_meta: PersistentMeta,
    env: &mut Uiua,
) -> UiuaResult {
    for new_rows in outputs.into_iter().rev() {
        let mut val = Value::from_row_values(new_rows, env)?;
        if is_scalar {
            val.undo_fix();
        } else if is_empty {
            val.pop_row();
        }
        val.set_per_meta(per_meta.clone());
        env.push(val);
    }
    Ok(())
}

pub fn rows1(f: SigNode, mut xs: Value, inv: bool, env: &mut Uiua) -> UiuaResult {
    if !inv {
        if let Some((f, d)) = f_mon_fast_fn(&f.node, env) {
            let maybe_through_boxes = matches!(&xs, Value::Box(arr) if arr.rank() <= d + 1);
            if !maybe_through_boxes {
                let val = f(xs, d + 1, env)?;
                env.push(val);
                return Ok(());
            }
        } else if let Some((f, d)) = f_mon2_fast_fn(&f.node, env) {
            let maybe_through_boxes = matches!(&xs, Value::Box(arr) if arr.rank() <= d + 1);
            if !maybe_through_boxes {
                let (xs, ys) = f(xs, d + 1, env)?;
                env.push(ys);
                env.push(xs);
                return Ok(());
            }
        }
    }
    let outputs = f.sig.outputs();
    let is_scalar = xs.rank() == 0;
    let is_empty = outputs > 0 && xs.row_count() == 0;
    let mut new_rows = multi_output(
        outputs,
        Vec::with_capacity(xs.row_count() + is_empty as usize),
    );
    let mut per_meta = xs.take_per_meta();
    env.without_fill(|env| -> UiuaResult {
        if is_empty {
            if push_empty_rows_value(&f, [&xs], inv, &mut per_meta, env) {
                new_rows.clear();
            } else {
                env.push(xs.proxy_row(env));
                _ = env.exec_maintain_sig(f);
                for i in 0..outputs {
                    new_rows[i].push(env.pop("rows' function result")?.boxed_if(inv));
                }
            }
        } else {
            for row in xs.into_rows() {
                env.push(row.unboxed_if(inv));
                env.exec(f.clone())?;
                for i in 0..outputs {
                    new_rows[i].push(env.pop("rows' function result")?.boxed_if(inv));
                }
            }
        }
        Ok(())
    })?;
    collect_outputs(new_rows, is_scalar, is_empty, per_meta, env)
}

fn rows2(f: SigNode, mut xs: Value, mut ys: Value, inv: bool, env: &mut Uiua) -> UiuaResult {
    let outputs = f.sig.outputs();
    let both_scalar = xs.rank() == 0 && ys.rank() == 0;
    match (xs.row_count(), ys.row_count()) {
        (_, 1) => {
            ys.undo_fix();
            ys = ys.unboxed_if(inv);
            let is_empty = outputs > 0 && xs.row_count() == 0;
            let mut new_rows = multi_output(outputs, Vec::with_capacity(xs.row_count()));
            let mut per_meta = xs.take_per_meta();
            env.without_fill(|env| -> UiuaResult {
                if is_empty {
                    ys.fix();
                    if push_empty_rows_value(&f, [&xs, &ys], inv, &mut per_meta, env) {
                        new_rows.clear();
                    } else {
                        ys.undo_fix();
                        env.push(ys);
                        env.push(xs.proxy_row(env));
                        _ = env.exec_maintain_sig(f);
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?.boxed_if(inv));
                        }
                    }
                } else {
                    for x in xs.into_rows() {
                        env.push(ys.clone());
                        env.push(x.unboxed_if(inv));
                        env.exec(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?.boxed_if(inv));
                        }
                    }
                }
                Ok(())
            })?;
            collect_outputs(new_rows, both_scalar, is_empty, per_meta, env)
        }
        (1, _) => {
            xs.undo_fix();
            xs = xs.unboxed_if(inv);
            let is_empty = outputs > 0 && ys.row_count() == 0;
            let mut new_rows = multi_output(outputs, Vec::with_capacity(ys.row_count()));
            let mut per_meta = ys.take_per_meta();
            env.without_fill(|env| -> UiuaResult {
                if is_empty {
                    xs.fix();
                    if push_empty_rows_value(&f, [&xs, &ys], inv, &mut per_meta, env) {
                        new_rows.clear();
                    } else {
                        xs.undo_fix();
                        env.push(ys.proxy_row(env));
                        env.push(xs);
                        _ = env.exec_maintain_sig(f);
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?.boxed_if(inv));
                        }
                    }
                } else {
                    for y in ys.into_rows() {
                        env.push(y.unboxed_if(inv));
                        env.push(xs.clone());
                        env.exec(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?.boxed_if(inv));
                        }
                    }
                }
                Ok(())
            })?;
            collect_outputs(new_rows, both_scalar, is_empty, per_meta, env)
        }
        (a, b) => {
            if a != b {
                return Err(env.error(format!(
                    "Cannot {} arrays with different number of rows {a} and {b}",
                    if inv {
                        Primitive::Inventory
                    } else {
                        Primitive::Rows
                    }
                    .format(),
                )));
            }
            if !inv {
                if let Some((f, a, b)) = f_dy_fast_fn(
                    f.node.as_slice(),
                    env.fill().value_for(&xs).is_some(),
                    env.fill().value_for(&ys).is_some(),
                ) {
                    let val = f(xs, ys, a + 1, b + 1, env)?;
                    env.push(val);
                    return Ok(());
                }
            }
            let is_empty = outputs > 0 && (xs.row_count() == 0 || ys.row_count() == 0);
            let mut new_rows = multi_output(
                outputs,
                Vec::with_capacity(xs.row_count() + is_empty as usize),
            );
            let mut per_meta = xs.take_per_meta().xor(ys.take_per_meta());
            env.without_fill(|env| -> UiuaResult {
                if is_empty {
                    if push_empty_rows_value(&f, [&xs, &ys], inv, &mut per_meta, env) {
                        new_rows.clear();
                    } else {
                        env.push(if ys.row_count() == 0 {
                            ys.proxy_row(env)
                        } else {
                            ys
                        });
                        env.push(if xs.row_count() == 0 {
                            xs.proxy_row(env)
                        } else {
                            xs
                        });
                        _ = env.exec_maintain_sig(f);
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?.boxed_if(inv));
                        }
                    }
                } else {
                    for (x, y) in xs.into_rows().zip(ys.into_rows()) {
                        env.push(y.unboxed_if(inv));
                        env.push(x.unboxed_if(inv));
                        env.exec(f.clone())?;
                        for i in 0..outputs {
                            new_rows[i].push(env.pop("rows's function result")?.boxed_if(inv));
                        }
                    }
                }
                Ok(())
            })?;
            collect_outputs(new_rows, both_scalar, is_empty, per_meta, env)
        }
    }
}

fn rowsn(f: SigNode, args: Vec<Value>, inv: bool, env: &mut Uiua) -> UiuaResult {
    let outputs = f.sig.outputs();
    let prim = if inv {
        Primitive::Inventory
    } else {
        Primitive::Rows
    };
    let FixedRowsData {
        mut rows,
        row_count,
        is_empty,
        all_scalar,
        per_meta,
        ..
    } = fixed_rows(prim.format(), outputs, args, env)?;
    let mut new_values = multi_output(outputs, Vec::new());
    env.without_fill(|env| -> UiuaResult {
        for _ in 0..row_count {
            for arg in rows.iter_mut().rev() {
                match arg {
                    Ok(rows) => env.push(rows.next().unwrap().unboxed_if(inv)),
                    Err(row) => env.push(row.clone().unboxed_if(inv)),
                }
            }
            env.exec(f.clone())?;
            for i in 0..outputs {
                new_values[i].push(env.pop("rows's function result")?.boxed_if(inv));
            }
        }
        Ok(())
    })?;
    for new_values in new_values.into_iter().rev() {
        let mut rowsed = Value::from_row_values(new_values, env)?;
        if all_scalar {
            rowsed.undo_fix();
        } else if is_empty {
            rowsed.pop_row();
        }
        rowsed.validate();
        rowsed.set_per_meta(per_meta.clone());
        env.push(rowsed);
    }
    Ok(())
}

pub fn reduce_conjoin_inventory(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    if f.sig.outputs() != 1 {
        return Err(env.error(format!(
            "{}'s function does not return a single value. \
            This is a bug in the interpreter.",
            ImplPrimitive::ReduceConjoinInventory
        )));
    }
    let mut args = Vec::with_capacity(f.sig.args());
    for i in 0..f.sig.args() {
        args.push(env.pop(i + 1)?);
    }
    let FixedRowsData {
        mut rows,
        row_count,
        ..
    } = fixed_rows(Primitive::Inventory.format(), 1, args, env)?;
    let mut acc = if let Some(fv) = env.value_fill() {
        fv.value.clone()
    } else if row_count == 0 {
        env.push(Array::<Boxed>::default());
        return Ok(());
    } else {
        for arg in rows.iter_mut().rev() {
            match arg {
                Ok(rows) => env.push(rows.next().unwrap().unboxed()),
                Err(row) => env.push(row.clone().unboxed()),
            }
        }
        env.without_fill(|env| -> UiuaResult<Value> {
            env.exec(f.clone())?;
            env.pop("accumulator")
        })?
    };
    env.without_fill(|env| -> UiuaResult {
        for _ in 1..row_count {
            for arg in rows.iter_mut().rev() {
                match arg {
                    Ok(rows) => env.push(rows.next().unwrap().unboxed()),
                    Err(row) => env.push(row.clone().unboxed()),
                }
            }
            env.exec(f.clone())?;
            let item = env.pop("accumulator")?;
            acc = acc.join(item, true, env)?;
        }
        env.push(acc);
        Ok(())
    })
}
