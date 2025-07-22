//! Primitive definitions and top-level implementations
//!
//! For the meat of the actual array algorithms, see [`crate::algorithm`].

use ecow::EcoVec;
use regex::Regex;

use core::str;
use std::{
    borrow::{BorrowMut, Cow},
    cell::RefCell,
    collections::HashMap,
    f64::consts::{PI, TAU},
    iter::repeat_n,
    sync::{
        atomic::{self, AtomicUsize},
        OnceLock,
    },
};

use rand::prelude::*;

use crate::{
    algorithm::{self, ga::GaOp, loops, reduce, table, zip, *},
    array::Array,
    boxed::Boxed,
    grid_fmt::GridFmt,
    media, run_sys_op, run_sys_op_mod,
    value::*,
    FunctionId, ImplPrimitive, Ops, Primitive, Shape, SubSide, SysOp, Uiua, UiuaErrorKind,
    UiuaResult,
};

macro_rules! constant {
    ($name:ident, $value:expr) => {
        fn $name() -> Value {
            thread_local! {
                #[allow(non_upper_case_globals)]
                static $name: Value = $value.into();
            }
            $name.with(Value::clone)
        }
    };
}

constant!(eta, PI / 2.0);
constant!(pi, PI);
constant!(tau, TAU);
constant!(inf, f64::INFINITY);

macro_rules! fill {
    ($ops:expr, $side:expr, $env:expr, $with:ident, $without_but:ident) => {{
        let env = $env;
        let [fill, f] = get_ops($ops, env)?;
        let outputs = fill.sig.outputs();
        if outputs > 1 {
            return Err(env.error(format!(
                "{} function can have at most 1 output, but its signature is {}",
                Primitive::Fill.format(),
                fill.sig
            )));
        }
        if outputs == 0 {
            return env.$without_but(fill.sig.args(), |env| env.exec(fill), |env| env.exec(f));
        }
        env.exec(fill)?;
        let fill_value = env.pop("fill value")?;
        env.$with(fill_value, $side.into(), |env| env.exec(f))?;
    }};
}

/// Execute a primitive as a function
pub fn run_prim_func(prim: &Primitive, env: &mut Uiua) -> UiuaResult {
    match prim {
        Primitive::Eta => env.push(eta()),
        Primitive::Pi => env.push(pi()),
        Primitive::Tau => env.push(tau()),
        Primitive::Infinity => env.push(inf()),
        Primitive::Identity => env.touch_stack(1)?,
        Primitive::Not => env.monadic_env(Value::not)?,
        Primitive::Neg => env.monadic_env(Value::neg)?,
        Primitive::Abs => env.monadic_env(Value::abs)?,
        Primitive::Sign => env.monadic_env(Value::sign)?,
        Primitive::Sqrt => env.monadic_env(Value::sqrt)?,
        Primitive::Exp => env.monadic_env(Value::exp)?,
        Primitive::Sin => env.monadic_env(Value::sin)?,
        Primitive::Floor => env.monadic_env(Value::floor)?,
        Primitive::Ceil => env.monadic_env(Value::ceil)?,
        Primitive::Round => env.monadic_env(Value::round)?,
        Primitive::Eq => env.dyadic_oo_env(Value::is_eq)?,
        Primitive::Ne => env.dyadic_oo_env(Value::is_ne)?,
        Primitive::Lt => env.dyadic_oo_env(Value::other_is_lt)?,
        Primitive::Le => env.dyadic_oo_env(Value::other_is_le)?,
        Primitive::Gt => env.dyadic_oo_env(Value::other_is_gt)?,
        Primitive::Ge => env.dyadic_oo_env(Value::other_is_ge)?,
        Primitive::Add => env.dyadic_oo_env(Value::add)?,
        Primitive::Sub => env.dyadic_oo_env(Value::sub)?,
        Primitive::Mul => env.dyadic_oo_env(Value::mul)?,
        Primitive::Div => env.dyadic_oo_env(Value::div)?,
        Primitive::Modulus => env.dyadic_oo_env(Value::modulus)?,
        Primitive::Or => env.dyadic_oo_env(Value::or)?,
        Primitive::Pow => env.dyadic_oo_env(Value::pow)?,
        Primitive::Log => env.dyadic_oo_env(Value::log)?,
        Primitive::Min => env.dyadic_oo_env(Value::min)?,
        Primitive::Max => env.dyadic_oo_env(Value::max)?,
        Primitive::Atan => env.dyadic_oo_env(Value::atan2)?,
        Primitive::Complex => env.dyadic_oo_env(Value::complex)?,
        Primitive::Match => env.dyadic_rr(|a, b| a == b)?,
        Primitive::Join => env.dyadic_oo_env(|a, b, env| a.join(b, true, env))?,
        Primitive::Transpose => env.monadic_mut(Value::transpose)?,
        Primitive::Keep => env.dyadic_oo_env(Value::keep)?,
        Primitive::Take => env.dyadic_oo_env(Value::take)?,
        Primitive::Drop => env.dyadic_oo_env(Value::drop)?,
        Primitive::Rotate => {
            let amnt = env.pop(1)?;
            let mut val = env.pop(2)?;
            amnt.rotate(&mut val, env)?;
            env.push(val);
        }
        Primitive::Orient => env.dyadic_ro_env(|a, mut b, env| {
            a.orient(&mut b, env)?;
            Ok(b)
        })?,
        Primitive::Couple => env.dyadic_oo_env(|a, b, env| a.couple(b, true, env))?,
        Primitive::Sort => env.monadic_mut(Value::sort_up)?,
        Primitive::Rise => env.monadic_ref(Value::rise)?,
        Primitive::Fall => env.monadic_ref(Value::fall)?,
        Primitive::Pick => env.dyadic_oo_env(Value::pick)?,
        Primitive::Select => env.dyadic_oo_env(Value::select)?,
        Primitive::Where => env.monadic_ref_env(Value::wher)?,
        Primitive::Classify => env.monadic_ref(Value::classify)?,
        Primitive::Occurrences => env.monadic_ref(Value::occurrences)?,
        Primitive::Deduplicate => env.monadic_mut_env(Value::deduplicate)?,
        Primitive::Unique => env.monadic_ref(Value::unique)?,
        Primitive::MemberOf => env.dyadic_rr_env(Value::memberof)?,
        Primitive::Find => env.dyadic_rr_env(Value::find)?,
        Primitive::Mask => env.dyadic_rr_env(Value::mask)?,
        Primitive::IndexOf => env.dyadic_rr_env(Value::index_of)?,
        Primitive::ProgressiveIndexOf => env.dyadic_rr_env(Value::progressive_index_of)?,
        Primitive::Box => {
            let val = env.pop(1)?;
            if val.box_nesting() > 1000 {
                return Err(env.error("Box nesting too deep"));
            }
            env.push(val.box_depth(0));
        }
        Primitive::EncodeBytes => {
            let format = env.pop(1)?;
            let value = env.pop(2)?;
            let bytes = format.encode_bytes(value, None, env)?;
            env.push(bytes);
        }
        Primitive::Repr => env.monadic_ref(Value::representation)?,
        Primitive::Pretty => {
            let val = env.pop(1)?;
            let pretty = val.grid_string(true);
            let width = pretty
                .lines()
                .map(|line| line.chars().count())
                .max()
                .unwrap_or(0);
            let height = pretty.lines().count();
            let shape = Shape::from([height, width]);
            let mut data = EcoVec::with_capacity(shape.elements());
            for line in pretty.lines() {
                data.extend(line.chars());
                while data.len() % width != 0 {
                    data.push(' ');
                }
            }
            env.push(Array::new(shape, data));
        }
        Primitive::Parse => env.monadic_env(Value::parse_num)?,
        Primitive::Utf8 => env.monadic_ref_env(Value::utf8)?,
        Primitive::Graphemes => env.monadic_ref_env(Value::graphemes)?,
        Primitive::Range => env.monadic_ref_env(Value::range)?,
        Primitive::Reverse => env.monadic_mut(Value::reverse)?,
        Primitive::Deshape => env.monadic_mut(Value::deshape)?,
        Primitive::Fix => env.monadic_mut(Value::fix)?,
        Primitive::First => env.monadic_env(Value::first)?,
        Primitive::Last => env.monadic_env(Value::last)?,
        Primitive::Len => env.monadic_ref(Value::row_count)?,
        Primitive::Shape => env.monadic_ref(|v| v.shape.iter().copied().collect::<Value>())?,
        Primitive::Bits => env.monadic_ref_env(|val, env| val.bits(None, env))?,
        Primitive::Base => env.dyadic_rr_env(Value::base)?,
        Primitive::Reshape => {
            let shape = env.pop(1)?;
            let mut array = env.pop(2)?;
            array.reshape(&shape, env)?;
            env.push(array);
        }
        Primitive::Rerank => {
            let rank = env.pop(1)?;
            let mut array = env.pop(2)?;
            array.rerank(&rank, env)?;
            env.push(array);
        }
        Primitive::Dup => {
            let x = env.pop(1)?;
            env.push(x.clone());
            env.push(x);
        }
        Primitive::Flip => {
            let a = env.pop(1)?;
            let b = env.pop(2)?;
            env.push(a);
            env.push(b);
        }
        Primitive::Pop => {
            env.pop(1)?;
        }
        Primitive::Assert => {
            let msg = env.pop(1)?;
            let cond = env.pop(2)?;
            if !cond.as_nat(env, None).is_ok_and(|n| n == 1) {
                return Err(UiuaErrorKind::Throw(
                    msg.into(),
                    env.span().clone(),
                    env.asm.inputs.clone().into(),
                )
                .into());
            }
        }
        Primitive::Rand => env.push(random()),
        Primitive::Gen => env.dyadic_rr_env(Value::gen)?,
        Primitive::Tag => {
            static NEXT_TAG: AtomicUsize = AtomicUsize::new(0);
            let tag = NEXT_TAG.fetch_add(1, atomic::Ordering::Relaxed);
            env.push(tag);
        }
        Primitive::Type => {
            let val = env.pop(1)?;
            env.push(val.type_id());
        }
        Primitive::Wait => {
            let id = env.pop(1)?;
            env.wait(id)?;
        }
        Primitive::Send => {
            let id = env.pop(1)?;
            let val = env.pop(2)?;
            env.send(id, val)?;
        }
        Primitive::Recv => {
            let id = env.pop(1)?;
            env.recv(id)?;
        }
        Primitive::TryRecv => {
            let id = env.pop(1)?;
            env.try_recv(id)?;
        }
        Primitive::Now => env.push(env.rt.backend.now()),
        Primitive::TimeZone => {
            let o = env.rt.backend.timezone().map_err(|e| env.error(e))?;
            env.push(o);
        }
        Primitive::DateTime => env.monadic_ref_env(Value::datetime)?,
        Primitive::Insert => {
            let key = env.pop("key")?;
            let val = env.pop("value")?;
            let mut map = env.pop("map")?;
            map.insert(key, val, env)?;
            env.push(map);
        }
        Primitive::Has => {
            let key = env.pop("key")?;
            let map = env.pop("map")?;
            env.push(map.has_key(&key, env)?);
        }
        Primitive::Get => {
            let key = env.pop("key")?;
            let map = env.pop("map")?;
            let val = map.get(&key, env)?;
            env.push(val);
        }
        Primitive::Remove => {
            let key = env.pop("key")?;
            let mut map = env.pop("map")?;
            map.remove(key, env)?;
            env.push(map);
        }
        Primitive::Map => {
            let keys = env.pop("keys")?;
            let mut vals = env.pop("values")?;
            vals.map(keys, env)?;
            env.push(vals);
        }
        Primitive::Stack => stack(env, false)?,
        Primitive::Regex => regex(env)?,
        Primitive::Hsv => env.monadic_env(Value::rgb_to_hsv)?,
        Primitive::Json => env.monadic_ref_env(Value::to_json_string)?,
        Primitive::Binary => env.monadic_ref_env(Value::to_binary)?,
        Primitive::Csv => env.monadic_ref_env(Value::to_csv)?,
        Primitive::Xlsx => {
            env.monadic_ref_env(|value, env| value.to_xlsx(env).map(EcoVec::from))?
        }
        Primitive::ImageEncode => media::image_encode(env)?,
        Primitive::GifEncode => media::gif_encode(env)?,
        Primitive::ApngEncode => media::apng_encode(env)?,
        Primitive::AudioEncode => media::audio_encode(env)?,
        Primitive::Voxels => {
            let val = env.pop(1)?;
            let res = media::voxels(&val, env)?;
            env.push(res);
        }
        Primitive::Layout => {
            let size = env.pop(1)?;
            let text = env.pop(2)?;
            let res = media::layout_text(size, text, env)?;
            env.push(res);
        }
        Primitive::Fft => algorithm::fft(env)?,
        Primitive::Quote
        | Primitive::Comptime
        | Primitive::Un
        | Primitive::Anti
        | Primitive::Under
        | Primitive::Obverse
        | Primitive::Switch => {
            return Err(env.error(format!(
                "{} was not inlined. This is a bug in the interpreter",
                prim.format()
            )))
        }
        Primitive::Os => env.push(std::env::consts::OS),
        Primitive::OsFamily => env.push(std::env::consts::FAMILY),
        Primitive::Arch => env.push(std::env::consts::ARCH),
        Primitive::DllExt => env.push(std::env::consts::DLL_EXTENSION),
        Primitive::ExeExt => env.push(std::env::consts::EXE_EXTENSION),
        Primitive::PathSep => env.push(std::path::MAIN_SEPARATOR),
        Primitive::NumProcs => env.push(num_cpus::get()),
        Primitive::Sys(op) => run_sys_op(op, env)?,
        prim => {
            return Err(env.error(if prim.modifier_args().is_some() {
                format!(
                    "{} was not handled as a modifier. \
                        This is a bug in the interpreter",
                    prim.format()
                )
            } else {
                format!(
                    "{} was not handled as a function. \
                        This is a bug in the interpreter",
                    prim.format()
                )
            }))
        }
    }
    Ok(())
}
/// Execute a primitive as a modifier
pub fn run_prim_mod(prim: &Primitive, ops: Ops, env: &mut Uiua) -> UiuaResult {
    match prim {
        // Looping
        Primitive::Reduce => reduce::reduce(ops, 0, env)?,
        Primitive::Scan => reduce::scan(ops, env)?,
        Primitive::Fold => reduce::fold(ops, env)?,
        Primitive::Each => zip::each(ops, env)?,
        Primitive::Rows => {
            let [f] = get_ops(ops, env)?;
            zip::rows(f, 0, false, env)?
        }
        Primitive::Inventory => {
            let [f] = get_ops(ops, env)?;
            zip::rows(f, 0, true, env)?
        }
        Primitive::Table => table::table(ops, env)?,
        Primitive::Repeat => loops::repeat(ops, false, false, env)?,
        Primitive::Do => loops::do_(ops, env)?,
        Primitive::Group => {
            let [f] = get_ops(ops, env)?;
            groups::group(f, env)?
        }
        Primitive::Partition => {
            let [f] = get_ops(ops, env)?;
            groups::partition(f, env)?
        }
        Primitive::Tuples => tuples::tuples(ops, env)?,
        Primitive::Stencil => stencil::stencil(ops, env)?,

        // Stack
        Primitive::Fork => {
            let [f, g] = get_ops(ops, env)?;
            let f_args = env.prepare_fork(f.sig.args(), g.sig.args())?;
            env.exec(g)?;
            env.push_all(f_args);
            env.exec(f)?;
        }
        Primitive::Bracket => {
            let [f, g] = get_ops(ops, env)?;
            let vals = env.pop_n(f.sig.args())?;
            env.exec(g)?;
            env.push_all(vals);
            env.exec(f)?;
        }
        Primitive::Both => {
            let [f] = get_ops(ops, env)?;
            let vals = env.pop_n(f.sig.args())?;
            env.exec(f.node.clone())?;
            env.push_all(vals);
            env.exec(f.node)?;
        }
        Primitive::Dip => {
            let [f] = get_ops(ops, env)?;
            let val = env.pop(1)?;
            env.exec(f)?;
            env.push(val);
        }
        Primitive::On => {
            let [f] = get_ops(ops, env)?;
            let val = env.copy_nth(0)?;
            env.exec(f)?;
            env.push(val);
        }
        Primitive::By => {
            let [f] = get_ops(ops, env)?;
            env.dup_values(1, f.sig.args().max(1))?;
            env.exec(f)?;
        }
        Primitive::Above => {
            let [f] = get_ops(ops, env)?;
            let vals = env.copy_n(f.sig.args())?;
            env.exec(f)?;
            env.push_all(vals);
        }
        Primitive::Below => {
            let [f] = get_ops(ops, env)?;
            env.dup_values(f.sig.args(), f.sig.args())?;
            env.exec(f)?;
        }
        Primitive::With => {
            let [f] = get_ops(ops, env)?;
            let val = env.copy_nth(f.sig.args() - 1)?;
            env.exec(f)?;
            env.push(val);
        }
        Primitive::Off => {
            let [f] = get_ops(ops, env)?;
            let val = env.copy_nth(0)?;
            env.exec(f.node)?;
            env.push(val);
            env.rotate_up(1, f.sig.outputs() + 1)?;
        }
        Primitive::Content => {
            let [f] = get_ops(ops, env)?;
            for val in env.n_mut(f.sig.args())? {
                val.unbox();
            }
            env.exec(f)?;
        }

        // Misc
        Primitive::Fill => fill!(ops, None, env, with_fill, without_fill_but),
        Primitive::Try => algorithm::try_(ops, env)?,
        Primitive::Case => {
            let [f] = get_ops(ops, env)?;
            env.exec(f).map_err(|mut e| {
                e.meta.is_case = true;
                e
            })?;
        }
        Primitive::Dump => dump(ops, env, false)?,
        Primitive::Path => {
            let [neighbors, is_goal] = get_ops(ops, env)?;
            path::path(neighbors, is_goal, None, env)?;
        }
        Primitive::Memo => {
            let [f] = get_ops(ops, env)?;
            let mut args = Vec::with_capacity(f.sig.args());
            for i in 0..f.sig.args() {
                args.push(env.pop(i + 1)?);
            }
            let mut memo = env.rt.memo.get_or_default().borrow_mut();
            if let Some(f_memo) = memo.get_mut(&f.node) {
                if let Some(outputs) = f_memo.get(&args) {
                    let outputs = outputs.clone();
                    drop(memo);
                    for val in outputs {
                        env.push(val);
                    }
                    return Ok(());
                }
            }
            drop(memo);
            for arg in args.iter().rev() {
                env.push(arg.clone());
            }
            env.exec(f.node.clone())?;
            let outputs = env.clone_stack_top(f.sig.outputs())?;
            let mut memo = env.rt.memo.get_or_default().borrow_mut();
            memo.borrow_mut()
                .entry(f.node)
                .or_default()
                .insert(args, outputs.clone());
        }
        Primitive::Spawn => {
            let [f] = get_ops(ops, env)?;
            env.spawn(false, f)?;
        }
        Primitive::Pool => {
            let [f] = get_ops(ops, env)?;
            env.spawn(true, f)?;
        }
        Primitive::Sys(op) => run_sys_op_mod(op, ops, env)?,
        prim => {
            return Err(env.error(if prim.modifier_args().is_some() {
                format!(
                    "{} was not handled as a modifier. \
                        This is a bug in the interpreter",
                    prim.format()
                )
            } else {
                format!(
                    "{} was called as a modifier. \
                        This is a bug in the interpreter",
                    prim.format()
                )
            }))
        }
    }
    Ok(())
}

impl ImplPrimitive {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            ImplPrimitive::DeshapeSub(i) => {
                env.monadic_mut_env(|val, env| val.deshape_sub(*i, 0, true, env))?
            }
            &ImplPrimitive::ParseSub(i) => {
                let strs = env.pop(1)?;
                let nums = strs.parse_base(i, env)?;
                env.push(nums);
            }
            &ImplPrimitive::SidedEncodeBytes(side) => {
                let format = env.pop(1)?;
                let value = env.pop(2)?;
                let bytes = format.encode_bytes(value, Some(side), env)?;
                env.push(bytes);
            }
            &ImplPrimitive::DecodeBytes(side) => {
                let format = env.pop(1)?;
                let bytes = env.pop(2)?;
                let value = format.decode_bytes(bytes, side, env)?;
                env.push(value);
            }
            ImplPrimitive::NBits(n) => {
                let val = env.pop(1)?;
                let bits = val.bits(Some(*n), env)?;
                env.push(bits);
            }
            ImplPrimitive::Root => env.dyadic_oo_env(Value::root)?,
            ImplPrimitive::Cos => env.monadic_env(Value::cos)?,
            ImplPrimitive::Asin => env.monadic_env(Value::asin)?,
            ImplPrimitive::Acos => env.monadic_env(Value::acos)?,
            ImplPrimitive::Ln => env.monadic_env(Value::ln)?,
            ImplPrimitive::UnPop => {
                let fv = (env.last_fill()).ok_or_else(|| env.error("No fill set").fill())?;
                env.push(fv.value.clone());
            }
            ImplPrimitive::UnCouple => {
                let coupled = env.pop(1)?;
                let (a, b) = coupled.uncouple(env)?;
                env.push(b);
                env.push(a);
            }
            ImplPrimitive::UnMap => {
                let map = env.pop(1)?;
                let (keys, vals) = map.unmap(env)?;
                env.push(vals);
                env.push(keys);
            }
            ImplPrimitive::UnWhere => env.monadic_ref_env(Value::unwhere)?,
            ImplPrimitive::Utf16 => env.monadic_ref_env(Value::utf16)?,
            ImplPrimitive::UnUtf8 => env.monadic_ref_env(Value::unutf8)?,
            ImplPrimitive::UnUtf16 => env.monadic_ref_env(Value::unutf16)?,
            ImplPrimitive::UnGraphemes => env.monadic_env(Value::ungraphemes)?,
            ImplPrimitive::UnBits => env.monadic_ref_env(Value::unbits)?,
            ImplPrimitive::AntiDrop => env.dyadic_ro_env(Value::anti_drop)?,
            ImplPrimitive::AntiSelect => env.dyadic_oo_env(Value::anti_select)?,
            ImplPrimitive::AntiRotate => {
                let amnt = env.pop(1)?;
                let mut val = env.pop(2)?;
                amnt.anti_rotate(&mut val, env)?;
                env.push(val);
            }
            ImplPrimitive::AntiPick => env.dyadic_oo_env(Value::anti_pick)?,
            ImplPrimitive::AntiKeep => env.dyadic_oo_env(Value::anti_keep)?,
            ImplPrimitive::UnJoin => {
                let val = env.pop(1)?;
                let (first, rest) = val.unjoin(env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnJoinEnd => {
                let val = env.pop(1)?;
                let (first, rest) = val.unjoin_end(env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnJoinShape => {
                let shape = (env.pop(1))?.as_nats(env, "Shape must be natural numbers")?;
                let val = env.pop(2)?;
                let (first, rest) = val.unjoin_shape(&shape, None, false, env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnJoinShapeEnd => {
                let shape = (env.pop(1))?.as_nats(env, "Shape must be natural numbers")?;
                let val = env.pop(2)?;
                let (first, rest) = val.unjoin_shape(&shape, None, true, env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnJoinShape2 => {
                let a_shape = (env.pop(1))?.as_nats(env, "Shape must be natural numbers")?;
                let b_shape = (env.pop(1))?.as_nats(env, "Shape must be natural numbers")?;
                let val = env.pop(2)?;
                let (first, rest) = val.unjoin_shape(&a_shape, Some(&b_shape), false, env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnJoinShape2End => {
                let a_shape = (env.pop(1))?.as_nats(env, "Shape must be natural numbers")?;
                let b_shape = (env.pop(1))?.as_nats(env, "Shape must be natural numbers")?;
                let val = env.pop(2)?;
                let (first, rest) = val.unjoin_shape(&a_shape, Some(&b_shape), true, env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnKeep => {
                let val = env.pop(1)?;
                let (counts, dedup) = val.unkeep(env)?;
                env.push(dedup);
                env.push(counts);
            }
            ImplPrimitive::UnTake => {
                let mut val = env.pop(1)?;
                let taken = val.un_take(env)?;
                env.push(val);
                env.push(taken);
            }
            ImplPrimitive::UnAtan => {
                let x = env.pop(1)?;
                let sin = x.clone().sin(env)?;
                let cos = x.cos(env)?;
                env.push(cos);
                env.push(sin);
            }
            ImplPrimitive::UnComplex => {
                let x = env.pop(1)?;
                let im = x.clone().complex_im(env)?;
                let re = x.complex_re(env)?;
                env.push(re);
                env.push(im);
            }
            ImplPrimitive::UnAdd => {
                let x = env.pop(1)?;
                let (frac, whole) = x.un_add(env)?;
                env.push(whole);
                env.push(frac);
            }
            ImplPrimitive::UnMul => {
                let x = env.pop(1)?;
                let (sign, mag) = x.un_mul()?;
                env.push(mag);
                env.push(sign);
            }
            ImplPrimitive::UnDiv => {
                let x = env.pop(1)?;
                let (num, denom) = x.un_div(env)?;
                env.push(num);
                env.push(denom);
            }
            ImplPrimitive::UnParse => env.monadic_env(Value::unparse)?,
            &ImplPrimitive::UnParseSub(i) => {
                let nums = env.pop(1)?;
                let strs = nums.unparse_base(i, env)?;
                env.push(strs);
            }
            ImplPrimitive::UnFix => env.monadic_mut_env(Value::unfix)?,
            ImplPrimitive::UnShape => env.monadic_ref_env(Value::unshape)?,
            ImplPrimitive::StackN { n, inverse } => stack_n(env, *n, *inverse)?,
            ImplPrimitive::UnStack => stack(env, true)?,
            ImplPrimitive::Primes => env.monadic_ref_env(Value::primes)?,
            ImplPrimitive::UnBox => {
                let val = env.pop(1)?;
                env.push(val.unboxed());
            }
            ImplPrimitive::UnSort => env.monadic_mut(Value::shuffle)?,
            ImplPrimitive::UnHsv => env.monadic_env(Value::hsv_to_rgb)?,
            ImplPrimitive::UnJson => {
                let json = env.pop(1)?.as_string(env, "JSON expects a string")?;
                let val = Value::from_json_string(&json, env)?;
                env.push(val);
            }
            ImplPrimitive::UnBinary => {
                let bytes = env.pop(1)?.as_bytes(env, "Binary expects bytes")?;
                let val = Value::from_binary(&bytes, env)?;
                env.push(val);
            }
            ImplPrimitive::UnCsv => {
                let csv = env.pop(1)?.as_string(env, "CSV expects a string")?;
                let val = Value::from_csv(&csv, env)?;
                env.push(val);
            }
            ImplPrimitive::UnXlsx => {
                let xlsx = env.pop(1)?.as_bytes(env, "XLSX expects bytes")?;
                let val = Value::from_xlsx(&xlsx, env)?;
                env.push(val);
            }
            ImplPrimitive::UnFft => algorithm::unfft(env)?,
            ImplPrimitive::UnDatetime => env.monadic_ref_env(Value::undatetime)?,
            ImplPrimitive::ImageDecode => media::image_decode(env)?,
            ImplPrimitive::GifDecode => media::gif_decode(env)?,
            ImplPrimitive::AudioDecode => media::audio_decode(env)?,
            ImplPrimitive::UnRawMode => {
                let raw_mode = env.rt.backend.get_raw_mode().map_err(|e| env.error(e))?;
                env.push(raw_mode);
            }
            ImplPrimitive::UnClip => {
                let contents = env.pop(1)?.as_string(env, "Contents must be a string")?;
                (env.rt.backend)
                    .set_clipboard(&contents)
                    .map_err(|e| env.error(e))?;
            }
            ImplPrimitive::MatrixDiv => env.dyadic_rr_env(Value::matrix_div)?,
            ImplPrimitive::RangeStart => env.dyadic_rr_env(Value::range_start)?,
            // Unders
            ImplPrimitive::UndoUnBits => {
                let orig_shape = env.pop(1)?;
                let val = env.pop(2)?;
                env.push(val.undo_un_bits(&orig_shape, env)?);
            }
            ImplPrimitive::AntiBase => env.dyadic_rr_env(Value::antibase)?,
            &ImplPrimitive::UndoReverse { n, all } => {
                env.require_height(n)?;
                let end = env.stack_height() - n;
                let vals = &mut env.stack_mut()[end..];
                if all {
                    for val in vals {
                        val.reverse();
                    }
                } else {
                    let max_rank = vals.iter().map(|v| v.rank()).max().unwrap_or(0);
                    for val in vals {
                        if val.rank() == max_rank {
                            val.reverse();
                        }
                    }
                }
            }
            &ImplPrimitive::UndoTransposeN(n, amnt) => {
                env.touch_stack(n)?;
                let end = env.stack_height() - n;
                let vals = &mut env.stack_mut()[end..];
                let max_rank = vals.iter().map(|v| v.rank()).max().unwrap_or(0);
                for val in vals {
                    if val.rank() == max_rank {
                        val.transpose_depth(0, -amnt);
                    }
                }
            }
            &ImplPrimitive::UndoRotate(n) => {
                env.touch_stack(n + 1)?;
                let mut amount = env.pop(1)?;
                let depth = amount.rank().saturating_sub(1);
                if n == 1 {
                    let mut val = env.pop(2)?;
                    if amount.rank() > 0 && amount.row_count() > val.rank() {
                        amount.drop_n(amount.row_count() - val.rank());
                    }
                    amount.rotate_depth(&mut val, depth, false, env)?;
                    env.push(val);
                } else {
                    let end = env.stack_height() - n;
                    let mut vals = env.truncate_stack(end);
                    let max_rank = vals.iter().map(|v| v.rank()).max().unwrap_or(0);
                    if amount.rank() > 0 && amount.row_count() > max_rank {
                        amount.drop_n(amount.row_count() - max_rank);
                    }
                    for val in &mut vals {
                        let mut amount = amount.clone();
                        if amount.row_count() > 1 {
                            if amount.rank() > 0 && amount.row_count() > val.rank() {
                                amount.drop_n(amount.row_count() - val.rank());
                            }
                            amount.rotate_depth(val, depth, false, env)?;
                        } else if val.rank() == max_rank {
                            amount.rotate_depth(val, depth, false, env)?;
                        }
                    }
                    for val in vals {
                        env.push(val);
                    }
                }
            }
            ImplPrimitive::UndoPick => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_pick(index, into, env)?);
            }
            ImplPrimitive::UndoSelect => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_select(index, into, env)?);
            }
            ImplPrimitive::UndoWhere => {
                let shape = env.pop(1)?.as_nats(env, "Shape must be natural numbers")?;
                let indices = env.pop(2)?;
                let mask = indices.undo_where(&shape, env)?;
                env.push(mask);
            }
            ImplPrimitive::AntiOrient => env.dyadic_ro_env(Value::anti_orient)?,
            ImplPrimitive::UndoAntiOrient => {
                let indices = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_anti_orient(indices, into, env)?);
            }
            ImplPrimitive::UndoRerank => {
                let rank = env.pop(1)?;
                let shape = Shape::from(
                    env.pop(2)?
                        .as_nats(env, "Shape must be a list of natural numbers")?,
                );
                let mut array = env.pop(3)?;
                array.undo_rerank(&rank, &shape, env)?;
                env.push(array);
            }
            ImplPrimitive::UndoReshape => env.dyadic_ro_env(|orig_shape, mut val, env| {
                val.undo_reshape(orig_shape, env)?;
                Ok(val)
            })?,
            ImplPrimitive::UndoWindows => env.dyadic_ro_env(Value::undo_windows)?,
            ImplPrimitive::UndoFirst => {
                let into = env.pop(1)?;
                let from = env.pop(2)?;
                env.push(from.undo_first(into, env)?);
            }
            ImplPrimitive::UndoLast => {
                let into = env.pop(1)?;
                let from = env.pop(2)?;
                env.push(from.undo_last(into, env)?);
            }
            ImplPrimitive::UndoKeep => {
                let from = env.pop(1)?;
                let counts = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.undo_keep(counts, into, env)?);
            }
            ImplPrimitive::UndoTake => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_take(index, into, env)?);
            }
            ImplPrimitive::UndoDrop => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_drop(index, into, env)?);
            }
            ImplPrimitive::UndoFix => env.monadic_mut(Value::undo_fix)?,
            ImplPrimitive::UndoShape => {
                let mut val = env.pop(1)?;
                let shape = env.pop(2)?;
                val.undo_shape(&shape, env)?;
                env.push(val);
            }
            ImplPrimitive::UndoDeshape(sub) => {
                let shape = Shape::from(
                    env.pop(1)?
                        .as_nats(env, "Shape must be a list of natural numbers")?,
                );
                let mut val = env.pop(2)?;
                val.undo_deshape(*sub, &shape, env)?;
                env.push(val)
            }
            ImplPrimitive::UndoPartition2 => groups::undo_partition_part2(env)?,
            ImplPrimitive::UndoGroup2 => groups::undo_group_part2(env)?,
            ImplPrimitive::UndoJoin => {
                let a_shape = env.pop(1)?;
                let b_shape = env.pop(2)?;
                let val = env.pop(3)?;
                let (left, right) = val.undo_join(a_shape, b_shape, env)?;
                env.push(right);
                env.push(left);
            }
            ImplPrimitive::TryClose => _ = run_sys_op(&SysOp::Close, env),
            ImplPrimitive::UndoInsert => {
                let key = env.pop(1)?;
                let _value = env.pop(2)?;
                let original = env.pop(3)?;
                let mut map = env.pop(4)?;
                map.undo_insert(key, &original, env)?;
                env.push(map);
            }
            ImplPrimitive::UndoRemove => {
                let key = env.pop(1)?;
                let original = env.pop(2)?;
                let mut map = env.pop(3)?;
                map.undo_remove(key, &original, env)?;
                env.push(map);
            }
            &ImplPrimitive::MaxRowCount(n) => {
                let mut max_len: Option<usize> = None;
                let start = env.require_height(n)?;
                for val in &env.stack()[start..] {
                    if val.row_count() != 1 {
                        max_len = Some(max_len.unwrap_or(0).max(val.row_count()));
                    }
                }
                env.push(max_len.unwrap_or(1));
            }
            ImplPrimitive::SetSign => env.dyadic_oo_env(Value::set_sign)?,
            // Optimizations
            ImplPrimitive::AbsComplex => env.dyadic_oo_env(Value::abs_complex)?,
            ImplPrimitive::FirstMinIndex => env.monadic_ref_env(Value::first_min_index)?,
            ImplPrimitive::FirstMaxIndex => env.monadic_ref_env(Value::first_max_index)?,
            ImplPrimitive::LastMinIndex => env.monadic_ref_env(Value::last_min_index)?,
            ImplPrimitive::LastMaxIndex => env.monadic_ref_env(Value::last_max_index)?,
            ImplPrimitive::FirstWhere => env.monadic_ref_env(Value::first_where)?,
            ImplPrimitive::LenWhere => env.monadic_ref_env(Value::len_where)?,
            ImplPrimitive::MemberOfRange => env.dyadic_ro_env(Value::memberof_range)?,
            ImplPrimitive::MultidimMemberOfRange => {
                env.dyadic_ro_env(Value::multidim_memberof_range)?
            }
            ImplPrimitive::RandomRow => env.monadic_ref_env(Value::random_row)?,
            ImplPrimitive::LastWhere => env.monadic_ref_env(Value::last_where)?,
            ImplPrimitive::SortDown => env.monadic_mut(Value::sort_down)?,
            ImplPrimitive::AllSame => env.monadic_ref(Value::all_same)?,
            ImplPrimitive::ReplaceRand => {
                env.pop(1)?;
                env.push(random());
            }
            ImplPrimitive::ReplaceRand2 => {
                env.pop(1)?;
                env.pop(2)?;
                env.push(random());
            }
            ImplPrimitive::CountUnique => env.monadic_ref(Value::count_unique)?,
            ImplPrimitive::MatchPattern => {
                let expected = env.pop(1)?;
                let got = env.pop(2)?;
                match (&expected, &got) {
                    (Value::Num(a), Value::Num(b))
                        if a.shape == b.shape
                            && (a.data.iter().zip(&b.data)).all(|(a, b)| (a - b).abs() < 1e-12) =>
                    {
                        return Ok(())
                    }
                    (Value::Complex(a), Value::Complex(b))
                        if a.shape == b.shape
                            && a.data.iter().zip(&b.data).all(|(a, b)| {
                                (a.re - b.re).abs() < 1e-12 && (a.im - b.im).abs() < 1e-12
                            }) =>
                    {
                        return Ok(())
                    }
                    (a, b) if a == b => return Ok(()),
                    _ => {}
                }
                let message = match (
                    expected.rank() <= 1 && expected.row_count() <= 10,
                    got.rank() <= 1 && got.row_count() <= 10,
                ) {
                    (true, true) => format!(
                        "expected {} but got {}",
                        expected.grid_string(false),
                        got.grid_string(false)
                    ),
                    (true, false) if expected.type_id() != got.type_id() => {
                        format!(
                            "expected {} but got {}",
                            expected.grid_string(false),
                            got.type_name_plural()
                        )
                    }
                    (true, false) if expected.shape != got.shape => format!(
                        "expected {} but got array with shape {}",
                        expected.grid_string(false),
                        got.shape
                    ),
                    (true, false) => format!(
                        "expected {} but found {} array with shape {}",
                        expected.grid_string(false),
                        got.type_name(),
                        got.shape
                    ),
                    (false, true) if expected.type_id() != got.type_id() => {
                        format!(
                            "expected {} but got {}",
                            expected.type_name_plural(),
                            got.grid_string(false)
                        )
                    }
                    (false, true) if expected.shape != got.shape => format!(
                        "expected array with shape {} but got {}",
                        expected.shape,
                        got.grid_string(false)
                    ),
                    (false, true) => format!(
                        "expected {} array with shape {} but got {}",
                        expected.type_name(),
                        expected.shape,
                        got.grid_string(false)
                    ),
                    (false, false) if expected.type_id() != got.type_id() => {
                        format!(
                            "expected {} but got {}",
                            expected.type_name_plural(),
                            got.type_name_plural()
                        )
                    }
                    (false, false) if expected.shape != got.shape => format!(
                        "expected shape {} but got shape {}",
                        expected.shape, got.shape
                    ),
                    (false, false) => {
                        let different =
                            if expected.type_id() == got.type_id() && expected.shape == got.shape {
                                " different"
                            } else {
                                ""
                            };
                        format!(
                            "expected {} array with shape {} but \
                            got{different} {} array with shape {}",
                            expected.type_name(),
                            expected.shape,
                            got.type_name(),
                            got.shape
                        )
                    }
                };
                return Err(env.error(env.error(format!("Pattern match failed: {message}"))));
            }
            ImplPrimitive::MatchLe => {
                let max = env.pop(1)?;
                let val = env.pop(2)?;
                let le = max.clone().other_is_le(val.clone(), env)?;
                if le.all_true() {
                    env.push(val);
                    return Ok(());
                }
                let message = if max.rank() <= 1 && max.row_count() <= 10 {
                    format!("Not all values are {} {max}", Primitive::Le)
                } else {
                    format!("Not all values are {}", Primitive::Le)
                };
                return Err(env.error(env.error(format!("Pattern match failed: {message}"))));
            }
            ImplPrimitive::MatchGe => {
                let min = env.pop(1)?;
                let val = env.pop(2)?;
                let ge = min.clone().other_is_ge(val.clone(), env)?;
                if ge.all_true() {
                    env.push(val);
                    return Ok(());
                }
                let message = if min.rank() <= 1 && min.row_count() <= 10 {
                    format!("Not all values are {} {min}", Primitive::Ge)
                } else {
                    format!("Not all values are {}", Primitive::Ge)
                };
                return Err(env.error(env.error(format!("Pattern match failed: {message}"))));
            }
            ImplPrimitive::Retropose => env.monadic_mut(|val| val.retropose_depth(0))?,
            &ImplPrimitive::TransposeN(n) => env.monadic_mut(|val| val.transpose_depth(0, n))?,
            // Implementation details
            ImplPrimitive::ValidateType | ImplPrimitive::ValidateTypeConsume => {
                let type_num = env
                    .pop(1)?
                    .as_nat(env, "Type number must be a natural number")?;
                let val = env.pop(2)?;
                if val.type_id() as usize != type_num {
                    let found = if val.shape.elements() == 1 {
                        val.type_name()
                    } else {
                        val.type_name_plural()
                    };
                    let expected = match type_num {
                        0 => "numbers",
                        1 => "characters",
                        2 => "boxes",
                        3 => "complex numbers",
                        _ => return Err(env.error(format!("Invalid type number {type_num}"))),
                    };
                    return Err(env.error(format!("Expected {expected} but found {found}")));
                }
                if let ImplPrimitive::ValidateType = self {
                    env.push(val);
                }
            }
            ImplPrimitive::TestAssert => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                let mut res = Ok(());
                if !cond.as_nat(env, None).is_ok_and(|n| n == 1) {
                    res = Err(UiuaErrorKind::Throw(
                        msg.into(),
                        env.span().clone(),
                        env.asm.inputs.clone().into(),
                    )
                    .into());
                }
                env.rt.test_results.push(res);
            }
            ImplPrimitive::ValidateNonBoxedVariant => {
                let val = env.pop(1)?;
                if !matches!(val, Value::Num(_) | Value::Byte(_) | Value::Box(_)) {
                    return Err(env.error(format!(
                        "Non-boxed variant field must be numbers or boxes, but it is {}",
                        val.type_name_plural()
                    )));
                }
                if val.rank() > 0 {
                    return Err(env.error(format!(
                        "Non-boxed variant field must be rank 0, but it is rank {}",
                        val.rank()
                    )));
                }
                env.push(val);
            }
            ImplPrimitive::ValidateVariant => {
                let tag = env.pop(1)?;
                let val = env.pop(2)?;
                if val.row_count() == 0 {
                    return Err(env.error("Variant must have at least one row"));
                }
                if val.rank() == 0 {
                    return Err(env.error(format!("Variant tag is {val} instead of {tag}")));
                }
                let (head, tail) = val.unjoin(env).unwrap();
                let set_tag = head.unboxed();
                if tag != set_tag {
                    return Err(env.error(if set_tag.rank() == 0 {
                        format!("Variant tag is {set_tag} instead of {tag}")
                    } else {
                        format!("Variant tag is rank {} instead of {tag}", set_tag.rank())
                    }));
                }
                env.push(tail);
            }
            ImplPrimitive::TagVariant => {
                let mut tag = env.pop(1)?;
                let val = env.pop(2)?;
                if let Value::Box(_) = &val {
                    tag.box_if_not();
                }
                let res = tag.join(val, false, env)?;
                env.push(res);
            }
            ImplPrimitive::Over => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push(b.clone());
                env.push(a);
                env.push(b);
            }
            &ImplPrimitive::MultiKeep(dims) => {
                let count = env.pop(1)?;
                let val = env.pop(2)?;
                let res = count.multikeep(val, dims, env)?;
                env.push(res);
            }
            &ImplPrimitive::Ga(op, spec) => match op {
                GaOp::GeometricProduct => env.dyadic_oo_env_with(spec, ga::product)?,
                GaOp::GeometricInner => env.dyadic_oo_env_with(spec, ga::inner_product)?,
                GaOp::GeometricWedge => env.dyadic_oo_env_with(spec, ga::wedge_product)?,
                GaOp::GeometricRegressive => {
                    env.dyadic_oo_env_with(spec, ga::regressive_product)?
                }
                GaOp::GeometricDivide => env.dyadic_oo_env_with(spec, ga::divide)?,
                GaOp::GeometricMagnitude => env.monadic_env_with(spec, ga::magnitude)?,
                GaOp::GeometricNormalize => env.monadic_env_with(spec, ga::normalize)?,
                GaOp::GeometricSqrt => env.monadic_env_with(spec, ga::sqrt)?,
                GaOp::GeometricReverse => env.monadic_env_with(spec, ga::reverse)?,
                GaOp::GeometricDual => env.monadic_env_with(spec, ga::dual)?,
                GaOp::GeometricAdd => env.dyadic_oo_env_with(spec, ga::add)?,
                GaOp::GeometricSub => env.dyadic_oo_env_with(spec, |spec, a, b, env| {
                    let a = a.neg(env)?;
                    ga::add(spec, a, b, env)
                })?,
                GaOp::GeometricRotor => env.dyadic_oo_env_with(spec, ga::rotor)?,
                GaOp::GeometricSandwich => env.dyadic_oo_env_with(spec, ga::sandwich)?,
                GaOp::PadBlades => env.dyadic_oo_env_with(spec, ga::pad_blades)?,
                GaOp::ExtractBlades => env.dyadic_oo_env_with(spec, ga::extract_blades)?,
                GaOp::GeometricCouple => env.dyadic_oo_env(ga::couple)?,
                GaOp::GeometricUnCouple => {
                    let val = env.pop(1)?;
                    let (a, b) = ga::uncouple(val, env)?;
                    env.push(b);
                    env.push(a);
                }
                GaOp::GeometricParse => env.monadic_env_with(spec, ga::parse)?,
                GaOp::GeometricUnParse => env.monadic_env_with(spec, ga::unparse)?,
            },
            prim => {
                return Err(env.error(if prim.modifier_args().is_some() {
                    format!(
                        "{prim} was handled as a function. \
                        This is a bug in the interpreter"
                    )
                } else {
                    format!(
                        "{prim} was not handled as a function. \
                        This is a bug in the interpreter"
                    )
                }))
            }
        }
        Ok(())
    }
    pub(crate) fn run_mod(&self, ops: Ops, env: &mut Uiua) -> UiuaResult {
        match self {
            &ImplPrimitive::OnSub(n) => {
                let [f] = get_ops(ops, env)?;
                let kept = env.copy_n(n)?;
                env.exec(f)?;
                env.push_all(kept);
            }
            &ImplPrimitive::BySub(n) => {
                let [f] = get_ops(ops, env)?;
                env.dup_values(n, n.max(f.sig.args()))?;
                env.exec(f)?;
            }
            &ImplPrimitive::WithSub(n) => {
                let [f] = get_ops(ops, env)?;
                let kept = env.copy_n_down(n, n.max(f.sig.args()))?;
                env.exec(f)?;
                env.push_all(kept);
            }
            &ImplPrimitive::OffSub(n) => {
                let [f] = get_ops(ops, env)?;
                let outputs = f.sig.outputs();
                let kept = env.copy_n(n)?;
                let temp = env.pop_n(n.saturating_sub(f.sig.args()))?;
                env.exec(f)?;
                env.insert_stack(outputs, kept)?;
                env.push_all(temp);
            }
            ImplPrimitive::Astar => {
                let [neighbors, heuristic, is_goal] = get_ops(ops, env)?;
                path::path(neighbors, is_goal, Some(heuristic), env)?;
            }
            ImplPrimitive::UndoPartition1 => groups::undo_partition_part1(ops, env)?,
            ImplPrimitive::UndoGroup1 => groups::undo_group_part1(ops, env)?,
            ImplPrimitive::ReduceContent => reduce::reduce_content(ops, env)?,
            ImplPrimitive::ReduceConjoinInventory => zip::reduce_conjoin_inventory(ops, env)?,
            ImplPrimitive::AstarFirst => {
                let [neighbors, heuristic, is_goal] = get_ops(ops, env)?;
                path::path_first(neighbors, is_goal, Some(heuristic), env)?;
            }
            ImplPrimitive::AstarSignLen => {
                let [neighbors, heuristic, is_goal] = get_ops(ops, env)?;
                path::path_sign_len(neighbors, is_goal, Some(heuristic), env)?;
            }
            ImplPrimitive::AstarTake => {
                let [neighbors, heuristic, is_goal] = get_ops(ops, env)?;
                path::path_take(neighbors, is_goal, Some(heuristic), env)?;
            }
            ImplPrimitive::AstarPop => {
                let [neighbors, heuristic, is_goal] = get_ops(ops, env)?;
                path::path_pop(neighbors, is_goal, Some(heuristic), env)?;
            }
            ImplPrimitive::PathFirst => {
                let [neighbors, is_goal] = get_ops(ops, env)?;
                path::path_first(neighbors, is_goal, None, env)?;
            }
            ImplPrimitive::PathSignLen => {
                let [neighbors, is_goal] = get_ops(ops, env)?;
                path::path_sign_len(neighbors, is_goal, None, env)?;
            }
            ImplPrimitive::PathTake => {
                let [neighbors, is_goal] = get_ops(ops, env)?;
                path::path_take(neighbors, is_goal, None, env)?;
            }
            ImplPrimitive::PathPop => {
                let [neighbors, is_goal] = get_ops(ops, env)?;
                path::path_pop(neighbors, is_goal, None, env)?;
            }
            &ImplPrimitive::ReduceDepth(depth) => reduce::reduce(ops, depth, env)?,
            ImplPrimitive::RepeatWithInverse => loops::repeat(ops, true, false, env)?,
            ImplPrimitive::RepeatCountConvergence => loops::repeat(ops, false, true, env)?,
            ImplPrimitive::UnScan => reduce::unscan(ops, env)?,
            ImplPrimitive::UnDump => dump(ops, env, true)?,
            ImplPrimitive::UnFill => fill!(ops, None, env, with_unfill, without_unfill_but),
            &ImplPrimitive::SidedFill(side) => fill!(ops, side, env, with_fill, without_fill_but),
            ImplPrimitive::ReduceTable => table::reduce_table(ops, env)?,
            ImplPrimitive::UnBracket => {
                let [f, g] = get_ops(ops, env)?;
                env.exec(f.node)?;
                let f_outputs = env.pop_n(f.sig.outputs())?;
                env.exec(g.node)?;
                env.push_all(f_outputs);
            }
            ImplPrimitive::SplitByScalar => {
                let [f] = get_ops(ops, env)?;
                groups::split_by(f, true, false, env)?;
            }
            ImplPrimitive::SplitBy => {
                let [f] = get_ops(ops, env)?;
                groups::split_by(f, false, false, env)?;
            }
            ImplPrimitive::SplitByKeepEmpty => {
                let [f] = get_ops(ops, env)?;
                groups::split_by(f, false, true, env)?;
            }
            ImplPrimitive::UnGroup => {
                let [f] = get_ops(ops, env)?;
                groups::un_group(f, env)?
            }
            ImplPrimitive::UnPartition => {
                let [f] = get_ops(ops, env)?;
                groups::un_partition(f, env)?
            }
            &ImplPrimitive::EachSub(n) => {
                let [f] = get_ops(ops, env)?;
                let sig = f.sig;
                let vals = env.pop_n(sig.args())?;
                let max_shape = vals
                    .iter()
                    .map(|v| &v.shape)
                    .max_by_key(|sh| sh.len())
                    .cloned();
                let max_rank = max_shape.as_ref().map(|sh| sh.len()).unwrap_or(0);
                for mut val in vals {
                    val.deshape_sub(n + 1, 0, val.rank() == max_rank, env)?;
                    env.push(val);
                }
                zip::rows(f, 0, false, env)?;
                for mut value in env.pop_n(sig.outputs())? {
                    if let Some(max_shape) = &max_shape {
                        value.undo_deshape(Some(n + 1), max_shape, env)?;
                    }
                    env.push(value);
                }
            }
            &ImplPrimitive::RowsSub(sub, inv) => {
                let [f] = get_ops(ops, env)?;
                let sig = f.sig;
                let arg_count = sig
                    .args()
                    .max(sub.side.and_then(|side| side.n).unwrap_or(0));
                let vals = env.top_n_mut(arg_count)?;
                let max_rank = vals.iter().map(Value::rank).max().unwrap_or(0);
                let depth = match sub.num {
                    None => 1,
                    Some(n) if n < 0 => n.unsigned_abs() as usize,
                    Some(n) => max_rank.saturating_sub(n as usize),
                };
                if let Some(side) = sub.side {
                    let n = side.n.unwrap_or(1);
                    match side.side {
                        SubSide::Left => {
                            for val in vals.iter_mut().rev().take(n) {
                                for _ in 0..depth {
                                    val.fix()
                                }
                            }
                        }
                        SubSide::Right => {
                            for val in vals.iter_mut().take(n) {
                                for _ in 0..depth {
                                    val.fix()
                                }
                            }
                        }
                    }
                }
                if depth == 0 {
                    if inv {
                        for val in vals {
                            val.unbox();
                        }
                    }
                    env.exec(f)?;
                    if inv {
                        for val in env.top_n_mut(sig.outputs())? {
                            val.box_it();
                        }
                    }
                } else if depth == max_rank && sig.args() == 1 && !inv {
                    let xs = env.pop(1)?;
                    zip::each1(f, xs, env)?;
                } else {
                    zip::rows(f, depth - 1, inv, env)?;
                }
            }
            ImplPrimitive::UndoRows | ImplPrimitive::UndoInventory => {
                let [f] = get_ops(ops, env)?;
                let len = env
                    .pop(1)?
                    .as_nat(env, "Rows length must be a natural number")?;
                let start = env.require_height(f.sig.args())?;
                let inventory = matches!(self, ImplPrimitive::UndoInventory);
                for i in 0..f.sig.args() {
                    let val = &env.stack()[start + i];
                    if val.row_count() != len {
                        return Err(env.error(format!(
                            "Cannot undo {} of length {len} when \
                            transformed array has shape {}",
                            if inventory {
                                Primitive::Inventory
                            } else {
                                Primitive::Rows
                            }
                            .format(),
                            val.shape
                        )));
                    }
                    env.stack_mut()[start + i].reverse();
                }
                let outputs = f.sig.outputs();
                zip::rows(f, 0, inventory, env)?;
                let start = env.require_height(outputs)?;
                for val in &mut env.stack_mut()[start..] {
                    val.reverse();
                }
            }
            ImplPrimitive::BothImpl(sub) => {
                let [f] = get_ops(ops, env)?;
                match (sub.num, sub.side) {
                    (None | Some(2), None) => {
                        let vals = env.pop_n(f.sig.args())?;
                        env.exec(f.node.clone())?;
                        env.push_all(vals);
                        env.exec(f.node)?;
                    }
                    (Some(0), _) => {}
                    (Some(n), None) => {
                        let mut vals = env.pop_n(f.sig.args() * (n as usize - 1))?;
                        vals.reverse();
                        env.exec(f.node.clone())?;
                        for _ in 0..n - 1 {
                            env.push_all(vals.drain(vals.len() - f.sig.args()..).rev());
                            env.exec(f.node.clone())?;
                        }
                    }
                    (num, Some(side)) => {
                        let n = num.unwrap_or(2) as usize;
                        let reused = side.n.unwrap_or(1);
                        let unique_args = f.sig.args().saturating_sub(reused);
                        match side.side {
                            SubSide::Left => {
                                let reused_vals = env.pop_n(reused)?;
                                let mut unique_vals = env.pop_n(unique_args * n)?;
                                unique_vals.reverse();
                                for (reused_vals, f) in repeat_n((reused_vals, f.node), n) {
                                    env.push_all(
                                        unique_vals.drain(unique_vals.len() - unique_args..).rev(),
                                    );
                                    env.push_all(reused_vals);
                                    env.exec(f)?;
                                }
                            }
                            SubSide::Right => {
                                let mut unique_vals = env.pop_n(unique_args * n)?;
                                unique_vals.reverse();
                                let reused_vals = env.pop_n(reused)?;
                                for (reused_vals, f) in repeat_n((reused_vals, f.node), n) {
                                    env.push_all(reused_vals);
                                    env.push_all(
                                        unique_vals.drain(unique_vals.len() - unique_args..).rev(),
                                    );
                                    env.exec(f)?;
                                }
                            }
                        }
                    }
                }
            }
            ImplPrimitive::UnBothImpl(sub) => {
                let [f] = get_ops(ops, env)?;
                // dbg!(sub);
                match (sub.num, sub.side) {
                    (None | Some(2), None) => {
                        env.exec(f.node.clone())?;
                        let vals = env.pop_n(f.sig.outputs())?;
                        env.exec(f.node)?;
                        env.push_all(vals);
                    }
                    (Some(0), _) => {}
                    (Some(n), None) => {
                        let mut vals = Vec::with_capacity(f.sig.outputs() * (n as usize - 1));
                        env.exec(f.node.clone())?;
                        for _ in 0..n - 1 {
                            vals.extend(env.pop_n(f.sig.outputs())?.into_iter().rev());
                            env.exec(f.node.clone())?;
                        }
                        vals.reverse();
                        env.push_all(vals);
                    }
                    (_, Some(_)) => {
                        return Err(env.error(
                            "Attempted to invert sided both. \
                            This is a bug in the interpreter.",
                        ))
                    }
                }
            }
            prim => {
                return Err(env.error(if prim.modifier_args().is_some() {
                    format!(
                        "{prim} was not handled as a modifier. \
                        This is a bug in the interpreter"
                    )
                } else {
                    format!(
                        "{prim} was handled as a modifier. \
                        This is a bug in the interpreter"
                    )
                }))
            }
        }
        Ok(())
    }
}

fn regex(env: &mut Uiua) -> UiuaResult {
    thread_local! {
        pub static REGEX_CACHE: RefCell<HashMap<String, Regex>> = RefCell::new(HashMap::new());
    }
    let pattern = env.pop(1)?.as_string(env, "Pattern must be a string")?;
    let target = env
        .pop(1)?
        .as_string(env, "Matching target must be a string")?;
    REGEX_CACHE.with(|cache| -> UiuaResult {
        let mut cache = cache.borrow_mut();
        let regex = if let Some(regex) = cache.get(&pattern) {
            regex
        } else {
            let regex =
                Regex::new(&pattern).map_err(|e| env.error(format!("Invalid pattern: {}", e)))?;
            cache.entry(pattern.clone()).or_insert(regex.clone())
        };

        let mut matches: Value =
            Array::<Boxed>::new([0, regex.captures_len()].as_slice(), []).into();

        for caps in regex.captures_iter(&target) {
            let row: EcoVec<Boxed> = caps
                .iter()
                .flat_map(|m| {
                    m.map(|m| Boxed(Value::from(m.as_str()))).or_else(|| {
                        env.value_fill()
                            .map(|fv| fv.value.clone())
                            .map(Value::boxed_if_not)
                    })
                })
                .collect();
            matches.append(row.into(), false, env)?;
        }

        env.push(matches);
        Ok(())
    })
}

thread_local! {
    pub(crate) static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::from_entropy());
}

/// Generate a random number, equivalent to [`Primitive::Rand`]
pub fn random() -> f64 {
    random_with(|rng| rng.gen())
}

/// Access the interpreter's random number generator for the thread
pub fn random_with<T>(f: impl FnOnce(&mut SmallRng) -> T) -> T {
    RNG.with(|rng| f(&mut rng.borrow_mut()))
}

/// Seed the random number generator
pub fn seed_random(seed: u64) {
    random_with(|rng| *rng = SmallRng::seed_from_u64(seed));
}

fn stack_n(env: &mut Uiua, n: usize, inverse: bool) -> UiuaResult {
    env.require_height(n)?;
    let boundaries = stack_boundaries(env);
    let span = format!("{} {}", ImplPrimitive::StackN { n, inverse }, env.span());
    let max_line_len = span.chars().count() + 2;
    let stack_height = env.stack_height() - n;
    let item_lines: Vec<Vec<String>> = env.stack()[stack_height..]
        .iter()
        .map(Value::show)
        .map(|s| s.lines().map(Into::into).collect::<Vec<String>>())
        .map(|lines| format_trace_item_lines(lines, max_line_len))
        .enumerate()
        .flat_map(|(i, lines)| {
            if let Some((_, id)) = boundaries
                .iter()
                .find(|(height, _)| i + stack_height == *height)
            {
                let id = id.as_ref().map_or_else(String::new, ToString::to_string);
                vec![vec![format!("│╴╴╴{id}╶╶╶\n")], lines]
            } else {
                vec![lines]
            }
        })
        .collect();
    env.rt.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines.iter().flatten() {
        env.rt.backend.print_str_trace(line);
    }
    env.rt.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.rt.backend.print_str_trace("╴");
    }
    env.rt.backend.print_str_trace("\n");
    Ok(())
}

fn stack(env: &Uiua, inverse: bool) -> UiuaResult {
    let span = if inverse {
        format!("{}{} {}", Primitive::Un, Primitive::Stack, env.span())
    } else {
        format!("{} {}", Primitive::Stack, env.span())
    };
    let items = env.stack();
    let max_line_len = span.chars().count() + 2;
    let boundaries = stack_boundaries(env);
    let item_lines: Vec<Vec<String>> = items
        .iter()
        .map(Value::show)
        .map(|s| s.lines().map(Into::into).collect::<Vec<String>>())
        .map(|lines| format_trace_item_lines(lines, max_line_len))
        .enumerate()
        .flat_map(|(i, lines)| {
            if let Some((_, id)) = boundaries.iter().find(|(height, _)| i == *height) {
                let id = id.as_ref().map_or_else(String::new, ToString::to_string);
                vec![vec![format!("│╴╴╴{id}╶╶╶\n")], lines]
            } else {
                vec![lines]
            }
        })
        .collect();
    env.rt.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines.iter().flatten() {
        env.rt.backend.print_str_trace(line);
    }
    env.rt.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.rt.backend.print_str_trace("╴");
    }
    env.rt.backend.print_str_trace("\n");
    Ok(())
}

fn dump(ops: Ops, env: &mut Uiua, inverse: bool) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    if f.sig != (1, 1) {
        return Err(env.error(format!(
            "{}'s function's signature must be |1, but it is {}",
            Primitive::Dump.format(),
            f.sig
        )));
    }
    let span = if inverse {
        format!("{}{} {}", Primitive::Un, Primitive::Dump, env.span())
    } else {
        format!("{} {}", Primitive::Dump, env.span())
    };
    let unprocessed = env.stack().to_vec();
    let mut items = Vec::new();
    for item in unprocessed {
        env.push(item);
        match env.exec(f.clone()) {
            Ok(()) => items.push(env.pop("dump's function's processed result")?),
            Err(e) => items.push(e.value()),
        }
    }
    let max_line_len = span.chars().count() + 2;
    let boundaries = stack_boundaries(env);
    let item_lines: Vec<Vec<String>> = items
        .iter()
        .map(Value::show)
        .map(|s| s.lines().map(Into::into).collect::<Vec<String>>())
        .map(|lines| format_trace_item_lines(lines, max_line_len))
        .enumerate()
        .flat_map(|(i, lines)| {
            if let Some((_, id)) = boundaries.iter().find(|(height, _)| i == *height) {
                let id = id.as_ref().map_or_else(String::new, ToString::to_string);
                vec![vec![format!("│╴╴╴{id}╶╶╶\n")], lines]
            } else {
                vec![lines]
            }
        })
        .collect();
    env.rt.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines.iter().flatten() {
        env.rt.backend.print_str_trace(line);
    }
    env.rt.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.rt.backend.print_str_trace("╴");
    }
    env.rt.backend.print_str_trace("\n");
    Ok(())
}

fn stack_boundaries(env: &Uiua) -> Vec<(usize, &Option<FunctionId>)> {
    let mut boundaries: Vec<(usize, &Option<FunctionId>)> = Vec::new();
    let mut height = 0;
    for frame in env.call_frames().rev() {
        let delta = env.stack_height() as isize - frame.start_height as isize;
        height = height.max((frame.sig.args() as isize + delta).max(0) as usize);
        if matches!(frame.id, Some(FunctionId::Main)) {
            break;
        }
        boundaries.push((env.stack_height().saturating_sub(height), &frame.id));
    }
    boundaries
}

fn format_trace_item_lines(mut lines: Vec<String>, mut max_line_len: usize) -> Vec<String> {
    let lines_len = lines.len();
    for (j, line) in lines.iter_mut().enumerate() {
        let stick = if lines_len == 1 || j == 1 {
            "├╴"
        } else {
            "│ "
        };
        line.insert_str(0, stick);
        max_line_len = max_line_len.max(line.chars().count());
        line.push('\n');
    }
    lines
}

/// Documentation for a primitive
#[derive(Default, Debug)]
pub struct PrimDoc {
    /// The short description
    pub short: Vec<PrimDocFragment>,
    /// The full documentation
    pub lines: Vec<PrimDocLine>,
}

impl From<Primitive> for PrimDoc {
    fn from(prim: Primitive) -> Self {
        PrimDoc::from_lines(prim.doc())
    }
}

impl PrimDoc {
    /// Get the primitive's short description
    pub fn short_text(&self) -> Cow<str> {
        if self.short.len() == 1 {
            match &self.short[0] {
                PrimDocFragment::Text(t) => return Cow::Borrowed(t),
                PrimDocFragment::Code(c) => return Cow::Borrowed(c),
                PrimDocFragment::Emphasis(e) => return Cow::Borrowed(e),
                PrimDocFragment::Strong(s) => return Cow::Borrowed(s),
                PrimDocFragment::Primitive { prim, named: true } => {
                    return Cow::Borrowed(prim.name());
                }
                PrimDocFragment::Link { text, .. } => return Cow::Borrowed(text),
                PrimDocFragment::Primitive { .. } => {}
            }
        }
        let mut s = String::new();
        for frag in &self.short {
            match frag {
                PrimDocFragment::Text(t) => s.push_str(t),
                PrimDocFragment::Code(c) => s.push_str(c),
                PrimDocFragment::Emphasis(e) => s.push_str(e),
                PrimDocFragment::Strong(str) => s.push_str(str),
                PrimDocFragment::Link { text, .. } => s.push_str(text),
                PrimDocFragment::Primitive { prim, named } => {
                    if *named {
                        s.push_str(prim.name());
                    } else if let Some(c) = prim.glyph() {
                        s.push(c);
                    } else {
                        s.push_str(prim.name());
                    }
                }
            }
        }
        Cow::Owned(s)
    }
    pub(crate) fn from_lines(s: &str) -> Self {
        let mut short = Vec::new();
        let mut lines = Vec::new();
        for line in s.lines() {
            let line = line.trim();
            if let Some(mut ex) = line.strip_prefix("ex:") {
                // Example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                lines.push(PrimDocLine::Example(PrimExample {
                    input: ex.into(),
                    should_error: false,
                    output: OnceLock::new(),
                }));
            } else if let Some(mut ex) = line.strip_prefix("ex!") {
                // Example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                lines.push(PrimDocLine::Example(PrimExample {
                    input: ex.into(),
                    should_error: true,
                    output: OnceLock::new(),
                }));
            } else if let Some(mut ex) = line.strip_prefix(':') {
                // Continue example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                if let Some(PrimDocLine::Example(example)) = lines.last_mut() {
                    example.input.push('\n');
                    example.input.push_str(ex);
                } else {
                    lines.push(PrimDocLine::Text(parse_doc_line_fragments(line)));
                }
            } else if short.is_empty() {
                // Set short
                short = parse_doc_line_fragments(line);
            } else {
                // Add line
                lines.push(PrimDocLine::Text(parse_doc_line_fragments(line)));
            }
        }
        while let Some(PrimDocLine::Text(frags)) = lines.first() {
            if frags.is_empty() {
                lines.remove(0);
            } else {
                break;
            }
        }
        while let Some(PrimDocLine::Text(frags)) = lines.last() {
            if frags.is_empty() {
                lines.pop();
            } else {
                break;
            }
        }
        Self { short, lines }
    }
}

/// An primitive code example
#[derive(Debug)]
pub struct PrimExample {
    input: String,
    should_error: bool,
    output: OnceLock<UiuaResult<Vec<String>>>,
}

impl PrimExample {
    /// Get the example's source code
    pub fn input(&self) -> &str {
        &self.input
    }
    /// Check whether the example should error
    pub fn should_error(&self) -> bool {
        self.should_error
    }
    /// Get the example's output
    pub fn output(&self) -> &UiuaResult<Vec<String>> {
        self.output.get_or_init(|| {
            let mut env = Uiua::with_safe_sys();
            match env.run_str(&self.input) {
                Ok(_) => Ok(env.take_stack().into_iter().map(|val| val.show()).collect()),
                Err(e) => Err(e),
            }
        })
    }
    /// Get the example's output as strings
    pub fn output_strings(&self) -> Result<&Vec<String>, String> {
        self.output().as_ref().map_err(|e| {
            e.to_string()
                .lines()
                .next()
                .unwrap_or_default()
                .split_once(' ')
                .unwrap_or_default()
                .1
                .into()
        })
    }
}

/// A line in a primitive's documentation
#[derive(Debug)]
pub enum PrimDocLine {
    /// Just text
    Text(Vec<PrimDocFragment>),
    /// An example
    Example(PrimExample),
}

/// A pseudo-markdown fragment for primitive documentation
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum PrimDocFragment {
    Text(String),
    Code(String),
    Emphasis(String),
    Strong(String),
    Primitive { prim: Primitive, named: bool },
    Link { text: String, url: String },
}

pub(crate) fn parse_doc_line_fragments(mut line: &str) -> Vec<PrimDocFragment> {
    let mut end_link = None;
    if let Some(link_start) = line.find("https://") {
        let end = &line[link_start..];
        if !end.contains(' ') && !end.contains(')') {
            end_link = Some(end);
            line = &line[..link_start];
        }
    }
    let mut frags = Vec::new();
    #[derive(PartialEq, Eq)]
    enum FragKind {
        Text,
        Code,
        Emphasis,
        Strong,
        Primitive,
    }
    impl FragKind {
        fn open(&self) -> &str {
            match self {
                FragKind::Text => "",
                FragKind::Code => "`",
                FragKind::Emphasis => "*",
                FragKind::Strong => "**",
                FragKind::Primitive => "[",
            }
        }
    }
    let mut curr = String::new();
    let mut kind = FragKind::Text;
    let mut chars = line.char_indices().peekable();
    while let Some((i, c)) = chars.next() {
        match c {
            '\\' if chars.peek().map(|i| i.1) == Some('`') => {
                curr.push('`');
                chars.next();
            }
            '`' if kind == FragKind::Code => {
                if let Some(prim) = Primitive::from_name(&curr) {
                    frags.push(PrimDocFragment::Primitive { prim, named: false });
                } else {
                    frags.push(PrimDocFragment::Code(curr));
                }
                curr = String::new();
                kind = FragKind::Text;
            }
            '`' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Code;
            }
            '*' if kind == FragKind::Emphasis && curr.is_empty() => {
                kind = FragKind::Strong;
            }
            '*' if kind == FragKind::Emphasis => {
                frags.push(PrimDocFragment::Emphasis(curr));
                curr = String::new();
                kind = FragKind::Text;
            }
            '*' if kind == FragKind::Strong && chars.peek().map(|i| i.1) == Some('*') => {
                chars.next();
                frags.push(PrimDocFragment::Strong(curr));
                curr = String::new();
                kind = FragKind::Text;
            }
            '*' if kind == FragKind::Text && line[i + 1..].contains('*') => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Emphasis;
            }
            '[' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Primitive;
            }
            ']' if kind == FragKind::Primitive && chars.peek().map(|i| i.1) == Some('(') => {
                chars.next();
                let mut url = String::new();
                for (_, c) in chars.by_ref() {
                    if c == ')' {
                        break;
                    }
                    url.push(c);
                }
                frags.push(PrimDocFragment::Link { text: curr, url });
                curr = String::new();
                kind = FragKind::Text;
            }
            ']' if kind == FragKind::Primitive => {
                if let Some(prim) = Primitive::from_name(&curr) {
                    frags.push(PrimDocFragment::Primitive { prim, named: true });
                } else {
                    frags.push(PrimDocFragment::Text(curr));
                }
                curr = String::new();
                kind = FragKind::Text;
            }
            ']' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
            }
            c => curr.push(c),
        }
    }
    curr.insert_str(0, kind.open());
    if !curr.is_empty() {
        frags.push(PrimDocFragment::Text(curr));
    }
    if let Some(url) = end_link {
        frags.push(PrimDocFragment::Link {
            text: url.to_string(),
            url: url.to_string(),
        });
    }
    frags
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{split_name, PrimComponent};

    #[test]
    fn name_collisions() {
        for a in Primitive::all() {
            for b in Primitive::all() {
                if a >= b {
                    continue;
                }
                assert_ne!(a.name(), b.name(), "{a:?} and {b:?} have the same name",)
            }
        }
    }

    #[test]
    #[cfg(feature = "native_sys")]
    fn prim_docs() {
        use crate::SafeSys;

        for prim in Primitive::non_deprecated() {
            for line in PrimDoc::from(prim).lines {
                if let PrimDocLine::Example(ex) = line {
                    if [
                        "&sl", "&tcpc", "&tlsc", "&ast", "&clip", "&fo", "&fc", "&fde", "&ftr",
                        "&fld", "&fif", "&fras", "&frab", "&fmd", "timezone", "&b",
                    ]
                    .iter()
                    .any(|prim| ex.input.contains(prim))
                    {
                        continue;
                    }
                    eprintln!("{prim} example:\n{}", ex.input);
                    let mut env = Uiua::with_backend(SafeSys::with_thread_spawning());
                    match env.run_str(&ex.input) {
                        Ok(mut comp) => {
                            if let Some(diag) = comp.take_diagnostics().into_iter().next() {
                                if !ex.should_error {
                                    panic!("\nExample failed:\n{}\n{}", ex.input, diag.report());
                                }
                            } else if ex.should_error {
                                panic!("Example should have failed: {}", ex.input);
                            }
                        }
                        Err(e) => {
                            if !ex.should_error {
                                panic!("\nExample failed:\n{}\n{}", ex.input, e.report());
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn primitive_from_name() {
        for prim in Primitive::non_deprecated() {
            assert_eq!(Primitive::from_name(prim.name()), Some(prim));
        }
        for (name, test) in [
            (
                "from_format_name",
                PrimComponent::from_format_name as fn(&str) -> Option<PrimComponent>,
            ),
            ("from_format_name_multi", |name| {
                split_name(name).unwrap().first().map(|(prim, _)| *prim)
            }),
        ] {
            for prim in Primitive::non_deprecated() {
                if prim.name().contains(' ') {
                    continue;
                }
                let char_test = match prim.glyph() {
                    None => prim.name().len(),
                    Some(c) if c.is_ascii() => continue,
                    Some(_) => 4,
                };
                let short: String = prim.name().chars().take(char_test).collect();
                assert_eq!(test(&short), Some(prim.into()));
            }
            for prim in Primitive::non_deprecated() {
                if matches!(prim, Primitive::Rand | Primitive::Parse | Primitive::Slf) {
                    continue;
                }
                let char_test = match prim.glyph() {
                    None => prim.name().len(),
                    Some(c) if c.is_ascii() || prim.ascii().is_some() => continue,
                    Some(_) => 3,
                };
                let short: String = prim.name().chars().take(char_test).collect();
                assert_eq!(
                    test(&short),
                    Some(prim.into()),
                    "{} does not format from {:?} with {}",
                    prim.format(),
                    short,
                    name
                );
            }
        }
        assert_eq!(Primitive::from_format_name("id"), Some(Primitive::Identity));
    }

    #[test]
    fn from_multiname() {
        assert!(matches!(
            &*split_name("rev").expect("rev"),
            [(PrimComponent::Prim(Primitive::Reverse), _)]
        ));
        assert!(matches!(
            &*split_name("revrev").expect("revrev"),
            [
                (PrimComponent::Prim(Primitive::Reverse), _),
                (PrimComponent::Prim(Primitive::Reverse), _)
            ]
        ));
        assert!(matches!(
            &*split_name("tabkee").unwrap(),
            [
                (PrimComponent::Prim(Primitive::Table), _),
                (PrimComponent::Prim(Primitive::Keep), _)
            ]
        ));
        assert_eq!(split_name("foo"), None);
    }

    #[cfg(test)]
    #[test]
    fn gen_grammar_file() {
        use crate::PrimClass;
        fn gen_group(prims: impl Iterator<Item = Primitive> + Clone, additional: &str) -> String {
            let glyphs = prims
                .clone()
                .flat_map(|p| {
                    p.glyph()
                        .into_iter()
                        .chain(p.ascii().into_iter().flat_map(|ascii| {
                            Some(ascii.to_string())
                                .filter(|s| s.len() == 1)
                                .into_iter()
                                .flat_map(|s| s.chars().collect::<Vec<_>>())
                        }))
                })
                .collect::<String>()
                .replace('\\', "\\\\\\\\")
                .replace('-', "\\\\-")
                .replace('*', "\\\\*")
                .replace('^', "\\\\^");
            let format_names: Vec<_> = prims
                .clone()
                .map(|p| {
                    let name = p.name();
                    let min_len = if name.starts_with('&') {
                        name.len()
                    } else {
                        (2..=name.len())
                            .find(|&n| Primitive::from_format_name(&name[..n]) == Some(p))
                            .unwrap()
                    };
                    let mut start: String = name.chars().take(min_len).collect();
                    let mut end = String::new();
                    for c in name.chars().skip(min_len) {
                        start.push('(');
                        start.push(c);
                        end.push_str(")?");
                    }
                    format!("{}{}", start, end)
                })
                .collect();
            let format_names = format_names.join("|");
            let mut literal_names: Vec<_> = prims
                .map(|p| p.names())
                .filter(|p| p.ascii.is_none() && p.glyph.is_none())
                .map(|n| format!("|{}", n.text))
                .collect();
            literal_names.sort_by_key(|s| s.len());
            literal_names.reverse();
            let literal_names = literal_names.join("");
            format!(
                r#"[{glyphs}]|(?<![a-zA-Z$])({format_names}{literal_names})(?![a-zA-Z]){additional}"#
            )
        }

        let stack_functions = gen_group(
            Primitive::non_deprecated()
                .filter(|p| {
                    [PrimClass::Stack, PrimClass::Debug].contains(&p.class())
                        && p.modifier_args().is_none()
                })
                .chain(Some(Primitive::Identity)),
            "",
        );
        let noadic_functions = gen_group(
            Primitive::non_deprecated().filter(|p| {
                ![PrimClass::Stack, PrimClass::Debug, PrimClass::Constant].contains(&p.class())
                    && p.modifier_args().is_none()
                    && p.args() == Some(0)
            }),
            "",
        );
        let monadic_functions = gen_group(
            Primitive::non_deprecated().filter(|p| {
                ![PrimClass::Stack, PrimClass::Debug, PrimClass::Planet].contains(&p.class())
                    && p.modifier_args().is_none()
                    && p.args() == Some(1)
            }),
            "|⋊[a-zA-Z]*",
        );
        let dyadic_functions = gen_group(
            Primitive::non_deprecated().filter(|p| {
                ![PrimClass::Stack, PrimClass::Debug].contains(&p.class())
                    && p.modifier_args().is_none()
                    && p.args() == Some(2)
            }),
            "",
        );
        let monadic_modifiers = gen_group(
            Primitive::non_deprecated().filter(|p| matches!(p.modifier_args(), Some(1))),
            "",
        );
        let dyadic_modifiers: String = gen_group(
            Primitive::non_deprecated().filter(|p| matches!(p.modifier_args(), Some(n) if n >= 2)),
            "",
        );

        let text = format!(
            r##"{{
	"$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
	"name": "Uiua",
	"firstLineMatch": "^#!/.*\\buiua\\b",
	"fileTypes": [
		"ua"
	],
	"patterns": [
		{{
			"include": "#comments"
		}},
		{{
			"include": "#strings-multiline-format"
		}},
		{{
			"include": "#strings-multiline"
		}},
		{{
			"include": "#strings-format"
		}},
		{{
			"include": "#strings-normal"
		}},
        {{
            "include": "#characters"
        }},
        {{
            "include": "#labels"
        }},
        {{
            "include": "#module_delim"
        }},
        {{
            "include": "#strand"
        }},
		{{
			"include": "#stack"
		}},
		{{
			"include": "#noadic"
		}},
		{{
			"include": "#monadic"
		}},
		{{
			"include": "#dyadic"
		}},
		{{
			"include": "#mod1"
		}},
		{{
			"include": "#mod2"
		}},
        {{
            "include": "#idents"
        }},
		{{
			"include": "#numbers"
		}}
	],
	"repository": {{
        "idents": {{
            "name": "variable.parameter.uiua",
            "match": "\\b[a-zA-Z]+(₋?[₀₁₂₃₄₅₆₇₈₉]|,`?\\d+)*[!‼]*\\b"
        }},
		"comments": {{
			"name": "comment.line.uiua",
			"match": "(#.*$|$[a-zA-Z]*)"
		}},
		"strings-normal": {{
			"name": "constant.character.escape",
			"begin": "\"",
			"end": "\"",
			"patterns": [
				{{
					"name": "string.quoted",
					"match": "\\\\[\\\\\"0nrt]"
				}}
			]
		}},
		"strings-format": {{
			"name": "constant.character.escape",
			"begin": "\\$\"",
			"end": "\"",
			"patterns": [
				{{
					"name": "string.quoted",
					"match": "\\\\[\\\\\"0nrt_]"
				}},
				{{
					"name": "constant.numeric",
					"match": "(?<!\\\\)_"
				}}
			]
		}},
		"strings-multiline": {{
			"name": "constant.character.escape",
			"begin": "\\$ ",
			"end": "$"
		}},
		"strings-multiline-format": {{
			"name": "constant.character.escape",
			"begin": "\\$\\$ ",
			"end": "$",
			"patterns": [
				{{
					"name": "constant.numeric",
					"match": "(?<!\\\\)_"
				}}
			]
		}},
        "characters": {{
            "name": "constant.character.escape",
            "match": "@(\\\\(x[0-9A-Fa-f]{{2}}|u[0-9A-Fa-f]{{4}}|.)|.)"
        }},
        "labels": {{
            "name": "label.uiua",
            "match": "\\$[a-zA-Z]*"
        }},
		"numbers": {{
			"name": "constant.numeric.uiua",
			"match": "[`¯]?(\\d+|η|π|τ|∞|eta|pi|tau|inf(i(n(i(t(y)?)?)?)?)?)([./]\\d+|e[+-]?\\d+)?"
		}},
		"strand": {{
			"name": "comment.line",
			"match": "(_|‿)"
		}},
        "module_delim": {{
            "match": "---"
        }},
        "stack": {{
            "match": "{stack_functions}"
        }},
		"noadic": {{
			"name": "entity.name.tag.uiua",
            "match": "{noadic_functions}"
        }},
		"monadic": {{
			"name": "string.quoted",
            "match": "{monadic_functions}"
        }},
		"dyadic": {{
			"name": "entity.name.function.uiua",
            "match": "{dyadic_functions}"
        }},
		"mod1": {{
			"name": "entity.name.type.uiua",
            "match": "{monadic_modifiers}"
        }},
		"mod2": {{
			"name": "keyword.control.uiua",
            "match": "{dyadic_modifiers}"
        }}
    }},
	"scopeName": "source.uiua"
}}"##
        );

        std::fs::write("uiua.tmLanguage.json", text).expect("Failed to write grammar file");
    }
}
