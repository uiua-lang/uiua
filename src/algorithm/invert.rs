//! Algorithms for invert and under

use std::{cmp::Ordering, fmt};

use ecow::{eco_vec, EcoVec};

use crate::{
    check::instrs_signature,
    primitive::{ImplPrimitive, Primitive},
    Compiler, Function, FunctionId, Instr, Signature, Span, SysOp, TempStack, Value,
};

fn prim_inverse(prim: Primitive, span: usize) -> Option<Instr> {
    use ImplPrimitive::*;
    use Primitive::*;
    Some(match prim {
        Identity => Instr::Prim(Identity, span),
        Flip => Instr::Prim(Flip, span),
        Neg => Instr::Prim(Neg, span),
        Not => Instr::Prim(Not, span),
        Sin => Instr::ImplPrim(Asin, span),
        Atan => Instr::ImplPrim(InvAtan, span),
        Complex => Instr::ImplPrim(InvComplex, span),
        Reverse => Instr::Prim(Reverse, span),
        Transpose => Instr::ImplPrim(TransposeN(-1), span),
        Bits => Instr::ImplPrim(InverseBits, span),
        Couple => Instr::ImplPrim(InvCouple, span),
        Box => Instr::ImplPrim(InvBox, span),
        Where => Instr::ImplPrim(InvWhere, span),
        Utf => Instr::ImplPrim(InvUtf, span),
        Parse => Instr::ImplPrim(InvParse, span),
        Fix => Instr::ImplPrim(InvFix, span),
        Map => Instr::ImplPrim(InvMap, span),
        Trace => Instr::ImplPrim(InvTrace, span),
        Stack => Instr::ImplPrim(InvStack, span),
        _ => return None,
    })
}

fn impl_prim_inverse(prim: ImplPrimitive, span: usize) -> Option<Instr> {
    use ImplPrimitive::*;
    use Primitive::*;
    Some(match prim {
        Cos => Instr::ImplPrim(Acos, span),
        Asin => Instr::Prim(Sin, span),
        Acos => Instr::ImplPrim(Cos, span),
        TransposeN(n) => Instr::ImplPrim(TransposeN(-n), span),
        InverseBits => Instr::Prim(Bits, span),
        InvWhere => Instr::Prim(Where, span),
        InvUtf => Instr::Prim(Utf, span),
        InvAtan => Instr::Prim(Atan, span),
        InvComplex => Instr::Prim(Complex, span),
        InvCouple => Instr::Prim(Couple, span),
        InvParse => Instr::Prim(Parse, span),
        InvFix => Instr::Prim(Fix, span),
        InvMap => Instr::Prim(Map, span),
        InvTrace => Instr::Prim(Trace, span),
        InvStack => Instr::Prim(Stack, span),
        InvBox => Instr::Prim(Box, span),
        _ => return None,
    })
}

macro_rules! pat {
    (($($matching:expr),*), ($($before:expr),*) $(,($($after:expr),*))? $(,)?) => {
        (
            [$($matching,)*],
            [$(box_as_instr($before)),*],
            $([$(box_as_instr($after)),*],)?
        )
    };
    ($matching:expr, ($($before:expr),*) $(,($($after:expr),*))? $(,)?) => {
        (
            [$matching],
            [$(box_as_instr($before)),*],
            $([$(box_as_instr($after)),*],)?
        )
    };
    ($matching:expr, $before:expr $(,($($after:expr),*))? $(,)?) => {
        pat!($matching, ($before) $(,($($after),*))?)
    };
}

macro_rules! maybe_val {
    ($pat:expr) => {
        Either($pat, (Val, $pat))
    };
}

pub(crate) fn invert_instrs(instrs: &[Instr], comp: &mut Compiler) -> Option<EcoVec<Instr>> {
    use Primitive::*;

    if instrs.is_empty() {
        return Some(EcoVec::new());
    }

    // println!("inverting {:?}", instrs);

    let patterns: &[&dyn InvertPattern] = &[
        &invert_call_pattern,
        &invert_dump_pattern,
        &invert_invert_pattern,
        &invert_rectify_pattern,
        &invert_setinverse_pattern,
        &invert_setunder_setinverse_pattern,
        &invert_trivial_pattern,
        &invert_array_pattern,
        &invert_unpack_pattern,
        &invert_scan_pattern,
        &invert_repeat_pattern,
        &invert_reduce_mul_pattern,
        &invert_primes_pattern,
        &(Val, invert_repeat_pattern),
        &(Val, ([Rotate], [Neg, Rotate])),
        &([Rotate], [Neg, Rotate]),
        &pat!(Sqrt, (2, Pow)),
        &([Add], [Sub]),
        &([Mul], [Div]),
        &(Val, IgnoreMany(Flip), ([Add], [Sub])),
        &(Val, ([Sub], [Add])),
        &(Val, ([Flip, Sub], [Flip, Sub])),
        &(Val, IgnoreMany(Flip), ([Mul], [Div])),
        &(Val, ([Div], [Mul])),
        &(Val, ([Flip, Div], [Flip, Div])),
        &(Val, pat!(Pow, (1, Flip, Div, Pow))),
        &(Val, pat!((Flip, Pow), (Flip, 1, Flip, Div, Pow))),
        &(Val, ([Log], [Flip, Pow])),
        &(Val, ([Flip, Log], [Pow])),
        &pat!((Dup, Add), (2, Div)),
        &([Dup, Mul], [Sqrt]),
        &invert_temp_pattern,
    ];

    let mut inverted = EcoVec::new();
    let mut curr_instrs = instrs;
    'find_pattern: loop {
        for pattern in patterns {
            if let Some((input, mut inv)) = pattern.invert_extract(curr_instrs, comp) {
                inv.extend(inverted);
                inverted = inv;
                if input.is_empty() {
                    // println!("inverted {:?} to {:?}", instrs, inverted);
                    return Some(inverted);
                }
                curr_instrs = input;
                continue 'find_pattern;
            }
        }
        break;
    }

    None
}

type Under = (EcoVec<Instr>, EcoVec<Instr>);

pub(crate) fn under_instrs(
    instrs: &[Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(EcoVec<Instr>, EcoVec<Instr>)> {
    use ImplPrimitive::*;
    use Primitive::*;

    if instrs.is_empty() {
        return Some((EcoVec::new(), EcoVec::new()));
    }

    macro_rules! stash2 {
        ($before:expr, $after:expr) => {
            pat!($before, (CopyToTempN(2), $before), (PopTempN(2), $after),)
        };
    }

    macro_rules! bin {
        (Flip, $before:expr, $after:expr) => {
            pat!(
                (Flip, $before),
                (CopyToTempN(1), Flip, $before),
                (PopTempN(1), $after),
            )
        };
        (Flip, $before:expr) => {
            pat!(
                (Flip, $before),
                (CopyToTempN(1), Flip, $before),
                (PopTempN(1), Flip, $before),
            )
        };
        ($before:expr, $after:expr) => {
            pat!(($before), (CopyToTempN(1), $before), (PopTempN(1), $after),)
        };
    }

    macro_rules! round {
        ($before:expr) => {
            pat!($before, (Dup, $before, Flip, Over, Sub, Flip), (Add))
        };
    }

    let patterns: &[&dyn UnderPattern] = &[
        &UnderPatternFn(under_call_pattern, "call"),
        &UnderPatternFn(under_dump_pattern, "dump"),
        &UnderPatternFn(under_rows_pattern, "rows"),
        &UnderPatternFn(under_each_pattern, "each"),
        &UnderPatternFn(under_partition_pattern, "partition"),
        &UnderPatternFn(under_group_pattern, "group"),
        &UnderPatternFn(under_setunder_pattern, "setunder"),
        &UnderPatternFn(under_setinverse_setunder_pattern, "setinverse setunder"),
        &UnderPatternFn(under_trivial_pattern, "trivial"),
        &UnderPatternFn(under_array_pattern, "array"),
        &UnderPatternFn(under_unpack_pattern, "unpack"),
        &UnderPatternFn(under_touch_pattern, "touch"),
        &UnderPatternFn(under_repeat_pattern, "repeat"),
        &bin!(Flip, Add, Sub),
        &bin!(Flip, Mul, Div),
        &bin!(Flip, Sub),
        &bin!(Flip, Div),
        &bin!(Add, Sub),
        &bin!(Sub, Add),
        &bin!(Mul, Div),
        &bin!(Div, Mul),
        &maybe_val!(pat!(
            (Flip, Pow),
            (CopyToTempN(1), Flip, Pow),
            (PopTempN(1), Log)
        )),
        &maybe_val!(pat!(
            Pow,
            (CopyToTempN(1), Pow),
            (PopTempN(1), 1, Flip, Div, Pow)
        )),
        &maybe_val!(pat!(Log, (CopyToTempN(1), Log), (PopTempN(1), Flip, Pow))),
        &maybe_val!(pat!(
            (Flip, Log),
            (CopyToTempN(1), Flip, Log),
            (1, Flip, Div, PopTempN(1), Flip, Pow)
        )),
        &round!(Floor),
        &round!(Ceil),
        &round!(Round),
        &maybe_val!(stash2!(Take, Untake)),
        &maybe_val!(stash2!(Drop, Undrop)),
        &maybe_val!(stash2!(Select, Unselect)),
        &maybe_val!(stash2!(Pick, Unpick)),
        &maybe_val!(pat!(
            Get,
            (CopyToTempN(2), Get),
            (PopTempN(1), Flip, PopTempN(1), Insert),
        )),
        &maybe_val!(pat!(
            Keep,
            (CopyToTempN(2), Keep),
            (PopTempN(1), Flip, PopTempN(1), Unkeep),
        )),
        &pat!(Rotate, (CopyToTempN(1), Rotate), (PopTempN(1), Neg, Rotate)),
        &pat!(Abs, (CopyToTempN(1), Abs), (PopTempN(1), Sign, Mul)),
        &pat!(Sign, (Dup, Abs, PushTempN(1), Sign), (PopTempN(1), Mul)),
        &pat!(First, (CopyToTempN(1), First), (PopTempN(1), Unfirst)),
        &pat!(Last, (CopyToTempN(1), Last), (PopTempN(1), Unlast)),
        &pat!(Shape, (CopyToTempN(1), Shape), (PopTempN(1), Flip, Reshape)),
        &pat!(
            Deshape,
            (Dup, Shape, PushTempN(1), Deshape),
            (PopTempN(1), 0, Unrerank),
        ),
        &maybe_val!(pat!(
            Rerank,
            (Over, Shape, Over, PushTempN(2), Rerank),
            (PopTempN(2), Unrerank),
        )),
        &maybe_val!(pat!(
            Reshape,
            (Over, Shape, PushTempN(1), Reshape),
            (PopTempN(1), Unreshape),
        )),
        &pat!(
            Classify,
            (Dup, Deduplicate, PushTempN(1), Classify),
            (PopTempN(1), Flip, Select),
        ),
        &pat!(
            Deduplicate,
            (Dup, Classify, PushTempN(1), Deduplicate),
            (PopTempN(1), Select),
        ),
        &pat!(Now, (Now, PushTempN(1)), (PopTempN(1), Now, Flip, Sub)),
        &pat!(
            Sys(SysOp::FOpen),
            (Sys(SysOp::FOpen), CopyToTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &pat!(
            Sys(SysOp::FCreate),
            (Sys(SysOp::FCreate), CopyToTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &pat!(
            Sys(SysOp::TcpConnect),
            (Sys(SysOp::TcpConnect), CopyToTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &pat!(
            Sys(SysOp::TcpAccept),
            (Sys(SysOp::TcpAccept), CopyToTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &maybe_val!(pat!(
            Sys(SysOp::FReadAllStr),
            (CopyToTempN(1), Sys(SysOp::FReadAllStr)),
            (PopTempN(1), Sys(SysOp::FWriteAll)),
        )),
        &maybe_val!(pat!(
            Sys(SysOp::FReadAllBytes),
            (CopyToTempN(1), Sys(SysOp::FReadAllBytes)),
            (PopTempN(1), Sys(SysOp::FWriteAll)),
        )),
        &UnderPatternFn(under_temp_pattern, "temp"),
        &UnderPatternFn(under_from_inverse_pattern, "from inverse"), // This must come last!
    ];

    // println!("undering {:?}", instrs);

    let mut befores = EcoVec::new();
    let mut afters = EcoVec::new();
    let mut curr_instrs = instrs;
    'find_pattern: loop {
        for pattern in patterns {
            if let Some((input, (bef, aft))) = pattern.under_extract(curr_instrs, g_sig, comp) {
                // println!(
                //     "matched pattern {:?} on {:?} to {bef:?} {aft:?}",
                //     pattern,
                //     &curr_instrs[..curr_instrs.len() - input.len()],
                // );
                befores.extend(bef);
                afters = aft.into_iter().chain(afters).collect();
                if input.is_empty() {
                    // println!("undered {:?} to {:?} {:?}", instrs, befores, afters);
                    return Some((befores, afters));
                }
                curr_instrs = input;
                continue 'find_pattern;
            }
        }
        break;
    }

    // println!("under {:?} failed with remaining {:?}", instrs, curr_instrs);

    None
}

trait InvertPattern {
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)>;
}

trait UnderPattern: fmt::Debug {
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)>;
}

fn invert_call_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), Instr::Call(_), input @ ..] = input else {
        return None;
    };
    let instrs = f.instrs(comp).to_vec();
    let inverse = invert_instrs(&instrs, comp)?;
    Some((input, inverse))
}

fn under_call_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::PushFunc(f), Instr::Call(_), input @ ..] = input else {
        return None;
    };
    let instrs = f.instrs(comp).to_vec();
    let (befores, afters) = under_instrs(&instrs, g_sig, comp)?;
    Some((input, (befores, afters)))
}

fn invert_trivial_pattern<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    use Instr::*;
    match input {
        [Prim(prim, span), input @ ..] => {
            if let Some(inv) = prim_inverse(*prim, *span) {
                return Some((input, eco_vec![inv]));
            }
        }
        [instr @ SetOutputComment { .. }, input @ ..] => {
            return Some((input, eco_vec![instr.clone()]));
        }
        [ImplPrim(prim, span), input @ ..] => {
            if let Some(inv) = impl_prim_inverse(*prim, *span).map(|instr| eco_vec![instr]) {
                return Some((input, inv));
            }
        }
        [Comment(_) | PushSig(_) | PopSig, input @ ..] => return Some((input, EcoVec::new())),
        _ => {}
    }
    None
}

fn under_trivial_pattern<'a>(
    input: &'a [Instr],
    _: Signature,
    _: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    use Instr::*;
    match input {
        [instr @ SetOutputComment { .. }, input @ ..] => {
            Some((input, (eco_vec![instr.clone()], eco_vec![])))
        }
        [Comment(_) | PushSig(_) | PopSig, input @ ..] => Some((input, (eco_vec![], eco_vec![]))),
        _ => None,
    }
}

fn invert_invert_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(func), Instr::Prim(Primitive::Un, _), input @ ..] = input else {
        return None;
    };
    Some((input, func.instrs(comp).into()))
}

fn invert_rectify_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), Instr::Prim(Primitive::Rectify, _), input @ ..] = input else {
        return None;
    };
    Some((input, f.instrs(comp).into()))
}

fn invert_setinverse_pattern<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [inv @ Instr::PushFunc(_), normal @ Instr::PushFunc(_), set @ Instr::Prim(Primitive::SetInverse, _), input @ ..] =
        input
    else {
        return None;
    };
    Some((input, eco_vec![normal.clone(), inv.clone(), set.clone(),]))
}

fn invert_setunder_setinverse_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(_), Instr::PushFunc(_), Instr::PushFunc(normal), Instr::Prim(Primitive::SetUnder, _), input @ ..] =
        input
    else {
        return None;
    };
    let [Instr::PushFunc(inv), Instr::PushFunc(_), Instr::Prim(Primitive::SetInverse, _)] =
        normal.instrs(comp)
    else {
        return None;
    };
    Some((input, inv.instrs(comp).into()))
}

fn invert_dump_pattern<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    match input {
        [f @ Instr::PushFunc(_), Instr::Prim(Primitive::Dump, span), input @ ..] => Some((
            input,
            eco_vec![f.clone(), Instr::ImplPrim(ImplPrimitive::InvDump, *span)],
        )),
        [f @ Instr::PushFunc(_), Instr::ImplPrim(ImplPrimitive::InvDump, span), input @ ..] => {
            Some((
                input,
                eco_vec![f.clone(), Instr::Prim(Primitive::Dump, *span)],
            ))
        }
        _ => None,
    }
}

fn under_dump_pattern<'a>(
    input: &'a [Instr],
    _: Signature,
    _: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    match input {
        [f @ Instr::PushFunc(_), Instr::Prim(Primitive::Dump, span), input @ ..] => Some((
            input,
            (
                eco_vec![f.clone(), Instr::Prim(Primitive::Dump, *span)],
                eco_vec![f.clone(), Instr::ImplPrim(ImplPrimitive::InvDump, *span)],
            ),
        )),
        [f @ Instr::PushFunc(_), Instr::ImplPrim(ImplPrimitive::InvDump, span), input @ ..] => {
            Some((
                input,
                (
                    eco_vec![f.clone(), Instr::ImplPrim(ImplPrimitive::InvDump, *span)],
                    eco_vec![f.clone(), Instr::Prim(Primitive::Dump, *span)],
                ),
            ))
        }
        _ => None,
    }
}

fn under_from_inverse_pattern<'a>(
    input: &'a [Instr],
    _: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    if input.is_empty() {
        return None;
    }
    let mut end = 1;
    loop {
        if let Some(inverse) = invert_instrs(&input[..end], comp) {
            return Some((&input[end..], (input[..end].into(), inverse)));
        }
        if end == input.len() {
            return None;
        }
        end += 1;
    }
}

fn under_setunder_pattern<'a>(
    input: &'a [Instr],
    _: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::PushFunc(after), Instr::PushFunc(before), Instr::PushFunc(normal), Instr::Prim(Primitive::SetUnder, span), input @ ..] =
        input
    else {
        return None;
    };
    if before.signature().outputs < normal.signature().outputs {
        return None;
    }
    let to_save = before.signature().outputs - normal.signature().outputs;
    let mut befores = EcoVec::from(before.instrs(comp));
    let mut afters = EcoVec::from(after.instrs(comp));
    if to_save > 0 {
        befores.push(Instr::PushTemp {
            stack: TempStack::Under,
            count: to_save,
            span: *span,
        });
        afters.insert(
            0,
            Instr::PopTemp {
                stack: TempStack::Under,
                count: to_save,
                span: *span,
            },
        );
    }
    Some((input, (befores, afters)))
}

fn under_setinverse_setunder_pattern<'a>(
    input: &'a [Instr],
    _: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::PushFunc(_), Instr::PushFunc(normal), Instr::Prim(Primitive::SetInverse, _), input @ ..] =
        input
    else {
        return None;
    };
    let [Instr::PushFunc(after), Instr::PushFunc(before), Instr::PushFunc(_), Instr::Prim(Primitive::SetUnder, _)] =
        normal.instrs(comp)
    else {
        return None;
    };
    Some((
        input,
        (before.instrs(comp).into(), after.instrs(comp).into()),
    ))
}

fn try_temp_wrap<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], &'a Instr, &'a [Instr], &'a Instr)> {
    let (
        instr @ (Instr::PushTemp {
            stack: TempStack::Inline,
            ..
        }
        | Instr::CopyToTemp {
            stack: TempStack::Inline,
            ..
        }),
        input,
    ) = input.split_first()?
    else {
        return None;
    };
    // Find end
    let mut depth = 1;
    let mut end = 0;
    for (i, instr) in input.iter().enumerate() {
        match instr {
            Instr::PushTemp {
                stack: TempStack::Inline,
                ..
            }
            | Instr::CopyToTemp {
                stack: TempStack::Inline,
                ..
            } => depth += 1,
            Instr::PopTemp {
                stack: TempStack::Inline,
                ..
            } => {
                depth -= 1;
                if depth == 0 {
                    end = i;
                    break;
                }
            }
            _ => {}
        }
    }
    if end == 0 {
        return None;
    }
    let (inner, input) = input.split_at(end);
    let end_instr = input.first()?;
    let input = &input[1..];
    Some((input, instr, inner, end_instr))
}

fn invert_temp_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let (input, instr, inner, end_instr) = try_temp_wrap(input, comp)?;
    let mut instrs = invert_instrs(inner, comp)?;
    instrs.insert(0, instr.clone());
    instrs.push(end_instr.clone());
    Some((input, instrs))
}

fn under_temp_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let (input, start_instr, inner, end_instr) = try_temp_wrap(input, comp)?;
    // Calcular inner functions and signatures
    let (inner_befores, inner_afters) = under_instrs(inner, g_sig, comp)?;
    let inner_befores_sig = instrs_signature(&inner_befores).ok()?;
    let inner_afters_sig = instrs_signature(&inner_afters).ok()?;

    let mut befores = inner_befores;
    befores.insert(0, start_instr.clone());
    befores.push(end_instr.clone());

    let afters = match (g_sig.args, g_sig.outputs) {
        (g_args, g_outputs) if g_args > g_outputs || g_args == 1 && g_outputs == 1 => {
            let input_iter = input.iter().filter(|instr| !instr.is_compile_only());
            let inner_iter = inner.iter().filter(|instr| !instr.is_compile_only());
            let both = input_iter.clone().count() >= inner_iter.clone().count()
                && input_iter.zip(inner_iter).all(|(a, b)| a == b);

            if both {
                // Very dirty fix for stuff like: ⍜⊡(⍜∩×↥,) 2 [⍥9]3 ¯1 5
                while let Some(Instr::CopyToTemp {
                    stack: TempStack::Under,
                    count: 1, // This check properlty ignores things like: ⍜∩⊜□∘ ∩(≠@l.) "Hello" "World!"
                    ..
                }) = befores.get(1)
                {
                    befores.remove(1);
                }
            }

            if both && g_args > 1 {
                EcoVec::new()
            } else {
                let mut afters = inner_afters;
                let mut start_instr = start_instr.clone();
                let mut end_instr = end_instr.clone();
                match g_args.cmp(&1) {
                    Ordering::Equal if both || inner_befores_sig.args <= inner_afters_sig.args => {
                        let (start_count, end_count) =
                            temp_pair_counts(&mut start_instr, &mut end_instr)?;
                        *start_count = (*start_count).min(inner_afters_sig.outputs);
                        *end_count = (*end_count).min(inner_befores_sig.outputs);
                        afters.insert(0, start_instr);
                        afters.push(end_instr);
                    }
                    Ordering::Greater => {
                        let (start_count, end_count) =
                            temp_pair_counts(&mut start_instr, &mut end_instr)?;
                        let diff = g_args - 1;
                        *start_count = start_count.saturating_sub(diff);
                        *end_count = end_count.saturating_sub(diff);
                        if *start_count > 0 || *end_count > 0 {
                            afters.insert(0, start_instr);
                            afters.push(end_instr);
                        }
                    }
                    _ => {}
                }
                afters
            }
        }
        (0, 0) | (2, 2) => {
            let mut start_instr = start_instr.clone();
            let mut end_instr = end_instr.clone();
            let (start_count, end_count) = temp_pair_counts(&mut start_instr, &mut end_instr)?;
            *start_count = 1;
            *end_count = 1;
            let mut afters = inner_afters;
            afters.insert(0, start_instr.clone());
            afters.push(end_instr.clone());
            afters
        }
        (args, outputs) if args >= outputs => {
            let mut start_instr = start_instr.clone();
            let mut end_instr = end_instr.clone();
            let (start_count, end_count) = temp_pair_counts(&mut start_instr, &mut end_instr)?;
            let diff = args - outputs;
            if *start_count >= diff && *end_count >= diff {
                *start_count -= diff;
                *end_count -= diff;
            } else {
                return None;
            }
            let mut afters = inner_afters;
            afters.insert(0, start_instr);
            afters.push(end_instr);
            afters
        }
        _ => return None,
    };
    Some((input, (befores, afters)))
}

fn temp_pair_counts<'a, 'b>(
    start_instr: &'a mut Instr,
    end_instr: &'b mut Instr,
) -> Option<(&'a mut usize, &'b mut usize)> {
    match (start_instr, end_instr) {
        (
            Instr::PushTemp {
                count: start_count, ..
            },
            Instr::PopTemp {
                count: end_count, ..
            },
        ) => Some((start_count, end_count)),
        _ => None,
    }
}

fn make_fn(instrs: EcoVec<Instr>, span: usize, comp: &mut Compiler) -> Option<Function> {
    let sig = instrs_signature(&instrs).ok()?;
    let Span::Code(span) = comp.get_span(span) else {
        return None;
    };
    let id = FunctionId::Anonymous(span);
    Some(comp.add_function(id, sig, instrs))
}

fn under_each_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let &[Instr::PushFunc(ref f), Instr::Prim(Primitive::Each, span), ref input @ ..] = input
    else {
        return None;
    };
    let instrs = f.instrs(comp).to_vec();
    let (f_before, f_after) = under_instrs(&instrs, g_sig, comp)?;
    let befores = eco_vec![
        Instr::PushFunc(make_fn(f_before, span, comp)?),
        Instr::Prim(Primitive::Each, span),
    ];
    let afters = eco_vec![
        Instr::PushFunc(make_fn(f_after, span, comp)?),
        Instr::Prim(Primitive::Each, span),
    ];
    Some((input, (befores, afters)))
}

fn under_rows_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let &[Instr::PushFunc(ref f), Instr::Prim(Primitive::Rows, span), ref input @ ..] = input
    else {
        return None;
    };
    let instrs = f.instrs(comp).to_vec();
    let (f_before, f_after) = under_instrs(&instrs, g_sig, comp)?;
    let befores = eco_vec![
        Instr::PushFunc(make_fn(f_before, span, comp)?),
        Instr::Prim(Primitive::Rows, span),
    ];
    let afters = eco_vec![
        Instr::PushFunc(make_fn(f_after, span, comp)?),
        Instr::Prim(Primitive::Reverse, span),
        Instr::Prim(Primitive::Rows, span),
        Instr::Prim(Primitive::Reverse, span),
    ];
    Some((input, (befores, afters)))
}

macro_rules! partition_group {
    ($name:ident, $prim:ident, $impl_prim:ident) => {
        fn $name<'a>(
            input: &'a [Instr],
            g_sig: Signature,
            comp: &mut Compiler,
        ) -> Option<(&'a [Instr], Under)> {
            let &[Instr::PushFunc(ref f), Instr::Prim(Primitive::$prim, span), ref input @ ..] =
                input
            else {
                return None;
            };
            let instrs = f.instrs(comp).to_vec();
            let (f_before, f_after) = under_instrs(&instrs, g_sig, comp)?;
            let befores = eco_vec![
                Instr::CopyToTemp {
                    stack: TempStack::Under,
                    count: 2,
                    span,
                },
                Instr::PushFunc(make_fn(f_before, span, comp)?),
                Instr::Prim(Primitive::$prim, span),
            ];
            let afters = eco_vec![
                Instr::PushTemp {
                    stack: TempStack::Inline,
                    count: 1,
                    span
                },
                Instr::PopTemp {
                    stack: TempStack::Under,
                    count: 2,
                    span
                },
                Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: 1,
                    span
                },
                Instr::PushFunc(make_fn(f_after, span, comp)?),
                Instr::ImplPrim(ImplPrimitive::$impl_prim, span),
            ];
            Some((input, (befores, afters)))
        }
    };
}

partition_group!(under_partition_pattern, Partition, Unpartition);
partition_group!(under_group_pattern, Group, Ungroup);

fn try_array_wrap<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], &'a [Instr], usize, bool)> {
    let [Instr::BeginArray, input @ ..] = input else {
        return None;
    };
    let mut depth = 1;
    let mut end = 0;
    let mut end_arr = None;
    for (i, instr) in input.iter().enumerate() {
        match instr {
            Instr::BeginArray => depth += 1,
            Instr::EndArray { span, boxed } => {
                depth -= 1;
                if depth == 0 {
                    end = i;
                    end_arr = Some((*span, *boxed));
                    break;
                }
            }
            _ => {}
        }
    }
    let (span, boxed) = end_arr?;
    let (inner, input) = input.split_at(end);
    let input = &input[1..];
    Some((input, inner, span, boxed))
}

fn invert_array_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let (input, inner, span, unbox) = try_array_wrap(input, comp)?;
    let mut instrs = invert_instrs(inner, comp)?;
    let count = instrs_signature(&instrs).ok()?.args;
    instrs.insert(0, Instr::Unpack { count, span, unbox });
    Some((input, instrs))
}

fn under_array_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let (input, inner, span, unbox) = try_array_wrap(input, comp)?;
    let (mut befores, mut afters) = under_instrs(inner, g_sig, comp)?;
    befores.insert(0, Instr::BeginArray);
    befores.push(Instr::EndArray { span, boxed: unbox });
    let count = instrs_signature(&befores).ok()?.args;
    afters.insert(0, Instr::Unpack { count, span, unbox });
    Some((input, (befores, afters)))
}

fn invert_unpack_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::Unpack { span, unbox, .. }, input @ ..] = input else {
        return None;
    };
    let mut instrs = invert_instrs(input, comp)?;
    instrs.insert(0, Instr::BeginArray);
    instrs.push(Instr::EndArray {
        span: *span,
        boxed: *unbox,
    });
    Some((input, instrs))
}

fn under_unpack_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [unpack @ Instr::Unpack { count, span, unbox }, input @ ..] = input else {
        return None;
    };
    let (mut befores, mut afters) = under_instrs(input, g_sig, comp)?;
    befores.insert(0, unpack.clone());
    afters.insert(0, Instr::BeginArray);
    afters.extend([
        Instr::TouchStack {
            count: *count,
            span: *span,
        },
        Instr::EndArray {
            span: *span,
            boxed: *unbox,
        },
    ]);
    Some((input, (befores, afters)))
}

fn under_touch_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    _: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [instr @ Instr::TouchStack { count, span }, input @ ..] = input else {
        return None;
    };
    let mut afters = eco_vec![];
    let new_count = (*count + g_sig.outputs).saturating_sub(g_sig.args);
    if new_count > 0 {
        afters.push(Instr::TouchStack {
            count: new_count,
            span: *span,
        });
    }
    Some((input, (eco_vec![instr.clone()], afters)))
}

fn invert_scan_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), Instr::Prim(Primitive::Scan, span), input @ ..] = input else {
        return None;
    };
    let instrs = f.instrs(comp).to_vec();
    let inverse = invert_instrs(&instrs, comp)?;
    let inverse = make_fn(inverse, *span, comp)?;
    Some((
        input,
        eco_vec![
            Instr::PushFunc(inverse),
            Instr::ImplPrim(ImplPrimitive::InvScan, *span)
        ],
    ))
}

fn invert_repeat_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), repeat @ Instr::Prim(Primitive::Repeat, span), input @ ..] = input
    else {
        return None;
    };
    let instrs = f.instrs(comp).to_vec();
    let inverse = invert_instrs(&instrs, comp)?;
    let inverse = make_fn(inverse, *span, comp)?;
    Some((input, eco_vec![Instr::PushFunc(inverse), repeat.clone()]))
}

fn under_repeat_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    Some(match input {
        [Instr::PushFunc(f), repeat @ Instr::Prim(Primitive::Repeat, span), input @ ..] => {
            let instrs = f.instrs(comp).to_vec();
            let (befores, afters) = under_instrs(&instrs, g_sig, comp)?;
            let befores = eco_vec![
                Instr::CopyToTemp {
                    stack: TempStack::Under,
                    count: 1,
                    span: *span
                },
                Instr::PushFunc(make_fn(befores, *span, comp)?),
                repeat.clone()
            ];
            let afters = eco_vec![
                Instr::PopTemp {
                    stack: TempStack::Under,
                    count: 1,
                    span: *span
                },
                Instr::PushFunc(make_fn(afters, *span, comp)?),
                repeat.clone()
            ];
            (input, (befores, afters))
        }
        [push @ Instr::Push(_), Instr::PushFunc(f), repeat @ Instr::Prim(Primitive::Repeat, span), input @ ..] =>
        {
            let instrs = f.instrs(comp).to_vec();
            let (befores, afters) = under_instrs(&instrs, g_sig, comp)?;
            let befores = eco_vec![
                push.clone(),
                Instr::PushFunc(make_fn(befores, *span, comp)?),
                repeat.clone()
            ];
            let afters = eco_vec![
                push.clone(),
                Instr::PushFunc(make_fn(afters, *span, comp)?),
                repeat.clone()
            ];
            (input, (befores, afters))
        }
        _ => return None,
    })
}

fn invert_reduce_mul_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), Instr::Prim(Primitive::Reduce, span), input @ ..] = input else {
        return None;
    };
    let Some((Primitive::Mul, _)) = f.as_flipped_primitive(comp) else {
        return None;
    };
    let instrs = eco_vec![Instr::ImplPrim(ImplPrimitive::Primes, *span)];
    Some((input, instrs))
}

fn invert_primes_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::ImplPrim(ImplPrimitive::Primes, span), input @ ..] = input else {
        return None;
    };
    let f = make_fn(eco_vec![Instr::Prim(Primitive::Mul, *span)], *span, comp)?;
    Some((
        input,
        eco_vec![Instr::PushFunc(f), Instr::Prim(Primitive::Reduce, *span)],
    ))
}

impl<A: InvertPattern, B: InvertPattern> InvertPattern for (A, B) {
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        let (a, b) = self;
        let (input, mut a) = a.invert_extract(input, comp)?;
        let (input, b) = b.invert_extract(input, comp)?;
        a.extend(b);
        Some((input, a))
    }
}

#[derive(Debug)]
struct Either<A, B>(A, B);
impl<A: UnderPattern, B: UnderPattern> UnderPattern for Either<A, B> {
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        let Either(a, b) = self;
        a.under_extract(input, g_sig, comp)
            .or_else(|| b.under_extract(input, g_sig, comp))
    }
}

impl<A: UnderPattern, B: UnderPattern> UnderPattern for (A, B) {
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b) = self;
        let (input, (mut a_before, a_after)) = a.under_extract(input, g_sig, comp)?;
        let (input, (b_before, mut b_after)) = b.under_extract(input, g_sig, comp)?;
        a_before.extend(b_before);
        b_after.extend(a_after);
        Some((input, (a_before, b_after)))
    }
}

impl<A: InvertPattern, B: InvertPattern, C: InvertPattern> InvertPattern for (A, B, C) {
    fn invert_extract<'a>(
        &self,
        mut input: &'a [Instr],
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        let (a, b, c) = self;
        let (inp, mut a) = a.invert_extract(input, comp)?;
        input = inp;
        let (inp, b) = b.invert_extract(input, comp)?;
        input = inp;
        let (inp, c) = c.invert_extract(input, comp)?;
        a.extend(b);
        a.extend(c);
        Some((inp, a))
    }
}

struct IgnoreMany<T>(T);
impl<T: InvertPattern> InvertPattern for IgnoreMany<T> {
    fn invert_extract<'a>(
        &self,
        mut input: &'a [Instr],
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        while let Some((inp, _)) = self.0.invert_extract(input, comp) {
            input = inp;
        }
        Some((input, EcoVec::new()))
    }
}

impl InvertPattern for Primitive {
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        _: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        let next = input.first()?;
        match next {
            Instr::Prim(prim, span) if prim == self => {
                Some((&input[1..], eco_vec![Instr::Prim(*prim, *span)]))
            }
            _ => None,
        }
    }
}

impl<T> InvertPattern for (&[Primitive], &[T])
where
    T: AsInstr,
{
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        _: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        let (a, b) = *self;
        if a.len() > input.len() {
            return None;
        }
        let mut spans = Vec::new();
        for (instr, prim) in input.iter().zip(a.iter()) {
            match instr {
                Instr::Prim(instr_prim, span) if instr_prim == prim => spans.push(*span),
                _ => return None,
            }
        }
        Some((
            &input[a.len()..],
            b.iter()
                .zip(spans.iter().cycle())
                .map(|(p, s)| p.as_instr(*s))
                .collect(),
        ))
    }
}

impl<A, B> UnderPattern for (&[Primitive], &[A], &[B])
where
    A: AsInstr,
    B: AsInstr,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        _: Signature,
        _: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b, c) = *self;
        if a.len() > input.len() {
            return None;
        }
        let mut spans = Vec::new();
        for (instr, prim) in input.iter().zip(a.iter()) {
            match instr {
                Instr::Prim(instr_prim, span) if instr_prim == prim => spans.push(*span),
                _ => return None,
            }
        }
        Some((
            &input[a.len()..],
            (
                b.iter()
                    .zip(spans.iter().cycle())
                    .map(|(p, s)| p.as_instr(*s))
                    .collect(),
                c.iter()
                    .zip(spans.iter().cycle())
                    .map(|(p, s)| p.as_instr(*s))
                    .collect(),
            ),
        ))
    }
}

impl<A, B> UnderPattern for (&[ImplPrimitive], &[A], &[B])
where
    A: AsInstr,
    B: AsInstr,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        _: Signature,
        _: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b, c) = *self;
        if a.len() > input.len() {
            return None;
        }
        let mut spans = Vec::new();
        for (instr, prim) in input.iter().zip(a.iter()) {
            match instr {
                Instr::ImplPrim(instr_prim, span) if instr_prim == prim => spans.push(*span),
                _ => return None,
            }
        }
        Some((
            &input[a.len()..],
            (
                b.iter()
                    .zip(spans.iter().cycle())
                    .map(|(p, s)| p.as_instr(*s))
                    .collect(),
                c.iter()
                    .zip(spans.iter().cycle())
                    .map(|(p, s)| p.as_instr(*s))
                    .collect(),
            ),
        ))
    }
}

impl<T, const A: usize, const B: usize> InvertPattern for ([Primitive; A], [T; B])
where
    T: AsInstr,
{
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        let (a, b) = self;
        (a.as_ref(), b.as_ref()).invert_extract(input, comp)
    }
}

impl<T, U, const A: usize, const B: usize, const C: usize> UnderPattern
    for ([Primitive; A], [T; B], [U; C])
where
    T: AsInstr,
    U: AsInstr,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b, c) = self;
        (a.as_ref(), b.as_ref(), c.as_ref()).under_extract(input, g_sig, comp)
    }
}

impl<T, U, const A: usize, const B: usize, const C: usize> UnderPattern
    for ([ImplPrimitive; A], [T; B], [U; C])
where
    T: AsInstr,
    U: AsInstr,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b, c) = self;
        (a.as_ref(), b.as_ref(), c.as_ref()).under_extract(input, g_sig, comp)
    }
}

impl<F> InvertPattern for F
where
    F: for<'a> Fn(&'a [Instr], &mut Compiler) -> Option<(&'a [Instr], EcoVec<Instr>)>,
{
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        self(input, comp)
    }
}

struct UnderPatternFn<F>(F, &'static str);
impl<F> fmt::Debug for UnderPatternFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UnderPatternFn({})", self.1)
    }
}
impl<F> UnderPattern for UnderPatternFn<F>
where
    F: for<'a> Fn(&'a [Instr], Signature, &mut Compiler) -> Option<(&'a [Instr], Under)>,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        (self.0)(input, g_sig, comp)
    }
}

#[derive(Debug)]
struct Val;
impl InvertPattern for Val {
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        _: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        if input.is_empty() {
            return Some((input, EcoVec::new()));
        }
        for len in (1..input.len()).rev() {
            let chunk = &input[..len];
            if let Ok(sig) = instrs_signature(chunk) {
                if sig.args == 0 && sig.outputs == 1 && !chunk.iter().any(Instr::is_temp) {
                    return Some((&input[len..], chunk.into()));
                }
            }
        }
        match input.first() {
            Some(instr @ Instr::Push(_)) => Some((&input[1..], eco_vec![instr.clone()])),
            Some(instr @ Instr::Prim(prim, _))
                if prim.args() == Some(0) && prim.outputs() == Some(0) =>
            {
                Some((&input[1..], eco_vec![instr.clone()]))
            }
            Some(Instr::BeginArray) => {
                let mut depth = 1;
                let mut i = 1;
                loop {
                    if let Instr::EndArray { .. } = input.get(i)? {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    } else if let Instr::BeginArray = input.get(i)? {
                        depth += 1;
                    }
                    i += 1;
                }
                Some((&input[i + 1..], input[..=i].into()))
            }
            _ => None,
        }
    }
}
impl UnderPattern for Val {
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        _: Signature,
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], Under)> {
        if let Some((input, inverted)) = self.invert_extract(input, comp) {
            Some((input, (inverted, EcoVec::new())))
        } else {
            None
        }
    }
}

fn box_as_instr(instr: impl AsInstr + 'static) -> Box<dyn AsInstr> {
    Box::new(instr)
}

trait AsInstr: fmt::Debug {
    fn as_instr(&self, span: usize) -> Instr;
}

#[derive(Debug, Clone, Copy)]
struct PushTempN(usize);
impl AsInstr for PushTempN {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PushTemp {
            stack: TempStack::Under,
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CopyToTempN(usize);
impl AsInstr for CopyToTempN {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::CopyToTemp {
            stack: TempStack::Under,
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PopTempN(usize);
impl AsInstr for PopTempN {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PopTemp {
            stack: TempStack::Under,
            count: self.0,
            span,
        }
    }
}

impl AsInstr for i32 {
    fn as_instr(&self, _: usize) -> Instr {
        Instr::push(Value::from(*self))
    }
}

impl AsInstr for Primitive {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::Prim(*self, span)
    }
}

impl AsInstr for ImplPrimitive {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::ImplPrim(*self, span)
    }
}

impl AsInstr for Box<dyn AsInstr> {
    fn as_instr(&self, span: usize) -> Instr {
        self.as_ref().as_instr(span)
    }
}
