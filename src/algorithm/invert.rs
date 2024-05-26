//! Algorithms for invert and under

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt,
};

use ecow::{eco_vec, EcoString, EcoVec};
use regex::Regex;

use crate::{
    check::{instrs_signature, instrs_signature_no_temp},
    primitive::{ImplPrimitive, Primitive},
    Assembly, BindingKind, Compiler, Function, FunctionId, Instr, Signature, Span, SysOp,
    TempStack, Uiua, UiuaResult, Value,
};

use super::IgnoreError;

pub(crate) const DEBUG: bool = false;

pub(crate) fn match_pattern(env: &mut Uiua) -> UiuaResult {
    let pat = env.pop(1)?;
    let val = env.pop(2)?;
    if val != pat {
        return Err(env.pattern_match_error());
    }
    Ok(())
}

pub(crate) fn match_format_pattern(parts: EcoVec<EcoString>, env: &mut Uiua) -> UiuaResult {
    let val = env
        .pop(1)?
        .as_string(env, "Matching a format pattern expects a string")?;
    match parts.as_slice() {
        [] => {}
        [part] => {
            if val != part.as_ref() {
                return Err(env.pattern_match_error());
            }
        }
        _ => {
            thread_local! {
                static CACHE: RefCell<HashMap<EcoVec<EcoString>, Regex>> = RefCell::new(HashMap::new());
            }
            CACHE.with(|cache| {
                let mut cache = cache.borrow_mut();
                let re = cache.entry(parts.clone()).or_insert_with(|| {
                    let mut re = String::new();
                    re.push_str("(?s)^");
                    for (i, part) in parts.iter().enumerate() {
                        if i > 0 {
                            re.push_str("(.+?|.*)");
                        }
                        re.push_str(&regex::escape(part));
                    }
                    re.push('$');
                    Regex::new(&re).unwrap()
                });
                if !re.is_match(val.as_ref()) {
                    return Err(env.pattern_match_error());
                }
                let captures = re.captures(val.as_ref()).unwrap();
                let caps: Vec<_> = captures.iter().skip(1).flatten().collect();
                for cap in caps.into_iter().rev() {
                    env.push(cap.as_str());
                }
                Ok(())
            })?;
        }
    }
    Ok(())
}

fn prim_inverse(prim: Primitive, span: usize) -> Option<Instr> {
    use ImplPrimitive::*;
    use Primitive::*;
    Some(match prim {
        Identity => Instr::Prim(Identity, span),
        Flip => Instr::Prim(Flip, span),
        Pop => Instr::ImplPrim(UnPop, span),
        Neg => Instr::Prim(Neg, span),
        Not => Instr::Prim(Not, span),
        Sin => Instr::ImplPrim(Asin, span),
        Atan => Instr::ImplPrim(UnAtan, span),
        Complex => Instr::ImplPrim(UnComplex, span),
        Reverse => Instr::Prim(Reverse, span),
        Transpose => Instr::ImplPrim(TransposeN(-1), span),
        Bits => Instr::ImplPrim(UnBits, span),
        Couple => Instr::ImplPrim(UnCouple, span),
        Box => Instr::ImplPrim(UnBox, span),
        Where => Instr::ImplPrim(UnWhere, span),
        Utf => Instr::ImplPrim(UnUtf, span),
        Parse => Instr::ImplPrim(UnParse, span),
        Fix => Instr::ImplPrim(UnFix, span),
        Map => Instr::ImplPrim(UnMap, span),
        Trace => Instr::ImplPrim(UnTrace, span),
        Stack => Instr::ImplPrim(UnStack, span),
        Join => Instr::ImplPrim(UnJoin, span),
        Keep => Instr::ImplPrim(UnKeep, span),
        Sys(SysOp::GifDecode) => Instr::Prim(Sys(SysOp::GifEncode), span),
        Sys(SysOp::GifEncode) => Instr::Prim(Sys(SysOp::GifDecode), span),
        Sys(SysOp::AudioDecode) => Instr::Prim(Sys(SysOp::AudioEncode), span),
        Sys(SysOp::AudioEncode) => Instr::Prim(Sys(SysOp::AudioDecode), span),
        Sys(SysOp::ImDecode) => Instr::Prim(Sys(SysOp::ImEncode), span),
        Sys(SysOp::ImEncode) => Instr::Prim(Sys(SysOp::ImDecode), span),
        Sys(SysOp::ClipboardSet) => Instr::Prim(Sys(SysOp::ClipboardGet), span),
        Sys(SysOp::ClipboardGet) => Instr::Prim(Sys(SysOp::ClipboardSet), span),
        Json => Instr::ImplPrim(UnJson, span),
        Csv => Instr::ImplPrim(UnCsv, span),
        Xlsx => Instr::ImplPrim(UnXlsx, span),
        _ => return None,
    })
}

fn impl_prim_inverse(prim: ImplPrimitive, span: usize) -> Option<Instr> {
    use ImplPrimitive::*;
    use Primitive::*;
    Some(match prim {
        UnPop => Instr::Prim(Pop, span),
        Asin => Instr::Prim(Sin, span),
        TransposeN(n) => Instr::ImplPrim(TransposeN(-n), span),
        UnBits => Instr::Prim(Bits, span),
        UnWhere => Instr::Prim(Where, span),
        UnUtf => Instr::Prim(Utf, span),
        UnAtan => Instr::Prim(Atan, span),
        UnComplex => Instr::Prim(Complex, span),
        UnCouple => Instr::Prim(Couple, span),
        UnParse => Instr::Prim(Parse, span),
        UnFix => Instr::Prim(Fix, span),
        UnMap => Instr::Prim(Map, span),
        UnTrace => Instr::Prim(Trace, span),
        UnStack => Instr::Prim(Stack, span),
        UnJoin => Instr::Prim(Join, span),
        UnKeep => Instr::Prim(Keep, span),
        UnBox => Instr::Prim(Box, span),
        UnJson => Instr::Prim(Json, span),
        UnCsv => Instr::Prim(Csv, span),
        UnXlsx => Instr::Prim(Xlsx, span),
        BothTrace => Instr::ImplPrim(UnBothTrace, span),
        UnBothTrace => Instr::ImplPrim(BothTrace, span),
        _ => return None,
    })
}

/// Construct an `un`/`under` pattern
macro_rules! pat {
    (($($matching:expr),*), ($($before:expr),*) $(,($($after:expr),*))? $(,)?) => {
        (
            [$($matching,)*],
            [$(&$before as &dyn AsInstr),*],
            $([$(&$after as &dyn AsInstr),*],)?
        )
    };
    ($matching:expr, ($($before:expr),*) $(,($($after:expr),*))? $(,)?) => {
        (
            [$matching],
            [$(&$before as &dyn AsInstr),*],
            $([$(&$after as &dyn AsInstr),*],)?
        )
    };
    ($matching:expr, $before:expr $(,($($after:expr),*))? $(,)?) => {
        pat!($matching, ($before) $(,($($after),*))?)
    };
}

static INVERT_PATTERNS: &[&dyn InvertPattern] = {
    use ImplPrimitive::*;
    use Primitive::*;
    &[
        &InvertPatternFn(invert_call_pattern, "call"),
        &InvertPatternFn(invert_un_pattern, "un"),
        &InvertPatternFn(invert_dump_pattern, "dump"),
        &InvertPatternFn(invert_setinv_pattern, "setinv"),
        &InvertPatternFn(invert_setund_setinv_pattern, "setund_setinv"),
        &InvertPatternFn(invert_trivial_pattern, "trivial"),
        &InvertPatternFn(invert_array_pattern, "array"),
        &InvertPatternFn(invert_unpack_pattern, "unpack"),
        &InvertPatternFn(invert_scan_pattern, "scan"),
        &InvertPatternFn(invert_repeat_pattern, "repeat"),
        &InvertPatternFn(invert_reduce_mul_pattern, "reduce_mul"),
        &InvertPatternFn(invert_primes_pattern, "primes"),
        &InvertPatternFn(invert_format_pattern, "format"),
        &InvertPatternFn(invert_join_val_pattern, "join_val"),
        &InvertPatternFn(invert_insert_pattern, "insert"),
        &InvertPatternFn(invert_split_pattern, "split"),
        &InvertPatternFn(invert_rows_pattern, "rows"),
        &InvertPatternFn(invert_dup_pattern, "dup"),
        &InvertPatternFn(invert_stack_swizzle_pattern, "stack swizzle"),
        &InvertPatternFn(invert_select_pattern, "select"),
        &(Val, InvertPatternFn(invert_repeat_pattern, "repeat")),
        &(Val, ([Rotate], [Neg, Rotate])),
        &pat!(Sqrt, (Dup, Mul)),
        &(Val, IgnoreMany(Flip), ([Add], [Sub])),
        &(Val, ([Sub], [Add])),
        &(Val, ([Flip, Sub], [Flip, Sub])),
        &(Val, IgnoreMany(Flip), ([Mul], [Div])),
        &(Val, ([Div], [Mul])),
        &(Val, ([Flip, Div], [Flip, Div])),
        &(Val, pat!(Pow, (1, Flip, Div, Pow))),
        &(Val, ([Flip, Pow], [Log])),
        &(Val, ([Log], [Flip, Pow])),
        &(Val, pat!((Flip, Log), (Flip, 1, Flip, Div, Pow))),
        &pat!((Dup, Add), (2, Div)),
        &([Dup, Mul], [Sqrt]),
        &(Val, pat!(Min, (Over, Ge, 1, MatchPattern))),
        &(Val, pat!(Max, (Over, Le, 1, MatchPattern))),
        &InvertPatternFn(invert_temp_pattern, "temp"),
        &InvertPatternFn(invert_push_pattern, "push"),
    ]
};

static PSEUDO_INVERT_PATTERNS: &[&dyn InvertPattern] = {
    use ImplPrimitive::*;
    use Primitive::*;
    &[
        &([Add], [Sub]),
        &([Sub], [Add]),
        &([Mul], [Div]),
        &([Div], [Mul]),
        &([Rotate], [Neg, Rotate]),
        &([Neg, Rotate], [Rotate]),
        &([Min], [Min]),
        &([Max], [Max]),
        &pat!(
            Join,
            (
                CopyToInline(1),
                Shape,
                UnJoinPattern,
                PopInline(1),
                ImplPrimitive::MatchPattern
            )
        ),
    ]
};

/// Invert a sequence of instructions
pub(crate) fn invert_instrs(instrs: &[Instr], comp: &mut Compiler) -> Option<EcoVec<Instr>> {
    if instrs.is_empty() {
        return Some(EcoVec::new());
    }
    if DEBUG {
        println!("inverting {:?}", instrs);
    }

    let mut inverted = EcoVec::new();
    let mut curr_instrs = instrs;
    'find_pattern: loop {
        for pattern in INVERT_PATTERNS {
            if let Some((input, mut inv)) = pattern.invert_extract(curr_instrs, comp) {
                if DEBUG {
                    println!(
                        "matched pattern {:?} on {:?} to {inv:?}",
                        pattern,
                        &curr_instrs[..curr_instrs.len() - input.len()],
                    );
                }
                inv.extend(inverted);
                inverted = inv;
                if input.is_empty() {
                    if DEBUG {
                        println!("inverted {:?} to {:?}", instrs, inverted);
                    }
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

/// Calculate the "before" and "after" instructions for `under`ing a sequence of instructions.
///
/// `g_sig` is the signature of `under`'s second function
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
    if instrs.len() > 30 {
        return None;
    }

    /// Copy 1 value to the temp stack before the "before", and pop it before the "after"
    macro_rules! stash1 {
        (($($before:expr),*)) => {
            pat!(($($before),*), (CopyToUnder(1), $($before),*), (PopUnder(1), $($before),*))
        };
        (($($before:expr),*), ($($after:expr),*)) => {
            pat!(($($before),*), (CopyToUnder(1), $($before),*), (PopUnder(1), $($after),*))
        };
        (($($before:expr),*), $after:expr) => {
            pat!(($($before),*), (CopyToUnder(1), $($before),*), (PopUnder(1), $after))
        };
        ($before:expr, ($($after:expr),*)) => {
            pat!($before, (CopyToUnder(1), $before), (PopUnder(1), $($after),*))
        };
        ($before:expr, $after:expr) => {
            pat!($before, (CopyToUnder(1), $before), (PopUnder(1), $after))
        };
    }

    /// Copy 1 value to the temp stack after the "before", and pop it before the "after"
    macro_rules! store1copy {
        ($before:expr, $after:expr) => {
            pat!($before, ($before, CopyToUnder(1)), (PopUnder(1), $after),)
        };
    }

    /// Copy 2 values to the temp stack before the "before", and pop them before the "after"
    macro_rules! stash2 {
        (($($before:expr),*)) => {
            pat!(($($before),*), (CopyToUnder(2), $($before),*), (PopUnder(2), $($before),*))
        };
        (($($before:expr),*), ($($after:expr),*)) => {
            pat!(($($before),*), (CopyToUnder(2), $($before),*), (PopUnder(2), $($after),*))
        };
        (($($before:expr),*), $after:expr) => {
            pat!(($($before),*), (CopyToUnder(2), $($before),*), (PopUnder(2), $after))
        };
        ($before:expr, ($($after:expr),*)) => {
            pat!($before, (CopyToUnder(2), $before), (PopUnder(2), $($after),*))
        };
        ($before:expr, $after:expr) => {
            pat!($before, (CopyToUnder(2), $before), (PopUnder(2), $after))
        };
    }

    /// Handle dyadic arithmetic operations
    macro_rules! dyad {
        (Flip, $before:expr, $after:expr) => {
            stash1!((Flip, $before), $after)
        };
        (Flip, $before:expr) => {
            stash1!((Flip, $before))
        };
        ($before:expr, $after:expr) => {
            stash1!($before, $after)
        };
    }

    /// Rounding operations
    macro_rules! round {
        ($before:expr) => {
            pat!(
                $before,
                (Dup, $before, Flip, Over, Sub, PushToUnder(1)),
                (PopUnder(1), Add)
            )
        };
    }

    /// A dyadic operation can have a constant value either inside or outside the `under`
    macro_rules! maybe_val {
        ($pat:expr) => {
            Either($pat, (Val, $pat))
        };
    }

    let patterns: &[&dyn UnderPattern] = &[
        // Hand-written patterns
        &maybe_val!(UnderPatternFn(under_fill_pattern, "fill")),
        &UnderPatternFn(under_call_pattern, "call"),
        &UnderPatternFn(under_dump_pattern, "dump"),
        &UnderPatternFn(under_rows_pattern, "rows"),
        &UnderPatternFn(under_each_pattern, "each"),
        &UnderPatternFn(under_partition_pattern, "partition"),
        &UnderPatternFn(under_group_pattern, "group"),
        &UnderPatternFn(under_setinv_setund_pattern, "setinv setund"), // This must come before setinv
        &maybe_val!(UnderPatternFn(under_setinv_pattern, "setinv")),
        &maybe_val!(UnderPatternFn(under_setund_pattern, "setund")),
        &UnderPatternFn(under_trivial_pattern, "trivial"),
        &UnderPatternFn(under_array_pattern, "array"),
        &UnderPatternFn(under_unpack_pattern, "unpack"),
        &UnderPatternFn(under_touch_pattern, "touch"),
        &UnderPatternFn(under_repeat_pattern, "repeat"),
        &UnderPatternFn(under_fold_pattern, "fold"),
        &UnderPatternFn(under_switch_pattern, "switch"),
        &UnderPatternFn(under_dup_pattern, "dup"),
        // Basic math
        &dyad!(Flip, Add, Sub),
        &dyad!(Flip, Mul, Div),
        &dyad!(Flip, Sub),
        &dyad!(Flip, Div),
        &dyad!(Add, Sub),
        &dyad!(Sub, Add),
        &dyad!(Mul, Div),
        &dyad!(Div, Mul),
        &maybe_val!(pat!(
            Mod,
            (Over, Over, Flip, Over, Div, Floor, Mul, PushToUnder(1), Mod),
            (PopUnder(1), Add)
        )),
        // Exponential math
        &maybe_val!(stash1!((Flip, Pow), Log)),
        &maybe_val!(stash1!(Pow, (1, Flip, Div, Pow))),
        &maybe_val!(stash1!(Log, (Flip, Pow))),
        &maybe_val!(stash1!((Flip, Log), (Flip, 1, Flip, Div, Pow))),
        // Rounding
        &round!(Floor),
        &round!(Ceil),
        &round!(Round),
        // Sign ops
        &stash1!(Abs, (Sign, Mul)),
        &stash1!(Sign, (Abs, Mul)),
        // Pop
        &pat!(Pop, (PushToUnder(1)), (PopUnder(1))),
        // Array restructuring
        &maybe_val!(stash2!(Take, UndoTake)),
        &maybe_val!(stash2!(Drop, UndoDrop)),
        &maybe_val!(pat!(
            Keep,
            (CopyToUnder(2), Keep),
            (PopUnder(1), Flip, PopUnder(1), UndoKeep),
        )),
        &stash1!(Rotate, (Neg, Rotate)),
        &maybe_val!(pat!(
            Join,
            (Over, Shape, Over, Shape, PushToUnder(2), Join),
            (PopUnder(2), UndoJoin),
        )),
        // Rise and fall
        &pat!(
            Rise,
            (CopyToUnder(1), Rise, Dup, Rise, PushToUnder(1)),
            (PopUnder(1), Select, PopUnder(1), Flip, Select)
        ),
        &pat!(
            Fall,
            (CopyToUnder(1), Fall, Dup, Rise, PushToUnder(1)),
            (PopUnder(1), Select, PopUnder(1), Flip, Select)
        ),
        // Index of
        &maybe_val!(pat!(
            IndexOf,
            (Over, PushToUnder(1), IndexOf),
            (PopUnder(1), Flip, Select)
        )),
        // Value retrieval
        &stash1!(First, UndoFirst),
        &stash1!(Last, UndoLast),
        &maybe_val!(stash2!(Pick, UndoPick)),
        &maybe_val!(stash2!(Select, UndoSelect)),
        // Map control
        &maybe_val!(pat!(
            Get,
            (CopyToUnder(2), Get),
            (PopUnder(1), Flip, PopUnder(1), Insert),
        )),
        &maybe_val!(stash2!(Remove, UndoRemove)),
        &maybe_val!(pat!(
            Insert,
            (CopyToUnder(3), Insert),
            (PopUnder(3), UndoInsert)
        )),
        // Shaping
        &pat!(Fix, (Fix), (UndoFix)),
        &pat!(UndoFix, (UndoFix), (Fix)),
        &stash1!(Shape, (Flip, Reshape)),
        &pat!(
            Deshape,
            (Dup, Shape, PushToUnder(1), Deshape),
            (PopUnder(1), 0, UndoRerank),
        ),
        &maybe_val!(pat!(
            Rerank,
            (Over, Shape, Over, PushToUnder(2), Rerank),
            (PopUnder(2), UndoRerank),
        )),
        &maybe_val!(pat!(
            Reshape,
            (Over, Shape, PushToUnder(1), Reshape),
            (PopUnder(1), UndoReshape),
        )),
        // Classify and deduplicate
        &pat!(
            Classify,
            (Dup, Deduplicate, PushToUnder(1), Classify),
            (PopUnder(1), Flip, Select),
        ),
        &pat!(
            Deduplicate,
            (Dup, Classify, PushToUnder(1), Deduplicate),
            (PopUnder(1), Select),
        ),
        // System stuff
        &pat!(Now, (Now, PushToUnder(1)), (PopUnder(1), Now, Flip, Sub)),
        &maybe_val!(store1copy!(Sys(SysOp::FOpen), Sys(SysOp::Close))),
        &maybe_val!(store1copy!(Sys(SysOp::FCreate), Sys(SysOp::Close))),
        &maybe_val!(store1copy!(Sys(SysOp::RunStream), Sys(SysOp::Close))),
        &maybe_val!(store1copy!(Sys(SysOp::TcpConnect), Sys(SysOp::Close))),
        &maybe_val!(store1copy!(Sys(SysOp::TlsConnect), Sys(SysOp::Close))),
        &maybe_val!(store1copy!(Sys(SysOp::TcpAccept), Sys(SysOp::Close))),
        &maybe_val!(store1copy!(Sys(SysOp::TcpListen), Sys(SysOp::Close))),
        &maybe_val!(store1copy!(Sys(SysOp::TlsListen), Sys(SysOp::Close))),
        &maybe_val!(stash1!(Sys(SysOp::FReadAllStr), Sys(SysOp::FWriteAll))),
        &maybe_val!(stash1!(Sys(SysOp::FReadAllBytes), Sys(SysOp::FWriteAll))),
        &pat!(BothTrace, (BothTrace), (UnTrace)),
        // Patterns that need to be last
        &UnderPatternFn(under_flip_pattern, "flip"),
        &UnderPatternFn(under_push_temp_pattern, "push temp"),
        &UnderPatternFn(under_copy_temp_pattern, "copy temp"),
        &UnderPatternFn(under_un_pattern, "un"),
        &UnderPatternFn(under_from_inverse_pattern, "from inverse"), // These must come last!
    ];

    if DEBUG {
        println!("undering {:?}", instrs);
    }

    let comp_instrs_backup = comp.asm.instrs.clone();

    let mut befores = EcoVec::new();
    let mut afters = EcoVec::new();
    let mut curr_instrs = instrs;
    'find_pattern: loop {
        for pattern in patterns {
            if let Some((input, (bef, aft))) = pattern.under_extract(curr_instrs, g_sig, comp) {
                if DEBUG {
                    println!(
                        "matched pattern {:?} on {:?} to {bef:?} {aft:?}",
                        pattern,
                        &curr_instrs[..curr_instrs.len() - input.len()],
                    );
                }
                befores.extend(bef);
                afters = aft.into_iter().chain(afters).collect();
                if input.is_empty() {
                    let befores = resolve_uns(befores, comp)?;
                    let afters = resolve_uns(afters, comp)?;
                    if DEBUG {
                        println!("undered {:?} to {:?} {:?}", instrs, befores, afters);
                    }
                    return Some((befores, afters));
                }
                curr_instrs = input;
                continue 'find_pattern;
            }
        }
        break;
    }

    comp.asm.instrs = comp_instrs_backup;
    // println!("under {:?} failed with remaining {:?}", instrs, curr_instrs);

    None
}

fn resolve_uns(instrs: EcoVec<Instr>, comp: &mut Compiler) -> Option<EcoVec<Instr>> {
    fn contains_un(instrs: &[Instr], asm: &Assembly) -> bool {
        instrs.iter().any(|instr| match instr {
            Instr::PushFunc(f) => contains_un(f.instrs(asm), asm),
            Instr::Prim(Primitive::Un, _) => true,
            _ => false,
        })
    }
    if !contains_un(&instrs, &comp.asm) {
        return Some(instrs);
    }
    fn resolve_uns(instrs: EcoVec<Instr>, comp: &mut Compiler) -> Option<EcoVec<Instr>> {
        let mut resolved = EcoVec::new();
        for instr in instrs {
            match instr {
                Instr::Prim(Primitive::Un, _) => {
                    let prev = resolved.pop()?;
                    let Instr::PushFunc(f) = prev else {
                        return None;
                    };
                    let instrs = f.instrs(comp).to_vec();
                    let inverse = invert_instrs(&instrs, comp)?;
                    resolved.extend(inverse);
                }
                Instr::PushFunc(f) => {
                    let instrs = f.instrs(comp);
                    if !contains_un(instrs, &comp.asm) {
                        resolved.push(Instr::PushFunc(f));
                        continue;
                    }
                    let instrs = EcoVec::from(instrs);
                    let id = f.id.clone();
                    let resolved_f = resolve_uns(instrs, comp)?;
                    let sig = instrs_signature(&resolved_f).ok()?;
                    let func = comp.make_function(id, sig, resolved_f);
                    resolved.push(Instr::PushFunc(func));
                }
                instr => resolved.push(instr.clone()),
            }
        }
        Some(resolved)
    }
    resolve_uns(instrs, comp)
}

trait InvertPattern: fmt::Debug + Sync {
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

fn invert_un_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), Instr::Prim(Primitive::Un, _), input @ ..] = input else {
        return None;
    };
    let f_instrs = EcoVec::from(f.instrs(comp));
    let double_inv = invert_instrs(&invert_instrs(&f_instrs, comp)?, comp)?;
    Some((input, double_inv))
}

fn under_un_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::PushFunc(f), Instr::Prim(Primitive::Un, _), input @ ..] = input else {
        return None;
    };
    let instrs = EcoVec::from(f.instrs(comp));
    let befores = invert_instrs(&instrs, comp)?;
    if let [Instr::PushFunc(_), Instr::PushFunc(_), Instr::Prim(Primitive::SetInverse, _)] =
        befores.as_slice()
    {
        if let Some(under) = under_instrs(&befores, g_sig, comp) {
            return Some((input, under));
        }
    }
    Some((input, (befores, instrs)))
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
        [instr @ (SetOutputComment { .. } | TouchStack { .. }), input @ ..] => {
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

fn invert_dup_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::Prim(Primitive::Dup, dup_span), input @ ..] = input else {
        return None;
    };
    let Some(dyadic_i) = (0..=input.len())
        .find(|&i| instrs_signature_no_temp(&input[..i]).is_some_and(|sig| sig == (2, 1)))
    else {
        let sig = instrs_signature(input).ok()?;
        return if sig.args == sig.outputs {
            let inv = eco_vec![
                Instr::Prim(Primitive::Over, *dup_span),
                Instr::ImplPrim(ImplPrimitive::MatchPattern, *dup_span)
            ];
            Some((input, inv))
        } else {
            None
        };
    };
    let dyadic_whole = &input[..dyadic_i];
    let input = &input[dyadic_i..];
    let monadic_i = (0..=dyadic_whole.len()).rev().find(|&i| {
        instrs_signature_no_temp(&dyadic_whole[..i])
            .is_some_and(|sig| sig.args == 0 && sig.outputs == 0)
    })?;
    let monadic_part = &dyadic_whole[..monadic_i];
    let dyadic_part = &dyadic_whole[monadic_i..];
    if DEBUG {
        println!("inverse monadic part: {monadic_part:?}");
        println!("inverse dyadic part: {dyadic_part:?}");
    }
    let monadic_inv = invert_instrs(monadic_part, comp)?;
    let inverse = match *dyadic_part {
        [Instr::Prim(Primitive::Add, span)] => {
            let mut inv = monadic_inv;
            inv.push(Instr::push(2));
            inv.push(Instr::Prim(Primitive::Div, span));
            inv
        }
        [Instr::Prim(Primitive::Mul, span)] => {
            let mut inv = eco_vec![Instr::Prim(Primitive::Sqrt, span)];
            if !monadic_inv.is_empty() {
                inv.push(Instr::Prim(Primitive::Dup, *dup_span));
                inv.extend(monadic_inv);
                inv.push(Instr::Prim(Primitive::Pop, span));
            }
            inv
        }
        _ => return None,
    };
    Some((input, inverse))
}

fn under_dup_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::Prim(Primitive::Dup, dup_span), input @ ..] = input else {
        return None;
    };
    let dyadic_i = (0..=input.len())
        .find(|&i| instrs_signature_no_temp(&input[..i]).is_some_and(|sig| sig == (2, 1)))?;
    let dyadic_whole = &input[..dyadic_i];
    let input = &input[dyadic_i..];
    let (monadic_i, monadic_sig) = (0..=dyadic_whole.len())
        .rev()
        .filter_map(|i| instrs_signature_no_temp(&dyadic_whole[..i]).map(|sig| (i, sig)))
        .find(|(_, sig)| sig.args == sig.outputs)?;
    let monadic_part = &dyadic_whole[..monadic_i];
    let dyadic_part = &dyadic_whole[monadic_i..];
    if DEBUG {
        println!("under monadic part: {monadic_part:?}");
        println!("under dyadic part: {dyadic_part:?}");
    }
    let (monadic_befores, monadic_afters) = under_instrs(monadic_part, g_sig, comp)?;

    let mut befores = eco_vec![Instr::Prim(Primitive::Dup, *dup_span),];
    let mut afters = EcoVec::new();

    let temp = |temp: bool| {
        if temp {
            befores.push(Instr::CopyToTemp {
                stack: TempStack::Under,
                count: 1,
                span: *dup_span,
            });
        }
        befores.extend(monadic_befores);
        befores.extend(dyadic_part.iter().cloned());

        if temp {
            afters.push(Instr::PopTemp {
                stack: TempStack::Under,
                count: 1,
                span: *dup_span,
            });
        }
    };

    match dyadic_part {
        [Instr::Prim(Primitive::Add, span)] if monadic_sig == (0, 0) => {
            temp(false);
            afters.push(Instr::push(2));
            afters.push(Instr::Prim(Primitive::Div, *span));
        }
        [Instr::Prim(Primitive::Add, span)] => {
            temp(true);
            afters.push(Instr::Prim(Primitive::Sub, *span));
        }
        [Instr::Prim(Primitive::Sub, span)] => {
            temp(true);
            afters.push(Instr::Prim(Primitive::Add, *span));
        }
        [Instr::Prim(Primitive::Mul, span)] if monadic_sig == (0, 0) => {
            temp(false);
            afters.push(Instr::Prim(Primitive::Sqrt, *span));
        }
        [Instr::Prim(Primitive::Mul, span)] => {
            temp(true);
            afters.push(Instr::Prim(Primitive::Div, *span));
        }
        [Instr::Prim(Primitive::Div, span)] => {
            temp(true);
            afters.push(Instr::Prim(Primitive::Mul, *span));
        }
        _ => return None,
    }
    afters.extend(monadic_afters);
    Some((input, (befores, afters)))
}

fn invert_stack_swizzle_pattern<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::StackSwizzle(swizzle, span), input @ ..] = input else {
        return None;
    };
    let instrs = eco_vec![Instr::StackSwizzle(swizzle.inverse()?, *span)];
    Some((input, instrs))
}

fn invert_select_pattern<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    Some(match input {
        [Instr::Push(val), sel @ Instr::Prim(Primitive::Select, _), input @ ..] => {
            let indices = val.as_nats(&IgnoreError, "").ok()?;
            let unique_indices: HashSet<usize> = indices.iter().copied().collect();
            if unique_indices.len() != indices.len() {
                return None;
            }
            let mut inverse_indices = vec![0; indices.len()];
            for (i, &j) in indices.iter().enumerate() {
                inverse_indices[j] = i;
            }
            let instrs = eco_vec![
                Instr::Push(inverse_indices.into_iter().collect()),
                sel.clone()
            ];
            (input, instrs)
        }
        [Instr::Prim(Primitive::Select, span), input @ ..] => {
            let instrs = eco_vec![
                Instr::CopyToTemp {
                    stack: TempStack::Inline,
                    count: 1,
                    span: *span,
                },
                Instr::Prim(Primitive::Deduplicate, *span),
                Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: 1,
                    span: *span,
                },
                Instr::Prim(Primitive::Classify, *span),
            ];
            (input, instrs)
        }
        _ => return None,
    })
}

fn invert_push_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let (instr, input) = match input {
        [instr @ Instr::Push(_), input @ ..] => (instr, input),
        [instr @ Instr::CallGlobal { index, .. }, input @ ..]
            if matches!(comp.asm.bindings[*index].kind, BindingKind::Const(_)) =>
        {
            (instr, input)
        }
        _ => return None,
    };
    if let [Instr::ImplPrim(ImplPrimitive::MatchPattern, _), input @ ..] = input {
        return Some((input, eco_vec![instr.clone()]));
    }
    let mut instrs = eco_vec![instr.clone()];
    instrs.push(Instr::ImplPrim(
        ImplPrimitive::MatchPattern,
        comp.asm.spans.len() - 1,
    ));
    Some((input, instrs))
}

fn invert_format_pattern<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    Some(match input {
        [Instr::Format { parts, span }, input @ ..] => (
            input,
            eco_vec![Instr::MatchFormatPattern {
                parts: parts.clone(),
                span: *span
            }],
        ),
        [Instr::MatchFormatPattern { parts, span }, input @ ..] => (
            input,
            eco_vec![Instr::Format {
                parts: parts.clone(),
                span: *span
            }],
        ),
        _ => return None,
    })
}

fn invert_join_val_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    for i in 0..input.len() {
        if let &Instr::Prim(Primitive::Join, span) = &input[i] {
            let Some((input, before)) = Val.invert_extract(&input[..i], comp) else {
                continue;
            };
            let mut instrs = before;
            instrs.extend([
                Instr::CopyToTemp {
                    stack: TempStack::Inline,
                    count: 1,
                    span,
                },
                Instr::Prim(Primitive::Shape, span),
                Instr::ImplPrim(ImplPrimitive::UnJoinPattern, span),
                Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: 1,
                    span,
                },
                Instr::ImplPrim(ImplPrimitive::MatchPattern, span),
            ]);
            return Some((input, instrs));
        }
    }
    None
}

fn invert_insert_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let (input, first) = Val.invert_extract(input, comp)?;
    let second = Val.invert_extract(input, comp);
    let &[Instr::Prim(Primitive::Insert, span), ref input @ ..] = input else {
        return None;
    };
    let (input, key, value) = if let Some((input, key)) = second {
        (input, key, Some(first))
    } else {
        (input, first, None)
    };
    let mut instrs = key;
    instrs.extend([
        Instr::Prim(Primitive::Over, span),
        Instr::Prim(Primitive::Over, span),
        Instr::Prim(Primitive::Has, span),
        Instr::push(1),
        Instr::ImplPrim(ImplPrimitive::MatchPattern, span),
        Instr::Prim(Primitive::Over, span),
        Instr::Prim(Primitive::Over, span),
        Instr::Prim(Primitive::Get, span),
        Instr::PushTemp {
            stack: TempStack::Inline,
            count: 1,
            span,
        },
        Instr::Prim(Primitive::Remove, span),
        Instr::PopTemp {
            stack: TempStack::Inline,
            count: 1,
            span,
        },
    ]);
    if let Some(value) = value {
        instrs.extend(value);
        instrs.push(Instr::ImplPrim(ImplPrimitive::MatchPattern, span));
    }
    Some((input, instrs))
}

fn invert_setinv_pattern<'a>(
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

fn invert_setund_setinv_pattern<'a>(
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
            eco_vec![f.clone(), Instr::ImplPrim(ImplPrimitive::UnDump, *span)],
        )),
        [f @ Instr::PushFunc(_), Instr::ImplPrim(ImplPrimitive::UnDump, span), input @ ..] => {
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
                eco_vec![f.clone(), Instr::ImplPrim(ImplPrimitive::UnDump, *span)],
            ),
        )),
        [f @ Instr::PushFunc(_), Instr::ImplPrim(ImplPrimitive::UnDump, span), input @ ..] => {
            Some((
                input,
                (
                    eco_vec![f.clone(), Instr::ImplPrim(ImplPrimitive::UnDump, *span)],
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
    let mut end = input.len();
    loop {
        for pattern in INVERT_PATTERNS {
            if let Some((inp, after)) = pattern.invert_extract(&input[..end], comp) {
                let before = EcoVec::from(&input[..input.len() - inp.len()]);
                if DEBUG {
                    println!(
                        "inverted for under ({:?}) {:?} to {:?}",
                        pattern, before, after
                    );
                }
                return Some((inp, (before, after)));
            }
        }
        end -= 1;
        if end == 0 {
            return None;
        }
    }
}

fn under_setinv_pattern<'a>(
    input: &'a [Instr],
    _: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::PushFunc(inv), Instr::PushFunc(normal), Instr::Prim(Primitive::SetInverse, _), input @ ..] =
        input
    else {
        return None;
    };
    Some((input, (normal.instrs(comp).into(), inv.instrs(comp).into())))
}

fn under_setund_pattern<'a>(
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

fn under_setinv_setund_pattern<'a>(
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

type TempWrap<'a> = (&'a [Instr], &'a Instr, &'a [Instr], &'a Instr, usize);

fn try_push_temp_wrap(input: &[Instr]) -> Option<TempWrap> {
    temp_wrap_impl(input, |instr| match instr {
        Instr::PushTemp {
            stack: TempStack::Inline,
            count,
            ..
        } => Some(*count),
        _ => None,
    })
}

fn try_copy_temp_wrap(input: &[Instr]) -> Option<TempWrap> {
    temp_wrap_impl(input, |instr| match instr {
        Instr::CopyToTemp {
            stack: TempStack::Inline,
            count,
            ..
        } => Some(*count),
        _ => None,
    })
}

fn temp_wrap_impl(input: &[Instr], f: impl Fn(&Instr) -> Option<usize>) -> Option<TempWrap> {
    let (instr, input) = input.split_first()?;
    let count = f(instr)?;
    // Find end
    let mut depth = count;
    let mut max_depth = depth;
    let mut end = 0;
    for (i, instr) in input.iter().enumerate() {
        match instr {
            Instr::PushTemp {
                count,
                stack: TempStack::Inline,
                ..
            }
            | Instr::CopyToTemp {
                count,
                stack: TempStack::Inline,
                ..
            } => {
                depth += *count;
                max_depth = max_depth.max(depth);
            }
            Instr::PopTemp {
                count,
                stack: TempStack::Inline,
                ..
            } => {
                depth = depth.saturating_sub(*count);
                if depth == 0 {
                    end = i;
                    break;
                }
            }
            _ => {}
        }
    }
    let (inner, input) = input.split_at(end);
    let end_instr = input.first()?;
    let input = &input[1..];
    Some((input, instr, inner, end_instr, max_depth))
}

fn invert_temp_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    // Push temp
    if let Some((input, instr, inner, end_instr, _)) = try_push_temp_wrap(input) {
        let mut instrs = invert_instrs(inner, comp)?;
        instrs.insert(0, instr.clone());
        instrs.push(end_instr.clone());
        return Some((input, instrs));
    }
    // Copy temp
    if let Some((input, start_instr @ Instr::CopyToTemp { span, .. }, inner, end_instr, count)) =
        try_copy_temp_wrap(input)
    {
        // Pseudo-inverse
        for mid in 0..inner.len() {
            let (before, after) = inner.split_at(mid);
            let Ok(before_sig) = instrs_signature(before) else {
                continue;
            };
            if before_sig.args == 0 && before_sig.outputs != 0 {
                continue;
            }
            for pat in PSEUDO_INVERT_PATTERNS {
                if let Some((after, pseudo_inv)) = pat.invert_extract(after, comp) {
                    if let Some(after_inv) = invert_instrs(after, comp) {
                        let mut instrs = eco_vec![start_instr.clone()];

                        if !after_inv.is_empty() {
                            instrs.push(Instr::PushTemp {
                                stack: TempStack::Inline,
                                count,
                                span: *span,
                            });
                            instrs.extend(after_inv);
                            instrs.push(end_instr.clone());
                        }

                        instrs.extend_from_slice(before);

                        instrs.extend(pseudo_inv);

                        instrs.push(end_instr.clone());
                        return Some((input, instrs));
                    }
                }
            }
        }
        // Pattern matching
        if let Some(inverse) = invert_instrs(inner, comp) {
            let mut instrs = eco_vec![Instr::PushTemp {
                stack: TempStack::Inline,
                count,
                span: *span
            }];
            instrs.extend(inverse);
            instrs.push(end_instr.clone());
            instrs.extend([
                Instr::Prim(Primitive::Over, *span),
                Instr::ImplPrim(ImplPrimitive::MatchPattern, *span),
            ]);
            return Some((input, instrs));
        }
    }
    None
}

fn under_flip_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::Prim(Primitive::Flip, span), input @ ..] = input else {
        return None;
    };
    let befores = eco_vec![Instr::Prim(Primitive::Flip, *span)];
    let (rest_befores, rest_afters) = under_instrs(input, g_sig, &mut comp.clone())?;
    let rest_befores_sig = instrs_signature(&rest_befores).ok()?;
    let rest_afters_sig = instrs_signature(&rest_afters).ok()?;
    let total_args = g_sig.args + rest_befores_sig.args + rest_afters_sig.args;
    let total_outputs = g_sig.outputs + rest_befores_sig.outputs + rest_afters_sig.outputs;
    let afters = if total_outputs < total_args {
        EcoVec::new()
    } else {
        eco_vec![Instr::Prim(Primitive::Flip, *span)]
    };
    Some((input, (befores, afters)))
}

fn under_copy_temp_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let (input, instr, inner, end_instr, _) = try_copy_temp_wrap(input)?;
    let (mut inner_befores, mut inner_afters) = under_instrs(inner, g_sig, comp)?;
    let Some((
        Instr::CopyToTemp {
            stack: TempStack::Under,
            count: before_count,
            ..
        },
        Instr::PopTemp {
            stack: TempStack::Under,
            count: after_count,
            ..
        },
    )) = inner_befores.first().zip(inner_afters.first())
    else {
        return None;
    };
    if g_sig.args > g_sig.outputs {
        inner_befores.insert(0, instr.clone());
        inner_befores.push(end_instr.clone());
    } else {
        let mut instr_copy = instr.clone();
        let mut end_instr_copy = end_instr.clone();
        let (start_count, end_count) = temp_pair_counts(&mut instr_copy, &mut end_instr_copy)?;
        *start_count = *before_count;
        *end_count = *after_count;
        inner_befores.make_mut()[0] = instr_copy;
        inner_afters.make_mut()[0] = instr.clone();
        inner_befores.push(end_instr_copy);
        inner_afters.push(end_instr.clone());
    }
    Some((input, (inner_befores, inner_afters)))
}

fn under_push_temp_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let (input, start_instr, inner, end_instr, _) = try_push_temp_wrap(input)?;
    // Calcular inner functions and signatures
    let (inner_befores, inner_afters) = under_instrs(inner, g_sig, comp)?;
    let inner_befores_sig = instrs_signature(&inner_befores).ok()?;
    let inner_afters_sig = instrs_signature(&inner_afters).ok()?;

    if g_sig.args < g_sig.outputs {
        return None;
    }

    let temp_depth = if let Instr::PopTemp {
        count,
        stack: TempStack::Inline,
        ..
    } = end_instr
    {
        *count
    } else {
        return None;
    };

    let input_iter = input.iter().filter(|instr| !instr.is_compile_only());
    let inner_iter = inner.iter().filter(|instr| !instr.is_compile_only());
    let both = inner_befores_sig.args == temp_depth
        && input_iter.clone().count() >= inner_iter.clone().count()
        && input_iter.zip(inner_iter).all(|(a, b)| a == b);

    let mut befores = inner_befores;
    befores.insert(0, start_instr.clone());
    befores.push(end_instr.clone());

    let afters = if both && g_sig.args > g_sig.outputs {
        let mut before_temp_count = 0;
        for instr in &befores {
            match instr {
                Instr::PushTemp {
                    count,
                    stack: TempStack::Under,
                    ..
                }
                | Instr::CopyToTemp {
                    count,
                    stack: TempStack::Under,
                    ..
                } => {
                    before_temp_count += *count;
                }
                Instr::PopTemp {
                    count,
                    stack: TempStack::Under,
                    ..
                } => {
                    before_temp_count = before_temp_count.saturating_sub(*count);
                }
                _ => {}
            }
        }
        if before_temp_count > 0 {
            let mut instrs = eco_vec![Instr::PopTemp {
                count: before_temp_count,
                stack: TempStack::Under,
                span: 0,
            }];
            for _ in 0..before_temp_count {
                instrs.push(Instr::Prim(Primitive::Pop, before_temp_count))
            }
            instrs
        } else {
            EcoVec::new()
        }
    } else {
        let mut afters = inner_afters;
        let mut start_instr = start_instr.clone();
        let mut end_instr = end_instr.clone();
        match g_sig.args.cmp(&g_sig.outputs) {
            Ordering::Equal if both || inner_befores_sig.args <= inner_afters_sig.args => {
                if inner_befores_sig.args != inner_afters_sig.outputs {
                    let (start_count, end_count) =
                        temp_pair_counts(&mut start_instr, &mut end_instr)?;
                    *start_count -= inner_afters_sig.outputs;
                    *end_count = (*end_count).min(inner_befores_sig.outputs);
                }
                let outers_sig = instrs_signature(input).ok()?;
                if (both || outers_sig.args <= outers_sig.outputs) && inner_afters_sig.outputs > 0 {
                    afters.insert(0, start_instr);
                    afters.push(end_instr);
                }
            }
            Ordering::Greater => {
                let (start_count, end_count) = temp_pair_counts(&mut start_instr, &mut end_instr)?;
                let diff = g_sig.args - g_sig.outputs;
                *start_count = start_count.saturating_sub(diff);
                *end_count = end_count.saturating_sub(diff);
                if *start_count > 0 || *end_count > 0 {
                    afters.insert(0, start_instr);
                    afters.push(end_instr);
                }
            }
            Ordering::Equal if inner_afters_sig.args + 1 == inner_afters_sig.outputs => {
                afters.insert(0, start_instr);
                afters.push(end_instr);
            }
            _ => {}
        }
        afters
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
        (
            Instr::CopyToTemp {
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
    Some(comp.make_function(id, sig, instrs))
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

fn invert_rows_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), instr @ Instr::Prim(Primitive::Rows, span), input @ ..] = input else {
        return None;
    };
    let instrs = f.instrs(comp).to_vec();
    let inverse = invert_instrs(&instrs, comp)?;
    let f = make_fn(inverse, *span, comp)?;
    Some((input, eco_vec![Instr::PushFunc(f), instr.clone()]))
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
    let after_fn = make_fn(f_after, span, comp)?;
    let after_sig = after_fn.signature();
    let mut afters = eco_vec![
        Instr::PushFunc(after_fn),
        Instr::Prim(Primitive::Reverse, span),
        Instr::Prim(Primitive::Rows, span),
    ];
    if after_sig.outputs > 0 {
        afters.push(Instr::Prim(Primitive::Reverse, span));
    }
    for count in 1..after_sig.outputs {
        afters.extend([
            Instr::PushTemp {
                stack: TempStack::Inline,
                count,
                span,
            },
            Instr::Prim(Primitive::Reverse, span),
        ]);
    }
    if after_sig.outputs > 0 {
        afters.push(Instr::PopTemp {
            stack: TempStack::Inline,
            count: after_sig.outputs - 1,
            span,
        });
    }
    Some((input, (befores, afters)))
}

fn under_fill_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::PushFunc(g), Instr::PushFunc(f), Instr::Prim(Primitive::Fill, span), input @ ..] =
        input
    else {
        return None;
    };
    let span = *span;
    if f.signature() != (0, 1) {
        return None;
    }
    let g_instrs = g.instrs(comp).to_vec();
    let (g_before, g_after) = under_instrs(&g_instrs, g_sig, comp)?;
    let g_before = make_fn(g_before, span, comp)?;
    let g_after = make_fn(g_after, span, comp)?;
    let befores = eco_vec![
        Instr::PushFunc(g_before),
        Instr::PushFunc(f.clone()),
        Instr::Prim(Primitive::Fill, span),
    ];
    let afters = eco_vec![
        Instr::PushFunc(g_after),
        Instr::PushFunc(f.clone()),
        Instr::Prim(Primitive::Fill, span),
    ];
    Some((input, (befores, afters)))
}

fn under_switch_pattern<'a>(
    mut input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let mut funcs = Vec::new();
    while let (Instr::PushFunc(f), rest) = input.split_first()? {
        funcs.push(f);
        input = rest;
    }
    if funcs.is_empty() {
        return None;
    }
    let [Instr::Switch {
        count,
        sig,
        span,
        under_cond: false,
    }, input @ ..] = input
    else {
        return None;
    };
    if funcs.len() != *count {
        return None;
    }
    let mut f_befores = Vec::with_capacity(funcs.len());
    let mut f_afters = Vec::with_capacity(funcs.len());
    let mut undo_sig: Option<Signature> = None;
    for f in &funcs {
        // Calc under f
        let f_instrs = f.instrs(comp).to_vec();
        let (before, after) = under_instrs(&f_instrs, g_sig, comp)?;
        f_befores.push(make_fn(before, *span, comp)?);
        let f_after = make_fn(after, *span, comp)?;
        let f_after_sig = f_after.signature();
        f_afters.push(f_after);
        // Aggregate sigs
        let undo_sig = undo_sig.get_or_insert(f_after_sig);
        if f_after_sig.is_compatible_with(*undo_sig) {
            *undo_sig = undo_sig.max_with(f_after_sig);
        } else if f_after_sig.outputs == undo_sig.outputs {
            undo_sig.args = undo_sig.args.max(f_after_sig.args)
        } else {
            return None;
        }
    }

    let mut befores = EcoVec::with_capacity(funcs.len() + 2);
    befores.extend(f_befores.into_iter().map(Instr::PushFunc));
    befores.push(Instr::Switch {
        count: *count,
        sig: *sig,
        span: *span,
        under_cond: true,
    });

    let mut afters = EcoVec::with_capacity(funcs.len() + 2);
    afters.extend(f_afters.into_iter().map(Instr::PushFunc));
    afters.extend([
        Instr::PopTemp {
            stack: TempStack::Under,
            count: 1,
            span: *span,
        },
        Instr::Switch {
            count: *count,
            sig: undo_sig?,
            span: *span,
            under_cond: false,
        },
    ]);
    Some((input, (befores, afters)))
}

macro_rules! partition_group {
    ($name:ident, $prim:ident, $impl_prim1:ident, $impl_prim2:ident) => {
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
                Instr::PushFunc(make_fn(f_after, span, comp)?),
                Instr::ImplPrim(ImplPrimitive::$impl_prim1, span),
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
                Instr::ImplPrim(ImplPrimitive::$impl_prim2, span),
            ];
            Some((input, (befores, afters)))
        }
    };
}

partition_group!(
    under_partition_pattern,
    Partition,
    UndoPartition1,
    UndpPartition2
);
partition_group!(under_group_pattern, Group, UndoGroup1, UndoGroup2);

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
    let count = instrs_signature(inner).ok()?.outputs;
    let mut instrs = invert_instrs(inner, comp)?;
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
    let [Instr::Unpack { count, span, unbox }, input @ ..] = input else {
        return None;
    };
    let mut instrs = invert_instrs(input, comp)?;
    instrs.insert(0, Instr::BeginArray);
    instrs.push(Instr::TouchStack {
        count: *count,
        span: *span,
    });
    instrs.push(Instr::EndArray {
        span: *span,
        boxed: *unbox,
    });
    Some((&[], instrs))
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
    Some((&[], (befores, afters)))
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

fn invert_split_pattern<'a>(
    input: &'a [Instr],
    _: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::CopyToTemp {
        stack: TempStack::Inline,
        count: 1,
        ..
    }, input @ ..] = input
    else {
        return None;
    };
    let pop_pos = input.iter().position(|instr| {
        matches!(
            instr,
            Instr::PopTemp {
                stack: TempStack::Inline,
                count: 1,
                ..
            }
        )
    })?;
    let (a, b) = input.split_at(pop_pos);
    let b = &b[1..];
    let (input, instrs) = match (a, b) {
        (
            [Instr::Prim(Primitive::Deduplicate, _)],
            [Instr::Prim(Primitive::Classify, span), input @ ..],
        ) => (input, eco_vec![Instr::Prim(Primitive::Select, *span)]),
        _ => return None,
    };
    Some((input, instrs))
}

fn invert_scan_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), instr, input @ ..] = input else {
        return None;
    };
    let (Instr::Prim(Primitive::Scan, span) | Instr::ImplPrim(ImplPrimitive::UnScan, span)) = instr
    else {
        return None;
    };
    let un = matches!(instr, Instr::ImplPrim(ImplPrimitive::UnScan, _));
    let inverse = match f.as_flipped_primitive(&comp.asm) {
        Some((Primitive::Add, false)) if !un => eco_vec![Instr::Prim(Primitive::Sub, *span)],
        Some((Primitive::Mul, false)) if !un => eco_vec![Instr::Prim(Primitive::Div, *span)],
        Some((Primitive::Sub, false)) if un => eco_vec![Instr::Prim(Primitive::Add, *span)],
        Some((Primitive::Div, false)) if un => eco_vec![Instr::Prim(Primitive::Mul, *span)],
        Some((Primitive::Eq, false)) => eco_vec![Instr::Prim(Primitive::Eq, *span)],
        Some((Primitive::Ne, false)) => eco_vec![Instr::Prim(Primitive::Ne, *span)],
        _ => {
            let instrs = f.instrs(comp).to_vec();
            invert_instrs(&instrs, comp)?
        }
    };
    let inverse = make_fn(inverse, *span, comp)?;
    Some((
        input,
        eco_vec![
            Instr::PushFunc(inverse),
            Instr::ImplPrim(ImplPrimitive::UnScan, *span)
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

fn under_fold_pattern<'a>(
    input: &'a [Instr],
    g_sig: Signature,
    comp: &mut Compiler,
) -> Option<(&'a [Instr], Under)> {
    let [Instr::PushFunc(f), fold @ Instr::Prim(Primitive::Fold, span), input @ ..] = input else {
        return None;
    };
    let span = *span;
    let inner = f.instrs(comp).to_vec();
    let (inner_befores, inner_afters) = under_instrs(&inner, g_sig, comp)?;
    let inner_befores_sig = instrs_signature(&inner_befores).ok()?;
    let inner_afters_sig = instrs_signature(&inner_afters).ok()?;
    if inner_befores_sig.outputs > inner_befores_sig.args
        || inner_afters_sig.outputs > inner_afters_sig.args
    {
        return None;
    }
    let befores_func = make_fn(inner_befores, span, comp)?;
    let afters_func = make_fn(inner_afters, span, comp)?;
    let befores = eco_vec![
        Instr::Prim(Primitive::Dup, span),
        Instr::Prim(Primitive::Len, span),
        Instr::PushTemp {
            stack: TempStack::Inline,
            span,
            count: 1,
        },
        Instr::PushFunc(befores_func),
        fold.clone()
    ];
    let afters = eco_vec![
        Instr::PopTemp {
            stack: TempStack::Inline,
            count: 1,
            span
        },
        Instr::PushFunc(afters_func),
        Instr::Prim(Primitive::Repeat, span)
    ];
    Some((input, (befores, afters)))
}

fn invert_reduce_mul_pattern<'a>(
    input: &'a [Instr],
    comp: &mut Compiler,
) -> Option<(&'a [Instr], EcoVec<Instr>)> {
    let [Instr::PushFunc(f), Instr::Prim(Primitive::Reduce, span), input @ ..] = input else {
        return None;
    };
    let Some((Primitive::Mul, _)) = f.as_flipped_primitive(&comp.asm) else {
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

#[derive(Debug)]
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
    T: AsInstr + Sync,
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

impl<T> InvertPattern for (&[ImplPrimitive], &[T])
where
    T: AsInstr + Sync,
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
                Instr::ImplPrim(instr_prim, span) if instr_prim == prim => spans.push(*span),
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
    T: AsInstr + Sync,
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

impl<T, const A: usize, const B: usize> InvertPattern for ([ImplPrimitive; A], [T; B])
where
    T: AsInstr + Sync,
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

struct InvertPatternFn<F>(F, &'static str);
impl<F> fmt::Debug for InvertPatternFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InvertPatternFn({})", self.1)
    }
}
impl<F> InvertPattern for InvertPatternFn<F>
where
    F: for<'a> Fn(&'a [Instr], &mut Compiler) -> Option<(&'a [Instr], EcoVec<Instr>)> + Sync,
{
    fn invert_extract<'a>(
        &self,
        input: &'a [Instr],
        comp: &mut Compiler,
    ) -> Option<(&'a [Instr], EcoVec<Instr>)> {
        (self.0)(input, comp)
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
            if (chunk.iter()).any(|instr| {
                matches!(
                    instr,
                    Instr::PushFunc(_) | Instr::PushTemp { .. } | Instr::PopTemp { .. }
                )
            }) {
                continue;
            }
            if let Ok(sig) = instrs_signature(chunk) {
                if sig == (0, 1) {
                    return Some((&input[len..], chunk.into()));
                }
            }
        }
        match input.first() {
            Some(instr @ Instr::Push(_)) => Some((&input[1..], eco_vec![instr.clone()])),
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

trait AsInstr: fmt::Debug + Sync {
    fn as_instr(&self, span: usize) -> Instr;
}

#[derive(Debug, Clone, Copy)]
struct PushToUnder(usize);
impl AsInstr for PushToUnder {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PushTemp {
            stack: TempStack::Under,
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CopyToUnder(usize);
impl AsInstr for CopyToUnder {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::CopyToTemp {
            stack: TempStack::Under,
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PushToInline(usize);
impl AsInstr for PushToInline {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PushTemp {
            stack: TempStack::Inline,
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CopyToInline(usize);
impl AsInstr for CopyToInline {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::CopyToTemp {
            stack: TempStack::Inline,
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PopUnder(usize);
impl AsInstr for PopUnder {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PopTemp {
            stack: TempStack::Under,
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PopInline(usize);
impl AsInstr for PopInline {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PopTemp {
            stack: TempStack::Inline,
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

impl AsInstr for [usize; 0] {
    fn as_instr(&self, _: usize) -> Instr {
        Instr::push(Value::default())
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

impl<'a> AsInstr for &'a dyn AsInstr {
    fn as_instr(&self, span: usize) -> Instr {
        (*self).as_instr(span)
    }
}
