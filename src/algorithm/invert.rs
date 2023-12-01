//! Algorithms for invert and under

use std::{cell::RefCell, collections::HashMap, fmt};

use crate::{
    check::instrs_signature,
    function::{Function, Instr, Signature},
    primitive::{ImplPrimitive, Primitive},
    value::Value,
    SysOp, TempStack,
};

impl Function {
    /// Get the function's inverse
    pub fn inverse(&self) -> Option<Self> {
        Function::new_inferred(self.id.clone(), invert_instrs(&self.instrs)?).ok()
    }
    /// Get the function's before and after functions for `under`
    ///
    /// `g_sig` should be the signature of `under`'s second function
    pub fn under(&self, g_sig: Signature) -> Option<(Self, Self)> {
        if let Some(f) = self.inverse() {
            Some((self.clone(), f))
        } else {
            let (befores, afters) = under_instrs(&self.instrs, g_sig)?;
            let before = Function::new_inferred(self.id.clone(), befores).ok()?;
            let after = Function::new_inferred(self.id.clone(), afters).ok()?;
            Some((before, after))
        }
    }
}

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
        Transpose => Instr::ImplPrim(InvTranspose, span),
        Bits => Instr::ImplPrim(InverseBits, span),
        Couple => Instr::ImplPrim(InvCouple, span),
        Trace => Instr::ImplPrim(InvTrace, span),
        Box => Instr::Prim(Unbox, span),
        Unbox => Instr::Prim(Box, span),
        Where => Instr::ImplPrim(InvWhere, span),
        Utf => Instr::ImplPrim(InvUtf, span),
        Parse => Instr::ImplPrim(InvParse, span),
        Fix => Instr::ImplPrim(InvFix, span),
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
        InvTranspose => Instr::Prim(Transpose, span),
        InverseBits => Instr::Prim(Bits, span),
        InvTrace => Instr::Prim(Trace, span),
        InvWhere => Instr::Prim(Where, span),
        InvUtf => Instr::Prim(Utf, span),
        InvAtan => Instr::Prim(Atan, span),
        InvComplex => Instr::Prim(Complex, span),
        InvCouple => Instr::Prim(Couple, span),
        InvParse => Instr::Prim(Parse, span),
        InvFix => Instr::Prim(Fix, span),
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

pub(crate) fn invert_instrs(instrs: &[Instr]) -> Option<Vec<Instr>> {
    if instrs.is_empty() {
        return Some(Vec::new());
    }

    thread_local! {
        static INVERT_CACHE: RefCell<HashMap<Vec<Instr>, Option<Vec<Instr>>>> = RefCell::new(HashMap::new());
    }
    if let Some(inverted) = INVERT_CACHE.with(|cache| cache.borrow().get(instrs).cloned()) {
        return inverted;
    }
    let inverted = invert_instr_impl(instrs);
    INVERT_CACHE.with(|cache| cache.borrow_mut().insert(instrs.to_vec(), inverted.clone()));
    inverted
}

fn invert_instr_impl(mut instrs: &[Instr]) -> Option<Vec<Instr>> {
    use Primitive::*;

    // println!("inverting {:?}", instrs);

    let patterns: &[&dyn InvertPattern] = &[
        &invert_invert_pattern,
        &invert_rectify_pattern,
        &invert_setinverse_pattern,
        &invert_trivial_pattern,
        &invert_array_pattern,
        &invert_unpack_pattern,
        &(Val, ([Rotate], [Neg, Rotate])),
        &([Rotate], [Neg, Rotate]),
        &pat!(Sqrt, (2, Pow)),
        &(Val, IgnoreMany(Flip), ([Add], [Sub])),
        &(Val, ([Sub], [Add])),
        &(Val, ([Flip, Sub], [Flip, Sub])),
        &(Val, IgnoreMany(Flip), ([Mul], [Div])),
        &(Val, ([Div], [Mul])),
        &(Val, ([Flip, Div], [Flip, Div])),
        &pat!((Dup, Add), (2, Div)),
        &([Dup, Mul], [Sqrt]),
        &(Val, pat!(Pow, (1, Flip, Div, Pow))),
        &(Val, ([Log], [Flip, Pow])),
        &invert_temp_pattern,
    ];

    let mut inverted = Vec::new();
    'find_pattern: loop {
        for pattern in patterns {
            if let Some((input, mut inv)) = pattern.invert_extract(instrs) {
                inv.extend(inverted);
                inverted = inv;
                if input.is_empty() {
                    return Some(inverted);
                }
                instrs = input;
                continue 'find_pattern;
            }
        }
        break;
    }

    None
}

type Under = (Vec<Instr>, Vec<Instr>);

pub(crate) fn under_instrs(instrs: &[Instr], g_sig: Signature) -> Option<Under> {
    if instrs.is_empty() {
        return Some((Vec::new(), Vec::new()));
    }

    type UnderCache = HashMap<Vec<Instr>, HashMap<Signature, Option<Under>>>;
    thread_local! {
        static UNDER_CACHE: RefCell<UnderCache> = RefCell::new(HashMap::new());
    }
    if let Some(under) = UNDER_CACHE.with(|cache| {
        cache
            .borrow()
            .get(instrs)
            .and_then(|map| map.get(&g_sig))
            .cloned()
    }) {
        return under;
    }
    let under = under_instrs_impl(instrs, g_sig);
    UNDER_CACHE.with(|cache| {
        cache
            .borrow_mut()
            .entry(instrs.to_vec())
            .or_default()
            .insert(g_sig, under.clone())
    });
    under
}

fn under_instrs_impl(instrs: &[Instr], g_sig: Signature) -> Option<(Vec<Instr>, Vec<Instr>)> {
    use ImplPrimitive::*;
    use Primitive::*;

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

    let patterns: &[&dyn UnderPattern] = &[
        &UnderPatternFn(under_rows_pattern, "rows"),
        &UnderPatternFn(under_each_pattern, "each"),
        &UnderPatternFn(under_partition_pattern, "partition"),
        &UnderPatternFn(under_group_pattern, "group"),
        &UnderPatternFn(under_setunder_pattern, "setunder"),
        &UnderPatternFn(under_array_pattern, "array"),
        &UnderPatternFn(under_unpack_pattern, "unpack"),
        &bin!(Flip, Add, Sub),
        &bin!(Flip, Mul, Div),
        &bin!(Flip, Sub),
        &bin!(Flip, Div),
        &bin!(Add, Sub),
        &bin!(Sub, Add),
        &bin!(Mul, Div),
        &bin!(Div, Mul),
        &pat!((Flip, Pow), (CopyToTempN(1), Flip, Pow), (PopTempN(1), Log)),
        &pat!(Pow, (CopyToTempN(1), Pow), (PopTempN(1), 1, Flip, Div, Pow)),
        &pat!(
            (Flip, Log),
            (CopyToTempN(1), Flip, Log),
            (1, Flip, Div, PopTempN(1), Flip, Pow)
        ),
        &pat!(Log, (CopyToTempN(1), Log), (PopTempN(1), Flip, Pow)),
        &(Val, stash2!(Take, Untake)),
        &stash2!(Take, Untake),
        &(Val, stash2!(Drop, Undrop)),
        &stash2!(Drop, Undrop),
        &(Val, stash2!(Select, Unselect)),
        &stash2!(Select, Unselect),
        &(Val, stash2!(Pick, Unpick)),
        &stash2!(Pick, Unpick),
        &(
            Val,
            pat!(
                Keep,
                (CopyToTempN(2), Keep),
                (PopTempN(1), Flip, PopTempN(1), Unkeep)
            ),
        ),
        &pat!(
            Keep,
            (CopyToTempN(2), Keep),
            (PopTempN(1), Flip, PopTempN(1), Unkeep),
        ),
        &pat!(Rotate, (CopyToTempN(1), Rotate), (PopTempN(1), Neg, Rotate),),
        &pat!(Abs, (CopyToTempN(1), Abs), (PopTempN(1), Sign, Mul),),
        &pat!(
            First,
            (CopyToTempN(1), First),
            (PopTempN(1), 1, Drop, Flip, Join),
        ),
        &pat!(Last, (CopyToTempN(1), Last), (PopTempN(1), -1, Drop, Join),),
        &pat!(Shape, (CopyToTempN(1), Shape), (PopTempN(1), Flip, Reshape),),
        &pat!(
            Deshape,
            (Dup, Shape, PushTempN(1), Deshape),
            (PopTempN(1), Reshape),
        ),
        &pat!(
            Rerank,
            (Over, Shape, Over, PushTempN(2), Rerank),
            (PopTempN(2), Unrerank),
        ),
        &(
            Val,
            pat!(
                Rerank,
                (Over, Shape, Over, PushTempN(2), Rerank),
                (PopTempN(2), Unrerank),
            ),
        ),
        &pat!((Now), (Now, PushTempN(1)), (PopTempN(1), Now, Flip, Sub),),
        &pat!(
            Sys(SysOp::FOpen),
            (Sys(SysOp::FOpen), Dup, PushTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &pat!(
            Sys(SysOp::FCreate),
            (Sys(SysOp::FCreate), Dup, PushTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &pat!(
            Sys(SysOp::TcpConnect),
            (Sys(SysOp::TcpConnect), Dup, PushTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &pat!(
            Sys(SysOp::TcpAccept),
            (Sys(SysOp::TcpAccept), Dup, PushTempN(1)),
            (PopTempN(1), Sys(SysOp::Close)),
        ),
        &pat!(Rock, Rock, (1, Drop)),
        &pat!(Surface, Surface, (1, Drop)),
        &pat!(Deep, Deep, (1, Drop)),
        &pat!(Abyss, Abyss, (1, Drop)),
        &pat!(Seabed, Seabed, (1, Drop)),
        &UnderPatternFn(under_temp_pattern, "temp"),
        &UnderPatternFn(under_from_inverse_pattern, "from inverse"), // This must come last!
    ];

    // println!("undering {:?}", instrs);

    let mut befores = Vec::new();
    let mut afters = Vec::new();
    let mut instrs_sections = instrs;
    'find_pattern: loop {
        for pattern in patterns {
            if let Some((input, (bef, aft))) = pattern.under_extract(instrs_sections, g_sig) {
                // println!(
                //     "matched pattern {:?} on {:?} to {bef:?} {aft:?}",
                //     pattern,
                //     &instrs_sections[..instrs_sections.len() - input.len()],
                // );
                befores.extend(bef);
                afters = aft.into_iter().chain(afters).collect();
                if input.is_empty() {
                    // println!("under {:?} to {:?} {:?}", instrs, befores, afters);
                    return Some((befores, afters));
                }
                instrs_sections = input;
                continue 'find_pattern;
            }
        }
        break;
    }
    if let Some(inverted) = invert_instrs(instrs) {
        return Some((instrs.to_vec(), inverted));
    }

    // println!(
    //     "under {:?} failed with remaining {:?}",
    //     instrs, instrs_sections
    // );

    None
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

#[derive(Debug)]
struct Call;
impl AsInstr for Call {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::Call(span)
    }
}

trait InvertPattern {
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)>;
}

trait UnderPattern: fmt::Debug {
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
    ) -> Option<(&'a [Instr], Under)>;
}

fn invert_trivial_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    use Instr::*;
    match input {
        [Prim(prim, span), input @ ..] => {
            if let Some(inv) = prim_inverse(*prim, *span) {
                return Some((input, vec![inv]));
            }
        }
        [ImplPrim(prim, span), input @ ..] => {
            if let Some(inv) = impl_prim_inverse(*prim, *span).map(|instr| vec![instr]) {
                return Some((input, inv));
            }
        }
        [PushFunc(val), input @ ..] => {
            if let Some((prim, span)) = val.as_primitive() {
                return Some((input, vec![prim_inverse(prim, span)?]));
            }
            if let Some((prim, span)) = val.as_impl_primitive() {
                return Some((input, vec![impl_prim_inverse(prim, span)?]));
            }
        }
        [PushSig(_) | PopSig, input @ ..] => return Some((input, Vec::new())),
        _ => {}
    }
    None
}

fn invert_invert_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let [Instr::PushFunc(func), Instr::Prim(Primitive::Invert, _), input @ ..] = input else {
        return None;
    };
    Some((input, func.instrs.clone()))
}

fn invert_rectify_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let [Instr::PushFunc(f), Instr::Prim(Primitive::Rectify, _), input @ ..] = input else {
        return None;
    };
    Some((input, f.instrs.clone()))
}

fn invert_setinverse_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let [Instr::PushFunc(inv), Instr::PushFunc(_), Instr::Prim(Primitive::SetInverse, _), input @ ..] =
        input
    else {
        return None;
    };
    Some((input, inv.instrs.clone()))
}

fn under_from_inverse_pattern(input: &[Instr], _: Signature) -> Option<(&[Instr], Under)> {
    if input.is_empty() {
        return None;
    }
    let mut end = 1;
    loop {
        if let Some(inverse) = invert_instrs(&input[..end]) {
            return Some((&input[end..], (input[..end].to_vec(), inverse)));
        }
        if end == input.len() {
            return None;
        }
        end += 1;
    }
}

fn under_setunder_pattern(input: &[Instr], _: Signature) -> Option<(&[Instr], Under)> {
    let [Instr::PushFunc(after), Instr::PushFunc(before), Instr::PushFunc(normal), Instr::Prim(Primitive::SetUnder, span), input @ ..] =
        input
    else {
        return None;
    };
    if before.signature().outputs < normal.signature().outputs {
        return None;
    }
    let to_save = before.signature().outputs - normal.signature().outputs;
    let mut befores = before.instrs.clone();
    let mut afters = after.instrs.clone();
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

fn try_temp_wrap(input: &[Instr]) -> Option<(&[Instr], &Instr, &[Instr], &Instr)> {
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
    let (inner, input) = input.split_at(end);
    let end_instr = input.first()?;
    let input = &input[1..];
    Some((input, instr, inner, end_instr))
}

fn invert_temp_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let (input, instr, inner, end_instr) = try_temp_wrap(input)?;
    let mut instrs = invert_instrs(inner)?;
    instrs.insert(0, instr.clone());
    instrs.push(end_instr.clone());
    Some((input, instrs))
}

fn under_temp_pattern(input: &[Instr], g_sig: Signature) -> Option<(&[Instr], Under)> {
    let (input, instr, inner, end_instr) = try_temp_wrap(input)?;
    // Calcular inner functions and signatures
    let (inner_befores, inner_afters) = under_instrs(inner, g_sig)?;
    let inner_befores_sig = instrs_signature(&inner_befores).ok()?;
    let inner_afters_sig = instrs_signature(&inner_afters).ok()?;
    let mut befores = inner_befores;
    befores.insert(0, instr.clone());
    befores.push(end_instr.clone());
    let afters = match (g_sig.args, g_sig.outputs) {
        (0, _) => return None,
        (_, 1) => {
            let both = input.len() >= inner.len() && inner.iter().zip(input).all(|(a, b)| a == b);
            if both {
                Vec::new()
            } else {
                let mut afters = inner_afters;
                if inner_befores_sig.args <= inner_afters_sig.args && g_sig.args <= g_sig.outputs {
                    afters.insert(0, instr.clone());
                    afters.push(end_instr.clone());
                }
                afters
            }
        }
        (2, 2) => {
            let mut afters = inner_afters;
            if inner_befores_sig.args <= inner_afters_sig.args && g_sig.args <= g_sig.outputs {
                afters.insert(0, instr.clone());
                afters.push(end_instr.clone());
            }
            afters
        }
        _ => return None,
    };
    Some((input, (befores, afters)))
}

fn under_each_pattern(input: &[Instr], g_sig: Signature) -> Option<(&[Instr], Under)> {
    let &[Instr::PushFunc(ref f), Instr::Prim(Primitive::Each, span), ref input @ ..] = input
    else {
        return None;
    };
    let (f_before, f_after) = f.under(g_sig)?;
    let befores = vec![
        Instr::PushFunc(f_before.into()),
        Instr::Prim(Primitive::Each, span),
    ];
    let afters = vec![
        Instr::PushFunc(f_after.into()),
        Instr::Prim(Primitive::Each, span),
    ];
    Some((input, (befores, afters)))
}

fn under_rows_pattern(input: &[Instr], g_sig: Signature) -> Option<(&[Instr], Under)> {
    let &[Instr::PushFunc(ref f), Instr::Prim(Primitive::Rows, span), ref input @ ..] = input
    else {
        return None;
    };
    let (f_before, f_after) = f.under(g_sig)?;
    let befores = vec![
        Instr::PushFunc(f_before.into()),
        Instr::Prim(Primitive::Rows, span),
    ];
    let afters = vec![
        Instr::PushFunc(f_after.into()),
        Instr::Prim(Primitive::Reverse, span),
        Instr::Prim(Primitive::Rows, span),
        Instr::Prim(Primitive::Reverse, span),
    ];
    Some((input, (befores, afters)))
}

fn under_partition_pattern(input: &[Instr], g_sig: Signature) -> Option<(&[Instr], Under)> {
    let &[Instr::PushFunc(ref f), Instr::Prim(Primitive::Partition, span), ref input @ ..] = input
    else {
        return None;
    };
    let (f_before, f_after) = f.under(g_sig)?;
    let befores = vec![
        Instr::CopyToTemp {
            stack: TempStack::Under,
            count: 2,
            span,
        },
        Instr::PushFunc(f_before.into()),
        Instr::Prim(Primitive::Partition, span),
    ];
    let afters = vec![
        Instr::PushFunc(f_after.into()),
        Instr::ImplPrim(ImplPrimitive::Unpartition, span),
    ];
    Some((input, (befores, afters)))
}

fn under_group_pattern(input: &[Instr], g_sig: Signature) -> Option<(&[Instr], Under)> {
    let &[Instr::PushFunc(ref f), Instr::Prim(Primitive::Group, span), ref input @ ..] = input
    else {
        return None;
    };
    let (f_before, f_after) = f.under(g_sig)?;
    let befores = vec![
        Instr::CopyToTemp {
            stack: TempStack::Under,
            count: 2,
            span,
        },
        Instr::PushFunc(f_before.into()),
        Instr::Prim(Primitive::Group, span),
    ];
    let afters = vec![
        Instr::PushFunc(f_after.into()),
        Instr::ImplPrim(ImplPrimitive::Ungroup, span),
    ];
    Some((input, (befores, afters)))
}

fn try_array_wrap(input: &[Instr]) -> Option<(&[Instr], &[Instr], usize, bool)> {
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
    let (inner, input) = input.split_at(end);
    let input = &input[1..];
    let (span, boxed) = end_arr?;
    Some((input, inner, span, boxed))
}

fn invert_array_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let (input, inner, span, unbox) = try_array_wrap(input)?;
    let mut instrs = invert_instrs(inner)?;
    let count = instrs_signature(&instrs).ok()?.args;
    instrs.insert(0, Instr::Unpack { count, span, unbox });
    Some((input, instrs))
}

fn under_array_pattern(input: &[Instr], g_sig: Signature) -> Option<(&[Instr], Under)> {
    let (input, inner, span, unbox) = try_array_wrap(input)?;
    let (mut befores, mut afters) = under_instrs(inner, g_sig)?;
    befores.insert(0, Instr::BeginArray);
    befores.push(Instr::EndArray { span, boxed: unbox });
    let count = instrs_signature(&afters).ok()?.args;
    afters.insert(0, Instr::Unpack { count, span, unbox });
    Some((input, (befores, afters)))
}

fn invert_unpack_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let [Instr::Unpack { span, unbox, .. }, input @ ..] = input else {
        return None;
    };
    let mut instrs = invert_instrs(input)?;
    instrs.insert(0, Instr::BeginArray);
    instrs.push(Instr::EndArray {
        span: *span,
        boxed: *unbox,
    });
    Some((input, instrs))
}

fn under_unpack_pattern(input: &[Instr], mut g_sig: Signature) -> Option<(&[Instr], Under)> {
    let [unpack @ Instr::Unpack { count, span, unbox }, input @ ..] = input else {
        return None;
    };
    if g_sig.args < *count {
        let diff = *count - g_sig.args;
        g_sig.args += diff;
        g_sig.outputs += diff;
    }
    let (mut befores, mut afters) = under_instrs(input, g_sig)?;
    befores.insert(0, unpack.clone());
    afters.insert(0, Instr::BeginArray);
    afters.push(Instr::EndArray {
        span: *span,
        boxed: *unbox,
    });
    Some((input, (befores, afters)))
}

impl<A: InvertPattern, B: InvertPattern> InvertPattern for (A, B) {
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        let (a, b) = self;
        let (input, mut a) = a.invert_extract(input)?;
        let (input, b) = b.invert_extract(input)?;
        a.extend(b);
        Some((input, a))
    }
}

impl<A: UnderPattern, B: UnderPattern> UnderPattern for (A, B) {
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b) = self;
        let (input, (mut a_before, a_after)) = a.under_extract(input, g_sig)?;
        let (input, (b_before, mut b_after)) = b.under_extract(input, g_sig)?;
        a_before.extend(b_before);
        b_after.extend(a_after);
        Some((input, (a_before, b_after)))
    }
}

impl<A: InvertPattern, B: InvertPattern, C: InvertPattern> InvertPattern for (A, B, C) {
    fn invert_extract<'a>(&self, mut input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        let (a, b, c) = self;
        let (inp, mut a) = a.invert_extract(input)?;
        input = inp;
        let (inp, b) = b.invert_extract(input)?;
        input = inp;
        let (inp, c) = c.invert_extract(input)?;
        a.extend(b);
        a.extend(c);
        Some((inp, a))
    }
}

struct IgnoreMany<T>(T);
impl<T: InvertPattern> InvertPattern for IgnoreMany<T> {
    fn invert_extract<'a>(&self, mut input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        while let Some((inp, _)) = self.0.invert_extract(input) {
            input = inp;
        }
        Some((input, Vec::new()))
    }
}

impl InvertPattern for Primitive {
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        let next = input.get(0)?;
        match next {
            Instr::Prim(prim, span) if prim == self => {
                Some((&input[1..], vec![Instr::Prim(*prim, *span)]))
            }
            _ => None,
        }
    }
}

impl<T> InvertPattern for (&[Primitive], &[T])
where
    T: AsInstr,
{
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
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
    fn under_extract<'a>(&self, input: &'a [Instr], _: Signature) -> Option<(&'a [Instr], Under)> {
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
    fn under_extract<'a>(&self, input: &'a [Instr], _: Signature) -> Option<(&'a [Instr], Under)> {
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
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        let (a, b) = self;
        (a.as_ref(), b.as_ref()).invert_extract(input)
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
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b, c) = self;
        (a.as_ref(), b.as_ref(), c.as_ref()).under_extract(input, g_sig)
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
    ) -> Option<(&'a [Instr], Under)> {
        let (a, b, c) = self;
        (a.as_ref(), b.as_ref(), c.as_ref()).under_extract(input, g_sig)
    }
}

impl<F> InvertPattern for F
where
    F: Fn(&[Instr]) -> Option<(&[Instr], Vec<Instr>)>,
{
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        self(input)
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
    F: Fn(&[Instr], Signature) -> Option<(&[Instr], Under)>,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Instr],
        g_sig: Signature,
    ) -> Option<(&'a [Instr], Under)> {
        (self.0)(input, g_sig)
    }
}

#[derive(Debug)]
struct Val;
impl InvertPattern for Val {
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        if input.is_empty() {
            return Some((input, Vec::new()));
        }
        for len in (1..input.len()).rev() {
            let chunk = &input[..len];
            if let Ok(sig) = instrs_signature(chunk) {
                if sig.args == 0 && sig.outputs == 1 && !chunk.iter().any(Instr::is_temp) {
                    let res = chunk.to_vec();
                    return Some((&input[len..], res));
                }
            }
        }
        match input.get(0) {
            Some(instr @ Instr::Push(_)) => Some((&input[1..], vec![instr.clone()])),
            Some(instr @ Instr::Prim(prim, _))
                if prim.args() == Some(0) && prim.outputs() == Some(0) =>
            {
                Some((&input[1..], vec![instr.clone()]))
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
                let array_construction = input[..=i].to_vec();
                Some((&input[i + 1..], array_construction))
            }
            _ => None,
        }
    }
}
impl UnderPattern for Val {
    fn under_extract<'a>(&self, input: &'a [Instr], _: Signature) -> Option<(&'a [Instr], Under)> {
        if let Some((input, inverted)) = self.invert_extract(input) {
            Some((input, (inverted, Vec::new())))
        } else {
            None
        }
    }
}
