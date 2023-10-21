//! Algorithms for invert and under

use std::{cell::RefCell, collections::HashMap, fmt};

use crate::{
    check::instrs_signature,
    function::{Function, Instr, Signature},
    primitive::Primitive,
    value::Value,
};

impl Function {
    pub fn inverse(&self) -> Option<Self> {
        Function::new_inferred(self.id.clone(), invert_instrs(&self.instrs)?).ok()
    }
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

    // println!("inverting {:?}", instrs);
    let mut inverted = Vec::new();
    let mut start = instrs.len() - 1;
    let mut end = instrs.len();
    loop {
        if let Some(mut inverted_fragment) = invert_instr_fragment(&instrs[start..end]) {
            inverted.append(&mut inverted_fragment);
            if start == 0 {
                break;
            }
            end = start;
            start = end - 1;
        } else if start == 0 {
            return None;
        } else {
            start -= 1;
        }
    }
    // println!("inverted {:?} to {:?}", instrs, inverted);
    INVERT_CACHE.with(|cache| {
        cache
            .borrow_mut()
            .insert(instrs.to_vec(), Some(inverted.clone()))
    });
    Some(inverted)
}

fn invert_instr_fragment(mut instrs: &[Instr]) -> Option<Vec<Instr>> {
    use Instr::*;
    use Primitive::*;
    match instrs {
        [Prim(prim, span)] => {
            return Some(match prim {
                Primitive::Sqrt => vec![Instr::push(2.0), Instr::Prim(Primitive::Pow, *span)],
                prim => vec![Instr::Prim(prim.inverse()?, *span)],
            })
        }
        [PushFunc(val)] => {
            if let Some((prim, span)) = val.as_primitive() {
                return Some(vec![Instr::Prim(prim.inverse()?, span)]);
            }
        }
        _ => {}
    }

    let patterns: &[&dyn InvertPattern] = &[
        &(function, ([Invert], <[Primitive; 0]>::default())),
        &(Val, ([Rotate], [Neg, Rotate])),
        &(Val, IgnoreMany(Flip), ([Add], [Sub])),
        &(Val, ([Sub], [Add])),
        &(Val, ([Flip, Sub], [Flip, Sub])),
        &(Val, IgnoreMany(Flip), ([Mul], [Div])),
        &(Val, ([Div], [Mul])),
        &(Val, ([Flip, Div], [Flip, Div])),
        &([Dup, Add], [2.i(), Div.i()]),
        &([Dup, Mul], [Sqrt]),
        &invert_pow_pattern,
        &invert_log_pattern,
        &invert_repeat_pattern,
    ];

    let mut inverted = Vec::new();
    'find_pattern: loop {
        for pattern in patterns {
            if let Some((input, inv)) = pattern.invert_extract(instrs) {
                inverted.extend(inv);
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
    use Primitive::*;

    macro_rules! stash2 {
        ($before:expr, $after:expr) => {
            (
                [$before],
                [Over.i(), Over.i(), PushTempUnderN(2).i(), $before.i()],
                [PopTempUnderN(2).i(), $after.i()],
            )
        };
    }

    macro_rules! bin {
        (Flip, $before:expr, $after:expr) => {
            (
                [Flip, $before],
                [Dup.i(), PushTempUnderN(1).i(), Flip.i(), $before.i()],
                [PopTempUnderN(1).i(), $after.i()],
            )
        };
        (Flip, $before:expr) => {
            (
                [Flip, $before],
                [Dup.i(), PushTempUnderN(1).i(), Flip.i(), $before.i()],
                [PopTempUnderN(1).i(), Flip.i(), $before.i()],
            )
        };
        ($before:expr, $after:expr) => {
            (
                [$before],
                [Dup.i(), PushTempUnderN(1).i(), $before.i()],
                [PopTempUnderN(1).i(), $after.i()],
            )
        };
    }

    let patterns: &[&dyn UnderPattern] = &[
        &UnderPatternFn(under_both_pattern),
        &bin!(Flip, Add, Sub),
        &bin!(Flip, Mul, Div),
        &bin!(Flip, Sub),
        &bin!(Flip, Div),
        &bin!(Add, Sub),
        &bin!(Sub, Add),
        &bin!(Mul, Div),
        &bin!(Div, Mul),
        // It is important that this comes after the things above
        &UnderPatternFn(under_from_inverse_pattern),
        &UnderPatternFn(under_temp_pattern),
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
            (
                [Keep],
                [Over.i(), Over.i(), PushTempUnderN(2).i(), Keep.i()],
                [
                    PopTempUnderN(1).i(),
                    Flip.i(),
                    PopTempUnderN(1).i(),
                    Unkeep.i(),
                ],
            ),
        ),
        &(
            [Keep],
            [Over.i(), Over.i(), PushTempUnderN(2).i(), Keep.i()],
            [
                PopTempUnderN(1).i(),
                Flip.i(),
                PopTempUnderN(1).i(),
                Unkeep.i(),
            ],
        ),
        &(
            [Rotate],
            [Dup.i(), PushTempUnderN(1).i(), Rotate.i()],
            [PopTempUnderN(1).i(), Neg.i(), Rotate.i()],
        ),
        &(
            [First],
            [Dup.i(), PushTempUnderN(1).i(), First.i()],
            [PopTempUnderN(1).i(), 1.i(), Drop.i(), Flip.i(), Join.i()],
        ),
        &(
            [Last],
            [Dup.i(), PushTempUnderN(1).i(), Last.i()],
            [PopTempUnderN(1).i(), (-1).i(), Drop.i(), Join.i()],
        ),
        &(
            [Shape],
            [Dup.i(), PushTempUnderN(1).i(), Shape.i()],
            [PopTempUnderN(1).i(), Flip.i(), Reshape.i()],
        ),
        &(
            [Deshape],
            [Dup.i(), Shape.i(), PushTempUnderN(1).i(), Deshape.i()],
            [PopTempUnderN(1).i(), Reshape.i()],
        ),
        &(
            [Pow],
            [Dup.i(), PushTempUnderN(1).i(), Pow.i()],
            [PopTempUnderN(1).i(), 1.i(), Div.i(), Pow.i()],
        ),
        &(
            [Log],
            [Dup.i(), PushTempUnderN(1).i(), Log.i()],
            [PopTempUnderN(1).i(), Pow.i()],
        ),
        &(
            [Now],
            [Now.i(), PushTempUnderN(1).i()],
            [PopTempUnderN(1).i(), Now.i(), Flip.i(), Sub.i()],
        ),
    ];

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
                    // Hacky solution for when there are both inline and under temps
                    // in the afters
                    // With this hack, both these will work:
                    // f ← |3 ⍜(|3 ↙⊙↘)(×10)
                    // f 2 1 [1 2 3 4 5]
                    // f ← ⍜⊙⇌(×10)
                    // f [1 2 3] [4 5 6]
                    // This can likely be fixes with the newer g_sig checking
                    if afters
                        .iter()
                        .any(|instr| matches!(instr, Instr::PopTempUnder { .. }))
                    {
                        afters.retain(|instr| {
                            !matches!(
                                instr,
                                Instr::PopTempInline { .. } | Instr::PushTempInline { .. }
                            )
                        });
                    }

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

trait AsInstr: fmt::Debug {
    fn as_instr(&self, span: usize) -> Instr;
    fn i(&self) -> Box<dyn AsInstr>
    where
        Self: Copy + 'static,
    {
        Box::new(*self)
    }
}

#[derive(Debug, Clone, Copy)]
struct PushTempUnderN(usize);
impl AsInstr for PushTempUnderN {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PushTempUnder {
            count: self.0,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct PopTempUnderN(usize);
impl AsInstr for PopTempUnderN {
    fn as_instr(&self, span: usize) -> Instr {
        Instr::PopTempUnder {
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

impl AsInstr for Box<dyn AsInstr> {
    fn as_instr(&self, span: usize) -> Instr {
        self.as_ref().as_instr(span)
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

fn under_from_inverse_pattern(input: &[Instr], _: Signature) -> Option<(&[Instr], Under)> {
    if input.is_empty() {
        return None;
    }
    let mut end = input.len();
    loop {
        if let Some(inverse) = invert_instrs(&input[..end]) {
            return Some((&input[end..], (input[..end].to_vec(), inverse)));
        }
        if end == 1 {
            return None;
        }
        end -= 1;
    }
}

fn under_temp_pattern(input: &[Instr], _: Signature) -> Option<(&[Instr], Under)> {
    match input.split_first()? {
        (&Instr::PushTempInline { count, span }, input) => Some((
            input,
            (
                vec![Instr::PushTempInline { count, span }],
                vec![Instr::PopTempInline { count, span }],
            ),
        )),
        (&Instr::PopTempInline { count, span }, input) => Some((
            input,
            (
                vec![Instr::PopTempInline { count, span }],
                vec![Instr::PushTempInline { count, span }],
            ),
        )),
        _ => None,
    }
}

fn under_both_pattern(input: &[Instr], g_sig: Signature) -> Option<(&[Instr], Under)> {
    let [input @ .., Instr::Prim(Primitive::Both, span)] = input else {
        return None;
    };
    let (Instr::PushFunc(func), input) = input.split_last()? else {
        return None;
    };
    let (befores, afters) = under_instrs(&func.instrs, g_sig)?;
    let (befores, afters) = match (g_sig.args, g_sig.outputs) {
        (2, 1) => {
            let before_func = Function::new(func.id.clone(), befores, func.signature());
            let befores = vec![
                Instr::push_func(before_func),
                Instr::Prim(Primitive::Both, *span),
            ];
            (befores, afters)
        }
        (2, 2) => {
            let before_func = Function::new(func.id.clone(), befores, func.signature());
            let after_func = Function::new(func.id.clone(), afters, func.signature());
            let befores = vec![
                Instr::push_func(before_func),
                Instr::Prim(Primitive::Both, *span),
            ];
            let afters = vec![
                Instr::push_func(after_func),
                Instr::Prim(Primitive::Both, *span),
            ];
            (befores, afters)
        }
        _ => return None,
    };
    Some((input, (befores, afters)))
}

impl<A: InvertPattern, B: InvertPattern> InvertPattern for (A, B) {
    fn invert_extract<'a>(&self, mut input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        let (a, b) = self;
        let (inp, mut a) = a.invert_extract(input)?;
        input = inp;
        let (inp, b) = b.invert_extract(input)?;
        a.extend(b);
        Some((inp, a))
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

impl<F> InvertPattern for F
where
    F: Fn(&[Instr]) -> Option<(&[Instr], Vec<Instr>)>,
{
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        self(input)
    }
}

struct UnderPatternFn<F>(F);
impl<F> fmt::Debug for UnderPatternFn<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UnderPatternFn")
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

fn function(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    if let (instr @ Instr::PushFunc(_), input) = input.split_first()? {
        Some((input, vec![instr.clone()]))
    } else {
        None
    }
}

fn invert_pow_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let (input, mut val) = Val.invert_extract(input)?;
    if let (Instr::Prim(Primitive::Pow, span), input) = input.split_first()? {
        val.insert(0, Instr::push(1.0));
        val.extend([
            Instr::Prim(Primitive::Div, *span),
            Instr::Prim(Primitive::Pow, *span),
        ]);
        Some((input, val))
    } else {
        None
    }
}

fn invert_log_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let (input, mut val) = Val.invert_extract(input)?;
    if let (Instr::Prim(Primitive::Log, span), input) = input.split_first()? {
        val.extend([
            Instr::Prim(Primitive::Flip, *span),
            Instr::Prim(Primitive::Pow, *span),
        ]);
        Some((input, val))
    } else {
        None
    }
}

fn invert_repeat_pattern(input: &[Instr]) -> Option<(&[Instr], Vec<Instr>)> {
    let (input, mut instrs) = Val.invert_extract(input)?;
    if input.len() < 2 {
        return None;
    }
    if let ([Instr::Push(f), Instr::Prim(Primitive::Repeat, span)], input) = input.split_at(2) {
        instrs.push(Instr::Prim(Primitive::Neg, *span));
        instrs.push(Instr::Push(f.clone()));
        instrs.push(Instr::Prim(Primitive::Repeat, *span));
        Some((input, instrs))
    } else {
        None
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
