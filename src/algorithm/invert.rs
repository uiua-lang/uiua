//! Algorithms for invert and under

use std::{borrow::Cow, cell::RefCell, collections::HashMap, fmt};

use crate::{
    check::instrs_signature,
    function::{Function, FunctionKind, Instr},
    primitive::Primitive,
    value::Value,
};

impl Function {
    pub fn inverse(&self) -> Option<Self> {
        if !matches!(self.kind, FunctionKind::Normal) {
            return None;
        }
        Function::new_inferred(
            self.id.clone(),
            invert_instrs(&self.instrs)?,
            FunctionKind::Normal,
        )
        .ok()
    }
    pub fn under(self) -> Option<(Self, Self)> {
        if let Some(f) = self.inverse() {
            Some((self, f))
        } else {
            let (befores, afters) = under_instrs(&self.instrs)?;
            let before =
                Function::new_inferred(self.id.clone(), befores, FunctionKind::Normal).ok()?;
            let after = Function::new_inferred(self.id, afters, FunctionKind::Normal).ok()?;
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

    // println!("invert {:?}", instrs);
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
        [Push(val)] => {
            if let Some((prim, span)) = val.as_primitive() {
                return Some(vec![Instr::Prim(prim.inverse()?, span)]);
            }
        }
        _ => {}
    }

    let patterns: &[&dyn InvertPattern] = &[
        &(Val, ([Invert], [Primitive::Call])),
        &(Val, ([Rotate], [Neg, Rotate])),
        &(Val, IgnoreMany(Flip), ([Add], [Sub])),
        &(Val, ([Sub], [Add])),
        &(Val, IgnoreMany(Flip), ([Mul], [Div])),
        &(Val, ([Div], [Mul])),
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

fn under_instrs(instrs: &[Instr]) -> Option<Under> {
    if instrs.is_empty() {
        return Some((Vec::new(), Vec::new()));
    }

    thread_local! {
        static UNDER_CACHE: RefCell<HashMap<Vec<Instr>, Option<Under>>> = RefCell::new(HashMap::new());
    }
    if let Some(under) = UNDER_CACHE.with(|cache| cache.borrow().get(instrs).cloned()) {
        return under;
    }

    let mut befores = Vec::new();
    let mut afters = Vec::new();
    let mut start = 0;
    let mut end = instrs.len();
    loop {
        if let Some((before, mut after)) = under_instr_fragment(&instrs[start..end]) {
            after.append(&mut afters);
            afters = after;
            match before {
                Cow::Borrowed(before) => befores.extend_from_slice(before),
                Cow::Owned(before) => befores.extend(before),
            }
            if start == 0 {
                break;
            }
            end = start;
            start = 0;
        } else if start == 0 {
            return None;
        } else {
            start += 1;
        }
    }
    // println!("under {:?} to {:?} {:?}", instrs, befores, afters);
    let under = (befores, afters);
    UNDER_CACHE.with(|cache| {
        cache
            .borrow_mut()
            .insert(instrs.to_vec(), Some(under.clone()))
    });
    Some(under)
}

fn under_instr_fragment(mut instrs: &[Instr]) -> Option<(Cow<[Instr]>, Vec<Instr>)> {
    use Primitive::*;
    if let Some(inverted) = invert_instr_fragment(instrs) {
        return Some((Cow::Borrowed(instrs), inverted));
    }

    let patterns: &[&dyn UnderPattern] = &[
        &(Val, ([Take], [Over, Over, Take], [Untake])),
        &([Take], [Over, Over, Take], [Untake]),
        &(Val, ([Drop], [Over, Over, Drop], [Undrop])),
        &([Drop], [Over, Over, Drop], [Undrop]),
        &(Val, ([Select], [Over, Over, Select], [Unselect])),
        &([Select], [Over, Over, Select], [Unselect]),
        &(Val, ([Pick], [Over, Over, Pick], [Unpick])),
        &([Pick], [Over, Over, Pick], [Unpick]),
        &([Rotate], [Flip, Over, Rotate], [Flip, Neg, Rotate]),
        &(
            [First],
            [Dup, First],
            [Flip.i(), 1.i(), Drop.i(), Flip.i(), Join.i()],
        ),
        &(
            [Last],
            [Dup, Last],
            [Flip.i(), (-1).i(), Drop.i(), Join.i()],
        ),
        &([Shape], [Dup, Shape], [Reshape]),
        &([Deshape], [Dup, Shape, Flip, Deshape], [Flip, Reshape]),
    ];

    let mut befores = Vec::new();
    let mut afters = Vec::new();
    'find_pattern: loop {
        for pattern in patterns {
            if let Some((input, (bef, aft))) = pattern.under_extract(instrs) {
                // println!("matched under pattern: {pattern:?}");
                befores.extend(bef);
                afters = aft.into_iter().chain(afters).collect();
                if input.is_empty() {
                    return Some((Cow::Owned(befores), afters));
                }
                instrs = input;
                continue 'find_pattern;
            }
        }
        break;
    }

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
    fn under_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Under)>;
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
    fn under_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Under)> {
        let (a, b) = self;
        let (input, (mut a_before, a_after)) = a.under_extract(input)?;
        let (input, (b_before, mut b_after)) = b.under_extract(input)?;
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

struct AnyOf<T, const N: usize>([T; N]);
impl<T: InvertPattern, const N: usize> InvertPattern for AnyOf<T, N> {
    fn invert_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Vec<Instr>)> {
        for pattern in self.0.iter() {
            if let Some((input, inverted)) = pattern.invert_extract(input) {
                return Some((input, inverted));
            }
        }
        None
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
    fn under_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Under)> {
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
                    .map(|(p, s)| p.clone().as_instr(*s))
                    .collect(),
                c.iter()
                    .zip(spans.iter().cycle())
                    .map(|(p, s)| p.clone().as_instr(*s))
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
    fn under_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Under)> {
        let (a, b, c) = self;
        (a.as_ref(), b.as_ref(), c.as_ref()).under_extract(input)
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
                if sig.args == 0 && sig.outputs == 1 {
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
    fn under_extract<'a>(&self, input: &'a [Instr]) -> Option<(&'a [Instr], Under)> {
        if let Some((input, inverted)) = self.invert_extract(input) {
            Some((input, (inverted, Vec::new())))
        } else {
            None
        }
    }
}
