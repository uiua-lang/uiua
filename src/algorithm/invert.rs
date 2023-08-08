use crate::{
    function::{Function, FunctionKind, Instr},
    primitive::Primitive,
};

impl Function {
    pub fn inverse(&self) -> Option<Self> {
        if !matches!(self.kind, FunctionKind::Normal) {
            return None;
        }
        Some(Function {
            id: self.id.clone(),
            instrs: invert_instrs(&self.instrs)?,
            kind: FunctionKind::Normal,
        })
    }
}

fn invert_instrs(instrs: &[Instr]) -> Option<Vec<Instr>> {
    if instrs.is_empty() {
        return Some(Vec::new());
    }
    let mut inverted = Vec::new();
    let mut start = instrs.len() - 1;
    let mut end = instrs.len();
    loop {
        if let Some(mut inverted_fragment) = invert_instr_fragment(&instrs[start..end]) {
            inverted_fragment.append(&mut inverted);
            inverted = inverted_fragment;
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
    Some(inverted)
}

fn invert_instr_fragment(instrs: &[Instr]) -> Option<Vec<Instr>> {
    use Instr::*;
    use Primitive::*;
    if let [Prim(prim, span)] = instrs {
        return Some(match prim {
            Primitive::Sqrt => vec![Instr::push(2.0), Instr::Prim(Primitive::Pow, *span)],
            prim => vec![Instr::Prim(prim.inverse()?, *span)],
        });
    }

    let patterns: &[&dyn InstrPattern] = &[
        &(Val, ([Rotate], [Neg, Rotate])),
        &(Val, IgnoreMany(Flip), ([Add], [Sub])),
        &(Val, ([Sub], [Add])),
        &(Val, IgnoreMany(Flip), ([Mul], [Div])),
        &(Val, ([Div], [Mul])),
        &(invert_pow_pattern),
        &(invert_log_pattern),
    ];

    for pattern in patterns {
        let mut input = instrs;
        if let Some(inverted) = pattern.extract(&mut input) {
            if input.is_empty() {
                return Some(inverted);
            }
        }
    }

    None
}

trait InstrPattern {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>>;
}

impl<A: InstrPattern, B: InstrPattern> InstrPattern for (A, B) {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        let (a, b) = self;
        let mut a = a.extract(input)?;
        let b = b.extract(input)?;
        a.extend(b);
        Some(a)
    }
}

impl<A: InstrPattern, B: InstrPattern, C: InstrPattern> InstrPattern for (A, B, C) {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        let (a, b, c) = self;
        let mut a = a.extract(input)?;
        let b = b.extract(input)?;
        let c = c.extract(input)?;
        a.extend(b);
        a.extend(c);
        Some(a)
    }
}

struct IgnoreMany<T>(T);
impl<T: InstrPattern> InstrPattern for IgnoreMany<T> {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        while self.0.extract(input).is_some() {}
        Some(Vec::new())
    }
}

struct AnyOf<T, const N: usize>([T; N]);
impl<T: InstrPattern, const N: usize> InstrPattern for AnyOf<T, N> {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        for pattern in self.0.iter() {
            let mut inp = *input;
            if let Some(inverted) = pattern.extract(&mut inp) {
                *input = inp;
                return Some(inverted);
            }
        }
        None
    }
}

impl InstrPattern for Primitive {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        let next = input.get(0)?;
        match next {
            Instr::Prim(prim, span) if prim == self => {
                *input = &input[1..];
                Some(vec![Instr::Prim(*prim, *span)])
            }
            _ => None,
        }
    }
}

impl InstrPattern for (&[Primitive], &[Primitive]) {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
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
        *input = &input[a.len()..];
        Some(
            b.iter()
                .zip(spans.iter().cycle())
                .map(|(p, s)| Instr::Prim(*p, *s))
                .collect(),
        )
    }
}

impl<const A: usize, const B: usize> InstrPattern for ([Primitive; A], [Primitive; B]) {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        let (a, b) = *self;
        (a.as_ref(), b.as_ref()).extract(input)
    }
}

impl<F> InstrPattern for F
where
    F: Fn(&mut &[Instr]) -> Option<Vec<Instr>>,
{
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        self(input)
    }
}

fn invert_pow_pattern(input: &mut &[Instr]) -> Option<Vec<Instr>> {
    let val = Val.extract(input)?;
    let next = input.get(0)?;
    if let Instr::Prim(Primitive::Pow, span) = next {
        *input = &input[1..];
        Some(vec![
            Instr::push(1),
            val[0].clone(),
            Instr::Prim(Primitive::Div, *span),
            Instr::Prim(Primitive::Pow, *span),
        ])
    } else {
        None
    }
}

fn invert_log_pattern(input: &mut &[Instr]) -> Option<Vec<Instr>> {
    let val = Val.extract(input)?;
    let next = input.get(0)?;
    if let Instr::Prim(Primitive::Log, span) = next {
        *input = &input[1..];
        Some(vec![
            val[0].clone(),
            Instr::Prim(Primitive::Flip, *span),
            Instr::Prim(Primitive::Pow, *span),
        ])
    } else {
        None
    }
}

pub struct Val;
impl InstrPattern for Val {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        if input.is_empty() {
            return None;
        }
        for len in (1..input.len()).rev() {
            let chunk = &input[..len];
            if instrs_args_outputs(chunk) == Some((0, 1)) {
                let res = chunk.to_vec();
                *input = &input[len..];
                return Some(res);
            }
        }
        match input.get(0) {
            Some(instr @ (Instr::Push(_) | Instr::DfnVal(_))) => {
                *input = &input[1..];
                Some(vec![instr.clone()])
            }
            Some(instr @ Instr::Prim(prim, _))
                if prim.args() == Some(0) && prim.outputs() == Some(1) =>
            {
                *input = &input[1..];
                Some(vec![instr.clone()])
            }
            Some(Instr::BeginArray) => {
                let mut depth = 1;
                let mut i = 1;
                loop {
                    if let Instr::EndArray(_) = input.get(i)? {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    } else if let Instr::BeginArray = input.get(i)? {
                        depth += 1;
                    }
                    i += 1;
                }
                let array_construction = &input[..=i];
                *input = &input[i + 1..];
                Some(array_construction.to_vec())
            }
            _ => None,
        }
    }
}

/// Count the number of arguments and outputs of list of instructions.
fn instrs_args_outputs(instrs: &[Instr]) -> Option<(usize, usize)> {
    let mut args = 0;
    let mut outputs = 0;
    let mut instrs = instrs.iter();
    while let Some(instr) = instrs.next() {
        match instr {
            Instr::Push(_) | Instr::DfnVal(_) => outputs += 1,
            Instr::Prim(prim, _) => {
                if let Some((..)) = prim.modifier_args() {
                    // TODO: handle modifiers
                    return None;
                } else {
                    let pargs = prim.args()? as usize;
                    let consumed_outputs = pargs.min(outputs);
                    outputs -= consumed_outputs;
                    args += pargs - consumed_outputs;
                    outputs += prim.outputs()? as usize;
                }
            }
            Instr::BeginArray => {
                let mut depth = 1;
                for instr in instrs.by_ref() {
                    match instr {
                        Instr::BeginArray => depth += 1,
                        Instr::EndArray(_) => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                outputs += 1;
            }
            Instr::EndArray(_) => return None,
            Instr::Call(_) => return None,
        }
    }
    Some((args, outputs))
}

#[cfg(test)]
mod test {
    use super::*;
    use Primitive::*;
    #[test]
    fn instrs_args_outputs() {
        let mut instrs = vec![Instr::push(10), Instr::push(2), Instr::Prim(Pow, 0)];
        assert_eq!(Some((0, 1)), super::instrs_args_outputs(&instrs));

        instrs.push(Instr::Prim(Add, 0));
        assert_eq!(Some((1, 1)), super::instrs_args_outputs(&instrs));
    }
}
