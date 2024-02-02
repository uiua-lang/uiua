use std::fmt;

use ecow::EcoVec;

use crate::{ImplPrimitive, Instr, Primitive};

pub(crate) fn optimize_instrs_mut(instrs: &mut EcoVec<Instr>, mut new: Instr, maximal: bool) {
    use ImplPrimitive::*;
    use Primitive::*;
    if let Instr::Push(val) = &mut new {
        val.compress();
    }
    match (instrs.make_mut(), new) {
        // Cosine
        ([.., Instr::Prim(Eta, _), Instr::Prim(Add, _)], Instr::Prim(Sin, span)) => {
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(Cos, span));
        }
        // First Rise = FirstMinIndex
        ([.., Instr::Prim(Rise, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.push(Instr::ImplPrim(FirstMinIndex, span))
        }
        // First Reverse Fall = LastMinIndex
        ([.., Instr::Prim(Fall, _), Instr::Prim(Reverse, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(LastMinIndex, span))
        }
        // First Fall = FirstMaxIndex
        ([.., Instr::Prim(Fall, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.push(Instr::ImplPrim(FirstMaxIndex, span))
        }
        // First Reverse Rise = LastMaxIndex
        ([.., Instr::Prim(Rise, _), Instr::Prim(Reverse, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(LastMaxIndex, span))
        }
        // First Reverse = last
        ([.., Instr::Prim(Reverse, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.push(Instr::ImplPrim(Last, span))
        }
        // Combine push temps
        (
            [.., Instr::PushTemp {
                stack: a_stack,
                count: a_count,
                ..
            }],
            Instr::PushTemp {
                stack: b_stack,
                count: b_count,
                ..
            },
        ) if *a_stack == b_stack => {
            *a_count += b_count;
        }
        // Combine pop temps
        (
            [.., Instr::PopTemp {
                stack: a_stack,
                count: a_count,
                ..
            }],
            Instr::PopTemp {
                stack: b_stack,
                count: b_count,
                ..
            },
        ) if *a_stack == b_stack => {
            *a_count += b_count;
        }
        // Dips
        (
            [.., Instr::PushTemp {
                stack: a_stack,
                count: a_count,
                ..
            }, Instr::Prim(Identity, span)],
            Instr::PopTemp {
                stack: b_stack,
                count: b_count,
                ..
            },
        ) if maximal && *a_stack == b_stack && *a_count == b_count => {
            let span = *span;
            let count = *a_count + 1;
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::TouchStack { count, span });
        }
        // Tranposes
        ([.., Instr::Prim(Transpose, span)], Instr::Prim(Transpose, _)) => {
            let span = *span;
            instrs.pop();
            instrs.push(Instr::ImplPrim(TransposeN(2), span));
        }
        ([.., Instr::ImplPrim(TransposeN(n), _)], Instr::Prim(Transpose, _)) => {
            *n += 1;
            if *n == 0 {
                instrs.pop();
            }
        }
        ([.., Instr::ImplPrim(TransposeN(a), _)], Instr::ImplPrim(TransposeN(b), _)) => {
            *a += b;
            if *a == 0 {
                instrs.pop();
            }
        }
        // Sorting
        ([.., Instr::Prim(Dup, _), Instr::Prim(Rise, _)], Instr::Prim(Select, span)) => {
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(SortUp, span));
        }
        ([.., Instr::Prim(Dup, _), Instr::Prim(Fall, _)], Instr::Prim(Select, span)) => {
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(SortDown, span));
        }
        // Replace rand
        ([.., Instr::Prim(Pop, span), Instr::Prim(Pop, _)], Instr::Prim(Rand, _)) => {
            let span = *span;
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(ReplaceRand2, span));
        }
        ([.., Instr::Prim(Pop, span)], Instr::ImplPrim(ReplaceRand, _)) => {
            let span = *span;
            instrs.pop();
            instrs.push(Instr::ImplPrim(ReplaceRand2, span));
        }
        ([.., Instr::Prim(Pop, span)], Instr::Prim(Rand, _)) => {
            let span = *span;
            instrs.pop();
            instrs.push(Instr::ImplPrim(ReplaceRand, span));
        }
        (_, instr) => instrs.push(instr),
    }
}

pub(crate) fn optimize_instrs<I>(instrs: I, maximal: bool) -> EcoVec<Instr>
where
    I: IntoIterator<Item = Instr> + fmt::Debug,
    I::IntoIter: ExactSizeIterator,
{
    // println!("optimize {:?}", instrs);
    let instrs = instrs.into_iter();
    let mut new = EcoVec::with_capacity(instrs.len());
    for instr in instrs {
        if instr.is_compile_only() {
            continue;
        }
        optimize_instrs_mut(&mut new, instr, maximal);
    }
    // println!("to       {:?}", new);
    new
}
