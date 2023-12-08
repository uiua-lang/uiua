use std::fmt;

use ecow::EcoVec;

use crate::{ImplPrimitive, Instr, Primitive};

pub(crate) fn optimize_instrs_mut(instrs: &mut EcoVec<Instr>, new: Instr, maximal: bool) {
    use ImplPrimitive::*;
    use Primitive::*;
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
