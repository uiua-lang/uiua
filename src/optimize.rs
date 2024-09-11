use std::fmt;

use ecow::EcoVec;

use crate::{Assembly, ImplPrimitive, Instr, Primitive, TempStack};

pub(crate) fn optimize_instrs_mut(
    instrs: &mut EcoVec<Instr>,
    mut new: Instr,
    maximal: bool,
    asm: &Assembly,
) {
    use ImplPrimitive::*;
    use Primitive::*;
    if let Instr::Push(val) = &mut new {
        val.compress();
    }
    match (instrs.make_mut(), new) {
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
        // First/last/len where
        ([.., Instr::Prim(Where, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.push(Instr::ImplPrim(FirstWhere, span))
        }
        ([.., Instr::Prim(Where, _), Instr::Prim(Reverse, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(LastWhere, span))
        }
        ([.., Instr::Prim(Where, _)], Instr::Prim(Len, span)) => {
            instrs.pop();
            instrs.push(Instr::ImplPrim(LenWhere, span))
        }
        // First Reverse = last
        ([.., Instr::Prim(Reverse, _)], Instr::Prim(First, span)) => {
            instrs.pop();
            instrs.push(Instr::Prim(Last, span))
        }
        // Count unique
        ([.., Instr::Prim(Deduplicate, _)], Instr::Prim(Len, span)) => {
            instrs.pop();
            instrs.push(Instr::ImplPrim(CountUnique, span))
        }
        // // Combine push temps
        // (
        //     [.., Instr::PushTemp {
        //         stack: a_stack,
        //         count: a_count,
        //         ..
        //     }],
        //     Instr::PushTemp {
        //         stack: b_stack,
        //         count: b_count,
        //         ..
        //     },
        // ) if *a_stack == b_stack => {
        //     *a_count += b_count;
        // }
        // // Combine pop temps
        // (
        //     [.., Instr::PopTemp {
        //         stack: a_stack,
        //         count: a_count,
        //         ..
        //     }],
        //     Instr::PopTemp {
        //         stack: b_stack,
        //         count: b_count,
        //         ..
        //     },
        // ) if *a_stack == b_stack => {
        //     *a_count += b_count;
        // }
        // Dips
        (
            [.., Instr::PushTemp {
                stack: a_stack,
                count: a_count,
                ..
            }],
            Instr::PopTemp {
                stack: b_stack,
                count: b_count,
                span,
                ..
            },
        ) if maximal && *a_stack == b_stack && *a_count == b_count => {
            let count = *a_count;
            instrs.pop();
            instrs.push(Instr::TouchStack { count, span });
        }
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
        (
            [.., Instr::PushTemp {
                stack: a_stack,
                count: a_count,
                ..
            }, Instr::TouchStack { count, span }],
            Instr::PopTemp {
                stack: b_stack,
                count: b_count,
                ..
            },
        ) if maximal && *a_stack == b_stack && *a_count == b_count => {
            let span = *span;
            let count = *a_count + *count;
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
            instrs.push(Instr::Prim(Sort, span));
        }
        ([.., Instr::Prim(Dup, _), Instr::Prim(Fall, _)], Instr::Prim(Select, span)) => {
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(SortDown, span));
        }
        ([.., Instr::Prim(Sort, _)], Instr::Prim(Reverse, span)) => {
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
        ([.., Instr::TouchStack { count: 1, span }], Instr::Prim(Rand, _)) => {
            // By rand
            let span = *span;
            instrs.pop();
            instrs.push(Instr::Prim(Dup, span));
            instrs.push(Instr::ImplPrim(ReplaceRand, span));
        }
        (
            [.., Instr::PushTemp {
                stack: TempStack::Inline,
                count: 1,
                span,
            }, Instr::Prim(Rand, _)],
            Instr::PopTemp {
                stack: TempStack::Inline,
                count: 1,
                span: pop_span,
            },
        ) => {
            // On rand
            let span = *span;
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::copy_inline(span));
            instrs.push(Instr::ImplPrim(ReplaceRand, span));
            instrs.push(Instr::pop_inline(1, pop_span));
        }
        ([.., Instr::Prim(Windows, _), Instr::PushFunc(f)], instr @ Instr::Prim(Rows, span)) => {
            match f.instrs(asm) {
                // Adjacent
                [inner @ Instr::PushFunc(reduced_f), Instr::Prim(Reduce, span)]
                    if reduced_f.signature() == (2, 1) =>
                {
                    let inner = inner.clone();
                    instrs.pop();
                    instrs.pop();
                    instrs.push(inner);
                    instrs.push(Instr::ImplPrim(ImplPrimitive::Adjacent, *span));
                }
                // Rows Windows
                _ if f.signature() == (1, 1) => {
                    let f = f.clone();
                    instrs.pop();
                    instrs.pop();
                    instrs.push(Instr::PushFunc(f));
                    instrs.push(Instr::ImplPrim(ImplPrimitive::RowsWindows, span));
                }
                _ => {
                    instrs.push(instr);
                }
            }
        }
        // Reduce depth
        ([.., Instr::PushFunc(f)], instr @ Instr::Prim(Rows, _)) => {
            if let [inner @ Instr::PushFunc(_), Instr::Prim(Reduce, span)] = f.instrs(asm) {
                let inner = inner.clone();
                instrs.pop();
                instrs.push(inner);
                instrs.push(Instr::ImplPrim(ImplPrimitive::ReduceDepth(1), *span));
            } else if let [inner @ Instr::PushFunc(_), Instr::ImplPrim(ImplPrimitive::ReduceDepth(depth), span)] =
                f.instrs(asm)
            {
                let inner = inner.clone();
                instrs.pop();
                instrs.push(inner);
                instrs.push(Instr::ImplPrim(
                    ImplPrimitive::ReduceDepth(depth + 1),
                    *span,
                ));
            } else {
                instrs.push(instr);
            }
        }
        // Reduce table
        (
            [.., Instr::PushFunc(g), Instr::Prim(Table, _), Instr::PushFunc(f)],
            Instr::Prim(Reduce, span),
        ) if g.signature() == (2, 1) && f.signature() == (2, 1) => {
            let f = instrs.pop().unwrap();
            instrs.pop();
            instrs.push(f);
            instrs.push(Instr::ImplPrim(ImplPrimitive::ReduceTable, span));
        }
        // Pop constant
        ([.., Instr::Push(_)], Instr::Prim(Pop, _)) => {
            instrs.pop();
        }
        // End array repeat rand
        (
            [.., Instr::PushFunc(f), Instr::Prim(Repeat, span)],
            Instr::EndArray { boxed: false, .. },
        ) if f.as_primitive(asm) == Some(Rand) => {
            let span = *span;
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::ImplPrim(EndRandArray, span));
        }
        // Astar first
        ([.., Instr::Prim(Astar, span)], Instr::Prim(First, _)) => {
            let span = *span;
            instrs.pop();
            instrs.push(Instr::ImplPrim(AstarFirst, span));
        }
        ([.., Instr::Prim(Astar, span)], Instr::Prim(Pop, pop_span)) => {
            let span = *span;
            instrs.pop();
            instrs.push(Instr::ImplPrim(AstarFirst, span));
            instrs.push(Instr::Prim(Pop, pop_span));
        }
        // TraceN
        ([.., Instr::Prim(Trace, span)], Instr::Prim(Trace, _)) => {
            let span = *span;
            instrs.pop();
            instrs.push(Instr::ImplPrim(TraceN(2, false), span));
        }
        ([.., Instr::ImplPrim(TraceN(n, false), _)], Instr::Prim(Trace, _)) => {
            *n += 1;
            if *n == 0 {
                instrs.pop();
            }
        }
        // Validate type
        (
            [.., Instr::Prim(Dup, _), Instr::Prim(Type, _), Instr::Push(val)],
            Instr::ImplPrim(MatchPattern, span),
        ) => {
            let val = val.clone();
            instrs.pop();
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::Push(val));
            instrs.push(Instr::ImplPrim(ValidateType, span));
        }
        ([.., Instr::Prim(Type, _), Instr::Push(val)], Instr::ImplPrim(MatchPattern, span)) => {
            let val = val.clone();
            instrs.pop();
            instrs.pop();
            instrs.push(Instr::Push(val));
            instrs.push(Instr::ImplPrim(ValidateTypeConsume, span));
        }
        ([.., Instr::ImplPrim(TraceN(a, inv_a), _)], Instr::ImplPrim(TraceN(b, inv_b), _))
            if *inv_a == inv_b =>
        {
            *a += b;
            if *a == 0 {
                instrs.pop();
            }
        }
        (_, instr) => instrs.push(instr),
    }
}

pub(crate) fn optimize_instrs<I>(instrs: I, maximal: bool, asm: &Assembly) -> EcoVec<Instr>
where
    I: IntoIterator<Item = Instr> + fmt::Debug,
    I::IntoIter: ExactSizeIterator,
{
    // println!("optimize {:?}", instrs);
    let instrs = instrs.into_iter();
    let mut new = EcoVec::with_capacity(instrs.len());
    for instr in instrs {
        if maximal && instr.is_compile_only() {
            continue;
        }
        optimize_instrs_mut(&mut new, instr, maximal, asm);
    }
    // println!("to       {:?}", new);
    new
}
