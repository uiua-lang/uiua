use super::*;

use crate::{ImplPrimitive::*, Node::*, Primitive::*};

pub(crate) const DEBUG: bool = false;

macro_rules! dbgln {
    ($($arg:tt)*) => {
        if DEBUG {
            println!($($arg)*); // Allow println
        }
    }
}
use dbgln;

impl Node {
    pub(super) fn optimize(&mut self) {
        dbgln!("optimizing {self:?}");
        match self {
            Run(nodes) => {
                for node in nodes.make_mut() {
                    node.optimize();
                }
                while OPTIMIZATIONS.iter().any(|op| {
                    if !op.match_and_replace(nodes) {
                        return false;
                    }
                    dbgln!("applied optimization {op:?}");
                    true
                }) {}
                if nodes.len() == 1 {
                    *self = take(nodes).remove(0);
                }
            }
            Mod(_, args, _) | ImplMod(_, args, _) => {
                for arg in args.make_mut() {
                    arg.node.optimize();
                }
            }
            CustomInverse(cust, _) => {
                let cust = Arc::make_mut(cust);
                if let Ok(normal) = cust.normal.as_mut() {
                    normal.node.optimize();
                }
                if let Some(un) = cust.un.as_mut() {
                    un.node.optimize();
                }
                if let Some(anti) = cust.anti.as_mut() {
                    anti.node.optimize();
                }
                if let Some((before, after)) = cust.under.as_mut() {
                    before.node.optimize();
                    after.node.optimize();
                }
            }
            _ => {}
        }
    }
}

static OPTIMIZATIONS: &[&dyn Optimization] = &[
    &((Reverse, First), Last),
    &((Reverse, Last), First),
    &((Rise, First), FirstMinIndex),
    &((Fall, Last), LastMinIndex),
    &((Fall, First), FirstMaxIndex),
    &((Rise, Last), LastMaxIndex),
    &((Where, First), FirstWhere),
    &((Where, Last), LastWhere),
    &((Where, Len), LenWhere),
    &((Range, MemberOf), MemberOfRange),
    &((Range, 1, Rerank, MemberOf), MultidimMemberOfRange),
    &((Range, DeshapeSub(2), MemberOf), MultidimMemberOfRange),
    &((UnSort, Or(First, Last)), RandomRow),
    &((Deduplicate, Len), CountUnique),
    &((Or((Dup, Rise), M(By, Rise)), Select), Sort),
    &((Or((Dup, Fall), M(By, Fall)), Select), SortDown),
    &((Sort, Reverse), SortDown),
    &((SortDown, Reverse), Sort),
    &((Pop, Rand), ReplaceRand),
    &((Pop, Pop, Rand), ReplaceRand2),
    &((1, Flip, Div, Pow), Root),
    &InlineCustomInverse,
    &TransposeOpt,
    &ReduceTableOpt,
    &ReduceDepthOpt,
    &AdjacentOpt,
    &AstarOpt,
    &SplitByOpt,
    &PopConst,
    &TraceOpt,
    &ValidateTypeOpt,
];

opt!(
    InlineCustomInverse,
    [CustomInverse(cust, _)](cust.normal.is_ok()),
    cust.normal.clone().unwrap().node
);

opt!(PopConst, [Push(_), Prim(Pop, _)], []);

opt!(
    TransposeOpt,
    (
        [Prim(Transpose, span), Prim(Transpose, _)],
        ImplPrim(TransposeN(2), *span)
    ),
    (
        [ImplPrim(TransposeN(n), span), Prim(Transpose, _)],
        ImplPrim(TransposeN(n + 1), *span)
    ),
    (
        [ImplPrim(TransposeN(a), span), ImplPrim(TransposeN(b), _)],
        ImplPrim(TransposeN(a + b), *span)
    ),
);

opt!(
    ReduceTableOpt,
    [Mod(Table, table_args, span), Mod(Reduce, reduce_args, _)](
        table_args[0].sig == (2, 1) && reduce_args[0].sig == (2, 1)
    ),
    ImplMod(
        ReduceTable,
        (reduce_args.iter().cloned())
            .chain(table_args.iter().cloned())
            .collect(),
        *span
    )
);

opt!(
    ValidateTypeOpt,
    (
        [
            Prim(Dup, _),
            Prim(Type, _),
            Push(val),
            ImplPrim(MatchPattern, span)
        ],
        [Push(val.clone()), ImplPrim(ValidateType, *span)]
    ),
    (
        [Prim(Type, _), Push(val), ImplPrim(MatchPattern, span)],
        [Push(val.clone()), ImplPrim(ValidateTypeConsume, *span)]
    )
);

#[derive(Debug)]
struct AdjacentOpt;
impl Optimization for AdjacentOpt {
    fn match_and_replace(&self, nodes: &mut EcoVec<Node>) -> bool {
        for i in 0..nodes.len() {
            let [Prim(Windows, _), Mod(Rows, args, span), ..] = &nodes[i..] else {
                continue;
            };
            let [f] = args.as_slice() else {
                continue;
            };
            match f.node.as_slice() {
                [Mod(Reduce, reduce_args, span)] if reduce_args[0].sig == (2, 1) => {
                    let impl_mod = ImplMod(ImplPrimitive::Adjacent, reduce_args.clone(), *span);
                    replace_nodes(nodes, i, 2, impl_mod);
                    return true;
                }
                _ if args[0].sig == (1, 1) => {
                    let impl_mod = ImplMod(ImplPrimitive::RowsWindows, args.clone(), *span);
                    replace_nodes(nodes, i, 2, impl_mod);
                    return true;
                }
                _ => {}
            }
        }
        false
    }
}

#[derive(Debug)]
struct ReduceDepthOpt;
impl Optimization for ReduceDepthOpt {
    fn match_and_replace(&self, nodes: &mut EcoVec<Node>) -> bool {
        for i in 0..nodes.len() {
            let [Mod(Rows, args, _), ..] = &nodes[i..] else {
                continue;
            };
            let [f] = args.as_slice() else {
                continue;
            };
            match f.node.as_slice() {
                [Mod(Reduce, reduce_args, span)] => {
                    let impl_mod = ImplMod(ReduceDepth(1), reduce_args.clone(), *span);
                    replace_nodes(nodes, i, 1, impl_mod);
                    return true;
                }
                [ImplMod(ReduceDepth(depth), reduce_args, span)] => {
                    let impl_mod = ImplMod(ReduceDepth(depth + 1), reduce_args.clone(), *span);
                    replace_nodes(nodes, i, 1, impl_mod);
                    return true;
                }
                _ => {}
            }
        }
        false
    }
}

opt!(
    AstarOpt,
    (
        [Mod(Astar, args, span), Prim(First, _)],
        ImplMod(AstarFirst, args.clone(), *span)
    ),
    (
        [Mod(Astar, args, span), Prim(Pop, _)],
        ImplMod(AstarPop, args.clone(), *span)
    ),
);

#[derive(Debug)]
struct SplitByOpt;
impl Optimization for SplitByOpt {
    fn match_and_replace(&self, nodes: &mut EcoVec<Node>) -> bool {
        fn par_f(node: &Node) -> Option<SigNode> {
            let Mod(Partition, args, _) = node else {
                return None;
            };
            let [f] = args.as_slice() else {
                return None;
            };
            Some(f.clone())
        }
        for i in 0..nodes.len() {
            match &nodes[i..] {
                [Mod(By, args, span), last, ..]
                    if matches!(args.as_slice(), [f]
                            if matches!(f.node, Prim(Ne, _))) =>
                {
                    let Some(f) = par_f(last) else {
                        continue;
                    };
                    replace_nodes(nodes, i, 2, ImplMod(SplitByScalar, eco_vec![f], *span));
                    break;
                }
                [Mod(By, args, span), Prim(Not, _), last, ..]
                    if matches!(args.as_slice(), [f]
                            if matches!(f.node, Prim(Mask, _))) =>
                {
                    let Some(f) = par_f(last) else {
                        continue;
                    };
                    replace_nodes(nodes, i, 3, ImplMod(SplitBy, eco_vec![f], *span));
                    break;
                }
                [Prim(Dup, span), Push(delim), Prim(Ne, _), last, ..] => {
                    let Some(f) = par_f(last) else {
                        continue;
                    };
                    let new = Node::from_iter([
                        Push(delim.clone()),
                        ImplMod(SplitByScalar, eco_vec![f], *span),
                    ]);
                    replace_nodes(nodes, i, 4, new);
                    break;
                }
                [Prim(Dup, span), Push(delim), Prim(Mask, _), Prim(Not, _), last, ..] => {
                    let Some(f) = par_f(last) else {
                        continue;
                    };
                    let new = Node::from_iter([
                        Push(delim.clone()),
                        ImplMod(SplitBy, eco_vec![f], *span),
                    ]);
                    replace_nodes(nodes, i, 5, new);
                    break;
                }
                _ => {}
            }
        }
        false
    }
}

opt!(
    TraceOpt,
    (
        [Prim(Trace, span), Prim(Trace, _)],
        ImplPrim(
            TraceN {
                n: 2,
                inverse: false,
                stack_sub: false
            },
            *span
        )
    ),
    (
        [
            ImplPrim(
                TraceN {
                    n,
                    inverse: false,
                    ..
                },
                span
            ),
            Prim(Trace, _)
        ],
        ImplPrim(
            TraceN {
                n: n + 1,
                inverse: false,
                stack_sub: false
            },
            *span
        )
    ),
    (
        [
            ImplPrim(
                TraceN {
                    n: a,
                    inverse: inv_a,
                    stack_sub,
                },
                span
            ),
            ImplPrim(
                TraceN {
                    n: b,
                    inverse: inv_b,
                    ..
                },
                _
            )
        ](inv_a == inv_b),
        ImplPrim(
            TraceN {
                n: a + b,
                inverse: *inv_a,
                stack_sub: *stack_sub
            },
            *span
        )
    )
);

trait Optimization: Debug + Sync {
    fn match_and_replace(&self, nodes: &mut EcoVec<Node>) -> bool;
}

impl<A, B> Optimization for (A, B)
where
    A: OptPattern,
    B: OptReplace,
{
    fn match_and_replace(&self, nodes: &mut EcoVec<Node>) -> bool {
        for i in 0..nodes.len() {
            if let Some((n, Some(span))) = self.0.match_nodes(&nodes[i..]) {
                replace_nodes(nodes, i, n, self.1.replacement_node(span));
                return true;
            }
        }
        false
    }
}

trait OptPattern: Debug + Sync {
    fn match_nodes(&self, nodes: &[Node]) -> Option<(usize, Option<usize>)>;
}
impl OptPattern for Primitive {
    fn match_nodes(&self, nodes: &[Node]) -> Option<(usize, Option<usize>)> {
        match nodes {
            [Node::Prim(p, span), ..] if self == p => Some((1, Some(*span))),
            _ => None,
        }
    }
}
impl OptPattern for ImplPrimitive {
    fn match_nodes(&self, nodes: &[Node]) -> Option<(usize, Option<usize>)> {
        match nodes {
            [Node::ImplPrim(p, span), ..] if self == p => Some((1, Some(*span))),
            _ => None,
        }
    }
}
impl OptPattern for i32 {
    fn match_nodes(&self, nodes: &[Node]) -> Option<(usize, Option<usize>)> {
        match nodes {
            [Node::Push(n), ..] if n == self => Some((1, None)),
            _ => None,
        }
    }
}
#[derive(Debug)]
struct Or<A, B>(A, B);
impl<A, B> OptPattern for Or<A, B>
where
    A: OptPattern,
    B: OptPattern,
{
    fn match_nodes(&self, nodes: &[Node]) -> Option<(usize, Option<usize>)> {
        self.0
            .match_nodes(nodes)
            .or_else(|| self.1.match_nodes(nodes))
    }
}

#[derive(Debug)]
struct M<A, B>(A, B);
impl OptPattern for M<Primitive, Primitive> {
    fn match_nodes(&self, nodes: &[Node]) -> Option<(usize, Option<usize>)> {
        let [Mod(a, args, span), ..] = nodes else {
            return None;
        };
        let [f] = args.as_slice() else {
            return None;
        };
        if *a == self.0 && f.node.as_primitive() == Some(self.1) {
            Some((1, Some(*span)))
        } else {
            None
        }
    }
}

trait OptReplace: Debug + Sync {
    fn replacement_node(&self, span: usize) -> Node;
}
impl OptReplace for Primitive {
    fn replacement_node(&self, span: usize) -> Node {
        Node::Prim(*self, span)
    }
}
impl OptReplace for ImplPrimitive {
    fn replacement_node(&self, span: usize) -> Node {
        Node::ImplPrim(*self, span)
    }
}

fn replace_nodes(nodes: &mut EcoVec<Node>, i: usize, n: usize, new: Node) {
    // dbg!(&nodes, i, n, &new);
    let orig_len = nodes.len();
    debug_assert!(orig_len - n >= i);
    nodes.make_mut()[i..].rotate_left(n);
    nodes.truncate(orig_len - n);
    nodes.extend(new);
    let added = nodes.len() - (orig_len - n);
    nodes.make_mut()[i..].rotate_right(added);
}

macro_rules! opt {
    ($name:ident, [$($pat:pat),*] $(($cond:expr))?, $new:expr) => {
        opt!($name, ([$($pat),*] $(($cond))?, $new));
    };
    ($name:ident, $(([$($pat:pat),*] $(($cond:expr))?, $new:expr)),* $(,)?) => {
        #[derive(Debug)]
        struct $name;
        impl Optimization for $name {
            fn match_and_replace(&self, nodes: &mut EcoVec<Node>) -> bool {
                for i in 0..nodes.len() {
                    match &nodes[i..] {
                        $(
                            [$($pat),*, ..] $(if $cond)? => {
                                const N: usize = 0 $(+ {stringify!($pat); 1})*;
                                let new = $new;
                                replace_nodes(nodes, i, N, new.into());
                                return true;
                            }
                        )*
                        _ => {}
                    }
                }
                false
            }
        }
    };
}
use fmt::Debug;
use opt;

macro_rules! opt_pattern {
    ($($T:ident),*) => {
        impl<$($T),*> OptPattern for ($($T,)*)
        where
            $($T: OptPattern,)*
        {
            #[allow(unused, non_snake_case)]
            fn match_nodes(&self, mut nodes: &[Node]) -> Option<(usize, Option<usize>)> {
                let ($($T,)*) = self;
                let mut i = 0;
                let mut span = None;
                $(
                    let (n, sp) = $T.match_nodes(nodes)?;
                    i += n;
                    nodes = &nodes[n..];
                    if sp.is_some() {
                        span = sp;
                    }
                )*
                Some((i, span))
            }
        }
        #[allow(non_snake_case)]
        impl<$($T),*> OptReplace for ($($T,)*)
        where
            $($T: OptReplace,)*
        {
            fn replacement_node(&self, span: usize) -> Node {
                let ($($T,)*) = self;
                Node::from_iter([
                    $($T.replacement_node(span),)*
                ])
            }
        }
    }
}

opt_pattern!(A, B);
opt_pattern!(A, B, C);
opt_pattern!(A, B, C, D);
