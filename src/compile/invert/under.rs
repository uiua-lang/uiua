use super::*;

impl Node {
    /// Get both parts of this node's under inverse
    pub fn under_inverse(
        &self,
        g_sig: Signature,
        inverse: bool,
        asm: &Assembly,
    ) -> InversionResult<(Node, Node)> {
        dbgln!("under-inverting {self:?}");
        under_inverse(self.as_slice(), g_sig, inverse, asm)
    }
}

impl SigNode {
    /// Get both parts of this node's under inverse
    pub fn under_inverse(
        &self,
        g_sig: Signature,
        inverse: bool,
        asm: &Assembly,
    ) -> InversionResult<(SigNode, SigNode)> {
        let (before, after) = self.node.under_inverse(g_sig, inverse, asm)?;
        let (before, after) = (before.sig_node()?, after.sig_node()?);
        Ok((before, after))
    }
}

fn under_inverse(
    input: &[Node],
    g_sig: Signature,
    inverse: bool,
    asm: &Assembly,
) -> InversionResult<(Node, Node)> {
    if input.is_empty() {
        return Ok((Node::empty(), Node::empty()));
    }

    type Key = (u64, Signature, bool);
    thread_local! {
        static CACHE: RefCell<HashMap<Key, InversionResult<(Node, Node)>>> = Default::default();
    }
    let mut hasher = DefaultHasher::new();
    for node in input {
        node.hash_with_span(&mut hasher);
    }
    let hash = hasher.finish();
    if let Some(cached) =
        CACHE.with(|cache| cache.borrow_mut().get(&(hash, g_sig, inverse)).cloned())
    {
        return cached;
    }
    let res = under_inverse_impl(input, g_sig, inverse, asm);
    CACHE.with(|cache| {
        cache
            .borrow_mut()
            .insert((hash, g_sig, inverse), res.clone())
    });
    res
}

fn under_inverse_impl(
    input: &[Node],
    g_sig: Signature,
    inverse: bool,
    asm: &Assembly,
) -> InversionResult<(Node, Node)> {
    if input.is_empty() {
        return Ok((Node::empty(), Node::empty()));
    }
    let mut before = Node::empty();
    let mut after = Node::empty();
    let mut curr = input;
    let mut error = Generic;
    'find_pattern: loop {
        for pattern in UNDER_PATTERNS {
            match pattern.under_extract(curr, g_sig, inverse, asm) {
                Ok((new, bef, aft)) => {
                    dbgln!(
                        "matched pattern {pattern:?}\n  on {curr:?}\n  to  {bef:?}\n  and {aft:?}"
                    );
                    after.prepend(aft);
                    before.push(bef);
                    if new.is_empty() {
                        dbgln!("under-inverted\n  {input:?}\n  to  {before:?}\n  and {after:?}");
                        return Ok((before, after));
                    }
                    curr = new;
                    continue 'find_pattern;
                }
                Err(e) => error = error.max(e),
            }
        }
        break;
    }
    Err(error)
}

static UNDER_PATTERNS: &[&dyn UnderPattern] = &[
    &CustomPat,
    &OnPat,
    &ForkPat,
    &BracketPat,
    &BothPat,
    &Trivial,
    &SwitchPat,
    &PartitionPat,
    &GroupPat,
    &EachPat,
    &RowsPat,
    &RepeatPat,
    &FoldPat,
    &ReversePat,
    &TransposePat,
    &RotatePat,
    &AtanPat,
    &ArrayPat,
    &UnpackPat,
    &FillPat,
    &DupPat,
    &FixMatchRanksPat,
    // Sign ops
    &(Abs, (CopyUnd(1), Abs), (PopUnd(1), Sign, Mul)),
    &(Sign, (CopyUnd(1), Sign), (PopUnd(1), Flip, SetSign)),
    // Mod
    &MaybeVal((
        Modulo,
        (Over, Over, Flip, Over, Div, Floor, Mul, PushUnd(1), Modulo),
        (PopUnd(1), Add),
    )),
    // Array restructuring
    &Stash(2, Take, UndoTake),
    &Stash(2, Drop, UndoDrop),
    &MaybeVal((
        Keep,
        (CopyUnd(2), Keep),
        (PopUnd(1), Flip, PopUnd(1), UndoKeep),
    )),
    // Rise and fall
    &(
        Rise,
        (CopyUnd(1), Rise, Dup, Rise, PushUnd(1)),
        (PopUnd(1), Select, PopUnd(1), Flip, Select),
    ),
    &(
        Fall,
        (CopyUnd(1), Fall, Dup, Rise, PushUnd(1)),
        (PopUnd(1), Select, PopUnd(1), Flip, Select),
    ),
    // Sort
    &(
        Sort,
        (Dup, Rise, CopyUnd(1), Select),
        (PopUnd(1), Rise, Select),
    ),
    &(
        SortDown,
        (Dup, Fall, CopyUnd(1), Select),
        (PopUnd(1), Rise, Select),
    ),
    // Pop
    &(Pop, PushUnd(1), PopUnd(1)),
    // Value retrieval
    &Stash(1, First, UndoFirst),
    &Stash(1, Last, UndoLast),
    &Stash(2, Pick, UndoPick),
    &Stash(2, Select, UndoSelect),
    &Stash(2, AntiOrient, UndoAntiOrient),
    &(
        Regex,
        (
            Over,
            Flip,
            DoRegex,
            PushUnd(1),
            CopyUnd(1),
            Flip,
            PushUnd(1),
        ),
        (PopUnd(3), UndoRegex),
    ),
    // Map control
    &MaybeVal((
        Get,
        (CopyUnd(2), Get),
        (PopUnd(1), Flip, PopUnd(1), UndoGet),
    )),
    &Stash(2, Remove, UndoRemove),
    &MaybeVal((Insert, (CopyUnd(3), Insert), (PopUnd(3), UndoInsert))),
    // Shaping
    &(Fix, (Fix), (UndoFix)),
    &(UndoFix, (UndoFix), (Fix)),
    &Stash(1, (Shape, Len), (Flip, 1, Sub, Rerank)),
    &Stash(1, Shape, UndoShape),
    &(
        Len,
        (CopyUnd(1), Dup, Shape, PushUnd(1), Len),
        (PopUnd(1), UndoFirst, PopUnd(1), Flip, Reshape),
    ),
    &(
        Deshape,
        (Dup, Shape, PushUnd(1), Deshape),
        (PopUnd(1), UndoDeshape(None)),
    ),
    &DeshapeSubPat,
    &ReduceJoinPat,
    &JoinPat,
    &MaybeVal((
        Rerank,
        (Over, Shape, Over, PushUnd(2), Rerank),
        (PopUnd(2), UndoRerank),
    )),
    &MaybeVal((
        Reshape,
        (Over, Shape, PushUnd(1), Reshape),
        (PopUnd(1), UndoReshape),
    )),
    &MaybeVal((Windows, (CopyUnd(1), Windows), (PopUnd(1), UndoWindows))),
    &MaybeVal(StencilPat),
    &MaybeVal(SidedStencilPat),
    // Classify and deduplicate
    &(
        Classify,
        (Dup, Deduplicate, PushUnd(1), Classify),
        (PopUnd(1), Flip, Select),
    ),
    &(
        Deduplicate,
        (Dup, Classify, PushUnd(1), Deduplicate),
        (PopUnd(1), Select),
    ),
    // Where and un bits
    &(
        Where,
        (Dup, Shape, PushUnd(1), Where),
        (PopUnd(1), UndoWhere),
    ),
    &(
        UnBits,
        (Dup, Shape, PushUnd(1), UnBits),
        (PopUnd(1), UndoUnBits),
    ),
    // Rounding
    &(
        Floor,
        (Dup, Floor, Flip, Over, Sub, PushUnd(1)),
        (PopUnd(1), Add),
    ),
    &(
        Ceil,
        (Dup, Ceil, Flip, Over, Sub, PushUnd(1)),
        (PopUnd(1), Add),
    ),
    &(
        Round,
        (Dup, Round, Flip, Over, Sub, PushUnd(1)),
        (PopUnd(1), Add),
    ),
    // System stuff
    &(Now, (Now, PushUnd(1)), (Now, PopUnd(1), Sub)),
    &MaybeVal(Store1Copy(Sys(SysOp::FOpen), Sys(SysOp::Close))),
    &MaybeVal(Store1Copy(Sys(SysOp::FCreate), Sys(SysOp::Close))),
    &MaybeVal((
        Sys(SysOp::ChangeDirectory),
        (
            ImplPrimitive::UnChangeDirectory,
            PushUnd(1),
            Sys(SysOp::ChangeDirectory),
        ),
        (PopUnd(1), Sys(SysOp::ChangeDirectory)),
    )),
    &MaybeVal(Store1Copy(Sys(SysOp::TcpConnect), Sys(SysOp::Close))),
    &MaybeVal(Store1Copy(Sys(SysOp::TlsConnect), Sys(SysOp::Close))),
    &MaybeVal(Store1Copy(Sys(SysOp::TcpAccept), Sys(SysOp::Close))),
    &MaybeVal(Store1Copy(Sys(SysOp::TcpListen), Sys(SysOp::Close))),
    &MaybeVal(Store1Copy(Sys(SysOp::TlsListen), Sys(SysOp::Close))),
    &MaybeVal(Stash(1, Sys(SysOp::FReadAllStr), Sys(SysOp::FWriteAll))),
    &MaybeVal(Stash(1, Sys(SysOp::FReadAllBytes), Sys(SysOp::FWriteAll))),
    &MaybeVal((
        Sys(SysOp::RunStream),
        (Sys(SysOp::RunStream), CopyUnd(3)),
        (PopUnd(3), TryClose, TryClose, TryClose),
    )),
    &MaybeVal((
        Sys(SysOp::RawMode),
        (UnRawMode, PushUnd(1), Sys(SysOp::RawMode)),
        (PopUnd(1), Sys(SysOp::RawMode)),
    )),
    // Patterns that need to be last
    &StashAntiPat,
    &FlipPat,
    &DipPat,
    &StashContraPat,
    &FromUnPat,
    &ConstPat,
];

trait UnderPattern: fmt::Debug + Sync {
    fn under_extract<'a>(
        &self,
        input: &'a [Node],
        g_sig: Signature,
        inverse: bool,
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node, Node)>;
}

macro_rules! under {
    // Optional parens
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), $body:expr) => {
        under!($(#[$attr])* $($doc,)? $($tt)*, $body);
    };
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), ref, $pat:pat, $body:expr) => {
        under!($(#[$attr])* $($doc,)? $($tt)*, ref, $pat, $body);
    };
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), $pat:pat, $body:expr) => {
        under!($(#[$attr])* $($doc,)? $($tt)*, $pat, $body);
    };
    // Main impl
    ($(#[$attr:meta])* $($doc:literal,)? $name:ident, $input:ident, $g_sig:tt, $inverse:tt, $asm:tt, $body:expr) => {
        #[derive(Debug)]
        $(#[$attr])*
        $(#[doc = $doc])?
        struct $name;
        impl UnderPattern for $name {
            fn under_extract<'a>(
                &self,
                $input: &'a [Node],
                $g_sig: Signature,
                $inverse: bool,
                $asm: &Assembly,
            ) -> InversionResult<(&'a [Node], Node, Node)> {
                $body
            }
        }
    };
    // Ref pattern
    ($(#[$attr:meta])* $($doc:literal)? $name:ident, $input:ident, $g_sig:tt, $inverse:tt, $asm:tt, ref, $pat:pat, $body:expr) => {
        under!($([$attr])* $($doc)? $name, $input, $g_sig, $inverse, $asm, {
            let [$pat, $input @ ..] = $input else {
                return generic();
            };
            $body
        });
    };
    // Non-ref pattern
    ($(#[$attr:meta])* $($doc:literal)? $name:ident, $input:ident, $g_sig:tt, $inverse:tt, $asm:tt, $pat:pat, $body:expr) => {
        under!($([$attr])* $($doc)? $name, $input, $g_sig, $inverse, $asm, {
            let &[$pat, ref $input @ ..] = $input else {
                return generic();
            };
            $body
        });
    };
    // Mod pattern
    ($(#[$attr:meta])* $($doc:literal)? $name:ident, $input:ident, $g_sig:tt, $inverse:tt, $asm:tt, $prim:ident, $span:ident, $args:pat, $body:expr) => {
        under!($([$attr])* $($doc)? $name, $input, $g_sig, $inverse, $asm, ref, Mod($prim, args, $span), {
            #[allow(irrefutable_let_patterns)]
            let $args = args.as_slice() else {
                return generic();
            };
            let $span = *$span;
            $body
        });
    };
}

under!(DipPat, input, g_sig, inverse, asm, {
    let (input, n, args, span) = match input {
        [Mod(Dip, args, span), input @ ..] => (input, 1, args, *span),
        [ImplMod(DipN(n), args, span), input @ ..] => (input, *n, args, *span),
        _ => return generic(),
    };
    let m = |sn: SigNode| {
        if n == 1 {
            Mod(Dip, eco_vec![sn], span)
        } else {
            ImplMod(DipN(n), eco_vec![sn], span)
        }
    };
    let [f] = args.as_slice() else {
        return generic();
    };
    if f.sig.args() == 0 {
        return generic();
    }
    // F inverse
    let inner_g_sig = Signature::new(
        g_sig.args().saturating_sub(n),
        if g_sig.args() > g_sig.outputs() {
            g_sig.outputs()
        } else {
            g_sig.outputs().saturating_sub(n)
        },
    );
    let (f_before, f_after) = f.under_inverse(inner_g_sig, inverse, asm)?;
    // Rest inverse
    let (rest_before, rest_after) = under_inverse(input, g_sig, inverse, asm)?;
    let rest_before_sig = rest_before.sig()?;
    let rest_after_sig = rest_after.sig()?;

    let bal_symmetric =
        g_sig.args() + rest_before_sig.args() <= g_sig.outputs() + rest_after_sig.outputs();
    let bal_assymetic = rest_before_sig.args() > 1 && rest_before_sig == rest_after_sig.inverse();
    let balanced = bal_symmetric || bal_assymetic;

    // Make before
    let mut before = if !inverse || balanced {
        m(f_before)
    } else {
        f_before.node
    };
    before.push(rest_before);
    // Make after
    let mut after = rest_after;
    let after_inner = if inverse || balanced {
        m(f_after)
    } else {
        f_after.node
    };
    after.push(after_inner);
    Ok((&[], before, after))
});

under!(BothPat, input, g_sig, inverse, asm, {
    let (input, val) = if let Ok((input, val)) = Val.invert_extract(input, asm) {
        (input, Some(val))
    } else {
        (input, None)
    };
    let (sub, args, span, input) = match input {
        [Mod(Both, args, span), input @ ..] => (Default::default(), args, span, input),
        [ImplMod(BothImpl(sub), args, span), input @ ..] => (*sub, args, span, input),
        _ => return generic(),
    };
    let span = *span;
    let [f] = args.as_slice() else {
        return generic();
    };
    let inner_g_sig = Signature::new(
        g_sig.args().saturating_sub(1),
        g_sig.outputs().saturating_sub(1),
    );
    let (f_before, mut f_after) = f.under_inverse(inner_g_sig, inverse, asm)?;
    let (rest_before, rest_after) = under_inverse(input, g_sig, inverse, asm)?;
    let rest_before_sig = rest_before.sig()?;
    let rest_after_sig = rest_after.sig()?;
    let other_sig = rest_after_sig.compose(g_sig.compose(rest_before_sig));
    let n_reduction = other_sig.args().saturating_sub(other_sig.outputs());
    // Make before
    let mut before = val.unwrap_or_default();
    before.push(if !inverse || n_reduction == 0 {
        ImplMod(BothImpl(sub), eco_vec![f_before], span)
    } else {
        let bef_node = f_before.node;
        let (mut node, val) = if let Ok((nodes, val)) = Val.invert_extract(&bef_node, asm) {
            (nodes.into(), Some(val))
        } else {
            (bef_node, None)
        };
        let to_copy = f_after.sig.under_args() * n_reduction;
        if to_copy > 0 {
            node.prepend(CopyToUnder(to_copy, span));
        }
        if let Some(val) = val {
            node.prepend(val);
        }
        node
    });
    // Make after
    let mut after = if inverse || n_reduction == 0 {
        let sub = Subscript { side: None, ..sub };
        ImplMod(UnBothImpl(sub), eco_vec![f_after], span)
    } else {
        let undo_n = (sub.num.unwrap_or(2) as usize)
            .saturating_sub(n_reduction)
            .max(1);
        let to_discard = f_after.sig.under_args() * n_reduction;
        if undo_n > 1 {
            let num = Some(undo_n as u32);
            let sub = Subscript { num, side: None };
            f_after = ImplMod(UnBothImpl(sub), eco_vec![f_after], span).sig_node()?;
        }
        if to_discard > 0 {
            f_after.node.push(PopUnder(to_discard, span));
            (0..to_discard).for_each(|_| f_after.node.push(Prim(Pop, span)));
        }
        f_after.node
    };
    before.push(rest_before);
    after.prepend(rest_after);
    Ok((&[], before, after))
});

under!(OnPat, input, g_sig, inverse, asm, On, span, [f], {
    // F inverse
    let inner_g_sig = Signature::new(g_sig.args().saturating_sub(1), g_sig.outputs());
    let (f_before, f_after) = f.under_inverse(inner_g_sig, inverse, asm)?;
    // Rest inverse
    let (rest_before, rest_after) = under_inverse(input, g_sig, inverse, asm)?;
    let rest_before_sig = rest_before.sig()?;
    let rest_after_sig = rest_after.sig()?;
    // Make before
    let mut before = Mod(On, eco_vec![f_before], span);
    before.push(rest_before);
    // Make after
    let mut after = rest_after;
    let after_inner = if g_sig.args() == g_sig.outputs() {
        Mod(Dip, eco_vec![f_after], span)
    } else if g_sig.args() + rest_before_sig.args() <= g_sig.outputs() + rest_after_sig.outputs() {
        Mod(On, eco_vec![f_after], span)
    } else {
        f_after.node
    };
    after.push(after_inner);
    Ok((&[], before, after))
});

under!(ForkPat, input, g_sig, inverse, asm, Fork, span, ops, {
    if ops.iter().any(|sn| sn.sig.args() != 1) {
        return Err(InversionError::UnderNonMonadicFork(span));
    }

    let (rest_before, rest_after) = under_inverse(input, g_sig, inverse, asm)?;

    let mut before = CopyToUnder(1, span);
    before.push(Mod(Fork, ops.into(), span));
    before.push(rest_before);

    let mut afters = Vec::with_capacity(ops.len());
    let mut dip = 0;
    for op in ops {
        afters.push(
            Mod(By, eco_vec![op.clone()], span)
                .un_inverse(asm)?
                .sig_node()?
                .dipped(dip, span),
        );
        dip += op.sig.outputs();
    }

    let mut after = rest_after;
    after.push(PopUnder(1, span).sig_node()?.dipped(dip, span).node);
    after.extend(afters.into_iter().rev().map(|sn| sn.node));

    Ok((&[], before, after))
});

under!(
    (BracketPat, input, g_sig, inverse, asm, Bracket, span, args),
    {
        let mut args_before = EcoVec::with_capacity(args.len());
        let mut args_after = EcoVec::with_capacity(args.len());
        for arg in args {
            let (before, after) = arg.under_inverse(g_sig, inverse, asm)?;
            args_before.push(before);
            args_after.push(after);
        }
        let before = Mod(Bracket, args_before, span);
        let after = ImplMod(UnBracket, args_after, span);
        Ok((input, before, after))
    }
);

under!(
    "Derives under inverses from un inverses",
    (FromUnPat, input, _, _, asm),
    {
        for pat in UN_PATTERNS.iter().filter(|pat| pat.allowed_in_under()) {
            if let Ok((inp, inv)) = pat.invert_extract(input, asm) {
                let node = Node::from(&input[..input.len() - inp.len()]);
                dbgln!(
                    "matched un pattern for under {pat:?}\n  on {input:?}\n  to {node:?}\n  and {inv:?}"
                );
                return Ok((inp, node, inv));
            }
        }
        generic()
    }
);

under!(
    "Derives under inverses from anti inverses",
    (StashAntiPat, input, _, _, asm),
    {
        for pat in ANTI_PATTERNS.iter().filter(|pat| pat.allowed_in_under()) {
            if let Ok((new, inv)) = pat.invert_extract(input, asm) {
                let nodes = &input[..input.len() - new.len()];
                let span = nodes
                    .iter()
                    .find_map(Node::span)
                    .or_else(|| inv.span())
                    .unwrap_or(0);
                let before = Node::from_iter([CopyToUnder(1, span), Node::from(nodes)]);
                let after = Node::from_iter([PopUnder(1, span), inv]);
                dbgln!(
                    "matched anti pattern for under {pat:?}\n  on {input:?}\n  to {before:?}\n  and {after:?}"
                );
                return Ok((new, before, after));
            }
        }
        generic()
    }
);

under!(
    "Derives under inverses from contra inverses",
    (StashContraPat, input, _, _, asm),
    {
        for pat in CONTRA_PATTERNS.iter().filter(|pat| pat.allowed_in_under()) {
            if let Ok((new, inv)) = pat.invert_extract(input, asm) {
                let nodes = &input[..input.len() - new.len()];
                let span = nodes
                    .iter()
                    .find_map(Node::span)
                    .or_else(|| inv.span())
                    .unwrap_or(0);
                let before =
                    Node::from_iter([ImplPrim(Over, span), PushUnder(1, span), Node::from(nodes)]);
                let after = Node::from_iter([PopUnder(1, span), Prim(Flip, span), inv]);
                dbgln!(
                    "matched contra pattern for under {pat:?}\n  on {input:?}\n  to {before:?}\n  and {after:?}"
                );
                return Ok((new, before, after));
            }
        }
        generic()
    }
);

under!(EachPat, input, g_sig, inverse, asm, Each, span, [f], {
    let (f_before, f_after) = f.under_inverse(g_sig, inverse, asm)?;
    let befores = Mod(Each, eco_vec![f_before], span);
    let afters = Mod(Each, eco_vec![f_after], span);
    Ok((input, befores, afters))
});

under!(RowsPat, input, g_sig, inverse, asm, {
    let [Mod(prim @ (Rows | Inventory), args, span), input @ ..] = input else {
        return generic();
    };
    let [f] = args.as_slice() else {
        return generic();
    };
    let (f_before, f_after) = f.under_inverse(g_sig, inverse, asm)?;
    let (befores, afters) = if f_before.sig.under() == (0, 0) && f_after.sig.under() == (0, 0) {
        (
            Mod(*prim, eco_vec![f_before], *span),
            Mod(*prim, eco_vec![f_after], *span),
        )
    } else {
        let befores = Node::from_iter([
            ImplPrim(MaxRowCount(f.sig.args()), *span),
            Mod(
                Dip,
                eco_vec![Mod(*prim, eco_vec![f_before], *span).sig_node()?],
                *span,
            ),
            PushUnder(1, *span),
        ]);
        let undo_prim = if matches!(*prim, Rows) {
            UndoRows
        } else {
            UndoInventory
        };
        let afters = Node::from_iter([
            PopUnder(1, *span),
            ImplMod(undo_prim, eco_vec![f_after], *span),
        ]);
        (befores, afters)
    };

    Ok((input, befores, afters))
});

under!(RepeatPat, input, g_sig, inverse, asm, {
    let (input, val) = if let Ok((input, val)) = Val.invert_extract(input, asm) {
        (input, Some(val))
    } else {
        (input, None)
    };
    let (f, span, input) = match input {
        [Mod(Repeat, args, span), input @ ..] => {
            let [f] = args.as_slice() else {
                return generic();
            };
            (f, *span, input)
        }
        [ImplMod(RepeatWithInverse, args, span), input @ ..] => {
            let [f, _] = args.as_slice() else {
                return generic();
            };
            (f, *span, input)
        }
        _ => return generic(),
    };
    let (f_before, f_after) = f.under_inverse(g_sig, inverse, asm)?;
    let mut befores = val.clone().unwrap_or(CopyToUnder(1, span));
    befores.push(Mod(Repeat, eco_vec![f_before], span));
    let mut afters = val.unwrap_or(PopUnder(1, span));
    afters.push(Mod(Repeat, eco_vec![f_after], span));
    Ok((input, befores, afters))
});

under!(FoldPat, input, g_sig, inverse, asm, Fold, span, [f], {
    let (f_before, f_after) = f.under_inverse(g_sig, inverse, asm)?;
    if f_before.sig.outputs() > f_before.sig.args() || f_after.sig.outputs() > f_after.sig.args() {
        return generic();
    }
    let before = Node::from_iter([
        Prim(Dup, span),
        Prim(Len, span),
        Mod(
            Dip,
            eco_vec![Mod(Fold, eco_vec![f_before], span).sig_node()?],
            span,
        ),
        PushUnder(1, span),
    ]);
    let after = Node::from_iter([PopUnder(1, span), Mod(Repeat, eco_vec![f_after], span)]);
    Ok((input, before, after))
});

under!(CustomPat, input, g_sig, inverse, asm, {
    let (input, val) = Val.try_invert_extract(input, asm);
    let [CustomInverse(cust, span), input @ ..] = input else {
        return generic();
    };
    let normal = cust.normal.clone()?;
    let (mut before, mut after, to_save) = if let Some((before, after)) = cust.under.clone() {
        // An under inverse is explicitly defined
        if before.sig.outputs() < normal.sig.outputs() {
            return generic();
        }
        let to_save = before.sig.outputs() - normal.sig.outputs();
        (before.node, after.node, to_save)
    } else if !cust.is_obverse {
        // The custom inverses were not created with obverse
        // This means that a supplied un inverse can be overridden
        match normal.node.under_inverse(g_sig, inverse, asm) {
            Ok((before, after)) => (before, after, 0),
            Err(e) => {
                if let Some(un) = cust.un.as_ref().filter(|un| un.sig == normal.sig) {
                    (normal.node.clone(), un.node.clone(), 0)
                } else {
                    return Err(e);
                }
            }
        }
    } else if let Some(un) = cust.un.as_ref().filter(|un| un.sig == normal.sig.inverse()) {
        // A compatible un inverse is defined
        (normal.node.clone(), un.node.clone(), 0)
    } else if let Ok((before, after)) = normal.node.under_inverse(g_sig, inverse, asm) {
        // An under inverse exists for the normal
        (before, after, 0)
    } else if let Some(anti) = cust.anti.as_ref() {
        // An anti inverse is defined
        let to_save = anti.sig.args() - normal.sig.outputs();
        let before = Mod(On, eco_vec![normal.clone()], *span);
        let after = anti.node.clone();
        (before, after, to_save)
    } else {
        return generic();
    };
    // Handle preceding value
    if before.sig()?.args() == 2 {
        if let Ok(val) = val {
            before.prepend(val);
        }
    } else if val.is_ok() {
        return generic();
    }
    // Add context save and recall
    if to_save > 0 {
        before.push(PushUnder(to_save, *span));
        after.prepend(PopUnder(to_save, *span));
    }
    Ok((input, before, after))
});

under!(DupPat, input, g_sig, inverse, asm, Prim(Dup, dup_span), {
    let dyadic_i = (0..=input.len())
        .find(|&i| nodes_clean_sig(&input[..i]).is_some_and(|sig| sig == (2, 1)))
        .ok_or(Generic)?;
    let dyadic_whole = &input[..dyadic_i];
    let input = &input[dyadic_i..];
    let (monadic_i, monadic_sig) = (0..=dyadic_whole.len())
        .rev()
        .filter_map(|i| nodes_clean_sig(&dyadic_whole[..i]).map(|sig| (i, sig)))
        .find(|(_, sig)| sig.args() == sig.outputs())
        .ok_or(Generic)?;
    let monadic_part = &dyadic_whole[..monadic_i];
    let dyadic_part = &dyadic_whole[monadic_i..];
    dbgln!("under monadic part: {monadic_part:?}");
    dbgln!("under dyadic part: {dyadic_part:?}");
    let (monadic_before, monadic_after) = under_inverse(monadic_part, g_sig, inverse, asm)?;

    let mut before = Prim(Dup, dup_span);
    let mut after = Node::empty();

    let temp = |temp: bool| {
        if temp {
            before.push(CopyToUnder(1, dup_span));
        }
        before.extend(monadic_before);
        before.extend(dyadic_part.iter().cloned());

        if temp {
            after.push(PopUnder(1, dup_span));
        }
    };

    match dyadic_part {
        [Prim(Add, span)] if monadic_sig == (0, 0) => {
            temp(false);
            after.push(Node::new_push(2));
            after.push(Prim(Div, *span));
        }
        [Prim(Add, span)] => {
            temp(true);
            after.push(Prim(Sub, *span));
        }
        [Prim(Sub, span)] => {
            temp(true);
            after.push(Prim(Add, *span));
        }
        [Prim(Mul, span)] if monadic_sig == (0, 0) => {
            temp(false);
            after.push(Prim(Sqrt, *span));
        }
        [Prim(Mul, span)] => {
            temp(true);
            after.push(Prim(Div, *span));
        }
        [Prim(Div, span)] => {
            temp(true);
            after.push(Prim(Mul, *span));
        }
        _ => return generic(),
    }
    after.push(monadic_after);
    Ok((input, before, after))
});

under!(
    (DeshapeSubPat, input, g_sig, inverse, asm),
    ImplPrim(DeshapeSub(i), span),
    Ok(if i == 0 {
        let (before, after) = under_inverse(
            &[Prim(Deshape, span), Prim(First, span)],
            g_sig,
            inverse,
            asm,
        )?;
        (input, before, after)
    } else {
        let before = Node::from_iter([
            Prim(Dup, span),
            Prim(Shape, span),
            PushUnder(1, span),
            ImplPrim(DeshapeSub(i), span),
        ]);
        let after = Node::from_iter([PopUnder(1, span), ImplPrim(UndoDeshape(Some(i)), span)]);
        (input, before, after)
    })
);

under!(ReduceJoinPat, input, _, _, _, Reduce, span, [f], {
    let Node::Prim(Join, _) = f.node else {
        return generic();
    };
    let before = Node::from_iter([
        Prim(Dup, span),
        Prim(Shape, span),
        PushUnder(1, span),
        Mod(Reduce, eco_vec![f.clone()], span),
    ]);
    let after = Node::from_iter([PopUnder(1, span), ImplPrim(UndoDeshape(Some(-1)), span)]);
    Ok((input, before, after))
});

under!(JoinPat, input, g_sig, inverse, asm, {
    let (input, val) = if let Ok((input, val)) = Val.invert_extract(input, asm) {
        (input, Some(val))
    } else {
        (input, None)
    };
    let (input, mut before, mut after, span) = match *input {
        [Prim(Flip, flip_span), ref input @ ..] => 'blk: {
            for (i, node) in input.iter().enumerate() {
                let &Prim(Join, span) = node else {
                    continue;
                };
                let between = &input[..i];
                if !nodes_clean_sig(between).is_some_and(|sig| sig == (0, 0) || sig == (1, 1)) {
                    continue;
                }
                let (betw_before, betw_after) = under_inverse(between, g_sig, inverse, asm)?;
                let before = Node::from_iter([
                    Prim(Dup, span),
                    Prim(Shape, span),
                    PushUnder(1, span),
                    ImplPrim(Over, span),
                    Prim(Shape, span),
                    PushUnder(1, span),
                    Prim(Flip, flip_span),
                    betw_before,
                    Prim(Join, span),
                ]);
                let after = Node::from_iter([
                    PopUnder(2, span),
                    ImplPrim(UnJoinShape2End, span),
                    betw_after,
                ]);
                break 'blk (&input[i + 1..], before, after, span);
            }
            return generic();
        }
        [Prim(Join, span), ref input @ ..] => {
            let before = Node::from_iter([
                Prim(Dup, span),
                Prim(Shape, span),
                PushUnder(1, span),
                ImplPrim(Over, span),
                Prim(Shape, span),
                PushUnder(1, span),
                Prim(Join, span),
            ]);
            let after = Node::from_iter([PopUnder(2, span), ImplPrim(UnJoinShape2, span)]);
            (input, before, after, span)
        }
        _ => return generic(),
    };
    if let Some(val) = val {
        before.prepend(val);
        after.push(Prim(Pop, span));
    }
    Ok((input, before, after))
});

under!(StencilPat, input, _, _, _, Stencil, span, [f], {
    if !matches!(f.node, Prim(Identity, _)) {
        return generic();
    }
    let before = Node::from_iter([
        CopyToUnder(1, span),
        Mod(Stencil, eco_vec![f.clone()], span),
    ]);
    let after = Node::from_iter([PopUnder(1, span), ImplPrim(UndoWindows, span)]);
    Ok((input, before, after))
});

under!(
    (SidedStencilPat, input, _, _, _),
    ref,
    ImplMod(SidedStencil(side), args, span),
    {
        let [f] = args.as_slice() else {
            return generic();
        };
        if !matches!(f.node, Prim(Identity, _)) {
            return generic();
        }
        let before = Node::from_iter([
            CopyToUnder(1, *span),
            ImplMod(SidedStencil(*side), eco_vec![f.clone()], *span),
        ]);
        let after = Node::from_iter([PopUnder(1, *span), ImplPrim(UndoWindows, *span)]);
        Ok((input, before, after))
    }
);

#[derive(Debug)]
struct Trivial;
impl UnderPattern for Trivial {
    fn under_extract<'a>(
        &self,
        input: &'a [Node],
        g_sig: Signature,
        inverse: bool,
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node, Node)> {
        match input {
            [NoInline(inner), input @ ..] => {
                let (before, after) = inner.under_inverse(g_sig, inverse, asm)?;
                Ok((input, NoInline(before.into()), NoInline(after.into())))
            }
            [TrackCaller(inner), input @ ..] => {
                let (before, after) = inner.under_inverse(g_sig, inverse, asm)?;
                Ok((input, TrackCaller(before.into()), TrackCaller(after.into())))
            }
            [node @ SetOutputComment { .. }, input @ ..] => {
                Ok((input, node.clone(), Node::empty()))
            }
            [Call(f, _), input @ ..] => {
                let (before, after) = asm[f]
                    .under_inverse(g_sig, inverse, asm)
                    .map_err(|e| e.func(f))?;
                Ok((input, before, after))
            }
            _ => generic(),
        }
    }
}

under!(
    (SwitchPat, input, g_sig, inverse, asm),
    ref,
    Node::Switch {
        branches,
        sig,
        span,
        under_cond: false
    },
    {
        let mut befores = EcoVec::with_capacity(branches.len());
        let mut afters = EcoVec::with_capacity(branches.len());
        let mut do_sig = *sig;
        let mut undo_sig: Option<Signature> = None;
        for branch in branches {
            // Calc under f
            let (before, after) = branch.under_inverse(g_sig, inverse, asm)?;
            let before_sig = before.sig;
            let after_sig = after.sig;
            befores.push(before);
            afters.push(after);
            // Aggregate sigs
            let undo_sig = undo_sig.get_or_insert(after_sig);
            if after_sig.is_compatible_with(*undo_sig) {
                do_sig = do_sig.max_with(before_sig);
                *undo_sig = undo_sig.max_with(after_sig);
            } else if after_sig.outputs() == undo_sig.outputs() {
                do_sig.update_under_outputs(|a| a.max(before_sig.under_outputs()));
                undo_sig.update_args(|a| a.max(after_sig.args()));
                undo_sig.update_under_args(|a| a.max(after_sig.under_args()));
            } else {
                return generic();
            }
        }
        let before = Node::Switch {
            branches: befores,
            sig: do_sig,
            span: *span,
            under_cond: true,
        };
        let after = Node::from_iter([
            Node::PopUnder(1, *span),
            Node::Switch {
                branches: afters,
                sig: undo_sig.ok_or(Generic)?,
                span: *span,
                under_cond: false,
            },
        ]);
        Ok((input, before, after))
    }
);

macro_rules! partition_group {
    ($name:ident, $prim:ident, $impl_prim1:ident, $impl_prim2:ident) => {
        under!($name, input, g_sig, inverse, asm, $prim, span, [f], {
            let (f_before, f_after) = f.under_inverse(g_sig, inverse, asm)?;
            let before =
                Node::from_iter([CopyToUnder(2, span), Mod($prim, eco_vec![f_before], span)]);
            let after = Node::from_iter([
                ImplMod($impl_prim1, eco_vec![f_after], span),
                Mod(Dip, eco_vec![PopUnder(2, span).sig_node()?], span),
                ImplPrim(ImplPrimitive::$impl_prim2, span),
            ]);
            Ok((input, before, after))
        });
    };
}

partition_group!(PartitionPat, Partition, UndoPartition1, UndoPartition2);
partition_group!(GroupPat, Group, UndoGroup1, UndoGroup2);

under!(ReversePat, input, g_sig, _, _, Prim(Reverse, span), {
    if g_sig.outputs() == 1 {
        return generic();
    }
    let count = if g_sig.args() == 1 || g_sig.outputs() == g_sig.args() * 2 {
        g_sig.outputs().max(1)
    } else {
        1
    };
    let after = ImplPrim(
        UndoReverse {
            n: count,
            all: false,
        },
        span,
    );
    Ok((input, Prim(Reverse, span), after))
});

under!(TransposePat, input, g_sig, _, _, {
    if g_sig.outputs() == 1 {
        return generic();
    }
    let (before, span, amnt, input) = match input {
        [node @ Prim(Transpose, span), input @ ..] => (node, *span, 1, input),
        [node @ ImplPrim(TransposeN(amnt), span), input @ ..] => (node, *span, *amnt, input),
        _ => return generic(),
    };
    let count = if g_sig.args() == 1 || g_sig.outputs() == g_sig.args() * 2 {
        g_sig.outputs().max(1)
    } else {
        1
    };
    let after = ImplPrim(UndoTransposeN(count, amnt), span);
    Ok((input, before.clone(), after))
});

under!(RotatePat, input, g_sig, _, _, Prim(Rotate, span), {
    let count = if g_sig.args() == 1 || g_sig.outputs() == g_sig.args() * 2 {
        g_sig.outputs().max(1)
    } else {
        1
    };
    let before = Node::from_iter([CopyToUnder(1, span), Prim(Rotate, span)]);
    let after = Node::from_iter([PopUnder(1, span), ImplPrim(UndoRotate(count), span)]);
    Ok((input, before, after))
});

under!(AtanPat, input, _, _, _, Prim(Atan, span), {
    let before = Node::from_iter([
        Mod(
            Fork,
            eco_vec![
                ImplPrim(AbsComplex, span).sig_node()?,
                Prim(Atan, span).sig_node()?
            ],
            span,
        ),
        PushUnder(1, span),
    ]);
    let after = Node::from_iter([
        ImplPrim(UnAtan, span),
        PopUnder(1, span),
        Prim(Flip, span),
        ImplPrim(Over, span),
        Mod(Both, eco_vec![Prim(Mul, span).sig_node()?], span),
    ]);
    Ok((input, before, after))
});

under!(
    (ArrayPat, input, g_sig, inverse, asm),
    ref,
    Array {
        len,
        inner,
        boxed,
        allow_ext,
        prim,
        span
    },
    {
        let (len, boxed, allow_ext, prim, span) = (*len, *boxed, *allow_ext, *prim, *span);
        let (inner_before, inner_after) = inner.under_inverse(g_sig, inverse, asm)?;
        let before = Array {
            len,
            inner: inner_before.into(),
            boxed,
            allow_ext,
            prim,
            span,
        };
        let after = Node::from([
            Unpack {
                count: len,
                unbox: boxed,
                allow_ext,
                prim,
                span,
            },
            inner_after,
        ]);
        Ok((input, before, after))
    }
);

under!(
    (UnpackPat, input, g_sig, inverse, asm),
    Unpack {
        count,
        unbox,
        allow_ext,
        prim,
        span,
    },
    {
        let (mut before, mut after) = under_inverse(input, g_sig, inverse, asm)?;
        after.push(Array {
            len: count,
            inner: Node::empty().into(),
            boxed: unbox,
            allow_ext,
            prim,
            span,
        });
        before.prepend(Unpack {
            count,
            unbox,
            allow_ext,
            prim,
            span,
        });
        Ok((&[], before, after))
    }
);

under!(
    (FillPat, input, g_sig, inverse, asm, Fill, span, [fill, f]),
    {
        if fill.sig != (0, 1) {
            return generic();
        }
        let (f_before, f_after) = f.under_inverse(g_sig, inverse, asm)?;
        let before = Mod(Fill, eco_vec![fill.clone(), f_before], span);
        let after = ImplMod(UnFill, eco_vec![fill.clone(), f_after], span);
        Ok((input, before, after))
    }
);

under!(FlipPat, input, g_sig, inverse, asm, Prim(Flip, span), {
    let (rest_before, rest_after) = under_inverse(input, g_sig, inverse, asm)?;
    let rest_before_sig = nodes_sig(&rest_before)?;
    let rest_after_sig = nodes_sig(&rest_after)?;
    let total_args = g_sig.args() + rest_before_sig.args() + rest_after_sig.args();
    let total_outputs = g_sig.outputs() + rest_before_sig.outputs() + rest_after_sig.outputs();
    let before = Prim(Flip, span);
    let after = if total_outputs < total_args {
        Node::empty()
    } else {
        before.clone()
    };
    Ok((input, before, after))
});

under!(
    (FixMatchRanksPat, input, g_sig, inverse, asm),
    ref,
    ImplMod(FixMatchRanks, args, span),
    {
        let [f] = args.as_slice() else {
            return generic();
        };
        let (before, after) = f.under_inverse(g_sig, inverse, asm)?;
        Ok((
            input,
            ImplMod(FixMatchRanks, eco_vec![before], *span),
            ImplMod(FixMatchRanks, eco_vec![after], *span),
        ))
    }
);

under!(ConstPat, input, _, _, asm, {
    let (input, val) = Val.invert_extract(input, asm)?;
    for end in 1..=input.len() {
        let frag = &input[..end];
        if let Some(sig) = nodes_clean_sig(frag) {
            match sig.args() {
                0 => {}
                1 => return generic(),
                _ => {
                    // println!("frag: {:?}", frag);
                    if let Some(sig) = un_inverse(frag, asm).ok().and_then(|inv| inv.clean_sig()) {
                        // println!("inv sig: {:?}", sig);
                        if sig.args() < sig.outputs() {
                            return generic();
                        }
                    }
                    return Ok((input, val, Node::empty()));
                }
            }
        }
    }
    generic()
});

/// Copy some values to the under stack at the beginning of the "do" step
/// and pop them at the beginning of the "undo" step
///
/// Allows a leading value if stashing at least 2 values
#[derive(Debug)]
struct Stash<A, B>(usize, A, B);
impl<A, B> UnderPattern for Stash<A, B>
where
    A: SpanFromNodes + AsNode + Copy,
    B: AsNode + Copy,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Node],
        g_sig: Signature,
        inverse: bool,
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node, Node)> {
        let &Stash(n, a, b) = self;
        let pat = (a, (CopyUnd(n), a), (PopUnd(n), b));
        if n >= 2 {
            MaybeVal(pat).under_extract(input, g_sig, inverse, asm)
        } else {
            pat.under_extract(input, g_sig, inverse, asm)
        }
    }
}
impl<P: UnderPattern> UnderPattern for MaybeVal<P> {
    fn under_extract<'a>(
        &self,
        mut input: &'a [Node],
        g_sig: Signature,
        inverse: bool,
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node, Node)> {
        let val = if let Ok((inp, val)) = Val.invert_extract(input, asm) {
            input = inp;
            Some(val)
        } else {
            None
        };
        let MaybeVal(p) = self;
        let (input, mut before, after) = p.under_extract(input, g_sig, inverse, asm)?;
        if let Some(val) = val {
            before.prepend(val);
        }
        Ok((input, before, after))
    }
}

#[derive(Debug)]
struct Store1Copy<A, B>(A, B);
impl<A, B> UnderPattern for Store1Copy<A, B>
where
    A: SpanFromNodes + AsNode + Copy,
    B: AsNode + Copy,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Node],
        g_sig: Signature,
        inverse: bool,
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node, Node)> {
        let &Store1Copy(a, b) = self;
        MaybeVal((a, (a, CopyUnd(1)), (PopUnd(1), b))).under_extract(input, g_sig, inverse, asm)
    }
}

impl<A, B, C> UnderPattern for (A, B, C)
where
    A: SpanFromNodes,
    B: AsNode,
    C: AsNode,
{
    fn under_extract<'a>(
        &self,
        input: &'a [Node],
        _: Signature,
        _: bool,
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node, Node)> {
        let (a, b, c) = self;
        let (input, span) = a.span_from_nodes(input, asm).ok_or(Generic)?;
        let span = span.ok_or(Generic)?;
        Ok((input, b.as_node(span), c.as_node(span)))
    }
}

#[derive(Debug)]
struct PushUnd(usize);
impl AsNode for PushUnd {
    fn as_node(&self, span: usize) -> Node {
        Node::PushUnder(self.0, span)
    }
}

#[derive(Debug)]
struct CopyUnd(usize);
impl AsNode for CopyUnd {
    fn as_node(&self, span: usize) -> Node {
        Node::CopyToUnder(self.0, span)
    }
}

#[derive(Debug)]
struct PopUnd(usize);
impl AsNode for PopUnd {
    fn as_node(&self, span: usize) -> Node {
        Node::PopUnder(self.0, span)
    }
}
