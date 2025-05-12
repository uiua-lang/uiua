use crate::ast::SubSide;

use super::*;

impl Node {
    /// Get the un inverse of this node
    pub fn un_inverse(&self, asm: &Assembly) -> InversionResult<Node> {
        dbgln!("un-inverting {self:?}");
        un_inverse(self.as_slice(), asm)
    }
    /// Get the anti inverse of this node
    pub fn anti_inverse(&self, asm: &Assembly) -> InversionResult<Node> {
        dbgln!("anti-inverting {self:?}");
        anti_inverse(self.as_slice(), asm, false)
    }
}

impl SigNode {
    /// Get the un-inverse of this node
    pub fn un_inverse(&self, asm: &Assembly) -> InversionResult<SigNode> {
        let inv = self.node.un_inverse(asm)?;
        Ok(SigNode::new(self.sig.inverse(), inv))
    }
    /// Get the anti inverse of this node
    pub fn anti_inverse(&self, asm: &Assembly) -> InversionResult<SigNode> {
        let inv = self.node.anti_inverse(asm)?;
        let sig = self.sig.anti().ok_or(Generic)?;
        Ok(SigNode::new(sig, inv))
    }
}

pub fn un_inverse(input: &[Node], asm: &Assembly) -> InversionResult<Node> {
    if input.is_empty() {
        return Ok(Node::empty());
    }

    thread_local! {
        static CACHE: RefCell<HashMap<u64, InversionResult<Node>>> = Default::default();
    }
    let mut hasher = DefaultHasher::new();
    for node in input {
        node.hash_with_span(&mut hasher);
    }
    let hash = hasher.finish();
    if let Some(cached) = CACHE.with(|cache| {
        (cache.borrow_mut().get(&hash))
            .filter(|node| {
                // Bit of a hack
                !node.as_ref().is_ok_and(|node| {
                    node.iter()
                        .any(|node| matches!(node, ImplPrim(MatchPattern, _)))
                })
            })
            .cloned()
    }) {
        return cached;
    }
    let res = un_inverse_impl(input, asm, false);
    CACHE.with(|cache| cache.borrow_mut().insert(hash, res.clone()));
    res
}

fn un_inverse_impl(
    input: &[Node],
    asm: &Assembly,
    require_for_under: bool,
) -> InversionResult<Node> {
    let mut node = Node::empty();
    let mut curr = input;
    let mut error = Generic;
    'find_pattern: loop {
        for pattern in UN_PATTERNS
            .iter()
            .filter(|pat| pat.allowed_in_under() || !require_for_under)
        {
            match pattern.invert_extract(curr, asm) {
                Ok((new, inv)) => {
                    dbgln!("matched pattern {pattern:?}\n  on {curr:?}\n  to {inv:?}");
                    node.prepend(inv);
                    if new.is_empty() {
                        dbgln!("un-inverted\n  {input:?}\n  to {node:?}");
                        return Ok(node);
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

fn anti_inverse(input: &[Node], asm: &Assembly, for_un: bool) -> InversionResult<Node> {
    if input.is_empty() {
        return generic();
    }
    thread_local! {
        static CACHE: RefCell<HashMap<u64, InversionResult<Node>>> = Default::default();
    }
    let mut hasher = DefaultHasher::new();
    for node in input {
        node.hash_with_span(&mut hasher);
    }
    let hash = hasher.finish();
    if let Some(cached) = CACHE.with(|cache| cache.borrow_mut().get(&hash).cloned()) {
        return cached;
    }
    let res = anti_inverse_impl(input, asm, for_un);
    CACHE.with(|cache| cache.borrow_mut().insert(hash, res.clone()));
    res
}

fn anti_inverse_impl(mut input: &[Node], asm: &Assembly, for_un: bool) -> InversionResult<Node> {
    // An anti inverse can be optionaly sandwiched by an un inverse on either side
    let orig_input = input;
    let mut error = Generic;

    // Anti inverse
    let mut got_anti = false;
    let mut pre = Node::empty();
    let mut anti = Node::empty();
    'find_anti: for s in 0..input.len() {
        error = Generic;
        let curr = &input[s..];
        for pattern in (ANTI_PATTERNS.iter()).filter(|pat| !for_un || pat.allowed_in_un()) {
            match pattern.invert_extract(curr, asm) {
                Ok((new, anti_inv)) => {
                    if nodes_clean_sig(new).is_none_or(|sig| sig.args() != sig.outputs()) {
                        continue;
                    }
                    dbgln!("matched anti pattern {pattern:?}\n  on {curr:?}\n  to {anti_inv:?}");
                    pre = Node::from(&input[..s]);
                    input = new;
                    anti = anti_inv;
                    got_anti = true;
                    break 'find_anti;
                }
                Err(e) => error = error.max(e),
            }
        }
    }
    if !got_anti {
        return Err(error);
    }

    // Leading value
    let val = if let Ok((rest, val)) = Val.invert_extract(&pre, asm) {
        let rest_sig = nodes_clean_sig(rest).ok_or(Generic)?;
        if rest_sig == (0, 0) || rest_sig == (1, 1) {
            pre = rest.into();
            Some(val)
        } else {
            None
        }
    } else {
        None
    };

    let pre_sig = pre.sig()?;
    if !(pre_sig == (0, 0) || pre_sig == (1, 1)) {
        return generic();
    }

    // Trailing un inverse
    let mut post = Node::empty();
    let mut curr = input;
    'outer: loop {
        for pattern in UN_PATTERNS {
            match pattern.invert_extract(curr, asm) {
                Ok((new, un_inv)) => {
                    dbgln!(
                        "matched un pattern for anti {pattern:?}\n  on {curr:?}\n  to {un_inv:?}"
                    );
                    curr = new;
                    post.prepend(un_inv);
                    continue 'outer;
                }
                Err(e) => error = error.max(e),
            }
        }
        break;
    }
    let span = post.span().or_else(|| anti.span()).or_else(|| pre.span());
    if !post.is_empty() {
        let span = span.ok_or(Generic)?;
        post = Mod(Dip, eco_vec![post.sig_node()?], span);
    }

    anti.prepend(post);
    anti.prepend(pre);

    if let Some(val) = val {
        let span = span.ok_or(Generic)?;
        anti.prepend(val);
        anti = Mod(Dip, eco_vec![anti.sig_node()?], span);
        anti.push(ImplPrim(MatchPattern, span));
    }

    dbgln!("anti-inverted\n     {orig_input:?}\n  to {anti:?}");
    Ok(anti)
}

pub static UN_PATTERNS: &[&dyn InvertPattern] = &[
    &AlgebraPat,
    &InnerAnti,
    &InnerContraDip,
    &JoinPat,
    &ArrayPat,
    &UnpackPat,
    &DipPat,
    &BothPat,
    &ImplBothPat,
    &BracketPat,
    &OnPat,
    &ByPat,
    &WithPat,
    &OffPat,
    &RowsPat,
    &Trivial,
    &ScanPat,
    &ReduceMulPat,
    &ReduceFormatPat,
    &GroupPat,
    &PartitionPat,
    &PrimesPat,
    &CustomPat,
    &FormatPat,
    &FillPat,
    &InsertPat,
    &RepeatPat,
    &DupPat,
    &DumpPat,
    &NBitsPat,
    &(Sqrt, (Dup, Mul)),
    &(Select, (Dup, Len, Range)),
    &(Pick, (Dup, Shape, Range)),
    &(Orient, (Dup, Shape, Len, Range)),
    &RequireVal((ValidateType, ValidateType)),
    &RequireVal((TagVariant, ValidateVariant)),
    &RequireVal((ValidateVariant, TagVariant)),
    &(Dup, (Over, Flip, MatchPattern)),
    &GetLocalPat,
    &AnaPat,
    &PrimPat,
    &ImplPrimPat,
    &NoUnder(MatchConst),
];

pub static ANTI_PATTERNS: &[&dyn InvertPattern] = &[
    &NoUn(NoUnder((Complex, (crate::Complex::I, Mul, Sub)))),
    &NoUn(NoUnder((Atan, (Flip, UnAtan, Div, Mul)))),
    &((IgnoreMany(Flip), Add), Sub),
    &(Sub, Add),
    &((Flip, Sub), (Flip, Sub)),
    &((IgnoreMany(Flip), Mul), Div),
    &(Div, Mul),
    &((Flip, Div), (Flip, Div)),
    &(Rotate, AntiRotate),
    &(AntiRotate, Rotate),
    &(Pow, Root),
    &(Root, Pow),
    &((1, Flip, Div, Pow), Pow),
    &((Flip, Pow), Log),
    &(Log, (Flip, Pow)),
    &((Flip, Log), (Flip, Root)),
    &((Flip, Root), (Flip, Log)),
    &((Flip, 1, Flip, Div, Pow), (Flip, Log)),
    &NoUn(NoUnder((Complex, (crate::Complex::I, Mul, Sub)))),
    &(Min, MatchLe),
    &(Max, MatchGe),
    &(Orient, AntiOrient),
    &(AntiOrient, Orient),
    &(Drop, AntiDrop),
    &(Select, AntiSelect),
    &(Pick, AntiPick),
    &(Keep, AntiKeep),
    &(Base, AntiBase),
    &(AntiBase, Base),
    &(Pop, Pop),
    &(EncodeBytes, DecodeBytes(None)),
    &(DecodeBytes(None), EncodeBytes),
    &(
        DecodeBytes(Some(SubSide::Left)),
        SidedEncodeBytes(SubSide::Left),
    ),
    &(
        DecodeBytes(Some(SubSide::Right)),
        SidedEncodeBytes(SubSide::Right),
    ),
    &(
        SidedEncodeBytes(SubSide::Left),
        DecodeBytes(Some(SubSide::Left)),
    ),
    &(
        SidedEncodeBytes(SubSide::Right),
        DecodeBytes(Some(SubSide::Right)),
    ),
    &AntiEncodings,
    &MatrixDivPat,
    &NoUnder(AntiCouplePat),
    &NoUnder(AntiArrayPat),
    &AntiFillPat,
    &AntiTrivial,
    &AntiRepeatPat,
    &AntiInsertPat,
    &AntiJoinPat,
    &AntiContraFlip,
    &AntiCustomPat,
];

pub static CONTRA_PATTERNS: &[&dyn InvertPattern] = &[
    &((IgnoreMany(Flip), Add), (Flip, Sub)),
    &(Sub, Sub),
    &((Flip, Sub), Add),
    &((IgnoreMany(Flip), Mul), (Flip, Div)),
    &(Div, Div),
    &((Flip, Div), Mul),
    &(Pow, (Flip, Log)),
    &(Log, Root),
    &((Flip, Log), Pow),
    &((Flip, Pow), (Flip, Root)),
    &(Min, Min),
    &(Max, Max),
    &(Select, IndexOf),
    &(IndexOf, Select),
    &NoUnder(ContraCouplePat),
];

pub trait InvertPattern: fmt::Debug + Sync {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)>;
    fn allowed_in_un(&self) -> bool {
        true
    }
    fn allowed_in_under(&self) -> bool {
        true
    }
    fn try_invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> (&'a [Node], InversionResult<Node>) {
        match self.invert_extract(input, asm) {
            Ok((input, node)) => (input, Ok(node)),
            Err(e) => (input, Err(e)),
        }
    }
}

macro_rules! inverse {
    // Optional parens
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), $body:expr) => {
        inverse!($(#[$attr])* $($doc,)? $($tt)*, $body);
    };
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), ref, $pat:pat, $body:expr) => {
        inverse!($(#[$attr])* $($doc,)? $($tt)*, ref, $pat, $body);
    };
    ($(#[$attr:meta])* $($doc:literal,)? ($($tt:tt)*), $pat:pat, $body:expr) => {
        inverse!($(#[$attr])* $($doc,)? $($tt)*, $pat, $body);
    };
    // Main impl
    ($(#[$attr:meta])* $($doc:literal,)? $name:ident, $input:ident, $asm:tt, $body:expr) => {
        #[derive(Debug)]
        pub(crate) struct $name;
        impl InvertPattern for $name {
            #[allow(irrefutable_let_patterns)]
            fn invert_extract<'a>(
                &self,
                $input: &'a [Node],
                $asm: &Assembly,
            ) -> InversionResult<(&'a [Node], Node)> {
                $body
            }
        }
    };
    ($(#[$attr:meta])* $($doc:literal,)? $name:ident, $input:ident, $asm:tt, ref, $pat:pat, $body:expr) => {
        inverse!($([$attr])* $($doc)? $name, $input, $asm, {
            let [$pat, ref $input @ ..] = $input else {
                return generic();
            };
            $body
        });
    };
    ($(#[$attr:meta])* $($doc:literal,)? $name:ident, $input:ident, $asm:tt, $pat:pat, $body:expr) => {
        inverse!($([$attr])* $($doc)? $name, $input, $asm, {
            let &[$pat, ref $input @ ..] = $input else {
                return generic();
            };
            $body
        });
    };
    ($(#[$attr:meta])* $($doc:literal,)? $name:ident, $input:ident, $asm:tt, $prim:ident, $span:ident, $args:pat, $body:expr) => {
        inverse!($([$attr])* $($doc)? $name, $input, $asm, ref, Mod($prim, args, $span), {
            let $args = args.as_slice() else {
                return generic();
            };
            let $span = *$span;
            $body
        });
    };
}

inverse!(
    (ArrayPat, input, asm),
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
        let mut inv = un_inverse(inner.as_slice(), asm)?;
        inv.prepend(Node::Unpack {
            count: *len,
            unbox: *boxed,
            allow_ext: *allow_ext,
            span: *span,
            prim: *prim,
        });
        Ok((input, inv))
    }
);

inverse!(DipPat, input, asm, Dip, span, [f], {
    let inv = f.un_inverse(asm)?;
    Ok((input, Mod(Dip, eco_vec![inv], span)))
});

inverse!(BothPat, input, asm, Both, span, [f], {
    let inv = f.un_inverse(asm)?;
    Ok((
        input,
        ImplMod(UnBothImpl(Default::default()), eco_vec![inv], span),
    ))
});

inverse!(
    (ImplBothPat, input, asm),
    ref,
    ImplMod(BothImpl(sub), args, span),
    {
        let [f] = args.as_slice() else {
            return generic();
        };
        if sub.side.is_some() {
            return Err(InversionError::SidedBoth);
        }
        let inv = f.un_inverse(asm)?;
        Ok((input, ImplMod(UnBothImpl(*sub), eco_vec![inv], *span)))
    }
);

inverse!(BracketPat, input, asm, Bracket, span, [f, g], {
    let f_inv = f.un_inverse(asm)?;
    let g_inv = g.un_inverse(asm)?;
    Ok((input, ImplMod(UnBracket, eco_vec![f_inv, g_inv], span)))
});

inverse!(OnPat, input, asm, On, span, [f], {
    let inv = Mod(On, eco_vec![f.anti_inverse(asm)?], span);
    Ok((input, inv))
});

inverse!(WithPat, input, asm, With, span, [f], {
    let mut f = f.node.clone();
    f.prepend(Prim(Flip, span));
    let inv = Mod(Off, eco_vec![f.anti_inverse(asm)?.sig_node()?], span);
    Ok((input, inv))
});

inverse!(OffPat, input, asm, Off, span, [f], {
    let mut inner = f.node.anti_inverse(asm)?;
    inner.prepend(Prim(Flip, span));
    let inv = Mod(With, eco_vec![inner.sig_node()?], span);
    Ok((input, inv))
});

inverse!(ByPat, input, asm, By, span, [f], {
    // Under's undo step
    if f.sig.args() == 1 {
        if let [Prim(Shape, span), Prim(Len, _)] = f.node.as_slice() {
            // Set rank
            let before = Node::from_iter([
                Node::new_push(1),
                Prim(Sub, *span),
                Prim(Over, *span),
                Prim(Shape, *span),
                Prim(Over, *span),
                PushUnder(2, *span),
                Prim(Rerank, *span),
            ])
            .sig_node()?;
            let after =
                Node::from_iter([PopUnder(2, *span), ImplPrim(UndoRerank, *span)]).sig_node()?;
            let adjust_rank = CustomInverse {
                normal: Ok(Node::from_iter([
                    Node::new_push(1),
                    Prim(Sub, *span),
                    Prim(Rerank, *span),
                ])
                .sig_node()?),
                un: Some(Mod(By, eco_vec![f.clone()], *span).sig_node()?),
                under: Some((before, after)),
                ..Default::default()
            };
            return Ok((input, CustomInverse(adjust_rank.into(), *span)));
        } else if let Ok((before, after)) = f.node.under_inverse(Signature::new(1, 1), false, asm) {
            let mut inv = before;
            (0..f.sig.outputs()).for_each(|_| inv.push(Prim(Pop, span)));
            for _ in 0..f.sig.outputs() {
                inv = Mod(Dip, eco_vec![inv.sig_node()?], span);
            }
            inv.push(after);
            return Ok((input, inv));
        }
    }
    // Contra inverse
    for pat in CONTRA_PATTERNS {
        if let Ok(([], inv)) = pat.invert_extract(&f.node, asm) {
            return Ok((input, Mod(By, eco_vec![inv.sig_node()?], span)));
        }
    }
    generic()
});

inverse!("Match a constant exactly", (MatchConst, input, asm), {
    let (input, mut val) = Val.invert_extract(input, asm)?;
    val.push(ImplPrim(MatchPattern, asm.spans.len() - 1));
    Ok((input, val))
});

inverse!(
    "Matches an anti inverse with the first argument as part of the input",
    (InnerAnti, input, asm),
    {
        if let Ok((inp, val)) = Val.invert_extract(input, asm) {
            // Starts with a value
            for end in 1..=inp.len() {
                if let Ok(mut inv) = anti_inverse(&inp[..end], asm, true) {
                    inv.prepend(val);
                    dbgln!("matched inner anti pattern for un\n  on {input:?}\n  to {inv:?}");
                    return Ok((&inp[end..], inv));
                }
            }
            generic()
        } else if let [Mod(Dip, args, dip_span), input @ ..] = input {
            // Starts with dip value
            let [f] = args.as_slice() else {
                return generic();
            };
            let ([], mut node) = Val.invert_extract(f.node.as_slice(), asm)? else {
                return generic();
            };
            node.push(Prim(Flip, *dip_span));
            node.extend(input.iter().cloned());
            let ([], inv) = InnerAnti.invert_extract(node.as_slice(), asm)? else {
                return generic();
            };
            Ok((&[], inv))
        } else {
            generic()
        }
    }
);

inverse!(InnerContraDip, input, asm, Dip, dip_span, [f], {
    let ([], val) = Val.invert_extract(f.node.as_slice(), asm)? else {
        return generic();
    };
    for pat in CONTRA_PATTERNS {
        if let Ok((inp, mut inv)) = pat.invert_extract(input, asm) {
            inv.prepend(Prim(Flip, dip_span));
            inv.prepend(val);
            return Ok((inp, inv));
        }
    }
    generic()
});

inverse!(
    (UnpackPat, input, asm),
    Unpack {
        count,
        unbox,
        allow_ext,
        prim,
        span,
        ..
    },
    {
        let mut inv = un_inverse(input, asm)?;
        inv.push(Array {
            len: count,
            inner: Node::empty().into(),
            boxed: unbox,
            allow_ext,
            prim,
            span,
        });
        Ok((&[], inv))
    }
);

inverse!(RowsPat, input, asm, Rows, span, [f], {
    Ok((input, Mod(Rows, eco_vec![f.un_inverse(asm)?], span)))
});

inverse!(ScanPat, input, asm, {
    let un = matches!(input, [ImplMod(UnScan, ..), ..]);
    let ([Mod(Scan, args, span), input @ ..] | [ImplMod(UnScan, args, span), input @ ..]) = input
    else {
        return generic();
    };
    let [f] = args.as_slice() else {
        return generic();
    };
    let inverse = match f.node.as_primitive() {
        Some(Primitive::Add) if !un => Prim(Sub, *span),
        Some(Primitive::Mul) if !un => Prim(Div, *span),
        Some(Primitive::Sub) if un => Prim(Add, *span),
        Some(Primitive::Div) if un => Prim(Mul, *span),
        Some(Primitive::Eq) => Prim(Eq, *span),
        Some(Primitive::Ne) => Prim(Ne, *span),
        _ => f.node.un_inverse(asm)?,
    }
    .sig_node()?;
    let inverse = if un {
        Mod(Scan, eco_vec![inverse], *span)
    } else {
        ImplMod(UnScan, eco_vec![inverse], *span)
    };
    Ok((input, inverse))
});

inverse!(
    (ReduceMulPat, input, _, Reduce, span),
    [SigNode {
        node: Prim(Mul, _),
        ..
    }],
    Ok((input, ImplPrim(Primes, span)))
);

inverse!(
    (PrimesPat, input, _, ImplPrim(Primes, span)),
    Ok((
        input,
        Mod(Reduce, eco_vec![Prim(Mul, span).sig_node()?], span)
    ))
);

inverse!(
    (RepeatPat, input, _),
    ref,
    ImplMod(RepeatWithInverse, args, span),
    {
        let [_f, inv] = args.as_slice() else {
            return generic();
        };
        Ok((
            input,
            ImplMod(RepeatCountConvergence, eco_vec![inv.clone()], *span),
        ))
    }
);

inverse!(
    (AntiRepeatPat, input, _),
    ref,
    ImplMod(RepeatWithInverse, args, span),
    {
        let [f, inv] = args.as_slice() else {
            return generic();
        };
        Ok((
            input,
            ImplMod(RepeatWithInverse, eco_vec![inv.clone(), f.clone()], *span),
        ))
    }
);

inverse!(ReduceFormatPat, input, _, Reduce, span, [f], {
    let Format(parts, fmt_span) = &f.node else {
        return generic();
    };
    if parts.len() != 3 || !parts[0].is_empty() || !parts[2].is_empty() {
        return Err(InversionError::ReduceFormat);
    }
    let inv = Node::from_iter([
        Node::new_push(parts[1].as_str()),
        ImplMod(
            SplitByKeepEmpty,
            eco_vec![Prim(Box, span).sig_node()?],
            *fmt_span,
        ),
    ]);
    Ok((input, inv))
});

inverse!(GroupPat, input, asm, Group, span, [f], {
    if f.sig != (1, 1) {
        return generic();
    }
    let f_inv = f.un_inverse(asm)?;
    let inv = ImplMod(UnGroup, eco_vec![f_inv], span);
    Ok((input, inv))
});

inverse!(PartitionPat, input, asm, Partition, span, [f], {
    if f.sig != (1, 1) {
        return generic();
    }
    let f_inv = f.un_inverse(asm)?;
    let inv = ImplMod(UnPartition, eco_vec![f_inv], span);
    Ok((input, inv))
});

inverse!(JoinPat, input, asm, {
    let orig_input = input;
    let mut input = input;
    let Some((join_index, join_span)) = (input.iter().enumerate().rev())
        .filter_map(|(i, node)| match node.inner() {
            Prim(Join, span) => Some((i, *span)),
            _ => None,
        })
        .find(|(i, _)| nodes_clean_sig(&input[..*i]).is_some())
    else {
        return generic();
    };
    let mut dipped = false;
    let node = if let Some((inp, mut node)) = Val.invert_extract(input, asm).ok().or_else(|| {
        let [Mod(Dip, args, _), input @ ..] = input else {
            return None;
        };
        let [inner] = args.as_slice() else {
            return None;
        };
        let Ok(([], val)) = Val.invert_extract(inner.node.as_slice(), asm) else {
            return None;
        };
        dipped = true;
        Some((input, val))
    }) {
        input = inp;
        if let Some(i) = (1..=input.len())
            .rev()
            .find(|&i| nodes_clean_sig(&input[..i]).is_some_and(|sig| sig == (0, 0)))
        {
            node.extend(un_inverse(&input[..i], asm)?);
            input = &input[i..];
        }
        let (prim, span) = match *input {
            [Prim(Join, span), ref inp @ ..] if dipped => {
                input = inp;
                (UnJoinShapeEnd, span)
            }
            [Prim(Join, span), ref inp @ ..] => {
                input = inp;
                (UnJoinShape, span)
            }
            [Prim(Flip, _), Prim(Join, span), ref inp @ ..] if !dipped => {
                input = inp;
                (UnJoinShapeEnd, span)
            }
            _ => return generic(),
        };
        let inner =
            Node::from_iter([Prim(Primitive::Shape, span), ImplPrim(prim, span)]).sig_node()?;
        node.extend([
            Mod(Dip, eco_vec![inner], span),
            ImplPrim(MatchPattern, span),
        ]);
        node
    } else if let Some(i) = (0..join_index)
        .find(|&i| nodes_clean_sig(&input[i..join_index]).is_some_and(|sig| sig == (0, 1)))
    {
        let mut node = ImplPrim(UnJoin, join_span);
        node.extend(un_inverse_impl(&input[i..join_index], asm, true)?);
        node.extend(un_inverse(&input[..i], asm)?);
        input = &input[join_index + 1..];
        node
    } else {
        fn invert_inner(mut input: &[Node], asm: &Assembly) -> InversionResult<Node> {
            let mut node = Node::empty();
            while !input.is_empty() {
                if let [Mod(Dip, args, _), inp @ ..] = input {
                    let [inner] = args.as_slice() else {
                        return generic();
                    };
                    node.extend(invert_inner(inner.node.as_slice(), asm)?);
                    input = inp;
                    continue;
                }
                if let Some((i, _)) = input.iter().enumerate().skip(1).find(|(i, node)| {
                    nodes_clean_sig(&input[..*i]).is_some() && matches!(node.inner(), Mod(Dip, ..))
                }) {
                    node.extend(un_inverse(&input[..i], asm)?);
                    input = &input[i..];
                    continue;
                }
                node.extend(un_inverse(input, asm)?);
                break;
            }
            Ok(node)
        }
        let flip_after = join_index > 0 && matches!(input[join_index - 1].inner(), Prim(Flip, _));
        let flip_before = join_index > 1 && matches!(input[0].inner(), Prim(Flip, _));
        let flip = flip_before ^ flip_after;
        let before = &input[flip_before as usize..join_index - flip_after as usize];
        input = &input[join_index + 1..];
        let before_inv = invert_inner(before, asm)?;
        let before_sig = nodes_clean_sig(&before_inv).ok_or(Generic)?;
        let mut node = Node::empty();
        let count = before_sig.outputs().saturating_sub(before_sig.args()) + 1;
        let prim = if count <= 1 {
            if flip {
                UnJoinEnd
            } else {
                UnJoin
            }
        } else {
            node.push(Push(count.into()));
            if flip {
                UnJoinShapeEnd
            } else {
                UnJoinShape
            }
        };
        node.push(ImplPrim(prim, join_span));
        node.push(before_inv);
        node
    };
    let orig_sig = nodes_sig(&orig_input[..orig_input.len() - input.len()])?;
    let inverted_sig = node.sig()?;
    if orig_sig.inverse() != inverted_sig {
        return generic();
    }
    Ok((input, node))
});

inverse!(AntiJoinPat, input, _, {
    Ok(match *input {
        [Prim(Join, span), ref input @ ..] => {
            let inv = Node::from_iter([
                Prim(Dup, span),
                Prim(Shape, span),
                Prim(Flip, span),
                PushUnder(1, span),
                ImplPrim(UnJoinShape, span),
                PopUnder(1, span),
                ImplPrim(MatchPattern, span),
            ]);
            (input, inv)
        }
        [Prim(Flip, span), Prim(Join, _), ref input @ ..] => {
            let inv = Node::from_iter([
                Prim(Dup, span),
                Prim(Shape, span),
                Prim(Flip, span),
                PushUnder(1, span),
                ImplPrim(UnJoinShapeEnd, span),
                PopUnder(1, span),
                ImplPrim(MatchPattern, span),
            ]);
            (input, inv)
        }
        _ => return generic(),
    })
});

inverse!(CustomPat, input, _, ref, CustomInverse(cust, span), {
    let mut cust = CustomInverse::clone(cust);
    let un = cust.un.take().ok_or(Generic)?;
    cust.un = cust.normal.ok();
    cust.normal = Ok(un);
    cust.anti = None;
    cust.under = None;
    Ok((input, CustomInverse(cust.into(), *span)))
});

inverse!(AntiCustomPat, input, asm, ref, CustomInverse(cust, span), {
    let mut cust = CustomInverse::clone(cust);
    if let Some(anti) = cust.anti.take() {
        cust.anti = cust.normal.ok();
        cust.normal = Ok(anti);
        cust.un = None;
        cust.under = None;
        Ok((input, CustomInverse(cust.into(), *span)))
    } else if input.is_empty() {
        Ok((&[], cust.normal?.anti_inverse(asm)?.node))
    } else {
        generic()
    }
});

inverse!(FormatPat, input, _, ref, Format(parts, span), {
    Ok((input, MatchFormatPattern(parts.clone(), *span)))
});

inverse!(FillPat, input, asm, Fill, span, [fill, f], {
    if fill.sig != (0, 1) {
        return generic();
    }
    let inv = f.un_inverse(asm)?;
    Ok((input, ImplMod(UnFill, eco_vec![fill.clone(), inv], span)))
});

inverse!(AntiFillPat, input, asm, Fill, span, [fill, f], {
    if fill.sig != (0, 1) {
        return generic();
    }
    let inv = f.anti_inverse(asm)?;
    Ok((input, ImplMod(UnFill, eco_vec![fill.clone(), inv], span)))
});

inverse!(InsertPat, input, asm, {
    let (input, first) = Val.invert_extract(input, asm)?;
    let second = Val.invert_extract(input, asm);
    let &[Prim(Insert, span), ref input @ ..] = input else {
        return generic();
    };
    let (input, key, value) = if let Ok((input, key)) = second {
        (input, key, Some(first))
    } else {
        (input, first, None)
    };
    let mut node = Node::from_iter([
        key,
        Prim(Over, span),
        Prim(Over, span),
        Prim(Has, span),
        Node::new_push(1),
        ImplPrim(MatchPattern, span),
        Prim(Over, span),
        Prim(Over, span),
        Prim(Get, span),
        PushUnder(1, span),
        Prim(Remove, span),
        PopUnder(1, span),
    ]);
    if let Some(value) = value {
        node.extend(value);
        node.push(ImplPrim(MatchPattern, span));
    }
    Ok((input, node))
});

inverse!(AntiInsertPat, input, _, Prim(Insert, span), {
    let args = eco_vec![Prim(Get, span).sig_node()?, Prim(Remove, span).sig_node()?];
    let inv = Mod(Fork, args, span);
    Ok((input, inv))
});

inverse!(AntiCouplePat, input, _, Prim(Couple, span), {
    let inv = Node::from_iter([
        Mod(Dip, eco_vec![ImplPrim(UnCouple, span).sig_node()?], span),
        ImplPrim(MatchPattern, span),
    ]);
    Ok((input, inv))
});

inverse!(
    AntiArrayPat,
    input,
    asm,
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
        let mut inner = un_inverse(inner.as_slice(), asm)?;
        inner.prepend(Node::Unpack {
            count: *len,
            unbox: *boxed,
            allow_ext: *allow_ext,
            span: *span,
            prim: *prim,
        });
        let inv = Node::from_iter([
            Mod(Dip, eco_vec![inner.sig_node()?], *span),
            ImplPrim(MatchPattern, *span),
        ]);
        Ok((input, inv))
    }
);

inverse!(ContraCouplePat, input, _, Prim(Couple, span), {
    let inv = Node::from_iter([
        ImplPrim(UnCouple, span),
        Mod(
            Dip,
            eco_vec![ImplPrim(MatchPattern, span).sig_node()?],
            span,
        ),
    ]);
    Ok((input, inv))
});

inverse!(AnaPat, input, asm, Reduce, span, [f], {
    if f.sig != (2, 1) {
        return generic();
    }
    let inv = f.un_inverse(asm)?;
    if inv.sig != (1, 2) {
        return generic();
    }
    Ok((
        input,
        Array {
            len: 2,
            inner: inv.node.into(),
            boxed: false,
            allow_ext: false,
            prim: None,
            span,
        },
    ))
});

inverse!(DupPat, input, asm, Prim(Dup, dup_span), {
    let Some(dyadic_i) =
        (0..=input.len()).find(|&i| nodes_clean_sig(&input[..i]).is_some_and(|sig| sig == (2, 1)))
    else {
        // Pattern matching
        let sig = nodes_sig(input)?;
        return if sig.args() == sig.outputs() {
            let inv = Node::from_iter([Prim(Over, dup_span), ImplPrim(MatchPattern, dup_span)]);
            Ok((input, inv))
        } else {
            generic()
        };
    };

    // Special cases
    let dyadic_whole = &input[..dyadic_i];
    let input = &input[dyadic_i..];
    let monadic_i = (0..=dyadic_whole.len())
        .rev()
        .find(|&i| {
            nodes_clean_sig(&dyadic_whole[..i])
                .is_some_and(|sig| sig.args() == 0 && sig.outputs() == 0)
        })
        .ok_or(Generic)?;
    let monadic_part = &dyadic_whole[..monadic_i];
    let dyadic_part = &dyadic_whole[monadic_i..];
    dbgln!("inverse monadic part: {monadic_part:?}");
    dbgln!("inverse dyadic part: {dyadic_part:?}");
    let monadic_inv = un_inverse(monadic_part, asm)?;
    let inverse = match *dyadic_part {
        [Prim(Primitive::Add, span)] => {
            Node::from_iter([monadic_inv, Node::new_push(2), Prim(Primitive::Div, span)])
        }
        [Prim(Primitive::Mul, span)] => {
            let mut inv = Prim(Primitive::Sqrt, span);
            if !monadic_inv.is_empty() {
                inv.extend([
                    Prim(Primitive::Dup, dup_span),
                    monadic_inv,
                    Prim(Primitive::Pop, span),
                ]);
            }
            inv
        }
        _ => {
            let mut inv = monadic_inv;
            inv.extend(un_inverse(dyadic_part, asm)?);
            inv.push(Prim(Primitive::Over, dup_span));
            inv.push(ImplPrim(ImplPrimitive::MatchPattern, dup_span));
            inv
        }
    };
    Ok((input, inverse))
});

inverse!(DumpPat, input, _, ref, Mod(Dump, args, span), {
    Ok((input, ImplMod(UnDump, args.clone(), *span)))
});

inverse!(AlgebraPat, input, asm, {
    let mut error = Generic;
    for end in (1..=input.len()).rev() {
        let chunk = &input[..end];
        match algebraic_inverse(chunk, asm) {
            Ok(inv) => return Ok((&input[end..], inv)),
            Err(Some(e)) => error = error.max(InversionError::AlgebraError(e)),
            Err(None) => {}
        }
    }
    Err(error)
});

inverse!(AntiContraFlip, input, asm, Prim(Flip, span), {
    if nodes_clean_sig(input).is_none_or(|sig| sig != (2, 1)) {
        return generic();
    }
    for pat in CONTRA_PATTERNS.iter() {
        if let Ok((inp, mut inv)) = pat.invert_extract(input, asm) {
            inv.prepend(Prim(Flip, span));
            return Ok((inp, inv));
        }
    }
    generic()
});

inverse!(MatrixDivPat, input, _, Prim(Transpose, _), {
    let [Mod(Table, args, span), ImplPrim(TransposeN(-1), _), input @ ..] = input else {
        return generic();
    };
    let [table_node] = args.as_slice() else {
        return generic();
    };
    let [Prim(Mul, _), Mod(Reduce, args, _)] = table_node.node.as_slice() else {
        return generic();
    };
    let [reduce_node] = args.as_slice() else {
        return generic();
    };
    let Prim(Add, _) = reduce_node.node else {
        return generic();
    };
    Ok((input, ImplPrim(MatrixDiv, *span)))
});

inverse!(GetLocalPat, input, asm, GetLocal { def, span }, {
    let by_rest = Mod(By, eco_vec![Node::from(input).sig_node().unwrap()], span);
    let rest_inv = by_rest.un_inverse(asm)?;
    let inv = Node::from_iter([
        GetLocal { def, span },
        Prim(Flip, span),
        rest_inv,
        SetLocal { def, span },
    ]);
    Ok((&[], inv))
});

inverse!(NBitsPat, input, _, ImplPrim(NBits(_), span), {
    Ok((input, ImplPrim(UnBits, span)))
});

#[derive(Debug)]
struct Trivial;
impl InvertPattern for Trivial {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        match input {
            [NoInline(inner), input @ ..] => Ok((input, NoInline(inner.un_inverse(asm)?.into()))),
            [TrackCaller(inner), input @ ..] => {
                Ok((input, TrackCaller(inner.un_inverse(asm)?.into())))
            }
            [Label(label, span), input @ ..] => {
                Ok((input, RemoveLabel(Some(label.clone()), *span)))
            }
            [RemoveLabel(Some(label), span), input @ ..] => {
                Ok((input, Label(label.clone(), *span)))
            }
            [node @ SetOutputComment { .. }, input @ ..] => Ok((input, node.clone())),
            [Call(f, _), input @ ..] => Ok((input, asm[f].un_inverse(asm).map_err(|e| e.func(f))?)),
            [ImplPrim(ValidateNonBoxedVariant, _), input @ ..] => Ok((input, Node::empty())),
            input => {
                for node in input {
                    if let BindGlobal { index, .. } = node.inner() {
                        let binding = &asm.bindings[*index];
                        let name = binding.span.as_str(&asm.inputs, |s| s.into());
                        return Err(InversionError::LateBinding(name));
                    }
                }
                generic()
            }
        }
    }
}

#[derive(Debug)]
struct AntiTrivial;
impl InvertPattern for AntiTrivial {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        match input {
            [NoInline(inner), input @ ..] => Ok((input, NoInline(inner.anti_inverse(asm)?.into()))),
            [TrackCaller(inner), input @ ..] => {
                Ok((input, TrackCaller(inner.anti_inverse(asm)?.into())))
            }
            [node @ SetOutputComment { .. }, input @ ..] => Ok((input, node.clone())),
            [Call(f, _), input @ ..] => {
                let mut node = asm[f].clone();
                node.extend(input.iter().cloned());
                Ok((input, node.anti_inverse(asm).map_err(|e| e.func(f))?))
            }
            _ => generic(),
        }
    }
}

#[derive(Debug)]
struct AntiEncodings;
impl InvertPattern for AntiEncodings {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        _: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        match input {
            [Prim(ImageEncode, span), input @ ..] => {
                let inv = Node::from_iter([
                    Prim(Pop, *span),
                    ImplPrim(ImageDecode, *span),
                    Prim(Pop, *span),
                ]);
                Ok((input, inv))
            }
            [Prim(GifEncode, span), input @ ..] => {
                let inv = Node::from_iter([
                    Prim(Pop, *span),
                    ImplPrim(GifDecode, *span),
                    Prim(Pop, *span),
                ]);
                Ok((input, inv))
            }
            _ => generic(),
        }
    }
}

inverse!(PrimPat, input, _, Prim(prim, span), {
    let inv = match prim {
        // Basic
        Identity => Prim(Identity, span),
        Flip => Prim(Flip, span),
        Pop => ImplPrim(UnPop, span),
        Neg => Prim(Neg, span),
        Not => Prim(Not, span),
        Sin => ImplPrim(Asin, span),
        Ln => ImplPrim(Exp, span),
        Atan => ImplPrim(UnAtan, span),
        Complex => ImplPrim(UnComplex, span),
        Add => ImplPrim(UnAdd, span),
        Mul => ImplPrim(UnMul, span),
        Div => ImplPrim(UnDiv, span),
        Reverse => Prim(Reverse, span),
        Transpose => ImplPrim(TransposeN(-1), span),
        Bits => ImplPrim(UnBits, span),
        Couple => ImplPrim(UnCouple, span),
        Box => ImplPrim(UnBox, span),
        Where => ImplPrim(UnWhere, span),
        Utf8 => ImplPrim(UnUtf8, span),
        Graphemes => ImplPrim(UnGraphemes, span),
        Parse => ImplPrim(UnParse, span),
        Fix => ImplPrim(UnFix, span),
        Shape => ImplPrim(UnShape, span),
        Map => ImplPrim(UnMap, span),
        Stack => ImplPrim(UnStack, span),
        Keep => ImplPrim(UnKeep, span),
        Take => ImplPrim(UnTake, span),
        Sort => ImplPrim(UnSort, span),
        GifEncode => ImplPrim(GifDecode, span),
        AudioEncode => ImplPrim(AudioDecode, span),
        ImageEncode => ImplPrim(ImageDecode, span),
        Sys(SysOp::Clip) => ImplPrim(UnClip, span),
        Sys(SysOp::RawMode) => ImplPrim(UnRawMode, span),
        Hsv => ImplPrim(UnHsv, span),
        Json => ImplPrim(UnJson, span),
        Binary => ImplPrim(UnBinary, span),
        Csv => ImplPrim(UnCsv, span),
        Xlsx => ImplPrim(UnXlsx, span),
        Fft => ImplPrim(UnFft, span),
        DateTime => ImplPrim(UnDatetime, span),
        _ => return generic(),
    };
    Ok((input, inv))
});

inverse!(ImplPrimPat, input, _, ImplPrim(prim, span), {
    let inv = match prim {
        UnPop => Prim(Pop, span),
        Asin => Prim(Sin, span),
        Cos => ImplPrim(Acos, span),
        Acos => ImplPrim(Cos, span),
        Exp => Prim(Ln, span),
        SortDown => ImplPrim(UnSort, span),
        TransposeN(n) => ImplPrim(TransposeN(-n), span),
        UnWhere => Prim(Where, span),
        UnUtf8 => Prim(Utf8, span),
        Utf16 => ImplPrim(UnUtf16, span),
        UnUtf16 => ImplPrim(Utf16, span),
        UnGraphemes => Prim(Graphemes, span),
        UnAtan => Prim(Atan, span),
        UnComplex => Prim(Complex, span),
        UnAdd => Prim(Add, span),
        UnMul => Prim(Mul, span),
        UnDiv => Prim(Div, span),
        UnCouple => Prim(Couple, span),
        UnParse => Prim(Parse, span),
        UnFix => Prim(Fix, span),
        UnShape => Prim(Shape, span),
        UnMap => Prim(Map, span),
        UnStack => Prim(Stack, span),
        UnJoin => Prim(Join, span),
        UnKeep => Prim(Keep, span),
        UnBox => Prim(Box, span),
        UnHsv => Prim(Hsv, span),
        UnJson => Prim(Json, span),
        UnBinary => Prim(Binary, span),
        UnCsv => Prim(Csv, span),
        UnXlsx => Prim(Xlsx, span),
        UnFft => Prim(Fft, span),
        ImageDecode => Prim(ImageEncode, span),
        GifDecode => Prim(GifEncode, span),
        AudioDecode => Prim(AudioEncode, span),
        UnDatetime => Prim(DateTime, span),
        UnRawMode => Prim(Sys(SysOp::RawMode), span),
        UnClip => Prim(Sys(SysOp::Clip), span),
        StackN { n, inverse } => ImplPrim(
            StackN {
                n,
                inverse: !inverse,
            },
            span,
        ),
        _ => return generic(),
    };
    Ok((input, inv))
});

inverse!(Val, input, asm, {
    for end in (1..=input.len()).rev() {
        let chunk = &input[..end];
        if let Some(sig) = nodes_clean_sig(chunk) {
            if sig == (0, 1) && chunk.iter().all(|n| n.is_pure(Purity::Pure, asm)) {
                return Ok((&input[end..], Node::from(chunk)));
            }
        }
    }
    generic()
});

impl SpanFromNodes for Val {
    fn span_from_nodes<'a>(
        &self,
        nodes: &'a [Node],
        asm: &Assembly,
    ) -> Option<(&'a [Node], Option<usize>)> {
        Some((self.invert_extract(nodes, asm).ok()?.0, None))
    }
}

impl<A, B> InvertPattern for (A, B)
where
    A: SpanFromNodes,
    B: AsNode,
{
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        let (a, b) = self;
        let (input, span) = a.span_from_nodes(input, asm).ok_or(Generic)?;
        let span = span.ok_or(Generic)?;
        Ok((input, b.as_node(span)))
    }
}

#[derive(Debug)]
struct IgnoreMany<T>(T);
impl<T> SpanFromNodes for IgnoreMany<T>
where
    T: SpanFromNodes,
{
    fn span_from_nodes<'a>(
        &self,
        mut nodes: &'a [Node],
        asm: &Assembly,
    ) -> Option<(&'a [Node], Option<usize>)> {
        let mut span = None;
        while let Some((nds, sp)) = self.0.span_from_nodes(nodes, asm) {
            nodes = nds;
            span = span.or(sp);
        }
        Some((nodes, span))
    }
}

impl<P: InvertPattern> InvertPattern for MaybeVal<P> {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        let (input, val) = if let Ok((input, val)) = Val.invert_extract(input, asm) {
            (input, Some(val))
        } else {
            (input, None)
        };
        let (input, mut inv) = self.0.invert_extract(input, asm)?;
        if let Some(val) = val {
            inv.prepend(val);
        }
        Ok((input, inv))
    }
}

impl<P: InvertPattern> InvertPattern for RequireVal<P> {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        let (input, val) = Val.invert_extract(input, asm)?;
        let (input, mut inv) = self.0.invert_extract(input, asm)?;
        inv.prepend(val);
        Ok((input, inv))
    }
}

#[derive(Debug)]
struct NoUnder<P>(P);
impl<P: InvertPattern> InvertPattern for NoUnder<P> {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        self.0.invert_extract(input, asm)
    }
    fn allowed_in_un(&self) -> bool {
        self.0.allowed_in_un()
    }
    fn allowed_in_under(&self) -> bool {
        false
    }
}

#[derive(Debug)]
struct NoUn<P>(P);
impl<P: InvertPattern> InvertPattern for NoUn<P> {
    fn invert_extract<'a>(
        &self,
        input: &'a [Node],
        asm: &Assembly,
    ) -> InversionResult<(&'a [Node], Node)> {
        self.0.invert_extract(input, asm)
    }
    fn allowed_in_un(&self) -> bool {
        false
    }
    fn allowed_in_under(&self) -> bool {
        self.0.allowed_in_under()
    }
}
