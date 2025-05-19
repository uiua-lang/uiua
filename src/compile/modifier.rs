//! Compiler code for modifiers
#![allow(clippy::redundant_closure_call)]

use crate::algorithm::ga;

use super::*;
use algebra::{derivative, integral};
use invert::InversionError;
use pre_eval::PreEvalMode;

const MAX_COMPTIME_DEPTH: usize = if cfg!(debug_assertions) { 5 } else { 20 };

impl Compiler {
    fn desugar_function_pack(
        &mut self,
        modifier: &Sp<Modifier>,
        operand: &Sp<Word>,
        subscript: Option<Sp<Subscript>>,
    ) -> UiuaResult<Option<Node>> {
        let Sp {
            value: Word::Pack(pack),
            span,
        } = operand
        else {
            return Ok(None);
        };
        if let Some(down_span) = &pack.down_span {
            self.experimental_error(down_span, || {
                "Lexical ordering is experimental. To use it, \
                add `# Experimental!` to the top of the file."
            });
        }
        let mut node = match &modifier.value {
            Modifier::Macro(..) => {
                let new = Modified {
                    modifier: modifier.clone(),
                    operands: (pack.lexical_order())
                        .map(|b| b.clone().map(Word::Func))
                        .collect(),
                    pack_expansion: true,
                };
                self.modified(new, subscript)
            }
            Modifier::Primitive(Primitive::Dip) => {
                let mut nodes = Vec::with_capacity(pack.branches.len());
                let mut errors = Vec::new();
                for br in pack.lexical_order().cloned() {
                    match self.func(br.value, br.span.clone()) {
                        Ok(node) => {
                            let sig = self.sig_of(&node, &br.span)?;
                            nodes.push(SigNode::new(sig, node));
                        }
                        Err(e) => errors.push(e),
                    }
                }
                if !errors.is_empty() {
                    return Err(UiuaError::from_multi(errors));
                }
                let mut nodes = nodes.into_iter().rev();
                let dip_span = self.add_span(modifier.span.clone());
                let mut res = Node::Mod(Primitive::Dip, eco_vec![nodes.next().unwrap()], dip_span);
                for node in nodes {
                    res = Node::Mod(
                        Primitive::Dip,
                        eco_vec![Node::from_iter([res, node.node]).sig_node().unwrap()],
                        dip_span,
                    );
                }
                Ok(res)
            }
            Modifier::Primitive(Primitive::Rows | Primitive::Inventory) => {
                let mut branches = pack.lexical_order().cloned();
                let mut new = Modified {
                    modifier: modifier.clone(),
                    operands: vec![branches.next().unwrap().map(Word::Func)],
                    pack_expansion: true,
                };
                for branch in branches {
                    let mut func = Func {
                        lines: branch.value.lines,
                        signature: None,
                        closed: true,
                    };
                    if func.word_lines().count() == 0 {
                        func.lines.push(Item::Words(Vec::new()));
                    }
                    (func.word_lines_mut().next().unwrap())
                        .insert(0, span.clone().sp(Word::Modified(Box::new(new))));
                    new = Modified {
                        modifier: modifier.clone(),
                        operands: vec![branch.span.clone().sp(Word::Func(func))],
                        pack_expansion: true,
                    };
                }
                self.modified(new, subscript)
            }
            Modifier::Primitive(
                Primitive::Fork | Primitive::Bracket | Primitive::Try | Primitive::Fill,
            ) => {
                let mut branches = pack.lexical_order().cloned().rev();
                let mut new = Modified {
                    modifier: modifier.clone(),
                    operands: {
                        let mut ops: Vec<_> = branches
                            .by_ref()
                            .take(2)
                            .map(|w| w.map(Word::Func))
                            .collect();
                        ops.reverse();
                        ops
                    },
                    pack_expansion: true,
                };
                for branch in branches {
                    new = Modified {
                        modifier: modifier.clone(),
                        operands: vec![
                            branch.map(Word::Func),
                            span.clone().sp(Word::Modified(Box::new(new))),
                        ],
                        pack_expansion: true,
                    };
                }
                self.modified(new, subscript)
            }
            Modifier::Primitive(Primitive::On) => {
                let mut words = Vec::new();
                for branch in pack.lexical_order().cloned() {
                    let mut word = Word::Modified(Box::new(Modified {
                        modifier: modifier.clone(),
                        operands: vec![branch.clone().map(Word::Func)],
                        pack_expansion: true,
                    }));
                    if let Some(sub) = &subscript {
                        word = Word::Subscripted(Box::new(Subscripted {
                            script: sub.clone(),
                            word: branch.span.clone().sp(word),
                        }))
                    }
                    words.push(branch.span.sp(word));
                }
                self.words(words)
            }
            Modifier::Primitive(Primitive::Switch) => self.switch(
                (pack.lexical_order().cloned())
                    .map(|sp| sp.map(Word::Func))
                    .collect(),
                modifier.span.clone(),
            ),
            Modifier::Primitive(Primitive::Obverse) => {
                let mut nodes = Vec::new();
                let mut spans = Vec::new();
                for br in pack.lexical_order() {
                    let word = br.clone().map(Word::Func);
                    let span = word.span.clone();
                    let sn = self.word_sig(word)?;
                    nodes.push(sn);
                    spans.push(span);
                }
                let mut cust = CustomInverse {
                    is_obverse: true,
                    ..Default::default()
                };
                match nodes.as_slice() {
                    [a, b] => {
                        cust.normal = Ok(a.clone());
                        if a.sig == b.sig.inverse() {
                            cust.un = Some(b.clone());
                        } else if a.sig.anti().is_some_and(|sig| sig == b.sig) {
                            cust.anti = Some(b.clone());
                        } else {
                            cust.under = Some((a.clone(), b.clone()));
                        }
                    }
                    [a, b, c] => {
                        cust.normal = Ok(a.clone());
                        if !b.node.is_empty() && !c.node.is_empty() {
                            cust.under = Some((b.clone(), c.clone()));
                        }
                    }
                    [a, b, c, d] => {
                        cust.normal = Ok(a.clone());
                        if !b.node.is_empty() {
                            if !a.sig.is_compatible_with(b.sig.inverse()) {
                                self.emit_diagnostic(
                                    format!(
                                        "First and second functions must have \
                                        opposite signatures, \
                                        but their signatures are {} and {}",
                                        a.sig, b.sig
                                    ),
                                    DiagnosticKind::Warning,
                                    modifier.span.clone(),
                                );
                            }
                            cust.un = Some(b.clone());
                        }
                        if !d.node.is_empty() {
                            if !c.node.is_empty() {
                                cust.under = Some((c.clone(), d.clone()));
                            }
                            if a.sig.anti().is_some_and(|sig| sig == d.sig) {
                                cust.anti = Some(d.clone());
                            }
                        }
                    }
                    [a, b, c, d, e] => {
                        cust.normal = Ok(a.clone());
                        if !b.node.is_empty() {
                            if !a.sig.is_compatible_with(b.sig.inverse()) {
                                self.emit_diagnostic(
                                    format!(
                                        "First and second functions must have \
                                        opposite signatures, \
                                        but their signatures are {} and {}",
                                        a.sig, b.sig
                                    ),
                                    DiagnosticKind::Warning,
                                    modifier.span.clone(),
                                );
                            }
                            cust.un = Some(b.clone());
                        }
                        if !c.node.is_empty() && !d.node.is_empty() {
                            cust.under = Some((c.clone(), d.clone()));
                        }
                        if !e.node.is_empty() {
                            match a.sig.anti() {
                                None => self.emit_diagnostic(
                                    format!(
                                        "An anti inverse is specified, but the first \
                                        function's signature {} cannot have an \
                                        anti inverse",
                                        a.sig
                                    ),
                                    DiagnosticKind::Warning,
                                    modifier.span.clone(),
                                ),
                                Some(sig) if sig != e.sig => {
                                    self.emit_diagnostic(
                                        format!(
                                            "The first function's signature implies an \
                                            anti inverse with signature {sig}, but the \
                                            fifth function's signature is {}",
                                            e.sig
                                        ),
                                        DiagnosticKind::Warning,
                                        modifier.span.clone(),
                                    );
                                }
                                Some(_) => {}
                            }
                            cust.anti = Some(e.clone());
                        }
                    }
                    funcs => {
                        return Err(self.error(
                            modifier.span.clone(),
                            format!(
                                "{} requires between 1 and 5 branches, \
                                but {} were provided",
                                Primitive::Obverse.format(),
                                funcs.len()
                            ),
                        ))
                    }
                };
                let set_inverses = SetInverses {
                    un: cust.un.is_some(),
                    anti: cust.anti.is_some(),
                    under: cust.under.is_some(),
                };
                self.code_meta
                    .obverses
                    .insert(modifier.span.clone(), set_inverses);
                for span in spans {
                    if let Some(sig_decl) = self.code_meta.function_sigs.get_mut(&span) {
                        sig_decl.set_inverses = set_inverses;
                    }
                }
                let span = self.add_span(modifier.span.clone());
                Ok(Node::CustomInverse(cust.into(), span))
            }
            Modifier::Primitive(Primitive::Path) if pack.branches.len() == 3 => {
                let mut args = EcoVec::with_capacity(3);
                for branch in pack.lexical_order().cloned() {
                    args.push(self.word_sig(branch.map(Word::Func))?);
                }
                args.make_mut().swap(1, 2);
                let span = self.add_span(modifier.span.clone());
                Ok(Node::ImplMod(ImplPrimitive::Astar, args, span))
            }
            Modifier::Primitive(Primitive::Geometric) if pack.branches.len() == 2 => {
                let mut args = pack.lexical_order().cloned().map(|w| w.map(Word::Func));
                let main = args.next().unwrap();
                let metrics = args.next().unwrap();
                self.geometric(main, subscript, Some(metrics))
            }
            m if m.args() >= 2 => {
                let new = Modified {
                    modifier: modifier.clone(),
                    operands: pack
                        .lexical_order()
                        .cloned()
                        .map(|w| w.map(Word::Func))
                        .collect(),
                    pack_expansion: true,
                };
                self.modified(new, subscript)
            }
            m => {
                if let Modifier::Ref(name) = m {
                    if let Ok(Some((_, local))) = self.ref_local(name) {
                        if self.code_macros.contains_key(&local.index) {
                            return Ok(None);
                        }
                    }
                }
                return Err(self.error(
                    modifier.span.clone().merge(operand.span.clone()),
                    format!("{m} cannot use a function pack"),
                ));
            }
        }?;
        if let Some(boxes) = pack.is_array {
            let sig = self.sig_of(&node, span)?;
            let span = self.add_span(span.clone());
            if sig.outputs() == 2 && !boxes {
                node.push(Node::Prim(Primitive::Couple, span));
            } else if sig.outputs() != 0 {
                node = Node::Array {
                    len: sig.outputs(),
                    boxed: boxes,
                    inner: node.into(),
                    allow_ext: false,
                    prim: None,
                    span,
                }
            }
        }
        Ok(Some(node))
    }
    #[allow(clippy::collapsible_match)]
    pub(crate) fn modified(
        &mut self,
        mut modified: Modified,
        subscript: Option<Sp<Subscript>>,
    ) -> UiuaResult<Node> {
        let mut op_count = modified.code_operands().count();

        // De-sugar function pack
        if op_count == 1 {
            let operand = modified.code_operands().next().unwrap();
            if let Some(node) =
                self.desugar_function_pack(&modified.modifier, operand, subscript.clone())?
            {
                return Ok(node);
            }
        }

        if op_count < modified.modifier.value.args() {
            let missing = modified.modifier.value.args() - op_count;
            let span = modified.modifier.span.clone();
            for _ in 0..missing {
                modified.operands.push(span.clone().sp(Word::Func(Func {
                    signature: None,
                    lines: Vec::new(),
                    closed: false,
                })));
            }
            op_count = modified.code_operands().count();
        }
        if op_count == modified.modifier.value.args() {
            // Inlining
            if let Some(node) = self.inline_modifier(&modified, subscript)? {
                return Ok(node);
            }
        } else {
            let strict_args = match &modified.modifier.value {
                Modifier::Primitive(_) => true,
                Modifier::Macro(..) => false,
                Modifier::Ref(name) => self
                    .ref_local(name)?
                    .is_some_and(|(_, local)| self.index_macros.contains_key(&local.index)),
            };
            if strict_args {
                // Validate operand count
                return Err(self.error(
                    modified.modifier.span.clone(),
                    format!(
                        "{} requires {} function argument{}, but {} {} provided",
                        modified.modifier.value,
                        modified.modifier.value.args(),
                        if modified.modifier.value.args() == 1 {
                            ""
                        } else {
                            "s"
                        },
                        op_count,
                        if op_count == 1 { "was" } else { "were" }
                    ),
                ));
            }
        }

        // Handle macros
        let prim = match modified.modifier.value {
            Modifier::Primitive(prim) => prim,
            Modifier::Ref(r) => {
                return self.modifier_ref(r, modified.modifier.span, modified.operands)
            }
            Modifier::Macro(mac) => {
                return self.inline_macro(mac, modified.modifier.span, modified.operands);
            }
        };

        let span = self.add_span(modified.modifier.span.clone());

        // Compile operands
        let ops = self.args(modified.operands)?;

        Ok(Node::Mod(prim, ops, span))
    }
    fn suppress_diagnostics<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let diagnostics = take(&mut self.diagnostics);
        let print_diagnostics = take(&mut self.print_diagnostics);
        let res = f(self);
        self.diagnostics
            .retain(|d| d.kind >= DiagnosticKind::Warning);
        self.diagnostics.extend(diagnostics);
        self.print_diagnostics = print_diagnostics;
        res
    }
    fn monadic_modifier_op(&mut self, m: &Modified) -> UiuaResult<(SigNode, CodeSpan)> {
        let operand = m.code_operands().next().unwrap().clone();
        let span = operand.span.clone();
        self.word_sig(operand).map(|sn| (sn, span))
    }
    fn dyadic_modifier_ops(
        &mut self,
        m: &Modified,
    ) -> UiuaResult<(SigNode, SigNode, CodeSpan, CodeSpan)> {
        let mut operands = m.code_operands().cloned();
        let a_op = operands.next().unwrap();
        let b_op = operands.next().unwrap();
        let a_span = a_op.span.clone();
        let b_span = b_op.span.clone();
        let a = self.word_sig(a_op)?;
        let b = self.word_sig(b_op)?;
        Ok((a, b, a_span, b_span))
    }
    /// Inline a modifier
    pub(super) fn inline_modifier(
        &mut self,
        modified: &Modified,
        subscript: Option<Sp<Subscript>>,
    ) -> UiuaResult<Option<Node>> {
        use Primitive::*;
        let Modifier::Primitive(prim) = modified.modifier.value else {
            return Ok(None);
        };

        // Validation
        self.handle_primitive_experimental(prim, &modified.modifier.span);
        self.handle_primitive_deprecation(prim, &modified.modifier.span);

        Ok(Some(match prim {
            Gap => {
                let (SigNode { mut node, .. }, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                node.prepend(Node::Prim(Pop, span));
                node
            }
            On => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                if let Some(n) = subscript
                    .and_then(|sub| {
                        self.subscript_n_only(&sub, On)
                            .map(|n| self.positive_subscript(n, On, &sub.span))
                    })
                    .filter(|&n| n > 1)
                {
                    Node::ImplMod(ImplPrimitive::OnSub(n), eco_vec![sn], span)
                } else {
                    let prim = if sn.sig.args() == 0 { Dip } else { On };
                    Node::Mod(prim, eco_vec![sn], span)
                }
            }
            By => {
                let (mut sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                if let Some(n) = subscript
                    .and_then(|sub| {
                        self.subscript_n_only(&sub, On)
                            .map(|n| self.positive_subscript(n, On, &sub.span))
                    })
                    .filter(|&n| n > 1)
                {
                    if n == sn.sig.args() {
                        self.emit_diagnostic(
                            format!(
                                "Prefer {} over subscripted {} here",
                                Below.format(),
                                By.format()
                            ),
                            DiagnosticKind::Style,
                            modified.modifier.span.clone(),
                        )
                    }
                    Node::ImplMod(ImplPrimitive::BySub(n), eco_vec![sn], span)
                } else if sn.sig.args() == 0 {
                    sn.node.prepend(Node::Prim(Identity, span));
                    sn.node
                } else {
                    Node::Mod(By, eco_vec![sn], span)
                }
            }
            Reach => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                let mut node = sn.node;
                if let Some(side) = subscript.and_then(|sub| self.subscript_side_only(&sub, On)) {
                    match side {
                        SubSide::Left => {
                            node = Node::Mod(Dip, eco_vec![SigNode::new(sn.sig, node)], span);
                            node.prepend(Node::Prim(Flip, span));
                        }
                        SubSide::Right => {
                            node.prepend(Node::Prim(Pop, span));
                            node = Node::Mod(Off, eco_vec![node.sig_node().unwrap()], span);
                            node.prepend(Node::Prim(Flip, span));
                        }
                    }
                } else {
                    node.prepend(Node::Mod(
                        Dip,
                        eco_vec![SigNode::new((1, 0), Node::Prim(Pop, span))],
                        span,
                    ));
                }
                node
            }
            Fork => {
                let (f, g, f_span, _) = self.dyadic_modifier_ops(modified)?;
                if !modified.pack_expansion && f.node.as_primitive() == Some(Primitive::Identity) {
                    self.emit_diagnostic(
                        "Prefer `⟜` over `⊃∘` for clarity",
                        DiagnosticKind::Style,
                        modified.modifier.span.clone().merge(f_span),
                    );
                }
                let span = self.add_span(modified.modifier.span.clone());
                match (f.sig.args(), g.sig.args()) {
                    (0, _) => Node::from_iter([g.node, f.node]),
                    (1, 0) => Node::from_iter([Node::Mod(Dip, eco_vec![g], span), f.node]),
                    (1, _) => Node::from_iter([Node::Mod(On, eco_vec![g], span), f.node]),
                    _ => Node::Mod(Fork, eco_vec![f, g], span),
                }
            }
            Both => {
                let Some(sub) = subscript else {
                    return Ok(None);
                };
                let sub = self.validate_subscript(sub);
                let sub_span = sub.span;
                let mut sub = sub
                    .value
                    .map_num(|n| self.positive_subscript(n, Both, &sub_span) as u32);
                let span = self.add_span(modified.modifier.span.clone());
                let op = self.monadic_modifier_op(modified)?.0;
                if let Some(side) = &mut sub.side {
                    if let Some(side_n) = side.n {
                        if side_n > op.sig.args() {
                            self.add_error(
                                modified.modifier.span.clone().merge(sub_span),
                                format!(
                                    "Sided {}'s quantifier cannot be greater than its \
                                    function's arguments, but {} > {}",
                                    Primitive::Both.format(),
                                    side_n,
                                    op.sig.args()
                                ),
                            );
                            side.n = Some(op.sig.args());
                        }
                    }
                }
                Node::ImplMod(ImplPrimitive::BothImpl(sub), eco_vec![op], span)
            }
            Bracket => {
                let Some(sub) = subscript else {
                    return Ok(None);
                };
                let Some(side) = self.subscript_side_only(&sub, Bracket.format()) else {
                    return Ok(None);
                };
                let span = self.add_span(modified.modifier.span.clone());
                let (a, b, _, _) = self.dyadic_modifier_ops(modified)?;
                if a.sig.args() != 2 || b.sig.args() != 2 {
                    self.add_error(
                        modified.modifier.span.clone().merge(sub.span),
                        format!(
                            "Sided {}'s functions must both have 2 arguments, \
                            but their signatures are {} and {}.",
                            Primitive::Bracket.format(),
                            a.sig,
                            b.sig
                        ),
                    );
                    Node::Mod(Bracket, eco_vec![a, b], span)
                } else {
                    let sub_span = self.add_span(sub.span);
                    let mut node = match side {
                        SubSide::Left => Node::Mod(
                            On,
                            eco_vec![Node::Prim(Flip, sub_span).sig_node().unwrap()],
                            sub_span,
                        ),
                        SubSide::Right => Node::Mod(
                            Dip,
                            eco_vec![Node::Prim(Over, sub_span).sig_node().unwrap()],
                            sub_span,
                        ),
                    };
                    node.push(Node::Mod(Bracket, eco_vec![a, b], span));
                    node
                }
            }
            prim @ (With | Off) => {
                let (mut sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                let sig = sn.sig;
                let sub_n = subscript
                    .and_then(|sub| {
                        self.subscript_n_only(&sub, prim.format())
                            .map(|n| self.positive_subscript(n, prim, &sub.span))
                    })
                    .filter(|&n| n > 1);
                if sig.args() < 2 {
                    self.emit_diagnostic(
                        format!(
                            "The current behavior of {} with {} argument{} \
                            is deprecated and may be changed or removed in the future. \
                            Use {}, {}, or {} instead",
                            prim.format(),
                            sig.args(),
                            if sig.args() == 1 { "" } else { "s" },
                            Fork.format(),
                            Bracket.format(),
                            Below.format()
                        ),
                        DiagnosticKind::Warning,
                        modified.modifier.span.clone(),
                    );
                }
                let (inner, before) = match sn.sig.args() {
                    0 => (SigNode::new((2, 2), Node::Prim(Identity, span)), sn.node),
                    1 if prim == With => {
                        sn.sig.update_outputs(|o| o + 1);
                        sn.sig.update_args(|_| 2);
                        (sn, Node::empty())
                    }
                    1 if prim == Off && sub_n.is_none() => {
                        let mut outer_sig = sig;
                        outer_sig.update_outputs(|o| o + 1);
                        outer_sig.update_args(|_| 2);
                        let inner = Node::Mod(Dip, eco_vec![sn], span);
                        (SigNode::new(outer_sig, inner), Node::empty())
                    }
                    _ => (sn, Node::empty()),
                };
                Node::from_iter([
                    before,
                    if let Some(n) = sub_n {
                        let prim = if prim == Off {
                            if n == sig.args() {
                                self.emit_diagnostic(
                                    format!(
                                        "Prefer {} over subscripted {} here",
                                        Below.format(),
                                        Off.format()
                                    ),
                                    DiagnosticKind::Style,
                                    modified.modifier.span.clone(),
                                )
                            }
                            ImplPrimitive::OffSub(n)
                        } else {
                            ImplPrimitive::WithSub(n)
                        };
                        Node::ImplMod(prim, eco_vec![inner], span)
                    } else {
                        Node::Mod(prim, eco_vec![inner], span)
                    },
                ])
            }
            prim @ (Above | Below) => {
                let (mut sn, _) = self.monadic_modifier_op(modified)?;
                if sn.sig.args() < 2 {
                    self.emit_diagnostic(
                        format!(
                            "The current behavior of {} with < 2 arguments \
                            is deprecated and will change in the future",
                            prim.format(),
                        ),
                        DiagnosticKind::Warning,
                        modified.modifier.span.clone(),
                    );
                    sn.sig.update_args(|a| a + 1);
                    sn.sig.update_outputs(|o| o + 1);
                }
                let span = self.add_span(modified.modifier.span.clone());
                Node::Mod(prim, eco_vec![sn], span)
            }
            Slf => {
                let (SigNode { mut node, sig }, _) = self.monadic_modifier_op(modified)?;
                match sig.args() {
                    0 | 1 => {
                        self.add_error(
                            modified.modifier.span.clone(),
                            format!(
                                "{}'s function must take at least 2 arguments, \
                                but its signature is {sig}",
                                Slf.format()
                            ),
                        );
                        node
                    }
                    n => {
                        let span = self.add_span(modified.modifier.span.clone());
                        for _ in 0..n - 1 {
                            node.prepend(Node::Prim(Dup, span))
                        }
                        node
                    }
                }
            }
            Backward => {
                let (SigNode { mut node, sig }, _) = self.monadic_modifier_op(modified)?;
                match sig.args() {
                    2 => {
                        let span = self.add_span(modified.modifier.span.clone());
                        node.prepend(Node::Prim(Flip, span));
                        node
                    }
                    4 => {
                        let span = self.add_span(modified.modifier.span.clone());
                        node.prepend(Node::Mod(
                            Dip,
                            eco_vec![Node::Prim(Flip, span).sig_node().unwrap()],
                            span,
                        ));
                        node
                    }
                    _ => {
                        self.add_error(
                            modified.modifier.span.clone(),
                            format!(
                                "Currently, {}'s function must take 2 or 4 arguments, \
                                but its signature is {sig}",
                                Backward.format(),
                            ),
                        );
                        node
                    }
                }
            }
            Content => {
                let mut sn = self.monadic_modifier_op(modified)?.0;
                let span = self.add_span(modified.modifier.span.clone());
                sn.node.prepend(
                    Node::ImplPrim(ImplPrimitive::UnBox, span)
                        .sig_node()
                        .unwrap()
                        .on_all(sn.sig.args(), span)
                        .node,
                );
                sn.node
            }
            Evert => {
                let mut sn = self.monadic_modifier_op(modified)?.0;
                let span = self.add_span(modified.modifier.span.clone());
                let retropose = Node::ImplPrim(ImplPrimitive::Retropose, span)
                    .sig_node()
                    .unwrap();
                sn.node
                    .prepend(retropose.clone().on_all(sn.sig.args(), span).node);
                sn.node.push(retropose.on_all(sn.sig.outputs(), span).node);
                sn.node
            }
            Repeat => {
                let (sn, span) = self.monadic_modifier_op(modified)?;
                let spandex = self.add_span(modified.modifier.span.clone());
                let mut node = if let Some((inv, inv_sig)) = sn
                    .node
                    .un_inverse(&self.asm)
                    .ok()
                    .and_then(|inv| inv.sig().ok().map(|sig| (inv, sig)))
                    .filter(|(_, inv_sig)| sn.sig.is_compatible_with(*inv_sig))
                {
                    // If an inverse for repeat's function exists we use a special
                    // implementation that allows for negative repeatition counts
                    if sn.sig.inverse() != inv_sig {
                        self.add_error(
                            span,
                            format!(
                                "Repeated function's inverse must have \
                                the inverse signature, but their signatures \
                                are {} and {}",
                                sn.sig, inv_sig
                            ),
                        )
                    }
                    Node::ImplMod(
                        ImplPrimitive::RepeatWithInverse,
                        eco_vec![sn, SigNode::new(inv_sig, inv)],
                        spandex,
                    )
                } else {
                    Node::Mod(Primitive::Repeat, eco_vec![sn], spandex)
                };
                if let Some(n) =
                    subscript.and_then(|sub| self.subscript_n_only(&sub, Repeat.format()))
                {
                    node.prepend(Node::new_push(n));
                }
                node
            }
            Tuples => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                let inner_sig = sn.sig;
                let mut node = Node::Mod(Primitive::Tuples, eco_vec![sn], span);
                if let Some(n) = subscript.and_then(|sub| {
                    if inner_sig.args() != 2 {
                        self.add_error(
                            modified.modifier.span.clone().merge(sub.span.clone()),
                            format!(
                                "{} can only be subscripted if its function \
                                is dyadic, but the signature is {inner_sig}",
                                Primitive::Tuples.format()
                            ),
                        );
                    }
                    self.subscript_n_only(&sub, Tuples.format())
                }) {
                    node.prepend(Node::new_push(n));
                }
                node
            }
            Stencil => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                let inner_sig = sn.sig;
                let mut node = Node::Mod(Primitive::Stencil, eco_vec![sn], span);
                if let Some(n) = subscript.and_then(|sub| {
                    if inner_sig.args() != 1 {
                        self.add_error(
                            modified.modifier.span.clone().merge(sub.span.clone()),
                            format!(
                                "{} can only be subscripted if its function \
                                is monadic, but the signature is {inner_sig}",
                                Primitive::Stencil.format()
                            ),
                        );
                    }
                    self.subscript_n_only(&sub, Stencil.format())
                }) {
                    node.prepend(Node::new_push(n));
                }
                node
            }
            Un => {
                let (sn, span) = self.monadic_modifier_op(modified)?;
                invert::dbgln!("\n//////////////\n// begin UN //\n//////////////");
                self.add_span(span.clone());
                let mut normal = sn.un_inverse(&self.asm);
                if let Node::Prim(Pretty, _) = sn.node {
                    if let Err(e) = &mut normal {
                        *e = InversionError::Pretty;
                    }
                }
                invert::dbgln!("////////////\n// end UN //\n////////////\n");
                if let Ok(Node::CustomInverse(..)) = normal.as_ref().map(|sn| &sn.node) {
                    normal.unwrap().node
                } else {
                    let cust = CustomInverse {
                        normal,
                        un: Some(sn),
                        ..Default::default()
                    };
                    let span = self.add_span(modified.modifier.span.clone());
                    Node::CustomInverse(cust.into(), span)
                }
            }
            Anti => {
                let (sn, span) = self.monadic_modifier_op(modified)?;
                invert::dbgln!("\n////////////////\n// begin ANTI //\n////////////////");
                self.add_span(span.clone());
                let normal = sn.anti_inverse(&self.asm);
                let cust = CustomInverse {
                    normal,
                    anti: Some(sn),
                    ..Default::default()
                };
                let span = self.add_span(modified.modifier.span.clone());
                invert::dbgln!("//////////////\n// end ANTI //\n//////////////\n");
                Node::CustomInverse(cust.into(), span)
            }
            Under => {
                let (f, g, f_span, _) = self.dyadic_modifier_ops(modified)?;
                invert::dbgln!("\n/////////////////\n// begin UNDER //\n/////////////////");
                let normal = {
                    let (f_before, f_after) = f
                        .node
                        .under_inverse(g.sig, false, &self.asm)
                        .map_err(|e| self.error(f_span.clone(), e))?;
                    let mut node = f_before;
                    node.push(g.node.clone());
                    node.push(f_after);
                    let sig = self.sig_of(&node, &f_span)?;
                    SigNode::new(sig, node)
                };
                let span = self.add_span(modified.modifier.span.clone());
                let un = if normal.sig == (1, 1) || self.allow_experimental() {
                    let (f_before, f_after) = f
                        .node
                        .under_inverse(g.sig, true, &self.asm)
                        .map_err(|e| self.error(f_span.clone(), e))?;
                    (g.node.un_inverse(&self.asm).ok())
                        .map(|g_inv| -> UiuaResult<SigNode> {
                            let mut node = f_before;
                            node.push(g_inv);
                            node.push(f_after);
                            let sig = self.sig_of(&node, &f_span)?;
                            Ok(SigNode::new(sig, node))
                        })
                        .transpose()?
                } else {
                    let cust = CustomInverse::from(InversionError::UnUnderExperimental);
                    Some(SigNode::new(
                        normal.sig.inverse(),
                        Node::CustomInverse(cust.into(), span),
                    ))
                };
                let under = if normal.sig.args() == normal.sig.outputs() {
                    un.clone().map(|un| (normal.clone(), un))
                } else {
                    None
                };
                let cust = CustomInverse {
                    normal: Ok(normal),
                    un,
                    under,
                    ..Default::default()
                };
                let span = self.add_span(modified.modifier.span.clone());
                invert::dbgln!("///////////////\n// end UNDER //\n///////////////\n");
                Node::CustomInverse(cust.into(), span)
            }
            Obverse => {
                // Empty inverse case, where only one function is supplied
                let (sn, span) = self.monadic_modifier_op(modified)?;
                let spandex = self.add_span(span.clone());
                let un = if sn.sig.args() == sn.sig.outputs() {
                    Some(SigNode::new(sn.sig, Node::empty()))
                } else {
                    None
                };
                let mut cust = CustomInverse {
                    normal: Ok(sn.clone()),
                    un,
                    anti: None,
                    under: Some((sn.clone(), SigNode::default())),
                    is_obverse: true,
                };
                if sn.sig.anti() == Some(sn.sig) {
                    cust.anti = Some(sn.clone());
                }
                let set_inverses = SetInverses {
                    un: cust.un.is_some(),
                    anti: cust.anti.is_some(),
                    under: cust.under.is_some(),
                };
                self.code_meta
                    .obverses
                    .insert(modified.modifier.span.clone(), set_inverses);
                if let Some(sig_decl) = self.code_meta.function_sigs.get_mut(&span) {
                    sig_decl.set_inverses = set_inverses;
                }
                Node::CustomInverse(cust.into(), spandex)
            }
            Try => {
                let in_try = replace(&mut self.in_try, true);
                let nodes = self.dyadic_modifier_ops(modified);
                self.in_try = in_try;
                let (mut tried, mut handler, _, handler_span) = nodes?;

                let span = self.add_span(modified.modifier.span.clone());

                // Normalize noreturn tried function
                if tried.node.is_noreturn(&self.asm) {
                    tried.sig.update_args_outputs(|a, o| {
                        (
                            a.max(handler.sig.args().saturating_sub(1)),
                            o.max(handler.sig.outputs()),
                        )
                    });
                }

                // Handler must have at least as many outputs as tried
                if handler.sig.outputs() < tried.sig.outputs()
                    && !handler.node.is_noreturn(&self.asm)
                {
                    let diff = tried.sig.outputs() - handler.sig.outputs();
                    (handler.sig).update_args_outputs(|a, o| (a + diff, o + diff));
                }
                // Tried must have at least 1 arg less than handler
                if tried.sig.args() + 1 < handler.sig.args() {
                    // Tried must pop arguments that are only for the handler
                    let arg_diff = handler.sig.args() - tried.sig.args() - 1;
                    let mut pre =
                        SigNode::new((arg_diff, 0), eco_vec![Node::Prim(Pop, span); arg_diff]);
                    for _ in 0..tried.sig.args() {
                        pre = SigNode::new(
                            (pre.sig.args() + 1, pre.sig.outputs() + 1),
                            Node::Mod(Dip, eco_vec![pre], span),
                        );
                    }
                    tried.sig.update_args(|a| a + arg_diff);
                    tried.node.prepend(pre.node);
                } else if tried.sig.outputs() < handler.sig.outputs() {
                    let diff = handler.sig.outputs() - tried.sig.outputs();
                    tried.sig.update_args_outputs(|a, o| (a + diff, o + diff));
                }
                // Handler must have at least as many args as tried
                if handler.sig.args() < tried.sig.args() {
                    let arg_diff = tried.sig.args() - handler.sig.args();
                    if handler.sig.outputs() <= tried.sig.outputs() {
                        let output_diff = tried.sig.outputs() - handler.sig.outputs();
                        let diff_diff = arg_diff.saturating_sub(output_diff);
                        if diff_diff > 0 {
                            // Handler must pop arguments that are only for the tried
                            let mut pre = SigNode::new(
                                (diff_diff, 0),
                                eco_vec![Node::Prim(Pop, span); diff_diff],
                            );
                            for _ in 0..handler.sig.args() + arg_diff - diff_diff {
                                pre = SigNode::new(
                                    (pre.sig.args() + 1, pre.sig.outputs() + 1),
                                    Node::Mod(Dip, eco_vec![pre], span),
                                );
                            }
                            (handler.sig)
                                .update_args_outputs(|a, o| (a + arg_diff, o + output_diff));
                            handler.node.prepend(pre.node);
                        } else {
                            (handler.sig).update_args_outputs(|a, o| (a + arg_diff, o + arg_diff));
                        }
                    } else {
                        (handler.sig).update_args_outputs(|a, o| (a + arg_diff, o + arg_diff));
                    }
                }
                if handler.sig.args() == tried.sig.args()
                    && handler.sig.outputs() + 1 == tried.sig.outputs()
                {
                    handler.sig.update_args_outputs(|a, o| (a + 1, o + 1));
                }

                if handler.sig.args() > tried.sig.args() + 1 {
                    self.add_error(
                        handler_span.clone(),
                        format!(
                            "Handler function must have at most \
                            one more argument than the tried function, \
                            but their signatures are {} and \
                            {} respectively.",
                            handler.sig, tried.sig
                        ),
                    );
                }

                Node::Mod(Primitive::Try, eco_vec![tried, handler], span)
            }
            Switch => self.switch(
                modified.code_operands().cloned().collect(),
                modified.modifier.span.clone(),
            )?,
            Fill => {
                let mut operands = modified.code_operands().rev().cloned();

                // Filled function
                let mode = replace(&mut self.pre_eval_mode, PreEvalMode::Lsp);
                let f = self.word_sig(operands.next().unwrap());
                self.pre_eval_mode = mode;
                let f = f?;

                // Get-fill function
                let fill_word = operands.next().unwrap();
                let fill_span = fill_word.span.clone();
                let fill = self.word_sig(fill_word)?;
                if fill.sig.outputs() > 1 && !self.scope.fill_sig_error {
                    self.scope.fill_sig_error = true;
                    self.add_error(
                        fill_span,
                        format!(
                            "{} function can have at most 1 output, but its signature is {}",
                            Primitive::Fill.format(),
                            fill.sig
                        ),
                    );
                }
                let span = self.add_span(modified.modifier.span.clone());
                if let Some(side) = subscript.and_then(|sub| {
                    self.experimental_error_it(&sub.span, || format!("Sided {}", Fill.format()));
                    self.subscript_side_only(&sub, Fill.format())
                }) {
                    Node::ImplMod(ImplPrimitive::SidedFill(side), eco_vec![fill, f], span)
                } else {
                    Node::Mod(Primitive::Fill, eco_vec![fill, f], span)
                }
            }
            Comptime => {
                let word = modified.code_operands().next().unwrap().clone();
                self.do_comptime("comptime's", word, &modified.modifier.span)?
            }
            Each => {
                // Each pervasive
                let operand = modified.code_operands().next().unwrap().clone();
                let op_span = operand.span.clone();
                let full_span = modified.modifier.span.clone().merge(op_span);
                let words_look_pervasive = subscript
                    .as_ref()
                    .is_none_or(|sub| sub.value.num == Some(NumericSubscript::N(0)))
                    && words_look_pervasive(slice::from_ref(&operand));
                let sn = self.word_sig(operand)?;
                if words_look_pervasive {
                    self.emit_diagnostic(
                        if let Some((prim, _)) = sn
                            .node
                            .as_flipped_primitive()
                            .filter(|(prim, _)| prim.class().is_pervasive())
                        {
                            format!(
                                "{} is pervasive, so {} is redundant here.",
                                prim.format(),
                                Each.format(),
                            )
                        } else {
                            format!(
                                "{m}'s function is pervasive, \
                                so {m} is redundant here.",
                                m = Each.format(),
                            )
                        },
                        DiagnosticKind::Advice,
                        full_span.clone(),
                    );
                }
                let span = self.add_span(modified.modifier.span.clone());
                if let Some((nos, nos_span)) = subscript
                    .and_then(|sub| {
                        self.subscript_n_or_side(&sub, Each.format())
                            .map(|n| (n, sub.span))
                    })
                    .filter(|&(n, _)| n != 0)
                {
                    if nos == SubNOrSide::N(-1) {
                        Node::Mod(Rows, eco_vec![sn], span)
                    } else {
                        match nos {
                            SubNOrSide::N(n) => {
                                Node::ImplMod(ImplPrimitive::EachSub(n), eco_vec![sn], span)
                            }
                            SubNOrSide::Side(side) => {
                                self.experimental_error_it(&nos_span, || {
                                    format!("Sided {}", Primitive::Each.format())
                                });
                                let sub_span = self.add_span(nos_span);
                                let mut node = match side {
                                    SubSide::Left => Node::Prim(Fix, sub_span),
                                    SubSide::Right => match sn.sig.args() {
                                        0 => Node::empty(),
                                        1 => Node::Prim(Fix, sub_span),
                                        n => {
                                            let mut node = Node::Prim(Fix, sub_span);
                                            for _ in 1..n {
                                                node = Node::Mod(
                                                    Dip,
                                                    eco_vec![node.sig_node().unwrap()],
                                                    sub_span,
                                                );
                                            }
                                            node
                                        }
                                    },
                                };
                                node.push(Node::Mod(Each, eco_vec![sn], span));
                                node
                            }
                        }
                    }
                } else {
                    Node::Mod(Each, eco_vec![sn], span)
                }
            }
            prim @ (Rows | Inventory) => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                let Some(sub) = subscript else {
                    return Ok(None);
                };
                let sub = self.validate_subscript(sub);
                let sub_span = sub.span;
                let mut sub = sub.value;
                if let Some(n) = &mut sub.num {
                    if n.abs() > 10 {
                        self.add_error(
                            sub_span.clone(),
                            format!("{} max subscript magnitude is 10", prim.format()),
                        );
                        *n = n.signum() * 10;
                    }
                }
                if let Some(side) = sub.side {
                    if let Some(n) = side.n {
                        if n >= sn.sig.args() {
                            self.emit_diagnostic(
                                format!(
                                    "Specifying {n} fixed arrays for a function \
                                    with signature {} is probably not what you want",
                                    sn.sig,
                                ),
                                DiagnosticKind::Advice,
                                sub_span.clone(),
                            );
                        }
                    }
                }
                Node::ImplMod(
                    ImplPrimitive::RowsSub(sub, prim == Inventory),
                    eco_vec![sn],
                    span,
                )
            }
            Table => {
                // Normal table compilation, but get some diagnostics
                let (sn, span) = self.monadic_modifier_op(modified)?;
                match sn.sig.args() {
                    0 => self.emit_diagnostic(
                        format!("{} of 0 arguments is redundant", Table.format()),
                        DiagnosticKind::Advice,
                        span,
                    ),
                    1 => self.emit_diagnostic(
                        format!(
                            "{} with 1 argument is just {rows}. \
                            Use {rows} instead.",
                            Table.format(),
                            rows = Rows.format()
                        ),
                        DiagnosticKind::Advice,
                        span,
                    ),
                    _ => {}
                }

                fn table_fork(sn: SigNode, table_span: usize, asm: &Assembly) -> Node {
                    match sn.node {
                        Node::Mod(Fork, args, fork_span)
                            if (args.iter()).all(|arg| arg.node.is_pure(Purity::Pure, asm))
                                && args.windows(2).all(|w| w[0].sig.args() == w[1].sig.args()) =>
                        {
                            let args: EcoVec<SigNode> = args
                                .into_iter()
                                .map(|arg| SigNode::new(arg.sig, table_fork(arg, table_span, asm)))
                                .collect();
                            Node::Mod(Fork, args, fork_span)
                        }
                        node => Node::Mod(Table, eco_vec![SigNode::new(sn.sig, node)], table_span),
                    }
                }
                let table_span = self.add_span(modified.modifier.span.clone());
                table_fork(sn, table_span, &self.asm)
            }
            Fold => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                if sn.sig.args() < sn.sig.outputs() {
                    self.experimental_error(&modified.modifier.span, || {
                        format!(
                            "{} with arguments < outputs is experimental. To use it, \
                            add `# Experimental!` to the top of the file.",
                            prim.format()
                        )
                    });
                }
                let span = self.add_span(modified.modifier.span.clone());
                Node::Mod(Fold, eco_vec![sn], span)
            }
            prim @ (Spawn | Pool) => {
                let recurses_before = self
                    .current_bindings
                    .last()
                    .map(|curr| curr.recurses)
                    .unwrap_or(0);
                let (sn, span) = self.monadic_modifier_op(modified)?;
                if let Some(curr) = self.current_bindings.last() {
                    if curr.recurses > recurses_before {
                        self.add_error(span, format!("Cannot {prim} recursive function"))
                    }
                }
                let span = self.add_span(modified.modifier.span.clone());
                Node::Mod(prim, eco_vec![sn], span)
            }
            Quote => {
                let operand = modified.code_operands().next().unwrap().clone();
                let node = self.do_comptime("quote's", operand, &modified.modifier.span)?;
                let code: String = match node {
                    Node::Push(Value::Char(chars)) if chars.rank() == 1 => {
                        chars.data.iter().collect()
                    }
                    Node::Push(Value::Char(chars)) => {
                        return Err(self.error(
                            modified.modifier.span.clone(),
                            format!(
                                "quote's argument compiled to a \
                                rank {} array rather than a string",
                                chars.rank()
                            ),
                        ))
                    }
                    Node::Push(value) => {
                        return Err(self.error(
                            modified.modifier.span.clone(),
                            format!(
                                "quote's argument compiled to a \
                                {} array rather than a string",
                                value.type_name()
                            ),
                        ))
                    }
                    _ => {
                        return Err(self.error(
                            modified.modifier.span.clone(),
                            "quote's argument did not compile to a string",
                        ));
                    }
                };
                self.quote(&code, Some("quote".into()), &modified.modifier.span)?
            }
            Derivative => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                self.add_span(modified.modifier.span.clone());
                match derivative(&sn.node, &self.asm) {
                    Ok(node) => node,
                    Err(e) => {
                        self.add_error(
                            modified.modifier.span.clone(),
                            format!("Cannot differentiate. {e}"),
                        );
                        sn.node
                    }
                }
            }
            Integral => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                self.add_span(modified.modifier.span.clone());
                match integral(&sn.node, &self.asm) {
                    Ok(node) => node,
                    Err(e) => {
                        self.add_error(
                            modified.modifier.span.clone(),
                            format!("Cannot integrate. {e}"),
                        );
                        sn.node
                    }
                }
            }
            Geometric => {
                let op = modified.code_operands().next().unwrap().clone();
                self.geometric(op, subscript, None)?
            }
            _ => return Ok(None),
        }))
    }
    // Compile an inline macro
    fn inline_macro(
        &mut self,
        mac: InlineMacro,
        span: CodeSpan,
        operands: Vec<Sp<Word>>,
    ) -> UiuaResult<Node> {
        // Track
        self.code_meta
            .inline_macros
            .insert(mac.func.span.clone(), ident_modifier_args(&mac.ident.value));
        Ok(if mac.caret_span.is_some() {
            let root = self.func(mac.func.value, mac.func.span.clone())?;
            let sig = self.sig_of(&root, &mac.func.span)?;
            let code_mac = CodeMacro {
                root: SigNode::new(sig, root),
                names: Default::default(),
            };
            self.code_macro(None, span, operands, code_mac)?
        } else {
            // Expand
            let items = mac.func.value.lines;
            let mut words = Vec::new();
            let mut errored = false;
            for item in items.into_iter().rev() {
                match item {
                    Item::Words(ws) => words.extend(ws),
                    item => {
                        if !errored {
                            self.add_error(
                                item.span().unwrap_or_else(|| span.clone()),
                                format!("Macro cannot contain {}s", item.kind_str()),
                            );
                            errored = true;
                        }
                    }
                }
            }
            self.expand_index_macro(None, &mut words, operands, span.clone(), false)?;
            // Compile
            let node = self.suppress_diagnostics(|comp| comp.words(words))?;
            // Add
            let sig = self.sig_of(&node, &span)?;
            let func = (self.asm).add_function(FunctionId::Macro(None, span.clone()), sig, node);
            let span = self.add_span(span);
            Node::Call(func, span)
        })
    }
    fn modifier_ref(
        &mut self,
        r: Ref,
        modifier_span: CodeSpan,
        operands: Vec<Sp<Word>>,
    ) -> UiuaResult<Node> {
        let Some((path_locals, local)) = self.ref_local(&r)? else {
            return Ok(Node::empty());
        };
        self.validate_local(&r.name.value, local, &r.name.span);
        self.code_meta
            .global_references
            .insert(r.name.span.clone(), local.index);
        for (local, comp) in path_locals.into_iter().zip(&r.path) {
            (self.code_meta.global_references).insert(comp.module.span.clone(), local.index);
        }
        // Handle recursion depth
        self.comptime_depth += 1;
        if self.comptime_depth > MAX_COMPTIME_DEPTH {
            return Err(self.error(
                modifier_span.clone(),
                "Macro makes compilation recur too deep",
            ));
        }
        let node = if let Some(mac) = self.index_macros.get(&local.index).cloned() {
            // Index macros
            self.index_macro(mac, operands, r.name, local, modifier_span)?
        } else if let Some(mac) = self.code_macros.get(&local.index).cloned() {
            // Code macros
            self.code_macro(Some(r.name.value), modifier_span, operands, mac)?
        } else if let Some(m) =
            (self.asm.bindings.get(local.index)).and_then(|binfo| match &binfo.kind {
                BindingKind::Module(m) => Some(m),
                BindingKind::Import(path) => self.imports.get(path),
                _ => None,
            })
        {
            // Module import macro
            let names = m.names.clone();
            let (_, node) = self.in_scope(ScopeKind::AllInModule, move |comp| {
                comp.scope.names.extend(names);
                comp.words(operands)
            })?;
            node
        } else {
            Node::empty()
        };
        self.comptime_depth -= 1;
        Ok(node)
    }
    fn index_macro(
        &mut self,
        mut mac: IndexMacro,
        operands: Vec<Sp<Word>>,
        name: Sp<Ident>,
        local: LocalName,
        ref_span: CodeSpan,
    ) -> UiuaResult<Node> {
        let span = self.add_span(ref_span.clone());
        Ok(match self.scope.kind {
            ScopeKind::Temp(Some(mac_local)) if mac_local.macro_index == local.index => {
                // Recursive
                if let Some(sig) = mac.sig {
                    Node::CallMacro {
                        index: mac_local.expansion_index,
                        sig,
                        span,
                    }
                } else {
                    Node::empty()
                }
            }
            _ => {
                // Expand
                self.expand_index_macro(
                    Some(name.value.clone()),
                    &mut mac.words,
                    operands,
                    ref_span.clone(),
                    true,
                )?;
                // Handle recursion
                // Recursive macros work by creating a binding for the expansion.
                // Recursive calls then call that binding.
                // We know that this is a recursive call if the scope tracks
                // a macro with the same index.
                let macro_local = mac.recursive.then(|| {
                    let expansion_index = self.next_global;
                    let count = ident_modifier_args(&name.value);
                    // Add temporary binding
                    self.asm.add_binding_at(
                        LocalName {
                            index: expansion_index,
                            public: false,
                        },
                        BindingKind::IndexMacro(count),
                        Some(ref_span.clone()),
                        BindingMeta::default(),
                    );
                    self.next_global += 1;
                    MacroLocal {
                        macro_index: local.index,
                        expansion_index,
                    }
                });
                // Compile
                let node = self.suppress_diagnostics(|comp| {
                    comp.temp_scope(mac.names, macro_local, |comp| comp.words(mac.words))
                })?;
                // Add
                let sig = self.sig_of(&node, &ref_span)?;
                let id = FunctionId::Macro(Some(name.value), name.span);
                let func = self.asm.add_function(id, sig, node);
                if let Some(macro_local) = macro_local {
                    self.asm.bindings.make_mut()[macro_local.expansion_index].kind =
                        BindingKind::Func(func.clone());
                }
                Node::Call(func, span)
            }
        })
    }
    fn code_macro(
        &mut self,
        mac_name: Option<Ident>,
        modifier_span: CodeSpan,
        operands: Vec<Sp<Word>>,
        mac: CodeMacro,
    ) -> UiuaResult<Node> {
        let full_span = (modifier_span.clone()).merge(operands.last().unwrap().span.clone());
        // Collect operands as strings
        let mut operands: Vec<Sp<Word>> = (operands.into_iter())
            .filter(|w| w.value.is_code())
            .collect();
        if operands.len() == 1 {
            let operand = operands.remove(0);
            operands = match operand.value {
                Word::Pack(pack) => pack
                    .into_lexical_order()
                    .map(|b| b.map(Word::Func))
                    .collect(),
                word => vec![operand.span.sp(word)],
            };
        }
        let op_sigs = if mac.root.sig.args() == 2 {
            // If the macro function has 2 arguments, we pass the signatures
            // of the operands as well
            let mut sig_data: EcoVec<u8> = EcoVec::with_capacity(operands.len() * 2);
            // Track the length of the instructions and spans so
            // they can be discarded after signatures are calculated
            for op in &operands {
                let sn = self.word_sig(op.clone()).map_err(|e| {
                    let message = format!(
                        "This error occurred while compiling a macro operand. \
                        This was attempted because the macro function's \
                        signature is {}.",
                        Signature::new(2, 1)
                    );
                    e.with_info([(message, None)])
                })?;
                sig_data.extend_from_slice(&[sn.sig.args() as u8, sn.sig.outputs() as u8]);
            }
            // Discard unnecessary instructions and spans
            Some(Array::<u8>::new([operands.len(), 2], sig_data))
        } else {
            None
        };
        let formatted: Array<Boxed> = operands
            .iter()
            .map(|w| {
                let mut formatted = format_word(w, &self.asm.inputs);
                if let Word::Func(_) = &w.value {
                    if formatted.starts_with('(') && formatted.ends_with(')') {
                        formatted = formatted[1..formatted.len() - 1].to_string();
                    }
                }
                Boxed(formatted.trim().into())
            })
            .collect();

        let mut code: Option<String> = None;
        (|| -> UiuaResult {
            if let Some(index) = self.node_unbound_index(&mac.root.node) {
                let name = self.scope.names.iter().find_map(|(name, local)| {
                    if local.index == index {
                        Some(name)
                    } else {
                        None
                    }
                });
                let message = match (&mac_name, name) {
                    (Some(mac_name), Some(name)) => {
                        format!("{} references runtime binding `{}`", mac_name, name)
                    }
                    (Some(mac_name), None) => format!("{} references runtime binding", mac_name),
                    (None, Some(name)) => format!("macro references runtime binding `{}`", name),
                    (None, None) => "macro references runtime binding".into(),
                };
                return Err(self.error(modifier_span.clone(), message));
            }

            let span = self.add_span(modifier_span.clone());
            let env = &mut self.macro_env;
            swap(&mut env.asm, &mut self.asm);
            env.rt.call_stack.last_mut().unwrap().call_span = span;

            // Run the macro function
            if let Some(sigs) = op_sigs {
                env.push(sigs);
            }
            env.push(formatted);

            let enabled = env
                .rt
                .backend
                .set_output_enabled(self.pre_eval_mode != PreEvalMode::Lsp);

            let res = (|| -> UiuaResult {
                env.exec(mac.root)?;

                let val = env.pop("macro result")?;

                // Parse the macro output
                let strings = match val
                    .as_strings(env, "Code macro output must be a string or list of strings")
                {
                    Ok(strings) => strings,
                    Err(_) => val.representation().lines().map(Into::into).collect(),
                };
                let code = code.get_or_insert_with(String::new);
                for s in strings {
                    if code.chars().last().is_some_and(|c| !c.is_whitespace()) {
                        code.push(' ');
                    }
                    code.push_str(&s);
                }
                Ok(())
            })();

            if let Err(e) = res {
                self.errors.push(e);
            }

            env.rt.backend.set_output_enabled(enabled);

            swap(&mut env.asm, &mut self.asm);
            Ok(())
        })()
        .map_err(|e| e.trace_macro(mac_name.clone(), modifier_span.clone()))?;

        // Quote
        if let Some(code) = code {
            self.code_meta
                .macro_expansions
                .insert(full_span, (mac_name.clone(), code.clone()));
            self.suppress_diagnostics(|comp| {
                comp.temp_scope(mac.names, None, |comp| {
                    comp.quote(&code, mac_name, &modifier_span)
                })
            })
        } else {
            Ok(Node::empty())
        }
    }
    fn node_unbound_index(&self, node: &Node) -> Option<usize> {
        match node {
            Node::Run(nodes) => nodes.iter().find_map(|node| self.node_unbound_index(node)),
            Node::CallGlobal(index, _)
                if (self.asm.bindings.get(*index))
                    .is_none_or(|binding| matches!(binding.kind, BindingKind::Const(None)))
                    && !self.macro_env.rt.unevaluated_constants.contains_key(index) =>
            {
                Some(*index)
            }
            Node::Call(func, _) => self.node_unbound_index(&self.asm[func]),
            Node::CustomInverse(cust, _) => cust
                .nodes()
                .find_map(|sn| self.node_unbound_index(&sn.node)),
            Node::Mod(_, ops, _) | Node::ImplMod(_, ops, _) => {
                ops.iter().find_map(|sn| self.node_unbound_index(&sn.node))
            }
            Node::Switch { branches, .. } => branches
                .iter()
                .find_map(|branch| self.node_unbound_index(&branch.node)),
            Node::Array { inner, .. } => self.node_unbound_index(inner),
            Node::NoInline(node) | Node::TrackCaller(node) => self.node_unbound_index(node),
            _ => None,
        }
    }
    /// Expand a index macro
    fn expand_index_macro(
        &mut self,
        name: Option<Ident>,
        macro_words: &mut Vec<Sp<Word>>,
        mut operands: Vec<Sp<Word>>,
        span: CodeSpan,
        hygenic: bool,
    ) -> UiuaResult {
        // Mark the operands as macro arguments
        if hygenic {
            set_in_macro_arg(&mut operands);
        }
        let span = span.merge(operands.last().unwrap().span.clone());
        let operands: Vec<Sp<Word>> = operands.into_iter().filter(|w| w.value.is_code()).collect();
        self.replace_placeholders(macro_words, &operands)?;
        // Format and store the expansion for the LSP
        let formatted = format_words(&*macro_words, &self.asm.inputs);
        (self.code_meta.macro_expansions).insert(span, (name, formatted));
        Ok(())
    }
    fn replace_placeholders(&self, words: &mut Vec<Sp<Word>>, initial: &[Sp<Word>]) -> UiuaResult {
        let mut error = None;
        recurse_words_mut(words, &mut |word| match &mut word.value {
            Word::Placeholder(n) => {
                if let Some(replacement) = initial.get(*n) {
                    *word = replacement.clone();
                } else {
                    error = Some(self.error(
                        word.span.clone(),
                        format!(
                            "Placeholder index {n} is out of bounds of {} operand{}",
                            initial.len(),
                            if initial.len() == 1 { "" } else { "s" }
                        ),
                    ))
                }
            }
            _ => {}
        });
        words.retain(|word| !matches!(word.value, Word::Placeholder(_)));
        error.map_or(Ok(()), Err)
    }
    fn quote(&mut self, code: &str, name: Option<Ident>, span: &CodeSpan) -> UiuaResult<Node> {
        let (items, errors, _) = parse(
            code,
            InputSrc::Macro(span.clone().into()),
            &mut self.asm.inputs,
        );
        if !errors.is_empty() {
            return Err(UiuaErrorKind::Parse(errors, self.asm.inputs.clone().into())
                .error()
                .trace_macro(name, span.clone()));
        }

        let root_node_len = self.asm.root.len();
        // Compile the generated items
        let temp_mode = self.pre_eval_mode.min(PreEvalMode::Line);
        let pre_eval_mod = replace(&mut self.pre_eval_mode, temp_mode);
        self.comptime_depth += 1;
        if self.comptime_depth > MAX_COMPTIME_DEPTH {
            return Err(self.error(span.clone(), "Compile-time evaluation recurs too deep"));
        }
        let errors_before = self.errors.len();
        let res = self
            .items(items, ItemCompMode::CodeMacro)
            .map_err(|e| e.trace_macro(name, span.clone()));
        let errors_after = self.errors.len();
        self.comptime_depth -= 1;
        self.pre_eval_mode = pre_eval_mod;
        // Extract generated root node
        if root_node_len >= self.asm.root.len() {
            return Ok(Node::empty());
        }
        let mut node = self.asm.root.split_off(root_node_len);
        res?;
        if errors_after > errors_before {
            node = Node::empty();
        }
        Ok(node)
    }
    fn do_comptime(
        &mut self,
        possesive: &str,
        operand: Sp<Word>,
        span: &CodeSpan,
    ) -> UiuaResult<Node> {
        self.do_comptime_vals(possesive, operand, span)
            .map(|(values, _)| values.into_iter().map(Node::new_push).collect())
    }
    fn do_comptime_vals(
        &mut self,
        possesive: &str,
        operand: Sp<Word>,
        span: &CodeSpan,
    ) -> UiuaResult<(Vec<Value>, Signature)> {
        let orig_spans_len = self.asm.spans.len();
        let sn = self.word_sig(operand)?;
        if sn.sig.args() > 0 {
            return Err(self.error(
                span.clone(),
                format!(
                    "{possesive} function must have no arguments, \
                    but its signature is {}",
                    sn.sig
                ),
            ));
        }
        if let Some(index) = self.node_unbound_index(&sn.node) {
            let name = (self.scope.names.iter())
                .find_map(|(ident, local)| (local.index == index).then_some(ident));
            return Err(if let Some(name) = name {
                let name_span = self.asm.bindings[index].span.clone();
                self.error(
                    span.clone(),
                    format!("Compile-time evaluation references runtime binding `{name}`"),
                )
                .with_info([(
                    format!("`{name}` is bound at runtime"),
                    Some(name_span.into()),
                )])
            } else {
                self.error(
                    span.clone(),
                    "Compile-time evaluation references runtime binding",
                )
            });
        }
        let root = replace(&mut self.asm.root, sn.node);
        let res = self.macro_env.run_asm(self.asm.clone());
        let mut stack = self.macro_env.take_stack();
        let values = if let Err(e) = res {
            if self.errors.is_empty() {
                self.errors.push(e.with_info([(
                    "Compile-time evaluation failed".into(),
                    Some(span.clone().into()),
                )]));
            }
            vec![Value::default(); sn.sig.outputs()]
        } else {
            stack.reverse();
            stack.truncate(sn.sig.outputs());
            stack.reverse();
            stack
        };
        self.asm.spans.truncate(orig_spans_len);
        self.asm.root = root;
        Ok((values, sn.sig))
    }
    /// Run a function in a temporary scope with the given names.
    /// Newly created bindings will be added to the current scope after the function is run.
    fn temp_scope<T>(
        &mut self,
        names: IndexMap<Ident, LocalName>,
        macro_local: Option<MacroLocal>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let orig_names = names.clone();
        // Create temp scope
        let temp_scope = Scope {
            kind: ScopeKind::Temp(macro_local),
            names,
            experimental: self.scope.experimental,
            experimental_error: self.scope.experimental_error,
            ..Default::default()
        };
        // Run function in temp scope
        self.higher_scopes
            .push(replace(&mut self.scope, temp_scope));
        let res = f(self);
        let mut replaced_scope = self.higher_scopes.pop().unwrap();
        // If temp scope has a new name, or if its binding index changed,
        // then it is a new binding and should be added to the current scope
        for (name, local) in self.scope.names.drain(..) {
            if orig_names.get(&name).is_none_or(|l| l.index != local.index) {
                replaced_scope.names.insert(name, local);
            }
        }
        self.scope = replaced_scope;
        res
    }
    fn geometric(
        &mut self,
        word: Sp<Word>,
        subscript: Option<Sp<Subscript>>,
        metrics: Option<Sp<Word>>,
    ) -> UiuaResult<Node> {
        let span = word.span.clone();
        let mut node = self.word(word)?;

        let mut met = ga::Metrics::VANILLA;
        if let Some(metrics) = metrics {
            let metrics_span = metrics.span.clone();
            let (vals, sig) = self.do_comptime_vals(
                &format!("{}'s second ", Primitive::Geometric.format()),
                metrics,
                &metrics_span,
            )?;
            if vals.len() == 1 {
                match ga::metrics_from_val(&vals[0]) {
                    Ok(metrics) => met = metrics,
                    Err(e) => self.add_error(metrics_span, e),
                }
            } else {
                self.add_error(
                    metrics_span,
                    format!(
                        "{}'s second function must have exactly one output, \
                        but its signature is {sig}",
                        Primitive::Geometric.format()
                    ),
                );
            }
        }
        let spec = if let Some(sub) = subscript {
            let sub = self.validate_subscript(sub);
            let dims = sub.value.num.map(|n| {
                let mut n = self.positive_subscript(n, Primitive::Geometric, &sub.span);
                const MAX_DIMS: usize = ga::MAX_DIMS as usize;
                if n > MAX_DIMS {
                    self.add_error(
                        sub.span.clone(),
                        format!("Max geometric algebra dimensions is currently {MAX_DIMS}"),
                    );
                    n = MAX_DIMS;
                }
                n as u8
            });
            if sub.value.side.is_some() {
                self.add_error(
                    sub.span,
                    format!(
                        "{} does not support side subscripts",
                        Primitive::Geometric.format()
                    ),
                );
            }
            Spec { dims, metrics: met }
        } else {
            Spec::default()
        };
        self.translate_geo(&mut node, spec, &span);
        Ok(node)
    }
    #[allow(unused_parens)]
    fn translate_geo(&mut self, node: &mut Node, spec: ga::Spec, span: &CodeSpan) -> bool {
        use {ImplPrimitive::*, Node::*, Primitive::*};
        match *node {
            Prim(Mul, span) => *node = ImplPrim(GeometricProduct(spec), span),
            Prim(Div, span) => *node = ImplPrim(GeometricDivide, span),
            Prim(Abs, span) => *node = ImplPrim(GeometricMagnitude(spec), span),
            Prim(Sign, span) => *node = ImplPrim(GeometricNormalize(spec), span),
            Prim(Sqrt, span) => *node = ImplPrim(GeometricSqrt(spec), span),
            Prim(Neg, span) => *node = ImplPrim(GeometricReverse(spec), span),
            Prim(Add, span) => *node = ImplPrim(GeometricAdd(spec), span),
            Prim(Sub, span) => *node = ImplPrim(GeometricSub(spec), span),
            Prim(Select, span) => *node = ImplPrim(ExtractBlades(spec), span),
            ImplPrim(AntiSelect, span) => *node = ImplPrim(PadBlades(spec), span),
            ImplPrim(prim, _) if prim.is_ga() => {}
            Prim(Identity | Dup | Flip | Over | Pop | Stack | Sys(_), _) => {}
            ImplPrim(StackN { .. }, _) => {}
            Push(_) => {}
            Mod(
                (Fork | Bracket | Both)
                | (Dip | Gap | Reach)
                | (On | By | With | Off)
                | (Above | Below)
                | (Slf | Backward),
                ..,
            ) => {}
            ImplMod(BothImpl(_), ..) => {}
            Run(_) | CustomInverse(..) | PushUnder(..) | PopUnder(..) | CopyToUnder(..) => {}
            Prim(prim, span) => {
                let span = self.get_span(span);
                let message = format!("{} does not support {}", Geometric.format(), prim.format());
                self.add_error(span, message);
                return false;
            }
            ImplPrim(prim, span) => {
                let span = self.get_span(span);
                let message = format!("{} does not support {prim}", Geometric.format());
                self.add_error(span, message);
                return false;
            }
            ref mut node => {
                let span = if let Some(spandex) = node.span() {
                    self.get_span(spandex)
                        .code()
                        .unwrap_or_else(|| span.clone())
                } else {
                    span.clone()
                };
                let message = format!("{} does not support {node:?}", Geometric.format());
                self.add_error(span, message);
                return false;
            }
        }
        for node in node.sub_nodes_mut() {
            if !self.translate_geo(node, spec, span) {
                return false;
            }
        }
        true
    }
}
