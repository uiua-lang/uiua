//! Compiler code for modifiers
#![allow(clippy::redundant_closure_call)]

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
        match &modifier.value {
            Modifier::Macro(..) => {
                let new = Modified {
                    modifier: modifier.clone(),
                    operands: (pack.branches.iter())
                        .map(|b| b.clone().map(Word::Func))
                        .collect(),
                    pack_expansion: true,
                };
                self.modified(new, subscript)
            }
            Modifier::Primitive(Primitive::Dip) => {
                let mut nodes = Vec::with_capacity(pack.branches.len());
                let mut errors = Vec::new();
                for br in pack.branches.iter().cloned() {
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
                let mut branches = pack.branches.iter().cloned();
                let mut new = Modified {
                    modifier: modifier.clone(),
                    operands: vec![branches.next().unwrap().map(Word::Func)],
                    pack_expansion: true,
                };
                for branch in branches {
                    let mut lines = branch.value.lines;
                    if lines.is_empty() {
                        lines.push(Vec::new());
                    }
                    (lines.first_mut().unwrap())
                        .insert(0, span.clone().sp(Word::Modified(Box::new(new))));
                    new = Modified {
                        modifier: modifier.clone(),
                        operands: vec![branch.span.clone().sp(Word::Func(Func {
                            signature: None,
                            lines,
                            closed: true,
                        }))],
                        pack_expansion: true,
                    };
                }
                self.modified(new, subscript)
            }
            Modifier::Primitive(
                Primitive::Fork | Primitive::Bracket | Primitive::Try | Primitive::Fill,
            ) => {
                let mut branches = pack.branches.iter().cloned().rev();
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
                for branch in pack.branches.iter().cloned() {
                    let mut word = Word::Modified(Box::new(Modified {
                        modifier: modifier.clone(),
                        operands: vec![branch.clone().map(Word::Func)],
                        pack_expansion: true,
                    }));
                    if let Some(sub) = &subscript {
                        word = Word::Subscripted(Box::new(Subscripted {
                            n: sub.clone(),
                            word: branch.span.clone().sp(word),
                        }))
                    }
                    words.push(branch.span.sp(word));
                }
                self.words(words)
            }
            Modifier::Primitive(Primitive::Switch) => self.switch(
                (pack.branches.iter().cloned())
                    .map(|sp| sp.map(Word::Func))
                    .collect(),
                modifier.span.clone(),
            ),
            Modifier::Primitive(Primitive::Obverse) => {
                let mut nodes = Vec::new();
                let mut spans = Vec::new();
                for br in &pack.branches {
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
            Modifier::Primitive(Primitive::Astar) if pack.branches.len() == 2 => {
                self.handle_primitive_deprecation(Primitive::Astar, &modifier.span);
                self.handle_primitive_experimental(Primitive::Astar, &modifier.span);
                let mut args = EcoVec::with_capacity(3);
                for branch in pack.branches.iter().cloned() {
                    args.push(self.word_sig(branch.map(Word::Func))?);
                }
                let span = self.add_span(modifier.span.clone());
                Ok(Node::Mod(Primitive::Path, args, span))
            }
            Modifier::Primitive(Primitive::Path) if pack.branches.len() == 3 => {
                let mut args = EcoVec::with_capacity(3);
                for branch in pack.branches.iter().cloned() {
                    args.push(self.word_sig(branch.map(Word::Func))?);
                }
                args.make_mut().swap(1, 2);
                let span = self.add_span(modifier.span.clone());
                Ok(Node::Mod(Primitive::Astar, args, span))
            }
            m if m.args() >= 2 => {
                let new = Modified {
                    modifier: modifier.clone(),
                    operands: pack
                        .branches
                        .iter()
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
        }
        .map(Some)
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
                if let Some(sub) = subscript
                    .and_then(|sub| self.subscript_n(sub, On))
                    .filter(|n| n.value > 1)
                {
                    let n = self.positive_subscript(sub.value, On, &sub.span)?;
                    Node::ImplMod(ImplPrimitive::OnSub(n), eco_vec![sn], span)
                } else {
                    let prim = if sn.sig.args == 0 { Dip } else { On };
                    Node::Mod(prim, eco_vec![sn], span)
                }
            }
            By => {
                let (mut sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                if let Some(sub) = subscript
                    .and_then(|sub| self.subscript_n(sub, By))
                    .filter(|n| n.value > 1)
                {
                    let n = self.positive_subscript(sub.value, By, &sub.span)?;
                    if n == sn.sig.args {
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
                } else if sn.sig.args == 0 {
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
                if let Some(side) = subscript
                    .and_then(|sub| self.subscript_n_or_side(sub))
                    .and_then(|ns| self.subscript_side_only(ns, Reach.format()))
                {
                    match side.value {
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
                match (f.sig.args, g.sig.args) {
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
                let Some(n) = self.subscript_n_or_side(sub) else {
                    return Ok(None);
                };
                let span = self.add_span(modified.modifier.span.clone());
                match n.value {
                    SubNOrSide::N(n) => {
                        let n = self.positive_subscript(n, Both, &modified.modifier.span)?;
                        self.monadic_modifier_op(modified)?.0.on_all(n, span)
                    }
                    SubNOrSide::Side(side) => {
                        let op = self.monadic_modifier_op(modified)?.0;
                        if op.sig.args != 2 {
                            self.add_error(
                                modified.modifier.span.clone().merge(n.span),
                                format!(
                                    "Sided {}'s function must have 2 arguments, \
                                    but its signature is {}",
                                    Primitive::Both.format(),
                                    op.sig
                                ),
                            );
                            op.on_all(2, span)
                        } else {
                            let sub_span = self.add_span(n.span);
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
                            node.push(op.on_all(2, span));
                            node
                        }
                    }
                }
            }
            Bracket => {
                let Some(sub) = subscript else {
                    return Ok(None);
                };
                let Some(side) = self
                    .subscript_n_or_side(sub)
                    .and_then(|n| self.subscript_side_only(n, Bracket.format()))
                else {
                    return Ok(None);
                };
                let span = self.add_span(modified.modifier.span.clone());
                let (a, b, _, _) = self.dyadic_modifier_ops(modified)?;
                if a.sig.args != 2 || b.sig.args != 2 {
                    self.add_error(
                        modified.modifier.span.clone().merge(side.span),
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
                    let sub_span = self.add_span(side.span);
                    let mut node = match side.value {
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
                let (inner, before) = match sn.sig.args {
                    0 => (SigNode::new((2, 2), Node::Prim(Identity, span)), sn.node),
                    1 => {
                        sn.sig.outputs += 1;
                        sn.sig.args = 2;
                        (sn, Node::empty())
                    }
                    _ => (sn, Node::empty()),
                };
                Node::from_iter([
                    before,
                    if let Some(sub) = subscript
                        .and_then(|sub| self.subscript_n(sub, prim))
                        .filter(|n| n.value > 1)
                    {
                        let n = self.positive_subscript(sub.value, prim, &sub.span)?;
                        let prim = if prim == Off {
                            if n == sig.args {
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
                if sn.sig.args < 2 {
                    self.emit_diagnostic(
                        format!(
                            "The current behavior of {} with < 2 arguments \
                            is deprecated and will change in the future",
                            prim.format(),
                        ),
                        DiagnosticKind::Warning,
                        modified.modifier.span.clone(),
                    );
                    sn.sig.args += 1;
                    sn.sig.outputs += 1;
                }
                let span = self.add_span(modified.modifier.span.clone());
                Node::Mod(prim, eco_vec![sn], span)
            }
            Slf => {
                let (SigNode { mut node, sig }, _) = self.monadic_modifier_op(modified)?;
                match sig.args {
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
                match sig.args {
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
                        .on_all(sn.sig.args, span),
                );
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
                if let Some(n) = subscript.and_then(|n| self.subscript_n(n, Repeat.format())) {
                    node.prepend(Node::new_push(n.value));
                }
                node
            }
            Tuples => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                let inner_sig = sn.sig;
                let mut node = Node::Mod(Primitive::Tuples, eco_vec![sn], span);
                if let Some(n) = subscript.and_then(|n| self.subscript_n(n, Tuples.format())) {
                    if inner_sig.args != 2 {
                        self.add_error(
                            modified.modifier.span.clone().merge(n.span),
                            format!(
                                "{} can only be subscripted if its function \
                                is dyadic, but the signature is {inner_sig}",
                                Primitive::Tuples.format()
                            ),
                        );
                    }
                    node.prepend(Node::new_push(n.value));
                }
                node
            }
            Stencil => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                let span = self.add_span(modified.modifier.span.clone());
                let inner_sig = sn.sig;
                let mut node = Node::Mod(Primitive::Stencil, eco_vec![sn], span);
                if let Some(n) = subscript.and_then(|n| self.subscript_n(n, Stencil.format())) {
                    if inner_sig.args != 1 {
                        self.add_error(
                            modified.modifier.span.clone().merge(n.span),
                            format!(
                                "{} can only be subscripted if its function \
                                is monadic, but the signature is {inner_sig}",
                                Primitive::Stencil.format()
                            ),
                        );
                    }
                    node.prepend(Node::new_push(n.value));
                }
                node
            }
            Un => {
                let (sn, span) = self.monadic_modifier_op(modified)?;
                self.add_span(span.clone());
                let normal = sn.un_inverse(&self.asm);
                let cust = CustomInverse {
                    normal,
                    un: Some(sn),
                    ..Default::default()
                };
                let span = self.add_span(modified.modifier.span.clone());
                Node::CustomInverse(cust.into(), span)
            }
            Anti => {
                let (sn, span) = self.monadic_modifier_op(modified)?;
                match sn.node.anti_inverse(&self.asm) {
                    Ok(inv) => inv,
                    Err(e) => return Err(self.error(span, e)),
                }
            }
            Under => {
                let (f, g, f_span, _) = self.dyadic_modifier_ops(modified)?;
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
                let un = if self.allow_experimental() {
                    if f.sig.args == f.sig.outputs {
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
                        let cust = CustomInverse::from(InversionError::UnUnderSignature(f.sig));
                        Some(SigNode::new(
                            normal.sig.inverse(),
                            Node::CustomInverse(cust.into(), span),
                        ))
                    }
                } else {
                    let cust = CustomInverse::from(InversionError::UnUnderExperimental);
                    Some(SigNode::new(
                        normal.sig.inverse(),
                        Node::CustomInverse(cust.into(), span),
                    ))
                };
                let cust = CustomInverse {
                    normal: Ok(normal),
                    un,
                    ..Default::default()
                };
                let span = self.add_span(modified.modifier.span.clone());
                Node::CustomInverse(cust.into(), span)
            }
            Obverse => {
                // Empty inverse case, where only one function is supplied
                let (sn, span) = self.monadic_modifier_op(modified)?;
                let spandex = self.add_span(span.clone());
                let mut cust = CustomInverse {
                    normal: Ok(sn.clone()),
                    un: Some(SigNode::default()),
                    anti: None,
                    under: Some((sn.clone(), SigNode::default())),
                    is_obverse: true,
                };
                if sn.sig == sn.sig.inverse() {
                    cust.un = Some(sn.clone());
                }
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

                // Adjust handler signature if it is a noreturn function
                if let [init @ .., Node::Prim(Assert, _)] = handler.node.as_slice() {
                    let noreturn = match init {
                        [.., Node::Push(val), Node::Prim(Dup | Flip, _)] if *val != 1 => true,
                        [.., Node::Format(..), Node::Prim(Dup, _)] => true,
                        [.., Node::Push(val), Node::Push(_)] if *val != 1 => true,
                        [.., Node::Mod(Dip, args, _)]
                            if args.len() == 1
                                && matches!(&args[0].node, Node::Push(val) if *val != 1) =>
                        {
                            true
                        }
                        _ => false,
                    };
                    if noreturn {
                        handler.sig.outputs = tried.sig.outputs;
                    }
                }

                match tried.sig.outputs.cmp(&handler.sig.outputs) {
                    Ordering::Equal => {}
                    Ordering::Less => {
                        tried.sig.args += handler.sig.outputs - tried.sig.outputs;
                        tried.sig.outputs = handler.sig.outputs;
                    }
                    Ordering::Greater => {
                        handler.sig.args += tried.sig.outputs - handler.sig.outputs;
                        handler.sig.outputs = tried.sig.outputs;
                    }
                }

                if handler.sig.args > tried.sig.args + 1 {
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

                let span = self.add_span(modified.modifier.span.clone());
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
                if fill.sig.outputs > 1 && !self.scope.fill_sig_error {
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
                Node::Mod(Primitive::Fill, eco_vec![fill, f], span)
            }
            Comptime => {
                let word = modified.code_operands().next().unwrap().clone();
                self.do_comptime(prim, word, &modified.modifier.span)?
            }
            Each => {
                // Each pervasive
                let operand = modified.code_operands().next().unwrap().clone();
                let op_span = operand.span.clone();
                let full_span = modified.modifier.span.clone().merge(op_span);
                let words_look_pervasive = subscript
                    .as_ref()
                    .is_none_or(|sub| sub.value == Subscript::N(0))
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
                if let Some(n) = subscript
                    .and_then(|n| self.subscript_n_or_side(n))
                    .filter(|i| i.value != 0)
                {
                    if n.value == -1 {
                        Node::Mod(Rows, eco_vec![sn], span)
                    } else {
                        match n.value {
                            SubNOrSide::N(n) => {
                                Node::ImplMod(ImplPrimitive::EachSub(n), eco_vec![sn], span)
                            }
                            SubNOrSide::Side(side) => {
                                self.experimental_error_it(&n.span, || {
                                    format!("Sided {}", Primitive::Each.format())
                                });
                                let sub_span = self.add_span(n.span);
                                let mut node = match side {
                                    SubSide::Left => Node::Prim(Fix, sub_span),
                                    SubSide::Right => match sn.sig.args {
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
                if let Some(n) = subscript
                    .and_then(|n| self.subscript_n_or_side(n))
                    .filter(|i| i.value != 1)
                {
                    let n_span = n.span;
                    match n.value {
                        SubNOrSide::N(n) => {
                            let mut n =
                                self.positive_subscript(n, prim, &modified.modifier.span)?;
                            if n == 0 {
                                sn.node
                            } else {
                                if n > 10 {
                                    self.add_error(
                                        n_span,
                                        format!("{} max subscript is 10", prim.format()),
                                    );
                                    n = 10;
                                }
                                let mut node = Node::Mod(prim, eco_vec![sn], span);
                                for _ in 1..n {
                                    node =
                                        Node::Mod(prim, eco_vec![node.sig_node().unwrap()], span);
                                }
                                node
                            }
                        }
                        SubNOrSide::Side(side) => {
                            self.experimental_error_it(&n_span, || {
                                format!("Sided {}", prim.format())
                            });
                            let sub_span = self.add_span(n_span);
                            let mut node = match side {
                                SubSide::Left => Node::Prim(Fix, sub_span),
                                SubSide::Right => match sn.sig.args {
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
                            node.push(Node::Mod(prim, eco_vec![sn], span));
                            node
                        }
                    }
                } else {
                    Node::Mod(prim, eco_vec![sn], span)
                }
            }
            Table => {
                // Normal table compilation, but get some diagnostics
                let (sn, span) = self.monadic_modifier_op(modified)?;
                match sn.sig.args {
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
                                && args.windows(2).all(|w| w[0].sig.args == w[1].sig.args) =>
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
                if sn.sig.args <= sn.sig.outputs {
                    self.experimental_error(&modified.modifier.span, || {
                        format!(
                            "{} with arguments ≤ outputs is experimental. To use it, \
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
            Stringify => {
                let operand = modified.code_operands().next().unwrap();
                let s = format_word(operand, &self.asm.inputs);
                Node::new_push(s)
            }
            Quote => {
                let operand = modified.code_operands().next().unwrap().clone();
                let node = self.do_comptime(prim, operand, &modified.modifier.span)?;
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
            Sig => {
                let (sn, _) = self.monadic_modifier_op(modified)?;
                Node::from_iter([Node::new_push(sn.sig.outputs), Node::new_push(sn.sig.args)])
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
        self.experimental_error(&span, || {
            "Inline macros are experimental. \
            To use them, add `# Experimental!` to the top of the file."
        });
        let mut words: Vec<_> = flip_unsplit_lines(
            (mac.func.value.lines.into_iter())
                .flat_map(split_words)
                .collect(),
        )
        .into_iter()
        .flatten()
        .collect();
        // Track
        self.code_meta
            .inline_macros
            .insert(mac.func.span, ident_modifier_args(&mac.ident.value));
        Ok(if mac.caret_span.is_some() {
            let root = self.words_sig(words)?;
            let code_mac = CodeMacro {
                root,
                names: Default::default(),
            };
            self.code_macro(None, span, operands, code_mac)?
        } else {
            // Expand
            self.expand_index_macro(None, &mut words, operands, span.clone(), true)?;
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
        let node = if let Some(mut mac) = self.index_macros.get(&local.index).cloned() {
            // Index macros
            let span = self.add_span(modifier_span.clone());
            match self.scope.kind {
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
                        Some(r.name.value.clone()),
                        &mut mac.words,
                        operands,
                        modifier_span.clone(),
                        mac.hygenic,
                    )?;
                    // Handle recursion
                    // Recursive macros work by creating a binding for the expansion.
                    // Recursive calls then call that binding.
                    // We know that this is a recursive call if the scope tracks
                    // a macro with the same index.
                    let macro_local = mac.recursive.then(|| {
                        let expansion_index = self.next_global;
                        let count = ident_modifier_args(&r.name.value);
                        // Add temporary binding
                        self.asm.add_binding_at(
                            LocalName {
                                index: expansion_index,
                                public: false,
                            },
                            BindingKind::IndexMacro(count),
                            Some(modifier_span.clone()),
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
                    let sig = self.sig_of(&node, &modifier_span)?;
                    let id = FunctionId::Macro(Some(r.name.value), r.name.span);
                    let func = self.asm.add_function(id, sig, node);
                    if let Some(macro_local) = macro_local {
                        self.asm.bindings.make_mut()[macro_local.expansion_index].kind =
                            BindingKind::Func(func.clone());
                    }
                    Node::Call(func, span)
                }
            }
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
                    .branches
                    .into_iter()
                    .map(|b| b.map(Word::Func))
                    .collect(),
                word => vec![operand.span.sp(word)],
            };
        }
        let op_sigs = if mac.root.sig.args == 2 {
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
                sig_data.extend_from_slice(&[sn.sig.args as u8, sn.sig.outputs as u8]);
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
        let mut words_to_format = Vec::new();
        for word in &*macro_words {
            match &word.value {
                Word::Func(func) => words_to_format.extend(func.lines.iter().flatten().cloned()),
                _ => words_to_format.push(word.clone()),
            }
        }
        let formatted = format_words(&words_to_format, &self.asm.inputs);
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
            .items(items, true)
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
        prim: Primitive,
        operand: Sp<Word>,
        span: &CodeSpan,
    ) -> UiuaResult<Node> {
        let orig_spans_len = self.asm.spans.len();
        let sn = self.word_sig(operand)?;
        if sn.sig.args > 0 {
            return Err(self.error(
                span.clone(),
                format!(
                    "{}'s function must have no arguments, but it has {}",
                    prim.format(),
                    sn.sig.args
                ),
            ));
        }
        let mut comp = self.clone();
        if let Some(index) = comp.node_unbound_index(&sn.node) {
            let name = comp.scope.names.iter().find_map(|(ident, local)| {
                if local.index == index {
                    Some(ident)
                } else {
                    None
                }
            });
            let message = if let Some(name) = name {
                format!("Compile-time evaluation references runtime binding `{name}`")
            } else {
                "Compile-time evaluation references runtime binding".into()
            };
            return Err(self.error(span.clone(), message));
        }
        let asm_root_len = comp.asm.root.len();
        comp.asm.root.push(sn.node);
        let res = comp.macro_env.run_asm(comp.asm.clone());
        let stack = comp.macro_env.take_stack();
        let values = if let Err(e) = res {
            if self.errors.is_empty() {
                self.add_error(span.clone(), format!("Compile-time evaluation failed: {e}"));
            }
            vec![Value::default(); sn.sig.outputs]
        } else {
            stack
        };
        self.asm.spans.truncate(orig_spans_len);
        comp.asm.root.truncate(asm_root_len);
        let val_count = sn.sig.outputs;
        let mut node = Node::empty();
        for value in values.into_iter().rev().take(val_count).rev() {
            node.push(Node::new_push(value));
        }
        Ok(node)
    }
    /// Run a function in a temporary scope with the given names.
    /// Newly created bindings will be added to the current scope after the function is run.
    fn temp_scope<T>(
        &mut self,
        names: IndexMap<Ident, LocalName>,
        macro_local: Option<MacroLocal>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let macro_names_len = names.len();
        let temp_scope = Scope {
            kind: ScopeKind::Temp(macro_local),
            names,
            experimental: self.scope.experimental,
            experimental_error: self.scope.experimental_error,
            ..Default::default()
        };
        self.higher_scopes
            .push(replace(&mut self.scope, temp_scope));
        let res = f(self);
        let mut scope = self.higher_scopes.pop().unwrap();
        (scope.names).extend(self.scope.names.drain(macro_names_len..));
        self.scope = scope;
        res
    }
}
