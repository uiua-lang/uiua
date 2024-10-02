//! Compiler code for modifiers

use crate::algorithm::invert::anti_instrs;

use super::*;

impl Compiler {
    fn desugar_function_pack_inner(
        &mut self,
        modifier: &Sp<Modifier>,
        operand: &Sp<Word>,
    ) -> UiuaResult<Option<Word>> {
        let Sp {
            value: Word::Pack(pack @ FunctionPack { angled: false, .. }),
            span,
        } = operand
        else {
            return Ok(None);
        };
        match &modifier.value {
            Modifier::Primitive(Primitive::Dip) => {
                let mut branches = pack.branches.iter().cloned().rev();
                let mut new = Modified {
                    modifier: modifier.clone(),
                    operands: vec![branches.next().unwrap().map(Word::Func)],
                };
                for branch in branches {
                    let mut lines = branch.value.lines;
                    (lines.last_mut().unwrap())
                        .push(span.clone().sp(Word::Modified(Box::new(new))));
                    new = Modified {
                        modifier: modifier.clone(),
                        operands: vec![branch.span.clone().sp(Word::Func(Func {
                            id: FunctionId::Anonymous(branch.span.clone()),
                            signature: None,
                            lines,
                            closed: true,
                        }))],
                    };
                }
                Ok(Some(Word::Modified(Box::new(new))))
            }
            Modifier::Primitive(Primitive::Rows | Primitive::Inventory) => {
                let mut branches = pack.branches.iter().cloned();
                let mut new = Modified {
                    modifier: modifier.clone(),
                    operands: vec![branches.next().unwrap().map(Word::Func)],
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
                            id: FunctionId::Anonymous(branch.span.clone()),
                            signature: None,
                            lines,
                            closed: true,
                        }))],
                    };
                }
                Ok(Some(Word::Modified(Box::new(new))))
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
                };
                for branch in branches {
                    new = Modified {
                        modifier: modifier.clone(),
                        operands: vec![
                            branch.map(Word::Func),
                            span.clone().sp(Word::Modified(Box::new(new))),
                        ],
                    };
                }
                Ok(Some(Word::Modified(Box::new(new))))
            }
            Modifier::Primitive(Primitive::On) => {
                let mut words = Vec::new();
                for branch in pack.branches.iter().cloned() {
                    words.push(branch.span.clone().sp(Word::Modified(Box::new(Modified {
                        modifier: modifier.clone(),
                        operands: vec![branch.map(Word::Func)],
                    }))));
                }
                Ok(Some(Word::Func(Func {
                    id: FunctionId::Anonymous(span.clone()),
                    signature: None,
                    lines: vec![words],
                    closed: true,
                })))
            }
            Modifier::Primitive(Primitive::SetUnder) if pack.branches.len() == 2 => {
                let mut pack = pack.clone();
                pack.branches.insert(0, pack.branches[0].clone());
                Ok(Some(Word::Modified(Box::new(Modified {
                    modifier: modifier.clone(),
                    operands: (pack.branches.into_iter())
                        .map(|sp| sp.map(Word::Func))
                        .collect(),
                }))))
            }
            _ => Ok(None),
        }
    }
    fn desugar_function_pack(
        &mut self,
        modifier: &Sp<Modifier>,
        operand: &Sp<Word>,
        subscript: Option<usize>,
        call: bool,
    ) -> UiuaResult<bool> {
        if let Some(word) = self.desugar_function_pack_inner(modifier, operand)? {
            let span = modifier.span.clone().merge(operand.span.clone());
            self.word(span.sp(word), call)?;
            Ok(true)
        } else if let Word::Pack(pack @ FunctionPack { angled: false, .. }) = &operand.value {
            match &modifier.value {
                Modifier::Primitive(Primitive::Switch) => {
                    self.switch(
                        (pack.branches.iter().cloned())
                            .map(|sp| sp.map(Word::Func))
                            .collect(),
                        modifier.span.clone(),
                        call,
                    )?;
                    Ok(true)
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
                    };
                    self.modified(new, subscript, call)?;
                    Ok(true)
                }
                m => {
                    if let Modifier::Ref(name) = m {
                        if let Ok((_, local)) = self.ref_local(name) {
                            if self.code_macros.contains_key(&local.index) {
                                return Ok(false);
                            }
                        }
                    }
                    Err(self.fatal_error(
                        modifier.span.clone().merge(operand.span.clone()),
                        format!("{m} cannot use a function pack"),
                    ))
                }
            }
        } else {
            Ok(false)
        }
    }
    #[allow(clippy::collapsible_match)]
    pub(crate) fn modified(
        &mut self,
        mut modified: Modified,
        subscript: Option<usize>,
        call: bool,
    ) -> UiuaResult {
        let mut op_count = modified.code_operands().count();

        // De-sugar function pack
        if op_count == 1 {
            let operand = modified.code_operands().next().unwrap();
            if self.desugar_function_pack(&modified.modifier, operand, subscript, call)? {
                return Ok(());
            }
        }

        if op_count < modified.modifier.value.args() {
            let missing = modified.modifier.value.args() - op_count;
            let span = modified.modifier.span.clone();
            for _ in 0..missing {
                modified.operands.push(span.clone().sp(Word::Func(Func {
                    id: FunctionId::Anonymous(span.clone()),
                    signature: None,
                    lines: Vec::new(),
                    closed: false,
                })));
            }
            op_count = modified.code_operands().count();
        }
        if op_count == modified.modifier.value.args() {
            // Inlining
            if self.inline_modifier(&modified, subscript, call)? {
                return Ok(());
            }
        } else {
            let strict_args = match &modified.modifier.value {
                Modifier::Primitive(_) => true,
                Modifier::Ref(name) => {
                    let (_, local) = self.ref_local(name)?;
                    self.index_macros.contains_key(&local.index)
                }
            };
            if strict_args {
                // Validate operand count
                return Err(self.fatal_error(
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
        let mut prim = match modified.modifier.value {
            Modifier::Primitive(prim) => prim,
            Modifier::Ref(r) => {
                let (path_locals, local) = self.ref_local(&r)?;
                self.validate_local(&r.name.value, local, &r.name.span);
                self.code_meta
                    .global_references
                    .insert(r.name.span.clone(), local.index);
                for (local, comp) in path_locals.into_iter().zip(&r.path) {
                    (self.code_meta.global_references)
                        .insert(comp.module.span.clone(), local.index);
                }
                // Handle recursion depth
                self.macro_depth += 1;
                const MAX_MACRO_DEPTH: usize = if cfg!(debug_assertions) { 10 } else { 20 };
                if self.macro_depth > MAX_MACRO_DEPTH {
                    return Err(
                        self.fatal_error(modified.modifier.span.clone(), "Macro recurs too deep")
                    );
                }
                if let Some(mut mac) = self.index_macros.get(&local.index).cloned() {
                    // Index macros
                    match self.scope.kind {
                        ScopeKind::Temp(Some(mac_local))
                            if mac_local.macro_index == local.index =>
                        {
                            // Recursive
                            let new_func = self.compile_words(modified.operands, false)?;
                            self.push_all_instrs(new_func);
                            if let Some(sig) = mac.sig {
                                self.push_instr(Instr::CallGlobal {
                                    index: mac_local.expansion_index,
                                    call,
                                    sig,
                                });
                            }
                        }
                        _ => {
                            // Expand
                            self.expand_positional_macro(
                                r.name.value.clone(),
                                &mut mac.words,
                                modified.operands,
                                modified.modifier.span.clone(),
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
                                    Some(modified.modifier.span.clone()),
                                    None,
                                );
                                self.next_global += 1;
                                MacroLocal {
                                    macro_index: local.index,
                                    expansion_index,
                                }
                            });
                            // Compile
                            let mut new_func = self.suppress_diagnostics(|comp| {
                                comp.temp_scope(mac.names, macro_local, |comp| {
                                    comp.compile_words(mac.words, true)
                                })
                            })?;
                            new_func.flags |= mac.flags;
                            // Add
                            let sig = self.sig_of(&new_func.instrs, &modified.modifier.span)?;
                            let mut func =
                                self.make_function(FunctionId::Macro(r.name.span), sig, new_func);
                            if mac.recursive {
                                func.flags |= FunctionFlags::RECURSIVE;
                            }
                            if let Some(macro_local) = macro_local {
                                self.asm.bindings.make_mut()[macro_local.expansion_index].kind =
                                    BindingKind::Func(func.clone());
                            }
                            self.push_instr(Instr::PushFunc(func));
                            if call {
                                let span = self.add_span(modified.modifier.span);
                                self.push_instr(if mac.recursive {
                                    Instr::CallRecursive(span)
                                } else {
                                    Instr::Call(span)
                                });
                            }
                        }
                    }
                } else if let Some(mac) = self.code_macros.get(&local.index).cloned() {
                    // Code macros
                    let full_span = (modified.modifier.span.clone())
                        .merge(modified.operands.last().unwrap().span.clone());

                    // Collect operands as strings
                    let mut operands: Vec<Sp<Word>> = (modified.operands.into_iter())
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
                    let op_sigs = if mac.function.signature().args == 2 {
                        // If the macro function has 2 arguments, we pass the signatures
                        // of the operands as well
                        let mut sig_data: EcoVec<u8> = EcoVec::with_capacity(operands.len() * 2);
                        // Track the length of the instructions and spans so
                        // they can be discarded after signatures are calculated
                        let instrs_len = self.asm.instrs.len();
                        let spans_len = self.asm.spans.len();
                        for op in &operands {
                            let (_, sig) = self.compile_operand_word(op.clone()).map_err(|e| {
                                let message = format!(
                                    "This error occurred while compiling a macro operand. \
                                    This was attempted because the macro function's \
                                    signature is {}.",
                                    Signature::new(2, 1)
                                );
                                e.with_info([(message, None)])
                            })?;
                            sig_data.extend_from_slice(&[sig.args as u8, sig.outputs as u8]);
                        }
                        // Discard unnecessary instructions and spans
                        self.asm.instrs.truncate(instrs_len);
                        self.asm.spans.truncate(spans_len);
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

                    let mut code = String::new();
                    (|| -> UiuaResult {
                        if let Some(index) =
                            instrs_unbound_index(mac.function.instrs(&self.asm), &self.asm)
                        {
                            let name = self.scope.names.iter().find_map(|(name, local)| {
                                if local.index == index {
                                    Some(name)
                                } else {
                                    None
                                }
                            });
                            let message = if let Some(name) = name {
                                format!("{} references runtime binding `{}`", r.name.value, name)
                            } else {
                                format!("{} references runtime binding", r.name.value)
                            };
                            return Err(self.fatal_error(modified.modifier.span.clone(), message));
                        }

                        let span = self.add_span(modified.modifier.span.clone());
                        let env = &mut self.macro_env;
                        env.asm = self.asm.clone();
                        env.rt.call_stack.last_mut().unwrap().call_span = span;

                        // Run the macro function
                        if let Some(sigs) = op_sigs {
                            env.push(sigs);
                        }
                        env.push(formatted);

                        #[cfg(feature = "native_sys")]
                        let enabled = crate::sys_native::set_output_enabled(
                            self.pre_eval_mode != PreEvalMode::Lsp,
                        );
                        env.call(mac.function)?;
                        #[cfg(feature = "native_sys")]
                        crate::sys_native::set_output_enabled(enabled);

                        let val = env.pop("macro result")?;

                        // Parse the macro output
                        if let Ok(s) = val.as_string(env, "") {
                            code = s;
                        } else {
                            for row in val.into_rows() {
                                let s = row.as_string(env, "Macro output rows must be strings")?;
                                if code.chars().last().is_some_and(|c| !c.is_whitespace()) {
                                    code.push(' ');
                                }
                                code.push_str(&s);
                            }
                        }
                        Ok(())
                    })()
                    .map_err(|e| e.trace_macro(modified.modifier.span.clone()))?;

                    // Quote
                    self.code_meta
                        .macro_expansions
                        .insert(full_span, (r.name.value.clone(), code.clone()));
                    self.suppress_diagnostics(|comp| {
                        comp.temp_scope(mac.names, None, |comp| {
                            comp.quote(&code, &modified.modifier.span, call)
                        })
                    })?;
                } else if let Some(m) =
                    (self.asm.bindings.get(local.index)).and_then(|binfo| match &binfo.kind {
                        BindingKind::Module(m) => Some(m),
                        _ => None,
                    })
                {
                    // Module import macro
                    let names = m.names.clone();
                    self.in_scope(ScopeKind::AllInModule, move |comp| {
                        comp.scope.names.extend(names);
                        comp.words(modified.operands, call)
                    })?;
                } else {
                    // Recursive positional macro inside itself
                    if !call {
                        self.new_functions.push(NewFunction::default());
                    }
                    self.words(modified.operands, false)?;
                    self.ident(r.name.value.clone(), r.name.span, true, r.in_macro_arg)?;
                    if !call {
                        let new_func = self.new_functions.pop().unwrap();
                        let sig = self.sig_of(&new_func.instrs, &modified.modifier.span)?;
                        let func = self.make_function(modified.modifier.span.into(), sig, new_func);
                        self.push_instr(Instr::PushFunc(func));
                    }
                }
                self.macro_depth -= 1;

                return Ok(());
            }
        };

        // De-sugar loop fork
        if let Primitive::Rows
        | Primitive::Each
        | Primitive::Table
        | Primitive::Group
        | Primitive::Partition
        | Primitive::Inventory = prim
        {
            let mut op = modified.code_operands().next().unwrap();
            if let Word::Func(func) = &op.value {
                if func.lines.len() == 1 && func.lines[0].len() == 1 {
                    op = &func.lines[0][0];
                }
            }
            if let Word::Modified(m) = &op.value {
                if (matches!(m.modifier.value, Modifier::Primitive(Primitive::Fork))
                    || matches!(m.modifier.value, Modifier::Primitive(Primitive::Bracket))
                        && prim != Primitive::Table)
                    && self.words_look_pure(&m.operands)
                {
                    let mut m = (**m).clone();
                    for op in m.operands.iter_mut().filter(|w| w.value.is_code()) {
                        if let Some(new) = self.desugar_function_pack_inner(&m.modifier, op)? {
                            op.value = new;
                        }
                        op.value = Word::Modified(
                            Modified {
                                modifier: modified.modifier.clone(),
                                operands: vec![op.clone()],
                            }
                            .into(),
                        );
                    }
                    return self.modified(m, subscript, call);
                }
            }
        }

        let mut before = EcoVec::new();
        let mut after = EcoVec::new();

        if let Some(n) = subscript {
            let span = self.add_span(modified.modifier.span.clone());
            match prim {
                Primitive::Rows if n == 0 => {
                    before = eco_vec![Instr::Prim(Primitive::Fix, span),];
                    after = eco_vec![Instr::ImplPrim(ImplPrimitive::UndoFix, span),];
                }
                Primitive::Rows => {
                    let stack = TempStack::Under;
                    let count = 2;
                    let n = -(n as i32);
                    before = eco_vec![
                        Instr::Prim(Primitive::Dup, span),
                        Instr::Prim(Primitive::Shape, span),
                        Instr::push(n),
                        Instr::PushTemp { stack, count, span },
                        Instr::push(n),
                        Instr::Prim(Primitive::Rerank, span),
                    ];
                    after = eco_vec![
                        Instr::PopTemp { stack, count, span },
                        Instr::ImplPrim(ImplPrimitive::UndoRerank, span)
                    ];
                }
                Primitive::Each => {
                    let stack = TempStack::Under;
                    let count = 2;
                    prim = Primitive::Rows;
                    before = eco_vec![
                        Instr::Prim(Primitive::Dup, span),
                        Instr::Prim(Primitive::Shape, span),
                        Instr::push(n),
                        Instr::PushTemp { stack, count, span },
                        Instr::push(n),
                        Instr::Prim(Primitive::Rerank, span),
                    ];
                    after = eco_vec![
                        Instr::PopTemp { stack, count, span },
                        Instr::ImplPrim(ImplPrimitive::UndoRerank, span)
                    ];
                }
                Primitive::Tuples => before = eco_vec![Instr::push(n)],
                _ => {}
            }
        }

        // Compile operands
        let operands_func = self.compile_words(modified.operands, false)?;

        if !call {
            self.new_functions.push(NewFunction::default());
        }
        self.push_all_instrs(before);
        self.push_all_instrs(operands_func);
        self.primitive(prim, modified.modifier.span.clone(), true)?;
        self.push_all_instrs(after);
        if !call {
            let new_func = self.new_functions.pop().unwrap();
            let sig = self.sig_of(&new_func.instrs, &modified.modifier.span)?;
            let func = self.make_function(modified.modifier.span.into(), sig, new_func);
            self.push_instr(Instr::PushFunc(func));
        }

        Ok(())
    }
    fn words_look_pure(&self, words: &[Sp<Word>]) -> bool {
        words.iter().all(|word| match &word.value {
            Word::Primitive(p) => p.purity() == Purity::Pure,
            Word::Func(func) => func.lines.iter().all(|line| self.words_look_pure(line)),
            Word::Pack(pack) => (pack.branches.iter())
                .all(|branch| (branch.value.lines.iter()).all(|line| self.words_look_pure(line))),
            Word::Modified(m) => self.words_look_pure(&m.operands),
            Word::Array(arr) => arr.lines.iter().all(|line| self.words_look_pure(line)),
            Word::Strand(items) => self.words_look_pure(items),
            Word::Ref(r) => {
                if let Ok((_, local)) = self.ref_local(r) {
                    match &self.asm.bindings[local.index].kind {
                        BindingKind::Const(_) | BindingKind::Module(_) | BindingKind::Import(_) => {
                            true
                        }
                        BindingKind::Func(f) => {
                            instrs_are_pure(f.instrs(&self.asm), &self.asm, Purity::Pure)
                        }
                        _ => false,
                    }
                } else {
                    true
                }
            }
            _ => true,
        })
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
    pub(super) fn inline_modifier(
        &mut self,
        modified: &Modified,
        subscript: Option<usize>,
        call: bool,
    ) -> UiuaResult<bool> {
        use Primitive::*;
        let Modifier::Primitive(prim) = modified.modifier.value else {
            return Ok(false);
        };

        // Validation
        self.handle_primitive_experimental(prim, &modified.modifier.span);
        self.handle_primitive_deprecation(prim, &modified.modifier.span);

        macro_rules! finish {
            ($instrs:expr, $sig:expr) => {{
                if call {
                    self.push_all_instrs($instrs);
                } else {
                    let func = self.make_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        $sig,
                        $instrs.into(),
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }};
        }
        /// Modify instructions for `on`, and return the new signature
        fn on(instrs: &mut EcoVec<Instr>, sig: Signature, span: usize) -> Signature {
            let save = if sig.args == 0 {
                Instr::push_inline(1, span)
            } else {
                Instr::copy_inline(span)
            };
            instrs.insert(0, save);
            instrs.push(Instr::pop_inline(1, span));
            Signature::new(sig.args.max(1), sig.outputs + 1)
        }
        match prim {
            Dip | Gap | On | By | With | Off | Above | Below => {
                // Compile operands
                let (mut new_func, mut sig) =
                    self.compile_operand_word(modified.operands[0].clone())?;
                let instrs = &mut new_func.instrs;
                // Dip (|1 …) . diagnostic
                if sig.args == 1 {
                    match prim {
                        Dip => {
                            if let Some(Instr::Prim(Dup, dup_span)) =
                                self.new_functions.last().and_then(|nf| nf.instrs.last())
                            {
                                if let Span::Code(dup_span) = self.get_span(*dup_span) {
                                    let span = modified.modifier.span.clone().merge(dup_span);
                                    self.emit_diagnostic(
                                        "Prefer `⟜(…)` over `⊙(…).` for clarity",
                                        DiagnosticKind::Style,
                                        span,
                                    );
                                }
                            }
                        }
                        _ => {}
                    }
                }

                let span = self.add_span(modified.modifier.span.clone());
                let sig = match prim {
                    Dip => {
                        instrs.insert(0, Instr::push_inline(1, span));
                        instrs.push(Instr::pop_inline(1, span));
                        Signature::new(sig.args + 1, sig.outputs + 1)
                    }
                    Gap => {
                        instrs.insert(0, Instr::Prim(Pop, span));
                        Signature::new(sig.args + 1, sig.outputs)
                    }
                    On => on(instrs, sig, span),
                    By => {
                        if sig.args > 0 {
                            let mut i = 0;
                            if sig.args > 1 {
                                instrs.insert(i, Instr::push_inline(sig.args - 1, span));
                                i += 1;
                            }
                            instrs.insert(i, Instr::Prim(Dup, span));
                            i += 1;
                            if sig.args > 1 {
                                instrs.insert(i, Instr::pop_inline(sig.args - 1, span));
                            }
                        } else {
                            instrs.insert(0, Instr::TouchStack { count: 1, span })
                        }
                        Signature::new(sig.args.max(1), sig.outputs + 1)
                    }
                    With => {
                        if sig.args < 2 {
                            instrs.insert(0, Instr::TouchStack { count: 2, span });
                            sig.outputs += 2 - sig.args;
                            sig.args = 2;
                        }
                        let mut prefix = eco_vec![
                            Instr::push_inline(sig.args - 1, span),
                            Instr::Prim(Dup, span),
                            Instr::pop_inline(sig.args - 1, span)
                        ];
                        if sig.outputs >= 2 {
                            instrs.push(Instr::push_inline(sig.outputs - 1, span));
                            for _ in 0..sig.outputs - 1 {
                                instrs.push(Instr::Prim(Flip, span));
                                instrs.push(Instr::pop_inline(1, span));
                            }
                        }
                        instrs.push(Instr::Prim(Flip, span));
                        prefix.extend(take(instrs));
                        *instrs = prefix;
                        Signature::new(sig.args.max(1), sig.outputs + 1)
                    }
                    Off => {
                        if sig.args < 2 {
                            instrs.insert(0, Instr::TouchStack { count: 2, span });
                            sig.outputs += 2 - sig.args;
                            sig.args = 2;
                        }
                        let mut prefix = eco_vec![Instr::Prim(Dup, span)];
                        for _ in 0..sig.args - 1 {
                            prefix.push(Instr::push_inline(1, span));
                            prefix.push(Instr::Prim(Flip, span));
                        }
                        prefix.push(Instr::pop_inline(sig.args - 1, span));
                        prefix.extend(take(instrs));
                        *instrs = prefix;
                        Signature::new(sig.args.max(1), sig.outputs + 1)
                    }
                    Above => {
                        if sig.args < 2 {
                            sig.args += 1;
                            sig.outputs += 1;
                        }
                        instrs.insert(
                            0,
                            Instr::CopyToTemp {
                                stack: TempStack::Inline,
                                count: sig.args,
                                span,
                            },
                        );
                        instrs.push(Instr::pop_inline(sig.args, span));
                        Signature::new(sig.args, sig.outputs + sig.args)
                    }
                    Below => {
                        if sig.args < 2 {
                            sig.args += 1;
                            sig.outputs += 1;
                        }
                        instrs.insert(
                            0,
                            Instr::CopyToTemp {
                                stack: TempStack::Inline,
                                count: sig.args,
                                span,
                            },
                        );
                        instrs.insert(1, Instr::pop_inline(sig.args, span));
                        Signature::new(sig.args, sig.outputs + sig.args)
                    }
                    _ => unreachable!(),
                };
                if call {
                    self.push_all_instrs(new_func);
                } else {
                    let func =
                        self.make_function(modified.modifier.span.clone().into(), sig, new_func);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Backward => {
                let operand = modified.code_operands().next().unwrap().clone();
                let (mut new_func, sig) = self.compile_operand_word(operand)?;
                if sig.args != 2 {
                    self.add_error(
                        modified.modifier.span.clone(),
                        format!(
                            "Currently, {}'s function must be dyadic, \
                            but its signature is {}",
                            prim, sig
                        ),
                    );
                }
                let spandex = self.add_span(modified.modifier.span.clone());
                new_func.instrs.insert(0, Instr::Prim(Flip, spandex));
                let sig = self.sig_of(&new_func.instrs, &modified.modifier.span)?;
                if call {
                    self.push_all_instrs(new_func);
                } else {
                    let func =
                        self.make_function(modified.modifier.span.clone().into(), sig, new_func);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Fork => {
                let mut operands = modified.code_operands().cloned();
                let first_op = operands.next().unwrap();
                // ⊃∘ diagnostic
                if let Word::Primitive(Primitive::Identity) = first_op.value {
                    self.emit_diagnostic(
                        "Prefer `⟜` over `⊃∘` for clarity",
                        DiagnosticKind::Style,
                        modified.modifier.span.clone().merge(first_op.span.clone()),
                    );
                }
                let (a_nf, a_sig) = self.compile_operand_word(first_op)?;
                let (b_nf, b_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let span = self.add_span(modified.modifier.span.clone());
                let mut instrs = EcoVec::new();
                let mut flags = FunctionFlags::default();
                flags |= a_nf.flags;
                flags |= b_nf.flags;
                if a_sig.args > 0 {
                    instrs.push(Instr::CopyToTemp {
                        stack: TempStack::Inline,
                        count: a_sig.args,
                        span,
                    });
                }
                if a_sig.args > b_sig.args {
                    let diff = a_sig.args - b_sig.args;
                    if b_sig.args > 0 {
                        instrs.push(Instr::PushTemp {
                            stack: TempStack::Inline,
                            count: b_sig.args,
                            span,
                        });
                    }
                    for _ in 0..diff {
                        instrs.push(Instr::Prim(Pop, span));
                    }
                    if b_sig.args > 0 {
                        instrs.push(Instr::pop_inline(b_sig.args, span));
                    }
                }
                instrs.extend(b_nf.instrs);
                if a_sig.args > 0 {
                    instrs.push(Instr::pop_inline(a_sig.args, span));
                }
                instrs.extend(a_nf.instrs);
                let sig = Signature::new(a_sig.args.max(b_sig.args), a_sig.outputs + b_sig.outputs);
                let new_func = NewFunction { instrs, flags };
                if call {
                    self.push_all_instrs(new_func);
                } else {
                    let func =
                        self.make_function(modified.modifier.span.clone().into(), sig, new_func);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Bracket => {
                let mut operands = modified.code_operands().cloned();
                let (a_nf, a_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let (b_nf, b_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let span = self.add_span(modified.modifier.span.clone());
                let mut flags = FunctionFlags::default();
                flags |= a_nf.flags;
                flags |= b_nf.flags;
                let mut instrs = eco_vec![Instr::PushTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                }];
                instrs.extend(b_nf.instrs);
                instrs.push(Instr::pop_inline(a_sig.args, span));
                instrs.extend(a_nf.instrs);
                let sig = Signature::new(a_sig.args + b_sig.args, a_sig.outputs + b_sig.outputs);
                let new_func = NewFunction { instrs, flags };
                if call {
                    self.push_all_instrs(new_func);
                } else {
                    let func = self.make_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        new_func,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Repeat => {
                if let Some(n) = subscript {
                    self.push_instr(Instr::push(n));
                }
                let operand = modified.code_operands().next().unwrap().clone();
                let (new_func, sig) = self.compile_operand_word(operand)?;
                let flags = new_func.flags;
                let spandex = self.add_span(modified.modifier.span.clone());
                let instrs = 'instrs: {
                    if let Some(curr) = self.new_functions.last_mut() {
                        if let [.., Instr::Push(val)] = curr.instrs.as_slice() {
                            if let Ok(n) = val.as_int(&IgnoreError, "") {
                                if n >= 0 && n.unsigned_abs() * new_func.instrs.len() <= 20 {
                                    curr.instrs.pop();
                                    break 'instrs repeat(&new_func.instrs)
                                        .take(n.unsigned_abs())
                                        .flatten()
                                        .cloned()
                                        .collect();
                                }
                            }
                        }
                    }
                    if let Some((inverse, inv_sig)) = invert_instrs(&new_func.instrs, self)
                        .ok()
                        .and_then(|inv| instrs_signature(&inv).ok().map(|sig| (inv, sig)))
                        .filter(|(_, inv_sig)| sig.is_compatible_with(*inv_sig))
                    {
                        // If an inverse for repeat's function exists we use a special
                        // implementation that allows for negative repeatition counts
                        let id = FunctionId::Anonymous(modified.modifier.span.clone());
                        let func = self.make_function(id, sig, new_func);
                        let inv_id = FunctionId::Anonymous(modified.modifier.span.clone());
                        let inv_new_func = NewFunction {
                            instrs: inverse,
                            flags,
                        };
                        let inv = self.make_function(inv_id, inv_sig, inv_new_func);
                        eco_vec![
                            Instr::PushFunc(inv),
                            Instr::PushFunc(func),
                            Instr::ImplPrim(ImplPrimitive::RepeatWithInverse, spandex)
                        ]
                    } else {
                        let id = FunctionId::Anonymous(modified.modifier.span.clone());
                        let func = self.make_function(id, sig, new_func);
                        eco_vec![Instr::PushFunc(func), Instr::Prim(Repeat, spandex)]
                    }
                };
                let new_func = NewFunction { instrs, flags };
                if call {
                    self.push_all_instrs(new_func);
                } else {
                    let sig = self.sig_of(&new_func.instrs, &modified.modifier.span)?;
                    let func =
                        self.make_function(modified.modifier.span.clone().into(), sig, new_func);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Un if !self.in_inverse => {
                let f = modified.code_operands().next().unwrap().clone();
                let span = f.span.clone();

                let in_inverse = self.in_inverse;
                self.in_inverse = !in_inverse;
                let f_res = self.compile_operand_word(f);
                self.in_inverse = in_inverse;
                let (mut new_func, _) = f_res?;

                self.add_span(span.clone());
                match invert_instrs(&new_func.instrs, self) {
                    Ok(inverted) => {
                        let sig = self.sig_of(&inverted, &span)?;
                        new_func.instrs = inverted;
                        finish!(new_func, sig);
                    }
                    Err(e) => return Err(self.fatal_error(span, e)),
                }
            }
            Anti if !self.in_inverse => {
                let f = modified.code_operands().next().unwrap().clone();
                let span = f.span.clone();

                let in_inverse = self.in_inverse;
                self.in_inverse = !in_inverse;
                let f_res = self.compile_operand_word(f);
                self.in_inverse = in_inverse;
                let (mut new_func, f_sig) = f_res?;
                if f_sig.args < 2 {
                    self.emit_diagnostic(
                        format!(
                            "Prefer {} over {} for functions \
                            with fewer than 2 arguments",
                            Primitive::Un.format(),
                            Primitive::Anti.format()
                        ),
                        DiagnosticKind::Style,
                        span.clone(),
                    );
                }

                match anti_instrs(&new_func.instrs, self) {
                    Ok(inverted) => {
                        let sig = self.sig_of(&inverted, &span)?;
                        new_func.instrs = inverted;
                        finish!(new_func, sig);
                    }
                    Err(e) => return Err(self.fatal_error(span, e)),
                }
            }
            Under if !self.in_inverse => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let f_span = f.span.clone();
                let g = operands.next().unwrap();

                // The not inverted function
                let (g_new_func, g_sig) = self.compile_operand_word(g)?;

                // The inverted function
                self.in_inverse = !self.in_inverse;
                let f_res = self.compile_operand_word(f);
                self.in_inverse = !self.in_inverse;
                let (f_new_func, f_new_sig) = f_res?;

                // Under pop diagnostic
                if let [Instr::Prim(Pop, _)] = f_new_func.instrs.as_slice() {
                    self.emit_diagnostic(
                        format!("Prefer {} over `⍜◌` for clarity", Dip.format()),
                        DiagnosticKind::Style,
                        modified.modifier.span.clone().merge(f_span.clone()),
                    );
                }

                match under_instrs(&f_new_func.instrs, g_sig, self) {
                    Ok((f_before, f_after)) => {
                        let mut instrs = f_before;
                        instrs.extend(g_new_func.instrs.iter().cloned());
                        instrs.extend(f_after);

                        // Register undered functions
                        let span = self.add_span(modified.modifier.span.clone());
                        self.undered_funcs.insert(
                            instrs.clone(),
                            UnderedFunctions {
                                f: f_new_func.instrs,
                                f_sig: f_new_sig,
                                g: g_new_func.instrs,
                                g_sig,
                                span,
                            },
                        );

                        let new_func = NewFunction {
                            instrs,
                            flags: f_new_func.flags | g_new_func.flags,
                        };
                        if call {
                            self.push_all_instrs(new_func);
                        } else {
                            let sig = self.sig_of(&new_func.instrs, &modified.modifier.span)?;
                            let func = self.make_function(
                                modified.modifier.span.clone().into(),
                                sig,
                                new_func,
                            );
                            self.push_instr(Instr::PushFunc(func));
                        }
                    }
                    Err(e) => return Err(self.fatal_error(f_span, e)),
                }
            }
            SetInverse => {
                let mut operands = modified.code_operands().cloned();
                let normal = operands.next().unwrap();
                let inverse = operands.next().unwrap();
                let normal_span = normal.span.clone();
                let inverse_span = inverse.span.clone();

                let old_in_inverse = replace(&mut self.in_inverse, false);
                let normal = self.compile_operand_word(normal);
                let inverse = self.compile_operand_word(inverse);
                self.in_inverse = old_in_inverse;
                let (normal_instrs, normal_sig) = normal?;
                let (inverse_instrs, inverse_sig) = inverse?;

                let opposite = Signature::new(inverse_sig.outputs, inverse_sig.args);
                if !normal_sig.is_compatible_with(opposite) {
                    self.emit_diagnostic(
                        format!(
                            "setinv's functions must have opposite signatures, \
                            but their signatures are {normal_sig} and {inverse_sig}",
                        ),
                        DiagnosticKind::Warning,
                        modified.modifier.span.clone(),
                    );
                }
                let normal_func = self.make_function(normal_span.into(), normal_sig, normal_instrs);
                let inverse_func =
                    self.make_function(inverse_span.into(), inverse_sig, inverse_instrs);
                let spandex = self.add_span(modified.modifier.span.clone());
                finish!(
                    eco_vec![
                        Instr::PushFunc(inverse_func),
                        Instr::PushFunc(normal_func),
                        Instr::Prim(Primitive::SetInverse, spandex),
                    ],
                    normal_sig
                )
            }
            Try => {
                let mut operands = modified.code_operands().cloned();
                let tried = operands.next().unwrap();
                let handler = operands.next().unwrap();
                let tried_span = tried.span.clone();
                let handler_span = handler.span.clone();
                let in_try = replace(&mut self.in_try, true);
                let (handler_new_func, handler_sig) = self.compile_operand_word(handler)?;
                let (mut try_new_func, mut try_sig) = self.compile_operand_word(tried)?;
                let span = self.add_span(modified.modifier.span.clone());

                match handler_sig.outputs.cmp(&try_sig.outputs) {
                    Ordering::Equal => {}
                    Ordering::Greater => {
                        try_sig.args += handler_sig.outputs - try_sig.outputs;
                        try_sig.outputs = handler_sig.outputs;
                        try_new_func.instrs.insert(
                            0,
                            Instr::TouchStack {
                                count: try_sig.args,
                                span,
                            },
                        );
                    }
                    Ordering::Less => self.add_error(
                        handler_span.clone(),
                        format!(
                            "Tried function cannot have more outputs \
                            than the handler function, but their \
                            signatures are {try_sig} and {handler_sig} \
                            respectively."
                        ),
                    ),
                }

                if handler_sig.args > try_sig.args + 1 {
                    self.add_error(
                        handler_span.clone(),
                        format!(
                            "Handler function must have at most \
                            one more argument than the tried function, \
                            but their signatures are {handler_sig} and \
                            {try_sig} respectively."
                        ),
                    );
                }

                let tried_func = self.make_function(tried_span.into(), try_sig, try_new_func);
                self.in_try = in_try;
                let handler_func =
                    self.make_function(handler_span.into(), handler_sig, handler_new_func);
                finish!(
                    eco_vec![
                        Instr::PushFunc(handler_func),
                        Instr::PushFunc(tried_func),
                        Instr::Prim(Primitive::Try, span),
                    ],
                    try_sig
                )
            }
            Switch => self.switch(
                modified.code_operands().cloned().collect(),
                modified.modifier.span.clone(),
                call,
            )?,
            Both => {
                let n = subscript.unwrap_or(2);
                let operand = modified.code_operands().next().unwrap().clone();
                let (mut new_func, sig) = self.compile_operand_word(operand)?;
                if let [Instr::Prim(Trace, span)] = new_func.instrs.as_slice() {
                    finish!(
                        eco_vec![Instr::ImplPrim(
                            ImplPrimitive::TraceN {
                                n,
                                inverse: false,
                                stack_sub: false
                            },
                            *span
                        )],
                        Signature::new(n, n)
                    )
                } else {
                    let span = self.add_span(modified.modifier.span.clone());
                    let instrs = take(&mut new_func.instrs);
                    for _ in 0..n.saturating_sub(1) {
                        new_func.instrs.push(Instr::PushTemp {
                            stack: TempStack::Inline,
                            count: sig.args,
                            span,
                        });
                    }
                    for _ in 0..n.saturating_sub(1) {
                        new_func.instrs.extend(instrs.iter().cloned());
                        new_func.instrs.push(Instr::pop_inline(sig.args, span));
                    }
                    new_func.instrs.extend(instrs);
                    let sig = Signature::new(sig.args * n, sig.outputs * n);
                    if call {
                        self.push_all_instrs(new_func);
                    } else {
                        let func = self.make_function(
                            modified.modifier.span.clone().into(),
                            sig,
                            new_func,
                        );
                        self.push_instr(Instr::PushFunc(func));
                    }
                }
            }
            Fill => {
                let mut operands = modified.code_operands().rev().cloned();
                if !call {
                    self.new_functions.push(NewFunction::default());
                }

                // Filled function
                let mode = replace(&mut self.pre_eval_mode, PreEvalMode::Lsp);
                let res = self.word(operands.next().unwrap(), false);
                self.pre_eval_mode = mode;
                res?;

                // Get-fill function
                let in_inverse = replace(&mut self.in_inverse, false);
                let fill_word = operands.next().unwrap();
                let fill_span = fill_word.span.clone();
                let fill = self.compile_operand_word(fill_word);
                self.in_inverse = in_inverse;
                let (fill_instrs, fill_sig) = fill?;
                if fill_sig.outputs > 1 && !self.scope.fill_sig_error {
                    self.scope.fill_sig_error = true;
                    self.add_error(
                        fill_span,
                        format!(
                            "{} function can have at most 1 output, but its signature is {}",
                            Primitive::Fill.format(),
                            fill_sig
                        ),
                    );
                }
                let fill_func = self.make_function(
                    modified.modifier.span.clone().into(),
                    fill_sig,
                    fill_instrs,
                );
                self.push_instr(Instr::PushFunc(fill_func));

                let span = self.add_span(modified.modifier.span.clone());
                self.push_instr(Instr::Prim(Primitive::Fill, span));
                if !call {
                    let new_func = self.new_functions.pop().unwrap();
                    let sig = self.sig_of(&new_func.instrs, &modified.modifier.span)?;
                    let func =
                        self.make_function(modified.modifier.span.clone().into(), sig, new_func);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Comptime => {
                let word = modified.code_operands().next().unwrap().clone();
                self.do_comptime(prim, word, &modified.modifier.span, call)?;
            }
            Reduce => {
                // Reduce content
                let operand = modified.code_operands().next().unwrap().clone();
                let Word::Modified(m) = &operand.value else {
                    return Ok(false);
                };
                let Modifier::Primitive(Content) = &m.modifier.value else {
                    return Ok(false);
                };
                if m.code_operands().count() != 1 {
                    return Ok(false);
                }
                let operand = m.code_operands().next().unwrap().clone();
                let (content_instrs, sig) = self.compile_operand_word(operand)?;
                let content_func =
                    self.make_function(m.modifier.span.clone().into(), sig, content_instrs);
                let span = self.add_span(modified.modifier.span.clone());
                let instrs = eco_vec![
                    Instr::PushFunc(content_func),
                    Instr::ImplPrim(ImplPrimitive::ReduceContent, span),
                ];
                finish!(instrs, Signature::new(1, 1));
            }
            Each => {
                // Each pervasive
                let operand = modified.code_operands().next().unwrap().clone();
                if !words_look_pervasive(slice::from_ref(&operand)) {
                    return Ok(false);
                }
                let (new_func, sig) = self.compile_operand_word(operand)?;
                let span = modified.modifier.span.clone();
                self.emit_diagnostic(
                    if let Some((prim, _)) =
                        instrs_as_flipped_primitive(&new_func.instrs, &self.asm)
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
                    span,
                );
                finish!(new_func, sig);
            }
            Table => {
                // Normal table compilation, but get some diagnostics
                let operand = modified.code_operands().next().unwrap().clone();
                let op_span = operand.span.clone();
                let instrs_len = self.asm.instrs.len();
                let (_, sig) = self.compile_operand_word(operand)?;
                match sig.args {
                    0 => self.emit_diagnostic(
                        format!("{} of 0 arguments is redundant", Table.format()),
                        DiagnosticKind::Advice,
                        op_span,
                    ),
                    1 => self.emit_diagnostic(
                        format!(
                            "{} with 1 argument is just {rows}. \
                            Use {rows} instead.",
                            Table.format(),
                            rows = Rows.format()
                        ),
                        DiagnosticKind::Advice,
                        op_span,
                    ),
                    _ => {}
                }
                self.asm.instrs.truncate(instrs_len);
                return Ok(false);
            }
            Content => {
                let operand = modified.code_operands().next().unwrap().clone();
                let (mut new_func, sig) = self.compile_operand_word(operand)?;
                let mut prefix = EcoVec::new();
                let span = self.add_span(modified.modifier.span.clone());
                if sig.args > 0 {
                    if sig.args > 1 {
                        prefix.push(Instr::push_inline(sig.args - 1, span));
                        for _ in 0..sig.args - 1 {
                            prefix.extend([
                                Instr::ImplPrim(ImplPrimitive::UnBox, span),
                                Instr::pop_inline(1, span),
                            ]);
                        }
                    }
                    prefix.push(Instr::ImplPrim(ImplPrimitive::UnBox, span));
                }
                prefix.extend(new_func.instrs);
                new_func.instrs = prefix;
                finish!(new_func, sig);
            }
            Fold => {
                let operand = modified.code_operands().next().unwrap().clone();
                let op_span = operand.span.clone();
                let (new_func, sig) = self.compile_operand_word(operand)?;
                if sig.args <= sig.outputs {
                    self.experimental_error(&modified.modifier.span, || {
                        format!(
                            "{} with arguments ≤ outputs is experimental. To use it, \
                            add `# Experimental!` to the top of the file.",
                            prim.format()
                        )
                    });
                }
                let func = self.make_function(op_span.into(), sig, new_func);
                let spandex = self.add_span(modified.modifier.span.clone());
                let instrs = eco_vec![Instr::PushFunc(func), Instr::Prim(Primitive::Fold, spandex)];
                finish!(instrs, sig);
            }
            Stringify => {
                let operand = modified.code_operands().next().unwrap();
                let s = format_word(operand, &self.asm.inputs);
                let instr = Instr::Push(s.into());
                finish!(eco_vec![instr], Signature::new(0, 1));
            }
            Quote => {
                let operand = modified.code_operands().next().unwrap().clone();
                self.new_functions.push(NewFunction::default());
                self.do_comptime(prim, operand, &modified.modifier.span, true)?;
                let new_func = self.new_functions.pop().unwrap();
                let code: String = match new_func.instrs.as_slice() {
                    [Instr::Push(Value::Char(chars))] if chars.rank() == 1 => {
                        chars.data.iter().collect()
                    }
                    [Instr::Push(Value::Char(chars))] => {
                        return Err(self.fatal_error(
                            modified.modifier.span.clone(),
                            format!(
                                "quote's argument compiled to a \
                                rank {} array rather than a string",
                                chars.rank()
                            ),
                        ))
                    }
                    [Instr::Push(value)] => {
                        return Err(self.fatal_error(
                            modified.modifier.span.clone(),
                            format!(
                                "quote's argument compiled to a \
                                {} array rather than a string",
                                value.type_name()
                            ),
                        ))
                    }
                    _ => {
                        return Err(self.fatal_error(
                            modified.modifier.span.clone(),
                            "quote's argument did not compile to a string",
                        ));
                    }
                };
                self.quote(&code, &modified.modifier.span, call)?;
            }
            Sig => {
                let operand = modified.code_operands().next().unwrap().clone();
                let (_, sig) = self.compile_operand_word(operand)?;
                let instrs = eco_vec![
                    Instr::Push(sig.outputs.into()),
                    Instr::Push(sig.args.into()),
                ];
                finish!(instrs, Signature::new(0, 2));
            }
            Struct => self.struct_(
                &modified.modifier.span,
                modified.code_operands().next().unwrap(),
            )?,
            _ => return Ok(false),
        }
        Ok(true)
    }
    fn struct_(&mut self, modifier_span: &CodeSpan, operand: &Sp<Word>) -> UiuaResult {
        if !self.current_bindings.is_empty() {
            self.add_error(
                modifier_span.clone(),
                "struct cannot be used inside a binding",
            );
        }
        let operand = operand.clone();
        match operand.value {
            Word::Array(arr) => {
                struct Field {
                    name: EcoString,
                    name_span: CodeSpan,
                    span: usize,
                    global_index: usize,
                    type_num: Option<u8>,
                    init: Option<(EcoVec<Instr>, Signature)>,
                }
                let mut fields = Vec::new();
                let module_name = if let ScopeKind::Module(name) = &self.scope.kind {
                    Some(name.clone())
                } else {
                    None
                };
                // Collect fields
                let mut invalid_syntax = false;
                for word in (arr.lines.into_iter().flatten()).filter(|word| word.value.is_code()) {
                    match word.value {
                        Word::Ref(r) if r.path.is_empty() => {
                            let span = self.add_span(word.span.clone());
                            fields.push(Field {
                                name: r.name.value,
                                name_span: word.span,
                                span,
                                global_index: 0,
                                type_num: None,
                                init: None,
                            });
                        }
                        Word::Func(func) if !fields.is_empty() => {
                            let field = fields.last_mut().unwrap();
                            if field.init.is_some() {
                                self.add_error(
                                    word.span,
                                    "Cannot have multiple initializers for a struct field",
                                );
                                continue;
                            }
                            let (_, sig, new_func) =
                                self.compile_func_instrs(func, word.span.clone(), true)?;
                            let mut instrs = new_func.instrs;
                            let sig = sig.unwrap();
                            if sig == (1, 0) {
                                instrs.insert(0, Instr::Prim(Primitive::Dup, field.span));
                            } else if sig.outputs != 1 {
                                self.add_error(
                                    word.span,
                                    format!(
                                        "Field initializer must have one output, \
                                        but its signature is {sig}"
                                    ),
                                );
                            }
                            field.init = Some((instrs, sig));
                        }
                        Word::Number(_, n) if !fields.is_empty() => {
                            let field = fields.last_mut().unwrap();
                            if field.type_num.is_some() {
                                self.add_error(
                                    word.span,
                                    "Cannot have multiple types for a struct field",
                                );
                                continue;
                            }
                            if n.fract() != 0.0 || !(0.0..4.0).contains(&n) {
                                self.add_error(word.span, format!("Invalid type number {n}"));
                            } else {
                                field.type_num = Some(n as u8);
                            }
                        }
                        Word::Primitive(Primitive::Complex) if !fields.is_empty() => {
                            let field = fields.last_mut().unwrap();
                            if field.type_num.is_some() {
                                self.add_error(
                                    word.span,
                                    "Cannot have multiple types for a struct field",
                                );
                                continue;
                            }
                            field.type_num = Some(1);
                        }
                        Word::Primitive(Primitive::Box) if !fields.is_empty() => {
                            let field = fields.last_mut().unwrap();
                            if field.type_num.is_some() {
                                self.add_error(
                                    word.span,
                                    "Cannot have multiple types for a struct field",
                                );
                                continue;
                            }
                            field.type_num = Some(3);
                        }
                        _ if !invalid_syntax => {
                            self.add_error(word.span.clone(), "invalid struct syntax");
                            invalid_syntax = true;
                        }
                        _ => {}
                    }
                }

                // Make getters
                for (i, field) in fields.iter_mut().enumerate() {
                    let name = &field.name;
                    let id = FunctionId::Named(name.clone());
                    let span = field.span;
                    let mut instrs = eco_vec![Instr::push(i), Instr::Prim(Primitive::Pick, span)];
                    if arr.boxes {
                        instrs.push(Instr::ImplPrim(ImplPrimitive::UnBox, span));
                        instrs.push(Instr::Label {
                            label: name.clone(),
                            span,
                            remove: true,
                        });
                    }
                    if let Some(type_num) = field.type_num {
                        instrs.push(Instr::ValidateType {
                            index: i + 1,
                            name: field.name.clone(),
                            type_num,
                            span,
                        });
                    }
                    let new_func = NewFunction {
                        instrs,
                        flags: FunctionFlags::TRACK_CALLER,
                    };
                    let func = self.make_function(id, Signature::new(1, 1), new_func);
                    let local = LocalName {
                        index: self.next_global,
                        public: true,
                    };
                    field.global_index = local.index;
                    self.next_global += 1;
                    let comment = if let Some(module_name) = &module_name {
                        format!("Get `{module_name}`'s `{name}`")
                    } else {
                        format!("Get `{name}`")
                    };
                    self.compile_bind_function(name.clone(), local, func, span, Some(&comment))?;
                    self.code_meta
                        .global_references
                        .insert(field.name_span.clone(), local.index);
                }

                // Make field names
                let span = self.add_span(operand.span.clone());
                let local = LocalName {
                    index: self.next_global,
                    public: true,
                };
                self.next_global += 1;
                let comment =
                    (module_name.as_ref()).map(|name| format!("Names of `{name}`'s fields"));
                let name = Ident::from("Fields");
                self.compile_bind_const(
                    name,
                    local,
                    Some(Array::from_iter(fields.iter().map(|f| f.name.as_str())).into()),
                    span,
                    comment.as_deref(),
                );

                // Make constructor
                let mut instrs = eco_vec![Instr::BeginArray];
                let constructor_args: usize = fields
                    .iter()
                    .map(|f| f.init.as_ref().map(|(_, sig)| sig.args).unwrap_or(1))
                    .sum();
                let has_inits = fields.iter().any(|f| f.init.is_some());
                if arr.boxes
                    || constructor_args < fields.len()
                    || fields.iter().any(|f| f.type_num.is_some())
                {
                    if has_inits {
                        for field in &fields {
                            if let Some((_, sig)) = field.init {
                                if sig.args > 0 {
                                    instrs.push(Instr::PushTemp {
                                        stack: TempStack::Inline,
                                        count: sig.args,
                                        span,
                                    })
                                }
                            } else {
                                instrs.push(Instr::PushTemp {
                                    stack: TempStack::Inline,
                                    count: 1,
                                    span,
                                });
                            }
                        }
                    } else if fields.len() > 1 {
                        for _ in 0..fields.len() - 1 {
                            instrs.push(Instr::PushTemp {
                                stack: TempStack::Inline,
                                count: 1,
                                span,
                            });
                        }
                    }
                    for (i, field) in fields.iter().rev().enumerate() {
                        if let Some((init, sig)) = &field.init {
                            if sig.args > 0 {
                                instrs.push(Instr::pop_inline(sig.args, span));
                            }
                            instrs.extend_from_slice(init);
                        } else if i > 0 || has_inits {
                            instrs.push(Instr::pop_inline(1, span));
                            self.code_meta
                                .global_references
                                .insert(field.name_span.clone(), field.global_index);
                        }
                        if let Some(type_num) = field.type_num {
                            instrs.push(Instr::ValidateType {
                                index: i + 1,
                                name: field.name.clone(),
                                type_num,
                                span,
                            });
                        }
                        if arr.boxes {
                            instrs.push(Instr::Label {
                                label: field.name.clone(),
                                span,
                                remove: false,
                            });
                        }
                    }
                } else {
                    instrs.push(Instr::TouchStack {
                        count: fields.len(),
                        span,
                    });
                }
                instrs.push(Instr::EndArray {
                    boxed: arr.boxes,
                    span,
                });
                let name = Ident::from("New");
                let id = FunctionId::Named(name.clone());
                let new_func = NewFunction {
                    instrs,
                    flags: FunctionFlags::TRACK_CALLER,
                };
                let func = self.make_function(id, Signature::new(constructor_args, 1), new_func);
                let local = LocalName {
                    index: self.next_global,
                    public: true,
                };
                self.next_global += 1;
                let mut comment = module_name
                    .as_ref()
                    .map(|name| format!("Create a new `{name}`\n{name} "))
                    .unwrap_or_default();
                comment.push('?');
                for field in &fields {
                    match field.init.as_ref().map(|(_, sig)| sig.args) {
                        Some(0) => continue,
                        Some(1) | None => {
                            comment.push(' ');
                            comment.push_str(&field.name);
                        }
                        Some(n) => {
                            for i in 0..n {
                                comment.push(' ');
                                comment.push_str(&field.name);
                                let mut i = i + 1;
                                while i > 0 {
                                    comment.push(SUBSCRIPT_NUMS[i % 10]);
                                    i /= 10;
                                }
                            }
                        }
                    }
                }
                self.compile_bind_function(name, local, func, span, Some(&comment))?;

                // Make args
                let args_module = self.in_scope(ScopeKind::Temp(None), |comp| {
                    // Arg getters
                    for field in &fields {
                        let name = &field.name;
                        let id = FunctionId::Named(name.clone());
                        comp.new_functions.push(NewFunction::default());
                        comp.push_instr(Instr::ImplPrim(ImplPrimitive::UnPop, field.span));
                        comp.global_index(field.global_index, field.name_span.clone(), true);
                        let mut new_func = comp.new_functions.pop().unwrap();
                        new_func.flags |= FunctionFlags::TRACK_CALLER;
                        let func = comp.make_function(id, Signature::new(0, 1), new_func);
                        let local = LocalName {
                            index: comp.next_global,
                            public: true,
                        };
                        comp.next_global += 1;
                        let comment = if let Some(module_name) = &module_name {
                            format!("`{module_name}`'s `{name}` argument")
                        } else {
                            format!("`{name}` argument")
                        };
                        comp.compile_bind_function(
                            field.name.clone(),
                            local,
                            func,
                            field.span,
                            Some(&comment),
                        )?;
                    }
                    Ok(())
                })?;
                let args_macro_index = self.next_global;
                self.next_global += 1;
                let span = &operand.span;
                self.index_macros.insert(
                    args_macro_index,
                    IndexMacro {
                        words: vec![span.clone().sp(Word::Modified(Box::new(Modified {
                            modifier: span.clone().sp(Modifier::Primitive(Primitive::Fill)),
                            operands: vec![
                                span.clone().sp(Word::Ref(Ref {
                                    path: Vec::new(),
                                    name: span.clone().sp("New".into()),
                                    in_macro_arg: false,
                                })),
                                span.clone().sp(Word::Placeholder(PlaceholderOp::Nth(0))),
                            ],
                        })))],
                        names: args_module.names,
                        sig: None,
                        hygenic: false,
                        recursive: false,
                        flags: FunctionFlags::default(),
                    },
                );
                let local = LocalName {
                    index: args_macro_index,
                    public: true,
                };
                self.scope.names.insert("Args!".into(), local);
                self.asm.add_binding_at(
                    local,
                    BindingKind::IndexMacro(1),
                    None,
                    Some(DocComment::from(format!(
                        "Take {} argument{} and bind {} to {} field name{}",
                        fields.len(),
                        if fields.len() == 1 { "" } else { "s" },
                        if fields.len() == 1 { "it" } else { "them" },
                        if let Some(name) = &module_name {
                            format!("`{name}`'s")
                        } else {
                            "the".into()
                        },
                        if fields.len() == 1 { "" } else { "s" }
                    ))),
                );
            }
            _ => self.add_error(operand.span, "struct's argument must be stack array syntax"),
        }
        Ok(())
    }
    /// Expand a positional macro
    fn expand_positional_macro(
        &mut self,
        name: Ident,
        macro_words: &mut Vec<Sp<Word>>,
        mut operands: Vec<Sp<Word>>,
        span: CodeSpan,
        hygenic: bool,
    ) -> UiuaResult {
        // Mark the operands as macro arguments
        if hygenic {
            set_in_macro_arg(&mut operands);
        }
        // Collect placeholders
        let mut ops = collect_placeholder(macro_words);
        ops.reverse();
        let span = span.merge(operands.last().unwrap().span.clone());
        // Initialize the placeholder stack
        let mut ph_stack: Vec<Sp<Word>> =
            operands.into_iter().filter(|w| w.value.is_code()).collect();
        let initial_stack = ph_stack.clone();
        let mut ignore_remaining = false;
        let mut replaced = Vec::new();
        // Run the placeholder operations
        for op in ops {
            let span = op.span;
            let op = op.value;
            if !matches!(op, PlaceholderOp::Nth(_)) {
                self.emit_diagnostic(
                    "Non-positional placeholders have been deprecated. \
                    Use positional placeholders (^0, ^1, etc.) instead.",
                    DiagnosticKind::Warning,
                    span.clone(),
                );
            }
            let mut pop = || {
                (ph_stack.pop())
                    .ok_or_else(|| self.fatal_error(span.clone(), "Operand stack is empty"))
            };
            match op {
                PlaceholderOp::Call => replaced.push(pop()?),
                PlaceholderOp::Dup => {
                    let a = pop()?;
                    ph_stack.push(a.clone());
                    ph_stack.push(a);
                }
                PlaceholderOp::Flip => {
                    let a = pop()?;
                    let b = pop()?;
                    ph_stack.push(a);
                    ph_stack.push(b);
                }
                PlaceholderOp::Over => {
                    let a = pop()?;
                    let b = pop()?;
                    ph_stack.push(b.clone());
                    ph_stack.push(a);
                    ph_stack.push(b);
                }
                PlaceholderOp::Nth(_) => {
                    ignore_remaining = true;
                }
            }
        }
        // Warn if there are operands left
        if !ignore_remaining && !ph_stack.is_empty() {
            let span = (ph_stack.first().unwrap().span.clone())
                .merge(ph_stack.last().unwrap().span.clone());
            self.emit_diagnostic(
                format!(
                    "Macro operand stack has {} item{} left",
                    ph_stack.len(),
                    if ph_stack.len() == 1 { "" } else { "s" }
                ),
                DiagnosticKind::Warning,
                span,
            );
        }
        // Replace placeholders in the macro's words
        replaced.reverse();
        self.replace_placeholders(macro_words, &initial_stack, &replaced, &mut 0)?;
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
    fn replace_placeholders(
        &self,
        words: &mut Vec<Sp<Word>>,
        initial: &[Sp<Word>],
        stack: &[Sp<Word>],
        next: &mut usize,
    ) -> UiuaResult {
        let mut error = None;
        recurse_words_mut(words, &mut |word| match &mut word.value {
            Word::Placeholder(PlaceholderOp::Call) => {
                *word = stack[*next].clone();
                *next += 1;
            }
            Word::Placeholder(PlaceholderOp::Nth(n)) => {
                if let Some(replacement) = initial.get(*n as usize) {
                    *word = replacement.clone();
                } else {
                    error = Some(self.fatal_error(
                        word.span.clone(),
                        format!(
                            "Placeholder index {n} is out of bounds of {} operands",
                            initial.len()
                        ),
                    ))
                }
            }
            _ => {}
        });
        words.retain(|word| !matches!(word.value, Word::Placeholder(_)));
        error.map_or(Ok(()), Err)
    }
    fn quote(&mut self, code: &str, span: &CodeSpan, call: bool) -> UiuaResult {
        let (items, errors, _) = parse(
            code,
            InputSrc::Macro(span.clone().into()),
            &mut self.asm.inputs,
        );
        if !errors.is_empty() {
            return Err(UiuaErrorKind::Parse(errors, self.asm.inputs.clone().into())
                .error()
                .trace_macro(span.clone()));
        }

        let top_slices_start = self.asm.top_slices.len();
        // Compile the generated items
        let temp_mode = self.pre_eval_mode.min(PreEvalMode::Line);
        let pre_eval_mod = replace(&mut self.pre_eval_mode, temp_mode);
        self.items(items, true)
            .map_err(|e| e.trace_macro(span.clone()))?;
        self.pre_eval_mode = pre_eval_mod;
        // Extract generated top-level instructions
        let mut instrs = EcoVec::new();
        for slice in (self.asm.top_slices)
            .split_off(top_slices_start)
            .into_iter()
            .rev()
        {
            for i in (slice.start..slice.start + slice.len).rev() {
                instrs.push(self.asm.instrs.remove(i));
            }
        }
        instrs.make_mut().reverse();
        if call {
            self.push_all_instrs(instrs);
        } else {
            let sig = self.sig_of(&instrs, span)?;
            let func = self.make_function(FunctionId::Anonymous(span.clone()), sig, instrs.into());
            self.push_instr(Instr::PushFunc(func));
        }
        Ok(())
    }
    fn do_comptime(
        &mut self,
        prim: Primitive,
        operand: Sp<Word>,
        span: &CodeSpan,
        call: bool,
    ) -> UiuaResult {
        if self.pre_eval_mode == PreEvalMode::Lsp {
            return self.word(operand, call);
        }
        let mut comp = self.clone();
        let (mut new_func, sig) = comp.compile_operand_word(operand)?;
        if sig.args > 0 {
            return Err(self.fatal_error(
                span.clone(),
                format!(
                    "{}'s function must have no arguments, but it has {}",
                    prim.format(),
                    sig.args
                ),
            ));
        }
        new_func.instrs = optimize_instrs(new_func.instrs, true, &comp.asm);
        if let Some(index) = instrs_unbound_index(&new_func.instrs, &comp.asm) {
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
            return Err(self.fatal_error(span.clone(), message));
        }
        let start = comp.asm.instrs.len();
        let len = new_func.instrs.len();
        comp.asm.instrs.extend(new_func.instrs);
        if len > 0 {
            comp.asm.top_slices.push(FuncSlice { start, len });
        }
        let values = match comp.macro_env.run_asm(&comp.asm) {
            Ok(_) => comp.macro_env.take_stack(),
            Err(e) => {
                if self.errors.is_empty() {
                    self.add_error(span.clone(), format!("Compile-time evaluation failed: {e}"));
                }
                vec![Value::default(); sig.outputs]
            }
        };
        if !call {
            self.new_functions.push(NewFunction::default());
        }
        let val_count = sig.outputs;
        for value in values.into_iter().rev().take(val_count).rev() {
            self.push_instr(Instr::push(value));
        }
        if !call {
            let new_func = self.new_functions.pop().unwrap();
            let sig = Signature::new(0, val_count);
            let func = self.make_function(FunctionId::Anonymous(span.clone()), sig, new_func);
            self.push_instr(Instr::PushFunc(func));
        }
        Ok(())
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

fn instrs_unbound_index(instrs: &[Instr], asm: &Assembly) -> Option<usize> {
    use Instr::*;
    for instr in instrs {
        match instr {
            CallGlobal { index, .. }
                if asm.bindings.get(*index).map_or(true, |binding| {
                    matches!(binding.kind, BindingKind::Const(None))
                }) =>
            {
                return Some(*index)
            }
            PushFunc(func) => {
                let index = instrs_unbound_index(func.instrs(asm), asm);
                if index.is_some() {
                    return index;
                }
            }
            _ => {}
        }
    }
    None
}
