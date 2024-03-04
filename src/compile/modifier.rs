//! Compiler code for modifiers

use crate::format::format_words;

use super::*;

impl Compiler {
    #[allow(clippy::collapsible_match)]
    pub(super) fn modified(&mut self, mut modified: Modified, call: bool) -> UiuaResult {
        let mut op_count = modified.code_operands().count();

        // De-sugar function pack
        if op_count == 1 {
            let operand = modified.code_operands().next().unwrap().clone();
            if let Sp {
                value: Word::Switch(sw @ Switch { angled: false, .. }),
                span,
            } = operand
            {
                match &modified.modifier.value {
                    Modifier::Primitive(Primitive::Dip) => {
                        let mut branches = sw.branches.into_iter().rev();
                        let mut new = Modified {
                            modifier: modified.modifier.clone(),
                            operands: vec![branches.next().unwrap().map(Word::Func)],
                        };
                        for branch in branches {
                            let mut lines = branch.value.lines;
                            (lines.last_mut().unwrap())
                                .push(span.clone().sp(Word::Modified(Box::new(new))));
                            new = Modified {
                                modifier: modified.modifier.clone(),
                                operands: vec![branch.span.clone().sp(Word::Func(Func {
                                    id: FunctionId::Anonymous(branch.span.clone()),
                                    signature: None,
                                    lines,
                                    closed: true,
                                }))],
                            };
                        }
                        return self.modified(new, call);
                    }
                    Modifier::Primitive(Primitive::Fork | Primitive::Bracket) => {
                        let mut branches = sw.branches.into_iter().rev();
                        let mut new = Modified {
                            modifier: modified.modifier.clone(),
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
                                modifier: modified.modifier.clone(),
                                operands: vec![
                                    branch.map(Word::Func),
                                    span.clone().sp(Word::Modified(Box::new(new))),
                                ],
                            };
                        }
                        return self.modified(new, call);
                    }
                    Modifier::Primitive(Primitive::Cascade) => {
                        let mut branches = sw.branches.into_iter().rev();
                        let mut new = Modified {
                            modifier: modified.modifier.clone(),
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
                                modifier: modified.modifier.clone(),
                                operands: vec![
                                    branch.map(Word::Func),
                                    span.clone().sp(Word::Modified(Box::new(new))),
                                ],
                            };
                        }
                        return self.modified(new, call);
                    }
                    modifier if modifier.args() >= 2 => {
                        if sw.branches.len() != modifier.args() {
                            return Err(self.fatal_error(
                                modified.modifier.span.clone().merge(span),
                                format!(
                                    "{} requires {} function arguments, but the \
                                    function pack has {} functions",
                                    modifier,
                                    modifier.args(),
                                    sw.branches.len()
                                ),
                            ));
                        }
                        let new = Modified {
                            modifier: modified.modifier.clone(),
                            operands: sw.branches.into_iter().map(|w| w.map(Word::Func)).collect(),
                        };
                        return self.modified(new, call);
                    }
                    modifier => 'blk: {
                        if let Modifier::Ref(name) = modifier {
                            if let Ok((_, local)) = self.ref_local(name) {
                                if self.array_macros.contains_key(&local.index) {
                                    break 'blk;
                                }
                            }
                        }
                        return Err(self.fatal_error(
                            modified.modifier.span.clone().merge(span),
                            format!(
                                "{modifier} cannot use a function pack. If you meant to \
                                use a switch function, add a layer of parentheses."
                            ),
                        ));
                    }
                }
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
            op_count = modified.operands.len();
        }
        if op_count == modified.modifier.value.args() {
            // Inlining
            if self.inline_modifier(&modified, call)? {
                return Ok(());
            }
        } else {
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

        // Handle macros
        let prim = match modified.modifier.value {
            Modifier::Primitive(prim) => prim,
            Modifier::Ref(r) => {
                let (path_locals, local) = self.ref_local(&r)?;
                self.validate_local(&r.name.value, local, &r.name.span);
                self.code_meta
                    .global_references
                    .insert(r.name.clone(), local.index);
                for (local, comp) in path_locals.into_iter().zip(&r.path) {
                    (self.code_meta.global_references).insert(comp.module.clone(), local.index);
                }
                // Handle recursion depth
                self.macro_depth += 1;
                if self.macro_depth > 20 {
                    return Err(
                        self.fatal_error(modified.modifier.span.clone(), "Macro recurs too deep")
                    );
                }
                if let Some(mut words) = self.stack_macros.get(&local.index).cloned() {
                    // Stack macros
                    let instrs = self
                        .expand_macro(
                            r.name.value.clone(),
                            &mut words,
                            modified.operands,
                            modified.modifier.span.clone(),
                        )
                        .and_then(|()| self.compile_words(words, true));
                    let instrs = instrs?;
                    match instrs_signature(&instrs) {
                        Ok(sig) => {
                            let func = self.add_function(
                                FunctionId::Named(r.name.value.clone()),
                                sig,
                                instrs,
                            );
                            self.push_instr(Instr::PushFunc(func));
                        }
                        Err(e) => self.add_error(
                            modified.modifier.span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        ),
                    }
                    if call {
                        let span = self.add_span(modified.modifier.span);
                        self.push_instr(Instr::Call(span));
                    }
                } else if let Some(function) = self.array_macros.get(&local.index).cloned() {
                    // Array macros
                    let full_span = (modified.modifier.span.clone())
                        .merge(modified.operands.last().unwrap().span.clone());

                    // Collect operands as strings
                    let mut operands: Vec<Sp<Word>> = (modified.operands.into_iter())
                        .filter(|w| w.value.is_code())
                        .collect();
                    if operands.len() == 1 {
                        let operand = operands.remove(0);
                        operands = match operand.value {
                            Word::Switch(sw) => {
                                sw.branches.into_iter().map(|b| b.map(Word::Func)).collect()
                            }
                            word => vec![operand.span.sp(word)],
                        };
                    }
                    let op_sigs = if function.signature().args == 2 {
                        let mut comp = self.clone();
                        let mut sig_data: EcoVec<u8> = EcoVec::with_capacity(operands.len() * 2);
                        for op in &operands {
                            let (_, sig) = comp.compile_operand_word(op.clone())?;
                            sig_data.extend_from_slice(&[sig.args as u8, sig.outputs as u8]);
                        }
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
                            Boxed(formatted.into())
                        })
                        .collect();

                    let mut code = String::new();
                    (|| -> UiuaResult {
                        self.prepare_env()?;
                        let env = &mut self.macro_env;
                        // Run the macro function
                        if let Some(sigs) = op_sigs {
                            env.push(sigs);
                        }
                        env.push(formatted);
                        env.call(function)?;
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
                    self.quote(&code, &modified.modifier.span, call)?;
                } else {
                    return Err(self.fatal_error(
                        modified.modifier.span.clone(),
                        format!(
                            "Macro {} not found. This is a bug in the interpreter.",
                            r.name.value
                        ),
                    ));
                }
                self.macro_depth -= 1;

                return Ok(());
            }
        };

        // Give advice about redundancy
        match prim {
            m @ Primitive::Each if self.macro_depth == 0 => {
                if let [Sp {
                    value: Word::Primitive(prim),
                    span,
                }] = modified.operands.as_slice()
                {
                    if prim.class().is_pervasive() {
                        let span = modified.modifier.span.clone().merge(span.clone());
                        self.emit_diagnostic(
                            format!(
                                "Using {m} with a pervasive primitive like {p} is \
                                redundant. Just use {p} by itself.",
                                m = m.format(),
                                p = prim.format(),
                            ),
                            DiagnosticKind::Advice,
                            span,
                        );
                    }
                } else if words_look_pervasive(&modified.operands) {
                    let span = modified.modifier.span.clone();
                    self.emit_diagnostic(
                        format!(
                            "{m}'s function is pervasive, \
                                so {m} is redundant here.",
                            m = m.format()
                        ),
                        DiagnosticKind::Advice,
                        span,
                    );
                }
            }
            _ => {}
        }

        // Compile operands
        let instrs = self.compile_words(modified.operands, false)?;

        // Reduce monadic deprectation message
        if let (Modifier::Primitive(Primitive::Reduce), [Instr::PushFunc(f)]) =
            (&modified.modifier.value, instrs.as_slice())
        {
            if f.signature().args == 1 {
                self.emit_diagnostic(
                    format!(
                        "{} with a monadic function is deprecated. \
                        Prefer {} with stack array notation, i.e. `°[⊙⊙∘]`",
                        Primitive::Reduce.format(),
                        Primitive::Un.format()
                    ),
                    DiagnosticKind::Warning,
                    modified.modifier.span.clone(),
                );
            }
        }

        if call {
            self.push_all_instrs(instrs);
            self.primitive(prim, modified.modifier.span, true);
        } else {
            self.new_functions.push(EcoVec::new());
            self.push_all_instrs(instrs);
            self.primitive(prim, modified.modifier.span.clone(), true);
            let instrs = self.new_functions.pop().unwrap();
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
                Err(e) => self.add_error(
                    modified.modifier.span.clone(),
                    format!("Cannot infer function signature: {e}"),
                ),
            }
        }
        Ok(())
    }
    pub(super) fn inline_modifier(&mut self, modified: &Modified, call: bool) -> UiuaResult<bool> {
        use Primitive::*;
        let Modifier::Primitive(prim) = modified.modifier.value else {
            return Ok(false);
        };
        macro_rules! finish {
            ($instrs:expr, $sig:expr) => {{
                if call {
                    self.push_all_instrs($instrs);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        $sig,
                        $instrs.to_vec(),
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }};
        }
        match prim {
            Dip | Gap | On | By => {
                // Compile operands
                let (mut instrs, sig) = self.compile_operand_word(modified.operands[0].clone())?;
                // Dip (|1 …) . diagnostic
                if prim == Dip && sig == (1, 1) {
                    if let Some(Instr::Prim(Dup, dup_span)) =
                        self.new_functions.last().and_then(|instrs| instrs.last())
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

                let span = self.add_span(modified.modifier.span.clone());
                let sig = match prim {
                    Dip => {
                        instrs.insert(
                            0,
                            Instr::PushTemp {
                                stack: TempStack::Inline,
                                count: 1,
                                span,
                            },
                        );
                        instrs.push(Instr::PopTemp {
                            stack: TempStack::Inline,
                            count: 1,
                            span,
                        });
                        Signature::new(sig.args + 1, sig.outputs + 1)
                    }
                    Gap => {
                        instrs.insert(0, Instr::Prim(Pop, span));
                        Signature::new(sig.args + 1, sig.outputs)
                    }
                    On => {
                        instrs.insert(
                            0,
                            Instr::CopyToTemp {
                                stack: TempStack::Inline,
                                count: 1,
                                span,
                            },
                        );
                        instrs.push(Instr::PopTemp {
                            stack: TempStack::Inline,
                            count: 1,
                            span,
                        });
                        Signature::new(sig.args, sig.outputs + 1)
                    }
                    By => {
                        if sig.args > 0 {
                            let mut i = 0;
                            if sig.args > 1 {
                                instrs.insert(
                                    i,
                                    Instr::PushTemp {
                                        stack: TempStack::Inline,
                                        count: sig.args - 1,
                                        span,
                                    },
                                );
                                i += 1;
                            }
                            instrs.insert(i, Instr::Prim(Dup, span));
                            i += 1;
                            if sig.args > 1 {
                                instrs.insert(
                                    i,
                                    Instr::PopTemp {
                                        stack: TempStack::Inline,
                                        count: sig.args - 1,
                                        span,
                                    },
                                );
                            }
                        }
                        Signature::new(sig.args, sig.outputs + 1)
                    }
                    _ => unreachable!(),
                };
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
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
                let (a_instrs, a_sig) = self.compile_operand_word(first_op)?;
                let (b_instrs, b_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let span = self.add_span(modified.modifier.span.clone());
                let count = a_sig.args.max(b_sig.args);
                let mut instrs = vec![Instr::PushTemp {
                    stack: TempStack::Inline,
                    count,
                    span,
                }];
                if b_sig.args > 0 {
                    instrs.push(Instr::CopyFromTemp {
                        stack: TempStack::Inline,
                        offset: count - b_sig.args,
                        count: b_sig.args,
                        span,
                    });
                }
                instrs.extend(b_instrs);
                if count - a_sig.args > 0 {
                    instrs.push(Instr::DropTemp {
                        stack: TempStack::Inline,
                        count: count - a_sig.args,
                        span,
                    });
                }
                instrs.push(Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                });
                instrs.extend(a_instrs);
                let sig = Signature::new(a_sig.args.max(b_sig.args), a_sig.outputs + b_sig.outputs);
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Cascade => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let (b_instrs, b_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let span = self.add_span(modified.modifier.span.clone());
                let count = a_sig.args.saturating_sub(b_sig.outputs);
                if a_sig.args < b_sig.outputs {
                    self.emit_diagnostic(
                        format!(
                            "{}'s second function has more outputs \
                            than its first function has arguments, \
                            so {} is redundant here.",
                            prim.format(),
                            prim.format()
                        ),
                        DiagnosticKind::Advice,
                        modified.modifier.span.clone(),
                    );
                }
                let mut instrs = Vec::new();
                if count > 0 {
                    instrs.push(Instr::CopyToTemp {
                        stack: TempStack::Inline,
                        count,
                        span,
                    });
                }
                instrs.extend(b_instrs);
                if count > 0 {
                    instrs.push(Instr::PopTemp {
                        stack: TempStack::Inline,
                        count,
                        span,
                    });
                }
                instrs.extend(a_instrs);
                let sig = Signature::new(
                    b_sig.args.max(count),
                    a_sig.outputs.max(count.saturating_sub(b_sig.outputs)),
                );
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Bracket => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let (b_instrs, b_sig) = self.compile_operand_word(operands.next().unwrap())?;
                let span = self.add_span(modified.modifier.span.clone());
                let mut instrs = vec![Instr::PushTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                }];
                instrs.extend(b_instrs);
                instrs.push(Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                });
                instrs.extend(a_instrs);
                let sig = Signature::new(a_sig.args + b_sig.args, a_sig.outputs + b_sig.outputs);
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Un => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let span = f.span.clone();
                let (instrs, _) = self.compile_operand_word(f)?;
                if let Some(inverted) = invert_instrs(&instrs, self) {
                    let sig = instrs_signature(&inverted).map_err(|e| {
                        self.fatal_error(
                            span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )
                    })?;
                    if call {
                        self.push_all_instrs(inverted);
                    } else {
                        let id = FunctionId::Anonymous(modified.modifier.span.clone());
                        let func = self.add_function(id, sig, inverted);
                        self.push_instr(Instr::PushFunc(func));
                    }
                } else {
                    return Err(self.fatal_error(span, "No inverse found"));
                }
            }
            Under => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let f_span = f.span.clone();
                let (f_instrs, _) = self.compile_operand_word(f)?;
                let (g_instrs, g_sig) = self.compile_operand_word(operands.next().unwrap())?;
                if let Some((f_before, f_after)) = under_instrs(&f_instrs, g_sig, self) {
                    let before_sig = instrs_signature(&f_before).map_err(|e| {
                        self.fatal_error(
                            f_span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )
                    })?;
                    let after_sig = instrs_signature(&f_after).map_err(|e| {
                        self.fatal_error(
                            f_span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )
                    })?;
                    let mut instrs = if call {
                        eco_vec![Instr::PushSig(before_sig)]
                    } else {
                        EcoVec::new()
                    };
                    instrs.extend(f_before);
                    if call {
                        instrs.push(Instr::PopSig);
                    }
                    instrs.extend(g_instrs);
                    if call {
                        instrs.push(Instr::PushSig(after_sig));
                    }
                    instrs.extend(f_after);
                    if call {
                        instrs.push(Instr::PopSig);
                    }
                    if call {
                        self.push_all_instrs(instrs);
                    } else {
                        match instrs_signature(&instrs) {
                            Ok(sig) => {
                                let func = self.add_function(
                                    FunctionId::Anonymous(modified.modifier.span.clone()),
                                    sig,
                                    instrs,
                                );
                                self.push_instr(Instr::PushFunc(func));
                            }
                            Err(e) => self.add_error(
                                modified.modifier.span.clone(),
                                format!("Cannot infer function signature: {e}"),
                            ),
                        }
                    }
                } else {
                    return Err(self.fatal_error(f_span, "No inverse found"));
                }
            }
            Both => {
                let mut operands = modified.code_operands().cloned();
                let (mut instrs, sig) = self.compile_operand_word(operands.next().unwrap())?;
                let span = self.add_span(modified.modifier.span.clone());
                instrs.insert(
                    0,
                    Instr::PushTemp {
                        stack: TempStack::Inline,
                        count: sig.args,
                        span,
                    },
                );
                instrs.push(Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: sig.args,
                    span,
                });
                for i in 1..instrs.len() - 1 {
                    instrs.push(instrs[i].clone());
                }
                let sig = Signature::new(sig.args * 2, sig.outputs * 2);
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Bind => {
                let operand = modified.code_operands().next().cloned().unwrap();
                let operand_span = operand.span.clone();
                self.scope.bind_locals.push(HashSet::new());
                let (mut instrs, mut sig) = self.compile_operand_word(operand)?;
                let locals = self.scope.bind_locals.pop().unwrap();
                let local_count = locals.into_iter().max().map_or(0, |i| i + 1);
                let span = self.add_span(modified.modifier.span.clone());
                sig.args += local_count;
                if sig.args < 3 {
                    self.emit_diagnostic(
                        format!(
                            "{} should be reserved for functions with at least 3 arguments, \
                            but this function has {} arguments",
                            Bind.format(),
                            sig.args
                        ),
                        DiagnosticKind::Advice,
                        operand_span,
                    );
                }
                instrs.insert(
                    0,
                    Instr::PushLocals {
                        count: sig.args,
                        span,
                    },
                );
                instrs.push(Instr::PopLocals);
                finish!(instrs, sig);
            }
            Comptime => {
                let word = modified.code_operands().next().unwrap().clone();
                self.do_comptime(prim, word, &modified.modifier.span, call)?;
            }
            Reduce => {
                // Reduce content
                let operand = modified.code_operands().next().cloned().unwrap();
                let Word::Modified(m) = &operand.value else {
                    return Ok(false);
                };
                let Modifier::Primitive(Content) = &m.modifier.value else {
                    return Ok(false);
                };
                if m.code_operands().count() != 1 {
                    return Ok(false);
                }
                let operand = m.code_operands().next().cloned().unwrap();
                let (content_instrs, sig) = self.compile_operand_word(operand)?;
                if sig.args == 1 {
                    self.emit_diagnostic(
                        format!(
                            "{} with a monadic function is deprecated. \
                                        Prefer {} with stack array notation, i.e. `°[⊙⊙∘]`",
                            Primitive::Reduce.format(),
                            Primitive::Un.format()
                        ),
                        DiagnosticKind::Warning,
                        modified.modifier.span.clone(),
                    );
                }
                let content_func = self.add_function(
                    FunctionId::Anonymous(m.modifier.span.clone()),
                    sig,
                    content_instrs,
                );
                let span = self.add_span(modified.modifier.span.clone());
                let instrs = eco_vec![
                    Instr::PushFunc(content_func),
                    Instr::ImplPrim(ImplPrimitive::ReduceContent, span),
                ];
                finish!(instrs, Signature::new(1, 1));
            }
            Table => {
                // Normalize table compilation, but get some diagnostics
                let operand = modified.code_operands().next().cloned().unwrap();
                let op_span = operand.span.clone();
                let function_id = FunctionId::Anonymous(op_span.clone());
                let (instrs, sig) = self.compile_operand_word(operand)?;
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
                let func = self.add_function(function_id, sig, instrs);
                let span = self.add_span(modified.modifier.span.clone());
                let instrs = [Instr::PushFunc(func), Instr::Prim(Table, span)];
                finish!(instrs, sig);
            }
            Content => {
                let operand = modified.code_operands().next().cloned().unwrap();
                let (instrs, sig) = self.compile_operand_word(operand)?;
                let mut prefix = EcoVec::new();
                let span = self.add_span(modified.modifier.span.clone());
                if sig.args > 0 {
                    if sig.args > 1 {
                        prefix.push(Instr::PushTemp {
                            stack: TempStack::Inline,
                            count: sig.args - 1,
                            span,
                        });
                        for _ in 0..sig.args - 1 {
                            prefix.extend([
                                Instr::ImplPrim(ImplPrimitive::InvBox, span),
                                Instr::PopTemp {
                                    stack: TempStack::Inline,
                                    count: 1,
                                    span,
                                },
                            ]);
                        }
                    }
                    prefix.push(Instr::ImplPrim(ImplPrimitive::InvBox, span));
                }
                prefix.extend(instrs);
                finish!(prefix, sig);
            }
            Stringify => {
                let operand = modified.code_operands().next().unwrap();
                let s = format_word(operand, &self.asm.inputs);
                let instr = Instr::Push(s.into());
                finish!([instr], Signature::new(0, 1));
            }
            Quote => {
                let operand = modified.code_operands().next().unwrap().clone();
                self.new_functions.push(EcoVec::new());
                self.do_comptime(prim, operand, &modified.modifier.span, true)?;
                let instrs = self.new_functions.pop().unwrap();
                let code: String = match instrs.as_slice() {
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
                let instrs = [
                    Instr::Push(sig.outputs.into()),
                    Instr::Push(sig.args.into()),
                ];
                finish!(instrs, Signature::new(0, 2));
            }
            _ => return Ok(false),
        }
        self.handle_primitive_experimental(prim, &modified.modifier.span);
        self.handle_primitive_deprecation(prim, &modified.modifier.span);
        Ok(true)
    }
    /// Expand a stack macro
    fn expand_macro(
        &mut self,
        name: Ident,
        macro_words: &mut Vec<Sp<Word>>,
        operands: Vec<Sp<Word>>,
        span: CodeSpan,
    ) -> UiuaResult {
        let mut ops = collect_placeholder(macro_words);
        ops.reverse();
        let span = span.merge(operands.last().unwrap().span.clone());
        let mut ph_stack: Vec<Sp<Word>> =
            operands.into_iter().filter(|w| w.value.is_code()).collect();
        let mut replaced = Vec::new();
        for op in ops {
            let span = op.span;
            let op = op.value;
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
            }
        }
        if !ph_stack.is_empty() {
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
        let mut operands = replaced.into_iter().rev();
        replace_placeholders(macro_words, &mut || operands.next().unwrap());
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
    fn quote(&mut self, code: &str, span: &CodeSpan, call: bool) -> UiuaResult {
        let (items, errors, _) = parse(
            code,
            InputSrc::Macro(span.clone().into()),
            &mut self.asm.inputs,
        );
        if !errors.is_empty() {
            return Err(
                UiuaError::Parse(errors, self.asm.inputs.clone().into()).trace_macro(span.clone())
            );
        }

        // Compile the generated items
        for item in items {
            match item {
                Item::Words(words) => {
                    for line in words {
                        self.words(line, call)
                            .map_err(|e| e.trace_macro(span.clone()))?;
                    }
                }
                Item::Binding(binding) => self
                    .binding(binding, None)
                    .map_err(|e| e.trace_macro(span.clone()))?,
                Item::Import(import) => self
                    .import(import, None)
                    .map_err(|e| e.trace_macro(span.clone()))?,
                Item::TestScope(_) => {
                    self.add_error(span.clone(), "Macros may not generate test scopes")
                }
            };
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
        let mut comp = self.clone();
        let (instrs, sig) = comp.compile_operand_word(operand)?;
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
        let instrs = optimize_instrs(instrs, true, &comp);
        let start = comp.asm.instrs.len();
        let len = instrs.len();
        comp.asm.instrs.extend(instrs);
        comp.asm.top_slices.push(FuncSlice { start, len });
        comp.prepare_env()?;
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
            self.new_functions.push(EcoVec::new());
        }
        let val_count = sig.outputs;
        for value in values.into_iter().rev().take(val_count).rev() {
            self.push_instr(Instr::push(value));
        }
        if !call {
            let instrs = self.new_functions.pop().unwrap();
            let sig = Signature::new(0, val_count);
            let func = self.add_function(FunctionId::Anonymous(span.clone()), sig, instrs);
            self.push_instr(Instr::PushFunc(func));
        }
        Ok(())
    }
    pub(super) fn prepare_env(&mut self) -> UiuaResult {
        let top_slices = take(&mut self.macro_env.asm.top_slices);
        let mut bindings = take(&mut self.macro_env.asm.bindings);
        bindings.extend_from_slice(&self.asm.bindings[bindings.len()..]);
        self.macro_env.asm = self.asm.clone();
        self.macro_env.asm.bindings = bindings;
        if let Some(last_slice) = top_slices.last() {
            (self.macro_env.asm.top_slices).retain(|slice| slice.start > last_slice.start);
        }
        self.macro_env.no_io(Uiua::run_top_slices)?;
        Ok(())
    }
}
