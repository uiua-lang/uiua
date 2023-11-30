use std::collections::BTreeSet;

use crate::{
    ast::*, check2::instrs_signature, parse::count_placeholders, CodeSpan, Diagnostic,
    DiagnosticKind, Function2, ImplPrimitive, Instr2, Primitive, Signature, Sp, Span, SysOp,
    TempStack, UiuaError, UiuaResult,
};

#[derive(Default)]
pub struct Units {
    pub units: Vec<Unit>,
}

#[derive(Default)]
pub struct Unit {
    pub instrs: Vec<Instr2>,
    pub spans: Vec<Span>,
}

impl Units {
    pub fn function(&self, function: Function2) -> &[Instr2] {
        &self.units[function.unit].instrs[function.start..function.end]
    }
}

struct Compiler {
    unit_index: usize,
    unit: Unit,
    new_functions: Vec<Vec<Instr2>>,
    experimental: bool,
    diagnostics: BTreeSet<Diagnostic>,
}

impl Compiler {
    pub(crate) fn items(&mut self, items: Vec<Item>, in_test: bool) -> UiuaResult {
        for item in items {
            self.item(item, in_test)?;
        }
        Ok(())
    }
    fn item(&mut self, item: Item, in_test: bool) -> UiuaResult {
        fn words_have_import(words: &[Sp<Word>]) -> bool {
            words
                .iter()
                .any(|w| matches!(w.value, Word::Primitive(Primitive::Sys(SysOp::Import))))
        }
        match item {
            Item::TestScope(items) => self.in_scope(|env| env.items(items.value, true))?,
            Item::Words(words) => self.words(words, true)?,
            Item::Binding(binding) => self.binding(binding)?,
            Item::ExtraNewlines(_) => {}
        }
        Ok(())
    }
    fn binding(&mut self, binding: Binding) -> UiuaResult {
        let name = binding.name.value;
        let span = &binding.name.span;
        let sig_declared = binding.signature.is_some();
        let placeholder_count = count_placeholders(&binding.words);

        let make_fn = |mut instrs: Vec<Instr2>, sig: Signature, compiler: &mut Self| {
            // Diagnostic for function that doesn't consume its arguments
            if let Some((Instr2::Prim(Primitive::Dup, span), rest)) = instrs.split_first() {
                if let Ok(rest_sig) = instrs_signature(rest) {
                    if rest_sig.args == sig.args && rest_sig.outputs + 1 == sig.outputs {
                        compiler.diagnostic_with_span(
                            "Functions should consume their arguments. \
                            Try removing this duplicate.",
                            DiagnosticKind::Style,
                            compiler.unit.spans[*span],
                        );
                        compiler.flush_diagnostics();
                    }
                }
            }

            // Handle placeholders
            if placeholder_count > 0 {
                increment_placeholders(&mut instrs, &mut 0);
                instrs.insert(0, Instr2::PushTempFunctions(placeholder_count));
                instrs.push(Instr2::PopTempFunctions(placeholder_count));
            }
            compiler.finish_function(sig, instrs)
        };
        // Compile the body
        let instrs = self.compile_words(binding.words, true)?;
        // Resolve signature
        match instrs_signature(&instrs) {
            Ok(mut sig) => {
                // Validate signature
                if let Some(declared_sig) = &binding.signature {
                    let sig_to_check = if let [Instr2::PushFunc(f)] = instrs.as_slice() {
                        // If this is a function wrapped in parens, check the signature of the
                        // function rather than the signature of the binding's words
                        f.sig()
                    } else {
                        sig
                    };
                    if declared_sig.value == sig_to_check {
                        sig = declared_sig.value;
                    } else {
                        return Err(UiuaError::Run(Span::Code(declared_sig.span.clone()).sp(
                            format!(
                                "Function signature mismatch:  declared {} but inferred {}",
                                declared_sig.value, sig_to_check
                            ),
                        )));
                    }
                }
                if let [Instr2::PushFunc(f)] = instrs.as_slice() {
                    // Binding is a single inline function
                    self.compile_bind_function(name, f.into(), sig_declared, span.clone().into())?;
                } else if sig.args == 0
                    && (sig.outputs > 0 || instrs.is_empty())
                    && placeholder_count == 0
                {
                    // Binding's instrs must be run
                    self.exec_global_instrs(instrs)?;
                    if let Some(f) = self.function_stack.pop() {
                        // Binding is an imported function
                        self.compile_bind_function(name, f, sig_declared, span.clone().into())?;
                    } else if let Some(value) = self.stack.pop() {
                        // Binding is a constant
                        self.compile_bind_value(name, value, span.clone().into())?;
                    } else {
                        // Binding is an empty function
                        let func = make_fn(Vec::new(), sig, self);
                        self.compile_bind_function(
                            name,
                            func.into(),
                            sig_declared,
                            span.clone().into(),
                        )?;
                    }
                } else {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig, self);
                    self.compile_bind_function(
                        name,
                        func.into(),
                        sig_declared,
                        span.clone().into(),
                    )?;
                }
            }
            Err(e) => {
                if let Some(sig) = binding.signature {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig.value, self);
                    self.compile_bind_function(
                        name,
                        func.into(),
                        sig_declared,
                        span.clone().into(),
                    )?;
                } else {
                    return Err(UiuaError::Run(Span::Code(binding.name.span.clone()).sp(
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the `←`."
                            } else {
                                ""
                            }
                        ),
                    )));
                }
            }
        }
        Ok(())
    }
    pub(crate) fn compile_bind_value(
        &mut self,
        name: Ident,
        mut value: Value,
        span: Span,
    ) -> UiuaResult {
        self.validate_binding_name(&name, &[], span)?;
        value.compress();
        let mut globals = self.globals.lock();
        let idx = globals.len();
        globals.push(Global::Val(value));
        self.scope.names.insert(name, idx);
        Ok(())
    }
    pub(crate) fn compile_bind_function(
        &mut self,
        name: Ident,
        function: Arc<Function>,
        sig_declared: bool,
        span: Span,
    ) -> UiuaResult {
        self.validate_binding_name(&name, &function.instrs, span)?;
        let mut globals = self.globals.lock();
        let idx = globals.len();
        globals.push(Global::Func {
            f: function,
            sig_declared,
        });
        self.scope.names.insert(name, idx);
        Ok(())
    }
    fn validate_binding_name(&self, name: &Ident, instrs: &[Instr2], span: Span) -> UiuaResult {
        let temp_function_count = count_temp_functions(instrs);
        let name_marg_count = ident_modifier_args(name) as usize;
        if temp_function_count != name_marg_count {
            let trimmed = name.trim_end_matches('!');
            let this = format!("{}{}", trimmed, "!".repeat(temp_function_count));
            return Err(span
                .clone()
                .sp(format!(
                    "The name {name} implies {name_marg_count} modifier arguments, \
                    but the binding body references {temp_function_count}. Try `{this}`."
                ))
                .into());
        }
        Ok(())
    }
    fn compile_words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult<Vec<Instr2>> {
        self.new_functions.push(Vec::new());
        self.words(words, call)?;
        self.flush_diagnostics();
        Ok(self.new_functions.pop().unwrap())
    }
    fn flush_diagnostics(&mut self) {
        if self.print_diagnostics {
            for diagnostic in self.take_diagnostics() {
                eprintln!("{}", diagnostic.report());
            }
        }
    }
    fn compile_operand_words(
        &mut self,
        words: Vec<Sp<Word>>,
    ) -> UiuaResult<(Vec<Instr2>, Signature)> {
        let span = words
            .first()
            .zip(words.last())
            .map(|(first, last)| first.span.clone().merge(last.span.clone()));
        let mut instrs = self.compile_words(words, true)?;
        let mut sig = None;
        // Extract function instrs if possible
        if let [Instr2::PushFunc(f)] = instrs.as_slice() {
            sig = Some(f.sig());
            instrs = f.instrs.clone();
        }
        let sig = if let Some(sig) = sig {
            sig
        } else {
            instrs_signature(&instrs).map_err(|e| {
                span.unwrap()
                    .sp(format!("Cannot infer function signature: {e}"))
            })?
        };
        Ok((instrs, sig))
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        for word in words.into_iter().rev() {
            self.word(word, call)?;
        }
        Ok(())
    }
    fn inline_modifier(&mut self, modified: &Modified, call: bool) -> UiuaResult<bool> {
        use Primitive::*;
        let Modifier::Primitive(prim) = modified.modifier.value else {
            return Ok(false);
        };
        match prim {
            Dip | Gap | Reach => {
                // Compile operands
                let (mut instrs, sig) = self.compile_operand_words(modified.operands.clone())?;
                // Dip (|1 …) . diagnostic
                if prim == Dip && sig == (1, 1) {
                    if let Some(Instr2::Prim(Dup, dup_span)) =
                        self.new_functions.last().and_then(|instrs| instrs.last())
                    {
                        let span = Span::Code(modified.modifier.span.clone())
                            .merge(self.get_span(*dup_span));
                        self.diagnostic_with_span(
                            "Prefer `⊃∘(…)` over `⊙(…).` for clarity",
                            DiagnosticKind::Style,
                            span,
                        );
                    }
                }

                let span = self.add_span(modified.modifier.span.clone());
                let sig = match prim {
                    Dip => {
                        instrs.insert(
                            0,
                            Instr2::PushTemp {
                                stack: TempStack::Inline,
                                count: 1,
                                span,
                            },
                        );
                        instrs.push(Instr2::PopTemp {
                            stack: TempStack::Inline,
                            count: 1,
                            span,
                        });
                        Signature::new(sig.args + 1, sig.outputs + 1)
                    }
                    Gap => {
                        instrs.insert(0, Instr2::Prim(Pop, span));
                        Signature::new(sig.args + 1, sig.outputs)
                    }
                    Reach => {
                        let mut init = vec![
                            Instr2::PushTemp {
                                stack: TempStack::Inline,
                                count: 1,
                                span,
                            },
                            Instr2::Prim(Pop, span),
                            Instr2::PopTemp {
                                stack: TempStack::Inline,
                                count: 1,
                                span,
                            },
                        ];
                        init.extend(instrs);
                        instrs = init;
                        Signature::new(sig.args + 1, sig.outputs)
                    }
                    _ => unreachable!(),
                };
                if call {
                    self.push_instr(Instr2::PushSig(sig));
                    self.extend_instrs(instrs);
                    self.push_instr(Instr2::PopSig);
                } else {
                    let func = Function::new(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        instrs,
                        sig,
                    );
                    self.push_instr(Instr2::push_func(func));
                }
                Ok(true)
            }
            Fork => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let (b_instrs, b_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let span = self.add_span(modified.modifier.span.clone());
                let count = a_sig.args.max(b_sig.args);
                let mut instrs = vec![Instr2::PushTemp {
                    stack: TempStack::Inline,
                    count,
                    span,
                }];
                if b_sig.args > 0 {
                    instrs.push(Instr2::CopyFromTemp {
                        stack: TempStack::Inline,
                        offset: count - b_sig.args,
                        count: b_sig.args,
                        span,
                    });
                }
                instrs.extend(b_instrs);
                if count - a_sig.args > 0 {
                    instrs.push(Instr2::DropTemp {
                        stack: TempStack::Inline,
                        count: count - a_sig.args,
                        span,
                    });
                }
                instrs.push(Instr2::PopTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                });
                instrs.extend(a_instrs);
                let sig = Signature::new(a_sig.args.max(b_sig.args), a_sig.outputs + b_sig.outputs);
                if call {
                    self.push_instr(Instr2::PushSig(sig));
                    self.extend_instrs(instrs);
                    self.push_instr(Instr2::PopSig);
                } else {
                    let func = Function::new(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        instrs,
                        sig,
                    );
                    self.push_instr(Instr2::push_func(func));
                }
                Ok(true)
            }
            Bracket => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let (b_instrs, b_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let span = self.add_span(modified.modifier.span.clone());
                let mut instrs = vec![Instr2::PushTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                }];
                instrs.extend(b_instrs);
                instrs.push(Instr2::PopTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                });
                instrs.extend(a_instrs);
                let sig = Signature::new(a_sig.args + b_sig.args, a_sig.outputs + b_sig.outputs);
                if call {
                    self.push_instr(Instr2::PushSig(sig));
                    self.extend_instrs(instrs);
                    self.push_instr(Instr2::PopSig);
                } else {
                    let func = Function::new(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        instrs,
                        sig,
                    );
                    self.push_instr(Instr2::push_func(func));
                }
                Ok(true)
            }
            Invert => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let span = f.span.clone();
                let (instrs, _) = self.compile_operand_words(vec![f])?;
                if let Some(inverted) = invert_instrs(&instrs) {
                    match instrs_signature(&inverted) {
                        Ok(sig) => {
                            if call {
                                self.extend_instrs(inverted);
                            } else {
                                let func = Function::new(
                                    FunctionId::Anonymous(modified.modifier.span.clone()),
                                    inverted,
                                    sig,
                                );
                                self.push_instr(Instr2::push_func(func));
                            }
                            Ok(true)
                        }
                        Err(e) => Err(UiuaError::Run(
                            Span::Code(modified.modifier.span.clone())
                                .sp(format!("Cannot infer function signature: {e}")),
                        )),
                    }
                } else {
                    Err(span.sp("No inverse found".into()).into())
                }
            }
            Under => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let f_span = f.span.clone();
                let (f_instrs, _) = self.compile_operand_words(vec![f])?;
                let (g_instrs, g_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                if let Some((f_before, f_after)) = under_instrs(&f_instrs, g_sig) {
                    let before_sig = instrs_signature(&f_before).map_err(|e| {
                        f_span
                            .clone()
                            .sp(format!("Cannot infer function signature: {e}"))
                    })?;
                    let after_sig = instrs_signature(&f_after)
                        .map_err(|e| f_span.sp(format!("Cannot infer function signature: {e}")))?;
                    let mut instrs = f_before;
                    instrs.extend(g_instrs);
                    instrs.extend(f_after);
                    let sig = Signature::new(
                        before_sig.args + g_sig.args + after_sig.args
                            - before_sig.outputs
                            - g_sig.outputs,
                        (before_sig.outputs + g_sig.outputs)
                            .saturating_sub(g_sig.args + after_sig.args)
                            + after_sig.outputs,
                    );
                    if call {
                        self.push_instr(Instr2::PushSig(sig));
                        self.extend_instrs(instrs);
                        self.push_instr(Instr2::PopSig);
                    } else {
                        let func = Function::new(
                            FunctionId::Anonymous(modified.modifier.span.clone()),
                            instrs,
                            sig,
                        );
                        self.push_instr(Instr2::push_func(func));
                    }
                    Ok(true)
                } else {
                    Err(f_span.sp("No inverse found".into()).into())
                }
            }
            Both => {
                let mut operands = modified.code_operands().cloned();
                let (mut instrs, sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let span = self.add_span(modified.modifier.span.clone());
                instrs.insert(
                    0,
                    Instr2::PushTemp {
                        stack: TempStack::Inline,
                        count: sig.args,
                        span,
                    },
                );
                instrs.push(Instr2::PopTemp {
                    stack: TempStack::Inline,
                    count: sig.args,
                    span,
                });
                for i in 1..instrs.len() - 1 {
                    instrs.push(instrs[i].clone());
                }
                let sig = Signature::new(sig.args * 2, sig.outputs * 2);
                if call {
                    self.push_instr(Instr2::PushSig(sig));
                    self.extend_instrs(instrs);
                    self.push_instr(Instr2::PopSig);
                } else {
                    let func = Function::new(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        instrs,
                        sig,
                    );
                    self.push_instr(Instr2::push_func(func));
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) -> UiuaResult {
        self.handle_primitive_experimental(prim, &span)?;
        self.handle_primitive_deprecation(prim, &span);
        let span_i = self.add_span(span.clone());
        if call {
            self.push_instr(Instr2::Prim(prim, span_i));
        } else {
            let instrs = [Instr2::Prim(prim, span_i)];
            let sig = match instrs_signature(&instrs) {
                Ok(sig) => sig,
                Err(e) => {
                    return Err(span
                        .sp(format!("Cannot infer function signature: {e}"))
                        .into())
                }
            };
            let func = self.finish_function(sig, instrs);
            self.push_instr(Instr2::PushFunc(func))
        }
        Ok(())
    }
    fn finish_function(
        &mut self,
        sig: Signature,
        instrs: impl IntoIterator<Item = Instr2>,
    ) -> Function2 {
        let start = self.unit.instrs.len();
        self.unit.instrs.extend(instrs);
        let end = self.unit.instrs.len();
        Function2 {
            unit: self.unit_index,
            start,
            end,
            sig,
        }
    }
    /// Push an instruction to the current function being compiled
    ///
    /// Also performs some optimizations if the instruction and the previous
    /// instruction form some known pattern
    fn push_instr(&mut self, instr: Instr2) {
        use ImplPrimitive::*;
        use Primitive::*;
        let instrs = self.new_functions.last_mut().unwrap();
        // Optimizations
        match (instrs.as_mut_slice(), instr) {
            // Cosine
            ([.., Instr2::Prim(Eta, _), Instr2::Prim(Add, _)], Instr2::Prim(Sin, span)) => {
                instrs.pop();
                instrs.pop();
                instrs.push(Instr2::ImplPrim(Cos, span));
            }
            // First Rise = FirstMinIndex
            ([.., Instr2::Prim(Rise, _)], Instr2::Prim(First, span)) => {
                instrs.pop();
                instrs.push(Instr2::ImplPrim(FirstMinIndex, span))
            }
            // First Reverse Fall = LastMinIndex
            ([.., Instr2::Prim(Fall, _), Instr2::Prim(Reverse, _)], Instr2::Prim(First, span)) => {
                instrs.pop();
                instrs.pop();
                instrs.push(Instr2::ImplPrim(LastMinIndex, span))
            }
            // First Fall = FirstMaxIndex
            ([.., Instr2::Prim(Fall, _)], Instr2::Prim(First, span)) => {
                instrs.pop();
                instrs.push(Instr2::ImplPrim(FirstMaxIndex, span))
            }
            // First Reverse Rise = LastMaxIndex
            ([.., Instr2::Prim(Rise, _), Instr2::Prim(Reverse, _)], Instr2::Prim(First, span)) => {
                instrs.pop();
                instrs.pop();
                instrs.push(Instr2::ImplPrim(LastMaxIndex, span))
            }
            // First Reverse = last
            ([.., Instr2::Prim(Reverse, _)], Instr2::Prim(First, span)) => {
                instrs.pop();
                instrs.push(Instr2::ImplPrim(Last, span))
            }
            // Combine push temps
            (
                [.., Instr2::PushTemp {
                    stack: a_stack,
                    count: a_count,
                    ..
                }],
                Instr2::PushTemp {
                    stack: b_stack,
                    count: b_count,
                    ..
                },
            ) if *a_stack == b_stack => {
                *a_count += b_count;
            }
            // Combine pop temps
            (
                [.., Instr2::PopTemp {
                    stack: a_stack,
                    count: a_count,
                    ..
                }],
                Instr2::PopTemp {
                    stack: b_stack,
                    count: b_count,
                    ..
                },
            ) if *a_stack == b_stack => {
                *a_count += b_count;
            }
            // // Coalesce inline stack ops
            // ([.., Instr2::])
            (_, instr) => instrs.push(instr),
        }
    }
    fn handle_primitive_deprecation(&mut self, prim: Primitive, span: &CodeSpan) {
        if let Some(suggestion) = prim.deprecation_suggestion() {
            let suggestion = if suggestion.is_empty() {
                String::new()
            } else {
                format!(", {suggestion}")
            };
            self.diagnostics.insert(Diagnostic::new(
                format!(
                    "{} is deprecated and will be removed in a future version{}",
                    prim.format(),
                    suggestion
                ),
                span.clone(),
                DiagnosticKind::Warning,
            ));
        }
    }
    fn handle_primitive_experimental(&self, prim: Primitive, span: &CodeSpan) -> UiuaResult {
        if prim.is_experimental() && !self.experimental {
            return Err(span
                .clone()
                .sp(format!(
                    "{} is experimental. To use it, add \
                    `# Experimental!` to the top of the file.",
                    prim.format()
                ))
                .into());
        }
        Ok(())
    }
    /// Register a span
    fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let idx = self.unit.spans.len();
        self.unit.spans.push(span.into());
        idx
    }
    /// Construct an error with the current span
    pub fn error(&self, message: impl ToString) -> UiuaError {
        UiuaError::Run(self.span().clone().sp(message.to_string()))
    }
    /// Construct and add a diagnostic with the current span
    pub fn diagnostic(&mut self, message: impl Into<String>, kind: DiagnosticKind) {
        self.diagnostic_with_span(message, kind, self.span());
    }
    /// Construct and add a diagnostic with a custom span
    pub fn diagnostic_with_span(
        &mut self,
        message: impl Into<String>,
        kind: DiagnosticKind,
        span: impl Into<Span>,
    ) {
        self.diagnostics
            .insert(Diagnostic::new(message.into(), span, kind));
    }
}

fn words_look_pervasive(words: &[Sp<Word>]) -> bool {
    use Primitive::*;
    words.iter().all(|word| match &word.value {
        Word::Primitive(p) if p.class().is_pervasive() => true,
        Word::Primitive(
            Dup | Flip | Over | Dip | Identity | Fork | Both | Bracket | Under | Each,
        ) => true,
        Word::Func(func) if func.lines.iter().all(|line| words_look_pervasive(line)) => true,
        Word::Number(..) | Word::Char(..) => true,
        Word::Modified(m) if m.modifier.value == Modifier::Primitive(Primitive::Each) => true,
        _ => false,
    })
}

fn increment_placeholders(instrs: &mut [Instr2], curr: &mut usize) {
    for instr in instrs {
        match instr {
            Instr2::GetTempFunction { offset, .. } => {
                *offset = *curr;
                *curr += 1;
            }
            Instr2::PushFunc(f) => {
                increment_placeholders(&mut Arc::make_mut(f).instrs, curr);
            }
            _ => (),
        }
    }
}

fn count_temp_functions(instrs: &[Instr2]) -> usize {
    let mut count = 0;
    for instr in instrs {
        match instr {
            Instr2::GetTempFunction { .. } => count += 1,
            Instr2::PushFunc(f) if matches!(f.id, FunctionId::Anonymous(_)) => {
                count += count_temp_functions(&f.instrs);
            }
            _ => (),
        }
    }
    count
}
