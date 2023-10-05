use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{
    array::Array,
    ast::*,
    check::instrs_signature,
    function::*,
    lex::{CodeSpan, Sp, Span},
    primitive::Primitive,
    run::RunMode,
    value::Value,
    Diagnostic, DiagnosticKind, Ident, SysOp, UiuaError, UiuaResult,
};

use crate::Uiua;

impl Uiua {
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
            Item::Scoped { items, test } => {
                let scope_stack = self.in_scope(true, |env| env.items(items, test))?;
                self.stack.extend(scope_stack);
            }
            Item::Words(words) => {
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::Test => in_test,
                    RunMode::All => true,
                };
                if can_run || words_have_import(&words) {
                    let instrs = self.compile_words(words, true)?;
                    self.exec_global_instrs(instrs)?;
                }
            }
            Item::Binding(binding) => {
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::All | RunMode::Test => true,
                };
                if can_run || words_have_import(&binding.words) {
                    self.binding(binding)?;
                }
            }
            Item::ExtraNewlines(_) => {}
        }
        Ok(())
    }
    fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let mut spans = self.spans.lock();
        let idx = spans.len();
        spans.push(span.into());
        idx
    }
    fn binding(&mut self, binding: Binding) -> UiuaResult {
        let instrs = self.compile_words(binding.words, true)?;
        let make_fn = |instrs: Vec<Instr>, sig: Signature| {
            let func = Function::new(
                FunctionId::Named(binding.name.value.clone()),
                instrs,
                FunctionKind::Normal,
                sig,
            );
            Value::from(func)
        };
        let mut val = match instrs_signature(&instrs) {
            Ok(mut sig) => {
                if let Some(declared_sig) = &binding.signature {
                    if declared_sig.value.is_superset_of(sig) {
                        sig = declared_sig.value;
                    } else {
                        return Err(UiuaError::Run(Span::Code(declared_sig.span.clone()).sp(
                            format!(
                                "Function signature mismatch: \
                                 declared {} but inferred {}",
                                declared_sig.value, sig
                            ),
                        )));
                    }
                }

                if sig.args == 0 {
                    self.exec_global_instrs(instrs)?;
                    if let Some(value) = self.stack.pop() {
                        match value {
                            Value::Func(fs) => match fs.into_scalar() {
                                Ok(mut f) => {
                                    Arc::make_mut(&mut f).id =
                                        FunctionId::Named(binding.name.value.clone());
                                    f.into()
                                }
                                Err(fs) => fs.into(),
                            },
                            val => val,
                        }
                    } else {
                        Function::new(
                            FunctionId::Named(binding.name.value.clone()),
                            Vec::new(),
                            FunctionKind::Normal,
                            sig,
                        )
                        .into()
                    }
                } else {
                    make_fn(instrs, sig)
                }
            }
            Err(e) => {
                if let Some(sig) = binding.signature {
                    make_fn(instrs, sig.value)
                } else {
                    return Err(UiuaError::Run(
                        Span::Code(binding.name.span.clone())
                            .sp(format!("Cannot infer function signature: {e}")),
                    ));
                }
            }
        };
        val.compress();
        let mut globals = self.globals.lock();
        let idx = globals.len();
        globals.push(val);
        self.scope.names.insert(binding.name.value, idx);
        Ok(())
    }
    fn compile_words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult<Vec<Instr>> {
        self.new_functions.push(Vec::new());
        self.words(words, call)?;
        if self.print_diagnostics {
            for diagnostic in self.take_diagnostics() {
                eprintln!("{}", diagnostic.show(true));
            }
        }
        let instrs = self.new_functions.pop().unwrap();
        Ok(instrs)
    }
    fn compile_operand_words(
        &mut self,
        words: Vec<Sp<Word>>,
    ) -> UiuaResult<(Vec<Instr>, Result<Signature, String>)> {
        let mut instrs = self.compile_words(words, true)?;
        let mut sig = None;
        // Extract function instrs if possible
        if let [Instr::Push(val)] = instrs.as_slice() {
            if let Some(f) = val.as_function() {
                sig = Some(f.signature());
                instrs = f.instrs.clone();
            }
        }
        let sig = if let Some(sig) = sig {
            Ok(sig)
        } else {
            instrs_signature(&instrs)
        };
        Ok((instrs, sig))
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        for word in words.into_iter().rev() {
            self.word(word, call)?;
        }
        Ok(())
    }
    /// Push an instruction to the current function being compiled
    ///
    /// Also performs some optimizations if the instruction and the previous
    /// instruction form some known pattern
    fn push_instr(&mut self, instr: Instr) {
        use Primitive::*;
        let instrs = self.new_functions.last_mut().unwrap();
        // Optimizations
        match (instrs.as_mut_slice(), instr) {
            // Cosine
            ([.., Instr::Prim(Eta, _), Instr::Prim(Add, _)], Instr::Prim(Sin, span)) => {
                instrs.pop();
                instrs.pop();
                instrs.push(Instr::Prim(Cos, span));
            }
            // First reverse = last
            ([.., Instr::Prim(top @ Reverse, _)], Instr::Prim(First, _)) => *top = Last,
            (_, instr) => instrs.push(instr),
        }
    }
    fn word(&mut self, word: Sp<Word>, call: bool) -> UiuaResult {
        match word.value {
            Word::Number(_, n) => {
                self.push_instr(Instr::push(n));
            }
            Word::Char(c) => self.push_instr(Instr::push(c)),
            Word::String(s) => self.push_instr(Instr::push(s)),
            Word::FormatString(frags) => {
                let signature = Signature::new(frags.len() - 1, 1);
                let f = Function::new(
                    FunctionId::Anonymous(word.span.clone()),
                    Vec::new(),
                    FunctionKind::Dynamic(DynamicFunctionKind {
                        id: {
                            let mut hasher = DefaultHasher::new();
                            frags.hash(&mut hasher);
                            hasher.finish()
                        },
                        f: Arc::new(move |env| {
                            let mut formatted = String::new();
                            for (i, frag) in frags.iter().enumerate() {
                                if i > 0 {
                                    let val = env.pop(format!("format argument {i}"))?;
                                    formatted.push_str(&format!("{}", val));
                                }
                                formatted.push_str(frag);
                            }
                            env.push(formatted);
                            Ok(())
                        }),
                    }),
                    signature,
                );
                self.push_instr(Instr::push(f));
                if call {
                    let span = self.add_span(word.span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::MultilineString(lines) => {
                let signature = Signature::new(lines.iter().map(|l| l.value.len() - 1).sum(), 1);
                let f = Function::new(
                    FunctionId::Anonymous(word.span.clone()),
                    Vec::new(),
                    FunctionKind::Dynamic(DynamicFunctionKind {
                        id: {
                            let mut hasher = DefaultHasher::new();
                            lines.hash(&mut hasher);
                            hasher.finish()
                        },
                        f: Arc::new(move |env| {
                            let mut formatted = String::new();
                            let mut i = 0;
                            for (j, line) in lines.iter().enumerate() {
                                if j > 0 {
                                    formatted.push_str("\r\n");
                                }
                                for (k, frag) in line.value.iter().enumerate() {
                                    if k > 0 {
                                        let val = env.pop(format!("format argument {i}"))?;
                                        formatted.push_str(&format!("{}", val));
                                    }
                                    formatted.push_str(frag);
                                    i += 1;
                                }
                            }
                            env.push(formatted);
                            Ok(())
                        }),
                    }),
                    signature,
                );
                self.push_instr(Instr::push(f));
                if call {
                    let span = self.add_span(word.span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::Ident(ident) => self.ident(ident, word.span, call)?,
            Word::Strand(items) => {
                self.push_instr(Instr::BeginArray);
                let inner = self.compile_words(items, false)?;
                let span = self.add_span(word.span);
                let instrs = self.new_functions.last_mut().unwrap();
                if inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    // Inline constant arrays
                    instrs.pop();
                    let values = inner.into_iter().rev().map(|instr| match instr {
                        Instr::Push(v) => *v,
                        _ => unreachable!(),
                    });
                    self.push_span(span, None);
                    let val = Value::from_row_values(values, self)?;
                    self.pop_span();
                    self.push_instr(Instr::push(val));
                } else {
                    // Normal case
                    instrs.extend(inner);
                    self.push_instr(Instr::EndArray {
                        span,
                        constant: false,
                    });
                }
            }
            Word::Array(arr) => {
                if !call {
                    self.new_functions.push(Vec::new());
                }
                self.push_instr(Instr::BeginArray);
                let mut inner = Vec::new();
                for lines in arr.lines.into_iter().rev() {
                    inner.extend(self.compile_words(lines, true)?);
                }
                let span = self.add_span(word.span.clone());
                let instrs = self.new_functions.last_mut().unwrap();
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    // Inline constant arrays
                    instrs.pop();
                    let empty = inner.is_empty();
                    let values = inner.into_iter().rev().map(|instr| match instr {
                        Instr::Push(v) => *v,
                        _ => unreachable!(),
                    });
                    self.push_span(span, None);
                    let val = if arr.constant {
                        if empty {
                            Array::<Arc<Function>>::default().into()
                        } else {
                            Value::from_row_values(values.map(Function::constant), self)?
                        }
                    } else {
                        Value::from_row_values(values, self)?
                    };
                    self.pop_span();
                    self.push_instr(Instr::push(val));
                } else {
                    instrs.extend(inner);
                    self.push_instr(Instr::EndArray {
                        span,
                        constant: arr.constant,
                    });
                    if !call {
                        let instrs = self.new_functions.pop().unwrap();
                        let sig =
                            instrs_signature(&instrs).unwrap_or_else(|_| Signature::new(0, 0));
                        let func = Function::new(
                            FunctionId::Anonymous(word.span),
                            instrs,
                            FunctionKind::Normal,
                            sig,
                        );
                        self.push_instr(Instr::push(func));
                    }
                }
            }
            Word::Func(func) => self.func(func, word.span)?,
            Word::Primitive(p) => self.primitive(p, word.span, call)?,
            Word::Modified(m) => self.modified(*m, call)?,
            Word::Spaces | Word::Comment(_) => {}
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool) -> UiuaResult {
        if let Some(idx) = self.scope.names.get(&ident).or_else(|| {
            self.higher_scopes
                .last()
                .filter(|_| self.scope.local)?
                .names
                .get(&ident)
        }) {
            // Name exists in scope
            let value = self.globals.lock()[*idx].clone();
            let should_call = matches!(&value, Value::Func(f) if f.shape.is_empty());
            self.push_instr(Instr::push(value));
            if should_call && call {
                let span = self.add_span(span);
                self.push_instr(Instr::Call(span));
            }
        } else if let Some(prim) = Primitive::all().find(|p| {
            p.names().is_some_and(|n| {
                (n.ascii.is_some() || n.unicode.is_some_and(|c| c as u32 <= 127))
                    && ident.len() >= 3
                    && n.text.starts_with(&*ident)
            })
        }) {
            return Err(span
                .sp(format!(
                    "Unknown identifier `{ident}`. Did you mean `{prim}`? \
                    Functions with ASCII glyphs do not format."
                ))
                .into());
        } else {
            return Err(span.sp(format!("Unknown identifier `{ident}`")).into());
        }
        Ok(())
    }
    fn func(&mut self, func: Func, span: CodeSpan) -> UiuaResult {
        let mut instrs = Vec::new();
        for line in func.lines {
            instrs.extend(self.compile_words(line, true)?);
        }

        // Validate signature
        let sig = match instrs_signature(&instrs) {
            Ok(mut sig) => {
                if let Some(declared_sig) = &func.signature {
                    if declared_sig.value.is_superset_of(sig) {
                        sig = declared_sig.value;
                    } else {
                        return Err(UiuaError::Run(Span::Code(declared_sig.span.clone()).sp(
                            format!(
                                "Function signature mismatch: declared {} but inferred {}",
                                declared_sig.value, sig
                            ),
                        )));
                    }
                }
                sig
            }
            Err(e) => {
                if let Some(declared_sig) = &func.signature {
                    declared_sig.value
                } else {
                    return Err(UiuaError::Run(
                        Span::Code(span.clone())
                            .sp(format!("Cannot infer function signature: {e}")),
                    ));
                }
            }
        };

        // De-nest function calls
        if let [Instr::Push(val), Instr::Call(_)] = instrs.as_slice() {
            if let Some(f) = val.as_function() {
                self.push_instr(Instr::push(f.clone()));
                return Ok(());
            }
        }

        let function = Function::new(func.id, instrs, FunctionKind::Normal, sig);
        self.push_instr(Instr::push(function));
        Ok(())
    }
    fn modified(&mut self, modified: Modified, call: bool) -> UiuaResult {
        // Give advice about redundant each
        match modified.modifier.value {
            Primitive::Each => {
                if let [word] = modified.operands.as_slice() {
                    if let Word::Primitive(prim) = &word.value {
                        if prim.class().is_pervasive() {
                            let span = modified.modifier.span.clone().merge(word.span.clone());
                            self.diagnostics.insert(Diagnostic::new(
                                format!(
                                    "Using each with a pervasive primitive like {prim} is \
                                    redundant. Just use {prim} by itself."
                                ),
                                span,
                                DiagnosticKind::Advice,
                            ));
                        }
                    }
                }
            }
            _ => {}
        }
        // Handle deprecation
        self.handle_primitive_deprecation(modified.modifier.value, &modified.modifier.span);

        // Inline bind
        if modified.modifier.value == Primitive::Bind && modified.operands.len() == 2 {
            let instrs = self.compile_words(modified.operands, true)?;
            return if call {
                for instr in instrs {
                    self.push_instr(instr);
                }
                Ok(())
            } else {
                match instrs_signature(&instrs) {
                    Ok(sig) => {
                        let func = Function::new(
                            FunctionId::Anonymous(modified.modifier.span),
                            instrs,
                            FunctionKind::Normal,
                            sig,
                        );
                        self.push_instr(Instr::push(func));
                        Ok(())
                    }
                    Err(e) => Err(UiuaError::Run(
                        Span::Code(modified.modifier.span.clone())
                            .sp(format!("Cannot infer function signature in bind: {e}")),
                    )),
                }
            };
        }

        // Inline dip and gap
        if matches!(modified.modifier.value, Primitive::Dip | Primitive::Gap)
            && modified.operands.len() == 1
        {
            let (mut instrs, _) = self.compile_operand_words(modified.operands)?;
            let span = self.add_span(modified.modifier.span.clone());
            if modified.modifier.value == Primitive::Dip {
                instrs.insert(0, Instr::PushTemp { count: 1, span });
                instrs.push(Instr::PopTemp { count: 1, span });
            } else {
                instrs.insert(0, Instr::Prim(Primitive::Pop, span));
            }
            return if call {
                for instr in instrs {
                    self.push_instr(instr);
                }
                Ok(())
            } else {
                match instrs_signature(&instrs) {
                    Ok(sig) => {
                        let func = Function::new(
                            FunctionId::Anonymous(modified.modifier.span),
                            instrs,
                            FunctionKind::Normal,
                            sig,
                        );
                        self.push_instr(Instr::push(func));
                        Ok(())
                    }
                    Err(e) => Err(UiuaError::Run(
                        Span::Code(modified.modifier.span.clone())
                            .sp(format!("Cannot infer function: {e}")),
                    )),
                }
            };
        }

        if call {
            self.words(modified.operands, false)?;
            let span = self.add_span(modified.modifier.span);
            self.push_instr(Instr::Prim(modified.modifier.value, span));
        } else {
            self.new_functions.push(Vec::new());
            self.words(modified.operands, false)?;
            self.primitive(
                modified.modifier.value,
                modified.modifier.span.clone(),
                true,
            )?;
            let instrs = self.new_functions.pop().unwrap();
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    let func = Function::new(
                        FunctionId::Anonymous(modified.modifier.span),
                        instrs,
                        FunctionKind::Normal,
                        sig,
                    );
                    self.push_instr(Instr::push(func));
                }
                Err(e) => {
                    return Err(UiuaError::Run(
                        Span::Code(modified.modifier.span.clone())
                            .sp(format!("Cannot infer function signature: {e}")),
                    ));
                }
            }
        }
        Ok(())
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
                    "Warning: {}{} is deprecated and will be removed in a future version{}",
                    prim.name().unwrap_or_default(),
                    prim,
                    suggestion
                ),
                span.clone(),
                DiagnosticKind::Warning,
            ));
        }
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) -> UiuaResult {
        self.handle_primitive_deprecation(prim, &span);
        let span = self.add_span(span);
        if call || prim.as_constant().is_some() {
            self.push_instr(Instr::Prim(prim, span));
        } else {
            let instrs = [Instr::Prim(prim, span)];
            let func =
                Function::new_inferred(FunctionId::Primitive(prim), instrs, FunctionKind::Normal);
            match func {
                Ok(func) => self.push_instr(Instr::push(func)),
                Err(e) => {
                    return Err(self.error(format!(
                        "{prim} cannot be used here because it does \
                        not have a well-defined signature: {e}"
                    )))
                }
            }
        }
        Ok(())
    }
}
