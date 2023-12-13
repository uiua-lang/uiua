use std::{
    collections::{HashMap, HashSet},
    fmt,
    mem::take,
};

use ecow::{eco_vec, EcoString, EcoVec};

use crate::{
    algorithm::invert::{invert_instrs, under_instrs},
    ast::*,
    check::instrs_signature,
    constants,
    function::*,
    lex::{CodeSpan, Sp, Span},
    optimize::{optimize_instrs, optimize_instrs_mut},
    parse::{count_placeholders, ident_modifier_args, split_words, unsplit_words},
    Array, Boxed, Diagnostic, DiagnosticKind, Global, Ident, ImplPrimitive, Primitive, RunMode,
    SysOp, UiuaResult, Value,
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
            Item::TestScope(items) => {
                self.in_scope(|env| env.items(items.value, true))?;
            }
            Item::Words(mut lines) => {
                let can_run = match self.ct.mode {
                    RunMode::Normal => !in_test,
                    RunMode::Test => in_test,
                    RunMode::All => true,
                };
                lines = unsplit_words(lines.into_iter().flat_map(split_words));
                for line in lines {
                    if line.is_empty() {
                        continue;
                    }
                    if can_run || words_have_import(&line) {
                        let span = line
                            .first()
                            .unwrap()
                            .span
                            .clone()
                            .merge(line.last().unwrap().span.clone());
                        if count_placeholders(&line) > 0 {
                            return Err(self.error_with_span(
                                span,
                                "Cannot use placeholder outside of function",
                            ));
                        }
                        let instrs = self.compile_words(line, true)?;
                        match instrs_signature(&instrs) {
                            Ok(sig) => {
                                if let Ok(height) = &mut self.ct.scope.stack_height {
                                    *height = (*height + sig.outputs).saturating_sub(sig.args);
                                }
                            }
                            Err(e) => self.ct.scope.stack_height = Err(span.sp(e)),
                        }
                        let start = self.asm.instrs.len();
                        self.asm.instrs.extend(optimize_instrs(instrs, true));
                        let end = self.asm.instrs.len();
                        self.asm.top_slices.push(FuncSlice {
                            address: start,
                            len: end - start,
                        });
                    }
                }
            }
            Item::Binding(binding) => {
                let can_run = match self.ct.mode {
                    RunMode::Normal => !in_test,
                    RunMode::All | RunMode::Test => true,
                };
                if can_run || words_have_import(&binding.words) {
                    self.binding(binding)?;
                }
            }
            Item::ExtraNewlines(_) => {}
            Item::OutputComment { i, n, .. } => {
                let address = self.asm.instrs.len();
                self.asm.instrs.push(Instr::SetOutputComment { i, n });
                self.asm.top_slices.push(FuncSlice { address, len: 1 });
            }
        }
        Ok(())
    }
    #[must_use]
    pub(crate) fn add_function<I>(&mut self, id: FunctionId, sig: Signature, instrs: I) -> Function
    where
        I: IntoIterator<Item = Instr> + fmt::Debug,
        I::IntoIter: ExactSizeIterator,
    {
        let instrs = optimize_instrs(instrs, true);
        let len = instrs.len();
        if len > 1 {
            (self.asm.instrs).push(Instr::Comment(format!("({id}").into()));
        }
        let address = self.asm.instrs.len();
        self.asm.instrs.extend(instrs);
        if len > 1 {
            (self.asm.instrs).push(Instr::Comment(format!("{id})").into()));
        }
        Function::new(id, sig, FuncSlice { address, len })
    }
    fn binding(&mut self, binding: Binding) -> UiuaResult {
        let name = binding.name.value;
        let span = &binding.name.span;
        let placeholder_count = count_placeholders(&binding.words);

        let make_fn = |mut instrs: EcoVec<Instr>, sig: Signature, env: &mut Self| {
            // Diagnostic for function that doesn't consume its arguments
            if let Some((Instr::Prim(Primitive::Dup, span), rest)) = instrs.split_first() {
                if let Ok(rest_sig) = instrs_signature(rest) {
                    if rest_sig.args == sig.args && rest_sig.outputs + 1 == sig.outputs {
                        env.diagnostic_with_span(
                            "Functions should consume their arguments. \
                            Try removing this duplicate.",
                            DiagnosticKind::Style,
                            env.get_span(*span),
                        );
                        env.flush_diagnostics();
                    }
                }
            }

            // Handle placeholders
            if placeholder_count > 0 {
                instrs.insert(0, Instr::PushTempFunctions(placeholder_count));
                instrs.push(Instr::PopTempFunctions(placeholder_count));
            }
            let f = env.add_function(FunctionId::Named(name.clone()), sig, instrs);
            if placeholder_count > 0 {
                env.increment_placeholders(&f, &mut 0, &mut HashMap::new());
            }
            f
        };
        // Compile the body
        let mut instrs = self.compile_words(binding.words, true)?;
        let span = self.add_span(span.clone());
        let global_index = self.ct.next_global;
        self.ct.next_global += 1;

        // Check if binding is an import
        let mut is_import = false;
        let mut sig = None;
        if let [init @ .., Instr::Prim(Primitive::Sys(SysOp::Import), _)] = instrs.as_slice() {
            is_import = true;
            match init {
                [Instr::Push(path)] => {
                    if let Some(sig) = &binding.signature {
                        return Err(self.error_with_span(
                            sig.span.clone(),
                            "Cannot declare a signature for a module import",
                        ));
                    }
                    let path = path.as_string(self, "Import path must be a string")?;
                    let module = self.import_compile(path.as_ref())?;
                    self.add_global_at(global_index, Global::Module { module });
                    self.ct.scope.names.insert(name.clone(), global_index);
                }
                [Instr::Push(item), Instr::Push(path)] => self.with_span(span, |env| {
                    let path = path.as_string(env, "Import path must be a string")?;
                    let module = env.import_compile(path.as_ref())?;
                    let item = item.as_string(env, "Import item must be a string")?;
                    let index = *env.ct.imports[&module].get(item.as_str()).ok_or_else(|| {
                        env.error(format!("Item `{item}` not found in module `{path}`"))
                    })?;
                    env.ct.scope.names.insert(name.clone(), index);
                    sig = Some(match &env.asm.globals[index] {
                        Global::Func(f) => f.signature(),
                        Global::Sig(s) => *s,
                        Global::Const(_) => Signature::new(0, 1),
                        Global::Module { .. } => {
                            return Err(env.error("Cannot define a signature for a module rebind"))
                        }
                    });
                    Ok(())
                })?,
                _ => {
                    return Err(self.error_with_span(
                        self.get_span(span),
                        "&i must be followed by one or two strings",
                    ))
                }
            }
        }

        // Resolve signature
        match instrs_signature(&instrs) {
            Ok(s) => {
                let mut sig = sig.unwrap_or(s);
                // Runtime-dependent binding
                if instrs.is_empty() {
                    // Binding from the stack set above
                    match &mut self.ct.scope.stack_height {
                        Ok(height) => {
                            if *height > 0 {
                                sig = Signature::new(0, 1);
                            }
                            *height = height.saturating_sub(1);
                        }
                        Err(sp) => {
                            let sp = sp.clone();
                            return Err(self.error_with_span(
                                sp.span,
                                format!(
                                    "This line's signature is undefined: {}. \
                                    This prevents the later binding of {}.",
                                    sp.value, name
                                ),
                            ));
                        }
                    }
                }
                // Validate signature
                if let Some(declared_sig) = &binding.signature {
                    let sig_to_check = if let [Instr::PushFunc(f)] = instrs.as_slice() {
                        // If this is a function wrapped in parens, check the signature of the
                        // function rather than the signature of the binding's words
                        f.signature()
                    } else {
                        sig
                    };
                    if declared_sig.value == sig_to_check {
                        sig = declared_sig.value;
                    } else {
                        return Err(self.error_with_span(
                            declared_sig.span.clone(),
                            format!(
                                "Function signature mismatch:  declared {} but inferred {}",
                                declared_sig.value, sig_to_check
                            ),
                        ));
                    }
                }
                #[rustfmt::skip]
                let is_setinv = matches!(
                    instrs.as_slice(),
                    [Instr::PushFunc(_), Instr::PushFunc(_), Instr::Prim(Primitive::SetInverse, _)]
                );
                #[rustfmt::skip]
                let is_setund = matches!(
                    instrs.as_slice(),
                    [Instr::PushFunc(_), Instr::PushFunc(_), Instr::PushFunc(_), Instr::Prim(Primitive::SetUnder, _)]
                );
                if is_import {
                } else if let [Instr::PushFunc(f)] = instrs.as_slice() {
                    // Binding is a single inline function
                    let func = make_fn(f.instrs(self).into(), f.signature(), self);
                    self.compile_bind_function(&name, global_index, func, span)?;
                } else if sig.args == 0
                    && sig.outputs <= 1
                    && (sig.outputs > 0 || instrs.is_empty())
                    && placeholder_count == 0
                    && !is_setinv
                    && !is_setund
                {
                    self.compile_bind_sig(&name, global_index, sig, span)?;
                    // Add binding instrs to top slices
                    instrs.push(Instr::BindGlobal {
                        name,
                        span,
                        index: global_index,
                    });
                    let start = self.asm.instrs.len();
                    self.asm.instrs.extend(optimize_instrs(instrs, true));
                    let end = self.asm.instrs.len();
                    self.asm.top_slices.push(FuncSlice {
                        address: start,
                        len: end - start,
                    });
                } else {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig, self);
                    self.compile_bind_function(&name, global_index, func, span)?;
                }
            }
            Err(e) => {
                if is_import {
                } else if let Some(sig) = binding.signature {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig.value, self);
                    self.compile_bind_function(&name, global_index, func, span)?;
                } else {
                    return Err(self.error_with_span(
                        binding.name.span.clone(),
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the `←`."
                            } else {
                                ""
                            }
                        ),
                    ));
                }
            }
        }
        Ok(())
    }
    pub(crate) fn bind_value(
        &mut self,
        name: &Ident,
        index: usize,
        mut value: Value,
        span: usize,
    ) -> UiuaResult {
        self.validate_binding_name(name, &[], self.get_span(span))?;
        value.compress();
        self.add_global_at(index, Global::Const(value));
        Ok(())
    }
    pub(crate) fn compile_bind_sig(
        &mut self,
        name: &Ident,
        index: usize,
        sig: Signature,
        _span: usize,
    ) -> UiuaResult {
        self.add_global_at(index, Global::Sig(sig));
        self.ct.scope.names.insert(name.clone(), index);
        Ok(())
    }
    pub(crate) fn compile_bind_function(
        &mut self,
        name: &Ident,
        index: usize,
        function: Function,
        span: usize,
    ) -> UiuaResult {
        self.validate_binding_name(name, function.instrs(self), self.get_span(span))?;
        self.ct.scope.names.insert(name.clone(), index);
        self.add_global_at(index, Global::Func(function));
        Ok(())
    }
    fn add_global_at(&mut self, index: usize, global: Global) {
        if index < self.asm.globals.len() {
            self.asm.globals.make_mut()[index] = global;
        } else {
            while self.asm.globals.len() < index {
                self.asm.globals.push(Global::Const(Value::default()));
            }
            self.asm.globals.push(global);
        }
    }
    fn validate_binding_name(&self, name: &Ident, instrs: &[Instr], span: Span) -> UiuaResult {
        let temp_function_count = self.count_temp_functions(instrs, &mut HashSet::new());
        let name_marg_count = ident_modifier_args(name) as usize;
        if temp_function_count != name_marg_count {
            let trimmed = name.trim_end_matches('!');
            let this = format!("{}{}", trimmed, "!".repeat(temp_function_count));
            return Err(self.error_with_span(
                span.clone(),
                format!(
                    "The name {name} implies {name_marg_count} modifier arguments, \
                    but the binding body references {temp_function_count}. Try `{this}`."
                ),
            ));
        }
        Ok(())
    }
    fn compile_words(&mut self, mut words: Vec<Sp<Word>>, call: bool) -> UiuaResult<EcoVec<Instr>> {
        words = unsplit_words(split_words(words))
            .into_iter()
            .flatten()
            .collect();

        self.ct.new_functions.push(EcoVec::new());
        self.words(words, call)?;
        self.flush_diagnostics();
        Ok(self.ct.new_functions.pop().unwrap())
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
    ) -> UiuaResult<(EcoVec<Instr>, Signature)> {
        let span = words
            .first()
            .zip(words.last())
            .map(|(first, last)| first.span.clone().merge(last.span.clone()));
        let mut instrs = self.compile_words(words, true)?;
        let mut sig = None;
        // Extract function instrs if possible
        if let [Instr::PushFunc(f)] = instrs.as_slice() {
            sig = Some(f.signature());
            instrs = f.instrs(self).into();
        }
        let sig = if let Some(sig) = sig {
            sig
        } else {
            instrs_signature(&instrs).map_err(|e| {
                self.error_with_span(
                    span.unwrap(),
                    format!("Cannot infer function signature: {e}"),
                )
            })?
        };
        Ok((instrs, sig))
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        let mut words = words
            .into_iter()
            .rev()
            .filter(|word| word.value.is_code() || matches!(&word.value, Word::Comment(_)))
            .peekable();
        while let Some(word) = words.next() {
            // Handle imports
            if let Some(next) = words.peek() {
                if let Word::Ident(name) = &next.value {
                    if let Some(index) = self.ct.scope.names.get(name) {
                        if let Global::Module { module } = &self.asm.globals[*index] {
                            if let Word::String(item_name) = &word.value {
                                let index = self.ct.imports[module]
                                    .get(item_name.as_str())
                                    .copied()
                                    .ok_or_else(|| {
                                        self.error_with_span(
                                            next.span.clone(),
                                            format!(
                                                "Item `{item_name}` not found in module `{}`",
                                                module.display()
                                            ),
                                        )
                                    })?;
                                self.global_index(index, next.span.clone(), call)?;
                                words.next();
                                continue;
                            } else {
                                return Err(self.error_with_span(
                                    next.span.clone(),
                                    format!(
                                        "Expected a string after `{name}` \
                                        to specify an item to import",
                                    ),
                                ));
                            }
                        }
                    }
                }
            }
            self.word(word, call)?;
        }
        Ok(())
    }
    /// Push an instruction to the current function being compiled
    ///
    /// Also performs some optimizations if the instruction and the previous
    /// instruction form some known pattern
    fn push_instr(&mut self, instr: Instr) {
        let instrs = self.ct.new_functions.last_mut().unwrap();
        optimize_instrs_mut(instrs, instr, false);
    }
    fn push_all_instrs(&mut self, instrs: impl IntoIterator<Item = Instr>) {
        for instr in instrs {
            self.push_instr(instr);
        }
    }
    fn word(&mut self, word: Sp<Word>, call: bool) -> UiuaResult {
        match word.value {
            Word::Number(_, n) => {
                if call {
                    self.push_instr(Instr::push(n));
                } else {
                    let f = self.add_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![Instr::push(n)],
                    );
                    self.push_instr(Instr::PushFunc(f))
                }
            }
            Word::Char(c) => {
                let val: Value = if c.chars().count() == 1 {
                    c.chars().next().unwrap().into()
                } else {
                    c.into()
                };
                if call {
                    self.push_instr(Instr::push(val));
                } else {
                    let f = self.add_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![Instr::push(val)],
                    );
                    self.push_instr(Instr::PushFunc(f))
                }
            }
            Word::String(s) => {
                if call {
                    self.push_instr(Instr::push(s));
                } else {
                    let f = self.add_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![Instr::push(s)],
                    );
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Word::FormatString(frags) => {
                let signature = Signature::new(frags.len() - 1, 1);
                let parts = frags.into_iter().map(Into::into).collect();
                let span = self.add_span(word.span.clone());
                let instr = Instr::Format(parts, span);
                if call {
                    self.push_instr(instr)
                } else {
                    let f =
                        self.add_function(FunctionId::Anonymous(word.span), signature, vec![instr]);
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Word::MultilineString(lines) => {
                let signature = Signature::new(
                    lines.iter().map(|l| l.value.len().saturating_sub(1)).sum(),
                    1,
                );
                let span = self.add_span(word.span.clone());
                let mut curr_part = EcoString::new();
                let mut parts = EcoVec::new();
                for (l, line) in lines.into_iter().enumerate() {
                    if l > 0 {
                        curr_part.push('\n');
                    }
                    for (f, frag) in line.value.into_iter().enumerate() {
                        if f > 0 {
                            parts.push(take(&mut curr_part));
                        }
                        curr_part.push_str(&frag);
                    }
                }
                parts.push(curr_part);
                let instr = Instr::Format(parts, span);
                if call {
                    self.push_instr(instr)
                } else {
                    let f =
                        self.add_function(FunctionId::Anonymous(word.span), signature, vec![instr]);
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Word::Ident(ident) => self.ident(ident, word.span, call)?,
            Word::Strand(items) => {
                if !call {
                    self.ct.new_functions.push(EcoVec::new());
                }
                self.push_instr(Instr::BeginArray);
                let inner = self.compile_words(items, true)?;
                if !inner.is_empty()
                    && inner.iter().all(
                        |instr| matches!(instr, Instr::Push(Value::Char(arr)) if arr.rank() == 0),
                    )
                {
                    self.diagnostic_with_span(
                        "Stranded characters should instead be written as a string",
                        DiagnosticKind::Advice,
                        word.span.clone(),
                    );
                }
                let mut instrs = inner.iter();
                while let Some(instr) = instrs.next() {
                    match instr {
                        Instr::Push(_) => {}
                        Instr::Prim(p, _)
                            if p.args() == Some(0)
                                && p.outputs() == Some(1)
                                && p.modifier_args().is_none() => {}
                        Instr::BeginArray => {
                            while (instrs.next())
                                .is_some_and(|instr| !matches!(instr, Instr::EndArray { .. }))
                            {
                            }
                        }
                        Instr::CallGlobal { sig, .. } if *sig == (0, 1) => {}
                        _ => {
                            return Err(
                                self.error_with_span(word.span, "Strand cannot contain functions")
                            )
                        }
                    }
                }
                let span = self.add_span(word.span.clone());
                let instrs = self.ct.new_functions.last_mut().unwrap();
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    // Inline constant arrays
                    instrs.pop();
                    let values = inner.into_iter().rev().map(|instr| match instr {
                        Instr::Push(v) => v,
                        _ => unreachable!(),
                    });
                    let val = self.with_span(span, |env| Value::from_row_values(values, env))?;
                    self.push_instr(Instr::push(val));
                } else {
                    // Normal case
                    instrs.extend(inner);
                    self.push_instr(Instr::EndArray { span, boxed: false });
                    if !call {
                        let instrs = self.ct.new_functions.pop().unwrap();
                        let sig = instrs_signature(&instrs).unwrap_or(Signature::new(0, 0));
                        let func = self.add_function(FunctionId::Anonymous(word.span), sig, instrs);
                        self.push_instr(Instr::PushFunc(func));
                    }
                }
            }
            Word::Array(arr) => {
                if !call {
                    self.ct.new_functions.push(EcoVec::new());
                }
                self.push_instr(Instr::BeginArray);
                let mut inner = Vec::new();
                let line_count = arr.lines.len();
                for lines in arr.lines.into_iter().rev() {
                    inner.extend(self.compile_words(lines, true)?);
                }
                if line_count <= 1
                    && !inner.is_empty()
                    && inner.iter().all(
                        |instr| matches!(instr, Instr::Push(Value::Char(arr)) if arr.rank() == 0),
                    )
                {
                    self.diagnostic_with_span(
                        "An array of characters should instead be written as a string",
                        DiagnosticKind::Advice,
                        word.span.clone(),
                    );
                }
                let span = self.add_span(word.span.clone());
                let instrs = self.ct.new_functions.last_mut().unwrap();
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    // Inline constant arrays
                    instrs.pop();
                    let empty = inner.is_empty();
                    let values = inner.into_iter().rev().map(|instr| match instr {
                        Instr::Push(v) => v,
                        _ => unreachable!(),
                    });
                    let val = self.with_span(span, |env| {
                        if arr.boxes {
                            if empty {
                                Ok(Array::<Boxed>::default().into())
                            } else {
                                Value::from_row_values(
                                    values.map(|v| Value::Box(Boxed(v).into())),
                                    env,
                                )
                            }
                        } else {
                            Value::from_row_values(values, env)
                        }
                    })?;
                    self.push_instr(Instr::push(val));
                } else {
                    instrs.extend(inner);
                    self.push_instr(Instr::EndArray {
                        span,
                        boxed: arr.boxes,
                    });
                    if !call {
                        let instrs = self.ct.new_functions.pop().unwrap();
                        let sig = instrs_signature(&instrs).unwrap_or(Signature::new(0, 0));
                        let func = self.add_function(FunctionId::Anonymous(word.span), sig, instrs);
                        self.push_instr(Instr::PushFunc(func));
                    }
                }
            }
            Word::Func(func) => self.func(func, word.span)?,
            Word::Switch(sw) => self.switch(sw, word.span, call)?,
            Word::Primitive(p) => self.primitive(p, word.span, call)?,
            Word::Modified(m) => self.modified(*m, call)?,
            Word::Placeholder(sig) => {
                let span = self.add_span(word.span);
                self.push_instr(Instr::GetTempFunction {
                    offset: 0,
                    sig,
                    span,
                });
                if call {
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::Comment(comment) => {
                if comment.trim() == "Experimental!" {
                    self.ct.scope.experimental = true;
                }
            }
            Word::Spaces | Word::BreakLine | Word::UnbreakLine => {}
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool) -> UiuaResult {
        if let Some(index) = (self.ct.scope.names.get(&ident))
            .or_else(|| self.ct.higher_scopes.last()?.names.get(&ident))
            .copied()
        {
            // Name exists in scope
            self.global_index(index, span, call)?;
        } else if let Some(constant) = constants().iter().find(|c| c.name == ident) {
            self.push_instr(Instr::push(constant.value.clone()));
        } else {
            return Err(self.error_with_span(span, format!("Unknown identifier `{ident}`")));
        }
        Ok(())
    }
    fn global_index(&mut self, index: usize, span: CodeSpan, call: bool) -> UiuaResult {
        let global = self.asm.globals[index].clone();
        match global {
            Global::Const(val) if call => self.push_instr(Instr::push(val)),
            Global::Const(val) => {
                let f = self.add_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    vec![Instr::push(val)],
                );
                self.push_instr(Instr::PushFunc(f));
            }
            Global::Func(f)
                if self.count_temp_functions(f.instrs(self), &mut HashSet::new()) == 0
                    && !self.has_tracing(f.instrs(self)) =>
            {
                if call {
                    // Inline instructions
                    self.push_instr(Instr::PushSig(f.signature()));
                    let instrs = f.instrs(self).to_vec();
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Global::Func(f) => {
                self.push_instr(Instr::PushFunc(f));
                if call {
                    let span = self.add_span(span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Global::Sig(sig) => self.push_instr(Instr::CallGlobal { index, call, sig }),
            Global::Module { .. } => {
                return Err(self.error_with_span(span, "Cannot import module item here."))
            }
        }
        Ok(())
    }
    fn func(&mut self, func: Func, span: CodeSpan) -> UiuaResult {
        let function = self.compile_func(func, span)?;
        self.push_instr(Instr::PushFunc(function));
        Ok(())
    }
    fn compile_func(&mut self, func: Func, span: CodeSpan) -> UiuaResult<Function> {
        let mut instrs = Vec::new();
        for line in func.lines {
            instrs.extend(self.compile_words(line, true)?);
        }

        // Validate signature
        let sig = match instrs_signature(&instrs) {
            Ok(mut sig) => {
                if let Some(declared_sig) = &func.signature {
                    if declared_sig.value == sig {
                        sig = declared_sig.value;
                    } else {
                        return Err(self.error_with_span(
                            declared_sig.span.clone(),
                            format!(
                                "Function signature mismatch: declared {} but inferred {}",
                                declared_sig.value, sig
                            ),
                        ));
                    }
                }
                sig
            }
            Err(e) => {
                if let Some(declared_sig) = &func.signature {
                    declared_sig.value
                } else {
                    return Err(self.error_with_span(
                        span,
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the opening `(`."
                            } else {
                                ""
                            }
                        ),
                    ));
                }
            }
        };

        if let [Instr::PushFunc(f), Instr::Call(_)] = instrs.as_slice() {
            return Ok(Function::clone(f));
        }

        Ok(self.add_function(func.id, sig, instrs))
    }
    fn switch(&mut self, sw: Switch, span: CodeSpan, call: bool) -> UiuaResult {
        let count = sw.branches.len();
        if !call {
            self.ct.new_functions.push(EcoVec::new());
        }
        let mut branches = sw.branches.into_iter();
        let first_branch = branches.next().expect("switch cannot have no branches");
        let f = self.compile_func(first_branch.value, first_branch.span)?;
        let mut sig = f.signature();
        self.push_instr(Instr::PushFunc(f));
        for branch in branches {
            let f = self.compile_func(branch.value, branch.span.clone())?;
            let f_sig = f.signature();
            if f_sig.is_compatible_with(sig) {
                sig = sig.max_with(f_sig);
            } else if f_sig.outputs == sig.outputs {
                sig.args = sig.args.max(f_sig.args)
            } else {
                return Err(self.error_with_span(
                    branch.span,
                    format!(
                        "Switch branch's signature {f_sig} is \
                        incompatible with previous branches {sig}",
                    ),
                ));
            }
            self.push_instr(Instr::PushFunc(f));
        }
        let span_idx = self.add_span(span.clone());
        self.push_instr(Instr::Switch {
            count,
            sig,
            span: span_idx,
        });
        if !call {
            let instrs = self.ct.new_functions.pop().unwrap();
            let sig = match instrs_signature(&instrs) {
                Ok(sig) => sig,
                Err(e) => {
                    return Err(self.error_with_span(
                        span,
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the opening `(`."
                            } else {
                                ""
                            }
                        ),
                    ));
                }
            };
            let function = self.add_function(FunctionId::Anonymous(span), sig, instrs);
            self.push_instr(Instr::PushFunc(function));
        }
        Ok(())
    }
    fn modified(&mut self, modified: Modified, call: bool) -> UiuaResult {
        let op_count = modified.code_operands().count();
        if let Modifier::Primitive(prim) = modified.modifier.value {
            // Give advice about redundancy
            match prim {
                m @ (Primitive::Each | Primitive::Rows) => {
                    if let [Sp {
                        value: Word::Primitive(prim),
                        span,
                    }] = modified.operands.as_slice()
                    {
                        if prim.class().is_pervasive() {
                            let span = modified.modifier.span.clone().merge(span.clone());
                            self.diagnostics.insert(Diagnostic::new(
                                format!(
                                    "Using {m} with a pervasive primitive like {p} is \
                                    redundant. Just use {p} by itself.",
                                    m = m.format(),
                                    p = prim.format(),
                                ),
                                span,
                                DiagnosticKind::Advice,
                                self.inputs().clone(),
                            ));
                        }
                    } else if words_look_pervasive(&modified.operands) {
                        let span = modified.modifier.span.clone();
                        self.diagnostics.insert(Diagnostic::new(
                            format!(
                                "{m}'s function is pervasive, \
                                so {m} is redundant here.",
                                m = m.format()
                            ),
                            span,
                            DiagnosticKind::Advice,
                            self.inputs().clone(),
                        ));
                    }
                }
                _ => {}
            }

            // Handle deprecation and experimental
            self.handle_primitive_experimental(prim, &modified.modifier.span)?;
            self.handle_primitive_deprecation(prim, &modified.modifier.span);
        }

        // De-sugar switched
        if op_count == 1 {
            let operand = modified.code_operands().next().unwrap().clone();
            if let Sp {
                value: Word::Switch(sw),
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
                    modifier if modifier.args() >= 2 => {
                        if sw.branches.len() != modifier.args() as usize {
                            return Err(self.error_with_span(
                                modified.modifier.span.merge(span),
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
                    _ => {}
                }
            }
        }

        // Validate operand count
        if op_count != modified.modifier.value.args() as usize {
            return Err(self.error_with_span(
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

        // Inlining
        if self.inline_modifier(&modified, call)? {
            return Ok(());
        }

        let instrs = self.compile_words(modified.operands, false)?;

        // Reduce monadic deprectation message
        if let (Modifier::Primitive(Primitive::Reduce), [Instr::PushFunc(f)]) =
            (&modified.modifier.value, instrs.as_slice())
        {
            if f.signature().args == 1 {
                self.diagnostic_with_span(
                    format!(
                        "{} with a monadic function is deprecated. \
                        Prefer {} with stack array notation.",
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
            match modified.modifier.value {
                Modifier::Primitive(prim) => self.primitive(prim, modified.modifier.span, true)?,
                Modifier::Ident(ident) => self.ident(ident, modified.modifier.span, true)?,
            }
        } else {
            self.ct.new_functions.push(EcoVec::new());
            self.push_all_instrs(instrs);
            match modified.modifier.value {
                Modifier::Primitive(prim) => {
                    self.primitive(prim, modified.modifier.span.clone(), true)?
                }
                Modifier::Ident(ident) => {
                    self.ident(ident, modified.modifier.span.clone(), true)?
                }
            }
            let instrs = self.ct.new_functions.pop().unwrap();
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
                Err(e) => {
                    return Err(self.error_with_span(
                        modified.modifier.span.clone(),
                        format!("Cannot infer function signature: {e}"),
                    ));
                }
            }
        }
        Ok(())
    }
    fn inline_modifier(&mut self, modified: &Modified, call: bool) -> UiuaResult<bool> {
        use Primitive::*;
        let Modifier::Primitive(prim) = modified.modifier.value else {
            return Ok(false);
        };
        match prim {
            Dip | Gap => {
                // Compile operands
                let (mut instrs, sig) = self.compile_operand_words(modified.operands.clone())?;
                // Dip (|1 …) . diagnostic
                if prim == Dip && sig == (1, 1) {
                    if let Some(Instr::Prim(Dup, dup_span)) = self
                        .ct
                        .new_functions
                        .last()
                        .and_then(|instrs| instrs.last())
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
                Ok(true)
            }
            Bracket => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let (b_instrs, b_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
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
                Ok(true)
            }
            Un => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let span = f.span.clone();
                let (instrs, _) = self.compile_operand_words(vec![f])?;
                if let Some(inverted) = invert_instrs(&instrs, self) {
                    match instrs_signature(&inverted) {
                        Ok(sig) => {
                            if call {
                                self.push_all_instrs(inverted);
                            } else {
                                let id = FunctionId::Anonymous(modified.modifier.span.clone());
                                let func = self.add_function(id, sig, inverted);
                                self.push_instr(Instr::PushFunc(func));
                            }
                            Ok(true)
                        }
                        Err(e) => Err(self.error_with_span(
                            modified.modifier.span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )),
                    }
                } else {
                    Err(self.error_with_span(span, "No inverse found"))
                }
            }
            Under => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let f_span = f.span.clone();
                let (f_instrs, _) = self.compile_operand_words(vec![f])?;
                let (g_instrs, g_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                if let Some((f_before, f_after)) = under_instrs(&f_instrs, g_sig, self) {
                    let before_sig = instrs_signature(&f_before).map_err(|e| {
                        self.error_with_span(
                            f_span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )
                    })?;
                    let after_sig = instrs_signature(&f_after).map_err(|e| {
                        self.error_with_span(
                            f_span,
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
                        let sig = Signature::new(
                            (before_sig.args + g_sig.args + after_sig.args)
                                .saturating_sub(before_sig.outputs + g_sig.outputs)
                                .max(before_sig.args),
                            (before_sig.outputs + g_sig.outputs)
                                .saturating_sub(g_sig.args + after_sig.args)
                                + after_sig.outputs,
                        );
                        let func = self.add_function(
                            FunctionId::Anonymous(modified.modifier.span.clone()),
                            sig,
                            instrs,
                        );
                        self.push_instr(Instr::PushFunc(func));
                    }
                    Ok(true)
                } else {
                    Err(self.error_with_span(f_span, "No inverse found"))
                }
            }
            Both => {
                let mut operands = modified.code_operands().cloned();
                let (mut instrs, sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
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
                Ok(true)
            }
            _ => Ok(false),
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
                self.inputs().clone(),
            ));
        }
    }
    fn handle_primitive_experimental(&self, prim: Primitive, span: &CodeSpan) -> UiuaResult {
        if prim.is_experimental() && !self.ct.scope.experimental {
            return Err(self.error_with_span(
                span.clone(),
                format!(
                    "{} is experimental. To use it, add \
                    `# Experimental!` to the top of the file.",
                    prim.format()
                ),
            ));
        }
        Ok(())
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) -> UiuaResult {
        self.handle_primitive_experimental(prim, &span)?;
        self.handle_primitive_deprecation(prim, &span);
        let span_i = self.add_span(span.clone());
        if call {
            self.push_instr(Instr::Prim(prim, span_i));
        } else {
            let instrs = [Instr::Prim(prim, span_i)];
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    let func = self.add_function(FunctionId::Primitive(prim), sig, instrs);
                    self.push_instr(Instr::PushFunc(func))
                }
                Err(e) => {
                    return Err(
                        self.error_with_span(span, format!("Cannot infer function signature: {e}"))
                    )
                }
            }
        }
        Ok(())
    }

    fn increment_placeholders(
        &mut self,
        f: &Function,
        curr: &mut usize,
        map: &mut HashMap<usize, usize>,
    ) {
        let len = f.instrs(self).len();
        for i in 0..len {
            match &mut f.instrs_mut(self)[i] {
                Instr::GetTempFunction { offset, span, .. } => {
                    *offset = *map.entry(*span).or_insert_with(|| {
                        let new = *curr;
                        *curr += 1;
                        new
                    });
                }
                Instr::PushFunc(f) => {
                    let f = f.clone();
                    self.increment_placeholders(&f, curr, map);
                }
                _ => (),
            }
        }
    }
    fn count_temp_functions(&self, instrs: &[Instr], counted: &mut HashSet<usize>) -> usize {
        let mut count = 0;
        for instr in instrs {
            match instr {
                Instr::GetTempFunction { span, .. } => count += counted.insert(*span) as usize,
                Instr::PushFunc(f) if matches!(f.id, FunctionId::Anonymous(_)) => {
                    count += self.count_temp_functions(f.instrs(self), counted);
                }
                _ => {}
            }
        }
        count
    }
    fn has_tracing(&self, instrs: &[Instr]) -> bool {
        for instr in instrs {
            match instr {
                Instr::Prim(Primitive::Trace | Primitive::Dump | Primitive::Stack, _) => {
                    return true
                }
                Instr::ImplPrim(
                    ImplPrimitive::InvTrace | ImplPrimitive::InvDump | ImplPrimitive::InvStack,
                    _,
                ) => return true,
                Instr::PushFunc(f) if self.has_tracing(f.instrs(self)) => return true,
                _ => {}
            }
        }
        false
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
