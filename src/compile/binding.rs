//! Compiler code for bindings

use super::*;

impl Compiler {
    pub(super) fn binding(&mut self, binding: Binding, prelude: BindingPrelude) -> UiuaResult {
        let public = binding.public;

        // Alias re-bound imports
        if ident_modifier_args(&binding.name.value) == 0
            && binding.words.iter().filter(|w| w.value.is_code()).count() == 1
        {
            if let Some(r) = binding.words.iter().find_map(|w| match &w.value {
                Word::Ref(r) if ident_modifier_args(&r.name.value) == 0 => Some(r),
                _ => None,
            }) {
                if let Ok((path_locals, local)) = self.ref_local(r) {
                    self.validate_local(&r.name.value, local, &r.name.span);
                    (self.code_meta.global_references)
                        .insert(binding.name.span.clone(), local.index);
                    for (local, comp) in path_locals.into_iter().zip(&r.path) {
                        (self.code_meta.global_references)
                            .insert(comp.module.span.clone(), local.index);
                    }
                    (self.code_meta.global_references).insert(r.name.span.clone(), local.index);
                    let local = LocalName { public, ..local };
                    self.scope.names.insert(binding.name.value, local);
                    return Ok(());
                }
            }
        }

        let name = binding.name.value;
        let span = &binding.name.span;

        let spandex = self.add_span(span.clone());
        let local = LocalName {
            index: self.next_global,
            public,
        };
        self.next_global += 1;

        let comment = prelude.comment.or_else(|| {
            binding.words.iter().last().and_then(|w| match &w.value {
                Word::Comment(c) => Some(c.as_str().into()),
                _ => None,
            })
        });
        let flags = prelude.flags;

        // Handle macro
        let ident_margs = ident_modifier_args(&name);
        let placeholder_count = count_placeholders(&binding.words);
        if binding.code_macro {
            if placeholder_count > 0 {
                return Err(
                    self.fatal_error(span.clone(), "Code macros may not contain placeholders")
                );
            }
            // Code macro
            if ident_margs == 0 {
                self.add_error(
                    span.clone(),
                    format!(
                        "Code macros must take at least 1 operand, \
                        but `{name}`'s name suggests it takes 0",
                    ),
                );
            }
            let mut new_func = self.compile_words(binding.words, true)?;
            new_func.flags |= flags;
            let sig = match instrs_signature(&new_func.instrs) {
                Ok(s) => {
                    if let Some(declared) = binding.signature {
                        if s != declared.value {
                            self.add_error(
                                span.clone(),
                                format!(
                                    "Code macro signature mismatch: \
                                    declared {} but inferred {s}",
                                    declared.value
                                ),
                            );
                        }
                    }
                    s
                }
                Err(e) => {
                    if let Some(sig) = binding.signature {
                        sig.value
                    } else {
                        self.add_error(
                            span.clone(),
                            format!("Cannot infer code macro signature: {e}"),
                        );
                        Signature::new(1, 1)
                    }
                }
            };
            const ALLOWED_SIGS: &[Signature] = &[
                Signature::new(1, 1),
                Signature::new(2, 1),
                Signature::new(0, 0),
            ];
            if !ALLOWED_SIGS.contains(&sig) {
                self.add_error(
                    span.clone(),
                    format!(
                        "Code macros must have a signature of {} or {}, \
                        but a signature of {} was inferred",
                        Signature::new(1, 1),
                        Signature::new(2, 1),
                        sig
                    ),
                );
            }
            let function = self.make_function(FunctionId::Named(name.clone()), sig, new_func);
            self.scope.names.insert(name.clone(), local);
            self.asm.add_binding_at(
                local,
                BindingKind::CodeMacro(function.slice),
                Some(span.clone()),
                comment.map(|text| DocComment::from(text.as_str())),
            );
            let mac = CodeMacro {
                function,
                names: self.scope.names.clone(),
            };
            self.code_macros.insert(local.index, mac);
            return Ok(());
        }
        // Index macro
        match (ident_margs > 0, placeholder_count > 0) {
            (true, true) | (false, false) => {}
            (true, false) => {
                self.add_error(
                    span.clone(),
                    format!(
                        "`{name}`'s name suggests it is a macro, \
                        but it has no placeholders"
                    ),
                );
            }
            (false, true) => {
                self.add_error(
                    span.clone(),
                    format!(
                        "`{name}` has placeholders, but its name \
                        does not suggest it is a macro"
                    ),
                );
                return Ok(());
            }
        }
        if placeholder_count > 0 || ident_margs > 0 {
            self.scope.names.insert(name.clone(), local);
            self.asm.add_binding_at(
                local,
                BindingKind::IndexMacro(ident_margs),
                Some(span.clone()),
                comment.map(|text| DocComment::from(text.as_str())),
            );
            let mut words = binding.words.clone();
            let mut recursive = false;
            recurse_words_mut(&mut words, &mut |word| {
                let mut path_locals = None;
                let mut name_local = None;
                match &word.value {
                    Word::Ref(r) => match self.ref_local(r) {
                        Ok((pl, l)) => {
                            path_locals = Some((&r.path, pl));
                            name_local = Some((&r.name, l));
                        }
                        Err(e) => self.errors.push(e),
                    },
                    Word::IncompleteRef { path, in_macro_arg } => {
                        match self.ref_path(path, *in_macro_arg) {
                            Ok(Some((_, pl))) => path_locals = Some((path, pl)),
                            Ok(None) => {}
                            Err(e) => self.errors.push(e),
                        }
                    }
                    Word::Modified(m) => {
                        if let Modifier::Ref(r) = &m.modifier.value {
                            match self.ref_local(r) {
                                Ok((pl, l)) => {
                                    path_locals = Some((&r.path, pl));
                                    name_local = Some((&r.name, l));
                                }
                                Err(e) => self.errors.push(e),
                            }
                        }
                    }
                    _ => {}
                }
                if let Some((nm, local)) = name_local {
                    if nm.value == name
                        && path_locals.as_ref().map_or(true, |(pl, _)| pl.is_empty())
                    {
                        recursive = true;
                    }
                    self.validate_local(&nm.value, local, &nm.span);
                    (self.code_meta.global_references).insert(nm.span.clone(), local.index);
                }
                if let Some((path, locals)) = path_locals {
                    for (local, comp) in locals.into_iter().zip(path) {
                        (self.code_meta.global_references)
                            .insert(comp.module.span.clone(), local.index);
                    }
                }
            });
            if recursive {
                self.experimental_error(span, || {
                    "Recursive positional macros are experimental. \
                    Add `# Experimental!` to the top of the file to use them."
                });
                if binding.signature.is_none() {
                    self.add_error(
                        span.clone(),
                        "Recursive positional macro must have a \
                        signature declared after the ←",
                    );
                }
            }
            let mac = IndexMacro {
                words,
                names: self.scope.names.clone(),
                hygenic: true,
                sig: binding.signature.map(|s| s.value),
                recursive,
                flags,
            };
            self.index_macros.insert(local.index, mac);
            return Ok(());
        }

        // A non-macro binding
        let mut make_fn: Box<dyn FnOnce(_, _, &mut Compiler) -> _> = {
            let name = name.clone();
            Box::new(
                move |mut new_func: NewFunction, sig: Signature, comp: &mut Compiler| {
                    // Diagnostic for function that doesn't consume its arguments
                    if let [Instr::Prim(Primitive::Dup, span), rest @ ..] =
                        new_func.instrs.as_slice()
                    {
                        if let Span::Code(dup_span) = comp.get_span(*span) {
                            if let Ok(rest_sig) = instrs_signature(rest) {
                                if rest_sig.args == sig.args && rest_sig.outputs + 1 == sig.outputs
                                {
                                    comp.emit_diagnostic(
                                        "Functions should consume their arguments. \
                                        Try removing this.",
                                        DiagnosticKind::Style,
                                        dup_span,
                                    );
                                }
                            }
                        }
                    }
                    new_func.flags |= flags;
                    comp.make_function(FunctionId::Named(name), sig, new_func)
                },
            )
        };
        let words_span = (binding.words.first())
            .zip(binding.words.last())
            .map(|(f, l)| f.span.clone().merge(l.span.clone()))
            .unwrap_or_else(|| {
                let mut span = binding.arrow_span;
                span.start = span.end;
                span
            });

        // Compile the body
        self.current_bindings.push(CurrentBinding {
            name: name.clone(),
            signature: binding.signature.as_ref().map(|s| s.value),
            referenced: false,
            global_index: local.index,
        });
        let mut binding_code_words = binding.words.iter().filter(|w| w.value.is_code());
        let is_single_func = binding_code_words.clone().count() == 1
            && (binding_code_words.next()).is_some_and(|w| matches!(&w.value, Word::Func(_)));
        let instrs_start = self.asm.instrs.len();
        let new_func = self.compile_words(binding.words, !is_single_func);
        let self_referenced = self.current_bindings.pop().unwrap().referenced;
        let mut new_func = new_func?;

        if self_referenced {
            let name = name.clone();
            let make = make_fn;
            make_fn = Box::new(move |new_func, sig, comp: &mut Compiler| {
                let mut f = make(new_func, sig, comp);
                f.flags |= FunctionFlags::RECURSIVE;
                let flags = f.flags;
                let instrs = eco_vec![Instr::PushFunc(f), Instr::CallRecursive(spandex)];
                let new_func = NewFunction { instrs, flags };
                comp.make_function(FunctionId::Named(name.clone()), sig, new_func)
            });
        }

        // Resolve signature
        match instrs_signature(&new_func.instrs) {
            Ok(mut sig) => {
                #[rustfmt::skip]
                let is_setinv = matches!(
                    new_func.instrs.as_slice(),
                    [Instr::PushFunc(_), Instr::PushFunc(_), Instr::Prim(Primitive::SetInverse, _)]
                );
                #[rustfmt::skip]
                let is_setund = matches!(
                    new_func.instrs.as_slice(),
                    [Instr::PushFunc(_), Instr::PushFunc(_), Instr::PushFunc(_), Instr::Prim(Primitive::SetUnder, _)]
                );
                if let [Instr::PushFunc(f)] = new_func.instrs.as_slice() {
                    // Binding is a single inline function
                    let func = if self_referenced {
                        make_fn(f.new_func(&self.asm), f.signature(), self)
                    } else {
                        let mut func = f.clone();
                        func.id = FunctionId::Named(name.clone());
                        func
                    };
                    sig = f.signature();
                    self.compile_bind_function(name, local, func, spandex, comment.as_deref())?;
                } else if sig == (0, 1) && !is_setinv && !is_setund {
                    if let &[Instr::Prim(Primitive::Tag, span)] = new_func.instrs.as_slice() {
                        new_func.instrs.push(Instr::Label {
                            label: name.clone(),
                            span,
                            remove: false,
                        })
                    }
                    // Binding is a constant
                    let val = if let [Instr::Push(v)] = new_func.instrs.as_slice() {
                        Some(v.clone())
                    } else {
                        match self.comptime_instrs(new_func.instrs.clone()) {
                            Ok(Some(vals)) => vals.into_iter().next(),
                            Ok(None) => None,
                            Err(e) => {
                                self.errors.push(e);
                                None
                            }
                        }
                    };

                    let is_const = val.is_some();
                    self.compile_bind_const(name, local, val, spandex, comment.as_deref());
                    if is_const {
                        if !(self.asm.top_slices.last())
                            .is_some_and(|slice| slice.start >= instrs_start)
                        {
                            self.asm.instrs.truncate(instrs_start);
                        }
                    } else {
                        // Add binding instrs to top slices
                        new_func.instrs.push(Instr::BindGlobal {
                            span: spandex,
                            index: local.index,
                        });
                        let start = self.asm.instrs.len();
                        (self.asm.instrs).extend(optimize_instrs(new_func.instrs, true, &self.asm));
                        let end = self.asm.instrs.len();
                        if end != start {
                            self.asm.top_slices.push(FuncSlice {
                                start,
                                len: end - start,
                            });
                        }
                    }
                } else if new_func.instrs.is_empty() {
                    // Binding binds the value above
                    match &mut self.scope.stack_height {
                        Ok(height) => {
                            if *height > 0 {
                                sig = Signature::new(0, 1);
                                *height -= 1;
                            }
                        }
                        Err(sp) => {
                            let sp = sp.clone();
                            self.add_error(
                                sp.span,
                                format!(
                                    "This line's signature is undefined: {}. \
                                    This prevents the later binding of {}.",
                                    sp.value, name
                                ),
                            );
                        }
                    }
                    if let Some(Instr::Push(val)) = self.asm.instrs.last().filter(|_| {
                        (self.asm.top_slices.last())
                            .is_some_and(|slice| slice.end() == self.asm.instrs.len())
                    }) {
                        let val = val.clone();
                        self.asm.instrs.pop();
                        self.compile_bind_const(
                            name,
                            local,
                            Some(val),
                            spandex,
                            comment.as_deref(),
                        );
                        let last_slice = self.asm.top_slices.last_mut().unwrap();
                        last_slice.len -= 1;
                        if last_slice.len == 0 {
                            self.asm.top_slices.pop();
                        }
                    } else if sig == (0, 0) {
                        let func = make_fn(new_func, sig, self);
                        self.compile_bind_function(name, local, func, spandex, comment.as_deref())?;
                    } else {
                        self.compile_bind_const(name, local, None, spandex, comment.as_deref());
                        let start = self.asm.instrs.len();
                        self.asm.instrs.push(Instr::BindGlobal {
                            span: spandex,
                            index: local.index,
                        });
                        self.asm.top_slices.push(FuncSlice { start, len: 1 });
                    }
                } else {
                    // Binding is a normal function
                    let func = make_fn(new_func, sig, self);
                    self.compile_bind_function(name, local, func, spandex, comment.as_deref())?;
                }

                // Validate signature
                if let Some(declared_sig) = &binding.signature {
                    if declared_sig.value != sig {
                        self.add_error(
                            declared_sig.span.clone(),
                            format!(
                                "Function signature mismatch: declared {} but inferred {}",
                                declared_sig.value, sig
                            ),
                        );
                    }
                }

                self.code_meta.function_sigs.insert(
                    words_span,
                    SigDecl {
                        sig,
                        explicit: binding.signature.is_some(),
                        inline: false,
                    },
                );
            }
            Err(e) => {
                if let Some(sig) = binding.signature {
                    // Binding is a normal function
                    if e.kind == SigCheckErrorKind::Ambiguous {
                        new_func.flags |= FunctionFlags::NO_INLINE;
                        let func = make_fn(new_func, sig.value, self);
                        self.compile_bind_function(name, local, func, spandex, comment.as_deref())?;
                    } else {
                        return Err(self.fatal_error(
                            sig.span.clone(),
                            format!(
                                "Cannot infer function signature: {e}. \
                                An explicit signature can only be used \
                                with ambiguous functions."
                            ),
                        ));
                    }
                } else {
                    self.add_error(
                        binding.name.span.clone(),
                        format!("Cannot infer function signature: {e}"),
                    );
                }
            }
        }
        Ok(())
    }
    pub(super) fn module(
        &mut self,
        m: Sp<ScopedModule>,
        prev_com: Option<EcoString>,
    ) -> UiuaResult {
        let m = m.value;
        let scope_kind = match &m.kind {
            ModuleKind::Named(name) => ScopeKind::Module(name.value.clone()),
            ModuleKind::Test => ScopeKind::Test,
        };
        let module = self.in_scope(scope_kind, |comp| comp.items(m.items))?;
        match m.kind {
            ModuleKind::Named(name) => {
                // Add imports
                if let Some(line) = m.imports {
                    for item in line.items {
                        if let Some(mut local) = module.names.get(&item.value).copied() {
                            local.public = false;
                            (self.code_meta.global_references)
                                .insert(item.span.clone(), local.index);
                            self.scope.names.insert(item.value, local);
                        } else {
                            self.add_error(
                                item.span.clone(),
                                format!("{} does not exist in {}", item.value, name.value),
                            );
                        }
                    }
                }
                // Add global
                let global_index = self.next_global;
                self.next_global += 1;
                let local = LocalName {
                    index: global_index,
                    public: true,
                };
                let comment = prev_com
                    .or_else(|| module.comment.clone())
                    .map(|text| DocComment::from(text.as_str()));
                self.asm.add_binding_at(
                    local,
                    BindingKind::Module(module),
                    Some(name.span.clone()),
                    comment,
                );
                // Add local
                self.scope.names.insert(name.value.clone(), local);
                (self.code_meta.global_references).insert(name.span.clone(), local.index);
            }
            ModuleKind::Test => {
                if let Some(line) = &m.imports {
                    self.add_error(
                        line.tilde_span.clone(),
                        "Items cannot be imported from test modules",
                    );
                }
            }
        }
        Ok(())
    }
    pub(super) fn import(
        &mut self,
        import: crate::ast::Import,
        prev_com: Option<EcoString>,
    ) -> UiuaResult {
        // Import module
        let module_path = self.import_module(&import.path.value, &import.path.span)?;
        // Bind name
        if let Some(name) = &import.name {
            let imported = self.imports.get(&module_path).unwrap();
            let global_index = self.next_global;
            self.next_global += 1;
            let local = LocalName {
                index: global_index,
                public: true,
            };
            self.asm.add_binding_at(
                local,
                BindingKind::Import(module_path.clone()),
                Some(name.span.clone()),
                prev_com
                    .or_else(|| imported.comment.clone())
                    .map(|text| DocComment::from(text.as_str())),
            );
            self.scope.names.insert(name.value.clone(), local);
        }
        // Bind items
        for item in import.items() {
            if let Some(local) = self
                .imports
                .get(&module_path)
                .and_then(|i| i.names.get(item.value.as_str()))
                .copied()
            {
                self.validate_local(&item.value, local, &item.span);
                (self.code_meta.global_references).insert(item.span.clone(), local.index);
                self.scope.names.insert(
                    item.value.clone(),
                    LocalName {
                        index: local.index,
                        public: true,
                    },
                );
            } else {
                self.add_error(
                    item.span.clone(),
                    format!(
                        "`{}` not found in module {}",
                        item.value,
                        module_path.display()
                    ),
                );
            }
        }
        Ok(())
    }
}
