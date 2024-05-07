//! Compiler code for bindings

use super::*;

impl Compiler {
    pub(super) fn binding(&mut self, binding: Binding, comment: Option<EcoString>) -> UiuaResult {
        let public = binding.public;

        // Alias re-bound imports
        if binding.words.iter().filter(|w| w.value.is_code()).count() == 1 {
            if let Some(r) = binding.words.iter().find_map(|w| match &w.value {
                Word::Ref(r) if !r.path.is_empty() => Some(r),
                _ => None,
            }) {
                if let Ok((path_locals, local)) = self.ref_local(r) {
                    self.validate_local(&r.name.value, local, &r.name.span);
                    (self.code_meta.global_references).insert(binding.name.clone(), local.index);
                    for (local, comp) in path_locals.into_iter().zip(&r.path) {
                        (self.code_meta.global_references).insert(comp.module.clone(), local.index);
                    }
                    (self.code_meta.global_references).insert(r.name.clone(), local.index);
                    self.scope.names.insert(
                        binding.name.value,
                        LocalName {
                            index: local.index,
                            public,
                        },
                    );
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

        let comment = comment.or_else(|| {
            binding.words.iter().last().and_then(|w| match &w.value {
                Word::Comment(c) => Some(c.as_str().into()),
                _ => None,
            })
        });

        // Handle macro
        let ident_margs = ident_modifier_args(&name);
        let placeholder_count = count_placeholders(&binding.words);
        if binding.array_macro {
            if placeholder_count > 0 {
                return Err(
                    self.fatal_error(span.clone(), "Array macros may not contain placeholders")
                );
            }
            // Array macro
            if ident_margs == 0 {
                self.add_error(
                    span.clone(),
                    format!(
                        "Array macros must take at least 1 operand, \
                        but `{name}`'s name suggests it takes 0",
                    ),
                );
            }
            let instrs = self.compile_words(binding.words, true)?;
            let sig = match instrs_signature(&instrs) {
                Ok(s) => s,
                Err(e) => {
                    if let Some(sig) = binding.signature {
                        sig.value
                    } else {
                        self.add_error(
                            span.clone(),
                            format!("Cannot infer array macro signature: {e}"),
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
                        "Array macros must have a signature of {} or {}, \
                        but a signature of {} was inferred",
                        Signature::new(1, 1),
                        Signature::new(2, 1),
                        sig
                    ),
                );
            }
            let function = self.make_function(FunctionId::Named(name.clone()), sig, instrs);
            self.scope.names.insert(name.clone(), local);
            (self.asm).add_global_at(
                local,
                BindingKind::Macro,
                Some(span.clone()),
                comment.map(|text| DocComment::from(text.as_str())),
            );
            let mac = ArrayMacro {
                function,
                names: self.scope.names.clone(),
            };
            self.array_macros.insert(local.index, mac);
            return Ok(());
        }
        // Stack macro
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
            (self.asm).add_global_at(
                local,
                BindingKind::Macro,
                Some(span.clone()),
                comment.map(|text| DocComment::from(text.as_str())),
            );
            let mut words = binding.words.clone();
            recurse_words(&mut words, &mut |word| match &word.value {
                Word::Ref(r) => {
                    if let Ok((path_locals, local)) = self.ref_local(r) {
                        self.validate_local(&r.name.value, local, &r.name.span);
                        (self.code_meta.global_references).insert(r.name.clone(), local.index);
                        for (local, comp) in path_locals.into_iter().zip(&r.path) {
                            (self.code_meta.global_references)
                                .insert(comp.module.clone(), local.index);
                        }
                    }
                }
                Word::IncompleteRef { path, in_macro_arg } => {
                    if let Ok(Some((_, path_locals))) = self.ref_path(path, *in_macro_arg) {
                        for (local, comp) in path_locals.into_iter().zip(path) {
                            (self.code_meta.global_references)
                                .insert(comp.module.clone(), local.index);
                        }
                    }
                }
                _ => {}
            });
            let mac = StackMacro {
                words,
                names: self.scope.names.clone(),
            };
            self.stack_macros.insert(local.index, mac);
            return Ok(());
        }

        let mut make_fn: Rc<dyn Fn(_, _, &mut Compiler) -> _> = Rc::new(
            |instrs: EcoVec<Instr>, sig: Signature, comp: &mut Compiler| {
                // Diagnostic for function that doesn't consume its arguments
                if let Some((Instr::Prim(Primitive::Dup, span), rest)) = instrs.split_first() {
                    if let Span::Code(dup_span) = comp.get_span(*span) {
                        if let Ok(rest_sig) = instrs_signature(rest) {
                            if rest_sig.args == sig.args && rest_sig.outputs + 1 == sig.outputs {
                                comp.emit_diagnostic(
                                    format!(
                                        "Functions should consume their arguments. \
                                        Try removing this {}.",
                                        Primitive::Dup.format()
                                    ),
                                    DiagnosticKind::Style,
                                    dup_span,
                                );
                            }
                        }
                    }
                }

                comp.make_function(FunctionId::Named(name.clone()), sig, instrs)
            },
        );
        let words_span = (binding.words.first())
            .zip(binding.words.last())
            .map(|(f, l)| f.span.clone().merge(l.span.clone()))
            .unwrap_or_else(|| {
                let mut span = binding.arrow_span;
                span.start = span.end;
                span
            });

        // Compile the body
        self.current_binding = Some(CurrentBinding {
            name: name.clone(),
            signature: binding.signature.as_ref().map(|s| s.value),
            referenced: false,
            global_index: local.index,
        });
        let mut binding_code_words = binding.words.iter().filter(|w| w.value.is_code());
        let is_single_func = binding_code_words.clone().count() == 1
            && (binding_code_words.next()).is_some_and(|w| matches!(&w.value, Word::Func(_)));
        let instrs_start = self.asm.instrs.len();
        let instrs = self.compile_words(binding.words, !is_single_func);
        let self_referenced = self.current_binding.take().unwrap().referenced;
        let mut instrs = instrs?;

        if self_referenced {
            let name = name.clone();
            let make = make_fn.clone();
            make_fn = Rc::new(move |instrs, sig, comp: &mut Compiler| {
                let mut f = make(instrs, sig, comp);
                f.recursive = true;
                let instrs = vec![Instr::PushFunc(f), Instr::CallRecursive(spandex)];
                comp.make_function(FunctionId::Named(name.clone()), sig, instrs)
            });
        }

        // Resolve signature
        match instrs_signature(&instrs) {
            Ok(mut sig) => {
                // Validate signature
                if let Some(declared_sig) = &binding.signature {
                    let sig_to_check = if let [Instr::PushFunc(f)] = instrs.as_slice() {
                        // If this is a function wrapped in parens, check the signature of the
                        // function rather than the signature of the binding's words
                        f.signature()
                    } else if instrs.is_empty() {
                        Signature::new(0, 1)
                    } else {
                        sig
                    };
                    if declared_sig.value != sig_to_check {
                        self.add_error(
                            declared_sig.span.clone(),
                            format!(
                                "Function signature mismatch: declared {} but inferred {}",
                                declared_sig.value, sig_to_check
                            ),
                        );
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
                if let [Instr::PushFunc(f)] = instrs.as_slice() {
                    // Binding is a single inline function
                    sig = f.signature();
                    let func = make_fn(f.instrs(self).into(), f.signature(), self);
                    self.compile_bind_function(&name, local, func, spandex, comment.as_deref())?;
                } else if sig == (0, 1) && !is_setinv && !is_setund {
                    if let &[Instr::Prim(Primitive::Tag, span)] = instrs.as_slice() {
                        instrs.push(Instr::Label {
                            label: name.clone(),
                            span,
                        })
                    }
                    // Binding is a constant
                    let val = if let [Instr::Push(v)] = instrs.as_slice() {
                        Some(v.clone())
                    } else {
                        match self.comptime_instrs(instrs.clone()) {
                            Ok(Some(vals)) => vals.into_iter().next(),
                            Ok(None) => None,
                            Err(e) => {
                                self.errors.push(e);
                                None
                            }
                        }
                    };
                    let is_const = val.is_some();
                    self.compile_bind_const(&name, local, val, spandex, comment.as_deref());
                    self.scope.names.insert(name.clone(), local);
                    if is_const {
                        self.asm.instrs.truncate(instrs_start);
                    } else {
                        // Add binding instrs to top slices
                        instrs.push(Instr::BindGlobal {
                            span: spandex,
                            index: local.index,
                        });
                        let start = self.asm.instrs.len();
                        (self.asm.instrs).extend(optimize_instrs(instrs, true, &self.asm));
                        let end = self.asm.instrs.len();
                        self.asm.top_slices.push(FuncSlice {
                            start,
                            len: end - start,
                        });
                    }
                } else if instrs.is_empty() {
                    // Binding binds the value above
                    match &mut self.scope.stack_height {
                        Ok(height) => {
                            if *height > 0 {
                                sig = Signature::new(0, 1);
                            }
                            *height = height.saturating_sub(1);
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
                    if let Some(Instr::Push(val)) = self.asm.instrs.last() {
                        let val = val.clone();
                        self.asm.instrs.pop();
                        self.compile_bind_const(
                            &name,
                            local,
                            Some(val),
                            spandex,
                            comment.as_deref(),
                        );
                        if let Some(last_slice) = self.asm.top_slices.last_mut() {
                            last_slice.len -= 1;
                            if last_slice.len == 0 {
                                self.asm.top_slices.pop();
                            }
                        }
                    } else {
                        self.compile_bind_const(&name, local, None, spandex, comment.as_deref());
                        let start = self.asm.instrs.len();
                        self.asm.instrs.push(Instr::BindGlobal {
                            span: spandex,
                            index: local.index,
                        });
                        self.asm.top_slices.push(FuncSlice { start, len: 1 });
                    }
                    self.scope.names.insert(name.clone(), local);
                } else {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig, self);
                    self.compile_bind_function(&name, local, func, spandex, comment.as_deref())?;
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
                    instrs.insert(0, Instr::NoInline);
                    let func = make_fn(instrs, sig.value, self);
                    self.compile_bind_function(&name, local, func, spandex, comment.as_deref())?;
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
            self.asm.add_global_at(
                local,
                BindingKind::Module(module_path.clone()),
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
                (self.code_meta.global_references).insert(item.clone(), local.index);
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
