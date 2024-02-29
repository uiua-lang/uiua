//! Compiler code for bindings

use super::*;

impl Compiler {
    pub(super) fn binding(&mut self, binding: Binding, comment: Option<Arc<str>>) -> UiuaResult {
        let public = binding.public;

        // Alias re-bound imports
        if binding.words.iter().filter(|w| w.value.is_code()).count() == 1 {
            if let Some(r) = binding.words.iter().find_map(|w| match &w.value {
                Word::Ref(r) if !r.path.is_empty() => Some(r),
                _ => None,
            }) {
                if let Ok((path_locals, local)) = self.ref_local(r) {
                    self.validate_local(&r.name.value, local, &r.name.span);
                    self.code_meta
                        .global_references
                        .insert(binding.name.clone(), local.index);
                    for (local, comp) in path_locals.into_iter().zip(&r.path) {
                        (self.code_meta.global_references).insert(comp.module.clone(), local.index);
                    }
                    self.code_meta
                        .global_references
                        .insert(r.name.clone(), local.index);
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

        let span_index = self.add_span(span.clone());
        let local = LocalName {
            index: self.next_global,
            public,
        };
        self.next_global += 1;

        // Handle macro
        let ident_margs = ident_modifier_args(&name);
        if binding.array_macro {
            // Array macro
            if !self.scope.experimental {
                self.add_error(
                    span.clone(),
                    "Array macros are experimental. To use them, \
                    add `# Experimental!` to the top of the file.",
                );
            }
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
                    self.add_error(
                        span.clone(),
                        format!("Cannot infer array macro signature: {e}"),
                    );
                    Signature::new(0, 1)
                }
            };
            const ALLOWED_SIGS: &[Signature] = &[
                Signature::new(1, 1),
                Signature::new(2, 1),
                Signature::new(0, 0),
            ];
            if !ALLOWED_SIGS.contains(&sig) {
                return Err(self.fatal_error(
                    span.clone(),
                    format!(
                        "Array macros must have a signature of {} or {}, \
                        but a signature of {} was inferred",
                        Signature::new(1, 1),
                        Signature::new(2, 1),
                        sig
                    ),
                ));
            }
            if let Some(sig) = &binding.signature {
                if !ALLOWED_SIGS.contains(&sig.value) {
                    self.add_error(
                        sig.span.clone(),
                        format!(
                            "Array macros must have a signature of {} or {}",
                            Signature::new(1, 1),
                            Signature::new(2, 1),
                        ),
                    );
                }
            }
            let func = self.add_function(FunctionId::Named(name.clone()), sig, instrs);
            self.scope.names.insert(name.clone(), local);
            (self.asm).add_global_at(local, Global::Macro, Some(span.clone()), comment.clone());
            self.array_macros.insert(local.index, func);
            return Ok(());
        }
        // Stack macro
        let placeholder_count = count_placeholders(&binding.words);
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
            self.asm
                .add_global_at(local, Global::Macro, Some(span.clone()), comment.clone());
            self.stack_macros.insert(local.index, binding.words.clone());
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
                                comp.flush_diagnostics();
                            }
                        }
                    }
                }

                comp.add_function(FunctionId::Named(name.clone()), sig, instrs)
            },
        );

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
        let instrs = self.compile_words(binding.words, !is_single_func);
        let self_referenced = self.current_binding.take().unwrap().referenced;
        let mut instrs = instrs?;

        if self_referenced {
            let name = name.clone();
            let make = make_fn.clone();
            make_fn = Rc::new(move |instrs, sig, comp: &mut Compiler| {
                let f = make(instrs, sig, comp);
                let instrs = vec![Instr::PushFunc(f), Instr::Prim(Primitive::This, span_index)];
                comp.add_function(FunctionId::Named(name.clone()), sig, instrs)
            });
        }

        // Check if binding is an import
        let mut is_import = false;
        let mut sig = None;
        if let [init @ .., Instr::Prim(Primitive::Sys(SysOp::Import), _)] = instrs.as_slice() {
            is_import = true;
            match init {
                [Instr::Push(path)] => {
                    if let Some(sig) = &binding.signature {
                        self.add_error(
                            sig.span.clone(),
                            "Cannot declare a signature for a module import",
                        );
                    }
                    match path {
                        Value::Char(arr) if arr.rank() == 1 => {
                            let path: String = arr.data.iter().copied().collect();
                            let module = self.import_module(path.as_ref(), span)?;
                            self.asm.add_global_at(
                                local,
                                Global::Module { module },
                                Some(binding.name.span.clone()),
                                comment.clone(),
                            );
                            self.scope.names.insert(name.clone(), local);
                        }
                        _ => self.add_error(span.clone(), "Import path must be a string"),
                    }
                }
                [Instr::Push(item), Instr::Push(path)] => match path {
                    Value::Char(arr) if arr.rank() == 1 => {
                        let path: String = arr.data.iter().copied().collect();
                        let module = self.import_module(path.as_ref(), span)?;
                        match item {
                            Value::Char(arr) if arr.rank() == 1 => {
                                let item: String = arr.data.iter().copied().collect();
                                if let Some(&local) = self.imports[&module].names.get(item.as_str())
                                {
                                    self.validate_local(&item, local, span);
                                    self.scope.names.insert(
                                        name.clone(),
                                        LocalName {
                                            index: local.index,
                                            public,
                                        },
                                    );
                                    if let Some(s) =
                                        self.asm.bindings[local.index].global.signature()
                                    {
                                        sig = Some(s);
                                    } else {
                                        self.add_error(
                                            span.clone(),
                                            "Cannot define a signature for a module rebind",
                                        )
                                    }
                                } else {
                                    self.add_error(
                                        span.clone(),
                                        format!("Item `{item}` not found in module `{path}`"),
                                    )
                                }
                            }
                            _ => self.add_error(span.clone(), "Import item must be a string"),
                        };
                    }
                    _ => self.add_error(span.clone(), "Import path must be a string"),
                },
                _ => self.add_error(span.clone(), "&i must be followed by one or two strings"),
            }
        }

        // Resolve signature
        match instrs_signature(&instrs) {
            Ok(s) => {
                let mut sig = sig.unwrap_or(s);
                // Runtime-dependent binding
                if instrs.is_empty() {
                    // Binding from the stack set above
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
                if is_import {
                } else if let [Instr::PushFunc(f)] = instrs.as_slice() {
                    // Binding is a single inline function
                    let func = make_fn(f.instrs(self).into(), f.signature(), self);
                    self.compile_bind_function(&name, local, func, span_index, comment)?;
                } else if (sig.args == 0 && sig.outputs <= 1)
                    && (sig.outputs > 0 || instrs.is_empty())
                    && !is_setinv
                    && !is_setund
                {
                    self.asm.bind_const(local, None, span_index, comment);
                    self.scope.names.insert(name.clone(), local);
                    // Add binding instrs to top slices
                    instrs.push(Instr::BindGlobal {
                        span: span_index,
                        index: local.index,
                    });
                    let start = self.asm.instrs.len();
                    self.asm
                        .instrs
                        .extend(optimize_instrs(instrs, true, &self.asm));
                    let end = self.asm.instrs.len();
                    self.asm.top_slices.push(FuncSlice {
                        start,
                        len: end - start,
                    });
                } else {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig, self);
                    self.compile_bind_function(&name, local, func, span_index, comment)?;
                }
            }
            Err(e) => {
                if is_import {
                } else if let Some(sig) = binding.signature {
                    // Binding is a normal function
                    instrs.insert(0, Instr::NoInline);
                    let func = make_fn(instrs, sig.value, self);
                    self.compile_bind_function(&name, local, func, span_index, comment)?;
                } else {
                    self.add_error(
                        binding.name.span.clone(),
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the `←`."
                            } else {
                                ""
                            }
                        ),
                    );
                }
            }
        }
        Ok(())
    }
    pub(super) fn import(
        &mut self,
        import: crate::ast::Import,
        prev_com: Option<Arc<str>>,
    ) -> UiuaResult {
        // Import module
        let module = self.import_module(&import.path.value, &import.path.span)?;
        // Bind name
        if let Some(name) = &import.name {
            let imported = self.imports.get(&module).unwrap();
            let global_index = self.next_global;
            self.next_global += 1;
            let local = LocalName {
                index: global_index,
                public: false,
            };
            self.asm.add_global_at(
                local,
                Global::Module {
                    module: module.clone(),
                },
                Some(name.span.clone()),
                prev_com.or_else(|| imported.comment.clone()),
            );
            self.scope.names.insert(name.value.clone(), local);
        }
        // Bind items
        for item in import.items() {
            if let Some(local) = self
                .imports
                .get(&module)
                .and_then(|i| i.names.get(item.value.as_str()))
                .copied()
            {
                self.validate_local(&item.value, local, &item.span);
                (self.code_meta.global_references).insert(item.clone(), local.index);
                self.scope.names.insert(
                    item.value.clone(),
                    LocalName {
                        index: local.index,
                        public: false,
                    },
                );
            } else {
                self.add_error(
                    item.span.clone(),
                    format!("`{}` not found in module {}", item.value, module.display()),
                );
            }
        }
        Ok(())
    }
}
