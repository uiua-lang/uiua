use super::*;

impl Compiler {
    pub(super) fn data_def(
        &mut self,
        data: DataDef,
        top_level: bool,
        prelude: BindingPrelude,
    ) -> UiuaResult {
        self.experimental_error(&data.init_span, || {
            "Data definitions are experimental. To use them, add \
            `# Experimental!` to the top of the file."
        });
        if top_level {
            if let Some(name) = data.name.clone() {
                let comment = prelude.comment.clone();
                let module = self.in_scope(ScopeKind::Module(name.value.clone()), |comp| {
                    comp.data_def(data, false, prelude)
                })?;

                // Add global
                let global_index = self.next_global;
                self.next_global += 1;
                let local = LocalName {
                    index: global_index,
                    public: true,
                };
                let comment = comment.map(|text| DocComment::from(text.as_str()));
                self.asm.add_binding_at(
                    local,
                    BindingKind::Module(module),
                    Some(name.span.clone()),
                    comment,
                );
                // Add local
                self.scope.names.insert(name.value.clone(), local);
                (self.code_meta.global_references).insert(name.span.clone(), local.index);
                return Ok(());
            }
        }

        struct Field {
            name: EcoString,
            name_span: CodeSpan,
            span: usize,
            global_index: usize,
            init: Option<(EcoVec<Instr>, Signature)>,
        }
        let mut fields = Vec::new();
        let module_name = if let ScopeKind::Module(name) = &self.scope.kind {
            Some(name.clone())
        } else {
            None
        };
        // Collect fields
        let mut boxed = false;
        let mut has_fields = false;
        if let Some(data_fields) = data.fields {
            boxed = data_fields.boxed;
            has_fields = true;
            for data_field in data_fields.fields {
                let span = self.add_span(data_field.name.span.clone());
                let init = if let Some(default) = data_field.default {
                    let new_func = self.compile_words(default.words, true)?;
                    let sig = self.sig_of(&new_func.instrs, &data_field.name.span)?;
                    if sig.outputs != 1 {
                        self.add_error(
                            data_field.name.span.clone(),
                            format!(
                                "Default field initializer must have \
                                1 output, but its signature is {sig}"
                            ),
                        );
                    }
                    Some((new_func.instrs, sig))
                } else {
                    None
                };
                fields.push(Field {
                    name: data_field.name.value,
                    name_span: data_field.name.span,
                    global_index: 0,
                    span,
                    init,
                });
            }
        }

        // Make getters
        for (i, field) in fields.iter_mut().enumerate() {
            let name = &field.name;
            let id = FunctionId::Named(name.clone());
            let span = field.span;
            let mut instrs = eco_vec![
                Instr::push(i + data.variant as usize),
                Instr::Prim(Primitive::Pick, span)
            ];
            if boxed {
                instrs.push(Instr::ImplPrim(ImplPrimitive::UnBox, span));
                instrs.push(Instr::Label {
                    label: name.clone(),
                    span,
                    remove: true,
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
        let span = self.add_span(data.init_span.clone());
        let local = LocalName {
            index: self.next_global,
            public: true,
        };
        self.next_global += 1;
        let comment = (module_name.as_ref()).map(|name| format!("Names of `{name}`'s fields"));
        let name = Ident::from("Fields");
        self.compile_bind_const(
            name,
            local,
            Some(Array::from_iter(fields.iter().map(|f| f.name.as_str())).into()),
            span,
            comment.as_deref(),
        );

        // Make constructor
        let mut instrs = EcoVec::new();
        if has_fields {
            instrs.push(Instr::BeginArray);
        }
        let constructor_args: usize = fields
            .iter()
            .map(|f| f.init.as_ref().map(|(_, sig)| sig.args).unwrap_or(1))
            .sum();
        let has_inits = fields.iter().any(|f| f.init.is_some());
        if boxed || constructor_args < fields.len() {
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
                if boxed {
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
        if has_fields {
            instrs.push(Instr::EndArray { boxed, span });
        }
        if data.variant {
            let module_scope = self
                .higher_scopes
                .iter_mut()
                .filter(|scope| !matches!(scope.kind, ScopeKind::File(_)))
                .fuse()
                .find(|scope| matches!(&scope.kind, ScopeKind::Module(_)));
            if let Some(module_scope) = module_scope {
                instrs.push(Instr::push(module_scope.data_variants));
                module_scope.data_variants += 1;
                if let Some(name) = data.name {
                    instrs.push(Instr::Label {
                        label: name.value,
                        remove: false,
                        span,
                    });
                } else {
                    self.add_error(data.init_span.clone(), "Variants must have a name");
                }
                if has_fields {
                    if boxed {
                        instrs.push(Instr::Prim(Primitive::Box, span));
                    }
                    instrs.push(Instr::Prim(Primitive::Join, span));
                }
            } else {
                self.add_error(
                    data.init_span.clone(),
                    "Variants must be defined in a module",
                );
            }
        }
        let name = Ident::from("New");
        let id = FunctionId::Named(name.clone());
        let new_func = NewFunction {
            instrs,
            flags: FunctionFlags::TRACK_CALLER,
        };
        let constructor_func =
            self.make_function(id, Signature::new(constructor_args, 1), new_func);
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

        let mut function_stuff = None;
        // Call function
        if let Some(words) = data.func {
            self.in_scope(ScopeKind::Temp(None), |comp| {
                // Fill getters
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
                let word_span =
                    (words.first().unwrap().span.clone()).merge(words.last().unwrap().span.clone());
                if data.variant {
                    comp.add_error(word_span.clone(), "Variants may not have functions");
                }
                let mut filled_func = comp.compile_words(words, true)?;
                let flags = filled_func.flags;
                filled_func.flags |= FunctionFlags::TRACK_CALLER;
                let filled_sig = comp.sig_of(&filled_func.instrs, &word_span)?;
                let filled_func = comp.make_function(
                    FunctionId::Anonymous(word_span.clone()),
                    filled_sig,
                    filled_func,
                );
                let span = comp.add_span(word_span.clone());
                let instrs = eco_vec![
                    Instr::PushFunc(filled_func),
                    Instr::PushFunc(constructor_func.clone()),
                    Instr::Prim(Primitive::Fill, span)
                ];
                let sig = comp.sig_of(&instrs, &word_span)?;
                let new_func = NewFunction { instrs, flags };
                let local = LocalName {
                    index: comp.next_global,
                    public: true,
                };
                comp.next_global += 1;
                let func = comp.make_function(FunctionId::Named(name.clone()), sig, new_func);
                function_stuff = Some((local, func, span));
                Ok(())
            })?;
        }

        // Bind the call function
        if let Some((local, func, span)) = function_stuff {
            self.compile_bind_function("Call".into(), local, func, span, None)?;
        }

        // Bind the constructor
        self.compile_bind_function(name, local, constructor_func, span, Some(&comment))?;

        Ok(())
    }
    pub(super) fn end_enum(&mut self) -> UiuaResult {
        Ok(())
    }
}
