use crate::ArrayLen;

use super::*;

impl Compiler {
    pub(super) fn data_def(
        &mut self,
        mut data: DataDef,
        top_level: bool,
        mut prelude: BindingPrelude,
    ) -> UiuaResult {
        self.experimental_error(&data.init_span, || {
            "Data definitions are experimental. To use them, add \
            `# Experimental!` to the top of the file."
        });
        if let Some(words) = &mut data.func {
            let word = words.pop();
            if let Some(word) = word {
                match word.value {
                    Word::Comment(com) => {
                        let pre_com = prelude.comment.get_or_insert_with(Default::default);
                        if !pre_com.is_empty() {
                            pre_com.push('\n');
                        }
                        pre_com.push_str(&com);
                    }
                    _ => words.push(word),
                }
            }
        }
        if (data.func.as_ref()).is_some_and(|words| !words.iter().any(|word| word.value.is_code()))
        {
            data.func = None;
        }
        if top_level {
            if let Some(name) = data.name.clone() {
                let comment = prelude.comment.clone();
                let (module, ()) = self
                    .in_scope(ScopeKind::Module(name.value.clone()), |comp| {
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
                    BindingMeta {
                        comment,
                        ..Default::default()
                    },
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
            comment: Option<String>,
            validator: Option<(Node, bool, CodeSpan)>,
            init: Option<SigNode>,
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
            for mut data_field in data_fields.fields {
                let span = self.add_span(data_field.name.span.clone());
                let mut comment = data_field.comments.as_ref().map(|comments| {
                    (comments.lines.iter().enumerate())
                        .flat_map(|(i, com)| {
                            if i == 0 {
                                vec![com.value.as_str()]
                            } else {
                                vec![com.value.as_str(), "\n"]
                            }
                        })
                        .collect::<String>()
                });
                // Compile validator
                let validator = if let Some(validator) = data_field.validator {
                    let mut sn = self.words_sig(validator.words)?;
                    if sn.sig.args != 1 {
                        self.add_error(
                            data_field.name.span.clone(),
                            format!(
                                "Field validator must have 1 \
                                argument, but its signature is {}",
                                sn.sig
                            ),
                        );
                    }
                    if sn.sig.outputs > 1 {
                        self.add_error(
                            data_field.name.span.clone(),
                            format!(
                                "Field validator must have 0 or 1 \
                                output, but its signature is {}",
                                sn.sig
                            ),
                        );
                    }
                    let mut validation_only = false;
                    if sn.sig.outputs == 0 {
                        validation_only = true;
                        sn.node.prepend(Node::Prim(Primitive::Dup, span));
                    }
                    Some((sn.node, validation_only, validator.open_span.clone()))
                } else {
                    None
                };
                // Compile initializer
                if (data_field.init.as_ref())
                    .is_some_and(|default| !default.words.iter().any(|w| w.value.is_code()))
                {
                    data_field.init = None;
                }
                let init = if let Some(mut init) = data_field.init {
                    // Process comment
                    let mut sem = None;
                    if let Some(word) = init.words.pop() {
                        match word.value {
                            Word::Comment(com) => {
                                if comment.is_none() {
                                    comment = Some(com)
                                }
                            }
                            Word::SemanticComment(com) => sem = Some(word.span.sp(com)),
                            _ => init.words.push(word),
                        }
                    }
                    // Compile words
                    let mut sn = self.words_sig(init.words)?;
                    if let Some((va_node, ..)) = &validator {
                        sn.node.push(va_node.clone());
                    }
                    if sn.sig.outputs != 1 {
                        self.add_error(
                            data_field.name.span.clone(),
                            format!(
                                "Field initializer must have \
                                1 output, but its signature is {}",
                                sn.sig
                            ),
                        );
                    }
                    if let Some(sem) = sem {
                        sn = SigNode::new(
                            sn.sig,
                            self.semantic_comment(sem.value, sem.span, sn.node),
                        );
                    }
                    Some(sn)
                } else {
                    validator
                        .as_ref()
                        .map(|(va_node, ..)| SigNode::new(Signature::new(1, 1), va_node.clone()))
                };
                fields.push(Field {
                    name: data_field.name.value,
                    name_span: data_field.name.span,
                    global_index: 0,
                    comment,
                    span,
                    validator,
                    init,
                });
            }
        }

        let mut variant_index = 0;
        if data.variant {
            let module_scope = self.higher_scopes.last_mut().unwrap_or(&mut self.scope);
            variant_index = module_scope.data_variants;
            module_scope.data_variants += 1;
        }

        // Make getters
        for (i, field) in fields.iter_mut().enumerate() {
            let name = &field.name;
            let id = FunctionId::Named(name.clone());
            let span = field.span;
            let mut node = Node::empty();
            if data.variant {
                node.push(Node::new_push(variant_index));
                if let Some(name) = &data.name {
                    node.push(Node::Label(name.value.clone(), span));
                }
                node.push(Node::ImplPrim(ImplPrimitive::ValidateVariant, span));
            }
            node.push(Node::new_push(i));
            node.push(Node::Prim(Primitive::Pick, span));
            if boxed {
                node.push(Node::ImplPrim(ImplPrimitive::UnBox, span));
                node.push(Node::RemoveLabel(Some(field.name.clone()), span));
            }
            // Add validator
            if let Some((va_instrs, validation_only, _va_span)) = field.validator.take() {
                let inverse = va_instrs.un_inverse(&self.asm);
                let make_node = |node: Node| SigNode::new(Signature::new(1, 1), node);
                match inverse {
                    Ok(va_inverse) => node.push(Node::CustomInverse(
                        CustomInverse {
                            normal: Ok(make_node(va_inverse.clone())),
                            un: Some(make_node(va_instrs.clone())),
                            under: Some((make_node(va_inverse), make_node(va_instrs))),
                            ..Default::default()
                        }
                        .into(),
                        field.span,
                    )),
                    Err(_) if validation_only => node.push(Node::CustomInverse(
                        CustomInverse {
                            un: Some(make_node(va_instrs.clone())),
                            under: Some((Default::default(), make_node(va_instrs))),
                            ..Default::default()
                        }
                        .into(),
                        field.span,
                    )),
                    Err(e) => self.add_error(
                        field.name_span.clone(),
                        format!("Transforming validator has no inverse: {e}"),
                    ),
                }
            }
            let func = self
                .asm
                .add_function(id.clone(), Signature::new(1, 1), node);
            let local = LocalName {
                index: self.next_global,
                public: true,
            };
            field.global_index = local.index;
            self.next_global += 1;
            let comment = match (&module_name, &field.comment) {
                (None, None) => format!("Get `{name}`"),
                (Some(module_name), None) => format!("Get `{module_name}`'s `{name}`"),
                (None, Some(comment)) => comment.into(),
                (Some(module_name), Some(comment)) => {
                    format!("Get `{module_name}`'s `{name}`\n{comment}")
                }
            };
            let meta = BindingMeta {
                comment: Some(DocComment::from(comment.as_str())),
                ..Default::default()
            };
            self.compile_bind_function(name.clone(), local, func, span, meta)?;
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
            BindingMeta {
                comment: comment.as_deref().map(DocComment::from),
                ..Default::default()
            },
        );

        // Make constructor
        let constructor_args: usize = fields
            .iter()
            .map(|f| f.init.as_ref().map(|sn| sn.sig.args).unwrap_or(1))
            .sum();
        let mut node = if has_fields {
            let mut inner = Node::default();
            for field in fields.iter().rev() {
                let mut arg = if let Some(sn) = &field.init {
                    let mut arg = sn.clone();
                    if !boxed {
                        arg.node.push(Node::ImplPrim(
                            ImplPrimitive::ValidateNonBoxedVariant,
                            field.span,
                        ));
                    }
                    arg
                } else {
                    self.code_meta
                        .global_references
                        .insert(field.name_span.clone(), field.global_index);
                    SigNode::new(Signature::new(1, 1), Node::empty())
                };
                if boxed {
                    arg.node.push(Node::Label(field.name.clone(), span));
                }
                if !inner.is_empty() {
                    for _ in 0..arg.sig.args {
                        inner = Node::Mod(
                            Primitive::Dip,
                            eco_vec![inner
                                .sig_node()
                                .expect("Field initializer should have a signature")],
                            span,
                        );
                    }
                }
                inner.push(arg.node);
            }
            Node::Array {
                len: ArrayLen::Static(fields.len()),
                inner: inner.into(),
                boxed,
                prim: None,
                span,
            }
        } else {
            Node::empty()
        };
        // Handle variant
        if data.variant {
            node.push(Node::new_push(variant_index));
            if let Some(name) = data.name {
                node.push(Node::Label(name.value, span));
            } else {
                self.add_error(data.init_span.clone(), "Variants must have a name");
            }
            if has_fields {
                node.push(Node::ImplPrim(ImplPrimitive::TagVariant, span));
            }
        }
        let name = Ident::from("New");
        let id = FunctionId::Named(name.clone());
        let constructor_func = self
            .asm
            .add_function(id, Signature::new(constructor_args, 1), node);
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
            match field.init.as_ref().map(|sn| sn.sig.args) {
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
                            comment.push(SUBSCRIPT_DIGITS[i % 10]);
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
                // Filled getters
                for field in &fields {
                    let name = &field.name;
                    let id = FunctionId::Named(name.clone());
                    let node = Node::from_iter([
                        Node::ImplPrim(ImplPrimitive::UnPop, field.span),
                        comp.global_index(field.global_index, field.name_span.clone()),
                    ]);
                    let func = comp.asm.add_function(id, Signature::new(0, 1), node);
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
                    let meta = BindingMeta {
                        comment: Some(DocComment::from(comment.as_str())),
                        ..Default::default()
                    };
                    comp.compile_bind_function(field.name.clone(), local, func, field.span, meta)?;
                }
                let word_span =
                    (words.first().unwrap().span.clone()).merge(words.last().unwrap().span.clone());
                if data.variant {
                    comp.add_error(word_span.clone(), "Variants may not have functions");
                }
                let filled = comp.words_sig(words)?;
                let span = comp.add_span(word_span.clone());
                let fill_construct = SigNode::new(
                    constructor_func.sig,
                    Node::Call(constructor_func.clone(), span),
                );
                let fill_pop = Node::Prim(Primitive::Pop, span).sig_node().unwrap();
                let mut node = Node::Mod(Primitive::Fill, eco_vec![fill_pop, filled], span);
                let sig = comp.sig_of(&node, &word_span)?;
                node = Node::Mod(
                    Primitive::Fill,
                    eco_vec![fill_construct, SigNode::new(sig, node)],
                    span,
                );
                let sig = comp.sig_of(&node, &word_span)?;
                let local = LocalName {
                    index: comp.next_global,
                    public: true,
                };
                comp.next_global += 1;
                let func = comp
                    .asm
                    .add_function(FunctionId::Named(name.clone()), sig, node);
                function_stuff = Some((local, func, span));
                Ok(())
            })?;
        }

        // Bind the call function
        if let Some((local, func, span)) = function_stuff {
            self.compile_bind_function("Call".into(), local, func, span, BindingMeta::default())?;
        }

        // Bind the constructor
        let meta = BindingMeta {
            comment: Some(DocComment::from(comment.as_str())),
            ..Default::default()
        };
        self.compile_bind_function(name, local, constructor_func, span, meta)?;

        Ok(())
    }
    pub(super) fn end_enum(&mut self) -> UiuaResult {
        Ok(())
    }
}
