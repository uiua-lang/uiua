use super::*;

#[derive(Clone)]
pub(crate) struct DataFuncInfo {
    pub name: EcoString,
    pub fields: BTreeMap<EcoString, FieldInfo>,
}

#[derive(Clone)]
pub(crate) struct FieldInfo {
    pub index: usize,
    pub init_sig: Option<Signature>,
    pub comment: EcoString,
}

impl Compiler {
    pub(super) fn data_def(
        &mut self,
        mut data: DataDef,
        top_level: bool,
        mut prelude: BindingPrelude,
    ) -> UiuaResult {
        // Clean up comment
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
        // Remove empty function
        if (data.func.as_ref()).is_some_and(|words| !words.iter().any(|word| word.value.is_code()))
        {
            data.func = None;
        }
        // Handle top-level named defs as modules
        let def_comment = prelude
            .comment
            .clone()
            .map(|text| DocComment::from(text.as_str()));
        if top_level {
            if let Some(name) = data.name.clone() {
                let global_index = self.next_global;
                self.next_global += 1;
                let local = LocalName {
                    index: global_index,
                    public: data.public,
                };

                let (module, ()) = self
                    .in_scope(ScopeKind::Module(name.value.clone()), |comp| {
                        comp.data_def(data, false, prelude)
                    })?;

                // Add global
                self.asm.add_binding_at(
                    local,
                    BindingKind::Module(module),
                    Some(name.span.clone()),
                    BindingMeta {
                        comment: def_comment,
                        ..Default::default()
                    },
                );
                // Add local
                self.scope.add_module_name(name.value.clone(), local);
                (self.code_meta.global_references).insert(name.span.clone(), local.index);
                return Ok(());
            } else if self.scope.has_data_def {
                return Err(self.error(
                    data.span(),
                    "A module cannot have multiple unnamed data definitions",
                ));
            } else if !data.public {
                self.add_error(
                    data.init_span.clone(),
                    "Unnamed data definitions cannot be marked private",
                );
            }
        }

        // Add to defs
        let def_name = if let ScopeKind::Module(name) = &self.scope.kind {
            Some(name.clone())
        } else {
            None
        };

        struct Field {
            name: EcoString,
            name_span: CodeSpan,
            span: usize,
            global_index: usize,
            comment: Option<String>,
            validator_inv: Option<Node>,
            init: Option<SigNode>,
        }
        let mut fields = Vec::new();
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
                let validator_and_inv = if let Some(validator) = data_field.validator {
                    self.experimental_error(&data.init_span, || {
                        "Field validators are experimental. To use them, add \
                        `# Experimental!` to the top of the file."
                    });
                    let mut validator = self.words_sig(validator.words)?;
                    if validator.sig.args() != 1 {
                        self.add_error(
                            data_field.name.span.clone(),
                            format!(
                                "Field validator must have 1 \
                                argument, but its signature is {}",
                                validator.sig
                            ),
                        );
                    }
                    if validator.sig.outputs() > 1 {
                        self.add_error(
                            data_field.name.span.clone(),
                            format!(
                                "Field validator must have 0 or 1 \
                                output, but its signature is {}",
                                validator.sig
                            ),
                        );
                    }
                    let mut validation_only = false;
                    if validator.sig.outputs() == 0 {
                        validation_only = true;
                        validator.node.prepend(Node::Prim(Primitive::Dup, span));
                        validator.sig.set_outputs(1);
                    }

                    let inverse = validator.node.un_inverse(&self.asm);
                    Some(match inverse {
                        Ok(inverse) => {
                            let inverse = SigNode::new(Signature::new(1, 1), inverse);
                            (
                                Node::CustomInverse(
                                    CustomInverse {
                                        normal: Ok(validator.clone()),
                                        un: Some(inverse.clone()),
                                        under: Some((validator.clone(), inverse.clone())),
                                        ..Default::default()
                                    }
                                    .into(),
                                    span,
                                ),
                                Node::CustomInverse(
                                    CustomInverse {
                                        normal: Ok(inverse.clone()),
                                        un: Some(validator.clone()),
                                        under: Some((inverse, validator)),
                                        ..Default::default()
                                    }
                                    .into(),
                                    span,
                                ),
                            )
                        }
                        Err(_) if validation_only => (
                            Node::CustomInverse(
                                CustomInverse {
                                    normal: Ok(validator.clone()),
                                    un: Some(SigNode::default()),
                                    under: Some((validator.clone(), SigNode::default())),
                                    ..Default::default()
                                }
                                .into(),
                                span,
                            ),
                            Node::CustomInverse(
                                CustomInverse {
                                    un: Some(validator.clone()),
                                    under: Some((SigNode::default(), validator)),
                                    ..Default::default()
                                }
                                .into(),
                                span,
                            ),
                        ),
                        Err(e) => {
                            self.add_error(
                                data_field.name.span.clone(),
                                format!("Transforming validator has no inverse: {e}"),
                            );
                            (validator.node, Node::empty())
                        }
                    })
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
                    if let Some((va_node, _)) = &validator_and_inv {
                        sn.node.push(va_node.clone());
                    }
                    if sn.sig.outputs() != 1 {
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
                    validator_and_inv
                        .as_ref()
                        .map(|(va_node, _)| SigNode::new(Signature::new(1, 1), va_node.clone()))
                };
                fields.push(Field {
                    name: data_field.name.value,
                    name_span: data_field.name.span,
                    global_index: 0,
                    comment,
                    span,
                    validator_inv: validator_and_inv.map(|(_, inv)| inv),
                    init,
                });
            }
        }

        let mut variant_index = 0;
        if data.variant {
            if let Some(name) = data.name.as_ref() {
                let module_scope = self.higher_scopes.last_mut().unwrap_or(&mut self.scope);
                variant_index = module_scope.data_variants.len();
                module_scope.data_variants.insert(name.value.clone());
            }
        }

        // Make getters
        for (i, field) in fields.iter_mut().enumerate() {
            let field_name = &field.name;
            let id = FunctionId::Named(field_name.clone());
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
            node = Node::TrackCaller(node.into());
            if boxed {
                node.push(Node::ImplPrim(ImplPrimitive::UnBox, span));
                node.push(Node::RemoveLabel(Some(field.name.clone()), span));
            }
            // Add validator
            node.extend(field.validator_inv.take());
            let func = self
                .asm
                .add_function(id.clone(), Signature::new(1, 1), node);
            let local = LocalName {
                index: self.next_global,
                public: true,
            };
            field.global_index = local.index;
            self.next_global += 1;
            let comment = match (&def_name, &field.comment) {
                (Some(def_name), Some(comment)) => {
                    format!("Get {def_name}'s {field_name}\n{comment}")
                }
                (Some(def_name), None) => format!("Get {def_name}'s {field_name}"),
                (None, Some(comment)) => format!("Get {field_name}\n{comment}"),
                (None, None) => format!("Get {field_name}"),
            };
            let meta = BindingMeta {
                comment: Some(DocComment::from(comment.as_str())),
                ..Default::default()
            };
            self.compile_bind_function(field_name.clone(), local, func, span, meta);
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
        let comment = match &def_name {
            Some(def_name) => format!("Names of {def_name}'s fields"),
            None => "Names of fields".into(),
        };
        let name = Ident::from("Fields");
        self.compile_bind_const(
            name,
            local,
            Some(
                fields
                    .iter()
                    .map(|f| f.name.as_str())
                    .collect::<Array<_>>()
                    .into(),
            ),
            span,
            BindingMeta {
                comment: Some(DocComment::from(comment.as_str())),
                ..Default::default()
            },
        );

        // Make constructor
        let constructor_args: usize = fields
            .iter()
            .map(|f| f.init.as_ref().map_or(1, |sn| sn.sig.args()))
            .sum();
        let mut node = if has_fields {
            let mut field_nodes = EcoVec::with_capacity(fields.len());
            for field in &fields {
                let mut arg = if let Some(sn) = &field.init {
                    sn.clone()
                } else {
                    self.code_meta
                        .global_references
                        .insert(field.name_span.clone(), field.global_index);
                    SigNode::new(Signature::new(1, 1), Node::empty())
                };
                if boxed {
                    arg.node.push(Node::Label(field.name.clone(), span));
                } else if data.variant {
                    arg.node.push(Node::TrackCaller(
                        Node::ImplPrim(ImplPrimitive::ValidateNonBoxedVariant, field.span).into(),
                    ));
                }
                field_nodes.push(arg);
            }
            let mut node = Node::Array {
                len: fields.len(),
                inner: Node::Mod(Primitive::Bracket, field_nodes, span).into(),
                boxed,
                allow_ext: true,
                prim: None,
                span,
            };
            if data.func.is_some() {
                let size = fields.len();
                node.push(Node::UseArgs { size, span });
            }
            node
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
        let constructor_name = Ident::from("New");
        let constructor_func = self.asm.add_function(
            FunctionId::Named(constructor_name.clone()),
            Signature::new(constructor_args, 1),
            node,
        );
        let constr_local = LocalName {
            index: self.next_global,
            public: true,
        };
        self.next_global += 1;
        let mut constr_comment = match (&def_name, data.func.is_some()) {
            (Some(name), false) => format!("Create a new {name}\n{name} ?"),
            (Some(name), true) => format!("Create a new array of {name}'s args\n{name}Args ?"),
            (None, false) => "Create a new data instance\nInstance ?".into(),
            (None, true) => "Create a new args instance\nArgs ?".into(),
        };
        for field in &fields {
            match field.init.as_ref().map(|sn| sn.sig.args()) {
                Some(0) => {}
                Some(1) | None => {
                    constr_comment.push(' ');
                    constr_comment.push_str(&field.name);
                }
                Some(n) => {
                    for i in 0..n {
                        constr_comment.push(' ');
                        constr_comment.push_str(&field.name);
                        let mut i = i + 1;
                        while i > 0 {
                            constr_comment.push(SUBSCRIPT_DIGITS[i % 10]);
                            i /= 10;
                        }
                    }
                }
            }
        }

        self.scope.has_data_def = true;

        let mut function_stuff = None;
        let mut args_function_stuff = None;
        // Data functions
        if let Some(words) = data.func {
            self.experimental_error(&data.init_span, || {
                "Data functions are experimental. To use them, add \
                `# Experimental!` to the top of the file."
            });
            self.in_scope(ScopeKind::Binding, |comp| {
                let word_span =
                    (words.first().unwrap().span.clone()).merge(words.last().unwrap().span.clone());
                // Forbid variant
                if data.variant {
                    comp.add_error(word_span.clone(), "Variants may not have functions");
                }
                let span = comp.add_span(word_span.clone());
                // Compile function
                let names = fields.iter().map(|field| {
                    let local = LocalName {
                        index: field.global_index,
                        public: false,
                    };
                    (field.name.clone(), local)
                });
                let (_, mut sn) = comp.in_scope(ScopeKind::AllInModule, move |comp| {
                    comp.scope.names.extend(names);
                    comp.words_sig(words)
                })?;

                // Make with args function
                if !fields.iter().any(|field| field.name == "Args") {
                    let mut sn = sn.clone();
                    let size = fields.len();
                    sn.node.prepend(Node::UseArgs { size, span });
                    let local = LocalName {
                        index: comp.next_global,
                        public: true,
                    };
                    comp.next_global += 1;
                    let func =
                        comp.asm
                            .add_function(FunctionId::Named("Args".into()), sn.sig, sn.node);
                    args_function_stuff = Some((local, func, span));
                }

                // Add constructor
                sn.node.prepend(Node::Call(constructor_func.clone(), span));
                sn.sig = sn.sig.compose(Signature::new(constructor_args, 1));
                // Make function
                let local = LocalName {
                    index: comp.next_global,
                    public: true,
                };
                comp.next_global += 1;
                let func = comp.asm.add_function(
                    FunctionId::Named(constructor_name.clone()),
                    sn.sig,
                    sn.node,
                );
                let fields = (fields.iter().enumerate())
                    .map(|(index, field)| {
                        let init_sig = field.init.as_ref().map(|sn| sn.sig);
                        let field_name = &field.name;
                        let comment = match (&def_name, &field.comment) {
                            (Some(def_name), Some(comment)) => {
                                format!("Set {def_name}'s {field_name}\n{comment}")
                            }
                            (Some(def_name), None) => format!("Set {def_name}'s {field_name}"),
                            (None, Some(comment)) => format!("Set {field_name}\n{comment}"),
                            (None, None) => format!("Set {field_name}"),
                        }
                        .into();
                        let info = FieldInfo {
                            index,
                            init_sig,
                            comment,
                        };
                        (field.name.clone(), info)
                    })
                    .collect();

                // Register data function info
                let info = Arc::new(DataFuncInfo {
                    name: def_name.clone().unwrap_or_default(),
                    fields,
                });
                comp.data_function_info.insert(local.index, info.clone());
                comp.data_function_info
                    .insert(constr_local.index, info.clone());
                if let Some((local, ..)) = &args_function_stuff {
                    comp.data_function_info.insert(local.index, info);
                }
                function_stuff = Some((local, func, span));
                Ok(())
            })?;
        }

        // Bind the call function
        if let Some((local, func, span)) = function_stuff {
            let meta = BindingMeta {
                comment: def_comment,
                ..Default::default()
            };
            self.compile_bind_function("Call".into(), local, func, span, meta);
        }
        if let Some((local, func, span)) = args_function_stuff {
            let meta = BindingMeta {
                comment: Some(DocComment::from(if let Some(name) = &def_name {
                    format!("Call {name}'s function from its constructed array")
                } else {
                    "Call the function from its constructed array".into()
                })),
                ..Default::default()
            };
            self.compile_bind_function("Args".into(), local, func, span, meta);
        }

        // Bind the constructor
        let meta = BindingMeta {
            comment: Some(DocComment::from(constr_comment.as_str())),
            ..Default::default()
        };
        self.compile_bind_function(constructor_name, constr_local, constructor_func, span, meta);

        Ok(())
    }
    pub(super) fn end_enum(&mut self) {
        // Add Variants binding
        if !self.scope.data_variants.is_empty()
            && !self.scope.names.get("Variants").is_some_and(|ln| ln.public)
        {
            let index = self.next_global;
            self.next_global += 1;
            let local = LocalName {
                index,
                public: true,
            };
            let value = take(&mut self.scope.data_variants)
                .into_iter()
                .map(|s| Boxed(s.chars().collect()))
                .collect();
            let meta = BindingMeta {
                comment: Some("Names of the data variants of the module".into()),
                ..Default::default()
            };
            self.compile_bind_const("Variants".into(), local, Some(value), 0, meta);
        }
    }
}
