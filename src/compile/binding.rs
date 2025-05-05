//! Compiler code for bindings

use crate::BindingMeta;

use super::*;

impl Compiler {
    pub(super) fn binding(&mut self, binding: Binding, prelude: BindingPrelude) -> UiuaResult {
        let public = binding.public;

        let last_word = binding.words.iter().last();

        // If marked external and already bound, don't bind again
        let is_external = prelude.external
            || last_word.is_some_and(|w| {
                matches!(w.value, Word::SemanticComment(SemanticComment::External))
            });
        if is_external
            && self.scopes().any(|sc| {
                sc.names
                    .get(&binding.name.value)
                    .is_some_and(|b| self.asm.bindings[b.index].meta.external)
            })
        {
            return Ok(());
        }

        // Get data def if this is a method
        let mut data_def = None;
        let is_method = if let Some(tilde_span) = binding.tilde_span {
            self.experimental_error(&tilde_span, || {
                "Methods are experimental. To use them, add \
                `# Experimental!` to the top of the file."
            });
            if let Some(def) = self.scope.data_def.clone() {
                let span = self.add_span(tilde_span);
                data_def = Some((def, span));
            } else {
                self.add_error(
                    tilde_span,
                    "Method has no data definition defined before it",
                );
            }
            true
        } else {
            false
        };

        // Create meta
        let comment = prelude
            .comment
            .map(|text| DocComment::from(text.as_str()))
            .or_else(|| {
                last_word.and_then(|w| match &w.value {
                    Word::Comment(c) => Some(DocComment::from(c.as_str())),
                    _ => None,
                })
            });
        let deprecation = prelude.deprecation.or_else(|| {
            last_word.and_then(|w| match &w.value {
                Word::SemanticComment(SemanticComment::Deprecated(s)) => Some(s.clone()),
                _ => None,
            })
        });
        let meta = BindingMeta {
            comment,
            deprecation,
            counts: Some(binding.counts),
            external: false,
        };

        // Alias re-bound imports
        let name = binding.name.value;
        let ident_margs = ident_modifier_args(&name);
        if ident_margs == 0
            && meta.comment.is_none()
            && binding.words.iter().filter(|w| w.value.is_code()).count() == 1
        {
            if let Some(r) = binding.words.iter().find_map(|w| match &w.value {
                Word::Ref(r)
                    if ident_modifier_args(&r.name.value) == 0
                        && !(r.path.is_empty() && r.name.value == name) =>
                {
                    Some(r)
                }
                _ => None,
            }) {
                if let Ok(Some((path_locals, local))) = self.ref_local(r) {
                    let is_noadic_function = match &self.asm.bindings[local.index].kind {
                        BindingKind::Func(f) if f.sig.args() == 0 => true,
                        _ => false,
                    };
                    if !is_noadic_function {
                        self.validate_local(&r.name.value, local, &r.name.span);
                        (self.code_meta.global_references)
                            .insert(binding.name.span.clone(), local.index);
                        for (local, comp) in path_locals.into_iter().zip(&r.path) {
                            (self.code_meta.global_references)
                                .insert(comp.module.span.clone(), local.index);
                        }
                        (self.code_meta.global_references).insert(r.name.span.clone(), local.index);
                        let local = LocalName { public, ..local };
                        self.scope.names.insert(name, local);
                        return Ok(());
                    }
                }
            }
        }

        let span = &binding.name.span;

        let spandex = self.add_span(span.clone());
        let local = LocalName {
            index: self.next_global,
            public,
        };
        self.next_global += 1;

        // Handle macro
        let max_placeholder = max_placeholder(&binding.words);
        if binding.code_macro {
            if is_external {
                self.add_error(span.clone(), "Macros cannot be external");
            }
            if is_method {
                self.add_error(span.clone(), "Methods cannot be macros");
            }
            if max_placeholder.is_some() {
                return Err(self.error(span.clone(), "Code macros may not contain placeholders"));
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
            let node = self.words(binding.words)?;
            let sig = match node.sig() {
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
            self.scope.names.insert(name.clone(), local);
            self.asm.add_binding_at(
                local,
                BindingKind::CodeMacro(node.clone()),
                Some(span.clone()),
                meta,
            );
            let mac = CodeMacro {
                root: SigNode::new(sig, node),
                names: self.scope.names.clone(),
            };
            self.code_macros.insert(local.index, mac);
            return Ok(());
        }
        // Index macro
        match (ident_margs, max_placeholder) {
            (0, None) => {}
            (_, None) => {
                self.add_error(
                    span.clone(),
                    format!(
                        "`{name}`'s name suggests it is a macro, \
                        but it has no placeholders"
                    ),
                );
            }
            (0, Some(_)) => {
                self.add_error(
                    span.clone(),
                    format!(
                        "`{name}` has placeholders, but its name \
                        does not suggest it is a macro"
                    ),
                );
                return Ok(());
            }
            (n, Some(max)) => {
                if max + 1 > n {
                    self.emit_diagnostic(
                        format!(
                            "`{name}`'s name suggest at most ^{}, \
                            but it contains a ^{max}",
                            n - 1
                        ),
                        DiagnosticKind::Warning,
                        span.clone(),
                    );
                }
            }
        }
        if max_placeholder.is_some() || ident_margs > 0 {
            if is_external {
                self.add_error(span.clone(), "Macros cannot be external");
            }
            if is_method {
                self.add_error(span.clone(), "Methods cannot be macros");
            }

            self.scope.names.insert(name.clone(), local);
            self.asm.add_binding_at(
                local,
                BindingKind::IndexMacro(ident_margs),
                Some(span.clone()),
                meta,
            );
            let words = binding.words.clone();
            let mut recursive = false;
            self.analyze_macro_body(&name, &words, false, &mut recursive);
            if recursive {
                self.experimental_error(span, || {
                    "Recursive index macros are experimental. \
                    Add `# Experimental!` to the top of the file to use them."
                });
                if binding.signature.is_none() {
                    self.add_error(
                        span.clone(),
                        "Recursive index macro must have a \
                        signature declared after the ←",
                    );
                }
            }
            let mac = IndexMacro {
                words,
                names: self.scope.names.clone(),
                sig: binding.signature.map(|s| s.value),
                recursive,
            };
            self.index_macros.insert(local.index, mac);
            return Ok(());
        }

        // A non-macro binding

        let is_func = binding
            .words
            .iter()
            .any(|w| matches!(w.value, Word::Func(_)));

        let make_fn = {
            let name = name.clone();
            move |mut node: Node, sig: Signature, comp: &mut Compiler| {
                // Diagnostic for function that doesn't consume its arguments
                if let [Node::Prim(Primitive::Dup, span), rest @ ..] = node.as_slice() {
                    if let Span::Code(dup_span) = comp.get_span(*span) {
                        if let Ok(rest_sig) = nodes_sig(rest) {
                            if rest_sig.args() == sig.args()
                                && rest_sig.outputs() + 1 == sig.outputs()
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
                if prelude.track_caller {
                    node = Node::TrackCaller(node.into());
                }
                if prelude.no_inline {
                    node = Node::NoInline(node.into());
                }
                comp.asm.add_function(FunctionId::Named(name), sig, node)
            }
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
        let already_in_binding = !self.current_bindings.is_empty();
        self.current_bindings.push(CurrentBinding {
            name: name.clone(),
            signature: binding.signature.as_ref().map(|s| s.value),
            recurses: 0,
            global_index: local.index,
        });
        let no_code_words = binding.words.iter().all(|w| !w.value.is_code());
        let compile = |comp: &mut Compiler| -> UiuaResult<Node> {
            // Compile the words
            let node = comp.line(binding.words, false);
            // Add an error binding if there was an error
            let mut node = match node {
                Ok(node) => node,
                Err(e) => {
                    comp.asm.add_binding_at(
                        local,
                        BindingKind::Error,
                        Some(span.clone()),
                        meta.clone(),
                    );
                    return Err(e);
                }
            };
            // Apply the signature comment
            if let Some(comment_sig) = meta.comment.as_ref().and_then(|c| c.sig.as_ref()) {
                comp.apply_node_comment(
                    &mut node,
                    comment_sig,
                    &format!("{name}'s"),
                    &binding.name.span,
                );
            }
            Ok(node)
        };
        // We may need to compile the words in the context of a data definition method
        let mut node = if let Some((def, _)) = &data_def {
            self.in_method(def, compile)?
        } else {
            compile(self)?
        };
        let self_referenced = self.current_bindings.pop().unwrap().recurses > 0;
        let is_obverse = node
            .iter()
            .any(|n| matches!(n, Node::CustomInverse(cust, _) if cust.is_obverse));

        // Normalize external
        if is_external {
            if node.is_empty() {
                let Some(sig) = &binding.signature else {
                    return Err(self.error(
                        span.clone(),
                        "Empty external functions must have a signature declared",
                    ));
                };
                let sig = sig.value;
                let span = self.add_span(span.clone());
                let zero = Value::from(0);
                for _ in 0..sig.args() {
                    node.push(Node::Prim(Primitive::Pop, span));
                }
                for _ in 0..sig.outputs() {
                    node.push(Node::Push(zero.clone()));
                }
                node.prepend(Node::Prim(Primitive::Assert, span));
                node.prepend(Node::new_push("Unbound external function"));
                node.prepend(Node::Push(zero));
            } else {
                node = Node::NoInline(node.into());
            }
            self.externals
                .insert(name.clone(), self.asm.functions.len());
        }

        // Resolve signature
        match node.sig() {
            Ok(mut sig) => {
                let binds_above =
                    !already_in_binding && node.is_empty() && no_code_words && !is_method;
                if !binds_above {
                    // Validate signature
                    if let Some(declared_sig) = &binding.signature {
                        node = self.force_sig(node, declared_sig.value, &declared_sig.span);
                        sig = declared_sig.value;
                    }
                }

                // Add data def local wrapper
                if let Some((def, span)) = data_def {
                    node = Node::WithLocal {
                        def: def.def_index,
                        inner: SigNode::new(sig, node).into(),
                        span,
                    };
                    sig.update_args(|a| a + 1);
                }

                if sig == (0, 1)
                    && !self_referenced
                    && !is_func
                    && !is_obverse
                    && !is_method
                    && !is_external
                {
                    // Binding is a constant
                    let val = if let [Node::Push(v)] = node.as_slice() {
                        Some(v.clone())
                    } else if node.is_pure(Purity::Pure, &self.asm) {
                        match self.comptime_node(&node) {
                            Ok(Some(vals)) => vals.into_iter().next(),
                            Ok(None) => None,
                            Err(e) => {
                                self.errors.push(e);
                                None
                            }
                        }
                    } else {
                        None
                    };

                    let is_const = val.is_some();
                    self.compile_bind_const(name, local, val, spandex, meta);
                    if !is_const {
                        // Add binding instrs to unevaluated constants
                        if node.is_pure(Purity::Pure, &self.asm) {
                            self.macro_env
                                .rt
                                .unevaluated_constants
                                .insert(local.index, node.clone());
                        }
                        // Add binding instrs to root
                        self.asm.root.push(node);
                        self.asm.root.push(Node::BindGlobal {
                            index: local.index,
                            span: spandex,
                        });
                    }
                } else if binds_above {
                    // Binding binds the value above
                    let mut has_stack_value = false;
                    for i in 0..self.asm.root.len() {
                        let nodes = &self.asm.root[self.asm.root.len() - 1 - i..];
                        let Ok(sig) = nodes_sig(nodes) else {
                            break;
                        };
                        if sig.outputs() > 0 {
                            has_stack_value = true;
                            break;
                        }
                    }
                    if has_stack_value {
                        sig = Signature::new(0, 1);
                    }
                    if let Some(Node::Push(val)) = self.asm.root.last() {
                        // Actually binds the constant
                        let val = val.clone();
                        self.asm.root.pop();
                        self.compile_bind_const(name, local, Some(val), spandex, meta);
                    } else if sig == (0, 0) {
                        // Empty function
                        let mut node = Node::empty();
                        // Validate signature
                        if let Some(declared_sig) = &binding.signature {
                            node = self.force_sig(node, declared_sig.value, &declared_sig.span);
                            sig = declared_sig.value;
                        }
                        let func = make_fn(node, sig, self);
                        self.compile_bind_function(name, local, func, spandex, meta)?;
                    } else {
                        // Binds some |0.1 code
                        self.compile_bind_const(name, local, None, spandex, meta);
                        self.asm.root.push(Node::BindGlobal {
                            index: local.index,
                            span: spandex,
                        });
                    }
                } else {
                    // Binding is a normal function
                    let func = make_fn(node, sig, self);
                    self.compile_bind_function(name, local, func, spandex, meta)?;
                }

                self.code_meta.function_sigs.insert(
                    words_span,
                    SigDecl {
                        sig,
                        explicit: binding.signature.is_some(),
                        inline: false,
                        set_inverses: Default::default(),
                    },
                );
            }
            Err(e) => self.add_error(
                binding.name.span.clone(),
                format!("Cannot infer function signature: {e}"),
            ),
        }
        Ok(())
    }
    pub(super) fn module(&mut self, m: Sp<ScopedModule>, prelude: BindingPrelude) -> UiuaResult {
        let m = m.value;
        let (scope_kind, name_and_local) = match m.kind {
            ModuleKind::Named(name) => {
                let global_index = self.next_global;
                self.next_global += 1;
                let local = LocalName {
                    index: global_index,
                    public: true,
                };
                let meta = BindingMeta {
                    comment: prelude.comment.as_deref().map(DocComment::from),
                    deprecation: prelude.deprecation.clone(),
                    ..Default::default()
                };
                self.asm.add_binding_at(
                    local,
                    BindingKind::Scope(self.higher_scopes.len() + 1),
                    Some(name.span.clone()),
                    meta,
                );
                // Add local
                self.scope.add_module_name(name.value.clone(), local);
                (self.code_meta.global_references).insert(name.span.clone(), local.index);
                (ScopeKind::Module(name.value.clone()), Some((name, local)))
            }
            ModuleKind::Test => (ScopeKind::Test, None),
        };
        let (module, ()) = self.in_scope(scope_kind, |comp| {
            comp.items(m.items, ItemCompMode::TopLevel)?;
            comp.end_enum()?;
            Ok(())
        })?;
        if let Some((name, local)) = name_and_local {
            // Named module
            // Add imports
            if let Some(line) = m.imports {
                for item in line.items {
                    if let Some(mut local) = module.names.get(&item.value).copied() {
                        local.public = false;
                        (self.code_meta.global_references).insert(item.span.clone(), local.index);
                        self.scope.names.insert(item.value, local);
                    } else {
                        self.add_error(
                            item.span.clone(),
                            format!("{} does not exist in {}", item.value, name.value),
                        );
                    }
                }
            }
            // Update global
            self.asm.bindings.make_mut()[local.index].kind = BindingKind::Module(module);
        } else {
            // Test module
            if let Some(line) = &m.imports {
                self.add_error(
                    line.tilde_span.clone(),
                    "Items cannot be imported from test modules",
                );
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
                BindingMeta {
                    comment: prev_com
                        .or_else(|| imported.comment.clone())
                        .map(|text| DocComment::from(text.as_str())),
                    ..Default::default()
                },
            );
            self.scope.add_module_name(name.value.clone(), local);
        }
        // Bind items
        for item in import.items() {
            if let Some(local) = (self.imports.get(&module_path))
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
    fn analyze_macro_body(
        &mut self,
        macro_name: &str,
        words: &[Sp<Word>],
        mut code_macro: bool,
        recursive: &mut bool,
    ) {
        for word in words {
            let mut path_locals = None;
            let mut name_local = None;
            match &word.value {
                Word::Strand(items) => {
                    self.analyze_macro_body(macro_name, items, code_macro, recursive)
                }
                Word::Array(arr) => arr.word_lines().for_each(|line| {
                    self.analyze_macro_body(macro_name, line, code_macro, recursive);
                }),
                Word::Func(func) => func.word_lines().for_each(|line| {
                    self.analyze_macro_body(macro_name, line, code_macro, recursive);
                }),
                Word::Pack(pack) => pack.branches.iter().for_each(|branch| {
                    (branch.value.word_lines()).for_each(|line| {
                        self.analyze_macro_body(macro_name, line, code_macro, recursive)
                    })
                }),
                Word::Ref(r) => match self.ref_local(r) {
                    Ok(Some((pl, l))) => {
                        path_locals = Some((&r.path, pl));
                        name_local = Some((&r.name, l));
                    }
                    Ok(None) => {}
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
                            Ok(Some((pl, l))) => {
                                path_locals = Some((&r.path, pl));
                                name_local = Some((&r.name, l));
                                code_macro |= self.code_macros.contains_key(&l.index);
                            }
                            Ok(None) => {}
                            Err(e) => self.errors.push(e),
                        }
                        if let Some(BindingKind::Module(module)) = name_local
                            .as_ref()
                            .and_then(|(_, local)| self.asm.bindings.get(local.index))
                            .map(|b| &b.kind)
                        {
                            let names = module.names.clone();
                            let recursive = &mut *recursive;
                            if let Err(e) = self.in_scope(ScopeKind::AllInModule, move |comp| {
                                comp.scope.names.extend(names);
                                comp.analyze_macro_body(macro_name, &m.operands, false, recursive);
                                Ok(())
                            }) {
                                self.errors.push(e);
                            }
                        } else {
                            // Name errors are ignored in code macros
                            let error_count = self.errors.len();
                            self.analyze_macro_body(macro_name, &m.operands, code_macro, recursive);
                            if code_macro {
                                self.errors.truncate(error_count);
                            }
                        }
                    } else {
                        self.analyze_macro_body(macro_name, &m.operands, code_macro, recursive)
                    }
                }
                _ => {}
            }
            if let Some((nm, local)) = name_local {
                if nm.value == macro_name
                    && path_locals.as_ref().is_none_or(|(pl, _)| pl.is_empty())
                {
                    *recursive = true;
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
        }
    }
}
