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
                Word::Ref(r)
                    if ident_modifier_args(&r.name.value) == 0
                        && !(r.path.is_empty() && r.name.value == binding.name.value) =>
                {
                    Some(r)
                }
                _ => None,
            }) {
                if let Ok(Some((path_locals, local))) = self.ref_local(r) {
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

        // Handle macro
        let ident_margs = ident_modifier_args(&name);
        let max_placeholder = max_placeholder(&binding.words);
        if binding.code_macro {
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
                comment.map(|text| DocComment::from(text.as_str())),
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
            self.scope.names.insert(name.clone(), local);
            self.asm.add_binding_at(
                local,
                BindingKind::IndexMacro(ident_margs),
                Some(span.clone()),
                comment.map(|text| DocComment::from(text.as_str())),
            );
            let words = binding.words.clone();
            let mut recursive = false;
            self.analyze_macro_body(&name, &words, &mut recursive);
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
                hygenic: true,
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
                            if rest_sig.args == sig.args && rest_sig.outputs + 1 == sig.outputs {
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
        self.current_bindings.push(CurrentBinding {
            name: name.clone(),
            signature: binding.signature.as_ref().map(|s| s.value),
            recurses: 0,
            global_index: local.index,
        });
        let no_code_words = binding.words.iter().all(|w| !w.value.is_code());
        let node = self.words(binding.words);
        let self_referenced = self.current_bindings.pop().unwrap().recurses > 0;
        let node = node?;

        // Resolve signature
        match node.sig() {
            Ok(mut sig) => {
                if sig == (0, 1) && !self_referenced && !is_func {
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
                    self.compile_bind_const(name, local, val, spandex, comment.as_deref());
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
                } else if node.is_empty() && no_code_words {
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
                    if let Some(Node::Push(val)) = self.asm.root.last() {
                        let val = val.clone();
                        self.asm.root.pop();
                        self.compile_bind_const(
                            name,
                            local,
                            Some(val),
                            spandex,
                            comment.as_deref(),
                        );
                    } else if sig == (0, 0) {
                        let func = make_fn(Node::empty(), sig, self);
                        self.compile_bind_function(name, local, func, spandex, comment.as_deref())?;
                    } else {
                        self.compile_bind_const(name, local, None, spandex, comment.as_deref());
                        self.asm.root.push(Node::BindGlobal {
                            index: local.index,
                            span: spandex,
                        });
                    }
                } else {
                    // Binding is a normal function
                    let func = make_fn(node, sig, self);
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
        let (module, ()) = self.in_scope(scope_kind, |comp| {
            comp.items(m.items, false)?;
            comp.end_enum()?;
            Ok(())
        })?;
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
    fn analyze_macro_body(&mut self, macro_name: &str, words: &[Sp<Word>], recursive: &mut bool) {
        for word in words {
            let mut path_locals = None;
            let mut name_local = None;
            match &word.value {
                Word::Strand(items) => self.analyze_macro_body(macro_name, items, recursive),
                Word::Array(arr) => arr.lines.iter().for_each(|line| {
                    self.analyze_macro_body(macro_name, line, recursive);
                }),
                Word::Func(func) => func.lines.iter().for_each(|line| {
                    self.analyze_macro_body(macro_name, line, recursive);
                }),
                Word::Pack(pack) => pack.branches.iter().for_each(|branch| {
                    (branch.value.lines.iter())
                        .for_each(|line| self.analyze_macro_body(macro_name, line, recursive))
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
                                comp.analyze_macro_body(macro_name, &m.operands, recursive);
                                Ok(())
                            }) {
                                self.errors.push(e);
                            }
                        } else {
                            self.analyze_macro_body(macro_name, &m.operands, recursive)
                        }
                    } else {
                        self.analyze_macro_body(macro_name, &m.operands, recursive)
                    }
                }
                _ => {}
            }
            if let Some((nm, local)) = name_local {
                if nm.value == macro_name
                    && path_locals.as_ref().map_or(true, |(pl, _)| pl.is_empty())
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
