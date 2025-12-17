use super::*;

impl Compiler {
    pub(super) fn import(
        &mut self,
        import: crate::ast::Import,
        prev_com: Option<EcoString>,
    ) -> UiuaResult {
        // Import module
        let module = self.import_module_from_path(&import.path.value, &import.path.span)?;
        // Bind items
        for item in import.items() {
            if let Some(local) = module.names.get_last(item.value.as_str()) {
                self.validate_local(&item.value, local, &item.span);
                (self.code_meta.global_references).insert(item.span.clone(), local.index);
                self.scope.names.insert(
                    item.value.clone(),
                    LocalIndex {
                        index: local.index,
                        public: true,
                    },
                );
            } else {
                self.add_error(
                    item.span.clone(),
                    format!("`{}` not found in module", item.value),
                );
            }
        }
        // Bind name
        if let Some(name) = &import.name {
            let global_index = self.next_global;
            self.next_global += 1;
            let local = LocalIndex {
                index: global_index,
                public: import.public,
            };
            let meta = BindingMeta {
                comment: prev_com
                    .or_else(|| module.comment.clone())
                    .map(|text| DocComment::from(text.as_str())),
                ..Default::default()
            };
            self.asm.add_binding_at(
                local,
                BindingKind::Module(module),
                Some(name.span.clone()),
                meta,
            );
            self.scope.add_module_name(name.value.clone(), local);
        }
        Ok(())
    }
    /// Import a module from a path
    pub(crate) fn import_module_from_path(
        &mut self,
        path_str: &str,
        span: &CodeSpan,
    ) -> UiuaResult<Module> {
        // Resolve path
        let (path, file_kind) = if let Some(url) =
            (path_str.trim().strip_prefix("git:").map(Into::into)).or_else(|| {
                (path_str.trim().strip_prefix("gh:")).map(|s| format!("github.com/{}", s.trim()))
            }) {
            let mut url = url.as_str();
            if url.contains("branch:") && url.contains("commit:") {
                return Err(self.error(
                    span.clone(),
                    "Cannot specify both branch and commit in git import",
                ));
            }
            let target = if let Some((a, b)) = url.split_once("branch:") {
                url = a;
                GitTarget::Branch(b.trim().into())
            } else if let Some((a, b)) = url.split_once("commit:") {
                url = a;
                GitTarget::Commit(b.trim().into())
            } else {
                GitTarget::Default
            };
            // Git import
            let mut url = url.trim().trim_end_matches(".git").to_string();
            if url.ends_with("/uiua") {
                return Err(self.error(span.clone(), "Cannot import what looks like a Uiua fork"));
            }
            if !(url.starts_with("https://") || url.starts_with("http://")) {
                url = format!("https://{url}");
            }
            self.code_meta
                .import_srcs
                .insert(span.clone(), ImportSrc::Git(url.clone()));
            let path = self
                .backend()
                .load_git_module(&url, target)
                .map_err(|e| self.error(span.clone(), e))?;
            (path, FileScopeKind::Git)
        } else {
            // Normal import
            let path = self.resolve_import_path(Path::new(path_str));
            self.code_meta
                .import_srcs
                .insert(span.clone(), ImportSrc::File(path.clone()));
            (path, FileScopeKind::Source)
        };
        // println!("Import path: {}", path.display());
        let module = if let Some(module) = (self.asm.bindings.iter())
            .filter_map(|binfo| binfo.kind.as_module())
            .find(|m| m.path.as_ref().is_some_and(|p| p == &path))
        {
            // println!("  {} already imported", path.display());
            module.clone()
        } else {
            // println!("  {} not yet imported", path.display());
            let bytes = self
                .backend()
                .file_read_all(&path)
                .or_else(|e| {
                    if path.ends_with(Path::new("example.ua")) {
                        Ok(EXAMPLE_UA.as_bytes().to_vec())
                    } else {
                        Err(e)
                    }
                })
                .map_err(|e| self.error(span.clone(), e))?;

            let mut hasher = DefaultHasher::default();
            bytes.hash(&mut hasher);
            let hash = hasher.finish();
            let cache_path = match file_kind {
                FileScopeKind::Source => PathBuf::from(format!(
                    "uiua-modules/cache/{}/{hash:016x}.uasm",
                    path.with_extension("").display()
                )),
                FileScopeKind::Git => {
                    let mut p = PathBuf::from("uiua-modules/cache");
                    if let Some(author) = path.components().nth_back(2) {
                        p = p.join(author);
                    }
                    if let Some(repo) = path.components().nth_back(1) {
                        p = p.join(repo);
                    }
                    p.join(format!("{hash:016x}.uasm"))
                }
            };
            // println!("  Cache path: {}", cache_path.display());
            let asm = if let Some(asm) =
                (self.backend().file_read_all(&cache_path).ok()).and_then(|uasm| {
                    Assembly::from_uasm(&String::from_utf8_lossy(&uasm))
                        .inspect_err(|e| {
                            self.emit_diagnostic(
                                format!("Error loading cached assemebly: {e}"),
                                DiagnosticKind::Warning,
                                span.clone(),
                            )
                        })
                        .ok()
                }) {
                // println!("  Cache Hit!");
                asm
            } else {
                // println!("  Cache Miss!");
                let input: EcoString = String::from_utf8(bytes)
                    .map_err(|e| self.error(span.clone(), format!("Failed to read file: {e}")))?
                    .into();

                if self.current_imports.iter().any(|p| p == &path) {
                    return Err(self.error(
                        span.clone(),
                        format!("Cycle detected importing {}", path.to_string_lossy()),
                    ));
                }

                let mut sub_comp = Compiler::with_backend(self.backend().clone());
                sub_comp.current_imports = self.current_imports.clone();
                sub_comp.mode = self.mode;
                sub_comp.in_scope(ScopeKind::File(file_kind), |comp| {
                    comp.load_str_src(&input, &path).map(drop)
                })?;
                let uasm = sub_comp.asm.to_uasm();
                if let Some(parent) = cache_path.parent() {
                    _ = self.backend().make_dir(parent);
                }
                if let Err(e) = self.backend().file_write_all(&cache_path, uasm.as_bytes()) {
                    self.emit_diagnostic(
                        format!("Unable to cache import: {e}"),
                        DiagnosticKind::Warning,
                        span.clone(),
                    );
                }
                sub_comp.asm
            };
            let mut module = asm.module();
            for local in module.names.0.values_mut().flatten() {
                local.index += self.asm.bindings.len();
            }
            self.import_assembly(asm);
            module
        };
        if module.experimental {
            self.experimental_error(span, || {
                format!(
                    "Module `{path_str}` is experimental. \
                    To use it, add `# Experimental!` to the top of this file."
                )
            });
        }
        Ok(Module {
            path: Some(path),
            ..module
        })
    }
    /// Resolve a declared import path relative to the path of the file that is being executed
    pub(crate) fn resolve_import_path(&self, path: &Path) -> PathBuf {
        let mut target = if let Some(parent) = self.current_imports.last().and_then(|p| p.parent())
        {
            parent.join(path)
        } else {
            path.to_path_buf()
        };
        if !target.exists() && target.extension().is_none() {
            target = target.with_extension("ua");
        }
        let base = Path::new(".");
        if let (Ok(canon_target), Ok(canon_base)) = (target.canonicalize(), base.canonicalize()) {
            pathdiff::diff_paths(canon_target, canon_base).unwrap_or(target)
        } else {
            pathdiff::diff_paths(&target, base).unwrap_or(target)
        }
    }
    fn import_assembly(&mut self, mut asm: Assembly) {
        fn offset_indices(
            node: &mut Node,
            span_offset: usize,
            bind_offset: usize,
            func_offset: usize,
        ) {
            // Offset span
            if let Some(span) = node.span_mut() {
                *span += span_offset;
            }
            // Offset bindings and functions
            match node {
                Node::Call(f, _) => f.index += func_offset,
                Node::CallGlobal(index, _)
                | Node::CallMacro { index, .. }
                | Node::BindGlobal { index, .. } => *index += bind_offset,
                _ => {}
            }
            // Recur
            node.sub_nodes_mut()
                .for_each(|n| offset_indices(n, span_offset, bind_offset, func_offset));
        }
        // Offset root and functions
        let span_offset = self.asm.spans.len();
        let func_offset = self.asm.functions.len();
        let bind_offset = self.asm.bindings.len();
        offset_indices(&mut asm.root, span_offset, bind_offset, func_offset);
        for node in asm.functions.make_mut() {
            offset_indices(node, span_offset, bind_offset, func_offset);
        }
        // Offset bindings
        for binfo in asm.bindings.make_mut() {
            match &mut binfo.kind {
                BindingKind::Func(f) => f.index += func_offset,
                BindingKind::Module(module) => (module.names.0.values_mut().flatten())
                    .for_each(|local| local.index += bind_offset),
                BindingKind::CodeMacro(node) => {
                    offset_indices(node, span_offset, bind_offset, func_offset)
                }
                _ => {}
            }
        }
        // Offset and append index macros
        Arc::make_mut(&mut self.asm.index_macros).extend(
            (Arc::unwrap_or_clone(asm.index_macros).into_iter()).map(|(i, mut mac)| {
                for (_, index) in mac.locals.make_mut() {
                    *index += bind_offset
                }
                (i + bind_offset, mac)
            }),
        );
        // Offset and append code macros
        Arc::make_mut(&mut self.asm.code_macros).extend(
            (Arc::unwrap_or_clone(asm.code_macros).into_iter()).map(|(i, mut mac)| {
                offset_indices(&mut mac.root.node, span_offset, bind_offset, func_offset);
                for local in &mut Arc::make_mut(&mut mac.names).0.values_mut().flatten() {
                    local.index += bind_offset;
                }
                (i + bind_offset, mac)
            }),
        );
        // Append everything else
        self.next_global += asm.bindings.len();
        self.asm.root.extend(asm.root);
        self.asm.spans.extend(asm.spans);
        self.asm.functions.extend(asm.functions);
        self.asm.bindings.extend(asm.bindings);
        self.asm.inputs.files.extend(asm.inputs.files);
        self.asm.test_assert_count += asm.test_assert_count;
    }
}
