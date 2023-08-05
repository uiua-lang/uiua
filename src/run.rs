use std::{
    collections::{HashMap, HashSet},
    fs,
    mem::take,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    ast::*,
    function::*,
    lex::{CodeSpan, Sp, Span},
    parse::parse,
    primitive::Primitive,
    value::Value,
    Ident, NativeSys, SysBackend, SysOp, TraceFrame, UiuaError, UiuaResult,
};

/// The Uiua runtime
#[derive(Clone)]
pub struct Uiua<'io> {
    // Compilation
    new_functions: Vec<Vec<Instr>>,
    new_dfns: Vec<Vec<u8>>,
    // Statics
    globals: Vec<Value>,
    spans: Vec<Span>,
    // Runtime
    stack: Vec<Value>,
    scope: Scope,
    lower_scopes: Vec<Scope>,
    mode: RunMode,
    // IO
    current_imports: HashSet<PathBuf>,
    imports: HashMap<PathBuf, Vec<Value>>,
    pub(crate) io: &'io dyn SysBackend,
}

#[derive(Default, Clone)]
pub struct Scope {
    array: Vec<usize>,
    dfn: Vec<DfnFrame>,
    call: Vec<StackFrame>,
    names: HashMap<Ident, usize>,
    local: bool,
}

#[derive(Clone)]
struct StackFrame {
    function: Rc<Function>,
    call_span: usize,
    pc: usize,
    spans: Vec<(usize, Option<Primitive>)>,
    dfn: bool,
}

#[derive(Clone)]
struct DfnFrame {
    function: Rc<Function>,
    args: Vec<Value>,
}

impl<'io> Default for Uiua<'io> {
    fn default() -> Self {
        Self::with_native_sys()
    }
}

/// A mode that affects how non-binding lines are run
///
/// Regardless of the mode, lines with a call to `import` will always be run
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RunMode {
    /// Only run lines outside of test blocks
    Normal,
    /// Only run non-binding lines inside of test blocks
    Test,
    /// Run everything
    Watch,
}

impl<'io> Uiua<'io> {
    /// Create a new Uiua runtime with the standard IO backend
    pub fn with_native_sys() -> Self {
        Uiua {
            spans: vec![Span::Builtin],
            stack: Vec::new(),
            scope: Scope::default(),
            lower_scopes: Vec::new(),
            globals: Vec::new(),
            new_functions: Vec::new(),
            new_dfns: Vec::new(),
            current_imports: HashSet::new(),
            imports: HashMap::new(),
            mode: RunMode::Normal,
            io: &NativeSys,
        }
    }
    /// Create a new Uiua runtime with a custom IO backend
    pub fn with_backend(io: &'io dyn SysBackend) -> Self {
        Uiua {
            io,
            ..Default::default()
        }
    }
    /// Set the [`RunMode`]
    ///
    /// Default is [`RunMode::Normal`]
    pub fn mode(mut self, mode: RunMode) -> Self {
        self.mode = mode;
        self
    }
    /// Load a Uiua file from a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<&mut Self> {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        self.load_impl(&input, Some(path))
    }
    /// Load a Uiua file from a string
    pub fn load_str(&mut self, input: &str) -> UiuaResult<&mut Self> {
        self.load_impl(input, None)
    }
    /// Load a Uiua file from a string with a path for error reporting
    pub fn load_str_path<P: AsRef<Path>>(&mut self, input: &str, path: P) -> UiuaResult<&mut Self> {
        self.load_impl(input, Some(path.as_ref()))
    }
    /// Run in a scoped context. Names defined in this context will be removed when the scope ends.
    ///
    /// While names defined in this context will be removed when the scope ends, values *bound* to
    /// those names will not.
    ///
    /// All other runtime state other than the stack, will also be restored.
    pub fn in_scope<T>(
        &mut self,
        local: bool,
        f: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<Vec<Value>> {
        self.lower_scopes.push(take(&mut self.scope));
        self.scope.local = local;
        let start_height = self.stack.len();
        f(self)?;
        let end_height = self.stack.len();
        self.scope = self.lower_scopes.pop().unwrap();
        Ok(self.stack.split_off(start_height.min(end_height)))
    }
    fn load_impl(&mut self, input: &str, path: Option<&Path>) -> UiuaResult<&mut Self> {
        let (items, errors) = parse(input, path);
        if !errors.is_empty() {
            return Err(errors.into());
        }
        if let Some(path) = path {
            self.current_imports.insert(path.into());
        }
        let res = self.items(items, false);
        if let Some(path) = path {
            self.current_imports.remove(path);
        }
        res.map(|_| self)
    }
    fn trace_error(&self, mut error: UiuaError, frame: StackFrame) -> UiuaError {
        let mut frames = Vec::new();
        for (span, prim) in &frame.spans {
            if let Some(prim) = prim {
                frames.push(TraceFrame {
                    id: FunctionId::Primitive(*prim),
                    span: self.spans[*span].clone(),
                });
            }
        }
        frames.push(TraceFrame {
            id: frame.function.id.clone(),
            span: self.spans[frame.call_span].clone(),
        });
        if let UiuaError::Traced { trace, .. } = &mut error {
            trace.extend(frames);
            error
        } else {
            UiuaError::Traced {
                error: error.into(),
                trace: frames,
            }
        }
    }
    pub(crate) fn import(&mut self, input: &str, path: &Path) -> UiuaResult {
        if self.current_imports.contains(path) {
            return Err(self.error(format!(
                "cycle detected importing {}",
                path.to_string_lossy()
            )));
        }
        if !self.imports.contains_key(path) {
            let import = self.in_scope(false, |env| env.load_str_path(input, path).map(drop))?;
            self.imports.insert(path.into(), import);
        }
        self.stack.extend(self.imports[path].iter().cloned());
        Ok(())
    }
    fn items(&mut self, items: Vec<Item>, in_test: bool) -> UiuaResult {
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
            Item::Scoped { items, test } => {
                let scope_stack = self.in_scope(true, |env| env.items(items, test))?;
                self.stack.extend(scope_stack);
            }
            Item::Words(words) => {
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::Test => in_test,
                    RunMode::Watch => true,
                };
                if can_run || words_have_import(&words) {
                    let instrs = self.compile_words(words)?;
                    self.exec_global_instrs(instrs)?;
                }
            }
            Item::Binding(binding) => {
                let can_run = match self.mode {
                    RunMode::Normal | RunMode::Watch => true,
                    RunMode::Test => in_test,
                };
                if can_run || words_have_import(&binding.words) {
                    self.binding(binding)?;
                }
            }
            Item::Newlines => {}
        }
        Ok(())
    }
    fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let idx = self.spans.len();
        self.spans.push(span.into());
        idx
    }
    fn binding(&mut self, binding: Binding) -> UiuaResult {
        let val = if binding.name.value.is_functiony() {
            let instrs = self.compile_words(binding.words)?;
            let func = Function {
                id: FunctionId::Named(binding.name.value.clone()),
                instrs,
                kind: FunctionKind::Normal,
            };
            Value::from(func)
        } else {
            let instrs = self.compile_words(binding.words)?;
            self.exec_global_instrs(instrs)?;
            self.stack.pop().unwrap_or_default()
        };
        let idx = self.globals.len();
        self.globals.push(val);
        self.scope.names.insert(binding.name.value, idx);
        Ok(())
    }
    fn compile_words(&mut self, words: Vec<Sp<Word>>) -> UiuaResult<Vec<Instr>> {
        self.new_functions.push(Vec::new());
        self.words(words, true)?;
        let instrs = self.new_functions.pop().unwrap();
        Ok(instrs)
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        for word in words.into_iter().rev() {
            self.word(word, call)?;
        }
        Ok(())
    }
    fn push_instr(&mut self, instr: Instr) {
        use Primitive::*;
        let instrs = self.new_functions.last_mut().unwrap();
        // Optimizations
        match (instrs.as_mut_slice(), instr) {
            // Call pick = if
            (
                [.., Instr::BeginArray, Instr::Push(if_true), Instr::Push(if_false), Instr::EndArray(_), Instr::Prim(Flip, _), Instr::Prim(Pick, _)],
                Instr::Prim(Call, span),
            ) => {
                if let (Value::Func(if_true), Value::Func(if_false)) = (&*if_true, &*if_false) {
                    if if_true.shape.is_empty() && if_false.shape.is_empty() {
                        let if_true = if_true.data[0].clone();
                        let if_false = if_false.data[0].clone();
                        for _ in 0..6 {
                            instrs.pop().unwrap();
                        }
                        instrs.push(Instr::If(if_true, if_false));
                        return;
                    }
                }
                instrs.push(Instr::Prim(Call, span))
            }
            // Cosine
            ([.., Instr::Prim(Eta, _), Instr::Prim(Add, _)], Instr::Prim(Sin, span)) => {
                instrs.pop();
                instrs.pop();
                instrs.push(Instr::Prim(Cos, span));
            }
            // First reverse = last
            ([.., Instr::Prim(top, _)], Instr::Prim(new, new_span)) => match (&top, new) {
                (Reverse, First) => *top = Last,
                (Reverse, Last) => *top = First,
                (a, b)
                    if a.args() == a.outputs()
                        && b.args() == b.outputs()
                        && a.inverse() == Some(b) =>
                {
                    instrs.pop();
                }
                _ => instrs.push(Instr::Prim(new, new_span)),
            },
            // Ignore noops
            (_, Instr::Prim(Noop, _)) => {}
            (_, instr) => instrs.push(instr),
        }
    }
    fn word(&mut self, word: Sp<Word>, call: bool) -> UiuaResult {
        match word.value {
            Word::Number(_, n) => {
                self.push_instr(Instr::Push(n.into()));
            }
            Word::Char(c) => self.push_instr(Instr::Push(c.into())),
            Word::String(s) => self.push_instr(Instr::Push(s.into())),
            Word::FormatString(frags) => {
                let f = Function {
                    id: FunctionId::Anonymous(word.span.clone()),
                    instrs: Vec::new(),
                    kind: FunctionKind::Dynamic(Rc::new(move |env| {
                        let mut formatted = String::new();
                        for (i, frag) in frags.iter().enumerate() {
                            if i > 0 {
                                let val = env.pop(format!("format argument {i}"))?;
                                formatted.push_str(&format!("{}", val));
                            }
                            formatted.push_str(frag);
                        }
                        env.push(formatted);
                        Ok(())
                    })),
                };
                self.push_instr(Instr::Push(f.into()));
                let span = self.add_span(word.span);
                self.push_instr(Instr::Call(span));
            }
            Word::MultilineString(lines) => {
                let f = Function {
                    id: FunctionId::Anonymous(word.span.clone()),
                    instrs: Vec::new(),
                    kind: FunctionKind::Dynamic(Rc::new(move |env| {
                        let mut formatted = String::new();
                        let mut i = 0;
                        for (j, line) in lines.iter().enumerate() {
                            if j > 0 {
                                formatted.push_str("\r\n");
                            }
                            for (k, frag) in line.value.iter().enumerate() {
                                if k > 0 {
                                    let val = env.pop(format!("format argument {i}"))?;
                                    formatted.push_str(&format!("{}", val));
                                }
                                formatted.push_str(frag);
                                i += 1;
                            }
                        }
                        env.push(formatted);
                        Ok(())
                    })),
                };
                self.push_instr(Instr::Push(f.into()));
                let span = self.add_span(word.span);
                self.push_instr(Instr::Call(span));
            }
            Word::Ident(ident) => self.ident(ident, word.span, call)?,
            Word::Strand(items) => {
                self.push_instr(Instr::BeginArray);
                self.words(items, false)?;
                let span = self.add_span(word.span);
                self.push_instr(Instr::EndArray(span));
            }
            Word::Array(items) => {
                self.push_instr(Instr::BeginArray);
                for item in items {
                    self.words(item, true)?;
                }
                let span = self.add_span(word.span);
                self.push_instr(Instr::EndArray(span));
            }
            Word::Func(func) => self.func(func, word.span)?,
            Word::Dfn(func) => self.dfn(func, word.span, call)?,
            Word::Primitive(p) => self.primitive(p, word.span, call),
            Word::Modified(m) => self.modified(*m, call)?,
            Word::Spaces | Word::Comment(_) => {}
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool) -> UiuaResult {
        if let Some(idx) = self.scope.names.get(&ident).or_else(|| {
            self.lower_scopes
                .last()
                .filter(|_| self.scope.local)?
                .names
                .get(&ident)
        }) {
            // Name exists in scope
            let value = self.globals[*idx].clone();
            let is_function = matches!(value, Value::Func(_));
            self.push_instr(Instr::Push(value));
            if is_function && call {
                let span = self.add_span(span);
                self.push_instr(Instr::Call(span));
            }
        } else if let Some(prims) = Primitive::from_format_name_multi(ident.as_str()) {
            // Name is a formattable primitive
            for (prim, _) in prims.into_iter().rev() {
                self.primitive(prim, span.clone(), call);
            }
        } else if let Some(prim) =
            Primitive::all().find(|p| p.name().is_some_and(|name| ident == name))
        {
            // Name is a non-formattable primitive
            self.primitive(prim, span.clone(), call);
        } else {
            if let Some(dfn) = self.new_dfns.last_mut() {
                if ident.as_str().len() == 1 {
                    let c = ident.as_str().chars().next().unwrap();
                    if c.is_ascii_lowercase() {
                        // Name is a dfn argument
                        let idx = c as u8 - b'a';
                        dfn.push(idx);
                        self.push_instr(Instr::DfnVal(idx as usize));
                        return Ok(());
                    }
                }
            }
            return Err(span.sp(format!("unknown identifier {}", ident)).into());
        }
        Ok(())
    }
    fn func(&mut self, func: Func, _span: CodeSpan) -> UiuaResult {
        let mut instrs = Vec::new();
        for line in func.body {
            instrs.extend(self.compile_words(line)?);
        }
        if let [Instr::Push(f), Instr::Call(..)] = instrs.as_slice() {
            if matches!(f, Value::Func(_)) {
                self.push_instr(Instr::Push(f.clone()));
                return Ok(());
            }
        }
        let func = Function {
            id: func.id,
            instrs,
            kind: FunctionKind::Normal,
        };
        self.push_instr(Instr::Push(func.into()));
        Ok(())
    }
    fn dfn(&mut self, func: Func, span: CodeSpan, call: bool) -> UiuaResult {
        self.new_dfns.push(Vec::new());
        let mut instrs = Vec::new();
        for line in func.body {
            instrs.extend(self.compile_words(line)?);
        }
        let refs = self.new_dfns.pop().unwrap();
        let span = self.add_span(span);
        let dfn_size = refs.into_iter().max().map(|n| n + 1).unwrap_or(0);
        let func = Function {
            id: func.id,
            instrs,
            kind: FunctionKind::Dfn(dfn_size),
        };
        self.push_instr(Instr::Push(func.into()));
        if call {
            self.push_instr(Instr::Call(span));
        }
        Ok(())
    }
    fn modified(&mut self, modified: Modified, call: bool) -> UiuaResult {
        self.new_functions.push(Vec::new());
        self.words(modified.words, false)?;
        self.primitive(
            modified.modifier.value,
            modified.modifier.span.clone(),
            true,
        );
        let instrs = self.new_functions.pop().unwrap();
        let func = Function {
            id: FunctionId::Anonymous(modified.modifier.span.clone()),
            instrs,
            kind: FunctionKind::Normal,
        };
        self.push_instr(Instr::Push(func.into()));
        if call {
            let span = self.add_span(modified.modifier.span);
            self.push_instr(Instr::Call(span));
        }
        Ok(())
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) {
        let span = self.add_span(span);
        if call {
            self.push_instr(Instr::Prim(prim, span));
        } else {
            self.push_instr(Instr::Push(Value::from(Function {
                id: FunctionId::Primitive(prim),
                instrs: vec![Instr::Prim(prim, span)],
                kind: FunctionKind::Normal,
            })))
        }
    }
    fn exec_global_instrs(&mut self, instrs: Vec<Instr>) -> UiuaResult {
        let func = Function {
            id: FunctionId::Main,
            instrs,
            kind: FunctionKind::Normal,
        };
        self.exec(StackFrame {
            function: Rc::new(func),
            call_span: 0,
            spans: Vec::new(),
            pc: 0,
            dfn: false,
        })
    }
    fn exec(&mut self, frame: StackFrame) -> UiuaResult {
        let ret_height = self.scope.call.len();
        self.scope.call.push(frame);
        while self.scope.call.len() > ret_height {
            let frame = self.scope.call.last().unwrap();
            let Some(instr) = frame.function.instrs.get(frame.pc) else {
                if let Some(frame) = self.scope.call.pop() {
                    if frame.dfn {
                        self.scope.dfn.pop();
                    }
                }
                continue;
            };
            // println!("{:?}", self.stack);
            // println!("  {:?}", instr);
            let res = match instr {
                Instr::Push(val) => {
                    self.stack.push(val.clone());
                    Ok(())
                }
                Instr::BeginArray => {
                    self.scope.array.push(self.stack.len());
                    Ok(())
                }
                &Instr::EndArray(span) => (|| {
                    let start = self.scope.array.pop().unwrap();
                    if start > self.stack.len() {
                        return Err(self.error("Array removed elements"));
                    }
                    let values: Vec<_> = self.stack.drain(start..).rev().collect();
                    self.push_span(span, None);
                    let val = Value::from_row_values(values, self)?;
                    self.pop_span();
                    self.push(val);
                    Ok(())
                })(),
                &Instr::Prim(prim, span) => (|| {
                    self.push_span(span, Some(prim));
                    prim.run(self)?;
                    self.pop_span();
                    Ok(())
                })(),
                &Instr::Call(span) => self.call_with_span(span),
                Instr::DfnVal(n) => {
                    if let Some(dfn) = self.scope.dfn.last() {
                        if let Some(value) = dfn.args.get(*n).cloned() {
                            self.stack.push(value);
                            Ok(())
                        } else {
                            Err(self.error(format!(
                                "Function references dfn argument `{}` outside of its dfn",
                                (*n as u8 + b'a') as char
                            )))
                        }
                    } else {
                        Err(self.error(format!(
                            "Function references dfn argument `{}` outside of a dfn",
                            (*n as u8 + b'a') as char
                        )))
                    }
                }
                Instr::If(if_true, if_false) => {
                    let if_true = if_true.clone();
                    let if_false = if_false.clone();
                    (|| {
                        let value = self.pop(2)?;
                        let condition = value.as_nat(self, "Index must be a list of integers")?;
                        match condition {
                            0 => {
                                self.push(if_false);
                                self.call()?;
                            }
                            1 => {
                                self.push(if_true);
                                self.call()?;
                            }
                            n => {
                                return Err(self.error(format!(
                                "Index {n} is out of bounds of length 2 (dimension 1) in shape [2]"
                            )))
                            }
                        }
                        Ok(())
                    })()
                }
            };
            if let Err(mut err) = res {
                // Trace errors
                let frames = self.scope.call.split_off(ret_height);
                for frame in frames {
                    err = self.trace_error(err, frame);
                }
                return Err(err);
            } else {
                // Go to next instruction
                self.scope.call.last_mut().unwrap().pc += 1;
            }
        }
        Ok(())
    }
    fn push_span(&mut self, span: usize, prim: Option<Primitive>) {
        self.scope.call.last_mut().unwrap().spans.push((span, prim));
    }
    fn pop_span(&mut self) {
        self.scope.call.last_mut().unwrap().spans.pop();
    }
    fn call_with_span(&mut self, call_span: usize) -> UiuaResult {
        let value = self.pop("called function")?;
        match value {
            Value::Func(f) if f.shape.is_empty() => {
                let f = f.into_scalar().unwrap();
                let mut dfn = false;
                match &f.kind {
                    FunctionKind::Normal => {}
                    FunctionKind::Dfn(n) => {
                        let n = *n as usize;
                        if self.stack.len() < n {
                            return Err(self.spans[call_span]
                                .clone()
                                .sp(format!("not enough arguments for dfn of {n} values"))
                                .into());
                        }
                        let args = self.stack.drain(self.stack.len() - n..).rev().collect();
                        self.scope.dfn.push(DfnFrame {
                            function: f.clone(),
                            args,
                        });
                        dfn = true;
                    }
                    FunctionKind::Dynamic(ff) => {
                        let new_frame = StackFrame {
                            function: f.clone(),
                            call_span,
                            spans: Vec::new(),
                            pc: 0,
                            dfn: false,
                        };
                        self.scope.call.push(new_frame);
                        ff(self)?;
                        self.scope.call.pop();
                        return Ok(());
                    }
                }
                let new_frame = StackFrame {
                    function: f,
                    call_span,
                    spans: Vec::new(),
                    pc: 0,
                    dfn,
                };
                self.exec(new_frame)
            }
            value => {
                self.pop("replaced value")?;
                self.push(value);
                Ok(())
            }
        }
    }
    /// Call the top of the stack as a function
    pub fn call(&mut self) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_span(call_span)
    }
    pub fn recur(&mut self) -> UiuaResult {
        let dfn = self
            .scope
            .dfn
            .last()
            .ok_or_else(|| self.error("Cannot recur outside of dfn"))?;
        self.push(dfn.function.clone());
        self.call()
    }
    pub fn call_catch_break(&mut self) -> UiuaResult<bool> {
        match self.call() {
            Ok(_) => Ok(false),
            Err(e) => match e.break_data() {
                Ok((0, _)) => Ok(true),
                Ok((n, span)) => Err(UiuaError::Break(n - 1, span)),
                Err(e) => Err(e),
            },
        }
    }
    pub fn call_error_on_break(&mut self, message: &str) -> UiuaResult {
        match self.call() {
            Ok(_) => Ok(()),
            Err(e) => match e.break_data() {
                Ok((0, span)) => Err(span.sp(message.into()).into()),
                Ok((n, span)) => Err(UiuaError::Break(n - 1, span)),
                Err(e) => Err(e),
            },
        }
    }
    fn span_index(&self) -> usize {
        self.scope.call.last().map_or(0, |frame| {
            frame
                .spans
                .last()
                .map(|(i, _)| *i)
                .unwrap_or(frame.call_span)
        })
    }
    /// Get the span of the current function call
    pub fn span(&self) -> &Span {
        &self.spans[self.span_index()]
    }
    /// Construct an error with the current span
    pub fn error(&self, message: impl ToString) -> UiuaError {
        UiuaError::Run(self.span().clone().sp(message.to_string()))
    }
    /// Pop a value from the stack
    pub fn pop(&mut self, arg: impl StackArg) -> UiuaResult<Value> {
        self.stack.pop().ok_or_else(|| {
            self.error(format!(
                "Stack was empty when evaluating {}",
                arg.arg_name()
            ))
        })
    }
    /// Pop a result value from the stack
    ///
    /// Equivalent to `Self::pop("result")`
    pub fn pop_result(&mut self) -> UiuaResult<Value> {
        self.pop("result")
    }
    /// Push a value onto the stack
    pub fn push(&mut self, val: impl Into<Value>) {
        self.stack.push(val.into());
    }
    /// Take the entire stack
    pub fn take_stack(&mut self) -> Vec<Value> {
        take(&mut self.stack)
    }
    /// Clone the entire stack
    pub fn clone_stack(&self) -> Vec<Value> {
        self.stack.clone()
    }
    pub(crate) fn monadic_ref<V: Into<Value>>(&mut self, f: fn(&Value) -> V) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(&value));
        Ok(())
    }
    pub(crate) fn monadic_env<V: Into<Value>>(
        &mut self,
        f: fn(Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(value, self)?);
        Ok(())
    }
    pub(crate) fn monadic_ref_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(&value, self)?);
        Ok(())
    }
    pub(crate) fn monadic_mut(&mut self, f: fn(&mut Value)) -> UiuaResult {
        let mut a = self.pop(1)?;
        f(&mut a);
        self.push(a);
        Ok(())
    }
    pub(crate) fn dyadic_ref<V: Into<Value>>(&mut self, f: fn(&Value, &Value) -> V) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, &b));
        Ok(())
    }
    pub(crate) fn dyadic_env<V: Into<Value>>(
        &mut self,
        f: fn(Value, Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(a, b, self)?);
        Ok(())
    }
    pub(crate) fn dyadic_ref_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, &b, self)?);
        Ok(())
    }
    pub(crate) fn dyadic_ref_own_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, b, self)?);
        Ok(())
    }
    pub(crate) fn stack_size(&self) -> usize {
        self.stack.len()
    }
    pub(crate) fn truncate_stack(&mut self, size: usize) {
        self.stack.truncate(size);
    }
}

/// A trait for types that can be used as argument specifiers for [`Uiua::pop`] and [`Uiua::antipop`]
///
/// If the stack is empty, the error message will be "Stack was empty when evaluating {arg_name}"
pub trait StackArg {
    fn arg_name(self) -> String;
}

impl StackArg for usize {
    fn arg_name(self) -> String {
        format!("argument {self}")
    }
}
impl StackArg for u8 {
    fn arg_name(self) -> String {
        format!("argument {self}")
    }
}
impl StackArg for i32 {
    fn arg_name(self) -> String {
        format!("argument {self}")
    }
}
impl<'a> StackArg for &'a str {
    fn arg_name(self) -> String {
        self.to_string()
    }
}

impl StackArg for String {
    fn arg_name(self) -> String {
        self
    }
}
