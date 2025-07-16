//! Pre-evaluate code at compile time

use std::time::Duration;

use indexmap::IndexSet;

use crate::check::nodes_clean_sig;

use super::*;

/// The mode that dictates how much code to pre-evaluate at compile time
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum PreEvalMode {
    /// Does not evalute pure constants and expressions at comptime, but still evaluates `comptime`
    Lazy,
    /// Pre-evaluate each line, but not multiple lines together
    Line,
    /// The normal mode. Tries to evaluate pure, time-bounded constants and expressions at comptime
    #[default]
    Normal,
    /// Evaluate as much as possible at compile time, even impure expressions
    ///
    /// Recursive functions and certain system functions are not evaluated
    Lsp,
}

const MAX_PRE_EVAL_ELEMS: usize = 1000;
const MAX_PRE_EVAL_RANK: usize = 4;

impl PreEvalMode {
    #[allow(unused)]
    fn matches_nodes(self, nodes: &[Node], asm: &Assembly) -> bool {
        if nodes.iter().all(|node| matches!(node, Node::Push(_))) {
            return false;
        }
        if let PreEvalMode::Lazy = self {
            return false;
        }
        fn recurse<'a>(
            mode: PreEvalMode,
            nodes: &'a [Node],
            asm: &'a Assembly,
            visited: &mut IndexSet<&'a Function>,
        ) -> bool {
            if nodes.iter().any(|node| {
                matches!(
                    node,
                    Node::Push(val)
                        if val.shape.elements() > MAX_PRE_EVAL_ELEMS
                        || val.rank() > MAX_PRE_EVAL_RANK
                )
            }) {
                return false;
            }
            let len = visited.len();
            let matches = nodes.iter().all(|node| match node {
                Node::Run(nodes) => nodes.iter().all(|node| recurse(mode, node, asm, visited)),
                Node::Mod(prim, args, _) => {
                    prim.purity() == Purity::Pure
                        && args.iter().all(|sn| recurse(mode, &sn.node, asm, visited))
                }
                Node::ImplMod(prim, args, _) => {
                    prim.purity() == Purity::Pure
                        && args.iter().all(|sn| recurse(mode, &sn.node, asm, visited))
                }
                Node::NoInline(_) => false,
                Node::Array { inner, .. } => recurse(mode, inner, asm, visited),
                Node::Call(func, _) => recurse(mode, &asm[func], asm, visited),
                Node::CustomInverse(cust, _) if cust.normal.is_err() => false,
                node => {
                    node.is_limit_bounded(asm)
                        && match mode {
                            PreEvalMode::Lsp => node.is_min_purity(Purity::Impure, asm),
                            _ => node.is_pure(asm),
                        }
                }
            });
            visited.truncate(len);
            matches
        }
        recurse(self, nodes, asm, &mut IndexSet::new())
    }
}

impl Compiler {
    fn can_pre_eval(&self, nodes: &[Node]) -> bool {
        self.pre_eval_mode.matches_nodes(nodes, &self.asm)
    }
    pub(super) fn pre_eval(&self, node: &Node) -> Option<(Node, Vec<UiuaError>)> {
        let mut errors = Vec::new();
        if self.pre_eval_mode == PreEvalMode::Lazy
            || node.iter().all(|node| matches!(node, Node::Push(_)))
        {
            return None;
        }
        // println!("pre eval {:?}", node);
        let mut start = 0;
        let mut new: Option<Node> = None;
        let allow_error = node.is_pure(&self.asm);
        'start: while start < node.len() {
            for end in (start + 1..=node.len()).rev() {
                let section = &node[start..end];
                if self.can_pre_eval(section)
                    && nodes_clean_sig(section)
                        .is_some_and(|sig| sig.args() == 0 && sig.outputs() > 0)
                {
                    // println!("section: {section:?}");
                    let mut success = false;
                    match self.comptime_node(&section.into()) {
                        Ok(Some(values)) => {
                            // println!("values: {values:?}");
                            for val in &values {
                                val.validate();
                            }
                            let new = new.get_or_insert_with(|| node[..start].into());
                            new.extend(values.into_iter().map(Node::Push));
                            success = true;
                        }
                        Ok(None) => {}
                        Err(e) if !allow_error || e.meta.is_fill || self.in_try => {}
                        Err(e) => {
                            // println!("error: {e:?}");
                            errors.push(e)
                        }
                    }
                    if !success {
                        if let Some(new) = &mut new {
                            new.extend(section.iter().cloned());
                        }
                    }
                    start = end;
                    continue 'start;
                }
            }
            if let Some(new) = &mut new {
                new.push(node[start].clone())
            }
            start += 1;
        }
        new.map(|new| (new, errors))
    }
    pub(super) fn comptime_node(&self, node: &Node) -> UiuaResult<Option<Vec<Value>>> {
        if node.iter().all(|node| matches!(node, Node::Push(_))) {
            return Ok(Some(
                node.iter()
                    .map(|node| match node {
                        Node::Push(val) => val.clone(),
                        _ => unreachable!(),
                    })
                    .collect(),
            ));
        }
        if !self.can_pre_eval(node) {
            return Ok(None);
        }
        thread_local! {
            static CACHE: RefCell<HashMap<Node, Option<Vec<Value>>>> = RefCell::new(HashMap::new());
        }
        CACHE.with(|cache| {
            if let Some(stack) = cache.borrow_mut().get(node) {
                return Ok(stack.clone());
            }
            let mut asm = self.asm.clone();
            asm.root = node.clone();
            let mut env = if self.pre_eval_mode == PreEvalMode::Lsp {
                #[cfg(feature = "native_sys")]
                {
                    Uiua::with_native_sys()
                }
                #[cfg(not(feature = "native_sys"))]
                Uiua::with_safe_sys()
            } else {
                Uiua::with_safe_sys()
            }
            .with_execution_limit(Duration::from_millis(40));
            match env.run_asm(asm) {
                Ok(()) => {
                    let stack = env.take_stack();
                    let res = if stack.iter().any(|v| {
                        v.shape.elements() > MAX_PRE_EVAL_ELEMS || v.rank() > MAX_PRE_EVAL_RANK
                    }) {
                        None
                    } else {
                        Some(stack)
                    };
                    cache.borrow_mut().insert(env.asm.root, res.clone());
                    Ok(res)
                }
                Err(e) if matches!(*e.kind, UiuaErrorKind::Timeout(..)) => {
                    cache.borrow_mut().insert(env.asm.root, None);
                    Ok(None)
                }
                Err(e) => Err(e),
            }
        })
    }
}
