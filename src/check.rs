//! Signature checker implementation

use std::{
    array,
    cell::RefCell,
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    slice,
};

use serde::*;

use crate::{ImplPrimitive, Node, Primitive, SigNode, Signature, SysOp};

impl Node {
    /// Get the signature of this node
    pub fn sig(&self) -> Result<Signature, SigCheckError> {
        nodes_all_sigs(self.as_slice())
    }
    /// Convert this node to a [`SigNode`]
    pub fn sig_node(self) -> Result<SigNode, SigCheckError> {
        let sig = self.sig()?;
        Ok(SigNode::new(sig, self.clone()))
    }
    /// Get the signature of this node if there is no net temp stack change
    pub fn clean_sig(&self) -> Option<Signature> {
        nodes_clean_sig(slice::from_ref(self))
    }
}

pub fn nodes_sig(nodes: &[Node]) -> Result<Signature, SigCheckError> {
    VirtualEnv::from_nodes(nodes).map(|env| env.stack.sig())
}

pub fn nodes_clean_sig(nodes: &[Node]) -> Option<Signature> {
    let sig = nodes_all_sigs(nodes).ok()?;
    if sig.under_args() != 0 || sig.under_outputs() != 0 {
        None
    } else {
        Some(sig)
    }
}

fn nodes_all_sigs(nodes: &[Node]) -> Result<Signature, SigCheckError> {
    type AllSigsCache = HashMap<u64, Signature>;
    thread_local! {
        static CACHE: RefCell<AllSigsCache> = RefCell::new(AllSigsCache::new());
    }
    let mut hasher = DefaultHasher::new();
    nodes.hash(&mut hasher);
    let hash = hasher.finish();
    CACHE.with(|cache| {
        if let Some(sigs) = cache.borrow().get(&hash) {
            return Ok(*sigs);
        }
        let env = VirtualEnv::from_nodes(nodes)?;
        let under_sig = env.under.sig();
        let sig = (env.stack.sig()).with_under(under_sig.args(), under_sig.outputs());
        cache.borrow_mut().insert(hash, sig);
        Ok(sig)
    })
}

/// An environment that emulates the runtime but only keeps track of the stack.
struct VirtualEnv {
    stack: Stack,
    under: Stack,
    node_depth: usize,
}

#[derive(Debug, Default)]
struct Stack {
    height: i32,
    min_height: usize,
}

impl Stack {
    // Simulate popping a value. Errors if the stack is empty, which means the function has too many args.
    fn pop(&mut self) {
        self.height -= 1;
        self.set_min_height();
    }
    fn pop_n(&mut self, n: usize) {
        self.height -= n as i32;
        self.set_min_height();
    }
    fn push(&mut self) {
        self.height += 1;
    }
    fn push_n(&mut self, n: usize) {
        self.height += n as i32;
    }
    fn handle_args_outputs(&mut self, args: usize, outputs: usize) {
        self.pop_n(args);
        self.push_n(outputs);
    }
    /// Set the current stack height as a potential minimum.
    /// At the end of checking, the minimum stack height is a component in calculating the signature.
    fn set_min_height(&mut self) {
        self.min_height = self.min_height.max((-self.height).max(0) as usize);
    }
    fn sig(&self) -> Signature {
        Signature::new(
            self.min_height,
            (self.height + self.min_height as i32).max(0) as usize,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SigCheckError {
    pub message: String,
    pub kind: SigCheckErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum SigCheckErrorKind {
    Incorrect,
    NoInverse,
}

impl SigCheckError {
    pub fn no_inverse(self) -> Self {
        Self {
            kind: SigCheckErrorKind::NoInverse,
            ..self
        }
    }
}

impl<'a> From<&'a str> for SigCheckError {
    fn from(s: &'a str) -> Self {
        Self {
            message: s.to_string(),
            kind: SigCheckErrorKind::Incorrect,
        }
    }
}

impl From<String> for SigCheckError {
    fn from(s: String) -> Self {
        Self {
            message: s,
            kind: SigCheckErrorKind::Incorrect,
        }
    }
}

impl fmt::Display for SigCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

const MAX_NODE_DEPTH: usize = if cfg!(debug_assertions) { 26 } else { 50 };

impl VirtualEnv {
    fn from_nodes(nodes: &[Node]) -> Result<Self, SigCheckError> {
        // println!("\ncheck sig: {nodes:?}");
        let mut env = VirtualEnv {
            stack: Stack::default(),
            under: Stack::default(),
            node_depth: 0,
        };
        env.nodes(nodes)?;
        Ok(env)
    }
    fn nodes(&mut self, nodes: &[Node]) -> Result<(), SigCheckError> {
        nodes.iter().try_for_each(|node| self.node(node))
    }
    fn sig_node(&mut self, sn: &SigNode) -> Result<(), SigCheckError> {
        self.handle_sig(sn.sig);
        Ok(())
    }
    fn node(&mut self, node: &Node) -> Result<(), SigCheckError> {
        use ImplPrimitive::*;
        use Primitive::*;
        if self.node_depth > MAX_NODE_DEPTH {
            return Err("Function is too complex".into());
        }
        self.node_depth += 1;
        match node {
            Node::Run(nodes) => nodes.iter().try_for_each(|node| self.node(node))?,
            Node::Push(_) => self.push(),
            Node::Array { len, inner, .. } => {
                self.node(inner)?;
                self.stack.pop_n(*len);
                self.stack.push();
            }
            Node::Label(..) | Node::RemoveLabel(..) => self.handle_args_outputs(1, 1),
            Node::Call(func, _) => self.handle_sig(func.sig),
            Node::CallMacro { sig, .. } | Node::CallGlobal(_, sig) => self.handle_sig(*sig),
            Node::BindGlobal { .. } => self.handle_args_outputs(1, 0),
            Node::CustomInverse(cust, _) => self.handle_sig(cust.sig()?),
            Node::Dynamic(dy) => self.handle_sig(dy.sig),
            &Node::Switch {
                sig, under_cond, ..
            } => {
                self.pop();
                self.handle_sig(sig);
                if under_cond {
                    self.under.push();
                }
            }
            Node::Format(parts, ..) => self.handle_args_outputs(parts.len().saturating_sub(1), 1),
            Node::MatchFormatPattern(parts, ..) => {
                self.handle_args_outputs(1, parts.len().saturating_sub(1))
            }
            Node::Unpack { count, .. } => self.handle_args_outputs(1, *count),
            Node::ImplMod(Astar, args, _) | Node::ImplMod(AstarFirst, args, _) => {
                self.pop();
                let [neighbors, heuristic, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors
                    .args()
                    .max(heuristic.args())
                    .max(is_goal.args())
                    .saturating_sub(1);
                self.handle_args_outputs(args, 1 + has_costs as usize);
            }
            Node::ImplMod(AstarTake, args, _) => {
                self.pop();
                self.pop();
                let [neighbors, heuristic, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors
                    .args()
                    .max(heuristic.args())
                    .max(is_goal.args())
                    .saturating_sub(1);
                self.handle_args_outputs(args, 1 + has_costs as usize);
            }
            Node::ImplMod(AstarPop, args, _) => {
                self.pop();
                let [neighbors, heuristic, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors
                    .args()
                    .max(heuristic.args())
                    .max(is_goal.args())
                    .saturating_sub(1);
                self.handle_args_outputs(args, has_costs as usize);
            }
            Node::Mod(Path, args, _) | Node::ImplMod(PathFirst, args, _) => {
                self.pop();
                let [neighbors, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors.args().max(is_goal.args()).saturating_sub(1);
                self.handle_args_outputs(args, 1 + has_costs as usize);
            }
            Node::ImplMod(PathTake, args, _) => {
                self.pop();
                self.pop();
                let [neighbors, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors.args().max(is_goal.args()).saturating_sub(1);
                self.handle_args_outputs(args, 1 + has_costs as usize);
            }
            Node::ImplMod(PathPop, args, _) => {
                self.pop();
                let [neighbors, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors.args().max(is_goal.args()).saturating_sub(1);
                self.handle_args_outputs(args, has_costs as usize);
            }
            Node::Prim(prim, _) => {
                let args = prim
                    .args()
                    .ok_or_else(|| format!("{prim} has indeterminate args"))?;
                let outputs = prim
                    .outputs()
                    .ok_or_else(|| format!("{prim} has indeterminate outputs"))?;
                self.handle_args_outputs(args, outputs);
            }
            Node::ImplPrim(prim, _) => {
                let args = prim
                    .args()
                    .ok_or_else(|| format!("{prim} has indeterminate args"))?;
                let outputs = prim
                    .outputs()
                    .ok_or_else(|| format!("{prim} has indeterminate outputs"))?;
                self.handle_args_outputs(args, outputs);
            }
            Node::Mod(prim, args, _) => match prim {
                Reduce | Scan => {
                    let [sig] = get_args(args)?;
                    let args = sig.args().saturating_sub(sig.outputs());
                    self.handle_args_outputs(args, sig.outputs());
                }
                Each | Rows | Inventory => {
                    let [f] = get_args_nodes(args)?;
                    self.sig_node(f)?;
                }
                Table | Tuples => {
                    let [sig] = get_args(args)?;
                    self.handle_sig(sig);
                }
                Stencil => {
                    let [sig] = get_args(args)?;
                    if sig.args() <= 1 {
                        self.pop();
                    }
                    self.handle_args_outputs(1, sig.outputs());
                }
                Group | Partition => {
                    let [sig] = get_args(args)?;
                    self.handle_args_outputs(sig.args().max(1) + 1, sig.outputs());
                }
                Spawn | Pool => {
                    let [sig] = get_args(args)?;
                    self.handle_args_outputs(sig.args(), 1);
                }
                Repeat => {
                    let [f] = get_args_nodes(args)?;
                    self.pop();
                    self.repeat(f)?;
                }
                Do => {
                    let [body, cond] = get_args(args)?;
                    let copy_count = cond.args().saturating_sub(cond.outputs().saturating_sub(1));
                    let cond_sub_sig = Signature::new(
                        cond.args(),
                        (cond.outputs() + copy_count).saturating_sub(1),
                    );
                    let comp_sig = body.compose(cond_sub_sig);
                    self.handle_args_outputs(
                        comp_sig.args(),
                        comp_sig.outputs() + cond_sub_sig.outputs().saturating_sub(cond.args()),
                    );
                    if comp_sig.args() < comp_sig.outputs() {
                        self.stack.pop_n(comp_sig.args());
                    }
                }
                Un => {
                    let [sig] = get_args(args)?;
                    self.handle_sig(sig.inverse());
                }
                Anti => {
                    let [sig] = get_args(args)?;
                    self.handle_sig(sig.anti().unwrap_or(sig));
                }
                Fold => {
                    let [f] = get_args(args)?;
                    if f.outputs() >= f.args() {
                        self.handle_args_outputs(f.args(), f.outputs() + 1 - f.args());
                    } else {
                        self.handle_sig(f);
                    }
                }
                Try => {
                    let [f_sig, _handler_sig] = get_args(args)?;
                    self.handle_sig(f_sig);
                }
                Case => {
                    let [f] = get_args(args)?;
                    self.handle_sig(f);
                }
                Fill => self.fill(args)?,
                Content | Memo | Comptime => {
                    let [f] = get_args(args)?;
                    self.handle_sig(f);
                }
                Dump => {
                    let [_] = get_args(args)?;
                }
                Fork => {
                    let [f, g] = get_args(args)?;
                    self.handle_args_outputs(f.args().max(g.args()), f.outputs() + g.outputs());
                }
                Bracket => {
                    let [f, g] = get_args(args)?;
                    self.handle_args_outputs(f.args() + g.args(), f.outputs() + g.outputs());
                }
                Both => {
                    let [f] = get_args_nodes(args)?;
                    self.stack.pop_n(f.sig.args());
                    self.sig_node(f)?;
                    self.stack.push_n(f.sig.args());
                    self.sig_node(f)?;
                }
                Dip => {
                    let [f] = get_args_nodes(args)?;
                    self.pop();
                    self.sig_node(f)?;
                    self.push();
                }
                Gap => {
                    let [f] = get_args_nodes(args)?;
                    self.pop();
                    self.sig_node(f)?;
                }
                Reach => {
                    let [f] = get_args_nodes(args)?;
                    self.pop();
                    self.pop();
                    self.push();
                    self.sig_node(f)?;
                }
                On => {
                    let [f] = get_args_nodes(args)?;
                    self.pop();
                    self.push();
                    self.sig_node(f)?;
                    self.push();
                }
                By => {
                    let [f] = get_args_nodes(args)?;
                    self.sig_node(f)?;
                    self.push();
                }
                Above | Below => {
                    let [f] = get_args(args)?;
                    self.handle_args_outputs(f.args(), f.args() + f.outputs());
                }
                With | Off => {
                    let [f] = get_args(args)?;
                    self.handle_args_outputs(f.args(), f.outputs() + 1);
                }
                Sys(SysOp::ReadLines) => {
                    let [f] = get_args(args)?;
                    self.handle_sig(f);
                }
                Sys(SysOp::AudioStream) => {
                    let [f] = get_args(args)?;
                    self.handle_args_outputs(
                        f.args().saturating_sub(1),
                        f.outputs().saturating_sub(1),
                    );
                }
                prim if prim.modifier_args().is_some() => {
                    if let Some(sig) = prim.sig() {
                        self.handle_sig(sig);
                    } else {
                        return Err(SigCheckError::from(format!(
                            "{} was not checked. This is a bug in the interpreter",
                            prim.format()
                        )));
                    }
                }
                prim => {
                    return Err(SigCheckError::from(format!(
                        "{} was checked as a modifier. This is a bug in the interpreter",
                        prim.format()
                    )));
                }
            },
            Node::ImplMod(prim, args, _) => match prim {
                &OnSub(n) | &BySub(n) | &WithSub(n) | &OffSub(n) => {
                    let [sn] = get_args_nodes(args)?;
                    let args = sn.sig.args().max(n);
                    self.handle_args_outputs(args, args);
                    self.sig_node(sn)?;
                    self.handle_args_outputs(0, n);
                }
                ReduceContent | ReduceDepth(_) => {
                    let [sig] = get_args(args)?;
                    let args = sig.args().saturating_sub(sig.outputs());
                    self.handle_args_outputs(args, sig.outputs());
                }
                ReduceConjoinInventory => {
                    let [sig] = get_args(args)?;
                    self.handle_sig(sig);
                }
                RepeatWithInverse => {
                    let [f, inv] = get_args_nodes(args)?;
                    if f.sig.inverse() != inv.sig {
                        return Err(SigCheckError::from(
                            "repeat inverse does not have inverse signature",
                        ));
                    }
                    self.pop();
                    self.repeat(f)?;
                }
                RepeatCountConvergence => {
                    let [f] = get_args_nodes(args)?;
                    self.repeat(f)?;
                    self.push();
                }
                UnFill | SidedFill(_) => self.fill(args)?,
                UnBoth => {
                    let [f] = get_args_nodes(args)?;
                    self.stack.pop_n(f.sig.args());
                    self.sig_node(f)?;
                    self.stack.push_n(f.sig.args());
                    self.sig_node(f)?;
                }
                UnBracket => {
                    let [f, g] = get_args(args)?;
                    self.handle_args_outputs(f.args() + g.args(), f.outputs() + g.outputs());
                }
                EachSub(_) => {
                    let [f] = get_args_nodes(args)?;
                    self.sig_node(f)?;
                }
                UndoRows | UndoInventory => {
                    let [f] = get_args_nodes(args)?;
                    self.stack.pop();
                    self.sig_node(f)?;
                }
                UnScan => self.handle_args_outputs(1, 1),
                SplitBy | SplitByScalar | SplitByKeepEmpty => {
                    let [f] = get_args(args)?;
                    self.handle_args_outputs(2, f.outputs());
                }
                prim => {
                    let args = prim
                        .args()
                        .ok_or_else(|| format!("{prim} has indeterminate args"))?;
                    let outputs = prim
                        .outputs()
                        .ok_or_else(|| format!("{prim} has indeterminate outputs"))?;
                    for _ in 0..args {
                        self.pop();
                    }
                    for _ in 0..outputs {
                        self.push();
                    }
                }
            },
            Node::SetOutputComment { .. } => {}
            Node::ValidateType { .. } => self.handle_args_outputs(1, 1),
            Node::PushUnder(n, _) => {
                for _ in 0..*n {
                    self.stack.pop();
                    self.under.push();
                }
            }
            Node::CopyToUnder(n, _) => {
                for _ in 0..*n {
                    self.stack.pop();
                    self.under.push();
                    self.stack.push();
                }
            }
            Node::PopUnder(n, _) => {
                for _ in 0..*n {
                    self.under.pop();
                    self.stack.push();
                }
            }
            Node::TrackCaller(inner) | Node::NoInline(inner) => self.node(inner)?,
            Node::WithLocal { inner, .. } => {
                self.stack.pop();
                self.sig_node(inner)?;
            }
            Node::GetLocal { .. } => self.handle_args_outputs(0, 1),
            Node::SetLocal { .. } => self.handle_args_outputs(1, 0),
        }
        self.node_depth -= 1;
        // println!("{node:?} -> {} ({})", self.stack.sig(), self.under.sig());
        Ok(())
    }
    fn push(&mut self) {
        self.stack.push();
    }
    fn pop(&mut self) {
        self.stack.pop()
    }
    fn handle_args_outputs(&mut self, args: usize, outputs: usize) {
        self.stack.handle_args_outputs(args, outputs);
    }
    fn handle_sig(&mut self, sig: Signature) {
        self.stack.handle_args_outputs(sig.args(), sig.outputs());
        self.under
            .handle_args_outputs(sig.under_args(), sig.under_outputs());
    }
    fn fill(&mut self, args: &[SigNode]) -> Result<(), SigCheckError> {
        let [fill, f] = get_args_nodes(args)?;
        if fill.sig.outputs() > 0 || fill.sig.args() > 0 && fill.sig.outputs() != 0 {
            self.sig_node(fill)?;
        }
        self.handle_args_outputs(fill.sig.outputs(), 0);
        self.sig_node(f)
    }
    fn repeat(&mut self, sn: &SigNode) -> Result<(), SigCheckError> {
        let sig = sn.sig;
        self.sig_node(sn)?;
        if sig.outputs() > sig.args() {
            self.stack.pop_n(sig.args());
        }
        Ok(())
    }
}

fn get_args_nodes<const N: usize>(args: &[SigNode]) -> Result<[&SigNode; N], SigCheckError> {
    if args.len() != N {
        return Err(format!(
            "Expected {} operand{}, but got {}",
            N,
            if N == 1 { "" } else { "s" },
            args.len()
        )
        .into());
    }
    Ok(array::from_fn(|i| &args[i]))
}

fn get_args<const N: usize>(args: &[SigNode]) -> Result<[Signature; N], SigCheckError> {
    let mut res = [Signature::default(); N];
    if args.len() != N {
        return Err(format!(
            "Expected {} operand{}, but got {}",
            N,
            if N == 1 { "" } else { "s" },
            args.len()
        )
        .into());
    }
    for (i, arg) in args.iter().enumerate() {
        res[i] = arg.sig;
    }
    Ok(res)
}
