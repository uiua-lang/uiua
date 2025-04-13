//! Signature checker implementation

use std::{
    array,
    cell::RefCell,
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    iter::repeat,
    slice,
};

use serde::*;

use crate::{
    algorithm::validate_size_of, Array, ArrayLen, ImplPrimitive, Node, Primitive, SigNode,
    Signature, SysOp, Value,
};

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
    array_depth: usize,
    node_depth: usize,
}

#[derive(Debug, Default)]
struct Stack {
    stack: Vec<BasicValue>,
    height: i32,
    min_height: usize,
}

impl Stack {
    // Simulate popping a value. Errors if the stack is empty, which means the function has too many args.
    fn pop(&mut self) -> BasicValue {
        self.height -= 1;
        self.set_min_height();
        self.stack.pop().unwrap_or(BasicValue::Other)
    }
    fn pop_n(&mut self, n: usize) {
        self.height -= n as i32;
        self.set_min_height();
        self.stack.truncate(self.stack.len().saturating_sub(n));
    }
    fn push(&mut self, val: BasicValue) {
        self.height += 1;
        self.stack.push(val);
    }
    fn push_n(&mut self, n: usize) {
        self.height += n as i32;
        self.stack.extend(repeat(BasicValue::Other).take(n));
    }
    fn remove(&mut self, i: usize) -> BasicValue {
        self.height -= 1;
        if i < self.stack.len() {
            self.stack.remove(self.stack.len() - i - 1)
        } else {
            BasicValue::Other
        }
    }
    fn handle_args_outputs(&mut self, args: usize, outputs: usize) {
        if args < 100 && outputs < 100 {
            for _ in 0..args {
                self.pop();
            }
            for _ in 0..outputs {
                self.push(BasicValue::Other);
            }
        } else {
            self.pop_n(args);
            self.push_n(outputs);
        }
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
    LoopOverreach,
    LoopVariable { args: usize },
    NoInverse,
}

impl SigCheckError {
    pub fn loop_overreach(self) -> Self {
        Self {
            kind: SigCheckErrorKind::LoopOverreach,
            ..self
        }
    }
    pub fn loop_variable(self, args: usize) -> Self {
        Self {
            kind: SigCheckErrorKind::LoopVariable { args },
            ..self
        }
    }
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

#[derive(Debug, Clone)]
enum BasicValue {
    Num(f64),
    Arr(Vec<Self>),
    Other,
}

impl BasicValue {
    fn from_val(value: &Value) -> Self {
        if let Some(n) = value.as_num_array().and_then(Array::as_scalar) {
            BasicValue::Num(*n)
        } else if let Some(n) = value.as_byte_array().and_then(Array::as_scalar) {
            BasicValue::Num(*n as f64)
        } else if value.rank() == 1 {
            BasicValue::Arr(match value {
                Value::Num(n) => n.data.iter().map(|n| BasicValue::Num(*n)).collect(),
                Value::Byte(b) => b.data.iter().map(|b| BasicValue::Num(*b as f64)).collect(),
                Value::Complex(c) => c.data.iter().map(|_| BasicValue::Other).collect(),
                Value::Char(c) => c.data.iter().map(|_| BasicValue::Other).collect(),
                Value::Box(b) => b.data.iter().map(|_| BasicValue::Other).collect(),
            })
        } else {
            BasicValue::Other
        }
    }
}

impl FromIterator<f64> for BasicValue {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = f64>,
    {
        BasicValue::Arr(iter.into_iter().map(BasicValue::Num).collect())
    }
}

const MAX_NODE_DEPTH: usize = if cfg!(debug_assertions) { 26 } else { 50 };

impl VirtualEnv {
    fn from_nodes(nodes: &[Node]) -> Result<Self, SigCheckError> {
        // println!("\ncheck sig: {nodes:?}");
        let mut env = VirtualEnv {
            stack: Stack::default(),
            under: Stack::default(),
            array_depth: 0,
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
            Node::Push(val) => self.push(BasicValue::from_val(val)),
            Node::Array { len, inner, .. } => match len {
                ArrayLen::Static(len) if *len < 100 => {
                    self.array_depth += 1;
                    self.node(inner)?;
                    self.array_depth -= 1;
                    let bottom = self.stack.height - *len as i32;
                    let stack_bottom = (bottom.max(0) as usize).min(self.stack.stack.len());
                    let mut items: Vec<_> = (self.stack.stack.drain(stack_bottom..))
                        .chain(repeat(BasicValue::Other).take((-bottom).max(0) as usize))
                        .collect();
                    self.stack.height = bottom;
                    self.stack.set_min_height();
                    items.reverse();
                    self.push(BasicValue::Arr(items));
                }
                ArrayLen::Static(len) => {
                    self.array_depth += 1;
                    self.node(inner)?;
                    self.array_depth -= 1;
                    self.handle_args_outputs(*len, 1)
                }
                ArrayLen::Dynamic(len) => self.handle_args_outputs(*len, 1),
            },
            Node::Label(..) | Node::RemoveLabel(..) => self.handle_args_outputs(1, 1),
            Node::Call(func, _) => self.handle_sig(func.sig),
            Node::CallMacro { sig, .. } | Node::CallGlobal(_, sig) => self.handle_sig(*sig),
            Node::BindGlobal { .. } => self.handle_args_outputs(1, 0),
            Node::CustomInverse(cust, _) => self.handle_sig(cust.sig()?),
            Node::Dynamic(dy) => self.handle_sig(dy.sig),
            &Node::Switch {
                sig, under_cond, ..
            } => {
                let cond = self.pop();
                self.handle_sig(sig);
                if under_cond {
                    self.under.push(cond);
                }
            }
            Node::Format(parts, ..) => self.handle_args_outputs(parts.len().saturating_sub(1), 1),
            Node::MatchFormatPattern(parts, ..) => {
                self.handle_args_outputs(1, parts.len().saturating_sub(1))
            }
            Node::Unpack { count, .. } => self.handle_args_outputs(1, *count),
            Node::Mod(Astar, args, _) | Node::ImplMod(AstarFirst, args, _) => {
                let _start = self.pop();
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
                let _n = self.pop();
                let _start = self.pop();
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
                let _start = self.pop();
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
                let _start = self.pop();
                let [neighbors, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors.args().max(is_goal.args()).saturating_sub(1);
                self.handle_args_outputs(args, 1 + has_costs as usize);
            }
            Node::ImplMod(PathTake, args, _) => {
                let _n = self.pop();
                let _start = self.pop();
                let [neighbors, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors.args().max(is_goal.args()).saturating_sub(1);
                self.handle_args_outputs(args, 1 + has_costs as usize);
            }
            Node::ImplMod(PathPop, args, _) => {
                let _start = self.pop();
                let [neighbors, is_goal] = get_args(args)?;
                let has_costs = neighbors.outputs() == 2;
                let args = neighbors.args().max(is_goal.args()).saturating_sub(1);
                self.handle_args_outputs(args, has_costs as usize);
            }
            Node::Prim(prim, _) => match prim {
                Dup => {
                    let val = self.pop();
                    self.push(val.clone());
                    self.push(val);
                }
                Flip => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(a);
                    self.push(b);
                }
                Pop => {
                    self.pop();
                }
                Over => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b.clone());
                    self.push(a);
                    self.push(b);
                }
                Around => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(a.clone());
                    self.push(b);
                    self.push(a);
                }
                Join => {
                    let a = self.pop();
                    let b = self.pop();
                    match (a, b) {
                        (BasicValue::Arr(mut a), BasicValue::Arr(b)) => {
                            a.extend(b);
                            self.push(BasicValue::Arr(a));
                        }
                        (BasicValue::Arr(mut a), b) => {
                            a.push(b);
                            self.push(BasicValue::Arr(a));
                        }
                        (a, BasicValue::Arr(mut b)) => {
                            b.insert(0, a);
                            self.push(BasicValue::Arr(b));
                        }
                        (a, b) => {
                            self.push(BasicValue::Arr(vec![a, b]));
                        }
                    }
                }
                prim => {
                    let args = prim
                        .args()
                        .ok_or_else(|| format!("{prim} has indeterminate args"))?;
                    let outputs = prim
                        .outputs()
                        .ok_or_else(|| format!("{prim} has indeterminate outputs"))?;
                    self.handle_args_outputs(args, outputs);
                }
            },
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
                        let _size = self.pop();
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
                    let n = self.pop();
                    self.repeat(f, n)?;
                }
                Do => {
                    let [body, cond] = get_args(args)?;
                    let copy_count = cond.args().saturating_sub(cond.outputs().saturating_sub(1));
                    let cond_sub_sig = Signature::new(
                        cond.args(),
                        (cond.outputs() + copy_count).saturating_sub(1),
                    );
                    let comp_sig = body.compose(cond_sub_sig);
                    if comp_sig.args() < comp_sig.outputs() && self.array_depth == 0 {
                        self.handle_args_outputs(comp_sig.args(), comp_sig.outputs());
                        return Err(SigCheckError::from(format!(
                            "do with a function with signature {comp_sig}"
                        ))
                        .loop_variable(self.stack.sig().args()));
                    }
                    self.handle_args_outputs(
                        comp_sig.args(),
                        comp_sig.outputs() + cond_sub_sig.outputs().saturating_sub(cond.args()),
                    );
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
                    self.handle_sig(f);
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
                    let mut args = Vec::with_capacity(f.sig.args());
                    for _ in 0..f.sig.args() {
                        args.push(self.pop());
                    }
                    self.sig_node(f)?;
                    for arg in args.into_iter().rev() {
                        self.push(arg);
                    }
                    self.sig_node(f)?;
                }
                Dip => {
                    let [f] = get_args_nodes(args)?;
                    let x = self.pop();
                    self.sig_node(f)?;
                    self.push(x);
                }
                Gap => {
                    let [f] = get_args_nodes(args)?;
                    _ = self.pop();
                    self.sig_node(f)?;
                }
                Reach => {
                    let [f] = get_args_nodes(args)?;
                    let x = self.pop();
                    _ = self.pop();
                    self.push(x);
                    self.sig_node(f)?;
                }
                On => {
                    let [f] = get_args_nodes(args)?;
                    let x = self.pop();
                    self.push(x.clone());
                    self.sig_node(f)?;
                    self.push(x);
                }
                By => {
                    let [f] = get_args_nodes(args)?;
                    let mut args = Vec::with_capacity(f.sig.args());
                    for _ in 0..f.sig.args() {
                        args.push(self.pop());
                    }
                    let x = args.last().cloned();
                    for arg in args.into_iter().rev() {
                        self.push(arg);
                    }
                    self.sig_node(f)?;
                    if let Some(x) = x {
                        self.push(x);
                    }
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
                    let n = self.pop();
                    self.repeat(f, n)?;
                }
                RepeatCountConvergence => {
                    let [f] = get_args_nodes(args)?;
                    self.repeat(f, BasicValue::Num(f64::INFINITY))?;
                    self.push(BasicValue::Other);
                }
                UnFill | SidedFill(_) => self.fill(args)?,
                UnBoth => {
                    let [f] = get_args_nodes(args)?;
                    let mut args = Vec::with_capacity(f.sig.args());
                    for _ in 0..f.sig.args() {
                        args.push(self.pop());
                    }
                    self.sig_node(f)?;
                    for arg in args.into_iter().rev() {
                        self.push(arg);
                    }
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
                    let _len = self.stack.pop();
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
                        self.push(BasicValue::Other);
                    }
                }
            },
            Node::SetOutputComment { .. } => {}
            Node::ValidateType { .. } => self.handle_args_outputs(1, 1),
            Node::PushUnder(n, _) => {
                for _ in 0..*n {
                    self.under.push(self.stack.pop());
                }
            }
            Node::CopyToUnder(n, _) => {
                for _ in 0..*n {
                    self.under.push(self.stack.pop());
                }
                for val in self.under.stack.iter().rev().take(*n).cloned() {
                    self.stack.push(val);
                }
            }
            Node::PopUnder(n, _) => {
                for _ in 0..*n {
                    self.stack.push(self.under.pop());
                }
            }
            Node::TrackCaller(inner) | Node::NoInline(inner) => self.node(inner)?,
            Node::WithLocal { inner, .. } => {
                let _val = self.stack.remove(inner.sig.args());
                self.sig_node(inner)?;
            }
            Node::GetLocal { .. } => self.handle_args_outputs(0, 1),
            Node::SetLocal { .. } => self.handle_args_outputs(1, 0),
            Node::NormalizeSoA { .. } => self.handle_args_outputs(1, 1),
        }
        self.node_depth -= 1;
        // println!("{node:?} -> {} ({})", self.stack.sig(), self.under.sig());
        Ok(())
    }
    fn push(&mut self, val: BasicValue) {
        self.stack.push(val);
    }
    fn pop(&mut self) -> BasicValue {
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
    fn repeat(
        &mut self,
        &SigNode { sig, ref node }: &SigNode,
        n: BasicValue,
    ) -> Result<(), SigCheckError> {
        if sig.args() < sig.outputs() {
            // More outputs than arguments
            if let BasicValue::Num(n) = n {
                let sig = if n >= 0.0 { sig } else { sig.inverse() };
                if n.fract() == 0.0 {
                    // If n is a known natural number, then it's fine
                    let n = n.abs() as usize;
                    if n > 0 {
                        if n <= 100 {
                            for _ in 0..n {
                                self.node(node)?;
                            }
                        } else {
                            let args = sig.args();
                            let outputs = n * (sig.outputs() - sig.args()) + sig.args();
                            if validate_size_of::<BasicValue>([outputs]).is_err() {
                                return Err("repeat with excessive outputs".into());
                            }
                            self.handle_args_outputs(args, outputs);
                        }
                    }
                } else if n.is_infinite() {
                    // If n is infinite, then we must be in an array
                    if self.array_depth == 0 {
                        self.handle_args_outputs(sig.args(), sig.outputs());
                        return Err(SigCheckError::from(format!(
                            "repeat with infinity and a function with signature {sig}"
                        ))
                        .loop_variable(self.stack.sig().args()));
                    } else {
                        self.handle_sig(sig);
                    }
                } else {
                    return Err("repeat without an integer or infinity".into());
                }
            } else {
                // If there is no number, then we must be in an array
                if self.array_depth == 0 {
                    self.handle_args_outputs(sig.args(), sig.outputs());
                    return Err(SigCheckError::from(format!(
                        "repeat with no number and a function with signature {sig}"
                    ))
                    .loop_variable(self.stack.sig().args()));
                } else {
                    self.handle_sig(sig);
                }
            }
        } else {
            // Non-positive case
            self.handle_sig(sig);
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
