use std::{
    borrow::Borrow,
    collections::hash_map::DefaultHasher,
    fmt,
    hash::{Hash, Hasher},
    mem::{discriminant, swap, take},
    ops::Deref,
    slice::{self, SliceIndex},
    sync::Arc,
};

use ecow::{eco_vec, EcoString, EcoVec};
use indexmap::IndexSet;
use serde::*;

use crate::{
    check::SigCheckError,
    compile::invert::{InversionError, InversionResult},
    Assembly, BindingKind, DynamicFunction, Function, ImplPrimitive, Primitive, Signature, Value,
};

node!(
    /// Create an array
    Array { len: ArrayLen, inner: Arc<Node>, boxed: bool, prim: Option<Primitive>, span: usize },
    /// Get a global value
    CallGlobal(index(usize), sig(Signature)),
    /// Call a recursive macro
    CallMacro { index: usize, sig: Signature, span: usize },
    /// Bind a global value
    BindGlobal { index: usize, span: usize },
    /// Set a value's label
    Label(label(EcoString), span(usize)),
    /// Remove a value's label
    RemoveLabel(label(Option<EcoString>), span(usize)),
    /// Format with a string
    Format(parts(EcoVec<EcoString>), span(usize)),
    /// Match a format pattern
    MatchFormatPattern(parts(EcoVec<EcoString>), span(usize)),
    /// A custom inverse
    CustomInverse(cust(Arc<CustomInverse>), span(usize)),
    /// A switch with branches
    Switch { branches: Ops, sig: Signature, under_cond: bool, span: usize },
    /// Unpack an array onto the stack
    Unpack { count: usize, unbox: bool, prim: Option<Primitive>, span: usize },
    /// Set some values for an output comment
    SetOutputComment { i: usize, n: usize },
    /// Validate that a value has a certain type
    ValidateType { index: usize, type_num: u8, name: EcoString, span: usize },
    /// Call a Rust function
    Dynamic(func(DynamicFunction)),
    /// Push some values to the under stack
    PushUnder(n(usize), span(usize)),
    /// Copy some values to the under stack
    CopyToUnder(n(usize), span(usize)),
    /// Pop some values from the under stack
    PopUnder(n(usize), span(usize)),
    /// Do not inline this node
    NoInline(inner(Arc<Node>)),
    /// Track the caller of this node
    TrackCaller(inner(Arc<Node>)),
    /// Push a value onto the stack
    (#[serde(untagged)] rep),
    Push(val(Value)),
    /// Run a primitive function
    (#[serde(untagged)] rep),
    Prim(prim(Primitive), span(usize)),
    /// Run an implementation primitive function
    (#[serde(untagged)] rep),
    ImplPrim(prim(ImplPrimitive), span(usize)),
    /// Run a modifier
    (#[serde(untagged)] rep),
    Mod(prim(Primitive), args(Ops), span(usize)),
    /// Run an implementation modifier
    (#[serde(untagged)] rep),
    ImplMod(prim(ImplPrimitive), args(Ops), span(usize)),
    /// Call a function
    (#[serde(untagged)] rep),
    Call(func(Function), span(usize)),
    /// Run some nodes in sequence.
    ///
    /// Do not edit the list directly. Use functions like [`Node::push`] and [`Node::prepend`] instead.
    (#[serde(untagged)] rep),
    Run(nodes(EcoVec<Node>)),
);

/// A node with a signature
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct SigNode {
    /// The node
    pub node: Node,
    /// The signature
    pub sig: Signature,
}

impl SigNode {
    /// Create a new signature node
    pub fn new(sig: impl Into<Signature>, node: impl Into<Node>) -> Self {
        Self {
            node: node.into(),
            sig: sig.into(),
        }
    }
    /// Call this node on N sets of arguments
    pub fn on_all(self, n: usize, span: usize) -> Node {
        match n {
            0 => Node::empty(),
            1 => self.node,
            n => {
                let mut sn = self;
                let inner = sn.clone();
                let prev_pow_2 = (n as f64).log2() as usize;
                for _ in 0..prev_pow_2 {
                    let mut sig = sn.sig;
                    sig.args *= 2;
                    sig.outputs *= 2;
                    let node = Node::Mod(Primitive::Both, eco_vec![sn], span);
                    sn = SigNode::new(sig, node);
                }
                let remain = n - 2usize.pow(prev_pow_2 as u32);
                if remain > 0 {
                    let SigNode { mut sig, node } = sn;
                    let args = inner.sig.args;
                    let mut both = node.clone();
                    for _ in 0..remain {
                        for _ in 0..args {
                            let inner = SigNode::new(sig, both);
                            both = Node::Mod(Primitive::Dip, eco_vec![inner], span);
                        }
                        both.push(inner.node.clone());
                        sig.args += args;
                        sig.outputs += args;
                        sig.outputs += inner.sig.outputs;
                        sig.outputs -= inner.sig.args;
                    }
                    both
                } else {
                    sn.node
                }
            }
        }
    }
}

impl From<SigNode> for Node {
    fn from(sn: SigNode) -> Self {
        sn.node
    }
}

impl From<Arc<Node>> for Node {
    fn from(node: Arc<Node>) -> Self {
        Arc::unwrap_or_clone(node)
    }
}

impl Serialize for SigNode {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        (self.sig.args, self.sig.outputs, &self.node).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SigNode {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let (args, outputs, node) = <(usize, usize, Node)>::deserialize(deserializer)?;
        Ok(SigNode::new(Signature::new(args, outputs), node))
    }
}

pub(crate) type Ops = EcoVec<SigNode>;

/// The length of an array when being constructed
///
/// This is used by [`Node::Array`]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArrayLen {
    /// A static number of rows
    Static(usize),
    /// A dynamic number of rows. Pulls is this number of values.
    Dynamic(usize),
}

impl fmt::Display for ArrayLen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Static(n) => write!(f, "{n}"),
            Self::Dynamic(n) => write!(f, "?{n}"),
        }
    }
}

/// A custom inverse node
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(default)]
pub struct CustomInverse {
    /// The normal function to call
    pub normal: InversionResult<SigNode>,
    /// The un inverse
    #[serde(skip_serializing_if = "Option::is_none")]
    pub un: Option<SigNode>,
    /// The under inverse
    #[serde(skip_serializing_if = "Option::is_none")]
    pub under: Option<(SigNode, SigNode)>,
    /// The anti inverse
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anti: Option<SigNode>,
    /// Whether this was created with obverse
    ///
    /// Inverses that are not set by obverse may be overridden
    pub is_obverse: bool,
}

impl Default for CustomInverse {
    fn default() -> Self {
        Self {
            normal: Ok(SigNode::default()),
            un: None,
            under: None,
            anti: None,
            is_obverse: false,
        }
    }
}

impl From<InversionError> for CustomInverse {
    fn from(e: InversionError) -> Self {
        Self {
            normal: Err(e),
            ..Default::default()
        }
    }
}

impl fmt::Debug for CustomInverse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("custom inverse");
        s.field("normal", &self.normal.as_ref().map(|sn| &sn.node));
        if let Some(un) = &self.un {
            s.field("un", &un.node);
        }
        if let Some(anti) = &self.anti {
            s.field("anti", &anti.node);
        }
        if let Some(under) = &self.under {
            s.field("do", &under.0.node);
            s.field("undo", &under.1.node);
        }
        s.finish()
    }
}

impl CustomInverse {
    /// Get the signature of the custom inverse
    pub fn sig(&self) -> Result<Signature, SigCheckError> {
        Ok(match &self.normal {
            Ok(n) => n.sig,
            Err(e) => {
                if let Some(un) = &self.un {
                    un.sig.inverse()
                } else if let Some(anti) = &self.anti {
                    anti.sig
                        .anti()
                        .ok_or_else(|| SigCheckError::from(e.to_string()).no_inverse())?
                } else {
                    return match e {
                        InversionError::Signature(e) => Err(e.clone()),
                        e => Err(SigCheckError::from(e.to_string()).no_inverse()),
                    };
                }
            }
        })
    }
    /// Iterate over all nodes
    pub fn nodes(&self) -> impl Iterator<Item = &SigNode> {
        (self.normal.as_ref().into_iter())
            .chain(self.un.as_ref())
            .chain(self.anti.as_ref())
            .chain(self.under.as_ref().into_iter().flat_map(|(b, a)| [a, b]))
    }
}

impl Default for Node {
    fn default() -> Self {
        Self::empty()
    }
}

impl Node {
    /// Create an empty node
    pub const fn empty() -> Self {
        Self::Run(EcoVec::new())
    }
    /// Create a push node from a value
    pub fn new_push(val: impl Into<Value>) -> Self {
        Self::Push(val.into())
    }
    /// Get a slice of the nodes in this node
    pub fn as_slice(&self) -> &[Node] {
        if let Node::Run(nodes) = self {
            nodes
        } else {
            slice::from_ref(self)
        }
    }
    /// Get a mutable slice of the nodes in this node
    pub fn as_mut_slice(&mut self) -> &mut [Node] {
        match self {
            Node::Run(nodes) => nodes.make_mut(),
            other => slice::from_mut(other),
        }
    }
    /// Slice the node to get a subnode
    pub fn slice<R>(&self, range: R) -> Self
    where
        R: SliceIndex<[Node], Output = [Node]>,
    {
        Self::from_iter(self.as_slice()[range].iter().cloned())
    }
    /// Get a mutable vector of the nodes in this node
    ///
    /// Transforms the node into a [`Node::Run`] if it is not already a [`Node::Run`]
    pub fn as_vec(&mut self) -> &mut EcoVec<Node> {
        match self {
            Node::Run(nodes) => nodes,
            other => {
                let first = take(other);
                let Node::Run(nodes) = other else {
                    unreachable!()
                };
                nodes.push(first);
                nodes
            }
        }
    }
    /// Turn the node into a vector
    pub fn into_vec(self) -> EcoVec<Node> {
        if let Node::Run(nodes) = self {
            nodes
        } else {
            eco_vec![self]
        }
    }
    /// Turn a run of 1 node into a single node
    pub fn normalize(&mut self) {
        if let Node::Run(nodes) = self {
            if nodes.len() == 1 {
                *self = take(nodes).remove(0);
            }
        }
    }
    /// Truncate the node to a certain length
    pub fn truncate(&mut self, len: usize) {
        if let Node::Run(nodes) = self {
            nodes.truncate(len);
            if nodes.len() == 1 {
                *self = take(nodes).remove(0);
            }
        } else if len == 0 {
            *self = Node::default();
        }
    }
    /// Split the node at the given index
    #[track_caller]
    pub fn split_off(&mut self, index: usize) -> Self {
        if let Node::Run(nodes) = self {
            let removed = EcoVec::from(&nodes[index..]);
            nodes.truncate(index);
            Node::Run(removed)
        } else if index == 0 {
            take(self)
        } else if index == 1 {
            Node::empty()
        } else {
            panic!(
                "Index {index} out of bounds of node with length {}",
                self.len()
            );
        }
    }
    /// Mutably iterate over the nodes of this node
    ///
    /// Transforms the node into a [`Node::Run`] if it is not already a [`Node::Run`]
    pub fn iter_mut(&mut self) -> slice::IterMut<Self> {
        self.as_mut_slice().iter_mut()
    }
    /// Push a node onto the end of the node
    ///
    /// Transforms the node into a [`Node::Run`] if it is not already a [`Node::Run`]
    pub fn push(&mut self, mut node: Node) {
        if let Node::Run(nodes) = self {
            if nodes.is_empty() {
                *self = node;
            } else {
                match node {
                    Node::Run(other) => nodes.extend(other),
                    node => nodes.push(node),
                }
            }
        } else if let Node::Run(nodes) = &node {
            if !nodes.is_empty() {
                swap(self, &mut node);
                self.as_vec().insert(0, node);
            }
        } else {
            self.as_vec().push(node);
        }
    }
    /// Push a node onto the beginning of the node
    ///
    /// Transforms the node into a [`Node::Run`] if it is not already a [`Node::Run`]
    pub fn prepend(&mut self, mut node: Node) {
        if let Node::Run(nodes) = self {
            if nodes.is_empty() {
                *self = node;
            } else {
                match node {
                    Node::Run(mut other) => {
                        swap(nodes, &mut other);
                        nodes.extend(other)
                    }
                    node => nodes.insert(0, node),
                }
            }
        } else if let Node::Run(nodes) = &node {
            if !nodes.is_empty() {
                swap(self, &mut node);
                self.as_vec().push(node);
            }
        } else {
            self.as_vec().insert(0, node);
        }
    }
    /// Pop a node from the end of this node
    pub fn pop(&mut self) -> Option<Node> {
        match self {
            Node::Run(nodes) => {
                let res = nodes.pop();
                if nodes.len() == 1 {
                    *self = take(nodes).remove(0);
                }
                res
            }
            node => Some(take(node)),
        }
    }
    /// Clear the node
    pub fn clear(&mut self) {
        *self = Node::empty();
    }
    pub(crate) fn as_flipped_primitive(&self) -> Option<(Primitive, bool)> {
        match self {
            Node::Prim(prim, _) => Some((*prim, false)),
            Node::Run(nodes) => match nodes.as_slice() {
                [Node::Prim(Primitive::Flip, _), Node::Prim(prim, _)] => Some((*prim, true)),
                _ => None,
            },
            _ => None,
        }
    }
    pub(crate) fn as_primitive(&self) -> Option<Primitive> {
        self.as_flipped_primitive()
            .filter(|(_, flipped)| !flipped)
            .map(|(prim, _)| prim)
    }
    pub(crate) fn as_flipped_impl_primitive(&self) -> Option<(ImplPrimitive, bool)> {
        match self {
            Node::ImplPrim(prim, _) => Some((*prim, false)),
            Node::Run(nodes) => match nodes.as_slice() {
                [Node::Prim(Primitive::Flip, _), Node::ImplPrim(prim, _)] => Some((*prim, true)),
                _ => None,
            },
            _ => None,
        }
    }
    pub(crate) fn as_impl_primitive(&self) -> Option<ImplPrimitive> {
        self.as_flipped_impl_primitive()
            .filter(|(_, flipped)| !flipped)
            .map(|(prim, _)| prim)
    }
    /// Call a function on the last node in the tree, recursing into function calls
    pub fn last_mut_recursive<T>(
        &mut self,
        asm: &mut Assembly,
        f: impl Fn(&mut Node) -> T + Copy,
    ) -> Option<T> {
        fn recurse<T>(
            target: Option<usize>,
            sub: Option<usize>,
            node: &mut Node,
            asm: &mut Assembly,
            f: impl FnOnce(&mut Node) -> T + Copy,
        ) -> Option<T> {
            let mut this_node = match target {
                Some(i) => asm.functions[i].inner(),
                None => node.inner(),
            };
            if let Some(i) = sub {
                this_node = this_node.get(i)?;
            }
            match this_node {
                Node::Run(nodes) if sub.is_none() => {
                    for i in (0..nodes.len()).rev() {
                        if let Some(res) = recurse(target, Some(i), node, asm, f) {
                            return Some(res);
                        }
                    }
                    None
                }
                Node::Call(func, _) => recurse(Some(func.index), None, node, asm, f),
                _ => {
                    let mut node = match target {
                        Some(i) => asm.functions.make_mut()[i].inner_mut(),
                        None => node.inner_mut(),
                    };
                    if let Some(i) = sub {
                        node = node.as_mut_slice().get_mut(i)?.inner_mut();
                    }
                    Some(f(node))
                }
            }
        }
        recurse(None, None, self, asm, f)
    }
    pub(crate) fn inner(&self) -> &Node {
        match self {
            Node::TrackCaller(inner) | Node::NoInline(inner) => inner.inner(),
            Node::CustomInverse(cust, ..) => {
                if let Ok(sn) = cust.normal.as_ref() {
                    sn.node.inner()
                } else {
                    self
                }
            }
            node => node,
        }
    }
    fn inner_mut(&mut self) -> &mut Node {
        match self {
            Node::TrackCaller(inner) | Node::NoInline(inner) => Arc::make_mut(inner),
            node => node,
        }
    }
    pub(crate) fn hash_with_span(&self, hasher: &mut impl Hasher) {
        self.hash(hasher);
        if let Some(span) = self.span() {
            span.hash(hasher);
        }
    }
    /// `⊓ bracket` several nodes
    pub fn bracket<I>(nodes: I, span: usize) -> Node
    where
        I: IntoIterator<Item = SigNode>,
        I::IntoIter: ExactSizeIterator,
    {
        let mut nodes = nodes.into_iter();
        let size = nodes.len();
        match size {
            0 => Node::empty(),
            1 => nodes.next().unwrap().node,
            2 => Node::Mod(Primitive::Bracket, nodes.collect(), span),
            _ => {
                let first = nodes.next().unwrap();
                let second = Self::bracket(nodes, span).sig_node().unwrap();
                Node::Mod(Primitive::Both, eco_vec![first, second], span)
            }
        }
    }
}

impl From<&[Node]> for Node {
    fn from(nodes: &[Node]) -> Self {
        Node::from_iter(nodes.iter().cloned())
    }
}

impl<const N: usize> From<[Node; N]> for Node {
    fn from(nodes: [Node; N]) -> Self {
        Node::from_iter(nodes)
    }
}

impl FromIterator<Node> for Node {
    fn from_iter<T: IntoIterator<Item = Node>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let Some(mut node) = iter.next() else {
            return Node::default();
        };
        for n in iter {
            node.push(n);
        }
        node
    }
}

impl Extend<Node> for Node {
    fn extend<T: IntoIterator<Item = Node>>(&mut self, iter: T) {
        for node in iter {
            self.push(node);
        }
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Run(eco_vec) => {
                let mut tuple = f.debug_tuple("");
                for node in eco_vec {
                    tuple.field(node);
                }
                tuple.finish()
            }
            Node::Push(value) => write!(f, "push {value}"),
            Node::Prim(prim, _) => write!(f, "{prim}"),
            Node::ImplPrim(impl_prim, _) => write!(f, "{impl_prim}"),
            Node::Mod(prim, args, _) => {
                let mut tuple = f.debug_tuple(&prim.to_string());
                for sn in args {
                    tuple.field(&sn.node);
                }
                tuple.finish()
            }
            Node::ImplMod(impl_prim, args, _) => {
                let mut tuple = f.debug_tuple(&impl_prim.to_string());
                for sn in args {
                    tuple.field(&sn.node);
                }
                tuple.finish()
            }
            Node::Array {
                len,
                inner,
                boxed: true,
                ..
            } => {
                write!(f, "{}{}{{", Primitive::Len, len)?;
                inner.fmt(f)?;
                write!(f, "}}")
            }
            Node::Array {
                len,
                inner,
                boxed: false,
                ..
            } => {
                write!(f, "{}{}[", Primitive::Len, len)?;
                inner.fmt(f)?;
                write!(f, "]")
            }
            Node::Call(func, _) => write!(f, "call {}", func.id),
            Node::CallGlobal(index, _) => write!(f, "<call global {index}>"),
            Node::CallMacro { index, .. } => write!(f, "<call macro {index}>"),
            Node::BindGlobal { index, .. } => write!(f, "<bind global {index}>"),
            Node::Label(label, _) => write!(f, "${label}"),
            Node::RemoveLabel(..) => write!(f, "remove label"),
            Node::Format(parts, _) => {
                write!(f, "$\"")?;
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, "_")?
                    }
                    write!(f, "{part}")?
                }
                write!(f, "\"")
            }
            Node::MatchFormatPattern(parts, _) => {
                write!(f, "°$\"")?;
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, "_")?
                    }
                    write!(f, "{part}")?
                }
                write!(f, "\"")
            }
            Node::Switch { branches, .. } => write!(f, "<switch {}>", branches.len()),
            Node::CustomInverse(cust, _) => cust.fmt(f),
            Node::Unpack {
                count,
                unbox: false,
                ..
            } => write!(f, "<unpack {count}>"),
            Node::Unpack {
                count, unbox: true, ..
            } => write!(f, "<unpack (unbox) {count}>"),
            Node::SetOutputComment { i, n, .. } => write!(f, "<set output comment {i}({n})>"),
            Node::ValidateType { type_num, name, .. } => {
                write!(f, "<validate {name} as {type_num}>")
            }
            Node::Dynamic(func) => write!(f, "<dynamic function {}>", func.index),
            Node::PushUnder(count, _) => write!(f, "push-u-{count}"),
            Node::CopyToUnder(count, _) => write!(f, "copy-u-{count}"),
            Node::PopUnder(count, _) => write!(f, "pop-u-{count}"),
            Node::NoInline(inner) => f.debug_tuple("no-inline").field(inner.as_ref()).finish(),
            Node::TrackCaller(inner) => {
                f.debug_tuple("track-caller").field(inner.as_ref()).finish()
            }
        }
    }
}

/// Levels of purity for an operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Purity {
    /// The operation visibly affects the environment
    Mutating,
    /// The operation reads from the environment but does not visibly affect it
    Impure,
    /// The operation is completely pure
    Pure,
}

impl Node {
    /// Check if the node is pure
    pub fn is_pure<'a>(&'a self, min_purity: Purity, asm: &'a Assembly) -> bool {
        fn recurse<'a>(
            node: &'a Node,
            purity: Purity,
            asm: &'a Assembly,
            visited: &mut IndexSet<&'a Function>,
        ) -> bool {
            let len = visited.len();
            let is = match node {
                Node::Run(nodes) => nodes.iter().all(|node| recurse(node, purity, asm, visited)),
                Node::Prim(prim, _) => prim.purity() >= purity,
                Node::ImplPrim(prim, _) => prim.purity() >= purity,
                Node::Mod(prim, args, _) => {
                    prim.purity() >= purity
                        && args
                            .iter()
                            .all(|arg| recurse(&arg.node, purity, asm, visited))
                }
                Node::ImplMod(prim, args, _) => {
                    prim.purity() >= purity
                        && args
                            .iter()
                            .all(|arg| recurse(&arg.node, purity, asm, visited))
                }
                Node::Array { inner, .. } => recurse(inner, purity, asm, visited),
                Node::Call(func, _) => {
                    visited.insert(func) && recurse(&asm[func], purity, asm, visited)
                }
                Node::CallGlobal(index, _) => {
                    if let Some(binding) = asm.bindings.get(*index) {
                        match &binding.kind {
                            BindingKind::Const(Some(_)) => true,
                            BindingKind::Func(f) => {
                                visited.insert(f) && recurse(&asm[f], purity, asm, visited)
                            }
                            _ => false,
                        }
                    } else {
                        false
                    }
                }
                Node::Switch { branches, .. } => branches
                    .iter()
                    .all(|br| recurse(&br.node, purity, asm, visited)),
                Node::CustomInverse(cust, _) => (cust.normal.as_ref().ok())
                    .or(cust.un.as_ref())
                    .is_some_and(|sn| recurse(&sn.node, purity, asm, visited)),
                _ => true,
            };
            visited.truncate(len);
            is
        }
        recurse(self, min_purity, asm, &mut IndexSet::new())
    }
    /// Check if the node has a bound runtime
    pub fn is_limit_bounded<'a>(&'a self, asm: &'a Assembly) -> bool {
        fn recurse<'a>(
            node: &'a Node,
            asm: &'a Assembly,
            visited: &mut IndexSet<&'a Function>,
        ) -> bool {
            let len = visited.len();
            let is = match node {
                Node::Run(nodes) => nodes.iter().all(|node| recurse(node, asm, visited)),
                Node::Prim(Primitive::Send | Primitive::Recv, _) => false,
                Node::Prim(Primitive::Sys(op), _) if op.purity() <= Purity::Mutating => false,
                Node::Mod(_, args, _) | Node::ImplMod(_, args, _) => {
                    args.iter().all(|arg| recurse(&arg.node, asm, visited))
                }
                Node::Array { inner, .. } => recurse(inner, asm, visited),
                Node::Call(func, _) => visited.insert(func) && recurse(&asm[func], asm, visited),
                Node::CallGlobal(index, _) => {
                    if let Some(binding) = asm.bindings.get(*index) {
                        match &binding.kind {
                            BindingKind::Const(Some(_)) => true,
                            BindingKind::Func(f) => {
                                visited.insert(f) && recurse(&asm[f], asm, visited)
                            }
                            _ => false,
                        }
                    } else {
                        false
                    }
                }
                Node::Switch { branches, .. } => {
                    branches.iter().all(|br| recurse(&br.node, asm, visited))
                }
                Node::CustomInverse(cust, _) => (cust.normal.as_ref().ok())
                    .or(cust.un.as_ref())
                    .is_some_and(|sn| recurse(&sn.node, asm, visited)),
                _ => true,
            };
            visited.truncate(len);
            is
        }
        recurse(self, asm, &mut IndexSet::new())
    }
    /// Check if the node is recursive
    pub fn is_recursive(&self, asm: &Assembly) -> bool {
        fn recurse<'a>(
            node: &'a Node,
            asm: &'a Assembly,
            visited: &mut IndexSet<&'a Function>,
        ) -> bool {
            let len = visited.len();
            let is = match node {
                Node::Run(nodes) => nodes.iter().any(|node| recurse(node, asm, visited)),
                Node::Mod(_, args, _) | Node::ImplMod(_, args, _) => {
                    args.iter().any(|sn| recurse(&sn.node, asm, visited))
                }
                Node::Call(f, _) => !visited.insert(f) || recurse(&asm[f], asm, visited),
                Node::Switch { branches, .. } => {
                    branches.iter().any(|br| recurse(&br.node, asm, visited))
                }
                Node::CustomInverse(cust, _) => (cust.normal.as_ref().ok())
                    .or(cust.un.as_ref())
                    .is_some_and(|sn| recurse(&sn.node, asm, visited)),
                Node::Array { inner, .. } => recurse(inner, asm, visited),
                _ => false,
            };
            visited.truncate(len);
            is
        }
        recurse(self, asm, &mut IndexSet::new())
    }
    /// Check if the node is callable
    pub fn check_callability<'a>(
        &'a self,
        asm: &'a Assembly,
    ) -> Result<(), (InversionError, Option<&'a Function>, Vec<usize>)> {
        fn recurse<'a>(
            node: &'a Node,
            asm: &'a Assembly,
            spans: &mut Vec<usize>,
            visited: &mut IndexSet<&'a Function>,
        ) -> Option<(InversionError, Option<&'a Function>)> {
            let len = visited.len();
            let e = match node {
                Node::Run(nodes) => nodes.iter().find_map(|n| recurse(n, asm, spans, visited)),
                Node::Call(f, span) => {
                    if visited.insert(f) {
                        recurse(&asm[f], asm, spans, visited).map(|(e, mut func)| {
                            spans.push(*span);
                            func.get_or_insert(f);
                            (e, func)
                        })
                    } else {
                        None
                    }
                }
                Node::CustomInverse(cust, span) => cust.normal.as_ref().err().cloned().map(|e| {
                    spans.push(*span);
                    (e, None)
                }),
                Node::Switch { branches, .. } => branches
                    .iter()
                    .find_map(|br| recurse(&br.node, asm, spans, visited)),
                Node::Array { inner, .. } => recurse(inner, asm, spans, visited),
                _ => None,
            };
            visited.truncate(len);
            e
        }
        let mut spans = Vec::new();
        if let Some((e, func)) = recurse(self, asm, &mut spans, &mut IndexSet::new()) {
            Err((e, func, spans))
        } else {
            Ok(())
        }
    }
}

impl Deref for Node {
    type Target = [Node];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl AsRef<[Node]> for Node {
    fn as_ref(&self) -> &[Node] {
        self.as_slice()
    }
}

impl Borrow<[Node]> for Node {
    fn borrow(&self) -> &[Node] {
        self.as_slice()
    }
}

impl<'a> IntoIterator for &'a Node {
    type Item = &'a Node;
    type IntoIter = slice::Iter<'a, Node>;
    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

impl<'a> IntoIterator for &'a mut Node {
    type Item = &'a mut Node;
    type IntoIter = slice::IterMut<'a, Node>;
    fn into_iter(self) -> Self::IntoIter {
        self.as_mut_slice().iter_mut()
    }
}

impl IntoIterator for Node {
    type Item = Node;
    type IntoIter = ecow::vec::IntoIter<Node>;
    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

macro_rules! node {
    ($(
        $(#[$attr:meta])*
        $((#[$rep_attr:meta] rep),)?
        $name:ident
        $(($($tup_name:ident($tup_type:ty)),* $(,)?))?
        $({$($field_name:ident : $field_type:ty),* $(,)?})?
    ),* $(,)?) => {
        /// A Uiua execution tree node
        ///
        /// A node is a tree structure of instructions. It can be used as both a single unit as well as a list.
        #[derive(Clone, Serialize, Deserialize)]
        #[repr(u8)]
        #[allow(missing_docs)]
        #[serde(from = "NodeRep", into = "NodeRep")]
        pub enum Node {
            $(
                $(#[$attr])*
                $name $(($($tup_type),*))? $({$($field_name : $field_type),*})?,
            )*
        }

        macro_rules! field_span {
            (span, $sp:ident) => {
                return Some($sp)
            };
            ($sp:ident, $sp2:ident) => {};
        }

        impl Node {
            /// Get the span index of this instruction
            #[allow(unreachable_code, unused)]
            pub fn span(&self) -> Option<usize> {
                if let Node::Run(nodes) = &self {
                    return nodes.iter().find_map(Node::span);
                }
                (|| match self {
                    $(
                        Self::$name $(($($tup_name),*))? $({$($field_name),*})? => {
                            $($(field_span!($tup_name, $tup_name);)*)*
                            $($(field_span!($field_name, $field_name);)*)*
                            return None;
                        },
                    )*
                })().copied()
            }
            /// Get a mutable reference to the span index of this instruction
            #[allow(unreachable_code, unused)]
            pub fn span_mut(&mut self) -> Option<&mut usize> {
                match self {
                    $(
                        Self::Run(nodes) => nodes.make_mut().iter_mut().find_map(Node::span_mut),
                        Self::$name $(($($tup_name),*))? $({$($field_name),*})? => {
                            $($(field_span!($tup_name, $tup_name);)*)*
                            $($(field_span!($field_name, $field_name);)*)*
                            return None;
                        },
                    )*
                }
            }
        }

        impl PartialEq for Node {
            #[allow(unused_variables)]
            fn eq(&self, other: &Self) -> bool {
                let mut hasher = DefaultHasher::new();
                self.hash(&mut hasher);
                let hash = hasher.finish();
                let mut other_hasher = DefaultHasher::new();
                other.hash(&mut other_hasher);
                let other_hash = other_hasher.finish();
                hash == other_hash
            }
        }

        impl Eq for Node {}

        impl Hash for Node {
            #[allow(unused_variables)]
            fn hash<H: Hasher>(&self, state: &mut H) {
                macro_rules! hash_field {
                    (span) => {};
                    ($nm:ident) => {Hash::hash($nm, state)};
                }
                match self {
                    $(
                        Self::$name $(($($tup_name),*))? $({$($field_name),*})? => {
                            discriminant(self).hash(state);
                            $($(hash_field!($field_name);)*)?
                            $($(hash_field!($tup_name);)*)?
                        }
                    )*
                }
            }
        }

        #[derive(Serialize, Deserialize)]
        #[serde(rename_all = "snake_case")]
        pub(crate) enum NodeRep {
            #[serde(rename = "e")]
            Empty(),
            $(
                $(#[$rep_attr])?
                $name(
                    $($($tup_type),*)?
                    $($($field_type),*)?
                ),
            )*
        }

        impl From<NodeRep> for Node {
            fn from(rep: NodeRep) -> Self {
                match rep {
                    NodeRep::Empty() => Self::empty(),
                    $(
                        NodeRep::$name (
                            $($($tup_name,)*)?
                            $($($field_name,)*)?
                        ) => Self::$name $(($($tup_name),*))? $({$($field_name),*})?,
                    )*
                }
            }
        }

        impl From<Node> for NodeRep {
            fn from(instr: Node) -> Self {
                match instr {
                    Node::Run(nodes) if nodes.is_empty() => NodeRep::Empty(),
                    $(
                        Node::$name $(($($tup_name),*))? $({$($field_name),*})? => NodeRep::$name (
                            $($($tup_name),*)?
                            $($($field_name),*)?
                        ),
                    )*
                }
            }
        }
    };
}
use node;
