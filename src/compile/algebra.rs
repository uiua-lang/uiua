use std::{array, cmp::Ordering, collections::BTreeMap, fmt, mem::take, ops};

use ecow::eco_vec;
use serde::*;

use crate::{
    Assembly,
    Node::{self, *},
    Primitive::*,
    SigNode, Value,
};

pub const DEBUG: bool = false;

macro_rules! dbgln {
    ($($arg:tt)*) => {
        if DEBUG {
            println!($($arg)*); // Allow println
        }
    }
}

pub fn algebraic_inverse(nodes: &[Node], asm: &Assembly) -> Result<Node, Option<AlgebraError>> {
    dbgln!("algebraic inverse of {nodes:?}");
    let (expr, handled) = nodes_expr(nodes, asm);
    if !handled {
        return Err(None);
    }
    let mut expr = expr.inspect_err(|e| dbgln!("{e:?}")).map_err(Some)?;
    dbgln!("expression: {expr:?}");

    let c = expr.0.remove(&Term::new(Base::X, 0.0)).unwrap_or(0.0);
    let b = expr.0.remove(&Term::new(Base::X, 1.0)).unwrap_or(0.0);
    let a = (expr.0)
        .remove(&Term::new(Base::X, 2.0))
        .filter(|&a| a != 0.0);

    if !expr.0.is_empty() {
        return Err(Some(AlgebraError::TooComplex));
    }

    let span = asm.spans.len() - 1;
    let node = if let Some(a) = a {
        // Quadratic
        if b == 0.0 {
            // Simple
            Node::from_iter([
                Node::new_push(c),
                Prim(Sub, span),
                Node::new_push(a),
                Prim(Div, span),
                Prim(Sqrt, span),
            ])
        } else {
            // Full quadratic
            Node::from_iter([
                Node::new_push(c),
                Prim(Flip, span),
                Prim(Sub, span),
                Node::new_push(-4.0 * a),
                Prim(Mul, span),
                Node::new_push(b * b),
                Prim(Add, span),
                Prim(Sqrt, span),
                Prim(Dup, span),
                Node::new_push(b),
                Prim(Sub, span),
                Prim(Flip, span),
                Prim(Neg, span),
                Node::new_push(b),
                Prim(Sub, span),
                Prim(Max, span),
                Node::new_push(2.0 * a),
                Prim(Div, span),
            ])
        }
    } else if b == 0.0 {
        // Constant
        Node::from_iter([Prim(Pop, span), Node::new_push(c)])
    } else if c == 0.0 {
        // Linear origin
        if b == 1.0 {
            Prim(Identity, span)
        } else if b.abs() > 1.0 {
            Node::from_iter([Node::new_push(b), Prim(Div, span)])
        } else {
            Node::from_iter([Node::new_push(1.0 / b), Prim(Mul, span)])
        }
    } else {
        // Linear
        Node::from_iter([
            Node::new_push(c),
            Prim(Sub, span),
            Node::new_push(b),
            Prim(Div, span),
        ])
    };
    dbgln!("algebraic inverted to {node:?}");
    Ok(node)
}

pub fn derivative(node: &Node, asm: &Assembly) -> AlgebraResult<Node> {
    dbgln!("derivative of {node:?}");
    let expr = (nodes_expr(node, asm).0).inspect_err(|e| dbgln!("{e:?}"))?;
    dbgln!("experession: {expr:?}");
    let mut deriv = Expr::default();
    for (mut term, mut coef) in expr.0 {
        match term.base {
            Base::X => {}
            Base::Expr(_) => return Err(AlgebraError::TooComplex),
        }
        coef *= term.power;
        if coef == 0.0 {
            continue;
        }
        term.power -= 1.0;
        deriv.0.insert(term, coef);
    }
    dbgln!("derivative: {deriv:?}");
    let node = expr_to_node(deriv, asm);
    dbgln!("derivative node: {node:?}");
    Ok(node)
}

pub fn integral(node: &Node, asm: &Assembly) -> AlgebraResult<Node> {
    dbgln!("integral of {node:?}");
    let expr = (nodes_expr(node, asm).0).inspect_err(|e| dbgln!("{e:?}"))?;
    dbgln!("experession: {expr:?}");
    let mut deriv = Expr::default();
    for (mut term, mut coef) in expr.0 {
        match term.base {
            Base::X => {}
            Base::Expr(_) => return Err(AlgebraError::TooComplex),
        }
        term.power += 1.0;
        coef /= term.power;
        deriv.0.insert(term, coef);
    }
    dbgln!("integral: {deriv:?}");
    let node = expr_to_node(deriv, asm);
    dbgln!("integral node: {node:?}");
    Ok(node)
}

fn expr_to_node(expr: Expr, asm: &Assembly) -> Node {
    let span = asm.spans.len() - 1;
    let mut node = Node::empty();
    fn recur(node: &mut Node, expr: Expr, span: usize) {
        for (i, (term, coef)) in expr.0.into_iter().enumerate() {
            if term.power == 0.0 {
                node.push(Prim(Pop, span));
                node.push(Node::new_push(1.0));
            } else {
                match term.base {
                    Base::X => {
                        if !node.is_empty() {
                            *node = Mod(On, eco_vec![take(node).sig_node().unwrap()], span);
                        }
                    }
                    Base::Expr(expr) => recur(node, expr, span),
                }
                if term.power != 1.0 {
                    node.push(Node::new_push(term.power));
                    node.push(Prim(Pow, span));
                }
            }
            if coef != 1.0 {
                node.push(Node::new_push(coef));
                node.push(Prim(Mul, span));
            }
            if i > 0 {
                node.push(Prim(Add, span));
            }
        }
    }
    recur(&mut node, expr, span);
    node
}

fn nodes_expr(node: &[Node], asm: &Assembly) -> (AlgebraResult<Expr>, bool) {
    let mut env = AlgebraEnv::new(asm);
    for node in node {
        if let Err(e) = env.node(node) {
            let handled = env.handled >= 2 || env.stack.iter().any(Expr::is_complex);
            return (Err(e), handled);
        }
    }
    let handled = env.handled >= 2 || env.stack.iter().any(Expr::is_complex);
    (env.result(), handled)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum AlgebraError {
    TooManyVariables,
    NotSupported(String),
    NoOutput,
    TooManyOutputs,
    NonScalar,
    NonReal,
    TooComplex,
    InterpreterBug,
}

impl fmt::Display for AlgebraError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooManyVariables => write!(
                f,
                "Too many variables. The algebra system \
                only supports a single variable."
            ),
            Self::NotSupported(s) => write!(f, "The algebra system does not support {s}"),
            Self::NoOutput => write!(f, "Not enough outputs for the algebra system"),
            Self::TooManyOutputs => write!(f, "Too many outputs for the algebra system"),
            Self::NonScalar => write!(f, "The algebra system only supports scalars"),
            Self::NonReal => write!(f, "The algebra system only supports reals"),
            Self::TooComplex => write!(f, "Algebraic expression is too complex"),
            Self::InterpreterBug => write!(f, "Bug in the interpreter"),
        }
    }
}

pub type AlgebraResult<T = ()> = Result<T, AlgebraError>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Base {
    X,
    Expr(Expr),
}

#[derive(Clone)]
struct Term {
    base: Base,
    power: f64,
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.base.fmt(f)?;
        write!(f, "^{}", self.power)
    }
}

impl Term {
    fn new(base: Base, power: f64) -> Self {
        Self { base, power }
    }
}

impl From<Base> for Term {
    fn from(base: Base) -> Self {
        Self { base, power: 1.0 }
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base
            && (self.power == other.power || self.power.is_nan() == other.power.is_nan())
    }
}

impl std::cmp::Eq for Term {}

impl PartialOrd for Term {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Term {
    fn cmp(&self, other: &Self) -> Ordering {
        self.base.cmp(&other.base).then_with(|| {
            self.power
                .partial_cmp(&other.power)
                .unwrap_or_else(|| self.power.is_nan().cmp(&other.power.is_nan()))
        })
    }
}

/// A map of terms to coefficients
#[derive(Clone, Default)]
struct Expr(BTreeMap<Term, f64>);

impl Expr {
    fn is_complex(&self) -> bool {
        self.0.keys().any(|term| {
            term.power != 0.0
                || term.power != 1.0
                || matches!(&term.base, Base::Expr(expr) if expr.is_complex())
        })
    }
    fn single(self) -> Option<(Term, f64)> {
        if self.0.len() != 1 {
            return None;
        }
        self.0.into_iter().next()
    }
    fn into_constant(self) -> Option<f64> {
        let (term, coef) = self.single()?;
        if term.base == Base::X && term.power == 0.0 {
            Some(coef)
        } else {
            None
        }
    }
    fn pow(self, power: Self) -> Option<Self> {
        let power = power.into_constant()?;
        Some(Expr(
            (self.0.into_iter())
                .map(|(mut term, coef)| {
                    term.power *= power;
                    (term, coef.powf(power))
                })
                .collect(),
        ))
    }
    fn log(self, base: Self) -> Option<Self> {
        let base = base.into_constant()?;
        Some(Expr(
            (self.0.into_iter())
                .map(|(mut term, coef)| {
                    term.power /= base;
                    (term, coef.log(base))
                })
                .collect(),
        ))
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, (term, coef)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " + ")?;
            }
            if term.power == 0.0 {
                write!(f, "{coef}")?;
            } else if *coef == 1.0 {
                write!(f, "{:?}", term.base)?;
            } else if *coef == -1.0 {
                write!(f, "-{:?}", term.base)?;
            } else {
                write!(f, "{coef}{:?}", term.base)?;
            }
            if term.power != 1.0 && term.power != 0.0 {
                write!(f, "^{}", term.power)?;
            }
        }
        write!(f, ")")
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len()
            && (self.0.iter().zip(other.0.iter()))
                .all(|((a, b), (c, d))| a == c && (b == d || b.is_nan() == d.is_nan()))
    }
}

impl std::cmp::Eq for Expr {}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.len().cmp(&other.0.len()).then_with(|| {
            (self.0.iter().zip(other.0.iter()))
                .find_map(|((a, b), (c, d))| {
                    let ord = a.cmp(c).then_with(|| {
                        b.partial_cmp(d)
                            .unwrap_or_else(|| b.is_nan().cmp(&d.is_nan()))
                    });
                    Some(ord).filter(|ord| !ord.is_eq())
                })
                .unwrap_or(Ordering::Equal)
        })
    }
}

impl From<Term> for Expr {
    fn from(term: Term) -> Self {
        let mut expr = Expr::default();
        expr.0.insert(term, 1.0);
        expr
    }
}

impl From<f64> for Expr {
    fn from(val: f64) -> Self {
        let mut expr = Expr::default();
        expr.0.insert(Term::new(Base::X, 0.0), val);
        expr
    }
}

impl ops::Neg for Expr {
    type Output = Self;
    fn neg(mut self) -> Self::Output {
        for coef in self.0.values_mut() {
            *coef = -*coef;
        }
        self
    }
}

impl ops::Add for Expr {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        for (term, coef) in rhs.0 {
            *self.0.entry(term).or_default() += coef;
        }
        self
    }
}

impl ops::Sub for Expr {
    type Output = Self;
    fn sub(mut self, rhs: Self) -> Self::Output {
        for (term, coef) in rhs.0 {
            *self.0.entry(term).or_default() += -coef;
        }
        self
    }
}

impl ops::Mul for Expr {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        let mut product = Expr::default();
        for (ta, &ca) in &self.0 {
            for (tb, &cb) in &rhs.0 {
                match (ta.base.clone(), tb.base.clone()) {
                    (Base::X, Base::X) => {
                        let term = Term::new(Base::X, ta.power + tb.power);
                        *product.0.entry(term).or_default() += ca * cb;
                    }
                    (Base::X, Base::Expr(expr)) => {
                        for (mut tb, cb) in expr.0 {
                            tb.power += ta.power;
                            *product.0.entry(tb).or_default() += ca * cb;
                        }
                    }
                    (Base::Expr(expr), Base::X) => {
                        for (mut ta, ca) in expr.0 {
                            ta.power += tb.power;
                            *product.0.entry(ta).or_default() += ca * cb;
                        }
                    }
                    (Base::Expr(a), Base::Expr(b)) => {
                        let prod = a * b;
                        for (term, coef) in prod.0 {
                            *product.0.entry(term).or_default() += coef * ca * cb;
                        }
                    }
                }
            }
        }
        product
    }
}

impl ops::Div for Expr {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        let mut product = Expr::default();
        for (ta, &ca) in &self.0 {
            for (tb, &cb) in &rhs.0 {
                match (ta.base.clone(), tb.base.clone()) {
                    (Base::X, Base::X) => {
                        let term = Term::new(Base::X, ta.power - tb.power);
                        *product.0.entry(term).or_default() += ca / cb;
                    }
                    (Base::X, Base::Expr(expr)) => {
                        for (mut tb, cb) in expr.0 {
                            tb.power += ta.power;
                            *product.0.entry(tb).or_default() += ca / cb;
                        }
                    }
                    (Base::Expr(expr), Base::X) => {
                        for (mut ta, ca) in expr.0 {
                            ta.power += tb.power;
                            *product.0.entry(ta).or_default() += ca / cb;
                        }
                    }
                    (Base::Expr(a), Base::Expr(b)) => {
                        let prod = a / b;
                        for (term, coef) in prod.0 {
                            *product.0.entry(term).or_default() += coef * ca / cb;
                        }
                    }
                }
            }
        }
        product
    }
}

struct AlgebraEnv<'a> {
    asm: &'a Assembly,
    stack: Vec<Expr>,
    call_stack: Vec<usize>,
    handled: usize,
}

impl<'a> AlgebraEnv<'a> {
    fn new(asm: &'a Assembly) -> Self {
        Self {
            asm,
            stack: vec![Expr::from(Term::from(Base::X))],
            call_stack: Vec::new(),
            handled: 0,
        }
    }
    fn node(&mut self, node: &Node) -> AlgebraResult {
        let mut has_span = false;
        if let Some(span) = node.span() {
            has_span = true;
            self.call_stack.push(span);
        }
        self.node_impl(node)?;
        if has_span {
            self.call_stack.pop();
        }
        Ok(())
    }
    fn node_impl(&mut self, node: &Node) -> AlgebraResult {
        match node {
            Run(nodes) => {
                for node in nodes {
                    self.node(node)?;
                }
            }
            Call(f, _) => self.node(&self.asm[f])?,
            Push(val) if val.rank() > 0 => return Err(AlgebraError::NonScalar),
            Push(val) => match val {
                Value::Num(arr) => self.stack.push(arr.data[0].into()),
                Value::Byte(arr) => self.stack.push((arr.data[0] as f64).into()),
                _ => return Err(AlgebraError::NonReal),
            },
            Prim(prim, _) => match prim {
                Identity => {
                    let a = self.pop()?;
                    self.stack.push(a);
                }
                Dup => {
                    let a = self.pop()?;
                    self.stack.push(a.clone());
                    self.stack.push(a);
                }
                Flip => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Over => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b.clone());
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Neg => {
                    let a = self.pop()?;
                    self.stack.push(-a);
                    self.handled += 1;
                }
                Not => {
                    let a = self.pop()?;
                    self.stack.push(Expr::from(1.0) - a);
                    self.handled += 1;
                }
                Sqrt => {
                    let mut a = self.pop()?;
                    if a.0.len() <= 1 {
                        a.0 =
                            a.0.into_iter()
                                .map(|(mut term, coeff)| {
                                    term.power *= 0.5;
                                    (term, coeff.sqrt())
                                })
                                .collect();
                    } else {
                        a = Term::new(Base::Expr(a), 0.5).into();
                    }
                    self.stack.push(a);
                    self.handled += 1;
                }
                Add => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b + a);
                    self.handled += 1;
                }
                Sub => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b - a);
                    self.handled += 1;
                }
                Mul => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b * a);
                    self.handled += 1;
                }
                Div => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b / a);
                    self.handled += 1;
                }
                Pow => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = b.pow(a).ok_or(AlgebraError::NonScalar)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Log => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = b.log(a).ok_or(AlgebraError::NonScalar)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                prim => return Err(AlgebraError::NotSupported(prim.format().to_string())),
            },
            ImplPrim(prim, _) => return Err(AlgebraError::NotSupported(prim.to_string())),
            Mod(prim, args, _) => match prim {
                Pop => _ = self.pop()?,
                Dip => {
                    let [f] = get_ops(args)?;
                    let a = self.pop()?;
                    self.node(&f.node)?;
                    self.stack.push(a);
                }
                Gap => {
                    let [f] = get_ops(args)?;
                    let _a = self.pop()?;
                    self.node(&f.node)?;
                }
                On => {
                    let [f] = get_ops(args)?;
                    let a = self.pop()?;
                    self.stack.push(a.clone());
                    self.node(&f.node)?;
                    self.stack.push(a);
                }
                By => {
                    let [f] = get_ops(args)?;
                    let mut args = Vec::with_capacity(f.sig.args);
                    for _ in 0..f.sig.args {
                        args.push(self.pop()?);
                    }
                    self.stack.extend(args.last().cloned());
                    for arg in args.into_iter().rev() {
                        self.stack.push(arg);
                    }
                    self.node(&f.node)?;
                }
                Both => {
                    let [f] = get_ops(args)?;
                    let mut args = Vec::with_capacity(f.sig.args);
                    for _ in 0..f.sig.args {
                        args.push(self.pop()?);
                    }
                    self.node(&f.node)?;
                    for arg in args.into_iter().rev() {
                        self.stack.push(arg);
                    }
                    self.node(&f.node)?;
                }
                Bracket => {
                    let [f, g] = get_ops(args)?;
                    let mut args = Vec::with_capacity(f.sig.args);
                    for _ in 0..f.sig.args {
                        args.push(self.pop()?);
                    }
                    self.node(&g.node)?;
                    for arg in args.into_iter().rev() {
                        self.stack.push(arg);
                    }
                    self.node(&f.node)?;
                }
                Fork => {
                    let [f, g] = get_ops(args)?;
                    if f.sig.args > g.sig.args {
                        let mut f_args = Vec::with_capacity(f.sig.args);
                        for _ in 0..f.sig.args {
                            f_args.push(self.pop()?);
                        }
                        for arg in f_args.iter().rev().take(g.sig.args) {
                            self.stack.push(arg.clone());
                        }
                        self.node(&g.node)?;
                        for arg in f_args {
                            self.stack.push(arg);
                        }
                        self.node(&f.node)?;
                    } else {
                        let mut f_args = Vec::with_capacity(f.sig.args);
                        for _ in 0..f.sig.args {
                            f_args.push(self.pop()?);
                        }
                        for arg in f_args.iter().rev() {
                            self.stack.push(arg.clone());
                        }
                        self.node(&g.node)?;
                        for arg in f_args {
                            self.stack.push(arg);
                        }
                        self.node(&f.node)?;
                    }
                }
                prim => return Err(AlgebraError::NotSupported(prim.to_string())),
            },
            ImplMod(prim, ..) => return Err(AlgebraError::NotSupported(prim.to_string())),
            CustomInverse(..) => return Err(AlgebraError::NotSupported("custom inverses".into())),
            node => return Err(AlgebraError::NotSupported(format!("{node:?}"))),
        }
        Ok(())
    }
    fn pop(&mut self) -> AlgebraResult<Expr> {
        self.stack.pop().ok_or(AlgebraError::TooManyVariables)
    }
    fn result(mut self) -> AlgebraResult<Expr> {
        match self.stack.len() {
            0 => Err(AlgebraError::NoOutput),
            1 => Ok(self.stack.pop().unwrap()),
            _ => Err(AlgebraError::TooManyOutputs),
        }
    }
}

fn get_ops<const N: usize>(ops: &[SigNode]) -> AlgebraResult<[&SigNode; N]> {
    if ops.len() != N {
        return Err(AlgebraError::InterpreterBug);
    }
    Ok(array::from_fn(|i| &ops[i]))
}
