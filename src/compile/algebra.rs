use std::{array, cmp::Ordering, collections::BTreeMap, fmt, mem::take, ops};

use ecow::eco_vec;
use serde::*;

use crate::{
    Assembly, Complex,
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

const ZERO: Complex = Complex::ZERO;
const ONE: Complex = Complex::ONE;

pub fn algebraic_inverse(nodes: &[Node], asm: &Assembly) -> Result<Node, Option<AlgebraError>> {
    dbgln!("algebraic inverse of {nodes:?}");
    let data = nodes_expr(nodes, asm);
    if !data.handled {
        return Err(None);
    }
    let mut expr = data.expr.inspect_err(|e| dbgln!("{e:?}")).map_err(Some)?;
    dbgln!("expression: {expr:?}");

    let c = expr.0.remove(&Term::new(0.0)).unwrap_or(ZERO);
    let b = expr.0.remove(&Term::new(1.0)).unwrap_or(ZERO);
    let a = (expr.0).remove(&Term::new(2.0)).filter(|&a| a != ZERO);

    if !expr.0.is_empty() {
        return Err(Some(AlgebraError::TooComplex));
    }

    let push = |x: Complex| {
        if data.any_complex {
            Node::new_push(x)
        } else {
            Node::new_push(x.into_real().unwrap_or(f64::NAN))
        }
    };

    let span = asm.spans.len() - 1;
    let node = if let Some(a) = a {
        // Quadratic
        if b == ZERO {
            // Simple
            Node::from_iter([
                push(c),
                Prim(Sub, span),
                push(a),
                Prim(Div, span),
                Prim(Sqrt, span),
            ])
        } else {
            // Full quadratic
            Node::from_iter([
                push(c),
                Prim(Flip, span),
                Prim(Sub, span),
                push(-4.0 * a),
                Prim(Mul, span),
                push(b * b),
                Prim(Add, span),
                Prim(Sqrt, span),
                Prim(Dup, span),
                push(b),
                Prim(Sub, span),
                Prim(Flip, span),
                Prim(Neg, span),
                push(b),
                Prim(Sub, span),
                Prim(Max, span),
                push(2.0 * a),
                Prim(Div, span),
            ])
        }
    } else if b == ZERO {
        // Constant
        Node::from_iter([Prim(Pop, span), Node::new_push(c)])
    } else if c == ZERO {
        // Linear origin
        if b == ONE {
            Prim(Identity, span)
        } else if b.abs() > 1.0 {
            Node::from_iter([push(b), Prim(Div, span)])
        } else {
            Node::from_iter([push(1.0 / b), Prim(Mul, span)])
        }
    } else {
        // Linear
        Node::from_iter([push(c), Prim(Sub, span), push(b), Prim(Div, span)])
    };
    dbgln!("algebraic inverted to {node:?}");
    Ok(node)
}

pub fn derivative(node: &Node, asm: &Assembly) -> AlgebraResult<Node> {
    dbgln!("derivative of {node:?}");
    let data = nodes_expr(node, asm);
    let expr = data.expr.inspect_err(|e| dbgln!("{e:?}"))?;
    dbgln!("experession: {expr:?}");
    let deriv = expr_deriv(expr).ok_or(AlgebraError::TooComplex)?;
    dbgln!("derivative: {deriv:?}");
    let node = expr_to_node(deriv, data.any_complex, asm);
    dbgln!("derivative node: {node:?}");
    Ok(node)
}

pub fn integral(node: &Node, asm: &Assembly) -> AlgebraResult<Node> {
    dbgln!("integral of {node:?}");
    let data = nodes_expr(node, asm);
    let expr = data.expr.inspect_err(|e| dbgln!("{e:?}"))?;
    dbgln!("experession: {expr:?}");
    let integral = expr_integral(expr).ok_or(AlgebraError::TooComplex)?;
    dbgln!("integral: {integral:?}");
    let node = expr_to_node(integral, data.any_complex, asm);
    dbgln!("integral node: {node:?}");
    Ok(node)
}

fn expr_deriv(expr: Expr) -> Option<Expr> {
    let mut deriv = Expr::default();
    for (mut term, mut coef) in expr.0 {
        if term.div.is_some() {
            return None;
        }

        // X
        coef *= term.x;
        if coef == ZERO {
            continue;
        }
        term.x -= 1.0;

        deriv.0.insert(term, coef);
    }
    if deriv.0.is_empty() {
        deriv = 0.0.into();
    }
    Some(deriv)
}

fn expr_integral(expr: Expr) -> Option<Expr> {
    let mut deriv = Expr::default();
    for (mut term, mut coef) in expr.0 {
        if term.div.is_some() {
            return None;
        }
        term.x += 1.0;
        coef /= term.x;
        deriv.0.insert(term, coef);
    }
    Some(deriv)
}

fn expr_to_node(expr: Expr, any_complex: bool, asm: &Assembly) -> Node {
    let span = asm.spans.len() - 1;
    let mut node = Node::empty();
    fn recur(node: &mut Node, expr: Expr, any_complex: bool, span: usize) {
        for (i, (term, coef)) in expr.0.into_iter().enumerate() {
            if coef == ZERO {
                node.push(Node::new_push(0.0));
                node.push(Prim(Mul, span));
            } else if term.x == 0.0 {
                node.push(Prim(Pop, span));
                node.push(Node::new_push(1.0));
            } else {
                if i > 0 {
                    *node = Mod(On, eco_vec![take(node).sig_node().unwrap()], span);
                }
                if term.x != 1.0 {
                    node.push(Node::new_push(term.x));
                    node.push(Prim(Pow, span));
                }
            }
            if coef != ZERO && coef != ONE {
                node.push(if any_complex {
                    Node::new_push(coef)
                } else {
                    Node::new_push(coef.into_real().unwrap_or(f64::NAN))
                });
                node.push(Prim(Mul, span));
            }
            if i > 0 {
                node.push(Prim(Add, span));
            }
        }
    }
    recur(&mut node, expr, any_complex, span);
    node
}

struct AlgebraData {
    expr: AlgebraResult<Expr>,
    handled: bool,
    any_complex: bool,
}

fn nodes_expr(node: &[Node], asm: &Assembly) -> AlgebraData {
    let mut env = AlgebraEnv::new(asm);
    for node in node {
        if let Err(e) = env.node(node) {
            let handled = env.handled >= 2 || env.stack.iter().any(Expr::is_complex);
            return AlgebraData {
                expr: Err(e),
                handled,
                any_complex: env.any_complex,
            };
        }
    }
    let handled = env.handled >= 2 || env.stack.iter().any(Expr::is_complex);
    AlgebraData {
        any_complex: env.any_complex,
        expr: env.result(),
        handled,
    }
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
    NoInverse,
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
            Self::NoInverse => write!(f, "No inverse found"),
        }
    }
}

struct AlgebraEnv<'a> {
    asm: &'a Assembly,
    stack: Vec<Expr>,
    call_stack: Vec<usize>,
    handled: usize,
    any_complex: bool,
}

impl<'a> AlgebraEnv<'a> {
    fn new(asm: &'a Assembly) -> Self {
        Self {
            asm,
            stack: vec![Expr::from(Term::new(1.0))],
            call_stack: Vec::new(),
            handled: 0,
            any_complex: false,
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
                Value::Complex(arr) => {
                    self.stack.push(arr.data[0].into());
                    self.any_complex = true;
                }
                _ => return Err(AlgebraError::NonReal),
            },
            Prim(prim, _) => match prim {
                Identity => {
                    let a = self.pop()?;
                    self.stack.push(a);
                }
                Pop => _ = self.pop()?,
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
                    let a = self.pop()?;
                    let sqrt = a.pow(0.5.into()).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(sqrt);
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
                Pow => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = b.pow(a).ok_or(AlgebraError::NonScalar)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Complex => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (a.as_constant(), b.as_constant()) {
                        (Some(a), Some(b)) => self.stack.push((a * Complex::I + b).into()),
                        _ => {
                            let im = a * Expr::from(Complex::I);
                            self.stack.push(b + im);
                        }
                    }
                    self.any_complex = true;
                }
                prim => return Err(AlgebraError::NotSupported(prim.format().to_string())),
            },
            ImplPrim(prim, _) => return Err(AlgebraError::NotSupported(prim.to_string())),
            Mod(prim, args, _) => match prim {
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
            CustomInverse(cust, _) => {
                if cust.is_obverse {
                    return Err(AlgebraError::NotSupported("custom inverses".into()));
                } else if let Ok(normal) = &cust.normal {
                    self.node(&normal.node)?;
                } else {
                    return Err(AlgebraError::NoInverse);
                }
            }
            CopyToUnder(..) | PushUnder(..) | PopUnder(..) => {}
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

pub type AlgebraResult<T = ()> = Result<T, AlgebraError>;

#[derive(Clone)]
struct Term {
    /// The power of x
    x: f64,
    /// A divided expression
    div: Option<Expr>,
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.x == 0.0 {
            write!(f, "_")
        } else if self.x == 1.0 {
            write!(f, "x")
        } else {
            write!(f, "x^{}", self.x)
        }
    }
}

impl Term {
    fn new(x: f64) -> Self {
        Self { x, div: None }
    }
    fn pow(self, power: f64) -> Self {
        Self {
            x: self.x * power,
            div: self.div.and_then(|div| div.pow(power.into())),
        }
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x || self.x.is_nan() == other.x.is_nan()
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
        self.x
            .partial_cmp(&other.x)
            .unwrap_or_else(|| self.x.is_nan().cmp(&other.x.is_nan()))
    }
}

/// A map of terms to coefficients
#[derive(Clone, Default)]
struct Expr(BTreeMap<Term, Complex>);

impl Expr {
    fn new_single(term: Term, coef: Complex) -> Self {
        let mut expr = Expr::default();
        expr.0.insert(term, coef);
        expr
    }
    fn is_complex(&self) -> bool {
        self.0.keys().any(|term| term.x != 0.0 && term.x != 1.0)
    }
    fn single(&self) -> Option<(Term, Complex)> {
        if self.0.len() != 1 {
            return None;
        }
        self.0
            .iter()
            .next()
            .map(|(term, coef)| (term.clone(), *coef))
    }
    fn as_constant(&self) -> Option<Complex> {
        let (term, coef) = self.single()?;
        if term.x == 0.0 {
            Some(coef)
        } else {
            None
        }
    }
    fn pow(self, power: Self) -> Option<Self> {
        let power = power.as_constant()?.into_real()?;
        if power.fract() == 0.0 && power >= 0.0 {
            let n = power as usize;
            Some(if n == 0 {
                0.0.into()
            } else {
                let mut acc = self.clone();
                for _ in 1..n {
                    acc = acc * self.clone();
                }
                acc
            })
        } else if let Some((term, coef)) = self.single() {
            Some(Expr::new_single(term.pow(power), coef.powf(power)))
        } else if self.0.is_empty() {
            Some(self)
        } else {
            None
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, (term, coef)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " + ")?;
            }
            if term.x == 0.0 {
                write!(f, "{coef}")?;
            } else if *coef == ONE {
                write!(f, "{:?}", term)?;
            } else if *coef == -ONE {
                write!(f, "-{term:?}")?;
            } else {
                write!(f, "{coef}{term:?}")?;
            }
            if term.x != 1.0 && term.x != 0.0 {
                write!(f, "^{}", term.x)?;
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
        expr.0.insert(term, 1.0.into());
        expr
    }
}

impl From<f64> for Expr {
    fn from(val: f64) -> Self {
        Complex::from(val).into()
    }
}

impl From<Complex> for Expr {
    fn from(val: Complex) -> Self {
        Expr::new_single(Term::new(0.0), val)
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

#[allow(clippy::suspicious_arithmetic_impl)]
impl ops::Mul for Term {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            div: match (self.div, rhs.div) {
                (None, None) => None,
                (Some(a), Some(b)) => Some(a * b),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
            },
        }
    }
}

impl ops::Mul for Expr {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        let mut product = Expr::default();
        for (ta, &ca) in &self.0 {
            for (tb, &cb) in &rhs.0 {
                *product.0.entry(ta.clone() * tb.clone()).or_default() += ca * cb;
            }
        }
        product
    }
}
