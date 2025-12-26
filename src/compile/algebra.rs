use core::f64;
use std::{array, cmp::Ordering, collections::BTreeMap, f64::consts::E, fmt, mem::take, ops};

use ecow::eco_vec;
use serde::*;

use crate::{
    Assembly, Complex,
    ImplPrimitive::*,
    Node::{self, *},
    Primitive::*,
    SigNode, Value,
};

pub const DEBUG: bool = false;

macro_rules! dbgln {
    ($($arg:tt)*) => {
        if DEBUG {
            eprintln!($($arg)*);
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

    let c = expr.0.remove(&Term::ONE).unwrap_or(ZERO);
    let b = expr.0.remove(&Term::X(1.0)).unwrap_or(ZERO);
    let a = (expr.0).remove(&Term::X(2.0)).filter(|&a| a != ZERO);

    let span = asm.spans.len() - 1;

    let push = |x: Complex| {
        if data.any_complex {
            Node::new_push(x)
        } else {
            Node::new_push(x.into_real().unwrap_or(f64::NAN))
        }
    };

    if !expr.0.is_empty() {
        if expr.0.len() == 1
            && b == ZERO
            && a.is_none()
            && let (Term::X(p), k) = expr.0.into_iter().next().unwrap()
        {
            // y = kx^p + c
            let mut node = Node::empty();
            if c != ZERO {
                node.push(push(c));
                node.push(Prim(Sub, span));
            }

            // The algebra system expression representation sometimes accumulates
            // floating point error when other approaches would not. This code
            // does something different depending on whether k^(1/p) seems to be
            // a roundish number with a bit of error.
            // - If it looks roundish, we do (y - c)^(1/p) / k^(1/p) with k^(1/p) getting its error fixed
            // - Otherwise, we do x = ((y - c)/k)^(1/p)
            let mut root_p_of_k = k.powc(1.0 / p);
            const ROUND_TO: f64 = 2.0 * f64::EPSILON;
            let rounded = (root_p_of_k / ROUND_TO).round() * ROUND_TO;
            let k_is_int = k.im == 0.0 && k.re.fract() == 0.0;
            let pre_calc_root = !k_is_int && root_p_of_k != rounded;
            root_p_of_k = rounded;

            if k != ONE && !pre_calc_root {
                node.push(push(k));
                node.push(Prim(Div, span));
            }

            if p == 2.0 {
                node.push(Prim(Sqrt, span));
            } else if p == 0.5 {
                node.push(Prim(Dup, span));
                node.push(Prim(Mul, span));
            } else if p == -1.0 {
                node.push(Prim(Reciprocal, span));
            } else if p != 1.0 {
                node.push(push(p.into()));
                node.push(ImplPrim(Root, span));
            }

            if k != ONE && pre_calc_root {
                node.push(push(root_p_of_k));
                node.push(Prim(Div, span));
            }
            dbgln!("algebraic inverted to {node:?}");
            return Ok(node);
        }
        return Err(Some(AlgebraError::TooComplex));
    }

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
    dbgln!("expression: {expr:?}");
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
    dbgln!("expression: {expr:?}");
    let integral = expr_integral(expr).ok_or(AlgebraError::TooComplex)?;
    dbgln!("integral: {integral:?}");
    let node = expr_to_node(integral, data.any_complex, asm);
    dbgln!("integral node: {node:?}");
    Ok(node)
}

fn expr_deriv(expr: Expr) -> Option<Expr> {
    let mut deriv = Expr::default();
    for (term, mut coef) in expr.0 {
        match term {
            Term::X(mut x) => {
                coef *= x;
                if coef == ZERO {
                    continue;
                }
                x -= 1.0;
                *deriv.0.entry(Term::X(x)).or_default() += coef;
            }
            Term::Div(expr) => {
                *deriv.0.entry(Term::Div(expr.pow(2.0.into())?)).or_default() += coef
            }
            Term::Exp(expr) => {
                let (term, coef2) = expr.single()?;
                if !matches!(term, Term::X(1.0)) {
                    return None;
                }
                *deriv.0.entry(Term::Exp(expr)).or_default() += coef * coef2;
            }
            Term::Log(base, expr) => {
                let prime = expr_deriv(expr.clone())?.as_constant()?;
                let term = Term::Div(expr);
                *deriv.0.entry(term).or_default() += coef * prime / base.ln();
            }
            Term::Sin(expr) => {
                let prime = expr_deriv(expr.clone())?.as_constant()?;
                *deriv.0.entry(Term::Cos(expr)).or_default() += coef * prime;
            }
            Term::Cos(expr) => {
                let prime = expr_deriv(expr.clone())?.as_constant()?;
                *deriv.0.entry(Term::Sin(expr)).or_default() -= coef * prime;
            }
        }
    }
    if deriv.0.is_empty() {
        deriv = 0.0.into();
    }
    Some(deriv)
}

fn expr_integral(expr: Expr) -> Option<Expr> {
    let mut deriv = Expr::default();
    for (term, mut coef) in expr.0 {
        match term {
            Term::X(mut x) => {
                x += 1.0;
                if x == 0.0 {
                    deriv.0.insert(Term::Log(E, Term::X(1.0).into()), coef);
                } else {
                    coef /= x;
                    deriv.0.insert(Term::X(x), coef);
                }
            }
            Term::Sin(expr) if expr == Term::X(1.0).into() => {
                deriv.0.insert(Term::Cos(Term::X(1.0).into()), -coef);
            }
            Term::Cos(expr) if expr == Term::X(1.0).into() => {
                deriv.0.insert(Term::Sin(Term::X(1.0).into()), coef);
            }
            _ => return None,
        }
    }
    Some(deriv)
}

fn expr_to_node(expr: Expr, any_complex: bool, asm: &Assembly) -> Node {
    let span = asm.spans.len() - 1;
    let mut node = Node::empty();
    fn recur(node: &mut Node, expr: Expr, any_complex: bool, span: usize) {
        let mut identity = false;
        for (term, coef) in expr.0 {
            if coef == ZERO {
                continue;
            }
            let is_first_term = !identity && node.is_empty();
            let mut mul_coef = coef != ONE;
            match term {
                Term::X(pow) => {
                    if !is_first_term {
                        *node = if node.is_empty() {
                            Prim(Dup, span)
                        } else {
                            Mod(On, eco_vec![take(node).sig_node().unwrap()], span)
                        };
                    }
                    match pow {
                        0.0 => {
                            node.push(Node::new_push(0));
                            node.push(Prim(Mul, span));
                            node.push(if any_complex {
                                Node::new_push(coef)
                            } else {
                                Node::new_push(coef.into_real().unwrap_or(f64::NAN))
                            });
                            node.push(Prim(Add, span));
                            mul_coef = false;
                        }
                        0.5 => node.push(Prim(Sqrt, span)),
                        1.0 => identity = true,
                        2.0 => {
                            node.push(Prim(Dup, span));
                            node.push(Prim(Mul, span));
                        }
                        _ => {
                            node.push(Node::new_push(pow));
                            node.push(Prim(Pow, span));
                        }
                    }
                }
                Term::Div(expr) if coef == Complex::ONE => {
                    recur(node, expr, any_complex, span);
                    node.push(Prim(Reciprocal, span));
                }
                Term::Div(expr) => {
                    recur(node, expr, any_complex, span);
                    node.push(Node::new_push(1.0));
                    node.push(Prim(Flip, span));
                    node.push(Prim(Div, span));
                }
                Term::Exp(expr) => {
                    recur(node, expr, any_complex, span);
                    node.push(Prim(Exp, span));
                }
                Term::Log(base, expr) => {
                    recur(node, expr, any_complex, span);
                    node.push(Node::new_push(base));
                    node.push(Prim(Log, span));
                }
                Term::Sin(expr) => {
                    recur(node, expr, any_complex, span);
                    node.push(Prim(Sin, span));
                }
                Term::Cos(expr) => {
                    recur(node, expr, any_complex, span);
                    node.push(ImplPrim(Cos, span));
                }
            }
            if mul_coef {
                node.push(if any_complex {
                    Node::new_push(coef)
                } else {
                    Node::new_push(coef.into_real().unwrap_or(f64::NAN))
                });
                node.push(Prim(Mul, span));
            }
            if !is_first_term {
                node.push(Prim(Add, span));
            }
        }
        if node.is_empty() {
            if identity {
                node.push(Prim(Identity, span))
            } else {
                node.push(Node::new_push(0));
                node.push(Prim(Mul, span));
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
            stack: vec![Expr::from(Term::X(1.0))],
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
                    self.stack.push((b * a).ok_or(AlgebraError::TooComplex)?);
                    self.handled += 1;
                }
                Div => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push((b / a).ok_or(AlgebraError::TooComplex)?);
                    self.handled += 1;
                }
                Pow => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = b.pow(a).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Log => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = b.log(a).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Exp => {
                    let a = self.pop()?;
                    let res = Expr::from(E).pow(a).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Sin => {
                    let a = self.pop()?;
                    self.stack.push(Term::Sin(a).into());
                    self.handled += 1;
                }
                Reciprocal => {
                    let a = self.pop()?;
                    self.stack
                        .push((Expr::from(1.0) / a).ok_or(AlgebraError::TooComplex)?);
                    self.handled += 1;
                }
                Complex => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (a.as_constant(), b.as_constant()) {
                        (Some(a), Some(b)) => self.stack.push((a * Complex::I + b).into()),
                        _ => {
                            let im =
                                (a * Expr::from(Complex::I)).ok_or(AlgebraError::TooComplex)?;
                            self.stack.push(b + im);
                        }
                    }
                    self.any_complex = true;
                }
                prim => return Err(AlgebraError::NotSupported(prim.format().to_string())),
            },
            ImplPrim(prim, _) => match prim {
                Cos => {
                    let a = self.pop()?;
                    self.stack.push(Term::Cos(a).into());
                    self.handled += 1;
                }
                Exp2 => {
                    let a = self.pop()?;
                    let res = Expr::from(2.0).pow(a).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Exp10 => {
                    let a = self.pop()?;
                    let res = Expr::from(10.0).pow(a).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Ln => {
                    let a = self.pop()?;
                    let res = a.log(E.into()).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Log2 => {
                    let a = self.pop()?;
                    let res = a.log(2.0.into()).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Log10 => {
                    let a = self.pop()?;
                    let res = a.log(10.0.into()).ok_or(AlgebraError::TooComplex)?;
                    self.stack.push(res);
                    self.handled += 1;
                }
                Over => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.stack.push(b.clone());
                    self.stack.push(a);
                    self.stack.push(b);
                }
                _ => return Err(AlgebraError::NotSupported("this".into())),
            },
            Mod(prim, ops, _) => match prim {
                Dip => {
                    let [f] = get_ops(ops)?;
                    let a = self.pop()?;
                    self.node(&f.node)?;
                    self.stack.push(a);
                }
                Gap => {
                    let [f] = get_ops(ops)?;
                    let _a = self.pop()?;
                    self.node(&f.node)?;
                }
                On => {
                    let [f] = get_ops(ops)?;
                    let a = self.pop()?;
                    self.stack.push(a.clone());
                    self.node(&f.node)?;
                    self.stack.push(a);
                }
                By => {
                    let [f] = get_ops(ops)?;
                    let mut args = Vec::with_capacity(f.sig.args());
                    for _ in 0..f.sig.args() {
                        args.push(self.pop()?);
                    }
                    self.stack.extend(args.last().cloned());
                    for arg in args.into_iter().rev() {
                        self.stack.push(arg);
                    }
                    self.node(&f.node)?;
                }
                Both => {
                    let [f] = get_ops(ops)?;
                    let mut args = Vec::with_capacity(f.sig.args());
                    for _ in 0..f.sig.args() {
                        args.push(self.pop()?);
                    }
                    self.node(&f.node)?;
                    for arg in args.into_iter().rev() {
                        self.stack.push(arg);
                    }
                    self.node(&f.node)?;
                }
                Bracket => {
                    let [f, g] = get_ops(ops)?;
                    let mut args = Vec::with_capacity(f.sig.args());
                    for _ in 0..f.sig.args() {
                        args.push(self.pop()?);
                    }
                    self.node(&g.node)?;
                    for arg in args.into_iter().rev() {
                        self.stack.push(arg);
                    }
                    self.node(&f.node)?;
                }
                Fork => {
                    let arg_count = ops.iter().map(|sn| sn.sig.args()).max().unwrap_or(0);
                    let mut args = Vec::with_capacity(arg_count);
                    for _ in 0..arg_count {
                        args.push(self.pop()?);
                    }
                    args.reverse();
                    for op in ops.iter().rev() {
                        for arg in args.iter().take(op.sig.args()) {
                            self.stack.push(arg.clone());
                        }
                        self.node(&op.node)?;
                    }
                }
                prim => return Err(AlgebraError::NotSupported(prim.to_string())),
            },
            ImplMod(..) => return Err(AlgebraError::NotSupported("this".into())),
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

#[allow(clippy::derive_ord_xor_partial_ord)]
#[derive(Clone, PartialEq, PartialOrd)]
enum Term {
    X(f64),
    Div(Expr),
    Log(f64, Expr),
    Exp(Expr),
    Sin(Expr),
    Cos(Expr),
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Term::ONE => write!(f, "1"),
            &Term::X(1.0) => write!(f, "x"),
            &Term::X(x) => write!(f, "x^{x}"),
            Term::Div(expr) => {
                write!(f, "1/")?;
                expr.fmt(f)
            }
            Term::Exp(expr) => {
                write!(f, "e^(")?;
                expr.fmt(f)?;
                write!(f, ")")
            }
            Term::Log(base, expr) => {
                write!(f, "log_{base}")?;
                expr.fmt(f)
            }
            Term::Sin(expr) => {
                write!(f, "sin")?;
                expr.fmt(f)
            }
            Term::Cos(expr) => {
                write!(f, "cos")?;
                expr.fmt(f)
            }
        }
    }
}

impl Term {
    const ONE: Self = Term::X(0.0);
    fn pow(self, power: f64) -> Option<Self> {
        Some(match self {
            Term::X(x) => Term::X(x * power),
            Term::Div(expr) => Term::Div(expr.pow(power.into())?),
            _ => return None,
        })
    }
}

impl std::cmp::Eq for Term {}

impl Ord for Term {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Term::X(a), Term::X(b)) => a
                .partial_cmp(b)
                .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan())),
            (a, b) => a.partial_cmp(b).unwrap(),
        }
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
        self.0.keys().any(|term| match term {
            Term::X(x) => *x != 0.0 && *x != 1.0,
            Term::Div(expr)
            | Term::Log(_, expr)
            | Term::Sin(expr)
            | Term::Cos(expr)
            | Term::Exp(expr) => expr.is_complex(),
        })
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
        if term == Term::ONE { Some(coef) } else { None }
    }
    fn pow(self, power: Self) -> Option<Self> {
        if self.as_constant().is_some_and(|c| c == E.into())
            && let Some((Term::X(1.0), _)) = power.single()
        {
            return Some(Term::Exp(power).into());
        }
        let power = power.as_constant()?.into_real()?;
        if power.fract() == 0.0 && power >= 0.0 {
            let n = power as usize;
            Some(if n == 0 {
                0.0.into()
            } else {
                let mut acc = self.clone();
                for _ in 1..n {
                    acc = (acc * self.clone())?;
                }
                acc
            })
        } else if let Some((term, coef)) = self.single() {
            Some(Expr::new_single(term.pow(power)?, coef.powf(power)))
        } else if self.0.is_empty() {
            Some(self)
        } else {
            None
        }
    }
    fn log(self, base: Self) -> Option<Self> {
        let base = base.as_constant()?.into_real()?;
        Some(Term::Log(base, self).into())
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, (term, coef)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " + ")?;
            }
            if *coef == ONE {
                write!(f, "{term:?}")?;
            } else if *coef == -ONE {
                write!(f, "-{term:?}")?;
            } else {
                write!(f, "{coef}{term:?}")?;
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
        Expr::new_single(Term::ONE, val)
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
    type Output = Option<Self>;
    fn mul(self, other: Self) -> Self::Output {
        Some(match (self, other) {
            (Term::X(a), Term::X(b)) => Term::X(a + b),
            (Term::Div(a), Term::Div(b)) => Term::Div((a * b)?),
            _ => return None,
        })
    }
}

impl ops::Mul for Expr {
    type Output = Option<Self>;
    fn mul(self, rhs: Self) -> Self::Output {
        let mut product = Expr::default();
        for (ta, &ca) in &self.0 {
            for (tb, &cb) in &rhs.0 {
                *product.0.entry((ta.clone() * tb.clone())?).or_default() += ca * cb;
            }
        }
        Some(product)
    }
}

impl ops::Div for Term {
    type Output = Option<Expr>;
    fn div(self, other: Self) -> Self::Output {
        Some(match (self, other) {
            (Term::ONE, Term::Div(b)) => b,
            (Term::ONE, Term::X(p)) => Term::X(-p).into(),
            (Term::ONE, b) => Term::Div(b.into()).into(),
            (Term::X(a), Term::X(b)) => Term::X(a - b).into(),
            (a @ Term::X(_), Term::Div(b)) => (Expr::from(a) * b)?,
            (a, b) if a == b => Term::X(1.0).into(),
            _ => return None,
        })
    }
}

impl ops::Div for Expr {
    type Output = Option<Self>;
    fn div(self, b: Self) -> Self::Output {
        if let Some((b, cb)) = b.single() {
            let mut quotient = Expr::default();
            for (ta, ca) in self.0 {
                for (term, coef) in (ta / b.clone())?.0 {
                    *quotient.0.entry(term).or_default() += ca / cb * coef;
                }
            }
            Some(quotient)
        } else if self.as_constant() == Some(ZERO) {
            Some(self)
        } else if self.as_constant() == Some(ONE) {
            Some(Term::Div(b).into())
        } else {
            None
        }
    }
}
