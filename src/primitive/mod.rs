//! Primitive definitions and top-level implementations
//!
//! For the meat of the actual array algorithms, see [`crate::algorithm`].

mod defs;
pub use defs::*;

use std::{
    borrow::Cow,
    cell::RefCell,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    fmt::{self, Write},
    iter::once,
    sync::{
        atomic::{self, AtomicUsize},
        OnceLock,
    },
};

use enum_iterator::{all, Sequence};
use once_cell::sync::Lazy;
use rand::prelude::*;

use crate::{
    algorithm::{fork, loops},
    function::Function,
    lex::AsciiToken,
    run::FunctionArg,
    sys::*,
    value::*,
    Uiua, UiuaError, UiuaResult,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
pub enum PrimClass {
    Stack,
    Constant,
    MonadicPervasive,
    DyadicPervasive,
    MonadicArray,
    DyadicArray,
    IteratingModifier,
    AggregatingModifier,
    OtherModifier,
    Control,
    Misc,
    Sys,
}

impl PrimClass {
    pub fn all() -> impl Iterator<Item = Self> {
        all()
    }
    pub fn is_pervasive(&self) -> bool {
        matches!(
            self,
            PrimClass::MonadicPervasive | PrimClass::DyadicPervasive
        )
    }
    pub fn primitives(self) -> impl Iterator<Item = Primitive> {
        Primitive::all().filter(move |prim| prim.class() == self)
    }
}

/// The names of a primitive
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PrimNames {
    pub text: &'static str,
    pub ascii: Option<AsciiToken>,
    pub unicode: Option<char>,
}

impl PrimNames {
    pub fn is_name_formattable(&self) -> bool {
        self.ascii.is_none() && self.unicode.is_some_and(|c| (c as u32) > 127)
    }
}

impl From<&'static str> for PrimNames {
    fn from(text: &'static str) -> Self {
        Self {
            text,
            ascii: None,
            unicode: None,
        }
    }
}
impl From<(&'static str, char)> for PrimNames {
    fn from((text, unicode): (&'static str, char)) -> Self {
        Self {
            text,
            ascii: None,
            unicode: Some(unicode),
        }
    }
}
impl From<(&'static str, AsciiToken, char)> for PrimNames {
    fn from((text, ascii, unicode): (&'static str, AsciiToken, char)) -> Self {
        Self {
            text,
            ascii: Some(ascii),
            unicode: Some(unicode),
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(c) = self.unicode() {
            write!(f, "{}", c)
        } else if let Some(s) = self.ascii() {
            write!(f, "{}", s)
        } else if let Some(s) = self.name() {
            write!(f, "{}", s)
        } else {
            use Primitive::*;
            match self {
                InvTranspose => write!(f, "⍘{Transpose}"),
                InverseBits => write!(f, "⍘{Bits}"),
                InvTrace => write!(f, "⍘{Trace}"),
                Uncouple => write!(f, "⍘{Couple}"),
                Untake => write!(f, "⍘{Take}"),
                Undrop => write!(f, "⍘{Drop}"),
                Unselect => write!(f, "⍘{Select}"),
                Unpick => write!(f, "⍘{Pick}"),
                Cos => write!(f, "{Sin}{Add}{Eta}"),
                Asin => write!(f, "{Invert}{Sin}"),
                Acos => write!(f, "{Invert}{Cos}"),
                Last => write!(f, "{First}{Reverse}"),
                _ => write!(f, "{self:?}"),
            }
        }
    }
}

impl Primitive {
    pub fn all() -> impl Iterator<Item = Self> + Clone {
        all()
    }
    pub fn non_deprecated() -> impl Iterator<Item = Self> + Clone {
        Self::all().filter(|p| !p.is_deprecated())
    }
    pub fn name(&self) -> Option<&'static str> {
        self.names().map(|n| n.text)
    }
    pub fn ascii(&self) -> Option<AsciiToken> {
        self.names().and_then(|n| n.ascii)
    }
    pub fn unicode(&self) -> Option<char> {
        self.names().and_then(|n| n.unicode)
    }
    /// Find a primitive by its text name
    pub fn from_name(name: &str) -> Option<Self> {
        Self::all().find(|p| p.names().is_some_and(|n| n.text.eq_ignore_ascii_case(name)))
    }
    pub fn from_simple(s: AsciiToken) -> Option<Self> {
        Self::all().find(|p| p.ascii() == Some(s))
    }
    pub fn from_unicode(c: char) -> Option<Self> {
        Self::all().find(|p| p.unicode() == Some(c))
    }
    pub fn is_modifier(&self) -> bool {
        self.modifier_args().is_some()
    }
    pub(crate) fn deprecation_suggestion(&self) -> Option<String> {
        match self {
            Primitive::Roll | Primitive::Unroll => {
                Some(format!("try using dip{} instead", Primitive::Dip))
            }
            Primitive::Restack => Some(String::new()),
            _ => None,
        }
    }
    pub fn is_deprecated(&self) -> bool {
        self.deprecation_suggestion().is_some()
    }
    pub fn inverse(&self) -> Option<Self> {
        use Primitive::*;
        Some(match self {
            Noop => Noop,
            Flip => Flip,
            Neg => Neg,
            Not => Not,
            Sin => Asin,
            Cos => Acos,
            Asin => Sin,
            Acos => Cos,
            Reverse => Reverse,
            Transpose => InvTranspose,
            InvTranspose => Transpose,
            Bits => InverseBits,
            InverseBits => Bits,
            Couple => Uncouple,
            Call => Constant,
            Roll => Unroll,
            Unroll => Roll,
            Trace => InvTrace,
            InvTrace => Trace,
            _ => return None,
        })
    }
    /// Try to parse a primitive from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        if name.chars().any(char::is_uppercase) {
            return None;
        }
        if name.len() < 2 {
            return None;
        }
        let mut matching = Primitive::all().filter(|p| {
            p.names().is_some_and(|n| {
                n.is_name_formattable()
                    && n.text.starts_with(name)
                    && (name.len() >= 3 || n.text.len() < 3)
                    || n.ascii.is_none() && n.unicode.is_none() && name == n.text
            })
        });
        let res = matching.next()?;
        let exact_match = res.names().unwrap().text == name;
        (exact_match || matching.next().is_none()).then_some(res)
    }
    /// Try to parse multiple primitives from the concatenation of their name prefixes
    pub fn from_format_name_multi(name: &str) -> Option<Vec<(Self, &str)>> {
        let indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
        if indices.len() < 2 {
            return None;
        }
        let mut prims = Vec::new();
        let mut start = 0;
        'outer: loop {
            if start == indices.len() {
                break Some(prims);
            }
            for len in (2..=indices.len() - start).rev() {
                let start_index = indices[start];
                let end_index = indices.get(start + len).copied().unwrap_or(name.len());
                let sub_name = &name[start_index..end_index];
                if let Some(p) = Primitive::from_format_name(sub_name) {
                    if len >= 2 || p == Primitive::Pi {
                        prims.push((p, sub_name));
                        start += len;
                        continue 'outer;
                    }
                }
            }
            break None;
        }
    }
    pub fn as_constant(&self) -> Option<f64> {
        Some(match self {
            Primitive::Pi => PI,
            Primitive::Tau => TAU,
            Primitive::Eta => PI / 2.0,
            Primitive::Infinity => INFINITY,
            _ => return None,
        })
    }
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            Primitive::Eta => env.push(PI / 2.0),
            Primitive::Pi => env.push(PI),
            Primitive::Tau => env.push(TAU),
            Primitive::Infinity => env.push(INFINITY),
            Primitive::Noop => {}
            Primitive::Not => env.monadic_env(Value::not)?,
            Primitive::Neg => env.monadic_env(Value::neg)?,
            Primitive::Abs => env.monadic_env(Value::abs)?,
            Primitive::Sign => env.monadic_env(Value::sign)?,
            Primitive::Sqrt => env.monadic_env(Value::sqrt)?,
            Primitive::Sin => env.monadic_env(Value::sin)?,
            Primitive::Cos => env.monadic_env(Value::cos)?,
            Primitive::Asin => env.monadic_env(Value::asin)?,
            Primitive::Acos => env.monadic_env(Value::acos)?,
            Primitive::Floor => env.monadic_env(Value::floor)?,
            Primitive::Ceil => env.monadic_env(Value::ceil)?,
            Primitive::Round => env.monadic_env(Value::round)?,
            Primitive::Eq => env.dyadic_rr_env(Value::is_eq)?,
            Primitive::Ne => env.dyadic_rr_env(Value::is_ne)?,
            Primitive::Lt => env.dyadic_rr_env(Value::is_lt)?,
            Primitive::Le => env.dyadic_rr_env(Value::is_le)?,
            Primitive::Gt => env.dyadic_rr_env(Value::is_gt)?,
            Primitive::Ge => env.dyadic_rr_env(Value::is_ge)?,
            Primitive::Add => env.dyadic_rr_env(Value::add)?,
            Primitive::Sub => env.dyadic_rr_env(Value::sub)?,
            Primitive::Mul => env.dyadic_rr_env(Value::mul)?,
            Primitive::Div => env.dyadic_rr_env(Value::div)?,
            Primitive::Mod => env.dyadic_rr_env(Value::modulus)?,
            Primitive::Pow => env.dyadic_rr_env(Value::pow)?,
            Primitive::Log => env.dyadic_rr_env(Value::log)?,
            Primitive::Min => env.dyadic_rr_env(Value::min)?,
            Primitive::Max => env.dyadic_rr_env(Value::max)?,
            Primitive::Atan => env.dyadic_rr_env(Value::atan2)?,
            Primitive::Match => env.dyadic_rr(|a, b| a == b)?,
            Primitive::Join => env.dyadic_oo_env(Value::join)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::InvTranspose => env.monadic_mut(Value::inv_transpose)?,
            Primitive::Keep => env.dyadic_ro_env(Value::keep)?,
            Primitive::Unkeep => {
                let from = env.pop(1)?;
                let counts = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.unkeep(counts, into, env)?);
            }
            Primitive::Take => env.dyadic_oo_env(Value::take)?,
            Primitive::Untake => {
                let from = env.pop(1)?;
                let index = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.untake(index, into, env)?);
            }
            Primitive::Drop => env.dyadic_oo_env(Value::drop)?,
            Primitive::Undrop => {
                let from = env.pop(1)?;
                let index = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.undrop(index, into, env)?);
            }
            Primitive::Rotate => env.dyadic_ro_env(Value::rotate)?,
            Primitive::Couple => env.dyadic_oo_env(Value::couple)?,
            Primitive::Uncouple => {
                let coupled = env.pop(1)?;
                let (a, b) = coupled.uncouple(env)?;
                env.push(b);
                env.push(a);
            }
            Primitive::Grade => env.monadic_ref_env(|v, env| v.grade(env))?,
            Primitive::Pick => env.dyadic_oo_env(Value::pick)?,
            Primitive::Unpick => {
                let from = env.pop(1)?;
                let index = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.unpick(index, into, env)?);
            }
            Primitive::Select => env.dyadic_rr_env(Value::select)?,
            Primitive::Unselect => {
                let from = env.pop(1)?;
                let index = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.unselect(index, into, env)?);
            }
            Primitive::Windows => env.dyadic_rr_env(Value::windows)?,
            Primitive::Classify => env.monadic_ref_env(Value::classify)?,
            Primitive::Deduplicate => env.monadic_mut(Value::deduplicate)?,
            Primitive::Member => env.dyadic_rr_env(Value::member)?,
            Primitive::Find => env.dyadic_rr_env(Value::find)?,
            Primitive::IndexOf => env.dyadic_rr_env(Value::index_of)?,
            Primitive::Constant => {
                let val = env.pop(1)?;
                let constant = Function::constant(val);
                env.push(constant);
            }
            Primitive::Call => {
                let f = env.pop(1)?;
                let sig = f.signature();
                if !(sig.outputs == 1 || sig == (0, 0)) {
                    return Err(env.error(format!(
                        "Only functions with 1 output may be explicitly called, \
                        but the function {f} has {} outputs",
                        sig.outputs
                    )));
                }
                env.call(f)?
            }
            Primitive::Parse => env.monadic_env(|v, env| v.parse_num(env))?,
            Primitive::Range => env.monadic_ref_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_env(Value::first)?,
            Primitive::Last => env.monadic_env(Value::last)?,
            Primitive::Len => env.monadic_ref(Value::row_count)?,
            Primitive::Shape => {
                env.monadic_ref(|v| v.shape().iter().copied().collect::<Value>())?
            }
            Primitive::Bits => env.monadic_ref_env(Value::bits)?,
            Primitive::InverseBits => env.monadic_ref_env(Value::inverse_bits)?,
            Primitive::Fold => loops::fold(env)?,
            Primitive::Reduce => loops::reduce(env)?,
            Primitive::Each => loops::each(env)?,
            Primitive::Rows => loops::rows(env)?,
            Primitive::Distribute => loops::distribute(env)?,
            Primitive::Table => loops::table(env)?,
            Primitive::Cross => loops::cross(env)?,
            Primitive::Scan => loops::scan(env)?,
            Primitive::Repeat => loops::repeat(env)?,
            Primitive::Level => loops::level(env)?,
            Primitive::Group => loops::group(env)?,
            Primitive::Partition => loops::partition(env)?,
            Primitive::Reshape => {
                let shape = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.reshape(&shape, env)?;
                env.push(array);
            }
            Primitive::Break => {
                let n = env.pop(1)?.as_nat(env, "Break expects a natural number")?;
                if n > 0 {
                    return Err(UiuaError::Break(n - 1, env.span().clone()));
                }
            }
            Primitive::Recur => {
                let n = env.pop(1)?.as_nat(env, "Recur expects a natural number")?;
                env.recur(n)?;
            }
            Primitive::Dup => {
                let x = env.pop(1)?;
                env.push(x.clone());
                env.push(x);
            }
            Primitive::Flip => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push(a);
                env.push(b);
            }
            Primitive::Over => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push(b.clone());
                env.push(a);
                env.push(b);
            }
            Primitive::Pop => {
                env.pop(1)?;
            }
            Primitive::Roll => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                let c = env.pop(3)?;
                env.push(a);
                env.push(c);
                env.push(b);
            }
            Primitive::Unroll => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                let c = env.pop(3)?;
                env.push(b);
                env.push(a);
                env.push(c);
            }
            Primitive::Dip => {
                let f = env.pop(FunctionArg(1))?;
                let x = env.pop(1)?;
                env.call(f)?;
                env.push(x);
            }
            Primitive::Restack => fork::restack(env)?,
            Primitive::PushTemp => env.push_temp()?,
            Primitive::PopTemp => env.pop_temp()?,
            Primitive::Invert => {
                let f = env.pop(FunctionArg(1))?;
                let inv_f = f.invert(env)?;
                env.call(inv_f)?;
            }
            Primitive::Under => {
                let f = env.pop(FunctionArg(1))?;
                let g = env.pop(FunctionArg(2))?;
                let (f_before, f_after) = f.under(env)?;
                env.call(f_before)?;
                env.call(g)?;
                env.call(f_after)?;
            }
            Primitive::Fill => {
                let fill = env.pop(FunctionArg(1))?;
                let f = env.pop(FunctionArg(2))?;
                env.with_fill(fill, |env| env.call(f))?;
            }
            Primitive::Bind => {
                // This is only run if bind was terminated with ^ and not optimized out
                let f = env.pop(FunctionArg(1))?;
                let g = env.pop(FunctionArg(2))?;
                match (f.into_function(), g.into_function()) {
                    (Ok(f), Ok(g)) => env.push(Function::compose(f, g)),
                    (Ok(f), Err(g)) => env.push(Function::compose(f, Function::constant(g).into())),
                    (Err(f), Ok(g)) => env.push(Function::compose(Function::constant(f).into(), g)),
                    (Err(f), Err(g)) => env.push(Function::compose(
                        Function::constant(f).into(),
                        Function::constant(g).into(),
                    )),
                }
            }
            Primitive::Both => fork::both(env)?,
            Primitive::Fork => fork::fork(env)?,
            Primitive::Bracket => fork::bracket(env)?,
            Primitive::If => fork::iff(env)?,
            Primitive::Try => {
                let f = env.pop(FunctionArg(1))?;
                let handler = env.pop(FunctionArg(2))?;
                let f_args = if let Some(f) = f.as_function() {
                    f.signature().args
                } else {
                    0
                };
                let backup = env.clone_stack_top(f_args);
                let bottom = env.stack_size().saturating_sub(f_args);
                if let Err(e) = env.call(f) {
                    env.truncate_stack(bottom);
                    env.backend.save_error_color(&e);
                    for val in backup {
                        env.push(val);
                    }
                    env.push(e.value());
                    env.call(handler)?;
                }
            }
            Primitive::Assert => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if !cond.as_nat(env, "").is_ok_and(|n| n == 1) {
                    return Err(UiuaError::Throw(msg.into(), env.span().clone()));
                }
            }
            Primitive::Rand => {
                thread_local! {
                    static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::seed_from_u64(instant::now().to_bits()));
                }
                env.push(RNG.with(|rng| rng.borrow_mut().gen::<f64>()));
            }
            Primitive::Gen => {
                let seed = env.pop(1)?;
                let mut rng =
                    SmallRng::seed_from_u64(seed.as_num(env, "Gen expects a number")?.to_bits());
                let val: f64 = rng.gen();
                let next_seed = f64::from_bits(rng.gen::<u64>());
                env.push(val);
                env.push(next_seed);
            }
            Primitive::Deal => {
                let seed = env.pop(1)?.as_num(env, "Deal expects a number")?.to_bits();
                let arr = env.pop(2)?;
                let mut rows: Vec<Value> = arr.into_rows().collect();
                rows.shuffle(&mut SmallRng::seed_from_u64(seed));
                env.push(Value::from_row_values_infallible(rows));
            }
            Primitive::Use => {
                let name = env.pop(1)?.as_string(env, "Use name must be a string")?;
                let lib = env.pop(2)?;
                let f = lib
                    .as_func_array()
                    .and_then(|fs| fs.data.iter().find(|f| f.id == name.as_str()))
                    .ok_or_else(|| env.error(format!("No function found for {name:?}")))?;
                env.push(f.clone());
            }
            Primitive::Tag => {
                static NEXT_TAG: AtomicUsize = AtomicUsize::new(0);
                let tag = NEXT_TAG.fetch_add(1, atomic::Ordering::Relaxed);
                env.push(tag);
            }
            Primitive::Type => {
                let val = env.pop(1)?;
                env.push(match val {
                    Value::Num(_) | Value::Byte(_) => 0,
                    Value::Char(_) => 1,
                    Value::Func(_) => 2,
                });
            }
            Primitive::Spawn => {
                let f = env.pop("thread function")?;
                let handle = env.spawn(f.signature().args, |env| env.call(f))?;
                env.push(handle);
            }
            Primitive::Wait => {
                let handle = env.pop(1)?;
                env.wait(handle)?;
            }
            Primitive::Trace => trace(env, TraceKind::Trace)?,
            Primitive::InvTrace => trace(env, TraceKind::InverseTrace)?,
            Primitive::Dump => trace(env, TraceKind::Dump)?,
            Primitive::Sys(io) => io.run(env)?,
        }
        Ok(())
    }
}

enum TraceKind {
    Trace,
    InverseTrace,
    Dump,
}

fn trace(env: &mut Uiua, kind: TraceKind) -> UiuaResult {
    let (val, formatted) = if let TraceKind::Dump = kind {
        let vals = env.clone_stack_top(env.stack_size());
        let formatted = vals
            .iter()
            .map(|val| val.show())
            .collect::<Vec<_>>()
            .join("\n");
        (None, formatted)
    } else {
        let val = env.pop(1)?;
        let formatted = val.show();
        (Some(val), formatted)
    };
    let span = if let TraceKind::InverseTrace = kind {
        format!("{} {}", env.span(), Primitive::Invert)
    } else {
        env.span().to_string()
    };
    const MD_ARRAY_INIT: &str = "╭─";
    let message = if let Some(first_line) = formatted
        .lines()
        .next()
        .filter(|line| line.starts_with(MD_ARRAY_INIT))
    {
        let first_line = format!("{}{}", first_line.trim(), span);
        once(first_line.as_str())
            .chain(formatted.lines().skip(1))
            .fold(String::new(), |mut output, line| {
                _ = writeln!(output, "{line}");
                output
            })
    } else {
        format!("  {span}\n{formatted}\n")
    };
    if let Some(val) = val {
        env.push(val);
    }
    env.backend.print_str_trace(&message);
    Ok(())
}

#[derive(Default, Debug)]
pub struct PrimDoc {
    pub short: Vec<PrimDocFragment>,
    pub lines: Vec<PrimDocLine>,
}

impl PrimDoc {
    pub fn short_text(&self) -> Cow<str> {
        if self.short.len() == 1 {
            match &self.short[0] {
                PrimDocFragment::Text(t) => return Cow::Borrowed(t),
                PrimDocFragment::Code(c) => return Cow::Borrowed(c),
                PrimDocFragment::Emphasis(e) => return Cow::Borrowed(e),
                PrimDocFragment::Primitive { prim, named: true } => {
                    if let Some(s) = prim.name() {
                        return Cow::Owned(s.to_owned());
                    }
                }
                PrimDocFragment::Primitive { .. } => {}
            }
        }
        let mut s = String::new();
        for frag in &self.short {
            match frag {
                PrimDocFragment::Text(t) => s.push_str(t),
                PrimDocFragment::Code(c) => s.push_str(c),
                PrimDocFragment::Emphasis(e) => s.push_str(e),
                PrimDocFragment::Primitive { prim, named } => {
                    let mut name = String::new();
                    if *named {
                        s.push_str(prim.name().unwrap_or_else(|| {
                            name = format!("{prim:?}");
                            &name
                        }));
                    } else if let Some(c) = prim.unicode() {
                        s.push(c);
                    } else {
                        s.push_str(prim.name().unwrap_or_else(|| {
                            name = format!("{prim:?}");
                            &name
                        }));
                    }
                }
            }
        }
        Cow::Owned(s)
    }
    pub fn from_lines(s: &str) -> Self {
        let mut short = Vec::new();
        let mut lines = Vec::new();
        for line in s.lines() {
            let line = line.trim();
            if let Some(mut ex) = line.strip_prefix("ex:") {
                // Example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                lines.push(PrimDocLine::Example(PrimExample {
                    input: ex.into(),
                    should_error: false,
                    output: OnceLock::new(),
                }));
            } else if let Some(mut ex) = line.strip_prefix("ex!") {
                // Example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                lines.push(PrimDocLine::Example(PrimExample {
                    input: ex.into(),
                    should_error: true,
                    output: OnceLock::new(),
                }));
            } else if let Some(mut ex) = line.strip_prefix(':') {
                // Continue example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                if let Some(PrimDocLine::Example(example)) = lines.last_mut() {
                    example.input.push('\n');
                    example.input.push_str(ex);
                } else {
                    lines.push(PrimDocLine::Text(parse_doc_line_fragments(line)));
                }
            } else if short.is_empty() {
                // Set short
                short = parse_doc_line_fragments(line);
            } else {
                // Add line
                lines.push(PrimDocLine::Text(parse_doc_line_fragments(line)));
            }
        }
        while let Some(PrimDocLine::Text(frags)) = lines.first() {
            if frags.is_empty() {
                lines.remove(0);
            } else {
                break;
            }
        }
        while let Some(PrimDocLine::Text(frags)) = lines.last() {
            if frags.is_empty() {
                lines.pop();
            } else {
                break;
            }
        }
        Self { short, lines }
    }
}

#[derive(Debug)]
pub struct PrimExample {
    input: String,
    should_error: bool,
    output: OnceLock<Result<Vec<String>, String>>,
}

impl PrimExample {
    pub fn input(&self) -> &str {
        &self.input
    }
    pub fn should_error(&self) -> bool {
        self.should_error
    }
    pub fn should_run(&self) -> bool {
        !["&sl", "&tcpc"]
            .iter()
            .any(|prim| self.input.contains(prim))
    }
    pub fn output(&self) -> &Result<Vec<String>, String> {
        self.output.get_or_init(|| {
            let env = &mut Uiua::with_native_sys();
            match env.load_str(&self.input) {
                Ok(()) => Ok(env.take_stack().into_iter().map(|val| val.show()).collect()),
                Err(e) => Err(e
                    .to_string()
                    .lines()
                    .next()
                    .unwrap_or_default()
                    .split_once(' ')
                    .unwrap_or_default()
                    .1
                    .into()),
            }
        })
    }
}

#[derive(Debug)]
pub enum PrimDocLine {
    Text(Vec<PrimDocFragment>),
    Example(PrimExample),
}

#[derive(Debug, Clone)]
pub enum PrimDocFragment {
    Text(String),
    Code(String),
    Emphasis(String),
    Primitive { prim: Primitive, named: bool },
}

fn parse_doc_line_fragments(line: &str) -> Vec<PrimDocFragment> {
    let mut frags = Vec::new();
    #[derive(PartialEq, Eq)]
    enum FragKind {
        Text,
        Code,
        Emphasis,
        Primitive,
    }
    impl FragKind {
        fn open(&self) -> &str {
            match self {
                FragKind::Text => "",
                FragKind::Code => "`",
                FragKind::Emphasis => "*",
                FragKind::Primitive => "[",
            }
        }
    }
    let mut curr = String::new();
    let mut kind = FragKind::Text;
    let mut chars = line.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\\' if chars.peek() == Some(&'`') => {
                curr.push('`');
                chars.next();
            }
            '`' if kind == FragKind::Code => {
                if let Some(prim) = Primitive::from_name(&curr) {
                    frags.push(PrimDocFragment::Primitive { prim, named: false });
                } else {
                    frags.push(PrimDocFragment::Code(curr));
                }
                curr = String::new();
                kind = FragKind::Text;
            }
            '`' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Code;
            }
            '*' if kind == FragKind::Emphasis => {
                frags.push(PrimDocFragment::Emphasis(curr));
                curr = String::new();
                kind = FragKind::Text;
            }
            '*' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Emphasis;
            }
            '[' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Primitive;
            }
            ']' if kind == FragKind::Primitive => {
                if let Some(prim) = Primitive::from_name(&curr) {
                    frags.push(PrimDocFragment::Primitive { prim, named: true });
                } else {
                    frags.push(PrimDocFragment::Text(curr));
                }
                curr = String::new();
                kind = FragKind::Text;
            }
            ']' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
            }
            c => curr.push(c),
        }
    }
    curr.insert_str(0, kind.open());
    if !curr.is_empty() {
        frags.push(PrimDocFragment::Text(curr));
    }
    frags
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_collisions() {
        for a in Primitive::all() {
            for b in Primitive::all() {
                if a >= b {
                    continue;
                }
                if let Some((an, bn)) = a.name().zip(b.name()) {
                    assert_ne!(an, bn, "{:?} and {:?} have the same name", a, b)
                }
            }
        }
    }

    #[test]
    fn prim_docs() {
        for prim in Primitive::all() {
            if let Some(doc) = prim.doc() {
                for line in &doc.lines {
                    if let PrimDocLine::Example(ex) = line {
                        if !ex.should_run() {
                            continue;
                        }
                        println!("{prim} example:\n{}", ex.input);
                        if let Err(e) = Uiua::with_native_sys().load_str(&ex.input) {
                            if !ex.should_error {
                                panic!("\nExample failed:\n{}\n{}", ex.input, e.show(true));
                            }
                        } else if ex.should_error {
                            panic!("Example should have failed: {}", ex.input);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn primitive_from_name() {
        assert_eq!(Primitive::from_format_name("rev"), Some(Primitive::Reverse));
        assert_eq!(Primitive::from_format_name("re"), None);
        assert_eq!(
            Primitive::from_format_name("resh"),
            Some(Primitive::Reshape)
        );
    }

    #[test]
    fn from_multiname() {
        assert!(matches!(
            &*Primitive::from_format_name_multi("rev").expect("rev"),
            [(Primitive::Reverse, _)]
        ));
        assert!(matches!(
            &*Primitive::from_format_name_multi("revrev").expect("revrev"),
            [(Primitive::Reverse, _), (Primitive::Reverse, _)]
        ));
        assert!(matches!(
            &*Primitive::from_format_name_multi("tabkee").unwrap(),
            [(Primitive::Table, _), (Primitive::Keep, _)]
        ));
        assert_eq!(Primitive::from_format_name_multi("foo"), None);
    }

    #[cfg(test)]
    #[test]
    fn gen_grammar_file() {
        fn gen_group(prims: impl Iterator<Item = Primitive> + Clone) -> String {
            let glyphs = prims
                .clone()
                .flat_map(|p| {
                    p.unicode()
                        .into_iter()
                        .chain(p.ascii().into_iter().flat_map(|ascii| {
                            Some(ascii.to_string())
                                .filter(|s| s.len() == 1)
                                .into_iter()
                                .flat_map(|s| s.chars().collect::<Vec<_>>())
                        }))
                })
                .collect::<String>()
                .replace('\\', "\\\\\\\\")
                .replace('-', "\\\\-")
                .replace('*', "\\\\*")
                .replace('^', "\\\\^");
            let format_names: Vec<_> = prims
                .clone()
                .filter_map(|p| p.names())
                .filter(|p| p.is_name_formattable())
                .map(|n| n.text.to_string())
                .map(|name| {
                    let min_len = (2..=name.len())
                        .find(|&n| Primitive::from_format_name(&name[..n]).is_some())
                        .unwrap();
                    let mut start: String = name.chars().take(min_len).collect();
                    let mut end = String::new();
                    for c in name.chars().skip(min_len) {
                        start.push('(');
                        start.push(c);
                        end.push_str(")?");
                    }
                    format!("{}{}", start, end)
                })
                .collect();
            let format_names = format_names.join("|");
            let mut literal_names: Vec<_> = prims
                .filter_map(|p| p.names())
                .filter(|p| !p.is_name_formattable() && p.ascii.is_none() && p.unicode.is_none())
                .map(|n| format!("|{}", n.text))
                .collect();
            literal_names.sort_by_key(|s| s.len());
            literal_names.reverse();
            let literal_names = literal_names.join("");
            format!(r#"[{glyphs}]|(?<![a-zA-Z])({format_names}{literal_names})(?![a-zA-Z])"#)
        }

        let stack_functions = gen_group(Primitive::all().filter(|p| p.class() == PrimClass::Stack));
        let noadic_functions = gen_group(Primitive::all().filter(|p| {
            p.class() != PrimClass::Stack && p.modifier_args().is_none() && p.args() == Some(0)
        }));
        let monadic_functions = gen_group(Primitive::all().filter(|p| {
            p.class() != PrimClass::Stack && p.modifier_args().is_none() && p.args() == Some(1)
        }));
        let dyadic_functions = gen_group(Primitive::all().filter(|p| {
            p.class() != PrimClass::Stack && p.modifier_args().is_none() && p.args() == Some(2)
        }));
        let monadic_modifiers = gen_group(
            Primitive::all()
                .filter(|p| p.class() != PrimClass::Stack && matches!(p.modifier_args(), Some(1))),
        );
        let dyadic_modifiers: String = gen_group(Primitive::all().filter(|p| {
            p.class() != PrimClass::Stack && matches!(p.modifier_args(), Some(n) if n >= 2)
        }));

        let text = format!(
            r##"{{
	"$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
	"name": "Uiua",
	"patterns": [
		{{
			"include": "#comments"
		}},
		{{
			"include": "#strings-multiline"
		}},
		{{
			"include": "#strings-format"
		}},
		{{
			"include": "#strings-normal"
		}},
        {{
            "include": "#characters"
        }},
		{{
			"include": "#numbers"
		}},
        {{
            "include": "#strand"
        }},
		{{
			"include": "#stack"
		}},
		{{
			"include": "#noadic"
		}},
		{{
			"include": "#monadic"
		}},
		{{
			"include": "#dyadic"
		}},
		{{
			"include": "#mod1"
		}},
		{{
			"include": "#mod2"
		}},
        {{
            "include": "#idents"
        }}
	],
	"repository": {{
        "idents": {{
            "name": "variable.parameter.uiua",
            "match": "\\b[a-zA-Z]+\\b"
        }},
		"comments": {{
			"name": "comment.line.uiua",
			"match": "#.*$"
		}},
		"strings-normal": {{
			"name": "constant.character.escape",
			"begin": "\"",
			"end": "\"",
			"patterns": [
				{{
					"name": "string.quoted",
					"match": "\\\\[\\\\\"0nrt]"
				}}
			]
		}},
		"strings-format": {{
			"name": "constant.character.escape",
			"begin": "\\$\"",
			"end": "\"",
			"patterns": [
				{{
					"name": "string.quoted",
					"match": "\\\\[\\\\\"0nrt_]"
				}},
				{{
					"name": "constant.numeric",
					"match": "(?<!\\\\)_"
				}}
			]
		}},
		"strings-multiline": {{
			"name": "constant.character.escape",
			"begin": "\\$ ",
			"end": "$",
			"patterns": [
				{{
					"name": "string.quoted",
					"match": "\\\\[\\\\\"0nrt_]"
				}},
				{{
					"name": "constant.numeric",
					"match": "(?<!\\\\)_"
				}}
			]
		}},
        "characters": {{
            "name": "constant.character.escape",
            "match": "@\\\\?."
        }},
		"numbers": {{
			"name": "constant.numeric.uiua",
			"match": "\\d+(\\.\\d+(e[+-]?\\d+)?)?"
		}},
		"strand": {{
			"name": "comment.line",
			"match": "_"
		}},
        "stack": {{
            "match": "{stack_functions}"
        }},
		"noadic": {{
			"name": "entity.name.tag.uiua",
            "match": "{noadic_functions}"
        }},
		"monadic": {{
			"name": "string.quoted",
            "match": "{monadic_functions}"
        }},
		"dyadic": {{
			"name": "entity.name.function.uiua",
            "match": "{dyadic_functions}"
        }},
		"mod1": {{
			"name": "entity.name.type.uiua",
            "match": "{monadic_modifiers}"
        }},
		"mod2": {{
			"name": "keyword.control.uiua",
            "match": "{dyadic_modifiers}"
        }}
    }},
	"scopeName": "source.uiua"
}}"##
        );

        std::fs::write("uiua.tmLanguage.json", text).expect("Failed to write grammar file");
    }
}
