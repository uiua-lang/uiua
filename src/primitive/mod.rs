//! Primitive definitions and top-level implementations
//!
//! For the meat of the actual array algorithms, see [`crate::algorithm`].

mod defs;
pub use defs::*;
use ecow::EcoVec;
use regex::Regex;

use std::{
    borrow::{BorrowMut, Cow},
    cell::RefCell,
    collections::HashMap,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    fmt,
    sync::{
        atomic::{self, AtomicUsize},
        OnceLock,
    },
};

use enum_iterator::{all, Sequence};
use once_cell::sync::Lazy;
use rand::prelude::*;
use serde::*;

use crate::{
    algorithm::{self, invert, loops, reduce, table, zip},
    array::Array,
    boxed::Boxed,
    check::instrs_signature,
    lex::AsciiToken,
    sys::*,
    value::*,
    FunctionId, Signature, Uiua, UiuaError, UiuaResult,
};

/// Categories of primitives
#[derive(Clone, Copy, PartialEq, Eq, Hash, Sequence)]
#[allow(missing_docs)]
pub enum PrimClass {
    Stack,
    Constant,
    MonadicPervasive,
    DyadicPervasive,
    MonadicArray,
    DyadicArray,
    IteratingModifier,
    AggregatingModifier,
    InversionModifier,
    OtherModifier,
    Planet,
    Thread,
    Map,
    Encoding,
    Misc,
    Sys(SysOpClass),
}

impl PrimClass {
    /// Get an iterator over all primitive classes
    pub fn all() -> impl Iterator<Item = Self> {
        all()
    }
    /// Check if this class is pervasive
    pub fn is_pervasive(&self) -> bool {
        matches!(
            self,
            PrimClass::MonadicPervasive | PrimClass::DyadicPervasive
        )
    }
    /// Get an iterator over all primitives in this class
    pub fn primitives(self) -> impl Iterator<Item = Primitive> {
        Primitive::all().filter(move |prim| prim.class() == self)
    }
}

impl fmt::Debug for PrimClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PrimClass::*;
        match self {
            Stack => write!(f, "Stack"),
            Constant => write!(f, "Constant"),
            MonadicPervasive => write!(f, "MonadicPervasive"),
            DyadicPervasive => write!(f, "DyadicPervasive"),
            MonadicArray => write!(f, "MonadicArray"),
            DyadicArray => write!(f, "DyadicArray"),
            IteratingModifier => write!(f, "IteratingModifier"),
            AggregatingModifier => write!(f, "AggregatingModifier"),
            InversionModifier => write!(f, "InversionModifier"),
            OtherModifier => write!(f, "OtherModifier"),
            Planet => write!(f, "Planet"),
            Thread => write!(f, "Thread"),
            Map => write!(f, "Map"),
            Encoding => write!(f, "Encoding"),
            Misc => write!(f, "Misc"),
            Sys(op) => op.fmt(f),
        }
    }
}

/// The names of a primitive
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PrimNames {
    /// The text name
    pub text: &'static str,
    /// An ASCII token that formats to the primitive
    pub ascii: Option<AsciiToken>,
    /// The primitive's glyph
    pub glyph: Option<char>,
}

impl From<&'static str> for PrimNames {
    fn from(text: &'static str) -> Self {
        Self {
            text,
            ascii: None,
            glyph: None,
        }
    }
}
impl From<(&'static str, char)> for PrimNames {
    fn from((text, glyph): (&'static str, char)) -> Self {
        Self {
            text,
            ascii: None,
            glyph: Some(glyph),
        }
    }
}
impl From<(&'static str, AsciiToken, char)> for PrimNames {
    fn from((text, ascii, glyph): (&'static str, AsciiToken, char)) -> Self {
        Self {
            text,
            ascii: Some(ascii),
            glyph: Some(glyph),
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(c) = self.glyph() {
            write!(f, "{}", c)
        } else if let Some(s) = self.ascii() {
            write!(f, "{}", s)
        } else {
            write!(f, "{}", self.name())
        }
    }
}

impl fmt::Display for ImplPrimitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ImplPrimitive::*;
        use Primitive::*;
        match self {
            UnPop => write!(f, "{Un}{Pop}"),
            UnBits => write!(f, "{Un}{Bits}"),
            UnWhere => write!(f, "{Un}{Where}"),
            UnCouple => write!(f, "{Un}{Couple}"),
            UnMap => write!(f, "{Un}{Map}"),
            UnAtan => write!(f, "{Un}{Atan}"),
            UnComplex => write!(f, "{Un}{Complex}"),
            UnUtf => write!(f, "{Un}{Utf}"),
            UnParse => write!(f, "{Un}{Parse}"),
            UnFix => write!(f, "{Un}{Fix}"),
            UnJoin | UnJoinPattern => write!(f, "{Un}{Join}"),
            UnKeep => write!(f, "{Un}{Keep}"),
            UnScan => write!(f, "{Un}{Scan}"),
            UnTrace => write!(f, "{Un}{Trace}"),
            UnStack => write!(f, "{Un}{Stack}"),
            UnDump => write!(f, "{Un}{Dump}"),
            UnBox => write!(f, "{Un}{Box}"),
            UnJson => write!(f, "{Un}{Json}"),
            UnCsv => write!(f, "{Un}{Csv}"),
            UnXlsx => write!(f, "{Un}{Xlsx}"),
            UndoTake => write!(f, "{Under}{Take}"),
            UndoDrop => write!(f, "{Under}{Drop}"),
            UndoSelect => write!(f, "{Under}{Select}"),
            UndoPick => write!(f, "{Under}{Pick}"),
            UndoInsert => write!(f, "{Under}{Insert}"),
            UndoRemove => write!(f, "{Under}{Remove}"),
            UndoPartition1 | UndpPartition2 => write!(f, "{Under}{Partition}"),
            UndoGroup1 | UndoGroup2 => write!(f, "{Under}{Group}"),
            Asin => write!(f, "{Un}{Sin}"),
            Last => write!(f, "{First}{Reverse}"),
            UndoFix => write!(f, "{Under}{Fix}"),
            UndoFirst => write!(f, "{Under}{First}"),
            UndoLast => write!(f, "{Under}{Last}"),
            UndoKeep => write!(f, "{Under}{Keep}"),
            UndoRerank => write!(f, "{Under}{Rerank}"),
            UndoReshape => write!(f, "{Un}{Reshape}"),
            UndoJoin => write!(f, "{Under}{Join}"),
            FirstMinIndex => write!(f, "{First}{Rise}"),
            FirstMaxIndex => write!(f, "{First}{Fall}"),
            LastMinIndex => write!(f, "{First}{Reverse}{Rise}"),
            LastMaxIndex => write!(f, "{First}{Reverse}{Fall}"),
            FirstWhere => write!(f, "{First}{Where}"),
            SortUp => write!(f, "{Select}{Rise}{Dup}"),
            SortDown => write!(f, "{Select}{Fall}{Dup}"),
            Primes => write!(f, "{Un}{Reduce}{Mul}"),
            ReplaceRand => write!(f, "{Gap}{Rand}"),
            ReplaceRand2 => write!(f, "{Gap}{Gap}{Rand}"),
            ReduceContent => write!(f, "{Reduce}{Content}"),
            Adjacent => write!(f, "{Rows}{Reduce}(…){Windows}2"),
            BothTrace => write!(f, "{Both}{Trace}"),
            UnBothTrace => write!(f, "{Un}{Both}{Trace}"),
            CountUnique => write!(f, "{Len}{Deduplicate}"),
            MatchPattern => write!(f, "pattern match"),
            &ReduceDepth(n) => {
                for _ in 0..n {
                    write!(f, "{Rows}")?;
                }
                write!(f, "{Reduce}(…)")?;
                Ok(())
            }
            &TransposeN(n) => {
                if n < 0 {
                    write!(f, "{Un}")?;
                    if n < -1 {
                        write!(f, "(")?;
                    }
                }
                for _ in 0..n.unsigned_abs() {
                    write!(f, "{Transpose}")?;
                }
                if n < -1 {
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

macro_rules! constant {
    ($name:ident, $value:expr) => {
        fn $name() -> Value {
            thread_local! {
                #[allow(non_upper_case_globals)]
                static $name: Value = $value.into();
            }
            $name.with(Value::clone)
        }
    };
}

constant!(eta, PI / 2.0);
constant!(pi, PI);
constant!(tau, TAU);
constant!(inf, INFINITY);

/// A wrapper that nicely prints a `Primitive`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FormatPrimitive(pub Primitive);

impl fmt::Debug for FormatPrimitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for FormatPrimitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.glyph().is_none() {
            self.0.fmt(f)
        } else {
            write!(f, "{} {}", self.0, self.0.name())
        }
    }
}

impl Primitive {
    /// Get an iterator over all primitives
    pub fn all() -> impl Iterator<Item = Self> + Clone {
        all()
    }
    /// Get an iterator over all non-deprecated primitives
    pub fn non_deprecated() -> impl Iterator<Item = Self> + Clone {
        Self::all().filter(|p| !p.is_deprecated())
    }
    /// Get the primitive's name
    ///
    /// This is the name that is used for formatting
    pub fn name(&self) -> &'static str {
        self.names().text
    }
    /// Get the ASCII token that formats to the primitive
    pub fn ascii(&self) -> Option<AsciiToken> {
        self.names().ascii
    }
    /// Get the primitive's glyph
    pub fn glyph(&self) -> Option<char> {
        self.names().glyph
    }
    /// Find a primitive by its text name
    pub fn from_name(name: &str) -> Option<Self> {
        Self::all().find(|p| p.name() == name)
    }
    /// Find a primitive by its ASCII token
    pub fn from_ascii(s: AsciiToken) -> Option<Self> {
        Self::all().find(|p| p.ascii() == Some(s))
    }
    /// Find a primitive by its glyph
    pub fn from_glyph(c: char) -> Option<Self> {
        Self::all().find(|p| p.glyph() == Some(c))
    }
    /// Get the primitive's signature, if it is always well-defined
    pub fn signature(&self) -> Option<Signature> {
        let (args, outputs) = self.args().zip(self.outputs())?;
        Some(Signature { args, outputs })
    }
    /// Check if this primitive is a modifier
    pub fn is_modifier(&self) -> bool {
        self.modifier_args().is_some()
    }
    /// Check if this primitive is a constant
    pub fn is_constant(&self) -> bool {
        self.constant().is_some()
    }
    /// Get the a constant's value
    pub fn constant(&self) -> Option<f64> {
        use Primitive::*;
        match self {
            Eta => Some(PI / 2.0),
            Pi => Some(PI),
            Tau => Some(TAU),
            Infinity => Some(INFINITY),
            _ => None,
        }
    }
    /// Get a pretty-printable wrapper for this primitive
    pub fn format(&self) -> FormatPrimitive {
        FormatPrimitive(*self)
    }
    pub(crate) fn deprecation_suggestion(&self) -> Option<String> {
        use Primitive::*;
        Some(match self {
            Sys(SysOp::GifDecode) => format!(
                "use {} {} instead",
                Un.format(),
                Sys(SysOp::GifEncode).format()
            ),
            Sys(SysOp::AudioDecode) => format!(
                "use {} {} instead",
                Un.format(),
                Sys(SysOp::AudioEncode).format()
            ),
            Sys(SysOp::ImDecode) => format!(
                "use {} {} instead",
                Un.format(),
                Sys(SysOp::ImEncode).format()
            ),
            Deal => format!("use {Select}{Rise}[{Pop}{Repeat}{Gen}]{Len}{Over} instead"),
            _ => return None,
        })
    }
    /// Check if this primitive is experimental
    #[allow(unused_parens)]
    pub fn is_experimental(&self) -> bool {
        use Primitive::*;
        use SysOp::*;
        matches!(
            self,
            Coordinate
                | By
                | Sys(Ffi | MemCopy | MemFree | TlsListen | TlsConnect)
                | (Repr | Stringify | Quote | Sig)
        )
    }
    /// Check if this primitive is deprecated
    pub fn is_deprecated(&self) -> bool {
        self.deprecation_suggestion().is_some()
    }
    /// Try to parse a primitive from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        if name.chars().any(char::is_uppercase) {
            return None;
        }
        if name.len() < 2 {
            return None;
        }
        match name {
            "id" => return Some(Primitive::Identity),
            "ga" => return Some(Primitive::Gap),
            "po" => return Some(Primitive::Pop),
            "pi" => return Some(Primitive::Pi),
            "ran" => return Some(Primitive::Range),
            "tra" => return Some(Primitive::Transpose),
            "par" => return Some(Primitive::Partition),
            "dup" => return Some(Primitive::Dup),
            _ => {}
        }
        if let Some(prim) = Primitive::non_deprecated().find(|p| p.name() == name) {
            return Some(prim);
        }
        if let Some(prim) = Primitive::all().find(|p| p.glyph().is_none() && p.name() == name) {
            return Some(prim);
        }
        if let Some(prim) = SysOp::ALL.iter().find(|s| s.name() == name) {
            return Some(Primitive::Sys(*prim));
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching = Primitive::non_deprecated()
            .filter(|p| p.glyph().is_some_and(|u| !u.is_ascii()) && p.name().starts_with(name));
        let res = matching.next()?;
        let exact_match = res.name() == name;
        (exact_match || matching.next().is_none()).then_some(res)
    }
    /// Try to parse multiple primitives from the concatenation of their name prefixes
    pub fn from_format_name_multi(name: &str) -> Option<Vec<(Self, &str)>> {
        let mut indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
        if indices.len() < 2 {
            return None;
        }
        indices.push(name.len());
        // Forward parsing
        let mut prims = Vec::new();
        let mut start = 0;
        'outer: loop {
            if start == indices.len() {
                return Some(prims);
            }
            let start_index = indices[start];
            for len in (2..=indices.len() - start).rev() {
                let end_index = indices.get(start + len).copied().unwrap_or(name.len());
                if end_index - start_index < 2 {
                    continue;
                }
                let sub_name = &name[start_index..end_index];
                if let Some(p) = Primitive::from_format_name(sub_name) {
                    // Normal primitive matching
                    prims.push((p, sub_name));
                    start += len;
                    continue 'outer;
                }
                if sub_name
                    .strip_prefix('f')
                    .unwrap_or(sub_name)
                    .strip_suffix(['i', 'p', 'f'])
                    .unwrap_or(sub_name)
                    .chars()
                    .all(|c| "gd".contains(c))
                {
                    // 1-letter planet notation
                    for (i, c) in sub_name.char_indices() {
                        let prim = match c {
                            'f' if i == 0 => Primitive::Fork,
                            'f' => Primitive::Fix,
                            'g' => Primitive::Gap,
                            'd' => Primitive::Dip,
                            'i' => Primitive::Identity,
                            'p' => Primitive::Pop,
                            _ => unreachable!(),
                        };
                        prims.push((prim, &sub_name[i..i + 1]))
                    }
                    start += len;
                    continue 'outer;
                }
                if sub_name
                    .strip_suffix('f')
                    .unwrap_or(sub_name)
                    .chars()
                    .all(|c| c == 'd')
                {
                    // Dip fix
                    for (i, c) in sub_name.char_indices() {
                        let prim = match c {
                            'd' => Primitive::Dip,
                            'f' => Primitive::Fix,
                            _ => unreachable!(),
                        };
                        prims.push((prim, &sub_name[i..i + 1]))
                    }
                    start += len;
                    continue 'outer;
                }
            }
            break;
        }
        // Backward parsing
        prims.clear();
        let mut end = indices.len() - 1;
        'outer: loop {
            if end == 0 {
                prims.reverse();
                return Some(prims);
            }
            let end_index = indices[end];
            for len in (2..=end).rev() {
                let start_index = indices.get(end - len).copied().unwrap_or(0);
                let sub_name = &name[start_index..end_index];
                if let Some(p) = Primitive::from_format_name(sub_name) {
                    prims.push((p, sub_name));
                    end -= len;
                    continue 'outer;
                }
            }
            break;
        }
        None
    }
    /// Execute the primitive
    pub fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            Primitive::Eta => env.push(eta()),
            Primitive::Pi => env.push(pi()),
            Primitive::Tau => env.push(tau()),
            Primitive::Infinity => env.push(inf()),
            Primitive::Identity => env.touch_array_stack(1)?,
            Primitive::Not => env.monadic_env(Value::not)?,
            Primitive::Neg => env.monadic_env(Value::neg)?,
            Primitive::Abs => env.monadic_env(Value::abs)?,
            Primitive::Sign => env.monadic_env(Value::sign)?,
            Primitive::Sqrt => env.monadic_env(Value::sqrt)?,
            Primitive::Sin => env.monadic_env(Value::sin)?,
            Primitive::Floor => env.monadic_env(Value::floor)?,
            Primitive::Ceil => env.monadic_env(Value::ceil)?,
            Primitive::Round => env.monadic_env(Value::round)?,
            Primitive::Eq => env.dyadic_oo_00_env(Value::is_eq)?,
            Primitive::Ne => env.dyadic_oo_00_env(Value::is_ne)?,
            Primitive::Lt => env.dyadic_oo_00_env(Value::is_lt)?,
            Primitive::Le => env.dyadic_oo_00_env(Value::is_le)?,
            Primitive::Gt => env.dyadic_oo_00_env(Value::is_gt)?,
            Primitive::Ge => env.dyadic_oo_00_env(Value::is_ge)?,
            Primitive::Add => env.dyadic_oo_00_env(Value::add)?,
            Primitive::Sub => env.dyadic_oo_00_env(Value::sub)?,
            Primitive::Mul => env.dyadic_oo_00_env(Value::mul)?,
            Primitive::Div => env.dyadic_oo_00_env(Value::div)?,
            Primitive::Mod => env.dyadic_oo_00_env(Value::modulus)?,
            Primitive::Pow => env.dyadic_oo_00_env(Value::pow)?,
            Primitive::Log => env.dyadic_oo_00_env(Value::log)?,
            Primitive::Min => env.dyadic_oo_00_env(Value::min)?,
            Primitive::Max => env.dyadic_oo_00_env(Value::max)?,
            Primitive::Atan => env.dyadic_oo_00_env(Value::atan2)?,
            Primitive::Complex => env.dyadic_oo_00_env(Value::complex)?,
            Primitive::Match => env.dyadic_rr(|a, b| a == b)?,
            Primitive::Join => env.dyadic_oo_env(Value::join)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::Keep => env.dyadic_ro_env(Value::keep)?,
            Primitive::Take => env.dyadic_oo_env(Value::take)?,
            Primitive::Drop => env.dyadic_oo_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_ro_env(Value::rotate)?,
            Primitive::Couple => env.dyadic_oo_env(Value::couple)?,
            Primitive::Rise => env.monadic_ref_env(|v, env| v.rise(env).map(Array::from))?,
            Primitive::Fall => env.monadic_ref_env(|v, env| v.fall(env).map(Array::from))?,
            Primitive::Pick => env.dyadic_oo_env(Value::pick)?,
            Primitive::Select => env.dyadic_rr_env(Value::select)?,
            Primitive::Windows => env.dyadic_rr_env(Value::windows)?,
            Primitive::Where => env.monadic_ref_env(Value::wher)?,
            Primitive::Classify => env.monadic_ref(Value::classify)?,
            Primitive::Deduplicate => env.monadic_mut_env(Value::deduplicate)?,
            Primitive::Unique => env.monadic_ref(Value::unique)?,
            Primitive::Member => env.dyadic_rr_env(Value::member)?,
            Primitive::Find => env.dyadic_rr_env(Value::find)?,
            Primitive::Mask => env.dyadic_rr_env(Value::mask)?,
            Primitive::IndexOf => env.dyadic_rr_env(Value::index_of)?,
            Primitive::Coordinate => env.dyadic_rr_env(Value::coordinate)?,
            // Primitive::ProgressiveIndexOf => env.dyadic_rr_env(Value::progressive_index_of)?,
            Primitive::Box => {
                let val = env.pop(1)?;
                env.push(Boxed(val));
            }
            Primitive::Repr => env.monadic_ref(Value::representation)?,
            Primitive::Parse => env.monadic_ref_env(Value::parse_num)?,
            Primitive::Utf => env.monadic_ref_env(Value::utf8)?,
            Primitive::Range => env.monadic_ref_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::Fix => env.monadic_mut(Value::fix)?,
            Primitive::First => env.monadic_env(Value::first)?,
            Primitive::Len => env.monadic_ref(Value::row_count)?,
            Primitive::Shape => {
                env.monadic_ref(|v| v.shape().iter().copied().collect::<Value>())?
            }
            Primitive::Bits => env.monadic_ref_env(Value::bits)?,
            Primitive::Reduce => reduce::reduce(0, env)?,
            Primitive::Scan => reduce::scan(env)?,
            Primitive::Fold => reduce::fold(env)?,
            Primitive::Each => zip::each(env)?,
            Primitive::Rows => zip::rows(env)?,
            Primitive::Table => table::table(env)?,
            Primitive::Inventory => zip::inventory(env)?,
            Primitive::Repeat => loops::repeat(env)?,
            Primitive::Do => loops::do_(env)?,
            Primitive::Group => loops::group(env)?,
            Primitive::Partition => loops::partition(env)?,
            Primitive::Reshape => {
                let shape = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.reshape(&shape, env)?;
                env.push(array);
            }
            Primitive::Rerank => {
                let rank = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.rerank(&rank, env)?;
                env.push(array);
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
            Primitive::Fill => {
                let fill = env.pop_function()?;
                let f = env.pop_function()?;
                let outputs = fill.signature().outputs;
                if outputs > 1 {
                    return Err(env.error(format!(
                        "{} function can have at most 1 output, but its signature is {}",
                        Primitive::Fill.format(),
                        fill.signature()
                    )));
                }
                if outputs == 0 {
                    return env.without_fill_but(
                        fill.signature().args,
                        |env| env.call(fill),
                        |env| env.call(f),
                    );
                }
                env.call(fill)?;
                let fill_value = env.pop("fill value")?;
                env.with_fill(fill_value, |env| {
                    if matches!(f.id, FunctionId::Named(_)) {
                        env.use_fill();
                    }
                    env.call(f)
                })?;
            }
            Primitive::Try => algorithm::try_(env)?,
            Primitive::Assert => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if !cond.as_nat(env, "").is_ok_and(|n| n == 1) {
                    return Err(UiuaError::Throw(
                        msg.into(),
                        env.span().clone(),
                        env.inputs().clone().into(),
                    ));
                }
            }
            Primitive::Rand => env.push(random()),
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
            Primitive::Tag => {
                static NEXT_TAG: AtomicUsize = AtomicUsize::new(0);
                let tag = NEXT_TAG.fetch_add(1, atomic::Ordering::Relaxed);
                env.push(tag);
            }
            Primitive::Type => {
                let val = env.pop(1)?;
                env.push(val.type_id());
            }
            Primitive::Memo => {
                let f = env.pop_function()?;
                let sig = f.signature();
                let mut args = Vec::with_capacity(sig.args);
                for i in 0..sig.args {
                    args.push(env.pop(i + 1)?);
                }
                let mut memo = env.rt.memo.get_or_default().borrow_mut();
                if let Some(f_memo) = memo.get_mut(&f.id) {
                    if let Some(outputs) = f_memo.get(&args) {
                        let outputs = outputs.clone();
                        drop(memo);
                        for val in outputs {
                            env.push(val);
                        }
                        return Ok(());
                    }
                }
                drop(memo);
                for arg in args.iter().rev() {
                    env.push(arg.clone());
                }
                let id = f.id.clone();
                env.call(f)?;
                let outputs = env.clone_stack_top(sig.outputs)?;
                let mut memo = env.rt.memo.get_or_default().borrow_mut();
                memo.borrow_mut()
                    .entry(id)
                    .or_default()
                    .insert(args, outputs.clone());
            }
            Primitive::Spawn => {
                let f = env.pop_function()?;
                env.spawn(f.signature().args, false, |env| env.call(f))?;
            }
            Primitive::Pool => {
                let f = env.pop_function()?;
                env.spawn(f.signature().args, true, |env| env.call(f))?;
            }
            Primitive::Wait => {
                let id = env.pop(1)?;
                env.wait(id)?;
            }
            Primitive::Send => {
                let id = env.pop(1)?;
                let val = env.pop(2)?;
                env.send(id, val)?;
            }
            Primitive::Recv => {
                let id = env.pop(1)?;
                env.recv(id)?;
            }
            Primitive::TryRecv => {
                let id = env.pop(1)?;
                env.try_recv(id)?;
            }
            Primitive::Now => env.push(instant::now() / 1000.0),
            Primitive::SetInverse => {
                let f = env.pop_function()?;
                let _inv = env.pop_function()?;
                env.call(f)?;
            }
            Primitive::SetUnder => {
                let f = env.pop_function()?;
                let _before = env.pop_function()?;
                let _after = env.pop_function()?;
                env.call(f)?;
            }
            Primitive::Insert => {
                let key = env.pop("key")?;
                let val = env.pop("value")?;
                let mut map = env.pop("map")?;
                map.insert(key, val, env)?;
                env.push(map);
            }
            Primitive::Has => {
                let key = env.pop("key")?;
                let map = env.pop("map")?;
                env.push(map.has_key(&key, env)?);
            }
            Primitive::Get => {
                let key = env.pop("key")?;
                let map = env.pop("map")?;
                let val = map.get(&key, env)?;
                env.push(val);
            }
            Primitive::Remove => {
                let key = env.pop("key")?;
                let mut map = env.pop("map")?;
                map.remove(key, env)?;
                env.push(map);
            }
            Primitive::Map => {
                let keys = env.pop("keys")?;
                let mut vals = env.pop("values")?;
                vals.map(keys, env)?;
                env.push(vals);
            }
            Primitive::Trace => trace(env, false)?,
            Primitive::Stack => stack(env, false)?,
            Primitive::Dump => dump(env, false)?,
            Primitive::Regex => regex(env)?,
            Primitive::Json => env.monadic_ref_env(Value::to_json_string)?,
            Primitive::Csv => env.monadic_ref_env(Value::to_csv)?,
            Primitive::Xlsx => {
                env.monadic_ref_env(|value, env| value.to_xlsx(env).map(EcoVec::from))?
            }
            Primitive::Stringify
            | Primitive::Quote
            | Primitive::Sig
            | Primitive::Comptime
            | Primitive::Dip
            | Primitive::On
            | Primitive::By
            | Primitive::Gap
            | Primitive::Un
            | Primitive::Under
            | Primitive::Content
            | Primitive::Both
            | Primitive::Fork
            | Primitive::Bracket => {
                return Err(env.error(format!(
                    "{} was not inlined. This is a bug in the interpreter",
                    self.format()
                )))
            }
            Primitive::Sys(io) => io.run(env)?,
        }
        Ok(())
    }
}

impl ImplPrimitive {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            ImplPrimitive::UnPop => {
                let val = (env.last_fill()).ok_or_else(|| env.error("No fill set").fill())?;
                env.push(val.clone());
            }
            ImplPrimitive::Asin => env.monadic_env(Value::asin)?,
            ImplPrimitive::UndoKeep => {
                let from = env.pop(1)?;
                let counts = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.undo_keep(counts, into, env)?);
            }
            ImplPrimitive::UndoTake => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_take(index, into, env)?);
            }
            ImplPrimitive::UndoDrop => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_drop(index, into, env)?);
            }
            ImplPrimitive::UnCouple => {
                let coupled = env.pop(1)?;
                let (a, b) = coupled.uncouple(env)?;
                env.push(b);
                env.push(a);
            }
            ImplPrimitive::UnMap => {
                let map = env.pop(1)?;
                let (keys, vals) = map.unmap(env)?;
                env.push(vals);
                env.push(keys);
            }
            ImplPrimitive::UndoPick => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_pick(index, into, env)?);
            }
            ImplPrimitive::UndoSelect => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undo_select(index, into, env)?);
            }
            ImplPrimitive::UndoRerank => {
                let rank = env.pop(1)?;
                let shape = env.pop(2)?;
                let mut array = env.pop(3)?;
                array.undo_rerank(&rank, &shape, env)?;
                env.push(array);
            }
            ImplPrimitive::UndoReshape => {
                let orig_shape = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.undo_reshape(&orig_shape, env)?;
                env.push(array);
            }
            ImplPrimitive::UndoFirst => {
                let into = env.pop(1)?;
                let from = env.pop(2)?;
                env.push(from.undo_first(into, env)?);
            }
            ImplPrimitive::UndoLast => {
                let into = env.pop(1)?;
                let from = env.pop(2)?;
                env.push(from.undo_last(into, env)?);
            }
            ImplPrimitive::UnWhere => env.monadic_ref_env(Value::unwhere)?,
            ImplPrimitive::UnUtf => env.monadic_ref_env(Value::unutf8)?,
            ImplPrimitive::UnBits => env.monadic_ref_env(Value::unbits)?,
            ImplPrimitive::UndoPartition1 => loops::undo_partition_part1(env)?,
            ImplPrimitive::UndpPartition2 => loops::undo_partition_part2(env)?,
            ImplPrimitive::UndoGroup1 => loops::undo_group_part1(env)?,
            ImplPrimitive::UndoGroup2 => loops::undo_group_part2(env)?,
            ImplPrimitive::UndoJoin => {
                let a_shape = env.pop(1)?;
                let b_shape = env.pop(2)?;
                let val = env.pop(3)?;
                let (left, right) = val.undo_join(a_shape, b_shape, env)?;
                env.push(right);
                env.push(left);
            }
            ImplPrimitive::UnJoin => {
                let val = env.pop(1)?;
                let (first, rest) = val.unjoin(env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnKeep => {
                let val = env.pop(1)?;
                let (counts, dedup) = val.unkeep(env)?;
                env.push(dedup);
                env.push(counts);
            }
            ImplPrimitive::UnJoinPattern => {
                let shape = (env.pop(1))?.as_nats(env, "Shape must be natural numbers")?;
                let val = env.pop(2)?;
                let (first, rest) = val.unjoin_shape(&shape, env)?;
                env.push(rest);
                env.push(first);
            }
            ImplPrimitive::UnAtan => {
                let x = env.pop(1)?;
                let sin = x.clone().sin(env)?;
                let cos = x.cos(env)?;
                env.push(cos);
                env.push(sin);
            }
            ImplPrimitive::UnComplex => {
                let x = env.pop(1)?;
                let im = x.clone().complex_im(env)?;
                let re = x.complex_re(env)?;
                env.push(re);
                env.push(im);
            }
            ImplPrimitive::UnParse => env.monadic_ref_env(Value::unparse)?,
            ImplPrimitive::UnFix => env.monadic_mut_env(Value::unfix)?,
            ImplPrimitive::UndoFix => env.monadic_mut(Value::undo_fix)?,
            ImplPrimitive::UnScan => reduce::unscan(env)?,
            ImplPrimitive::UnTrace => trace(env, true)?,
            ImplPrimitive::BothTrace => both_trace(env, false)?,
            ImplPrimitive::UnBothTrace => both_trace(env, true)?,
            ImplPrimitive::UnStack => stack(env, true)?,
            ImplPrimitive::UnDump => dump(env, true)?,
            ImplPrimitive::Primes => env.monadic_ref_env(Value::primes)?,
            ImplPrimitive::UnBox => {
                let val = env.pop(1)?;
                env.push(val.unboxed());
            }
            ImplPrimitive::UnJson => {
                let json = env.pop(1)?.as_string(env, "JSON expects a string")?;
                let val = Value::from_json_string(&json, env)?;
                env.push(val);
            }
            ImplPrimitive::UnCsv => {
                let csv = env.pop(1)?.as_string(env, "CSV expects a string")?;
                let val = Value::from_csv(&csv, env)?;
                env.push(val);
            }
            ImplPrimitive::UnXlsx => {
                let xlsx = env.pop(1)?.as_bytes(env, "XLSX expects bytes")?;
                let val = Value::from_xlsx(&xlsx, env)?;
                env.push(val);
            }
            ImplPrimitive::UndoInsert => {
                let key = env.pop(1)?;
                let _value = env.pop(2)?;
                let original = env.pop(3)?;
                let mut map = env.pop(4)?;
                map.undo_insert(key, &original, env)?;
                env.push(map);
            }
            ImplPrimitive::UndoRemove => {
                let key = env.pop(1)?;
                let original = env.pop(2)?;
                let mut map = env.pop(3)?;
                map.undo_remove(key, &original, env)?;
                env.push(map);
            }
            // Optimizations
            ImplPrimitive::Last => env.monadic_env(Value::last)?,
            ImplPrimitive::FirstMinIndex => env.monadic_ref_env(Value::first_min_index)?,
            ImplPrimitive::FirstMaxIndex => env.monadic_ref_env(Value::first_max_index)?,
            ImplPrimitive::LastMinIndex => env.monadic_ref_env(Value::last_min_index)?,
            ImplPrimitive::LastMaxIndex => env.monadic_ref_env(Value::last_max_index)?,
            ImplPrimitive::FirstWhere => env.monadic_ref_env(Value::first_where)?,
            ImplPrimitive::SortUp => env.monadic_mut_env(Value::sort_up)?,
            ImplPrimitive::SortDown => env.monadic_mut_env(Value::sort_down)?,
            ImplPrimitive::ReduceContent => reduce::reduce_content(env)?,
            ImplPrimitive::ReplaceRand => {
                env.pop(1)?;
                env.push(random());
            }
            ImplPrimitive::ReplaceRand2 => {
                env.pop(1)?;
                env.pop(2)?;
                env.push(random());
            }
            ImplPrimitive::Adjacent => reduce::adjacent(env)?,
            ImplPrimitive::CountUnique => env.monadic_ref(Value::count_unique)?,
            ImplPrimitive::MatchPattern => invert::match_pattern(env)?,
            &ImplPrimitive::ReduceDepth(depth) => reduce::reduce(depth, env)?,
            &ImplPrimitive::TransposeN(n) => env.monadic_mut(|val| val.transpose_depth(0, n))?,
        }
        Ok(())
    }
}

fn regex(env: &mut Uiua) -> UiuaResult {
    thread_local! {
        pub static REGEX_CACHE: RefCell<HashMap<String, Regex>> = RefCell::new(HashMap::new());
    }
    let pattern = env.pop(1)?.as_string(env, "Pattern must be a string")?;
    let target = env
        .pop(1)?
        .as_string(env, "Matching target must be a string")?;
    REGEX_CACHE.with(|cache| -> UiuaResult {
        let mut cache = cache.borrow_mut();
        let regex = if let Some(regex) = cache.get(&pattern) {
            regex
        } else {
            let regex =
                Regex::new(&pattern).map_err(|e| env.error(format!("Invalid pattern: {}", e)))?;
            cache.entry(pattern.clone()).or_insert(regex.clone())
        };

        let mut matches: Value =
            Array::<Boxed>::new([0, regex.captures_len()].as_slice(), []).into();

        for caps in regex.captures_iter(&target) {
            let row: EcoVec<Boxed> = caps
                .iter()
                .flat_map(|m| {
                    m.map(|m| Boxed(Value::from(m.as_str())))
                        .or_else(|| env.value_fill().cloned().map(Value::boxed_if_not))
                })
                .collect();
            matches.append(row.into(), env)?;
        }

        env.push(matches);
        Ok(())
    })
}

/// Generate a random number, equivalent to [`Primitive::Rand`]
pub fn random() -> f64 {
    thread_local! {
        static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::from_entropy());
    }
    RNG.with(|rng| rng.borrow_mut().gen::<f64>())
}

fn trace(env: &mut Uiua, inverse: bool) -> UiuaResult {
    let val = env.pop(1)?;
    let span: String = if inverse {
        format!("{}{} {}", Primitive::Un, Primitive::Trace, env.span())
    } else {
        env.span().to_string()
    };
    let max_line_len = span.chars().count() + 2;
    let item_lines =
        format_trace_item_lines(val.show().lines().map(Into::into).collect(), max_line_len);
    env.push(val);
    env.rt.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines {
        env.rt.backend.print_str_trace(&line);
    }
    env.rt.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.rt.backend.print_str_trace("╴");
    }
    env.rt.backend.print_str_trace("\n");
    Ok(())
}

fn both_trace(env: &mut Uiua, inverse: bool) -> UiuaResult {
    let a = env.pop(1)?;
    let b = env.pop(2)?;
    let span: String = if inverse {
        format!(
            "{}{}{} {}",
            Primitive::Un,
            Primitive::Both,
            Primitive::Trace,
            env.span()
        )
    } else {
        format!("{}{} {}", Primitive::Both, Primitive::Trace, env.span())
    };
    let max_line_len = span.chars().count() + 2;
    let mut item_lines =
        format_trace_item_lines(b.show().lines().map(Into::into).collect(), max_line_len);
    item_lines.extend(format_trace_item_lines(
        a.show().lines().map(Into::into).collect(),
        max_line_len,
    ));
    env.push(b);
    env.push(a);
    env.rt.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines {
        env.rt.backend.print_str_trace(&line);
    }
    env.rt.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.rt.backend.print_str_trace("╴");
    }
    env.rt.backend.print_str_trace("\n");
    Ok(())
}

fn stack(env: &Uiua, inverse: bool) -> UiuaResult {
    let span = if inverse {
        format!("{}{} {}", Primitive::Un, Primitive::Stack, env.span())
    } else {
        format!("{} {}", Primitive::Stack, env.span())
    };
    let items = env.stack();
    let max_line_len = span.chars().count() + 2;
    let boundaries = stack_boundaries(env);
    let item_lines: Vec<Vec<String>> = items
        .iter()
        .map(Value::show)
        .map(|s| s.lines().map(Into::into).collect::<Vec<String>>())
        .map(|lines| format_trace_item_lines(lines, max_line_len))
        .enumerate()
        .flat_map(|(i, lines)| {
            if let Some((_, id)) = boundaries.iter().find(|(height, _)| i == *height) {
                vec![vec![format!("│╴╴╴{id}╶╶╶\n")], lines]
            } else {
                vec![lines]
            }
        })
        .collect();
    env.rt.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines.iter().flatten() {
        env.rt.backend.print_str_trace(line);
    }
    env.rt.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.rt.backend.print_str_trace("╴");
    }
    env.rt.backend.print_str_trace("\n");
    Ok(())
}

fn dump(env: &mut Uiua, inverse: bool) -> UiuaResult {
    let f = env.pop_function()?;
    if f.signature() != (1, 1) {
        return Err(env.error(format!(
            "Dump's function's signature must be |1.1, but it is {}",
            f.signature()
        )));
    }
    let span = if inverse {
        format!("{}{} {}", Primitive::Un, Primitive::Dump, env.span())
    } else {
        format!("{} {}", Primitive::Dump, env.span())
    };
    let unprocessed = env.stack().to_vec();
    let mut items = Vec::new();
    for item in unprocessed {
        env.push(item);
        match env.call(f.clone()) {
            Ok(()) => items.push(env.pop("dump's function's processed result")?),
            Err(e) => items.push(e.value()),
        }
    }
    let max_line_len = span.chars().count() + 2;
    let boundaries = stack_boundaries(env);
    let item_lines: Vec<Vec<String>> = items
        .iter()
        .map(Value::show)
        .map(|s| s.lines().map(Into::into).collect::<Vec<String>>())
        .map(|lines| format_trace_item_lines(lines, max_line_len))
        .enumerate()
        .flat_map(|(i, lines)| {
            if let Some((_, id)) = boundaries.iter().find(|(height, _)| i == *height) {
                vec![vec![format!("│╴╴╴{id}╶╶╶\n")], lines]
            } else {
                vec![lines]
            }
        })
        .collect();
    env.rt.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines.iter().flatten() {
        env.rt.backend.print_str_trace(line);
    }
    env.rt.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.rt.backend.print_str_trace("╴");
    }
    env.rt.backend.print_str_trace("\n");
    Ok(())
}

fn stack_boundaries(env: &Uiua) -> Vec<(usize, &FunctionId)> {
    let mut boundaries: Vec<(usize, &FunctionId)> = Vec::new();
    let mut height = 0;
    let mut reduced = 0;
    for (i, frame) in env.call_frames().rev().enumerate() {
        if i == 0 {
            let before_sig = instrs_signature(&env.instrs(frame.slice)[..frame.pc])
                .ok()
                .unwrap_or(frame.sig);
            reduced = before_sig.args as isize - before_sig.outputs as isize;
        }
        height = height.max(((frame.sig.args as isize) - reduced).max(0) as usize);
        if matches!(frame.id, FunctionId::Main) {
            break;
        }
        boundaries.push((env.stack_height().saturating_sub(height), &frame.id));
    }
    boundaries
}

fn format_trace_item_lines(mut lines: Vec<String>, mut max_line_len: usize) -> Vec<String> {
    let lines_len = lines.len();
    for (j, line) in lines.iter_mut().enumerate() {
        let stick = if lines_len == 1 || j == 1 {
            "├╴"
        } else {
            "│ "
        };
        line.insert_str(0, stick);
        max_line_len = max_line_len.max(line.chars().count());
        line.push('\n');
    }
    lines
}

/// Documentation for a primitive
#[derive(Default, Debug)]
pub struct PrimDoc {
    /// The short description
    pub short: Vec<PrimDocFragment>,
    /// The full documentation
    pub lines: Vec<PrimDocLine>,
}

impl PrimDoc {
    /// Get the primitive's short description
    pub fn short_text(&self) -> Cow<str> {
        if self.short.len() == 1 {
            match &self.short[0] {
                PrimDocFragment::Text(t) => return Cow::Borrowed(t),
                PrimDocFragment::Code(c) => return Cow::Borrowed(c),
                PrimDocFragment::Emphasis(e) => return Cow::Borrowed(e),
                PrimDocFragment::Strong(s) => return Cow::Borrowed(s),
                PrimDocFragment::Primitive { prim, named: true } => {
                    return Cow::Borrowed(prim.name());
                }
                PrimDocFragment::Link { text, .. } => return Cow::Borrowed(text),
                PrimDocFragment::Primitive { .. } => {}
            }
        }
        let mut s = String::new();
        for frag in &self.short {
            match frag {
                PrimDocFragment::Text(t) => s.push_str(t),
                PrimDocFragment::Code(c) => s.push_str(c),
                PrimDocFragment::Emphasis(e) => s.push_str(e),
                PrimDocFragment::Strong(str) => s.push_str(str),
                PrimDocFragment::Link { text, .. } => s.push_str(text),
                PrimDocFragment::Primitive { prim, named } => {
                    if *named {
                        s.push_str(prim.name());
                    } else if let Some(c) = prim.glyph() {
                        s.push(c);
                    } else {
                        s.push_str(prim.name());
                    }
                }
            }
        }
        Cow::Owned(s)
    }
    pub(crate) fn from_lines(s: &str) -> Self {
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

/// An primitive code example
#[derive(Debug)]
pub struct PrimExample {
    input: String,
    should_error: bool,
    output: OnceLock<Result<Vec<String>, String>>,
}

impl PrimExample {
    /// Get the example's source code
    pub fn input(&self) -> &str {
        &self.input
    }
    /// Check whether the example should error
    pub fn should_error(&self) -> bool {
        self.should_error
    }
    /// Get the example's output
    pub fn output(&self) -> &Result<Vec<String>, String> {
        self.output.get_or_init(|| {
            let mut env = Uiua::with_safe_sys();
            match env.run_str(&self.input) {
                Ok(_) => Ok(env.take_stack().into_iter().map(|val| val.show()).collect()),
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

/// A line in a primitive's documentation
#[derive(Debug)]
pub enum PrimDocLine {
    /// Just text
    Text(Vec<PrimDocFragment>),
    /// An example
    Example(PrimExample),
}

/// A pseudo-markdown fragment for primitive documentation
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum PrimDocFragment {
    Text(String),
    Code(String),
    Emphasis(String),
    Strong(String),
    Primitive { prim: Primitive, named: bool },
    Link { text: String, url: String },
}

fn parse_doc_line_fragments(line: &str) -> Vec<PrimDocFragment> {
    let mut frags = Vec::new();
    #[derive(PartialEq, Eq)]
    enum FragKind {
        Text,
        Code,
        Emphasis,
        Strong,
        Primitive,
    }
    impl FragKind {
        fn open(&self) -> &str {
            match self {
                FragKind::Text => "",
                FragKind::Code => "`",
                FragKind::Emphasis => "*",
                FragKind::Strong => "**",
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
            '*' if kind == FragKind::Emphasis && curr.is_empty() => {
                kind = FragKind::Strong;
            }
            '*' if kind == FragKind::Emphasis => {
                frags.push(PrimDocFragment::Emphasis(curr));
                curr = String::new();
                kind = FragKind::Text;
            }
            '*' if kind == FragKind::Strong && chars.peek() == Some(&'*') => {
                chars.next();
                frags.push(PrimDocFragment::Strong(curr));
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
            ']' if kind == FragKind::Primitive && chars.peek() == Some(&'(') => {
                chars.next();
                let mut url = String::new();
                for c in chars.by_ref() {
                    if c == ')' {
                        break;
                    }
                    url.push(c);
                }
                frags.push(PrimDocFragment::Link {
                    text: curr,
                    url: url.trim().to_owned(),
                });
                curr = String::new();
                kind = FragKind::Text;
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
                assert_ne!(a.name(), b.name(), "{a:?} and {b:?} have the same name",)
            }
        }
    }

    #[test]
    #[cfg(feature = "native_sys")]
    fn prim_docs() {
        for prim in Primitive::non_deprecated() {
            for line in &prim.doc().lines {
                if let PrimDocLine::Example(ex) = line {
                    if [
                        "&sl", "&tcpc", "&ast", "&clset", "&fo", "&fc", "&fde", "&ftr", "&fld",
                        "&fif", "&fras",
                    ]
                    .iter()
                    .any(|prim| ex.input.contains(prim))
                    {
                        continue;
                    }
                    println!("{prim} example:\n{}", ex.input);
                    match Uiua::with_safe_sys().run_str(&ex.input) {
                        Ok(mut comp) => {
                            if let Some(diag) = comp.take_diagnostics().into_iter().next() {
                                if !ex.should_error {
                                    panic!("\nExample failed:\n{}\n{}", ex.input, diag.report());
                                }
                            } else if ex.should_error {
                                panic!("Example should have failed: {}", ex.input);
                            }
                        }
                        Err(e) => {
                            if !ex.should_error {
                                panic!("\nExample failed:\n{}\n{}", ex.input, e.report());
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn primitive_from_name() {
        for prim in Primitive::non_deprecated() {
            assert_eq!(Primitive::from_name(prim.name()), Some(prim));
        }
        for (name, test) in [
            (
                "from_format_name",
                Primitive::from_format_name as fn(&str) -> Option<Primitive>,
            ),
            ("from_format_name_multi", |name| {
                Primitive::from_format_name_multi(name)
                    .unwrap()
                    .first()
                    .map(|(prim, _)| *prim)
            }),
        ] {
            for prim in Primitive::non_deprecated() {
                let char_test = match prim.glyph() {
                    None => prim.name().len(),
                    Some(c) if c.is_ascii() => continue,
                    Some(_) => 4,
                };
                let short: String = prim.name().chars().take(char_test).collect();
                assert_eq!(test(&short), Some(prim));
            }
            for prim in Primitive::non_deprecated() {
                if matches!(prim, Primitive::Rand | Primitive::Trace | Primitive::Parse) {
                    continue;
                }
                let char_test = match prim.glyph() {
                    None => prim.name().len(),
                    Some(c) if c.is_ascii() || prim.ascii().is_some() => continue,
                    Some(_) => 3,
                };
                let short: String = prim.name().chars().take(char_test).collect();
                assert_eq!(
                    test(&short),
                    Some(prim),
                    "{} does not format from {:?} with {}",
                    prim.format(),
                    short,
                    name
                );
            }
        }
        assert_eq!(Primitive::from_format_name("id"), Some(Primitive::Identity));
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
                    p.glyph()
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
                .map(|p| {
                    let name = p.name();
                    let min_len = if name.starts_with('&') {
                        name.len()
                    } else {
                        (2..=name.len())
                            .find(|&n| Primitive::from_format_name(&name[..n]) == Some(p))
                            .unwrap()
                    };
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
                .map(|p| p.names())
                .filter(|p| p.ascii.is_none() && p.glyph.is_none())
                .map(|n| format!("|{}", n.text))
                .collect();
            literal_names.sort_by_key(|s| s.len());
            literal_names.reverse();
            let literal_names = literal_names.join("");
            format!(r#"[{glyphs}]|(?<![a-zA-Z$])({format_names}{literal_names})(?![a-zA-Z])"#)
        }

        let stack_functions = gen_group(
            Primitive::non_deprecated()
                .filter(|p| p.class() == PrimClass::Stack && p.modifier_args().is_none())
                .chain(Some(Primitive::Identity)),
        );
        let noadic_functions = gen_group(Primitive::non_deprecated().filter(|p| {
            ![PrimClass::Stack, PrimClass::Constant].contains(&p.class())
                && p.modifier_args().is_none()
                && p.args() == Some(0)
        }));
        let monadic_functions = gen_group(Primitive::non_deprecated().filter(|p| {
            ![PrimClass::Stack, PrimClass::Planet].contains(&p.class())
                && p.modifier_args().is_none()
                && p.args() == Some(1)
        }));
        let dyadic_functions = gen_group(Primitive::non_deprecated().filter(|p| {
            p.class() != PrimClass::Stack && p.modifier_args().is_none() && p.args() == Some(2)
        }));
        let monadic_modifiers =
            gen_group(Primitive::non_deprecated().filter(|p| matches!(p.modifier_args(), Some(1))));
        let dyadic_modifiers: String = gen_group(
            Primitive::non_deprecated().filter(|p| matches!(p.modifier_args(), Some(n) if n >= 2)),
        );

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
            "match": "\\b[a-zA-Z]+[!‼]*\\b"
        }},
		"comments": {{
			"name": "comment.line.uiua",
			"match": "(#.*$|$[a-zA-Z]*)"
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
            "match": "@(\\\\(x[0-9A-Fa-f]{{2}}|u[0-9A-Fa-f]{{4}}|.)|.)"
        }},
		"numbers": {{
			"name": "constant.numeric.uiua",
			"match": "[`¯]?(\\d+|η|π|τ|∞|eta|pi|tau|inf(i(n(i(t(y)?)?)?)?)?)([./]\\d+|e[+-]?\\d+)?"
		}},
		"strand": {{
			"name": "comment.line",
			"match": "(_|‿)"
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
