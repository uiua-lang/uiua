//! Primitive definitions and top-level implementations
//!
//! For the meat of the actual array algorithms, see [`crate::algorithm`].

mod defs;
pub use defs::*;
use ecow::EcoVec;

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    fmt::{self},
    sync::{
        atomic::{self, AtomicUsize},
        OnceLock,
    },
};

use enum_iterator::{all, Sequence};
use once_cell::sync::Lazy;
use rand::prelude::*;
use regex::Regex;

use crate::{
    algorithm::{fork, loops, reduce, table, zip},
    array::Array,
    boxed::Boxed,
    lex::AsciiToken,
    sys::*,
    value::*,
    Uiua, UiuaError, UiuaResult,
};

/// Categories of primitives
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
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
    OtherModifier,
    Control,
    Planet,
    Ocean,
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
            InvTranspose => write!(f, "⍘{Transpose}"),
            InverseBits => write!(f, "⍘{Bits}"),
            InvTrace => write!(f, "⍘{Trace}"),
            InvWhere => write!(f, "⍘{Where}"),
            InvCouple => write!(f, "⍘{Couple}"),
            Untake => write!(f, "⍘{Take}"),
            Undrop => write!(f, "⍘{Drop}"),
            Unselect => write!(f, "⍘{Select}"),
            Unpick => write!(f, "⍘{Pick}"),
            Unpartition => write!(f, "⍘{Partition}"),
            Cos => write!(f, "{Sin}{Add}{Eta}"),
            Asin => write!(f, "{Invert}{Sin}"),
            Acos => write!(f, "{Invert}{Cos}"),
            Last => write!(f, "{First}{Reverse}"),
            _ => write!(f, "{self:?}"),
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
        Self::all().find(|p| p.name().eq_ignore_ascii_case(name))
    }
    /// Find a primitive by its ASCII token
    pub fn from_ascii(s: AsciiToken) -> Option<Self> {
        Self::all().find(|p| p.ascii() == Some(s))
    }
    /// Find a primitive by its glyph
    pub fn from_glyph(c: char) -> Option<Self> {
        Self::all().find(|p| p.glyph() == Some(c))
    }
    /// Check if this primitive is a modifier
    pub fn is_modifier(&self) -> bool {
        self.modifier_args().is_some()
    }
    /// Get the value joined by an ocean function
    pub fn ocean_constant(&self) -> Option<f64> {
        use Primitive::*;
        match self {
            Rock => Some(INFINITY),
            Surface => Some(-1.0),
            Deep => Some(2.0),
            Abyss => Some(1.0),
            Seabed => Some(0.0),
            _ => None,
        }
    }
    /// Check if this primitive is an ocean function
    pub fn is_ocean(&self) -> bool {
        self.ocean_constant().is_some()
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
    pub(crate) fn deprecation_suggestion(&self) -> Option<String> {
        match self {
            Primitive::Break => Some(format!(
                "try using {}{} instead",
                Primitive::Do,
                Primitive::Do.name()
            )),
            _ => None,
        }
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
            "re" => return Some(Primitive::Reach),
            "pi" => return Some(Primitive::Pi),
            "ro" => return Some(Primitive::Rock),
            "de" => return Some(Primitive::Deep),
            "ab" => return Some(Primitive::Abyss),
            "se" => return Some(Primitive::Seabed),
            _ => {}
        }
        if let Some(prim) = Primitive::all().find(|p| p.name() == name) {
            return Some(prim);
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching = Primitive::all()
            .filter(|p| p.glyph().is_some_and(|u| !u.is_ascii()) && p.name().starts_with(name));
        let res = matching.next()?;
        let exact_match = res.name() == name;
        (exact_match || matching.next().is_none()).then_some(res)
    }
    /// Try to parse multiple primitives from the concatenation of their name prefixes
    pub fn from_format_name_multi(name: &str) -> Option<Vec<(Self, &str)>> {
        let indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
        if indices.len() < 2 {
            return None;
        }
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
                let sub_name = &name[start_index..end_index];
                if let Some(p) = Primitive::from_format_name(sub_name) {
                    // Normal primitive matching
                    prims.push((p, sub_name));
                    start += len;
                    continue 'outer;
                } else if !(sub_name.contains("rd") || sub_name.contains("rg"))
                    && (sub_name.strip_suffix('i').unwrap_or(sub_name).chars())
                        .all(|c| "gdr".contains(c))
                {
                    // 1-letter planet notation
                    for (i, c) in sub_name.char_indices() {
                        match c {
                            'g' => prims.push((Primitive::Gap, &sub_name[i..i + 1])),
                            'd' => prims.push((Primitive::Dip, &sub_name[i..i + 1])),
                            'r' => prims.push((Primitive::Reach, &sub_name[i..i + 1])),
                            'i' => prims.push((Primitive::Identity, &sub_name[i..i + 1])),
                            _ => unreachable!(),
                        }
                    }
                    start += len;
                    continue 'outer;
                }
            }
            break;
        }
        // Backward parsing
        prims.clear();
        let mut end = indices.len();
        'outer: loop {
            if end == 0 {
                prims.reverse();
                return Some(prims);
            }
            let end_index = indices[end - 1];
            for len in (2..=end).rev() {
                let start_index = indices.get(end - len).copied().unwrap_or(0);
                let sub_name = &name[start_index..=end_index];
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
            Primitive::Identity => env.touch_array_stack(),
            Primitive::Not => env.monadic_env(Value::not)?,
            Primitive::Neg => env.monadic_env(Value::neg)?,
            Primitive::Abs => env.monadic_env(Value::abs)?,
            Primitive::Sign => env.monadic_env(Value::sign)?,
            Primitive::Sqrt => env.monadic_env(Value::sqrt)?,
            Primitive::Sin => env.monadic_env(Value::sin)?,
            Primitive::Floor => env.monadic_env(Value::floor)?,
            Primitive::Ceil => env.monadic_env(Value::ceil)?,
            Primitive::Round => env.monadic_env(Value::round)?,
            Primitive::Eq => env.dyadic_oo_env(Value::is_eq)?,
            Primitive::Ne => env.dyadic_oo_env(Value::is_ne)?,
            Primitive::Lt => env.dyadic_oo_env(Value::is_lt)?,
            Primitive::Le => env.dyadic_oo_env(Value::is_le)?,
            Primitive::Gt => env.dyadic_oo_env(Value::is_gt)?,
            Primitive::Ge => env.dyadic_oo_env(Value::is_ge)?,
            Primitive::Add => env.dyadic_oo_env(Value::add)?,
            Primitive::Sub => env.dyadic_oo_env(Value::sub)?,
            Primitive::Mul => env.dyadic_oo_env(Value::mul)?,
            Primitive::Div => env.dyadic_oo_env(Value::div)?,
            Primitive::Mod => env.dyadic_oo_env(Value::modulus)?,
            Primitive::Pow => env.dyadic_oo_env(Value::pow)?,
            Primitive::Log => env.dyadic_oo_env(Value::log)?,
            Primitive::Min => env.dyadic_oo_env(Value::min)?,
            Primitive::Max => env.dyadic_oo_env(Value::max)?,
            Primitive::Atan => env.dyadic_oo_env(Value::atan2)?,
            Primitive::Complex => env.dyadic_oo_env(Value::complex)?,
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
            Primitive::Classify => env.monadic_ref_env(Value::classify)?,
            Primitive::Deduplicate => env.monadic_mut(Value::deduplicate)?,
            Primitive::Member => env.dyadic_rr_env(Value::member)?,
            Primitive::Find => env.dyadic_rr_env(Value::find)?,
            Primitive::IndexOf => env.dyadic_rr_env(Value::index_of)?,
            Primitive::Box => {
                let val = env.pop(1)?;
                env.push(Boxed(val));
            }
            Primitive::Unbox => {
                let val = match env.pop(1)? {
                    Value::Box(boxed) => match boxed.into_scalar() {
                        Ok(scalar) => scalar.0,
                        Err(boxed) => Value::Box(boxed),
                    },
                    val => val,
                };
                env.push(val);
            }
            Primitive::Parse => env.monadic_ref_env(Value::parse_num)?,
            Primitive::Utf => env.monadic_ref_env(Value::utf8)?,
            Primitive::Range => env.monadic_ref_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_env(Value::first)?,
            Primitive::Len => env.monadic_ref(|val| {
                val.generic_ref_deep(
                    Array::row_count,
                    Array::row_count,
                    Array::row_count,
                    Array::row_count,
                    Array::row_count,
                )
            })?,
            Primitive::Shape => env.monadic_ref(|v| {
                v.generic_ref_deep(
                    Array::shape,
                    Array::shape,
                    Array::shape,
                    Array::shape,
                    Array::shape,
                )
                .iter()
                .copied()
                .collect::<Value>()
            })?,
            Primitive::Bits => env.monadic_ref_env(Value::bits)?,
            Primitive::Reduce => reduce::reduce(env)?,
            Primitive::Scan => reduce::scan(env)?,
            Primitive::Fold => reduce::fold(env)?,
            Primitive::Each => zip::each(env)?,
            Primitive::Rows => zip::rows(env)?,
            Primitive::Distribute => zip::distribute(env)?,
            Primitive::Tribute => zip::tribute(env)?,
            Primitive::Level => zip::level(env)?,
            Primitive::Table => table::table(env)?,
            Primitive::Cross => table::cross(env)?,
            Primitive::Combinate => table::combinate(env)?,
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
            Primitive::Break => {
                let n = env.pop(1)?.as_nat(env, "Break expects a natural number")?;
                if n > 0 {
                    return Err(UiuaError::Break(n - 1, env.span().clone()));
                }
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
            Primitive::Dip => {
                // Dip is inlined, so this should never actually run
                let f = env.pop_function()?;
                let x = env.pop(1)?;
                env.call(f)?;
                env.push(x);
            }
            Primitive::Gap => {
                // Gap is inlined, so this should never actually run
                let f = env.pop_function()?;
                let _x = env.pop(2)?;
                env.call(f)?;
            }
            Primitive::Reach => {
                // Reach is inlined, so this should never actually run
                let f = env.pop_function()?;
                let x = env.pop(1)?;
                let _y = env.pop(2)?;
                env.push(x);
                env.call(f)?;
            }
            Primitive::Rock => {
                let x = env.pop(1)?;
                env.push(x.ocean(INFINITY, env)?);
            }
            Primitive::Surface => {
                let x = env.pop(1)?;
                env.push(x.ocean(-1.0, env)?);
            }
            Primitive::Deep => {
                let x = env.pop(1)?;
                env.push(x.ocean(2.0, env)?);
            }
            Primitive::Abyss => {
                let x = env.pop(1)?;
                env.push(x.ocean(1.0, env)?);
            }
            Primitive::Seabed => {
                let x = env.pop(1)?;
                env.push(x.ocean(0.0, env)?);
            }
            Primitive::Invert => {
                let f = env.pop_function()?;
                let inv_f = f.invert("", env)?;
                env.call(inv_f)?;
            }
            Primitive::Under => {
                let f = env.pop_function()?;
                let g = env.pop_function()?;
                let (f_before, f_after) = f.undered(g.signature(), env)?;
                env.call(f_before)?;
                env.call(g)?;
                env.call(f_after)?;
            }
            Primitive::Pack => {
                let f = env.pop_function()?;
                env.with_pack(|env| env.call(f))?;
            }
            Primitive::Fill => {
                let fill = env.pop_function()?;
                let f = env.pop_function()?;
                env.call(fill)?;
                let fill_value = env.pop("fill value")?;
                env.with_fill(fill_value, |env| env.call(f))?;
            }
            Primitive::Both => fork::both(env)?,
            Primitive::Fork => fork::fork(env)?,
            Primitive::Bracket => fork::bracket(env)?,
            Primitive::If => fork::iff(env)?,
            Primitive::Try => {
                let f = env.pop_function()?;
                let handler = env.pop_function()?;
                let f_args = f.signature().args;
                let backup = env.clone_stack_top(f_args);
                let bottom = env.stack_size().saturating_sub(f_args);
                if let Err(e) = env.call(f) {
                    env.truncate_stack(bottom);
                    env.backend.save_error_color(&e);
                    env.push(e.value());
                    for val in backup {
                        env.push(val);
                    }
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
            Primitive::Tag => {
                static NEXT_TAG: AtomicUsize = AtomicUsize::new(0);
                let tag = NEXT_TAG.fetch_add(1, atomic::Ordering::Relaxed);
                env.push(tag);
            }
            Primitive::Type => {
                let val = env.pop(1)?;
                env.push(val.type_id());
            }
            Primitive::Spawn => {
                let f = env.pop_function()?;
                env.spawn(f.signature().args, |env| env.call(f))?;
            }
            Primitive::Wait => {
                let id = env.pop(1)?;
                env.wait(id)?;
            }
            Primitive::Send => {
                let val = env.pop(1)?;
                let id = env.pop(2)?;
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
            Primitive::Trace => trace(env, false)?,
            Primitive::Dump => dump(env)?,
            Primitive::Sys(io) => io.run(env)?,
            Primitive::Regex => {
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
                        let regex = Regex::new(&pattern)
                            .map_err(|e| env.error(format!("Invalid pattern: {}", e)))?;
                        cache.entry(pattern.clone()).or_insert(regex.clone())
                    };
                    let matches: EcoVec<Boxed> = if regex.captures_len() == 1 {
                        regex
                            .find_iter(&target)
                            .map(|m| Boxed(Value::from(m.as_str())))
                            .collect()
                    } else {
                        regex
                            .captures(&target)
                            .map(|caps| {
                                caps.iter()
                                    .flatten()
                                    .map(|m| Boxed(Value::from(m.as_str())))
                                    .collect()
                            })
                            .unwrap_or_default()
                    };
                    env.push(matches);
                    Ok(())
                })?
            }
        }
        Ok(())
    }
}

impl ImplPrimitive {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            ImplPrimitive::Asin => env.monadic_env(Value::asin)?,
            ImplPrimitive::Acos => env.monadic_env(Value::acos)?,
            ImplPrimitive::InvTranspose => env.monadic_mut(Value::inv_transpose)?,
            ImplPrimitive::Unkeep => {
                let from = env.pop(1)?;
                let counts = env.pop(2)?;
                let into = env.pop(3)?;
                env.push(from.unkeep(counts, into, env)?);
            }
            ImplPrimitive::Untake => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.untake(index, into, env)?);
            }
            ImplPrimitive::Undrop => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.undrop(index, into, env)?);
            }
            ImplPrimitive::InvCouple => {
                let coupled = env.pop(1)?;
                let (a, b) = coupled.uncouple(env)?;
                env.push(b);
                env.push(a);
            }
            ImplPrimitive::Unpick => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.unpick(index, into, env)?);
            }
            ImplPrimitive::Unselect => {
                let index = env.pop(1)?;
                let into = env.pop(2)?;
                let from = env.pop(3)?;
                env.push(from.unselect(index, into, env)?);
            }
            ImplPrimitive::InvWhere => env.monadic_ref_env(Value::inverse_where)?,
            ImplPrimitive::InvUtf => env.monadic_ref_env(Value::inv_utf8)?,
            ImplPrimitive::InverseBits => env.monadic_ref_env(Value::inverse_bits)?,
            ImplPrimitive::Unpartition => loops::unpartition(env)?,
            ImplPrimitive::Ungroup => loops::ungroup(env)?,
            ImplPrimitive::InvTrace => trace(env, true)?,
            ImplPrimitive::InvComplex => {
                let x = env.pop(1)?;
                let im = x.clone().complex_im(env)?;
                let re = x.complex_re(env)?;
                env.push(re);
                env.push(im);
            }
            // Optimizations
            ImplPrimitive::Cos => env.monadic_env(Value::cos)?,
            ImplPrimitive::Last => env.monadic_env(Value::last)?,
            ImplPrimitive::FirstMinIndex => env.monadic_ref_env(Value::first_min_index)?,
            ImplPrimitive::FirstMaxIndex => env.monadic_ref_env(Value::first_max_index)?,
            ImplPrimitive::LastMinIndex => env.monadic_ref_env(Value::last_min_index)?,
            ImplPrimitive::LastMaxIndex => env.monadic_ref_env(Value::last_max_index)?,
            ImplPrimitive::FirstWhere => env.monadic_ref_env(Value::first_where)?,
        }
        Ok(())
    }
}

fn trace(env: &mut Uiua, inverse: bool) -> UiuaResult {
    let val = env.pop(1)?;
    let span: String = if inverse {
        format!("{} {}", env.span(), Primitive::Invert)
    } else {
        env.span().to_string()
    };
    let max_line_len = span.chars().count() + 2;
    let item_lines =
        format_trace_item_lines(val.show().lines().map(Into::into).collect(), max_line_len);
    env.push(val);
    env.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines {
        env.backend.print_str_trace(&line);
    }
    env.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.backend.print_str_trace("╴");
    }
    env.backend.print_str_trace("\n");
    Ok(())
}

fn dump(env: &mut Uiua) -> UiuaResult {
    let f = env.pop_function()?;
    if f.signature() != (1, 1) {
        return Err(env.error(format!(
            "Dump's function's signature must be |1.1, but it is {}",
            f.signature()
        )));
    }
    let span = env.span().to_string();
    let unprocessed = env.clone_stack_top(env.stack_size());
    let mut items = Vec::new();
    for item in unprocessed {
        env.push(item);
        match env.call(f.clone()) {
            Ok(()) => items.push(env.pop("dump's function's processed result")?),
            Err(e) => items.push(e.value()),
        }
    }
    let max_line_len = span.chars().count() + 2;
    let item_lines: Vec<Vec<String>> = items
        .iter()
        .map(Value::show)
        .map(|s| s.lines().map(Into::into).collect::<Vec<String>>())
        .map(|lines| format_trace_item_lines(lines, max_line_len))
        .collect();
    env.backend.print_str_trace(&format!("┌╴{span}\n"));
    for line in item_lines.iter().flatten() {
        env.backend.print_str_trace(line);
    }
    env.backend.print_str_trace("└");
    for _ in 0..max_line_len - 1 {
        env.backend.print_str_trace("╴");
    }
    env.backend.print_str_trace("\n");
    Ok(())
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
    /// Check whether the example should run automatically in certain contexts
    pub fn should_run(&self) -> bool {
        !["&sl", "&tcpc", "&ast"]
            .iter()
            .any(|prim| self.input.contains(prim))
    }
    /// Get the example's output
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
    fn prim_docs() {
        for prim in Primitive::all() {
            if let Some(doc) = prim.doc() {
                for line in &doc.lines {
                    if let PrimDocLine::Example(ex) = line {
                        if !ex.should_run() {
                            continue;
                        }
                        println!("{prim} example:\n{}", ex.input);
                        let mut env = Uiua::with_native_sys();
                        if let Err(e) = env.load_str(&ex.input) {
                            if !ex.should_error {
                                panic!("\nExample failed:\n{}\n{}", ex.input, e.report());
                            }
                        } else if let Some(diag) = env.take_diagnostics().into_iter().next() {
                            if !ex.should_error {
                                panic!("\nExample failed:\n{}\n{}", ex.input, diag.report());
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
        for prim in Primitive::all() {
            assert_eq!(Primitive::from_name(prim.name()), Some(prim));
        }
        for test in [
            Primitive::from_format_name as fn(&str) -> Option<Primitive>,
            |name| {
                Primitive::from_format_name_multi(name)
                    .unwrap()
                    .first()
                    .map(|(prim, _)| *prim)
            },
        ] {
            for prim in Primitive::all() {
                let char_test = match prim.glyph() {
                    None => prim.name().len(),
                    Some(c) if c.is_ascii() => continue,
                    Some(_) => 4,
                };
                let short: String = prim.name().chars().take(char_test).collect();
                assert_eq!(test(&short), Some(prim));
            }
            for prim in Primitive::all() {
                if matches!(
                    prim,
                    Primitive::Range
                        | Primitive::Rand
                        | Primitive::Transpose
                        | Primitive::Trace
                        | Primitive::Complex
                        | Primitive::Combinate
                ) {
                    continue;
                }
                let char_test = match prim.glyph() {
                    None => prim.name().len(),
                    Some(c) if c.is_ascii() || prim.ascii().is_some() => continue,
                    Some(_) => 3,
                };
                let short: String = prim.name().chars().take(char_test).collect();
                assert_eq!(test(&short), Some(prim));
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
            format!(r#"[{glyphs}]|(?<![a-zA-Z])({format_names}{literal_names})(?![a-zA-Z])"#)
        }

        let stack_functions = gen_group(
            Primitive::all()
                .filter(|p| p.class() == PrimClass::Stack && p.modifier_args().is_none())
                .chain(Some(Primitive::Identity)),
        );
        let noadic_functions = gen_group(Primitive::all().filter(|p| {
            p.class() != PrimClass::Stack && p.modifier_args().is_none() && p.args() == Some(0)
        }));
        let monadic_functions = gen_group(Primitive::all().filter(|p| {
            ![PrimClass::Stack, PrimClass::Planet].contains(&p.class())
                && p.modifier_args().is_none()
                && p.args() == Some(1)
        }));
        let dyadic_functions = gen_group(Primitive::all().filter(|p| {
            p.class() != PrimClass::Stack && p.modifier_args().is_none() && p.args() == Some(2)
        }));
        let monadic_modifiers =
            gen_group(Primitive::all().filter(|p| matches!(p.modifier_args(), Some(1))));
        let dyadic_modifiers: String =
            gen_group(Primitive::all().filter(|p| matches!(p.modifier_args(), Some(n) if n >= 2)));

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
			"match": "[`¯]?\\d+([./]\\d+(e[+-]?\\d+)?)?"
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
