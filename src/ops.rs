use std::{f64::consts::*, fmt};

use crate::{
    array::Array, grid_fmt::GridFmt, io::IoBackend, lex::Simple, value::*, vm::CallEnv,
    RuntimeResult,
};

pub(crate) fn constants() -> Vec<(&'static str, Value)> {
    vec![
        ("PI", PI.into()),
        ("TAU", TAU.into()),
        ("E", E.into()),
        ("INFINITY", f64::INFINITY.into()),
        ("NEG_INFINITY", f64::NEG_INFINITY.into()),
        ("NAN", f64::NAN.into()),
        ("MAX_REAL", f64::MAX.into()),
        ("MIN_REAL", f64::MIN.into()),
        ("EPSILON", f64::EPSILON.into()),
    ]
}

pub struct PrimitiveName {
    pub ident: Option<&'static str>,
    pub ascii: Option<Simple>,
    pub unicode: Option<char>,
}

macro_rules! primitive {
    ($(($($args:literal,)? $name:ident $({$modifier:ident ($margs:literal)})? $(,$ident:literal)? $(,$ascii:ident)? $(+ $unicode:literal)?)),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Primitive {
            $($name,)*
        }

        impl Primitive {
            pub const ALL: [Self; 0 $(+ {stringify!($name); 1})*] = [
                $(Self::$name,)*
            ];
            #[allow(path_statements)]
            pub fn name(&self) -> PrimitiveName {
                match self {
                    $(Primitive::$name => PrimitiveName {
                        ident: {None::<&'static str> $(;Some($ident))?},
                        ascii: {None::<Simple> $(;Some(Simple::$ascii))?},
                        unicode: {None::<char> $(;Some($unicode))?},
                    },)*
                }
            }
            pub fn from_simple(s: Simple) -> Option<Self> {
                match s {
                    $($(Simple::$ascii => Some(Self::$name),)?)*
                    _ => None
                }
            }
            pub fn from_unicode(c: char) -> Option<Self> {
                match c {
                    $($($unicode => Some(Self::$name),)?)*
                    _ => None
                }
            }
            pub fn is_modifier(&self) -> bool {
                match self {
                    $($(Primitive::$name => {
                        stringify!($modifier);
                        true
                    },)?)*
                    _ => false
                }
            }
            pub fn modifier_args(&self) -> Option<u8> {
                match self {
                    $($(Primitive::$name => Some($margs),)?)*
                    _ => None
                }
            }
            pub fn args(&self) -> Option<u8> {
                match self {
                    $($(Primitive::$name => Some($args),)?)*
                    _ => None
                }
            }
        }
    };
}

primitive!(
    // Stack ops
    (1, Dup, "duplicate", Period),
    (2, Flip, "flip", Tilde),
    (1, Pop, "pop", SemiColon),
    // Pervasive monadic ops
    (1, Not, "not" + '¬'),
    (1, Neg, "negate" + '¯'),
    (1, Abs, "absolute value" + '⌵'),
    (1, Sqrt, "sqrt" + '√'),
    (1, Sin, "sine"),
    (1, Cos, "cosine"),
    (1, Asin),
    (1, Acos),
    (1, Floor, "floor" + '⌊'),
    (1, Ceil, "ceiling" + '⌈'),
    (1, Round, "round" + '⁅'),
    // Pervasive dyadic ops
    (2, Eq, "equals", Equal),
    (2, Ne, "not equals", BangEqual + '≠'),
    (2, Lt, "less than", Less),
    (2, Le, "less or equal", LessEqual + '≤'),
    (2, Gt, "greater than", Greater),
    (2, Ge, "greater or equal", GreaterEqual + '≥'),
    (2, Add, "add", Plus),
    (2, Sub, "subtract", Minus),
    (2, Mul, "multiply", Star + '×'),
    (2, Div, "divide", Percent + '÷'),
    (2, Mod, "modulus" + '◿'),
    (2, Pow, "power" + 'ⁿ'),
    (2, Root, "root" + '◱'),
    (2, Min, "minimum" + '↧'),
    (2, Max, "maximum" + '↥'),
    (2, Atan, "atangent"),
    // Monadic array ops
    (1, Len, "length" + '⇀'),
    (1, Rank, "rank" + '⸫'),
    (1, Shape, "shape" + '△'),
    (1, Range, "range" + '⇡'),
    (1, First, "first" + '⊢'),
    (1, Reverse, "reverse" + '⇌'),
    (1, Enclose, "enclose" + '⊓'),
    (1, Deshape, "deshape" + '♭'),
    (1, Transpose, "transpose" + '⍉'),
    (1, Grade, "grade" + '⍋'),
    (1, Classify, "classify" + '⊛'),
    (1, Deduplicate, "deduplicate" + '⊝'),
    // Dyadic array ops
    (2, Match, "match" + '≅'),
    (2, NotMatch, "notmatch" + '≇'),
    (2, Join, "join" + '≍'),
    (2, Pair, "pair" + '⚇'),
    (2, Couple, "couple" + '⊟'),
    (2, Pick, "pick" + '⊡'),
    (2, Select, "select" + '⊏'),
    (2, Take, "take" + '↙'),
    (2, Drop, "drop" + '↘'),
    (2, Reshape, "reshape" + '↯'),
    (2, Rotate, "rotate" + '↻'),
    (2, Windows, "windows" + '◫'),
    (2, Replicate, "replicate" + '‡'),
    (2, Member, "member" + '∈'),
    (2, Group, "group" + '⊕'),
    (2, IndexOf, "indexof" + '⦶'),
    // IO ops
    (1, Show, "show"),
    (1, Print, "print"),
    (1, Println, "println"),
    (1, String, "string"),
    (0, ScanLn, "scanln"),
    (0, Args, "args"),
    (1, Var, "var"),
    // Modifiers
    (Invert { modifier(1) }, "invert" + '↩'),
    (Under { modifier(2) }, "under" + '⍜'),
    (Reduce { modifier(1) }, "reduce", Slash),
    (Scan { modifier(1) }, "scan", BackSlash),
    (Each { modifier(1) }, "each" + '⸪'),
    (Cells { modifier(1) }, "cells" + '≡'),
    (Table { modifier(1) }, "table", Caret + '⊞'),
    (Fold { modifier(1) }, "fold"),
    (Repeat { modifier(1) }, "repeat" + '⍥'),
    (Try { modifier(2) }, "try", Question),
    // Misc
    (2, Assert, "assert", Bang),
    (0, Nop, "noop" + '·'),
    (Call, "call", Colon),
);

fn _keep_primitive_small(_: std::convert::Infallible) {
    let _: [u8; 1] = unsafe { std::mem::transmute(Some(Primitive::Not)) };
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name();
        if let Some(c) = name.unicode {
            write!(f, "{}", c)
        } else if let Some(s) = name.ascii {
            write!(f, "{}", s)
        } else if let Some(s) = name.ident {
            write!(f, "{}", s)
        } else {
            write!(f, "{:?}", self)
        }
    }
}

impl Primitive {
    pub fn inverse(&self) -> Option<Self> {
        use Primitive::*;
        Some(match self {
            Neg => Neg,
            Not => Not,
            Sin => Asin,
            Cos => Acos,
            Reverse => Reverse,
            Add => Sub,
            Sub => Add,
            Mul => Div,
            Div => Mul,
            _ => return None,
        })
    }
    pub fn from_name(name: &str) -> Option<Self> {
        if name.len() < 3 {
            return None;
        }
        let name = name.to_lowercase();
        let mut matching = Primitive::ALL.into_iter().filter(|p| {
            p.name()
                .ident
                .map_or(false, |i| i.to_lowercase().starts_with(&name))
        });
        let res = matching.next()?;
        matching.next().is_none().then_some(res)
    }
    pub(crate) fn run<B: IoBackend>(&self, env: &mut CallEnv<B>) -> RuntimeResult {
        match self {
            Primitive::Nop => {}
            Primitive::Not => env.monadic_env(Value::not)?,
            Primitive::Neg => env.monadic_env(Value::neg)?,
            Primitive::Abs => env.monadic_env(Value::abs)?,
            Primitive::Sqrt => env.monadic_env(Value::sqrt)?,
            Primitive::Sin => env.monadic_env(Value::sin)?,
            Primitive::Cos => env.monadic_env(Value::cos)?,
            Primitive::Asin => env.monadic_env(Value::asin)?,
            Primitive::Acos => env.monadic_env(Value::acos)?,
            Primitive::Floor => env.monadic_env(Value::floor)?,
            Primitive::Ceil => env.monadic_env(Value::ceil)?,
            Primitive::Round => env.monadic_env(Value::round)?,
            Primitive::Eq => env.dyadic_env(Value::is_eq)?,
            Primitive::Ne => env.dyadic_env(Value::is_ne)?,
            Primitive::Lt => env.dyadic_env(Value::is_lt)?,
            Primitive::Le => env.dyadic_env(Value::is_le)?,
            Primitive::Gt => env.dyadic_env(Value::is_gt)?,
            Primitive::Ge => env.dyadic_env(Value::is_ge)?,
            Primitive::Add => env.dyadic_env(Value::add)?,
            Primitive::Sub => env.dyadic_env(Value::sub)?,
            Primitive::Mul => env.dyadic_env(Value::mul)?,
            Primitive::Div => env.dyadic_env(Value::div)?,
            Primitive::Mod => env.dyadic_env(Value::modulus)?,
            Primitive::Pow => env.dyadic_env(Value::pow)?,
            Primitive::Root => env.dyadic_env(Value::root)?,
            Primitive::Min => env.dyadic_env(Value::min)?,
            Primitive::Max => env.dyadic_env(Value::max)?,
            Primitive::Atan => env.dyadic_env(Value::atan2)?,
            Primitive::Match => env.dyadic(|a, b| a == b)?,
            Primitive::NotMatch => env.dyadic(|a, b| a != b)?,
            Primitive::Join => env.dyadic_mut_env(Value::join)?,
            Primitive::Reshape => env.dyadic_mut_env(Value::reshape)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::Pick => env.dyadic_mut_env(Value::pick)?,
            Primitive::Replicate => env.dyadic_mut_env(Value::replicate)?,
            Primitive::Take => env.dyadic_mut_env(Value::take)?,
            Primitive::Drop => env.dyadic_mut_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_mut_env(Value::rotate)?,
            Primitive::Enclose => env.monadic_mut(Value::enclose)?,
            Primitive::Pair => env.dyadic_mut(Value::pair)?,
            Primitive::Couple => env.dyadic_mut_env(Value::couple)?,
            Primitive::Grade => env.monadic_mut_env(Value::grade)?,
            Primitive::Select => env.dyadic_mut_env(Value::select)?,
            Primitive::Windows => env.dyadic_mut_env(Value::windows)?,
            Primitive::Classify => env.monadic_mut_env(Value::classify)?,
            Primitive::Deduplicate => env.monadic_mut_env(Value::deduplicate)?,
            Primitive::Member => env.dyadic_mut(Value::member)?,
            Primitive::Group => env.dyadic_mut_env(Value::group)?,
            Primitive::IndexOf => env.dyadic_mut_env(Value::index_of)?,
            Primitive::Call => env.call()?,
            Primitive::Dup => {
                let x = env.top_mut(1)?.clone();
                env.push(x);
            }
            Primitive::Flip => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push(a);
                env.push(b);
            }
            Primitive::Pop => {
                env.pop(1)?;
            }
            Primitive::Invert => {
                let f = env.pop(1)?;
                if !f.is_function() {
                    return Err(env.error("Only functions can be inverted"));
                }
                let f_inv = f.function().inverse(&env.env(), false)?;
                env.push(f_inv);
                env.call()?;
            }
            Primitive::Under => {
                let f = env.pop(1)?;
                let g = env.pop(2)?;
                if !f.is_function() || !g.is_function() {
                    return Err(env.error("Only functions can be inverted"));
                }
                let f_inv = f.function().inverse(&env.env(), true)?;
                env.push(f);
                env.call()?;
                env.push(g);
                env.call()?;
                env.push(f_inv);
                env.call()?;
            }
            Primitive::Fold => {
                let f = env.pop(1)?;
                let mut acc = env.pop(2)?;
                let xs = env.pop(3)?;
                if !xs.is_array() {
                    env.push(acc);
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                for cell in xs.into_array().into_values() {
                    env.push(acc);
                    env.push(cell);
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop("folded function result")?;
                }
                env.push(acc);
            }
            Primitive::Reduce => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                if !xs.is_array() {
                    env.push(xs);
                    return Ok(());
                }
                let mut cells = xs.into_array().into_values().into_iter();
                let Some(mut acc) = cells.next() else {
                    return Err(env.error("Cannot reduce empty array"));
                };
                for cell in cells {
                    env.push(cell);
                    env.push(acc);
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop("reduced function result")?;
                }
                env.push(acc);
            }
            Primitive::Each => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                if !xs.is_array() {
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                let (shape, values) = xs.into_array().into_shape_flat_values();
                let mut new_values = Vec::with_capacity(values.len());
                for val in values {
                    env.push(val);
                    env.push(f.clone());
                    env.call()?;
                    new_values.push(env.pop("each's function result")?);
                }
                env.push(Array::from((shape, new_values)).normalized(0));
            }
            Primitive::Cells => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                if !xs.is_array() {
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                let array = xs.into_array();
                let mut cells = Vec::with_capacity(array.len());
                for cell in array.into_values() {
                    env.push(cell);
                    env.push(f.clone());
                    env.call()?;
                    cells.push(env.pop("cells' function result")?);
                }
                env.push(Array::from(cells).normalized(1));
            }
            Primitive::Table => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                let ys = env.pop(3)?;
                if !xs.is_array() && !ys.is_array() {
                    env.push(ys);
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                let a = if xs.is_array() {
                    xs.into_array()
                } else {
                    Array::from(xs)
                };
                let b = if ys.is_array() {
                    ys.into_array()
                } else {
                    Array::from(ys)
                };
                let mut table = Vec::with_capacity(a.len());
                for a in a.into_values() {
                    let mut row = Vec::with_capacity(b.len());
                    for b in b.clone().into_values() {
                        env.push(b);
                        env.push(a.clone());
                        env.push(f.clone());
                        env.call()?;
                        row.push(env.pop("tabled function result")?);
                    }
                    table.push(Value::from(Array::from(row).normalized(1)));
                }
                env.push(Array::from(table).normalized(1));
            }
            Primitive::Scan => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                if !xs.is_array() {
                    env.push(xs);
                    return Ok(());
                }
                let arr = xs.into_array();
                let ty = arr.ty();
                let len = arr.len();
                let mut cells = arr.into_values().into_iter();
                let Some(mut acc) = cells.next() else {
                    env.push(Array::from(ty));
                    return Ok(())
                };
                let mut scanned = Vec::with_capacity(len);
                scanned.push(acc.clone());
                for cell in cells {
                    env.push(cell);
                    env.push(acc.clone());
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop("scanned function result")?;
                    scanned.push(acc.clone());
                }
                env.push(Array::from(scanned).normalized(1));
            }
            Primitive::Repeat => {
                let f = env.pop(1)?;
                let mut acc = env.pop(2)?;
                let n = env.pop(3)?;
                let Some(n) = n.as_nat() else {
                    return Err(env.error("Repetitions must be a natural number"));
                };
                for _ in 0..n {
                    env.push(acc);
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop("repeated function result")?;
                }
                env.push(acc);
            }
            Primitive::Try => {
                let f = env.pop(1)?;
                let handler = env.pop(2)?;
                let size = env.stack_size();
                env.push(f);
                if let Err(e) = env.call() {
                    env.truncate(size);
                    env.push(e.value);
                    env.push(handler);
                    env.call()?;
                }
            }
            Primitive::Assert => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if !(cond.is_num() && (cond.number() - 1.0).abs() < 1e-10) {
                    return Err(env.error(&msg.to_string()));
                }
            }
            Primitive::Show => {
                let s = env.pop(1)?.grid_string();
                env.vm.io.print_str_ln(&s);
            }
            Primitive::Print => {
                let val = env.pop(1)?;
                env.vm.io.print_str(&val.to_string());
            }
            Primitive::Println => {
                let val = env.pop(1)?;
                env.vm.io.print_str_ln(&val.to_string());
            }
            Primitive::Len => env.monadic(|v| v.len() as f64)?,
            Primitive::Rank => env.monadic(|v| v.rank() as f64)?,
            Primitive::Shape => {
                env.monadic(|v| Array::from_iter(v.shape().into_iter().map(|i| i as f64)))?
            }
            Primitive::Range => env.monadic_mut_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_mut_env(Value::first)?,
            Primitive::String => env.monadic(|v| v.to_string())?,
            Primitive::ScanLn => {
                let mut line = env.vm.io.scan_line();
                line.pop();
                line.pop();
                env.push(line);
            }
            Primitive::Args => {
                let args = env.vm.io.args();
                env.push(Array::from_iter(
                    args.into_iter().map(Array::from).map(Value::from),
                ))
            }
            Primitive::Var => {
                let name = env.pop(1)?;
                if !name.is_array() || !name.array().is_chars() {
                    return Err(env.error("Argument to var must be a string"));
                }
                let key: String = name.array().chars().iter().collect();
                let var = env.vm.io.var(&key).unwrap_or_default();
                env.push(var);
            }
        }
        Ok(())
    }
}

#[test]
fn primitive_from_name() {
    assert_eq!(Primitive::from_name("rev"), Some(Primitive::Reverse));
    assert_eq!(Primitive::from_name("re"), None);
    assert_eq!(Primitive::from_name("resh"), Some(Primitive::Reshape));
}

#[test]
fn glyph_size() {
    use std::{fs::File, io::Write};
    let mut file = File::create("glyph_test.txt").unwrap();
    writeln!(file, "A |").unwrap();
    writeln!(file, "a |").unwrap();
    for p in Primitive::ALL {
        if let Some(glyph) = p.name().unicode {
            writeln!(file, "{} |", glyph).unwrap();
        }
    }
}
