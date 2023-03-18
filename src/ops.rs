use std::{
    f64::consts::*,
    fmt,
    io::{stdout, Write},
};

use crate::{array::Array, grid_fmt::GridFmt, lex::Simple, value::*, vm::CallEnv, RuntimeResult};

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
    pub unicode: Option<&'static str>,
}

macro_rules! primitive {
    ($(($name:ident $({$modifier:ident})?,  $($ident:literal)? $($ascii:ident)? $(+ $unicode:literal)?)),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Primitive {
            $($name,)*
            AdicFork(u8)
        }

        impl Primitive {
            pub const ALL: [Self; 0 $(+ {stringify!($name); 1})*] = [$(Self::$name,)*];
            #[allow(path_statements)]
            pub fn name(&self) -> PrimitiveName {
                match self {
                    $(Primitive::$name => PrimitiveName {
                        ident: {None::<&'static str> $(;Some($ident))?},
                        ascii: {None::<Simple> $(;Some(Simple::$ascii))?},
                        unicode: {None::<&'static str> $(;Some($unicode))?},
                    },)*
                    Primitive::AdicFork(n) => PrimitiveName {
                        ident: None,
                        ascii: Some(Simple::Colons(*n)),
                        unicode: None
                    },
                }
            }
            pub fn from_simple(s: Simple) -> Option<Self> {
                match s {
                    $($(Simple::$ascii => Some(Self::$name),)?)*
                    Simple::Colons(n) => Some(Self::AdicFork(n)),
                    _ => None
                }
            }
            pub fn from_unicode(s: &str) -> Option<Self> {
                match s {
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
        }
    };
}

primitive!(
    (Nop, +"·"),
    // Pervasive monadic ops
    (Not, "not" + "¬"),
    (Neg, "neg" + "¯"),
    (Abs, "abs"),
    (Sqrt, "sqrt" + "√"),
    (Sin, "sin"),
    (Cos, "cos"),
    (Asin, "asin"),
    (Acos, "acos"),
    (Floor, "floor" + "⌊"),
    (Ceil, "ceil" + "⌈"),
    (Round, "round" + "⁅"),
    // Pervasive dyadic ops
    (Eq, Equal),
    (Ne, BangEqual + "≠"),
    (Lt, Less),
    (Le, LessEqual + "≤"),
    (Gt, Greater),
    (Ge, GreaterEqual + "≥"),
    (Add, Plus),
    (Sub, Minus),
    (Mul, Star + "×"),
    (Div, Percent + "÷"),
    (Mod, "mod" + "◿"),
    (Pow, "pow" + "↗"),
    (Min, "min" + "↧"),
    (Max, "max" + "↥"),
    (Atan, "atan"),
    // Stack ops
    (Dup, Period),
    (Flip, Tilde),
    (Pop, SemiColon),
    // Control flow ops
    (ExclusiveFork, Bang),
    // Monadic array ops
    (Len, "len" + "𝄩"),
    (Rank, "rank" + "⧈"),
    (Shape, "shape" + "⬠"),
    (First, "first" + "◱"),
    (Range, "range" + "↕"),
    (Reverse, "reverse" + "⇌"),
    (Deshape, "deshape" + "♭"),
    (Transpose, "transpose" + "🏳️‍⚧️"),
    // Dyadic array ops
    (Match, "match" + "≡"),
    (NoMatch, "nomatch" + "≢"),
    (Join, "join" + "∾"),
    (Pick, "pick" + "⊞"),
    (Filter, "filter" + "ꖛ"),
    (Take, "take" + "↤"),
    (Drop, "drop" + "↦"),
    (Rotate, "rotate" + "↻"),
    (Reshape, "reshape" + "↯"),
    // IO ops
    (Show, "show"),
    (Print, "print"),
    (Println, "println"),
    (String, "string"),
    (ScanLn, "scanln"),
    (Args, "args"),
    (Var, "var"),
    // Modifiers
    (Fold { modifier }, "Fold"),
    (Reduce { modifier }, Slash),
    (Each { modifier }, "Each"),
    (Cells { modifier }, BackTick),
    (Table { modifier }, Caret),
    (Scan { modifier }, BackSlash),
    (Repeat {modifier}, "Repeat" + "⥀"),
);

fn _keep_primitive_small(_: std::convert::Infallible) {
    let _: [u8; 2] = unsafe { std::mem::transmute(Some(Primitive::Not)) };
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
    pub fn from_name(name: &str) -> Option<Self> {
        if name.len() < 2 {
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
    pub(crate) fn run(&self, env: &mut CallEnv) -> RuntimeResult {
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
            Primitive::Min => env.dyadic_env(Value::min)?,
            Primitive::Max => env.dyadic_env(Value::max)?,
            Primitive::Atan => env.dyadic_env(Value::atan2)?,
            Primitive::Match => env.dyadic(|a, b| a == b)?,
            Primitive::NoMatch => env.dyadic(|a, b| a != b)?,
            Primitive::Join => env.dyadic_mut_env(Value::join)?,
            Primitive::Reshape => env.dyadic_mut_env(Value::reshape)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::Pick => env.dyadic_mut_env(Value::pick)?,
            Primitive::Filter => env.dyadic_mut_env(Value::replicate)?,
            Primitive::Take => env.dyadic_mut_env(Value::take)?,
            Primitive::Drop => env.dyadic_mut_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_mut_env(Value::rotate)?,
            Primitive::Dup => {
                let x = env.top_mut()?.clone();
                env.push(x);
            }
            Primitive::Flip => {
                let a = env.pop()?;
                let b = env.pop()?;
                env.push(a);
                env.push(b);
            }
            Primitive::Pop => {
                env.pop()?;
            }
            Primitive::ExclusiveFork => {
                let fs = env.pop()?;
                if !fs.is_array() {
                    env.push(fs);
                    return env.call();
                }
                let arr = fs.into_array();
                let values = env.pop_n(arr.len())?;
                for (f, v) in arr.into_values().into_iter().rev().zip(values.into_iter()) {
                    env.push(v);
                    env.push(f);
                    env.call()?;
                }
            }
            Primitive::AdicFork(n) => {
                let fs = env.pop()?;
                if !fs.is_array() {
                    env.push(fs);
                    return env.call();
                }
                let args = env.pop_n(*n as usize)?;
                for f in fs.into_array().into_values().into_iter().rev() {
                    for arg in args.iter() {
                        env.push(arg.clone());
                    }
                    env.push(f);
                    env.call()?;
                }
            }
            Primitive::Fold => {
                let f = env.pop()?;
                let mut acc = env.pop()?;
                let xs = env.pop()?;
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
                    acc = env.pop()?;
                }
                env.push(acc);
            }
            Primitive::Reduce => {
                let f = env.pop()?;
                let xs = env.pop()?;
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
                    acc = env.pop()?;
                }
                env.push(acc);
            }
            Primitive::Each => {
                let f = env.pop()?;
                let xs = env.pop()?;
                if !xs.is_array() {
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                let (shape, values) = xs.into_array().into_parts();
                let mut new_values = Vec::with_capacity(values.len());
                for val in values {
                    env.push(val);
                    env.push(f.clone());
                    env.call()?;
                    new_values.push(env.pop()?);
                }
                env.push(Array::from((shape, new_values)).normalized(0));
            }
            Primitive::Cells => {
                let f = env.pop()?;
                let xs = env.pop()?;
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
                    cells.push(env.pop()?);
                }
                env.push(Array::from(cells).normalized(1));
            }
            Primitive::Table => {
                let f = env.pop()?;
                let xs = env.pop()?;
                let ys = env.pop()?;
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
                        row.push(env.pop()?);
                    }
                    table.push(Value::from(Array::from(row).normalized(1)));
                }
                env.push(Array::from(table).normalized(1));
            }
            Primitive::Scan => {
                let f = env.pop()?;
                let xs = env.pop()?;
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
                    acc = env.pop()?;
                    scanned.push(acc.clone());
                }
                env.push(Array::from(scanned).normalized(1));
            }
            Primitive::Repeat => {
                let f = env.pop()?;
                let n = env.pop()?;
                let Some(n) = n.as_nat() else {
                    return Err(env.error("Repetitions must be a natural number"));
                };
                let mut acc = env.pop()?;
                for _ in 0..n {
                    env.push(acc);
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop()?;
                }
                env.push(acc);
            }
            Primitive::Show => {
                let mut s = env.pop()?.grid_string();
                s.pop();
                println!("{s}");
            }
            Primitive::Print => {
                print!("{}", env.pop()?);
                let _ = stdout().flush();
            }
            Primitive::Println => println!("{}", env.pop()?),
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
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap_or_default();
                line.pop();
                line.pop();
                env.push(line);
            }
            Primitive::Args => env.push(Array::from_iter(
                std::env::args().map(Array::from).map(Value::from),
            )),
            Primitive::Var => {
                let name = env.top_mut()?;
                if !name.is_array() || !name.array().is_chars() {
                    return Err(env.error("Argument to var must be a string"));
                }
                let key: String = name.array().chars().iter().collect();
                *name = std::env::var(key).unwrap_or_default().into();
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
