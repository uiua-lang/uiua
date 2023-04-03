use std::{
    f64::{consts::PI, INFINITY},
    fmt,
};

use crate::{
    algorithm::pervade::bin_pervade_fallible, array::Array, function::FunctionId, io::*,
    lex::Simple, value::*, Uiua, UiuaError, UiuaResult,
};

macro_rules! primitive {
    ($((
        $($($args:literal)? $([$antiargs:literal])? $(($outputs:expr))? $({$antioutputs:literal})?,)?
        $name:ident $({$modifier:ident: $margs:literal})?
        $(,$ident:literal)? $(,$ascii:ident)? $(+ $character:literal)?
    )),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Primitive {
            $($name,)*
            Io(IoOp)
        }

        impl Primitive {
            pub const ALL: [Self; 0 $(+ {stringify!($name); 1})*] = [
                $(Self::$name,)*
            ];
            #[allow(path_statements)]
            pub fn name(&self) -> Option<&'static str > {
                match self {
                    $(Primitive::$name => { None::<&'static str> $(;Some($ident))? },)*
                    Primitive::Io(op) => Some(op.name())
                }
            }
            pub fn ascii(&self) -> Option<Simple> {
                match self {
                    $($(Primitive::$name => Some(Simple::$ascii),)?)*
                    _ => None
                }
            }
            pub fn unicode(&self) -> Option<char> {
                match self {
                    $($(Primitive::$name => Some($character),)?)*
                    _ => None
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
                    $($($character => Some(Self::$name),)?)*
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
                    $($($(Primitive::$name => Some($args),)?)?)*
                    Primitive::Io(op) => Some(op.args()),
                    _ => None
                }
            }
            pub fn outputs(&self) -> Option<u8> {
                match self {
                    $($($(Primitive::$name => $outputs.into(),)?)?)*
                    Primitive::Io(op) => op.outputs(),
                    _ => Some(1)
                }
            }
            pub fn antiargs(&self) -> Option<u8> {
                match self {
                    $($($(Primitive::$name => Some($antiargs),)?)?)*
                    _ => None
                }
            }
            pub fn antioutputs(&self) -> Option<u8> {
                match self {
                    $($($(Primitive::$name => Some($antioutputs),)?)?)*
                    _ => None
                }
            }
        }
    };
}

primitive!(
    // Stack ops
    (1(2), Dup, "duplicate" + '.'),
    (2(3), Over, "over" + ','),
    (2(2), Flip, "flip" + '~'),
    (1(0), Pop, "pop" + ';'),
    (1(0){1}, Save, "save" + '⇟'),
    (0[1](1), Load, "load" + '⇞'),
    ((None), Pack, "pack" + '⊓'),
    // Pervasive monadic ops
    (1, Sign, "sign" + '$'),
    (1, Not, "not" + '¬'),
    (1, Neg, "negate", Backtick + '¯'),
    (1, Abs, "absolute" + '⌵'),
    (1, Sqrt, "sqrt" + '√'),
    (1, Sin, "sine"),
    (1, Cos, "cosine"),
    (1, Asin, "asine"),
    (1, Acos, "acosine"),
    (1, Floor, "floor" + '⌊'),
    (1, Ceil, "ceiling" + '⌈'),
    (1, Round, "round" + '⁅'),
    // Pervasive dyadic ops
    (2, Eq, "equals", Equal),
    (2, Ne, "not equals", BangEqual + '≠'),
    (2, Lt, "less than" + '<'),
    (2, Le, "less or equal", LessEqual + '≤'),
    (2, Gt, "greater than" + '>'),
    (2, Ge, "greater or equal", GreaterEqual + '≥'),
    (2, Add, "add" + '+'),
    (2, Sub, "subtract" + '-'),
    (2, Mul, "multiply", Star + '×'),
    (2, Div, "divide", Percent + '÷'),
    (2, Mod, "modulus" + '◿'),
    (2, Pow, "power" + 'ⁿ'),
    (2, Log, "log"),
    (2, Min, "minimum" + '↧'),
    (2, Max, "maximum" + '↥'),
    (2, Atan, "atangent"),
    // Monadic array ops
    (1, Len, "length" + '⇀'),
    (1, Rank, "rank" + '∴'),
    (1, Shape, "shape" + '△'),
    (1, Range, "range" + '⇡'),
    (1, First, "first" + '⊢'),
    (1, Last),
    (1, Reverse, "reverse" + '⇌'),
    (1, Enclose, "enclose" + '⊡'),
    (1, Normalize, "normalize" + '□'),
    (1, Deshape, "deshape" + '♭'),
    (1, Transpose, "transpose" + '⍉'),
    (1, Sort, "sort" + '∧'),
    (1, Grade, "grade" + '⍋'),
    (1, Indices, "indices" + '⊘'),
    (1, Classify, "classify" + '⊛'),
    (1, Deduplicate, "deduplicate" + '⊝'),
    // Dyadic array ops
    (2, Match, "match" + '≅'),
    (2, NoMatch, "notmatch" + '≇'),
    (2, Join, "join" + '⊂'),
    (2, Pair, "pair" + '⚇'),
    (2, Couple, "couple" + '⊟'),
    (2, Pick, "pick" + '⊙'),
    (2, Select, "select" + '⊏'),
    (2, Take, "take" + '↙'),
    (2, Drop, "drop" + '↘'),
    (2, Reshape, "reshape" + '↯'),
    (2, Rotate, "rotate" + '↻'),
    (2, Windows, "windows" + '◫'),
    (2, Replicate, "replicate" + '‡'),
    (2, Member, "member" + '∊'),
    (2, Group, "group" + '⊕'),
    (2, IndexOf, "indexof" + '⊗'),
    // Triadic array op
    (3, Put),
    // Modifiers
    (Reduce { modifier: 1 }, "reduce" + '/'),
    (Fold { modifier: 1 }, "fold" + '⌿'),
    (Scan { modifier: 1 }, "scan" + '\\'),
    (Each { modifier: 1 }, "each" + '∵'),
    (Zip { modifier: 1 }, "zip" + '∺'),
    (Rows { modifier: 1 }, "rows" + '≡'),
    (Bridge { modifier: 1 }, "bridge" + '≑'),
    (Table { modifier: 1 }, "table" + '⊞'),
    (Repeat { modifier: 1 }, "repeat" + '⍥'),
    (Try { modifier: 2 }, "try" + '?'),
    // Misc
    (2, Assert, "assert" + '!'),
    (1(0), Break, "break" + '⎋'),
    (1, Trace, "trace" + '|'),
    (1(None), Call, "call" + ':'),
    (0, Noop, "noop" + '·'),
    (1, String, "string"),
    (1, Parse, "parsenumber"),
    (1, Use, "use"),
    // Constants
    (0(1), Pi, "pi" + 'π'),
    (0(1), Infinity, "infinity" + '∞')
);

fn _keep_primitive_small(_: std::convert::Infallible) {
    let _: [u8; 1] = unsafe { std::mem::transmute(Some(Primitive::Not)) };
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
            write!(f, "{:?}", self)
        }
    }
}

impl Primitive {
    pub fn inverse(&self) -> Option<Self> {
        use Primitive::*;
        Some(match self {
            Flip => Flip,
            Neg => Neg,
            Not => Not,
            Sin => Asin,
            Cos => Acos,
            Reverse => Reverse,
            Add => Sub,
            Sub => Add,
            Mul => Div,
            Div => Mul,
            Pow => Log,
            Log => Pow,
            Pick => Put,
            Save => Load,
            Load => Save,
            _ => return None,
        })
    }
    pub fn reduce_identity(&self) -> Option<Value> {
        Some(match self {
            Primitive::Add => Value::from(0),
            Primitive::Mul => Value::from(1),
            Primitive::Min => Value::from(std::f64::INFINITY),
            Primitive::Max => Value::from(std::f64::NEG_INFINITY),
            _ => return None,
        })
    }
    pub fn from_name(name: &str) -> Option<Self> {
        let lower = name.to_lowercase();
        if let Some(io) = IoOp::from_name(&lower) {
            return Some(Primitive::Io(io));
        }
        if lower == "pi" || lower == "π" {
            return Some(Primitive::Pi);
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching = Primitive::ALL.into_iter().filter(|p| {
            p.name()
                .map_or(false, |i| i.to_lowercase().starts_with(&lower))
        });
        let res = matching.next()?;
        let exact_match = res.name().map_or(false, |i| i == lower);
        (exact_match || matching.next().is_none()).then_some(res)
    }
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            Primitive::Pi => env.push(PI),
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
            Primitive::Log => env.dyadic_env(Value::log)?,
            Primitive::Min => env.dyadic_env(Value::min)?,
            Primitive::Max => env.dyadic_env(Value::max)?,
            Primitive::Atan => env.dyadic_env(Value::atan2)?,
            Primitive::Match => env.dyadic(|a, b| a == b)?,
            Primitive::NoMatch => env.dyadic(|a, b| a != b)?,
            Primitive::Join => env.dyadic_mut_env(Value::join)?,
            Primitive::Reshape => env.dyadic_mut_env(Value::reshape)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::Pick => env.dyadic_mut_env(Value::pick)?,
            Primitive::Replicate => env.dyadic_mut_env(Value::replicate)?,
            Primitive::Take => env.dyadic_mut_env(Value::take)?,
            Primitive::Drop => env.dyadic_mut_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_mut_env(Value::rotate)?,
            Primitive::Enclose => env.monadic_mut(Value::enclose)?,
            Primitive::Normalize => env.monadic_mut_env(Value::normalize)?,
            Primitive::Pair => env.dyadic_mut(Value::pair)?,
            Primitive::Couple => env.dyadic_mut_env(Value::couple)?,
            Primitive::Sort => env.monadic_mut_env(Value::sort)?,
            Primitive::Grade => env.monadic_mut_env(Value::grade)?,
            Primitive::Indices => env.monadic_mut_env(Value::indices)?,
            Primitive::Select => env.dyadic_mut_env(Value::select)?,
            Primitive::Windows => env.dyadic_mut_env(Value::windows)?,
            Primitive::Classify => env.monadic_mut_env(Value::classify)?,
            Primitive::Deduplicate => env.monadic_mut_env(Value::deduplicate)?,
            Primitive::Member => env.dyadic_mut(Value::member)?,
            Primitive::Group => env.dyadic_mut_env(Value::group)?,
            Primitive::IndexOf => env.dyadic_mut_env(Value::index_of)?,
            Primitive::Call => env.call()?,
            Primitive::Parse => env.monadic_mut_env(Value::parse_num)?,
            Primitive::Break => {
                let n = env
                    .pop(1)?
                    .as_nat()
                    .ok_or_else(|| env.error("break expects a natural number"))?
                    as usize;
                if n > 0 {
                    return Err(UiuaError::Break(n - 1, env.span().clone()));
                }
            }
            Primitive::Trace => {
                let value = env.pop(1)?;
                env.io.print_str(&value.show());
                env.io.print_str("\n");
                env.push(value);
            }
            Primitive::Pack => {
                let n = env
                    .pop(1)?
                    .as_nat()
                    .ok_or_else(|| env.error("pack expects a natural number"))?
                    as usize;
                if env.stack_size() < n {
                    return Err(env.error(format!(
                        "pack needs {n} values from the stack, but there are only {}",
                        env.stack_size()
                    )));
                }
                let mut values = Vec::with_capacity(n);
                for n in 0..n {
                    values.push(env.pop(n).unwrap());
                }
                env.push(Array::from(values));
            }
            Primitive::Put => {
                let mut index = env.pop(1)?;
                let value = env.pop(2)?;
                let array = env.pop(3)?;
                index.put(value, array, env)?;
                env.push(index);
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
            Primitive::Save => {
                let x = env.pop(1)?;
                env.antipush(x);
            }
            Primitive::Load => {
                let x = env.antipop(1)?;
                env.push(x);
            }
            Primitive::Fold => {
                let f = env.pop(1)?;
                let mut acc = env.pop(2)?;
                let xs = env.pop(3)?;
                if !xs.is_array() {
                    env.push(acc);
                    env.push(xs);
                    env.push(f);
                    env.call_catch_break()?;
                    return Ok(());
                }
                for cell in xs.into_array().into_values().into_iter().rev() {
                    env.push(acc);
                    env.push(cell);
                    env.push(f.clone());
                    if env.call_catch_break()? {
                        return Ok(());
                    }
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
                let mut cells = xs.into_array().into_values().into_iter().rev();
                let mut acc = cells
                    .next()
                    .or_else(|| f.as_primitive().and_then(|p| p.reduce_identity()))
                    .ok_or_else(|| env.error("Cannot reduce empty array"))?;
                for cell in cells {
                    env.push(acc);
                    env.push(cell);
                    env.push(f.clone());
                    if env.call_catch_break()? {
                        return Ok(());
                    }
                    acc = env.pop("reduced function result")?;
                }
                env.push(acc);
            }
            Primitive::Each => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                const BREAK_ERROR: &str = "break is not allowed in each";
                if !xs.is_array() {
                    env.push(xs);
                    env.push(f);
                    return env.call_error_on_break(BREAK_ERROR);
                }
                let (shape, values) = xs.into_array().into_shape_flat_values();
                let mut new_values = Vec::with_capacity(values.len());
                for val in values {
                    env.push(val);
                    env.push(f.clone());
                    env.call_error_on_break(BREAK_ERROR)?;
                    new_values.push(env.pop("each's function result")?);
                }
                env.push(Array::from((shape, new_values)).normalized_type());
            }
            Primitive::Zip => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                let ys = env.pop(3)?;
                const BREAK_ERROR: &str = "break is not allowed in zip";
                match (xs.is_array(), ys.is_array()) {
                    (false, false) => {
                        env.push(ys);
                        env.push(xs);
                        env.push(f);
                        env.call_error_on_break(BREAK_ERROR)?;
                    }
                    (true, true) => {
                        let (x_shape, x_values) = xs.into_array().into_shape_flat_values();
                        let (y_shape, y_values) = ys.into_array().into_shape_flat_values();
                        let (shape, values) = bin_pervade_fallible(
                            &x_shape,
                            x_values,
                            &y_shape,
                            y_values,
                            env,
                            |x, y, env| {
                                env.push(y);
                                env.push(x);
                                env.push(f.clone());
                                env.call_error_on_break(BREAK_ERROR)?;
                                env.pop("zip's function result")
                            },
                        )?;
                        env.push(Array::from((shape, values)).normalized_type());
                    }
                    (true, false) => {
                        let (x_shape, x_values) = xs.into_array().into_shape_flat_values();
                        let mut new_values = Vec::with_capacity(x_values.len());
                        for x in x_values {
                            env.push(ys.clone());
                            env.push(x);
                            env.push(f.clone());
                            env.call_error_on_break(BREAK_ERROR)?;
                            new_values.push(env.pop("zip's function result")?);
                        }
                        env.push(Array::from((x_shape, new_values)).normalized_type());
                    }
                    (false, true) => {
                        let (y_shape, y_values) = ys.into_array().into_shape_flat_values();
                        let mut new_values = Vec::with_capacity(y_values.len());
                        for y in y_values {
                            env.push(y);
                            env.push(xs.clone());
                            env.push(f.clone());
                            env.call_error_on_break(BREAK_ERROR)?;
                            new_values.push(env.pop("zip's function result")?);
                        }
                        env.push(Array::from((y_shape, new_values)).normalized_type());
                    }
                }
            }
            Primitive::Rows => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                const BREAK_ERROR: &str = "break is not allowed in rows";
                if !xs.is_array() {
                    env.push(xs);
                    env.push(f);
                    return env.call_error_on_break(BREAK_ERROR);
                }
                let array = xs.into_array();
                let mut new_rows: Vec<Value> = Vec::with_capacity(array.len());
                for row in array.into_values() {
                    env.push(row);
                    env.push(f.clone());
                    env.call_error_on_break(BREAK_ERROR)?;
                    new_rows.push(env.pop("rows' function result")?);
                }
                let mut array = Array::from(new_rows);
                if let Some((a, b)) = array.normalize() {
                    return Err(env.error(format!(
                        "Rows in resulting array have different shapes {a:?} and {b:?}"
                    )));
                }
                env.push(array);
            }
            Primitive::Bridge => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                let ys = env.pop(3)?;
                const BREAK_ERROR: &str = "break is not allowed in bridge";
                match (xs.is_array(), ys.is_array()) {
                    (false, false) => {
                        env.push(ys);
                        env.push(xs);
                        env.push(f);
                        env.call_error_on_break(BREAK_ERROR)?;
                    }
                    (true, true) => {
                        let x_rows = xs.into_array().into_values();
                        let y_rows = ys.into_array().into_values();
                        if x_rows.len() != y_rows.len() {
                            return Err(env.error(format!(
                                "Cannot bridge arrays with different number of rows {:?} and {:?}",
                                x_rows.len(),
                                y_rows.len()
                            )));
                        }
                        let mut new_rows = Vec::with_capacity(x_rows.len());
                        for (x, y) in x_rows.into_iter().zip(y_rows) {
                            env.push(y);
                            env.push(x);
                            env.push(f.clone());
                            env.call_error_on_break(BREAK_ERROR)?;
                            new_rows.push(env.pop("bridge's function result")?);
                        }
                        let mut array = Array::from(new_rows);
                        if let Some((a, b)) = array.normalize() {
                            return Err(env.error(format!(
                                "Rows in resulting array have different shapes {a:?} and {b:?}"
                            )));
                        }
                        env.push(array);
                    }
                    (true, false) => {
                        let x_rows = xs.into_array().into_values();
                        let mut new_rows = Vec::with_capacity(x_rows.len());
                        for x in x_rows {
                            env.push(ys.clone());
                            env.push(x);
                            env.push(f.clone());
                            env.call_error_on_break(BREAK_ERROR)?;
                            new_rows.push(env.pop("bridge's function result")?);
                        }
                        let mut array = Array::from(new_rows);
                        if let Some((a, b)) = array.normalize() {
                            return Err(env.error(format!(
                                "Rows in resulting array have different shapes {a:?} and {b:?}"
                            )));
                        }
                        env.push(array);
                    }
                    (false, true) => {
                        let y_rows = ys.into_array().into_values();
                        let mut new_rows = Vec::with_capacity(y_rows.len());
                        for y in y_rows {
                            env.push(y);
                            env.push(xs.clone());
                            env.push(f.clone());
                            env.call_error_on_break(BREAK_ERROR)?;
                            new_rows.push(env.pop("bridge's function result")?);
                        }
                        let mut array = Array::from(new_rows);
                        if let Some((a, b)) = array.normalize() {
                            return Err(env.error(format!(
                                "Rows in resulting array have different shapes {a:?} and {b:?}"
                            )));
                        }
                        env.push(array);
                    }
                }
            }
            Primitive::Table => {
                let f = env.pop(1)?;
                let xs = env.pop(2)?;
                let ys = env.pop(3)?;
                const BREAK_ERROR: &str = "break is not allowed in table";
                if !xs.is_array() && !ys.is_array() {
                    env.push(ys);
                    env.push(xs);
                    env.push(f);
                    return env.call_error_on_break(BREAK_ERROR);
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
                let mut new_shape = a.shape().to_vec();
                new_shape.extend_from_slice(b.shape());
                let mut items = Vec::with_capacity(a.len() * b.len());
                for a in a.into_flat_values() {
                    for b in b.clone().into_flat_values() {
                        env.push(b);
                        env.push(a.clone());
                        env.push(f.clone());
                        env.call_error_on_break(BREAK_ERROR)?;
                        items.push(env.pop("tabled function result")?);
                    }
                }
                env.push(Array::from((new_shape, items)).normalized_type());
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
                    let start_height = env.stack_size();
                    env.push(cell);
                    env.push(acc.clone());
                    env.push(f.clone());
                    let should_break = env.call_catch_break()?;
                    let new_acc = env.pop("scanned function result")?;
                    if should_break {
                        env.truncate_stack(start_height);
                        break;
                    }
                    acc = new_acc;
                    scanned.push(acc.clone());
                }
                env.push(Array::from(scanned).normalized());
            }
            Primitive::Repeat => {
                let f = env.pop(1)?;
                let mut acc = env.pop(2)?;
                let n = env.pop(3)?;
                if n.is_number() && n.number() == INFINITY {
                    loop {
                        env.push(acc);
                        env.push(f.clone());
                        if env.call_catch_break()? {
                            break;
                        }
                        acc = env.pop("repeated function result")?;
                    }
                } else {
                    let Some(n) = n.as_nat() else {
                        return Err(env.error("Repetitions must be a natural number or infinity"));
                    };
                    for _ in 0..n {
                        env.push(acc);
                        env.push(f.clone());
                        if env.call_catch_break()? {
                            return Ok(());
                        }
                        acc = env.pop("repeated function result")?;
                    }
                    env.push(acc);
                }
            }
            Primitive::Try => {
                let f = env.pop(1)?;
                let handler = env.pop(2)?;
                let size = env.stack_size();
                let antisize = env.antistack_size();
                env.push(f);
                if let Err(e) = env.call() {
                    env.truncate_stack(size);
                    env.truncate_antistack(antisize);
                    env.push(e.message());
                    env.push(handler);
                    env.call()?;
                }
            }
            Primitive::Assert => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if !(cond.is_number() && (cond.number() - 1.0).abs() < 1e-10) {
                    return Err(env.error(msg.to_string()));
                }
            }
            Primitive::Len => env.monadic(|v| v.len() as f64)?,
            Primitive::Rank => env.monadic(|v| v.rank() as f64)?,
            Primitive::Shape => {
                env.monadic(|v| Array::from_iter(v.shape().iter().map(|i| *i as f64)))?
            }
            Primitive::Range => env.monadic_mut_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_mut_env(Value::first)?,
            Primitive::Last => env.monadic_mut_env(Value::last)?,
            Primitive::String => env.monadic(|v| v.to_string())?,
            Primitive::Use => {
                let name = env.pop(1)?.as_string(env, "Use name must be a string")?;
                let mut lib = env.pop(2)?;
                let lowername = name.to_lowercase();
                let arr = lib.coerce_array();
                let f = arr.data(
                    |_, _| None,
                    |_, _| None,
                    |_, _| None,
                    |_, values| {
                        values.iter().filter(|v| v.is_function()).find_map(|v| {
                            let f = v.function();
                            matches!(&f.id, FunctionId::Named(n) if n.as_str().to_lowercase() == lowername)
                                .then(|| f.clone())
                        })
                    },
                ).ok_or_else(|| env.error(format!("No function found for {name:?}")))?;
                env.push(f);
            }
            Primitive::Io(io) => io.run(env)?,
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
        if let Some(glyph) = p.unicode() {
            writeln!(file, "{} |", glyph).unwrap();
        }
    }
}
