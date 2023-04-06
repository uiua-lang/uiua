use std::{
    f64::{consts::PI, INFINITY},
    fmt,
    rc::Rc,
};

use crate::{
    algorithm::loops, function::FunctionId, io::*, lex::Simple, value::*, Uiua, UiuaError,
    UiuaResult,
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
    (0(None), Clear, DoubleBacktick),
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
    (1, Fill, "fill" + '∘'),
    (1, Truncate, "truncate" + '⍛'),
    (1, Reverse, "reverse" + '⇌'),
    (1, Deshape, "deshape" + '♭'),
    (1, Transpose, "transpose" + '⍉'),
    (1, Sort, "sort" + '∧'),
    (1, Grade, "grade" + '⍋'),
    (1, Indices, "indices" + '⊙'),
    (1, Classify, "classify" + '⊛'),
    (1, Deduplicate, "deduplicate" + '⊝'),
    // Dyadic array ops
    (2, Match, "match" + '≅'),
    (2, NoMatch, "notmatch" + '≇'),
    (2, Join, "join" + '⊂'),
    (2, Couple, "couple" + '⊟'),
    (2, Pick, "pick" + '⊡'),
    (2, Select, "select" + '⊏'),
    (2, Take, "take" + '↙'),
    (2, Drop, "drop" + '↘'),
    (2, Reshape, "reshape" + '↯'),
    (2, Rotate, "rotate" + '↻'),
    (2, Windows, "windows" + '◫'),
    (2, Replicate, "replicate" + '‡'),
    (2, Member, "member" + '∊'),
    (2, Find, "find" + '⌕'),
    (2, IndexOf, "indexof" + '⊗'),
    (2, Group, "group" + '⊕'),
    (2, Partition, "partition" + '⊘'),
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
            Save => Load,
            Load => Save,
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
            Primitive::Eq => env.dyadic_ref_env(Value::is_eq)?,
            Primitive::Ne => env.dyadic_ref_env(Value::is_ne)?,
            Primitive::Lt => env.dyadic_ref_env(Value::is_lt)?,
            Primitive::Le => env.dyadic_ref_env(Value::is_le)?,
            Primitive::Gt => env.dyadic_ref_env(Value::is_gt)?,
            Primitive::Ge => env.dyadic_ref_env(Value::is_ge)?,
            Primitive::Add => env.dyadic_ref_env(Value::add)?,
            Primitive::Sub => env.dyadic_ref_env(Value::sub)?,
            Primitive::Mul => env.dyadic_ref_env(Value::mul)?,
            Primitive::Div => env.dyadic_ref_env(Value::div)?,
            Primitive::Mod => env.dyadic_ref_env(Value::modulus)?,
            Primitive::Pow => env.dyadic_ref_env(Value::pow)?,
            Primitive::Log => env.dyadic_ref_env(Value::log)?,
            Primitive::Min => env.dyadic_ref_env(Value::min)?,
            Primitive::Max => env.dyadic_ref_env(Value::max)?,
            Primitive::Atan => env.dyadic_ref_env(Value::atan2)?,
            Primitive::Match => env.dyadic_ref(|a, b| a == b)?,
            Primitive::NoMatch => env.dyadic_ref(|a, b| a != b)?,
            Primitive::Join => env.dyadic_env(Value::join)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::Pick => env.dyadic_env(Value::pick)?,
            Primitive::Replicate => env.dyadic_ref_own_env(Value::replicate)?,
            Primitive::Take => env.dyadic_env(Value::take)?,
            Primitive::Drop => env.dyadic_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_ref_own_env(Value::rotate)?,
            Primitive::Couple => env.dyadic_env(Value::couple)?,
            Primitive::Sort => env.monadic_mut(Value::sort)?,
            Primitive::Grade => env.monadic_ref_env(|v, env| v.grade(env))?,
            Primitive::Indices => env.monadic_ref_env(|v, env| v.indices(env))?,
            Primitive::Select => env.dyadic_ref_env(Value::select)?,
            Primitive::Windows => env.dyadic_ref_env(Value::windows)?,
            Primitive::Classify => env.monadic_ref_env(Value::classify)?,
            Primitive::Deduplicate => env.monadic_mut(Value::deduplicate)?,
            Primitive::Member => env.dyadic_ref_env(Value::member)?,
            Primitive::Find => env.dyadic_ref_env(Value::find)?,
            Primitive::IndexOf => env.dyadic_ref_env(Value::index_of)?,
            Primitive::Group => env.dyadic_ref_env(Value::group)?,
            Primitive::Partition => env.dyadic_ref_env(Value::partition)?,
            Primitive::Call => env.call()?,
            Primitive::Parse => env.monadic_env(|v, env| v.parse_num(env))?,
            Primitive::Reshape => {
                let shape = env.pop(1)?;
                let mut array = env.pop(2)?;
                Rc::make_mut(&mut array).reshape(&shape, env)?;
                env.push_ref(array);
            }
            Primitive::Break => {
                let n = env.pop(1)?.as_nat(env, "break expects a natural number")?;
                if n > 0 {
                    return Err(UiuaError::Break(n - 1, env.span().clone()));
                }
            }
            Primitive::Trace => {
                let value = env.pop(1)?;
                env.io.print_str(&value.show());
                env.io.print_str("\n");
                env.push_ref(value);
            }
            Primitive::Dup => {
                let x = env.pop(1)?;
                env.push_ref(x.clone());
                env.push_ref(x);
            }
            Primitive::Flip => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push_ref(a);
                env.push_ref(b);
            }
            Primitive::Over => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push_ref(b.clone());
                env.push_ref(a);
                env.push_ref(b);
            }
            Primitive::Pop => {
                env.pop(1)?;
            }
            Primitive::Save => {
                let x = env.pop(1)?;
                env.antipush_ref(x);
            }
            Primitive::Load => {
                let x = env.antipop(1)?;
                env.push_ref(x);
            }
            Primitive::Clear => {
                env.take_stack();
            }
            Primitive::Fold => loops::fold(env)?,
            Primitive::Reduce => loops::reduce(env)?,
            Primitive::Each => loops::each(env)?,
            Primitive::Zip => loops::zip(env)?,
            Primitive::Rows => loops::rows(env)?,
            Primitive::Bridge => loops::bridge(env)?,
            Primitive::Table => loops::table(env)?,
            Primitive::Scan => loops::scan(env)?,
            Primitive::Repeat => loops::repeat(env)?,
            Primitive::Try => {
                let f = env.pop(1)?;
                let handler = env.pop(2)?;
                let size = env.stack_size();
                let antisize = env.antistack_size();
                env.push_ref(f);
                if let Err(e) = env.call() {
                    env.truncate_stack(size);
                    env.truncate_antistack(antisize);
                    env.push(e.message());
                    env.push_ref(handler);
                    env.call()?;
                }
            }
            Primitive::Fill => env.monadic_mut(|v| *v.fill_mut() = true)?,
            Primitive::Truncate => env.monadic_mut(Value::truncate)?,
            Primitive::Assert => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if cond.as_nat(env, "").map_or(true, |n| n == 0) {
                    return Err(UiuaError::Assert(msg));
                }
            }
            Primitive::Len => env.monadic_ref(Value::len)?,
            Primitive::Rank => env.monadic_ref(Value::rank)?,
            Primitive::Shape => {
                env.monadic_ref(|v| v.shape().iter().copied().collect::<Value>())?
            }
            Primitive::Range => env.monadic_ref_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_env(Value::first)?,
            Primitive::Last => env.monadic_env(Value::last)?,
            Primitive::String => env.monadic_ref(|v| v.to_string())?,
            Primitive::Use => {
                let name = env.pop(1)?.as_string(env, "Use name must be a string")?;
                let lib = env.pop(2)?;
                let lowername = name.to_lowercase();
                let f = match &*lib {
                    Value::Func(fs) => fs.data.iter().find_map(|f| {
                        matches!(&f.id, FunctionId::Named(n) if n.as_str().to_lowercase() == lowername)
                            .then(|| f.clone())
                    }),
                    _ => None
                }.ok_or_else(|| env.error(format!("No function found for {name:?}")))?;
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
