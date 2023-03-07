use std::{
    cmp::Ordering,
    env,
    f64::consts::*,
    fmt,
    io::{stdin, stdout, Write},
    mem::{swap, take},
    ops::*,
    sync::Arc,
};

use enum_iterator::Sequence;

use crate::{
    array::Array, compile::Assembly, list::List, value::*, vm::Instr, RuntimeError, RuntimeResult,
};

#[derive(Clone, Copy)]
pub(crate) struct Env<'a> {
    pub span: usize,
    pub assembly: &'a Assembly,
}

impl<'a> Env<'a> {
    pub fn error(&self, message: impl Into<String>) -> RuntimeError {
        self.assembly.spans[self.span].error(message)
    }
}

pub(crate) fn constants() -> Vec<(&'static str, Value)> {
    vec![
        ("PI", PI.into()),
        ("TAU", TAU.into()),
        ("E", E.into()),
        ("INFINITY", f64::INFINITY.into()),
        ("NEG_INFINITY", f64::NEG_INFINITY.into()),
        ("NAN", f64::NAN.into()),
        ("MAX_INT", i64::MAX.into()),
        ("MIN_INT", i64::MIN.into()),
        ("MAX_REAL", f64::MAX.into()),
        ("MIN_REAL", f64::MIN.into()),
        ("EPSILON", f64::EPSILON.into()),
    ]
}

/// 1-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Op1 {
    Id,
    Default,
    Byte,
    Int,
    Real,
    String,
    List,
    Array,
    Not,
    Neg,
    Abs,
    Sqrt,
    Sin,
    Cos,
    Floor,
    Ceil,
    Round,
    Len,
    Print,
    Println,
    ScanLn,
    Args,
    Var,
}

impl fmt::Display for Op1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op1::Id => write!(f, "id"),
            Op1::Default => write!(f, "default"),
            Op1::Byte => write!(f, "byte"),
            Op1::Int => write!(f, "int"),
            Op1::Real => write!(f, "real"),
            Op1::String => write!(f, "string"),
            Op1::List => write!(f, "list"),
            Op1::Array => write!(f, "array"),
            Op1::Not => write!(f, "not"),
            Op1::Neg => write!(f, "neg"),
            Op1::Abs => write!(f, "abs"),
            Op1::Sqrt => write!(f, "sqrt"),
            Op1::Sin => write!(f, "sin"),
            Op1::Cos => write!(f, "cos"),
            Op1::Floor => write!(f, "floor"),
            Op1::Ceil => write!(f, "ceil"),
            Op1::Round => write!(f, "round"),
            Op1::Len => write!(f, "len"),
            Op1::Print => write!(f, "print"),
            Op1::Println => write!(f, "println"),
            Op1::ScanLn => write!(f, "scanln"),
            Op1::Args => write!(f, "args"),
            Op1::Var => write!(f, "var"),
        }
    }
}

impl Value {
    pub(crate) fn op1(&mut self, op: Op1, env: Env) -> RuntimeResult {
        match op {
            Op1::Id => self.id(env),
            Op1::Default => self.default(env),
            Op1::Byte => self.byte(env),
            Op1::Int => self.int(env),
            Op1::Real => self.real(env),
            Op1::String => self.string(env),
            Op1::List => self.list(env),
            Op1::Array => self.array(env),
            Op1::Not => self.not(env),
            Op1::Neg => self.neg(env),
            Op1::Abs => self.abs(env),
            Op1::Sqrt => self.sqrt(env),
            Op1::Sin => self.sin(env),
            Op1::Cos => self.cos(env),
            Op1::Floor => self.floor(env),
            Op1::Ceil => self.ceil(env),
            Op1::Round => self.round(env),
            Op1::Len => self.len(env),
            Op1::Print => self.print(env),
            Op1::Println => self.println(env),
            Op1::ScanLn => self.scanln(env),
            Op1::Args => self.args(env),
            Op1::Var => self.var(env),
        }
    }
}
macro_rules! op1_fn {
    ($name:ident, $message:literal, $this:ident, $(($a_val:pat, $f:expr)),* $(,)?) => {
        #[allow(unused_mut, clippy::no_effect, unreachable_patterns)]
        impl Value {
            pub(crate) fn $name(&mut self, env: Env) -> RuntimeResult {
                let mut $this = self;
                match &mut $this {
                    $($a_val => $f,)*
                    Value::Array(a) => {
                        for a in a.iter_mut() {
                            a.$name(env)?;
                        }
                    },
                    a => return Err(env.error(format!($message, a.ty()))),
                }
                Ok(())
            }
        }
    };
}
pub(crate) use op1_fn;

impl Value {
    pub(crate) fn id(&mut self, _env: Env) -> RuntimeResult {
        Ok(())
    }
    pub(crate) fn not(&mut self, _env: Env) -> RuntimeResult {
        *self = (!self.is_truthy()).into();
        Ok(())
    }
}
op1_fn!(
    default,
    "Cannot convert {} to default",
    this,
    (Value::Unit, ()),
    (Value::Bool(_), *this = false.into()),
    (Value::Byte(_), *this = 0u8.into()),
    (Value::Int(_), *this = 0i64.into()),
    (Value::Real(_), *this = 0.0.into()),
    (Value::Char(_), *this = '\0'.into()),
    (Value::Function(_), *this = Function::default().into()),
    (Value::Partial(_), *this = Function::default().into()),
    (Value::String(_), *this = String::new().into()),
    (Value::List(_), *this = List::new().into()),
    (Value::Array(_), *this = Array::new().into()),
);
op1_fn!(
    byte,
    "Cannot convert {} to byte",
    this,
    (Value::Bool(a), *this = (*a as u8).into()),
    (Value::Byte(_), ()),
    (Value::Int(a), *this = (*a as u8).into()),
    (Value::Real(a), *this = (*a as u8).into()),
);
op1_fn!(
    int,
    "Cannot convert {} to int",
    this,
    (Value::Bool(a), *this = (*a as i64).into()),
    (Value::Byte(a), *this = (*a as i64).into()),
    (Value::Int(_), ()),
    (Value::Real(a), *this = (*a as i64).into()),
);
op1_fn!(
    real,
    "Cannot convert {} to real",
    this,
    (Value::Bool(a), *this = (*a as u8 as f64).into()),
    (Value::Byte(a), *this = (*a as f64).into()),
    (Value::Int(a), *this = (*a as f64).into()),
    (Value::Real(_), ()),
);
op1_fn!(
    string,
    "Cannot format {}",
    this,
    (Value::Array(a), *this = format!("{a}").into()),
    (a, *this = format!("{a}").into())
);
op1_fn!(
    list,
    "Cannot convert {} to list",
    this,
    (Value::List(_), ()),
    (
        Value::String(a),
        *this = List::from_items(a.chars().map(Into::into)).into()
    ),
    (
        Value::Array(a),
        *this = List::from_items(a.take_inner()).into()
    ),
);
op1_fn!(
    array,
    "Cannot convert {} to array",
    this,
    (Value::Array(_), ()),
    (
        Value::String(a),
        *this = Array::from_iter(a.chars().map(|c| c.into())).into()
    ),
    (Value::List(a), *this = a.iter().collect::<Array>().into()),
);
op1_fn!(
    neg,
    "Cannot negate {}",
    this,
    (Value::Int(a), *this = (-*a).into()),
    (Value::Real(a), *this = (-*a).into())
);
op1_fn!(
    abs,
    "Cannot get absolute value of {}",
    this,
    (Value::Int(a), *this = a.abs().into()),
    (Value::Real(a), *this = a.abs().into())
);
op1_fn!(
    sqrt,
    "Cannot get square root of {}",
    this,
    (Value::Real(a), *this = a.sqrt().into())
);
op1_fn!(
    sin,
    "Cannot get sine of {}",
    this,
    (Value::Real(a), *this = a.sin().into())
);
op1_fn!(
    cos,
    "Cannot get cosine of {}",
    this,
    (Value::Real(a), *this = a.cos().into())
);
op1_fn!(
    floor,
    "Cannot get floor of {}",
    this,
    (Value::Byte(_), ()),
    (Value::Int(_), ()),
    (Value::Real(a), *this = a.floor().into())
);
op1_fn!(
    ceil,
    "Cannot get ceiling of {}",
    this,
    (Value::Byte(_), ()),
    (Value::Int(_), ()),
    (Value::Real(a), *this = a.ceil().into())
);
op1_fn!(
    round,
    "Cannot round {}",
    this,
    (Value::Byte(_), ()),
    (Value::Int(_), ()),
    (Value::Real(a), *this = a.round().into())
);
op1_fn!(
    len,
    "Cannot get length of {}",
    this,
    (Value::String(a), *this = (a.len() as i64).into()),
    (Value::List(a), *this = (a.len() as i64).into()),
    (Value::Array(a), *this = (a.len() as i64).into()),
);
op1_fn!(
    print,
    "Cannot print {}",
    this,
    (Value::Array(a), {
        print!("{a}");
        stdout().flush().unwrap();
    }),
    (a, {
        print!("{a}");
        stdout().flush().unwrap();
    })
);
op1_fn!(
    println,
    "Cannot print {}",
    this,
    (Value::Array(a), println!("{}", a)),
    (a, println!("{a}"))
);
op1_fn!(
    scanln,
    "Cannot read from stdin {}",
    this,
    (_, *this = stdin().lines().next().unwrap().unwrap().into())
);
op1_fn!(
    args,
    "Cannot get program args {}",
    this,
    (
        _,
        *this = Array::from_iter(env::args().skip(1).map(Into::into)).into()
    )
);
op1_fn!(
    var,
    "Cannot get environment variable by {}",
    this,
    (
        Value::String(a),
        *this = env::var(&**a).map(Into::into).unwrap_or(Value::Unit)
    )
);

/// 2-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Op2 {
    Left,
    Right,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Atan2,
    Get,
    Push,
    Concat,
}

impl fmt::Display for Op2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op2::Left => write!(f, "left"),
            Op2::Right => write!(f, "right"),
            Op2::Eq => write!(f, "eq"),
            Op2::Ne => write!(f, "ne"),
            Op2::Lt => write!(f, "lt"),
            Op2::Le => write!(f, "le"),
            Op2::Gt => write!(f, "gt"),
            Op2::Ge => write!(f, "ge"),
            Op2::Add => write!(f, "add"),
            Op2::Sub => write!(f, "sub"),
            Op2::Mul => write!(f, "mul"),
            Op2::Div => write!(f, "div"),
            Op2::Mod => write!(f, "mod"),
            Op2::Pow => write!(f, "pow"),
            Op2::Atan2 => write!(f, "atan2"),
            Op2::Get => write!(f, "get"),
            Op2::Push => write!(f, "push"),
            Op2::Concat => write!(f, "concat"),
        }
    }
}

impl Value {
    pub(crate) fn op2(&mut self, other: &mut Value, op: Op2, env: Env) -> RuntimeResult {
        match op {
            Op2::Left => self.pick_left(other, env),
            Op2::Right => self.pick_right(other, env),
            Op2::Eq => self.is_eq(other, env),
            Op2::Ne => self.is_ne(other, env),
            Op2::Lt => self.is_lt(other, env),
            Op2::Le => self.is_le(other, env),
            Op2::Gt => self.is_gt(other, env),
            Op2::Ge => self.is_ge(other, env),
            Op2::Add => self.add_assign(other, env),
            Op2::Sub => self.sub_assign(other, env),
            Op2::Mul => self.mul_assign(other, env),
            Op2::Div => self.div_assign(other, env),
            Op2::Mod => self.modulus(other, env),
            Op2::Pow => self.pow(other, env),
            Op2::Atan2 => self.atan2(other, env),
            Op2::Get => self.get(other, env),
            Op2::Push => self.push(other, env),
            Op2::Concat => self.concat(other, env),
        }
    }
}
macro_rules! op2_fn {
    ($name:ident, $message:expr, $this:ident, $other:ident,
        $(($a:pat, $b:pat, $f:expr)),*
        $(,(!$env:ident, $ea:pat, $eb:pat, $ef:expr))*
    $(,)?) => {
        impl Value {
            #[allow(unused_mut, unreachable_patterns)]
            pub(crate) fn $name(&mut self, other: &mut Self, env: Env) -> RuntimeResult {
                let mut $this = self;
                let mut $other = other;
                match (&mut $this, &mut $other) {
                    $(($a, $b) => $f,)*
                    $(($ea, $eb) => {
                        let $env = env;
                        $ef
                    },)*
                    (Value::Array(a), Value::Array(b)) => {
                        if a.len() != b.len() {
                            return Err(env.error(format!(
                                concat!($message, " because they have different lengths: {} and {}"),
                                Type::Array,
                                Type::Array,
                                a.len(),
                                b.len()
                            )));
                        }
                        for (a, b) in a
                            .iter_mut()
                            .zip(b.iter_mut())
                        {
                            a.$name(b, env)?;
                        }
                    },
                    (Value::Array(a), b) => {
                        for a in a.iter_mut() {
                            a.$name(&mut *b, env)?;
                        }
                    },
                    (a, Value::Array(b)) => {
                        for b in b.iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$name(b, env)?;
                            *b = a_clone;
                        }
                        swap(*a, $other);
                    },
                    (a, b) => return Err(env.error(format!($message, a.ty(), b.ty()))),
                }
                Ok(())
            }
        }
    };
}
pub(crate) use op2_fn;

op2_fn!(
    pick_left,
    "Cannot pick left of {} and {}",
    this,
    other,
    (_, _, ())
);
op2_fn!(
    pick_right,
    "Cannot pick right of {} and {}",
    this,
    other,
    (a, b, **a = take(*b))
);
op2_fn!(
    modulus,
    "Cannot get remainder of {} % {}",
    this,
    other,
    (Value::Int(a), Value::Int(b), {
        let x = *b;
        let m = *a;
        *this = ((x % m + m) % m).into();
    }),
    (Value::Real(a), Value::Int(b), {
        let x = *b as f64;
        let m = *a;
        *this = (((x % m + m) % m) as i64).into()
    }),
    (Value::Int(a), Value::Real(b), {
        let x = *b;
        let m = *a as f64;
        *this = (((x % m + m) % m) as i64).into()
    }),
    (Value::Real(a), Value::Real(b), {
        let x = *b;
        let m = *a;
        *this = (((x % m + m) % m) as i64).into()
    })
);
op2_fn!(
    pow,
    "Cannot raise {} to {} power",
    this,
    other,
    (Value::Int(a), Value::Int(b), {
        *this = a.pow(*b as u32).into()
    }),
    (Value::Real(a), Value::Int(b), {
        *this = a.powi(*b as i32).into()
    }),
    (Value::Real(a), Value::Real(b), *this = a.powf(*b).into()),
);
op2_fn!(
    atan2,
    "Cannot get arctangent of {}/{}",
    this,
    other,
    (Value::Real(a), Value::Real(b), *this = a.atan2(*b).into()),
);
op2_fn!(
    get,
    "Cannot get index {} from {}",
    this,
    other,
    (Value::Byte(a), Value::String(b), {
        *this = b
            .chars()
            .nth(*a as usize)
            .map(Into::into)
            .unwrap_or(Value::Unit)
    }),
    (Value::Byte(a), Value::List(b), {
        *this = b.get(*a as usize).unwrap_or(Value::Unit)
    }),
    (Value::Byte(a), Value::Array(b), {
        *this = b.get(*a as usize).cloned().unwrap_or(Value::Unit)
    }),
    (Value::Int(a), Value::String(b), {
        *this = b
            .chars()
            .nth(*a as usize)
            .map(Into::into)
            .unwrap_or(Value::Unit)
    }),
    (Value::Int(a), Value::List(b), {
        *this = b.get(*a as usize).unwrap_or(Value::Unit)
    }),
    (Value::Int(a), Value::Array(b), {
        *this = b.get(*a as usize).cloned().unwrap_or(Value::Unit)
    }),
    (!env, Value::Array(a), b, {
        for index in a.iter_mut() {
            index.get(b, env)?;
        }
    }),
);
op2_fn!(
    push,
    "Cannot push {} onto {}",
    this,
    other,
    (Value::Char(a), Value::String(b), {
        Arc::make_mut(b).push(*a);
        swap(this, other);
    }),
    (a, Value::List(b), {
        b.push(take(a));
        swap(this, other);
    }),
    (a, Value::Array(b), {
        b.push(take(a));
        swap(this, other);
    }),
);
op2_fn!(
    concat,
    "Cannot concatenate {} and {}",
    this,
    other,
    (Value::String(a), Value::String(b), {
        swap(a, b);
        Arc::make_mut(a).push_str(b);
    }),
    (Value::List(a), Value::List(b), {
        swap(a, b);
        a.extend(b.iter());
    }),
    (Value::Array(a), Value::Array(b), {
        swap(a, b);
        a.extend(take(b).into_iter())
    }),
);

fn real_ordering(a: f64, b: f64) -> Ordering {
    match (a.is_nan(), b.is_nan()) {
        (true, true) => Ordering::Equal,
        (false, true) => Ordering::Less,
        (true, false) => Ordering::Greater,
        (false, false) => a.partial_cmp(&b).unwrap(),
    }
}

fn is_eq(ordering: Ordering) -> bool {
    ordering == Ordering::Equal
}
fn is_ne(ordering: Ordering) -> bool {
    ordering != Ordering::Equal
}
fn is_lt(ordering: Ordering) -> bool {
    ordering == Ordering::Less
}
fn is_le(ordering: Ordering) -> bool {
    ordering != Ordering::Greater
}
fn is_gt(ordering: Ordering) -> bool {
    ordering == Ordering::Greater
}
fn is_ge(ordering: Ordering) -> bool {
    ordering != Ordering::Less
}

macro_rules! cmp_fn {
    ($name:ident) => {
        op2_fn!(
            $name,
            "Cannot compare {} and {}",
            this,
            other,
            (Value::Unit, Value::Unit, {
                *this = $name(Ordering::Equal).into()
            }),
            (Value::Bool(a), Value::Bool(b), {
                *this = $name((*a).cmp(b)).into()
            }),
            (Value::Byte(a), Value::Byte(b), {
                *this = $name(a.cmp(&b)).into()
            }),
            (Value::Int(a), Value::Int(b), {
                *this = $name(a.cmp(&b)).into()
            }),
            (Value::Real(a), Value::Real(b), {
                *this = $name(real_ordering(*a, *b)).into()
            }),
            (Value::Byte(a), Value::Int(b), {
                *this = $name((*a as i64).cmp(b)).into()
            }),
            (Value::Int(a), Value::Byte(b), {
                *this = $name((*a).cmp(&(*b as i64))).into()
            }),
            (Value::Byte(a), Value::Real(b), {
                *this = $name(real_ordering(*a as f64, *b)).into()
            }),
            (Value::Real(a), Value::Byte(b), {
                *this = $name(real_ordering(*a, *b as f64)).into()
            }),
            (Value::Real(a), Value::Int(b), {
                *this = $name(real_ordering(*a, *b as f64)).into()
            }),
            (Value::Int(a), Value::Real(b), {
                *this = $name(real_ordering(*a as f64, *b)).into()
            }),
            (Value::Char(a), Value::Char(b), {
                *this = $name(a.cmp(&b)).into()
            }),
            (Value::String(a), Value::String(b), {
                *this = $name(a.cmp(&b)).into()
            }),
            (Value::Function(a), Value::Function(b), {
                *this = $name((*a).cmp(b)).into()
            }),
            (!env, Value::Partial(a), Value::Partial(b), {
                if $name(a.function.cmp(&b.function)) {
                    *this = true.into();
                } else {
                    let mut a_args = a.args.to_vec();
                    let mut b_args = b.args.to_vec();
                    for (a, b) in a_args.iter_mut().zip(&mut b_args) {
                        a.$name(b, env)?;
                        if let Value::Bool(true) = a {
                            *this = true.into();
                            return Ok(());
                        }
                    }
                    *this = false.into();
                }
            }),
        );
    };
}

cmp_fn!(is_eq);
cmp_fn!(is_ne);
cmp_fn!(is_lt);
cmp_fn!(is_le);
cmp_fn!(is_gt);
cmp_fn!(is_ge);

macro_rules! math_fn {
    ($trait:ident, $method:ident, $verb:literal, $this:ident, $other:ident
        $(,($a:pat, $b:pat, $f:expr))*
    $(,)?) => {
        op2_fn!(
            $method,
            concat!("Cannot ", $verb, " {} and {}"),
            $this,
            $other,
            (Value::Unit, Value::Unit, {}),
            (Value::Byte(a), Value::Byte(b), $trait::$method(a, *b)),
            (Value::Int(a), Value::Int(b), $trait::$method(a, *b)),
            (Value::Real(a), Value::Real(b), $trait::$method(a, *b)),
            (Value::Real(a), Value::Int(b), $trait::$method(a, *b as f64)),
            (Value::Int(a), Value::Real(b), $trait::$method(a, *b as i64)),
            $(($a, $b, $f),)*
        );
    };
}

math_fn!(
    AddAssign,
    add_assign,
    "add",
    this,
    other,
    (
        Value::Char(a),
        Value::Byte(b),
        *a = char::from_u32(*a as u32 + *b as u32).unwrap_or('\0')
    ),
    (
        Value::Char(a),
        Value::Int(b),
        *a = char::from_u32(*a as u32 + *b as u32).unwrap_or('\0')
    ),
);
math_fn!(
    SubAssign,
    sub_assign,
    "subtract",
    this,
    other,
    (
        Value::Char(a),
        Value::Char(b),
        *this = (*a as i64 - *b as i64).into()
    )
);
math_fn!(MulAssign, mul_assign, "multiply", this, other);
math_fn!(DivAssign, div_assign, "divide", this, other);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Algorithm {
    Compose,
    BlackBird,
    Flip,
    Fold,
    Each,
    Sum,
    Product,
    While,
    LeftThen,
    RightThen,
}

impl fmt::Display for Algorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Algorithm::Compose => write!(f, "compose"),
            Algorithm::BlackBird => write!(f, "blackbird"),
            Algorithm::Flip => write!(f, "flip"),
            Algorithm::Fold => write!(f, "fold"),
            Algorithm::Each => write!(f, "each"),
            Algorithm::Sum => write!(f, "sum"),
            Algorithm::Product => write!(f, "product"),
            Algorithm::While => write!(f, "while"),
            Algorithm::LeftThen => write!(f, "left_then"),
            Algorithm::RightThen => write!(f, "right_then"),
        }
    }
}

impl Algorithm {
    pub(crate) fn params(&self) -> u8 {
        match self {
            Algorithm::Compose => 3,
            Algorithm::BlackBird => 4,
            Algorithm::Flip => 3,
            Algorithm::Each => 2,
            Algorithm::Fold => 3,
            Algorithm::Sum => 1,
            Algorithm::Product => 1,
            Algorithm::While => 3,
            Algorithm::LeftThen => 3,
            Algorithm::RightThen => 3,
        }
    }
    pub(crate) fn instrs(&self, assembly: &Assembly) -> Vec<Instr> {
        #[allow(unused_imports)]
        use {
            crate::ast::BinOp::*,
            crate::builtin::{Op1, Op2},
            Instr::*,
        };
        let mut instrs = match self {
            Algorithm::Compose => vec![
                // x g f
                Rotate(3), // f x g
                Call(0),   // f gx
                Swap,      // gx f
                Call(0),   // f(gx)
            ],
            Algorithm::BlackBird => vec![
                // y x g f
                Rotate(4), // f y x g
                Call(0),   // f y gx
                Call(0),   // f gxy
                Swap,      // gxy f
                Call(0),   // f(gxy)
            ],
            Algorithm::Flip => vec![
                // b, a, f
                Rotate(3), // f, b, a
                Swap,      // f, a, b
                Move(3),   // a, b, f
                Call(0),
                Call(0),
            ],
            Algorithm::Fold => vec![
                // [1, 2, 3], 0, f = (_+_)
                Swap,              // [1, 2, 3], f, 0
                CopyRel(3),        // [1, 2, 3], f, 0, [1, 2, 3]
                Op1(Op1::Len),     // [1, 2, 3], f, 0, 3
                Push(0i64.into()), // [1, 2, 3], f, 0, 3, 0
                // Loop start
                CopyRel(2),          // [1, 2, 3], f, 0, 3, 0, 3
                CopyRel(2),          // [1, 2, 3], f, 0, 3, 0, 3, 0
                Op2(Op2::Eq, 0),     // [1, 2, 3], f, 0, 3, 0, false
                PopJumpIf(12, true), // [1, 2, 3], f, 0, 3, 0
                CopyRel(5),          // [1, 2, 3], f, 0, 3, 0, [1, 2, 3]
                CopyRel(2),          // [1, 2, 3], f, 0, 3, 0, [1, 2, 3], 0
                Op2(Op2::Get, 0),    // [1, 2, 3], f, 0, 3, 0, 1
                Move(4),             // [1, 2, 3], f, 3, 0, 1, 0
                CopyRel(5),          // [1, 2, 3], f, 3, 0, 1, 0, f
                Call(0),             // partial
                Call(0),             // [1, 2, 3], f, 3, 0, 1
                Rotate(3),           // [1, 2, 3], f, 1, 3, 0
                Push(1i64.into()),   // [1, 2, 3], f, 1, 3, 0, 1
                Op2(Op2::Add, 0),    // [1, 2, 3], f, 1, 3, 1
                Jump(-14),
                // Loop end
                Pop(2), // [1, 2, 3], f, 6
            ],
            Algorithm::Each => vec![
                // [1, 2, 3], f = (_ * 2)
                CopyRel(2),        // [1, 2, 3], f, [1, 2, 3]
                Op1(Op1::Default), // [1, 2, 3], f, []
                CopyRel(3),        // [1, 2, 3], f, [], [1, 2, 3]
                Op1(Op1::Len),     // [1, 2, 3], f, [], 3
                Push(0i64.into()), // [1, 2, 3], f, [], 3, 0
                // Loop start
                CopyRel(2),          // [1, 2, 3], f, [], 3, 0, 3
                CopyRel(2),          // [1, 2, 3], f, [], 3, 0, 3, 0
                Op2(Op2::Eq, 0),     // [1, 2, 3], f, [], 3, 0, false
                PopJumpIf(13, true), // [1, 2, 3], f, [], 3, 0
                CopyRel(5),          // [1, 2, 3], f, [], 3, 0, [1, 2, 3]
                CopyRel(2),          // [1, 2, 3], f, [], 3, 0, [1, 2, 3], 0
                Op2(Op2::Get, 0),    // [1, 2, 3], f, [], 3, 0, 1
                CopyRel(5),          // [1, 2, 3], f, [], 3, 0, 1, f
                Call(0),             // [1, 2, 3], f, [], 3, 0, 2
                Move(4),             // [1, 2, 3], f, 3, 0, 2, [],
                Swap,                // [1, 2, 3], f, 3, 0, [], 2
                Op2(Op2::Push, 0),   // [1, 2, 3], f, 3, 0, [2]
                Rotate(3),           // [1, 2, 3], f, [2], 3, 0
                Push(1i64.into()),   // [1, 2, 3], f, [2], 3, 0, 1
                Op2(Op2::Add, 0),    // [1, 2, 3], f, [2], 3, 1
                Jump(-15),
                // Loop end
                Pop(2), // [1, 2, 3], f, [2, 4, 6]
            ],
            Algorithm::Sum => {
                let fold = assembly.find_function(Algorithm::Fold).unwrap();
                let add = assembly.find_function(Op2::Add).unwrap();
                vec![
                    // [1, 2, 3]
                    Push(0i64.into()), // [1, 2, 3], 0
                    Push(add.into()),  // [1, 2, 3], 0, add
                    Push(fold.into()), // [1, 2, 3], 0, add, fold
                    Call(0),
                    Call(0),
                    Call(0),
                ]
            }
            Algorithm::Product => {
                let fold = assembly.find_function(Algorithm::Fold).unwrap();
                let mul = assembly.find_function(Op2::Mul).unwrap();
                vec![
                    // [1, 2, 3]
                    Push(1i64.into()), // [1, 2, 3], 0
                    Push(mul.into()),  // [1, 2, 3], 0, mul
                    Push(fold.into()), // [1, 2, 3], 0, mul, fold
                    Call(0),
                    Call(0),
                    Call(0),
                ]
            }
            Algorithm::While => vec![
                // 0, next = (+1), cond = (<3)
                Move(3), // next, cond, 0
                // Loop start
                CopyRel(1),          // next, cond, 0, 0
                CopyRel(3),          // next, cond, 0, 0, cond
                Call(0),             // next, cond, 0, true
                PopJumpIf(4, false), // next, cond, 0
                CopyRel(3),          // next, cond, 0, next
                Call(0),             // next, cond, 1
                Jump(-6),
            ],
            Algorithm::LeftThen => vec![
                // x f g
                CopyRel(3), // x, f, g, x
                Swap,       // x, f, x, g
                Call(0),    // x, f, gx
                Swap,       // x, gx, f
                Call(0),    // x, f(gx)
                Call(0),    // f(gx)x
            ],
            Algorithm::RightThen => vec![
                // x g f
                CopyRel(3), // x, g, f, x
                Move(3),    // x, f, x, g
                Call(0),    // x, f, gx
                Rotate(3),  // gx, x, f
                Call(0),    // gx, fx
                Call(0),    // fx(gx)
            ],
        };
        instrs.insert(0, Comment(self.to_string()));
        instrs
    }
}
