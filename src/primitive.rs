use std::{
    f64::{consts::PI, INFINITY},
    fmt,
    mem::take,
    rc::Rc,
    sync::OnceLock,
};

use crate::{
    algorithm::loops, function::FunctionId, io::*, lex::Simple, value::*, Uiua, UiuaError,
    UiuaResult,
};

macro_rules! primitive {
    ($(
        $(#[doc = $doc:literal])*
        (
            $($($args:literal)? $([$antiargs:literal])? $(($outputs:expr))? $({$antioutputs:literal})?,)?
            $name:ident $({$modifier:ident: $margs:literal})?
            $(,$ident:literal)? $(,$ascii:ident)? $(+ $character:literal)?
        )
    ),* $(,)?) => {
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
            pub fn doc(&self) -> Option<&'static PrimDoc> {
                match self {
                    $(Primitive::$name => {
                        let doc_str = concat!($($doc, "\n"),*);
                        static DOC: OnceLock<PrimDoc> = OnceLock::new();
                        if doc_str.is_empty() {
                            return None;
                        }
                        Some(DOC.get_or_init(|| PrimDoc::from_str(doc_str)))
                    },)*
                    _ => None,
                }
            }
        }
    };
}

#[derive(Debug)]
pub struct PrimDoc {
    pub intro: String,
    pub examples: Vec<PrimExample>,
    pub outro: String,
}

impl PrimDoc {
    fn from_str(s: &str) -> Self {
        let mut intro = String::new();
        let mut examples = Vec::new();
        let mut got_break = false;
        let mut primer = String::new();
        for line in s.lines() {
            let line = line.trim();
            if line.is_empty() {
                got_break = true;
            } else if let Some(ex) = line.strip_prefix("ex:") {
                let input = ex.trim().to_owned();
                examples.push(PrimExample {
                    primer: take(&mut primer),
                    input,
                    output: OnceLock::new(),
                });
            } else if got_break {
                primer.push_str(line);
                primer.push('\n');
            } else {
                intro.push_str(line);
                intro.push('\n');
            }
        }
        let outro = take(&mut primer);
        Self {
            intro,
            examples,
            outro,
        }
    }
}

impl fmt::Display for PrimDoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.intro.trim())?;
        for ex in &self.examples {
            if !ex.primer.is_empty() {
                writeln!(f, "primer: {}", ex.primer)?;
            }
            writeln!(f, "ex: {}", ex.input)?;
            match ex.output() {
                Ok(output) => {
                    for formatted in output {
                        for (i, line) in formatted.lines().enumerate() {
                            if i == 0 {
                                write!(f, " => ")?
                            } else {
                                write!(f, "    ")?;
                            }
                            writeln!(f, "{line}")?;
                        }
                    }
                }
                Err(e) => {
                    writeln!(f, " => error: {e}")?;
                }
            }
        }
        if !self.outro.is_empty() {
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct PrimExample {
    pub primer: String,
    pub input: String,
    output: OnceLock<Result<Vec<String>, String>>,
}

impl PrimExample {
    pub fn output(&self) -> &Result<Vec<String>, String> {
        self.output.get_or_init(|| {
            Uiua::with_backend(&StdIo)
                .load_str(&self.input)
                .map(|env| env.take_stack().into_iter().map(|val| val.show()).collect())
                .map_err(|e| {
                    e.to_string()
                        .lines()
                        .next()
                        .unwrap_or_default()
                        .split_once(' ')
                        .unwrap_or_default()
                        .1
                        .into()
                })
        })
    }
}

primitive!(
    // Stack ops
    /// Duplicate the top value on the stack
    ///
    /// ex: . 1 2 3
    (1(2), Dup, "duplicate" + '.'),
    /// Duplicate the second-to-top value to the top of the stack
    ///
    /// ex: , 1 2 3
    (2(3), Over, "over" + ','),
    /// Swap the top two values on the stack
    ///
    /// ex: ~ 1 2 3
    (2(2), Flip, "flip" + '~'),
    /// Pop the top value off the stack
    (1(0), Pop, "pop" + ';'),
    /// Pop the top value off the stack and push it to the antistack
    ///
    /// This is the inverse of [load].
    /// ex: ⇞ 1 2 ⇟ 3
    (1(0){1}, Save, "save" + '⇟'),
    /// Pop the top value off the antistack and push it to the stack
    ///
    /// This is the inverse of [save].
    /// ex: ⇞ 1 2 ⇟ 3
    (0[1](1), Load, "load" + '⇞'),
    // Pervasive monadic ops
    /// Logical not (equivalent to 1 - x)
    ///
    /// ex: ¬1
    /// ex: ¬[0 1 1 0]
    (1, Not, "not" + '¬'),
    /// Numerical sign (1, -1, or 0)
    ///
    /// ex: $ 1
    /// ex: $ -5
    /// ex: $ [-2 -1 0 1 2]
    (1, Sign, "sign" + '$'),
    /// Negate a number
    ///
    /// ex: ¯ 1
    /// ex: ¯ ¯3
    (1, Neg, "negate", Backtick + '¯'),
    (1, Abs, "absolute value" + '⌵'),
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
    /// The number of rows in an array
    ///
    /// ex: ≢5
    /// ex: ≢[]
    /// ex: ≢1_2_3
    /// ex: ≢[1_2 3_4 5_6]
    (1, Len, "length" + '≢'),
    /// The number of dimensions in an array
    ///
    /// ex: ∴5
    /// ex: ∴[]
    /// ex: ∴1_2_3
    /// ex: ∴[1_2 3_4 5_6]
    (1, Rank, "rank" + '∴'),
    /// The dimensions of an array
    ///
    /// ex: △5
    /// ex: △[]
    /// ex: △1_2_3
    /// ex: △[1_2 3_4 5_6]
    (1, Shape, "shape" + '△'),
    /// Make an array of [0, x)
    ///
    /// ex: ⇡5
    /// ex: ⇡2_3
    (1, Range, "range" + '⇡'),
    /// The first element of an array
    ///
    /// ex: ⊢1_2_3
    (1, First, "first" + '⊢'),
    /// The last element of an array
    (1, Last),
    /// Allow an array to be combined with arrays of incompatible shapes
    (1, Fill, "fill" + '∘'),
    /// Remove fill elements from the end of an array
    (1, Truncate, "truncate" + '⍛'),
    /// Reverse the rows of an array
    ///
    /// ex: ⇌1_2_3_9
    /// ex: ⇌[1_2 3_4 5_6]
    (1, Reverse, "reverse" + '⇌'),
    /// Make an array 1-dimensional
    ///
    /// ex: ♭[1_2 3_4 5_6]
    (1, Deshape, "deshape" + '♭'),
    /// Rotate the shape of an array
    ///
    /// ex: ⍉.[1_2 3_4 5_6]
    /// ex: ⍉.[[1_2 3_4] [5_6 7_8]]
    (1, Transpose, "transpose" + '⍉'),
    (1, InvTranspose),
    /// Sort the rows of an array
    ///
    /// ex: ∧6_2_7_0_¯1_5
    (1, Sort, "sort" + '∧'),
    /// Grade the rows of an array
    ///
    /// ex: ⍋6_2_7_0_¯1_5
    (1, Grade, "grade" + '⍋'),
    /// Repeat the index of each array element the element's value times
    ///
    /// ex: ⊙2_0_4_1
    (1, Indices, "indices" + '⊙'),
    /// Assign a unique index to each unique element in an array
    ///
    /// ex: ⊛7_7_8_0_1_2_0
    (1, Classify, "classify" + '⊛'),
    /// Remove duplicate elements from an array
    ///
    /// ex: ⊝7_7_8_0_1_2_0
    (1, Deduplicate, "deduplicate" + '⊝'),
    // Dyadic array ops
    /// Check if two arrays' elements match exactly
    ///
    /// ex: ≅ 1_2_3 [1 2 3]
    ///
    /// ex: ≅ 1_2_3 [1 2]
    (2, Match, "match" + '≅'),
    /// Check if two arrays' elements do not match exactly
    ///
    /// ex: ≇ 1_2_3 [1 2 3]
    ///
    /// ex: ≇ 1_2_3 [1 2]
    (2, NoMatch, "notmatch" + '≇'),
    /// Append two arrays or an array and a scalar
    ///
    /// ex: ⊂ 1 2
    ///
    /// ex: ⊂ 1 [2 3]
    ///
    /// ex: ⊂ [1 2] 3
    ///
    /// ex: ⊂ [1 2] [3 4]
    (2, Join, "join" + '⊂'),
    /// Combine two arrays as rows
    ///
    /// ex: ⊟ [1 2 3] [4 5 6]
    ///
    /// ex: ⊟ [1 2 3] [4 5]
    ///
    /// Couple can utilize [fill].
    /// ex: ⊟ [1 2 3] ∘[4 5]
    (2, Couple, "couple" + '⊟'),
    /// Index a single row or element from an array
    ///
    /// ex: ⊡ 2 [8 3 9 2 0]
    (2, Pick, "pick" + '⊡'),
    /// Select multiple elements from an array
    ///
    /// ex: ⊏ 4_2 [8 3 9 2 0]
    (2, Select, "select" + '⊏'),
    /// Take the first n elements of an array
    ///
    /// ex: ↙ 3 [8 3 9 2 0]
    (2, Take, "take" + '↙'),
    /// Drop the first n elements of an array
    ///
    /// ex: ↘ 3 [8 3 9 2 0]
    (2, Drop, "drop" + '↘'),
    /// Change the shape of an array
    ///
    /// ex: ↯ 2_3 [1 2 3 4 5 6]
    (2, Reshape, "reshape" + '↯'),
    /// Rotate the elements of an array by n
    ///
    /// ex: ↻1 ⇡5
    /// ex: ↻2 ⇡5
    /// ex: ↻¯1 ⇡5
    (2, Rotate, "rotate" + '↻'),
    /// The n-wise windows of an array
    ///
    /// ex: ◫2 .⇡4
    /// ex: ◫4 .⇡6
    /// ex: ◫ 2_2 .[1_2_3 4_5_6 7_8_9]
    (2, Windows, "windows" + '◫'),
    /// Use an array to replicate the elements of another array
    ///
    /// ex: ‡ [1 0 2 1 4] [8 3 9 2 0]
    ///
    /// This can be used as a filter.
    /// ex: ‡ ≥'a' ."lOWERCASe onLY"
    (2, Replicate, "replicate" + '‡'),
    /// Check if each element of an array is a member of another array
    ///
    /// ex: ∊ [1 2 3] [0 3 4 5 1]
    (2, Member, "member" + '∊'),
    /// Find the indices of an element in an array
    ///
    /// ex: ⌕ 5 [1 8 5 2 3 5 4 5 6 7]
    (2, Find, "find" + '⌕'),
    /// Find the first index of an element in an array
    ///
    /// ex: ⊗ 5 [1 8 5 2 3 5 4 5 6 7]
    (2, IndexOf, "indexof" + '⊗'),
    /// Group elements of an array into buckets by index
    ///
    /// ex: ⊕ [0 1 0 2 1 1] [1 2 3 4 5 6]
    /// ex: ⊕ =0◿2. [1 2 3 4 5 6]
    (2, Group, "group" + '⊕'),
    /// Group elements of an array into buckets by sequential keys
    ///
    /// ex: ⊘ [1 1 2 2 2 3] [1 2 3 4 5 6]
    /// ex: ⊘ ≠' '. "Hey there friendo"
    (2, Partition, "partition" + '⊘'),
    // Modifiers
    /// Apply a reducing function to an array
    /// For reducing with an initial value, see [fold].
    ///
    /// ex: /+ 1_2_3_4_5
    /// ex: /- 1_2_3_4_5
    /// ex: /(-~) 1_2_3_4_5
    /// ex: /(×+1) 1_2_3_4_5
    (Reduce { modifier: 1 }, "reduce" + '/'),
    /// Apply a reducing function to an array with an initial value
    /// For reducing without an initial value, see [reduce].
    ///
    /// ex: ⌿+ 10 1_2_3_4
    (Fold { modifier: 1 }, "fold" + '⌿'),
    /// Reduce, but keep intermediate values
    ///
    /// ex: \+ 1_2_3_4
    /// ex: \- 1_2_3_4
    /// ex: \(-~) 1_2_3_4
    /// ex: \(⊂∘) 1_2_3_4
    (Scan { modifier: 1 }, "scan" + '\\'),
    /// Apply a function to each element of an array
    ///
    /// ex: ∵(⊟.) 1_2_3_4
    /// ex: ∵(∘⇡) 1_2_3_4
    (Each { modifier: 1 }, "each" + '∵'),
    /// Pervade a function through two arrays
    /// For operations that are already pervasive, like [add], this is redundant.
    ///
    /// ex: ∺⊂ 1_2_3 4_5_6
    /// ex: ∺⊂ 1_2 [4_5 6_7]
    (Zip { modifier: 1 }, "zip" + '∺'),
    /// Apply a function to each row of an array
    ///
    /// ex: /+ [1_2_3 4_5_6 7_8_9]  # Sum columns
    /// ex: ≡/+ [1_2_3 4_5_6 7_8_9]  # Sum rows
    (Rows { modifier: 1 }, "rows" + '≡'),
    /// Apply a function to each pair of rows in two arrays
    ///
    /// ex: ≑⊂ 1_2 [4_5 6_7]
    /// ex: ≑⌿+ 1_2 [4_5 6_7]
    (Bridge { modifier: 1 }, "bridge" + '≑'),
    /// Apply a function to a fixed value and each row of an array
    ///
    /// ex: ∹⊂ 2_3_4 1
    /// ex: ∹⊂ 1_2_3 4_5_6
    (Distribute { modifier: 1 }, "distribute" + '∹'),
    /// Apply a function to each combination of elements of two arrays
    ///
    /// ex: ⊞+ 1_2_3 4_5_6
    /// ex: ⊞⊂ 1_2 3_4
    (Table { modifier: 1 }, "table" + '⊞'),
    /// Repeat a function n times
    ///
    /// ex: ⍥(+2) 0 5
    /// ex: ⍥(⊂2) [] 5
    (Repeat { modifier: 1 }, "repeat" + '⍥'),
    /// Invert the behavior of a function
    /// Most functions are not invertible.
    ///
    /// ex: √2
    /// ex: ↶√2
    (Invert { modifier: 1 }, "invert" + '↶'),
    /// Apply a function under another
    /// This is a more powerful version of [invert].
    ///
    /// [under] takes 2 functions f and g and another argument x.
    /// It applies f to x, then applies g to the result.
    /// It then applies the inverse of f to the result of g.
    ///
    /// Here, we negate 5, subtract 2, then negate again.
    /// ex: ⍜¯(-2) 5
    ///
    /// Uiua has no built-in function to get the last element of an array.
    /// Instead, we can use [first] under [reverse]:
    /// ex: ⍜⇌⊢ 1_2_3
    (Under { modifier: 2 }, "under" + '⍜'),
    /// Apply a function at a different array depth
    ///
    /// ex: ↯2_2_3 ⇡12
    /// ex: /+ ↯2_2_3 ⇡12
    /// ex: ⍚0/+ ↯2_2_3 ⇡12
    /// ex: ⍚¯1/+ ↯2_2_3 ⇡12
    /// ex: ⍚¯2/+ ↯2_2_3 ⇡12
    (Level { modifier: 2 }, "level" + '⍚'),
    /// Call a function and catch errors
    ///
    /// ex: ?(+1 2)"failure"
    /// ex: ?(+'a' 'b')"failure"
    (Try { modifier: 2 }, "try" + '?'),
    // Misc
    /// Throw an error
    ///
    /// ex: !"Oh no!" "any array"
    /// ex: !"Oh no!" 1
    /// ex: !"Oh no!" 0
    (2, Throw, "throw" + '!'),
    /// Break out of a loop
    /// Expects a non-negative integer. This integer is how many loops will be broken out of.
    /// Not all looping functions can be broken out of.
    ///
    /// ex: /(⎋>10.+) ⇌⇡40  # Break when the sum exceeds 10
    /// ex: ⍥(⎋>100.×2) 1 40  # Break when the product exceeds 100
    (1(0), Break, "break" + '⎋'),
    /// Call the current function recursively
    /// Expects a non-negative integer. Recursion happens when the integer is not 0.
    /// Only dfns can be recurred in.
    ///
    /// ex: {↬<10.×2} 1 # Recur if the product is less than 10
    (1(0), Recur, "recur" + '↬'),
    /// Debug print a value without popping it
    ///
    /// ex: /+ | 1_2_3
    (1, Debug, "debug" + '|'),
    /// Call a function
    ///
    /// ex: :(+) 1 2
    (1(None), Call, "call" + ':'),
    /// Do nothing
    (0, Noop, "noop" + '·'),
    /// Convert a value to a string
    ///
    /// ex: string 5
    (1, String, "string"),
    /// Parse a string as a number
    ///
    /// ex: parsenum "17"
    /// ex: parsenum "3.1415926535897932"
    /// ex: parsenum "dog"
    (1, Parse, "parsenumber"),
    /// Import a function from another file
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
            Save => Load,
            Load => Save,
            Transpose => InvTranspose,
            InvTranspose => Transpose,
            Debug => Debug,
            _ => return None,
        })
    }
    pub fn from_name(name: &str) -> Option<Self> {
        if name.chars().any(char::is_uppercase) {
            return None;
        }
        if let Some(io) = IoOp::from_name(name) {
            return Some(Primitive::Io(io));
        }
        if name == "pi" || name == "π" {
            return Some(Primitive::Pi);
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching = Primitive::ALL
            .into_iter()
            .filter(|p| p.name().map_or(false, |pn| pn.starts_with(name)));
        let res = matching.next()?;
        let exact_match = res.name().map_or(false, |i| i == name);
        (exact_match || matching.next().is_none()).then_some(res)
    }
    pub fn from_multiname(name: &str) -> Option<Vec<(Self, &str)>> {
        if name == "pi" || name == "π" {
            return Some(vec![(Primitive::Pi, name)]);
        }
        if name.len() < 3 {
            return None;
        }
        let mut start = 0;
        let indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
        let mut prims = Vec::new();
        'outer: loop {
            if start == name.len() {
                break Some(prims);
            }
            for len in (3..=name.len() - start).rev() {
                let start_index = indices[start];
                let end_index = indices[start + len - 1];
                if let Some(p) = Primitive::from_name(&name[start_index..=end_index]) {
                    prims.push((p, &name[start_index..=end_index]));
                    start += len;
                    continue 'outer;
                }
            }
            break None;
        }
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
            Primitive::InvTranspose => env.monadic_mut(Value::inv_transpose)?,
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
            Primitive::Range => env.monadic_ref_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_env(Value::first)?,
            Primitive::Last => env.monadic_env(Value::last)?,
            Primitive::Len => env.monadic_ref(Value::len)?,
            Primitive::Rank => env.monadic_ref(Value::rank)?,
            Primitive::Fill => env.monadic_mut(|v| *v.fill_mut() = true)?,
            Primitive::Truncate => env.monadic_mut(Value::truncate)?,
            Primitive::Fold => loops::fold(env)?,
            Primitive::Reduce => loops::reduce(env)?,
            Primitive::Each => loops::each(env)?,
            Primitive::Zip => loops::zip(env)?,
            Primitive::Rows => loops::rows(env)?,
            Primitive::Bridge => loops::bridge(env)?,
            Primitive::Distribute => loops::distribute(env)?,
            Primitive::Table => loops::table(env)?,
            Primitive::Scan => loops::scan(env)?,
            Primitive::Repeat => loops::repeat(env)?,
            Primitive::Level => loops::rank(env)?,
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
            Primitive::Recur => {
                let n = env.pop(1)?.as_nat(env, "recur expects a natural number")?;
                if n > 0 {
                    env.recur()?
                }
            }
            Primitive::Debug => {
                let value = env.pop(1)?;
                env.io.print_str(&value.show()).map_err(|e| env.error(e))?;
                env.io.print_str("\n").map_err(|e| env.error(e))?;
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
            Primitive::Invert => {
                let f = env.pop(1)?;
                let inv_f = f.invert(env)?;
                env.push(inv_f);
                env.call()?;
            }
            Primitive::Under => {
                let f = env.pop(1)?;
                let g = env.pop(2)?;
                let inv_f = f.invert(env)?;
                env.push_ref(f);
                env.call()?;
                env.push_ref(g);
                env.call()?;
                env.push(inv_f);
                env.call()?;
            }
            Primitive::Throw => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if cond.as_nat(env, "").map_or(true, |n| n == 0) {
                    return Err(UiuaError::Throw(msg, env.span().clone()));
                }
            }
            Primitive::Shape => {
                env.monadic_ref(|v| v.shape().iter().copied().collect::<Value>())?
            }
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

#[cfg(test)]
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

#[cfg(test)]
#[test]
fn from_multiname() {
    assert!(matches!(
        &*Primitive::from_multiname("rev").expect("rev"),
        [(Primitive::Reverse, _)]
    ));
    assert!(matches!(
        &*Primitive::from_multiname("revrev").expect("revrev"),
        [(Primitive::Reverse, _), (Primitive::Reverse, _)]
    ));
    assert!(matches!(
        &*Primitive::from_multiname("tabrepl").unwrap(),
        [(Primitive::Table, _), (Primitive::Replicate, _)]
    ));
    assert_eq!(Primitive::from_multiname("foo"), None);
}

#[cfg(test)]
#[test]
fn word_collisions() {
    let mut collisions = 0;
    for word in std::fs::read_to_string("src/words.txt").unwrap().lines() {
        if let Some(prims) = Primitive::from_multiname(word) {
            println!("{word:>10}: {prims:?}");
            collisions += 1;
        }
    }
    println!("{collisions} collisions")
}
