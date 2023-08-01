use std::{
    cell::RefCell,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    fmt,
    mem::take,
    sync::OnceLock,
};

use enum_iterator::{all, Sequence};
use rand::prelude::*;

use crate::{
    algorithm::loops, function::FunctionId, io::*, lex::Simple, value::*, Uiua, UiuaError,
    UiuaResult,
};

macro_rules! primitive {
    ($(
        $(#[doc = $doc:literal])*
        (
            $($($args:literal)? $([$antiargs:literal])? $(($outputs:expr))? $({$antioutputs:literal})?,)?
            $name:ident, $class:ident $({$modifier:ident: $margs:literal})?
            $(,$ident:literal)? $(,$ascii:ident)? $(+ $character:literal)?
        )
    ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
        pub enum Primitive {
            $($name,)*
            Io(IoOp)
        }

        impl Primitive {
            pub fn all() -> impl Iterator<Item = Self> {
                all()
            }
            #[allow(path_statements)]
            pub fn name(&self) -> Option<&'static str > {
                match self {
                    $(Primitive::$name => { None::<&'static str> $(;Some($ident))? },)*
                    Primitive::Io(op) => Some(op.name())
                }
            }
            pub fn class(&self) -> PrimClass {
                match self {
                    $(Primitive::$name => PrimClass::$class,)*
                    Primitive::Io(_) => PrimClass::Io,
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
                        Some(DOC.get_or_init(|| PrimDoc::from_lines(doc_str)))
                    },)*
                    Primitive::Io(op) => op.doc(),
                }
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Sequence)]
pub enum PrimClass {
    Stack,
    Control,
    MonadicPervasive,
    DyadicPervasive,
    MonadicArray,
    DyadicArray,
    MonadicModifier,
    DyadicModifier,
    OtherModifier,
    Misc,
    Constant,
    Io,
}

impl PrimClass {
    pub fn all() -> impl Iterator<Item = Self> {
        all()
    }
}

primitive!(
    // Stack ops
    /// Duplicate the top value on the stack
    ///
    /// ex: [. 1 2 3]
    ///
    /// This can be used to make a monadic left-hook, such as a palindrome checker:
    /// ex: ≅⇌. "friend"
    /// ex: ≅⇌. "racecar"
    (1(2), Dup, Stack, "duplicate" + '.'),
    /// Duplicate the second-to-top value to the top of the stack
    ///
    /// ex: [, 1 2 3]
    (2(3), Over, Stack, "over" + ','),
    /// Swap the top two values on the stack
    ///
    /// ex: [~ 1 2 3 4]
    ///
    /// When combined with [duplicate], this can be used to make a monadic right-hook or monadic fork, such as an average calculator:
    /// ex: ÷≢~/+. 1_8_2_5
    (2(2), Flip, Stack, "flip" + '~'),
    /// Pop the top value off the stack
    (1(0), Pop, Stack, "pop" + ';'),
    // Pervasive monadic ops
    /// Logical not (equivalent to `1 - x`)
    ///
    /// ex: ¬1
    /// ex: ¬[0 1 1 0]
    (1, Not, MonadicPervasive, "not" + '¬'),
    /// Numerical sign (1, ¯1, or 0)
    ///
    /// ex: $ 1
    /// ex: $ ¯5
    /// ex: $ [¯2 ¯1 0 1 2]
    (1, Sign, MonadicPervasive, "sign" + '$'),
    /// Negate a number
    ///
    /// ex: ¯ 1
    /// ex: ¯ ¯3
    (1, Neg, MonadicPervasive, "negate", Backtick + '¯'),
    /// The absolute value of a number
    ///
    /// ex: ⌵ ¯1
    /// ex: ⌵ 1
    ///
    /// The symbol looks like the graph of `|x|`.
    (1, Abs, MonadicPervasive, "absolute value" + '⌵'),
    /// The square root of a number
    (1, Sqrt, MonadicPervasive, "sqrt" + '√'),
    /// The sine of a number
    ///
    /// You can get an arcsine function with [invert].
    (1, Sin, MonadicPervasive, "sine"),
    /// The cosine of a number
    ///
    /// You can get an arccosine function with [invert].
    (1, Cos, MonadicPervasive, "cosine"),
    /// The tangent of a number
    (1, Tan, MonadicPervasive, "tangent"),
    (1, Asin, MonadicPervasive),
    (1, Acos, MonadicPervasive),
    /// Round to the nearest integer towards `¯∞`
    (1, Floor, MonadicPervasive, "floor" + '⌊'),
    /// Round to the nearest integer towards `∞`
    (1, Ceil, MonadicPervasive, "ceiling" + '⌈'),
    /// Round to the nearest integer
    (1, Round, MonadicPervasive, "round" + '⁅'),
    // Pervasive dyadic ops
    (2, Eq, DyadicPervasive, "equals", Equal),
    (2, Ne, DyadicPervasive, "not equals", BangEqual + '≠'),
    (2, Lt, DyadicPervasive, "less than" + '<'),
    (2, Le, DyadicPervasive, "less or equal", LessEqual + '≤'),
    (2, Gt, DyadicPervasive, "greater than" + '>'),
    (
        2,
        Ge,
        DyadicPervasive,
        "greater or equal",
        GreaterEqual + '≥'
    ),
    (2, Add, DyadicPervasive, "add" + '+'),
    (2, Sub, DyadicPervasive, "subtract" + '-'),
    (2, Mul, DyadicPervasive, "multiply", Star + '×'),
    (2, Div, DyadicPervasive, "divide", Percent + '÷'),
    (2, Mod, DyadicPervasive, "modulus" + '◿'),
    (2, Pow, DyadicPervasive, "power" + 'ⁿ'),
    (2, Log, DyadicPervasive),
    /// Take the minimum of two arrays
    ///
    /// ex: ↧ 3 5
    /// ex: ↧ [1 4 2] [3 7 1]
    (2, Min, DyadicPervasive, "minimum" + '↧'),
    /// Take the maximum of two arrays
    ///
    /// ex: ↥ 3 5
    /// ex: ↥ [1 4 2] [3 7 1]
    (2, Max, DyadicPervasive, "maximum" + '↥'),
    /// The arctangent of two numbers
    ///
    /// This takes a `y` and `x` argument and returns the angle in radians in the range `(-π, π]`.
    ///
    /// ex: ∠ 1 0
    /// ex: ∠ ¯1 0
    /// ex: ∠ √2 √2
    (2, Atan, DyadicPervasive, "atangent" + '∠'),
    // Monadic array ops
    /// The number of rows in an array
    ///
    /// ex: ≢5
    /// ex: ≢[]
    /// ex: ≢1_2_3
    /// ex: ≢[1_2 3_4 5_6]
    (1, Len, MonadicArray, "length" + '≢'),
    /// The number of dimensions in an array
    ///
    /// [rank] is equivalent to [length] of [shape].
    ///
    /// ex: ∴5
    /// ex: ∴[]
    /// ex: ∴1_2_3
    /// ex: ∴[1_2 3_4 5_6]
    ///
    /// It is three dots arranged in a triangle because it is counting [shape].
    (1, Rank, MonadicArray, "rank" + '∴'),
    /// The dimensions of an array
    ///
    /// ex: △5
    /// ex: △[]
    /// ex: △1_2_3
    /// ex: △[1_2 3_4 5_6]
    ///
    /// It is a triangle because a triangle is a shape.
    (1, Shape, MonadicArray, "shape" + '△'),
    /// Make an array of [0, x)
    ///
    /// ex: ⇡5
    /// ex: ⇡2_3
    (1, Range, MonadicArray, "range" + '⇡'),
    /// The first row of an array
    ///
    /// ex: ⊢1_2_3
    /// ex: ⊢[1_2 3_4 5_6]
    /// ex: ⊢[]
    /// ex: ⊢1
    (1, First, MonadicArray, "first" + '⊢'),
    /// The last element of an array
    (1, Last, MonadicArray),
    /// Remove fill elements from the end of an array
    ///
    /// ex: /· \⊂1_2_3_4
    /// ex: /⌀ \⊂1_2_3_4
    (1, Truncate, MonadicArray, "truncate" + '⌀'),
    /// Reverse the rows of an array
    ///
    /// ex: ⇌1_2_3_9
    /// ex: ⇌[1_2 3_4 5_6]
    (1, Reverse, MonadicArray, "reverse" + '⇌'),
    /// Make an array 1-dimensional
    ///
    /// ex: ♭5
    /// ex: ♭[1_2 3_4 5_6]
    ///
    /// It looks like `♭` because it flattens the array.
    ///
    /// See also: [reshape]
    (1, Deshape, MonadicArray, "deshape" + '♭'),
    /// Rotate the shape of an array
    ///
    /// ex: ⍉.[1_2 3_4 5_6]
    /// ex: ⍉.[[1_2 3_4] [5_6 7_8]]
    (1, Transpose, MonadicArray, "transpose" + '⍉'),
    (1, InvTranspose, MonadicArray),
    /// Sort the rows of an array
    ///
    /// ex: ∧6_2_7_0_¯1_5
    ///
    /// See also: [grade]
    (1, Sort, MonadicArray, "sort" + '∧'),
    /// Grade the rows of an array
    ///
    /// ex: ⍋6_2_7_0_¯1_5
    ///
    /// Using the grading as a selector in [select] yields the sorted array.
    /// ex: ⊏⍋.6_2_7_0_¯1_5
    ///
    /// See also: [sort]
    (1, Grade, MonadicArray, "grade" + '⍋'),
    /// Repeat the index of each array element the element's value times
    ///
    /// ex: ⊙2_0_4_1
    (1, Indices, MonadicArray, "indices" + '⊙'),
    /// Assign a unique index to each unique element in an array
    ///
    /// ex: ⊛7_7_8_0_1_2_0
    (1, Classify, MonadicArray, "classify" + '⊛'),
    /// Remove duplicate elements from an array
    ///
    /// ex: ⊝7_7_8_0_1_2_0
    (1, Deduplicate, MonadicArray, "deduplicate" + '⊝'),
    // Dyadic array ops
    /// Check if two arrays are the same, ignoring fill elements
    ///
    /// ex: ≅ 1_2_3 [1 2 3]
    /// ex: ≅ 1_2_3 [1 2]
    ///
    /// See also: [notmatch]
    (2, Match, DyadicArray, "match" + '≅'),
    /// Check if two arrays are not the same, ignoring fill elements
    ///
    /// ex: ≇ 1_2_3 [1 2 3]
    /// ex: ≇ 1_2_3 [1 2]
    ///
    /// See also: [match]
    (2, NotMatch, DyadicArray, "notmatch" + '≇'),
    /// Append two arrays or an array and a scalar
    ///
    /// For scalars, it is equivalent to [couple].
    /// ex: ⊂ 1 2
    ///
    /// If the arrays have the same [rank], it will append the second array to the first.
    /// ex: ⊂ [1 2] [3 4]
    ///
    /// If the arrays have the same [rank], but their shapes are different, then the arrays will be filled so that the join makes sense.
    /// ex: ⊂ [1_2 3_4] [5_6_7 8_9_10]
    ///
    /// If the arrays have a [rank] difference of 1, then the array with the smaller [rank] will be prepended or appended to the other as a row.
    /// ex: ⊂ 1 [2 3]
    /// ex: ⊂ [1 2] 3
    /// ex: ⊂ [1_2] [3_4 5_6]
    /// ex: ⊂ [1_2 3_4] [5_6]
    ///
    /// If the arrays have a [rank] difference greater than 1, then the arrays will be filled so that the join makes sense.
    /// ex: ⊂ 1 [2_3 4_5]
    /// ex: ⊂ [1_2] [[3_4 5_6] [7_8 9_10]]
    (2, Join, DyadicArray, "join" + '⊂'),
    /// Combine two arrays as rows of a new array
    ///
    /// For scalars, it is equivalent to [join].
    /// ex: ⊟ 1 2
    ///
    /// For arrays, a new array is created with the first array as the first row and the second array as the second row.
    /// ex: ⊟ [1 2 3] [4 5 6]
    ///
    /// Before coupling, the arrays are filled to make their [shape]s match.
    /// ex: ⊟ [1 2 3] [4 5]
    /// ex: ⊟ 1 [2 3]
    /// ex: ⊟ [1 2 3] [4_5 6_7]
    ///
    /// [first] of the [shape] of the coupled array will *always* be `2`.
    (2, Couple, DyadicArray, "couple" + '⊟'),
    /// Replace the fill elements of an array with a scalar
    ///
    /// ex: ⍛∞.\⊂1_2_3_4
    (2, Fill, DyadicArray, "fill" + '⍛'),
    /// Index a single row or element from an array
    ///
    /// ex: ⊡ 2 [8 3 9 2 0]
    /// ex: ⊡ 1_1 .[1_2_3 4_5_6]
    ///
    /// [pick] combined with [call] is the easiest way to emulate an if-else expression.
    /// ex: :⊡~("not 5")_("5") =5 3
    (2, Pick, DyadicArray, "pick" + '⊡'),
    /// Select multiple elements from an array
    ///
    /// ex: ⊏ 4_2 [8 3 9 2 0]
    (2, Select, DyadicArray, "select" + '⊏'),
    /// Take the first n elements of an array
    /// This is the opposite of [drop].
    ///
    /// ex: ↙ 3 [8 3 9 2 0]
    /// ex: ↙ ¯3 [8 3 9 2 0]
    (2, Take, DyadicArray, "take" + '↙'),
    /// Drop the first n elements of an array
    /// This is the opposite of [take].
    ///
    /// ex: ↘ 3 [8 3 9 2 0]
    /// ex: ↘ ¯3 [8 3 9 2 0]
    (2, Drop, DyadicArray, "drop" + '↘'),
    /// Change the shape of an array
    ///
    /// Shapes that have fewer elements than the original array will truncate it.
    /// Shapes that have more elements than the original array will repeat elements.
    ///
    /// ex: ↯ 2_3 [1 2 3 4 5 6]
    /// ex: ↯ 5 2
    /// ex: ↯ 3_7 1_2_3_4
    ///
    /// See also: [deshape]
    (2, Reshape, DyadicArray, "reshape" + '↯'),
    /// Rotate the elements of an array by n
    ///
    /// ex: ↻1 ⇡5
    /// ex: ↻2 ⇡5
    /// ex: ↻¯1 ⇡5
    (2, Rotate, DyadicArray, "rotate" + '↻'),
    /// The n-wise windows of an array
    ///
    /// Multi-dimensional window sizes are supported.
    ///
    /// ex: ◫2 .⇡4
    /// ex: ◫4 .⇡6
    /// ex: ◫ 2_2 .[1_2_3 4_5_6 7_8_9]
    (2, Windows, DyadicArray, "windows" + '◫'),
    /// Use an array to replicate the elements of another array
    ///
    /// ex: ‡ [1 0 2 1 4] [8 3 9 2 0]
    ///
    /// This can be used as a filter.
    /// ex: ‡ ≥'a' ."lOWERCASe onLY"
    (2, Replicate, DyadicArray, "replicate" + '‡'),
    /// Check if each row of an array exists in another array
    ///
    /// The result is always a [rank]`1` array of booleans with the same number of elements as the first array's [length].
    ///
    /// ex: ∊ 2 [1 2 3]
    /// ex: ∊ 5 [1 2 3]
    /// ex: ∊ [1 2 3] [0 3 4 5 1]
    /// ex: ∊ "cat"_"dog" "bird"_"cat"_"fish"
    ///
    /// With the help of [deduplicate] and [replicate], you can use [member] to get a set intersection.
    /// ex: ‡∊,⊝~⊝ "abracadabra" "that's really cool"
    (2, Member, DyadicArray, "member" + '∊'),
    /// Find the occurences of one array in another
    ///
    /// ex: ⌕ 5 [1 8 5 2 3 5 4 5 6 7]
    /// ex: ⌕ "ab" "abracadabra"
    /// ex: ⌕ 1_2 . ↯4_4⇡3
    (2, Find, DyadicArray, "find" + '⌕'),
    /// Find the first index of an element in an array
    ///
    /// ex: ⊗ 5 [1 8 5 2 3 5 4 5 6 7]
    (2, IndexOf, DyadicArray, "indexof" + '⊗'),
    /// Group elements of an array into buckets by index
    ///
    /// ex: ⊕ [0 1 0 2 1 1] [1 2 3 4 5 6]
    /// ex: ⊕ =0◿2. [1 2 3 4 5 6]
    (2, Group, DyadicArray, "group" + '⊕'),
    /// Group elements of an array into buckets by sequential keys
    ///
    /// ex: ⊘ [1 1 2 2 2 3] [1 2 3 4 5 6]
    /// ex: ⊘ ≠' '. "Hey there friendo"
    (2, Partition, DyadicArray, "partition" + '⊘'),
    // Modifiers
    /// Apply a reducing function to an array
    /// For reducing with an initial value, see [fold].
    /// Unlike other modifiers, [reduce] and [fold] traverse the array from right to left.
    ///
    /// ex: /+ 1_2_3_4_5
    /// ex: /- 1_2_3_4_5
    /// ex: /(-~) 1_2_3_4_5
    /// ex: /(×+1) 1_2_3_4_5
    ///
    /// [reduce] traverses the array backwards so that [reduce][noop] unloads all rows onto the stack with the first row on top.
    /// ex: /· 1_2_3
    /// ex: /· [1_2 3_4]
    (Reduce, MonadicModifier { modifier: 1 }, "reduce" + '/'),
    /// Apply a reducing function to an array with an initial value
    /// For reducing without an initial value, see [reduce].
    /// Unlike other modifiers, [fold] and [reduce] traverse the array from right to left.
    ///
    /// ex: ⌿+ 10 1_2_3_4
    ///
    /// [fold] traverses the array backwards so that [join] appends to the front of an accumulator array.
    /// ex: ⌿⊂ [] 1_2_3_4
    (Fold, MonadicModifier { modifier: 1 }, "fold" + '⌿'),
    /// Reduce, but keep intermediate values
    ///
    /// ex: \+ 1_2_3_4
    /// ex: \- 1_2_3_4
    /// ex: \(-~) 1_2_3_4
    /// ex: \⊂ 1_2_3_4
    (Scan, MonadicModifier { modifier: 1 }, "scan" + '\\'),
    /// Apply a function to each element of an array
    /// This is the element-wise version of [rows].
    ///
    /// ex: ∵(⊟.) 1_2_3_4
    /// ex: ∵⇡ 1_2_3_4
    (Each, MonadicModifier { modifier: 1 }, "each" + '∵'),
    /// Pervade a function through two arrays
    /// For operations that are already pervasive, like [add], this is redundant.
    /// This is the element-wise version of [bridge].
    ///
    /// ex: ∺⊂ 1_2_3 4_5_6
    /// ex: ∺⊂ 1_2 [4_5 6_7]
    (Zip, DyadicModifier { modifier: 1 }, "zip" + '∺'),
    /// Apply a function to each row of an array
    /// This is the row-wise version of [each].
    ///
    /// ex: /+ [1_2_3 4_5_6 7_8_9]  # Sum columns
    /// ex: ≡/+ [1_2_3 4_5_6 7_8_9]  # Sum rows
    ///
    /// [rows] is equivalent to [level]`¯1`.
    /// ex: ⍚¯1/+ [1_2_3 4_5_6 7_8_9]
    /// ex: ≡/+   [1_2_3 4_5_6 7_8_9]
    (Rows, MonadicModifier { modifier: 1 }, "rows" + '≡'),
    /// Apply a function to each pair of rows in two arrays
    /// This is the row-wise version of [zip].
    ///
    /// ex: ≑⊂ 1_2 [4_5 6_7]
    /// ex: ≑⌿+ 1_2 [4_5 6_7]
    (Bridge, DyadicModifier { modifier: 1 }, "bridge" + '≑'),
    /// Apply a function to a fixed value and each row of an array
    ///
    /// ex: ∹⊂ 1 2_3_4
    /// ex: ∹⊂ 1_2_3 4_5_6
    (
        Distribute,
        DyadicModifier { modifier: 1 },
        "distribute" + '∹'
    ),
    /// Apply a function to each combination of elements of two arrays
    /// This is the element-wise version of [cross].
    ///
    /// ex: ⊞+ 1_2_3 4_5_6
    /// ex: ⊞⊂ 1_2 3_4
    (Table, DyadicModifier { modifier: 1 }, "table" + '⊞'),
    /// Apply a function to each combination of rows of two arrays
    /// This is the row-wise version of [table].
    ///
    /// ex: ⊠⊂ [1_2 3_4] [5_6 7_8]
    (Cross, DyadicModifier { modifier: 1 }, "cross" + '⊠'),
    /// Repeat a function a number of times
    ///
    /// ex: ⍥(+2) 5 0
    /// ex: ⍥(⊂2) 5 []
    ///
    /// One interesting use of [repeat] is to collect some number of stack values into an array.
    /// ex: ⍥⊂3 [] 1 2 3
    ///
    /// Repeating for [infinity] times will create an infinite loop.
    /// You can use [break] to break out of the loop.
    /// ex: ⍥(⎋>1000. ×2)∞ 1
    (Repeat, OtherModifier { modifier: 1 }, "repeat" + '⍥'),
    /// Invert the behavior of a function
    /// Most functions are not invertible.
    ///
    /// ex: √2
    /// ex: ↶√2
    (Invert, OtherModifier { modifier: 1 }, "invert" + '↶'),
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
    (Under, OtherModifier { modifier: 2 }, "under" + '⍜'),
    /// Apply a function at a different array depth
    ///
    /// [level]`0` does nothing.
    /// [level]`¯1` is equivalent to [rows], applying the function to each row of the array's major axis.
    /// [level]`1` applies the function to each row of the array's last axis.
    ///
    /// ex: ↯2_2_3 ⇡12
    /// ex: /+ ↯2_2_3 ⇡12
    /// ex: ⍚0/+ ↯2_2_3 ⇡12
    /// ex: ⍚¯1/+ ↯2_2_3 ⇡12
    /// ex: ⍚¯2/+ ↯2_2_3 ⇡12
    /// ex: ⍚1/+ ↯2_2_3 ⇡12
    (Level, OtherModifier { modifier: 2 }, "level" + '⍚'),
    /// Call a function and catch errors
    ///
    /// ex: ?(+1 2)"failure"
    /// ex: ?(+'a' 'b')"failure"
    (Try, OtherModifier { modifier: 2 }, "try" + '?'),
    // Misc
    /// Throw an error
    ///
    /// ex: !"Oh no!" "any array"
    /// ex: !"Oh no!" 1
    /// ex: !"Oh no!" 0
    (2, Throw, Control, "throw" + '!'),
    /// Break out of a loop
    /// Expects a non-negative integer. This integer is how many loops will be broken out of.
    /// Loops that can be broken out of are [reduce], [fold], [scan], [each], [rows], and [repeat].
    ///
    /// ex: /(⎋>10.+) ⇌⇡40  # Break when the sum exceeds 10
    /// ex: ⍥(⎋>100.×2)∞ 1  # Break when the product exceeds 100
    (1(0), Break, Control, "break" + '⎋'),
    /// Call the current dfn recursively
    /// Only dfns can be recurred in.
    ///
    /// To check for a base case, you can use [pick].
    /// ex: {:⊡~·_↬ <10.×2} 1
    ///
    /// Here is a recursive factorial function:
    /// ex: {:⊡~(×a ↬-1a)_(1) <2a} 5
    ///
    /// Here is a recursive fibonacci function:
    /// ex: {:⊡~(+ ↬-1a ↬-2a)_(a) <2a} 10
    (1(0), Recur, Control, "recur" + '↬'),
    /// Debug print a value without popping it
    ///
    /// ex: /+ | 1_2_3
    (1, Debug, Io, "debug" + '|'),
    /// Call a function
    ///
    /// ex: :(+) 1 2
    (1(None), Call, Misc, "call" + ':'),
    /// Do nothing
    ///
    /// While this may seem useless, one way to use it is to put all of an array's values on the stack.
    /// ex: /· [1 2 3]
    ///
    /// The formatter converts an empty `()` function into [noop].
    /// ex: ()
    (0, Noop, Misc, "noop" + '·'),
    /// Convert a value to a string
    ///
    /// ex: string 5
    (1, String, Misc, "string"),
    /// Parse a string as a number
    ///
    /// ex: parsenum "17"
    /// ex: parsenum "3.1415926535897932"
    /// ex: parsenum "dog"
    (1, ParseNum, Misc, "parsenum"),
    /// Generate a random number between 0 and 1
    /// If you need a seeded random number, use [gen].
    ///
    /// ex: rand
    /// ex: [rand rand rand]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: [⍥(⌊*10 rand)5]
    (0, Rand, Misc, "rand"),
    /// Generate a random number between 0 and 1 from a seed, as well as the next seed
    /// If you don't care about a seed, you can use [rand].
    ///
    /// The same seed will always produce the same random number.
    /// ex: [;gen gen gen 0]
    /// ex: [;⍥gen3 0]
    /// ex: [;⍥gen3 1]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: [;⍥(~⌊*10~gen)5 0]
    (1(2), Gen, Misc, "gen"),
    /// Extract a named function from a module
    ///
    /// Can be used after [Import].
    ///
    /// ex: import "example.ua"
    ///   : square ← use "square".
    ///   : increment ← use "increment"
    ///   : square increment 5
    (2, Use, Misc, "use"),
    // Constants
    /// The ratio of a circle's circumference to its diameter
    (0(1), Pi, Constant, "pi" + 'π'),
    /// The ratio of a circle's circumference to its radius
    (0(1), Tau, Constant, "tau" + 'τ'),
    /// The biggest number
    (0(1), Infinity, Constant, "infinity" + '∞')
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
            Asin => Sin,
            Acos => Cos,
            Reverse => Reverse,
            Transpose => InvTranspose,
            InvTranspose => Transpose,
            Debug => Debug,
            _ => return None,
        })
    }
    /// Find a primitive by its proper name
    pub fn from_name(name: &str) -> Option<Self> {
        Self::from_format_name(name).or_else(|| Self::all().find(|p| p.name() == Some(name)))
    }
    /// Try to parse a primitive from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        if name.chars().any(char::is_uppercase) {
            return None;
        }
        if name == "pi" || name == "π" {
            return Some(Primitive::Pi);
        }
        if name == "tau" || name == "τ" {
            return Some(Primitive::Tau);
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching =
            Primitive::all().filter(|p| p.format_name().map_or(false, |pn| pn.starts_with(name)));
        let res = matching.next()?;
        let exact_match = res.format_name().map_or(false, |i| i == name);
        (exact_match || matching.next().is_none()).then_some(res)
    }
    /// Try to parse multiple primitives from the concatentation of their name prefixes
    pub fn from_format_name_multi(name: &str) -> Option<Vec<(Self, &str)>> {
        if name == "pi" || name == "π" {
            return Some(vec![(Primitive::Pi, name)]);
        }
        if name == "tau" || name == "τ" {
            return Some(vec![(Primitive::Tau, name)]);
        }
        let mut start = 0;
        let indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
        if indices.len() < 3 {
            return None;
        }
        let mut prims = Vec::new();
        'outer: loop {
            if start == name.len() {
                break Some(prims);
            }
            for len in (3..=name.len() - start).rev() {
                let start_index = indices[start];
                let end_index = indices[start + len - 1];
                if let Some(p) = Primitive::from_format_name(&name[start_index..=end_index]) {
                    prims.push((p, &name[start_index..=end_index]));
                    start += len;
                    continue 'outer;
                }
            }
            break None;
        }
    }
    /// The longest name of the primitive that the formatter will replace
    pub fn format_name(&self) -> Option<&'static str> {
        if [
            Primitive::Sin,
            Primitive::Cos,
            Primitive::Tan,
            Primitive::Atan,
        ]
        .contains(self)
        {
            return self.name();
        }
        if self.ascii().is_some() || self.unicode().is_none() {
            return None;
        }
        self.name()
    }
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
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
            Primitive::Tan => env.monadic_env(Value::tan)?,
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
            Primitive::NotMatch => env.dyadic_ref(|a, b| a != b)?,
            Primitive::Join => env.dyadic_env(Value::join)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::InvTranspose => env.monadic_mut(Value::inv_transpose)?,
            Primitive::Pick => env.dyadic_env(Value::pick)?,
            Primitive::Replicate => env.dyadic_ref_own_env(Value::replicate)?,
            Primitive::Take => env.dyadic_env(Value::take)?,
            Primitive::Drop => env.dyadic_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_ref_own_env(Value::rotate)?,
            Primitive::Couple => env.dyadic_env(Value::couple)?,
            Primitive::Fill => {
                let fill = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.fill(fill, env)?;
                env.push(array);
            }
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
            Primitive::ParseNum => env.monadic_env(|v, env| v.parse_num(env))?,
            Primitive::Range => env.monadic_ref_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_env(Value::first)?,
            Primitive::Last => env.monadic_env(Value::last)?,
            Primitive::Len => env.monadic_ref(Value::len)?,
            Primitive::Rank => env.monadic_ref(Value::rank)?,
            Primitive::Truncate => env.monadic_mut(Value::truncate)?,
            Primitive::Fold => loops::fold(env)?,
            Primitive::Reduce => loops::reduce(env)?,
            Primitive::Each => loops::each(env)?,
            Primitive::Zip => loops::zip(env)?,
            Primitive::Rows => loops::rows(env)?,
            Primitive::Bridge => loops::bridge(env)?,
            Primitive::Distribute => loops::distribute(env)?,
            Primitive::Table => loops::table(env)?,
            Primitive::Cross => loops::cross(env)?,
            Primitive::Scan => loops::scan(env)?,
            Primitive::Repeat => loops::repeat(env)?,
            Primitive::Level => loops::level(env)?,
            Primitive::Reshape => {
                let shape = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.reshape(&shape, env)?;
                env.push(array);
            }
            Primitive::Break => {
                let n = env.pop(1)?.as_nat(env, "break expects a natural number")?;
                if n > 0 {
                    return Err(UiuaError::Break(n - 1, env.span().clone()));
                }
            }
            Primitive::Recur => env.recur()?,
            Primitive::Debug => {
                let value = env.pop(1)?;
                env.io.print_str(&value.show()).map_err(|e| env.error(e))?;
                env.io.print_str("\n").map_err(|e| env.error(e))?;
                env.push(value);
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
            Primitive::Try => {
                let f = env.pop(1)?;
                let handler = env.pop(2)?;
                let size = env.stack_size();
                env.push(f);
                if let Err(e) = env.call() {
                    env.truncate_stack(size);
                    env.push(e.message());
                    env.push(handler);
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
                env.push(f);
                env.call()?;
                env.push(g);
                env.call()?;
                env.push(inv_f);
                env.call()?;
            }
            Primitive::Throw => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if cond.as_nat(env, "").map_or(true, |n| n == 0) {
                    return Err(UiuaError::Throw(msg.into(), env.span().clone()));
                }
            }
            Primitive::Shape => {
                env.monadic_ref(|v| v.shape().iter().copied().collect::<Value>())?
            }
            Primitive::String => env.monadic_ref(|v| v.to_string())?,
            Primitive::Rand => {
                thread_local! {
                    static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::seed_from_u64(instant::now().to_bits()));
                }
                env.push(RNG.with(|rng| rng.borrow_mut().gen::<f64>()));
            }
            Primitive::Gen => {
                let seed = env.pop(1)?;
                let mut rng =
                    SmallRng::seed_from_u64(seed.as_num(env, "gen expects a number")?.to_bits());
                let val: f64 = rng.gen();
                let next_seed = f64::from_bits(rng.gen::<u64>());
                env.push(val);
                env.push(next_seed);
            }
            Primitive::Use => {
                let name = env.pop(1)?.as_string(env, "Use name must be a string")?;
                let lib = env.pop(2)?;
                let lowername = name.to_lowercase();
                let f = match lib {
                    Value::Func(fs) => fs.data.iter().find_map(|f| {
                        matches!(&f.id, FunctionId::Named(n) if n == lowername.as_str())
                            .then(|| f.clone())
                    }),
                    _ => None,
                }
                .ok_or_else(|| env.error(format!("No function found for {name:?}")))?;
                env.push(f);
            }
            Primitive::Io(io) => io.run(env)?,
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct PrimDoc {
    pub short: String,
    pub examples: Vec<PrimExample>,
    pub outro: String,
}

impl PrimDoc {
    pub fn from_lines(s: &str) -> Self {
        let mut short = String::new();
        let mut examples = Vec::new();
        let mut primer = String::new();
        for line in s.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            if let Some(ex) = line.strip_prefix("ex:") {
                let input = ex.trim().to_owned();
                examples.push(PrimExample {
                    primer: take(&mut primer),
                    input,
                    output: OnceLock::new(),
                });
            } else if let Some(ex) = line.strip_prefix(':') {
                if let Some(example) = examples.last_mut() {
                    example.input.push('\n');
                    example.input.push_str(ex.trim());
                }
            } else if short.is_empty() {
                short = line.into();
            } else {
                primer.push_str(line);
                primer.push('\n');
            }
        }
        let outro = take(&mut primer);
        Self {
            short,
            examples,
            outro,
        }
    }
}

impl fmt::Display for PrimDoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.short.trim())?;
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

#[test]
fn primitive_from_name() {
    assert_eq!(Primitive::from_format_name("rev"), Some(Primitive::Reverse));
    assert_eq!(Primitive::from_format_name("re"), None);
    assert_eq!(
        Primitive::from_format_name("resh"),
        Some(Primitive::Reshape)
    );
}

#[cfg(test)]
#[test]
fn glyph_size() {
    use std::{fs::File, io::Write};
    let mut file = File::create("glyph_test.txt").unwrap();
    writeln!(file, "A |").unwrap();
    writeln!(file, "a |").unwrap();
    for p in Primitive::all() {
        if let Some(glyph) = p.unicode() {
            writeln!(file, "{} |", glyph).unwrap();
        }
    }
}

#[cfg(test)]
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
        &*Primitive::from_format_name_multi("tabrepl").unwrap(),
        [(Primitive::Table, _), (Primitive::Replicate, _)]
    ));
    assert_eq!(Primitive::from_format_name_multi("foo"), None);
}

#[cfg(test)]
#[test]
fn word_collisions() {
    let mut collisions = 0;
    for word in std::fs::read_to_string("src/words.txt").unwrap().lines() {
        if let Some(prims) = Primitive::from_format_name_multi(word) {
            println!("{word:>10}: {prims:?}");
            collisions += 1;
        }
    }
    println!("{collisions} collisions")
}
