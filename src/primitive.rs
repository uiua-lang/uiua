use std::{
    borrow::Cow,
    cell::RefCell,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    fmt,
    sync::OnceLock,
};

use enum_iterator::{all, Sequence};
use rand::prelude::*;
use regex::Regex;

use crate::{
    algorithm::loops, function::FunctionId, lex::Simple, sys::*, value::*, Uiua, UiuaError,
    UiuaResult,
};

macro_rules! primitive {
    ($(
        $(#[doc = $doc:literal])*
        (
            $(
                $($args:literal)?
                $(($outputs:expr))?
                $([$mod_func_args:expr, $mod_array_args:expr $(,$mod_inner_args:expr)?])?
            ,)?
            $variant:ident, $class:ident
            $(,$names:expr)?
        )
    ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
        pub enum Primitive {
            $($variant,)*
            Sys(SysOp)
        }

        impl Primitive {
            pub fn all() -> impl Iterator<Item = Self> {
                all()
            }
            #[allow(path_statements)]
            pub fn names(&self) -> Option<PrimNames> {
                match self {
                    $(Primitive::$variant => { None::<PrimNames> $(; Some($names.into()))* },)*
                    Primitive::Sys(op) => Some(op.name().into())
                }
            }
            pub fn class(&self) -> PrimClass {
                match self {
                    $(Primitive::$variant => PrimClass::$class,)*
                    Primitive::Sys(_) => PrimClass::Sys,
                }
            }
            pub fn modifier_args(&self) -> Option<(u8, u8)> {
                match self {
                    $($($(Primitive::$variant => Some(($mod_func_args, $mod_array_args)),)?)?)*
                    _ => None
                }
            }
            pub fn modifier_inner_args(&self) -> Option<u8> {
                match self {
                    $($($($(Primitive::$variant => Some($mod_inner_args),)?)?)?)*
                    _ => None
                }
            }
            pub fn args(&self) -> Option<u8> {
                match self {
                    $($($(Primitive::$variant => Some($args),)?)?)*
                    Primitive::Sys(op) => Some(op.args()),
                    _ => None
                }
            }
            pub fn outputs(&self) -> Option<u8> {
                match self {
                    $($($(Primitive::$variant => $outputs.into(),)?)?)*
                    Primitive::Sys(op) => op.outputs(),
                    _ => Some(1)
                }
            }
            pub fn doc(&self) -> Option<&'static PrimDoc> {
                match self {
                    $(Primitive::$variant => {
                        let doc_str = concat!($($doc, "\n"),*);
                        static DOC: OnceLock<PrimDoc> = OnceLock::new();
                        if doc_str.is_empty() {
                            return None;
                        }
                        Some(DOC.get_or_init(|| PrimDoc::from_lines(doc_str)))
                    },)*
                    Primitive::Sys(op) => op.doc(),
                }
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
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
    pub ascii: Option<Simple>,
    pub unicode: Option<char>,
}

impl PrimNames {
    pub fn is_formattable(&self) -> bool {
        self.unicode.is_some()
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
impl From<(&'static str, Simple, char)> for PrimNames {
    fn from((text, ascii, unicode): (&'static str, Simple, char)) -> Self {
        Self {
            text,
            ascii: Some(ascii),
            unicode: Some(unicode),
        }
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
    (1(2), Dup, Stack, ("duplicate", '.')),
    /// Duplicate the second-to-top value to the top of the stack
    ///
    /// ex: [, 1 2 3]
    (2(3), Over, Stack, ("over", ',')),
    /// Swap the top two values on the stack
    ///
    /// ex: [~ 1 2 3 4]
    ///
    /// When combined with [duplicate], this can be used to make a monadic right-hook or monadic fork, such as an average calculator:
    /// ex: ÷⧻~/+. 1_8_2_5
    (2(2), Flip, Stack, ("flip", '~')),
    /// Pop the top value off the stack
    (1(0), Pop, Stack, ("pop", ';')),
    /// Do nothing
    ///
    /// While this may seem useless, one way to use it is to pass it to [reduce], which will put all of an array's values on the stack.
    /// ex: /· [1 2 3]
    ///
    /// If you use this on an array that has fill values, the fill values will not be removed.
    /// To remove them, use [truncate] instead.
    /// ex: /· \⊂1_2_3_4
    ///
    /// The formatter converts an empty `()` function into `noop`.
    /// ex: () # Try running to format
    (0, Noop, Stack, ("noop", '·')),
    // Pervasive monadic ops
    /// Logical not
    ///
    /// ex: ¬0
    /// ex: ¬1
    /// ex: ¬[0 1 1 0]
    ///
    /// This is equivalent to `subtract``flip``1`
    /// ex: ¬7
    /// ex: ¬[1 2 3 4]
    (1, Not, MonadicPervasive, ("not", '¬')),
    /// Numerical sign (1, ¯1, or 0)
    ///
    /// ex: ± 1
    /// ex: ± ¯5
    /// ex: ± [¯2 ¯1 0 1 2]
    (1, Sign, MonadicPervasive, ("sign", '±')),
    /// Negate a number
    ///
    /// ex: ¯ 1
    /// ex: ¯ ¯3
    (1, Neg, MonadicPervasive, ("negate", Simple::Backtick, '¯')),
    /// The absolute value of a number
    ///
    /// ex: ⌵ ¯1
    /// ex: ⌵ 1
    ///
    /// The symbol looks like the graph of `|x|`.
    (1, Abs, MonadicPervasive, ("absolute value", '⌵')),
    /// The square root of a number
    (1, Sqrt, MonadicPervasive, ("sqrt", '√')),
    /// The sine of a number
    ///
    /// ex: ○ 1
    ///
    /// You can get a cosine function by [add]ing [eta].
    /// ex: ○+η 1
    ///
    /// You can get an arcsine function with [invert].
    /// ex: ↶○ 1
    ///
    /// You can get an arccosine function by [invert]ing the cosine.
    /// ex: ↶(○+η) 1
    ///
    /// You can get a tangent function by [divide]ing the [sine] by the cosine.
    /// ex: ÷○+η~○. 0
    (1, Sin, MonadicPervasive, ("sine", '○')),
    (1, Cos, MonadicPervasive),
    (1, Asin, MonadicPervasive),
    (1, Acos, MonadicPervasive),
    /// Round to the nearest integer towards `¯∞`
    (1, Floor, MonadicPervasive, ("floor", '⌊')),
    /// Round to the nearest integer towards `∞`
    (1, Ceil, MonadicPervasive, ("ceiling", '⌈')),
    /// Round to the nearest integer
    (1, Round, MonadicPervasive, ("round", '⁅')),
    // Pervasive dyadic ops
    (2, Eq, DyadicPervasive, ("equals", Simple::Equal, '=')),
    (
        2,
        Ne,
        DyadicPervasive,
        ("not equals", Simple::BangEqual, '≠')
    ),
    (2, Lt, DyadicPervasive, ("less than", '<')),
    (
        2,
        Le,
        DyadicPervasive,
        ("less or equal", Simple::LessEqual, '≤')
    ),
    (2, Gt, DyadicPervasive, ("greater than", '>')),
    (
        2,
        Ge,
        DyadicPervasive,
        ("greater or equal", Simple::GreaterEqual, '≥')
    ),
    (2, Add, DyadicPervasive, ("add", '+')),
    (2, Sub, DyadicPervasive, ("subtract", '-')),
    (2, Mul, DyadicPervasive, ("multiply", Simple::Star, '×')),
    (2, Div, DyadicPervasive, ("divide", Simple::Percent, '÷')),
    (2, Mod, DyadicPervasive, ("modulus", '◿')),
    (2, Pow, DyadicPervasive, ("power", 'ⁿ')),
    (2, Log, DyadicPervasive, ("logarithm", 'ₙ')),
    /// Take the minimum of two arrays
    ///
    /// ex: ↧ 3 5
    /// ex: ↧ [1 4 2] [3 7 1]
    (2, Min, DyadicPervasive, ("minimum", '↧')),
    /// Take the maximum of two arrays
    ///
    /// ex: ↥ 3 5
    /// ex: ↥ [1 4 2] [3 7 1]
    (2, Max, DyadicPervasive, ("maximum", '↥')),
    /// The arctangent of two numbers
    ///
    /// This takes a `y` and `x` argument and returns the angle in radians in the range `(-π, π]`.
    ///
    /// ex: ∠ 1 0
    /// ex: ∠ ¯1 0
    /// ex: ∠ √2 √2
    (2, Atan, DyadicPervasive, ("atangent", '∠')),
    // Monadic array ops
    /// The number of rows in an array
    ///
    /// ex: ⧻5
    /// ex: ⧻[]
    /// ex: ⧻1_2_3
    /// ex: ⧻[1_2 3_4 5_6]
    (1, Len, MonadicArray, ("length", '⧻')),
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
    (1, Rank, MonadicArray, ("rank", '∴')),
    /// The dimensions of an array
    ///
    /// ex: △5
    /// ex: △[]
    /// ex: △1_2_3
    /// ex: △[1_2 3_4 5_6]
    ///
    /// It is a triangle because a triangle is a shape.
    (1, Shape, MonadicArray, ("shape", '△')),
    /// Make an array of [0, x)
    ///
    /// The [rank] of the input must be `0` or `1`.
    /// ex: ⇡5
    /// ex: ⇡2_3
    ///
    /// When creating ranges with upper bounds that a [rank]`1` (lists), [pick]ing the generated range array from an array with the [shape] of the input will yield that array.
    /// ex:     [1_2_3 4_5_6]
    ///   :    △[1_2_3 4_5_6]
    ///   :   ⇡△[1_2_3 4_5_6]
    ///   : ⊡⇡△.[1_2_3 4_5_6]
    (1, Range, MonadicArray, ("range", '⇡')),
    /// The first row of an array
    ///
    /// ex: ⊢1_2_3
    /// ex: ⊢[1_2 3_4 5_6]
    /// ex: ⊢[]
    /// ex: ⊢1
    (1, First, MonadicArray, ("first", '⊢')),
    /// The last element of an array
    (1, Last, MonadicArray),
    /// Remove fill elements from the end of an array
    ///
    /// Using [noop] to unpack an array onto the stack preserves the fill values.
    /// Use [truncate] instead to remove them.
    /// ex: /· \⊂1_2_3_4
    /// ex: /⌀ \⊂1_2_3_4
    (1, Truncate, MonadicArray, ("truncate", '⌀')),
    /// Reverse the rows of an array
    ///
    /// ex: ⇌1_2_3_9
    /// ex: ⇌[1_2 3_4 5_6]
    (1, Reverse, MonadicArray, ("reverse", '⇌')),
    /// Make an array 1-dimensional
    ///
    /// ex: ♭5
    /// ex: ♭[1_2 3_4 5_6]
    ///
    /// [deshape] *only* changes the [shape] of an array. It does not remove fill elements.
    /// If you want to flatten an array and remove fill elements, use [reduce][join].
    /// ex: ♭  \⊂⇡4
    ///   : /⊂ \⊂⇡4
    ///
    /// It looks like `♭` because it flattens the array.
    ///
    /// See also: [reshape]
    (1, Deshape, MonadicArray, ("deshape", '♭')),
    /// Encode an array as bits (big-endian)
    ///
    /// The result will always be 1 [rank] higher than the input.
    /// ex: ⋯27
    /// ex: ⋯⇡8
    /// ex: ⋯[1_2 3_4 5_6]
    ///
    /// One use of [bits] is to get all subsets of an array.
    /// You get the [length] of the array, raise `2` to the [power] of it, encode the [range] into [bits], and then [plow][replicate] the array with the bits.
    /// ex: ⫫‡⋯⇡ⁿ~2⧻.[1 2 3]
    ///
    /// To turn the bits back into numbers, use [invert].
    /// ex: ↶⋯[1 0 1 0]
    /// ex: ↶⋯[1_1_1 0_1_0 0_1_1]
    ///
    /// Bits are encoded in big-endian so that any fill elements do not affect the result.
    /// ex: ↶⋯.\⊂◿2⇡6
    (1, Bits, MonadicArray, ("bits", '⋯')),
    (1, InverseBits, MonadicArray),
    /// Rotate the shape of an array
    ///
    /// ex: ⍉.[1_2 3_4 5_6]
    /// ex: ⍉.[[1_2 3_4] [5_6 7_8]]
    ///
    /// `shape``transpose` is always equivalent to `rotate``1``shape`.
    /// ex: ≅ △⍉ ~ ↻1△ .[1_2 3_4 5_6]
    (1, Transpose, MonadicArray, ("transpose", '⍉')),
    (1, InvTranspose, MonadicArray),
    /// Sort the rows of an array
    ///
    /// ex: ∧6_2_7_0_¯1_5
    ///
    /// See also: [grade]
    (1, Sort, MonadicArray, ("sort", '∧')),
    /// Grade the rows of an array
    ///
    /// ex: ⍋6_2_7_0_¯1_5
    ///
    /// Using the grading as a selector in [select] yields the sorted array.
    /// ex: ⊏⍋.6_2_7_0_¯1_5
    ///
    /// See also: [sort]
    (1, Grade, MonadicArray, ("grade", '⍋')),
    /// Repeat the index of each array element the element's value times
    ///
    /// ex: ⊙2_0_4_1
    (1, Indices, MonadicArray, ("indices", '⊙')),
    /// Assign a unique index to each unique element in an array
    ///
    /// ex: ⊛7_7_8_0_1_2_0
    (1, Classify, MonadicArray, ("classify", '⊛')),
    /// Remove duplicate elements from an array
    ///
    /// ex: ⊝7_7_8_0_1_2_0
    (1, Deduplicate, MonadicArray, ("deduplicate", '⊝')),
    // Dyadic array ops
    /// Check if two arrays are the same, ignoring fill elements
    ///
    /// ex: ≅ 1_2_3 [1 2 3]
    /// ex: ≅ 1_2_3 [1 2]
    /// ex: ≅ 1_2 .⊢[1_2 4_5_6]
    (2, Match, DyadicArray, ("match", '≅')),
    /// Append two arrays end-to-end
    ///
    /// For scalars, it is equivalent to [couple].
    /// ex: ⊂ 1 2
    ///   : ⊟ 1 2
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
    (2, Join, DyadicArray, ("join", '⊂')),
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
    /// `first``shape` of the coupled array will *always* be `2`.
    (2, Couple, DyadicArray, ("couple", '⊟')),
    /// Replace the fill elements of an array with a elements from another.
    ///
    /// The most basic case is filling with a scalar
    /// ex: ⍛7 .↙10⇡5
    /// ex: ⍛7 .↙5↯3_3⇡9
    ///
    /// The [shape] of the array being filled must end with the [shape] of the fill array.
    /// ex: ⍛1_2_3 .↙5↯3_3⇡9
    /// ex: ⍛1_2_3_4 .↙5↯3_3⇡9
    (2, Fill, DyadicArray, ("fill", '⍛')),
    /// Index a row or elements from an array
    ///
    /// An index with [rank] `0` or `1` will pick a single row or element from an array.
    /// ex: ⊡ 2 [8 3 9 2 0]
    /// ex: ⊡ 1_1 .[1_2_3 4_5_6]
    ///
    /// If the index's [rank] is `2` or greater, then multiple rows or elements will be picked.
    /// ex: ⊡ [1_2 0_1] [1_2_3 4_5_6]
    ///
    /// For index [rank] `2` or greater, it should hold that `pick``range``shape``duplicate``x` is equivalent to `x`.
    /// ex: ⊡⇡△. [1_2_3 4_5_6]
    (2, Pick, DyadicArray, ("pick", '⊡')),
    /// Select multiple rows from an array
    ///
    /// ex: ⊏ 4_2 [8 3 9 2 0]
    /// ex: ⊏ 0_2_1_1 [1_2_3 4_5_6 7_8_9]
    (2, Select, DyadicArray, ("select", '⊏')),
    /// Take the first n elements of an array
    /// This is the opposite of [drop].
    ///
    /// ex: ↙ 3 [8 3 9 2 0]
    /// ex: ↙ ¯3 [8 3 9 2 0]
    /// ex: ↙ 2 ↯3_3⇡9
    /// ex: ↙ ¯2 ↯3_3⇡9
    ///
    /// Taking more than the length of the array will extend the array with fill elements.
    /// ex: ↙ 7 [8 3 9 2 0]
    /// ex: ↙ ¯7 [8 3 9 2 0]
    /// ex: ↙ 5 ↯3_3⇡9
    /// ex: ↙ ¯5 ↯3_3⇡9
    ///
    /// To extend with a specific value, use [fill];
    /// ex: ⍛0 ↙10 [1 2 3 4 5]
    (2, Take, DyadicArray, ("take", '↙')),
    /// Drop the first n elements of an array
    /// This is the opposite of [take].
    ///
    /// ex: ↘ 3 [8 3 9 2 0]
    /// ex: ↘ ¯3 [8 3 9 2 0]
    /// ex: ↘ 2 ↯3_3⇡9
    /// ex: ↘ ¯2 ↯3_3⇡9
    ///
    /// Dropping more than the length of the array will leave an empty array.
    /// ex: ↘ 7 [8 3 9 2 0]
    /// ex: ↘ ¯7 [8 3 9 2 0]
    /// ex: ↘ 5 ↯3_3⇡9
    /// ex: ↘ ¯5 ↯3_3⇡9
    (2, Drop, DyadicArray, ("drop", '↘')),
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
    (2, Reshape, DyadicArray, ("reshape", '↯')),
    /// Rotate the elements of an array by n
    ///
    /// ex: ↻1 ⇡5
    /// ex: ↻2 ⇡5
    /// ex: ↻¯1 ⇡5
    (2, Rotate, DyadicArray, ("rotate", '↻')),
    /// The n-wise windows of an array
    ///
    /// Multi-dimensional window sizes are supported.
    ///
    /// ex: ◫2 .⇡4
    /// ex: ◫4 .⇡6
    /// ex: ◫2_2 .[1_2_3 4_5_6 7_8_9]
    (2, Windows, DyadicArray, ("windows", '◫')),
    /// Use an array to replicate the elements of another array
    ///
    /// ex: ‡ [1 0 2 1 4] [8 3 9 2 0]
    ///
    /// This can be used as a filter.
    /// ex: ‡ ≥'a' ."lOWERCASe onLY"
    (2, Replicate, DyadicArray, ("replicate", '‡')),
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
    (2, Member, DyadicArray, ("member", '∊')),
    /// Find the occurences of one array in another
    ///
    /// ex: ⌕ 5 [1 8 5 2 3 5 4 5 6 7]
    /// ex: ⌕ "ab" "abracadabra"
    /// ex: ⌕ 1_2 . ↯4_4⇡3
    (2, Find, DyadicArray, ("find", '⌕')),
    /// Find the first index of an element in an array
    ///
    /// ex: ⊗ 5 [1 8 5 2 3 5 4 5 6 7]
    (2, IndexOf, DyadicArray, ("indexof", '⊗')),
    /// Group elements of an array into buckets by index
    ///
    /// ex: ⊕ [0 1 0 2 1 1] [1 2 3 4 5 6]
    /// ex: ⊕ =0◿2. [1 2 3 4 5 6]
    (2, Group, DyadicArray, ("group", '⊕')),
    /// Group elements of an array into buckets by sequential keys
    ///
    /// The first array must be rank 1, and the arrays must have the same length.
    /// Buckets with mismatched lengths have fill elements.
    /// ex: ⊘ [1 1 2 2 2 3] [1 2 3 4 5 6]
    ///
    /// This can be used to split an array by a delimiter.
    /// ex: ⊘ ≠' '. $ Hey there friendo
    /// For non-scalar delimiters, you may have to get a little more creative.
    /// ex: ⊘/↧⊞≠~, " | " $ Um | I | um | arrays
    (2, Partition, DyadicArray, ("partition", '⊘')),
    // Modifiers
    /// Apply a reducing function to an array
    /// For reducing with an initial value, see [fold].
    /// Unlike other modifiers, [reduce] and [fold] traverse the array from right to left.
    ///
    /// ex: /+ 1_2_3_4_5
    /// ex: /- 1_2_3_4_5
    ///
    /// If you want to see the intermediate values, you can use [scan], [flip], and [reverse].
    /// ex: /-     1_2_3_4_5
    ///   : \(-~)⇌ 1_2_3_4_5
    ///
    /// You can can fold with arbitrary functions.
    /// ex: /(×+1) 1_2_3_4_5
    ///
    /// [reduce] traverses the array backwards so that `reduce``noop` unloads all rows onto the stack with the first row on top.
    /// ex: /· 1_2_3
    /// ex: /· [1_2 3_4]
    ([1, 1, 2], Reduce, MonadicModifier, ("reduce", '/')),
    /// Apply a reducing function to an array with an initial value
    /// For reducing without an initial value, see [reduce].
    /// Unlike other modifiers, [fold] and [reduce] traverse the array from right to left.
    ///
    /// ex: ⌿+ 10 1_2_3_4
    ([1, 2, 2], Fold, MonadicModifier, ("fold", '⌿')),
    /// Reduce, but keep intermediate values
    ///
    /// ex: \+    1_2_3_4
    /// ex: \-    1_2_3_4
    /// ex: \(-~) 1_2_3_4
    /// ex: \⊂    1_2_3_4
    ([1, 1, 2], Scan, MonadicModifier, ("scan", '\\')),
    /// Apply a function to each element of an array
    /// This is the element-wise version of [rows].
    ///
    /// ex: ∵(⊟.) 1_2_3_4
    /// ex: ∵⇡     1_2_3_4
    ([1, 1, 1], Each, MonadicModifier, ("each", '∵')),
    /// Apply a function to each row of an array
    /// This is the row-wise version of [each].
    ///
    /// ex:  /+ [1_2_3 4_5_6 7_8_9]  # Sum columns
    /// ex: ≡/+ [1_2_3 4_5_6 7_8_9]  # Sum rows
    ///
    /// [rows] is equivalent to [level]`¯1`.
    /// ex: ⍚¯1/+ [1_2_3 4_5_6 7_8_9]
    /// ex: ≡/+   [1_2_3 4_5_6 7_8_9]
    ([1, 1, 1], Rows, MonadicModifier, ("rows", '≡')),
    /// Pervade a function through two arrays
    /// This is the element-wise version of [bridge].
    ///
    /// ex: ≕⊂ 1_2_3 4_5_6
    /// ex: ≕⊂ 1_2 [4_5 6_7]
    ///
    /// For operations that are already pervasive, like `add` or `maximum`, `zip` is redundant.
    /// ex:  + 1_2_3 [4_5 6_7 8_9]
    ///   : ≕+ 1_2_3 [4_5 6_7 8_9]
    ([1, 2, 2], Zip, DyadicModifier, ("zip", '≕')),
    /// Apply a function to each pair of rows in two arrays
    /// This is the row-wise version of [zip].
    ///
    /// ex: ≍⊂  1_2 [4_5 6_7]
    /// ex: ≍⌿+ 1_2 [4_5 6_7]
    ([1, 2, 2], Bridge, DyadicModifier, ("bridge", '≍')),
    /// Apply a function to each element of an array and a fixed value
    /// This is the element-wise version of [plow].
    ///
    /// ex: ≐⊂ 1_2_3 4
    /// ex: ≐⊂ 1_2_3 4_5_6
    ///
    /// One nice use of this is to [call] multiple functions on a single argument.
    /// ex: ≐:√_¯_⌊_⌈_(×4) 6.25
    ([1, 2, 2], Distribute, DyadicModifier, ("distribute", '≐')),
    /// Apply a function to each row of an array and a fixed value
    /// This is the row-wise version of [distribute].
    ///
    /// ex: ⫫⊂ 1_2_3 4
    /// ex: ⫫⊂ 1_2_3 4_5_6
    ///
    /// One nice use of this is to [call] multiple functions on a single argument.
    /// ex: ⫫:√_¯_⌊_⌈_(×4) 6.25
    ([1, 2, 2], Plow, DyadicModifier, ("plow", '⫫')),
    /// Apply a function to each combination of elements of two arrays
    /// This is the element-wise version of [cross].
    ///
    /// ex: ⊞+ 1_2_3 4_5_6
    /// ex: ⊞⊂ 1_2 3_4
    ([1, 2, 2], Table, DyadicModifier, ("table", '⊞')),
    /// Apply a function to each combination of rows of two arrays
    /// This is the row-wise version of [table].
    ///
    /// ex: ⊠⊂ [1_2 3_4] [5_6 7_8]
    ([1, 2, 2], Cross, DyadicModifier, ("cross", '⊠')),
    /// Repeat a function a number of times
    ///
    /// ex: ⍥(+2) 5 0
    /// ex: ⍥(⊂2) 5 []
    ///
    /// One interesting use of `repeat` is to collect some number of stack values into an array.
    /// ex: ⍥⊂3 [] 1 2 3
    ///
    /// Repeating for [infinity] times will create an infinite loop.
    /// You can use [break] to break out of the loop.
    /// ex: ⍥(⎋>1000. ×2)∞ 1
    ([1, 1], Repeat, OtherModifier, ("repeat", '⍥')),
    /// Invert the behavior of a function
    /// Most functions are not invertible.
    ///
    /// ex: √2
    /// ex: ↶√2
    ([1, 1], Invert, OtherModifier, ("invert", '↶')),
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
    /// One interesting use of `under` is to round a number to a certain number of decimal places.
    /// ex: ⍜(×ⁿ4 10)⁅ π
    ///
    /// [under][transpose] is sometimes useful.
    /// ex: ⍜⍉(↙2).↯3_4⇡12
    ([2, 1, 1], Under, OtherModifier, ("under", '⍜')),
    /// Apply a function at a different array depth
    ///
    /// `level``0` does nothing.
    /// `level``¯1` is equivalent to [rows], applying the function to each row of the array's major axis.
    /// `level``1` applies the function to each row of the array's last axis.
    ///
    /// ex: ↯2_2_3 ⇡12
    /// ex: /+ ↯2_2_3 ⇡12
    /// ex: ⍚0/+ ↯2_2_3 ⇡12
    /// ex: ⍚¯1/+ ↯2_2_3 ⇡12
    /// ex: ⍚¯2/+ ↯2_2_3 ⇡12
    /// ex: ⍚1/+ ↯2_2_3 ⇡12
    ([2, 1, 1], Level, OtherModifier, ("level", '⍚')),
    /// Call a function and catch errors
    ///
    /// ex: ?(+1 2)"failure"
    /// ex: ?(+'a' 'b')"failure"
    ([2, 0], Try, OtherModifier, ("try", '?')),
    /// Call a function
    ///
    /// When passing a scalar function array, the function is simply called.
    /// ex: :(+5) 2
    ///
    /// Non-function scalars can be called. They pop nothing and push themselves.
    /// ex: :5
    /// This is not useful on its own, but we'll see below how it can be used.
    ///
    /// The behavior when passing a non-scalar array is different.
    /// An additional argument is expected, which is the index of the function to call.
    /// With this, you can do if-else expressions.
    /// ex: Abs ← :·_¯ <0.
    ///   : Abs 5
    ///   : Abs ¯2
    /// This is equivalent to [call][pick][flip]:
    /// ex: Abs ← :⊡~·_¯ <0.
    ///
    /// Using [call] in this way is *not* recursive. If the selected value is also a function array, it will not be called unless you used [call] again, wich will pop another index.
    /// ex:  :[+_- ×_÷] 1   3 12
    /// ex:  :[+_- ×_÷] 1_1 3 12
    /// ex:  :[+_- ×_÷] 1 1 3 12
    /// ex: ::[+_- ×_÷] 1 1 3 12
    (1(None), Call, Control, ("call", ':')),
    // Misc
    /// Throw an error
    ///
    /// Expects a message and a test value.
    /// If the test value is anything but `1`, then the message will be thrown as an error.
    ///
    /// ex: !"Oh no!" "any array"
    /// ex: !"Oh no!" 1
    /// ex: !"Oh no!" 0
    ///
    /// Use [duplicate] if you do not care about the message.
    /// ex: !. =6 6
    /// ex: !. =8 9
    (2, Throw, Control, ("throw", '!')),
    /// Break out of a loop
    /// Expects a non-negative integer. This integer is how many loops will be broken out of.
    /// Loops that can be broken out of are [reduce], [fold], [scan], [each], [rows], and [repeat].
    ///
    /// ex: /(⎋>10.+) ⇌⇡40  # Break when the sum exceeds 10
    /// ex: ⍥(⎋>100.×2)∞ 1  # Break when the product exceeds 100
    (1(0), Break, Control, ("break", '⎋')),
    /// Call the current dfn recursively
    /// Only dfns can be recurred in.
    ///
    /// To check for a base case, you can use [call].
    /// ex: {:·_↬ <10.×2} 1
    ///
    /// Here is a recursive factorial function:
    /// ex: {:(×a ↬-1a)_1 <2a} 5
    ///
    /// Here is a recursive fibonacci function:
    /// ex: {:(+ ↬-1a ↬-2a)_a <2a} 10
    (1(0), Recur, Control, ("recur", '↬')),
    /// Debug print a value without popping it
    ///
    /// ex: /+ | 1_2_3
    (1, Debug, Sys, ("debug", '|')),
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
    /// The number of radians in a quarter circle
    ///
    /// Equivalent to `divide``2``pi` or `divide``4``tau`
    /// ex: [η ÷2π ÷4τ]
    (0(1), Eta, Constant, ("eta", 'η')),
    /// The ratio of a circle's circumference to its diameter
    ///
    /// Equivalent to `multiply``2``eta` or `divide``2``tau`
    /// ex: [×2η π ÷2τ]
    (0(1), Pi, Constant, ("pi", 'π')),
    /// The ratio of a circle's circumference to its radius
    ///
    /// Equivalent to `multiply``4``eta` or `multiply``2``pi`
    /// ex: [×4η ×2π τ]
    (0(1), Tau, Constant, ("tau", 'τ')),
    /// The biggest number
    (0(1), Infinity, Constant, ("infinity", '∞')),
    (0(None), FillValue, Stack),
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
        } else if let Primitive::FillValue = self {
            write!(f, "_")
        } else {
            write!(f, "{:?}", self)
        }
    }
}

impl Primitive {
    pub fn name(&self) -> Option<&'static str> {
        self.names().map(|n| n.text)
    }
    pub fn ascii(&self) -> Option<Simple> {
        self.names().and_then(|n| n.ascii)
    }
    pub fn unicode(&self) -> Option<char> {
        self.names().and_then(|n| n.unicode)
    }
    /// Find a primitive by its text name
    pub fn from_name(name: &str) -> Option<Self> {
        Self::all().find(|p| p.names().is_some_and(|n| n.text.eq_ignore_ascii_case(name)))
    }
    pub fn from_simple(s: Simple) -> Option<Self> {
        Self::all().find(|p| p.ascii() == Some(s))
    }
    pub fn from_unicode(c: char) -> Option<Self> {
        Self::all().find(|p| p.unicode() == Some(c))
    }
    pub fn is_modifier(&self) -> bool {
        self.modifier_args().is_some()
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
            Debug => Debug,
            _ => return None,
        })
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
        if name == "eta" || name == "η" {
            return Some(Primitive::Eta);
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching = Primitive::all().filter(|p| {
            p.names()
                .is_some_and(|n| n.is_formattable() && n.text.starts_with(name))
        });
        let res = matching.next()?;
        let exact_match = res.names().unwrap().text == name;
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
        if name == "eta" || name == "η" {
            return Some(vec![(Primitive::Eta, name)]);
        }
        let indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
        if indices.len() < 3 {
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
                let end_index = indices[start + len - 1];
                let sub_name = &name[start_index..=end_index];
                if let Some(p) = Primitive::from_format_name(sub_name) {
                    if len >= 3 || p == Primitive::Pi {
                        prims.push((p, sub_name));
                        start += len;
                        continue 'outer;
                    }
                }
            }
            break None;
        }
    }
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            Primitive::Eta => env.push(PI / 2.0),
            Primitive::Pi => env.push(PI),
            Primitive::Tau => env.push(TAU),
            Primitive::Infinity => env.push(INFINITY),
            Primitive::Noop | Primitive::FillValue => {}
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
            Primitive::Bits => env.monadic_ref_env(Value::bits)?,
            Primitive::InverseBits => env.monadic_ref_env(Value::inverse_bits)?,
            Primitive::Fold => loops::fold(env)?,
            Primitive::Reduce => loops::reduce(env)?,
            Primitive::Each => loops::each(env)?,
            Primitive::Zip => loops::zip(env)?,
            Primitive::Rows => loops::rows(env)?,
            Primitive::Bridge => loops::bridge(env)?,
            Primitive::Distribute => loops::distribute(env)?,
            Primitive::Plow => loops::plow(env)?,
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
                env.sys.print_str(&value.show()).map_err(|e| env.error(e))?;
                env.sys.print_str("\n").map_err(|e| env.error(e))?;
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
                let f = match lib {
                    Value::Func(fs) => fs.data.iter().find_map(|f| {
                        matches!(&f.id, FunctionId::Named(n) if n.as_str().eq_ignore_ascii_case(&name))
                            .then(|| f.clone())
                    }),
                    _ => None,
                }
                .ok_or_else(|| env.error(format!("No function found for {name:?}")))?;
                env.push(f);
            }
            Primitive::Sys(io) => io.run(env)?,
        }
        Ok(())
    }
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
    output: OnceLock<Result<Vec<String>, String>>,
}

impl PrimExample {
    pub fn input(&self) -> &str {
        &self.input
    }
    pub fn output(&self) -> &Result<Vec<String>, String> {
        self.output.get_or_init(|| {
            Uiua::with_backend(&NativeSys)
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
    thread_local! {
        static RE: Regex = Regex::new(r"\[(.*?)\]|`(.*?)`|\*(.*?)\*|([^\[\]`\*]+)").unwrap();
    }
    RE.with(|re| {
        re.captures_iter(line)
            .map(|c| {
                let (mat, [s]) = c.extract();
                if mat.starts_with('[') {
                    if let Some(prim) =
                        Primitive::from_name(s).or_else(|| Primitive::from_format_name(s))
                    {
                        PrimDocFragment::Primitive { prim, named: true }
                    } else {
                        PrimDocFragment::Text(mat.into())
                    }
                } else if mat.starts_with('`') {
                    if let Some(prim) =
                        Primitive::from_name(s).or_else(|| Primitive::from_format_name(s))
                    {
                        PrimDocFragment::Primitive { prim, named: false }
                    } else {
                        PrimDocFragment::Code(s.into())
                    }
                } else if mat.starts_with('*') {
                    PrimDocFragment::Emphasis(s.into())
                } else {
                    PrimDocFragment::Text(s.into())
                }
            })
            .collect::<Vec<_>>()
    })
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
