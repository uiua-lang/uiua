//! All primitive definitions

use super::*;

pub struct ConstantDef {
    pub name: &'static str,
    pub value: Value,
    pub doc: &'static str,
}

macro_rules! constant {
    ($(#[doc = $doc:literal] ($name:ident, $value:expr)),* $(,)?) => {
        /// Named constants that can be redefined
        pub static CONSTANTS: Lazy<[ConstantDef; 0 $(+ { _ = stringify!($name) ; 1})*]> = Lazy::new(|| {
            [$(
                ConstantDef {
                    name: stringify!($name),
                    value: $value.into(),
                    doc: $doc,
                },
            )*]
        });
    }
}

constant!(
    /// Euler's constant
    (e, std::f64::consts::E),
    /// IEEE 754-2008's `NaN`
    (NaN, std::f64::NAN),
    /// A string identifying the operating system
    (os, std::env::consts::OS),
    /// A string identifying family of the operating system
    (Family, std::env::consts::FAMILY),
    /// A string identifying the architecture of the CPU
    (Arch, std::env::consts::ARCH),
    /// The executable file extension
    (ExeExt, std::env::consts::EXE_EXTENSION),
    /// The file extension for shared libraries
    (DllExt, std::env::consts::DLL_EXTENSION),
    /// The primary path separator character
    (Sep, std::path::MAIN_SEPARATOR),
    /// The number of processors available
    (NumProcs, num_cpus::get() as f64),
);

macro_rules! primitive {
    ($(
        #[doc = $doc_rust:literal]
        $(#[doc = $doc:literal])*
        (
            $(
                $($args:literal)?
                $(($delta:expr))?
                $([$mod_args:expr])?
            ,)?
            $variant:ident, $class:ident
            $(,$names:expr)?
        )
    ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
        pub enum Primitive {
            $(
                #[doc = $doc_rust]
                $variant,
            )*
            Sys(SysOp)
        }

        impl Primitive {
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
            pub fn modifier_args(&self) -> Option<u8> {
                match self {
                    $($($(Primitive::$variant => Some($mod_args),)?)?)*
                    Primitive::Sys(op) => op.modifier_args(),
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
                    $($($(Primitive::$variant => $delta.into(),)?)?)*
                    Primitive::Sys(op) => Some(op.outputs()),
                    _ => Some(1)
                }
            }
            pub fn doc(&self) -> Option<&'static PrimDoc> {
                match self {
                    $(Primitive::$variant => {
                        let doc_str = concat!($doc_rust, $($doc, "\n"),*);
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

primitive!(
    /// Duplicate the top value on the stack
    ///
    /// ex: [. 1 2 3 4]
    ///
    /// [duplicate] is often used in examples to show both the input and output of a function.
    /// ex: √.144
    /// ex: .[1 2 3 4]
    ///   : +1⇌
    ///
    /// [duplicate] is often combined with [flip] to process a single value two different ways.
    /// For example, to find the average value of an array, we [divide] its sum(`reduce``add`) by its [length].
    /// ex: ÷⧻∶/+. [4 0 1 2]
    /// Or, maybe you want to find all the numbers in an array that lie within a certain range.
    /// Here, we use [multiply] as a logical AND function.
    /// ex: ×≥5∶≤8. [6 2 5 9 6 5 0 4]
    /// This is a very common pattern.
    ///
    /// [duplicate] can be used to make a monadic left-hook, such as in this palindrome checker:
    /// ex: ≍⇌. "friend"
    /// ex: ≍⇌. "racecar"
    /// Another commonly hooked function is [keep].
    /// ex: ▽=0◿3. [1 4 2 3 9 1 0 6 2 6 3]
    (1(2), Dup, Stack, ("duplicate", '.')),
    /// Duplicate the second-to-top value to the top of the stack
    ///
    /// ex: [, 1 2 3 4 5]
    ///
    /// [over] is often used in examples of functions with two inputs two show both inputs and the output.
    /// ex: [+,, +3 4 5]
    (2(3), Over, Stack, ("over", ',')),
    /// Swap the top two values on the stack
    ///
    /// ex: [∶ 1 2 3 4 5]
    ///
    /// When combined with [duplicate], you can apply two different functions to the same value.
    /// If you have two functions `f` and `g`, the pattern `f``flip``g``duplicate` will call both functions on the top value.
    /// This is a very common pattern.
    /// For example, maybe you want to find all the uppercase letters in a string.
    /// ex: $ Characters On uppercase OnLy
    ///   : ▽×≥@A∶≤@Z..
    /// Or maybe you want to calculate the averge of a list of numbers.
    /// Here, we get the [length] and the `reduce``add``sum` of the list, then [divide] them.
    /// ex: ÷⧻∶/+. 1_8_2_5
    (2(2), Flip, Stack, ("flip", AsciiToken::Colon, '∶')),
    /// Discard the top stack value
    ///
    /// This is usually used to discard values that are no longer needed.
    ///
    /// For example, [gen] returns both a random number and a seed for the next call.
    /// When you have all the random numbers you need, you often want to discard the seed.
    /// ex: ⌊×10[;⍥gen10 0]
    (1(0), Pop, Stack, ("pop", ';')),
    /// Do nothing
    ///
    /// [identity] is mostly useless on its own. See the [Advanced Stack Manipulation Tutorial](/docs/advancedstack) to understand what it is for.
    ///
    /// One way to use it is to pass it to [reduce], which will put all of an array's values on the stack.
    /// ex: /∘ [1 2 3]
    /// However, doing this in a function prevents the signature from being inferred.
    ///
    /// The formatter converts an empty `()` function into `identity`.
    /// ex: /() [1 2] # Try running to format
    ///
    /// While [identity]'s signature is `|1.1`, it will not throw an error if the stack is empty.
    /// ex: ∘
    (1, Identity, Planet, ("identity", '∘')),
    // Pervasive monadic ops
    /// Logical not
    ///
    /// ex: ¬0
    /// ex: ¬1
    /// ex: ¬[0 1 1 0]
    /// ex: ¬[0 1 2 3]
    ///
    /// This is equivalent to `subtract``flip``1`
    /// ex: ¬7
    /// ex: ¬[1 2 3 4]
    (1, Not, MonadicPervasive, ("not", '¬')),
    /// Numerical sign (1, ¯1, or 0)
    ///
    /// ex: ± 1
    /// ex: ± ¯5
    /// ex: ± 0
    /// ex: ± [¯2 ¯1 0 1 2]
    (1, Sign, MonadicPervasive, ("sign", '±')),
    /// Negate a number
    ///
    /// Formats from `\``.
    ///
    /// ex: ¯ 1
    /// ex: ¯ ¯3
    /// ex: ¯ [1 2 ¯3]
    (
        1,
        Neg,
        MonadicPervasive,
        ("negate", AsciiToken::Backtick, '¯')
    ),
    /// Get the absolute value of a number
    ///
    /// ex: ⌵ ¯1
    /// ex: ⌵ 1
    ///
    /// The glyph looks like the graph of `|x|`.
    (1, Abs, MonadicPervasive, ("absolute value", '⌵')),
    /// Take the square root of a number
    ///
    /// ex: √4
    /// ex: √[1 4 9 16]
    /// ex: √¯1
    (1, Sqrt, MonadicPervasive, ("sqrt", '√')),
    /// Get the sine of a number
    ///
    /// ex: ○ 1
    ///
    /// You can get a cosine function by [add]ing [eta].
    /// ex: ○+η 1
    ///
    /// You can get an arcsine function with [invert].
    /// ex: ⍘○ 1
    ///
    /// You can get an arccosine function by [invert]ing the cosine.
    /// ex: ⍘(○+η) 1
    ///
    /// You can get a tangent function by [divide]ing the [sine] by the cosine.
    /// ex: ÷○+η∶○. 0
    (1, Sin, MonadicPervasive, ("sine", '○')),
    /// Get the cosine of a number
    (1, Cos, MonadicPervasive),
    /// Get the arcsine of a number
    (1, Asin, MonadicPervasive),
    /// Get the arccosine of a number
    (1, Acos, MonadicPervasive),
    /// Round to the nearest integer towards `¯∞`
    ///
    /// ex: ⌊1.5
    /// ex: ⌊¯1.5
    /// ex: ⌊[1.5 ¯1.5 0.5 ¯0.5]
    (1, Floor, MonadicPervasive, ("floor", '⌊')),
    /// Round to the nearest integer towards `∞`
    ///
    /// ex: ⌈1.5
    /// ex: ⌈¯1.5
    /// ex: ⌈[1.5 ¯1.5 0.5 ¯0.5]
    (1, Ceil, MonadicPervasive, ("ceiling", '⌈')),
    /// Round to the nearest integer
    ///
    /// ex: ⁅1.2
    /// ex: ⁅¯1.2
    /// ex: ⁅1.5
    /// ex: ⁅[0.1 π 2 9.9 7.5]
    (1, Round, MonadicPervasive, ("round", '⁅')),
    /// Compare for equality
    ///
    /// ex: =1 2
    /// ex: =5 5
    /// ex: =1 [1 2 3]
    /// ex: = [1 2 2] [1 2 3]
    (2, Eq, DyadicPervasive, ("equals", AsciiToken::Equal, '=')),
    /// Compare for inequality
    ///
    /// Formats from `!=`.
    ///
    /// ex: ≠1 2
    /// ex: ≠5 5
    /// ex: ≠1 [1 2 3]
    /// ex: ≠ [1 2 2] [1 2 3]
    (
        2,
        Ne,
        DyadicPervasive,
        ("not equals", AsciiToken::BangEqual, '≠')
    ),
    /// Compare for less than
    ///
    /// The first value is checked to be less than the second.
    /// This is so you can think of `<``x` as a single unit.
    /// ex: <1 2
    /// ex: <5 5
    /// ex: <7 3
    /// ex: <2 [1 2 3]
    /// ex: < [1 2 2] [1 2 3]
    (2, Lt, DyadicPervasive, ("less than", '<')),
    /// Compare for less than or equal
    ///
    /// Formats from `<=`.
    ///
    /// The first value is checked to be less than or equal to the second.
    /// This is so you can think of `≤``x` as a single unit.
    /// ex: ≤1 2
    /// ex: ≤5 5
    /// ex: ≤7 3
    /// ex: ≤2 [1 2 3]
    /// ex: ≤ [1 2 2] [1 2 3]
    (
        2,
        Le,
        DyadicPervasive,
        ("less or equal", AsciiToken::LessEqual, '≤')
    ),
    /// Compare for greater than
    ///
    /// The first value is checked to be greater than the second.
    /// This is so you can think of `>``x` as a single unit.
    /// ex: >1 2
    /// ex: >5 5
    /// ex: >7 3
    /// ex: >2 [1 2 3]
    /// ex: > [1 2 2] [1 2 3]
    (2, Gt, DyadicPervasive, ("greater than", '>')),
    /// Compare for greater than or equal
    ///
    /// Formats from `>=`.
    ///
    /// The first value is checked to be greater than or equal to the second.
    /// This is so you can think of `≥``x` as a single unit.
    /// ex: ≥1 2
    /// ex: ≥5 5
    /// ex: ≥7 3
    /// ex: ≥2 [1 2 3]
    /// ex: ≥ [1 2 2] [1 2 3]
    (
        2,
        Ge,
        DyadicPervasive,
        ("greater or equal", AsciiToken::GreaterEqual, '≥')
    ),
    /// Add values
    ///
    /// ex: +1 2
    /// ex: +1 [2 3 4]
    /// ex: + [1 2 3] [4 5 6]
    (2, Add, DyadicPervasive, ("add", '+')),
    /// Subtract values
    ///
    /// The first value is subtracted from the second.
    /// This is so you can think of `-``x` as a single unit.
    /// ex: -1 2
    /// ex: -1 [2 3 4]
    /// ex: - [1 2 3] [4 5 6]
    (2, Sub, DyadicPervasive, ("subtract", '-')),
    /// Multiply values
    ///
    /// Formats from `*`.
    ///
    /// ex: ×3 5
    /// ex: ×2 [1 2 3]
    /// ex: × [1 2 3] [4 5 6]
    ///
    /// Uiua does not have dedicated boolean logical operators.
    /// [multiply] can be used as a logical AND.
    /// ex: ×,,≥5∶≤8. [6 2 5 9 6 5 0 4]
    (2, Mul, DyadicPervasive, ("multiply", AsciiToken::Star, '×')),
    /// Divide values
    ///
    /// Formats from `%`.
    ///
    /// The second value is divided by the first.
    /// This is so you can think of `÷``x` as a single unit.
    /// ex: ÷3 12
    /// ex: ÷2 [1 2 3]
    /// ex: ÷ [1 2 3] [4 5 6]
    (
        2,
        Div,
        DyadicPervasive,
        ("divide", AsciiToken::Percent, '÷')
    ),
    /// Modulo values
    ///
    /// The second value is divided by the first, and the remainder is returned.
    /// This is so you can think of `◿``x` as a single unit.
    /// ex: ◿10 27
    /// ex: ◿5 [3 7 14]
    /// ex: ◿ [3 4 5] [10 10 10]
    (2, Mod, DyadicPervasive, ("modulus", '◿')),
    /// Raise a value to a power
    ///
    /// The second value is raised to the power of the first.
    /// This is so you can think of `ⁿ``x` as a single unit.
    /// ex: ⁿ2 3
    /// ex: ⁿ2 [1 2 3]
    /// ex: ⁿ [1 2 3] [4 5 6]
    (2, Pow, DyadicPervasive, ("power", 'ⁿ')),
    /// Get the based logarithm of a number
    ///
    /// The first value is the base, and the second value is the power.
    /// ex: ₙ2 8
    /// ex: ₙ2 [8 16 32]
    /// ex: ₙ [2 3 4] [16 27 1024]
    (2, Log, DyadicPervasive, ("logarithm", 'ₙ')),
    /// Take the minimum of two arrays
    ///
    /// ex: ↧ 3 5
    /// ex: ↧ [1 4 2] [3 7 1]
    ///
    /// Uiua does not have dedicated boolean logical operators.
    /// [minimum] can be used as a logical AND.
    /// ex: ≥5∶≤8. [6 2 5 9 6 5 0 4]
    ///   : ↧,,
    (2, Min, DyadicPervasive, ("minimum", '↧')),
    /// Take the maximum of two arrays
    ///
    /// ex: ↥ 3 5
    /// ex: ↥ [1 4 2] [3 7 1]
    ///
    /// Uiua does not have dedicated boolean logical operators.
    /// [maximum] can be used as a logical OR.
    /// ex: ↥,,≤5∶≥8. [6 2 5 9 6 5 0 4]
    (2, Max, DyadicPervasive, ("maximum", '↥')),
    /// Take the arctangent of two numbers
    ///
    /// This takes a `y` and `x` argument and returns the angle in radians in the range `(-π, π]`.
    /// ex: ∠ 1 0
    /// ex: ∠ ¯1 0
    /// ex: ∠ √2 √2
    (2, Atan, DyadicPervasive, ("atangent", '∠')),
    /// Get the number of rows in an array
    ///
    /// ex: ⧻5
    /// ex: ⧻[]
    /// ex: ⧻1_2_3
    /// ex: ⧻[1_2 3_4 5_6]
    ///
    /// [length] is equivalent to the [first] of the [shape].
    /// ex:  ⧻[1_2_3 4_5_6]
    ///   : ⊢△[1_2_3 4_5_6]
    (1, Len, MonadicArray, ("length", '⧻')),
    /// Get the dimensions of an array
    ///
    /// ex: △5
    /// ex: △[]
    /// ex: △1_2_3
    /// ex: △[1_2 3_4 5_6]
    ///
    /// It is a triangle`△` because a triangle is a shape.
    (1, Shape, MonadicArray, ("shape", '△')),
    /// Make an array of all natural numbers less than a number
    ///
    /// The rank of the input must be `0` or `1`.
    /// ex: ⇡5
    /// ex: ⇡2_3
    ///
    /// When creating ranges with upper bounds that are rank `1`, [pick]ing the generated range array from an array with the [shape] of the input will yield that array.
    /// ex:     [1_2_3 4_5_6]
    ///   :    △[1_2_3 4_5_6]
    ///   :   ⇡△[1_2_3 4_5_6]
    ///   : ⊡⇡△.[1_2_3 4_5_6]
    (1, Range, MonadicArray, ("range", '⇡')),
    /// Get the first row of an array
    ///
    /// ex: ⊢1_2_3
    /// ex: ⊢[1_2 3_4 5_6]
    /// ex! ⊢[]
    /// ex! ⊢1
    (1, First, MonadicArray, ("first", '⊢')),
    /// Get the last element of an array
    (1, Last, MonadicArray),
    /// Reverse the rows of an array
    ///
    /// ex: ⇌1_2_3_9
    /// ex: ⇌[1_2 3_4 5_6]
    (1, Reverse, MonadicArray, ("reverse", '⇌')),
    /// Make an array 1-dimensional
    ///
    /// ex: ♭5
    /// ex: ♭[1 2 3]
    /// ex: ♭.[1_2 3_4 5_6]
    ///
    /// It looks like `♭` because it *flat*tens the array.
    ///
    /// See also: [reshape]
    (1, Deshape, MonadicArray, ("deshape", '♭')),
    /// Encode an array as bits (big-endian)
    ///
    /// The result will always be 1 rank higher than the input.
    /// ex: ⋯27
    /// ex: ⋯⇡8
    /// ex: ⋯[1_2 3_4 5_6]
    ///
    /// [invert][bits] can be used to decode the bits back into numbers.
    /// ex: ⍘⋯ [1 0 1]
    /// ex: ⍘⋯ [0 1 1 0 1]
    /// ex: ⍘⋯ [[0 1 1]
    ///   :     [1 0 0]
    ///   :     [1 1 0]]
    (1, Bits, MonadicArray, ("bits", '⋯')),
    /// Inverse of Bits
    (1, InverseBits, MonadicArray),
    /// Rotate the shape of an array
    ///
    /// ex: ⍉.[1_2 3_4 5_6]
    /// ex: ⍉.[[1_2 3_4] [5_6 7_8]]
    ///
    /// `shape``transpose` is always equivalent to `rotate``1``shape`.
    /// ex: [1_2 3_4 5_6]
    ///   : ↻1△ .
    ///   : △⍉  ∶
    (1, Transpose, MonadicArray, ("transpose", '⍉')),
    /// Inverse of Transpose
    (1, InvTranspose, MonadicArray),
    /// Get the indices into an array if it were sorted ascending
    ///
    /// The [rise] of an array is the list of indices that would sort the array ascending if used with [select].
    /// ex: ⍏6_2_7_0_¯1_5
    /// Using the [rise] as a selector in [select] yields the sorted array.
    /// ex: ⊏⍏.6_2_7_0_¯1_5
    ///
    /// If we transform the array before [rise]ing, we can sort by a key.
    /// Here, we sort the array ascending by the [absolute value] of its elements.
    /// ex: ⊏⍏⌵.6_2_7_0_¯1_5
    (1, Rise, MonadicArray, ("rise", '⍏')),
    /// Get the indices into an array if it were sorted descending
    ///
    /// The [fall] of an array is the list of indices that would sort the array descending if used with [select].
    /// ex: ⍖6_2_7_0_¯1_5
    /// Using the [fall] as a selector in [select] yields the sorted array.
    /// ex: ⊏⍖.6_2_7_0_¯1_5
    ///
    /// If we transform the array before [fall]ing, we can sort by a key.
    /// Here, we sort the array descending by the [absolute value] of its elements.
    /// ex: ⊏⍖⌵.6_2_7_0_¯1_5
    (1, Fall, MonadicArray, ("fall", '⍖')),
    /// Get indices where array values are not equal to zero
    ///
    /// The most basic use is to convert a mask into a list of indices.
    /// ex: ⊚ [1 0 0 1 0 1 1 0]
    /// ex: ⊚.=0◿3.[1 0 2 9 3 8 3 4 6]
    /// It also works for non-zero counts
    /// ex: ⊚ 1_2_3
    /// ex: ⊚ 1_4_2
    /// [where] is equivalent to `keep``flip``range``length``duplicate`
    /// ex:     ⊚ [0 1 0 0 2 0 1]
    /// ex: ▽∶⇡⧻. [0 1 0 0 2 0 1]
    ///
    /// [invert][where] will convert the indices back into a a list of counts
    /// ex: ⍘⊚ [0 0 0 1 1 2 2 2 2 2 3]
    /// The indices need not be in order
    /// ex: ⍘⊚ [0 1 2 2 0 3 2 1 2 0 2]
    ///
    /// [where] on a scalar is equivalent to [where] on a singleton array of that scalar, and so creates a list of `0`s.
    /// ex: ⊚3
    /// ex: ⊚8
    (1, Where, MonadicArray, ("where", '⊚')),
    /// Inverse of where
    (1, InvWhere, MonadicArray),
    /// Assign a unique index to each unique element in an array
    ///
    /// ex: ⊛7_7_8_0_1_2_0
    /// ex: ⊛"Hello, World!"
    ///
    /// When combined with [group], you can do things like counting the number of occurrences of each character in a string.
    /// ex: $ Count the characters is this string
    ///   : ⊕($"_ _"⊢∶⧻.) ⊛.⊏⍏.
    (1, Classify, MonadicArray, ("classify", '⊛')),
    /// Remove duplicate elements from an array
    ///
    /// ex: ⊝7_7_8_0_1_2_0
    /// ex: ⊝"Hello, World!"
    /// ex: ⊝[3_2 1_4 3_2 5_6 1_4 7_8]
    (1, Deduplicate, MonadicArray, ("deduplicate", '⊝')),
    /// Turn an array into a box
    ///
    /// This is Uiua's primary way to create nested or mixed-type arrays.
    /// Normally, arrays can only be created if their rows have the same shape and type.
    /// [fill] can help you with the shape part, but it is not always wanted, and it can't help with the type part.
    /// ex! [@a 3 7_8_9]
    /// [box] creates a box element that contains the array. All boxes, no matter the type of shape of their contents, are considered the same type and can be put into arrays together.
    /// ex: [□@a □3 □7_8_9]
    /// The more ergonomic way to make box arrays is to use `{}`s instead of `[]`s.
    /// ex: {@a 3 7_8_9}
    /// Use [unbox] to get the values back out.
    /// ex: ⊔ □1_2_3
    /// [reduce][unbox] will unpack an array of boxs onto the stack.
    /// ex: /⊔ {@a 3 7_8_9}
    ///
    /// You would not normally construct arrays like the one above.
    /// The more important use case of [box] is for jagged or nested data.
    /// If you want to collect unevenly-sized groups from [partition] or [group], without [fill]ing, you must use [box].
    /// ex: $ Words of different lengths
    ///   : ⊜□≠@ .
    ///
    /// Most monadic functions, like [reverse], will work on box elements without needing to [unbox] them.
    /// ex: $ Reverse these words
    ///   : ⊜□≠@ .
    ///   : ∵⇌.
    ///
    /// For more complex operations, you can use [under][unbox] or [pack].
    /// ex: Parts ← .⊜□≠@ . $ Prepend the word length
    ///   : F ← ($"_ _"⧻.)
    ///   : ∵⍜⊔F Parts
    ///   : ⊐∵F Parts.
    /// [under][unbox] works because `invert``unbox` is just `box`. For each element, it un-[box]s the [box] function to get the array out, does something to it, then [box]s the result.
    /// The difference between [under][unbox] and [pack] is that [pack] will only [box] the results if it is necessary.
    /// ex: A ← .{1_2_3 4_5 [7]}
    ///   : ∵⍜⊔(⬚0↙3) A
    ///   : ⊐∵(⬚0↙3) A
    (1, Box, MonadicArray, ("box", '□')),
    /// Take an array out of a box
    ///
    /// ex: ⊔□5
    /// ex: ∵⊔{1_2_3 4_5_6}
    ///
    /// Boxes are created with [box].
    (1, Unbox, MonadicArray, ("unbox", '⊔')),
    /// Check if two arrays are exactly the same
    ///
    /// ex: ≍ 1_2_3 [1 2 3]
    /// ex: ≍ 1_2_3 [1 2]
    (2, Match, DyadicArray, ("match", '≍')),
    /// Combine two arrays as rows of a new array
    ///
    /// `first``shape` of the coupled array will *always* be `2`.
    ///
    /// For scalars, it is equivalent to [join].
    /// ex: ⊟ 1 2
    ///   : ⊂ 1 2
    ///
    /// For arrays, a new array is created with the first array as the first row and the second array as the second row.
    /// ex: ⊟ [1 2 3] [4 5 6]
    ///
    /// By default, arrays with different shapes cannot be [couple]ed.
    /// ex! ⊟ [1 2 3] [4 5]
    /// Use [fill] to make their shapes match
    /// ex: ⬚∞⊟ [1 2 3] [4 5]
    ///
    /// [couple] is compatible with [under].
    /// ex: ⍜⊟'×2 3 5
    (2, Couple, DyadicArray, ("couple", '⊟')),
    /// Split an array into two arrays
    (1(2), Uncouple, MonadicArray),
    /// Append two arrays end-to-end
    ///
    /// For scalars, it is equivalent to [couple].
    /// ex: ⊂ 1 2
    ///   : ⊟ 1 2
    ///
    /// If the arrays have the same rank, it will append the second array to the first.
    /// ex: ⊂ [1 2] [3 4]
    /// ex: ⊂ [1_2 3_4] [5_6 7_8]
    ///
    /// If the arrays have a rank difference of 1, then the array with the smaller rank will be prepended or appended to the other as a row.
    /// ex: ⊂ 1 [2 3]
    /// ex: ⊂ [1 2] 3
    /// ex: ⊂ 1_2 [3_4 5_6]
    /// ex: ⊂ [1_2 3_4] 5_6
    ///
    /// By default, arrays that do not have equal [shape] suffixes cannot be joined.
    /// ex! ⊂ [1_2 3_4] [5_6_7 8_9_10]
    /// Use [fill] to make their shapes compatible.
    /// ex: ⬚0⊂ [1_2 3_4] [5_6_7 8_9_10]
    ///
    /// [join]'s glyph is `⊂` because it kind of looks like a magnet pulling its two arguments together.
    (2, Join, DyadicArray, ("join", '⊂')),
    /// Select multiple rows from an array
    ///
    /// For a scalar selector, [select] is equivalent to [pick].
    /// ex: ⊏ 2 [8 3 9 2 0]
    ///   : ⊡ 2 [8 3 9 2 0]
    /// For a rank `1` selector, [select] will pick multiple items from an array.
    /// ex: ⊏ 4_2 [8 3 9 2 0]
    /// ex: ⊏ 0_2_1_1 [1_2_3 4_5_6 7_8_9]
    /// If the selector's rank is `greater than``1`, then each row of the selector will be selected separately.
    /// ex: ⊏ [0_1 1_2 2_3] [2 3 5 7]
    /// ex: ⊏ [0_1 1_2 2_0] [1_2_3 4_5_6 7_8_9]
    (2, Select, DyadicArray, ("select", '⊏')),
    /// End step of under select
    (3, Unselect, Misc),
    /// Index a row or elements from an array
    ///
    /// An index with rank `0` or `1` will pick a single row or element from an array.
    /// ex: ⊡ 2 [8 3 9 2 0]
    /// ex: ⊡ 1_1 .[1_2_3 4_5_6]
    ///
    /// If the index's rank is `2` or greater, then multiple rows or elements will be picked.
    /// ex: ⊡ [1_2 0_1] [1_2_3 4_5_6]
    ///
    /// For index rank `2` or greater, it should hold that `pick``range``shape``duplicate``x` is equivalent to `x`.
    /// ex: ⊡⇡△. [1_2_3 4_5_6]
    (2, Pick, DyadicArray, ("pick", '⊡')),
    /// End step of under pick
    (3, Unpick, Misc),
    /// Change the shape of an array
    ///
    /// ex: ↯ 2_3 [1 2 3 4 5 6]
    /// Shapes that have fewer elements than the original array will truncate it.
    /// ex: ↯ 2_2 [1_2_3 4_5_6]
    /// Shapes that have more elements than the original array will repeat elements.
    /// ex: ↯ [5] 2
    /// ex: ↯ 3_7 1_2_3_4
    ///
    /// Scalar shapes will copy the array as rows of a new array.
    /// ex: ↯ 4 [1 2 3 4 5]
    /// ex: ↯ 2 [1_2_3 4_5_6]
    /// This is in constrast to scalar [keep], which repeats the rows but preserves rank.
    /// ex: ▽ 4 [1 2 3 4 5]
    /// ex: ▽ 2 [1_2_3 4_5_6]
    ///
    /// At most one of the dimensions of the new shape may be negative. This indicates that this is a *derived* dimension, and it will be calculated to make the total number of elements in the new shape be `less or equal` the total number of elements in the original shape.
    /// ex: ↯5_¯1 ⇡15
    /// ex: ↯¯1_5 ⇡15
    /// ex: ↯2_2_¯1 ⇡15
    /// ex: ↯¯1_2_2 ⇡15
    /// ex: ↯3_¯1_5 ⇡30
    /// If [fill] is used, the total number of elements in the new shape will always be [equal] to the total number of elements in the original shape.
    /// ex: ⬚0↯ ¯1_5 ⇡12
    ///
    /// See also: [deshape]
    (2, Reshape, DyadicArray, ("reshape", '↯')),
    /// Take the first n elements of an array
    ///
    /// This is the opposite of [drop].
    ///
    /// ex: ↙ 3 [8 3 9 2 0]
    /// ex: ↙ ¯3 [8 3 9 2 0]
    /// ex: ↙ 2 ↯3_3⇡9
    /// ex: ↙ ¯2 ↯3_3⇡9
    /// The amount to take can also be a list to take along multiple axes.
    /// ex: .↯3_4⇡12
    ///   : ↘2_3   .
    ///   : ↘¯2_¯2 :
    ///
    /// By default, taking more than the length of the array will throw an error.
    /// ex! ↙7 [8 3 9 2 0]
    /// If you would like to fill the excess length with some fill value, use [fill].
    /// ex: ⬚π↙ 7 [8 3 9 2 0]
    (2, Take, DyadicArray, ("take", '↙')),
    /// End step of under take
    (3, Untake, Misc),
    /// Drop the first n elements of an array
    ///
    /// This is the opposite of [take].
    ///
    /// ex: ↘ 3 [8 3 9 2 0]
    /// ex: ↘ ¯3 [8 3 9 2 0]
    /// ex: ↘ 2 ↯3_3⇡9
    /// ex: ↘ ¯2 ↯3_3⇡9
    /// The amount to drop can also be a list to drop along multiple axes.
    /// ex: .↯3_4⇡12
    ///   : ↘1_2   .
    ///   : ↘¯2_¯1 :
    ///
    /// Dropping more than the length of the array will leave an empty array.
    /// ex: ↘ 7 [8 3 9 2 0]
    /// ex: ↘ ¯7 [8 3 9 2 0]
    /// ex: ↘ 5 ↯3_3⇡9
    /// ex: ↘ ¯5 ↯3_3⇡9
    (2, Drop, DyadicArray, ("drop", '↘')),
    /// End step of under drop
    (3, Undrop, Misc),
    /// Rotate the elements of an array by n
    ///
    /// ex: ↻1 ⇡5
    /// ex: ↻2 ⇡5
    /// ex: ↻¯1 ⇡5
    /// ex: ↻2 .↯3_4⇡12
    ///
    /// Multi-dimensional rotations are supported.
    /// ex: ↻1_2 .↯4_5⇡20
    (2, Rotate, DyadicArray, ("rotate", '↻')),
    /// The n-wise windows of an array
    ///
    /// ex: ◫2 .⇡4
    /// ex: ◫4 .⇡6
    ///
    /// Multi-dimensional window sizes are supported.
    /// ex: ◫2_2 .[1_2_3 4_5_6 7_8_9]
    (2, Windows, DyadicArray, ("windows", '◫')),
    /// Discard or copy some rows of an array
    ///
    /// Takes two arrays. The first array is the number of copies to keep of each row of the second array.
    /// ex: ▽ [1 0 2 3 1] [8 3 9 2 0]
    ///
    /// By making the first array a mask derived from the second, [keep] becomes a filter.
    /// In this example, the input string is [duplicate]ed, and a mask is created from it using `greater or equal``@a`. Then, [keep] uses the mask to filter the string.
    /// ex: ▽≥@a . "lOWERCASe onLY"
    ///
    /// [keep] with a scalar for the first argument repeats the rows of the second argument that many times.
    /// ex: ▽ 3 [1 2 3]
    /// ex: ▽ 2 [1_2_3 4_5_6]
    /// This is in constrast to scalar [reshape], which copies the array as rows of a new array.
    /// ex: ↯ 3 [1 2 3]
    /// ex: ↯ 2 [1_2_3 4_5_6]
    ///
    /// The counts list can be [fill]ed if it is shorter than the kept array.
    /// ex: ⬚3▽ [1 0 2] [8 3 9 2 0]
    ///
    /// [keep]'s glyph is `▽` because its main use is to filter, and `▽` kind of looks like a coffee filter.
    (2, Keep, DyadicArray, ("keep", '▽')),
    /// End step of under keep
    (3, Unkeep, Misc),
    /// Find the occurences of one array in another
    ///
    /// ex: ⌕ 5 [1 8 5 2 3 5 4 5 6 7]
    /// ex: ⌕ "ab" "abracadabra"
    /// ex: ⌕ 1_2 . ↯4_4⇡3
    (2, Find, DyadicArray, ("find", '⌕')),
    /// Check if each row of one array exists in another
    ///
    /// ex: ∊ 2 [1 2 3]
    /// ex: ∊ 5 [1 2 3]
    /// ex: ∊ [1 2 3] [0 3 4 5 1]
    /// ex: ∊ [4 5 6] [1_2_3 4_5_6]
    /// ex: ∊ [1_2_3 4_5_6] [3 4 5]
    /// ex: ∊ 2 [1_2_3 4_5_6]
    ///
    /// With the help of [keep], you can use [member] to get a set intersection.
    /// ex: ▽∊, "abracadabra" "that's really cool"
    ///
    /// [member] is closely related to [indexof].
    (2, Member, DyadicArray, ("member", '∊')),
    /// Find the index of each row of one array in another
    ///
    /// If the index cannot be found, the [length] of the searched-in array is returned.
    /// ex: ⊗ 2 [1 2 3]
    /// ex: ⊗ 5 [1 2 3]
    /// ex: ⊗ [1 2 3] [0 3 4 5 1]
    /// ex: ⊗ [4 5 6] [1_2_3 4_5_6]
    /// ex: ⊗ [1_2_3 4_5_6] [3 4 5]
    /// ex: ⊗ 2 [1_2_3 4_5_6]
    ///
    /// You can use the returned indices with [select] to get the rows that were found.
    /// If you expect one of the searched-for rows to be missing, you can use [fill] to set a default value.
    /// ex: a ← [2 3 5 7 11 13]
    ///   : .⊗,a [1 2 3 4 5]
    ///   : ⬚∞⊏∶a
    ///
    /// [indexof] is closely related to [member].
    (2, IndexOf, DyadicArray, ("indexof", '⊗')),
    /// Apply a reducing function to an array
    ///
    /// For reducing with an initial value, see [fold].
    ///
    /// `reduce``add` sums the rows of an array.
    /// ex: /+ 1_2_3_4_5
    /// [reduce] goes from left to right. This is important for non-commutative functions like [subtract].
    /// ex: /- 1_2_3_4_5
    ///
    /// If you want to see the intermediate values, you can use [scan].
    /// ex: /- 1_2_3_4_5
    ///   : \- 1_2_3_4_5
    ///
    /// You can can reduce with arbitrary functions.
    /// ex: /(×+1) 1_2_3_4_5
    ///
    /// [break]ing out of [reduce] discards the unreduced values.
    /// ex: /(⎋≥10.+) [3 4 8 9]
    ///
    /// Some functions have default values if the array is empty.
    /// Functions without default values will throw an error if the array is empty.
    /// ex: /+ []
    /// ex: /× []
    /// ex: /↥ []
    /// ex: /↧ []
    /// ex! /∠ []
    (1[1], Reduce, AggregatingModifier, ("reduce", '/')),
    /// Reduce, but keep intermediate values
    ///
    /// ex: \+   1_2_3_4
    /// ex: \-   1_2_3_4
    /// ex: \'-∶ 1_2_3_4
    ///
    /// [break]ing out of [scan] appends the unscanned values without applying the function to them.
    /// This means the length of the output is always the same as that of the input.
    /// ex: \(⎋≥10.+) [1 2 3 4 5 6 7 8]
    (1[1], Scan, AggregatingModifier, ("scan", '\\')),
    /// Apply a function to each element of an array or arrays.
    ///
    /// This is the element-wise version of [rows].
    /// **This is often not what you want.** Prefer using pervasive functions or [table] when possible.
    ///
    /// The number of arrays used depends on how many arguments the function takes.
    /// ex: ∵'⊟. 1_2_3_4
    /// ex: ∵⊂ 1_2_3 4_5_6
    /// ex: ∵⊂ 1_2 [4_5 6_7]
    ///
    /// If the function is already pervasive, then [each] is redundant.
    /// ex! ∵+ 1_2_3 4_5_6
    /// ex:  + 1_2_3 4_5_6
    ///
    /// [each] is equivalent to [level]`0` (or `level``[0 0 …]` for multiple arrays).
    ([1], Each, IteratingModifier, ("each", '∵')),
    /// Apply a function to each row of an array or arrays
    ///
    /// This is the row-wise version of [each].
    ///
    /// ex:  /+ [1_2_3 4_5_6 7_8_9]  # Sum each row with the next
    /// ex: ≡/+ [1_2_3 4_5_6 7_8_9]  # Sum the elements of each row
    ///
    /// The number of arrays used depends on how many arguments the function takes.
    /// ex: ≡⊂    1_2 [4_5 6_7]
    /// ex: ≡∧⋄~+ 1_2 [4_5 6_7]
    ///
    /// [rows] is equivalent to [level]`¯1` (or `level``[¯1 ¯1 …]` for multiple arrays).
    /// ex: ≑¯1/+ [1_2_3 4_5_6 7_8_9]
    /// ex:   ≡/+ [1_2_3 4_5_6 7_8_9]
    ([1], Rows, IteratingModifier, ("rows", '≡')),
    /// Apply a function to a fixed value and each row of an array
    ///
    /// This is a reversed version of [tribute].
    ///
    /// ex: ∺⊂ 1 2_3_4
    /// ex: ∺⊂ 1_2_3 4_5_6
    ///
    /// [distribute] is equivalent to [level]`[``infinity``¯1]`.
    /// ex:       ∺⊂ 1_2_3 4_5_6
    ///   : ≑[∞ ¯1]⊂ 1_2_3 4_5_6
    ///   :     ≑⋄~⊂ 1_2_3 4_5_6
    (2[1], Distribute, IteratingModifier, ("distribute", '∺')),
    /// Apply a function to each row of an array and a fixed value
    ///
    /// This is a reversed version of [distribute].
    ///
    /// ex: ≐⊂ 1_2_3 4
    /// ex: ≐⊂ 1_2_3 4_5_6
    ///
    /// [tribute] is equivalent to [level]`[¯1``infinity``]`.
    /// ex:       ≐⊂ 1_2_3 4_5_6
    ///   : ≑[¯1 ∞]⊂ 1_2_3 4_5_6
    ///   :     ≑~⋄⊂ 1_2_3 4_5_6
    (2[1], Tribute, IteratingModifier, ("tribute", '≐')),
    /// Apply a function to each combination of elements of two arrays
    ///
    /// This is the element-wise version of [cross].
    /// This is probably what you want instead of [each].
    ///
    /// ex: ⊞+ 1_2_3 4_5_6_7
    /// ex: ⊞⊂ 1_2 3_4
    ///
    /// The first array's shape becomes the first part of the result's shape, and the second array's shape becomes the next part.
    /// The end of the result's shape is determined by the function's result.
    /// ex: △⊞+ 1_2 3_4_5
    /// ex: △⊞⊂ 1_2 3_4_5
    /// ex: △⊞+ [1_2_3 4_5_6] [7 8 9 10]
    /// ex: △⊞⊂ [1_2_3 4_5_6] [7 8 9 10]
    (2[1], Table, IteratingModifier, ("table", '⊞')),
    /// Apply a function to each combination of rows of two arrays
    ///
    /// This is the row-wise version of [table].
    ///
    /// ex: a ← .[1_2 3_4 5_6]
    ///   : b ← .[7_8 9_10]
    ///   : ⊠⊂ a b
    (2[1], Cross, IteratingModifier, ("cross", '⊠')),
    /// Repeat a function a number of times
    ///
    /// ex: ⍥(+2)5 0
    /// ex: ⍥(⊂2)5 []
    /// One interesting use of `repeat` is to collect some number of stack values into an array.
    /// ex: ⍥⊂3 [] 1 2 3
    /// Repeating [infinity] times will create an infinite loop.
    /// You can use [break] to break out of the loop.
    /// ex: ⍥(⎋>1000. ×2)∞ 1
    /// [repeat]ing a negative number of times will repeat the [invert]ed function.
    /// ex: ⍥√¯3 2
    ///
    /// [repeat]'s glyph is a combination of a circle, representing a loop, and the 𝄇 symbol from musical notation.
    (1[1], Repeat, IteratingModifier, ("repeat", '⍥')),
    /// Group elements of an array into buckets by index
    ///
    /// Takes a function and two arrays.
    /// The arrays must be the same [length].
    /// The first array must be rank `1` and contain integers.
    /// Rows in the second array will be grouped into buckets by the indices in the first array.
    /// Keys `less than``0` will be omitted.
    /// The function then processes each group in order. The result depends on what the function is.
    /// If the function takes 0 or 1 arguments, then [group] behaves like [rows].
    /// ex: ⊕∘ [0 2 2 1 0 1] [1 2 3 4 5 6]
    /// If the function takes 2 arguments, then [group] requires and accumulator and behaves like [fold].
    /// ex: ⊕⊂ [] [0 2 2 1 0 1] [1 2 3 4 5 6]
    /// If the values returned by the function do not have the same [shape], concatenation will fail.
    /// ex! ⊕∘ [0 1 0 2 1 1] [1 2 3 4 5 6]
    /// It is common to use [box] to encapsulate groups of different [shape]s.
    /// ex: ⊕□ [0 1 0 2 1 1] [1 2 3 4 5 6]
    ///
    /// If you want to get the length of each group, use [length].
    /// ex: ⊕⧻ [0 1 0 2 1 1] [1 2 3 4 5 6]
    ///
    /// When combined with [classify], you can do things like counting the number of occurrences of each character in a string.
    /// ex: $ Count the characters is this string
    ///   : ⊕{⊢∶⧻.} ⊛.⊏⍏.
    ///
    /// [group] is closely related to [partition].
    (2[1], Group, AggregatingModifier, ("group", '⊕')),
    /// Group elements of an array into buckets by sequential keys
    ///
    /// Takes a function and two arrays.
    /// The arrays must be the same [length].
    /// The first array must be rank `1` and contain integers.
    /// Rows in the second array that line up with sequential keys in the first array will be grouped together.
    /// Keys `less or equal``0` will be omitted.
    /// The function then processes each group in order. The result depends on what the function is.
    /// If the function takes 0 or 1 arguments, then [partition] behaves like [rows].
    /// ex: ⊜∘ [0 0 2 2 1 1 3 3] [1 2 3 4 5 6 7 8]
    /// If the function takes 2 arguments, then [partition] requires and accumulator and behaves like [fold].
    /// ex: ⊜⊂ [] [0 0 2 2 1 1 3 3] [1 2 3 4 5 6 7 8]
    /// If the values returned by the function do not have the same [shape], concatenation will fail.
    /// ex! ⊜∘ [0 2 3 3 3 0 1 1] [1 2 3 4 5 6 7 8]
    /// It is common to use [box] to encapsulate groups of different [shape]s.
    /// ex: ⊜□ [0 2 3 3 3 0 1 1] [1 2 3 4 5 6 7 8]
    ///
    /// If you want to get the length of each group, use [length].
    /// ex: ⊜⧻ [0 2 3 3 3 0 1 1] [1 2 3 4 5 6 7 8]
    ///
    /// This can be used to split an array by a delimiter.
    /// ex: ⊜□ ≠@ . $ Hey there friendo
    ///
    /// [partition] is closely related to [group].
    (2[1], Partition, AggregatingModifier, ("partition", '⊜')),
    /// Apply a function with implicit (un)boxing
    ///
    /// When working with [box]ed data, [pack] will automatically [unbox] the data for functions like [join].
    /// ex:  /⊂ {"a" "bc" "def"}
    /// ex: ⊐/⊂ {"a" "bc" "def"}
    ///
    /// If combining arrays would fail because of a shape mismatch, [pack] will automatically [box] the arrays.
    /// ex!  \⊂⇡5
    /// ex: ⊐\⊂⇡5
    /// ex: ⊐⊟ 5 "hello"
    ///
    /// While `{}`s will always [box] their contents, [pack] will only [box] if necessary.
    /// ex:  {1 2_3}
    ///   : ⊐[1 2_3]
    /// ex:  {1 2 3}
    ///   : ⊐[1 2 3]
    ///
    /// [pack] is overridden by [fill], regardless of order.
    /// ex: ⬚0⊐∵⇡ ⇡5
    ([1], Pack, OtherModifier, ("pack", '⊐')),
    /// Invert the behavior of a function
    ///
    /// Most functions are not invertible.
    ///
    /// ex: √2
    /// ex: ⍘√2
    ///
    /// [invert][couple] uncouples a [length]`2` array and pushes both rows onto the stack.
    /// ex: ⍘⊟ .[1_2_3 4_5_6]
    ///
    /// [invert][transpose] transposes in the opposite direction.
    /// This is useful for arrays with rank `greater than``2`.
    /// ex: ⍘⍉ .⊟.[1_2_3 4_5_6]
    ///
    /// [invert][bits] converts an array of bits into a number.
    /// ex: ⍘⋯ [1 0 1 0 1 0 1 0]
    ///
    /// [invert][sine] gives the arcsine.
    /// ex: ⍘○ 1
    ///
    /// While more inverses exists, most of them are not useful on their own.
    /// They are usually used within [under].
    ([1], Invert, OtherModifier, ("invert", '⍘')),
    /// Discard the top stack value then call a function
    ///
    /// See the [Advanced Stack Manipulation Tutorial](/docs/advancedstack) for a more complete understanding of why [gap] is useful.
    ///
    /// ex: ⋅+ 1 2 3
    /// This may seem useless when [pop] exists, but [gap] really shines when used with [fork].
    /// In a [fork] expression, you can use [dip], [gap], and [identity] to select out values.
    /// For example, if you wanted to add 3 values but keep the last value on top of the stack:
    /// ex: [⊃⋅⋅∘(++) 3 5 10]
    /// By using fewer `gap`s, you can select a different value.
    /// ex: [⊃⋅∘(++) 3 5 10]
    /// ex: [⊃∘(++) 3 5 10]
    /// By replacing a `gap` with a `dip`, you keep the argument in that spot instead of popping it:
    /// ex: [⊃⊙⋅∘(++) 3 5 10]
    /// ex: [⊃⋅⊙∘(++) 3 5 10]
    /// ex: [⊃⊙⊙∘(++) 3 5 10]
    ([1], Gap, Planet, ("gap", '⋅')),
    /// Temporarily pop the top value off the stack and call a function
    ///
    /// See the [Advanced Stack Manipulation Tutorial](/docs/advancedstack) for a more complete understanding of why [dip] is useful.
    ///
    /// ex: [⊙+ 1 2 3]
    /// ex: [⊙⊙+ 1 2 3 4]
    /// This is especially useful when used in a [fork].
    /// In a [fork] expression, you can use [dip], [gap], and [identity] to select out values.
    /// For example, if you wanted to add 3 values but keep the all 3 on top of the stack:
    /// ex: [⊃⊙⊙∘(++) 3 5 10]
    /// By replacing a `dip` with a `gap`, you pop the argument in that spot instead of keeping it:
    /// ex: [⊃⊙⊙∘(++) 3 5 10]
    /// ex: [⊃⊙⋅∘(++) 3 5 10]
    /// ex: [⊃⋅⊙∘(++) 3 5 10]
    /// ex: [⊃⊙∘(++) 3 5 10]
    ([1], Dip, Planet, ("dip", '⊙')),
    /// Call a function on two sets of values
    ///
    /// For monadic functions, [both] calls its function on each of the top 2 values on the stack.
    /// ex: ∩⇡ 3 5
    ///
    /// For a function that takes `n` arguments, [both] calls the function on the 2 sets of `n` values on top of the stack.
    /// ex: [∩+ 1 2 3 4]
    /// ex: [∩(++) 1 2 3 4 5 6]
    ///
    /// [both] can also be chained. Every additional [both] doubles the number of arguments taken from the stack.
    /// ex: [∩∩(□+2) 1 @a 2_3 5]
    /// ex: [∩∩∩± 1 ¯2 0 42 ¯5 6 7 8 99]
    ///
    /// If you have 3 values on the stack, `a`, `b`, and `c`, but you want to call the same function on `a c` and `b c`, you can combine [both] with [dip] and [over].
    /// ex: ∩+⊙,2 5 10
    (2[1], Both, Stack, ("both", '∩')),
    /// Call two functions on the same values
    ///
    /// [fork] is one of the most important functions for working with the stack.
    /// See the [Advanced Stack Manipulation Tutorial](/docs/advancedstack) for a more complete understanding as to why.
    ///
    /// ex: ⊃⇌⊝ 1_2_2_3
    /// [fork] can be chained to apply more functions to the arguments. `n` functions require the chaining of `subtract``1n` [fork].
    /// ex: [⊃⊃⊃+-×÷ 5 8]
    /// If the functions take different numbers of arguments, then the number of arguments is the maximum. Functions that take fewer than the maximum will work on the top values.
    /// ex: [⊃+¯ 3 5]
    ([2], Fork, Planet, ("fork", '⊃')),
    /// Call two functions on two distinct sets of values
    ///
    /// ex: ⊓⇌⊝ 1_2_3 [1 4 2 4 2]
    /// Each function will always be called on its own set of values.
    /// ex: ⊓+× 1 2 3 4
    /// The functions' signatures need not be the same.
    /// ex: ⊓+(++) 1 2 3 4 5
    /// [bracket] can be chained to apply additional functions to arguments deeper on the stack.
    /// ex: ⊓⊓⇌(↻1)△ 1_2_3 4_5_6 7_8_9
    /// ex: [⊓⊓⊓+-×÷ 10 20 5 8 3 7 2 5]
    ([2], Bracket, Planet, ("bracket", '⊓')),
    /// Apply a function under another
    ///
    /// This is a more powerful version of [invert].
    /// Conceptually, [under] transforms a value, modifies it, then reverses the transformation.
    ///
    /// [under] takes 2 functions `f` and `g` and another argument `x`.
    /// It applies `f` to `x`, then applies `g` to the result.
    /// It then applies the inverse of `f` to the result of `g`.
    ///
    /// Any function that can be [invert]ed can be used with [under].
    /// Some functions that can't be [invert]ed can still be used with [under].
    ///
    /// Here, we [negate] 5, [subtract] 2, then [negate] again.
    /// ex: ⍜¯(-2) 5
    /// You can use [under] with [round] to round to a specific number of decimal places.
    /// ex: ⍜'×1e3⁅ π
    ///
    /// The above examples involve an *arithmetic* under. That is, [invert]`f` is well-definined independent of [under]'s concept of "undoing".
    /// The remaining examples below involve `f`s which cannot be normally [invert]ed, but which are valid as functions to use with [under].
    ///
    /// [under][deshape] will [reshape] the array after `g` finishes.
    /// ex: ⍜♭⇌ .↯3_4⇡12
    /// If you want to insert a value somewhere in the middle of an array, you can use [under], [rotate], and [join].
    /// ex: ⍜'↻3'⊂π 1_2_3_4_5
    /// You can use [under][first] to apply a function to the first row of an array.
    /// ex: ⍜⊢'×10 1_2_3_4_5
    /// If you need to work on more of the array's rows, can use [under] with [take] or [drop].
    /// ex: ⍜'↙3'×10 1_2_3_4_5
    /// ex: ⍜'↘3'×10 1_2_3_4_5
    /// You can chain [under]-compatible functions.
    /// ex: ⍜(↙2↘1)'×10 1_2_3_4_5
    /// [pick] and [select] also work.
    /// ex: ⍜⊡'×10 2_1 ↯3_3⇡9
    /// ex: ⍜⊏'×10 1_3 1_2_3_4_5
    /// Although, [under][select] only works if the indices are unique.
    /// ex! ⍜⊏'×10 1_3_3 1_2_3_4_5
    /// [under][keep] works as long as the counts list is boolean.
    /// ex: ⍜▽'×10 =0◿3.⇡10
    ///
    /// If `g` takes more than 1 argument, keep in mind that `f` will be called on the stack as it is when the full under expression begins.
    /// This means you may have to flip the arguments to `g`.
    /// Consider this equivalence:
    /// ex: ⍜(↙2)(÷∶)  [1 2 3 4 5] 10
    ///   : ⍜(↙2)(÷10) [1 2 3 4 5]
    ///
    /// [under][both] works, and whether [both] is applied when undoing depends on the signature of `g`.
    /// For example, this hypotenuse function does not use [both] when undoing because its `g` (`add`) returns a single value.
    /// ex: ⍜∩(×.)+ 3 4
    /// However, this function whose `g` returns *2* values *does* use [both] when undoing, in this case re-[box]ing the outputs.
    /// ex: ⍜∩⊔(⊂⊢,) □[1 2 3] □[4 5 6 7 8]
    ([2], Under, OtherModifier, ("under", '⍜')),
    /// Set the fill value for a function
    ///
    /// By default, some operations require that arrays' [shape]s are in some way compatible.
    /// [fill] allows you to specify a value that will be used to extend the shape of one or both of the operands to make an operation succeed.
    /// The function is modified to take a fill value which will be used to fill in shapes.
    ///
    /// [fill] allows you to set default values for [take].
    /// ex: ⬚0↙ 7 [8 3 9 2 1]
    /// ex: ⬚π↙ ¯6 [1 2 3]
    /// ex: ⬚42↙ 4 [1_2_3 4_5_6]
    ///
    /// Using [fill] with [couple] will fill both arrays until their shapes match.
    /// ex: ⬚0⊟ 1 2_3
    /// ex: ⬚0⊟ 1_2 3_4_5_6
    /// ex: ⬚0⊟ 1_2_3 [4_5 6_7]
    ///
    /// Using [fill] with [join] will fill both arrays until the [join] makes sense.
    /// ex: ⬚0⊂ 1 [2_3_4 5_6_7]
    /// ex: ⬚0⊂ [1_2 3_4] 5_6_7
    ///
    /// Because array construction is implemented in terms of [couple] and [join], [fill] can be used when building arrays.
    /// ex: ⬚0[1 2_3 4_5_6]
    ///
    /// [fill] also works with pervasive operations where the shapes don't match.
    /// ex: ⬚0+ 1_2_3 10_9_8_7_6_5
    ///
    /// Many functions, like [scan] and [partition], implicitly build arrays and require compatible shapes.
    /// [fill] can be used with them as well. In some cases, this prevents the need to use [box].
    /// ex: ⬚0\⊂ 1_2_3_4_5
    /// ex: ⬚@ ⊜∘≠@ . "No □ needed!"
    ///
    /// [fill] will prevent [pick] and [select] from throwing an error if an index is out of bounds.
    /// ex: ⬚∞⊏ 3_7_0 [8 3 9 2 0]
    ///
    /// [fill] allows the list of counts for [keep] to be shorter than the kept array.
    /// This is especially useful when used with functions like [windows] or [find] which make an array shorter than their input.
    /// ex: ⬚0▽ ≡/>◫2. [1 8 0 2 7 2 3]
    ///
    /// [fill][reshape] fills in the shape with the fill element instead of cycling the data.
    /// ex:   ↯ 3_5 ⇡9
    ///   : ⬚0↯ 3_5 ⇡9
    ///
    /// To [fill] with a value that is on the stack, use [identity].
    /// ex: F = ⬚∘+
    ///   : F 100 [1 2 3 4] [5 6]
    ([2], Fill, OtherModifier, ("fill", '⬚')),
    /// Apply a function at a different array depth
    ///
    /// Expects a rank to operate on, a function, and an array.
    /// The rank supplied indicates the desired rank of the operand.
    /// The array will be split into arrays of that rank, and the function will be applied to each of those arrays.
    ///
    /// Ranks may be specified using [Ocean Notation](/docs/advancedarray#ocean-notation), but this page uses numbers and lists only.
    ///
    /// `level``0` is equivalent to [each], applying the function to each element of the array.
    /// `level``¯1` is equivalent to [rows], applying the function to each row of the array's major axis.
    /// `level``1` applies the function to each row of the array's last axis.
    /// `level``infinity` calls the function on the array without splitting it.
    ///
    /// One nice way to see what this means is to test it using [reverse].
    /// For each of these examples, pay attention to the number passed to [level] and which elements change position.
    /// ex: ↯2_2_3 ⇡12
    /// ex: ⇌ ↯2_2_3 ⇡12 # Reverse as normal
    /// ex: ≑0⇌ ↯2_2_3 ⇡12 # Reverse each element (does nothing)
    /// ex: ≑¯1⇌ ↯2_2_3 ⇡12 # Reverse each row
    /// ex: ≑¯2⇌ ↯2_2_3 ⇡12 # Reverse each row of each row
    /// ex: ≑1⇌ ↯2_2_3 ⇡12 # Reverse each last axis row
    ///
    /// [level] can operate on multiple arrays at once if passed a list of ranks.
    /// While `level``¯1` is equivelent to [rows] called with a single array, `level``[¯1 ¯1]` is equivalent to [rows] called with two arrays, and so on.
    /// ex: a ← ↯3_3   ⇡9
    ///   : b ← ↯3_3+10⇡9
    ///   :      ≡⊂ a b
    ///   : ≑¯1_¯1⊂ a b
    ///
    /// One way to think of the number(s) passed to [level] is as the rank of the array that the function will be applied to.
    /// `level``1` will always apply to rank `1` arrays, no matter how many dimensions the original array has.
    /// ex: ≑[1 1]⊂ ↯3_3⇡9 10_11_12 # Join two rank 1 arrays
    /// ex: ≑[1 0]⊂ ↯3_3⇡9 10_11_12 # Join rank 1 arrays with scalars
    ///
    /// [each] is equivalent to `level``0` or `level``[0 0 …]`.
    /// [rows] is equivalent to `level``¯1` or `level``[¯1 ¯1 …]`.
    /// [distribute] is equivalent to `level``[``infinity``¯1]` or `level``[``infinity``…``infinity``¯1]`.
    /// [tribute] is equivalent to `level``[¯1``infinity``]` or `level``[¯1``infinity``…``infinity``]`.
    ([2], Level, IteratingModifier, ("level", '≑')),
    /// Apply a function to aggregate at different array depths
    ///
    /// Expect a list of ranks to operate on, a function, and some arrays.
    /// The ranks supplied indicate the desired ranks of the arguments to the function.
    /// The arrays will be split into arrays of those ranks, and the function will be applied to tuples of those arrays.
    /// The function must take as many arguments as there are ranks.
    ///
    /// Ranks may be specified using [Ocean Notation](/docs/advancedarray#ocean-notation), but this page uses number lists only.
    ///
    /// A simple example is summing with an initial value.
    /// Here, we specify rank `0` for the first array. `10` is rank `0`, so it is used as an accumulator.
    /// We specify rank `0` for the second array. `[1 2 3]` is rank `1`, so it's elements will be iterated over.
    /// ex: ∧0_0+ 10 [1 2 3]
    /// Let's use three array arguments. The first is a matrix. The second is a list where each element will be used to [rotate] the corresponding row in the matrix. The third is an accumulator to which each [rotate]d row will be [add]ed.
    /// ex: ∧1_0_0(+↻∶) ↯3_4⇡12 [1 2 3] 10
    ///
    /// Negative ranks indicate that many ranks *less* than the associated array's rank.
    /// [infinity] ranks indicate exactly the associated array's rank.
    /// To [fold] all the top-level rows an array, use rank `¯1`.
    /// ex: ∧∞_¯1(⊂⊙⍉) [π π π] ↯2_3_4⇡24
    ///
    /// [fold] is good when you need to keep track of values that change in some way.
    /// For example, if you had a list of accelerations at different time steps and you wanted to find the final velocity and position of some object, you would simply mark the position and velocity vectors as accumulators.
    /// ex: [0_1 1_1 1_0 ¯1_¯1]
    ///   : ∧∞_∞_1(+⊙(.+)) 0_0 0_0
    /// However, for this particular example, it is probably better to use [scan].
    /// ex: /+∶⊢⇌.\+[0_1 1_1 1_0 ¯1_¯1]
    ([2], Fold, AggregatingModifier, ("fold", '∧')),
    /// Apply a function to combinations at array depths
    ///
    /// Expect a list of ranks to operate on, a function, and some arrays.
    /// The ranks supplied indicate the desired ranks of the arguments to the function.
    /// The arrays will be split into arrays of those ranks, and the function will be applied to combinations of those arrays.
    /// The function must take as many arguments as there are ranks.
    ///
    /// Ranks may be specified using [Ocean Notation](/docs/advancedarray#ocean-notation), but this page uses number lists only.
    ///
    /// ex: ◳0_1(+×10) ,, ↯2_2⇡4 ↯2_4⇡8
    /// ex: ◳∞_¯1_¯1(▽⊙⊂) 3 [1 2 3] [4 5 6]
    ///
    /// [table] is equivalent to `combinate``[0 0]`.
    /// [cross] is equivalent to `combinate``[¯1 ¯1]`.
    ([2], Combinate, IteratingModifier, ("combinate", '◳')),
    /// `join` `infinity` to an array
    ///
    /// See the [Ocean Notation tutorial](/docs/advancedarray#ocean-notation) to understand what this is for.
    /// ex: ⋄5
    /// ex: ⋄[1_2_3 4_5_6]
    /// *Rocks line the shores of the ocean, their immovable forms jutting from the surface.*
    (1, Rock, Ocean, ("rock", '⋄')),
    /// `join` `¯1` to an array
    ///
    /// See the [Ocean Notation tutorial](/docs/advancedarray#ocean-notation) to understand what this is for.
    /// ex: ~5
    /// ex: ~[1_2_3 4_5_6]
    /// ex: ≑~/+[[1_2 3_4] [5_6 7_8]]
    /// *The light of the sun reflects reflects off the rippling surface of the ocean.*
    (1, Surface, Ocean, ("surface", '~')),
    /// `join` `2` to an array
    ///
    /// See the [Ocean Notation tutorial](/docs/advancedarray#ocean-notation) to understand what this is for.
    /// ex: ≊5
    /// ex: ≊[1_2_3 4_5_6]
    /// ex: ≑≊/+[[1_2 3_4] [5_6 7_8]]
    /// *In the deep ocean live many wonderous creatures.*
    (1, Deep, Ocean, ("deep", '≊')),
    /// `join` `1` to an array
    ///
    /// See the [Ocean Notation tutorial](/docs/advancedarray#ocean-notation) to understand what this is for.
    /// ex: ≃5
    /// ex: ≃[1_2_3 4_5_6]
    /// ex: ≑≃/+[[1_2 3_4] [5_6 7_8]]
    /// *In the abyss, where no light reaches, life is restricted to simpler forms.*
    (1, Abyss, Ocean, ("abyss", '≃')),
    /// `join` `0` to an array
    ///
    /// See the [Ocean Notation tutorial](/docs/advancedarray#ocean-notation) to understand what this is for.
    /// ex: ∸5
    /// ex: ∸[1_2_3 4_5_6]
    /// ex: ≑∸≃⊂.[1_2_3 4_5_6]
    /// *At the seabed, countless small scavengers feed on the detritus of the ocean above.*
    (1, Seabed, Ocean, ("seabed", '∸')),
    /// Syntactically bind two functions
    ///
    /// This modifier mostly exists for syntactic convenience.
    /// It lets you change any function with 2 terms into a modifer call, saving a single character.
    /// ex: ≡(⇌⊢)↯3_3⇡9
    ///   : ≡'⇌⊢↯3_3⇡9
    /// This especially nice when used with modifiers that take 2 functions, like [under], where you can save up to 2 characters!
    /// ex: ⍜(↻3)(⊂π) [1 2 3 4 5]
    ///   : ⍜'↻3'⊂π [1 2 3 4 5]
    /// Don't chain [bind].
    /// ex! ≡''⊂⇌. ↯2_3⇡6
    ([2], Bind, OtherModifier, ("bind", '\'')),
    /// Call one of two functions based on a condition
    ///
    /// If the condition is `1`, then the first function is called.
    /// If the condition is `0`, then the second function is called.
    /// Any other values are not allowed.
    /// ex: ?+- 1 3 5
    /// ex: ?+- 0 3 5
    /// ex: Abs ← ?¯∘ <0.
    ///   : Abs 2
    ///   : Abs ¯5
    ///
    /// If the functions have different but compatible signatures - that is, the difference between their arguments and outputs is the same - then [if] will still have a well-defined signature.
    /// ex: f ← ?∘(.+)
    ///   : f 0 2 3
    ///   : f 1 2 3
    /// If functions have incompatible signatures but the same number of outputs, then [if] works similarly to [fork]. The function that takes fewer arguments will use the arguments higher on the stack.
    /// ex: ?+¯ 1 3 5
    /// ex: ?+¯ 0 3 5
    ///
    /// [if] can be chained to check more than one condition.
    /// Make sure to use [pop] or [gap] to git rid of excess conditions if the number of branches is not a [power] of `2`.
    /// ex: f ← ??+×⋅-
    ///   : f ← ?(?+×)(-;) # Equivalent
    ///   : xs ← (3 5)
    ///   : f 1 1 xs
    ///   : f 1 0 xs
    ///   : f 0 1 xs
    ///   : f 0 0 xs
    ///
    /// The condition can be a list of booleans. In this case, the maximum of the function's arguments *must* be 2.
    /// Which function to be called is determined on a row-wise basis.
    /// ex: ?∘¯ .=0◿2 [1 2 3 4]
    /// ex: ?∘⋅∘ [1 0 0 1] [1 2 3 4] [π π π π]
    ([2], If, Control, ("if", '?')),
    /// Call a function and catch errors
    ///
    /// If the first function errors, the second function is called with the original arguments and the error value below.
    ///
    /// Normal runtime errors become strings.
    /// ex: ⍣(+1 2)$"Error: _"
    /// ex: ⍣(+@a @b)$"Error: _"
    /// Errors thrown with [assert] can be any value.
    /// ex: ⍣(⍤5 1 3)(×5)
    /// ex: ⍣(⍤5 0 3)(×5)
    /// If the first function has the signature `|n.r`, then the second function must have the signature `|(n+1).r`. The additional value is the error.
    /// If you don't care about the input values, you can simply [pop] them.
    /// ex: ⍣parse; "dog"
    /// ex: ⍣parse(0;;) "dog"
    /// ex: ⍣parse(0;;) "5"
    /// [gap] can often look nicer.
    /// ex: ⍣parse⋅⋅0 "dog"
    /// ex: ⍣parse⋅⋅0 "5"
    ([2], Try, Control, ("try", '⍣')),
    /// Throw an error if a condition is not met
    ///
    /// Expects a message and a test value.
    /// If the test value is anything but `1`, then the message will be thrown as an error.
    ///
    /// ex! ⍤"Oh no!" "any array"
    /// ex: ⍤"Oh no!" 1
    /// ex! ⍤"Oh no!" 0
    ///
    /// Use [duplicate] if you do not care about the message.
    /// ex: ⍤. =6 6
    /// ex! ⍤. =8 9
    ///
    /// Errors thrown by [assert] can be caught with [try].
    (2(0), Assert, Control, ("assert", '⍤')),
    /// Spawn a thread
    ///
    /// Expects a function.
    /// In the native interpreter, the function is called in a new OS thread.
    /// In the web editor, the function is called and blocks until it returns.
    /// A handle that can be passed to [wait] is pushed to the stack. Handles are just numbers.
    /// [wait] consumes the handle and appends the thread's stack to the current stack.
    /// ex:  spawn⇡ 10
    ///   : waitspawn⇡ 10
    /// ex:  spawn(+10+) 1 2
    ///   : waitspawn(+10+) 1 2
    ///
    /// You can use [each] to spawn a thread for each element of an array.
    /// ex: ∵spawn(/+⇡×.) ⇡10
    ///
    /// [wait] will call [each] implicitly.
    /// ex: ↯3_3⇡9
    ///   : wait≡spawn/+.
    ([1], Spawn, OtherModifier, "spawn"),
    /// Wait for a thread to finish and push its results to the stack
    ///
    /// The argument must be a handle returned by [spawn].
    /// ex: wait spawn(/+⇡) 10
    ///
    /// If the handle has already been [wait]ed on, then an error is thrown.
    /// ex! h ← spawn(/+⇡) 10
    ///   : wait h
    ///   : wait h
    ///
    /// [wait] is pervasive and will call [each] implicitly.
    /// ex: ↯3_3⇡9
    ///   : wait≡spawn/+.
    (1, Wait, Misc, ("wait")),
    /// Break out of a loop
    ///
    /// Expects a non-negative integer. This integer is how many loops will be broken out of.
    /// Loops that can be broken out of are [reduce], [fold], [scan], [each], [rows], and [repeat].
    ///
    /// ex: /(⎋>10.+) ⇡8  # Break when the sum exceeds 10
    /// ex: ⍥(⎋>100.×2)∞ 1  # Break when the product exceeds 100
    (1(0), Break, Control, ("break", '⎋')),
    /// Generate a random number between 0 and 1
    ///
    /// If you need a seeded random number, use [gen].
    ///
    /// ex: ⚂
    /// ex: [⚂⚂⚂]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: ⌊×10 [⍥⚂5]
    (0, Rand, Misc, ("random", '⚂')),
    /// Generate a random number between 0 and 1 from a seed, as well as the next seed
    ///
    /// If you don't care about a seed, you can use [random].
    ///
    /// The same seed will always produce the same random number.
    /// ex: [;gen gen gen 0]
    /// ex: [;⍥gen3 0]
    /// ex: [;⍥gen3 1]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: ⌊*10[;⍥gen5 0]
    (1(2), Gen, Misc, "gen"),
    /// Randomly reorder the rows of an array with a seed
    ///
    /// ex: deal0 [1 2 3 4 5]
    /// ex: deal5 [1_2 3_4 5_6 7_8]
    /// If you don't care about a seed, just seed with [random].
    /// ex: deal⚂ [1 2 3 4 5]
    /// ex: deal⚂ [1_2 3_4 5_6 7_8]
    (2, Deal, Misc, "deal"),
    /// Parse a string as a number
    ///
    /// ex: parse "17"
    /// ex: parse "3.1415926535897932"
    /// ex! parse "dog"
    (1, Parse, Misc, "parse"),
    /// Match a regex pattern
    ///
    /// Returns an list of [box]ed strings, with one string per matching group
    /// ex: regex "h[io]" "hihaho"
    /// ex: regex "hi" "dog"
    /// ex: regex "[a-z]+" "hello world"
    /// Escaped regex characters must be double-escaped.
    /// ex: regex "\\d+" "123"
    /// ex: P ← $"(\\d{_})"
    ///   : regex $"_-_-_"P3P3P4 "123-456-7890"
    ///
    /// Uiua uses the [Rust regex crate](https://docs.rs/regex/latest/regex/) internally.
    (2, Regex, Misc, "regex"),
    /// Convert a string to UTF-8 bytes
    ///
    /// ex: utf "hello!"
    /// ex: utf "❤️"
    /// You can use [invert] to convert UTF-8 bytes back to a string.
    /// ex: ⍘utf [226 156 168 32 119 111 119 33]
    ///
    /// [utf] is different from just [add]ing or [subtracting] `@\0`.
    /// Character math can only convert to and from UTF-32.
    /// ex: -@\0 "👩🏽‍👩🏻‍👦🏻‍👧🏽"
    /// ex: utf "👩🏽‍👩🏻‍👦🏻‍👧🏽"
    (1, Utf, Misc, "utf"),
    /// Convert UTF-8 bytes to a string
    (1, InvUtf, Misc),
    /// Generate a unique tag
    ///
    /// Tags are just numbers and are unique across multiple threads, but not across multiple runs.
    /// ex: [⍥tag5]
    ///   : [⍥tag5]
    (0, Tag, Misc, "tag"),
    /// Check the type of an array
    ///
    /// `0` indicates a number array.
    /// `1` indicates a character array.
    /// `2` indicates a box array.
    /// ex: type 5
    /// ex: type "hello"
    /// ex: type □[5 6]
    /// ex: ∵ type   {10 "dog" [1 2 3]}
    ///   : ∵(type⊔) {10 "dog" [1 2 3]}
    (1, Type, Misc, "type"),
    /// Get the current time in seconds
    ///
    /// ex: now
    /// [under][now] can be used to time a function.
    /// ex: ⍜now(5&sl1)
    (0, Now, Misc, "now"),
    /// The number of radians in a quarter circle
    ///
    /// Equivalent to `divide``2``pi` or `divide``4``tau`
    /// ex: [η ÷2π ÷4τ]
    (0, Eta, Constant, ("eta", 'η')),
    /// The ratio of a circle's circumference to its diameter
    ///
    /// Equivalent to `multiply``2``eta` or `divide``2``tau`
    /// ex: [×2η π ÷2τ]
    (0, Pi, Constant, ("pi", 'π')),
    /// The ratio of a circle's circumference to its radius
    ///
    /// Equivalent to `multiply``4``eta` or `multiply``2``pi`
    /// ex: [×4η ×2π τ]
    (0, Tau, Constant, ("tau", 'τ')),
    /// The biggest number
    (0, Infinity, Constant, ("infinity", '∞')),
    /// Debug print the top value on the stack without popping it
    ///
    /// ex: ⸮[1 2 3]
    /// This is useful when you want to inspect an intermediate value.
    /// For example, let's say you are trying to find all the numbers in some range:
    /// ex: [1 5 2 9 11 0 7 12 8 3]
    ///   : ▽×≥5∶≤10..
    /// `greater or equal` and `less or equal` each create a partial mask.
    /// To see them, use [trace].
    /// ex: [1 5 2 9 11 0 7 12 8 3]
    ///   : ▽×⸮≥5∶⸮≤10..
    (1, Trace, Stack, ("trace", '⸮')),
    /// The inverse of trace
    (1, InvTrace, Stack),
    /// Debug print all the values currently on stack without popping them
    ///
    /// The function is used to preprocess the values before printing.
    /// If you just want to print the values, use [dump][identity].
    /// ex: dump∘ 1 2 3
    /// This is useful when you want to inspect the current ordering of the stack.
    /// For example, if you are juggling some values on the stack, you can use [dump] to inspect the stack afterwards:
    /// ex: 1 2 3
    ///   : ,,⊙.:
    ///   : dump∘
    ///   : +×-×+
    /// [dump][shape] is useful if your raw array data isn't worth looking at, but the shapes are.
    /// ex: 2_3_10 17 ↯3_4⇡12
    ///   : dump△
    ///   : ++
    /// ex: ↯¯1_5 ⇡30
    ///   : ⍉.⊃≡(⊟.)(⊞+.).
    ///   : dump△
    ///   : +++∩∩⧻
    /// Error encountered within [dump]'s function are caught and dumped as strings.
    /// ex: 1_2_3 4 5_6_7
    ///   : dump⊢
    (0(0)[1], Dump, Stack, "dump"),
);
