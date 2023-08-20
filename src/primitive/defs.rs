use super::*;

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
            pub fn all() -> impl Iterator<Item = Self> + Clone {
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
            pub fn modifier_args(&self) -> Option<u8> {
                match self {
                    $($($(Primitive::$variant => Some($mod_args),)?)?)*
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
    /// ex: [. 1 2 3]
    ///
    /// [duplicate] is often used in examples to show both the input and output of a function.
    /// ex: √.144
    /// ex: .[1 2 3]
    ///   : +1⇌
    ///
    /// [duplicate] can be used to make a monadic left-hook, such as in this palindrome checker:
    /// ex: ≅⇌. "friend"
    /// ex: ≅⇌. "racecar"
    /// Another commonly hooked function is [replicate].
    /// ex: ‡=0◿3. [1 4 2 3 9 1 0 6 2 6 3]
    (1(2), Dup, Stack, ("duplicate", '.')),
    /// Duplicate the second-to-top value to the top of the stack
    ///
    /// ex: [, 1 2 3]
    (2(3), Over, Stack, ("over", ',')),
    /// Swap the top two values on the stack
    ///
    /// ex: [∶ 1 2 3 4]
    ///
    /// When combined with [duplicate], you can apply two different functions to the same value.
    /// If you have two functions `f` and `g`, the pattern `f``flip``g``duplicate` will call both functions on the top value.
    /// This is a very common pattern.
    /// For example, maybe you want to find all the uppercase letters in a string.
    /// ex: $ Characters On uppercase OnLy
    ///   : ‡×≥@A∶≤@Z..
    /// Or maybe you want to calculate the averge of a list of numbers.
    /// Here, we get the [length] and the `reduce``add``sum` of the list, then [divide] them.
    /// ex: ÷⧻∶/+. 1_8_2_5
    (2(2), Flip, Stack, ("flip", AsciiToken::Colon, '∶')),
    /// Pop the top value off the stack
    (1(0), Pop, Stack, ("pop", ';')),
    /// Do nothing
    ///
    /// While this may seem useless, one way to use it is to pass it to [reduce], which will put all of an array's values on the stack.
    /// ex: /· [1 2 3]
    ///
    /// The formatter converts an empty `()` function into `noop`.
    /// ex: () # Try running to format
    (0(0), Noop, Stack, ("noop", '·')),
    /// Move a value from the stack to the antistack
    ///
    /// Formats from `^`.
    ///
    /// Get the value back with [load].
    /// ex: [↓ 1 2 3 ↑4 5]
    ///
    /// If you want to temporarily get a value on the top of the stack out of the way while you work on values below it, you can use [under][save].
    /// ex: [⍜↑+ 1 2 3 4]
    /// You can use as many `save`s as values you want to move out of the way.
    /// ex: [⍜(↑↑↑)+ 1 2 3 4 5]
    /// For deep stack manipulation, use this with [repeat].
    /// ex: [⍜(⍥↑8)+ 1 2 3 4 5 6 7 8 9 10]
    (1(0), Save, Stack, ("save", AsciiToken::Caret, '↑')),
    /// Move a value from the antistack to the stack
    ///
    /// Move values to the antistack in the first place with [save].
    /// ex: [↓ 1 2 3 ↑4 5]
    ///
    /// [load] is equivalent to (and formats from) [anti][noop].
    /// ex: [~() 2 3 ↑4 5]
    (0, Load, Stack, ("load", '↓')),
    /// Call a function on the antistack and move the result to the stack
    ///
    /// [anti][noop] is equivalent to (and formats to) [load].
    /// ex: [~() 2 3 ↑4 5]
    ///
    /// Combining [anti] with [duplicate] and [over] gives you a way to randomly sample up to two values from the antistack.
    /// ex: [↓↓~,~.~,~,~.] ↑↑ 0 1
    ([1], Anti, Stack, ("anti", '~')),
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
    /// ex: ¯ 1
    /// ex: ¯ ¯3
    /// ex: ¯ [1 2 ¯3]
    (
        1,
        Neg,
        MonadicPervasive,
        ("negate", AsciiToken::Backtick, '¯')
    ),
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
    /// ex: ÷○+η∶○. 0
    (1, Sin, MonadicPervasive, ("sine", '○')),
    /// The cosine of a number
    (1, Cos, MonadicPervasive),
    /// The arcsine of a number
    (1, Asin, MonadicPervasive),
    /// The arccosine of a number
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
    ///
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
    ///
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
    ///
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
    ///
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
    ///
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
    (2, Mul, DyadicPervasive, ("multiply", AsciiToken::Star, '×')),
    /// Divide values
    ///
    /// Formats from `%`.
    ///
    /// The second value is divided by the first.
    /// This is so you can think of `÷``x` as a single unit.
    ///
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
    ///
    /// ex: ◿10 27
    /// ex: ◿5 [3 7 14]
    /// ex: ◿ [3 4 5] [10 10 10]
    (2, Mod, DyadicPervasive, ("modulus", '◿')),
    /// Raise a value to a power
    ///
    /// The second value is raised to the power of the first.
    /// This is so you can think of `ⁿ``x` as a single unit.
    ///
    /// ex: ⁿ2 3
    /// ex: ⁿ2 [1 2 3]
    /// ex: ⁿ [1 2 3] [4 5 6]
    (2, Pow, DyadicPervasive, ("power", 'ⁿ')),
    /// The based logarithm of a number
    ///
    /// The first value is the base, and the second value is the power.
    ///
    /// ex: ₙ2 8
    /// ex: ₙ2 [8 16 32]
    /// ex: ₙ [2 3 4] [16 27 1024]
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
    /// The number of rows in an array
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
    /// When creating ranges with upper bounds that are [rank]`1`, [pick]ing the generated range array from an array with the [shape] of the input will yield that array.
    /// ex:     [1_2_3 4_5_6]
    ///   :    △[1_2_3 4_5_6]
    ///   :   ⇡△[1_2_3 4_5_6]
    ///   : ⊡⇡△.[1_2_3 4_5_6]
    (1, Range, MonadicArray, ("range", '⇡')),
    /// The first row of an array
    ///
    /// ex: ⊢1_2_3
    /// ex: ⊢[1_2 3_4 5_6]
    /// ex! ⊢[]
    /// ex! ⊢1
    (1, First, MonadicArray, ("first", '⊢')),
    /// The last element of an array
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
    /// ex: ♭[1_2 3_4 5_6]
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
    /// Grade the rows of an array
    ///
    /// ex: ⌂6_2_7_0_¯1_5
    ///
    /// Using the grading as a selector in [select] yields the sorted array.
    /// ex: ⊏⌂.6_2_7_0_¯1_5
    ///
    /// See also: [sort]
    (1, Grade, MonadicArray, ("grade", '⌂')),
    /// Assign a unique index to each unique element in an array
    ///
    /// ex: ⊛7_7_8_0_1_2_0
    /// ex: ⊛"Hello, World!"
    ///
    /// When combined with [group], you can do things like counting the number of occurrences of each character in a string.
    /// ex: $ Count the characters is this string
    ///   : ⊕($"_ _"⊢∶⧻.) ⊛.⊏⌂.
    (1, Classify, MonadicArray, ("classify", '⊛')),
    /// Remove duplicate elements from an array
    ///
    /// ex: ⊝7_7_8_0_1_2_0
    (1, Deduplicate, MonadicArray, ("deduplicate", '⊝')),
    /// Turn an array into a constant function
    ///
    /// This is Uiua's primary way to create nested or mixed-type arrays.
    /// Normally, arrays can only be created if their rows have the same shape and type.
    /// [fill] can help you with the shape part, but it is not always wanted, and it can't help with the type part.
    /// ex! [@a 3 7_8_9]
    /// [constant] turns any array into a function that pushes that array onto the stack.
    /// These functions are just like any other, so they can be put in arrays themselves.
    /// ex: [□@a □3 □7_8_9]
    /// Use [call] to get the values back out.
    /// ex: !□1_2_3
    /// [reduce][call] will unpack an array of constant functions onto the stack.
    /// ex: /![□@a □3 □7_8_9]
    ///
    /// You would not normally construct arrays like the one above.
    /// The more important use case of [constant] is for jagged or nested data.
    /// If you want to collect unevenly-sized groups from [partition] or [group], without [fill]ing, you must use [constant].
    /// ex: $ Words of different lengths
    ///   : ⊜□≠@ .
    ///
    /// If you want to manipulate the arrays inside a constant function array without removing them, you can use [each][under][call].
    /// ex: $ Reverse these words
    ///   : ⊜□≠@ .
    ///   : ∵⍜!⇌.
    /// This works because [call] [invert]ed is [constant]. For each element, it [call]s the function to get the array out, [reverse]s it, then [constant]s it again.
    (1, Constant, MonadicArray, ("constant", '□')),
    /// Append two arrays end-to-end
    ///
    /// For scalars, it is equivalent to [couple].
    /// ex: ⊂ 1 2
    ///   : ⊟ 1 2
    ///
    /// If the arrays have the same [rank], it will append the second array to the first.
    /// ex: ⊂ [1 2] [3 4]
    /// ex: ⊂ [1_2 3_4] [5_6 7_8]
    ///
    /// If the arrays have a [rank] difference of 1, then the array with the smaller [rank] will be prepended or appended to the other as a row.
    /// ex: ⊂ 1 [2 3]
    /// ex: ⊂ [1 2] 3
    /// ex: ⊂ 1_2 [3_4 5_6]
    /// ex: ⊂ [1_2 3_4] 5_6
    ///
    /// By default, arrays that do not have equal [shape] suffixes cannot be joined.
    /// ex! ⊂ [1_2 3_4] [5_6_7 8_9_10]
    /// Use [fill] to make their shapes compatible.
    /// ex: ⍛0⊂ [1_2 3_4] [5_6_7 8_9_10]
    ///
    /// [join]'s glyphs is `⊂` because it kind of looks like a magnet pulling its two arguments together.
    (2, Join, DyadicArray, ("join", '⊂')),
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
    /// ex: ⍛∞⊟ [1 2 3] [4 5]
    ///
    /// [couple] is compatible with [under].
    /// ex: ⍜⊟'×2 3 5
    (2, Couple, DyadicArray, ("couple", '⊟')),
    /// Split an array into two arrays
    (1(2), Uncouple, MonadicArray),
    /// Check if two arrays are exactly the same
    ///
    /// ex: ≅ 1_2_3 [1 2 3]
    /// ex: ≅ 1_2_3 [1 2]
    (2, Match, DyadicArray, ("match", '≅')),
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
    /// For a scalar selector, [select] is equivalent to [pick].
    /// ex: ⊏ 2 [8 3 9 2 0]
    ///   : ⊡ 2 [8 3 9 2 0]
    /// For a [rank]`1` selector, [select] will pick multiple items from an array.
    /// ex: ⊏ 4_2 [8 3 9 2 0]
    /// ex: ⊏ 0_2_1_1 [1_2_3 4_5_6 7_8_9]
    /// If the selector's [rank] is `greater than``1`, then earch row of the selector will be selected seperately.
    /// ex: ⊏ [0_1 1_2 2_3] [2 3 5 7]
    /// ex: ⊏ [0_1 1_2 2_0] [1_2_3 4_5_6 7_8_9]
    (2, Select, DyadicArray, ("select", '⊏')),
    /// End step of under select
    (3, Unselect, Misc),
    /// Take the first n elements of an array
    ///
    /// This is the opposite of [drop].
    ///
    /// ex: ↙ 3 [8 3 9 2 0]
    /// ex: ↙ ¯3 [8 3 9 2 0]
    /// ex: ↙ 2 ↯3_3⇡9
    /// ex: ↙ ¯2 ↯3_3⇡9
    ///
    /// By default, taking more than the length of the array will throw an error.
    /// ex! ↙7 [8 3 9 2 0]
    /// If you would like to fill the excess length with some fill value, use [fill].
    /// ex: ⍛π↙7 [8 3 9 2 0]
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
    ///
    /// Dropping more than the length of the array will leave an empty array.
    /// ex: ↘ 7 [8 3 9 2 0]
    /// ex: ↘ ¯7 [8 3 9 2 0]
    /// ex: ↘ 5 ↯3_3⇡9
    /// ex: ↘ ¯5 ↯3_3⇡9
    (2, Drop, DyadicArray, ("drop", '↘')),
    /// End step of under drop
    (3, Undrop, Misc),
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
    /// The first array is the number of times to replicate each element of the second array.
    /// ex: ‡ [1 0 2 1 4] [8 3 9 2 0]
    ///
    /// This can be used as a filter.
    /// In this example, the input string is [duplicate]ed, and a mask is created from it using `greater or equal``@a`. Then, [replicate] uses the mask to filter the string.
    /// ex: ‡ ≥@a . "lOWERCASe onLY"
    (2, Replicate, DyadicArray, ("replicate", '‡')),
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
    /// With the help of [replicate], you can use [member] to get a set intersection.
    /// ex: ‡∊, "abracadabra" "that's really cool"
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
    /// If you expect one of the searched-for rows to be missing, you can [join] a default item to the end of the [select]ed-from array.
    /// ex: of ← .[1 2 3 4 5]
    ///   : in ← [2 3 5 7 11 13]
    ///   : .⊗of in
    ///   : ⊏∶⊂in∞
    ///
    /// [indexof] is closely related to [member].
    (2, IndexOf, DyadicArray, ("indexof", '⊗')),
    /// Apply a reducing function to an array
    ///
    /// For reducing with an initial value, see [fold].
    /// Unlike other modifiers, [reduce] and [fold] traverse the array from right to left.
    ///
    /// ex: /+ 1_2_3_4_5
    /// ex: /- 1_2_3_4_5
    ///
    /// If you want to see the intermediate values, you can use [scan], [flip], and [reverse].
    /// ex: /-     1_2_3_4_5
    ///   : \'-∶ ⇌ 1_2_3_4_5
    ///
    /// You can can fold with arbitrary functions.
    /// ex: /(×+1) 1_2_3_4_5
    ///
    /// [reduce] traverses the array backwards so that `reduce``noop` unloads all rows onto the stack with the first row on top.
    /// ex: /· 1_2_3
    /// ex: /· [1_2 3_4]
    ([1], Reduce, MonadicModifier, ("reduce", '/')),
    /// Apply a reducing function to an array with an initial value
    ///
    /// For reducing without an initial value, see [reduce].
    /// Unlike other modifiers, [fold] and [reduce] traverse the array from right to left.
    ///
    /// ex: ∧+ 10 1_2_3_4
    ([1], Fold, MonadicModifier, ("fold", '∧')),
    /// Reduce, but keep intermediate values
    ///
    /// ex: \+   1_2_3_4
    /// ex: \-   1_2_3_4
    /// ex: \'-∶ 1_2_3_4
    ([1], Scan, MonadicModifier, ("scan", '\\')),
    /// Apply a function to each element of an array or between elements of arrays
    ///
    /// This is the element-wise version of [rows].
    ///
    /// The number of arrays used depends on how many arguments the function takes.
    /// ex: ∵'⊟. 1_2_3_4
    /// ex: ∵⊂ 1_2_3 4_5_6
    /// ex: ∵⊂ 1_2 [4_5 6_7]
    ([1], Each, MonadicModifier, ("each", '∵')),
    /// Apply a function to each row of an array
    ///
    /// This is the row-wise version of [each].
    ///
    /// ex:  /+ [1_2_3 4_5_6 7_8_9]  # Sum each row with the next
    /// ex: ≡/+ [1_2_3 4_5_6 7_8_9]  # Sum the elements of each row
    ///
    /// The number of arrays used depends on how many arguments the function takes.
    /// ex: ≡⊂  1_2 [4_5 6_7]
    /// ex: ≡∧+ 1_2 [4_5 6_7]
    ///
    /// [rows] is equivalent to [level]`¯1` (or `level``[¯1 ¯1 ..]` for multiple arrays).
    /// ex: ⍚¯1/+ [1_2_3 4_5_6 7_8_9]
    /// ex:   ≡/+ [1_2_3 4_5_6 7_8_9]
    ([1], Rows, MonadicModifier, ("rows", '≡')),
    /// Apply a function to each row of an array and a fixed value
    ///
    /// ex: ⫫⊂ 1_2_3 4
    /// ex: ⫫⊂ 1_2_3 4_5_6
    ///
    /// One nice use of this is to [call] multiple functions on a single argument.
    /// ex: ⫫!√_¯_⌊_⌈_(×4) 6.25
    ///
    /// [distribute] is equivalent to [level]`[¯1``infinity``]`.
    /// ex:       ⫫⊂ 1_2_3 4_5_6
    ///   : ⍚[¯1 ∞]⊂ 1_2_3 4_5_6
    ([1], Distribute, DyadicModifier, ("distribute", '⫫')),
    /// Apply a function to each combination of elements of two arrays
    ///
    /// This is the element-wise version of [cross].
    ///
    /// ex: ⊞+ 1_2_3 4_5_6_7
    /// ex: ⊞⊂ 1_2 3_4
    ([1], Table, DyadicModifier, ("table", '⊞')),
    /// Apply a function to each combination of rows of two arrays
    ///
    /// This is the row-wise version of [table].
    ///
    /// ex: a ← .[1_2 3_4 5_6]
    ///   : b ← .[7_8 9_10]
    ///   : ⊠⊂ a b
    ([1], Cross, DyadicModifier, ("cross", '⊠')),
    /// Repeat a function a number of times
    ///
    /// ex: ⍥'+2 5 0
    /// ex: ⍥'⊂2 5 []
    ///
    /// One interesting use of `repeat` is to collect some number of stack values into an array.
    /// ex: ⍥⊂3 [] 1 2 3
    ///
    /// Repeating for [infinity] times will create an infinite loop.
    /// You can use [break] to break out of the loop.
    /// ex: ⍥(⎋>1000. ×2)∞ 1
    ([1], Repeat, OtherModifier, ("repeat", '⍥')),
    /// Group elements of an array into buckets by index
    ///
    /// Takes a function and two arrays.
    /// The arrays must be the same [length].
    /// The first array must be [rank]`1` and contain integers.
    /// Rows in the second array will be grouped into buckets by the indices in the first array.
    /// Keys `less than``0` will be omitted.
    /// The function then processes each group in order. The result depends on what the function is.
    /// If the function takes 0 or 1 arguments, then [group] behaves like [each].
    /// ex: ⊕· [0 2 2 1 0 1] [1 2 3 4 5 6]
    /// If the function takes 2 arguments, then [group] behaves like [reduce].
    /// ex: ⊕⊂ [0 2 2 1 0 1] [1 2 3 4 5 6]
    /// If the values returned by the function do not have the same [shape], concatenation will fail.
    /// It is common to use [constant] to encapsulate groups of different [shape]s.
    /// ex: ⊕□ [0 1 0 2 1 1] [1 2 3 4 5 6]
    ///
    /// If you wanted to get the length of each group, use [length].
    /// ex: ⊕⧻ [0 1 0 2 1 1] [1 2 3 4 5 6]
    ///
    /// When combined with [classify], you can do things like counting the number of occurrences of each character in a string.
    /// ex: $ Count the characters is this string
    ///   : ⊕($"_:_"⊢∶⧻.) ⊛.⊏⌂.
    ///
    /// [group] is closely related to [partition].
    ([1], Group, DyadicModifier, ("group", '⊕')),
    /// Group elements of an array into buckets by sequential keys
    ///
    /// Takes a function and two arrays.
    /// The arrays must be the same [length].
    /// The first array must be [rank]`1` and contain integers.
    /// Rows in the second array that line up with sequential keys in the first array will be grouped together.
    /// Keys `less or equal``0` will be omitted.
    /// The function then processes each group in order. The result depends on what the function is.
    /// If the function takes 0 or 1 arguments, then [partition] behaves like [each].
    /// ex: ⊜· [0 0 2 2 1 1 3 3] [1 2 3 4 5 6 7 8]
    /// If the function takes 2 arguments, then [partition] behaves like [reduce].
    /// ex: ⊜⊂ [0 0 2 2 1 1 3 3] [1 2 3 4 5 6 7 8]
    /// If the values returned by the function do not have the same [shape], concatenation will fail.
    /// It is common to use [constant] to encapsulate groups of different [shape]s.
    /// ex: ⊜□ [0 2 3 3 3 0 1 1] [1 2 3 4 5 6 7 8]
    ///
    /// If you wanted to get the length of each group, use [length].
    /// ex: ⊜⧻ [0 2 3 3 3 0 1 1] [1 2 3 4 5 6 7 8]
    ///
    /// This can be used to split an array by a delimiter.
    /// ex: ⊜□ ≠@ . $ Hey there friendo
    ///
    /// [partition] is closely related to [group].
    ([1], Partition, DyadicModifier, ("partition", '⊜')),
    /// Invert the behavior of a function
    ///
    /// Most functions are not invertible.
    ///
    /// ex: √2
    /// ex: ↶√2
    ([1], Invert, OtherModifier, ("invert", '↶')),
    /// Apply a function under another
    ///
    /// This is a more powerful version of [invert].
    ///
    /// [under] takes 2 functions `f` and `g` and another argument `x`.
    /// It applies `f` to `x`, then applies `g` to the result.
    /// It then applies the inverse of `f` to the result of `g`.
    ///
    /// Any function that can be [invert]ed can be used with [under].
    /// Some functions that can't be inverted can still be used with [under].
    ///
    /// Here, we negate 5, subtract 2, then negate again.
    /// ex: ⍜¯'-2 5
    ///
    /// If you want to insert a value somewhere in the middle of an array, you can use [under], [rotate], and [join].
    /// ex: ⍜'↻3'⊂π 1_2_3_4_5
    ///
    /// You can use [under] with [round] to round to a specific number of decimal places.
    /// ex: ⍜(×ⁿ4 10)⁅ π
    ///
    /// You can use [under] [take] to modify only part of an array.
    /// ex: ⍜'↙2'×10 1_2_3_4_5
    ([2], Under, OtherModifier, ("under", '⍜')),
    /// Set a fill context
    ///
    /// By default, some operations require that arrays have some compatible [shape].
    /// [fill] allows you to specify a value that will be used to extend the shape of one or both of the operands to make an operation succeed.
    /// The first argument is the fill value, and the second argument is a function in which the fill value will be used.
    ///
    /// [fill] allows you to set default values for [take].
    /// ex: ⍛0↙7 [8 3 9 2 1]
    /// ex: ⍛π↙¯6 [1 2 3]
    /// ex: ⍛42↙4 [1_2_3 4_5_6]
    ///
    /// Using [fill] with [couple] will fill both arrays until their shapes match.
    /// ex: ⍛0⊟ 1 2_3
    /// ex: ⍛0⊟ 1_2 3_4_5_6
    /// ex: ⍛0⊟ 1_2_3 [4_5 6_7]
    ///
    /// Using [fill] with [join] will fill both arrays until the [join] makes sense.
    /// ex: ⍛0⊂ 1 [2_3_4 5_6_7]
    /// ex: ⍛0⊂ [1_2 3_4] 5_6_7
    ///
    /// Because array construction is implemented in terms of [couple] and [join], [fill] can be used when building arrays.
    /// ex: ⍛0[1 2_3 4_5_6]
    ///
    /// [fill] also works with pervasive operations where the shapes don't match.
    /// ex: ⍛0+ 1_2_3 10_9_8_7_6_5
    ///
    /// Many functions, like [scan] and [partition], implicitly build arrays and require compatible shapes.
    /// [fill] can be used with them as well. In some cases, this prevents the need to use [constant].
    /// ex: ⍛0\⊂ 1_2_3_4_5
    /// ex: ⍛@ ⊜·≠@ . "No □ needed!"
    ///
    /// [fill] will prevent [pick] and [select] from throwing an error if an index is out of bounds.
    /// ex: ⍛∞⊏ 3_7_0 [8 3 9 2 0]
    ([2], Fill, OtherModifier, ("fill", '⍛')),
    /// Apply a function at a different array depth
    ///
    /// Expects a rank to operate on, a function, and an array.
    /// The rank supplied indicates the desired rank of the operand.
    /// The array will be split into arrays of that rank, and the function will be applied to each of those arrays.
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
    /// ex: ⍚0⇌ ↯2_2_3 ⇡12 # Reverse each element (does nothing)
    /// ex: ⍚¯1⇌ ↯2_2_3 ⇡12 # Reverse each row
    /// ex: ⍚¯2⇌ ↯2_2_3 ⇡12 # Reverse each row of each row
    /// ex: ⍚1⇌ ↯2_2_3 ⇡12 # Reverse each last axis row
    ///
    /// [level] can operate on multiple arrays at once if passed a list of ranks.
    /// While `level``¯1` is equivelent to [rows] called with a single array, `level``[¯1 ¯1]` is equivalent to [rows] called with two arrays, and so on.
    /// ex: a ← ↯3_3   ⇡9
    ///   : b ← ↯3_3+10⇡9
    ///   :        ≡⊂ a b
    ///   : ⍚[¯1 ¯1]⊂ a b
    ///
    /// [level]`[¯1``infinity``]` is equivalent to [distribute].
    /// ex:       ⫫⊂ 1_2_3 4_5_6
    ///   : ⍚[¯1 ∞]⊂ 1_2_3 4_5_6
    ([2], Level, OtherModifier, ("level", '⍚')),
    /// Call a function and catch errors
    ///
    /// If the first function errors, the second function is called with the error value.
    ///
    /// Normal runtime errors become strings.
    /// ex: ⍣(+1 2)$"Error: _"
    /// ex: ⍣(+@a @b)$"Error: _"
    ///
    /// Errors thrown with [assert] can be any value.
    /// ex: ⍣(⍤5 1 3)(×5)
    /// ex: ⍣(⍤5 0 3)(×5)
    ([2], Try, OtherModifier, ("try", '⍣')),
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
    (2, Assert, Control, ("assert", '⍤')),
    /// Spawn a thread
    ///
    /// Expects a number and a function.
    /// The number is the number of arguments to pop from the stack and pass to the function.
    /// The function is called in a new thread.
    /// A handle that can be passed to [wait] is pushed to the stack. Handles are just numbers.
    /// [wait] consumes the handle and appends the thread's stack to the current stack.
    /// ex:  ↰1⇡ 10
    ///   : ↲↰1⇡ 10
    /// ex:  ↰2(+10+) 1 2
    ///   : ↲↰2(+10+) 1 2
    ///
    /// You can use [each] to spawn a thread for each element of an array.
    /// ex: ∵↰1(/+⇡×.) ⇡10
    ///
    /// [wait] will call [each] implicitly.
    /// ex: ↯3_3⇡9
    ///   : ↲≡↰1/+.
    ([2], Spawn, OtherModifier, ("spawn", '↰')),
    /// Wait for a thread to finish and push its results to the stack
    ///
    /// The argument must be a handle returned by [spawn].
    /// ex: ↲↰1(/+⇡) 10
    ///
    /// If the handle has already been [wait]ed on, then an error is thrown.
    /// ex! h ← ↰1(/+⇡) 10
    ///   : ↲h
    ///   : ↲h
    ///
    /// [wait] will call [each] implicitly.
    /// ex: ↯3_3⇡9
    ///   : ↲≡↰1/+.
    (1, Wait, Misc, ("wait", '↲')),
    /// Call a function
    ///
    /// When passing a scalar function array, the function is simply called.
    /// ex: !(+5) 2
    ///
    /// The behavior when passing a non-scalar array is different.
    /// An additional argument is expected, which is the index of the function to call.
    /// With this, you can do if-else expressions.
    /// ex: Abs ← !·_¯ <0. # If less than 0, negate
    ///   : Abs 5
    ///   : Abs ¯2
    /// This is equivalent to [call][pick][flip]:
    /// ex: Abs ← !⊡∶·_¯ <0.
    ///   : Abs 5
    ///   : Abs ¯2
    ///
    /// Using [call] in this way is *not* recursive. If the selected value is also a function array, it will not be called unless you used [call] again, wich will pop another index.
    /// ex:  ![+_- ×_÷] 1   3 12 # Pick a function array
    /// ex:  ![+_- ×_÷] 1_0 3 12 # Call the function at 1_0
    /// ex:  ![+_- ×_÷] 1 0 3 12 # Not enough calls
    /// ex: !![+_- ×_÷] 1 0 3 12 # 2 calls is enough
    (1, Call, Control, ("call", '!')),
    /// Break out of a loop
    ///
    /// Expects a non-negative integer. This integer is how many loops will be broken out of.
    /// Loops that can be broken out of are [reduce], [fold], [scan], [each], [rows], and [repeat].
    ///
    /// ex: /(⎋>10.+) ⇌⇡40  # Break when the sum exceeds 10
    /// ex: ⍥(⎋>100.×2)∞ 1  # Break when the product exceeds 100
    (1(0), Break, Control, ("break", '⎋')),
    /// Call a function recursively
    ///
    /// Expects a natural number. This number is how many levels up the recured function is.
    /// `recur``0` does nothing.
    /// `recur``1` calls current function.
    /// `recur``2` calls the function that called the current function, and so on.
    ///
    /// Here, we recur until the value is not `less than``10`.
    /// ex: !(↬<10.×2) 1
    ///
    /// Here is a recursive factorial function:
    /// ex: !(×↬>2.-1.) 5
    ///
    /// Here is a recursive fibonacci function.
    /// It uses [call] to decide whether to recur.
    /// ex: !(!(+↬2-1∶↬2-2.)_· <2.) 10
    (0(None), Recur, Control, ("recur", '↬')),
    /// Parse a string as a number
    ///
    /// ex: parse "17"
    /// ex: parse "3.1415926535897932"
    /// ex! parse "dog"
    (1, Parse, Misc, "parse"),
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
    /// If you don't care about a seed, you can use [rand].
    ///
    /// The same seed will always produce the same random number.
    /// ex: [;gen gen gen 0]
    /// ex: [;⍥gen3 0]
    /// ex: [;⍥gen3 1]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: [;⍥(∶⌊*10∶gen)5 0]
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
    /// `2` indicates a function array.
    /// ex: type 5
    /// ex: type "hello"
    /// ex: type (+)
    /// ex: ∵type  {10 "dog" (≅⇌.)}
    ///   : ∵'type! {10 "dog" (≅⇌.)}
    (1, Type, Misc, "type"),
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
);
