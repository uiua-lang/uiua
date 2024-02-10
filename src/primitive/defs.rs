//! All primitive definitions

use super::*;

/// The definition of a shadowable constant
pub struct ConstantDef {
    /// The constant's name
    pub name: &'static str,
    /// The constant's value
    pub value: Value,
    /// The constant's documentation
    pub doc: &'static str,
}

/// Get the list of all shadowable constants
pub fn constants() -> &'static [ConstantDef] {
    &*CONSTANTS
}

macro_rules! constant {
    ($(#[doc = $doc:literal] $(#[$attr:meta])* ($name:literal, $value:expr)),* $(,)?) => {
        const COUNT: usize = {
            let mut count = 0;
            $(
                $(#[$attr])*
                {
                    _ = $name;
                    count += 1;
                }
            )*
            count
        };
        static CONSTANTS: Lazy<[ConstantDef; COUNT]> = Lazy::new(|| {
            [$(
                $(#[$attr])*
                ConstantDef {
                    name: $name,
                    value: $value.into(),
                    doc: $doc,
                },
            )*]
        });
    }
}

constant!(
    /// Euler's constant
    ("e", std::f64::consts::E),
    /// The imaginary unit
    ("i", crate::Complex::I),
    /// IEEE 754-2008's `NaN`
    ("NaN", std::f64::NAN),
    /// The maximum integer that can be represented exactly
    ("MaxInt", 2f64.powi(53)),
    /// A string identifying the operating system
    ("Os", std::env::consts::OS),
    /// A string identifying family of the operating system
    ("Family", std::env::consts::FAMILY),
    /// A string identifying the architecture of the CPU
    ("Arch", std::env::consts::ARCH),
    /// The executable file extension
    ("ExeExt", std::env::consts::EXE_EXTENSION),
    /// The file extension for shared libraries
    ("DllExt", std::env::consts::DLL_EXTENSION),
    /// The primary path separator character
    ("Sep", std::path::MAIN_SEPARATOR),
    /// The number of processors available
    ("NumProcs", num_cpus::get() as f64),
    ///
    (
        "⍼",
        "A dance of lines, a zigzag's descent,
A symbol obscure, with a right angle bent.
No purpose apparent, no function defined,
Yet it captivates minds, with its pleasant design."
    ),
);

macro_rules! primitive {
    ($(
        #[doc = $doc_rust:literal]
        $(#[doc = $doc:literal])*
        (
            $(
                $($args:literal)?
                $(($outputs:expr))?
                $([$mod_args:expr])?
            ,)?
            $variant:ident, $class:ident, $names:expr
        )
    ),* $(,)?) => {
        /// A built-in function
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence, Serialize, Deserialize)]
        #[serde(rename_all = "snake_case")]
        pub enum Primitive {
            $(
                #[doc = $doc_rust]
                $variant,
            )*
            /// System function
            #[serde(untagged)]
            Sys(SysOp)
        }

        impl Primitive {
            /// Get the primitive's names
            #[allow(path_statements)]
            pub fn names(&self) -> PrimNames {
                match self {
                    $(Primitive::$variant => $names.into(),)*
                    Primitive::Sys(op) => op.name().into()
                }
            }
            /// Get the primitive's class
            pub fn class(&self) -> PrimClass {
                match self {
                    $(Primitive::$variant => PrimClass::$class,)*
                    Primitive::Sys(op) => PrimClass::Sys(op.class()),
                }
            }
            /// Get the number of function arguments the primitive takes
            pub fn modifier_args(&self) -> Option<usize> {
                match self {
                    $($($(Primitive::$variant => Some($mod_args),)?)?)*
                    Primitive::Sys(op) => op.modifier_args(),
                    _ => None
                }
            }
            /// Get the number of arguments the primitive takes
            pub fn args(&self) -> Option<usize> {
                match self {
                    $($($(Primitive::$variant => Some($args),)?)?)*
                    Primitive::Sys(op) => Some(op.args()),
                    _ => None
                }
            }
            /// Get the number of outputs the primitive produces
            pub fn outputs(&self) -> Option<usize> {
                match self {
                    $($($(Primitive::$variant => $outputs.into(),)?)?)*
                    Primitive::Sys(op) => Some(op.outputs()),
                    _ => Some(1)
                }
            }
            /// Get the primitive's documentation
            pub fn doc(&self) -> &'static PrimDoc {
                match self {
                    $(Primitive::$variant => {
                        let doc_str = concat!($doc_rust, $($doc, "\n"),*);
                        static DOC: OnceLock<PrimDoc> = OnceLock::new();
                        DOC.get_or_init(|| PrimDoc::from_lines(doc_str))
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
    /// ex: ÷⧻:/+. [4 0 1 2]
    /// Or, maybe you want to find all the numbers in an array that lie within a certain range.
    /// Here, we use [multiply] as a logical AND function.
    /// ex: ×≥5:≤8. [6 2 5 9 6 5 0 4]
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
    /// [over] is often used in examples of functions with two inputs to show both inputs and the output.
    /// ex: [+,, +3 4 5]
    (2(3), Over, Stack, ("over", ',')),
    /// Swap the top two values on the stack
    ///
    /// ex: [: 1 2 3 4 5]
    ///
    /// When combined with [duplicate], you can apply two different functions to the same value.
    /// If you have two functions `f` and `g`, the pattern `f``flip``g``duplicate` will call both functions on the top value.
    /// This is a very common pattern.
    /// For example, maybe you want to find all the uppercase letters in a string.
    /// ex: $ Characters On uppercase OnLy
    ///   : ▽×≥@A:≤@Z..
    /// Or maybe you want to calculate the averge of a list of numbers.
    /// Here, we get the [length] and the `reduce``add``sum` of the list, then [divide] them.
    /// ex: ÷⧻:/+. 1_8_2_5
    (2(2), Flip, Stack, ("flip", ':')),
    /// Discard the top stack value
    ///
    /// This is usually used to discard values that are no longer needed.
    ///
    /// For example, [gen] returns both a random number and a seed for the next call.
    /// When you have all the random numbers you need, you often want to discard the seed.
    /// ex: ⌊×10[◌⍥gen10 0]
    (1(0), Pop, Stack, ("pop", AsciiToken::Semicolon, '◌')),
    /// Do nothing with one value
    ///
    /// [identity] is mostly useless on its own. See the [Advanced Stack Manipulation Tutorial](/docs/advancedstack) to understand what it is for.
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
    /// [sign] on a [complex] number normalizes it to a magnitude of 1.
    /// ex: ± ℂ3 4
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
    /// [absolute value] converts complex numbers to their magnitude.
    /// ex: ⌵ ℂ3 4
    ///
    /// The glyph looks like the graph of `|x|`.
    (1, Abs, MonadicPervasive, ("absolute value", '⌵')),
    /// Take the square root of a number
    ///
    /// ex: √4
    /// ex: √[1 4 9 16]
    /// ex: √¯1
    /// You can only take the square root of a negative number if it is complex.
    /// ex: √  ¯4
    ///   : √ℂ0¯4
    (1, Sqrt, MonadicPervasive, ("sqrt", '√')),
    /// Get the sine of a number
    ///
    /// ex: ○ 1
    /// You can get a cosine function by [add]ing [eta].
    /// ex: ○+η 1
    /// You can get an arcsine function with [un].
    /// ex: °○ 1
    /// You can get an arccosine function by [un]ing the cosine.
    /// ex: °(○+η) 1
    /// You can get a tangent function by [divide]ing the [sine] by the cosine.
    /// ex: ÷○+η:○. 0
    (1, Sin, MonadicPervasive, ("sine", '○')),
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
    /// The second value is checked to be less than the first.
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
    /// The second value is checked to be less than or equal to the first.
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
    /// The second value is checked to be greater than the first.
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
    /// The second value is checked to be greater than or equal to the first.
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
    /// ex: ×,,≥5:≤8. [6 2 5 9 6 5 0 4]
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
    /// ex: ≥5:≤8. [6 2 5 9 6 5 0 4]
    ///   : ↧,,
    (2, Min, DyadicPervasive, ("minimum", '↧')),
    /// Take the maximum of two arrays
    ///
    /// ex: ↥ 3 5
    /// ex: ↥ [1 4 2] [3 7 1]
    ///
    /// Uiua does not have dedicated boolean logical operators.
    /// [maximum] can be used as a logical OR.
    /// ex: ↥,,≤5:≥8. [6 2 5 9 6 5 0 4]
    (2, Max, DyadicPervasive, ("maximum", '↥')),
    /// Take the arctangent of two numbers
    ///
    /// This takes a `y` and `x` argument and returns the angle in radians in the range `(-π, π]`.
    /// ex: ∠ 1 0
    /// ex: ∠ ¯1 0
    /// ex: ∠ √2 √2
    (2, Atan, DyadicPervasive, ("atangent", '∠')),
    /// Make a complex number
    ///
    /// The first argument is the imaginary part, and the second argument is the real part.
    /// ex: ℂ 3 5
    /// ex: ℂ [0 1 2] [3 4 5]
    /// [complex] is equivalent to `add``multiply``i`.
    /// You can use [absolute value] to get the magnitude of the complex number.
    /// ex: ⌵ ℂ3 4
    /// You can use [sign] to normalize the complex number to a magnitude of 1.
    /// ex: ± ℂ3 4
    /// You can use [un][complex] to get the imaginary and real parts back out.
    /// ex: [°ℂ] i
    /// ex: [°ℂ] ×. ℂ3 4
    (2, Complex, DyadicPervasive, ("complex", 'ℂ')),
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
    ///
    /// [first][reverse] is optimized in the interpreter to be O(1).
    /// ex: ⊢⇌ [1 8 4 9 2 3]
    (1, First, MonadicArray, ("first", '⊢')),
    /// Reverse the rows of an array
    ///
    /// ex: ⇌1_2_3_9
    /// ex: ⇌[1_2 3_4 5_6]
    /// [reverse] works through boxes.
    /// ex: ⇌ □[1 2 3]
    /// ex: ≡⇌ {1_2_3_4 5_6_7 8_9}
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
    /// Add a length-1 axis to an array
    ///
    /// ex: ¤5
    /// ex: ¤¤5
    /// ex: ¤[1 2 3]
    /// ex: ¤¤[1 2 3]
    /// This is useful when combine with [rows] or [table] to re-use an entire array for each row of others.
    /// ex: ≡⊂ ¤ 1_2_3 4_5_6
    /// [fix] can also be used with pervasive dyadic functions.
    /// ex: -  [1 2 3]  [4 5 6]
    ///   : - ¤[1 2 3]  [4 5 6]
    ///   : -  [1 2 3] ¤[4 5 6]
    /// ex! -  1_3 [3_4 5_6 7_8]
    /// ex: - ¤1_3 [3_4 5_6 7_8]
    /// [fix]'s name come from the way it "fixes" an array in this way.
    /// See the [Advanced Array Manipulation Tutorial](/docs/advancedarray) for more information on this use case.
    (1, Fix, MonadicArray, ("fix", '¤')),
    /// Encode an array as bits (LSB-first)
    ///
    /// The result will always be 1 rank higher than the input.
    /// ex: ⋯27
    /// ex: ⋯⇡8
    /// ex: ⋯[1_2 3_4 5_6]
    ///
    /// [un][bits] can be used to decode the bits back into numbers.
    /// ex: °⋯ [1 0 1]
    /// ex: °⋯ [0 1 1 0 1]
    /// ex: °⋯ [[0 1 1]
    ///   :     [1 0 0]
    ///   :     [1 1 0]]
    (1, Bits, MonadicArray, ("bits", '⋯')),
    /// Rotate the shape of an array
    ///
    /// ex: ⍉.[1_2 3_4 5_6]
    /// ex: ⍉.[[1_2 3_4] [5_6 7_8]]
    /// [transpose] works through boxes.
    /// ex: ⍉ □[1_2_3 4_5_6]
    /// ex: ≡⍉ {[1_2 3_4] [1_2_3 4_5_6]}
    ///
    /// `shape``transpose` is always equivalent to `rotate``1``shape`.
    /// ex: [1_2 3_4 5_6]
    ///   : ↻1△ .
    ///   : △⍉  :
    ///
    /// Multiple [transpose]s, as well as [rows][transpose], are optimized in the interpreter to only do a single operation.
    (1, Transpose, MonadicArray, ("transpose", '⍉')),
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
    ///
    /// [first][rise] and [first][reverse][rise] are optimized in the interpreter to be O(n).
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
    ///
    /// [first][fall] and [first][reverse][fall] are optimized in the interpreter to be O(n).
    (1, Fall, MonadicArray, ("fall", '⍖')),
    /// Get indices where array values are not equal to zero
    ///
    /// The most basic use is to convert a mask into a list of indices.
    /// ex: ⊚ [1 0 0 1 0 1 1 0]
    /// ex: ⊚.=0◿3.[1 0 2 9 3 8 3 4 6]
    /// It also works for counts `greater than` 1.
    /// ex: ⊚ 1_2_3
    /// ex: ⊚ 1_4_2
    /// [where] on a list is equivalent to `keep``flip``range``length``duplicate`
    /// ex:     ⊚ [0 1 0 0 2 0 1]
    /// ex: ▽:⇡⧻. [0 1 0 0 2 0 1]
    ///
    /// [un][where] will convert the indices back into a a list of counts
    /// ex: °⊚ [0 0 0 1 1 2 2 2 2 2 3]
    /// The indices need not be in order
    /// ex: °⊚ [0 1 2 2 0 3 2 1 2 0 2]
    ///
    /// [where] can be used on multidimensional arrays, and the result will always be rank-2
    /// ex: ⊚.[1_0_0 0_1_1 0_2_0]
    /// The inverse works as well
    /// ex: °⊚[3_4 2_1 0_3]
    ///
    /// [where] on a scalar is equivalent to [where] on a singleton array of that scalar, and so creates a list of `0`s.
    /// ex: ⊚3
    /// ex: ⊚8
    (1, Where, MonadicArray, ("where", '⊚')),
    /// Assign a unique index to each unique element in an array
    ///
    /// ex: ⊛7_7_8_0_1_2_0
    /// ex: ⊛"Hello, World!"
    ///
    /// When combined with [group], you can do things like counting the number of occurrences of each character in a string.
    /// ex: $ Count the characters is this string
    ///   : ⊕($"_ _"⊢:⧻.) ⊛.⊏⍏.
    (1, Classify, MonadicArray, ("classify", '⊛')),
    /// Remove duplicate elements from an array
    ///
    /// ex: ◴ 7_7_8_0_1_2_0
    /// ex: ◴ "Hello, World!"
    /// ex: ◴ [3_2 1_4 3_2 5_6 1_4 7_8]
    (1, Deduplicate, MonadicArray, ("deduplicate", '◴')),
    /// Get a mask of first occurrences of items in an array
    ///
    /// ex: ◰ 7_7_8_0_1_2_0
    /// ex: ◰ "Hello, World!"
    /// ex: ◰ [3_2 1_4 3_2 5_6 1_4 7_8]
    /// [keep][unique][duplicate] deduplicates an array and keeps the first occurrence of each element.
    /// ex: ▽◰. 7_7_8_0_1_2_0
    /// This can also be used to deduplicate by a certain property.
    /// Here, we deduplicate by the [absolute value] of the elements.
    /// ex: ▽◰⌵. [1 ¯2 ¯5 2 3 1 5]
    (1, Unique, MonadicArray, ("unique", '◰')),
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
    /// Use [un][box] to get the values back out.
    /// ex: °□ □1_2_3
    /// Use [un] with `{}`s, [dip]s, and [identity] to get the values back onto the stack
    /// ex: °{⊙⊙∘} {@a 3 7_8_9}
    ///
    /// You would not normally construct arrays like the one above.
    /// The more important use case of [box] is for jagged or nested data.
    /// If you want to collect unevenly-sized groups from [partition] or [group], without [fill]ing, you must use [box].
    /// ex: $ Words of different lengths
    ///   : ⊜□≠@ .
    ///
    /// Pervasive functions work through boxes and preserve the maximum [box] depth of their arguments.
    /// ex: ¯ 1
    ///   : ¯ □1
    ///   : ¯ □□1
    /// ex: +1 4
    ///   : +1 □4
    ///   : +1 □□4
    ///   : +□□1 4
    /// There is an exception for comparison functions, which compare lexicographically if both arguments are boxes.
    /// ex: =  [1 2 3]  [1 2 5]
    ///   : = □[1 2 3] □[1 2 5]
    ///   : >  [1 2 3]  [1 2 5]
    ///   : > □[1 2 3] □[1 2 5]
    ///   : >  "banana"  "orange"
    ///   : > □"banana" □"orange"
    ///   : > □"banana"  "orange"
    ///
    /// For non-pervasive functions, boxed arrays need to be [un][box]ed before they can be operated on.
    /// ex! ⊢  □[1 2 3]
    /// ex: ⊢ °□[1 2 3]
    /// [under][un][box] is useful when you want to re-[box] the result.
    /// ex: $ Reverse these words
    ///   : ⊜□≠@ .
    ///   : ∵⍜°□⇌.
    /// ex: {"Hey" "there" "world"}
    ///   : ≡⍜°□(⊂⊢.)
    /// ex: PrepLen ← $"_ _"⧻.
    ///   : .⊜□≠@ . $ Prepend the word length
    ///   : ∵⍜°□PrepLen
    /// [under][un][box] works because `un``un``box` is just `box`. For each element, it [un][box]es the array out, does something to it, then [box]es the result.
    /// ex: .{1_2_3 4_5 [7]}
    ///   : ∵⍜°□(⬚0↙3)
    /// If you do not need to re-[box] the result, you can use [content] instead.
    /// [content] [un][box]es all box elements that are passed to a function before calling it.
    /// ex: {1_2_3 9_2 5_5_5_5}
    ///   : ≡⊔/+
    /// This is the main way to [join] a list of [box]ed strings.
    /// ex: /⊔⊂       {"Join" "these" "strings"}
    /// ex: /⊔(⊂⊂:@ ) {"Join" "these" "strings"}
    (1, Box, MonadicArray, ("box", '□')),
    /// Parse a string as a number
    ///
    /// ex: ⋕ "17"
    /// ex: ⋕ "3.1415926535897932"
    /// ex: ⋕ "1/2"
    /// ex! ⋕ "dog"
    ///
    /// [parse] is semi-pervasive. It works on multidimensional arrays of characters or boxes.
    /// ex: ⋕ {"5" "24" "106"}
    /// ex: ⋕ .↯3_4 "012"
    ///
    /// [un][parse] will convert a scalar number into a string.
    /// ex: °⋕ 58
    /// ex: °⋕ 6.283185307179586
    /// [un][parse] on a non-scalar number array will [box] each string.
    /// ex: °⋕ 1_2_3
    /// ex: °⋕ ↯3_4⇡12
    (1, Parse, Misc, ("parse", '⋕')),
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
    /// By default, arrays with different shapes cannot be [couple]d.
    /// ex! ⊟ [1 2 3] [4 5]
    /// Use [fill] to make their shapes match
    /// ex: ⬚∞⊟ [1 2 3] [4 5]
    ///
    /// [couple] is compatible with [under].
    /// ex: ⍜⊟(×2) 3 5
    (2, Couple, DyadicArray, ("couple", '⊟')),
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
    /// Index a row or elements from an array
    ///
    /// An index with rank `0` or `1` will pick a single row or element from an array.
    /// ex: ⊡ 2 [8 3 9 2 0]
    /// ex: ⊡ 1_1 .[1_2_3 4_5_6]
    ///
    /// If the index's rank is `2` or greater, then multiple rows or elements will be picked.
    /// ex: ⊡ [1_2 0_1] [1_2_3 4_5_6]
    ///
    /// [under][pick] can be used to modify the value at an index.
    /// ex: ⍜⊡(×10) 2 [8 3 9 2 0]
    /// This works with multiple and/or deeper indices.
    /// ex: ⍜⊡(×10) [2_1 0_2] +1↯3_4⇡12
    /// To simply set a value, you can use [under][pick][pop].
    /// ex: ⍜⊡◌ 2 [8 3 9 2 0] 42
    ///
    /// For index rank `2` or greater, it should hold that `pick``range``shape``duplicate``x` is equivalent to `x`.
    /// ex: ⊡⇡△. [1_2_3 4_5_6]
    (2, Pick, DyadicArray, ("pick", '⊡')),
    /// Change the shape of an array
    ///
    /// ex: ↯ 2_3 [1 2 3 4 5 6]
    /// Shapes that have fewer elements than the original array will truncate it.
    /// ex: ↯ 2_2 [1_2_3 4_5_6]
    /// Shapes that have more elements than the original array will cycle elements.
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
    /// [fill][reshape] fills in the shape with the fill element instead of cycling the data.
    /// ex:   ↯ 3_5 ⇡9
    ///   : ⬚0↯ 3_5 ⇡9
    ///
    /// At most one of the dimensions of the new shape may be negative. This indicates that this is a *derived* dimension, and it will be calculated to make the total number of elements in the new shape be `less or equal` the total number of elements in the original shape.
    /// ex: ↯5_¯1 ⇡15
    /// ex: ↯¯1_5 ⇡15
    /// ex: ↯2_2_¯1 ⇡15
    /// ex: ↯¯1_2_2 ⇡15
    /// ex: ↯3_¯1_5 ⇡30
    /// If [fill] is used, the total number of elements in the new shape will always be `greater or equal` the total number of elements in the original shape.
    /// ex: ⬚0↯ ¯1_5 ⇡12
    ///
    /// See also: [deshape]
    (2, Reshape, DyadicArray, ("reshape", '↯')),
    /// Change the rank of an array's rows
    ///
    /// The resulting array will always have the given rank plus `1`.
    /// ex: ☇ 0 ↯2_3_3⇡18
    ///   : ☇ 1 ↯2_3_3⇡18
    ///   : ☇ 2 ↯2_3_3⇡18
    /// Ranks greater than the rank of the original rows will prepend `1` to the array's [shape].
    /// ex: ☇ 2 [1 2 3 4]
    /// ex: ☇ 3 ↯2_3_3⇡18
    ///   : ☇ 4 ↯2_3_3⇡18
    /// Negative ranks are relative to the rank of the array.
    /// ex: ☇ ¯1 ↯2_3_3⇡18
    ///   : ☇ ¯2 ↯2_3_3⇡18
    ///   : ☇ ¯3 ↯2_3_3⇡18
    ///
    /// [under][rerank] will set the rank back when it is done.
    /// ex: ⍜(☇1)□ ↯2_3_3⇡18
    /// ex: ⍜☇□  2 ↯2_3_3⇡18
    (2, Rerank, DyadicArray, ("rerank", '☇')),
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
    ///   : ↙2_3   .
    ///   : ↙¯2_¯2 :
    ///
    /// By default, taking more than the length of the array will throw an error.
    /// ex! ↙7 [8 3 9 2 0]
    /// If you would like to fill the excess length with some fill value, use [fill].
    /// ex: ⬚π↙ 7 [8 3 9 2 0]
    (2, Take, DyadicArray, ("take", '↙')),
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
    /// Rotate the elements of an array by n
    ///
    /// ex: ↻1 ⇡5
    /// ex: ↻2 ⇡5
    /// ex: ↻¯1 ⇡5
    /// ex: ↻2 .↯3_4⇡12
    ///
    /// Multi-dimensional rotations are supported.
    /// ex: ↻1_2 .↯4_5⇡20
    ///
    /// [fill][rotate] fills in array elements instead of wrapping them.
    /// ex: ⬚0↻ 2 [1 2 3 4 5]
    ///   :   ↻ 2 [1 2 3 4 5]
    /// ex: ⬚0↻ 1_2 .↯4_5⇡20
    (2, Rotate, DyadicArray, ("rotate", '↻')),
    /// The n-wise windows of an array
    ///
    /// ex: ◫2 .⇡4
    /// ex: ◫4 .⇡6
    ///
    /// Multi-dimensional window sizes are supported.
    /// ex: ◫2_2 .[1_2_3 4_5_6 7_8_9]
    ///
    /// Negative window sizes gives the absolute value number of windows.
    /// ex: ◫¯2 ↯4_4⇡16
    /// ex: ◫¯3 ↯4_4⇡16
    /// This can be useful when you want to get horizontal windows.
    /// ex: ◫¯1_2 ↯4_4⇡16
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
    /// Find the occurences of one array in another
    ///
    /// A `1` marker will be placed the the start of each occurence of the first array in the second array.
    /// ex: ⌕ 5 [1 8 5 2 3 5 4 5 6 7]
    /// ex: ⌕ "ab" "abracadabra"
    /// If the searched-in array is multidimensional, the `1` marker will be placed in the minimum index "top left" corner.
    /// ex: ⌕ 1_2 . ↯4_4⇡3
    /// ex: ⌕ [1_2 2_0] . ↯4_4⇡3
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
    /// Find the first index of each row of one array in another
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
    ///   : ⬚∞⊏:a
    ///
    /// [indexof] is closely related to [member].
    (2, IndexOf, DyadicArray, ("indexof", '⊗')),
    // /// Find sequential indices of each row of one array in another
    // ///
    // /// Unlike [indexof], [progressive indexof] will return the sequential indices of each row of the first array in the second array; the same index will not be used twice.
    // /// When a searched-for row runs out of indices in the searched-in array, the length of the searched-in array is returned.
    // /// Note here where the results are the same and where they are different:
    // /// ex: ⊗ [1 1 2 2 3 3 4 4] [2 2 1 4 1 2 3 4]
    // ///   : ⊘ [1 1 2 2 3 3 4 4] [2 2 1 4 1 2 3 4]
    // ///
    // /// One use of this is to find the first occurence of each row.
    // /// ex: > ⊃⊘⋅⧻ ⊃∘◴ . [1 4 3 3 2 1 3 5 2 1]
    // ///
    // /// The [progressive indexof] an array in itself is the [range][length] of the array.
    // /// ex: ⊘. [1 4 3 3 2 1 3 5 2 1]
    // (
    //     2,
    //     ProgressiveIndexOf,
    //     DyadicArray,
    //     ("progressive indexof", '⊘')
    // ),
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
    /// [reduce][join] is the simplest way to combine the first two dimensions of an array.
    /// It is optimized in the interpreter to be very fast.
    /// ex: /⊂ .↯2_2_4⇡16
    ///
    /// Some functions have default values if the array is empty.
    /// Functions without default values will throw an error if the array is empty.
    /// ex: /+ []
    /// ex: /× []
    /// ex: /↥ []
    /// ex: /↧ []
    /// ex: /∠ []
    /// ex! /⊡ []
    ///
    /// An initial value can be set with [fill].
    /// ex:   /↥ []
    /// ex: ⬚5/↥ []
    /// ex:   /↥ [1 2 3]
    /// ex: ⬚5/↥ [1 2 3]
    (1[1], Reduce, AggregatingModifier, ("reduce", '/')),
    /// Apply a function to aggregate arrays
    ///
    /// Expects as many arguments as its function takes.
    /// The function must take at least 1 more argument than it returns outputs.
    /// Arguments that are lower on the stack that will be used as accumulators.
    /// Arguments that are higher on the stack will be iterated over.
    /// The function will be repeatdely called with the rows of the iterated arrays followed by the accumulators.
    /// On each iteration, the returned values will be used as the new accumulators.
    ///
    /// For example, [fold] can be used to [reduce] an array with a default value.
    /// ex: ∧+ [1 2 3] 10
    ///   : ∧+ [] 10
    ///
    /// Multiple accumulators can be used
    /// ex: ∧(⊃+(×⊙⋅∘)) +1⇡5 0 1
    /// If the iterated array is already on the stack, you can use [dip] to place the accumulators below it.
    /// ex: ∧(⊃+(×⊙⋅∘))⊙(0 1) +1⇡5
    ///
    /// Multiple iterated arrays are also fine.
    /// Here, we accumulate the first array with [add] and the second with [multiply].
    /// ex: ∧⊃(+⊙⋅∘)(×⋅⊙⋅∘) 1_2_3 4_5_6 0 1
    ///
    /// Like [rows], [fold] will repeat the row of arrays that have exactly one row.
    /// ex: ∧(⊂⊂) 1_2_3 4 []
    ///
    /// Here is a reimplementation of [scan] using [fold].
    /// ex: ⇌∧(⊂+⊙(⊢.)) ⊃↘↙1 [1 2 3 4]
    ([1], Fold, AggregatingModifier, ("fold", '∧')),
    /// Reduce, but keep intermediate values
    ///
    /// ex: \+   1_2_3_4
    /// ex: \-   1_2_3_4
    /// ex: \(-:) 1_2_3_4
    /// [scan] is often used to do something with masks.
    /// [scan]ning with [minimum] or [maximum] will propogate `0`s or `1`s.
    /// ex: ▽\↧≠@ . "Hello World!"
    /// [scan]ning with [add] and then using [group] can split by a delimiter while keeping the delimiter.
    /// ex: ⊕□\+=@    . "Everyday man's on the block"
    ///   : ⊕□\+↻¯1=@ . "Everyday man's on the block"
    (1[1], Scan, AggregatingModifier, ("scan", '\\')),
    /// Apply a function to each element of an array or arrays.
    ///
    /// This is the element-wise version of [rows].
    /// **This is often not what you want.** Prefer using pervasive functions or [table] when possible.
    ///
    /// The number of arrays used depends on how many arguments the function takes.
    /// ex: ∵(⊟.) 1_2_3_4
    /// ex: ∵⊂ 1_2_3 4_5_6
    /// ex: ∵⊂ 1_2 [4_5 6_7]
    ///
    /// If the function is already pervasive, then [each] is redundant.
    /// ex! ∵+ 1_2_3 4_5_6
    /// ex:  + 1_2_3 4_5_6
    ///
    /// [each] is one of a few modifiers that uses [proxy values](/docs/functions#proxy).
    ([1], Each, IteratingModifier, ("each", '∵')),
    /// Apply a function to each row of an array or arrays
    ///
    /// This is the row-wise version of [each].
    ///
    /// ex:  /+ [1_2_3 4_5_6 7_8_9]  # Sum each row with the next
    /// ex: ≡/+ [1_2_3 4_5_6 7_8_9]  # Sum the elements of each row
    ///
    /// The number of arrays used depends on how many arguments the function takes.
    /// ex: ≡/+ [1_2 3_4] 5_6 # One argument
    /// ex: ≡⊂  [1_2 3_4] 5_6 # Two arguments
    ///
    /// In general, when [rows] uses multiple arrays, the arrays must have the same number of rows.
    /// ex! ≡⊂ 1_2_3 4_5
    /// However, if any of the arrays have exactly one row, then that row will be reused for each row of the other arrays.
    /// Scalars are considered to have one row.
    /// ex: ≡⊂ 1_2_3 4
    /// ex: ≡⊂ 1 2_3_4
    /// ex: ≡(⊂⊂) 1 2_3_4 5
    /// You can use [fix] to take advantage of this functionailty and re-use an entire array for each row of another.
    /// ex: ≡⊂ ¤  1_2_3 4_5_6
    /// ex: ≡⊂ ⊙¤ 1_2_3 4_5_6
    /// [fold] also has this behavior.
    ///
    /// [rows] is one of a few modifiers that uses [proxy values](/docs/functions#proxy).
    ([1], Rows, IteratingModifier, ("rows", '≡')),
    /// Apply a function to each combination of rows of two arrays
    ///
    /// This is often what you want instead of [each].
    ///
    /// ex: ⊞+ 1_2_3 4_5_6_7
    /// ex: ⊞⊂ 1_2 3_4
    ///
    /// The resulting array will always have a shape starting with the lengths of the two inputs.
    /// ex: △⊞+ 1_2 3_4_5
    /// ex: △⊞⊂ 1_2 3_4_5
    /// ex: △⊞+ [1_2_3 4_5_6] [7 8 9 10]
    /// ex: △⊞⊂ [1_2_3 4_5_6] [7 8 9 10]
    ///
    /// [table] also works with more than two arrays.
    /// ex: ⊞(⊂⊂) 1_2 3_4 5_6
    /// If you want to fix one of the arrays so that it is present in every call of the function, you can simply add a dimension to it, though you may need to collapse it later.
    /// Here, we add a dimension to the second array to [fix] it, then collapse with `reduce``join`.
    /// ex: /⊂ ⊞(⊂⊂) ⊙¤ 1_2 3_4 5_6
    (2[1], Table, IteratingModifier, ("table", '⊞')),
    /// Apply a function to each combination of rows of arrays
    ///
    /// This *was* the row-wise version of [table].
    ///
    /// ex: [1_2 3_4 5_6]
    ///   : [7_8 9_10]
    ///   : ⊠⊂ ,,
    /// [cross] works with more than two arrays.
    /// ex: ⊠(⊂⊂) 1_2 3_4 5_6
    /// If you want to fix one of the arrays so that it is present in every call of the function, you can simply add a dimension to it, though you may need to collapse it later.
    /// Here, we add a dimension to the second array to fix it, then collapse with `reduce``join`.
    /// ex: /⊂ ⊠(⊂⊂) ⊙¤ 1_2 3_4 5_6
    (2[1], Cross, IteratingModifier, ("cross", '⊠')),
    /// Repeat a function a number of times
    ///
    /// ex: ⍥(+2)5 0
    /// ex: ⍥(⊂2)5 []
    /// One interesting use of `repeat` is to collect some number of stack values into an array.
    /// ex: ⍥⊂3 [] 1 2 3
    /// Repeating [infinity] times will create an infinite loop that can only be terminated by ending the program.
    /// If you want an infinite loop that ends when some condition is met, use [do].
    ///
    /// [repeat]'s glyph is a combination of a circle, representing a loop, and the 𝄇 symbol from musical notation.
    ([1], Repeat, IteratingModifier, ("repeat", '⍥')),
    /// Group elements of an array into buckets by index
    ///
    /// Takes a function and two arrays.
    /// The arrays must be the same [length].
    /// The first array must be rank `1` and contain integers.
    /// Rows in the second array will be grouped into buckets by the indices in the first array.
    /// Keys `less than``0` will be omitted.
    /// The function then processes each group in order. The result depends on what the function is.
    /// If the function takes 0 or 1 arguments, then [group] behaves like [rows]. This is called *iterating* [group].
    /// ex: ⊕∘ [0 2 2 1 0 1] [1 2 3 4 5 6]
    /// If the function takes 2 or more arguments, then [group] behaves like [reduce]. This is called *reducing* [group].
    /// ex: ⊕⊂ [0 2 2 1 0 1] [1 2 3 4 5 6]
    /// If the values returned by the function do not have the same [shape], concatenation will fail.
    /// ex! ⊕∘ [0 1 0 2 1 1] [1 2 3 4 5 6]
    /// It is common to use [box] to encapsulate groups of different [shape]s.
    /// ex: ⊕□ [0 1 0 2 1 1] [1 2 3 4 5 6]
    ///
    /// When combined with [classify], you can do things like counting the number of occurrences of each character in a string.
    /// ex: $ Count the characters is this string
    ///   : ⊕{⊢:⧻.} ⊛.⊏⍏.
    ///
    /// [under][group] works if [group]'s function is [under]able.
    /// ex: ⍜⊕□≡⇌ ≠@ . $ These are some words
    /// The length of each group must not change.
    /// ex! ⍜⊕□⇌ ≠@ . $ These are some words
    ///
    /// [group] is closely related to [partition].
    (2[1], Group, AggregatingModifier, ("group", '⊕')),
    /// Group sequential sections of an array
    ///
    /// The most common use of [partition] is to split an array by a delimiter.
    ///
    /// Takes a function and two arrays.
    /// The arrays must be the same [length].
    /// The first array must be rank `1` and contain integers.
    /// Consecutive rows in the second array that line up with groups of the same key in the first array will be grouped together.
    /// Keys `less or equal``0` will be omitted.
    /// The function then processes each group in order. The result depends on what the function is.
    /// If the function takes 0 or 1 arguments, then [partition] behaves like [rows]. This is called *iterating* [partition].
    /// ex: ⊜∘ [0 0 2 2 1 1 3 3] [1 2 3 4 5 6 7 8]
    /// If the function takes 2 or more arguments, then [partition] behaves like [reduce]. This is called *reducing* [partition].
    /// ex: ⊜⊂ [0 0 2 2 1 1 3 3] [1 2 3 4 5 6 7 8]
    /// If the values returned by the function do not have the same [shape], concatenation will fail.
    /// ex! ⊜∘ [0 2 3 3 3 0 1 1] [1 2 3 4 5 6 7 8]
    /// It is common to use [box] to encapsulate groups of different [shape]s.
    /// ex: ⊜□ [0 2 3 3 3 0 1 1] [1 2 3 4 5 6 7 8]
    ///
    /// This can be used to split an array by a delimiter.
    /// ex: ⊜□ ≠@ . $ Hey there friendo
    /// You can nest [partition]s to split by multiple delimiters and create a multi-dimensional array.
    /// ex: $ 1 1 2 3
    ///   : $ 5 8 13 21
    ///   : ⊜(⊜⋕≠@ .)≠@\n.
    ///
    /// [under][partition] works if [partition]'s function is [under]able.
    /// ex: ⍜⊜□⇌  ≠@ . $ These are some words
    /// ex: ⍜⊜□≡⇌ ≠@ . $ These are some words
    ///
    /// [partition] is closely related to [group].
    (2[1], Partition, AggregatingModifier, ("partition", '⊜')),
    /// Apply a function with implicit unboxing
    ///
    /// When working with [box]ed data, [unpack] will automatically [un][box] the data for functions like [join].
    /// ex:  /⊂ {"a" "bc" "def"}
    /// ex: ⊐/⊂ {"a" "bc" "def"}
    ///
    /// Anything that is [box]ed inside the function will be [un][box]ed as soon as it is used.
    /// This may lead to unexpected behavior if you are not aware of it.
    /// ex: ⊐(¯□3) # Used
    /// ex: ⊐( □3) # Not used
    ([1], Unpack, OtherModifier, ("unpack", '⊐')),
    /// Unbox the arguments to a function before calling it
    ///
    /// ex:  ⊂ □[1 2 3] □[4 5 6]
    ///   : ⊔⊂ □[1 2 3] □[4 5 6]
    /// A common use of [content] is to collapse a list of [box]ed arrays with [reduce].
    /// ex: /⊔⊂ {1_2_3 4_5 6}
    /// This case will still unbox a single element.
    /// ex: /⊔⊂ {"Hi"}
    ([1], Content, OtherModifier, ("content", '⊔')),
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
    ///
    /// [dip] is compatible with function packs.
    /// It is equivalent to nesting [dip]s.
    /// ⚠ Using [dip] in this way is experimental and may change in the future.
    /// ex: +⊙(×⊙(↙⊙↘)) 2 10 3 1 [1 2 3 4 5]
    /// ex: +⊙(×|↙|↘)   2 10 3 1 [1 2 3 4 5]
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
    /// There are two common patterns that involve a dyadic function and three values.
    /// If we call the function `f` and the values `a`, `b`, and `c`, then the patterns are:
    /// - `fac fbc`
    /// - `fab fac`
    /// Both involve applying a stack idiom before calling [both].
    /// For `fac fbc`, the idiom is `dip``over`.
    /// For `fab fac`, the idiom is `over``flip`.
    /// For example, if you wanted to check that a number is divisible by two other numbers:
    /// ex: F ← ∩(=0◿) ⊙,
    ///   : F 3 5 ⇡16
    /// ex: G ← ∩(=0◿:) ,:
    ///   : G ⇡16 3 5
    (2[1], Both, Planet, ("both", '∩')),
    /// Invert the behavior of a function
    ///
    /// Most functions are not invertible.
    ///
    /// ex: √2
    /// ex: °√2
    ///
    /// [un][couple] uncouples a [length]`2` array and pushes both rows onto the stack.
    /// ex: °⊟ .[1_2_3 4_5_6]
    ///
    /// [un][transpose] transposes in the opposite direction.
    /// This is useful for arrays with rank `greater than``2`.
    /// ex: °⍉ .⊟.[1_2_3 4_5_6]
    ///
    /// [un][bits] converts an array of bits into a number.
    /// ex: °⋯ [1 0 1 0 1 0 1 0]
    ///
    /// [un][sine] gives the arcsine.
    /// ex: °○ 1
    ///
    /// [un] can be used with stack array notation and [dip] and [identity] to unpack the items of an array onto the stack.
    /// ex: [⊙⊙∘] 1 2 3
    /// ex: °[⊙⊙∘] [1 2 3]
    ///
    /// `un``reduce``multiply` finds the prime factors of a number. It also works with arrays, filling in the shape with `1`s.
    /// ex: °/× 42
    /// ex: °/× +1⇡10
    ///
    /// While more inverses exists, most of them are not useful on their own.
    /// [under] also uses inverses, but is more powerful.
    /// A function's inverse can be set with [setinv].
    ([1], Un, InversionModifier, ("un", '°')),
    /// Set a function as its own inverse
    ///
    /// ex: # Experimental!
    ///   : F ← ⌅⧻
    ///   : F   1_2_4
    ///   : °F  1_2_4
    ///   : ⍜F∘ 1_2_4 # Calls ⧻ twice
    /// This is useful when combined with [under]. It allows you to call a function twice with another function in between.
    /// Finding the standard deviation of a list of numbers requires finding the mean twice. Here, we only need to write the mean code once.
    /// ex: # Experimental!
    ///   : StdDev ← √⍜⌅(÷⊃⧻/+)(×.-).
    ///   : StdDev [1 2 5 8 9]
    ///
    /// For more complex inverse defining, see [setinv] and [setund].
    ([1], Rectify, InversionModifier, ("rectify", '⌅')),
    /// Set the [un]-compatible inverse of a function
    ///
    /// The first function is the uninverted function, and the second function is the inverse.
    /// ex: # Experimental!
    ///   : F ← setinv(&p$"Forward _" .)(&p$"Backward _" .)
    ///   : ◌F   @A
    ///   : ◌°F  @B
    ///   : ◌⍜F∘ @C
    ///
    /// Unlike built-in functions, [setinv] cannot properly make inverses that save context for use in [under].
    /// This can lead to errors if you are unaware of it.
    /// ex! # Experimental!
    ///   : F ← setinv+-
    ///   : ⍜F∘ 3 5
    ///
    /// For [under]-compatible inverse defining, see [setund].
    ([2], SetInverse, InversionModifier, "setinv"),
    /// Set the [under]-compatible inverse of a function
    ///
    /// The first function will be called if the function is *outside* an [under].
    /// The second function will be called in the "do" part of an [under].
    /// The third function will be called in the "undo" part of an [under].
    ///
    /// Any outputs of the second function that excede the number of outputs of the first function will be popped and saved as *context* after the "do" part of the [under]. On the "undo" part, the context will be pushed onto the stack before calling the third function.
    ///
    /// For example, here is a manual re-implementation of [add]'s [under] behavior. Note that the second function has 2 outputs. The extra output is saved as context.
    /// ex: # Experimental!
    ///   : F ← setund(+|⊃∘+|-)
    ///   : ⍜+(×10) 1 2
    ///   : ⍜F(×10) 1 2
    ///
    /// This example demonstrates the flow of input, output, and context.
    /// ex: # Experimental!
    ///   : F ← setund(
    ///   :   &p$"Normal _".
    ///   : | &p$"Do:   set ctx = _, value = _" ,, +1.
    ///   : | &p$"Undo: get ctx = _, value = _" ⊙.
    ///   : )
    ///   : ◌F 5
    ///   : ◌⍜F(×10) 5
    ///
    /// Inverses set with [setund] cannot be used with [un]. For simpler inverse defining, see [setinv].
    ([3], SetUnder, InversionModifier, "setund"),
    /// Apply a function under another
    ///
    /// This is a more powerful version of [un].
    /// Conceptually, [under] transforms a value, modifies it, then reverses the transformation.
    ///
    /// [under] takes 2 functions `f` and `g` and another argument `x`.
    /// It applies `f` to `x`, then applies `g` to the result.
    /// It then applies the inverse of `f` to the result of `g`.
    ///
    /// Any function that can be [un]ed can be used with [under].
    /// Some functions that can't be [un]ed can still be used with [under].
    ///
    /// Here, we [negate] 5, [subtract] 2, then [negate] again.
    /// ex: ⍜¯(-2) 5
    /// You can use [under] with [round] to round to a specific number of decimal places.
    /// ex: ⍜(×1e3)⁅ π
    ///
    /// The above examples involve an *arithmetic* under. That is, [un]`f` is well-definined independent of [under]'s concept of "undoing".
    /// The remaining examples below involve `f`s which cannot be normally [un]ed, but which are valid as functions to use with [under].
    ///
    /// [under][deshape] will [reshape] the array after `g` finishes.
    /// ex: ⍜♭⇌ .↯3_4⇡12
    /// If you want to insert a value somewhere in the middle of an array, you can use [under], [rotate], and [join].
    /// ex: ⍜(↻3)(⊂π) 1_2_3_4_5
    /// You can use [under][first] to apply a function to the first row of an array.
    /// ex: ⍜⊢(×10) 1_2_3_4_5
    /// If you need to work on more of the array's rows, can use [under] with [take] or [drop].
    /// ex: ⍜(↙3)(×10) 1_2_3_4_5
    /// ex: ⍜(↘3)(×10) 1_2_3_4_5
    /// You can chain [under]-compatible functions.
    /// ex: ⍜(↙2↘1)(×10) 1_2_3_4_5
    /// [pick] and [select] also work.
    /// ex: ⍜⊡(×10) 2_1 ↯3_3⇡9
    /// ex: ⍜⊏(×10) 1_3 1_2_3_4_5
    /// Although, [under][select] only works if the indices are unique.
    /// ex! ⍜⊏(×10) 1_3_3 1_2_3_4_5
    /// [under][keep] works as long as the counts list is boolean.
    /// ex: ⍜▽(×10) =0◿3.⇡10
    ///
    /// If `g` takes more than 1 argument, keep in mind that `f` will be called on the stack as it is when the full under expression begins.
    /// This means you may have to flip the arguments to `g`.
    /// Consider this equivalence:
    /// ex: ⍜(↙2)(÷:)  [1 2 3 4 5] 10
    ///   : ⍜(↙2)(÷10) [1 2 3 4 5]
    ///
    /// [under][both] works, and whether [both] is applied when undoing depends on the signature of `g`.
    /// For example, this hypotenuse function does not use [both] when undoing because its `g` (`add`) returns a single value.
    /// ex: ⍜∩(×.)+ 3 4
    /// However, this function whose `g` returns *2* values *does* use [both] when undoing, in this case re-[box]ing the outputs.
    /// ex: ⍜∩°□(⊂⊢,) □[1 2 3] □[4 5 6 7 8]
    ///
    /// [under] works with [&fo], [&fc], [&tcpa], and [&tcpc]. It calls [&cl] when `g` is done.
    ///
    /// [setund] can be used to define a function's [under] behavior.
    ([2], Under, InversionModifier, ("under", '⍜')),
    /// Call two functions on the same values
    ///
    /// [fork] is one of the most important functions for working with the stack.
    /// See the [Advanced Stack Manipulation Tutorial](/docs/advancedstack) for a more complete understanding as to why.
    ///
    /// ex: ⊃⇌◴ 1_2_2_3
    /// [fork] can be chained to apply more functions to the arguments. `n` functions require the chaining of `subtract``1n` [fork].
    /// ex: [⊃⊃⊃+-×÷ 5 8]
    /// If the functions take different numbers of arguments, then the number of arguments is the maximum. Functions that take fewer than the maximum will work on the top values.
    /// ex: [⊃+¯ 3 5]
    ([2], Fork, Planet, ("fork", '⊃')),
    /// Call one function after another, reusing some values
    ///
    /// [cascade]'s second function is called, then its first argument(s) are reused, along with its output, as arguments to the first function.
    /// ex: # Experimental!
    ///   : ×⊃∘+ 5 2
    ///   :  ⪾×+ 5 2
    /// Nesting [cascade] can be useful, and doing so is equivalent to using a function pack.
    /// ex: # Experimental!
    ///   : ÷:-÷2:⇡..  10
    ///   : ⪾⪾÷(-÷2)⇡  10
    ///   : ⪾(÷|-÷2|⇡) 10
    /// In the examples above, [cascade] only reuses a single argument. However, it can reuse multiple arguments.
    /// ex: # Experimental!
    ///   : ⪾$"_ + _ = _"+ 1 2
    ([2], Cascade, Planet, ("cascade", '⪾')),
    /// Call two functions on two distinct sets of values
    ///
    /// ex: ⊓⇌◴ 1_2_3 [1 4 2 4 2]
    /// Each function will always be called on its own set of values.
    /// ex: ⊓+× 1 2 3 4
    /// The functions' signatures need not be the same.
    /// ex: ⊓+(++) 1 2 3 4 5
    /// [bracket] can be chained to apply additional functions to arguments deeper on the stack.
    /// ex: ⊓⊓⇌(↻1)△ 1_2_3 4_5_6 7_8_9
    /// ex: [⊓⊓⊓+-×÷ 10 20 5 8 3 7 2 5]
    ([2], Bracket, Planet, ("bracket", '⊓')),
    /// Call a function on many distinct sets of values
    ///
    /// For just 2 sets of values, [both] is often simpler.
    ///
    /// The second function will be called, then the first function will be called on groups of values.
    /// To do something similar to [both] on more than 2 sets of values, you can use [dip] and [identity] to select values.
    /// ex: # Experimental!
    ///   : ⋔(↯3)⊙⊙∘ 1 2 3
    /// If you wanted to use 3 different constants, you could put the constants in the second function.
    /// ex: # Experimental!
    ///   : ⋔↯(2 3 4) 1 2 3
    /// This can also be done if the constants are on the stack.
    /// ex: # Experimental!
    ///   : ⋔↯⊙⊙∘ 2 3 4 1 2 3
    /// This can also be an interesting way to reorder many values.
    /// ex: # Experimental!
    ///   : [⋔⊙∘⊙⊙∘ 1 2 3 4 5 6]
    ([2], All, Planet, ("all", '⋔')),
    /// Repeat a function while a condition holds
    ///
    /// The first function is the loop function, and it is run as long as the condition is true.
    /// The second function is the condition. It's top return value must be a boolean.
    /// ex: ⍢(×2)(<1000) 1
    /// Return values from the condition function that are under the condition itself will be passed to the loop function.
    /// Here is an example that evaluates a [Collatz sequence](https://en.wikipedia.org/wiki/Collatz_conjecture).
    /// The next number in the sequence is calculated in the condition function but [join]ed to the sequence in the loop function.
    /// ex: C ← (+1×3|÷2)=0◿2.
    ///   : ◌⍢⊂(¬∊,,C⊢.) [7]
    /// If the condition function consumes its only arguments to evaluate the condition, then those arguments will be implicitly copied.
    /// Consider this equivalence:
    /// ex: ⍢(×3)(<100)  1
    ///   : ⍢(×3)(<100.) 1
    /// The net stack change of the two functions, minus the condition, must be 0.
    /// ex! ⍢(×2.)(<1000) 1
    /// This means that unlike [repeat], [do] cannot be wrapped in `[]`s to collect items into an array.
    /// Instead, [join] the items to an initial list.
    /// ex: ◌⍢(⊃(×2)⊂)(<100) 1 []
    ([2], Do, IteratingModifier, ("do", '⍢')),
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
    /// [fill][rotate] fills in array elements instead of wrapping them.
    /// ex: ⬚0↻ 2 [1 2 3 4 5]
    ///   :   ↻ 2 [1 2 3 4 5]
    ///
    /// To [fill] with a value that is on the stack, use [identity].
    /// ex: F = ⬚∘+
    ///   : F 100 [1 2 3 4] [5 6]
    ([2], Fill, OtherModifier, ("fill", '⬚')),
    /// Call a function and catch errors
    ///
    /// If the first function errors, the second function is called with the original arguments and the error value below.
    ///
    /// Normal runtime errors become strings.
    /// ex: ⍣(+1 2)$"Error: _"
    /// ex: ⍣(+@a @b)$"Error: _"
    /// Errors thrown with [assert] can be any value.
    /// ex: ⍣(⍤5 1 3|×5)
    /// ex: ⍣(⍤5 0 3|×5)
    /// The functions must have the same number of outputs.
    /// The handler function must have a number of arguments equal to 0, 1, or 1 + the number of arguments to the first function.
    /// If the handler function has 0 arguments, then it is simply called.
    /// ex: ⍣⋕0 "5"
    ///   : ⍣⋕0 "dog"
    /// If the handler function has 1 argument, then the error is passed to it.
    /// ex: ⍣⋕∘ "5"
    ///   : ⍣⋕∘ "dog"
    /// If the handler function has 1 + the number of arguments to the first function, then the error is passed to it along with the original arguments.
    /// ex: ⍣⋕{⊙∘} "5"
    ///   : ⍣⋕{⊙∘} "dog"
    ([2], Try, Misc, ("try", '⍣')),
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
    (2(0), Assert, Misc, ("assert", '⍤')),
    /// Set a function to recur to
    ///
    /// A function must have been set with [this] before calling [recur].
    /// Here is a recursive factorial function.
    /// ex: # Experimental!
    ///   : ↬((|1 ×↫-1.|1)<2.) 5
    /// This is only for demonstration purposes, as factorial can be implemented much more simply.
    /// ex: # Experimental!
    ///   : /×+1⇡ 5
    ([1], This, Misc, ("this", '↬')),
    /// Call a function recursively
    ///
    /// A function must have been set with [this] before calling [recur].
    /// Here is a recursive factorial function.
    /// ex: # Experimental!
    ///   : ↬((|1 ×↫-1.|1)<2.) 5
    /// This is only for demonstration purposes, as factorial can be implemented much more simply.
    /// ex: /×+1⇡ 5
    ///
    /// The presence of a [recur] prevents the signature checker from working, so a signature must always be provided at the innermost function that contains a [recur].
    /// ex: # Experimental!
    ///   : ↬((+∩(|2 ↫ -)1,2|1)<2.) 5
    (0(None), Recur, Misc, ("recur", '↫')),
    /// Generate a random number in the range `[0, 1)`
    ///
    /// If you need a seeded random number, use [gen].
    ///
    /// ex: ⚂
    /// ex: [⚂⚂⚂]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: ⌊×10 [⍥⚂5]
    (0, Rand, Misc, ("random", '⚂')),
    /// Bind local values
    ///
    /// The values can be accessed in the function with lowercase letters.
    /// ex: # Experimental!
    ///   : bind(⊂ b + a c) 1 2 3
    /// Adjacent letters can be part of a single identifier.
    /// ex: # Experimental!
    ///   : bind[[beef][babe]] 1 2 3 4 5 6
    /// Here is a simple quadratic formula function.
    /// ex: # Experimental!
    ///   : Quad ← bind(÷×2a -b ⊟¯.√ -××4ac ×.b)
    ///   : Quad 1 2 0
    ///
    /// [bind] is not really a true modifier. [bind]'s function does not make sense without it.
    /// ex! # Experimental!
    ///   : F ← ×b +ac
    ///   : bindF 1 2 3
    ([1], Bind, OtherModifier, "bind"),
    /// Memoize a function
    ///
    /// If a function is [memo]ized, then its results are cached.
    /// Calling the function with the same arguments will return the cached result instead of recalculating it.
    /// ex: F ← +⌊×10⚂
    ///   : ∵F [1 1 2 2 3 3]
    /// ex: F ← memo(+⌊×10⚂)
    ///   : ∵F [1 1 2 2 3 3]
    /// In general, this should only be used with functions that perform a potentially expensive calculation.
    ([1], Memo, OtherModifier, "memo"),
    /// Run a function at compile time
    ///
    /// ex: F ← (⌊×10[⚂⚂⚂])
    ///   : [F F F]
    /// ex: F ← comptime(⌊×10[⚂⚂⚂])
    ///   : [F F F]
    /// [comptime]'s function must take no arguments.
    /// If you would like to pass arguments to [comptime]'s function, make them part of the function
    /// ex! comptime(+) 1 2
    /// ex: comptime(+ 1 2)
    ([1], Comptime, OtherModifier, "comptime"),
    /// Spawn a thread
    ///
    /// Expects a function.
    /// In the native interpreter, the function is called in a new OS thread.
    /// In the web editor, the function is called and blocks until it returns.
    /// A thread id that can be passed to [wait] is pushed to the stack. Handles are just numbers.
    /// [wait] consumes the thread id and appends the thread's stack to the current stack.
    /// ex:      spawn⇡ 10
    ///   : wait spawn⇡ 10
    /// ex:      spawn(+10+) 1 2
    ///   : wait spawn(+10+) 1 2
    ///
    /// You can use [rows] to spawn a thread for each row of an array.
    /// ex: ≡spawn(/+⇡×.) ⇡10
    ///
    /// [wait] will call [each] implicitly.
    /// ex: ↯3_3⇡9
    ///   : wait≡spawn/+.
    ([1], Spawn, OtherModifier, "spawn"),
    /// Wait for a thread to finish and push its results to the stack
    ///
    /// The argument must be a thread id returned by [spawn].
    /// ex: wait spawn(/+⇡) 10
    ///
    /// If the thread id has already been [wait]ed on, then an error is thrown.
    /// ex! h ← spawn(/+⇡) 10
    ///   : wait h
    ///   : wait h
    ///
    /// [wait] is pervasive and will call [each] implicitly.
    /// ex: ↯3_3⇡9
    ///   : wait≡spawn/+.
    (1, Wait, Misc, "wait"),
    /// Send a value to a thread
    ///
    /// Expects a value to send and a thread id returned by [spawn].
    /// The thread id `0` corresponds to the parent thread.
    /// The sent-to thread can receive the value with [recv] or [tryrecv].
    (2(0), Send, Misc, "send"),
    /// Receive a value from a thread
    ///
    /// Expects a thread id returned by [spawn].
    /// The thread id `0` corresponds to the parent thread.
    /// The sending thread can send a value with [send].
    ///
    /// Unlike [tryrecv], [recv] blocks until a value is received.
    (1, Recv, Misc, "recv"),
    /// Try to receive a value from a thread
    ///
    /// Expects a thread id returned by [spawn].
    /// The thread id `0` corresponds to the parent thread.
    /// The sending thread can send a value with [send].
    ///
    /// Unlike [recv], [tryrecv] does not block.
    /// If no value is available, then an error is thrown.
    /// The error can be caught with [try].
    (1, TryRecv, Misc, "tryrecv"),
    /// Generate a random number between 0 and 1 from a seed, as well as the next seed
    ///
    /// If you don't care about a seed, you can use [random].
    ///
    /// The same seed will always produce the same random number.
    /// ex: [◌gen gen gen 0]
    /// ex: [◌⍥gen3 0]
    /// ex: [◌⍥gen3 1]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: ⌊*10[◌⍥gen5 0]
    (1(2), Gen, Misc, "gen"),
    /// Randomly reorder the rows of an array with a seed
    ///
    /// ex: deal0 [1 2 3 4 5]
    /// ex: deal5 [1_2 3_4 5_6 7_8]
    /// If you don't care about a seed, just seed with [random].
    /// ex: deal⚂ [1 2 3 4 5]
    /// ex: deal⚂ [1_2 3_4 5_6 7_8]
    (2, Deal, Misc, "deal"),
    /// Match a regex pattern
    ///
    /// Returns a rank-2 array of [box]ed strings, with one string per matching group and one row per match
    /// ex: regex "h([io])" "hihaho"
    /// ex: regex "hi" "dog"
    ///   : △.
    /// ex: regex "[a-z]+" "hello world"
    /// Escaped regex characters must be double-escaped.
    /// ex: regex "\\d+" "123"
    /// ex: P ← $"(\\d{_})"
    ///   : regex $"_-_-_"P3P3P4 "123-456-7890"
    /// Regex patterns with optional captures can be used with [fill].
    /// ex: ⬚(□"")regex "a(b)?" "a ab"
    ///
    /// Uiua uses the [Rust regex crate](https://docs.rs/regex/latest/regex/) internally.
    (2, Regex, Misc, "regex"),
    /// Convert a string to UTF-8 bytes
    ///
    /// ex: utf "hello!"
    /// ex: utf "❤️"
    /// You can use [un] to convert UTF-8 bytes back to a string.
    /// ex: °utf [226 156 168 32 119 111 119 33]
    ///
    /// [utf] is different from just [add]ing or [subtracting] `@\0`.
    /// Character math can only convert to and from UTF-32.
    /// ex: -@\0 "👩🏽‍👩🏻‍👦🏻‍👧🏽"
    /// ex: utf "👩🏽‍👩🏻‍👦🏻‍👧🏽"
    (1, Utf, Misc, "utf"),
    /// Generate a unique tag
    ///
    /// Tags are just numbers and are unique across multiple threads, but not across multiple runs.
    /// ex: [⍥tag5]
    ///   : [⍥tag5]
    (0, Tag, Misc, "tag"),
    /// Check the type of an array
    ///
    /// `0` indicates a number array.
    /// `1` indicates a complex array.
    /// `2` indicates a character array.
    /// `3` indicates a box array.
    /// ex: type 5
    /// ex: type i
    /// ex: type "hello"
    /// ex: type □[5 6]
    /// ex: ∵ type    {10 "dog" [1 2 3]}
    ///   : ∵(type°□) {10 "dog" [1 2 3]}
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
    ///
    /// ex: ∞
    /// ex: +1 ∞
    /// ex: -1 ∞
    /// ex: ↧5 ∞
    /// ex: ↥5 ∞
    (0, Infinity, Constant, ("infinity", '∞')),
    /// Create a hashmap from lists of keys and values
    ///
    /// A hashmap is a normal array that is used as a mapping from keys to values.
    /// The related map functions [insert], [has], [get], and [remove] all treat the array as an actual hashmap, so they have O(1) amortized time complexity.
    ///
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : map {"Alice" "Bob" "Carol"} [3_8 12_2 4_5]
    /// The `…n` annotion in the output indicates the number of empty entries in the hashmap.
    ///
    /// Use [get] to get the value corresponding to a key.
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : get 2 .
    /// Use [insert] to insert additional key-value pairs.
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : insert 5 6
    ///
    /// A box array `{}` works as an empty map.
    /// You can use [un][map] to get the keys and values back.
    /// ex: # Experimental!
    ///   : {}
    ///   : insert 1 2_3
    ///   : insert 4 5_6
    ///   : insert 7 8_9
    ///   : °map .
    ///
    /// Map array are just normal box arrays. Their shape is usually `[2]`; one element is the boxed keys, and the other is the boxed values.
    /// Performing non-map operations on a map array will work, but it will usually break the mapping.
    /// ex! # Experimental!
    ///   : map 1_2_3 4_5_6
    ///   : get 1 ⇌
    (2, Map, Map, "map"),
    /// Insert a key-value pair into a map array
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// The array is used as an actual hashmap, so some entries may be empty.
    /// ex: # Experimental!
    ///   : {}
    ///   : insert 1 2
    ///   : insert 3 4
    ///   : insert 5 6
    /// If the key is already present, it is replaced.
    /// ex: # Experimental!
    ///   : {}
    ///   : insert 1 2
    ///   : insert 3 4
    ///   : insert 3 5
    /// All keys (and all value) must have the same shape and type.
    /// ex! # Experimental!
    ///   : insert 1 2 {}
    ///   : insert "hi" "there"
    /// [box] keys or values if you need to.
    /// ex: # Experimental!
    ///   : insert □1 □2 {}
    ///   : insert □"hi" □"there"
    ///
    /// See also: [has], [get], [remove]
    (3, Insert, Map, "insert"),
    /// Check if a map array has a key
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : [fork(has 2|has 5)].
    ///
    /// See also: [insert], [get], [remove]
    (2, Has, Map, "has"),
    /// Get the value corresponding to a key in a map array
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : get 2 .
    /// If the key is not found, an error is thrown.
    /// ex! # Experimental!
    ///   : map 1_2 3_4
    ///   : get 5 .
    /// You can use [try] or [has] to avoid the error.
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : ⍣get0 5 .
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : (⋅⋅0|get) has,, 5 .
    /// You can provide a default value with [fill].
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : ⬚0get 1 .
    ///   : ⬚0get 5 :
    ///
    /// See also: [insert], [has], [remove]
    (2, Get, Map, "get"),
    /// Remove the value corresponding to a key from a map array
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// If the key is present, it is replaced with a tombstone NaN value.
    /// If the key is not present, the array is unchanged.
    /// ex: # Experimental!
    ///   : map 1_2 3_4
    ///   : remove 2 .
    ///   : remove 5 .
    ///
    /// See also: [insert], [has], [get]
    (2, Remove, Map, "remove"),
    /// Debug print all stack values without popping them
    ///
    /// This is equivalent to [dump][identity], but is easier to type.
    ///
    /// This is useful when you want to inspect the current ordering of the stack.
    /// For example, if you are juggling some values on the stack, you can use [stack] to inspect the stack afterwards:
    /// ex: 1 2 3
    ///   : ,,⊙.:
    ///   : ?
    ///   : +×-×+
    /// ex: 2_3_10 ? 17 ↯3_4⇡12
    ///   : ++
    (0(0), Stack, Stack, ("stack", '?')),
    /// Debug print the top value on the stack without popping it
    ///
    /// ex: ⸮[1 2 3]
    /// This is useful when you want to inspect an intermediate value.
    /// For example, let's say you are trying to find all the numbers in some range:
    /// ex: [1 5 2 9 11 0 7 12 8 3]
    ///   : ▽×≥5:≤10..
    /// `greater or equal` and `less or equal` each create a partial mask.
    /// To see them, use [trace].
    /// ex: [1 5 2 9 11 0 7 12 8 3]
    ///   : ▽×⸮≥5:⸮≤10..
    (1, Trace, Stack, ("trace", '⸮')),
    /// Debug print all the values currently on stack without popping them
    ///
    /// The function is used to preprocess the values before printing.
    /// [dump][identity] is equivalent to [stack].
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
    /// Errors encountered within [dump]'s function are caught and dumped as strings.
    /// ex: 1_2_3 4 5_6_7
    ///   : dump⊢
    (0(0)[1], Dump, Stack, "dump"),
    /// Stringify any value
    ///
    /// ex: repr π
    /// use [&p][repr] to produce a representation that can be pasted directly into the interpreter
    /// ex: &p repr ↯2_2_2 0
    /// ex: &p repr {"Uiua" @A [1 2 3] □4}
    /// [map]s need to me unmapped before calling [repr]
    /// ex: # Experimental!
    ///   : map {"Alice" "Bob" "Carol"} [3_8 12_2 4_5]
    ///   : &p repr{°map}
    (1, Repr, Misc, "repr"),
);

macro_rules! impl_primitive {
    ($(
        (
            $args:literal
            $(($outputs:expr))?
            $([$margs:expr])?,
            $variant:ident
        )
    ),* $(,)?) => {
        /// Primitives that exist as an implementation detail
        #[doc(hidden)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[serde(rename_all = "snake_case")]
        pub enum ImplPrimitive {
            $($variant,)*
            TransposeN(i32),
        }

        impl ImplPrimitive {
            pub fn args(&self) -> usize {
                match self {
                    $(ImplPrimitive::$variant => $args,)*
                    ImplPrimitive::TransposeN(_) => 1,
                }
            }
            pub fn outputs(&self) -> usize {
                match self {
                    $($(ImplPrimitive::$variant => $outputs,)?)*
                    _ => 1
                }
            }
            pub fn modifier_args(&self) -> Option<usize> {
                match self {
                    $($(ImplPrimitive::$variant => Some($margs),)?)*
                    _ => None
                }
            }
        }
    };
}

impl_primitive!(
    // Inverses
    (1, Asin),
    (1, Acos),
    (1, InverseBits),
    (1, InvWhere),
    (1(2), InvCouple),
    (1, InvUtf),
    (1(2), InvAtan),
    (1(2), InvComplex),
    (1, InvParse),
    (1, InvFix),
    (1[1], InvScan),
    (1(2), InvMap),
    (1, InvTrace),
    (0(0), InvStack),
    (0[1], InvDump),
    (1, Primes),
    (1, InvBox),
    (1(2), InvJoin),
    // Unders
    (3, Unselect),
    (3, Unpick),
    (3, Untake),
    (3, Undrop),
    (2, Unfirst),
    (2, Unlast),
    (3, Unkeep),
    (3, Unrerank),
    (2, Unreshape),
    (3(2), Unjoin),
    (3[1], Unpartition),
    (3[1], Ungroup),
    // Optimizations
    (1, Cos),
    (1, Last),
    (1, FirstMinIndex),
    (1, FirstMaxIndex),
    (1, LastMinIndex),
    (1, LastMaxIndex),
    (1, FirstWhere),
    (1, SortUp),
    (1, SortDown),
    (1[1], ReduceContent),
    (1, ReplaceRand),
    (2, ReplaceRand2),
);
