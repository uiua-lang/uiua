//! All primitive definitions

use std::path::{Path, PathBuf};

use crate::{Purity, WILDCARD_NAN};

use super::*;

/// The definition of a shadowable constant
pub struct ConstantDef {
    /// The constant's name
    pub name: &'static str,
    /// The constant's class
    pub class: ConstClass,
    /// The constant's value
    pub value: Lazy<ConstantValue>,
    /// The constant's documentation
    pub doc: Lazy<String>,
}

impl ConstantDef {
    /// Get the constant's documentation
    pub fn doc(&self) -> &str {
        &self.doc
    }
    /// Get the constant's documentation as fragments
    pub fn doc_frags(&self) -> Vec<PrimDocFragment> {
        parse_doc_line_fragments(self.doc())
    }
}

/// The value of a shadowable constant
pub enum ConstantValue {
    /// A static value that is always the same
    Static(Value),
    /// The music constant
    Music,
    /// The path of the current source file relative to the current working directory
    ThisFile,
    /// The name of the current source file
    ThisFileName,
    /// The name of the directory containing the current source file
    ThisFileDir,
    /// The compile-time working directory
    WorkingDir,
}

impl ConstantValue {
    /// Resolve the constant to a value
    pub(crate) fn resolve(
        &self,
        current_file_path: Option<&Path>,
        backend: &dyn SysBackend,
    ) -> Value {
        let current_file_path = current_file_path.map(|p| {
            let mut path = PathBuf::new();
            for comp in p.components() {
                path.push(comp);
            }
            path
        });
        match self {
            ConstantValue::Static(val) => val.clone(),
            ConstantValue::Music => {
                static MUSIC: OnceLock<Value> = OnceLock::new();
                MUSIC.get_or_init(|| music_constant(backend)).clone()
            }
            ConstantValue::ThisFile => {
                current_file_path.map_or_else(|| "".into(), |p| p.display().to_string().into())
            }
            ConstantValue::ThisFileName => current_file_path
                .and_then(|p| p.file_name().map(|f| f.to_string_lossy().into_owned()))
                .unwrap_or_default()
                .into(),
            ConstantValue::ThisFileDir => current_file_path
                .and_then(|p| p.parent().map(|f| f.display().to_string()))
                .unwrap_or_default()
                .into(),
            ConstantValue::WorkingDir => std::env::current_dir()
                .unwrap_or_default()
                .display()
                .to_string()
                .into(),
        }
    }
}

impl<T> From<T> for ConstantValue
where
    T: Into<Value>,
{
    fn from(val: T) -> Self {
        ConstantValue::Static(val.into())
    }
}

macro_rules! constant {
    ($($(#[doc = $doc:literal])+ ($(#[$attr:meta])* $name:literal, $class:ident, $value:expr)),* $(,)?) => {
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
        /// The list of all shadowable constants
        pub static CONSTANTS: [ConstantDef; COUNT] =
            [$(
                $(#[$attr])*
                ConstantDef {
                    name: $name,
                    value: Lazy::new(|| {$value.into()}),
                    class: ConstClass::$class,
                    doc: Lazy::new(|| {
                        let mut s = String::new();
                        $(
                            s.push_str($doc.trim());
                            s.push('\n');
                        )*
                        s.pop();
                        s
                    }),
                },
            )*];
    };
}

/// Kinds of shadowable constants
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstClass {
    Math,
    External,
    Time,
    Media,
    System,
    Color,
    Flags,
    Fun,
}

constant!(
    /// Euler's constant
    ("e", Math, std::f64::consts::E),
    /// The imaginary unit
    ("i", Math, crate::Complex::I),
    /// IEEE 754-2008's `NaN`
    ("NaN", Math, f64::NAN),
    /// The wildcard `NaN` value that equals any other number
    ("W", Math, WILDCARD_NAN),
    /// The maximum integer that can be represented exactly
    ("MaxInt", Math, 2f64.powi(53)),
    /// A string identifying the operating system
    ("Os", System, std::env::consts::OS),
    /// A string identifying family of the operating system
    ("Family", System, std::env::consts::FAMILY),
    /// A string identifying the architecture of the CPU
    ("Arch", System, std::env::consts::ARCH),
    /// The executable file extension
    ("ExeExt", System, std::env::consts::EXE_EXTENSION),
    /// The file extension for shared libraries
    ("DllExt", System, std::env::consts::DLL_EXTENSION),
    /// The primary path separator character
    ("Sep", System, std::path::MAIN_SEPARATOR),
    /// The path of the current source file relative to `WorkingDir`
    ("ThisFile", System, ConstantValue::ThisFile),
    /// The name of the current source file
    ("ThisFileName", System, ConstantValue::ThisFileName),
    /// The name of the directory containing the current source file
    ("ThisFileDir", System, ConstantValue::ThisFileDir),
    /// The compile-time working directory
    ("WorkingDir", System, ConstantValue::WorkingDir),
    /// The number of processors available
    ("NumProcs", System, num_cpus::get() as f64),
    /// A boolean `true` value for use in `json`
    ("True", External, Array::json_bool(true)),
    /// A boolean `false` value for use in `json`
    ("False", External, Array::json_bool(false)),
    /// A NULL pointer for use in `&ffi`
    ("NULL", External, Value::null()),
    /// The hexadecimal digits
    ("HexDigits", Math, "0123456789abcdef"),
    /// The days of the week
    (
        "Days",
        Time,
        [
            "Sunday",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday"
        ]
        .as_slice()
    ),
    /// The months of the year
    (
        "Months",
        Time,
        [
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"
        ]
        .as_slice()
    ),
    /// The number of days in each month in a non-leap year
    (
        "MonthDays",
        Time,
        [31u8, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    ),
    /// The number of days in each month in a leap year
    (
        "LeapMonthDays",
        Time,
        [31u8, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    ),
    /// The color white
    ("White", Color, [1.0, 1.0, 1.0]),
    /// The color black
    ("Black", Color, [0.0, 0.0, 0.0]),
    /// The color red
    ("Red", Color, [1.0, 0.0, 0.0]),
    /// The color orange
    ("Orange", Color, [1.0, 0.5, 0.0]),
    /// The color yellow
    ("Yellow", Color, [1.0, 1.0, 0.0]),
    /// The color green
    ("Green", Color, [0.0, 1.0, 0.0]),
    /// The color cyan
    ("Cyan", Color, [0.0, 1.0, 1.0]),
    /// The color blue
    ("Blue", Color, [0.0, 0.0, 1.0]),
    /// The color purple
    ("Purple", Color, [0.5, 0.0, 1.0]),
    /// The color magenta
    ("Magenta", Color, [1.0, 0.0, 1.0]),
    /// The planets of the solar system
    (
        "Planets",
        Fun,
        ["Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"].as_slice()
    ),
    /// The symbols of the zodiac
    (
        "Zodiac",
        Fun,
        [
            "Aries",
            "Taurus",
            "Gemini",
            "Cancer",
            "Leo",
            "Virgo",
            "Libra",
            "Scorpio",
            "Sagittarius",
            "Capricorn",
            "Aquarius",
            "Pisces"
        ]
        .as_slice()
    ),
    /// The suits of a standard deck of playing cards
    ("Suits", Fun, ['♣', '♦', '♥', '♠']),
    /// The ranks of a standard deck of playing cards
    (
        "Cards",
        Fun,
        ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"].as_slice()
    ),
    /// The symbols of the standard chess pieces
    (
        "Chess",
        Fun,
        Array::new(
            [2, 6],
            ['♟', '♜', '♞', '♝', '♛', '♚', '♙', '♖', '♘', '♗', '♕', '♔']
        )
    ),
    /// The phases of the moon
    ("Moon", Fun, "🌑🌒🌓🌔🌕🌖🌗🌘"),
    /// Skin color modifiers for emoji
    ("Skin", Fun, "🏻🏼🏽🏾🏿"),
    /// People emoji
    ("People", Fun, "👨👩👦👧"),
    /// Emoji hair components
    ("Hair", Fun, "🦰🦱🦲🦳"),
    /// The Uiua logo
    (#[cfg(feature = "image")] "Logo", Media, crate::encode::image_bytes_to_array(include_bytes!("assets/uiua-logo-512.png"), true).unwrap()),
    /// Ethically sourced Lena picture
    /// Morten Rieger Hannemose
    /// 2019
    /// https://mortenhannemose.github.io/lena/
    (#[cfg(feature = "image")] "Lena", Media, crate::encode::image_bytes_to_array(include_bytes!("assets/lena.jpg"), false).unwrap()),
    /// A picture of two cats
    ///
    /// Their names are Murphy and Louie
    (#[cfg(feature = "image")] "Cats", Media, crate::encode::image_bytes_to_array(include_bytes!("assets/cats.webp"), false).unwrap()),
    /// Sample music data
    ("Music", Media, ConstantValue::Music),
    /// Lorem Ipsum text
    ("Lorem", Media, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
    /// Gay flag colors
    ("Gay", Flags, [[0.894, 0.012, 0.012], [1.0, 0.647, 0.173], [1.0, 1.0, 0.255], [0.0, 0.502, 0.094], [0.0, 0.0, 0.976], [0.525, 0.0, 0.49]]),
    /// Lesbian flag colors
    ("Lesbian", Flags, [[0.831, 0.173, 0.0], [0.992, 0.596, 0.333], [1.0, 1.0, 1.0], [0.82, 0.38, 0.635], [0.635, 0.004, 0.38]]),
    /// Bi flag colors
    ("Bi", Flags, [[0.839, 0.008, 0.439], [0.839, 0.008, 0.439], [0.608, 0.31, 0.588], [0.0, 0.22, 0.659], [0.0, 0.22, 0.659]]),
    /// Trans flag colors
    ("Trans", Flags, [[0.357, 0.808, 0.98], [0.961, 0.663, 0.722], [1.0, 1.0, 1.0], [0.961, 0.663, 0.722], [0.357, 0.808, 0.98]]),
    /// Pan flag colors
    ("Pan", Flags, [[1.0, 0.129, 0.549], [1.0, 0.847, 0.0], [0.129, 0.694, 1.0]]),
    /// Ace flag colors
    ("Ace", Flags, [[0.0, 0.0, 0.0], [0.639, 0.639, 0.639], [1.0, 1.0, 1.0], [0.502, 0.0, 0.502]]),
    /// Aro flag colors
    ("Aro", Flags, [[0.0, 0.0, 0.0], [0.663, 0.663, 0.663], [1.0, 1.0, 1.0], [0.655, 0.827, 0.475], [0.239, 0.647, 0.259]]),
    /// Aroace flag colors
    ("AroAce", Flags, [[0.937, 0.565, 0.027], [0.965, 0.827, 0.09], [1.0, 1.0, 1.0], [0.271, 0.737, 0.933], [0.118, 0.247, 0.329]]),
    /// Enby flag colors
    ("Enby", Flags, [[0.988, 0.957, 0.204], [1.0, 1.0, 1.0], [0.612, 0.349, 0.82], [0.173, 0.173, 0.173]]),
    /// Genderfluid flag colors
    ("Fluid", Flags, [[1.0, 0.463, 0.643], [1.0, 1.0, 1.0], [0.753, 0.067, 0.843], [0.0, 0.0, 0.0], [0.184, 0.235, 0.745]]),
    /// Genderqueer flag colors
    ("Queer", Flags, [[0.71, 0.494, 0.863], [1.0, 1.0, 1.0], [0.29, 0.506, 0.137]]),
    /// Agender flag colors
    ("Agender", Flags, [[0.0; 3], [0.74, 0.77, 0.78], [1.0; 3], [0.72, 0.96, 0.52], [1.0; 3], [0.74, 0.77, 0.78], [0.0; 3],]),
    /// All pride flags
    ("PrideFlags", Flags, {
        CONSTANTS
            .iter()
            .skip_while(|def| def.name != "Gay")
            .take_while(|def| def.name != "PrideFlags")
            .map(|def| match &*def.value {
                ConstantValue::Static(val) => val.clone(),
                _ => unreachable!()
            })
            .map(Boxed).collect::<Array<Boxed>>()
    }),
    /// All pride flag names
    ("PrideFlagNames", Flags, {
        CONSTANTS
            .iter()
            .skip_while(|def| def.name != "Gay")
            .take_while(|def| def.name != "PrideFlags")
            .map(|def| def.name.to_string())
            .collect::<Value>()
    }),
);

fn music_constant(backend: &dyn SysBackend) -> Value {
    const TEMPO: f64 = 128.0;
    const BEAT: f64 = 60.0 / TEMPO;
    const C4: f64 = 261.63;
    const B3: f64 = 246.94;
    const G3: f64 = 196.00;
    const E3: f64 = 164.81;
    const C2: f64 = 65.41;
    const D2: f64 = 73.42;
    const E2: f64 = 82.41;
    const G2: f64 = 98.00;
    #[rustfmt::skip]
    let down = [
        C4, C4, C4, C4, B3, B3, B3, B3, G3, G3, G3, G3, E3, E3, E3, E3,
        C4, C4, B3, B3, G3, G3, E3, E3, C4, C4, B3, B3, G3, G3, E3, E3,
        C4, C4, C4, C4, B3, B3, B3, B3, G3, G3, G3, G3, E3, E3, E3, E3,
        C4, B3, G3, E3, C4, B3, G3, E3, C4, B3, G3, E3, C4, B3, G3, E3,
    ];
    #[rustfmt::skip]
    let up = [
        E3, G3, B3, C4, E3, G3, B3, C4, E3, G3, B3, C4, E3, G3, B3, C4,
        E3, E3, E3, E3, G3, G3, G3, G3, B3, B3, B3, B3, C4, C4, C4, C4,
        E3, E3, G3, G3, B3, B3, C4, C4, E3, E3, G3, G3, B3, B3, C4, C4,
        E3, E3, E3, E3, G3, G3, G3, G3, B3, B3, B3, B3, C4, C4, C4, C4,
    ];
    let mut melody = Vec::with_capacity(down.len() * 2 + up.len() * 2);
    melody.extend(down);
    // melody.extend(down);
    melody.extend(up);
    // melody.extend(up);
    let harmony = [C2, D2, E2, G2];
    let mut hat_mask = Vec::new();
    let mut hat_bits: u64 = 0xbeef_babe;
    for _ in 0..32 {
        hat_mask.push((hat_bits & 1) as f64);
        hat_bits >>= 1;
    }
    let mut rng = SmallRng::seed_from_u64(0);
    let sr = backend.audio_sample_rate();
    (0..(BEAT * 2.0 * 16.0 * sr as f64) as usize)
        .map(|s| {
            let secs = s as f64 / sr as f64;
            let beat = secs / BEAT;

            let m = melody[(4.0 * beat) as usize % melody.len()];
            let h = harmony[(beat / 4.0) as usize % harmony.len()];

            let m = (1.0 - (m * secs % 1.0) * 2.0) / 3.0; // Saw wave
            let h = if (h * secs % 1.0) < 0.5 { 1.0 } else { -1.0 } / 3.0; // Square wave
            let kick = ((secs % BEAT).powf(0.4) * 40.0 * TAU).sin();
            let hat = 0.3
                * rng.gen_range(-1.0..=1.0)
                * hat_mask[(4.0 * beat) as usize % 32]
                * (0.0..=0.1).contains(&(secs % (BEAT / 4.0) / (BEAT / 4.0))) as u8 as f64;
            let snare = 0.5
                * rng.gen_range(-1.0..=1.0)
                * ((0.5..=0.6).contains(&(secs % (2.0 * BEAT) / (2.0 * BEAT))) as u8 as f64);

            0.5 * (m + h + kick + hat + snare)
        })
        .collect::<EcoVec<_>>()
        .into()
}

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
            $variant:ident, $class:ident, $names:expr $(,$purity:ident)*
        )
    ),* $(,)?) => {
        /// A built-in function
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence, Serialize, Deserialize)]
        #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
        #[allow(rustdoc::broken_intra_doc_links)]
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
            /// Whether the primitive is pure
            pub fn purity(&self) -> Purity {
                match self {
                    $($(Primitive::$variant => Purity::$purity,)*)*
                    Primitive::Sys(op) => op.purity(),
                    _ => Purity::Pure
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
    /// For example, maybe you want to find all the numbers in an array that lie within a certain range.
    /// Here, we use [multiply] as a logical AND function.
    /// ex: ×≥5:≤8. [6 2 5 9 6 5 0 4]
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
    /// Duplicate the top value on the stack to the third-to-top position
    ///
    /// Formats from `'`.
    ///
    /// ex: # Experimental!
    ///   : [’ 1 2 3]
    /// This can be usful when used with [both] or [bracket] to combine one array with each of two others.
    /// ex: # Experimental!
    ///   : [∩+’] 10 2 5
    /// ex: # Experimental!
    ///   : [⊓+×’] 10 2 5
    (2(3), Around, Stack, ("around", AsciiToken::Quote, '’')),
    /// Swap the top two values on the stack
    ///
    /// ex: [: 1 2 3 4 5]
    ///
    /// When combined with [duplicate], you can apply two different functions to the same value.
    /// If you have two functions `f` and `g`, the pattern `f``flip``g``duplicate` will call both functions on the top value.
    /// This is a very common pattern.
    /// For example, maybe you want to find all the uppercase letters in a string.
    /// ex: $ Characters On uppercase OnLy
    ///   : ▽ ×≥@A:≤@Z. .
    (2(2), Flip, Stack, ("flip", AsciiToken::Colon, ':')),
    /// Discard the top stack value
    ///
    /// ex: [◌ 1 2 ◌ 3 4]
    /// This is usually used to discard values that are no longer needed.
    ///
    /// [un][pop] can be used to retrieve the [fill] value.
    /// ex: ⬚3(+°◌°◌)
    (1(0), Pop, Stack, ("pop", '◌')),
    /// Do nothing with one value
    ///
    /// ex: ∘ 5
    ///
    /// [identity] is mostly useless on its own. See the [Advanced Stack Manipulation Tutorial](/tutorial/advancedstack) to understand what it is for.
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
    ///
    /// [sign] also works on characters to get their case.
    /// - `¯1` for lowercase
    /// - `1` for uppercase
    /// - `0` for caseless
    /// ex: ± "Hello, World!"
    (1, Sign, MonadicPervasive, ("sign", '±')),
    /// Negate a number
    ///
    /// Formats from `\``.
    ///
    /// ex: ¯ 1
    /// ex: ¯ ¯3
    /// ex: ¯ [1 2 ¯3]
    /// ex: ¯ ℂ3 5
    ///
    /// [negate] also works on characters to toggle their case.
    /// ex: ¯ "Hello, World!"
    /// Use this with [absolute value] to lowercase a string.
    /// ex: ¯⌵ "Hello, World!"
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
    /// [absolute value] works on characters to uppercase them.
    /// ex: ⌵ "Hello, World!"
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
    /// ex: ∿ 1
    /// You can get a cosine function by [add]ing [eta].
    /// ex: ∿+η 1
    /// You can get an arcsine function with [un].
    /// ex: °∿ 1
    /// You can get an arccosine function by [subtract]ing the arcsine from [eta].
    /// ex: -:η°∿ 0
    /// You can get a tangent function by [divide]ing the [sine] by the cosine.
    /// ex: ÷∩∿+η. 0
    (1, Sin, MonadicPervasive, ("sine", '∿')),
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
    /// Numbers with fraction `0.5` always round away from zero.
    /// ex: ⁅1.5
    /// ex: ⁅[0.1 π 2 9.9 7.5]
    /// ex: ⁅[4/3_¯2.5 9.81_¯3.6]
    (1, Round, MonadicPervasive, ("round", '⁅')),
    /// Compare for equality
    ///
    /// ex: =1 2
    /// ex: =5 5
    /// ex: =1 [1 2 3]
    /// ex: = [1 2 2] [1 2 3]
    /// Boxes compare lexicographically
    /// ex: = {1 2_3 4_5_6} {1_2 3 4_5_6}
    (2, Eq, DyadicPervasive, ("equals", AsciiToken::Equal, '=')),
    /// Compare for inequality
    ///
    /// Formats from `!=`.
    ///
    /// ex: ≠1 2
    /// ex: ≠5 5
    /// ex: ≠1 [1 2 3]
    /// ex: ≠ [1 2 2] [1 2 3]
    /// Boxes compare lexicographically
    /// ex: ≠ {1 2_3 4_5_6} {1_2 3 4_5_6}
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
    /// Boxes compare lexicographically
    /// ex: < {1 2_3 4_5_6} {1_2 3 4_5_6}
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
    /// Boxes compare lexicographically
    /// ex: ≤ {1 2_3 4_5_6} {1_2 3 4_5_6}
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
    /// Boxes compare lexicographically
    /// ex: > {1 2_3 4_5_6} {1_2 3 4_5_6}
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
    /// Boxes compare lexicographically
    /// ex: ≥ {1 2_3 4_5_6} {1_2 3 4_5_6}
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
    /// ex: × [¯1 0 1] "hey"
    ///
    /// Uiua does not have dedicated boolean logical operators.
    /// [multiply] can be used as a logical AND.
    /// ex: ×,,⊓≥≤5,8 . [6 2 5 9 6 5 0 4]
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
    /// ex: ÷ [¯1 0 1] "hey"
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
    ///
    /// The result is always non-negative:
    /// ex: ◿ 4 ¯21
    /// If you prefer the negative modulo instead of the remainder, you may use [under]:
    /// ex: ⍜⊙⌵◿ 4 ¯21
    (2, Modulus, DyadicPervasive, ("modulus", '◿')),
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
    /// Boxes compare lexicographically
    /// ex: ↧ {1_2_3 "dog"} {1_4_5 "cat"}
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
    /// Boxes compare lexicographically
    /// ex: ↥ {1_2_3 "dog"} {1_4_5 "cat"}
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
    ///
    /// [un][atangent] gives the [sine] and `cosine` of an angle.
    /// ex: °∠ 0
    /// ex: °∠ η
    /// ex: °∠ π
    /// ex: °∠ ÷3π
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
    /// A complex number [equals] a real one if the imaginary part is 0 and the real parts [match].
    /// ex: = 5 ℂ0 5
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
    /// [un][shape] creates an array of incrementing elements with the given shape.
    /// ex: °△ 2_3_4
    ///
    /// It is a triangle`△` because a triangle is a shape.
    (1, Shape, MonadicArray, ("shape", '△')),
    /// Make an array of all natural numbers less than a number
    ///
    /// The rank of the input must be `0` or `1`.
    /// ex: ⇡5
    /// ex: ⇡2_3
    /// ex: ⇡[3]
    ///
    /// When creating ranges with upper bounds that are rank `1`, [pick]ing the generated range array from an array with the [shape] of the input will yield that array.
    /// ex:     [1_2_3 4_5_6]
    ///   :    △[1_2_3 4_5_6]
    ///   :   ⇡△[1_2_3 4_5_6]
    ///   : ⊡⇡△.[1_2_3 4_5_6]
    ///
    /// Taking the [range] of a negative number will yield a decreasing sequence starting at `¯1`.
    /// ex: ⇡¯5
    /// [pick]ing from an array with the [range] of its [negate]d [shape] will reverse all elements.
    /// ex:       [1_2_3 4_5_6]
    ///   : ⊡⇡¯△. [1_2_3 4_5_6]
    ///   :  ⍜♭⇌  [1_2_3 4_5_6]
    (1, Range, MonadicArray, ("range", '⇡')),
    /// Get the first row of an array
    ///
    /// ex: ⊢1_2_3
    /// ex: ⊢[1_2 3_4 5_6]
    /// ex: ⊢1
    /// ex! ⊢[]
    ///
    /// [first][reverse] is optimized in the interpreter to be O(1).
    /// ex: ⊢⇌ [1 8 4 9 2 3]
    (1, First, MonadicArray, ("first", '⊢')),
    /// Get the last row of an array
    ///
    /// ex: # Experimental!
    ///   : ⊣1_2_3
    /// ex: # Experimental!
    ///   : ⊣[1_2 3_4 5_6]
    /// ex: # Experimental!
    ///   : ⊣1
    /// ex! # Experimental!
    ///   : ⊣[]
    (1, Last, MonadicArray, ("last", '⊣')),
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
    /// See the [Advanced Array Manipulation Tutorial](/tutorial/advancedarray) for more information on this use case.
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
    ///
    /// [under][bits] can be used to perform bit-wise operations.
    /// ex: ⍜⋯(¬⬚0↙8) 5
    (1, Bits, MonadicArray, ("bits", '⋯')),
    /// Rotate the shape of an array
    ///
    /// ex: ⍉.[1_2 3_4 5_6]
    /// ex: ⍉.[[1_2 3_4] [5_6 7_8]]
    /// [transpose] works through boxes.
    /// ex: ⍉ □[1_2_3 4_5_6]
    /// ex: ≡⍉ {[1_2 3_4] [1_2_3 4_5_6]}
    /// [un][transpose] transposes in the opposite direction.
    /// This is useful for arrays with rank `greater than``2`.
    /// ex: °⍉ .⊟.[1_2_3 4_5_6]
    ///
    /// `shape``transpose` is always equivalent to `rotate``1``shape`.
    /// ex: [1_2 3_4 5_6]
    ///   : ↻1△ .
    ///   : △⍉  :
    ///
    /// Multiple [transpose]s, as well as [rows][transpose], are optimized in the interpreter to only do a single operation.
    (1, Transpose, MonadicArray, ("transpose", '⍉')),
    /// Sort an array
    ///
    /// ex: # Experimental!
    ///   : ⍆ [3 9 1 8 2 7]
    /// ex: # Experimental!
    ///   : ⍆ "uiua"
    /// Multidimensional arrays have their rows sorted lexicographically.
    /// ex: # Experimental!
    ///   : ⍆ . [1_5_3 4_3_2 1_5_2]
    /// If you want to sort by some key rather than the data itself, use [rise] or [fall].
    (1, Sort, MonadicArray, ("sort", '⍆')),
    /// Get the indices into an array if it were sorted ascending
    ///
    /// The [rise] of an array is the list of indices that would sort the array ascending if used with [select].
    /// ex: ⍏ 6_2_7_0_¯1_5
    /// Using the [rise] as a selector in [select] yields the sorted array.
    /// ex: ⊏⍏. 6_2_7_0_¯1_5
    /// ex: ⊏⊸⍏ 6_2_7_0_¯1_5
    /// If we transform the array before [rise]ing, we can sort by a key.
    /// Here, we sort the array ascending by the [absolute value] of its elements.
    /// ex: ⊏⍏⌵. 6_2_7_0_¯1_5
    ///
    /// [first][rise] and [first][reverse][rise] are optimized in the interpreter to be O(n).
    (1, Rise, MonadicArray, ("rise", '⍏')),
    /// Get the indices into an array if it were sorted descending
    ///
    /// The [fall] of an array is the list of indices that would sort the array descending if used with [select].
    /// ex: ⍖ 6_2_7_0_¯1_5
    /// Using the [fall] as a selector in [select] yields the sorted array.
    /// ex: ⊏⍖. 6_2_7_0_¯1_5
    /// ex: ⊏⊸⍖ 6_2_7_0_¯1_5
    /// If we transform the array before [fall]ing, we can sort by a key.
    /// Here, we sort the array descending by the [absolute value] of its elements.
    /// ex: ⊏⍖⌵. 6_2_7_0_¯1_5
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
    /// Assign a unique index to each unique row in an array
    ///
    /// ex: ⊛7_7_8_0_1_2_0
    /// ex: ⊛"Hello, World!"
    ///
    /// When combined with [group], you can do things like counting the number of occurrences of each character in a string.
    /// ex: $ Count the characters in this string
    ///   : ⊕($"_ _"⊃⊢⧻) ⊛.⊏⍏.
    (1, Classify, MonadicArray, ("classify", '⊛')),
    /// Remove duplicate rows from an array
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
    /// [keep][by][unique] is equivalent to [deduplicate].
    /// ex: ▽⊸◰ 7_7_8_0_1_2_0
    /// [unique] is mainly useful for deduplicating by a certain property.
    /// Here, we deduplicate by the [absolute value] of the elements.
    /// ex: ▽◰⊸⌵ [1 ¯2 ¯5 2 3 1 5]
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
    /// ex: ⊢  □[1 2 3]
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
    ///   : ≡◇/+
    /// This is the main way to [join] a list of [box]ed strings.
    /// ex: /◇⊂       {"Join" "these" "strings"}
    /// ex: /◇(⊂⊂:@ ) {"Join" "these" "strings"}
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
    ///
    /// [parse] accepts both `a+bi` and `arbi` formats, [un][parse] returns a string using the latter.
    /// ex: ∩⋕ "8+3i" "8-3i"
    /// ex: ∩⋕ "8r3i" "8r-3i"
    /// ex: °⋕ ℂ3 8
    ///
    /// [fill][parse] sets a default value for failed parses.
    /// ex: ⬚5⋕ {"13" "124" "not a number"}
    /// [fill][un][parse] pads the strings to make a character array instead of a box array.
    /// ex: ⬚@ °⋕ +9÷4⇡10
    /// ex: ⬚@0°⋕ +9÷4⇡10
    (1, Parse, Misc, ("parse", '⋕')),
    /// Check if two arrays are exactly the same
    ///
    /// ex: ≍ 1_2_3 [1 2 3]
    /// ex: ≍ 1_2_3 [1 2]
    ///
    /// Although one number [equals] another, they may not [match] if they have different [type]s
    /// ex: = 5 ℂ0 5
    ///   : ≍ 5 ℂ0 5
    (2, Match, DyadicArray, ("match", '≍')),
    /// Combine two arrays as rows of a new array
    ///
    /// [length] of the coupled array will *always* be `2`.
    ///
    /// For scalars, it is equivalent to [join].
    /// ex: ⊟ 1 2
    ///   : ⊂ 1 2
    /// For arrays, a new array is created with the first array as the first row and the second array as the second row.
    /// ex: ⊟ [1 2 3] [4 5 6]
    /// [un][couple] uncouples a [length] `2` array and pushes both rows onto the stack.
    /// ex: °⊟ .[1_2_3 4_5_6]
    /// ex: °⊟ [1_2 3_4]
    ///
    /// If one array's shape is a suffix of the other's, the smaller array will be repeated to match the shape of the larger array.
    /// ex: ⊟ [1 2 3] 4
    /// ex: ⊟ [1_2 3_4] 5
    /// ex: ⊟ [1_2 3_4] 5_6
    ///
    /// By default, arrays with different shape suffixes cannot be [couple]d.
    /// ex! ⊟ [1 2 3] [4 5]
    /// Use [fill] to make their shapes match
    /// ex: ⬚∞⊟ [1 2 3] [4 5]
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
    /// If the arrays have a rank difference of 2 or more, then the array with the smaller rank will be repeated as rows to match the rank of the other.
    /// This still requires the shape of the smaller array to be a suffix of the shape of the larger array.
    /// ex: ⊂ 0 [1_2 3_4]
    /// ex: ⊂ 1_2 [[3_4 5_6] [7_8 9_10]]
    ///
    /// By default, arrays that do not have equal [shape] suffixes cannot be [join]ed.
    /// ex! ⊂ [1_2 3_4] [5_6_7 8_9_10]
    /// Use [fill] to make their shapes compatible.
    /// ex: ⬚0⊂ [1_2 3_4] [5_6_7 8_9_10]
    ///
    /// [un][join] splits the first row of the array from the rest.
    /// ex: °⊂ [1 2 3 4]
    /// ex: °⊂ [1_2 3_4 5_6]
    ///
    /// [join]ing to the front of an array is a bit slower than [join]ing to the back because it requires all the existing rows to be shifted.
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
    ///
    /// [un][select] is equivalent to [range][length][duplicate]. This is a common way to enumerate the indices of the rows an array.
    /// ex: °⊏ "hello!"
    /// ex: °⊏ {1 2_3 4_5_6}
    ///
    /// [under][select] can be used to modify, replace, insert, or delete the rows of an array.
    /// ex: ⍜⊏(×10) 1_5 ⇡10
    /// ex: ⍜⊏⋅π 1_5 ⇡10
    /// ex: ⍜⊏⋅η_τ 1_5 ⇡10
    /// ex: ⍜⊏≡⋅[] 1_5 ⇡10
    /// ex: ⍜⊏≡⋅η_τ_π 1_5 ⇡10
    ///
    /// [anti][select] puts the rows of an array at their corresponding indices. This requires a [fill] value if not all indices are present.
    /// ex: ⌝⊏ 3_1_2_0 "abcd"
    /// ex: ⬚@-⌝⊏ 1_2_5 "abc"
    /// ex: ⬚@.⌝⊏ [1_5 7_2] ["ab" "cd"]
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
    /// [un][pick] is equivalent to [range][shape][duplicate]. This is a common way to enumerate the indices of the elements of an array.
    /// ex: °⊡ "hello!"
    /// ex: °⊡ ["ab" "cd"]
    ///
    /// [under][pick] can be used to modify or replace the value at an index.
    /// ex: ⍜⊡(×10) 2 [8 3 9 2 0]
    /// This works with multiple and/or deeper indices.
    /// ex: ⍜⊡(×10) [2_1 0_2] +1↯3_4⇡12
    /// To simply set a value, you can use [under][pick][pop].
    /// ex: ⍜⊡◌ 2 [8 3 9 2 0] 42
    /// Or [under][pick][gap] if the replacement is static.
    ///
    /// [anti][pick] puts the values of an array at their corresponding indices. This requires a [fill] value if not all indices are present.
    /// ex: ⬚0⌝⊡ 1_2 5
    /// ex: ⬚0⌝⊡ 1_1 1_2
    /// ex: ⬚@-⌝⊡ [1_2 3_4] "ab"
    /// ex: ⌝⊡ [1_0 0_0 1_1 0_1] "abcd"
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
    /// This is in contrast to scalar [keep], which repeats each row but preserves rank.
    /// ex: ▽ 4 [1 2 3 4 5]
    /// ex: ▽ 2 [1_2_3 4_5_6]
    ///
    /// [fill][reshape] fills in the shape with the fill element instead of cycling the data.
    /// ex:   ↯ 3_5 ⇡9
    ///   : ⬚0↯ 3_5 ⇡9
    ///
    /// At most one of the dimensions of the new shape may be [infinity]. This indicates that this is a *derived* dimension, and it will be calculated to make the total number of elements in the new shape be `less or equal` the total number of elements in the original shape.
    /// ex: ↯5_∞ ⇡15
    /// ex: ↯∞_5 ⇡15
    /// ex: ↯2_2_∞ ⇡15
    /// ex: ↯∞_2_2 ⇡15
    /// ex: ↯3_∞_5 ⇡30
    /// If [fill] is used, the total number of elements in the new shape will always be `greater or equal` the total number of elements in the original shape.
    /// ex: ⬚0↯ ∞_5 ⇡12
    ///
    /// [under][shape] will [reshape] the array as an inverse.
    /// ex: ⍜△⇌. ↯2_3_4⇡24
    ///
    /// Negative axes in the shape will reverse the corresponding axes of the array.
    /// ex: ↯[¯3] 1_2_3
    /// ex: ↯2_3_4⇡24
    ///   : ⍜△⍜(⊏0_2)¯
    /// ex: ↯¯3 [1 2 3 4]
    /// ex: ↯¯∞ [1 2 3 4 5]
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
    /// ex: ⍜(☇1)≡□ ↯2_3_3⇡18
    /// ex: ⍜☇≡□  2 ↯2_3_3⇡18
    (2, Rerank, DyadicArray, ("rerank", '☇')),
    /// Take the first n rows of an array
    ///
    /// This is the opposite of [drop].
    ///
    /// ex: ↙ 3 [8 3 9 2 0]
    /// ex: ↙ 2 ↯3_3⇡9
    /// Negative amounts take from the end.
    /// ex: ↙ ¯3 [8 3 9 2 0]
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
    /// This works with negative values as well.
    /// ex: ⬚π↙ ¯7 [8 3 9 2 0]
    ///
    /// [infinity] can be used to take every row along an axis.
    /// ex: ↯2_3_4⇡24
    ///   : ↙¯1_∞_2.
    (2, Take, DyadicArray, ("take", '↙')),
    /// Drop the first n rows of an array
    ///
    /// This is the opposite of [take].
    ///
    /// ex: ↘ 3 [8 3 9 2 0]
    /// ex: ↘ 2 ↯3_3⇡9
    /// Negative amounts drop from the end.
    /// ex: ↘ ¯3 [8 3 9 2 0]
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
    ///
    /// [un] can be used with [drop] to pad an array. Because this takes the same number of arguments as [drop], [on] (and likely [pop]) are required to make the inverse correct.
    /// By default, the pad value is a "zero element" of the array's type.
    /// - For number arrays, it is `0`.
    /// - For character arrays, it is `@ ` (space).
    /// - For complex arrays, it is `0ℂ`.
    /// - For box arrays, it is `⟦⟧`.
    /// A scalar first argument will pad the first axis of the array on both sides.
    /// ex: ◌°⟜↘ 2 [1 2 3]
    /// ex: ◌°⟜↘ ¯2 [1 2 3]
    /// ex: ◌°⟜↘ 3 "Hello!"
    /// ex: ◌°⟜↘ 1 [1_2 3_4]
    /// [fill] can be used to set the fill value. Non-scalar fills are allowed if they are compatible with the array's shape.
    /// ex: ◌⬚10°⟜↘ 2 [1 2 3]
    /// ex: ◌⬚@-°⟜↘ 2 "abc"
    /// ex: ◌⬚10°⟜↘ 1 [1_2 3_4]
    /// ex: ◌⬚10_20°⟜↘ 1 [1_2 3_4]
    /// If the first argument is a list, each axis will be padded on both sides with the corresponding amount.
    /// ex: ◌°⟜↘ 1_2 [1_2 3_4]
    /// ex: ◌°⟜↘ 1_¯2 [1_2 3_4]
    /// ex: ◌°⟜↘ ¯1_2 +1°△2_2_4
    /// ex: ◌°⟜↘ ¯1_1_2 +1°△2_2_4
    /// ex: ◌°⟜↘ ¯1_0_2 +1°△2_2_4
    /// This can be good for padding images.
    /// ex: ⬚(⊂:1Purple)(◌°⟜↘¯°⟜↘) 20_20 Logo
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
    ///
    /// [rotate] works through boxes.
    /// ex: ↻1 □[1 2 3 4]
    /// ex: ≡↻1 {1_2_3 4_5_6}
    (2, Rotate, DyadicArray, ("rotate", '↻')),
    /// Change the order of the axes of an array
    ///
    /// The first argument is a list of unique axis indices.
    /// The corresponding axes of the array will be moved to the front of the array's shape.
    /// Positive indices start from the leading axis. Negative indices start from the trailing axis.
    /// ex: °△ 2_3_4
    ///   : ⤸ 1 .
    /// ex: △ ⤸ 2_1 °△ 2_3_4_5
    /// [orient]`¯1` is equivalent to [un][transpose].
    /// ex: °△ 2_3_4
    ///   : ∩△ ⊃°⍉(⤸¯1)
    ///
    /// [anti][orient] moves the axes *to* the given indices.
    /// ex: △  ⤸ 3_1 °△ 2_3_4_5
    ///   : △ ⌝⤸ 3_1 °△ 2_3_4_5
    /// This does not have the requirement that the indices be unique. Repeated indices will retrive the *diagonal* along those axes.
    /// ex: ⌝⤸ 0_0 . °△ 3_3
    /// ex: ⌝⤸ 0_0_0 . °△ 3_3_3
    /// ex: ⌝⤸ 0_0 °△ 3_3_3
    /// ex: ⌝⤸ 0_1_1 . °△ 2_2_2
    /// ex: ⌝⤸ 1_1_0 °△ 2_2_2
    /// ex: ⌝⤸ 1_0_1 °△ 2_2_2
    ///
    /// [un][orient] is equivalent to [range][length][shape][duplicate]. This is an easy way to enumerate the indices of the axes of an array.
    /// ex: °⤸ "hello!"
    /// ex: °⤸ ["ab" "cd"]
    /// ex: °⤸ [[1_2 3_4] [5_6 7_8]]
    (2, Orient, DyadicArray, ("orient", '⤸')),
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
    ///
    /// Usually, [windows] "materializes" the windows. This means that the windows are copied into a new array. While this is very general, it can be slow and wasteful if you end up immediately reducing the windows.
    /// For this reason, the pattern `rows``reduce``F``windows` is optimized for scalar window sizes to [reduce] windows as they are generated.
    /// ex: ≡/+◫ 5 [1 8 2 9 3 0 2 4 4 5 1] # Fast!
    ///
    /// You can use [fill] to pad the array with a value.
    /// This can be useful for things like convolutions.
    /// ex: +1↯2_3⇡6
    ///   : ⬚0◫2_3
    ///   : ≡≡□
    ///
    /// [windows] with a scalar or list window size will always produce overlapping windows that shift by one row at a time.
    /// 2-dimensional windows sizes allow more control over the windows.
    /// A rank-2 array with only one row will "chunk" the array with non-overlapping windows.
    /// ex: ◫[[4]] ⇡12
    /// ex: ◫¤¤4   ⇡12
    /// ex: ≡≡□ ◫¤[2 2] . °△4_6
    /// Negative sizes still specify the number of windows desired.
    /// ex: ◫¤¤¯4 ⇡12
    /// ex: ≡≡□ ◫¤[¯2 ¯2] . °△4_6
    /// A rank-2 array with two rows allows the "stride" of the windows to be specified.
    /// The first row specifies the window size, and the second row specifies the stride.
    /// ex: ◫[[3] [4]] ⇡12
    /// ex: ◫[[4] [2]] ⇡12
    /// ex: ≡≡□ ◫[2_2 1_3] . °△4_6
    (2, Windows, DyadicArray, ("windows", '◫')),
    /// Get the n-wise chunks of an array
    ///
    /// [chunks] produces non-overlapping subarrays of equal size.
    /// ex: # Experimental!
    ///   : ⑄ 3 [1 2 3 4 5 6]
    /// ex: # Experimental!
    ///   : ≡≡□ ⑄ 2_3 . °△ 4_6
    /// If the chunk size does not evenly divide the shape of the array, the edge chunks will be omitted.
    /// ex: # Experimental!
    ///   : ⑄ 3 [1 2 3 4 5 6 7 8]
    /// ex: # Experimental!
    ///   : ≡≡□ ⑄ 2_2 . °△ 5_5
    /// If [fill] is used, the edge chunks will be filled with the fill value.
    /// ex: # Experimental!
    ///   : ⬚0⑄ 3 [1 2 3 4 5 6 7 8]
    /// ex: # Experimental!
    ///   : ≡≡□ ⬚0⑄ 2_2 . °△ 5_5
    /// Negative chunk sizes specify the number of chunks desired.
    /// ex: # Experimental!
    ///   : ⑄ ¯2 [1 2 3 4 5 6]
    ///   : ⑄ ¯3 [1 2 3 4 5 6]
    /// Negative and positive chunk sizes can be mixed.
    /// ex: # Experimental!
    ///   : ≡≡□ ⑄ ¯2_2 . °△ 6_6
    (2, Chunks, DyadicArray, ("chunks", '⑄')),
    /// Discard or copy some rows of an array
    ///
    /// Takes two arrays. The first array is the number of copies to keep of each row of the second array.
    /// ex: ▽ [1 0 2 3 1] [8 3 9 2 0]
    ///
    /// By making the first array a mask derived from the second, [keep] becomes a filter.
    /// In this example, the input string is [duplicate]ed, and a mask is created from it using `greater or equal``@a`. Then, [keep] uses the mask to filter the string.
    /// ex: ▽≥@a . "lOWERCASe onLY"
    ///
    /// [keep] with a scalar for the first argument repeats each row of the second argument that many times.
    /// ex: ▽ 3 [1 2 3]
    /// ex: ▽ 2 [1_2_3 4_5_6]
    /// This is in contrast to scalar [reshape], which copies the array as rows of a new array.
    /// ex: ↯ 3 [1 2 3]
    /// ex: ↯ 2 [1_2_3 4_5_6]
    ///
    /// The counts list can be [fill]ed if it is shorter than the kept array.
    /// ex: ⬚3▽ [1 0 2] [8 3 9 2 0]
    /// The fill value may be a list, in which case it will be repeated.
    /// ex: ⬚[1 2 0]▽ [0] ⇡10
    ///
    /// [un][keep] splits an array into a counts list and an array with adjacent similar rows deduplicated.
    /// ex: °▽ "mississippi"
    ///
    /// A non-integer scalar count will either remove or duplicate rows at regular intervals.
    /// ex: ▽ 0.5 ⇡10
    /// ex: ▽ 1.5 ⇡10
    ///
    /// [under][keep] allows you to modify part of an array according to a mask.
    /// ex: ⍜▽(+1) =@s. "mississippi"
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
    ///
    /// If you want to mark the entire occurence, use [mask] instead.
    (2, Find, DyadicArray, ("find", '⌕')),
    /// Mask the occurences of one array in another
    ///
    /// Occurences of the first array in the second array will be marked with increasing numbers.
    /// While [find] only marks the start of each occurence, [mask] marks the entire occurence.
    /// ex: ⦷ "ab" "abracadabra"
    /// ex: ⦷ [1 2 3].[0 1 2 3 1 2 3 4 5 1 2 3 4 5 6]
    /// Increasing numbers are used so that adjacent occurences can be distinguished.
    /// An occurence that would overlap with a previous occurence is not marked.
    /// ex: ⦷ [3 4 3 4].[0 3 4 3 4 3 4 0 0 3 4 3 4 0]
    ///
    /// Arbitrary rank arrays are supported.
    /// The first array's rank must be `less or equal` the rank of the second.
    /// ex: ⦷,, 3_4 ↯2_3⇡6
    /// ex: ⦷,, [1_2 5_6] [1_2_3_4 5_6_1_2 7_8_5_6 4_3_1_2]
    ///
    /// [mask] works well with [partition] in a way that [find] does not.
    /// Here, we [not] the [mask] of a non-scalar delimiter to split a string.
    /// ex: ⊜∘ ¬⦷" - ". "foo - bar - baz"
    (2, Mask, DyadicArray, ("mask", '⦷')),
    /// Check if each row of one array exists in another
    ///
    /// The second argument is checked for membership in the first argument.
    /// ex: ∈ [1 2 3] 2
    /// ex: ∈ [1 2 3] 5
    /// ex: ∈ [0 3 4 5 1] [1 2 3]
    /// ex: ∈ [1_2_3 4_5_6] [4 5 6]
    /// ex: ∈ [3 4 5] [1_2_3 4_5_6]
    /// ex: ∈ [1_2_3 4_5_6] 2
    ///
    /// With the help of [keep], you can use [memberof] to get a set intersection.
    /// ex: ▽⊸∈ "abracadabra" "that's really cool"
    ///
    /// [memberof] is closely related to [indexof].
    (2, MemberOf, DyadicArray, ("memberof", '∈')),
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
    /// ex: ⊗ 2 [1 2 3]
    /// ex: ⊗ [4 5 6] [1_2_3 4_5_6]
    /// ex: ⊗ 2 [1_2_3 4_5_6]
    /// If the index cannot be found, the [length] of the searched-in array is returned.
    /// ex: ⊗ [1 2 3] [0 3 4 5 1]
    /// ex: ⊗ [1_2_3 4_5_6] [3 4 5]
    /// ex: ⊗ 5 [1 2 3]
    ///
    /// [fill] can be used to set the value of missing items.
    /// ex:   ⊗ [4 8 2 9 1] [1 2 3 4]
    ///   : ⬚∞⊗ [4 8 2 9 1] [1 2 3 4]
    ///
    /// You can use the returned indices with [select] to get the rows that were found.
    /// If you expect one of the searched-for rows to be missing, you can use [fill] to set a default value.
    /// ex: A ← [2 3 5 7 11 13]
    ///   : .⊗,A [1 2 3 4 5]
    ///   : ⬚∞⊏:A
    ///
    /// [indexof] is closely related to [memberof].
    (2, IndexOf, DyadicArray, ("indexof", '⊗')),
    /// Get the base digits of a number
    ///
    /// When passed a scalar number, [base] returns the base-N digits of the numbers in an array.
    /// Digits are always listed least-significant to most-significant.
    /// ex: # Experimental!
    ///   : base 10 123
    /// ex: # Experimental!
    ///   : base 2 10
    /// ex: # Experimental!
    ///   : base 16 256
    /// When passed an array of numbers, [base] treats each digit as having a different base.
    /// Any remainder will be truncated.
    /// ex: # Experimental!
    ///   : base [10 2] 35 # Truncated
    /// ex: # Experimental!
    ///   : base [60 60 24 365.25] now
    /// If you want to keep the remainder, use [infinity].
    /// ex: # Experimental!
    ///   : base [10 2 ∞] 35
    /// ex: # Experimental!
    ///   : base [60 60 24 365.25 ∞] now
    /// Non-integer bases are supported.
    /// ex: # Experimental!
    ///   : base π [η π τ]
    /// ex: # Experimental!
    ///   : base 1.5 [1 2 3 4 5]
    ///
    /// [base] is compatible with [under].
    /// ex: # Experimental!
    ///   : ⍜(°⍉base4|⬚0↙3) [10 100 1000]
    /// It can also be used with [anti] to convert digits in a certain base back to numbers.
    /// ex: # Experimental!
    ///   : ⌝base 2 [1 0 0 1 0]
    ///   : ⌝base 2 [1_0_0 0_1_1 1_1_1]
    ///   : ⌝base 10 [1 2 3]
    /// For a scalar base, this is equivalent to evaluating a polynomial.
    /// The polynomial x²-2x+1 could be represented like this:
    /// ex: # Experimental!
    ///   : ⌝base 0 [1 ¯2 1]
    ///   : ⌝base 1 [1 ¯2 1]
    ///   : ⌝base 2 [1 ¯2 1]
    (2, Base, DyadicArray, "base"),
    /// Get all combinations of `k` rows from an array
    ///
    /// The first argument must be a natural number.
    /// ex: # Experimental!
    ///   : choose 3 ⇡5
    /// ex: # Experimental!
    ///   : choose 4 "hello!"
    /// If the second argument is a scalar number, the number of combinations is returned.
    /// ex: # Experimental!
    ///   : choose 3 5
    ///
    /// See also: [permute]
    (2, Choose, DyadicArray, "choose"),
    /// Get all permutations of `k` rows from an array
    ///
    /// The first argument must be a natural number.
    /// ex: # Experimental!
    ///   : permute 3 ⇡4
    /// ex: # Experimental!
    ///   : permute 2 "hello!"
    /// If the second argument is a scalar number, the number of permutations is returned.
    /// ex: # Experimental!
    ///   : permute 3 5
    ///
    /// See also: [choose]
    (2, Permute, DyadicArray, "permute"),
    /// Apply a reducing function to an array
    ///
    /// For reducing with an initial value, see [fold].
    ///
    /// `reduce``add` sums the rows of an array.
    /// ex: /+ 1_2_3_4_5
    /// [reduce] goes from left to right. This is important for non-commutative functions like [subtract].
    /// ex: /- 1_2_3_4_5
    /// [reduce] works on arrays of arbitrary rank. The leading-axis rows will always be iterated over.
    /// ex: /+ . [1_2_3 4_5_6]
    /// ex: /+ . [[0_1 1_0] [2_0 0_0] [0_0 0_3]]
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
    ///
    /// If the function takes more than 2 arguments, additional arguments above the array on the stack will be passed to the function on every iteration. This is useful for things like interspersing one array between the rows of another.
    /// ex: /(⊂⊂) 0_1 [2 3 4 5]
    /// ex: /◇(⊂⊂) @, {"cat" "bird" "dog"}
    ([1], Reduce, AggregatingModifier, ("reduce", '/')),
    /// Apply a function to aggregate arrays
    ///
    /// Expects as many arguments as its function takes.
    /// In the simplest case, [fold] can be used to [reduce] an array with a default value.
    /// ex: ∧+ [1 2 3] 10
    ///   : ∧+ [] 10
    ///
    /// If the function takes at least 1 more argument than it returns:
    /// Arguments that are lower on the stack that will be used as accumulators.
    /// There will be as many accumulators as the function's outputs.
    /// Arguments that are higher on the stack will be iterated over.
    /// The number of iterated arrays is the number of arguments minus the number of outputs.
    /// The function will be repeatedly called with the rows of the iterated arrays, followed by the accumulators.
    /// On each iteration, the returned values will be used as the new accumulators.
    ///
    /// Multiple accumulators can be used
    /// ex: ∧(⊃+(×⊙⋅∘)) +1⇡5 0 1
    /// If the iterated array is already on the stack, you can use [dip] to place the accumulators below it.
    /// ex: ∧(⊃+(×⊙⋅∘))⊙(0 1) +1⇡5
    ///
    /// Multiple iterated arrays are also fine.
    /// Here, we accumulate the first array with [add] and the second with [multiply].
    /// ex: ∧⊃(+⊙⋅∘|×⋅⊙⋅∘) 1_2_3 4_5_6 0 1
    ///
    /// Like [rows], [fold] will repeat the row of arrays that have exactly one row.
    /// ex: ∧(⊂⊂) 1_2_3 4 []
    ///
    /// If the function returns the same or more values than it takes as arguments:
    /// There will be exactly one iterated array. The rest of the arguments will be used as accumulators.
    /// Excess outputs will be collected into arrays. When the [fold] is done, these arrays will be placed *below* the accumulators on the stack.
    /// This behavior is currently `# Experimental!`.
    ///
    /// For example, [scan] can be manually reimplemented by [duplicate]ing the result of the function.
    /// ex: # Experimental!
    ///   : ∧(.+) [1 2 3 4 5] 0
    /// ex: # Experimental!
    ///   : ∧(◡∘⊓+×⟜:) [1 2 3 4 5] 0 1
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
    ///
    /// If the function takes more than 2 arguments, additional arguments above the array on the stack will be passed to the function on every iteration.
    /// ex: \(+×) 10 [1 2 3 4]
    /// ex: ⬚@ \(⊂⊂) @, "abcd"
    (1[1], Scan, AggregatingModifier, ("scan", '\\')),
    /// Apply a function to each element of an array or arrays
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
    ([1], Rows, IteratingModifier, ("rows", '≡')),
    /// Apply a function to each combination of rows of some arrays
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
    ([1], Table, IteratingModifier, ("table", '⊞')),
    /// Get permutations or combinations of an array
    ///
    /// When given a dyadic function, [tuples] takes two arguments.
    /// The first argument must be a natural number. The second argument may be any array.
    /// If the function takes 2 arguments, combinations of rows from the array whose indices pass the function will be returned.
    /// The most common functions to use are `less than`, `less or equal`, `greater than`, `greater or equal`, and `not equals`.
    ///
    /// The examples here are [transpose]d to take up less vertical space.
    ///
    /// `less than` and `greater than` will give all unique *combinations* of rows from the array.
    /// ex: # Experimental!
    ///   : ⍉ ⧅< 2 ⇡5
    ///   : ⍉ ⧅> 2 ⇡5
    /// ex: # Experimental!
    ///   : ⍉ ⧅< 3 ⇡5
    /// ex: # Experimental!
    ///   : ⍉ ⧅< 4 ⇡5
    /// `less or equal` and `greater or equal` will include values that are the same.
    /// ex: # Experimental!
    ///   : ⍉ ⧅≤ 2 ⇡5
    /// ex: # Experimental!
    ///   : ⍉ ⧅≥ 2 ⇡5
    /// `not equals` will give all *permutations* of rows from the array.
    /// ex: # Experimental!
    ///   : ⍉ ⧅≠ 2 ⇡5
    /// ex: # Experimental!
    ///   : ⍉ ⧅≠ 3 ⇡5
    /// ex: # Experimental!
    ///   : ⍉ ⧅≠ 4 ⇡5
    /// If the size is `2`, the function is allowed to return non-booleans. Tuples will be copied as many times as the value.
    /// ex: # Experimental!
    ///   : ⍉ ⧅(+1<) 2 ⇡4
    /// If the second argument is a scalar, the number of tuples that would be returned for the [range] of that number is returned.
    /// ex: # Experimental!
    ///   : ⧅≠ 2 4
    ///   : ⧅≠ 2 ⇡4
    ///
    /// If [tuples] is given a monadic function, it takes only one argument.
    /// The function will be called on all prefixes of the array.
    /// Both the empty prefix and full-length prefix will be included, so the output will have one more row than the input.
    /// ex: # Experimental!
    ///   : ⧅□ ⇡5
    /// ex: # Experimental!
    ///   : ⧅□ "Hello!"
    /// ex: # Experimental!
    ///   : ⧅□ °△5_2
    /// You can get suffixes with a few [reverse]s.
    /// ex: # Experimental!
    ///   : ⍜⇌⧅(□⇌) "Hello!"
    /// Monadic [tuples] is compatible with [fill].
    /// ex: # Experimental!
    ///   : ⬚@-⧅∘ "Uiua"
    ///
    /// With [un][where], we can see where the inspiration for [tuples]'s glyph comes from.
    /// ex: # Experimental!
    ///   : °⊚ ⧅< 2 ⇡50
    ///   : °⊚ ⧅> 2 ⇡50
    ///   : °⊚ ⧅≠ 2 ⇡50
    /// We can get something similar with the monadic form.
    /// ex: # Experimental!
    ///   : ⬚0⧅∘ +1⇡50
    ([1], Tuples, IteratingModifier, ("tuples", '⧅')),
    /// Apply a function to each unboxed row of an array and re-box the results
    ///
    /// For box arrays, this is equivalent to `rows``under``un``box`.
    /// ex: ≡⍜°□(⊂:@!) {"a" "bc" "def"}
    ///   :    ⍚(⊂:@!) {"a" "bc" "def"}
    /// For non-box arrays, [inventory] works identically to [rows], except it [box]es each result row.
    /// ex: ≡⇌ [1_2_3 4_5_6]
    ///   : ⍚⇌ [1_2_3 4_5_6]
    /// This can be useful when you expect the function to yield arrays of different [shape]s.
    /// ex: ⍚⇡ [3 8 5 4]
    /// ex: ⍚↙⊙¤ [2 0 3 4 1] [4 8 9 2]
    /// For a box and non-box array, [inventory] will unbox the box array's rows and then re-box the results.
    /// ex: ⍚⊂ {"a" "bc" "def"} "123"
    ///
    /// A common use case is in conjunction with [under] and boxing array notation as a sort of n-wise [both].
    /// ex: {⍜ {⊙⊙∘}⍚⊂    1_2 3_4_5 6_7_8_9 10}
    ///   : {⍜⊙{⊙⊙∘}⍚⊂ 10 1_2 3_4_5 6_7_8_9   }
    ([1], Inventory, IteratingModifier, ("inventory", '⍚')),
    /// Repeat a function a number of times
    ///
    /// ex: ⍥(+2)5 0
    /// ex: ⍥(⊂2)5 []
    /// Repeating [infinity] times will do a fixed-point iteration.
    /// The loop will end when the top value of the function's output is equal to the top value of the function's input.
    /// For example, this could be used to flatten a deeply nested array.
    /// ex: ⍥/◇⊂∞ {1 {2 3} {4 {5 6 {7}}}}
    /// The number of repetitions may be non-scalar. In this case, the function will be repeated each row of the input a different number of times.
    /// ex: ⍥(×2) [1 2 3 4] [5 5 5 5]
    /// If you want to conditionally either run some function or not, you can use [repeat] to repeat `0` or `1` times.
    /// ex: F ← ⍥(×10)<10.
    ///   : F 5
    ///   : F 12
    /// [repeat]ing a negative number of times will repeat the function's [un]-inverse.
    /// ex: ⍥(×2)¯5 1024
    ///
    /// [repeat]'s glyph is a combination of a circle, representing a loop, and the 𝄇 symbol from musical notation.
    ([1], Repeat, IteratingModifier, ("repeat", '⍥')),
    /// Group elements of an array into buckets by index
    ///
    /// [group] is similar to `group_by` functions in other languages.
    /// Takes a function and two arrays.
    /// The first array must contain integers and have a shape that is a prefix of the shape of the second array.
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
    /// ex: $ Count the characters in this string
    ///   : ⊟∩≡□ ⊕⊃⊢⧻ ⊛.
    ///
    /// The indices may be multidimensional.
    /// ex: ⊕□ [0_2 2_1] ["ab" "cd"]
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
    /// The first array is called the "markers". It must be rank `1` and contain integers.
    /// Consecutive rows in the second array that line up with groups of the same key in the markers will be grouped together.
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
    /// [partition] also works with multidimensional markers. Groups are formed from markers that are adjacent along any axis.
    /// Each group will be flattened before being passed to the function.
    /// ex: ⊜□.. ↯4_4 [0 1 1 2 2]
    /// If we wanted to group the indices that are adjacent, we could use the array to [partition] its own indices.
    /// ex: ⊜□:⇡△.. ↯4_4 [0 1 1 2 2]
    ///
    /// [under][partition] works if [partition]'s function is [under]able.
    /// ex: ⍜⊜□⇌  ≠@ . $ These are some words
    /// ex: ⍜⊜□≡⇌ ≠@ . $ These are some words
    /// ex: ⍜⊜⊢⌵  ≠@ . $ These are some words
    ///
    /// [partition] is closely related to [group].
    (2[1], Partition, AggregatingModifier, ("partition", '⊜')),
    /// Apply a function to each shrinking row of an array
    ///
    /// Similar to [rows], [triangle] calls its function on each row of an array.
    /// However, [triangle] will call its function on a descreasing-size suffix of each row.
    /// We can see an example of what this means with [triangle][box].
    /// ex: # Experimental!
    ///   : ◹□ . ↯4_4⇡16
    /// Notice that the elements included correspond to the upper-right [triangle] of the input array.
    ///
    /// Non-square arrays will have extra rows either be truncated or longer.
    /// ex: # Experimental!
    ///   : ◹□ . ↯3_4⇡12
    /// ex: # Experimental!
    ///   : ◹□ . ↯5_3⇡15
    /// You can use [triangle][first] to get the diagonal of an array.
    /// ex: # Experimental!
    ///   : ◹⊢ . ↯3_4⇡12
    /// If [triangle]'s function takes more than one argument, then it functions similarly to [table], except some of the combinations are skipped.
    /// For example, we can use [triangle][couple] to get all unique combinations of rows from two arrays.
    /// Notice the difference from [table].
    /// ex: # Experimental!
    ///   : ☇1⊞⊟ ⇡3⇡3
    ///   : ◹⊟ ⇡3⇡3
    /// We can also see the pattern by rearranging the combinations with [group].
    /// ex: # Experimental!
    ///   : ◹⊂.⇡4
    ///   : ⊕□≡°⊟ .
    ///   : ⬚0⊕∘≡°⊟ :
    /// [triangle] also works with 3-argument functions (but currently no more).
    /// ex: # Experimental!
    ///   : ◹(⊂⊂)..⇡3
    /// It follows the same pattern that we can see with [group].
    /// ex: # Experimental!
    ///   : ◹(⊂⊂)..⇡3
    ///   : ⊕(□⊕□≡°⊟)≡°⊂ .
    ///   : ⬚0⊕(⬚0⊕∘≡°⊟)≡°⊂ :
    ([1], Triangle, AggregatingModifier, ("triangle", '◹')),
    /// Unbox the arguments to a function before calling it
    ///
    /// ex:  ⊂ □[1 2 3] □[4 5 6]
    ///   : ◇⊂ □[1 2 3] □[4 5 6]
    /// A common use of [content] is to collapse a list of [box]ed arrays with [reduce].
    /// ex: /◇⊂ {1_2_3 4_5 6}
    /// This case will still unbox a single element.
    /// ex: /◇⊂ {"Hi"}
    ([1], Content, OtherModifier, ("content", '◇')),
    /// Discard the top stack value then call a function
    ///
    /// See the [Advanced Stack Manipulation Tutorial](/tutorial/advancedstack) for a more complete understanding of why [gap] is useful.
    ///
    /// ex: ⋅+ 1 2 3
    /// This may seem useless when [pop] exists, but [gap] really shines when used with [fork].
    /// In a [fork] expression, you can use [dip], [gap], and [identity] to select out values.
    /// For example, if you wanted to add 3 values but keep the last value on top of the stack:
    /// ex: [⊃⋅⋅∘(++) 3 5 10]
    /// By using fewer `gap`s, you can select a different value.
    /// ex: [⊃⋅∘(++) 3 5 10]
    /// ex! [⊃∘(++) 3 5 10]
    /// By replacing a `gap` with a `dip`, you keep the argument in that spot instead of popping it:
    /// ex: [⊃⊙⋅∘(++) 3 5 10]
    /// ex: [⊃⋅⊙∘(++) 3 5 10]
    /// ex: [⊃⊙⊙∘(++) 3 5 10]
    ([1], Gap, Planet, ("gap", '⋅')),
    /// Temporarily pop the top value off the stack and call a function
    ///
    /// See the [Advanced Stack Manipulation Tutorial](/tutorial/advancedstack) for a more complete understanding of why [dip] is useful.
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
    /// [dip] can be used with a function pack.
    /// `dip``(F|G|H|..)` is equivalent to `dip``(F``dip``(G``dip``(H``dip``(..))))`.
    /// ex: ⊙(+|×) 1 2 3 4
    /// ex: ⊙(⊂×10|{⊙∘}|⊟) 1 2 3 4 5
    ([1], Dip, Planet, ("dip", '⊙')),
    /// Call a function but keep its first argument on the top of the stack
    ///
    /// ex: [⟜+ 2 5]
    ///   : [⟜- 2 5]
    /// ex: ÷⟜⇡ 10
    /// ex: +⟜(⇡-) 4 10
    /// ex: +⟜(×-) 10 20 0.3
    /// ex: ↯⟜⊚ 4
    ///
    /// [on] can be thought of as a compliment of [duplicate].
    /// ex: [¯. 1]
    ///   : [⟜¯ 1]
    ///
    /// [on] in planet notation acts as a way of [duplicate]ing a value.
    /// You can read `on``dip` or `on``identity` as a single unit that keeps 2 copies of the value at that position.
    /// ex: [⟜⊙⋅⟜⊙◌   1 2 3 4] # Easy to read with ⟜
    ///   : [.⊙⋅(.⊙◌) 1 2 3 4] # Hard to read with .
    ///   : [∩⊓.◌     1 2 3 4] # Shorter, maybe hard to read
    /// ex: [⊙⟜⊙⋅⟜∘  1 2 3 4] # Easy to read with ⟜
    ///   : [⊙(.⊙⋅.) 1 2 3 4] # Hard to read with .
    ///   : [⊙.⊙⊙⋅.  1 2 3 4] # Hard to read with .
    ///
    /// [on] can be used with a function pack. `on``(F|G)` becomes `on``F``on``G`.
    /// ex: [⟜(+1|×2|¯)] 5
    ///
    /// [on] is equivalent to [fork][identity], but can often be easier to read.
    ([1], On, Stack, ("on", '⟜')),
    /// Duplicate a function's last argument before calling it
    ///
    /// If you want to filter out every element of an array that is not [less than] 10, you can use [keep].
    /// ex: ▽<10. [1 27 8 3 14 9]
    /// However, if you want to make this a function, you have to [dip] below the first argument to [duplicate] the array.
    /// ex: F ← ▽<⊙.
    ///   : F 10 [1 27 8 3 14 9]
    /// While this works, it may take a moment to process in your mind how the stack is changing.
    /// [by] expresses the common pattern of performing an operation but preserving the last argument so that it can be used again.
    /// With [by], the filtering function above can be written more simply.
    /// ex: F ← ▽⊸<
    ///   : F 10 [1 27 8 3 14 9]
    /// Here are some more examples of [by] in action.
    /// ex: ⊂⊸↙ 2 [1 2 3 4 5]
    ///   : ⊜□⊸≠ @  "Hey there buddy"
    ///   : ⊕□⊸◿ 5 [2 9 5 21 10 17 3 35]
    ([1], By, Stack, ("by", '⊸')),
    /// Call a function but keep its last argument on the top of the stack
    ///
    /// ex: [⤙+ 2 5]
    ///   : [⤙- 2 5]
    /// [with] makes it easy to call multiple dyadic functions with the same last argument.
    /// There are many cases where this can read quite nicely.
    /// "Couple +1 with ×2"
    /// ex: ⊟+1⤙×2 5
    /// There is the common testing pattern "assert with match".
    /// ex: ⍤⤙≍ 5 +2 3
    /// ex! ⍤⤙≍ 5 +2 2
    /// [with] can be used to copy a value from deep in the stack, or to move it.
    /// ex: [⤙⊙⊙⊙∘ 1 2 3 4]
    ///   : [⤙⊙⊙⊙◌ 1 2 3 4]
    /// [with] always takes at least 2 arguments, even if its function takes fewer.
    /// ex! [⤙¯ 2]
    /// ex: [⤙¯ 2 5]
    /// ex: [⤙1 2 3]
    ([1], With, Stack, ("with", '⤙')),
    /// Call a function but keep its first argument under the outputs on the stack
    ///
    /// ex: # Experimental!
    ///   : [⤚+ 2 5]
    ///   : [⤚- 2 5]
    /// [off] can be used to copy a value from the top of the stack to a position deeper, or to move it.
    /// ex: # Experimental!
    ///   : [⤚⊙⊙⊙∘ 1 2 3 4]
    ///   : [⤚⋅⊙⊙∘ 1 2 3 4]
    /// [off] always takes at least 2 arguments, even if its function takes fewer.
    /// ex! # Experimental!
    ///   : [⤚¯ 2]
    /// ex: # Experimental!
    ///   : [⤚¯ 2 5]
    /// ex: # Experimental!
    ///   : [⤚1 2 3]
    ([1], Off, Stack, ("off", '⤚')),
    /// Keep all arguments to a function above the outputs on the stack
    ///
    /// ex: # Experimental!
    ///   : [◠+ 1 2]
    /// ex: # Experimental!
    ///   : [◠(++) 1 2 3]
    /// Functions with 0 or 1 arguments will be coerced to have 1 additional argument and output.
    /// ex: # Experimental!
    ///   : [◠¯ 1 2]
    /// ex: # Experimental!
    ///   : [◠1 2]
    /// If you do not want this behavior, use [on] instead.
    ///
    /// See also: [below]
    ([1], Above, Stack, ("above", '◠')),
    /// Keep all arguments to a function below the outputs on the stack
    ///
    /// ex: [◡+ 1 2]
    /// ex: [◡(++) 1 2 3]
    /// This can be used with [gap] and [identity] to copy values from arbitrarily low in the stack.
    /// ex: [◡⋅⋅⋅⋅∘ 1 2 3 4 5]
    /// Functions with 0 or 1 arguments will be coerced to have 1 additional argument and output.
    /// ex: [◡¯ 1 2]
    /// ex: [◡1 2]
    /// If you do not want this behavior, use [by] instead.
    /// [below] is often useful in conjunction with [both].
    /// For example, you can split an array by a mask by [not]ting the mask with [below] and then using [both][keep].
    /// ex: ∩▽◡¬ ⊸◿2 [2 8 3 9 2 7 1]
    ///
    /// See also: [above]
    ([1], Below, Stack, ("below", '◡')),
    /// Call a function with its arguments reversed
    ///
    /// This is a modifier version of [flip].
    /// It is experimental because it is unclear whether this is a desirable direction for the language.
    /// ex: # Experimental!
    ///   : ˜⊂ 1 2
    ([1], Backward, Stack, ("backward", '˜')),
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
    /// For `fab fac`, the idiom is `on``flip`.
    /// For example, if you wanted to check that a number is divisible by two other numbers:
    /// ex: F ← ∩(=0◿) ⊙,
    ///   : F 3 5 ⇡16
    /// ex: G ← ∩(=0◿:) ⟜:
    ///   : G ⇡16 3 5
    ([1], Both, Planet, ("both", '∩')),
    /// Define the various inverses of a function
    ///
    /// [obverse] defines how a function should interact with [un], [anti], and [under].
    /// It can either take a single function, or a function pack with up to 5 functions.
    ///
    /// If only a single function is provided, the function will be called in both directions of an [under].
    /// This is useful when you want to call one bit of code both before and after some other code.
    /// For example, calculating the standard deviation of some data involves taking the mean twice.
    /// ex: StdDev ← √ ÷⧻⟜/+ ×.- ÷⧻⟜/+ .
    ///   : StdDev [1 2 3 4]
    /// The averaging section can be factored out using [under][obverse].
    /// ex: StdDev ← √⍜⌅(÷⧻⟜/+)(×.-).
    ///   : StdDev [1 2 3 4]
    /// If given 2 functions, which inverse is set depends on the functions' signatures.
    /// If the functions have opposite signatures, then an [un]-compatible inverse is set.
    /// ex: F ← ⌅(+|⊃⌊⌈÷2)
    ///   : F 1 2
    ///   : [°F 25]
    /// If the functions have signatures `|a.b` and `|(b+1).(a-1)`, then an [anti]-compatible inverse is set.
    /// The most commonly used signatures for which this holds is when both signatures are `|2.1`.
    /// ex: F ← ⌅(+×10:|÷10-)
    ///   : F 2 3
    ///   : ⌝F 2 32
    /// This sort of inverse also works with [under].
    /// ex: F ← ⌅(+×10:|÷10-)
    ///   : ⍜F? 2 5
    /// Otherwise, an [under]-compatible inverse is set.
    /// ex: F ← ⌅(+|¯)
    ///   : ⍜F? 1 2
    /// Leaving the second function empty is useful when a function has to do some setup before the main [under]able part.
    /// ex! F ← ▽⊸◿2
    ///   : F [1 2 3 4 5]
    ///   : ⍜F(×10) [1 2 3 4 5]
    /// ex: F ← ▽⌅(⊸◿2|)
    ///   : F [1 2 3 4 5]
    ///   : ⍜F(×10) [1 2 3 4 5]
    /// If given 3 functions, an [under]-compatible inverse always set.
    /// The first function is the normal case.
    /// The second function is the "do" part of the [under].
    /// The third function is the "undo" part of the [under].
    /// ex: F ← ⌅(⊂10|⊂:1|⊂:2)
    ///   : F 3
    ///   : ⍜F⇌ 0_0
    /// If the second function returns more values than the first function, the excess values will be saved as "context". These context values will be passed to the "undo" part of the [under].
    /// Here is a manual implementation of [add]'s [under] behavior.
    /// ex: F ← ⌅(+|⟜+|-)
    ///   : F 2 5
    ///   : ⍜F(×10) 2 5
    /// If given 4 functions, both [un]-compatible and [under]-compatible inverses are set.
    /// The first function is the normal case.
    /// The second function is the [un]-compatible inverse.
    /// The third and fourth functions are for the [under]-compatible inverse.
    /// If the fourth function has the same signature as the first, it will also be used as the [anti]-compatible inverse.
    /// Finally, a fifth function can be given to specify the [anti]-compatible inverse.
    /// Here is our fully-specified [add] implementation.
    /// ex: F ← ⌅(+|⊃⌊⌈÷2|⟜+|-|$Anti -)
    ///   : F 2 5
    ///   : ⌝F 2 5
    ///   : [°F] 15
    ///   : ⍜F(÷3) 10 5
    /// Note that [anti] inverses also work with [un][on].
    /// ex: F ← ⌅(×|+÷2)
    ///   : F 4 10
    ///   : ⌝F 4 10
    ///   : [°⟜F] 4 10
    ([1], Obverse, InversionModifier, ("obverse", '⌅')),
    /// Invert the behavior of a function
    ///
    /// A list of all [un]-compatible functions can be found [below](#uns).
    ///
    /// ex: °√ 5
    /// Two functions that are invertible alone can be inverted together
    /// ex: °(+1√) 5
    /// Most functions are not invertible.
    /// [under] also uses inverses, but expresses a different pattern and is generally more powerful.
    /// A function's [un]-inverse can be set with [setinv].
    /// For more about inverses, see the [Inverse Tutorial](/tutorial/inverses).
    ([1], Un, InversionModifier, ("un", '°')),
    /// Invert the behavior of a function, treating its first argument as a constant
    ///
    /// [un] has a guarantee that the inverted function will have a signature that is the inverse of original function's signature. For dyadic functions, if we want the inverse to *also* be dyadic, then we have to do some workarounds. We can either include the first argument in the inverted function, or we can use [on].
    /// For example, here are two ways to invert [rotate].
    /// ex: °(↻1) [1 2 3]
    ///   : ◌°⟜↻ 1 [1 2 3]
    /// The first way requires the first argument to be a constant, which is not always applicable. The second way works but it is a bit verbose.
    /// [anti] does the [pop][un][on] for you.
    /// ex: ⌝↻ 1 [1 2 3]
    /// This simplifies some interesting inverses.
    /// ex: ⌝+ 1 5
    /// ex: ⌝↘ 3 [1 2 3]
    /// ex: ⬚@-⌝⊏ [0 2 5] "abc"
    /// ex: ⬚@-⌝⊡ [1_2 3_4] "xy"
    /// ex: ⌝⍥(+1) 3 10
    /// ex: ⌝⊂ 1 [1 2 3]
    /// ex! ⌝⊂ 1 [2 3 4]
    /// For more about inverses, see the [Inverse Tutorial](/tutorial/inverses).
    ([1], Anti, InversionModifier, ("anti", '⌝')),
    /// Operate on a transformed array, then reverse the transformation
    ///
    /// This is a more powerful version of [un].
    /// Conceptually, [under] transforms a value, modifies it, then reverses the transformation.
    ///
    /// A list of all [under]-compatible functions can be found [below](#unders).
    ///
    /// [under] takes 2 functions `f` and `g` and some other arguments `xs`.
    /// It applies `f` to `xs`, then applies `g` to the result.
    /// It then applies the inverse of `f` to the result of `g`.
    ///
    /// Any function that can be [un]ed can be used with [under].
    /// Some functions that can't be [un]ed can still be used with [under].
    ///
    /// Here, we [negate] 5, [subtract] 2, then [negate] again.
    /// ex: ⍜¯(-2) 5
    /// You can use [under][multiply][round] to round to a specific number of decimal places.
    /// ex: ⍜×⁅ 1e3 π
    ///
    /// In general, if two functions are compatible with [under] separately, then they are compatible together.
    /// ex: ⍜(↙⊙↘|×10) 2 1 [1 2 3 4 5]
    ///
    /// [under][both] works, and whether [both] is applied when undoing depends on the signature of `g`.
    /// For example, this hypotenuse function does not use [both] when undoing because its `g` (`add`) returns a single value.
    /// ex: ⍜∩(×.)+ 3 4
    /// However, this function whose `g` returns *2* values *does* use [both] when undoing, in this case re-[box]ing the outputs.
    /// ex: ⍜∩°□(⊂⊢,) □[1 2 3] □[4 5 6 7 8]
    ///
    /// [setund] can be used to define a function's [under] behavior.
    ///
    /// For more about [under] and inverses, see the [Inverse Tutorial](/tutorial/inverses).
    ([2], Under, InversionModifier, ("under", '⍜')),
    /// Call two functions on the same values
    ///
    /// [fork] is one of the most important functions for working with the stack.
    /// See the [Advanced Stack Manipulation Tutorial](/tutorial/advancedstack) for a more complete understanding as to why.
    ///
    /// ex: ⊃⇌◴ 1_2_2_3
    /// ex: ⊃(+1)(×2) 5
    /// [fork] can be chained to apply more functions to the arguments. `n` functions require the chaining of `subtract``1n` [fork].
    /// ex: [⊃⊃⊃+-×÷ 5 8]
    /// If the functions take different numbers of arguments, then the number of arguments is the maximum. Functions that take fewer than the maximum will work on the top values.
    /// ex: [⊃+¯ 3 5]
    /// By default, [fork] can only work with two functions. However, a function pack can be used to pass the same arguments to many functions.
    /// ex: ⊃(+1|×3|÷|$"_ and _") 6 12
    ([2], Fork, Planet, ("fork", '⊃')),
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
    /// ex: [⊓(+|-|×|÷) 10 20 5 8 3 7 2 5]
    /// [bracket] is a nice way to check if a number is within a range.
    /// ex: ×,,⊓≥≤5,8 . [6 2 5 9 6 5 0 4]
    ([2], Bracket, Planet, ("bracket", '⊓')),
    /// Repeat a function while a condition holds
    ///
    /// The first function is the loop function, and it is run as long as the condition is true.
    /// The second function is the condition. It's top return value must be a boolean.
    /// ex: ⍢(×2|<1000) 1
    /// Return values from the condition function that are under the condition itself will be passed to the loop function.
    /// Here is an example that evaluates a [Collatz sequence](https://en.wikipedia.org/wiki/Collatz_conjecture).
    /// The next number in the sequence is calculated in the condition function but [join]ed to the sequence in the loop function.
    /// ex: C ← ⨬(+1×3|÷2)=0◿2.
    /// : ◌⍢⊂(¬∈:,,C⊢.) [7]
    /// If the condition function consumes its only arguments to evaluate the condition, then those arguments will be implicitly copied.
    /// Consider this equivalence:
    /// ex: ⍢(×3|<100)  1
    ///   : ⍢(×3|<100.) 1
    /// The net stack change of the two functions, minus the condition, must be 0.
    /// ex! ⍢(×2.|<1000) 1
    /// This requirement is relaxed inside an array.
    /// ex: [⍢(×2.|<1000)] 1
    /// Alternatively, you can [join] the items to an initial list.
    /// ex: ◌⍢(⊃(×2)⊂|<100) 1 []
    ([2], Do, IteratingModifier, ("do", '⍢')),
    /// Set the fill value for a function
    ///
    /// By default, some operations require that arrays' [shape]s are in some way compatible.
    /// [fill] allows you to specify a value that will be used to extend the shape of one or both of the operands to make an operation succeed.
    /// The function is modified to take a fill value which will be used to fill in shapes.
    ///
    /// A list of all [fill]-compatible functions can be found [below](#fills).
    ///
    /// ex: ⬚0[1 2_3_4 5_6]
    /// ex: ⬚10+ [1 2 3 4] [5 6]
    /// ex: ⬚0≡⇡ [3 6 2]
    /// A fill value can be pulled from the stack with [identity].
    /// ex: ⬚∘[1 2_3_4] 0
    /// ex: ⬚∘+ ∞ [1 2] [3 4 5 6]
    ///
    /// Fill values are temporarily removed for the body of looping modifiers that can use them to fix their row shapes.
    /// These include [reduce], [scan], [rows], [each], [partition], and [group].
    /// ex! ⬚0≡(↙3) [3 4]
    /// [un][pop] can be used to retrieve the fill value. This ignores loop nesting and so can be used to "pull" the fill into the loop.
    /// ex: ⬚0≡(⬚°◌↙3) [3 4]
    ///
    /// Fill values cannot cross the boundary of a named function call.
    /// ex: ⬚0/⊂ [1 2 3]
    ///   : F ← /⊂
    ///   : ⬚0F [1 2 3]
    /// [un][pop] *can* get the fill value through the function call. This means you can use [fill][un][pop] to get the fill value into a function.
    /// ex: F ← ⬚°◌/⊂
    ///   : ⬚0F [1 2 3]
    /// This property includes index macros, but *not* code macros.
    ///
    /// [fill][pop] can be used to temporarily remove the fill value.
    /// ex: ⬚0  ↻ 2 [1 2 3 4 5]
    ///   : ⬚0⬚◌↻ 2 [1 2 3 4 5]
    /// This does not affect [un][pop].
    /// ex: ⬚0  °◌
    /// ex: ⬚0⬚◌°◌
    ///
    /// [fill] and [un][pop] can be used to make a sort of ad-hoc variable system.
    /// ex: a ← (°□⊡0°◌)
    ///   : b ← (°□⊡1°◌)
    ///   : c ← (°□⊡2°◌)
    ///   : ⬚{⊙⊙∘}(×b+c×a a) 2 3 4
    ([2], Fill, OtherModifier, ("fill", '⬚')),
    /// Call the function at the given index
    ///
    /// [switch] takes at least 1 argument, an index.
    /// If the index is `0`, the first function is called.
    /// If the index is `1`, the second function is called.
    /// ex: ⨬+- 0 3 5
    ///   : ⨬+- 1 3 5
    /// The signatures of the functions do not need to match exactly.
    /// Excess arguments will be discarded.
    /// ex: ⨬(×.)+ 0 3 5
    ///   : ⨬(×.)+ 1 3 5
    /// A function pack can be used to switch between more than 2 functions.
    /// ex: ⨬(+|-|×|÷) 0 2 5
    ///   : ⨬(+|-|×|÷) 1 2 5
    ///   : ⨬(+|-|×|÷) 2 2 5
    ///   : ⨬(+|-|×|÷) 3 2 5
    /// The index does not have to be a scalar.
    /// ex: ⨬(+|-|×|÷) [0 1 2 3] 2 5
    /// In this case, [switch] behaves similarly to [rows]. The index will be iterated along with other arguments.
    /// ex: ⨬(+|-|×|÷) [0 1 2 3] [1 6 10 2] 5
    ([2], Switch, OtherModifier, ("switch", '⨬')),
    /// Call a function and catch errors
    ///
    /// If the first function errors, the second function is called with the original arguments and the error value.
    ///
    /// If the handler function has 0 arguments, then it is simply called. This is a nice way to provide a default value.
    /// ex: ⍣⋕0 "5"
    ///   : ⍣⋕0 "dog"
    /// The handler function will be passed at most the same arguments as the tried function, plus the error. It will only be passed as many arguments as it takes.
    /// Normal runtime errors become strings. If you only care about the error, you can use [gap] or [pop] to ignore the arguments passed to the handler.
    /// ex: ⍣(+1)⋅$"Error: _" 2   # No error
    /// ex: ⍣(+@a)⋅$"Error: _" @b # Error
    /// Errors thrown with [assert] can be any value.
    /// ex: ⍣(⍤5>10.)⋅(×5) 12 # No error
    /// ex: ⍣(⍤5>10.)⋅(×5) 7  # Error
    /// We can see how values are passed to the handler by wrapping them in an array.
    /// ex: ⍣⋕{⊙∘} "5"   # No error
    ///   : ⍣⋕{⊙∘} "dog" # Error
    /// ex: ⍣(⍤0.+)10    3 5 # Ignore both arguments and error
    ///   : ⍣(⍤0.+)¤     3 5 # First argument only
    ///   : ⍣(⍤0.+)⊟     3 5 # Both arguments
    ///   : ⍣(⍤0.+)[⊙⊙∘] 3 5 # Both arguments and error
    /// If we want to provide a default value from the stack, we can ignore it in the tried function with [gap] and then use [identity] in the handler.
    /// ex: ⍣⋅⋕∘ 5 "12"  # No error
    ///   : ⍣⋅⋕∘ 5 "dog" # Error
    /// [try] works with function packs of more than 2 functions. Each function will by tried in order, and all functions after the first will be passed the error value from the previous function.
    /// ex: F ← ⍣(⋕|{⊂2⊙∘}|{⊙∘})
    ///   : F "5"
    ///   : F [1]
    ///   : F "hi"
    ([2], Try, Misc, ("try", '⍣')),
    /// Call a pattern matching case
    ///
    /// [case] calls its function and prevents errors from escaping a single [try].
    /// Its primary use is in pattern matching.
    /// Consider this function:
    /// ex: F ← ⍣(
    ///   :   ⊏3 °(⊂1)
    ///   : | ⊏1 °(⊂2)
    ///   : | 0
    ///   : )
    /// `F` attempts to [un]`(`[join]`1)` from the input array. Failing that, it attempts to [un]`(`[join]`2)`. In either `un``join` case, we subsequently [select] from the array. If both pattern matches fail, it returns `0` as a default.
    /// ex: F ← ⍣(
    ///   :   ⊏3 °(⊂1)
    ///   : | ⊏1 °(⊂2)
    ///   : | 0
    ///   : )
    ///   : F [1 2 3 4 5]
    ///   : F [2 3 4 5]
    ///   : F [5 2 3]
    /// However, there is a problem with this code.
    /// Pattern matching in a [try] works by throwing an error and passing the inputs to the next handler. However, if an error is thrown in a branch *after a successful pattern match*, the next branch will still be tried anyway.
    /// This could lead to some unexpected behavior.
    /// ex: F ← ⍣(
    ///   :   ⊏3 °(⊂1)
    ///   : | ⊏1 °(⊂2)
    ///   : | 0
    ///   : )
    ///   : F [1 5 8]
    /// In the example above, we successfully `un``(``join``1)`. However, the code after that pattern match fails. [select] errors because the index `3` is out of bounds of our array `[5 8]`. Instead of failing the whole function, the next branch is tried. It fails too, so we end up with `0`.
    /// This could be especially problematic if the next branches have side-effects.
    /// ex: F ← ⍣(
    ///   :   ⊏3 &p"Matched 1!" °(⊂1)
    ///   : | ⊏1 &p"Matched 2!" °(⊂2)
    ///   : | 0  &p"Matched nothing!"
    ///   : )
    ///   : F [1 2 3 4]
    /// This prints 2 messages, even though the whole function should have failed.
    /// Code that doesn't fail when it should can lead to bugs that are hard to track down.
    /// We want our errors to be loud!
    ///
    /// This is where [case] comes in. [case] has one special thing it does that makes it useful: errors returned from [case]'s first function can escape a single [try].
    /// We can then arrange our [try] pattern matching with a [case] for each branch. The code in each branch that comes after the pattern match is wrapped in a [case].
    /// ex! # Experimental!
    ///   : F ← ⍣(
    ///   :   case(⊏3) °(⊂1)
    ///   : | case(⊏1) °(⊂2)
    ///   : | 0
    ///   : )
    ///   : F [1 2 3 4]
    /// And there we go. Task failed successfully!
    ([1], Case, Misc, "case"),
    /// Throw an error if a condition is not met
    ///
    /// Expects a message and a test value.
    /// If the test value is anything but `1`, then the message will be thrown as an error.
    /// ex! ⍤"Oh no!" "any array"
    /// ex: ⍤"Oh no!" 1
    /// ex! ⍤"Oh no!" 0
    /// As you can see, a top-level [assert] is interpreted as a test in some contexts. See the [Testing Tutorial](/tutorial/testing) for more information.
    /// Use [duplicate] if you do not care about the message.
    /// ex: ⍤. =6 6
    /// ex! ⍤. =8 9
    /// Errors thrown by [assert] can be caught with [try].
    (2(0), Assert, Misc, ("assert", '⍤'), Impure),
    /// Generate a random number in the range `[0, 1)`
    ///
    /// If you need a seeded random number, use [gen].
    ///
    /// ex: ⚂
    /// ex: [⚂⚂⚂]
    ///
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: ⌊×10 [⍥⚂5]
    ///
    /// `each``gap``random` and `table``gap``gap``random` are optimized in the interpreter to generate a lot of random numbers very fast.
    /// ex: ⌊×10 ∵⋅⚂ ⇡10
    /// ex: ⌊×10 ⊞⋅⋅⚂ .⇡10
    (0, Rand, Misc, ("random", '⚂'), Impure),
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
    ([1], Comptime, Comptime, "comptime"),
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
    ///
    /// For spawn threads in a thread pool, use [pool].
    ([1], Spawn, Thread, "spawn", Impure),
    /// Spawn a thread in a thread pool
    ///
    /// Has the same functionality as [spawn], but uses a thread pool instead of spawning a new thread.
    /// While [spawn]'s function will be called immediately, [pool]'s function will be called when a thread in the pool is available.
    /// The thread pool has as many threads as the machine has processors.
    /// If all threads in the pool are busy, then [pool] will block until a thread is available.
    ([1], Pool, Thread, "pool", Impure),
    /// Wait for a thread to finish and push its results to the stack
    ///
    /// The argument must be a thread id returned by [spawn] or [pool].
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
    (1, Wait, Thread, "wait", Mutating),
    /// Send a value to a thread
    ///
    /// Expects a thread id returned by [spawn] or [pool] and a value to send.
    /// The thread id `0` corresponds to the parent thread.
    /// The sent-to thread can receive the value with [recv] or [tryrecv].
    (2(0), Send, Thread, "send", Impure),
    /// Receive a value from a thread
    ///
    /// Expects a thread id returned by [spawn] or [pool].
    /// The thread id `0` corresponds to the parent thread.
    /// The sending thread can send a value with [send].
    ///
    /// Unlike [tryrecv], [recv] blocks until a value is received.
    (1, Recv, Thread, "recv", Impure),
    /// Try to receive a value from a thread
    ///
    /// Expects a thread id returned by [spawn] or [pool].
    /// The thread id `0` corresponds to the parent thread.
    /// The sending thread can send a value with [send].
    ///
    /// Unlike [recv], [tryrecv] does not block.
    /// If no value is available, then an error is thrown.
    /// The error can be caught with [try].
    (1, TryRecv, Thread, "tryrecv", Impure),
    /// Generate an array of random numbers with a seed
    ///
    /// The first argument is the shape, the second argument is the seed. The returned array will have the given shape where each element is in the range [0, 1).
    /// If you don't care about the seed or shape, you can use [random] instead.
    /// ex: gen [] 0
    /// ex: gen [] 1
    /// ex: gen 3 0
    /// ex: gen 3 1
    /// Use [multiply] and [floor] to generate a random integer in a range.
    /// ex: ⌊×10 gen 10 42
    /// ex: ⌊×10 gen 2_3 0
    /// ex: ⌊×10 gen 2_3 1
    /// A rank-2 array or box array of shapes can be used to generate multiple arrays. The resulting arrays will be in a boxed list
    /// ex: ⌊×10 gen [2_3 3_4] 0
    /// ex: ⌊×10 gen {2_2 [] 2_3_3 4} 0
    /// If you want a seed to use for a subsequent [gen], you can use [fork] and `[]`.
    /// ex: gen 8 ⊃(gen[]|gen5) 0
    ///   : ∩(⌊×10)
    /// For non-determinism, [random] can be used as a seed.
    /// ex: ⌊×10 gen 3_4 ⚂
    (2, Gen, Misc, "gen"),
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
    /// ex: ⬚""regex "a(b)?" "a ab"
    ///
    /// Uiua uses the [Rust regex crate](https://docs.rs/regex/latest/regex/) internally.
    (2, Regex, Misc, "regex"),
    /// Convert a string to UTF-8 bytes
    ///
    /// ex: utf₈ "hello!"
    /// ex: utf₈ "❤️"
    /// You can use [un] to convert UTF-8 bytes back to a string.
    /// ex: °utf₈ [226 156 168 32 119 111 119 33]
    ///
    /// [utf₈] is different from just [add]ing or [subtracting] `@\0`.
    /// Character math can only convert to and from UTF-32.
    /// ex: -@\0 "👩🏽‍👩🏻‍👦🏻‍👧🏽"
    /// ex: utf₈ "👩🏽‍👩🏻‍👦🏻‍👧🏽"
    (1, Utf8, Encoding, "utf₈"),
    /// Convert a string to a list of UTF-8 grapheme clusters
    ///
    /// A Uiua character is a single Unicode code point.
    /// A [grapheme cluster](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries) is a sequence of Unicode code points that combine into a single visual character.
    /// [graphemes] splits a string into its grapheme clusters.
    /// ex: graphemes "🏳️‍⚧️ 👩🏼‍🤝‍👩🏽 ȗ̵̬ị̶̿u̴̠͘ă̸̰"
    ///   : ≡¤
    ///   : -@\0.
    ///
    /// [graphemes] works with [un] and [under].
    /// ex: ⍜graphemes≡◇⊢ "ų̶̢̢̛̥͈̖̦̜̥͔͕̙͚̜͚͊͋̑̿̔̓̐͐̀̓̐̈́̑͆͆͘į̴̥̞̀̑̋̀̽̌̓̐̓̚ư̷̯̖͈͇̌͌́̄̿͑̓̚͜͜à̶͓̜̗̩̝̰̲͎͉̲͆̇͗̄̆̏̍̑̍͌͝ͅ"
    (1, Graphemes, Encoding, "graphemes"),
    /// Generate a unique tag
    ///
    /// Tags are just numbers and are unique across multiple threads, but not across multiple runs.
    /// ex: [⍥tag5]
    ///   : [⍥tag5]
    (0, Tag, Misc, "tag", Impure),
    /// Check the type of an array
    ///
    /// `0` indicates a number array.
    /// `1` indicates a character array.
    /// `2` indicates a box array.
    /// `3` indicates a complex array.
    /// ex: type 5
    /// ex: type i
    /// ex: type "hello"
    /// ex: type □[5 6]
    /// ex: ∵ type    {10 "dog" [1 2 3]}
    ///   : ∵(type°□) {10 "dog" [1 2 3]}
    (1, Type, Misc, "type"),
    /// Get the current time in seconds
    ///
    /// Time is expressed in seconds since the Unix epoch.
    /// ex: now
    /// [under][now] can be used to time a function.
    /// ex: ⍜now(5&sl1)
    (0, Now, Misc, "now", Impure),
    /// Get the date and time information from a time
    ///
    /// You can use [now] to get the current time in seconds since the Unix epoch.
    /// [datetime] turns a time into an array with 6 numbers:
    /// - Year
    /// - Month (1-12)
    /// - Day (1-31)
    /// - Hour (0-23)
    /// - Minute (0-59)
    /// - Second (0-59)
    ///
    /// ex: datetime now
    /// The time is always in UTC.
    /// [datetime] is semi-pervasive.
    /// ex: datetime [1e8 1e9 1e10]
    /// You can format the time like this:
    /// ex: datetime now         # Time
    ///   : ⍚(⬚@0↙¯⊙°⋕) [4....2] # Pad
    ///   : °[°$"_-_-_ _:_:_"]   # Format
    ///
    /// You can use [un][datetime] to convert an array back into a time.
    /// An array with fewer than 6 numbers will be padded with zeros.
    /// ex: °datetime [2023 2 28 1 2 3]
    /// ex: °datetime [2014_4_1 2022_3_31]
    /// Invalid numbers in the datetime will be normalized.
    /// ex: ⍜°datetime∘ [2023 2 29]
    /// ex: ⍜°datetime∘ [1917 5 0]
    /// ex: ⍜°datetime∘ [1996 12 ¯100]
    (1, DateTime, Misc, "datetime"),
    /// Get the local timezone offset
    ///
    /// ex: timezone
    /// ex: datetime +×3600 timezone now
    (0, TimeZone, Misc, "timezone", Impure),
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
    /// Create a hashmap from a list of keys and list values
    ///
    /// A hashmap is a normal array that is used as a mapping from keys to values.
    /// The related map functions [insert], [has], and [get], all treat the array as an actual hashmap, so they have O(1) amortized time complexity.
    /// Because the values array maintains insertion order, the [remove] function has O(n) time complexity.
    ///
    /// ex: map 1_2 3_4
    ///   : map {"Alice" "Bob" "Carol"} [3_8 12_2 4_5]
    /// Use [get] to get the value corresponding to a key.
    /// ex: map 1_2 3_4
    ///   : get 2 .
    /// Use [insert] to insert additional key-value pairs.
    /// ex: map 1_2 3_4
    ///   : insert 5 6
    /// An empty array can be used as an empty map, event if it was not created with [map].
    /// ex: has 5 []
    ///   : insert 1 2 []
    /// You can use [un][map] to get the keys list and values list back.
    /// ex: []
    ///   : insert 1 2_3
    ///   : insert 4 5_6
    ///   : insert 7 8_9
    ///   : °map .
    ///
    /// Pervasive operations work on the values of a map, but not on the keys.
    /// ex: ×10 map 1_2_3 4_5_6
    /// Some normal array operations work on maps:
    /// - [reverse]
    /// - [rotate]
    /// - [take]
    /// - [drop]
    /// - [join]
    /// - [each]
    /// - [rows]
    /// - [classify]
    /// - [deduplicate]
    /// Operations that do not specifically work on maps will remove the keys and turn the map into a normal array.
    ///
    /// [fix]ing a map will [fix] the keys and values. This exposes the true structure of the keys array.
    /// ex: ¤ map 3_10_5 "abc"
    /// This is usually only useful with [rows].
    /// ex: ≡get [1 3 3 2] ¤ map 1_2_3 4_5_6
    ///
    /// Map keys are stored as metadata on the values array. For this reason, they cannot be put in arrays together without being [box]ed, as the metadata for each map would be lost.
    ///
    /// Regardless of the size of the map, operations on it have O(1) amortized time complexity.
    /// In this example, we time [get] and [insert] operations on maps from 10 entries up to 100,000 entries.
    /// ex: Times ← (
    ///   :   map.⇡
    ///   :   [⊙◌⍜now(get 5):
    ///   :    ⊙◌⍜now(insert 1 2).]
    ///   : )
    ///   : ⁿ:10+1⇡5
    ///   : ≡Times.
    (2, Map, Map, "map"),
    /// Insert a key-value pair into a map array
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// The array is used as an actual hashmap, so some entries may be empty.
    /// ex: []
    ///   : insert 1 2
    ///   : insert 3 4
    ///   : insert 5 6
    /// If the key is already present, it is replaced.
    /// ex: []
    ///   : insert 1 2
    ///   : insert 3 4
    ///   : insert 3 5
    /// Keys that are already present keep their order.
    /// ex: map 1_2_3 4_5_6
    ///   : insert 1 10
    /// Here is a pattern for [remove]ing a key if it is present before [insert]ing it, so that the key moves to the end.
    /// ex: map 1_2_3 4_5_6
    ///   : insert⟜⍜⊙◌remove 1 10
    /// All keys (and all values) must have the same shape and type.
    /// ex! map 1 ["wow"]
    ///   : insert "hi" "there"
    /// [box] keys or values if you need to. Values will coerce to boxes if necessary.
    /// ex: map 1 ["wow"]
    ///   : insert □"hi" □"there"
    /// ex: map □1 □"wow"
    ///   : insert "hi" "there"
    ///
    /// See also: [has], [get], [remove]
    (3, Insert, Map, "insert"),
    /// Check if a map array has a key
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// ex: map 1_2 3_4
    ///   : [fork(has 2|has 5)].
    ///
    /// See also: [insert], [get], [remove]
    (2, Has, Map, "has"),
    /// Get the value corresponding to a key in a map array
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// ex: map 1_2 3_4
    ///   : get 2 .
    /// If the key is not found, an error is thrown.
    /// ex! map 1_2 3_4
    ///   : get 5 .
    /// You can use [fill], [try], or [has] to avoid the error.
    /// ex: map 1_2 3_4
    ///   : ⬚0get 5 .
    /// ex: map 1_2 3_4
    ///   : ⍣get0 5 .
    /// ex: map 1_2 3_4
    ///   : ⨬⋅⋅0get has,, 5 .
    /// You can provide a default value with [fill].
    /// ex: map 1_2 3_4
    ///   : ⬚0get 1 .
    ///   : ⬚0get 5 :
    /// You can use [under][get] to modify the value at the key.
    /// ex: /map⍉ [1_2 3_4 5_6]
    ///   : ⍜(get3|×10)
    ///
    /// See also: [insert], [has], [remove]
    (2, Get, Map, "get"),
    /// Remove the value corresponding to a key from a map array
    ///
    /// See [map] for an overview of map arrays.
    ///
    /// The key is removed if it is present.
    /// If the key is not present, the array is unchanged.
    /// ex: map 1_2 3_4
    ///   : remove 2 .
    ///   : remove 5 .
    ///
    /// Unlike the other map functions, [remove] has O(n) time complexity.
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
    (0(0), Stack, Debug, ("stack", '?'), Impure),
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
    (1, Trace, Debug, ("trace", '⸮'), Impure),
    /// Preprocess and print all stack values without popping them
    ///
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
    /// ex: 1_2_3 [] 5_6_7
    ///   : dump⊢
    (0(0)[1], Dump, Debug, "dump", Impure),
    /// Convert code into a string instead of compiling it
    ///
    /// ex: # Experimental!
    ///   : stringify(/+ran+1)
    /// This is mostly useful when used in a macro.
    /// ex: # Experimental!
    ///   : F! ← ^0 &p$"Running code: _" stringify^0
    ///   : F!(+ 1 2)
    ///
    /// The opposite of [stringify] is [quote].
    (0[1], Stringify, Comptime, "stringify"),
    /// Convert a string into code at compile time
    ///
    /// ex: # Experimental!
    ///   : quote("+1") 5
    ///
    /// The opposite of [quote] is [stringify].
    (0[1], Quote, Comptime, "quote"),
    /// Get the signature of a function
    ///
    /// ex: # Experimental!
    ///   : [signature+]
    /// ex: # Experimental!
    ///   : [signature°⊟]
    /// ex: # Experimental!
    ///   : [signature(+++++++)]
    /// ex: # Experimental!
    ///   : [signature⊙⊙⊙∘]
    /// ex: # Experimental!
    ///   : [signature[⊙⊙⊙∘]]
    /// ex: # Experimental!
    ///   : [signature°[⊙⊙⊙∘]]
    ///
    /// At the moment, this is only useful for debugging.
    /// While theoretically, it could be used in a macro to choose a branch of a [switch] appropriate for the function, this is not yet possible because of the way that macros and signature checking work.
    (0(2)[1], Sig, Comptime, "signature"),
    /// Run the Fast Fourier Transform on an array
    ///
    /// The Fast Fourier Transform (FFT) is an optimized algorithm for computing the Discrete Fourier Transform (DFT). The DFT is a transformation that converts a signal from the time domain to the frequency domain.
    ///
    /// The input array must be either real or complex.
    /// The result will always be complex.
    /// Multi-dimensional arrays are supported. Each rank-1 row is treated as a separate array.
    ///
    /// In this example, we generate some data that is the sum of some [sine] waves.
    /// We then run [fft] on it and create a plot of the resulting frequency bins.
    /// ex: # Experimental!
    ///   : ÷⟜⇡200             # 200 numbers between 0 and 1
    ///   : /+∿⊞×[100 200 400] # Add some frequencies
    ///   : ⌵ fft              # Run the FFT
    ///   : ↘⌊÷2⧻.             # Drop the top half
    ///   : ⬚0≡▽:1 ×15         # Render
    ///
    /// You can use [un][fft] to calculate the inverse FFT.
    /// In this example, we generate a list of `1`s representing frequency bins and run `un``fft` on it to get time-domain data. We can listen to the result as audio.
    /// ex: # Experimental!
    ///   : [220 277 330 440] # Frequencies
    ///   : ⬚0↙ &asr °⊚       # Put 1 in buffer for each frequency
    ///   : ◌°ℂ °fft          # Run inverse FFT and get the real part
    (1, Fft, Misc, "fft"),
    /// Find shortest paths in a graph
    ///
    /// Expects 3 functions and at least 1 value.
    /// The value is the starting node.
    /// The first function should return 1 or 2 arrays of equal [length].
    /// - An array of the neighboring nodes must always be returned.
    /// - An array of costs may be returned above the nodes array on the stack. If ommitted, all costs are assumed to be 1.
    /// The second function should return a heuristic cost to reach the goal node.
    /// - The heuristic may return a value [less or equal] the actual cost
    /// - It must *never* overestimate the cost, or the algorithm may not find the shortest path
    /// The third function should return whether or not the goal node has been reached.
    ///
    /// When called, [astar] will pop any additional arguments its functions need from the stack.
    /// On each iteration, the current node will be passed to each function, along with any of the additional arguments that each function needs.
    ///
    /// If a path is found, a list of [box]ed arrays of all shortest paths is returned, as well as the cost.
    /// If no path is found, an empty list and a cost of `infinity` are returned.
    ///
    /// In this example, we find the shortest path from the 2D point `0_0` to `3_5` in a grid.
    /// The neighbors function returns the 4 cardinal directions with all costs of 1.
    /// The heuristic function `absolute value``reduce``complex``subtract` calculates the euclidean distance between two points.
    /// The goal function simply checks if the current node [match]es the given goal node.
    /// ex: # Experimental!
    ///   : Neis ← [∩¯,,⇌.⇡2]
    ///   : Neis # Side-adjacent neighbors offsets
    ///   :
    ///   : °□⊢ astar(
    ///   :   ≡⋅1. +Neis¤ # Costs and neighbors
    ///   : | ⌵/ℂ-        # Heuristic
    ///   : | ≍           # Check if goal
    ///   : )0_0 3_5      # Start and goal
    /// If we omit the cost array from the neighbors function and simply use `0` as the heuristic, the algorithm becomes Dijkstra's algorithm.
    /// ex: # Experimental!
    ///   : Neis ← [∩¯,,⇌.⇡2]
    ///   : °□⊢ astar(+Neis¤)0≍ 0_0 3_5
    /// In the examples above, we use `un``box``first` to get only the first path. [first][astar] and [pop][astar] are optimized to not do extra work.
    /// If we want *all* shortest paths, we can omit [first].
    /// ex: # Experimental!
    ///   : Neis ← [∩¯,,⇌.⇡2]
    ///   : astar(+Neis¤)0≍ 0_0 1_2
    /// If pathing on a grid like the examples above, we can use [un][where] to visualize the path that was taken!
    /// ex: # Experimental!
    ///   : Neis ← [∩¯,,⇌.⇡2]
    ///   : °□⊢ astar(+Neis¤|⌵/ℂ-|≍) 3_4 10_14
    ///   : °⊚
    ///   : ▽⟜≡▽8 # Upscale
    ///
    /// [astar] is designed to be maximally flexible, so it can be used with graphs or grids or any other structure.
    ((2)[3], Astar, Misc, "astar"),
    /// Calculate the derivative of a mathematical expression
    ///
    /// Currently, only polynomials are supported.
    /// ex: # Experimental!
    ///   : # x² → 2x
    ///   : ∂(×.) 5
    /// ex: # Experimental!
    ///   : # √x → 1/(2√x)
    ///   : ∂√ 1/9
    /// ex: # Experimental!
    ///   : # x² - 2x - 4  →  2x² - 2x
    ///   : ∂(++⊃(ⁿ2|×¯2|¯4)) [0 1 2]
    ///
    /// See also: [integral]
    ([1], Derivative, Misc, ("derivative", '∂')),
    /// Calculate the integral of a mathematical expression
    ///
    /// Currently, only polynomials are supported.
    /// The constant integration term is not included.
    /// ex: # Experimental!
    ///   : # x² → x³/3
    ///   : ∫(×.) 3
    /// ex: # Experimental!
    ///   : # √x → (2x^1.5)/3
    ///   : ∫√ 1
    /// ex: # Experimental!
    ///   : # 2x + 5  →  x² + 5x
    ///   : ∫(+5×2) 2
    ///
    /// See also: [derivative]
    ([1], Integral, Misc, ("integral", '∫')),
    /// Encode an array into a JSON string
    ///
    /// ex: json [1 2 3]
    /// ex: json {"some" "words"}
    /// ex: json map {"hey" "there" "buddy"} {1 2 [3 4 5]}
    /// You can use [un][json] to decode a JSON string back into an array.
    /// ex: °json "[4,5,6]"
    /// ex: °json $ ["what's","the","plan"]
    /// ex: °json $ {"foo": "bar", "baz": [1, 2, 3]}
    ///
    /// While the number literals `0` and `1` are converted to their number equivalents in JSON, the shadowable constants `True` and `False` are converted to JSON `true` and `false`.
    /// ex: json {0 1 2 3 True False}
    ///
    /// While [json] always produces ECMA-compliant JSON, [un][json] can parse [JSON5](https://json5.org/).
    /// This means that you can use single quotes, unquoted keys, trailing commas, and comments.
    /// ex: °json $ {foo: 'bar', /* cool */ baz: [1, 2, 3,],}
    ///
    /// Note that `NaN` and [infinity] convert to JSON `null`, and JSON `null` converts to `NaN`.
    /// This means that [infinity] is converted to `NaN` in a round-trip.
    /// ex: json [1 ¯5 NaN ∞]
    /// ex: °json "[1,null,-3,null]"
    (1, Json, Encoding, "json"),
    /// Encode an array into a CSV string
    ///
    /// The input array must be at most rank-`2`.
    /// ex: csv [1 2 3]
    /// ex: csv ↯3_4⇡12
    /// ex: csv [{"Foo" "Bar"} [1 2] [3 4] [5 6]]
    /// You can use [un][csv] to decode a CSV string back into an array.
    /// ex: °csv "#,Count\n1,5\n2,21\n3,8\n"
    /// By default, rows of mismatched length are padded with empty strings.
    /// ex: °csv "1,2,3\n4\n5,6"
    /// This can be changed with [fill].
    /// ex: ⬚"x"°csv "1,2,3\n4\n5,6"
    /// The default delimiter is (of course) a comma. However, [fill] can be used to change it.
    /// ex: °⬚@;csv "1;2;3\n4\n5,6;7"
    /// [fill] outside the [un] pads rows of different lengths. [fill] inside the [un] chooses the delimiter.
    /// ex: ⬚"x"°⬚@;csv "1;2;3\n4\n5,6;7"
    /// The decoding result will always be a rank-`2` array of boxed strings.
    /// You can use `each``try``parse``gap``identity` to convert the strings that represent numbers.
    /// ex: ∵⍣⋕∘ °csv "#,Count\n1,5\n2,21\n3,8\n"
    /// If you know there are headers, you can use [un][join] to separate them.
    /// ex: ⊙⋕°⊂ °csv "#,Count\n1,5\n2,21\n3,8\n"
    /// You can easily create a [map] with the headers as keys.
    /// ex: map⊙(⍉⋕)°⊂ °csv "#,Count\n1,5\n2,21\n3,8\n"
    (1, Csv, Encoding, "csv"),
    /// Encode an array into XLSX bytes
    ///
    /// XLSX is a spreadsheet format that can be edited in programs like Microsoft Excel, Google Sheets, and LibreOffice Calc.
    /// Spreadsheets are just arrays, so array languages like Uiua are a natural fit for working with them.
    ///
    /// The input value must be a sheet array or a [map] array with sheet names as keys and sheet arrays as values.
    /// Sheet arrays may be at most rank `2`.
    /// XLSX is a binary format, so the output is a byte array.
    ///
    /// You can use [un][xlsx] to decode an XLSX byte array back into a sheet map.
    /// In the resulting sheet map, each sheet will be a boxed rank-`2` array of boxed values.
    ///
    /// While it is not useful to display the output bytes here, we can see how the result of decoding works:
    /// ex: °xlsx xlsx . ↯3_6⇡18
    (1, Xlsx, Encoding, "xlsx"),
    /// Convert a value to its code representation
    ///
    /// ex: repr π
    /// Use [&p][repr] to produce a representation that can be pasted directly into the
    /// interpreter.
    /// ex: &p repr ↯2_2_2 0
    /// ex: &p repr {"Uiua" @A [1 2 3] □4}
    ///
    /// [repr] can be used in code macros to make the macro generate code that produces the same array.
    /// ex! F! ←^ ⧻°□⊢
    ///   : F!+
    /// ex: F! ←^ repr ⧻°□⊢
    ///   : F!+
    ///   : F!≡(¬×)
    ///   : F!repr
    ///   : F!(.....)
    ///
    /// Append commas to whitespace for a more traditional notation:
    /// ex: -5↯2_2_3⇡12
    ///   : ⍜⊜□⍚(⊂@,)∈" \n". repr # add commas
    ///   : &p ⍜▽∵⋅@-=@¯.        # replace negate glyphs with minus signs
    (1, Repr, Misc, "repr"),
    /// Encode an image into a byte array with the specified format
    ///
    /// The first argument is the format, and the second is the image.
    ///
    /// The image must be a rank 2 or 3 numeric array.
    /// Axes 0 and 1 contain the rows and columns of the image.
    /// A rank 2 array is a grayscale image.
    /// A rank 3 array is an RGB image.
    /// In a rank 3 image array, the last axis must be length 1, 2, 3, or 4.
    /// A length 1 last axis is a grayscale image.
    /// A length 2 last axis is a grayscale image with an alpha channel.
    /// A length 3 last axis is an RGB image.
    /// A length 4 last axis is an RGB image with an alpha channel.
    ///
    /// You can decode a byte array into an image with [un][img].
    ///
    /// Supported formats are `jpg`, `png`, `bmp`, `gif`, `ico`, and `qoi`.
    ///
    /// See also: [&ims]
    (2, ImageEncode, Encoding, "img"),
    /// Encode a gif into a byte array
    ///
    /// The first argument is a framerate in seconds.
    /// The second argument is the gif data and must be a rank 3 or 4 numeric array.
    /// The rows of the array are the frames of the gif, and their format must conform to that of [img].
    ///
    /// You can decode a byte array into a gif with [un][gif].
    ///
    /// See also: [&gifs]
    (2, GifEncode, Encoding, "gif"),
    /// Encode audio into a byte array
    ///
    /// The first argument is the format, the second is the audio sample rate, and the third is the audio samples.
    ///
    /// The sample rate must be a positive integer.
    ///
    /// The audio samples must be a rank 1 or 2 numeric array.
    ///
    /// A rank 1 array is a list of mono audio samples.
    /// For a rank 2 array, each row is a channel.
    ///
    /// The samples must be between -1 and 1.
    /// The sample rate is [&asr].
    ///
    /// You can decode a byte array into audio with [un][audio].
    /// This returns the audio format as a string, the audio sample rate, and an array representing the audio samples.
    ///
    /// Currently, only the `wav` format is supported.
    ///
    /// This simple example will load an audio file, halve its sample rate, and re-encode it.
    /// ex: ⍜(°audio &frab "test.wav")⊙⊓(⌊÷2|▽0.5)
    ///
    /// See also: [&ap]
    (3, AudioEncode, Encoding, "audio"),
    /// Render text into an image array
    ///
    /// In the most basic usage, the first argument is a font size and the second argument is the text to render.
    /// The result is a rank-2 array of pixel values.
    /// In this example, we map the pixel values to ASCII characters to visualize the result.
    /// ex: # Experimental!
    ///   : layout 12 "Hello!"
    ///   : ⊏:" @" ⁅ +0.1
    /// Multi-line text is supported.
    /// ex: # Experimental!
    ///   : layout 30 "Hello,\nWorld!"
    /// The text to be rendered can be a character array or box array where all leaf nodes are strings.
    /// The top-level rows are treated as lines and will be separated by newlines.
    /// The bottom-level rows are treated as words and will be separated by spaces.
    /// ex: # Experimental!
    ///   : {{"Words" "can" "be" "on"}
    ///   :  {"multiple" "lines"}}
    ///   : layout 30
    /// ex: # Experimental!
    ///   : layout 15 ⬚""↯∞_12 ⊜□⊸≠@  Lorem
    ///
    /// Additionally, the first argument can be a list of options.
    /// The first scalar option is the font size (default 30)
    /// The second scalar option is the line height (default 1)
    /// The first array of 2 numbers is the canvas size. Use `∞` to use the smallest possible size.
    /// The first array of 3 or 4 numbers is the color. If set, the background defaults to transparent.
    /// ex: # Experimental!
    ///   : $ Uiua is a
    ///   : $ stack-based
    ///   : $ array-oriented
    ///   : $ programming
    ///   : $ language
    ///   : layout {30 1.5 300_350 0.5_0.5_1}
    /// [fill] sets the background color.
    /// ex: # Experimental!
    ///   : ⬚[1 0 0] layout {100 0_1_0} "Green on Red!"
    (2, Layout, Encoding, "layout", Impure),
    /// Run some code on the GPU
    ([1], Gpu, Misc, "gpu"),
);

macro_rules! impl_primitive {
    ($(
        $(#[$attr:meta])*
        (
            $args:literal
            $(($outputs:expr))?
            $([$margs:expr])?,
            $variant:ident
            $(, $purity:ident)?
        )
    ),* $(,)?) => {
        /// Primitives that exist as an implementation detail
        #[doc(hidden)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
        pub enum ImplPrimitive {
            $(
                $(#[$attr])*
                $variant,
            )*
            TransposeN(i32),
            UndoTransposeN(usize, i32),
            UndoReverse { n: usize, all: bool },
            UndoRotate(usize),
            ReduceDepth(usize),
            TraceN { n: usize, inverse: bool, stack_sub: bool},
        }

        impl ImplPrimitive {
            pub fn args(&self) -> usize {
                match self {
                    $(ImplPrimitive::$variant => $args,)*
                    ImplPrimitive::TransposeN(_) => 1,
                    ImplPrimitive::UndoTransposeN(n, _) => *n,
                    ImplPrimitive::UndoReverse { n, .. } => *n,
                    ImplPrimitive::UndoRotate(n) => *n + 1,
                    ImplPrimitive::ReduceDepth(_) => 1,
                    ImplPrimitive::TraceN { n, .. } => *n,
                }
            }
            pub fn outputs(&self) -> usize {
                match self {
                    $($(ImplPrimitive::$variant => $outputs,)?)*
                    ImplPrimitive::UndoTransposeN(n, _) => *n,
                    ImplPrimitive::UndoReverse { n, .. } => *n,
                    ImplPrimitive::UndoRotate(n) => *n,
                    ImplPrimitive::TraceN { n, .. } => *n,
                    _ => 1
                }
            }
            pub fn modifier_args(&self) -> Option<usize> {
                match self {
                    $($(ImplPrimitive::$variant => Some($margs),)?)*
                    ImplPrimitive::ReduceDepth(_) => Some(1),
                    _ => None
                }
            }
            pub fn purity(&self) -> Purity {
                match self {
                    $($(ImplPrimitive::$variant => {Purity::$purity},)*)*
                    ImplPrimitive::TraceN { .. } => Purity::Impure,
                    _ => Purity::Pure
                }
            }
        }
    };
}

impl_primitive!(
    // Inverses
    (0, UnPop),
    (1, Asin),
    (1, UnBits),
    (1, UnWhere),
    (1(2), UnCouple),
    (1, UnUtf),
    (1, UnGraphemes),
    (1(2), UnAtan),
    (1(2), UnComplex),
    (1, UnParse),
    (1, UnFix),
    (1, UnShape),
    (1[1], UnScan),
    (1(2), UnMap),
    (0(0), UnStack, Impure),
    (0(0)[1], UnDump, Impure),
    (0[2], UnFill),
    (1, Primes),
    (1, UnBox),
    (2, AntiDrop),
    (2, AntiSelect),
    (2, AntiPick),
    (1(2), UnJoin),
    (1(2), UnJoinEnd),
    (2(2), UnJoinShape),
    (2(2), UnJoinShapeEnd),
    (1(2), UnKeep),
    (1, UnSort, Impure),
    (1, UnJson),
    (1, UnCsv),
    (1, UnXlsx),
    (1, UnFft),
    (1, UnDatetime),
    (2, ProgressiveIndexOf),
    (2(0), MatchPattern),
    (2(1), MatchLe),
    (2(1), MatchGe),
    (1(2), ImageDecode),
    (1(2), GifDecode),
    (1(3), AudioDecode),
    (0(1), UnRawMode, Impure),
    (1(0), UnClip, Mutating),
    // Unders
    (1, UndoFix),
    (2, UndoUnbits),
    (2, AntiBase),
    (2, UndoDeshape),
    (3, UndoSelect),
    (3, UndoPick),
    (3, UndoTake),
    (3, UndoDrop),
    (2, UndoFirst),
    (2, UndoLast),
    (3, UndoKeep),
    (3, UndoRerank),
    (2, UndoReshape),
    (2, UndoWindows),
    (2, UndoChunks),
    (2, UndoWhere),
    (2, AntiOrient),
    (3(2), UndoJoin),
    (1[1], UndoPartition1),
    (3, UndoPartition2),
    (1[1], UndoGroup1),
    (3, UndoGroup2),
    (4, UndoInsert),
    (3, UndoRemove),
    (1(0), TryClose),
    (2[1], UnBoth),
    // Optimizations
    (1, FirstMinIndex),
    (1, FirstMaxIndex),
    (1, LastMinIndex),
    (1, LastMaxIndex),
    (1, FirstWhere),
    (1, LastWhere),
    (1, LenWhere),
    (2, MemberOfRange),
    (2, MultidimMemberOfRange),
    (1, RandomRow, Impure),
    (1, SortDown),
    (1[1], ReduceContent),
    (2[2], ReduceTable),
    (1, ReplaceRand, Impure),
    (2, ReplaceRand2, Impure),
    (2[1], Adjacent),
    (2[1], RowsWindows),
    (1, CountUnique),
    (1(2)[3], AstarFirst),
    (2, Root),
    // Implementation details
    (1[2], RepeatWithInverse),
    (2(1), ValidateType),
    (2(0), ValidateTypeConsume),
    (2(0), TestAssert, Impure),
    /// Validate that a non-boxed variant field has a valid type and rank
    (1, ValidateNonBoxedVariant),
    (2(1), ValidateVariant),
    (2(1), TagVariant),
);
