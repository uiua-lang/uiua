use std::{
    f64::consts::TAU,
    path::{Path, PathBuf},
    sync::OnceLock,
};

use ecow::EcoVec;
use once_cell::sync::Lazy;
use rand::prelude::*;

use crate::{
    parse_doc_line_fragments, Array, Boxed, PrimDocFragment, SysBackend, Value, WILDCARD_NAN,
};

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
    /// The suggested replacement because of deprecation
    pub deprecation: Option<&'static str>,
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
    /// Check if the constant is deprecated
    pub fn is_deprecated(&self) -> bool {
        self.deprecation.is_some()
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
    ($(
        $(#[doc = $doc:literal])+
        (
            $(#[$attr:meta])*
            $name:literal,
            $class:ident,
            $value:expr
            $(, deprecated($deprecation:literal))?
        )
    ),* $(,)?) => {
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
        #[allow(path_statements)]
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
                    deprecation: { None::<&'static str> $(; Some($deprecation))? }
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
    Spatial,
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
    /// The machine epsilon for Uiua numbers
    ///
    /// It is the difference between 1 and the next larger representable number.
    ("ε", Math, f64::EPSILON),
    /// 1-dimensional adjacent neighbors offsets
    ("A₁", Spatial, [1, -1]),
    /// 2-dimensional adjacent neighbors offsets
    ("A₂", Spatial, [[0, 1], [1, 0], [0, -1], [-1, 0]]),
    /// 3-dimensional adjacent neighbors offsets
    ("A₃", Spatial, [[0, 1, 0], [1, 0, 0], [0, -1, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1]]),
    /// 2-dimensional corner neighbors offsets
    ("C₂", Spatial, [[1, 1], [1, -1], [-1, -1], [-1, 1]]),
    /// 3-dimensional corner neighbors offsets
    ("C₃", Spatial, [
        [1, 1, 1], [1, -1, 1], [-1, -1, 1], [-1, 1, 1],
        [1, 1, -1], [1, -1, -1], [-1, -1, -1], [-1, 1, -1]
    ]),
    /// 3-dimensional edge neighbors offsets
    ("E₃", Spatial, [
        [1, 1, 0], [1, -1, 0], [-1, -1, 0], [-1, 1, 0],
        [0, 1, 1], [1, 0, 1], [0, -1, 1], [-1, 0, 1],
        [0, 1, -1], [1, 0, -1], [0, -1, -1], [-1, 0, -1]
    ]),
    /// A string identifying the operating system
    ("Os", System, std::env::consts::OS, deprecated("Use the os function instead")),
    /// A string identifying family of the operating system
    ("Family", System, std::env::consts::FAMILY, deprecated("Use the osfamily function instead")),
    /// A string identifying the architecture of the CPU
    ("Arch", System, std::env::consts::ARCH, deprecated("Use the arch function instead")),
    /// The executable file extension
    ("ExeExt", System, std::env::consts::EXE_EXTENSION, deprecated("Use the exeext function instead")),
    /// The file extension for shared libraries
    ("DllExt", System, std::env::consts::DLL_EXTENSION, deprecated("Use the dllext function instead")),
    /// The primary path separator character
    ("Sep", System, std::path::MAIN_SEPARATOR, deprecated("Use the pathsep function instead")),
    /// The path of the current source file relative to `WorkingDir`
    ("ThisFile", System, ConstantValue::ThisFile),
    /// The name of the current source file
    ("ThisFileName", System, ConstantValue::ThisFileName),
    /// The name of the directory containing the current source file
    ("ThisFileDir", System, ConstantValue::ThisFileDir),
    /// The compile-time working directory
    ("WorkingDir", System, ConstantValue::WorkingDir),
    /// The number of processors available
    ("NumProcs", System, num_cpus::get() as f64, deprecated("Use the numprocs function instead")),
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
    (#[cfg(feature = "image")] "Logo", Media, crate::media::image_bytes_to_array(include_bytes!("assets/uiua-logo-512.png"), true).unwrap()),
    /// Ethically sourced Lena picture
    /// Morten Rieger Hannemose
    /// 2019
    /// https://mortenhannemose.github.io/lena/
    (#[cfg(feature = "image")] "Lena", Media, crate::media::image_bytes_to_array(include_bytes!("assets/lena.jpg"), false).unwrap()),
    /// A picture of two cats
    ///
    /// Their names are Murphy and Louie
    (#[cfg(feature = "image")] "Cats", Media, crate::media::image_bytes_to_array(include_bytes!("assets/cats.webp"), false).unwrap()),
    /// Sample music data
    ("Music", Media, ConstantValue::Music),
    /// Lorem Ipsum text
    ("Lorem", Media, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
    /// Rainbow flag colors
    ("Rainbow", Flags, [[0.894, 0.012, 0.012], [1.0, 0.647, 0.173], [1.0, 1.0, 0.255], [0.0, 0.502, 0.094], [0.0, 0.0, 0.976], [0.525, 0.0, 0.49]]),
    /// Lesbian flag colors
    ("Lesbian", Flags, [[0.831, 0.173, 0.0], [0.992, 0.596, 0.333], [1.0, 1.0, 1.0], [0.82, 0.38, 0.635], [0.635, 0.004, 0.38]]),
    /// Gay flag colors
    ("Gay", Flags, [[0.031, 0.55, 0.44], [0.149, 0.808, 0.667], [0.596, 0.91, 0.757], [1.0, 1.0, 1.0], [0.482, 0.678, 0.886], [0.314, 0.286, 0.8], [0.239, 0.102, 0.471]]),
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
            .skip_while(|def| def.name != "Rainbow")
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
            .skip_while(|def| def.name != "Rainbow")
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
