use std::{
    any::Any,
    fmt,
    mem::take,
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::{Arc, OnceLock},
    time::Duration,
};

use enum_iterator::{all, Sequence};
#[cfg(feature = "image")]
use image::DynamicImage;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use serde::*;
use time::UtcOffset;

use crate::{
    algorithm::validate_size, cowslice::cowslice, primitive::PrimDoc, Array, Boxed, FfiType,
    Purity, Signature, Uiua, UiuaResult, Value,
};

/// The text of Uiua's example module
pub const EXAMPLE_UA: &str = "\
# Uiua's example module

Square ← ×.
Double ← +.
Increment ← +1
RangeDiff ↚ ⇡-
Span ← +⟜RangeDiff
Mac! ← /^0 [1 2 3 4 5]
Foo ← 5
Bar ← \"bar\"";

/// The text of Uiua's example text file
pub const EXAMPLE_TXT: &str = "\
This is a simple text file for 
use in example Uiua code ✨";

/// Access the built-in `example.ua` file
pub fn example_ua<T>(f: impl FnOnce(&mut String) -> T) -> T {
    static S: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(EXAMPLE_UA.to_string()));
    f(&mut S.lock())
}

/// Access the built-in `example.txt` file
pub fn example_txt<T>(f: impl FnOnce(&mut String) -> T) -> T {
    static S: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(EXAMPLE_TXT.to_string()));
    f(&mut S.lock())
}

macro_rules! sys_op {
    ($(
        #[doc = $doc_rust:literal]
        $(#[doc = $doc:literal])*
        (
            $args:literal$(($outputs:expr))?$([$mod_args:expr])?,
            $variant:ident, $class:ident, $name:literal, $long_name:literal
            $(,$purity:ident)*
        )
    ),* $(,)?) => {
        /// A system function
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence, Serialize, Deserialize)]
        #[serde(rename_all = "snake_case")]
        pub enum SysOp {
            $(
                #[doc = $doc_rust]
                $variant
            ),*
        }

        impl SysOp {
            /// All system functions
            pub const ALL: [Self; 0 $(+ {stringify!($variant); 1})*] = [
                $(Self::$variant,)*
            ];
            /// Get the system function's short name
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $name),*
                }
            }
            /// Get the system function's long name
            pub fn long_name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $long_name),*
                }
            }
            /// Get the number of arguments the system function expects
            pub fn args(&self) -> usize {
                match self {
                    $(SysOp::$variant => $args,)*
                }
            }
            /// Get the number of function arguments the system function expects if it is a modifier
            pub fn modifier_args(&self) -> Option<usize> {
                match self {
                    $($(
                        SysOp::$variant => Some($mod_args),
                    )?)*
                    _ => None
                }
            }
            /// Get the number of outputs the system function returns
            pub fn outputs(&self) -> usize {
                match self {
                    $($(SysOp::$variant => $outputs as usize,)?)*
                    _ => 1
                }
            }
            /// Get the documentation for the system function
            pub fn doc(&self) -> &'static PrimDoc {
                match self {
                    $(SysOp::$variant => {
                        let doc_str = concat!($doc_rust, $($doc, "\n"),*);
                        static DOC: OnceLock<PrimDoc> = OnceLock::new();
                        DOC.get_or_init(|| PrimDoc::from_lines(doc_str))
                    },)*
                }
            }
            /// Get the system function's class
            pub fn class(&self) -> SysOpClass {
                match self {
                    $(SysOp::$variant => SysOpClass::$class),*
                }
            }
            /// Whether the system function is pure
            pub fn purity(&self) -> Purity {
                match self {
                    $($(SysOp::$variant => Purity::$purity,)*)*
                    _ => Purity::Impure
                }
            }
        }
    };
}

/// Categories of system functions
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
#[allow(missing_docs)]
pub enum SysOpClass {
    Filesystem,
    StdIO,
    Env,
    Stream,
    Command,
    Media,
    Tcp,
    Ffi,
    Misc,
}

impl SysOpClass {
    /// All system function classes
    pub fn all() -> impl Iterator<Item = Self> {
        all()
    }
}

sys_op! {
    /// Print a nicely formatted representation of a value to stdout
    (1(0), Show, StdIO, "&s", "show", Mutating),
    /// Print a value to stdout
    (1(0), Prin, StdIO, "&pf", "print and flush", Mutating),
    /// Print a value to stdout followed by a newline
    (1(0), Print, StdIO, "&p", "print with newline", Mutating),
    /// Read a line from stdin
    ///
    /// The normal output is a string.
    /// If EOF is reached, the number `0` is returned instead.
    /// Programs that wish to properly handle EOF should check for this.
    (0, ScanLine, StdIO, "&sc", "scan line", Mutating),
    /// Get the size of the terminal
    ///
    /// The result is a 2-element array of the height and width of the terminal.
    /// Height comes first so that the array can be used as a shape in [reshape].
    (0, TermSize, Env, "&ts", "terminal size", Mutating),
    /// Exit the program with a status code
    (1(0), Exit, Misc, "&exit", "exit", Mutating),
    /// Set the terminal to raw mode
    ///
    /// Expects a boolean.
    /// If enabled, the terminal will not echo characters or wait for a newline.
    ///
    /// [&sc] will still work, but it will not return until the user presses enter.
    /// To get individual characters, use [&rs] or [&rb] with a count of `1` and a handle of `0`, which is stdin.
    ///
    /// [un][&raw] will return the current state of the terminal.
    /// [under][&raw] will set raw mode, and then revert it to the previous state.
    (1(0), RawMode, Env, "&raw", "set raw mode", Mutating),
    /// Get the command line arguments
    ///
    /// The first element will always be the name of your script
    (0, Args, Env, "&args", "arguments"),
    /// Get the value of an environment variable
    ///
    /// Expects a string and returns a string.
    /// If the environment variable does not exist, an error is thrown.
    (1, Var, Env, "&var", "environment variable"),
    /// Run a command and wait for it to finish
    ///
    /// Standard IO will be inherited. Returns the exit code of the command.
    ///
    /// Expects either a string, a rank `2` character array, or a rank `1` array of [box] strings.
    (1, RunInherit, Command, "&runi", "run command inherit", Mutating),
    /// Run a command and wait for it to finish
    ///
    /// Standard IO will be captured. The exit code, stdout, and stderr will each be pushed to the stack.
    ///
    /// Expects either a string, a rank `2` character array, or a rank `1` array of [box] strings.
    (1(3), RunCapture, Command, "&runc", "run command capture", Mutating),
    /// Run a command with streaming IO
    ///
    /// Expects either a string, a rank `2` character array, or a rank `1` array of [box] strings.
    /// Returns 3 stream handles.
    /// The first can be written to with [&w] to send input to the command's stdin.
    /// The second and third can be read from with [&rs], [&rb], or [&ru] to read from the command's stdout and stderr.
    /// Using [&cl] on *all 3* handles will kill the child process.
    /// [under][&runs] calls [&cl] on all 3 streams automatically.
    (1(3), RunStream, Command, "&runs", "run command stream", Mutating),
    /// Change the current directory
    (1(0), ChangeDirectory, Filesystem, "&cd", "change directory", Mutating),
    /// Get the contents of the clipboard
    ///
    /// Returns a string of the clipboard's contents.
    /// This is not supported on the web.
    ///
    /// See also: [&clset]
    (0, ClipboardGet, Misc, "&clget", "get clipboard contents"),
    /// Set the contents of the clipboard
    ///
    /// Expects a string.
    /// ex: &clset +@A⇡6 # Try running then pasting!
    ///
    /// See also: [&clget]
    (1(0), ClipboardSet, Misc, "&clset", "set clipboard contents", Mutating),
    /// Sleep for n seconds
    ///
    /// On the web, this example will hang for 1 second.
    /// ex: ⚂ &sl 1
    (1(0), Sleep, Misc, "&sl", "sleep", Mutating),
    /// Read characters formed by at most n bytes from a stream
    ///
    /// Expects a count and a stream handle.
    /// The stream handle `0` is stdin.
    /// ex: &rs 4 &fo "example.txt"
    /// Using [infinity] as the count will read until the end of the stream.
    /// ex: &rs ∞ &fo "example.txt"
    ///
    /// [&rs] will attempt to read the given number of *bytes* from the stream.
    /// If the read bytes are not valid UTF-8, up to 3 additional bytes will be read in an attempt to finish a valid UTF-8 character.
    ///
    /// See also: [&rb]
    (2, ReadStr, Stream, "&rs", "read to string", Mutating),
    /// Read at most n bytes from a stream
    ///
    /// Expects a count and a stream handle.
    /// The stream handle `0` is stdin.
    /// ex: &rb 4 &fo "example.txt"
    /// Using [infinity] as the count will read until the end of the stream.
    /// ex: &rb ∞ &fo "example.txt"
    ///
    /// See also: [&rs]
    (2, ReadBytes, Stream, "&rb", "read to bytes", Mutating),
    /// Read from a stream until a delimiter is reached
    ///
    /// Expects a delimiter and a stream handle.
    /// The result will be a rank-`1` byte or character array. The type will match the type of the delimiter.
    /// The stream handle `0` is stdin.
    /// ex: &ru "Uiua" &fo "example.txt"
    (2, ReadUntil, Stream, "&ru", "read until", Mutating),
    /// Write an array to a stream
    ///
    /// If the stream is a file, the file may not be written to until it is closed with [&cl].
    /// The stream handle `1` is stdout.
    /// The stream handle `2` is stderr.
    /// ex: &cl &w "Hello, world!" . &fc "file.txt"
    ///   : &fras "file.txt"
    (2(0), Write, Stream, "&w", "write", Mutating),
    /// Invoke a path with the system's default program
    (1(1), Invoke, Command, "&invk", "invoke", Mutating),
    /// Close a stream by its handle
    ///
    /// This will close files, tcp listeners, and tcp sockets.
    (1(0), Close, Stream, "&cl", "close handle", Mutating),
    /// Open a file and return a handle to it
    ///
    /// ex: &fo "example.txt"
    /// The file can be read from with [&rs], [&rb], or [&ru].
    /// The file can be written to with [&w].
    /// In some cases, the file may not be actually written to until it is closed with [&cl].
    /// [under][&fo] calls [&cl] automatically.
    (1, FOpen, Filesystem, "&fo", "file - open"),
    /// Create a file and return a handle to it
    ///
    /// ex: &fc "file.txt"
    /// The file can be read from with [&rs], [&rb], or [&ru].
    /// The file can be written to with [&w].
    /// In some cases, the file may not be actually written to until it is closed with [&cl].
    /// [under][&fc] calls [&cl] automatically.
    (1, FCreate, Filesystem, "&fc", "file - create", Mutating),
    /// Create a directory
    ///
    /// ex: &fmd "path/to/dir"
    /// Nested directories will be created automatically.
    (1(0), FMakeDir, Filesystem, "&fmd", "file - make directory", Mutating),
    /// Delete a file or directory
    ///
    /// ex: &fde "example.txt"
    /// Deletes the file or directory at the given path.
    /// Be careful with this function, as deleted files and directories cannot be recovered!
    /// For a safer alternative, see [&ftr].
    (1(0), FDelete, Filesystem, "&fde", "file - delete", Mutating),
    /// Move a file or directory to the trash
    ///
    /// ex: &ftr "example.txt"
    /// Moves the file or directory at the given path to the trash.
    /// This is a safer alternative to [&fde].
    (1(0), FTrash, Filesystem, "&ftr", "file - trash", Mutating),
    /// Check if a file, directory, or symlink exists at a path
    ///
    /// ex: &fe "example.txt"
    /// ex: &fe "foo.bar"
    (1, FExists, Filesystem, "&fe", "file - exists"),
    /// List the contents of a directory
    ///
    /// The result is a list of boxed strings.
    /// ex: &fld "."
    (1, FListDir, Filesystem, "&fld", "file - list directory"),
    /// Check if a path is a file
    ///
    /// ex: &fif "example.txt"
    (1, FIsFile, Filesystem, "&fif", "file - is file"),
    /// Read all the contents of a file into a string
    ///
    /// Expects a path and returns a rank-`1` character array.
    ///
    /// ex: &fras "example.txt"
    /// You can use [under][&fras] to write back to the file after modifying the string.
    /// ex: ⍜&fras(⊂:"\n# Wow!") "example.txt"
    ///   : &p&fras "example.txt"
    ///
    /// See [&frab] for reading into a byte array.
    (1, FReadAllStr, Filesystem, "&fras", "file - read all to string"),
    /// Read all the contents of a file into a byte array
    ///
    /// Expects a path and returns a rank-`1` numeric array.
    ///
    /// ex: &frab "example.txt"
    /// You can use [under][&frab] to write back to the file after modifying the array.
    /// ex: ⍜&frab(⊂:-@\0"\n# Wow!") "example.txt"
    ///   : &p&fras "example.txt"
    ///
    /// See [&fras] for reading into a rank-`1` character array.
    (1, FReadAllBytes, Filesystem, "&frab", "file - read all to bytes"),
    /// Write the entire contents of an array to a file
    ///
    /// Expects a path and a rank-`1` array of either numbers or characters.
    /// The file will be created if it does not exist and overwritten if it does.
    ///
    /// The editor on the website has a virtual filesystem. Files written with [&fwa] can be read with [&fras] or [&frab].
    /// ex: Path ← "test.txt"
    ///   : &fwa Path +@A⇡26
    ///   : &fras Path
    (2(0), FWriteAll, Filesystem, "&fwa", "file - write all", Mutating),
    /// Show an image
    ///
    /// How the image is shown depends on the system backend.
    ///
    /// In the default backend, the image is shown in the terminal.
    /// On the web, the image is shown in the output area.
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
    /// See also: [img]
    (1(0), ImShow, Media, "&ims", "image - show", Mutating),
    /// Show a gif
    ///
    /// The first argument is a framerate in seconds.
    /// The second argument is the gif data and must be a rank 3 or 4 numeric array.
    /// The rows of the array are the frames of the gif, and their format must conform to that of [img].
    ///
    /// See also: [gif]
    (2(0), GifShow, Media, "&gifs", "gif - show", Mutating),
    /// Play some audio
    ///
    /// The audio must be a rank 1 or 2 numeric array.
    ///
    /// A rank 1 array is a list of mono audio samples.
    /// For a rank 2 array, each row is a channel.
    ///
    /// The samples must be between -1 and 1.
    /// The sample rate is [&asr].
    ///
    /// See also: [audio]
    (1(0), AudioPlay, Media, "&ap", "audio - play", Mutating),
    /// Get the sample rate of the audio output backend
    ///
    /// ex: &asr
    /// Here is how you can generate a list of sample times for `4` seconds of audio:
    /// ex: ÷:⇡×, 4 &asr
    /// Pass that to a periodic function, and you get a nice tone!
    /// ex: ÷4∿×τ×220 ÷:⇡×, 4 &asr
    (0, AudioSampleRate, Media, "&asr", "audio - sample rate"),
    /// Synthesize and stream audio
    ///
    /// Expects a function that takes a list of sample times and returns a list of samples.
    /// The samples returned from the function must either be a rank 1 array or a rank 2 array with 2 rows.
    /// The function will be called repeatedly to generate the audio.
    /// ex: Sp ← 1.5
    ///   : Bass ← (
    ///   :   +110×20⌊÷4◿8. # Choose note
    ///   :   ±∿×π×÷Sp,     # Square wave
    ///   :   ×2+1⌊◿2.:     # Modulation frequency
    ///   :   ×0.2∿×π××:    # Apply modulation
    ///   : )
    ///   : Kick ← ∿×τ×40√√◿1
    ///   : Noise ← [⍥⚂10000]
    ///   : Hit ← ×↯:Noise △. ×⊓><0.5,0.6 ÷⟜◿2
    ///   : Hat ← ×0.3×↯:Noise △.<0.1 ÷⟜◿0.25
    ///   : &ast(÷3/+[⊃(Hat|Kick|Hit|Bass)]×Sp)
    /// On the web, this will simply use the function to generate a fixed amount of audio.
    /// How long the audio is can be configure in the editor settings.
    (0(0)[1], AudioStream, Media, "&ast", "audio - stream", Mutating),
    /// Create a TCP listener and bind it to an address
    ///
    /// Use [&tcpa] on the returned handle to accept connections.
    ///
    /// See also: [&tlsl]
    (1, TcpListen, Tcp, "&tcpl", "tcp - listen", Mutating),
    /// Create a TLS listener and bind it to an address
    ///
    /// Use [&tcpa] on the returned handle to accept connections.
    ///
    /// See also: [&tcpl]
    (1, TlsListen, Tcp, "&tlsl", "tls - listen", Mutating),
    /// Accept a connection with a TCP or TLS listener
    ///
    /// Returns a stream handle
    /// [under][&tcpl] calls [&cl] automatically.
    (1, TcpAccept, Tcp, "&tcpa", "tcp - accept", Mutating),
    /// Create a TCP socket and connect it to an address
    ///
    /// Returns a stream handle
    /// You can make a request with [&w] and read the response with [&rs], [&rb], or [&ru].
    /// [under][&tcpc] calls [&cl] automatically.
    /// ex: "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n"
    ///   : ⍜(&tlsc "example.com:443"|&rs∞⊸&w:)
    ///
    /// See also: [&tlsc]
    (1, TcpConnect, Tcp, "&tcpc", "tcp - connect", Mutating),
    /// Create a TCP socket with TLS support
    ///
    /// Returns a stream handle
    /// You can make a request with [&w] and read the response with [&rs], [&rb], or [&ru].
    /// [under][&tlsc] calls [&cl] automatically.
    /// ex:
    ///
    /// See also: [&tcpc]
    (1, TlsConnect, Tcp, "&tlsc", "tls - connect", Mutating),
    /// Set a TCP socket to non-blocking mode
    (1, TcpSetNonBlocking, Tcp, "&tcpsnb", "tcp - set non-blocking", Mutating),
    /// Set the read timeout of a TCP socket in seconds
    (2(0), TcpSetReadTimeout, Tcp, "&tcpsrt", "tcp - set read timeout", Mutating),
    /// Set the write timeout of a TCP socket in seconds
    (2(0), TcpSetWriteTimeout, Tcp, "&tcpswt", "tcp - set write timeout", Mutating),
    /// Get the connection address of a TCP socket
    (1, TcpAddr, Tcp, "&tcpaddr", "tcp - address", Mutating),
    /// Make an HTTP(S) request
    ///
    /// Takes in an 1.x HTTP request and returns an HTTP response.
    ///
    /// Requires the `Host` header to be set.
    /// Using port 443 makes an HTTPS request. Any other port makes an HTTP request.
    ///
    /// ex: &httpsw "GET / " &tcpc "example.com:443"
    ///
    /// It is also possible to put in entire HTTP requests.
    ///
    /// ex: &tcpc "example.com:443"
    ///   : &httpsw $ GET /api HTTP/1.0
    ///   :         $ Host: example.com\r\n
    ///   :         $ <BODY>
    ///
    /// There are a few things the function tries to automatically fill in if it finds they are missing from the request:
    /// - 2 trailing newlines (if there is no body)
    /// - The HTTP version
    /// - The `Host` header (if not defined)
    (2, HttpsWrite, Tcp, "&httpsw", "https - Make an HTTP(S) request", Mutating),
    /// Capture an image from a webcam
    ///
    /// Takes the index of the webcam to capture from.
    ///
    /// Returnes a rank-3 numeric array representing the image.
    (1, WebcamCapture, Misc, "&camcap", "webcam - capture", Mutating),
    /// Call a foreign function interface
    ///
    /// *Warning ⚠️: Using FFI is deeply unsafe. Calling a function incorrectly is undefined behavior.*
    ///
    /// The first argument is a list of boxed strings specifying the source and signature of the foreign function.
    /// The second argument is a list of values to pass to the foreign function.
    ///
    /// The first argument must be of the form `{"lib_path" "return_type" "function_name" "arg1_type" "arg2_type" …}`.
    /// The lib path is the path to the shared library or DLL that contains the function.
    /// Type names roughly match C types. The available primitive types are:
    /// - `void`
    /// - `char`
    /// - `short`
    /// - `int`
    /// - `long`
    /// - `long long`
    /// - `float`
    /// - `double`
    /// - `unsigned char`
    /// - `unsigned short`
    /// - `unsigned int`
    /// - `unsigned long`
    /// - `unsigned long long`
    /// Suffixing any of these with `*` makes them a pointer type.
    /// Struct types are defined as a list of types between `{}`s separated by `;`s, i.e. `{int; float}`. A trailing `;` is optional.
    ///
    /// Arguments are passed as a list of boxed values.
    /// If we have a C function `int add(int a, int b)` in a shared library `example.dll`, we can call it like this:
    /// ex! # Experimental!
    ///   : Lib ← &ffi ⊂□"example.dll"
    ///   : Add ← Lib {"int" "add" "int" "int"}
    ///   : Add {2 3} # 5
    ///
    /// Uiua arrays can be passed to foreign functions as pointer-length pairs.
    /// To do this, specify the type of the list items followed by `:n`, where `n` is the index of the parameter that corresponds to the length.
    /// The interpreter will automatically pass the number of elements in the array to this parameter.
    /// Arrays passed in this way will be implicitely [deshape]ed, unless the item type is a struct.
    /// If we wave a C function `int sum(const int* arr, int len)` in a shared library `example.dll`, we can call it like this:
    /// ex! # Experimental!
    ///   : Lib ← &ffi ⊂□"example.dll"
    ///   : Sum ← Lib {"int" "sum" "const int:1" "int"}
    ///   : Sum {[1 2 3 4 5]} # 15
    ///
    /// [&ffi] calls can return multiple values.
    /// In addition to the return value, any non-`const` pointer parameters will be interpreted as out-parameters.
    /// If there is more than one output value (including the return value), [&ffi] will return a list of the boxed output values.
    /// If we have a C function `int split_head(int* list, int* len)` in a shared library `example.dll`, we can call it like this:
    /// ex! # Experimental!
    ///   : Lib ← &ffi ⊂□"example.dll"
    ///   : SplitHead ← Lib {"int" "split_head" "int:1" "int*"}
    ///   : SplitHead {[1 2 3 4 5]} # {1 [2 3 4 5]}
    /// Note that the length parameter is a non-`const` pointer. This is because the function will modify it.
    ///
    /// `const char*` parameters and return types are interpreted as null-terminated strings, without an associated length parameter.
    ///
    /// Structs can be passed either as lists of boxed values or, if all fields are of the same type, as a normal array.
    /// If all fields of a struct returned by a foreign function are of the same type, the interpreter will automatically interpret it as an array rather than a list of boxed values.
    /// If we have a C struct `struct Vec2 { float x; float y; }` and a function `Vec2 vec2_add(Vec2 a, Vec2 b)` in a shared library `example.dll`, we can call it like this:
    /// ex! # Experimental!
    ///   : Lib ← &ffi ⊂□"example.dll"
    ///   : VecII ← "{float; float}"
    ///   : Add ← Lib {VecII "vec2_add" VecII VecII}
    ///   : Add {[1 2] [3 4]} # [4 6]
    ///
    /// If a foreign function returns or has an out-parameter that is a pointer type, a special array is returned representing the pointer. This array is not useful as a normal array, but it can be passed back as an [&ffi] argument, read from with [&memcpy], or freed with [&memfree].
    ///
    /// Coverage of types that are supported for binding is currently best-effort.
    /// If you encounter a type that you need support for, please [open an issue](https://github.com/uiua-lang/uiua/issues/new).
    (2, Ffi, Ffi, "&ffi", "foreign function interface", Mutating),
    /// Copy data from a pointer into an array
    ///
    /// *Warning ⚠️: [&memcpy] can lead to undefined behavior if used incorrectly.*
    ///
    /// This is useful for complex [&ffi] calls that are meant to return arrays.
    /// Expects a string indicating the type, a pointer, and a length.
    ///
    /// The returned array will always be rank-`1`.
    /// The type of the array depends on the given type.
    /// Types are specified in the same way as in [&ffi].
    /// `"char"` will create a character array.
    /// `"unsigned char"` will create a number array with efficient byte storage.
    /// All other number types will create a normal number array.
    ///
    /// For example, if we have a C function `int* get_ints(int len)` in a shared library `example.dll`, we can call it and copy the result like this:
    /// ex! # Experimental!
    ///   : Lib ← &ffi ⊂□"example.dll"
    ///   : GetInts ← Lib {"int*" "get_ints" "int"}
    ///   : &memcpy "int":3 GetInts 3
    ///
    /// Importantly, [&memcpy] does *not* free the memory allocated by the foreign function.
    /// Use [&memfree] to free the memory.
    /// ex! # Experimental!
    ///   : Lib ← &ffi ⊂□"example.dll"
    ///   : GetInts ← Lib {"int*" "get_ints" "int"}
    ///   : ⊃&memfree(&memcpy "int":3) GetInts 3
    (3, MemCopy, Ffi, "&memcpy", "foreign function interface - copy", Mutating),
    /// Free a pointer
    ///
    /// *Warning ⚠️: [&memfree] can lead to undefined behavior if used incorrectly.*
    ///
    /// This is useful for freeing memory allocated by a foreign function.
    /// Expects a pointer.
    /// See [&memcpy] for an example.
    (1(0), MemFree, Ffi, "&memfree", "free memory", Mutating),
}

/// A handle to an IO stream
///
/// 0 is stdin, 1 is stdout, 2 is stderr.
///
/// Other handles can be used by files or sockets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Handle(pub u64);

impl Handle {
    const STDIN: Self = Self(0);
    const STDOUT: Self = Self(1);
    const STDERR: Self = Self(2);
    /// The first handle that can be used by the user
    pub const FIRST_UNRESERVED: Self = Self(3);
}

impl From<usize> for Handle {
    fn from(n: usize) -> Self {
        Self(n as u64)
    }
}

impl Handle {
    pub(crate) fn value(self, kind: HandleKind) -> Value {
        let mut arr = Array::from(self.0 as f64);
        arr.meta_mut().handle_kind = Some(kind);
        Boxed(arr.into()).into()
    }
}

impl Value {
    /// Attempt to convert the array to systme handle
    pub fn as_handle(&self, env: &Uiua, mut expected: &'static str) -> UiuaResult<Handle> {
        if expected.is_empty() {
            expected = "Expected value to be a handle";
        }
        match self {
            Value::Box(b) => {
                if let Some(b) = b.as_scalar() {
                    b.0.as_nat(env, expected).map(|h| Handle(h as u64))
                } else {
                    Err(env.error(format!("{expected}, but it is rank {}", b.rank())))
                }
            }
            value => value.as_nat(env, expected).map(|h| Handle(h as u64)),
        }
    }
}

/// The function type passed to `&ast`
pub type AudioStreamFn = Box<dyn FnMut(&[f64]) -> UiuaResult<Vec<[f64; 2]>> + Send>;

/// The kind of a handle
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum HandleKind {
    File(PathBuf),
    TcpListener(SocketAddr),
    TlsListener(SocketAddr),
    TcpSocket(SocketAddr),
    TlsSocket(SocketAddr),
    ChildStdin(String),
    ChildStdout(String),
    ChildStderr(String),
}

impl fmt::Display for HandleKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::File(path) => write!(f, "file {}", path.display()),
            Self::TcpListener(addr) => write!(f, "tcp listener {}", addr),
            Self::TlsListener(addr) => write!(f, "tls listener {}", addr),
            Self::TcpSocket(addr) => write!(f, "tcp socket {}", addr),
            Self::TlsSocket(addr) => write!(f, "tls socket {}", addr),
            Self::ChildStdin(com) => write!(f, "stdin {com}"),
            Self::ChildStdout(com) => write!(f, "stdout {com}"),
            Self::ChildStderr(com) => write!(f, "stderr {com}"),
        }
    }
}

#[cfg(feature = "image")]
pub(crate) type WebcamImage = image::RgbImage;
#[cfg(not(feature = "image"))]
pub(crate) type WebcamImage = ();

/// Trait for defining a system backend
#[allow(unused_variables)]
pub trait SysBackend: Any + Send + Sync + 'static {
    /// Cast the backend to `&dyn Any`
    fn any(&self) -> &dyn Any;
    /// Cast the backend to `&mut dyn Any`
    fn any_mut(&mut self) -> &mut dyn Any;
    /// Save a color-formatted version of an error message for later printing
    fn save_error_color(&self, message: String, colored: String) {}
    /// Print a string (without a newline) to stdout
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        Err("Printing to stdout is not supported in this environment".into())
    }
    /// Print a string (without a newline) to stderr
    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        Err("Printing to stderr is not supported in this environment".into())
    }
    /// Print a string that was create by `trace`
    fn print_str_trace(&self, s: &str) {}
    /// Read a line from stdin
    ///
    /// Should return `Ok(None)` if EOF is reached.
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        Err("Reading from stdin is not supported in this environment".into())
    }
    /// Read a number of bytes from stdin
    fn scan_stdin(&self, count: usize) -> Result<Vec<u8>, String> {
        Err("Reading from stdin is not supported in this environment".into())
    }
    /// Read from stdin until a delimiter is reached
    fn scan_until_stdin(&self, delim: &[u8]) -> Result<Vec<u8>, String> {
        let mut buffer = Vec::new();
        loop {
            let bytes = self.scan_stdin(1)?;
            if bytes.is_empty() {
                break;
            }
            buffer.extend_from_slice(&bytes);
            if buffer.ends_with(delim) {
                break;
            }
        }
        Ok(buffer)
    }
    /// Set the terminal to raw mode
    fn set_raw_mode(&self, raw_mode: bool) -> Result<(), String> {
        Err("Setting raw mode is not supported in this environment".into())
    }
    /// Get the terminal raw mode
    fn get_raw_mode(&self) -> Result<bool, String> {
        Err("Getting raw mode is not supported in this environment".into())
    }
    /// Get an environment variable
    fn var(&self, name: &str) -> Option<String> {
        None
    }
    /// Get the size of the terminal
    fn term_size(&self) -> Result<(usize, usize), String> {
        Err("Getting the terminal size is not supported in this environment".into())
    }
    /// Exit the program with a status code
    fn exit(&self, status: i32) -> Result<(), String> {
        Err("Exiting is not supported in this environment".into())
    }
    /// Check if a file or directory exists
    fn file_exists(&self, path: &str) -> bool {
        false
    }
    /// List the contents of a directory
    fn list_dir(&self, path: &str) -> Result<Vec<String>, String> {
        Err("Listing directories is not supported in this environment".into())
    }
    /// Check if a path is a file
    fn is_file(&self, path: &str) -> Result<bool, String> {
        Err("Checking if a path is a file is not supported in this environment".into())
    }
    /// Delete a file or directory
    fn delete(&self, path: &str) -> Result<(), String> {
        Err("Deleting files is not supported in this environment".into())
    }
    /// Move a file or directory to the trash
    fn trash(&self, path: &str) -> Result<(), String> {
        Err("Trashing files is not supported in this environment".into())
    }
    /// Read at most `count` bytes from a stream
    fn read(&self, handle: Handle, count: usize) -> Result<Vec<u8>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }
    /// Read from a stream until the end
    fn read_all(&self, handle: Handle) -> Result<Vec<u8>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }
    /// Read from a stream until a delimiter is reached
    fn read_until(&self, handle: Handle, delim: &[u8]) -> Result<Vec<u8>, String> {
        let mut buffer = Vec::new();
        loop {
            let bytes = self.read(handle, 1)?;
            if bytes.is_empty() {
                break;
            }
            buffer.extend_from_slice(&bytes);
            if buffer.ends_with(delim) {
                break;
            }
        }
        Ok(buffer)
    }
    /// Write bytes to a stream
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        Err("Writing to streams is not supported in this environment".into())
    }
    /// Create a file
    fn create_file(&self, path: &Path) -> Result<Handle, String> {
        Err("Creating files is not supported in this environment".into())
    }
    /// Open a file
    fn open_file(&self, path: &Path, write: bool) -> Result<Handle, String> {
        Err("Opening files is not supported in this environment".into())
    }
    /// Create a directory
    fn make_dir(&self, path: &Path) -> Result<(), String> {
        Err("Creating directories is not supported in this environment".into())
    }
    /// Read all bytes from a file
    fn file_read_all(&self, path: &Path) -> Result<Vec<u8>, String> {
        let handle = self.open_file(path, false)?;
        let bytes = self.read(handle, usize::MAX)?;
        self.close(handle)?;
        Ok(bytes)
    }
    /// Write all bytes to a file
    fn file_write_all(&self, path: &Path, contents: &[u8]) -> Result<(), String> {
        let handle = self.create_file(path)?;
        self.write(handle, contents)?;
        self.close(handle)?;
        Ok(())
    }
    /// Get the clipboard contents
    fn clipboard(&self) -> Result<String, String> {
        Err("Getting the clipboard is not supported in this environment".into())
    }
    /// Set the clipboard contents
    fn set_clipboard(&self, contents: &str) -> Result<(), String> {
        Err("Setting the clipboard is not supported in this environment".into())
    }
    /// Sleep the current thread for `seconds` seconds
    fn sleep(&self, seconds: f64) -> Result<(), String> {
        Err("Sleeping is not supported in this environment".into())
    }
    /// Show an image
    #[cfg(feature = "image")]
    fn show_image(&self, image: DynamicImage, label: Option<&str>) -> Result<(), String> {
        Err("Showing images not supported in this environment".into())
    }
    /// Show a GIF
    fn show_gif(&self, gif_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        Err("Showing gifs not supported in this environment".into())
    }
    /// Play audio from WAV bytes
    fn play_audio(&self, wave_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        Err("Playing audio not supported in this environment".into())
    }
    /// Get the audio sample rate
    fn audio_sample_rate(&self) -> u32 {
        44100
    }
    /// Stream audio
    fn stream_audio(&self, f: AudioStreamFn) -> Result<(), String> {
        Err("Streaming audio not supported in this environment".into())
    }
    /// The result of the `now` function
    ///
    /// Should be in seconds
    fn now(&self) -> f64 {
        now()
    }
    /// Create a TCP listener and bind it to an address
    fn tcp_listen(&self, addr: &str) -> Result<Handle, String> {
        Err("TCP listeners are not supported in this environment".into())
    }
    /// Create a TLS listener and bind it to an address
    fn tls_listen(&self, addr: &str, cert: &[u8], key: &[u8]) -> Result<Handle, String> {
        Err("TLS listeners are not supported in this environment".into())
    }
    /// Accept a connection with a TCP listener
    fn tcp_accept(&self, handle: Handle) -> Result<Handle, String> {
        Err("TCP listeners are not supported in this environment".into())
    }
    /// Create a TCP socket and connect it to an address
    fn tcp_connect(&self, addr: &str) -> Result<Handle, String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    /// Create a TCP socket with TLS support and connect it to an address
    fn tls_connect(&self, addr: &str) -> Result<Handle, String> {
        Err("TLS sockets are not supported in this environment".into())
    }
    /// Get the connection address of a TCP socket or listener
    fn tcp_addr(&self, handle: Handle) -> Result<SocketAddr, String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    /// Set a TCP socket to non-blocking mode
    fn tcp_set_non_blocking(&self, handle: Handle, non_blocking: bool) -> Result<(), String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    /// Set the read timeout of a TCP socket
    fn tcp_set_read_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    /// Set the write timeout of a TCP socket
    fn tcp_set_write_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    /// Close a stream
    fn close(&self, handle: Handle) -> Result<(), String> {
        Ok(())
    }
    /// Invoke a path with the system's default program
    fn invoke(&self, path: &str) -> Result<(), String> {
        Err("Invoking paths is not supported in this environment".into())
    }
    /// Run a command, inheriting standard IO
    fn run_command_inherit(&self, command: &str, args: &[&str]) -> Result<i32, String> {
        Err("Running inheritting commands is not supported in this environment".into())
    }
    /// Run a command, capturing standard IO
    fn run_command_capture(
        &self,
        command: &str,
        args: &[&str],
    ) -> Result<(i32, String, String), String> {
        Err("Running capturing commands is not supported in this environment".into())
    }
    /// Run a command and return an IO stream handle
    fn run_command_stream(&self, command: &str, args: &[&str]) -> Result<[Handle; 3], String> {
        Err("Running streamed commands is not supported in this environment".into())
    }
    /// Change the current directory
    fn change_directory(&self, path: &str) -> Result<(), String> {
        Err("Changing directories is not supported in this environment".into())
    }
    /// Make an HTTPS request on a TCP socket
    fn https_get(&self, request: &str, handle: Handle) -> Result<String, String> {
        Err("Making HTTPS requests is not supported in this environment".into())
    }
    /// Capture an image from the webcam
    fn webcam_capture(&self, index: usize) -> Result<WebcamImage, String> {
        Err("Capturing from webcam is not supported in this environment".into())
    }
    /// Call a foreign function interface
    fn ffi(
        &self,
        file: &str,
        result_ty: FfiType,
        name: &str,
        arg_tys: &[FfiType],
        args: &[Value],
    ) -> Result<Value, String> {
        Err("FFI is not supported in this environment".into())
    }
    /// Copy the data from a pointer into an array
    fn mem_copy(&self, ty: FfiType, ptr: *const (), len: usize) -> Result<Value, String> {
        Err("Pointer copying is not supported in this environment".into())
    }
    /// Free a pointer
    fn mem_free(&self, ptr: *const ()) -> Result<(), String> {
        Err("Pointer freeing is not supported in this environment".into())
    }
    /// Load a git repo as a module
    ///
    /// The returned path should be loadable via [`SysBackend::file_read_all`]
    fn load_git_module(&self, url: &str, target: GitTarget) -> Result<PathBuf, String> {
        Err("Loading git modules is not supported in this environment".into())
    }
    /// Get the local timezone offset in hours
    fn timezone(&self) -> Result<f64, String> {
        if cfg!(target_arch = "wasm32") {
            return Err("Getting the timezone is not supported in this environment".into());
        }
        let offset = UtcOffset::current_local_offset().map_err(|e| e.to_string())?;
        let (h, m, s) = offset.as_hms();
        let mut o = h as f64;
        o += m as f64 / 60.0;
        o += s as f64 / 3600.0;
        Ok(o)
    }
}

/// A target for a git repository
#[derive(Debug, Clone, Default)]
pub enum GitTarget {
    /// The latest commit on the default branch
    #[default]
    Default,
    /// The latest commit on a specific branch
    Branch(String),
    /// A specific commit
    Commit(String),
}

impl fmt::Debug for dyn SysBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<sys backend>")
    }
}

/// A safe backend with no IO other than captured stdout and stderr
#[derive(Default)]
pub struct SafeSys {
    stdout: Arc<Mutex<Vec<u8>>>,
    stderr: Arc<Mutex<Vec<u8>>>,
}
impl SysBackend for SafeSys {
    fn any(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        self.stdout.lock().extend_from_slice(s.as_bytes());
        Ok(())
    }
    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        self.stderr.lock().extend_from_slice(s.as_bytes());
        Ok(())
    }
}

impl SafeSys {
    /// Create a new safe system backend
    pub fn new() -> Self {
        Self::default()
    }
    /// Take the captured stdout
    pub fn take_stdout(&self) -> Vec<u8> {
        take(&mut *self.stdout.lock())
    }
    /// Take the captured stderr
    pub fn take_stderr(&self) -> Vec<u8> {
        take(&mut *self.stderr.lock())
    }
}

/// Trait for converting to a system backend
pub trait IntoSysBackend {
    /// Convert to a reference counted system backend
    fn into_sys_backend(self) -> Arc<dyn SysBackend>;
}

impl<T> IntoSysBackend for T
where
    T: SysBackend + Send + Sync + 'static,
{
    fn into_sys_backend(self) -> Arc<dyn SysBackend> {
        Arc::new(self)
    }
}

impl IntoSysBackend for Arc<dyn SysBackend> {
    fn into_sys_backend(self) -> Arc<dyn SysBackend> {
        self
    }
}

impl SysOp {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            SysOp::Show => {
                let s = env.pop(1)?.show();
                (env.rt.backend)
                    .print_str_stdout(&s)
                    .map_err(|e| env.error(e))?;
                (env.rt.backend)
                    .print_str_stdout("\n")
                    .map_err(|e| env.error(e))?;
            }
            SysOp::Prin => {
                let val = env.pop(1)?;
                (env.rt.backend)
                    .print_str_stdout(&val.format())
                    .map_err(|e| env.error(e))?;
            }
            SysOp::Print => {
                let val = env.pop(1)?;
                (env.rt.backend)
                    .print_str_stdout(&val.format())
                    .map_err(|e| env.error(e))?;
                (env.rt.backend)
                    .print_str_stdout("\n")
                    .map_err(|e| env.error(e))?;
            }
            SysOp::ScanLine => {
                if let Some(line) = env.rt.backend.scan_line_stdin().map_err(|e| env.error(e))? {
                    env.push(line);
                } else {
                    env.push(0u8);
                }
            }
            SysOp::TermSize => {
                let (width, height) = env.rt.backend.term_size().map_err(|e| env.error(e))?;
                env.push(cowslice![height as f64, width as f64])
            }
            SysOp::Exit => {
                let status = env.pop(1)?.as_int(env, "Status must be an integer")? as i32;
                (env.rt.backend).exit(status).map_err(|e| env.error(e))?;
            }
            SysOp::RawMode => {
                let raw_mode = env.pop(1)?.as_bool(env, "Raw mode must be a boolean")?;
                (env.rt.backend)
                    .set_raw_mode(raw_mode)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::Args => {
                let mut args = Vec::new();
                args.push(env.file_path().to_string_lossy().into_owned());
                args.extend(env.args().to_owned());
                env.push(Array::<Boxed>::from_iter(args));
            }
            SysOp::Var => {
                let key = env
                    .pop(1)?
                    .as_string(env, "Augument to var must be a string")?;
                let var =
                    env.rt.backend.var(&key).ok_or_else(|| {
                        env.error(format!("Environment variable `{key}` is not set"))
                    })?;
                env.push(var);
            }
            SysOp::FOpen => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle = (env.rt.backend)
                    .open_file(path.as_ref(), true)
                    .map_err(|e| env.error(e))?
                    .value(HandleKind::File(path.into()));
                env.push(handle);
            }
            SysOp::FCreate => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle: Value = (env.rt.backend)
                    .create_file(path.as_ref())
                    .map_err(|e| env.error(e))?
                    .value(HandleKind::File(path.into()));
                env.push(handle);
            }
            SysOp::FMakeDir => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                (env.rt.backend)
                    .make_dir(path.as_ref())
                    .map_err(|e| env.error(e))?;
            }
            SysOp::FDelete => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                env.rt.backend.delete(&path).map_err(|e| env.error(e))?;
            }
            SysOp::FTrash => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                env.rt.backend.trash(&path).map_err(|e| env.error(e))?;
            }
            SysOp::ReadStr => {
                let count = env
                    .pop(1)?
                    .as_nat_or_inf(env, "Count must be an integer or infinity")?;
                if let Some(count) = count {
                    validate_size::<char>([count], env)?;
                }
                let handle = env.pop(2)?.as_handle(env, "")?;
                let s = match handle {
                    Handle::STDOUT => return Err(env.error("Cannot read from stdout")),
                    Handle::STDERR => return Err(env.error("Cannot read from stderr")),
                    Handle::STDIN => {
                        if let Some(count) = count {
                            let buf = env.rt.backend.scan_stdin(count).map_err(|e| env.error(e))?;
                            match String::from_utf8(buf) {
                                Ok(s) => s,
                                Err(e) => {
                                    let valid_to = e.utf8_error().valid_up_to();
                                    let mut buf = e.into_bytes();
                                    let mut rest = buf.split_off(valid_to);
                                    for _ in 0..3 {
                                        rest.extend(
                                            env.rt
                                                .backend
                                                .scan_stdin(1)
                                                .map_err(|e| env.error(e))?,
                                        );
                                        if let Ok(s) = std::str::from_utf8(&rest) {
                                            buf.extend_from_slice(s.as_bytes());
                                            break;
                                        }
                                    }
                                    String::from_utf8(buf).map_err(|e| env.error(e))?
                                }
                            }
                        } else {
                            return Err(env.error("Cannot read an infinite amount from stdin"));
                        }
                    }
                    _ => {
                        if let Some(count) = count {
                            let buf = env
                                .rt
                                .backend
                                .read(handle, count)
                                .map_err(|e| env.error(e))?;
                            match String::from_utf8(buf) {
                                Ok(s) => s,
                                Err(e) => {
                                    let valid_to = e.utf8_error().valid_up_to();
                                    let mut buf = e.into_bytes();
                                    let mut rest = buf.split_off(valid_to);
                                    for _ in 0..3 {
                                        rest.extend(
                                            env.rt
                                                .backend
                                                .read(handle, 1)
                                                .map_err(|e| env.error(e))?,
                                        );
                                        if let Ok(s) = std::str::from_utf8(&rest) {
                                            buf.extend_from_slice(s.as_bytes());
                                            break;
                                        }
                                    }
                                    String::from_utf8(buf).map_err(|e| env.error(e))?
                                }
                            }
                        } else {
                            let bytes =
                                env.rt.backend.read_all(handle).map_err(|e| env.error(e))?;
                            String::from_utf8(bytes).map_err(|e| env.error(e))?
                        }
                    }
                };
                env.push(s);
            }
            SysOp::ReadBytes => {
                let count = env
                    .pop(1)?
                    .as_nat_or_inf(env, "Count must be an integer or infinity")?;
                if let Some(count) = count {
                    validate_size::<u8>([count], env)?;
                }
                let handle = env.pop(2)?.as_handle(env, "")?;
                let bytes = match handle {
                    Handle::STDOUT => return Err(env.error("Cannot read from stdout")),
                    Handle::STDERR => return Err(env.error("Cannot read from stderr")),
                    Handle::STDIN => {
                        if let Some(count) = count {
                            env.rt.backend.scan_stdin(count).map_err(|e| env.error(e))?
                        } else {
                            return Err(env.error("Cannot read an infinite amount from stdin"));
                        }
                    }
                    _ => {
                        if let Some(count) = count {
                            env.rt
                                .backend
                                .read(handle, count)
                                .map_err(|e| env.error(e))?
                        } else {
                            env.rt.backend.read_all(handle).map_err(|e| env.error(e))?
                        }
                    }
                };
                env.push(Array::from(bytes.as_slice()));
            }
            SysOp::ReadUntil => {
                let delim = env.pop(1)?;
                let handle = env.pop(2)?.as_handle(env, "")?;
                if delim.rank() > 1 {
                    return Err(env.error("Delimiter must be a rank 0 or 1 string or byte array"));
                }
                match handle {
                    Handle::STDOUT => return Err(env.error("Cannot read from stdout")),
                    Handle::STDERR => return Err(env.error("Cannot read from stderr")),
                    Handle::STDIN => {
                        let mut is_string = false;
                        let delim_bytes: Vec<u8> = match delim {
                            Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                            Value::Byte(arr) => arr.data.into(),
                            Value::Char(arr) => {
                                is_string = true;
                                arr.data.iter().collect::<String>().into()
                            }
                            _ => return Err(env.error("Delimiter must be a string or byte array")),
                        };
                        let buffer = env
                            .rt
                            .backend
                            .scan_until_stdin(&delim_bytes)
                            .map_err(|e| env.error(e))?;
                        if is_string {
                            let s = String::from_utf8_lossy(&buffer).into_owned();
                            env.push(s);
                        } else {
                            env.push(Array::from(buffer.as_slice()));
                        }
                    }
                    _ => match delim {
                        Value::Num(arr) => {
                            let delim: Vec<u8> = arr.data.iter().map(|&x| x as u8).collect();
                            let bytes = env
                                .rt
                                .backend
                                .read_until(handle, &delim)
                                .map_err(|e| env.error(e))?;
                            env.push(Array::from(bytes.as_slice()));
                        }
                        Value::Byte(arr) => {
                            let delim: Vec<u8> = arr.data.into();
                            let bytes = env
                                .rt
                                .backend
                                .read_until(handle, &delim)
                                .map_err(|e| env.error(e))?;
                            env.push(Array::from(bytes.as_slice()));
                        }
                        Value::Char(arr) => {
                            let delim: Vec<u8> = arr.data.iter().collect::<String>().into();
                            let bytes = env
                                .rt
                                .backend
                                .read_until(handle, &delim)
                                .map_err(|e| env.error(e))?;
                            let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                            env.push(s);
                        }
                        _ => return Err(env.error("Delimiter must be a string or byte array")),
                    },
                }
            }
            SysOp::Write => {
                let data = env.pop(1)?;
                let handle = env.pop(2)?.as_handle(env, "")?;
                let bytes: Vec<u8> = match data {
                    Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                    Value::Byte(arr) => arr.data.into(),
                    Value::Complex(_) => return Err(env.error("Cannot write complex array")),
                    Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                    Value::Box(_) => return Err(env.error("Cannot write box array")),
                };
                match handle {
                    Handle::STDOUT => env
                        .rt
                        .backend
                        .print_str_stdout(&String::from_utf8_lossy(&bytes))
                        .map_err(|e| env.error(e))?,
                    Handle::STDERR => env
                        .rt
                        .backend
                        .print_str_stderr(&String::from_utf8_lossy(&bytes))
                        .map_err(|e| env.error(e))?,
                    Handle::STDIN => return Err(env.error("Cannot write to stdin")),
                    _ => env
                        .rt
                        .backend
                        .write(handle, &bytes)
                        .map_err(|e| env.error(e))?,
                }
            }
            SysOp::FReadAllStr => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = (env.rt.backend)
                    .file_read_all(path.as_ref())
                    .or_else(|e| match path.as_str() {
                        "example.ua" => Ok(EXAMPLE_UA.as_bytes().to_vec()),
                        "example.txt" => Ok(EXAMPLE_TXT.as_bytes().to_vec()),
                        _ => Err(e),
                    })
                    .map_err(|e| env.error(e))?;
                let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                env.push(s);
            }
            SysOp::FReadAllBytes => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = (env.rt.backend)
                    .file_read_all(path.as_ref())
                    .or_else(|e| match path.as_str() {
                        "example.ua" => Ok(EXAMPLE_UA.as_bytes().to_vec()),
                        "example.txt" => Ok(EXAMPLE_TXT.as_bytes().to_vec()),
                        _ => Err(e),
                    })
                    .map_err(|e| env.error(e))?;
                let bytes = bytes.into_iter().map(Into::into);
                env.push(Array::<u8>::from_iter(bytes));
            }
            SysOp::FWriteAll => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let data = env.pop(2)?;
                let bytes: Vec<u8> = match data {
                    Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                    Value::Byte(arr) => arr.data.into(),

                    Value::Complex(_) => {
                        return Err(env.error("Cannot write complex array to file"))
                    }
                    Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                    Value::Box(_) => return Err(env.error("Cannot write box array to file")),
                };
                (env.rt.backend)
                    .file_write_all(path.as_ref(), &bytes)
                    .or_else(|e| {
                        if path == "example.ua" {
                            let new_ex = String::from_utf8(bytes).map_err(|e| e.to_string())?;
                            example_ua(move |ex| *ex = new_ex);
                            Ok(())
                        } else {
                            Err(e)
                        }
                    })
                    .map_err(|e| env.error(e))?;
            }
            SysOp::FExists => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let exists = env.rt.backend.file_exists(&path);
                env.push(exists);
            }
            SysOp::FListDir => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let paths = env.rt.backend.list_dir(&path).map_err(|e| env.error(e))?;
                env.push(Array::<Boxed>::from_iter(paths));
            }
            SysOp::FIsFile => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let is_file = env.rt.backend.is_file(&path).map_err(|e| env.error(e))?;
                env.push(is_file);
            }
            SysOp::Invoke => {
                let path = env.pop(1)?.as_string(env, "Invoke path must be a string")?;
                env.rt.backend.invoke(&path).map_err(|e| env.error(e))?;
            }
            SysOp::ImShow => {
                #[cfg(feature = "image")]
                {
                    let value = env.pop(1)?;
                    let image = crate::encode::value_to_image(&value).map_err(|e| env.error(e))?;
                    (env.rt.backend)
                        .show_image(image, value.meta().label.as_deref())
                        .map_err(|e| env.error(e))?;
                }
                #[cfg(not(feature = "image"))]
                return Err(env.error("Image encoding is not supported in this environment"));
            }
            SysOp::GifShow => {
                #[cfg(feature = "gif")]
                {
                    let delay = env.pop(1)?.as_num(env, "Delay must be a number")?;
                    let value = env.pop(2)?;
                    let bytes = crate::encode::value_to_gif_bytes(&value, delay)
                        .map_err(|e| env.error(e))?;
                    (env.rt.backend)
                        .show_gif(bytes, value.meta().label.as_deref())
                        .map_err(|e| env.error(e))?;
                }
                #[cfg(not(feature = "gif"))]
                return Err(env.error("GIF showing is not supported in this environment"));
            }
            SysOp::AudioPlay => {
                #[cfg(feature = "audio_encode")]
                {
                    let value = env.pop(1)?;
                    let bytes = crate::encode::value_to_wav_bytes(
                        &value,
                        env.rt.backend.audio_sample_rate(),
                    )
                    .map_err(|e| env.error(e))?;
                    (env.rt.backend)
                        .play_audio(bytes, value.meta().label.as_deref())
                        .map_err(|e| env.error(e))?;
                }
                #[cfg(not(feature = "audio_encode"))]
                return Err(env.error("Audio encoding is not supported in this environment"));
            }
            SysOp::AudioSampleRate => {
                let sample_rate = env.rt.backend.audio_sample_rate();
                env.push(f64::from(sample_rate));
            }
            SysOp::AudioStream => {
                let f = env.pop_function()?;
                if f.signature() != (1, 1) {
                    return Err(env.error(format!(
                        "&ast's function's signature must be {}, but it is {}",
                        Signature::new(1, 1),
                        f.signature()
                    )));
                }
                let mut stream_env = env.clone();
                if let Err(e) = env.rt.backend.stream_audio(Box::new(move |time_array| {
                    let time_array = Array::<f64>::from(time_array);
                    stream_env.push(time_array);
                    stream_env.call(f.clone())?;
                    let samples = &stream_env.pop(1)?;
                    let samples = samples.as_num_array().ok_or_else(|| {
                        stream_env.error("Audio stream function must return a numeric array")
                    })?;
                    match samples.shape().dims() {
                        [_] => Ok(samples.data.iter().map(|&x| [x, x]).collect()),
                        &[2, n] => {
                            let mut samps: Vec<[f64; 2]> = Vec::with_capacity(n);
                            for i in 0..n {
                                samps.push([samples.data[i], samples.data[i + n]]);
                            }
                            Ok(samps)
                        }
                        _ => Err(stream_env.error(format!(
                            "Audio stream function must return either a \
                            rank 1 array or a rank 2 array with 2 rows, \
                            but its shape is {}",
                            samples.shape()
                        ))),
                    }
                })) {
                    return Err(env.error(e));
                }
            }
            SysOp::ClipboardGet => {
                let contents = env.rt.backend.clipboard().map_err(|e| env.error(e))?;
                env.push(contents);
            }
            SysOp::ClipboardSet => {
                let contents = env.pop(1)?.as_string(env, "Contents must be a string")?;
                (env.rt.backend)
                    .set_clipboard(&contents)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::Sleep => {
                let mut seconds = env.pop(1)?.as_num(env, "Sleep time must be a number")?;
                if seconds < 0.0 {
                    return Err(env.error("Sleep time must be positive"));
                }
                if seconds.is_infinite() {
                    return Err(env.error("Sleep time cannot be infinite"));
                }
                if let Some(limit) = env.rt.execution_limit {
                    let elapsed = env.rt.backend.now() - env.rt.execution_start;
                    let max = limit - elapsed;
                    seconds = seconds.min(max);
                }
                env.rt.backend.sleep(seconds).map_err(|e| env.error(e))?;
            }
            SysOp::TcpListen => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let handle = (env.rt.backend)
                    .tcp_listen(&addr)
                    .map_err(|e| env.error(e))?;
                let sock_addr = env.rt.backend.tcp_addr(handle).map_err(|e| env.error(e))?;
                let handle = handle.value(HandleKind::TcpListener(sock_addr));
                env.push(handle);
            }
            SysOp::TlsListen => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let cert = env
                    .pop(2)?
                    .into_bytes(env, "Cert must be a byte or character array")?;
                let key = env
                    .pop(3)?
                    .into_bytes(env, "Key must be a byte or character array")?;
                let handle = (env.rt.backend)
                    .tls_listen(&addr, &cert, &key)
                    .map_err(|e| env.error(e))?;
                let sock_addr = env.rt.backend.tcp_addr(handle).map_err(|e| env.error(e))?;
                let handle = handle.value(HandleKind::TlsListener(sock_addr));
                env.push(handle);
            }
            SysOp::TcpAccept => {
                let handle = env.pop(1)?.as_handle(env, "")?;
                let handle = (env.rt.backend)
                    .tcp_accept(handle)
                    .map_err(|e| env.error(e))?;
                let addr = (env.rt.backend)
                    .tcp_addr(handle)
                    .map_err(|e| env.error(e))?;
                let handle = handle.value(HandleKind::TcpSocket(addr));
                env.push(handle);
            }
            SysOp::TcpConnect => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let handle = (env.rt.backend)
                    .tcp_connect(&addr)
                    .map_err(|e| env.error(e))?;
                let sock_addr = env.rt.backend.tcp_addr(handle).map_err(|e| env.error(e))?;
                let handle = handle.value(HandleKind::TcpSocket(sock_addr));
                env.push(handle);
            }
            SysOp::TlsConnect => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let handle = (env.rt.backend)
                    .tls_connect(&addr)
                    .map_err(|e| env.error(e))?;
                let sock_addr = env.rt.backend.tcp_addr(handle).map_err(|e| env.error(e))?;
                let handle = handle.value(HandleKind::TlsSocket(sock_addr));
                env.push(handle);
            }
            SysOp::TcpAddr => {
                let handle = env.pop(1)?.as_handle(env, "")?;
                let addr = env.rt.backend.tcp_addr(handle).map_err(|e| env.error(e))?;
                env.push(addr.to_string());
            }
            SysOp::TcpSetNonBlocking => {
                let handle = env.pop(1)?.as_handle(env, "")?;
                (env.rt.backend)
                    .tcp_set_non_blocking(handle, true)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::TcpSetReadTimeout => {
                let timeout = env.pop(1)?.as_num(env, "Timeout must be a number")?.abs();
                let timeout = if timeout.is_infinite() {
                    None
                } else {
                    Some(Duration::from_secs_f64(timeout))
                };
                let handle = env.pop(2)?.as_handle(env, "")?;
                (env.rt.backend)
                    .tcp_set_read_timeout(handle, timeout)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::TcpSetWriteTimeout => {
                let timeout = env.pop(1)?.as_num(env, "Timeout must be a number")?.abs();
                let timeout = if timeout.is_infinite() {
                    None
                } else {
                    Some(Duration::from_secs_f64(timeout))
                };
                let handle = env.pop(2)?.as_handle(env, "")?;
                (env.rt.backend)
                    .tcp_set_write_timeout(handle, timeout)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::HttpsWrite => {
                let http = env
                    .pop(1)?
                    .as_string(env, "HTTP request must be a string")?;
                let handle = env.pop(2)?.as_handle(env, "")?;
                let res = (env.rt.backend)
                    .https_get(&http, handle)
                    .map_err(|e| env.error(e))?;
                env.push(res);
            }
            SysOp::Close => {
                let handle = env.pop(1)?.as_handle(env, "")?;
                env.rt.backend.close(handle).map_err(|e| env.error(e))?;
            }
            SysOp::RunInherit => {
                let (command, args) = value_to_command(&env.pop(1)?, env)?;
                let args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
                let code = (env.rt.backend)
                    .run_command_inherit(&command, &args)
                    .map_err(|e| env.error(e))?;
                env.push(code);
            }
            SysOp::RunCapture => {
                let (command, args) = value_to_command(&env.pop(1)?, env)?;
                let args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
                let (code, stdout, stderr) = (env.rt.backend)
                    .run_command_capture(&command, &args)
                    .map_err(|e| env.error(e))?;
                env.push(stderr);
                env.push(stdout);
                env.push(code);
            }
            SysOp::RunStream => {
                let (command, args) = value_to_command(&env.pop(1)?, env)?;
                let args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
                let handles = (env.rt.backend)
                    .run_command_stream(&command, &args)
                    .map_err(|e| env.error(e))?;
                for (handle, kind) in handles
                    .into_iter()
                    .zip([
                        HandleKind::ChildStdin,
                        HandleKind::ChildStdout,
                        HandleKind::ChildStderr,
                    ])
                    .rev()
                {
                    env.push(handle.value(kind(command.clone())));
                }
            }
            SysOp::ChangeDirectory => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                (env.rt.backend)
                    .change_directory(&path)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::WebcamCapture => {
                let index = env.pop(1)?.as_nat(env, "Webcam index must be an integer")?;
                let _image = (env.rt.backend)
                    .webcam_capture(index)
                    .map_err(|e| env.error(e))?;
                #[cfg(feature = "image")]
                env.push(crate::encode::rgb_image_to_array(_image));
                #[cfg(not(feature = "image"))]
                return Err(env.error("Webcam capture is not supported in this environment"));
            }
            SysOp::Ffi => {
                let sig_def = env.pop(1)?;
                let sig_def = match sig_def {
                    Value::Box(arr) => arr,
                    val => {
                        return Err(env.error(format!(
                            "FFI signature must be a box array, but it is {}",
                            val.type_name_plural()
                        )))
                    }
                };
                if sig_def.rank() != 1 {
                    return Err(env.error(format!(
                        "FFI signature must be a rank 1 array, but it is rank {}",
                        sig_def.rank()
                    )));
                }
                if sig_def.row_count() < 3 {
                    return Err(env.error("FFI signature array must have at least two elements"));
                }
                let mut sig_frags = sig_def.data.into_iter().map(|b| b.0);
                let file_name =
                    (sig_frags.next().unwrap()).as_string(env, "FFI file name must be a string")?;
                let result_ty = (sig_frags.next().unwrap())
                    .as_string(env, "FFI result type must be a string")?
                    .parse::<FfiType>()
                    .map_err(|e| env.error(e))?;
                let name =
                    (sig_frags.next().unwrap()).as_string(env, "FFI name must be a string")?;
                let arg_tys = sig_frags
                    .map(|frag| {
                        frag.as_string(env, "FFI argument type must be a string")
                            .and_then(|ty| ty.parse::<FfiType>().map_err(|e| env.error(e)))
                    })
                    .collect::<UiuaResult<Vec<_>>>()?;
                let args = env.pop(2)?;
                let args: Vec<Value> = args.into_rows().map(Value::unpacked).collect();
                let result = (env.rt.backend)
                    .ffi(&file_name, result_ty, &name, &arg_tys, &args)
                    .map_err(|e| env.error(e))?;
                env.push(result);
            }
            SysOp::MemCopy => {
                let ty = env
                    .pop(1)?
                    .as_string(env, "Pointer copy type must be a string")?;
                let ty = ty.parse::<FfiType>().map_err(|e| env.error(e))?;
                let ptr = env
                    .pop(2)?
                    .meta()
                    .pointer
                    .map(|p| p.get())
                    .ok_or_else(|| env.error("Copied pointer must be a pointer value"))?;
                let len = env
                    .pop(3)?
                    .as_nat(env, "Copied length must be a non-negative integer")?;
                let value = (env.rt.backend)
                    .mem_copy(ty, ptr, len)
                    .map_err(|e| env.error(e))?;
                env.push(value);
            }
            SysOp::MemFree => {
                let ptr = env
                    .pop(1)?
                    .meta()
                    .pointer
                    .map(|p| p.get())
                    .ok_or_else(|| env.error("Freed pointer must be a pointer value"))?;
                (env.rt.backend).mem_free(ptr).map_err(|e| env.error(e))?;
            }
        }
        Ok(())
    }
}

fn value_to_command(value: &Value, env: &Uiua) -> UiuaResult<(String, Vec<String>)> {
    let mut strings = Vec::new();
    match value {
        Value::Char(arr) => match arr.rank() {
            0 | 1 => strings.push(arr.data.iter().collect::<String>()),
            2 => {
                for row in arr.rows() {
                    strings.push(row.data.iter().collect::<String>());
                }
            }
            n => {
                return Err(env.error(format!(
                    "Character array as command must be rank 0, 1, \
                    or 2, but its rank is {n}"
                )))
            }
        },
        Value::Box(arr) => match arr.rank() {
            0 | 1 => {
                for bx in &arr.data {
                    match bx.as_value() {
                        Value::Char(arr) if arr.rank() <= 1 => {
                            strings.push(arr.data.iter().collect::<String>())
                        }
                        val => {
                            return Err(env.error(format!(
                                "Function array as command must be all boxed strings, \
                                but at least one is a {}",
                                val.type_name()
                            )))
                        }
                    }
                }
            }
            n => {
                return Err(env.error(format!(
                    "Function array as command must be rank 0 or 1, \
                    but its rank is {n}"
                )))
            }
        },
        Value::Num(_) => {
            return Err(env.error(format!(
                "Command must be a string or box array, but it is {}",
                value.type_name_plural()
            )))
        }
        Value::Byte(_) => {
            return Err(env.error(format!(
                "Command must be a string or box array, but it is {}",
                value.type_name_plural()
            )))
        }

        Value::Complex(_) => {
            return Err(env.error(format!(
                "Command must be a string or box array, but it is {}",
                value.type_name_plural()
            )))
        }
    }
    if strings.is_empty() {
        return Err(env.error("Command array not be empty"));
    }
    let command = strings.remove(0);
    Ok((command, strings))
}

/// Get the current time in seconds
///
/// This function works on both native and web targets.
pub fn now() -> f64 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .expect("System clock was before 1970.")
            .as_secs_f64()
    }
    #[cfg(target_arch = "wasm32")]
    {
        #[cfg(not(feature = "web"))]
        {
            compile_error!("Web target requires the `web` feature")
        }
        #[cfg(feature = "web")]
        {
            use wasm_bindgen::{prelude::*, JsCast};
            js_sys::Reflect::get(&js_sys::global(), &JsValue::from_str("performance"))
                .expect("failed to get performance from global object")
                .unchecked_into::<web_sys::Performance>()
                .now()
                / 1000.0
        }
    }
}

pub(crate) fn terminal_size() -> Option<(usize, usize)> {
    #[cfg(all(not(target_arch = "wasm32"), feature = "terminal_size"))]
    {
        terminal_size::terminal_size().map(|(w, h)| (w.0 as usize, h.0 as usize))
    }

    #[cfg(not(all(not(target_arch = "wasm32"), feature = "terminal_size")))]
    None
}
