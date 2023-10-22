use std::{
    any::Any,
    collections::{HashMap, HashSet},
    io::{stderr, stdin, Cursor, Read, Write},
    sync::OnceLock,
    time::Duration,
};

use ecow::EcoVec;
use enum_iterator::Sequence;
use hound::{SampleFormat, WavReader, WavSpec, WavWriter};
use image::{DynamicImage, ImageOutputFormat};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use tinyvec::tiny_vec;

use crate::{
    array::Array,
    boxed::Boxed,
    cowslice::{cowslice, CowSlice},
    primitive::PrimDoc,
    value::Value,
    Uiua, UiuaError, UiuaResult,
};

pub fn example_ua<T>(f: impl FnOnce(&mut String) -> T) -> T {
    static EXAMPLE_UA: Lazy<Mutex<String>> = Lazy::new(|| {
        Mutex::new(
            "\
Square ← ×.
Double ← +.
Increment ← +1"
                .into(),
        )
    });
    f(&mut EXAMPLE_UA.lock())
}

macro_rules! sys_op {
    ($(
        $(#[doc = $doc:literal])*
        (
            $args:literal$(($outputs:expr))?$([$mod_args:expr])?,
            $variant:ident, $name:literal, $long_name:literal
        )
    ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
        pub enum SysOp {
            $($variant),*
        }

        impl SysOp {
            pub const ALL: [Self; 0 $(+ {stringify!($variant); 1})*] = [
                $(Self::$variant,)*
            ];
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $name),*
                }
            }
            pub fn long_name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $long_name),*
                }
            }
            pub fn args(&self) -> u8 {
                match self {
                    $(SysOp::$variant => $args,)*
                }
            }
            pub fn modifier_args(&self) -> Option<u8> {
                match self {
                    $($(
                        SysOp::$variant => Some($mod_args),
                    )?)*
                    _ => None
                }
            }
            #[allow(unreachable_patterns)]
            pub fn outputs(&self) -> u8 {
                match self {
                    $($(SysOp::$variant => $outputs.into(),)?)*
                    _ => 1
                }
            }
            pub fn doc(&self) -> Option<&'static PrimDoc> {
                match self {
                    $(SysOp::$variant => {
                        let doc_str = concat!($($doc, "\n"),*);
                        static DOC: OnceLock<PrimDoc> = OnceLock::new();
                        if doc_str.is_empty() {
                            return None;
                        }
                        Some(DOC.get_or_init(|| PrimDoc::from_lines(doc_str)))
                    },)*
                }
            }
        }
    };
}

sys_op! {
    /// Print a nicely formatted representation of a value to stdout
    (1(0), Show, "&s", "show"),
    /// Print a value to stdout
    (1(0), Prin, "&pf", "print and flush"),
    /// Print a value to stdout followed by a newline
    (1(0), Print, "&p", "print with newline"),
    /// Read a line from stdin
    ///
    /// The normal output is a string.
    /// If EOF is reached, the number `0` is returned instead.
    /// Programs that wish to properly handle EOF should check for this.
    (0, ScanLine, "&sc", "scan line"),
    /// Get the size of the terminal
    ///
    /// The result is a 2-element array of the height and width of the terminal.
    /// Height comes first so that the array can be used as a shape in [reshape].
    (0, TermSize, "&ts", "terminal size"),
    /// Get the command line arguments
    ///
    /// The first element will always be the name of your script
    (0, Args, "&args", "arguments"),
    /// Get the value of an environment variable
    (1, Var, "&var", "environment variable"),
    /// Run a command and wait for it to finish
    ///
    /// Standard IO will be inherited. Returns the exit code of the command.
    ///
    /// Expects either a string, a rank `2` character array, or a rank `1` array of [box] strings.
    (1(1), RunInherit, "&runi", "run command inherit"),
    /// Run a command and wait for it to finish
    ///
    /// Standard IO will be captured. The exit code, stdout, and stderr will each be pushed to the stack.
    ///
    /// Expects either a string, a rank `2` character array, or a rank `1` array of [box] strings.
    (1(3), RunCapture, "&runc", "run command capture"),
    /// Change the current directory
    (1(0), ChangeDirectory, "&cd", "change directory"),
    /// Sleep for n seconds
    ///
    /// On the web, this example will hang for 1 second.
    /// ex: ⚂ &sl 1
    (1(0), Sleep, "&sl", "sleep"),
    /// Read at most n bytes from a stream
    (2, ReadStr, "&rs", "read to string"),
    /// Read at most n bytes from a stream
    (2, ReadBytes, "&rb", "read to bytes"),
    /// Read from a stream until a delimiter is reached
    (2, ReadUntil, "&ru", "read until"),
    /// Write an array to a stream
    (2(0), Write, "&w", "write"),
    /// Import an item from a file
    ///
    /// The first argument is the path to the file. The second is the name of the item to import.
    /// ex: Dub ← &i "example.ua" "Double"
    ///   : Dub 5
    /// To import multiple items, you can make a function that imports from a specific path.
    /// ex: ex ← &i "example.ua"
    ///   : Double ← ex "Double"
    ///   : Square ← ex "Square"
    ///   : Square Double 5
    (2, Import, "&i", "import"),
    /// Invoke a path with the system's default program
    (1(1), Invoke, "&invk", "invoke"),
    /// Close a stream by its handle
    ///
    /// This will close files, tcp listeners, and tcp sockets.
    (1(0), Close, "&cl", "close handle"),
    /// Open a file and return a handle to it
    ///
    /// The file can be read from with [&rs], [&rb], or [&ru].
    /// The file can be written to with [&w].
    (1, FOpen, "&fo", "file - open"),
    /// Create a file and return a handle to it
    ///
    /// The file can be read from with [&rs], [&rb], or [&ru].
    /// The file can be written to with [&w].
    (1, FCreate, "&fc", "file - create"),
    /// Check if a file exists at a path
    (1, FExists, "&fe", "file - exists"),
    /// List the contents of a directory
    (1, FListDir, "&fld", "file - list directory"),
    /// Check if a path is a file
    (1, FIsFile, "&fif", "file - is file"),
    /// Read all the contents of a file into a string
    ///
    /// Expects a path and returns a [rank]`1` character array.
    (1, FReadAllStr, "&fras", "file - read all to string"),
    /// Read all the contents of a file into a byte array
    ///
    /// Expects a path and returns a [rank]`1` numeric array.
    (1, FReadAllBytes, "&frab", "file - read all to bytes"),
    /// Write the entire contents of an array to a file
    ///
    /// Expects a path and a [rank]`1` array or either numbers or characters.
    (2(0), FWriteAll, "&fwa", "file - write all"),
    /// Decode an image from a byte array
    ///
    /// Supported formats are `jpg`, `png`, `bmp`, `gif`, and `ico`.
    ///
    /// See also: [&ime]
    (1, ImDecode, "&imd", "image - decode"),
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
    /// Supported formats are `jpg`, `png`, `bmp`, `gif`, and `ico`.
    ///
    /// See also: [&ims] [&imd]
    (2, ImEncode, "&ime", "image - encode"),
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
    /// See also: [&ime]
    (1(0), ImShow, "&ims", "image - show"),
    /// Encode a gif into a byte array
    ///
    /// The first argument is a framerate in seconds.
    /// The second argument is the gif data and must be a rank 3 or 4 numeric array.
    /// The rows of the array are the frames of the gif, and their format must conform to that of [&ime].
    ///
    /// See also: [&gifs]
    (2, GifEncode, "&gife", "gif - encode"),
    /// Show a gif
    ///
    /// The first argument is a framerate in seconds.
    /// The second argument is the gif data and must be a rank 3 or 4 numeric array.
    /// The rows of the array are the frames of the gif, and their format must conform to that of [&ime].
    ///
    /// See also: [&gife]
    (2(0), GifShow, "&gifs", "gif - show"),
    /// Decode audio from a byte array
    ///
    /// Only the `wav` format is supported.
    ///
    /// See also: [&ae]
    (1, AudioDecode, "&ad", "audio - decode"),
    /// Encode audio into a byte array
    ///
    /// The first argument is the format, and the second is the audio samples.
    ///
    /// The audio samples must be a rank 1 or 2 numeric array.
    ///
    /// A rank 1 array is a list of mono audio samples.
    /// For a rank 2 array, each row is a channel.
    ///
    /// The samples must be between -1 and 1.
    /// The sample rate is [&asr].
    ///
    /// Only the `wav` format is supported.
    ///
    /// See also: [&ap] [&ad]
    (2, AudioEncode, "&ae", "audio - encode"),
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
    /// See also: [&ae]
    (1(0), AudioPlay, "&ap", "audio - play"),
    /// Get the sample rate of the audio output backend
    ///
    /// ex: &asr
    /// Here is how you can generate a list of sample times for `4` seconds of audio:
    /// ex: ÷∶⇡×, 4 &asr
    /// Pass that to a periodic function, and you get a nice tone!
    /// ex: ÷4○×τ×220 ÷∶⇡×, 4 &asr
    (0, AudioSampleRate, "&asr", "audio - sample rate"),
    /// Synthesize and stream audio
    ///
    /// Expects a function that takes a list of sample times and returns a list of samples.
    /// The function will be called repeatedly to generate the audio.
    (0(0)[1], AudioStream, "&ast", "audio - stream"),
    /// Create a TCP listener and bind it to an address
    (1, TcpListen, "&tcpl", "tcp - listen"),
    /// Accept a connection with a TCP listener
    (1, TcpAccept, "&tcpa", "tcp - accept"),
    /// Create a TCP socket and connect it to an address
    (1, TcpConnect, "&tcpc", "tcp - connect"),
    /// Set a TCP socket to non-blocking mode
    (1, TcpSetNonBlocking, "&tcpsnb", "tcp - set non-blocking"),
    /// Set the read timeout of a TCP socket in seconds
    (2(0), TcpSetReadTimeout, "&tcpsrt", "tcp - set read timeout"),
    /// Set the write timeout of a TCP socket in seconds
    (2(0), TcpSetWriteTimeout, "&tcpswt", "tcp - set write timeout"),
    /// Get the connection address of a TCP socket
    (1, TcpAddr, "&tcpaddr", "tcp - address"),
    /// Make an HTTP request
    ///
    /// Takes in an 1.x HTTP request and returns an HTTP response.
    ///
    /// Requires the `Host` header to be set.
    /// Using port 443 is recommended for HTTPS.
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
    (2, HttpsWrite, "&httpsw", "http - Make an HTTP request"),
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
    pub const FIRST_UNRESERVED: Self = Self(3);
}

impl From<usize> for Handle {
    fn from(n: usize) -> Self {
        Self(n as u64)
    }
}

impl From<Handle> for Value {
    fn from(handle: Handle) -> Self {
        (handle.0 as f64).into()
    }
}

pub type AudioStreamFn = Box<dyn FnMut(Vec<f64>) -> UiuaResult<Vec<[f64; 2]>> + Send>;

#[allow(unused_variables)]
pub trait SysBackend: Any + Send + Sync + 'static {
    fn any(&self) -> &dyn Any;
    /// Save a color-formatted version of an error message for later printing
    fn save_error_color(&self, error: &UiuaError) {}
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        Err("Printing to stdout is not supported in this environment".into())
    }
    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        Err("Printing to stderr is not supported in this environment".into())
    }
    fn print_str_trace(&self, s: &str) {
        eprint!("{s}");
        _ = stderr().flush();
    }
    /// Read a line from stdin
    ///
    /// Should return `Ok(None)` if EOF is reached.
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        Err("Reading from stdin is not supported in this environment".into())
    }
    fn var(&self, name: &str) -> Option<String> {
        None
    }
    fn term_size(&self) -> Result<(usize, usize), String> {
        Err("Getting the terminal size is not supported in this environment".into())
    }
    fn file_exists(&self, path: &str) -> bool {
        false
    }
    fn list_dir(&self, path: &str) -> Result<Vec<String>, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn read(&self, handle: Handle, count: usize) -> Result<Vec<u8>, String> {
        Err("This IO operation is not supported in this environment".into())
    }
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
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn create_file(&self, path: &str) -> Result<Handle, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn open_file(&self, path: &str) -> Result<Handle, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn file_read_all(&self, path: &str) -> Result<Vec<u8>, String> {
        let handle = self.open_file(path)?;
        let bytes = self.read(handle, usize::MAX)?;
        self.close(handle)?;
        Ok(bytes)
    }
    fn file_write_all(&self, path: &str, contents: &[u8]) -> Result<(), String> {
        let handle = self.create_file(path)?;
        self.write(handle, contents)?;
        self.close(handle)?;
        Ok(())
    }
    fn sleep(&self, seconds: f64) -> Result<(), String> {
        Err("Sleeping is not supported in this environment".into())
    }
    fn show_image(&self, image: DynamicImage) -> Result<(), String> {
        Err("Showing images not supported in this environment".into())
    }
    fn show_gif(&self, gif_bytes: Vec<u8>) -> Result<(), String> {
        Err("Showing gifs not supported in this environment".into())
    }
    fn play_audio(&self, wave_bytes: Vec<u8>) -> Result<(), String> {
        Err("Playing audio not supported in this environment".into())
    }
    fn audio_sample_rate(&self) -> u32 {
        44100
    }
    fn stream_audio(&self, f: AudioStreamFn) -> Result<(), String> {
        Err("Streaming audio not supported in this environment".into())
    }
    fn tcp_listen(&self, addr: &str) -> Result<Handle, String> {
        Err("TCP listeners are not supported in this environment".into())
    }
    fn tcp_accept(&self, handle: Handle) -> Result<Handle, String> {
        Err("TCP listeners are not supported in this environment".into())
    }
    fn tcp_connect(&self, addr: &str) -> Result<Handle, String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    fn tcp_addr(&self, handle: Handle) -> Result<String, String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    fn tcp_set_non_blocking(&self, handle: Handle, non_blocking: bool) -> Result<(), String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    fn tcp_set_read_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    fn tcp_set_write_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        Err("TCP sockets are not supported in this environment".into())
    }
    fn close(&self, handle: Handle) -> Result<(), String> {
        Ok(())
    }
    fn invoke(&self, path: &str) -> Result<(), String> {
        Err("Invoking paths is not supported in this environment".into())
    }
    fn spawn(
        &self,
        env: Uiua,
        f: Box<dyn FnOnce(&mut Uiua) -> UiuaResult + Send>,
    ) -> Result<Handle, String> {
        Err("Spawning threads is not supported in this environment".into())
    }
    fn wait(&self, handle: Handle) -> Result<Vec<Value>, Result<UiuaError, String>> {
        Err(Err(
            "Joining threads is not supported in this environment".into()
        ))
    }
    fn run_command_inherit(&self, command: &str, args: &[&str]) -> Result<i32, String> {
        Err("Running commands is not supported in this environment".into())
    }
    fn run_command_capture(
        &self,
        command: &str,
        args: &[&str],
    ) -> Result<(i32, String, String), String> {
        Err("Running commands is not supported in this environment".into())
    }
    fn change_directory(&self, path: &str) -> Result<(), String> {
        Err("Changing directories is not supported in this environment".into())
    }
    fn https_get(&self, request: &str, handle: Handle) -> Result<String, String> {
        Err("Making HTTPS requests is not supported in this environment".into())
    }
}

impl SysOp {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            SysOp::Show => {
                let s = env.pop(1)?.show();
                env.backend.print_str_stdout(&s).map_err(|e| env.error(e))?;
                env.backend
                    .print_str_stdout("\n")
                    .map_err(|e| env.error(e))?;
            }
            SysOp::Prin => {
                let val = env.pop(1)?;
                env.backend
                    .print_str_stdout(&val.to_string())
                    .map_err(|e| env.error(e))?;
            }
            SysOp::Print => {
                let val = env.pop(1)?;
                env.backend
                    .print_str_stdout(&val.to_string())
                    .map_err(|e| env.error(e))?;
                env.backend
                    .print_str_stdout("\n")
                    .map_err(|e| env.error(e))?;
            }
            SysOp::ScanLine => {
                if let Some(line) = env.backend.scan_line_stdin().map_err(|e| env.error(e))? {
                    env.push(line);
                } else {
                    env.push(0u8);
                }
            }
            SysOp::TermSize => {
                let (width, height) = env.backend.term_size().map_err(|e| env.error(e))?;
                env.push(cowslice![height as f64, width as f64])
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
                let var = env.backend.var(&key).unwrap_or_default();
                env.push(var);
            }
            SysOp::FOpen => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle = env.backend.open_file(&path).map_err(|e| env.error(e))?;
                env.push(handle);
            }
            SysOp::FCreate => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle = env.backend.create_file(&path).map_err(|e| env.error(e))?;
                env.push(handle.0 as f64);
            }
            SysOp::ReadStr => {
                let count = env.pop(1)?.as_nat(env, "Count must be an integer")?;
                let handle = env
                    .pop(2)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                let bytes = match handle {
                    Handle::STDOUT => return Err(env.error("Cannot read from stdout")),
                    Handle::STDERR => return Err(env.error("Cannot read from stderr")),
                    Handle::STDIN => stdin()
                        .lock()
                        .bytes()
                        .take(count)
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|e| env.error(e))?,
                    _ => env.backend.read(handle, count).map_err(|e| env.error(e))?,
                };
                let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                env.push(s);
            }
            SysOp::ReadBytes => {
                let count = env.pop(1)?.as_nat(env, "Count must be an integer")?;
                let handle = env
                    .pop(2)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                let bytes = match handle {
                    Handle::STDOUT => return Err(env.error("Cannot read from stdout")),
                    Handle::STDERR => return Err(env.error("Cannot read from stderr")),
                    Handle::STDIN => stdin()
                        .lock()
                        .bytes()
                        .take(count)
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|e| env.error(e))?,
                    _ => env.backend.read(handle, count).map_err(|e| env.error(e))?,
                };
                env.push(Array::from(bytes.as_slice()));
            }
            SysOp::ReadUntil => {
                let delim = env.pop(1)?;
                let handle = env
                    .pop(2)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
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
                        let mut buffer = Vec::new();
                        let stdin = stdin().lock();
                        for byte in stdin.bytes() {
                            let byte = byte.map_err(|e| env.error(e))?;
                            buffer.push(byte);
                            if buffer.ends_with(&delim_bytes) {
                                break;
                            }
                        }
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
                                .backend
                                .read_until(handle, &delim)
                                .map_err(|e| env.error(e))?;
                            env.push(Array::from(bytes.as_slice()));
                        }
                        Value::Byte(arr) => {
                            let delim: Vec<u8> = arr.data.into();
                            let bytes = env
                                .backend
                                .read_until(handle, &delim)
                                .map_err(|e| env.error(e))?;
                            env.push(Array::from(bytes.as_slice()));
                        }
                        Value::Char(arr) => {
                            let delim: Vec<u8> = arr.data.iter().collect::<String>().into();
                            let bytes = env
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
                let handle = env
                    .pop(2)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                let bytes: Vec<u8> = match data {
                    Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                    Value::Byte(arr) => arr.data.into(),
                    Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                    Value::Box(_) => return Err(env.error("Cannot write function array to file")),
                };
                match handle {
                    Handle::STDOUT => env
                        .backend
                        .print_str_stdout(&String::from_utf8_lossy(&bytes))
                        .map_err(|e| env.error(e))?,
                    Handle::STDERR => env
                        .backend
                        .print_str_stderr(&String::from_utf8_lossy(&bytes))
                        .map_err(|e| env.error(e))?,
                    Handle::STDIN => return Err(env.error("Cannot write to stdin")),
                    _ => env
                        .backend
                        .write(handle, &bytes)
                        .map_err(|e| env.error(e))?,
                }
            }
            SysOp::FReadAllStr => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env
                    .backend
                    .file_read_all(&path)
                    .or_else(|e| {
                        if path == "example.ua" {
                            Ok(example_ua(|ex| ex.as_bytes().to_vec()))
                        } else {
                            Err(e)
                        }
                    })
                    .map_err(|e| env.error(e))?;
                let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                env.push(s);
            }
            SysOp::FReadAllBytes => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env
                    .backend
                    .file_read_all(&path)
                    .or_else(|e| {
                        if path == "example.ua" {
                            Ok(example_ua(|ex| ex.as_bytes().to_vec()))
                        } else {
                            Err(e)
                        }
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
                    Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                    Value::Box(_) => return Err(env.error("Cannot write function array to file")),
                };
                env.backend
                    .file_write_all(&path, &bytes)
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
                let exists = env.backend.file_exists(&path);
                env.push(exists);
            }
            SysOp::FListDir => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let paths = env.backend.list_dir(&path).map_err(|e| env.error(e))?;
                env.push(Array::<Boxed>::from_iter(paths));
            }
            SysOp::FIsFile => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let is_file = env.backend.is_file(&path).map_err(|e| env.error(e))?;
                env.push(is_file);
            }
            SysOp::Import => {
                let path = env.pop(1)?.as_string(env, "Import path must be a string")?;
                let item = env.pop(2)?.as_string(env, "Item name must be a string")?;
                let input = String::from_utf8(
                    env.backend
                        .file_read_all(&path)
                        .or_else(|e| {
                            if path == "example.ua" {
                                Ok(example_ua(|ex| ex.as_bytes().to_vec()))
                            } else {
                                Err(e)
                            }
                        })
                        .map_err(|e| env.error(e))?,
                )
                .map_err(|e| env.error(format!("Failed to read file: {e}")))?;
                env.import(&input, path.as_ref(), &item)?;
            }
            SysOp::Invoke => {
                let path = env.pop(1)?.as_string(env, "Invoke path must be a string")?;
                env.backend.invoke(&path).map_err(|e| env.error(e))?;
            }
            SysOp::ImDecode => {
                let bytes = match env.pop(1)? {
                    Value::Byte(arr) => {
                        if arr.rank() != 1 {
                            return Err(env.error(format!(
                                "Image bytes array must be rank 1, but is rank {}",
                                arr.rank()
                            )));
                        }
                        arr.data
                    }
                    Value::Num(arr) => {
                        if arr.rank() != 1 {
                            return Err(env.error(format!(
                                "Image bytes array must be rank 1, but is rank {}",
                                arr.rank()
                            )));
                        }
                        arr.data.iter().map(|&x| x as u8).collect()
                    }
                    _ => return Err(env.error("Image bytes must be a numeric array")),
                };
                let image = image::load_from_memory(&bytes)
                    .map_err(|e| env.error(format!("Failed to read image: {}", e)))?
                    .into_rgba8();
                let shape = tiny_vec![image.height() as usize, image.width() as usize, 4];
                let array = Array::<f64>::new(
                    shape,
                    image
                        .into_raw()
                        .into_iter()
                        .map(|b| b as f64 / 255.0)
                        .collect::<CowSlice<_>>(),
                );
                env.push(array);
            }
            SysOp::ImEncode => {
                let format = env
                    .pop(1)?
                    .as_string(env, "Image format must be a string")?;
                let value = env.pop(2)?;
                let output_format = match format.as_str() {
                    "jpg" | "jpeg" => ImageOutputFormat::Jpeg(100),
                    "png" => ImageOutputFormat::Png,
                    "bmp" => ImageOutputFormat::Bmp,
                    "gif" => ImageOutputFormat::Gif,
                    "ico" => ImageOutputFormat::Ico,
                    format => return Err(env.error(format!("Invalid image format: {}", format))),
                };
                let bytes =
                    value_to_image_bytes(&value, output_format).map_err(|e| env.error(e))?;
                env.push(Array::<u8>::from(bytes.as_slice()));
            }
            SysOp::ImShow => {
                let value = env.pop(1)?;
                let image = value_to_image(&value).map_err(|e| env.error(e))?;
                env.backend.show_image(image).map_err(|e| env.error(e))?;
            }
            SysOp::GifEncode => {
                let delay = env.pop(1)?.as_num(env, "Delay must be a number")?;
                let value = env.pop(2)?;
                let bytes = value_to_gif_bytes(&value, delay).map_err(|e| env.error(e))?;
                env.push(Array::<u8>::from(bytes.as_slice()));
            }
            SysOp::GifShow => {
                let delay = env.pop(1)?.as_num(env, "Delay must be a number")?;
                let value = env.pop(2)?;
                let bytes = value_to_gif_bytes(&value, delay).map_err(|e| env.error(e))?;
                env.backend.show_gif(bytes).map_err(|e| env.error(e))?;
            }
            SysOp::AudioDecode => {
                let bytes = match env.pop(1)? {
                    Value::Byte(arr) => {
                        if arr.rank() != 1 {
                            return Err(env.error(format!(
                                "Audio bytes array must be rank 1, but is rank {}",
                                arr.rank()
                            )));
                        }
                        arr.data
                    }
                    Value::Num(arr) => {
                        if arr.rank() != 1 {
                            return Err(env.error(format!(
                                "Audio bytes array must be rank 1, but is rank {}",
                                arr.rank()
                            )));
                        }
                        arr.data.iter().map(|&x| x as u8).collect()
                    }
                    _ => return Err(env.error("Audio bytes be a numeric array")),
                };
                let array = array_from_wav_bytes(&bytes, env).map_err(|e| env.error(e))?;
                env.push(array);
            }
            SysOp::AudioEncode => {
                let format = env
                    .pop(1)?
                    .as_string(env, "Audio format must be a string")?;
                let value = env.pop(2)?;
                let bytes = match format.as_str() {
                    "wav" => value_to_wav_bytes(&value, env.backend.audio_sample_rate())
                        .map_err(|e| env.error(e))?,
                    format => return Err(env.error(format!("Invalid audio format: {}", format))),
                };
                env.push(Array::<u8>::from(bytes.as_slice()));
            }
            SysOp::AudioPlay => {
                let value = env.pop(1)?;
                let bytes = value_to_wav_bytes(&value, env.backend.audio_sample_rate())
                    .map_err(|e| env.error(e))?;
                env.backend.play_audio(bytes).map_err(|e| env.error(e))?;
            }
            SysOp::AudioSampleRate => {
                let sample_rate = env.backend.audio_sample_rate();
                env.push(f64::from(sample_rate));
            }
            SysOp::AudioStream => {
                let f = env.pop_function()?;
                let mut stream_env = env.clone();
                if let Err(e) = env.backend.stream_audio(Box::new(move |time_array| {
                    let time_array = Array::<f64>::from(time_array.as_slice());
                    stream_env.push(time_array);
                    stream_env.call(f.clone())?;
                    let samples = &stream_env.pop(1)?;
                    let samples = samples.as_num_array().ok_or_else(|| {
                        stream_env.error("Audio stream function must return a numeric array")
                    })?;
                    match samples.shape() {
                        [_] => Ok(samples.data.iter().map(|&x| [x, x]).collect()),
                        [_, 2] => Ok(samples
                            .data
                            .chunks(2)
                            .map(|s| [s[0], s.get(1).copied().unwrap_or(0.0)])
                            .collect()),
                        _ => Err(stream_env.error(format!(
                            "Audio stream function must return a rank 1 or 2 array, but returned a rank {} array",
                            samples.rank()
                        ))),
                    }
                })) {
                    return Err(env.error(e));
                }
            }
            SysOp::Sleep => {
                let seconds = env
                    .pop(1)?
                    .as_num(env, "Sleep time must be a number")?
                    .max(0.0);
                env.backend.sleep(seconds).map_err(|e| env.error(e))?;
            }
            SysOp::TcpListen => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let handle = env.backend.tcp_listen(&addr).map_err(|e| env.error(e))?;
                env.push(handle);
            }
            SysOp::TcpAccept => {
                let handle = env
                    .pop(1)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                let new_handle = env.backend.tcp_accept(handle).map_err(|e| env.error(e))?;
                env.push(new_handle);
            }
            SysOp::TcpConnect => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let handle = env.backend.tcp_connect(&addr).map_err(|e| env.error(e))?;
                env.push(handle);
            }
            SysOp::TcpAddr => {
                let handle = env
                    .pop(1)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                let addr = env.backend.tcp_addr(handle).map_err(|e| env.error(e))?;
                env.push(addr);
            }
            SysOp::TcpSetNonBlocking => {
                let handle = env
                    .pop(1)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                env.backend
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
                let handle = env
                    .pop(2)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                env.backend
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
                let handle = env
                    .pop(2)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                env.backend
                    .tcp_set_write_timeout(handle, timeout)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::HttpsWrite => {
                let http = env
                    .pop(1)?
                    .as_string(env, "HTTP request must be a string")?;
                let handle = env
                    .pop(2)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                let res = env
                    .backend
                    .https_get(&http, handle)
                    .map_err(|e| env.error(e))?;
                env.push(res);
            }
            SysOp::Close => {
                let handle = env
                    .pop(1)?
                    .as_nat(env, "Handle must be an natural number")?
                    .into();
                env.backend.close(handle).map_err(|e| env.error(e))?;
            }
            SysOp::RunInherit => {
                let (command, args) = value_to_command(&env.pop(1)?, env)?;
                let args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
                let code = env
                    .backend
                    .run_command_inherit(&command, &args)
                    .map_err(|e| env.error(e))?;
                env.push(code);
            }
            SysOp::RunCapture => {
                let (command, args) = value_to_command(&env.pop(1)?, env)?;
                let args: Vec<_> = args.iter().map(|s| s.as_str()).collect();
                let (code, stdout, stderr) = env
                    .backend
                    .run_command_capture(&command, &args)
                    .map_err(|e| env.error(e))?;
                env.push(stderr);
                env.push(stdout);
                env.push(code);
            }
            SysOp::ChangeDirectory => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                env.backend
                    .change_directory(&path)
                    .map_err(|e| env.error(e))?;
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
        Value::Num(_) | Value::Byte(_) => {
            return Err(env.error(format!(
                "Command must be a string or function array, but it is {}",
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

pub fn value_to_image_bytes(value: &Value, format: ImageOutputFormat) -> Result<Vec<u8>, String> {
    image_to_bytes(&value_to_image(value)?, format)
}

pub fn image_to_bytes(image: &DynamicImage, format: ImageOutputFormat) -> Result<Vec<u8>, String> {
    let mut bytes = Cursor::new(Vec::new());
    image
        .write_to(&mut bytes, format)
        .map_err(|e| format!("Failed to write image: {e}"))?;
    Ok(bytes.into_inner())
}

pub fn value_to_image(value: &Value) -> Result<DynamicImage, String> {
    if ![2, 3].contains(&value.rank()) {
        return Err("Image must be a rank 2 or 3 numeric array".into());
    }
    let bytes = match value {
        Value::Num(nums) => nums
            .data
            .iter()
            .map(|f| (*f * 255.0).floor() as u8)
            .collect(),
        Value::Byte(bytes) => bytes.data.iter().map(|&b| (b > 0) as u8 * 255).collect(),
        _ => return Err("Image must be a numeric array".into()),
    };
    #[allow(clippy::match_ref_pats)]
    let [height, width, px_size] = match value.shape() {
        &[a, b] => [a, b, 1],
        &[a, b, c] => [a, b, c],
        _ => unreachable!("Shape checked above"),
    };
    Ok(match px_size {
        1 => image::GrayImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        2 => image::GrayAlphaImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        3 => image::RgbImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        4 => image::RgbaImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        n => {
            return Err(format!(
                "For a color image, the last dimension of the image array must be between 1 and 4 but it is {n}"
            ))
        }
    })
}

pub fn value_to_sample(audio: &Value) -> Result<Vec<[f32; 2]>, String> {
    let unrolled: Vec<f32> = match audio {
        Value::Num(nums) => nums.data.iter().map(|&f| f as f32).collect(),
        Value::Byte(byte) => byte.data.iter().map(|&b| b as f32).collect(),
        _ => return Err("Audio must be a numeric array".into()),
    };
    let (length, mut channels) = match audio.rank() {
        1 => (unrolled.len(), vec![unrolled]),
        2 => (
            audio.row_len(),
            unrolled
                .chunks_exact(audio.row_len())
                .map(|c| c.to_vec())
                .collect(),
        ),
        n => {
            return Err(format!(
                "Audio must be a rank 1 or 2 numeric array, but it is rank {n}"
            ))
        }
    };
    if channels.is_empty() {
        channels.push(vec![0.0; length]);
    }
    let mut sterio = Vec::new();
    if channels.len() == 1 {
        for sample in channels.into_iter().next().unwrap() {
            sterio.push([sample, sample]);
        }
    } else {
        for i in 0..length {
            let left = channels[0][i];
            let right = channels[1][i];
            sterio.push([left, right]);
        }
    }
    Ok(sterio)
}

pub fn value_to_audio_channels(audio: &Value) -> Result<Vec<Vec<f64>>, String> {
    let interleaved: Vec<f64> = match audio {
        Value::Num(nums) => nums.data.iter().copied().collect(),
        Value::Byte(byte) => byte.data.iter().map(|&b| b as f64).collect(),
        _ => return Err("Audio must be a numeric array".into()),
    };
    let (length, mut channels) = match audio.rank() {
        1 => (interleaved.len(), vec![interleaved]),
        2 => (
            audio.row_len(),
            interleaved
                .chunks_exact(audio.row_len())
                .map(|c| c.to_vec())
                .collect(),
        ),
        n => {
            return Err(format!(
                "Audio must be a rank 1 or 2 numeric array, but it is rank {n}"
            ))
        }
    };
    if channels.len() > 5 {
        return Err(format!(
            "Audio can have at most 5 channels, but its shape is {}",
            audio.format_shape()
        ));
    }

    if channels.is_empty() {
        channels.push(vec![0.0; length]);
    }
    Ok(channels)
}

pub fn value_to_wav_bytes(audio: &Value, sample_rate: u32) -> Result<Vec<u8>, String> {
    #[cfg(not(feature = "audio"))]
    {
        value_to_wav_bytes_impl(
            audio,
            |f| (f * i16::MAX as f64) as i16,
            16,
            SampleFormat::Int,
            sample_rate,
        )
    }
    #[cfg(feature = "audio")]
    {
        value_to_wav_bytes_impl(audio, |f| f as f32, 32, SampleFormat::Float, sample_rate)
    }
}

fn value_to_wav_bytes_impl<T: hound::Sample + Copy>(
    audio: &Value,
    convert_samples: impl Fn(f64) -> T + Copy,
    bits_per_sample: u16,
    sample_format: SampleFormat,
    sample_rate: u32,
) -> Result<Vec<u8>, String> {
    // We use i16 samples for compatibility with Firefox (if I remember correctly)
    let channels = value_to_audio_channels(audio)?;
    let channels: Vec<Vec<T>> = channels
        .into_iter()
        .map(|c| c.into_iter().map(convert_samples).collect())
        .collect();
    let spec = WavSpec {
        channels: channels.len() as u16,
        sample_rate,
        bits_per_sample,
        sample_format,
    };
    let mut bytes = Cursor::new(Vec::new());
    let mut writer = WavWriter::new(&mut bytes, spec).map_err(|e| e.to_string())?;
    for i in 0..channels[0].len() {
        for channel in &channels {
            writer
                .write_sample(channel[i])
                .map_err(|e| format!("Failed to write audio: {e}"))?;
        }
    }
    writer
        .finalize()
        .map_err(|e| format!("Failed to finalize audio: {e}"))?;
    Ok(bytes.into_inner())
}

fn array_from_wav_bytes(bytes: &[u8], env: &Uiua) -> UiuaResult<Array<f64>> {
    let mut reader: WavReader<Cursor<&[u8]>> =
        WavReader::new(Cursor::new(bytes)).map_err(|e| env.error(e.to_string()))?;
    let spec = reader.spec();
    match (spec.sample_format, spec.bits_per_sample) {
        (SampleFormat::Int, 16) => {
            array_from_wav_bytes_impl::<i16>(&mut reader, |i| i as f64 / i16::MAX as f64, env)
        }
        (SampleFormat::Int, 32) => {
            array_from_wav_bytes_impl::<i32>(&mut reader, |i| i as f64 / i32::MAX as f64, env)
        }
        (SampleFormat::Float, 32) => {
            array_from_wav_bytes_impl::<f32>(&mut reader, |f| f as f64, env)
        }
        (sample_format, bits_per_sample) => Err(env.error(format!(
            "Unsupported sample format: {:?} {} bits per sample",
            sample_format, bits_per_sample
        ))),
    }
}

fn array_from_wav_bytes_impl<T: hound::Sample>(
    reader: &mut WavReader<Cursor<&[u8]>>,
    sample_to_f64: impl Fn(T) -> f64,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    let channel_count = reader.spec().channels as usize;
    let mut channels = vec![EcoVec::new(); channel_count];
    let mut curr_channel = 0;
    for sample in reader.samples::<T>() {
        let sample = sample.map_err(|e| env.error(e.to_string()))?;
        channels[curr_channel].push(sample_to_f64(sample));
        curr_channel = (curr_channel + 1) % channel_count;
    }
    if channel_count == 1 {
        let channel = channels.pop().unwrap();
        Ok(channel.into())
    } else {
        Array::from_row_arrays(channels.into_iter().map(|ch| ch.into()), env)
    }
}

pub fn value_to_gif_bytes(value: &Value, frame_rate: f64) -> Result<Vec<u8>, String> {
    if value.row_count() == 0 {
        return Err("Cannot convert empty array into GIF".into());
    }
    let mut frames = Vec::with_capacity(value.row_count());
    let mut width = 0;
    let mut height = 0;
    for row in value.rows() {
        let image = value_to_image(&row)?.into_rgb8();
        width = image.width();
        height = image.height();
        frames.push(image);
    }
    if width > u16::MAX as u32 || height > u16::MAX as u32 {
        return Err(format!(
            "GIF dimensions must be at most {}x{}, but the frames are {}x{}",
            u16::MAX,
            u16::MAX,
            width,
            height
        ));
    }
    let mut reduction = 1;
    let mut bytes = Cursor::new(Vec::new());
    let mut all_colors = HashSet::new();
    for frame in &frames {
        for pixel in frame.pixels() {
            all_colors.insert(pixel.0);
        }
    }
    let mut used_colors = HashSet::new();
    let used_colors = 'colors: loop {
        used_colors.clear();
        let adder = reduction - 1;
        for color in &all_colors {
            used_colors.insert(color.map(|p| p.saturating_add(adder) / reduction));
            if used_colors.len() > 256 {
                reduction += 1;
                continue 'colors;
            }
        }
        break used_colors;
    };
    let mut palette = Vec::with_capacity(used_colors.len() * 3);
    let mut color_map: HashMap<[u8; 3], usize> = HashMap::new();
    for color in used_colors {
        color_map.insert(color, palette.len() / 3);
        palette.extend(color);
    }
    let mut encoder = gif::Encoder::new(&mut bytes, width as u16, height as u16, &palette)
        .map_err(|e| e.to_string())?;
    const MIN_FRAME_RATE: f64 = 1.0 / 60.0;
    let delay = ((1.0 / frame_rate.max(MIN_FRAME_RATE)).abs() * 100.0) as u16;
    encoder
        .set_repeat(gif::Repeat::Infinite)
        .map_err(|e| e.to_string())?;
    for image in frames {
        let mut frame = gif::Frame::from_rgb(width as u16, height as u16, image.as_raw());
        frame.delay = delay;
        encoder.write_frame(&frame).map_err(|e| e.to_string())?;
    }
    drop(encoder);
    Ok(bytes.into_inner())
}
