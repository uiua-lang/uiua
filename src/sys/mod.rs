#[cfg(feature = "native_sys")]
pub(crate) mod native;

use std::{
    any::Any,
    fmt,
    mem::take,
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

#[cfg(feature = "image")]
use image::DynamicImage;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use time::UtcOffset;

#[cfg(feature = "native_sys")]
pub use self::native::*;
use crate::{
    algorithm::{multi_output, validate_size},
    cowslice::cowslice,
    get_ops, Array, Boxed, FfiArg, FfiType, MetaPtr, Ops, Primitive, SysOp, Uiua, UiuaErrorKind,
    UiuaResult, Value,
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
        arr.meta.handle_kind = Some(kind);
        Boxed(arr.into()).into()
    }
}

impl Value {
    /// Attempt to convert the array to system handle
    ///
    /// The `requirement` parameter is used in error messages.
    pub fn as_handle(
        &self,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Handle> {
        let requirement = requirement
            .into()
            .unwrap_or("Expected value to be a stream handle");
        match self {
            Value::Box(b) => {
                if let Some(b) = b.as_scalar() {
                    b.0.as_nat(env, requirement).map(|h| Handle(h as u64))
                } else {
                    Err(env.error(format!("{requirement}, but it is rank {}", b.rank())))
                }
            }
            value => value.as_nat(env, requirement).map(|h| Handle(h as u64)),
        }
    }
}

/// The function type passed to `&rl`'s returned function
#[cfg(not(target_arch = "wasm32"))]
pub type ReadLinesFn<'a> = Box<dyn FnMut(String, &mut Uiua) -> UiuaResult + Send + 'a>;
/// The function type passed to `&rl`'s returned function
#[cfg(target_arch = "wasm32")]
pub type ReadLinesFn<'a> = Box<dyn FnMut(String, &mut Uiua) -> UiuaResult + 'a>;

/// The function type returned by `&rl`
#[cfg(not(target_arch = "wasm32"))]
pub type ReadLinesReturnFn<'a> = Box<dyn FnMut(&mut Uiua, ReadLinesFn) -> UiuaResult + Send + 'a>;
/// The function type returned by `&rl`
#[cfg(target_arch = "wasm32")]
pub type ReadLinesReturnFn<'a> = Box<dyn FnMut(&mut Uiua, ReadLinesFn) -> UiuaResult + 'a>;

/// The function type passed to `&ast`
#[cfg(not(target_arch = "wasm32"))]
pub type AudioStreamFn = Box<dyn FnMut(&[f64]) -> UiuaResult<Vec<[f64; 2]>> + Send>;
/// The function type passed to `&ast`
#[cfg(target_arch = "wasm32")]
pub type AudioStreamFn = Box<dyn FnMut(&[f64]) -> UiuaResult<Vec<[f64; 2]>>>;

/// The kind of a handle
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum HandleKind {
    File(PathBuf),
    TcpListener(SocketAddr),
    TlsListener(SocketAddr),
    TcpSocket(SocketAddr),
    TlsSocket(SocketAddr),
    UdpSocket(String),
    ChildStdin(String),
    ChildStdout(String),
    ChildStderr(String),
}

impl fmt::Display for HandleKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::File(path) => write!(f, "file {}", path.display()),
            Self::TcpListener(addr) => write!(f, "tcp listener {addr}"),
            Self::TlsListener(addr) => write!(f, "tls listener {addr}"),
            Self::TcpSocket(addr) => write!(f, "tcp socket {addr}"),
            Self::TlsSocket(addr) => write!(f, "tls socket {addr}"),
            Self::UdpSocket(addr) => write!(f, "udp socket {addr}"),
            Self::ChildStdin(com) => write!(f, "stdin {com}"),
            Self::ChildStdout(com) => write!(f, "stdout {com}"),
            Self::ChildStderr(com) => write!(f, "stderr {com}"),
        }
    }
}

/// Reference point and offset to seek a position in a stream with
/// This is used instead of `std::io::SeekFrom` because the latter is more general than what Uiua uses, so using a more specific enum reduces checks elsewhere in the code.
pub enum StreamSeek {
    /// Seek a position forward from the start of the stream
    Start(usize),
    /// Seek a position backward from the end of the stream
    End(usize),
}

impl From<StreamSeek> for std::io::SeekFrom {
    fn from(value: StreamSeek) -> Self {
        match value {
            StreamSeek::Start(off) => std::io::SeekFrom::Start(off as u64),
            StreamSeek::End(off) => std::io::SeekFrom::End(-(off as i64)),
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
    /// Check whether output is enabled
    fn output_enabled(&self) -> bool {
        true
    }
    /// Set whether output should be enabled
    ///
    /// Returns the previous value.
    ///
    /// It is the trait implementor's responsibility to ensure that this value is respected.
    fn set_output_enabled(&self, enabled: bool) -> bool {
        true
    }
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
    /// Show a value
    fn show(&self, value: Value) -> Result<(), String> {
        self.print_str_stdout(&format!("{}\n", value.show()))
    }
    /// Read a line from stdin
    ///
    /// Should return `Ok(None)` if EOF is reached.
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        Err("Reading from stdin is not supported in this environment".into())
    }
    /// Read a number of bytes from stdin
    ///
    /// If `count` is `None`, read until EOF.
    fn scan_stdin(&self, count: Option<usize>) -> Result<Vec<u8>, String> {
        Err("Reading from stdin is not supported in this environment".into())
    }
    /// Read from stdin until a delimiter is reached
    fn scan_until_stdin(&self, delim: &[u8]) -> Result<Vec<u8>, String> {
        let mut buffer = Vec::new();
        loop {
            let bytes = self.scan_stdin(Some(1))?;
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
    /// Read lines from a stream
    fn read_lines<'a>(&self, handle: Handle) -> Result<ReadLinesReturnFn<'a>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }
    /// Write bytes to a stream
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        Err("Writing to streams is not supported in this environment".into())
    }
    /// Go to an absolute file position
    fn seek(&self, handle: Handle, offset: StreamSeek) -> Result<(), String> {
        Err("Seeking streams is not supported in this environment".into())
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
    /// Whether thread spawning is allowed
    fn allow_thread_spawning(&self) -> bool {
        false
    }
    /// Show an image
    #[cfg(feature = "image")]
    fn show_image(&self, image: DynamicImage, label: Option<&str>) -> Result<(), String> {
        Err("Showing images is not supported in this environment".into())
    }
    /// Show a GIF
    fn show_gif(&self, gif_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        Err("Showing gifs is not supported in this environment".into())
    }
    /// Show an APNG
    fn show_apng(&self, apng_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        Err("Showing APNGs is not supported in this environment".into())
    }
    /// Play audio from WAV bytes
    fn play_audio(&self, wave_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        Err("Playing audio is not supported in this environment".into())
    }
    /// Get the audio sample rate
    fn audio_sample_rate(&self) -> u32 {
        44100
    }
    /// Stream audio
    fn stream_audio(&self, f: AudioStreamFn) -> Result<(), String> {
        Err("Streaming audio is not supported in this environment".into())
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
    /// Create a UDP socket and bind it to an address
    fn udp_bind(&self, addr: &str) -> Result<Handle, String> {
        Err("UDP sockets are not supported in this environment".into())
    }
    /// Receive a single datagram on a UDP socket
    fn udp_recv(&self, handle: Handle) -> Result<(Vec<u8>, SocketAddr), String> {
        Err("UDP sockets are not supported in this environment".into())
    }
    /// Send a datagram to an address over a UDP socket
    fn udp_send(&self, handle: Handle, packet: Vec<u8>, addr: &str) -> Result<(), String> {
        Err("UDP sockets are not supported in this environment".into())
    }
    /// Set the maximum message length for a UDP socket
    fn udp_set_max_msg_length(&self, handle: Handle, max_len: usize) -> Result<(), String> {
        Err("UDP sockets are not supported in this environment".into())
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
    /// Get the current directory
    fn get_current_directory(&self) -> Result<String, String> {
        Err("Getting the current directory is not supported in this environment".into())
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
        arg_tys: &[FfiArg],
        args: Vec<Value>,
    ) -> Result<Value, String> {
        Err("FFI is not supported in this environment".into())
    }
    /// Copy the data from a pointer into an array
    fn mem_copy(&self, ptr: MetaPtr, len: usize) -> Result<Value, String> {
        Err("Pointer copying is not supported in this environment".into())
    }
    /// Write data from an array into a pointer
    fn mem_set(&self, ptr: MetaPtr, idx: usize, value: Value) -> Result<(), String> {
        Err("Pointer writing is not supported in this environment".into())
    }
    /// Free a pointer
    fn mem_free(&self, ptr: &MetaPtr) -> Result<(), String> {
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
    /// Hit a breakpoint
    ///
    /// Returns whether to continue the program
    fn breakpoint(&self, env: &Uiua) -> Result<bool, String> {
        Err("Breakpoints are not supported in this environment".into())
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
    /// Whether to allow thread spawning
    pub allow_thread_spawning: bool,
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
    fn allow_thread_spawning(&self) -> bool {
        self.allow_thread_spawning
    }
}

impl SafeSys {
    /// Create a new safe system backend
    pub fn new() -> Self {
        Self::default()
    }
    /// Create a new safe system backend that allows thread spawning
    pub fn with_thread_spawning() -> Self {
        Self {
            allow_thread_spawning: true,
            ..Self::default()
        }
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

pub(crate) fn run_sys_op(op: &SysOp, env: &mut Uiua) -> UiuaResult {
    match op {
        SysOp::Show => {
            let val = env.pop(1)?;
            env.rt.backend.show(val).map_err(|e| env.error(e))?;
        }
        SysOp::Prin => {
            let s = env.pop(1)?.format();
            (env.rt.backend)
                .print_str_stdout(&s)
                .map_err(|e| env.error(e))?;
        }
        SysOp::Print => {
            let s = env.pop(1)?.format();
            (env.rt.backend)
                .print_str_stdout(&format!("{s}\n"))
                .map_err(|e| env.error(e))?;
        }
        SysOp::PrinErr => {
            let s = env.pop(1)?.format();
            (env.rt.backend)
                .print_str_stderr(&s)
                .map_err(|e| env.error(e))?;
        }
        SysOp::PrintErr => {
            let s = env.pop(1)?.format();
            (env.rt.backend)
                .print_str_stderr(&format!("{s}\n"))
                .map_err(|e| env.error(e))?;
        }
        SysOp::ScanLine => {
            let start = env.rt.backend.now();
            let res = env.rt.backend.scan_line_stdin().map_err(|e| env.error(e));
            env.rt.execution_start += env.rt.backend.now() - start;
            if let Some(line) = res? {
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
            let var = env
                .rt
                .backend
                .var(&key)
                .ok_or_else(|| env.error(format!("Environment variable `{key}` is not set")))?;
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
            let handle = env.pop(2)?.as_handle(env, None)?;
            let s = match handle {
                Handle::STDOUT => return Err(env.error("Cannot read from stdout")),
                Handle::STDERR => return Err(env.error("Cannot read from stderr")),
                Handle::STDIN => {
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
                                        .scan_stdin(Some(1))
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
                                        env.rt.backend.read(handle, 1).map_err(|e| env.error(e))?,
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
                        let bytes = env.rt.backend.read_all(handle).map_err(|e| env.error(e))?;
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
            let handle = env.pop(2)?.as_handle(env, None)?;
            let bytes = match handle {
                Handle::STDOUT => return Err(env.error("Cannot read from stdout")),
                Handle::STDERR => return Err(env.error("Cannot read from stderr")),
                Handle::STDIN => env.rt.backend.scan_stdin(count).map_err(|e| env.error(e))?,
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
            let handle = env.pop(2)?.as_handle(env, None)?;
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
            let handle = env.pop(2)?.as_handle(env, None)?;
            let bytes: Vec<u8> = match data {
                Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                Value::Byte(arr) => arr.data.into(),
                Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                val => return Err(env.error(format!("Cannot write {} array", val.type_name()))),
            };
            match handle {
                Handle::STDOUT => (env.rt.backend)
                    .print_str_stdout(&String::from_utf8_lossy(&bytes))
                    .map_err(|e| env.error(e))?,
                Handle::STDERR => (env.rt.backend)
                    .print_str_stderr(&String::from_utf8_lossy(&bytes))
                    .map_err(|e| env.error(e))?,
                Handle::STDIN => return Err(env.error("Cannot write to stdin")),
                _ => (env.rt.backend.write(handle, &bytes)).map_err(|e| env.error(e))?,
            }
        }
        SysOp::Seek => {
            let pos = env.pop(1)?.as_int(env, None)?;
            let pos = match pos {
                ..0 => StreamSeek::End((-pos) as usize),
                0.. => StreamSeek::Start(pos as usize),
            };
            let handle = env.pop(2)?.as_handle(env, None)?;
            env.rt.backend.seek(handle, pos).map_err(|e| env.error(e))?;
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
            env.push(Array::<u8>::from_iter(bytes));
        }
        SysOp::FWriteAll => {
            let path = env.pop(1)?.as_string(env, "Path must be a string")?;
            let data = env.pop(2)?;
            let bytes: Vec<u8> = match data {
                Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                Value::Byte(arr) => arr.data.into(),
                Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                val => {
                    return Err(env.error(format!("Cannot write {} array to file", val.type_name())))
                }
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
                let image = crate::media::value_to_image(&value).map_err(|e| env.error(e))?;
                (env.rt.backend)
                    .show_image(image, value.meta.label.as_deref())
                    .map_err(|e| env.error(e))?;
            }
            #[cfg(not(feature = "image"))]
            return Err(env.error("Image encoding is not supported in this environment"));
        }
        SysOp::GifShow => {
            #[cfg(feature = "gif")]
            {
                let delay = env.pop(1)?.as_num(env, "Framerate must be a number")?;
                let value = env.pop(2)?;
                let start = env.rt.backend.now();
                let bytes =
                    crate::media::value_to_gif_bytes(&value, delay).map_err(|e| env.error(e))?;
                env.rt.execution_start += env.rt.backend.now() - start;
                (env.rt.backend)
                    .show_gif(bytes, value.meta.label.as_deref())
                    .map_err(|e| env.error(e))?;
            }
            #[cfg(not(feature = "gif"))]
            return Err(env.error("GIF showing is not supported in this environment"));
        }
        SysOp::ApngShow => {
            #[cfg(feature = "apng")]
            {
                let delay = env.pop(1)?.as_num(env, "Framerate must be a number")?;
                let value = env.pop(2)?;
                let start = env.rt.backend.now();
                let bytes =
                    crate::media::value_to_apng_bytes(&value, delay).map_err(|e| env.error(e))?;
                env.rt.execution_start += env.rt.backend.now() - start;
                (env.rt.backend)
                    .show_apng(bytes.into_iter().collect(), value.meta.label.as_deref())
                    .map_err(|e| env.error(e))?;
            }
            #[cfg(not(feature = "apng"))]
            return Err(env.error("APNG showing is not supported in this environment"));
        }
        SysOp::AudioPlay => {
            #[cfg(feature = "audio_encode")]
            {
                let value = env.pop(1)?;
                let bytes =
                    crate::media::value_to_wav_bytes(&value, env.rt.backend.audio_sample_rate())
                        .map_err(|e| env.error(e))?;
                (env.rt.backend)
                    .play_audio(bytes, value.meta.label.as_deref())
                    .map_err(|e| env.error(e))?;
            }
            #[cfg(not(feature = "audio_encode"))]
            return Err(env.error("Audio encoding is not supported in this environment"));
        }
        SysOp::AudioSampleRate => {
            let sample_rate = env.rt.backend.audio_sample_rate();
            env.push(f64::from(sample_rate));
        }
        SysOp::Clip => {
            let contents = env.rt.backend.clipboard().map_err(|e| env.error(e))?;
            env.push(contents);
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
            let handle = env.pop(1)?.as_handle(env, None)?;
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
            let handle = env.pop(1)?.as_handle(env, None)?;
            let addr = env.rt.backend.tcp_addr(handle).map_err(|e| env.error(e))?;
            env.push(addr.to_string());
        }
        SysOp::TcpSetNonBlocking => {
            let handle = env.pop(1)?.as_handle(env, None)?;
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
            let handle = env.pop(2)?.as_handle(env, None)?;
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
            let handle = env.pop(2)?.as_handle(env, None)?;
            (env.rt.backend)
                .tcp_set_write_timeout(handle, timeout)
                .map_err(|e| env.error(e))?;
        }
        SysOp::UdpBind => {
            let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
            let handle = (env.rt.backend).udp_bind(&addr).map_err(|e| env.error(e))?;
            let handle = handle.value(HandleKind::UdpSocket(addr));
            env.push(handle)
        }
        SysOp::UdpReceive => {
            let handle = env.pop(1)?.as_handle(env, None)?;
            let (bytes, addr) = (env.rt.backend)
                .udp_recv(handle)
                .map_err(|e| env.error(e))?;
            env.push(addr.to_string());
            env.push(Array::from(bytes.as_slice()));
        }
        SysOp::UdpSend => {
            let bytes = env.pop(1)?.as_bytes(env, "Datagram must be bytes")?;
            let addr = env.pop(2)?.as_string(env, "Address must be a string")?;
            let handle = env.pop(3)?.as_handle(env, None)?;
            (env.rt.backend)
                .udp_send(handle, bytes, &addr)
                .map_err(|e| env.error(e))?;
        }
        SysOp::UdpSetMaxMsgLength => {
            let length = env
                .pop(1)?
                .as_nat(env, "Message length must be a natural number")?;
            let handle = env.pop(2)?.as_handle(env, None)?;
            (env.rt.backend)
                .udp_set_max_msg_length(handle, length)
                .map_err(|e| env.error(e))?;
        }
        SysOp::Close => {
            let handle = env.pop(1)?.as_handle(env, None)?;
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
            env.push(crate::media::rgb_image_to_array(_image));
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
            let name = (sig_frags.next().unwrap()).as_string(env, "FFI name must be a string")?;
            let arg_tys = sig_frags
                .map(|frag| {
                    frag.as_string(env, "FFI argument type must be a string")
                        .and_then(|ty| ty.parse::<FfiArg>().map_err(|e| env.error(e)))
                })
                .collect::<UiuaResult<Vec<_>>>()?;
            let args = env.pop(2)?;
            let args: Vec<Value> = args.into_rows().map(Value::unpacked).collect();
            let result = (env.rt.backend)
                .ffi(&file_name, result_ty, &name, &arg_tys, args)
                .map_err(|e| env.error(e))?;
            env.push(result);
        }
        SysOp::MemCopy => {
            let ptr = env
                .pop("pointer")?
                .meta
                .pointer
                .as_ref()
                .ok_or_else(|| env.error("Copied pointer must be a pointer value"))?
                .clone();
            let len = env
                .pop("length")?
                .as_nat(env, "Copied length must be a non-negative integer")?;
            let value = (env.rt.backend)
                .mem_copy(ptr, len)
                .map_err(|e| env.error(e))?;
            env.push(value);
        }
        SysOp::MemSet => {
            let ptr = env
                .pop("pointer")?
                .meta
                .pointer
                .as_ref()
                .ok_or_else(|| env.error("Target pointer must be a pointer value"))?
                .clone();
            let idx = env
                .pop("index")?
                .as_nat(env, "Target index must be a non-negative integer")?;
            let value = env.pop(3)?;
            (env.rt.backend)
                .mem_set(ptr, idx, value)
                .map_err(|e| env.error(e))?;
        }
        SysOp::MemFree => {
            let val = env.pop(1)?;
            let ptr = val
                .meta
                .pointer
                .as_ref()
                .ok_or_else(|| env.error("Freed pointer must be a pointer value"))?;

            (env.rt.backend).mem_free(ptr).map_err(|e| env.error(e))?;
        }
        SysOp::Breakpoint => {
            if !env.rt.backend.breakpoint(env).map_err(|e| env.error(e))? {
                return Err(UiuaErrorKind::Interrupted.into());
            }
        }
        prim => {
            return Err(env.error(if prim.modifier_args().is_some() {
                format!(
                    "{} was not handled as a modifier. \
                        This is a bug in the interpreter",
                    Primitive::Sys(*prim)
                )
            } else {
                format!(
                    "{} was not handled as a function. \
                        This is a bug in the interpreter",
                    Primitive::Sys(*prim)
                )
            }))
        }
    }
    Ok(())
}
pub(crate) fn run_sys_op_mod(op: &SysOp, ops: Ops, env: &mut Uiua) -> UiuaResult {
    match op {
        SysOp::ReadLines => {
            let [f] = get_ops(ops, env)?;
            let handle = env.pop(1)?.as_handle(env, None)?;
            let mut read_lines = env
                .rt
                .backend
                .read_lines(handle)
                .map_err(|e| env.error(e))?;
            let sig = f.sig;
            if sig.args() == 0 {
                return env.exec(f);
            }
            let acc_count = sig.args().saturating_sub(1);
            let out_count = sig.outputs().saturating_sub(acc_count);
            let mut outputs = multi_output(out_count, Vec::new());
            env.without_fill(|env| {
                read_lines(
                    env,
                    Box::new(|s, env| {
                        let val = Value::from(s);
                        env.push(val);
                        env.exec(f.clone())?;
                        for i in 0..out_count {
                            outputs[i].push(env.pop("read lines output")?);
                        }
                        Ok(())
                    }),
                )
            })?;
            for rows in outputs.into_iter().rev() {
                let val = Value::from_row_values(rows, env)?;
                env.push(val);
            }
        }
        SysOp::AudioStream => {
            let [f] = get_ops(ops, env)?;
            let push_time = f.sig.args() > 0;
            if f.sig != (0, 1) && f.sig.args() != f.sig.outputs() {
                return Err(env.error(format!(
                    "&ast's function must have the same number \
                        of inputs and outputs, but its signature is {}",
                    f.sig
                )));
            }
            if f.sig == (0, 0) {
                return Ok(());
            }
            let mut stream_env = env.clone();
            let res = env.rt.backend.stream_audio(Box::new(move |time_array| {
                if push_time {
                    let time_array = Array::<f64>::from(time_array);
                    stream_env.push(time_array);
                }
                stream_env.exec(f.clone())?;
                let samples = &stream_env.pop(1)?;
                let samples = samples.as_num_array().ok_or_else(|| {
                    stream_env.error("Audio stream function must return a numeric array")
                })?;
                match &*samples.shape {
                    [_] => Ok(samples.data.iter().map(|&x| [x, x]).collect()),
                    &[n, 2] => {
                        let mut samps: Vec<[f64; 2]> = Vec::with_capacity(n);
                        for samp in samples.data.chunks_exact(2) {
                            samps.push([samp[0], samp[1]]);
                        }
                        Ok(samps)
                    }
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
                        samples.shape
                    ))),
                }
            }));
            res.map_err(|e| env.error(e))?;
        }
        prim => {
            return Err(env.error(if prim.modifier_args().is_some() {
                format!(
                    "{} was not handled as a modifier. \
                        This is a bug in the interpreter",
                    Primitive::Sys(*prim)
                )
            } else {
                format!(
                    "{} was handled as a modifier. \
                        This is a bug in the interpreter",
                    Primitive::Sys(*prim)
                )
            }))
        }
    }
    Ok(())
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
                for Boxed(val) in &arr.data {
                    match val {
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
        val => {
            return Err(env.error(format!(
                "Command must be a string or box array, but it is {}",
                val.type_name_plural()
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
