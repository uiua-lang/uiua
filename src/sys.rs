use std::{
    collections::HashMap,
    env,
    fs::{self, File},
    io::{stderr, stdin, stdout, Cursor, Read, Write},
    mem::take,
    net::*,
    sync::{Mutex, OnceLock},
    thread::sleep,
    time::Duration,
};

use bufreaderwriter::seq::BufReaderWriterSeq;
use enum_iterator::Sequence;
use hound::{SampleFormat, WavSpec, WavWriter};
use image::{DynamicImage, ImageOutputFormat};

use crate::{
    array::Array, grid_fmt::GridFmt, primitive::PrimDoc, value::Value, Byte, Uiua, UiuaResult,
};

macro_rules! sys_op {
    ($(
        $(#[doc = $doc:literal])*
        (
            $args:literal$(($outputs:expr))?,
            $variant:ident, $name:literal
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
            pub fn args(&self) -> u8 {
                match self {
                    $(SysOp::$variant => $args,)*
                }
            }
            pub fn outputs(&self) -> Option<u8> {
                match self {
                    $($(SysOp::$variant => $outputs.into(),)?)*
                    _ => Some(1)
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
    (1(0), Show, "Show"),
    /// Print a value to stdout
    (1(0), Prin, "Prin"),
    /// Print a value to stdout followed by a newline
    (1(0), Print, "Print"),
    /// Read a line from stdin
    (0, ScanLine, "ScanLine"),
    /// Get the command line arguments
    (0, Args, "Args"),
    /// Get the value of an environment variable
    (1, Var, "Var"),
    /// Open a file and return a handle to it
    (1, FOpen, "FOpen"),
    /// Create a file and return a handle to it
    (1, FCreate, "FCreate"),
    /// Check if a file exists at a path
    (1, FExists, "FExists"),
    /// List the contents of a directory
    (1, FListDir, "FListDir"),
    /// Check if a path is a file
    (1, FIsFile, "FIsFile"),
    /// Read all the contents of a file into a string
    (1, FReadAllStr, "FReadAllStr"),
    /// Read all the contents of a file into a byte array
    (1, FReadAllBytes, "FReadAllBytes"),
    /// Write the entire contents of an array to a file
    (2, FWriteAll, "FWriteAll"),
    /// Read at most n bytes from a stream
    (2, ReadStr, "ReadStr"),
    /// Read at most n bytes from a stream
    (2, ReadBytes, "ReadBytes"),
    /// Read from a stream until a delimiter is reached
    (2, ReadUntil, "ReadUntil"),
    /// Write an array to a stream
    (2, Write, "Write"),
    /// Run the code from a file in a scope
    ///
    /// If the file has already been imported, its code will not be run again, but the values it originally pushed onto the stack will be pushed again.
    ///
    /// ex: import "example.ua"
    (1, Import, "Import"),
    /// Get the current time in milliseconds
    (0, Now, "Now"),
    /// Read an image from a file
    ///
    /// Supported formats are `jpg`, `png`, `bmp`, `gif`, and `ico`.
    (1, ImRead, "ImRead"),
    /// Write an image to a file
    ///
    /// The first argument is the path, and the second is the image.
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
    /// The format is determined by the file extension.
    /// Supported formats are `jpg`, `png`, `bmp`, `gif`, and `ico`.
    (2, ImWrite, "ImWrite"),
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
    (1(0), ImShow, "ImShow"),
    /// Play some audio
    ///
    /// The audio must be a rank 1 or 2 numeric array.
    ///
    /// A rank 1 array is a list of mono audio samples.
    /// For a rank 2 array, each row is a channel.
    ///
    /// The samples must be between -1 and 1.
    /// The sample rate is 44100 Hz.
    (1(0), AudioPlay, "AudioPlay"),
    /// Sleep for n milliseconds
    ///
    /// On the web, this example will hang for 2 seconds.
    /// ex: rand sleep 2000
    (1(0), Sleep, "Sleep"),
    (1, TcpListen, "TcpListen"),
    (1, TcpAccept, "TcpAccept"),
    (1, TcpConnect, "TcpConnect"),
    /// Close a stream by its handle
    ///
    /// This will close files, tcp listeners, and tcp sockets.
    (1, Close, "Close"),
}

/// A handle to an IO stream
///
/// 0 is stdin, 1 is stdout, 2 is stderr.
///
/// Other handles can be used by files or sockets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Handle(pub u64);

impl Handle {
    pub const STDIN: Self = Self(0);
    pub const STDOUT: Self = Self(1);
    pub const STDERR: Self = Self(2);
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

#[allow(unused_variables)]
pub trait SysBackend {
    fn print_str(&self, s: &str) -> Result<(), String> {
        self.write(Handle::STDOUT, s.as_bytes())
    }
    fn show_image(&self, image: DynamicImage) -> Result<(), String> {
        Err("Showing images not supported in this environment".into())
    }
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        Err("Playing audio not supported in this environment".into())
    }
    fn scan_line(&self) -> String {
        let mut bytes = Vec::new();
        while let Ok(b) = self.read(Handle::STDIN, 1) {
            if b.is_empty() || b[0] == b'\n' {
                break;
            }
            bytes.extend_from_slice(&b);
        }
        String::from_utf8_lossy(&bytes).into_owned()
    }
    fn var(&self, name: &str) -> Option<String> {
        None
    }
    fn args(&self) -> Vec<String> {
        Vec::new()
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
    fn sleep(&self, ms: f64) -> Result<(), String> {
        Err("Sleeping is not supported in this environment".into())
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
    fn close(&self, handle: Handle) -> Result<(), String> {
        Ok(())
    }
    fn teardown(&self) {}
}

#[derive(Default)]
pub struct NativeSys;

type Buffered<T> = BufReaderWriterSeq<T>;

struct GlobalNativeSys {
    next_handle: Handle,
    files: HashMap<Handle, Buffered<File>>,
    tcp_listeners: HashMap<Handle, TcpListener>,
    tcp_sockets: HashMap<Handle, Buffered<TcpStream>>,
}

enum SysStream<'a> {
    File(&'a mut Buffered<File>),
    TcpListener(&'a mut TcpListener),
    TcpSocket(&'a mut Buffered<TcpStream>),
}

impl Default for GlobalNativeSys {
    fn default() -> Self {
        Self {
            next_handle: Handle::FIRST_UNRESERVED,
            files: HashMap::new(),
            tcp_listeners: HashMap::new(),
            tcp_sockets: HashMap::new(),
        }
    }
}

impl GlobalNativeSys {
    fn new_handle(&mut self) -> Handle {
        for _ in 0..u64::MAX {
            let handle = self.next_handle;
            self.next_handle.0 = self.next_handle.0.overflowing_add(1).0;
            if !self.files.contains_key(&handle)
                && !self.tcp_listeners.contains_key(&handle)
                && !self.tcp_sockets.contains_key(&handle)
            {
                return handle;
            }
        }
        panic!("Ran out of file handles");
    }
    fn get_stream(&mut self, handle: Handle) -> Result<SysStream, String> {
        Ok(if let Some(file) = self.files.get_mut(&handle) {
            SysStream::File(file)
        } else if let Some(listener) = self.tcp_listeners.get_mut(&handle) {
            SysStream::TcpListener(listener)
        } else if let Some(socket) = self.tcp_sockets.get_mut(&handle) {
            SysStream::TcpSocket(socket)
        } else {
            return Err("Invalid file handle".to_string());
        })
    }
}

static NATIVE_SYS: OnceLock<Mutex<GlobalNativeSys>> = OnceLock::new();

fn stdio<T>(mut f: impl FnMut(&mut GlobalNativeSys) -> T) -> T {
    f(&mut NATIVE_SYS.get_or_init(Default::default).lock().unwrap())
}

thread_local! {
    #[cfg(feature = "rodio")]
    static AUDIO_STREAM: std::cell::RefCell<Option<rodio::OutputStream>> = None.into();
}

impl SysBackend for NativeSys {
    fn var(&self, name: &str) -> Option<String> {
        env::var(name).ok()
    }
    fn args(&self) -> Vec<String> {
        env::args().collect()
    }
    fn file_exists(&self, path: &str) -> bool {
        fs::metadata(path).is_ok()
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        fs::metadata(path)
            .map(|m| m.is_file())
            .map_err(|e| e.to_string())
    }
    fn list_dir(&self, path: &str) -> Result<Vec<String>, String> {
        let mut paths = Vec::new();
        for entry in fs::read_dir(path).map_err(|e| e.to_string())? {
            let entry = entry.map_err(|e| e.to_string())?;
            paths.push(entry.path().to_string_lossy().into());
        }
        Ok(paths)
    }
    fn open_file(&self, path: &str) -> Result<Handle, String> {
        stdio(|io| {
            let handle = io.new_handle();
            let file = File::open(path).map_err(|e| e.to_string())?;
            io.files.insert(handle, Buffered::new_reader(file));
            Ok(handle)
        })
    }
    fn create_file(&self, path: &str) -> Result<Handle, String> {
        stdio(|io| {
            let handle = io.new_handle();
            let file = File::create(path).map_err(|e| e.to_string())?;
            io.files.insert(handle, Buffered::new_writer(file));
            Ok(handle)
        })
    }
    fn read(&self, handle: Handle, len: usize) -> Result<Vec<u8>, String> {
        match handle {
            Handle::STDIN => {
                let mut buf = Vec::new();
                stdin()
                    .lock()
                    .take(len as u64)
                    .read_to_end(&mut buf)
                    .map_err(|e| e.to_string())?;
                Ok(buf)
            }
            Handle::STDOUT => Err("Cannot read from stdout".into()),
            Handle::STDERR => Err("Cannot read from stderr".into()),
            _ => stdio(|io| {
                Ok(match io.get_stream(handle)? {
                    SysStream::File(file) => {
                        let mut buf = Vec::new();
                        file.take(len as u64)
                            .read_to_end(&mut buf)
                            .map_err(|e| e.to_string())?;
                        buf
                    }
                    SysStream::TcpListener(_) => {
                        return Err("Cannot read from a tcp listener".to_string())
                    }
                    SysStream::TcpSocket(socket) => {
                        let mut buf = Vec::new();
                        socket
                            .take(len as u64)
                            .read_to_end(&mut buf)
                            .map_err(|e| e.to_string())?;
                        buf
                    }
                })
            }),
        }
    }
    fn write(&self, handle: Handle, conts: &[u8]) -> Result<(), String> {
        match handle {
            Handle::STDIN => Err("Cannot write to stdin".into()),
            Handle::STDOUT => stdout().lock().write_all(conts).map_err(|e| e.to_string()),
            Handle::STDERR => stderr().lock().write_all(conts).map_err(|e| e.to_string()),
            _ => stdio(|io| match io.get_stream(handle)? {
                SysStream::File(file) => file.write_all(conts).map_err(|e| e.to_string()),
                SysStream::TcpListener(_) => Err("Cannot write to a tcp listener".to_string()),
                SysStream::TcpSocket(socket) => socket.write_all(conts).map_err(|e| e.to_string()),
            }),
        }
    }
    #[cfg(feature = "terminal_image")]
    fn show_image(&self, image: DynamicImage) -> Result<(), String> {
        let (width, height) = if image.width() > image.height() {
            (term_size::dimensions().map(|(w, _)| w as u32), None)
        } else {
            (
                None,
                term_size::dimensions().map(|(_, h)| h.saturating_sub(1) as u32),
            )
        };
        viuer::print(
            &image,
            &viuer::Config {
                width,
                height,
                absolute_offset: false,
                transparent: true,
                ..Default::default()
            },
        )
        .map(drop)
        .map_err(|e| format!("Failed to show image: {e}"))
    }
    #[cfg(feature = "audio")]
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        use rodio::Source;
        let decoder = rodio::Decoder::new_wav(Cursor::new(wav_bytes))
            .map_err(|e| format!("Failed to decode audio: {e}"))?;
        let (stream, handle) = rodio::OutputStream::try_default()
            .map_err(|e| format!("Failed to create audio output stream: {e}"))?;
        AUDIO_STREAM.with(|s| *s.borrow_mut() = Some(stream));
        handle
            .play_raw(decoder.convert_samples())
            .map_err(|e| format!("Failed to play audio: {e}"))?;
        Ok(())
    }
    fn sleep(&self, ms: f64) -> Result<(), String> {
        sleep(Duration::from_secs_f64(ms / 1000.0));
        Ok(())
    }
    fn tcp_listen(&self, addr: &str) -> Result<Handle, String> {
        stdio(|io| {
            let handle = io.new_handle();
            let listener = TcpListener::bind(addr).map_err(|e| e.to_string())?;
            io.tcp_listeners.insert(handle, listener);
            Ok(handle)
        })
    }
    fn tcp_accept(&self, handle: Handle) -> Result<Handle, String> {
        stdio(|io| {
            let listener = io
                .tcp_listeners
                .get_mut(&handle)
                .ok_or_else(|| "Invalid tcp listener handle".to_string())?;
            let (stream, _) = listener.accept().map_err(|e| e.to_string())?;
            let handle = io.new_handle();
            io.tcp_sockets.insert(handle, Buffered::new_reader(stream));
            Ok(handle)
        })
    }
    fn tcp_connect(&self, addr: &str) -> Result<Handle, String> {
        stdio(|io| {
            let handle = io.new_handle();
            let stream = TcpStream::connect(addr).map_err(|e| e.to_string())?;
            io.tcp_sockets.insert(handle, Buffered::new_writer(stream));
            Ok(handle)
        })
    }
    fn close(&self, handle: Handle) -> Result<(), String> {
        stdio(|io| {
            if io.files.remove(&handle).is_some()
                || io.tcp_listeners.remove(&handle).is_some()
                || io.tcp_sockets.remove(&handle).is_some()
            {
                Ok(())
            } else {
                Err("Invalid stream handle".to_string())
            }
        })
    }
    fn teardown(&self) {
        stdio(take);
    }
}

impl SysOp {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            SysOp::Show => {
                let s = env.pop(1)?.grid_string();
                env.io.print_str(&s).map_err(|e| env.error(e))?;
                env.io.print_str("\n").map_err(|e| env.error(e))?;
            }
            SysOp::Prin => {
                let val = env.pop(1)?;
                env.io
                    .print_str(&val.to_string())
                    .map_err(|e| env.error(e))?;
            }
            SysOp::Print => {
                let val = env.pop(1)?;
                env.io
                    .print_str(&val.to_string())
                    .map_err(|e| env.error(e))?;
                env.io.print_str("\n").map_err(|e| env.error(e))?;
            }
            SysOp::ScanLine => {
                let line = env.io.scan_line();
                env.push(line);
            }
            SysOp::Args => {
                let args = env.io.args();
                env.push(Array::<char>::from_iter(args));
            }
            SysOp::Var => {
                let key = env
                    .pop(1)?
                    .as_string(env, "Augument to var must be a string")?;
                let var = env.io.var(&key).unwrap_or_default();
                env.push(var);
            }
            SysOp::FOpen => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle = env.io.open_file(&path).map_err(|e| env.error(e))?;
                env.push(handle);
            }
            SysOp::FCreate => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle = env.io.create_file(&path).map_err(|e| env.error(e))?;
                env.push(handle.0 as f64);
            }
            SysOp::ReadStr => {
                let count = env.pop(1)?.as_nat(env, "Count must be an integer")?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                let bytes = env.io.read(handle, count).map_err(|e| env.error(e))?;
                let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                env.push(s);
            }
            SysOp::ReadBytes => {
                let count = env.pop(1)?.as_nat(env, "Count must be an integer")?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                let bytes = env.io.read(handle, count).map_err(|e| env.error(e))?;
                let bytes = bytes.into_iter().map(Into::into);
                env.push(Array::<Byte>::from_iter(bytes));
            }
            SysOp::ReadUntil => {
                let delim = env.pop(1)?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                if delim.rank() > 1 {
                    return Err(env.error("Delimiter must be a rank 0 or 1 string or byte array"));
                }
                match delim {
                    Value::Num(arr) => {
                        let delim: Vec<u8> = arr.data.iter().map(|&x| x as u8).collect();
                        let bytes = env
                            .io
                            .read_until(handle, &delim)
                            .map_err(|e| env.error(e))?;
                        let bytes: Vec<Byte> = bytes.into_iter().map(Byte::from).collect();
                        env.push(bytes);
                    }
                    Value::Byte(arr) => {
                        let delim: Vec<u8> = arr.data.iter().filter_map(|x| x.value()).collect();
                        let bytes = env
                            .io
                            .read_until(handle, &delim)
                            .map_err(|e| env.error(e))?;
                        let bytes: Vec<Byte> = bytes.into_iter().map(Byte::from).collect();
                        env.push(bytes);
                    }
                    Value::Char(arr) => {
                        let delim: Vec<u8> = arr.data.iter().collect::<String>().into();
                        let bytes = env
                            .io
                            .read_until(handle, &delim)
                            .map_err(|e| env.error(e))?;
                        let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                        env.push(s);
                    }
                    _ => return Err(env.error("Delimiter must be a string or byte array")),
                }
            }
            SysOp::Write => {
                let data = env.pop(1)?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                let bytes: Vec<u8> = match data {
                    Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                    Value::Byte(arr) => arr.data.iter().filter_map(|x| x.value()).collect(),
                    Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                    Value::Func(_) => return Err(env.error("Cannot write function array to file")),
                };
                env.io.write(handle, &bytes).map_err(|e| env.error(e))?;
            }
            SysOp::FReadAllStr => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env.io.file_read_all(&path).map_err(|e| env.error(e))?;
                let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                env.push(s);
            }
            SysOp::FReadAllBytes => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env.io.file_read_all(&path).map_err(|e| env.error(e))?;
                let bytes = bytes.into_iter().map(Into::into);
                env.push(Array::<Byte>::from_iter(bytes));
            }
            SysOp::FWriteAll => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let data = env.pop(2)?;
                let bytes: Vec<u8> = match data {
                    Value::Num(arr) => arr.data.iter().map(|&x| x as u8).collect(),
                    Value::Byte(arr) => arr.data.iter().filter_map(|x| x.value()).collect(),
                    Value::Char(arr) => arr.data.iter().collect::<String>().into(),
                    Value::Func(_) => return Err(env.error("Cannot write function array to file")),
                };
                env.io
                    .file_write_all(&path, &bytes)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::FExists => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let exists = env.io.file_exists(&path);
                env.push(exists);
            }
            SysOp::FListDir => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let paths = env.io.list_dir(&path).map_err(|e| env.error(e))?;
                env.push(Array::<char>::from_iter(paths));
            }
            SysOp::FIsFile => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let is_file = env.io.is_file(&path).map_err(|e| env.error(e))?;
                env.push(is_file);
            }
            SysOp::Import => {
                let path = env.pop(1)?.as_string(env, "Import path must be a string")?;
                let input =
                    String::from_utf8(env.io.file_read_all(&path).map_err(|e| env.error(e))?)
                        .map_err(|e| env.error(format!("Failed to read file: {e}")))?;
                env.import(&input, path.as_ref())?;
            }
            SysOp::Now => env.push(instant::now()),
            SysOp::ImRead => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env.io.file_read_all(&path).map_err(|e| env.error(e))?;
                let image = image::load_from_memory(&bytes)
                    .map_err(|e| env.error(format!("Failed to read image: {}", e)))?
                    .into_rgba8();
                let shape = vec![image.height() as usize, image.width() as usize, 4];
                let bytes: Vec<Byte> = image.into_raw().into_iter().map(Into::into).collect();
                let array = Array::<Byte>::from((shape, bytes));
                env.push(array);
            }
            SysOp::ImWrite => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let value = env.pop(2)?;
                let ext = path.split('.').last().unwrap_or("");
                let output_format = match ext {
                    "jpg" | "jpeg" => ImageOutputFormat::Jpeg(100),
                    "png" => ImageOutputFormat::Png,
                    "bmp" => ImageOutputFormat::Bmp,
                    "gif" => ImageOutputFormat::Gif,
                    "ico" => ImageOutputFormat::Ico,
                    _ => ImageOutputFormat::Png,
                };
                let bytes =
                    value_to_image_bytes(&value, output_format).map_err(|e| env.error(e))?;
                env.io
                    .file_write_all(&path, &bytes)
                    .map_err(|e| env.error(e))?;
            }
            SysOp::ImShow => {
                let value = env.pop(1)?;
                let image = value_to_image(&value).map_err(|e| env.error(e))?;
                env.io.show_image(image).map_err(|e| env.error(e))?;
            }
            SysOp::AudioPlay => {
                let value = env.pop(1)?;
                let bytes = value_to_wav_bytes(&value).map_err(|e| env.error(e))?;
                env.io.play_audio(bytes).map_err(|e| env.error(e))?;
            }
            SysOp::Sleep => {
                let ms = env
                    .pop(1)?
                    .as_num(env, "Sleep time must be a number")?
                    .max(0.0);
                env.io.sleep(ms).map_err(|e| env.error(e))?;
            }
            SysOp::TcpListen => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let handle = env.io.tcp_listen(&addr).map_err(|e| env.error(e))?;
                env.push(handle);
            }
            SysOp::TcpAccept => {
                let handle = env.pop(1)?.as_nat(env, "Handle must be an integer")?.into();
                let new_handle = env.io.tcp_accept(handle).map_err(|e| env.error(e))?;
                env.push(new_handle);
            }
            SysOp::TcpConnect => {
                let addr = env.pop(1)?.as_string(env, "Address must be a string")?;
                let handle = env.io.tcp_connect(&addr).map_err(|e| env.error(e))?;
                env.push(handle);
            }
            SysOp::Close => {
                let handle = env.pop(1)?.as_nat(env, "Handle must be an integer")?.into();
                env.io.close(handle).map_err(|e| env.error(e))?;
            }
        }
        Ok(())
    }
}

pub fn value_to_image_bytes(value: &Value, format: ImageOutputFormat) -> Result<Vec<u8>, String> {
    let mut bytes = Cursor::new(Vec::new());
    value_to_image(value)?
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
        Value::Byte(bytes) => bytes.data.iter().map(|&b| b.or(0).min(1) * 255).collect(),
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

pub fn value_to_wav_bytes(audio: &Value) -> Result<Vec<u8>, String> {
    // We use i16 samples for compatibility with Firefox (if I remember correctly)
    let values: Vec<i16> = match audio {
        Value::Num(nums) => nums
            .data
            .iter()
            .map(|&f| (f.clamp(-1.0, 1.0) * i16::MAX as f64) as i16)
            .collect(),
        Value::Byte(byte) => byte
            .data
            .iter()
            .map(|&b| b.value().map_or(0, |_| i16::MAX))
            .collect(),
        _ => return Err("Audio must be a numeric array".into()),
    };
    let (length, channels) = match audio.rank() {
        1 => (values.len(), vec![values]),
        2 => (
            audio.row_len(),
            values
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
    let spec = WavSpec {
        channels: channels.len() as u16,
        sample_rate: 44100,
        bits_per_sample: 16,
        sample_format: SampleFormat::Int,
    };
    let mut bytes = Cursor::new(Vec::new());
    let mut writer = WavWriter::new(&mut bytes, spec).map_err(|e| e.to_string())?;
    for i in 0..length {
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
