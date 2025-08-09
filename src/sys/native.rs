use std::{
    any::Any,
    env::{self, set_current_dir},
    fs::{self, File, OpenOptions},
    io::{stderr, stdin, stdout, BufRead, BufReader, Read, Write},
    net::*,
    path::{Path, PathBuf},
    process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, Stdio},
    slice,
    sync::{
        atomic::{self, AtomicBool, AtomicU64},
        Arc, LazyLock,
    },
    thread::sleep,
    time::Duration,
};

use colored::Colorize;
#[cfg(feature = "webcam")]
use crossbeam_channel as channel;
use dashmap::DashMap;

use crate::{
    terminal_size, GitTarget, Handle, MetaPtr, ReadLinesFn, ReadLinesReturnFn, Span, SysBackend,
    Uiua, Value,
};

/// The default native system backend
#[derive(Default)]
pub struct NativeSys;

struct GlobalNativeSys {
    output_enabled: AtomicBool,
    next_handle: AtomicU64,
    files: DashMap<Handle, BufReader<File>>,
    child_stdins: DashMap<Handle, ChildStream<ChildStdin>>,
    child_stdouts: DashMap<Handle, ChildStream<BufReader<ChildStdout>>>,
    child_stderrs: DashMap<Handle, ChildStream<BufReader<ChildStderr>>>,
    tcp_listeners: DashMap<Handle, TcpListener>,
    tls_listeners: DashMap<Handle, TlsListener>,
    tcp_sockets: DashMap<Handle, TcpStream>,
    tls_sockets: DashMap<Handle, TlsSocket>,
    udp_sockets: DashMap<Handle, UdpSocket>,
    #[cfg(feature = "webcam")]
    cam_channels: DashMap<usize, WebcamChannel>,
    hostnames: DashMap<Handle, String>,
    git_paths: DashMap<String, Result<PathBuf, String>>,
    #[cfg(feature = "audio")]
    audio_stream_time: parking_lot::Mutex<Option<f64>>,
    #[cfg(feature = "audio")]
    audio_time_socket: parking_lot::Mutex<Option<std::sync::Arc<std::net::UdpSocket>>>,
    colored_errors: DashMap<String, String>,
    #[cfg(feature = "ffi")]
    ffi: crate::FfiState,
    #[cfg(all(feature = "gif", feature = "invoke"))]
    gifs_child: parking_lot::Mutex<Option<Child>>,
}

enum SysStream<'a> {
    File(dashmap::mapref::one::RefMut<'a, Handle, BufReader<File>>),
    ChildStdin(dashmap::mapref::one::RefMut<'a, Handle, ChildStream<ChildStdin>>),
    ChildStdout(dashmap::mapref::one::RefMut<'a, Handle, ChildStream<BufReader<ChildStdout>>>),
    ChildStderr(dashmap::mapref::one::RefMut<'a, Handle, ChildStream<BufReader<ChildStderr>>>),
    TcpSocket(dashmap::mapref::one::Ref<'a, Handle, TcpStream>),
    TlsSocket(dashmap::mapref::one::Ref<'a, Handle, TlsSocket>),
}

struct UdpSocket {
    socket: std::net::UdpSocket,
    max_msg_len: usize,
}

struct ChildStream<T> {
    stream: T,
    child: Arc<Child>,
}

impl<T> Drop for ChildStream<T> {
    fn drop(&mut self) {
        if let Some(child) = Arc::get_mut(&mut self.child) {
            _ = child.kill();
        }
    }
}

#[cfg(feature = "webcam")]
struct WebcamChannel {
    send: channel::Sender<()>,
    recv: channel::Receiver<Result<image::RgbImage, String>>,
}

#[cfg(feature = "webcam")]
impl WebcamChannel {
    fn new(index: usize) -> Result<Self, String> {
        use nokhwa::{
            pixel_format::RgbFormat,
            utils::{CameraIndex, RequestedFormat, RequestedFormatType},
            Camera,
        };
        let (req_send, req_recv) = channel::unbounded();
        let (image_send, image_recv) = channel::unbounded();
        std::thread::spawn(move || {
            let mut camera = match Camera::new(
                CameraIndex::Index(index as u32),
                RequestedFormat::new::<RgbFormat>(RequestedFormatType::AbsoluteHighestResolution),
            ) {
                Ok(camera) => camera,
                Err(e) => {
                    _ = image_send.send(Err(e.to_string()));
                    return;
                }
            };
            if let Err(e) = camera.open_stream() {
                _ = image_send.send(Err(e.to_string()));
                return;
            }
            let mut recv_tries = 0;
            let sleep = || std::thread::sleep(std::time::Duration::from_millis(10));
            loop {
                if req_recv.try_recv().is_err() {
                    recv_tries += 1;
                    if recv_tries > 100 {
                        _ = camera.stop_stream();
                        break;
                    } else {
                        sleep();
                        continue;
                    }
                }
                recv_tries = 0;
                let res = camera
                    .frame()
                    .and_then(|buffer| buffer.decode_image::<RgbFormat>())
                    .map_err(|e| e.to_string());
                if image_send.send(res).is_err() {
                    _ = camera.stop_stream();
                    break;
                }
                sleep();
            }
        });
        Ok(Self {
            send: req_send,
            recv: image_recv,
        })
    }
}

struct TlsSocket {
    stream: TcpStream,
    #[cfg(feature = "tls")]
    conn: parking_lot::Mutex<TslConnection>,
}

#[cfg(feature = "tls")]
enum TslConnection {
    Client(rustls::ClientConnection),
    Server(rustls::ServerConnection),
}

impl Read for &TlsSocket {
    fn read(&mut self, _buf: &mut [u8]) -> std::io::Result<usize> {
        #[cfg(feature = "tls")]
        {
            match &mut *self.conn.lock() {
                TslConnection::Client(conn) => {
                    rustls::Stream::new(conn, &mut &self.stream).read(_buf)
                }
                TslConnection::Server(conn) => {
                    rustls::Stream::new(conn, &mut &self.stream).read(_buf)
                }
            }
        }
        #[cfg(not(feature = "tls"))]
        Ok(0)
    }
}

impl Write for &TlsSocket {
    fn write(&mut self, _buf: &[u8]) -> std::io::Result<usize> {
        #[cfg(feature = "tls")]
        {
            match &mut *self.conn.lock() {
                TslConnection::Client(conn) => {
                    rustls::Stream::new(conn, &mut &self.stream).write(_buf)
                }
                TslConnection::Server(conn) => {
                    rustls::Stream::new(conn, &mut &self.stream).write(_buf)
                }
            }
        }
        #[cfg(not(feature = "tls"))]
        Ok(0)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        (&mut &self.stream).flush()
    }
}

struct TlsListener {
    listener: TcpListener,
    #[cfg(feature = "tls")]
    config: std::sync::Arc<rustls::ServerConfig>,
}

impl Default for GlobalNativeSys {
    fn default() -> Self {
        Self {
            output_enabled: AtomicBool::new(true),
            next_handle: Handle::FIRST_UNRESERVED.0.into(),
            files: DashMap::new(),
            child_stdins: DashMap::new(),
            child_stdouts: DashMap::new(),
            child_stderrs: DashMap::new(),
            tcp_listeners: DashMap::new(),
            tls_listeners: DashMap::new(),
            tcp_sockets: DashMap::new(),
            tls_sockets: DashMap::new(),
            udp_sockets: DashMap::new(),
            #[cfg(feature = "webcam")]
            cam_channels: DashMap::new(),
            hostnames: DashMap::new(),
            git_paths: DashMap::new(),
            #[cfg(feature = "audio")]
            audio_stream_time: parking_lot::Mutex::new(None),
            #[cfg(feature = "audio")]
            audio_time_socket: parking_lot::Mutex::new(None),
            colored_errors: DashMap::new(),
            #[cfg(feature = "ffi")]
            ffi: Default::default(),
            #[cfg(all(feature = "gif", feature = "invoke"))]
            gifs_child: parking_lot::Mutex::new(None),
        }
    }
}

impl GlobalNativeSys {
    fn new_handle(&self) -> Handle {
        for _ in 0..u64::MAX {
            let handle = Handle(self.next_handle.fetch_add(1, atomic::Ordering::Relaxed));
            if !self.files.contains_key(&handle)
                && !self.child_stdins.contains_key(&handle)
                && !self.child_stdouts.contains_key(&handle)
                && !self.child_stderrs.contains_key(&handle)
                && !self.tcp_listeners.contains_key(&handle)
                && !self.tcp_sockets.contains_key(&handle)
                && !self.tls_sockets.contains_key(&handle)
                && !self.udp_sockets.contains_key(&handle)
            {
                return handle;
            }
        }
        panic!("Ran out of stream handles");
    }
    fn get_stream(&self, handle: Handle) -> Result<SysStream, String> {
        Ok(if let Some(file) = self.files.get_mut(&handle) {
            SysStream::File(file)
        } else if let Some(child) = self.child_stdins.get_mut(&handle) {
            SysStream::ChildStdin(child)
        } else if let Some(child) = self.child_stdouts.get_mut(&handle) {
            SysStream::ChildStdout(child)
        } else if let Some(child) = self.child_stderrs.get_mut(&handle) {
            SysStream::ChildStderr(child)
        } else if let Some(socket) = self.tcp_sockets.get(&handle) {
            SysStream::TcpSocket(socket)
        } else if let Some(tls_socket) = self.tls_sockets.get(&handle) {
            SysStream::TlsSocket(tls_socket)
        } else {
            return Err("Invalid stream handle".to_string());
        })
    }
    fn get_tcp_listener<T>(&self, handle: Handle, f: impl FnOnce(&TcpListener) -> T) -> Option<T> {
        if let Some(listener) = self.tcp_listeners.get(&handle) {
            Some(f(&listener))
        } else {
            (self.tls_listeners.get(&handle)).map(|listener| f(&listener.listener))
        }
    }
    fn get_tcp_stream<T>(&self, handle: Handle, f: impl FnOnce(&TcpStream) -> T) -> Option<T> {
        if let Some(sock) = self.tcp_sockets.get(&handle) {
            Some(f(&sock))
        } else {
            (self.tls_sockets.get(&handle)).map(|sock| f(&sock.stream))
        }
    }
    fn get_udp_socket<T>(&self, handle: Handle, f: impl FnOnce(&UdpSocket) -> T) -> Option<T> {
        self.udp_sockets.get(&handle).map(|sock| f(&sock))
    }
    fn get_udp_socket_mut<T>(
        &self,
        handle: Handle,
        f: impl FnOnce(&mut UdpSocket) -> T,
    ) -> Option<T> {
        self.udp_sockets
            .get_mut(&handle)
            .map(|mut sock| f(&mut sock))
    }
}

static NATIVE_SYS: LazyLock<GlobalNativeSys> = LazyLock::new(Default::default);

#[cfg(all(feature = "audio", feature = "binary"))]
#[doc(hidden)]
pub fn set_audio_stream_time(time: f64) {
    *NATIVE_SYS.audio_stream_time.lock() = Some(time);
}

#[cfg(all(feature = "audio", feature = "binary"))]
#[doc(hidden)]
pub fn set_audio_stream_time_port(port: u16) -> std::io::Result<()> {
    let socket = std::net::UdpSocket::bind(("127.0.0.1", 0))?;
    socket.connect(("127.0.0.1", port))?;
    *NATIVE_SYS.audio_time_socket.lock() = Some(std::sync::Arc::new(socket));
    Ok(())
}

impl SysBackend for NativeSys {
    fn any(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
    fn output_enabled(&self) -> bool {
        NATIVE_SYS.output_enabled.load(atomic::Ordering::Relaxed)
    }

    fn set_output_enabled(&self, enabled: bool) -> bool {
        NATIVE_SYS
            .output_enabled
            .swap(enabled, atomic::Ordering::Relaxed)
    }
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        if !self.output_enabled() {
            return Ok(());
        }
        let mut stdout = stdout().lock();
        stdout.write_all(s.as_bytes()).map_err(|e| e.to_string())?;
        stdout.flush().map_err(|e| e.to_string())?;
        Ok(())
    }
    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        if !self.output_enabled() {
            return Ok(());
        }
        let mut stderr = stderr().lock();
        stderr.write_all(s.as_bytes()).map_err(|e| e.to_string())?;
        stderr.flush().map_err(|e| e.to_string())
    }
    fn print_str_trace(&self, s: &str) {
        if !self.output_enabled() {
            return;
        }
        eprint!("{s}");
        _ = stderr().flush();
    }
    fn show(&self, value: Value) -> Result<(), String> {
        #[cfg(feature = "window")]
        if crate::window::use_window() {
            return crate::window::Request::Show(crate::media::SmartOutput::Normal(value.show()))
                .send();
        }
        self.print_str_stdout(&format!("{}\n", value.show()))
    }
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        if !self.output_enabled() {
            return Ok(None);
        }
        let mut buffer = Vec::new();
        let mut b = 0u8;
        loop {
            if let Err(e) = stdin().read_exact(slice::from_mut(&mut b)) {
                if e.kind() == std::io::ErrorKind::UnexpectedEof {
                    return Ok(None);
                }
                return Err(e.to_string());
            }

            match b {
                #[cfg(feature = "raw_mode")]
                b'\r' if rawrrr::is_raw() => break,
                b'\r' => continue,
                b'\n' | 3 => break,
                b => buffer.push(b),
            }
        }
        Ok(Some(String::from_utf8(buffer).map_err(|e| e.to_string())?))
    }
    fn scan_stdin(&self, count: Option<usize>) -> Result<Vec<u8>, String> {
        if !self.output_enabled() {
            return Ok(Vec::new());
        }
        Ok(if let Some(count) = count {
            let mut buffer = vec![0; count];
            stdin().read_exact(&mut buffer).map_err(|e| e.to_string())?;
            buffer
        } else {
            let mut buffer = Vec::new();
            stdin()
                .read_to_end(&mut buffer)
                .map_err(|e| e.to_string())?;
            buffer
        })
    }
    fn save_error_color(&self, message: String, colored: String) {
        NATIVE_SYS.colored_errors.insert(message, colored);
    }
    fn term_size(&self) -> Result<(usize, usize), String> {
        let (w, h) = terminal_size().ok_or("Failed to get terminal size")?;
        Ok((w, h.saturating_sub(1)))
    }
    fn exit(&self, code: i32) -> Result<(), String> {
        std::process::exit(code)
    }
    #[cfg(feature = "raw_mode")]
    fn set_raw_mode(&self, raw_mode: bool) -> Result<(), String> {
        if !self.output_enabled() {
            return Ok(());
        }
        if raw_mode {
            rawrrr::enable_raw()
        } else {
            rawrrr::disable_raw()
        }
        Ok(())
    }
    #[cfg(feature = "raw_mode")]
    fn get_raw_mode(&self) -> Result<bool, String> {
        Ok(rawrrr::is_raw())
    }
    fn var(&self, name: &str) -> Option<String> {
        env::var(name).ok()
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
    fn open_file(&self, path: &Path, write: bool) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let file = OpenOptions::new()
            .read(true)
            .write(write)
            .open(path)
            .map_err(|e| format!("{e} {}", path.display()))?;
        NATIVE_SYS.files.insert(handle, BufReader::new(file));
        Ok(handle)
    }
    fn file_read_all(&self, path: &Path) -> Result<Vec<u8>, String> {
        let handle = self.open_file(path, false)?;
        let bytes = self.read_all(handle)?;
        self.close(handle)?;
        Ok(bytes)
    }
    fn create_file(&self, path: &Path) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let file = File::create(path).map_err(|e| e.to_string())?;
        NATIVE_SYS.files.insert(handle, BufReader::new(file));
        Ok(handle)
    }
    fn make_dir(&self, path: &Path) -> Result<(), String> {
        fs::create_dir_all(path).map_err(|e| e.to_string())
    }
    fn delete(&self, path: &str) -> Result<(), String> {
        let path = Path::new(path);
        if !path.exists() {
            return Ok(());
        }
        if path.is_dir() {
            fs::remove_dir_all(path).map_err(|e| e.to_string())
        } else {
            fs::remove_file(path).map_err(|e| e.to_string())
        }
    }
    #[cfg(feature = "trash")]
    fn trash(&self, path: &str) -> Result<(), String> {
        trash::delete(path).map_err(|e| e.to_string())
    }
    fn read(&self, handle: Handle, len: usize) -> Result<Vec<u8>, String> {
        Ok(match NATIVE_SYS.get_stream(handle)? {
            SysStream::File(mut file) => {
                let mut buf = vec![0; len];
                let n = file.read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
            SysStream::ChildStdin(_) => return Err("Cannot read from child stdin".into()),
            SysStream::ChildStdout(mut child) => {
                let mut buf = vec![0; len];
                let n = child.stream.read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
            SysStream::ChildStderr(mut child) => {
                let mut buf = vec![0; len];
                let n = child.stream.read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
            SysStream::TcpSocket(socket) => {
                let mut buf = vec![0; len];
                let n = (&mut &*socket).read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
            SysStream::TlsSocket(socket) => {
                let mut buf = vec![0; len];
                let n = (&mut &*socket).read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
        })
    }
    fn read_all(&self, handle: Handle) -> Result<Vec<u8>, String> {
        Ok(match NATIVE_SYS.get_stream(handle)? {
            SysStream::File(mut file) => {
                let mut buf = Vec::new();
                file.read_to_end(&mut buf).map_err(|e| e.to_string())?;
                buf
            }
            SysStream::ChildStdin(_) => return Err("Cannot read from child stdin".into()),
            SysStream::ChildStdout(mut child) => {
                let mut buf = Vec::new();
                child
                    .stream
                    .read_to_end(&mut buf)
                    .map_err(|e| e.to_string())?;
                buf
            }
            SysStream::ChildStderr(mut child) => {
                let mut buf = Vec::new();
                child
                    .stream
                    .read_to_end(&mut buf)
                    .map_err(|e| e.to_string())?;
                buf
            }
            SysStream::TcpSocket(socket) => {
                let mut buf = Vec::new();
                ((&mut &*socket).read_to_end(&mut buf)).map_err(|e| e.to_string())?;
                buf
            }
            SysStream::TlsSocket(socket) => {
                let mut buf = Vec::new();
                ((&mut &*socket).read_to_end(&mut buf)).map_err(|e| e.to_string())?;
                buf
            }
        })
    }
    fn read_lines<'a>(&self, handle: Handle) -> Result<ReadLinesReturnFn<'a>, String> {
        Ok(Box::new(move |env: &mut Uiua, mut f: ReadLinesFn| {
            match NATIVE_SYS.get_stream(handle).map_err(|e| env.error(e))? {
                SysStream::File(mut file) => {
                    for line in (&mut *file).lines() {
                        let line =
                            line.map_err(|e| env.error(format!("Error reading line: {e}")))?;
                        f(line, env)?;
                    }
                }
                SysStream::ChildStdin(_) => return Err(env.error("Cannot read from child stdin")),
                SysStream::ChildStdout(mut child) => {
                    for line in (&mut child.stream).lines() {
                        let line =
                            line.map_err(|e| env.error(format!("Error reading line: {e}")))?;
                        f(line, env)?;
                    }
                }
                SysStream::ChildStderr(mut child) => {
                    for line in (&mut child.stream).lines() {
                        let line =
                            line.map_err(|e| env.error(format!("Error reading line: {e}")))?;
                        f(line, env)?;
                    }
                }
                SysStream::TcpSocket(socket) => {
                    for line in BufReader::new(&*socket).lines() {
                        let line =
                            line.map_err(|e| env.error(format!("Error reading line: {e}")))?;
                        f(line, env)?;
                    }
                }
                SysStream::TlsSocket(socket) => {
                    for line in &mut BufReader::new(&socket.stream).lines() {
                        let line =
                            line.map_err(|e| env.error(format!("Error reading line: {e}")))?;
                        f(line, env)?;
                    }
                }
            }
            Ok(())
        }))
    }
    fn write(&self, handle: Handle, conts: &[u8]) -> Result<(), String> {
        let mut conts = conts;
        let colored;
        if let Some(colored_error) = NATIVE_SYS
            .colored_errors
            .get(String::from_utf8_lossy(conts).as_ref())
            .as_deref()
            .cloned()
        {
            colored = colored_error;
            conts = colored.as_bytes();
        }
        match NATIVE_SYS.get_stream(handle)? {
            SysStream::File(mut file) => file.get_mut().write_all(conts).map_err(|e| e.to_string()),
            SysStream::ChildStdin(mut child) => {
                child.stream.write_all(conts).map_err(|e| e.to_string())
            }
            SysStream::ChildStdout(_) => Err("Cannot write to child stdout".into()),
            SysStream::ChildStderr(_) => Err("Cannot write to child stderr".into()),
            SysStream::TcpSocket(socket) => {
                (&mut &*socket).write_all(conts).map_err(|e| e.to_string())
            }
            SysStream::TlsSocket(socket) => {
                (&mut &*socket).write_all(conts).map_err(|e| e.to_string())
            }
        }
    }
    fn seek(&self, handle: Handle, offset: super::StreamSeek) -> Result<(), String> {
        use std::io::Seek;

        match NATIVE_SYS.get_stream(handle)? {
            SysStream::File(mut file) => file
                .seek(offset.into())
                .map_err(|e| e.to_string())
                .map(|_| ()),
            _ => Err("Cannot seek this stream".into()),
        }
    }
    #[cfg(feature = "clipboard")]
    fn clipboard(&self) -> Result<String, String> {
        use arboard::*;
        match Clipboard::new() {
            Ok(mut provider) => provider.get_text().map_err(|e| e.to_string()),
            Err(e) => Err(format!("Failed to get clipboard provider: {e}")),
        }
    }
    #[cfg(feature = "clipboard")]
    fn set_clipboard(&self, contents: &str) -> Result<(), String> {
        use arboard::*;
        match Clipboard::new() {
            Ok(mut provider) => provider.set_text(contents).map_err(|e| e.to_string()),
            Err(e) => Err(format!("Failed to get clipboard provider: {e}")),
        }
    }
    fn sleep(&self, seconds: f64) -> Result<(), String> {
        sleep(Duration::from_secs_f64(seconds));
        Ok(())
    }
    fn allow_thread_spawning(&self) -> bool {
        true
    }
    #[allow(clippy::print_stdout)]
    #[cfg(all(feature = "terminal_image", feature = "image"))]
    fn show_image(&self, image: image::DynamicImage, _label: Option<&str>) -> Result<(), String> {
        let (_width, _height) = if let Some((w, h)) = terminal_size() {
            let (tw, th) = (w as u32, h.saturating_sub(1) as u32);
            let (iw, ih) = (image.width(), (image.height() / 2).max(1));
            let scaled_to_height = (iw * th / ih.max(1), th);
            let scaled_to_width = (tw, ih * tw / iw.max(1));
            let (w, h) = if scaled_to_height.0 <= tw {
                scaled_to_height
            } else {
                scaled_to_width
            };
            (Some(w), Some(h))
        } else {
            (None, None)
        };
        if std::env::var("TERM")
            .unwrap_or("".to_owned())
            .contains("sixel")
            || std::env::var("UIUA_ENABLE_SIXEL").is_ok_and(|s| s == "1")
        {
            let img_rgba8 = image.to_rgba8();
            let sixel = icy_sixel::sixel_string(
                image.to_rgba8().as_raw(),
                img_rgba8.width() as i32,
                img_rgba8.height() as i32,
                icy_sixel::PixelFormat::RGBA8888,
                icy_sixel::DiffusionMethod::Stucki,
                icy_sixel::MethodForLargest::Auto,
                icy_sixel::MethodForRep::Auto,
                icy_sixel::Quality::HIGH,
            );
            let s = sixel.map_err(|e| e.to_string())?;
            print!("{s}");
            Ok(())
        } else {
            #[cfg(feature = "window")]
            if crate::window::use_window() {
                return crate::window::Request::Show(crate::media::SmartOutput::Png(
                    crate::media::image_to_bytes(&image, image::ImageFormat::Png)
                        .map_err(|e| e.to_string())?,
                    _label.map(Into::into),
                ))
                .send();
            }
            viuer::print(
                &image,
                &viuer::Config {
                    width: _width,
                    height: _height,
                    absolute_offset: false,
                    transparent: true,
                    ..Default::default()
                },
            )
            .map(drop)
            .map_err(|e| format!("Failed to show image: {e}"))
        }
    }
    #[cfg(all(feature = "gif", feature = "invoke"))]
    fn show_gif(&self, gif_bytes: Vec<u8>, _label: Option<&str>) -> Result<(), String> {
        #[cfg(feature = "window")]
        if crate::window::use_window() {
            return crate::window::Request::Show(crate::media::SmartOutput::Gif(
                gif_bytes,
                _label.map(Into::into),
            ))
            .send();
        }
        (move || -> std::io::Result<()> {
            let temp_path = std::env::temp_dir().join("show.gif");
            fs::write(&temp_path, gif_bytes)?;
            let commands = open::commands(&temp_path);
            if let Some(mut command) = commands.into_iter().next() {
                if let Some(mut child) = NATIVE_SYS
                    .gifs_child
                    .lock()
                    .replace(command.arg(&temp_path).spawn()?)
                {
                    child.kill()?;
                }
            }
            Ok(())
        })()
        .map_err(|e| e.to_string())
    }
    #[cfg(all(feature = "apng", feature = "invoke"))]
    fn show_apng(&self, apng_bytes: Vec<u8>, _label: Option<&str>) -> Result<(), String> {
        // #[cfg(feature = "window")]
        // if crate::window::use_window() {
        //     return crate::window::Request::Show(crate::media::SmartOutput::Apng(
        //         apng_bytes,
        //         _label.map(Into::into),
        //     ))
        //     .send();
        // }
        (move || -> std::io::Result<()> {
            let temp_path = std::env::temp_dir().join("show.apng");
            fs::write(&temp_path, apng_bytes)?;
            let commands = open::commands(&temp_path);
            if let Some(mut command) = commands.into_iter().next() {
                if let Some(mut child) = NATIVE_SYS
                    .gifs_child
                    .lock()
                    .replace(command.arg(&temp_path).spawn()?)
                {
                    child.kill()?;
                }
            }
            Ok(())
        })()
        .map_err(|e| e.to_string())
    }
    #[cfg(feature = "audio")]
    fn play_audio(&self, wav_bytes: Vec<u8>, _label: Option<&str>) -> Result<(), String> {
        use hodaun::*;
        #[cfg(feature = "window")]
        if crate::window::use_window() {
            return crate::window::Request::Show(crate::media::SmartOutput::Wav(
                wav_bytes,
                _label.map(Into::into),
            ))
            .send();
        }
        match default_output::<Stereo>() {
            Ok(mut mixer) => {
                match wav::WavSource::new(std::collections::VecDeque::from(wav_bytes)) {
                    Ok(source) => {
                        mixer.add(source.resample());
                        mixer.block();
                        Ok(())
                    }
                    Err(e) => Err(format!("Failed to read wav bytes: {e}")),
                }
            }
            Err(e) => Err(format!("Failed to initialize audio output stream: {e}").to_string()),
        }
    }
    #[cfg(feature = "audio")]
    fn audio_sample_rate(&self) -> u32 {
        hodaun::default_output_device()
            .and_then(|device| {
                hodaun::cpal::traits::DeviceTrait::default_output_config(&device).ok()
            })
            .map(|config| config.sample_rate().0)
            .unwrap_or(44100)
    }
    #[cfg(feature = "audio")]
    fn stream_audio(&self, f: crate::AudioStreamFn) -> Result<(), String> {
        use hodaun::*;
        struct TheSource {
            time: f64,
            samples: std::vec::IntoIter<[f64; 2]>,
            f: crate::AudioStreamFn,
        }
        impl Source for TheSource {
            type Frame = Stereo;
            fn next(&mut self, sample_rate: f64) -> Option<Self::Frame> {
                if let Some([left, right]) = self.samples.next() {
                    return Some(Stereo { left, right });
                }
                const LEN: usize = 10000;
                let mut times = Vec::with_capacity(LEN);
                for _ in 0..LEN {
                    times.push(self.time);
                    self.time += 1.0 / sample_rate;
                }
                if let Some(socket) = NATIVE_SYS.audio_time_socket.lock().as_ref() {
                    if let Err(e) = socket.send(&self.time.to_be_bytes()) {
                        eprintln!("Failed to send audio time: {e}");
                    }
                }
                match (self.f)(&times) {
                    Ok(samples) => {
                        self.samples = samples.into_iter();
                        self.next(sample_rate)
                    }
                    Err(e) => {
                        eprintln!("{e}");
                        None
                    }
                }
            }
        }
        let source = TheSource {
            time: NATIVE_SYS.audio_stream_time.lock().unwrap_or(0.0),
            samples: Vec::new().into_iter(),
            f,
        };
        match default_output::<Stereo>() {
            Ok(mut mixer) => {
                mixer.add(source);
                mixer.block();
                Ok(())
            }
            Err(e) => Err(format!("Failed to initialize audio output stream: {e}").to_string()),
        }
    }
    fn tcp_listen(&self, addr: &str) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let listener = TcpListener::bind(addr).map_err(|e| e.to_string())?;
        NATIVE_SYS.tcp_listeners.insert(handle, listener);
        Ok(handle)
    }
    #[cfg(feature = "tls")]
    fn tls_listen(&self, addr: &str, cert: &[u8], key: &[u8]) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let listener = TcpListener::bind(addr).map_err(|e| e.to_string())?;
        let certs = rustls_pemfile::certs(&mut BufReader::new(cert))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| e.to_string())?;
        let private_key = rustls_pemfile::private_key(&mut BufReader::new(key))
            .map_err(|e| e.to_string())?
            .ok_or("No private key found")?;
        let config = rustls::ServerConfig::builder()
            .with_no_client_auth()
            .with_single_cert(certs, private_key)
            .map_err(|e| e.to_string())?
            .into();
        let listener = TlsListener { listener, config };
        NATIVE_SYS.tls_listeners.insert(handle, listener);
        Ok(handle)
    }
    fn tcp_accept(&self, handle: Handle) -> Result<Handle, String> {
        if let Some(listener) = NATIVE_SYS.tcp_listeners.get_mut(&handle) {
            let (stream, _) = listener.accept().map_err(|e| e.to_string())?;
            drop(listener);
            let handle = NATIVE_SYS.new_handle();
            NATIVE_SYS.tcp_sockets.insert(handle, stream);
            Ok(handle)
        } else {
            #[cfg(feature = "tls")]
            {
                if let Some(listener) = NATIVE_SYS.tls_listeners.get_mut(&handle) {
                    let (mut stream, _) = listener.listener.accept().map_err(|e| e.to_string())?;
                    let mut conn = rustls::ServerConnection::new(listener.config.clone())
                        .map_err(|e| e.to_string())?;
                    let _ = conn.complete_io(&mut stream).map_err(|e| e.to_string())?;
                    let handle = NATIVE_SYS.new_handle();
                    NATIVE_SYS.tls_sockets.insert(
                        handle,
                        TlsSocket {
                            stream,
                            #[cfg(feature = "tls")]
                            conn: parking_lot::Mutex::new(TslConnection::Server(conn)),
                        },
                    );
                    return Ok(handle);
                }
            }
            #[allow(unreachable_code)]
            Err("Invalid tcp listener handle".to_string())
        }
    }
    fn tcp_connect(&self, addr: &str) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let stream = TcpStream::connect(addr).map_err(|e| e.to_string())?;
        NATIVE_SYS.tcp_sockets.insert(handle, stream);
        NATIVE_SYS.hostnames.insert(
            handle,
            (addr.split_once(':').ok_or("No colon in address")?.0).to_string(),
        );
        Ok(handle)
    }
    #[cfg(feature = "tls")]
    fn tls_connect(&self, addr: &str) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let root_store =
            rustls::RootCertStore::from_iter(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());
        let config = rustls::ClientConfig::builder()
            .with_root_certificates(root_store)
            .with_no_client_auth();
        let stream = TcpStream::connect(addr).map_err(|e| e.to_string())?;
        let server_name = if let Some(name) =
            (addr.split(':').next()).and_then(|addr| addr.to_string().try_into().ok())
        {
            name
        } else {
            rustls::pki_types::ServerName::IpAddress(
                stream.peer_addr().map_err(|e| e.to_string())?.ip().into(),
            )
        };
        let client = rustls::ClientConnection::new(config.clone().into(), server_name.clone())
            .map_err(|e| e.to_string())?;
        NATIVE_SYS.tls_sockets.insert(
            handle,
            TlsSocket {
                stream,
                #[cfg(feature = "tls")]
                conn: parking_lot::Mutex::new(TslConnection::Client(client)),
            },
        );
        Ok(handle)
    }
    fn tcp_addr(&self, handle: Handle) -> Result<SocketAddr, String> {
        (NATIVE_SYS.get_tcp_stream(handle, |s| s.peer_addr()))
            .or_else(|| NATIVE_SYS.get_tcp_listener(handle, |l| l.local_addr()))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())
            .and_then(|r| r.map_err(|e| e.to_string()))
    }
    fn tcp_set_non_blocking(&self, handle: Handle, non_blocking: bool) -> Result<(), String> {
        NATIVE_SYS
            .get_tcp_stream(handle, |s| s.set_nonblocking(non_blocking))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn tcp_set_read_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        NATIVE_SYS
            .get_tcp_stream(handle, |s| s.set_read_timeout(timeout))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn tcp_set_write_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        NATIVE_SYS
            .get_tcp_stream(handle, |s| s.set_write_timeout(timeout))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn udp_bind(&self, addr: &str) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let socket = std::net::UdpSocket::bind(addr).map_err(|e| e.to_string())?;
        let socket = UdpSocket {
            max_msg_len: 256,
            socket,
        };
        NATIVE_SYS.udp_sockets.insert(handle, socket);
        NATIVE_SYS.hostnames.insert(
            handle,
            (addr.split_once(':').ok_or("No colon in address")?.0).to_string(),
        );
        Ok(handle)
    }
    fn udp_recv(&self, handle: Handle) -> Result<(Vec<u8>, SocketAddr), String> {
        NATIVE_SYS
            .get_udp_socket(handle, |sock| {
                let mut buf: Vec<u8> = vec![0; sock.max_msg_len];
                sock.socket.recv_from(&mut buf).map(|(n, addr)| {
                    buf.truncate(n);
                    (buf, addr)
                })
            })
            .ok_or_else(|| "Invalid UDP socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn udp_send(&self, handle: Handle, bytes: Vec<u8>, addr: &str) -> Result<(), String> {
        NATIVE_SYS
            .get_udp_socket(handle, |sock| {
                sock.socket.send_to(bytes.as_slice(), addr).map(|_| ())
            })
            .ok_or_else(|| "Invalid UDP socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn udp_set_max_msg_length(&self, handle: Handle, max_len: usize) -> Result<(), String> {
        NATIVE_SYS
            .get_udp_socket_mut(handle, |sock| {
                sock.max_msg_len = max_len;
            })
            .ok_or_else(|| "Invalid UDP socket handle".to_string())
    }
    fn close(&self, handle: Handle) -> Result<(), String> {
        if NATIVE_SYS.child_stdins.remove(&handle).is_some()
            | NATIVE_SYS.child_stdouts.remove(&handle).is_some()
            | NATIVE_SYS.child_stderrs.remove(&handle).is_some()
        {
            Ok(())
        } else if let Some((_, mut file)) = NATIVE_SYS.files.remove(&handle) {
            file.get_mut().flush().map_err(|e| e.to_string())
        } else if let Some((_, socket)) = NATIVE_SYS.tcp_sockets.remove(&handle) {
            NATIVE_SYS.hostnames.remove(&handle);
            (&mut &socket).flush().map_err(|e| e.to_string())
        } else if let Some((_, socket)) = NATIVE_SYS.tls_sockets.remove(&handle) {
            NATIVE_SYS.hostnames.remove(&handle);
            (&mut &socket).flush().map_err(|e| e.to_string())
        } else if NATIVE_SYS.tcp_listeners.remove(&handle).is_some()
            || NATIVE_SYS.tls_listeners.remove(&handle).is_some()
            || NATIVE_SYS.udp_sockets.remove(&handle).is_some()
        {
            NATIVE_SYS.hostnames.remove(&handle);
            Ok(())
        } else {
            Err("Invalid stream handle".to_string())
        }
    }
    #[cfg(feature = "invoke")]
    fn invoke(&self, path: &str) -> Result<(), String> {
        open::that(path).map_err(|e| e.to_string())
    }
    fn run_command_inherit(&self, command: &str, args: &[&str]) -> Result<i32, String> {
        let status = Command::new(command)
            .args(args)
            .spawn()
            .map_err(|e| e.to_string())?
            .wait()
            .map_err(|e| e.to_string())?;
        Ok(status.code().unwrap_or(0))
    }
    fn run_command_capture(
        &self,
        command: &str,
        args: &[&str],
    ) -> Result<(i32, String, String), String> {
        let output = Command::new(command)
            .args(args)
            .output()
            .map_err(|e| e.to_string())?;
        Ok((
            output.status.code().unwrap_or(0),
            String::from_utf8_lossy(&output.stdout).into(),
            String::from_utf8_lossy(&output.stderr).into(),
        ))
    }
    fn run_command_stream(&self, command: &str, args: &[&str]) -> Result<[Handle; 3], String> {
        let mut child = Command::new(command)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| e.to_string())?;
        let stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        let stderr = child.stderr.take().unwrap();
        let child = Arc::new(child);
        let stdin_handle = NATIVE_SYS.new_handle();
        NATIVE_SYS.child_stdins.insert(
            stdin_handle,
            ChildStream {
                stream: stdin,
                child: child.clone(),
            },
        );
        let stdout_handle = NATIVE_SYS.new_handle();
        NATIVE_SYS.child_stdouts.insert(
            stdout_handle,
            ChildStream {
                stream: BufReader::new(stdout),
                child: child.clone(),
            },
        );
        let stderr_handle = NATIVE_SYS.new_handle();
        NATIVE_SYS.child_stderrs.insert(
            stderr_handle,
            ChildStream {
                stream: BufReader::new(stderr),
                child,
            },
        );
        Ok([stdin_handle, stdout_handle, stderr_handle])
    }
    fn change_directory(&self, path: &str) -> Result<(), String> {
        env::set_current_dir(path).map_err(|e| e.to_string())
    }
    fn get_current_directory(&self) -> Result<String, String> {
        env::current_dir().map_err(|e| e.to_string()).and_then(|p| {
            p.into_os_string()
                .into_string()
                .map_err(|_| "Path was not valid unicode".into())
        })
    }
    #[cfg(feature = "webcam")]
    fn webcam_capture(&self, index: usize) -> Result<crate::WebcamImage, String> {
        let cam_channels = &NATIVE_SYS.cam_channels;
        if !cam_channels.contains_key(&index) {
            let ch = WebcamChannel::new(index)?;
            cam_channels.insert(index, ch);
        }
        let ch = cam_channels.get_mut(&index).unwrap();
        if ch.send.send(()).is_ok() {
            if let Ok(res) = ch.recv.recv() {
                res
            } else {
                Err("Failed to interact with webcam".into())
            }
        } else {
            let ch = WebcamChannel::new(index)?;
            cam_channels.insert(index, ch);
            let ch = cam_channels.get_mut(&index).unwrap();
            if ch.send.send(()).is_ok() {
                if let Ok(res) = ch.recv.recv() {
                    res
                } else {
                    Err("Failed to interact with webcam".into())
                }
            } else {
                Err("Failed to interact with webcam".into())
            }
        }
    }
    #[cfg(feature = "ffi")]
    fn ffi(
        &self,
        file: &str,
        return_ty: crate::FfiType,
        name: &str,
        arg_tys: &[crate::FfiArg],
        arg_values: Vec<crate::Value>,
    ) -> Result<crate::Value, String> {
        NATIVE_SYS
            .ffi
            .do_ffi(file, return_ty, name, arg_tys, arg_values)
    }
    #[cfg(feature = "ffi")]
    fn mem_copy(&self, ptr: MetaPtr, len: usize) -> Result<Value, String> {
        crate::ffi_copy(ptr, len)
    }
    #[cfg(feature = "ffi")]
    fn mem_set(&self, ptr: MetaPtr, idx: usize, value: Value) -> Result<(), String> {
        crate::ffi_set(ptr, idx, value)
    }
    #[cfg(feature = "ffi")]
    fn mem_free(&self, ptr: &MetaPtr) -> Result<(), String> {
        NATIVE_SYS.ffi.ffi_free(ptr);
        Ok(())
    }
    fn load_git_module(&self, url: &str, target: GitTarget) -> Result<PathBuf, String> {
        if let Some(path) = NATIVE_SYS.git_paths.get(url) {
            if path.is_err() || path.as_ref().unwrap().exists() {
                return path.clone();
            }
        }
        let mut parts = url.rsplitn(3, '/');
        let repo_name = parts.next().ok_or("Invalid git url")?;
        let repo_owner = parts.next().ok_or("Invalid git url")?;
        if parts.next().is_none_or(|s| s.is_empty()) {
            return Err("Invalid git url".to_string());
        }

        let mut changed_dir = false;
        // Add submodule
        let res = (|| {
            let parent_path = Path::new("uiua-modules").join(repo_owner);
            let submodule_path = parent_path.join(repo_name);
            let lib_path = submodule_path.join("lib.ua");
            if !submodule_path.exists() {
                // Ensure the repo exists
                let repo_exists = Command::new("git")
                    .args(["ls-remote", "--exit-code", url])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status()
                    .map_err(|e| e.to_string())?
                    .success();
                if !repo_exists {
                    return Err(format!("A git repository does not exist at {url}"));
                }
                // Create the parent directory if it doesn't exist
                if !parent_path.exists() {
                    fs::create_dir_all(&parent_path).map_err(|e| e.to_string())?;
                }
                // Clone the repo
                let mut child = Command::new("git")
                    .args(["clone", url, &submodule_path.to_string_lossy()])
                    .stderr(Stdio::piped())
                    .stdout(Stdio::null())
                    .spawn()
                    .map_err(|e| e.to_string())?;
                let status = child.wait().map_err(|e| e.to_string())?;
                if !status.success() {
                    let stderr = child.stderr.as_mut().unwrap();
                    let mut err = String::new();
                    stderr.read_to_string(&mut err).map_err(|e| e.to_string())?;
                    return Err(format!("Failed to clone git repository `{url}`: {err}"));
                }

                set_current_dir(&*submodule_path).map_err(|e| e.to_string())?;
                changed_dir = true;
                match &target {
                    GitTarget::Default => {}
                    GitTarget::Branch(branch) => {
                        let mut child = Command::new("git")
                            .args(["checkout", branch])
                            .stderr(Stdio::piped())
                            .stdout(Stdio::null())
                            .spawn()
                            .map_err(|e| e.to_string())?;
                        let status = child.wait().map_err(|e| e.to_string())?;
                        if !status.success() {
                            let stderr = child.stderr.as_mut().unwrap();
                            let mut err = String::new();
                            stderr.read_to_string(&mut err).map_err(|e| e.to_string())?;
                            return Err(format!("Failed to checkout branch `{branch}`: {err}"));
                        }
                    }
                    GitTarget::Commit(hash) => {
                        let mut child = Command::new("git")
                            .args(["checkout", hash])
                            .stderr(Stdio::piped())
                            .stdout(Stdio::null())
                            .spawn()
                            .map_err(|e| e.to_string())?;
                        let status = child.wait().map_err(|e| e.to_string())?;
                        if !status.success() {
                            let stderr = child.stderr.as_mut().unwrap();
                            let mut err = String::new();
                            stderr.read_to_string(&mut err).map_err(|e| e.to_string())?;
                            return Err(format!("Failed to checkout commit `{hash}`: {err}"));
                        }
                    }
                }
            }
            Ok(lib_path)
        })();
        if changed_dir {
            _ = set_current_dir("../../..");
        }
        NATIVE_SYS.git_paths.insert(url.to_string(), res.clone());
        res
    }
    #[allow(clippy::print_stdout)]
    fn breakpoint(&self, env: &Uiua) -> Result<bool, String> {
        if !self.output_enabled() {
            return Ok(true);
        }
        match env.span() {
            Span::Code(span) => eprintln!(
                "{} at {span} {}",
                "&b".truecolor(237, 94, 106),
                "(press enter to continue)".bright_black()
            ),
            Span::Builtin => {}
        }
        eprintln!();
        print_stack(env.stack(), true);
        eprintln!();
        _ = stdin().read_line(&mut String::new());
        Ok(true)
    }
}

#[allow(clippy::print_stdout)]
#[doc(hidden)]
pub fn print_stack(stack: &[Value], color: bool) {
    #[cfg(feature = "window")]
    if crate::window::use_window() {
        use crate::{media::SmartOutput, window::Request};
        _ = Request::Separator.send();
        _ = Request::ShowAll(
            (stack.iter())
                .map(|v| SmartOutput::from_value(v.clone(), 24.0, &NativeSys))
                .collect(),
        )
        .send();
        _ = Request::ClearBeforeNext.send();
        eprintln!(
            "{} value{} displayed in window",
            stack.len(),
            if stack.len() == 1 { "" } else { "s" }
        );
        return;
    }
    if stack.is_empty() {
        return;
    }
    if stack.len() == 1 || !color {
        for value in stack {
            eprintln!("{}", value.show());
        }
        return;
    }
    #[cfg(feature = "terminal-light")]
    let is_light = terminal_light::luma().is_ok_and(|luma| luma > 0.6);
    #[cfg(not(feature = "terminal-light"))]
    let is_light = false;
    for (i, value) in stack.iter().enumerate() {
        let (w, b) = if is_light { (0, 35) } else { (255, 200) };
        let (r, g, b) = match (i + 3) % 6 {
            0 => (w, b, b),
            1 => (w, w, b),
            2 => (b, w, b),
            3 => (b, w, w),
            4 => (b, b, w),
            5 => (w, b, w),
            _ => unreachable!(),
        };
        eprintln!("{}", value.show().truecolor(r, g, b));
    }
}
