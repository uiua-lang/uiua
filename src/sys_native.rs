use std::{
    any::Any,
    env,
    fs::{self, File, OpenOptions},
    io::{stderr, stdin, stdout, Read, Write},
    net::*,
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    slice,
    sync::atomic::{self, AtomicBool, AtomicU64},
    thread::sleep,
    time::Duration,
};

use crate::{Handle, SysBackend};
use bufreaderwriter::seq::BufReaderWriterSeq;
use dashmap::DashMap;
use once_cell::sync::Lazy;

/// The defualt native system backend
#[derive(Default)]
pub struct NativeSys;

type Buffered<T> = BufReaderWriterSeq<T>;

struct GlobalNativeSys {
    output_enabled: AtomicBool,
    next_handle: AtomicU64,
    files: DashMap<Handle, Buffered<File>>,
    child_procs: DashMap<Handle, Child>,
    tcp_listeners: DashMap<Handle, TcpListener>,
    tcp_sockets: DashMap<Handle, Buffered<TcpStream>>,
    tls_sockets: DashMap<Handle, TlsSocket>,
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
    File(dashmap::mapref::one::RefMut<'a, Handle, Buffered<File>>),
    Child(dashmap::mapref::one::RefMut<'a, Handle, Child>),
    TcpSocket(dashmap::mapref::one::RefMut<'a, Handle, Buffered<TcpStream>>),
    TlsSocket(dashmap::mapref::one::RefMut<'a, Handle, TlsSocket>),
}

struct TlsSocket {
    socket: Buffered<TcpStream>,
    #[cfg(feature = "https")]
    client: rustls::ClientConnection,
}

impl Read for TlsSocket {
    fn read(&mut self, _buf: &mut [u8]) -> std::io::Result<usize> {
        #[cfg(feature = "https")]
        {
            rustls::Stream::new(&mut self.client, &mut self.socket).read(_buf)
        }
        #[cfg(not(feature = "https"))]
        Ok(0)
    }
}

impl Write for TlsSocket {
    fn write(&mut self, _buf: &[u8]) -> std::io::Result<usize> {
        #[cfg(feature = "https")]
        {
            rustls::Stream::new(&mut self.client, &mut self.socket).write(_buf)
        }
        #[cfg(not(feature = "https"))]
        Ok(0)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.socket.flush()
    }
}

impl Default for GlobalNativeSys {
    fn default() -> Self {
        Self {
            output_enabled: AtomicBool::new(true),
            next_handle: Handle::FIRST_UNRESERVED.0.into(),
            files: DashMap::new(),
            child_procs: DashMap::new(),
            tcp_listeners: DashMap::new(),
            tcp_sockets: DashMap::new(),
            tls_sockets: DashMap::new(),
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
                && !self.child_procs.contains_key(&handle)
                && !self.tcp_listeners.contains_key(&handle)
                && !self.tcp_sockets.contains_key(&handle)
                && !self.tls_sockets.contains_key(&handle)
            {
                return handle;
            }
        }
        panic!("Ran out of file handles");
    }
    fn get_stream(&self, handle: Handle) -> Result<SysStream, String> {
        Ok(if let Some(file) = self.files.get_mut(&handle) {
            SysStream::File(file)
        } else if let Some(child) = self.child_procs.get_mut(&handle) {
            SysStream::Child(child)
        } else if let Some(socket) = self.tcp_sockets.get_mut(&handle) {
            SysStream::TcpSocket(socket)
        } else if let Some(tls_socket) = self.tls_sockets.get_mut(&handle) {
            SysStream::TlsSocket(tls_socket)
        } else {
            return Err("Invalid file handle".to_string());
        })
    }
    fn get_tcp_stream<T>(
        &self,
        handle: Handle,
        f: impl FnOnce(&mut Buffered<TcpStream>) -> T,
    ) -> Option<T> {
        if let Some(mut sock) = self.tcp_sockets.get_mut(&handle) {
            Some(f(&mut sock))
        } else if let Some(mut sock) = self.tls_sockets.get_mut(&handle) {
            Some(f(&mut sock.socket))
        } else {
            None
        }
    }
}

static NATIVE_SYS: Lazy<GlobalNativeSys> = Lazy::new(Default::default);

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

pub(crate) fn set_output_enabled(enabled: bool) -> bool {
    NATIVE_SYS
        .output_enabled
        .swap(enabled, atomic::Ordering::Relaxed)
}

impl SysBackend for NativeSys {
    fn any(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        if !NATIVE_SYS.output_enabled.load(atomic::Ordering::Relaxed) {
            return Ok(());
        }
        let mut stdout = stdout().lock();
        stdout.write_all(s.as_bytes()).map_err(|e| e.to_string())?;
        stdout.flush().map_err(|e| e.to_string())
    }
    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        let mut stderr = stderr().lock();
        stderr.write_all(s.as_bytes()).map_err(|e| e.to_string())?;
        stderr.flush().map_err(|e| e.to_string())
    }
    fn print_str_trace(&self, s: &str) {
        if !NATIVE_SYS.output_enabled.load(atomic::Ordering::Relaxed) {
            return;
        }
        eprint!("{s}");
        _ = stderr().flush();
    }
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        if !NATIVE_SYS.output_enabled.load(atomic::Ordering::Relaxed) {
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
    fn scan_stdin(&self, count: usize) -> Result<Vec<u8>, String> {
        if !NATIVE_SYS.output_enabled.load(atomic::Ordering::Relaxed) {
            return Ok(Vec::new());
        }
        let mut buffer = vec![0; count];
        stdin().read_exact(&mut buffer).map_err(|e| e.to_string())?;
        Ok(buffer)
    }
    fn save_error_color(&self, message: String, colored: String) {
        NATIVE_SYS.colored_errors.insert(message, colored);
    }
    fn term_size(&self) -> Result<(usize, usize), String> {
        let (w, h) = term_size::dimensions().ok_or("Failed to get terminal size")?;
        Ok((w, h.saturating_sub(1)))
    }
    #[cfg(feature = "raw_mode")]
    fn set_raw_mode(&self, raw_mode: bool) -> Result<(), String> {
        if !NATIVE_SYS.output_enabled.load(atomic::Ordering::Relaxed) {
            return Ok(());
        }
        if raw_mode {
            rawrrr::enable_raw()
        } else {
            rawrrr::disable_raw()
        }
        Ok(())
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
    fn open_file(&self, path: &Path) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(path)
            .map_err(|e| format!("{e} {}", path.display()))?;
        NATIVE_SYS.files.insert(handle, Buffered::new_reader(file));
        Ok(handle)
    }
    fn file_read_all(&self, path: &Path) -> Result<Vec<u8>, String> {
        let handle = self.open_file(path)?;
        let bytes = self.read_all(handle)?;
        self.close(handle)?;
        Ok(bytes)
    }
    fn create_file(&self, path: &Path) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let file = File::create(path).map_err(|e| e.to_string())?;
        NATIVE_SYS.files.insert(handle, Buffered::new_writer(file));
        Ok(handle)
    }
    fn delete(&self, path: &str) -> Result<(), String> {
        let path = Path::new(path);
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
                file.flush().map_err(|e| e.to_string())?;
                let mut buf = vec![0; len];
                let n = file.read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
            SysStream::Child(mut child) => {
                let mut buf = vec![0; len];
                let n = (child.stdout.as_mut().unwrap())
                    .read(&mut buf)
                    .map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
            SysStream::TcpSocket(mut socket) => {
                socket.flush().map_err(|e| e.to_string())?;
                let mut buf = vec![0; len];
                let n = socket.read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
            SysStream::TlsSocket(mut socket) => {
                socket.flush().map_err(|e| e.to_string())?;
                let mut buf = vec![0; len];
                let n = socket.read(&mut buf).map_err(|e| e.to_string())?;
                buf.truncate(n);
                buf
            }
        })
    }
    fn read_all(&self, handle: Handle) -> Result<Vec<u8>, String> {
        Ok(match NATIVE_SYS.get_stream(handle)? {
            SysStream::File(mut file) => {
                file.flush().map_err(|e| e.to_string())?;
                let mut buf = Vec::new();
                file.read_to_end(&mut buf).map_err(|e| e.to_string())?;
                buf
            }
            SysStream::Child(mut child) => {
                let mut buf = Vec::new();
                (child.stdout.as_mut().unwrap())
                    .read_to_end(&mut buf)
                    .map_err(|e| e.to_string())?;
                buf
            }
            SysStream::TcpSocket(mut socket) => {
                socket.flush().map_err(|e| e.to_string())?;
                let mut buf = Vec::new();
                socket.read_to_end(&mut buf).map_err(|e| e.to_string())?;
                buf
            }
            SysStream::TlsSocket(mut socket) => {
                socket.flush().map_err(|e| e.to_string())?;
                let mut buf = Vec::new();
                socket.read_to_end(&mut buf).map_err(|e| e.to_string())?;
                buf
            }
        })
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
            SysStream::File(mut file) => file.write_all(conts).map_err(|e| e.to_string()),
            SysStream::Child(mut child) => (child.stdin.as_mut().unwrap())
                .write_all(conts)
                .map_err(|e| e.to_string()),
            SysStream::TcpSocket(mut socket) => socket.write_all(conts).map_err(|e| e.to_string()),
            SysStream::TlsSocket(mut socket) => socket.write_all(conts).map_err(|e| e.to_string()),
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
    #[cfg(all(feature = "terminal_image", feature = "image"))]
    fn show_image(&self, image: image::DynamicImage) -> Result<(), String> {
        let (width, height) = if let Some((w, h)) = term_size::dimensions() {
            let (tw, th) = (w as u32, h.saturating_sub(1) as u32);
            let (iw, ih) = (image.width(), image.height() / 2);
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
    #[cfg(all(feature = "gif", feature = "invoke"))]
    fn show_gif(&self, gif_bytes: Vec<u8>) -> Result<(), String> {
        (move || -> std::io::Result<()> {
            let temp_path = std::env::temp_dir().join("show.gif");
            fs::write("show.gif", gif_bytes)?;
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
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        use hodaun::*;
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
    fn tcp_accept(&self, handle: Handle) -> Result<Handle, String> {
        let listener = NATIVE_SYS
            .tcp_listeners
            .get_mut(&handle)
            .ok_or_else(|| "Invalid tcp listener handle".to_string())?;
        let (stream, _) = listener.accept().map_err(|e| e.to_string())?;
        drop(listener);
        let handle = NATIVE_SYS.new_handle();
        NATIVE_SYS
            .tcp_sockets
            .insert(handle, Buffered::new_reader(stream));
        Ok(handle)
    }
    fn tcp_connect(&self, addr: &str) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let stream = TcpStream::connect(addr).map_err(|e| e.to_string())?;
        NATIVE_SYS
            .tcp_sockets
            .insert(handle, Buffered::new_writer(stream));
        NATIVE_SYS.hostnames.insert(
            handle,
            addr.split_once(':')
                .ok_or("No colon in address")?
                .0
                .to_string(),
        );
        Ok(handle)
    }
    #[cfg(feature = "https")]
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
        let client =
            rustls::ClientConnection::new(config.into(), server_name).map_err(|e| e.to_string())?;
        NATIVE_SYS.tls_sockets.insert(
            handle,
            TlsSocket {
                socket: Buffered::new_writer(stream),
                client,
            },
        );
        Ok(handle)
    }
    fn tcp_addr(&self, handle: Handle) -> Result<SocketAddr, String> {
        (NATIVE_SYS.get_tcp_stream(handle, |s| s.get_ref().peer_addr()))
            .or_else(|| (NATIVE_SYS.tcp_listeners.get(&handle)).map(|l| l.local_addr()))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())
            .and_then(|r| r.map_err(|e| e.to_string()))
    }
    fn tcp_set_non_blocking(&self, handle: Handle, non_blocking: bool) -> Result<(), String> {
        NATIVE_SYS
            .get_tcp_stream(handle, |s| s.get_ref().set_nonblocking(non_blocking))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn tcp_set_read_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        NATIVE_SYS
            .get_tcp_stream(handle, |s| s.get_ref().set_read_timeout(timeout))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn tcp_set_write_timeout(
        &self,
        handle: Handle,
        timeout: Option<Duration>,
    ) -> Result<(), String> {
        NATIVE_SYS
            .get_tcp_stream(handle, |s| s.get_ref().set_write_timeout(timeout))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?
            .map_err(|e| e.to_string())
    }
    fn close(&self, handle: Handle) -> Result<(), String> {
        if let Some((_, mut child)) = NATIVE_SYS.child_procs.remove(&handle) {
            child.kill().map_err(|e| e.to_string())?;
            Ok(())
        } else if let Some((_, mut file)) = NATIVE_SYS.files.remove(&handle) {
            file.flush().map_err(|e| e.to_string())
        } else if let Some((_, mut socket)) = NATIVE_SYS.tcp_sockets.remove(&handle) {
            NATIVE_SYS.hostnames.remove(&handle);
            socket.flush().map_err(|e| e.to_string())
        } else if let Some((_, mut socket)) = NATIVE_SYS.tls_sockets.remove(&handle) {
            NATIVE_SYS.hostnames.remove(&handle);
            socket.flush().map_err(|e| e.to_string())
        } else if NATIVE_SYS.tcp_listeners.remove(&handle).is_some() {
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
    fn run_command_stream(&self, command: &str, args: &[&str]) -> Result<Handle, String> {
        let handle = NATIVE_SYS.new_handle();
        let child = Command::new(command)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(|e| e.to_string())?;
        NATIVE_SYS.child_procs.insert(handle, child);
        Ok(handle)
    }
    fn change_directory(&self, path: &str) -> Result<(), String> {
        env::set_current_dir(path).map_err(|e| e.to_string())
    }
    #[cfg(feature = "https")]
    fn https_get(&self, request: &str, handle: Handle) -> Result<String, String> {
        use std::io;

        let host = (NATIVE_SYS.hostnames.get(&handle))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?
            .clone();
        let request = check_http(request.to_string(), &host)?;

        let mut socket = (NATIVE_SYS.tcp_sockets.get_mut(&handle))
            .ok_or_else(|| "Invalid tcp socket handle".to_string())?;

        let mut buffer = Vec::new();
        if let Ok(443) = socket.get_ref().peer_addr().map(|a| a.port()) {
            static CLIENT_CONFIG: Lazy<std::sync::Arc<rustls::ClientConfig>> = Lazy::new(|| {
                let mut store = rustls::RootCertStore::empty();
                store.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());
                rustls::ClientConfig::builder()
                    .with_root_certificates(store)
                    .with_no_client_auth()
                    .into()
            });

            let server_name =
                rustls::pki_types::ServerName::try_from(host).map_err(|e| e.to_string())?;
            let tcp_stream = socket.get_mut();
            let mut conn = rustls::ClientConnection::new(CLIENT_CONFIG.clone(), server_name)
                .map_err(|e| e.to_string())?;
            let mut tls = rustls::Stream::new(&mut conn, tcp_stream);
            tls.write_all(request.as_bytes())
                .map_err(|e| e.to_string())?;
            match tls.read_to_end(&mut buffer) {
                Ok(_) => {}
                Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {}
                Err(e) => return Err(e.to_string()),
            }
        } else {
            (socket.write_all(request.as_bytes())).map_err(|e| e.to_string())?;
            socket.read_to_end(&mut buffer).map_err(|e| e.to_string())?;
        }

        let s = String::from_utf8(buffer).map_err(|e| {
            "Error converting HTTP Response to utf-8: ".to_string() + &e.to_string()
        })?;

        Ok(s)
    }
    #[cfg(feature = "ffi")]
    fn ffi(
        &self,
        file: &str,
        return_ty: crate::FfiType,
        name: &str,
        arg_tys: &[crate::FfiType],
        arg_values: &[crate::Value],
    ) -> Result<crate::Value, String> {
        NATIVE_SYS
            .ffi
            .do_ffi(file, return_ty, name, arg_tys, arg_values)
    }
    fn load_git_module(&self, url: &str, branch: Option<&str>) -> Result<PathBuf, String> {
        if let Some(path) = NATIVE_SYS.git_paths.get(url) {
            if path.is_err() || path.as_ref().unwrap().exists() {
                return path.clone();
            }
        }
        let mut parts = url.rsplitn(3, '/');
        let repo_name = parts.next().ok_or("Invalid git url")?;
        let repo_owner = parts.next().ok_or("Invalid git url")?;
        if parts.next().map_or(true, |s| s.is_empty()) {
            return Err("Invalid git url".to_string());
        }
        let parent_path = Path::new("uiua-modules").join(repo_owner);
        let path = parent_path.join(repo_name).join("lib.ua");
        // Early return if the module already exists
        if path.exists() {
            return Ok(path);
        }
        // Create the parent directory if it doesn't exist
        if !parent_path.exists() {
            fs::create_dir_all(&parent_path).map_err(|e| e.to_string())?;
        }
        // Make sure this folder is a git repository
        let mut child = Command::new("git")
            .arg("init")
            .stderr(Stdio::piped())
            .stdout(Stdio::null())
            .spawn()
            .map_err(|e| e.to_string())?;
        let status = child.wait().map_err(|e| e.to_string())?;
        if !status.success() {
            let stderr = child.stderr.as_mut().unwrap();
            let mut err = String::new();
            stderr.read_to_string(&mut err).map_err(|e| e.to_string())?;
            return Err(format!("Failed to initialize git repository: {err}"));
        }

        // Add submodule
        let submodule_path = parent_path.join(repo_name);
        let res = (move || {
            if !submodule_path.exists() {
                let submod_path = submodule_path.to_string_lossy();
                let mut args = vec!["submodule", "add", "--force"];
                if let Some(branch) = branch {
                    args.push("-b");
                    args.push(branch);
                }
                args.push(url);
                args.push(&submod_path);
                let mut child = Command::new("git")
                    .args(args)
                    .stderr(Stdio::piped())
                    .stdout(Stdio::null())
                    .spawn()
                    .map_err(|e| e.to_string())?;
                let status = child.wait().map_err(|e| e.to_string())?;
                if !status.success() {
                    let stderr = child.stderr.as_mut().unwrap();
                    let mut err = String::new();
                    stderr.read_to_string(&mut err).map_err(|e| e.to_string())?;
                    return Err(format!("Failed to add submodule: {err}"));
                }
            }
            Ok(path)
        })();
        NATIVE_SYS.git_paths.insert(url.to_string(), res.clone());
        res
    }
}

/// Takes an HTTP request, validates it, and fixes it (if possible) by adding
/// the HTTP version and trailing newlines if they aren't present.
///
/// Also adds a host header if one isn't present.
///
/// ```no_run
/// # fn check_http(a: String, b: &str) -> Result<String, String> { Ok(a) }
/// assert_eq!(
///     check_http("GET /".to_string(), "example.com").unwrap(),
///     "GET / HTTP/1.0\r\nhost: example.com\r\n\r\n"
/// )
/// ```
#[cfg(feature = "https")]
fn check_http(mut request: String, hostname: &str) -> Result<String, String> {
    let mut headers = [httparse::EMPTY_HEADER; 64];
    let mut req = httparse::Request::new(&mut headers);

    let mut lines = request.lines().collect::<Vec<_>>();
    let mut trailing_newline = request.ends_with('\n');

    // check to make sure theres an empty line somewhere in there. if not, add 2 empty lines
    if !lines.iter().any(|line| line.is_empty()) {
        lines.push("");
        lines.push("");
        // so we dont unecessarily add another newline
        trailing_newline = false;
    }

    // If the first line doesn't have a version, add one
    let first = lines.first().ok_or("Empty HTTP request")?;
    let last_token = first
        .trim_end()
        .split_ascii_whitespace()
        .next_back()
        .ok_or("Empty first line")?;
    if !last_token.starts_with("HTTP/") {
        request = first.to_string()
            + " HTTP/1.0\r\n"
            + &lines.into_iter().skip(1).collect::<Vec<_>>().join("\r\n");
    } else {
        request = lines.join("\r\n");
    }
    if trailing_newline {
        request += "\r\n";
    }

    // Confirm that the request is valid
    let status = req.parse(request.as_bytes()).map_err(|e| {
        use httparse::Error;
        format!(
            "Failed to parse HTTP request: {}",
            match e {
                Error::HeaderName => "Invalid byte in header name",
                Error::HeaderValue => "Invalid byte in Header value",
                Error::NewLine => "Invalid byte in newline",
                Error::Status => "Invalid byte in response status",
                Error::Token => "Invalid byte where token is required",
                Error::TooManyHeaders => "Too many headers! Maximum of 64",
                Error::Version => "Invalid byte in HTTP version",
            }
        )
    })?;
    match status {
        httparse::Status::Partial => return Err("Incomplete (Partial) HTTP request".into()),
        httparse::Status::Complete(_) => {}
    };

    // If Status was Complete everything should be there
    // (but just in case we'll check)
    let _method = req.method.ok_or("No method in HTTP request")?;
    let _path = req.path.ok_or("No path in HTTP request")?;
    let _version = req.version.ok_or("No version in HTTP request")?;

    // add the host header
    // it's safe the unwrap here because if the http request is valid, it must
    // have a newline in it
    if !req
        .headers
        .iter()
        .any(|h| h.name.eq_ignore_ascii_case("host"))
    {
        let newline = request.find('\n').unwrap();
        request.insert_str(newline + 1, &format!("host: {hostname}\r\n"));
    }

    Ok(request)
}
