use std::{
    any::Any, cell::RefCell, collections::{BTreeSet, HashMap}, io::Cursor, path::{Path, PathBuf}, sync::Mutex
};

use crate::{editor::get_ast_time, weewuh, START_TIME};
use leptos::*;
use uiua::{now, GitTarget, Handle, Report, SysBackend, EXAMPLE_TXT, EXAMPLE_UA};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response};

pub struct WebBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
    streams: Mutex<HashMap<Handle, VirtualStream>>,
}

struct VirtualStream {
    path: Option<PathBuf>,
    contents: Vec<u8>,
    pos: usize,
    writeable: bool,
}

thread_local! {
    static FILES: RefCell<HashMap<PathBuf, Vec<u8>>> = RefCell::new(
        [
            ("example.ua", EXAMPLE_UA),
            ("example.txt", EXAMPLE_TXT)
        ]
        .map(|(path, content)| (PathBuf::from(path), content.as_bytes().to_vec()))
        .into(),
    );
}

pub fn drop_file(path: PathBuf, contents: Vec<u8>) {
    FILES.with(|files| files.borrow_mut().insert(path, contents));
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: Vec::new().into(),
            stderr: String::new().into(),
            trace: String::new().into(),
            streams: HashMap::new().into(),
        }
    }
}

pub enum OutputItem {
    String(String),
    Svg(String),
    Image(Vec<u8>, Option<String>),
    Gif(Vec<u8>, Option<String>),
    Audio(Vec<u8>, Option<String>),
    Report(Report),
    Faint(String),
    Classed(&'static str, String),
    Separator,
}

impl OutputItem {
    pub fn is_report(&self) -> bool {
        matches!(self, OutputItem::Report(_))
    }
}

impl WebBackend {
    fn new_handle(&self) -> Handle {
        let streams = self.streams.lock().unwrap();
        for handle in (Handle::FIRST_UNRESERVED.0..u64::MAX).map(Handle) {
            if !streams.contains_key(&handle) {
                return handle;
            }
        }
        panic!("Ran out of file handles");
    }
    fn file<T>(&self, path: &Path, f: impl FnOnce(&[u8]) -> T) -> Result<T, String> {
        FILES.with(|files| { 
            let files = files.borrow();
            files.get(path).map(|content| f(content)).ok_or(format!("File not found: {}", path.display()))
        })
    }
}

impl SysBackend for WebBackend {
    fn any(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        if s.contains('\u{07}') {
            weewuh();
        }
        let mut stdout = self.stdout.lock().unwrap();
        let mut lines = s.lines();
        let Some(first) = lines.next() else {
            return Ok(());
        };
        if let Some(OutputItem::String(prev)) = stdout.last_mut() {
            prev.push_str(first);
        } else {
            stdout.push(OutputItem::String(first.into()));
        }
        for line in lines {
            stdout.push(OutputItem::String(line.into()));
        }
        if s.ends_with('\n') {
            stdout.push(OutputItem::String("".into()));
        }
        Ok(())
    }
    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        self.stderr.lock().unwrap().push_str(s);
        Ok(())
    }
    fn print_str_trace(&self, s: &str) {
        self.trace.lock().unwrap().push_str(s);
    }
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        Ok(window()
            .prompt_with_message("Enter a line of text for stdin")
            .unwrap_or(None))
    }
    fn show_image(&self, image: image::DynamicImage, label: Option<&str>) -> Result<(), String> {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageOutputFormat::Png)
            .map_err(|e| format!("Failed to show image: {e}"))?;
        self.stdout
            .lock()
            .unwrap()
            .push(OutputItem::Image(bytes.into_inner(), label.map(Into::into)));
        Ok(())
    }
    fn show_gif(&self, gif_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(OutputItem::Gif(gif_bytes, label.map(Into::into)));
        Ok(())
    }
    fn list_dir(&self, mut path: &str) -> Result<Vec<String>, String> {
        if path.starts_with("./") {
            path = &path[2..];
        } else if path.starts_with('.') {
            path = &path[1..];
        }
        let path = Path::new(path);
        let mut set = BTreeSet::new();
        FILES.with(|files| {
            for file in files.borrow().keys() {
                if file.parent() == Some(path) {
                    set.insert(file.file_name().unwrap().to_string_lossy().into());
                }
            }
        });
        Ok(set.into_iter().collect())
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        Ok(self.file(path.as_ref(), |_| {}).is_ok())
    }
    fn file_exists(&self, path: &str) -> bool {
        self.file(path.as_ref(), |_| {}).is_ok()
    }
    fn file_write_all(&self, path: &Path, contents: &[u8]) -> Result<(), String> {
        FILES.with(|files| {
            if !files.borrow().contains_key(path) {
                files.borrow_mut().insert(path.into(), contents.to_vec());
            } else {
                *files.borrow_mut().get_mut(path).unwrap() = contents.to_vec();
            }
        });
        Ok(())
    }
    fn file_read_all(&self, path: &Path) -> Result<Vec<u8>, String> {
        self.file(path, |contents| contents.to_vec())
    }
    fn open_file(&self, path: &Path, write: bool) -> Result<Handle, String> {
        let handle = self.new_handle();
        let contents = self.file_read_all(path)?;
        self.streams.lock().unwrap().insert(
            handle,
            VirtualStream {
                path: Some(path.to_path_buf()),
                contents,
                pos: 0,
                writeable: write,
            },
        );
        Ok(handle)
    }
    fn create_file(&self, path: &Path) -> Result<Handle, String> {
        let handle = self.new_handle();
        FILES.with(|files| files.borrow_mut().insert(path.into(), Vec::new()));
        self.streams.lock().unwrap().insert(
            handle,
            VirtualStream {
                path: Some(path.to_path_buf()),
                contents: Vec::new(),
                pos: 0,
                writeable: true,
            },
        );
        Ok(handle)
    }
    fn close(&self, handle: Handle) -> Result<(), String> {
        let stream = self
            .streams
            .lock()
            .unwrap()
            .remove(&handle)
            .ok_or("Invalid stream handle")?;
        if let Some(path) = stream.path {
            self.file_write_all(&path, &stream.contents)?;
        }
        Ok(())
    }
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        let mut streams = self.streams.lock().unwrap();
        let stream = streams.get_mut(&handle).ok_or("Invalid stream handle")?;
        if !stream.writeable {
            return Err("Stream is not writeable".into());
        }
        let end = stream.pos + contents.len();
        if stream.contents.len() < end {
            stream.contents.resize(end, 0);
        }
        stream.contents[stream.pos..end].copy_from_slice(contents);
        stream.pos = end;
        Ok(())
    }
    fn read(&self, handle: Handle, len: usize) -> Result<Vec<u8>, String> {
        let mut streams = self.streams.lock().unwrap();
        let stream = streams.get_mut(&handle).ok_or("Invalid stream handle")?;
        let end = (stream.pos + len).min(stream.contents.len());
        let data = stream.contents[stream.pos..end].to_vec();
        stream.pos = end;
        Ok(data)
    }
    fn read_all(&self, handle: Handle) -> Result<Vec<u8>, String> {
        let mut streams = self.streams.lock().unwrap();
        let stream = streams.get_mut(&handle).ok_or("Invalid stream handle")?;
        let data = stream.contents[stream.pos..].to_vec();
        stream.pos = stream.contents.len();
        Ok(data)
    }
    fn read_until(&self, handle: Handle, delim: &[u8]) -> Result<Vec<u8>, String> {
        let mut streams = self.streams.lock().unwrap();
        let stream = streams.get_mut(&handle).ok_or("Invalid stream handle")?;
        let offset = stream.contents[stream.pos..]
            .windows(delim.len())
            .position(|w| w == delim)
            .unwrap_or(stream.contents.len());
        let end = stream.pos + offset;
        let data = stream.contents[stream.pos..end].to_vec();
        stream.pos = (end + delim.len()).min(stream.contents.len());
        Ok(data)
    }
    fn delete(&self, path: &str) -> Result<(), String> {
        FILES.with(|files| files.borrow_mut().remove(Path::new(path)));
        Ok(())
    }
    fn trash(&self, path: &str) -> Result<(), String> {
        self.delete(path)
    }
    fn play_audio(&self, wav_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(OutputItem::Audio(wav_bytes, label.map(Into::into)));
        Ok(())
    }
    fn stream_audio(&self, mut f: uiua::AudioStreamFn) -> Result<(), String> {
        let mut samples = Vec::new();
        const SAMPLE_RATE: u32 = 44100;
        const SAMPLES_PER_FRAME: usize = 10000;
        let mut times = Vec::with_capacity(SAMPLES_PER_FRAME);
        let ast_time = get_ast_time();
        let mut i = 0;
        while (i as f64 / SAMPLE_RATE as f64) < ast_time {
            times.clear();
            for _ in 0..SAMPLES_PER_FRAME {
                times.push(i as f64 / SAMPLE_RATE as f64);
                i += 1;
            }
            match f(&times) {
                Ok(s) => {
                    if s.len() > SAMPLES_PER_FRAME {
                        i += s.len() - SAMPLES_PER_FRAME;
                    }
                    samples.extend(s);
                }
                Err(err) => return Err(format!("{err}")),
            }
        }
        let bytes = uiua::encode::stereo_to_wave_bytes(
            &samples,
            |s| (s * i16::MAX as f64) as i16,
            16,
            hound::SampleFormat::Int,
            SAMPLE_RATE,
        )?;
        self.play_audio(bytes, None)
    }
    fn now(&self) -> f64 {
        *START_TIME.get_or_init(|| 0.0) + now()
    }
    fn set_clipboard(&self, contents: &str) -> Result<(), String> {
        _ = window().navigator().clipboard().write_text(contents);
        Ok(())
    }
    fn sleep(&self, seconds: f64) -> Result<(), String> {
        let start = now();
        while now() - start < seconds {}
        Ok(())
    }
    fn load_git_module(&self, url: &str, target: GitTarget) -> Result<PathBuf, String> {
        match target {
            GitTarget::Default => {}
            GitTarget::Branch(_) => {
                return Err("Git branch specification is not supported in the web backend".into())
            }
            GitTarget::Commit(_) => {
                return Err("Git commit specification is not supported in the web backend".into())
            }
        }
        let mut parts = url.rsplitn(3, '/');
        let repo_name = parts.next().ok_or("Invalid git url")?;
        let repo_owner = parts.next().ok_or("Invalid git url")?;
        let path = Path::new("uiua-modules")
            .join(repo_owner)
            .join(repo_name)
            .join("lib.ua");
        if FILES.with(|files| files.borrow().contains_key(&path)) {
            return Ok(path);
        }
        let mut url = url
            .trim_end_matches('/')
            .replace("www.", "")
            .replace("github.com", "raw.githubusercontent.com")
            .replace("src/branch/master", "raw/branch/master");
        if !url.ends_with(".ua") {
            url = format!("{url}/main/lib.ua");
        }
        thread_local! {
            static CACHE: RefCell<HashMap<String, Result<String, String>>> = Default::default();
        }
        let res = CACHE.with(|cache| {
            if let Some(res) = cache.borrow().get(&url) {
                logging::log!("Using cached module for {url:?}");
                Some(res.clone())
            } else {
                logging::log!("Fetching url: {url}");
                spawn_local(async move {
                    let res = fetch(&url).await;
                    CACHE.with(|cache| {
                        cache.borrow_mut().insert(url.clone(), res.clone());
                    });
                });
                None
            }
        });

        match res {
            Some(Ok(text)) => {
                let contents = text.as_bytes().to_vec();
                drop_file(path.clone(), contents);
                Ok(path)
            }
            Some(Err(err)) => Err(err),
            None => Err("Waiting for module, try running to check...".into()),
        }
    }
}

pub async fn fetch(url: &str) -> Result<String, String> {
    let opts = RequestInit::new();
    opts.set_method("GET");
    opts.set_mode(RequestMode::Cors);
    let request = Request::new_with_str_and_init(url, &opts).map_err(|e| format!("{e:?}"))?;
    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|e| format!("{e:?}"))?;
    assert!(resp_value.is_instance_of::<Response>());
    let resp: Response = resp_value.dyn_into().unwrap();
    let text = JsFuture::from(resp.text().map_err(|e| format!("{e:?}"))?)
        .await
        .map(|s| s.as_string().unwrap())
        .map_err(|e| format!("{e:?}"))?;
    if resp.status() == 200 {
        Ok(text)
    } else {
        Err(text)
    }
}
