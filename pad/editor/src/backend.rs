use std::{
    any::Any,
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    hash::{DefaultHasher, Hash, Hasher},
    io::Cursor,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Mutex,
    },
};

use crate::{get_ast_time, START_TIME};
use futures::future::join_all;
use js_sys::{Date, Uint8Array};
use leptos::*;
use uiua::{
    now, BigConstant, GitTarget, Handle, Report, Span, SysBackend, Uiua, EXAMPLE_TXT, EXAMPLE_UA,
};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{HtmlAudioElement, Request, RequestInit, RequestMode, Response};

pub struct WebBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
    streams: Mutex<HashMap<Handle, VirtualStream>>,
    id: u64,
    breakpoint: AtomicUsize,
    output_enabled: AtomicBool,
}

struct VirtualStream {
    path: Option<PathBuf>,
    contents: Vec<u8>,
    pos: usize,
    writeable: bool,
}

thread_local! {
    pub static FILES: RefCell<HashMap<PathBuf, Vec<u8>>> = RefCell::new(
        [
            ("example.ua", EXAMPLE_UA),
            ("example.txt", EXAMPLE_TXT),
            ("primitives.json", include_str!("../../../site/primitives.json"))
        ]
        .map(|(path, content)| (PathBuf::from(path), content.as_bytes().to_vec()))
        .into(),
    );
}

fn weewuh() {
    let i = (now() % 1.0 * 100.0) as u32;
    let src = match i {
        0 => "/assets/ooh-ee-ooh-ah.mp3",
        1..=4 => "/assets/wee-wah.mp3",
        _ => "/assets/wee-wuh.mp3",
    };
    if let Ok(audio) = HtmlAudioElement::new_with_src(src) {
        _ = audio.play();
    }
}

pub fn drop_file(path: PathBuf, contents: Vec<u8>) {
    FILES.with(|files| files.borrow_mut().insert(path, contents));
}

pub fn delete_file(path: &PathBuf) {
    FILES.with(|files| files.borrow_mut().remove(path));
}

thread_local! {
    static BREAKPOINTS: RefCell<HashMap<u64, (u64, usize)>> = Default::default();
}

impl Default for WebBackend {
    fn default() -> Self {
        Self::new(0, "")
    }
}

impl WebBackend {
    pub fn new(id: impl Hash, code: impl Hash) -> Self {
        let mut hasher = DefaultHasher::new();
        id.hash(&mut hasher);
        let id = hasher.finish();
        let mut hasher = DefaultHasher::new();
        code.hash(&mut hasher);
        let code = hasher.finish();
        BREAKPOINTS.with(|map| {
            let mut map = map.borrow_mut();
            let (old_code, bp) = map.entry(id).or_insert((code, 0));
            if *old_code != code {
                *old_code = code;
                *bp = 0;
            }
        });
        Self {
            stdout: Vec::new().into(),
            stderr: String::new().into(),
            trace: String::new().into(),
            streams: HashMap::new().into(),
            id,
            breakpoint: AtomicUsize::new(0),
            output_enabled: AtomicBool::new(true),
        }
    }
    pub fn finish(&self) {
        BREAKPOINTS.with(|map| {
            let mut map = map.borrow_mut();
            if let Some((_, bp)) = map.get_mut(&self.id) {
                *bp = 0;
            }
        });
    }
}

pub enum OutputItem {
    String(String),
    Svg(String, Option<String>),
    Image(Vec<u8>, Option<String>),
    Gif(Vec<u8>, Option<String>),
    Apng(Vec<u8>, Option<String>),
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
            files
                .get(path)
                .map(|content| f(content))
                .ok_or(format!("File not found: {}", path.display()))
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
    fn output_enabled(&self) -> bool {
        self.output_enabled.load(Ordering::Relaxed)
    }
    fn set_output_enabled(&self, enabled: bool) -> bool {
        self.output_enabled.swap(enabled, Ordering::Relaxed)
    }
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        if !self.output_enabled() {
            return Ok(());
        }
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
        if !self.output_enabled() {
            return Ok(());
        }
        self.stderr.lock().unwrap().push_str(s);
        Ok(())
    }
    fn print_str_trace(&self, s: &str) {
        if !self.output_enabled() {
            return;
        }
        self.trace.lock().unwrap().push_str(s);
    }
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        if !self.output_enabled() {
            return Ok(None);
        }
        Ok(window()
            .prompt_with_message("Enter a line of text for stdin")
            .unwrap_or(None))
    }
    fn show_image(&self, image: image::DynamicImage, label: Option<&str>) -> Result<(), String> {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageFormat::Png)
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
    fn show_apng(&self, apng_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(OutputItem::Apng(apng_bytes, label.map(Into::into)));
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
    fn seek(&self, handle: Handle, offset: uiua::StreamSeek) -> Result<(), String> {
        let mut streams = self.streams.lock().unwrap();
        let stream = streams.get_mut(&handle).ok_or("Invalid stream handle")?;
        let size = stream.contents.len();
        stream.pos = match offset {
            uiua::StreamSeek::Start(off) => {
                if off >= size {
                    Err(format!(
                        "Tried to seek to {off}, but the file stream is only {size} bytes"
                    ))?
                }
                off
            }

            uiua::StreamSeek::End(off) => size.checked_sub(off).ok_or_else(|| {
                format!(
                    "Tried to seek {off} bytes from file stream end, but the file stream is only {size} bytes"
                )
            })?,
        };
        Ok(())
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
        let bytes = uiua::media::stereo_to_wave_bytes(
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
    fn clipboard(&self) -> Result<String, String> {
        Ok(window()
            .prompt_with_message("Paste clipboard contents")
            .unwrap_or(None)
            .unwrap_or_default())
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
    fn allow_thread_spawning(&self) -> bool {
        true
    }
    fn load_git_module(&self, original_url: &str, target: GitTarget) -> Result<PathBuf, String> {
        thread_local! {
            static CACHE: RefCell<HashMap<String, Result<String, String>>> = Default::default();
            static WORKING: RefCell<HashSet<String>> = Default::default();
        }

        fn cache_url(url: &str, res: Result<String, String>) {
            CACHE.with(|cache| {
                cache.borrow_mut().insert(url.into(), res);
            });
        }

        fn mark_working(url: &str) {
            WORKING.with(|working| working.borrow_mut().insert(url.into()));
        }

        fn unmark_working(url: &str) {
            WORKING.with(|working| working.borrow_mut().remove(url));
        }

        if WORKING.with(|working| working.borrow().contains(original_url)) {
            return Err("Waiting for module, try running again in a moment...".into());
        }

        match target {
            GitTarget::Default => {}
            GitTarget::Branch(_) => {
                return Err("Git branch specification is not supported in the web backend".into())
            }
            GitTarget::Commit(_) => {
                return Err("Git commit specification is not supported in the web backend".into())
            }
        }
        let (repo_owner, repo_name, path) = {
            let mut parts = original_url.rsplitn(3, '/');
            let repo_name = parts.next().ok_or("Invalid git url")?;
            let repo_owner = parts.next().ok_or("Invalid git url")?;
            let path = Path::new("uiua-modules")
                .join(repo_owner)
                .join(repo_name)
                .join("lib.ua");

            (repo_owner.to_string(), repo_name.to_string(), path)
        };

        if FILES.with(|files| files.borrow().contains_key(&path)) {
            return Ok(path);
        }

        let mut url = original_url
            .trim_end_matches('/')
            .replace("www.", "")
            .replace("github.com", "raw.githubusercontent.com")
            .replace("src/branch/master", "raw/branch/master");

        if !url.ends_with(".ua") {
            url = format!("{url}/main/lib.ua");
        }

        let res = CACHE.with(|cache| {
            if let Some(res) = cache.borrow().get(&url) {
                logging::log!("Using cached module for {url:?}");
                Some(res.clone())
            } else if original_url.contains("github.com") && url.ends_with("/lib.ua") {
                logging::log!("Fetching github repo: {url}");
                mark_working(original_url);
                let original_url = original_url.to_string();

                spawn_local(async move {
                    let tree_res = fetch(&format!(
                        "https://api.github.com\
                        /repos/{repo_owner}/{repo_name}/git/trees/main?recursive=1",
                    ))
                    .await;

                    match tree_res {
                        Err(_) => {
                            cache_url(&url, tree_res);
                            unmark_working(&original_url);
                            return;
                        }
                        Ok(_) => {
                            let tree = tree_res.unwrap();
                            let tree: serde_json::Value = serde_json::from_str(&tree).unwrap();
                            let tree = tree.get("tree").unwrap().as_array().unwrap();
                            let paths = tree
                                .iter()
                                .filter_map(|entry| {
                                    let path = entry.get("path")?.as_str()?;
                                    if path.ends_with(".ua") {
                                        Some(path.to_string())
                                    } else {
                                        None
                                    }
                                })
                                .collect::<HashSet<_>>();

                            if !paths.contains("lib.ua") {
                                cache_url(&url, Err("lib.ua not found".into()));
                                unmark_working(&original_url);
                                return;
                            }

                            let results = join_all(paths.iter().map(|path| {
                                let repo_owner = repo_owner.clone();
                                let repo_name = repo_name.clone();
                                async move {
                                    let fetch_url = format!(
                                        "https://raw.githubusercontent.com\
                                        /{repo_owner}/{repo_name}/main/{path}",
                                    );
                                    let internal_path = Path::new("uiua-modules")
                                        .join(repo_owner)
                                        .join(repo_name)
                                        .join(path.clone());

                                    (path, internal_path, fetch(fetch_url.as_str()).await)
                                }
                            }))
                            .await;

                            for (original_path, internal_path, res) in results {
                                if original_path.eq("lib.ua") {
                                    cache_url(&url, res.clone());
                                }

                                if let Ok(text) = res {
                                    let contents = text.as_bytes().to_vec();
                                    drop_file(internal_path.clone(), contents);
                                }
                            }
                        }
                    }

                    unmark_working(&original_url);
                });
                None
            } else {
                logging::log!("Fetching url: {url}");
                mark_working(original_url);
                let original_url = original_url.to_string();
                spawn_local(async move {
                    let res = fetch(&url).await;
                    cache_url(&url, res);
                    unmark_working(&original_url);
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
            None => Err("Waiting for module, try running again in a moment...".into()),
        }
    }
    fn timezone(&self) -> Result<f64, String> {
        if !cfg!(target_arch = "wasm32") {
            return Ok(0.0);
        }
        Ok(-Date::new_0().get_timezone_offset() / 60.0)
    }
    fn breakpoint(&self, env: &Uiua) -> Result<bool, String> {
        let breakpoint = self.breakpoint.fetch_add(1, Ordering::Relaxed);
        let reached = BREAKPOINTS.with(|map| {
            let mut map = map.borrow_mut();
            if let Some((_, bp)) = map.get_mut(&self.id) {
                let reached = breakpoint >= *bp;
                if reached {
                    *bp += 1;
                }
                reached
            } else {
                false
            }
        });

        if !reached {
            return Ok(true);
        }

        let message = match env.span() {
            Span::Code(span) => format!("Break at {span}"),
            Span::Builtin => "Breakpoint".into(),
        };
        self.trace.lock().unwrap().push_str(&message);
        Ok(false)
    }
    fn big_constant(&self, key: BigConstant) -> Result<Cow<'static, [u8]>, String> {
        thread_local! {
            static CACHE: RefCell<HashMap<BigConstant, Result<Vec<u8>, String>>> = Default::default();
            static WORKING: RefCell<HashSet<BigConstant>> = Default::default();
        }

        if WORKING.with(|working| working.borrow().contains(&key)) {
            return Err("Waiting for data, try running again in a moment...".into());
        }

        CACHE.with(|cache| {
            if let Some(bytes) = cache.borrow().get(&key).cloned() {
                WORKING.with(|working| working.borrow_mut().remove(&key));
                return bytes.map(Cow::Owned);
            }
            WORKING.with(|working| working.borrow_mut().insert(key));
            spawn_local(async move {
                let path = match key {
                    BigConstant::Elevation => "/assets/elevation.webp",
                    BigConstant::BadAppleTransposed => "/assets/bad_apple_transposed.gif",
                };
                let fetch_res = fetch_bytes(path).await;
                CACHE.with(|cache| cache.borrow_mut().insert(key, fetch_res.clone()));
                WORKING.with(|working| working.borrow_mut().remove(&key));
            });
            match cache.borrow().get(&key).cloned() {
                Some(Ok(bytes)) => Ok(Cow::Owned(bytes)),
                Some(Err(err)) => Err(err),
                None => Err("Waiting for module, try running again in a moment...".into()),
            }
        })
    }
}

pub async fn fetch(url: &str) -> Result<String, String> {
    let opts = RequestInit::new();
    opts.set_method("GET");
    opts.set_mode(RequestMode::Cors);
    let request = Request::new_with_str_and_init(url, &opts)
        .map_err(|e| format!("Fetch request error: {e:?}"))?;
    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|e| format!("Fetch response error: {e:?}"))?;
    assert!(resp_value.is_instance_of::<Response>());
    let resp: Response = resp_value.dyn_into().unwrap();
    let text = JsFuture::from(resp.text().map_err(|e| format!("Fetch error: {e:?}"))?)
        .await
        .map(|s| s.as_string().unwrap())
        .map_err(|e| format!("Fetch error: {e:?}"))?;
    if resp.status() == 200 {
        Ok(text)
    } else {
        Err(text)
    }
}

pub async fn fetch_bytes(url: &str) -> Result<Vec<u8>, String> {
    let opts = RequestInit::new();
    opts.set_method("GET");
    opts.set_mode(RequestMode::Cors);
    let request = Request::new_with_str_and_init(url, &opts)
        .map_err(|e| format!("Fetch request error: {e:?}"))?;
    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|e| format!("Fetch response error: {e:?}"))?;
    assert!(resp_value.is_instance_of::<Response>());
    let resp: Response = resp_value.dyn_into().unwrap();
    if resp.status() == 200 {
        JsFuture::from(
            resp.array_buffer()
                .map_err(|e| format!("Fetch error: {e:?}"))?,
        )
        .await
        .map(|s| Uint8Array::new(&s).to_vec())
        .map_err(|e| format!("Fetch error: {e:?}"))
    } else {
        Err(
            JsFuture::from(resp.text().map_err(|e| format!("Fetch error: {e:?}"))?)
                .await
                .map(|s| s.as_string().unwrap())
                .map_err(|e| format!("Fetch error: {e:?}"))?,
        )
    }
}
