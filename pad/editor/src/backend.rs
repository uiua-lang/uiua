use std::{
    any::Any,
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    io::Cursor,
    path::{Component, Path, PathBuf},
    sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use js_sys::{Date, Uint8Array};
use leptos::{prelude::*, *};
use serde::*;
use uiua::{BigConstant, EXAMPLE_TXT, EXAMPLE_UA, GitTarget, Handle, Report, SysBackend, now};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::{JsFuture, spawn_local};
use web_sys::{
    HtmlAudioElement, Request, RequestInit, RequestMode, Response, WorkerGlobalScope,
    XmlHttpRequest, XmlHttpRequestResponseType,
};

use crate::{
    START_TIME, get_ast_time,
    worker::{self, OutputUpdate},
};

pub struct WebBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
    vars: Mutex<HashMap<String, String>>,
    streams: Mutex<HashMap<Handle, VirtualStream>>,
    output_enabled: AtomicBool,
    ast_time: Option<f64>,
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

pub(crate) fn weewuh() {
    if worker::is_worker() {
        worker::bell();
        return;
    }
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
    FILES.with(|files| files.borrow_mut().insert(normalize_path(&path), contents));
}

pub fn delete_file(path: &Path) {
    let path = normalize_path(path);
    FILES.with(|files| files.borrow_mut().remove(&path));
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => (),
            Component::ParentDir => {
                normalized.pop();
            }
            _ => normalized.push(component.as_os_str()),
        }
    }
    normalized
}

impl Default for WebBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl WebBackend {
    pub fn new() -> Self {
        Self {
            stdout: Vec::new().into(),
            stderr: String::new().into(),
            trace: String::new().into(),
            vars: Mutex::default(),
            streams: HashMap::new().into(),
            output_enabled: AtomicBool::new(true),
            ast_time: None,
        }
    }
    pub fn with_ast_time(mut self, ast_time: f64) -> Self {
        self.ast_time = Some(ast_time);
        self
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OutputItem {
    String(String),
    Svg(String, Option<String>),
    Image(#[serde(with = "serde_bytes")] Vec<u8>, Option<String>),
    Gif(#[serde(with = "serde_bytes")] Vec<u8>, Option<String>),
    Apng(#[serde(with = "serde_bytes")] Vec<u8>, Option<String>),
    Audio(#[serde(with = "serde_bytes")] Vec<u8>, Option<String>),
    Report(Report),
    Faint(String),
    Classed(String, String),
    Separator,
}

impl OutputItem {
    pub fn is_report(&self) -> bool {
        matches!(self, OutputItem::Report(_))
    }
}

pub(crate) fn append_stdout(stdout: &mut Vec<OutputItem>, s: &str) {
    let mut lines = s.split('\n');
    let Some(first) = lines.next() else {
        return;
    };
    if let Some(OutputItem::String(prev)) = stdout.last_mut() {
        prev.push_str(first);
    } else {
        stdout.push(OutputItem::String(first.into()));
    }
    for line in lines {
        stdout.push(OutputItem::String(line.into()));
    }
}

impl WebBackend {
    fn push_output(&self, item: OutputItem) {
        item.output();
        self.stdout.lock().unwrap().push(item);
    }
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
        let path = normalize_path(path);
        FILES.with(|files| {
            (files.borrow().get(&path).map(|content| f(content)))
                .ok_or_else(|| format!("File not found: {}", path.display()))
        })
    }
    fn file_mut<T>(&self, path: &Path, f: impl FnOnce(&mut Vec<u8>) -> T) -> Result<T, String> {
        FILES.with(|files| {
            (files.borrow_mut().get_mut(path).map(f))
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
        append_stdout(&mut stdout, s);
        OutputUpdate::Stdout(s.into()).send();
        Ok(())
    }
    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        if !self.output_enabled() {
            return Ok(());
        }
        self.stderr.lock().unwrap().push_str(s);
        OutputUpdate::Stderr(s.into()).send();
        Ok(())
    }
    fn print_str_trace(&self, s: &str) {
        if !self.output_enabled() {
            return;
        }
        self.trace.lock().unwrap().push_str(s);
        OutputUpdate::Trace(s.into()).send();
    }
    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        if !self.output_enabled() {
            return Ok(None);
        }
        if worker::is_worker() {
            return worker::input("Enter a line of text for stdin");
        }
        Ok(window()
            .prompt_with_message("Enter a line of text for stdin")
            .unwrap_or(None))
    }
    fn get_var(&self, name: &str) -> Option<String> {
        self.vars.lock().unwrap().get(name).map(Into::into)
    }
    unsafe fn set_var(&self, name: &str, val: &str) {
        self.vars.lock().unwrap().insert(name.into(), val.into());
    }
    fn show_image(&self, image: image::DynamicImage, label: Option<&str>) -> Result<(), String> {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageFormat::Png)
            .map_err(|e| format!("Failed to show image: {e}"))?;
        self.push_output(OutputItem::Image(bytes.into_inner(), label.map(Into::into)));
        Ok(())
    }
    fn show_gif(&self, gif_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        self.push_output(OutputItem::Gif(gif_bytes, label.map(Into::into)));
        Ok(())
    }
    fn show_apng(&self, apng_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        self.push_output(OutputItem::Apng(apng_bytes, label.map(Into::into)));
        Ok(())
    }
    fn list_dir(&self, mut path: &str) -> Result<Vec<String>, String> {
        if path.starts_with("./") {
            path = &path[2..];
        } else if path.starts_with('.') {
            path = &path[1..];
        }
        let mut set = BTreeSet::new();
        FILES.with(|files| {
            for file in files.borrow().keys() {
                logging::log!("check file {file:?}");
                if file.parent().is_some_and(|par| par == path)
                    || file.parent().is_none() && path.is_empty()
                {
                    set.insert(file.file_name().unwrap().to_string_lossy().into());
                } else if let Some(mut parent) = file
                    .parent()
                    .filter(|&par| par != path && par.starts_with(path))
                {
                    logging::log!("  parent: {parent:?}");
                    while let Some(par) = parent
                        .parent()
                        .filter(|&par| par != path && par.starts_with(path))
                    {
                        parent = par;
                        logging::log!("  parent: {parent:?}");
                    }
                    set.insert(
                        parent
                            .to_string_lossy()
                            .trim_start_matches(path)
                            .trim_start_matches("/")
                            .into(),
                    );
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
        drop_file(path.into(), contents.to_vec());
        Ok(())
    }
    fn file_append_all(&self, path: &Path, contents: &[u8]) -> Result<(), String> {
        self.file_mut(path, |file| file.extend_from_slice(contents))
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
        drop_file(path.into(), Vec::new());
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
        let end = (stream.pos + offset).min(stream.contents.len());
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
        delete_file(path.as_ref());
        Ok(())
    }
    fn trash(&self, path: &str) -> Result<(), String> {
        self.delete(path)
    }
    fn play_audio(&self, wav_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        self.push_output(OutputItem::Audio(wav_bytes, label.map(Into::into)));
        Ok(())
    }
    fn stream_audio(&self, mut f: uiua::AudioStreamFn) -> Result<(), String> {
        let mut samples = Vec::new();
        const SAMPLE_RATE: u32 = 44100;
        const SAMPLES_PER_FRAME: usize = 10000;
        let mut times = Vec::with_capacity(SAMPLES_PER_FRAME);
        let ast_time = self.ast_time.unwrap_or_else(get_ast_time);
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
        if worker::is_worker() {
            return worker::input("Paste clipboard contents").map(Option::unwrap_or_default);
        }
        Ok(window()
            .prompt_with_message("Paste clipboard contents")
            .unwrap_or(None)
            .unwrap_or_default())
    }
    fn set_clipboard(&self, contents: &str) -> Result<(), String> {
        if worker::is_worker() {
            worker::set_clipboard(contents);
            return Ok(());
        }
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
    fn load_git_module(
        &self,
        original_url: &str,
        target: GitTarget,
        subfolder: Option<&str>,
    ) -> Result<PathBuf, String> {
        let branch_name: String = match &target {
            GitTarget::Default => "main".to_owned(),
            GitTarget::Branch(branch) => branch.clone(),
            GitTarget::Commit(_) => {
                return Err("Git commit specification is not supported in the web backend".into());
            }
        };

        let (repo_owner, repo_name, path) = {
            let mut parts = original_url.rsplitn(3, '/');
            let repo_name = parts.next().ok_or("Invalid git url")?;
            let repo_owner = parts.next().ok_or("Invalid git url")?;
            let mut path = Path::new("uiua-modules")
                .join(repo_owner)
                .join(repo_name)
                .join(&branch_name);
            if let Some(subfolder) = subfolder {
                path = path.join(subfolder);
            }
            path = path.join("lib.ua");

            (repo_owner.to_string(), repo_name.to_string(), path)
        };
        let path = normalize_path(&path);

        if FILES.with(|files| files.borrow().contains_key(&path)) {
            return Ok(path);
        }

        let mut url = original_url
            .trim_end_matches('/')
            .replace("www.", "")
            .replace("github.com", "raw.githubusercontent.com")
            .replace("src/branch/master", "raw/branch/master");

        let branch = urlencoding::encode(&branch_name);
        if original_url.contains("github.com") {
            logging::log!("Fetching github repo: {url}");
            let tree = load_bytes(&format!(
                "https://api.github.com\
                /repos/{repo_owner}/{repo_name}/git/trees/{branch}?recursive=1",
            ))?;
            let tree: serde_json::Value = serde_json::from_slice(&tree).unwrap();
            let tree = tree.get("tree").unwrap().as_array().unwrap();
            let paths = tree
                .iter()
                .filter(|entry| {
                    entry.get("type").and_then(serde_json::Value::as_str) == Some("blob")
                })
                .filter_map(|entry| entry.get("path")?.as_str())
                .collect::<HashSet<_>>();

            let mut lib_path = PathBuf::from("lib.ua");
            if let Some(subfolder) = subfolder {
                lib_path = PathBuf::from(subfolder).join(lib_path);
            }
            let lib_path = lib_path.to_string_lossy();
            if !paths.contains(&*lib_path) {
                return Err(format!("{lib_path} not found"));
            }

            let mut downloaded = Vec::new();
            for path in paths {
                let file = path
                    .split('/')
                    .map(urlencoding::encode)
                    .collect::<Vec<_>>()
                    .join("/");
                let fetch_url = format!(
                    "https://raw.githubusercontent.com\
                    /{repo_owner}/{repo_name}/{branch}/{file}",
                );
                let internal_path = Path::new("uiua-modules")
                    .join(&repo_owner)
                    .join(&repo_name)
                    .join(&branch_name)
                    .join(path);
                if !FILES.with(|files| files.borrow().contains_key(&internal_path)) {
                    downloaded.push((internal_path, load_bytes(&fetch_url)?));
                }
            }
            // Publish the repo only after every download succeeds, so failed loads can be retried.
            FILES.with(|files| files.borrow_mut().extend(downloaded));
        } else {
            if !url.ends_with(".ua") {
                let subpath = subfolder.map(|path| format!("{path}/")).unwrap_or_default();
                url = format!("{url}/{branch}/{subpath}lib.ua");
            }
            logging::log!("Fetching url: {url}");
            drop_file(path.clone(), load_bytes(&url)?);
        }
        Ok(path)
    }
    fn timezone(&self) -> Result<f64, String> {
        if !cfg!(target_arch = "wasm32") {
            return Ok(0.0);
        }
        Ok(-Date::new_0().get_timezone_offset() / 60.0)
    }
    fn big_constant(&self, key: BigConstant) -> Result<Cow<'static, [u8]>, String> {
        #[cfg(not(target_arch = "wasm32"))]
        if key == BigConstant::Uiua386 {
            return Ok(Cow::Borrowed(include_bytes!(
                "../../../src/assets/Uiua386.ttf"
            )));
        }
        load_bytes(match key {
            BigConstant::Uiua386 => "/Uiua386.ttf",
            BigConstant::Elevation => "/assets/elevation.webp",
            BigConstant::BadAppleGif => "/assets/bad-apple.gif",
            BigConstant::Amen => "/assets/amen-break.wav",
        })
        .map(Cow::Owned)
    }
}

pub async fn fetch(url: &str) -> Result<String, String> {
    let opts = RequestInit::new();
    opts.set_method("GET");
    opts.set_mode(RequestMode::Cors);
    let request = Request::new_with_str_and_init(url, &opts)
        .map_err(|e| format!("Fetch request error: {e:?}"))?;
    let promise = if let Some(window) = web_sys::window() {
        window.fetch_with_request(&request)
    } else {
        js_sys::global()
            .unchecked_into::<WorkerGlobalScope>()
            .fetch_with_request(&request)
    };
    let resp_value = JsFuture::from(promise)
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
    let promise = if let Some(window) = web_sys::window() {
        window.fetch_with_request(&request)
    } else {
        js_sys::global()
            .unchecked_into::<WorkerGlobalScope>()
            .fetch_with_request(&request)
    };
    let resp_value = JsFuture::from(promise)
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

/// Load synchronously in the execution worker, asynchronously for syntax highlighting.
fn load_bytes(url: &str) -> Result<Vec<u8>, String> {
    if !cfg!(target_arch = "wasm32") {
        return Err(format!("Cannot fetch {url} outside the browser"));
    }
    if worker::is_worker() {
        let request = XmlHttpRequest::new().unwrap();
        request.open_with_async("GET", url, false).unwrap();
        request.set_response_type(XmlHttpRequestResponseType::Arraybuffer);
        (request.send()).map_err(|e| format!("Request failed for {url}: {e:?}"))?;
        let status = request.status().unwrap();
        let bytes = Uint8Array::new(&request.response().unwrap()).to_vec();
        return match status {
            200..300 => Ok(bytes),
            _ => Err(format!(
                "HTTP {status} for {url}: {}",
                String::from_utf8_lossy(&bytes)
            )),
        };
    }
    // Cache loads
    // None means loading
    type Cache = HashMap<String, Option<Result<Vec<u8>, String>>>;
    thread_local! {
        static CACHE: RefCell<Cache> = RefCell::default();
    }
    if let Some(result) = CACHE.with(|cache| cache.borrow().get(url).cloned().flatten()) {
        return result;
    }
    let working = CACHE.with(|cache| cache.borrow_mut().insert(url.into(), None).is_some());
    if !working {
        let url = url.to_string();
        spawn_local(async move {
            let result = fetch_bytes(&url).await;
            CACHE.with(|cache| cache.borrow_mut().insert(url, Some(result)));
        });
    }
    Err(format!("Loading {url}"))
}
