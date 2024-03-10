use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    io::Cursor,
    path::{Path, PathBuf},
    sync::Mutex,
};

use crate::{editor::get_ast_time, weewuh};
use leptos::*;
use uiua::{example_ua, Report, SysBackend};
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response};

pub struct WebBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
}

thread_local! {
    static FILES: RefCell<HashMap<PathBuf, Vec<u8>>> = RefCell::new([
        (PathBuf::from("example.ua"),
        example_ua(|ex| ex.clone()).into())
    ].into());
    static REQ: RefCell<Option<FetchReq>> = RefCell::new(None);
}

pub fn drop_file(path: PathBuf, contents: Vec<u8>) {
    FILES.with(|dropped_files| {
        dropped_files.borrow_mut().insert(path, contents);
    });
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: Vec::new().into(),
            stderr: String::new().into(),
            trace: String::new().into(),
        }
    }
}

pub enum OutputItem {
    String(String),
    Image(Vec<u8>),
    Gif(Vec<u8>),
    Audio(Vec<u8>),
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
    fn show_image(&self, image: image::DynamicImage) -> Result<(), String> {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageOutputFormat::Png)
            .map_err(|e| format!("Failed to show image: {e}"))?;
        self.stdout
            .lock()
            .unwrap()
            .push(OutputItem::Image(bytes.into_inner()));
        Ok(())
    }
    fn show_gif(&self, gif_bytes: Vec<u8>) -> Result<(), String> {
        self.stdout.lock().unwrap().push(OutputItem::Gif(gif_bytes));
        Ok(())
    }
    fn list_dir(&self, mut path: &str) -> Result<Vec<String>, String> {
        if path.starts_with("./") {
            path = &path[2..];
        } else if path.starts_with('.') {
            path = &path[1..];
        }
        let path = Path::new(path);
        FILES.with(|files| {
            let files = files.borrow();
            let mut in_dir = Vec::new();
            for file in files.keys() {
                if file.parent() == Some(path) {
                    in_dir.push(file.file_name().unwrap().to_string_lossy().into());
                }
            }
            Ok(in_dir)
        })
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        Ok(FILES.with(|files| files.borrow().contains_key(Path::new(path))))
    }
    fn file_write_all(&self, path: &Path, contents: &[u8]) -> Result<(), String> {
        drop_file(path.to_path_buf(), contents.to_vec());
        Ok(())
    }
    fn file_read_all(&self, path: &Path) -> Result<Vec<u8>, String> {
        FILES.with(|files| {
            (files.borrow().get(path).cloned())
                .ok_or_else(|| format!("File not found: {}", path.display()))
        })
    }
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(OutputItem::Audio(wav_bytes));
        Ok(())
    }
    fn stream_audio(&self, mut f: uiua::AudioStreamFn) -> Result<(), String> {
        let mut samples = Vec::new();
        let mut t = 0.0;
        const SAMPLE_RATE: u32 = 44100;
        const SAMPLES_PER_FRAME: usize = 10000;
        let mut times = Vec::with_capacity(SAMPLES_PER_FRAME);
        let ast_time = get_ast_time();
        while t <= ast_time {
            times.clear();
            for _ in 0..SAMPLES_PER_FRAME {
                times.push(t);
                t += 1.0 / SAMPLE_RATE as f64;
            }
            match f(&times) {
                Ok(s) => samples.extend(s),
                Err(err) => return Err(format!("{err}")),
            }
        }
        let bytes = uiua::stereo_to_wave_bytes(
            &samples,
            |s| (s * i16::MAX as f64) as i16,
            16,
            hound::SampleFormat::Int,
            SAMPLE_RATE,
        )?;
        self.play_audio(bytes)
    }
    fn set_clipboard(&self, contents: &str) -> Result<(), String> {
        _ = window()
            .navigator()
            .clipboard()
            .unwrap()
            .write_text(contents);
        Ok(())
    }
    fn sleep(&self, seconds: f64) -> Result<(), String> {
        let start = instant::now();
        while (instant::now() - start) / 1000.0 < seconds {}
        Ok(())
    }
    fn load_git_module(&self, url: &str, branch: Option<&str>) -> Result<PathBuf, String> {
        if branch.is_some() {
            return Err("Git branch specification is not supported in the web backend".into());
        }
        let mut parts = url.rsplitn(3, '/');
        let repo_name = parts.next().ok_or("Invalid git url")?;
        let repo_owner = parts.next().ok_or("Invalid git url")?;
        let path = Path::new("uiua-modules")
            .join(repo_owner)
            .join(repo_name)
            .join("lib.ua");
        if FILES.with(|dropped_files| dropped_files.borrow().contains_key(&path)) {
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
        logging::log!("url: {url}");
        let res = try_fetch_sync(&url);
        logging::log!("res: {res:?}");
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

struct FetchReq {
    src: String,
    resouce: Resource<(), Result<String, String>>,
}

// M ~ "git: https://github.com/uiua-lang/example_module"

pub fn try_fetch_sync(src: &str) -> Option<Result<String, String>> {
    REQ.with(|req| {
        let mut req = req.borrow_mut();
        if let Some(req) = req.as_mut().filter(|req| req.src == src) {
            req.resouce.get()
        } else {
            let src = src.to_string();
            *req = Some(FetchReq {
                src: src.clone(),
                resouce: create_resource(
                    || (),
                    move |_| {
                        let src = src.clone();
                        async move { fetch(&src).await }
                    },
                ),
            });
            None
        }
    })
}

pub async fn fetch(src: &str) -> Result<String, String> {
    let mut opts = RequestInit::new();
    opts.method("GET");
    opts.mode(RequestMode::Cors);
    let request = Request::new_with_str_and_init(src, &opts).map_err(|e| format!("{e:?}"))?;
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
