use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    io::Cursor,
    path::{Path, PathBuf},
    sync::Mutex,
};

use leptos::*;
use uiua::{example_ua, Report, SysBackend};

use crate::{editor::get_ast_time, weewuh};

pub struct WebBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
    pub files: Mutex<HashMap<PathBuf, Vec<u8>>>,
}

thread_local! {
    static DROPPED_FILES: RefCell<HashMap<PathBuf, Vec<u8>>> = RefCell::new([
        (PathBuf::from("example.ua"),
        example_ua(|ex| ex.clone()).into())
    ].into());
}

pub fn drop_file(path: PathBuf, contents: Vec<u8>) {
    DROPPED_FILES.with(|dropped_files| {
        dropped_files.borrow_mut().insert(path, contents);
    });
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: Vec::new().into(),
            stderr: String::new().into(),
            trace: String::new().into(),
            files: DROPPED_FILES
                .with(|dropped_files| dropped_files.borrow().clone())
                .into(),
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
        let files = self.files.lock().unwrap();
        let mut in_dir = Vec::new();
        for file in files.keys() {
            if file.parent() == Some(path) {
                in_dir.push(file.file_name().unwrap().to_string_lossy().into());
            }
        }
        Ok(in_dir)
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        Ok(self.files.lock().unwrap().contains_key(Path::new(path)))
    }
    fn file_write_all(&self, path: &Path, contents: &[u8]) -> Result<(), String> {
        (self.files.lock().unwrap()).insert(path.into(), contents.to_vec());
        Ok(())
    }
    fn file_read_all(&self, path: &Path) -> Result<Vec<u8>, String> {
        (self.files.lock().unwrap().get(path).cloned())
            .ok_or_else(|| format!("File not found: {}", path.display()))
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
    fn sleep(&self, seconds: f64) -> Result<(), String> {
        let start = instant::now();
        while (instant::now() - start) / 1000.0 < seconds {}
        Ok(())
    }
}
