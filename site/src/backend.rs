use std::{
    any::Any,
    collections::HashMap,
    io::Cursor,
    sync::{
        atomic::{AtomicU64, Ordering},
        Mutex,
    },
};

use leptos::*;
use uiua::{value::Value, Handle, Report, SysBackend, Uiua, UiuaError, UiuaResult};

pub struct WebBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
    pub files: Mutex<HashMap<String, Vec<u8>>>,
    next_thread_id: AtomicU64,
    thread_results: Mutex<HashMap<Handle, UiuaResult<Vec<Value>>>>,
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: Vec::new().into(),
            stderr: String::new().into(),
            trace: String::new().into(),
            files: HashMap::new().into(),
            next_thread_id: 0.into(),
            thread_results: HashMap::new().into(),
        }
    }
}

pub enum OutputItem {
    String(String),
    Image(Vec<u8>),
    Gif(Vec<u8>),
    Audio(Vec<u8>),
    Report(Report),
    Separator,
}

impl SysBackend for WebBackend {
    fn any(&self) -> &dyn Any {
        self
    }
    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
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
    fn file_write_all(&self, path: &str, contents: &[u8]) -> Result<(), String> {
        self.files
            .lock()
            .unwrap()
            .insert(path.to_string(), contents.to_vec());
        Ok(())
    }
    fn file_read_all(&self, path: &str) -> Result<Vec<u8>, String> {
        self.files
            .lock()
            .unwrap()
            .get(path)
            .cloned()
            .ok_or_else(|| format!("File not found: {path}"))
    }
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        self.stdout
            .lock()
            .unwrap()
            .push(OutputItem::Audio(wav_bytes));
        Ok(())
    }
    fn stream_audio(&self, mut f: uiua::AudioStreamFn) -> Result<(), String> {
        let mut samples = Vec::new();
        let mut t = 0.0;
        const SAMPLE_RATE: u32 = 44100;
        const SAMPLES_PER_FRAME: usize = 10000;
        let mut times = Vec::with_capacity(SAMPLES_PER_FRAME);
        while t <= 30.0 {
            times.clear();
            for _ in 0..SAMPLES_PER_FRAME {
                times.push(t);
                t += 1.0 / SAMPLE_RATE as f64;
            }
            match f(&times) {
                Ok(s) => samples.extend(s),
                Err(UiuaError::Break(0, _)) => break,
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
    fn spawn(
        &self,
        env: Uiua,
        f: Box<dyn FnOnce(&mut Uiua) -> UiuaResult + Send>,
    ) -> Result<Handle, String> {
        let handle = Handle(self.next_thread_id.fetch_add(1, Ordering::SeqCst));
        let mut env = env.clone();
        let res = f(&mut env).map(|_| env.take_stack());
        self.thread_results.lock().unwrap().insert(handle, res);
        Ok(handle)
    }
    fn wait(&self, handle: Handle) -> Result<Vec<Value>, Result<UiuaError, String>> {
        match self.thread_results.lock().unwrap().remove(&handle) {
            Some(Ok(stack)) => Ok(stack),
            Some(Err(err)) => Err(Ok(err)),
            None => Err(Err("Invalid thread handle".into())),
        }
    }
}
