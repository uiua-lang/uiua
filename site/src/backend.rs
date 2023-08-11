use std::{
    any::Any,
    collections::HashMap,
    io::Cursor,
    sync::{
        atomic::{AtomicU64, Ordering},
        Mutex,
    },
};

use uiua::{value::Value, Handle, SysBackend, Uiua, UiuaError, UiuaResult};

pub struct WebBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub files: Mutex<HashMap<String, Vec<u8>>>,
    next_thread_id: AtomicU64,
    thread_results: Mutex<HashMap<Handle, UiuaResult<Vec<Value>>>>,
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: Vec::new().into(),
            stderr: String::new().into(),
            files: HashMap::new().into(),
            next_thread_id: 0.into(),
            thread_results: HashMap::new().into(),
        }
    }
}

pub enum OutputItem {
    String(String),
    Image(Vec<u8>),
    Audio(Vec<u8>),
    Error(String),
}

impl SysBackend for WebBackend {
    fn any(&self) -> &dyn Any {
        self
    }
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        match handle {
            Handle::STDOUT => {
                let s = String::from_utf8_lossy(contents);
                let mut stdout = self.stdout.lock().unwrap();
                for line in s.lines() {
                    stdout.push(OutputItem::String(line.into()));
                }
                Ok(())
            }
            Handle::STDERR => {
                self.stderr
                    .lock()
                    .unwrap()
                    .push_str(&String::from_utf8_lossy(contents));
                Ok(())
            }
            _ => Err("Only stdout and stderr are supported on the website".into()),
        }
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
    fn sleep(&self, ms: f64) -> Result<(), String> {
        let start = instant::now();
        while instant::now() - start < ms {}
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
