use uiua::{now, BigConstant, GitTarget, Handle, SysBackend, Uiua};
use uiua_pad_shared::SerializableOutputItem;

use std::{
    io::Cursor,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

pub struct WebBackend {
    pub stdout: Mutex<Vec<SerializableOutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
    output_enabled: AtomicBool,
}

impl WebBackend {
    pub fn new() -> Self {
        Self {
            stdout: Mutex::new(Vec::new()),
            stderr: Mutex::new(String::new()),
            trace: Mutex::new(String::new()),
            output_enabled: AtomicBool::new(true),
        }
    }
}

impl Default for WebBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl SysBackend for WebBackend {
    fn any(&self) -> &dyn std::any::Any {
        self
    }

    fn any_mut(&mut self) -> &mut dyn std::any::Any {
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

        let mut stdout = self.stdout.lock().unwrap();
        let mut lines = s.lines();
        let Some(first) = lines.next() else {
            return Ok(());
        };

        if let Some(SerializableOutputItem::String(prev)) = stdout.last_mut() {
            prev.push_str(first);
        } else {
            stdout.push(SerializableOutputItem::String(first.into()));
        }

        for line in lines {
            stdout.push(SerializableOutputItem::String(line.into()));
        }

        if s.ends_with('\n') {
            stdout.push(SerializableOutputItem::String("".into()));
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
        // TODO: Implement prompt via JS interop
        return Ok(None);
    }

    fn file_exists(&self, _path: &str) -> bool {
        false
    }

    fn list_dir(&self, _path: &str) -> Result<Vec<String>, String> {
        Err("Listing directories is not supported in this environment".into())
    }

    fn is_file(&self, _path: &str) -> Result<bool, String> {
        Err("Checking if a path is a file is not supported in this environment".into())
    }

    fn delete(&self, _path: &str) -> Result<(), String> {
        Err("Deleting files is not supported in this environment".into())
    }

    fn trash(&self, _path: &str) -> Result<(), String> {
        Err("Trashing files is not supported in this environment".into())
    }

    fn read(&self, _handle: Handle, _count: usize) -> Result<Vec<u8>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }

    fn read_all(&self, _handle: Handle) -> Result<Vec<u8>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }

    fn read_lines<'a>(&self, _handle: Handle) -> Result<uiua::ReadLinesReturnFn<'a>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }

    fn write(&self, _handle: Handle, _contents: &[u8]) -> Result<(), String> {
        Err("Writing to streams is not supported in this environment".into())
    }

    fn seek(&self, _handle: Handle, _offset: uiua::StreamSeek) -> Result<(), String> {
        Err("Seeking streams is not supported in this environment".into())
    }

    fn create_file(&self, _path: &std::path::Path) -> Result<Handle, String> {
        Err("Creating files is not supported in this environment".into())
    }

    fn open_file(&self, _path: &std::path::Path, _write: bool) -> Result<Handle, String> {
        Err("Opening files is not supported in this environment".into())
    }

    fn make_dir(&self, _path: &std::path::Path) -> Result<(), String> {
        Err("Creating directories is not supported in this environment".into())
    }

    fn clipboard(&self) -> Result<String, String> {
        Err("Getting the clipboard is not supported in this environment".into())
    }

    fn set_clipboard(&self, _contents: &str) -> Result<(), String> {
        Err("Setting the clipboard is not supported in this environment".into())
    }

    fn sleep(&self, seconds: f64) -> Result<(), String> {
        let start = now();
        while now() - start < seconds {}
        Ok(())
    }

    fn allow_thread_spawning(&self) -> bool {
        true
    }

    fn show_image(&self, image: image::DynamicImage, label: Option<&str>) -> Result<(), String> {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageFormat::Png)
            .map_err(|e| format!("Failed to show image: {e}"))?;
        self.stdout
            .lock()
            .unwrap()
            .push(SerializableOutputItem::Image {
                data: bytes.into_inner(),
                label: label.map(Into::into),
            });
        Ok(())
    }

    fn show_gif(&self, gif_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(SerializableOutputItem::Gif {
            data: gif_bytes,
            label: label.map(Into::into),
        });
        Ok(())
    }

    fn show_apng(&self, apng_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(SerializableOutputItem::Apng {
            data: apng_bytes,
            label: label.map(Into::into),
        });
        Ok(())
    }

    fn play_audio(&self, wav_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(SerializableOutputItem::Audio {
            data: wav_bytes,
            label: label.map(Into::into),
        });
        Ok(())
    }

    fn stream_audio(&self, _f: uiua::AudioStreamFn) -> Result<(), String> {
        Err("Streaming audio is not supported in this environment".into())
    }

    fn now(&self) -> f64 {
        now()
    }

    fn close(&self, _handle: Handle) -> Result<(), String> {
        Ok(())
    }

    fn change_directory(&self, _path: &str) -> Result<(), String> {
        Err("Changing directories is not supported in this environment".into())
    }

    fn get_current_directory(&self) -> Result<String, String> {
        Err("Getting the current directory is not supported in this environment".into())
    }

    fn load_git_module(
        &self,
        _url: &str,
        _target: GitTarget,
    ) -> Result<std::path::PathBuf, String> {
        Err("Loading git modules is not supported in this environment".into())
    }

    fn breakpoint(&self, _env: &Uiua) -> Result<bool, String> {
        Err("Breakpoints are not supported in this environment".into())
    }

    fn big_constant(&self, _key: BigConstant) -> Result<std::borrow::Cow<'static, [u8]>, String> {
        Err("Retrieval of big constants is not supported in this environment".into())
    }
}
