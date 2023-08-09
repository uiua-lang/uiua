use std::{any::Any, collections::HashMap, io::Cursor, sync::Mutex};

use uiua::{Handle, SysBackend};

pub struct WebBackend {
    pub stdout: Mutex<String>,
    pub stderr: Mutex<String>,
    pub image_bytes: Mutex<Option<Vec<u8>>>,
    pub audio_bytes: Mutex<Option<Vec<u8>>>,
    pub files: Mutex<HashMap<String, Vec<u8>>>,
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: String::new().into(),
            stderr: String::new().into(),
            image_bytes: None.into(),
            audio_bytes: None.into(),
            files: HashMap::new().into(),
        }
    }
}

impl SysBackend for WebBackend {
    fn any(&self) -> &dyn Any {
        self
    }
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        match handle {
            Handle::STDOUT => {
                self.stdout
                    .lock()
                    .unwrap()
                    .push_str(&String::from_utf8_lossy(contents));
                Ok(())
            }
            Handle::STDERR => {
                self.stderr
                    .lock()
                    .unwrap()
                    .push_str(&String::from_utf8_lossy(contents));
                Ok(())
            }
            _ => Err("Only stdout and stderr are supported".into()),
        }
    }
    fn show_image(&self, image: image::DynamicImage) -> Result<(), String> {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageOutputFormat::Png)
            .map_err(|e| format!("Failed to show image: {e}"))?;
        *self.image_bytes.lock().unwrap() = Some(bytes.into_inner());
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
        *self.audio_bytes.lock().unwrap() = Some(wav_bytes);
        Ok(())
    }
    fn sleep(&self, ms: f64) -> Result<(), String> {
        let start = instant::now();
        while instant::now() - start < ms {}
        Ok(())
    }
}
