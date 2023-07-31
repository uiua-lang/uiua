use std::{cell::RefCell, collections::HashMap, io::Cursor};

use uiua::{Handle, IoBackend};

pub struct WebBackend {
    pub stdout: RefCell<String>,
    pub stderr: RefCell<String>,
    pub image_bytes: RefCell<Option<Vec<u8>>>,
    pub audio_bytes: RefCell<Option<Vec<u8>>>,
    pub files: RefCell<HashMap<String, Vec<u8>>>,
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: String::new().into(),
            stderr: String::new().into(),
            image_bytes: None.into(),
            audio_bytes: None.into(),
            files: HashMap::from([(
                "example.ua".into(),
                "\
Square ← ×.
Double ← +.
Increment ← +1
Square_Double_Increment
                "
                .into(),
            )])
            .into(),
        }
    }
}

impl IoBackend for WebBackend {
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        match handle {
            Handle::STDOUT => {
                self.stdout
                    .borrow_mut()
                    .push_str(&String::from_utf8_lossy(contents));
                Ok(())
            }
            Handle::STDERR => {
                self.stderr
                    .borrow_mut()
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
        *self.image_bytes.borrow_mut() = Some(bytes.into_inner());
        Ok(())
    }
    fn file_write_all(&self, path: &str, contents: &[u8]) -> Result<(), String> {
        self.files
            .borrow_mut()
            .insert(path.to_string(), contents.to_vec());
        Ok(())
    }
    fn file_read_all(&self, path: &str) -> Result<Vec<u8>, String> {
        self.files
            .borrow()
            .get(path)
            .cloned()
            .ok_or_else(|| format!("File not found: {path}"))
    }
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        *self.audio_bytes.borrow_mut() = Some(wav_bytes);
        Ok(())
    }
    fn sleep(&self, ms: f64) -> Result<(), String> {
        let start = instant::now();
        while instant::now() - start < ms {}
        Ok(())
    }
}
