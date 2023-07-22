use std::{cell::RefCell, io::Cursor};

use rand::prelude::*;
use uiua::{Handle, IoBackend};

pub struct WebBackend {
    pub stdout: RefCell<String>,
    pub stderr: RefCell<String>,
    rng: RefCell<SmallRng>,
    pub image_bytes: RefCell<Option<Vec<u8>>>,
    pub audio_bytes: RefCell<Option<Vec<u8>>>,
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: String::new().into(),
            stderr: String::new().into(),
            rng: SmallRng::seed_from_u64(instant::now().to_bits()).into(),
            image_bytes: None.into(),
            audio_bytes: None.into(),
        }
    }
}

impl IoBackend for WebBackend {
    fn rand(&self) -> f64 {
        self.rng.borrow_mut().gen()
    }
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
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        *self.audio_bytes.borrow_mut() = Some(wav_bytes);
        Ok(())
    }
}
