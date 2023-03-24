use std::io::Cursor;

use rand::prelude::*;
use uiua::{Env, IoBackend};

pub struct WebBackend {
    pub stdout: String,
    rng: SmallRng,
    pub image_bytes: Option<Vec<u8>>,
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: String::new(),
            rng: SmallRng::seed_from_u64(instant::now().to_bits()),
            image_bytes: None,
        }
    }
}

impl IoBackend for WebBackend {
    fn print_str(&mut self, s: &str) {
        self.stdout.push_str(s);
    }
    fn rand(&mut self) -> f64 {
        self.rng.gen()
    }
    fn show_image(&mut self, image: image::DynamicImage, env: &Env) -> uiua::RuntimeResult {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageOutputFormat::Png)
            .map_err(|e| env.error(format!("Failed to show image: {}", e)))?;
        self.image_bytes = Some(bytes.into_inner());
        Ok(())
    }
}
