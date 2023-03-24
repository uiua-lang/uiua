use rand::prelude::*;
use uiua::IoBackend;

pub struct WebBackend {
    pub stdout: String,
    rng: SmallRng,
}

impl Default for WebBackend {
    fn default() -> Self {
        Self {
            stdout: String::new(),
            rng: SmallRng::seed_from_u64(instant::now().to_bits()),
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
}
