use uiua::IoBackend;

#[derive(Default)]
pub struct WebBackend {
    pub stdout: String,
}

impl IoBackend for WebBackend {
    fn print_str(&mut self, s: &str) {
        self.stdout.push_str(s);
    }
}
