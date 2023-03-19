use std::{
    env,
    io::{stdin, stdout, BufRead, Write},
};

#[allow(unused_variables)]
pub trait IoBackend {
    fn print_str(&mut self, s: &str);
    fn scan_line(&mut self) -> String {
        String::new()
    }
    fn print_str_ln(&mut self, s: &str) {
        self.print_str(s);
        self.print_str("\r");
    }
    fn var(&mut self, name: &str) -> Option<String> {
        None
    }
    fn args(&mut self) -> Vec<String> {
        Vec::new()
    }
}

#[derive(Default)]
pub struct StdIo;

impl IoBackend for StdIo {
    fn print_str(&mut self, s: &str) {
        print!("{}", s);
        let _ = stdout().lock().flush();
    }
    fn scan_line(&mut self) -> String {
        stdin()
            .lock()
            .lines()
            .next()
            .and_then(Result::ok)
            .unwrap_or_default()
    }
    fn var(&mut self, name: &str) -> Option<String> {
        env::var(name).ok()
    }
    fn args(&mut self) -> Vec<String> {
        env::args().collect()
    }
}

#[derive(Default)]
pub struct PipedIo {
    pub buffer: String,
}

impl IoBackend for PipedIo {
    fn print_str(&mut self, s: &str) {
        self.buffer.push_str(s);
    }
}
