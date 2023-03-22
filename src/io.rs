use std::{
    env, fs,
    io::{stdin, stdout, BufRead, Write},
};

use crate::{vm::Env, RuntimeResult};

#[allow(unused_variables)]
pub trait IoBackend {
    fn print_str(&mut self, s: &str);
    fn scan_line(&mut self) -> String {
        String::new()
    }
    fn print_str_ln(&mut self, s: &str) {
        self.print_str(s);
        self.print_str("\n");
    }
    fn var(&mut self, name: &str) -> Option<String> {
        None
    }
    fn args(&mut self) -> Vec<String> {
        Vec::new()
    }
    fn file_exists(&self, path: &str) -> bool {
        false
    }
    fn list_dir(&self, path: &str, env: &Env) -> RuntimeResult<Vec<String>> {
        Err(env.error("File IO not supported in this environment"))
    }
    fn is_file(&self, path: &str, env: &Env) -> RuntimeResult<bool> {
        Err(env.error("File IO not supported in this environment"))
    }
    fn read_file(&mut self, path: &str, env: &Env) -> RuntimeResult<Vec<u8>> {
        Err(env.error("File IO not supported in this environment"))
    }
    fn write_file(&mut self, path: &str, contents: Vec<u8>, env: &Env) -> RuntimeResult {
        Err(env.error("File IO not supported in this environment"))
    }
    fn read_file_string(&mut self, path: &str, env: &Env) -> RuntimeResult<String> {
        String::from_utf8(self.read_file(path, env)?).map_err(|e| env.error(e.to_string()))
    }
    fn write_file_string(&mut self, path: &str, contents: String, env: &Env) -> RuntimeResult {
        self.write_file(path, contents.into_bytes(), env)
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
    fn file_exists(&self, path: &str) -> bool {
        fs::metadata(path).is_ok()
    }
    fn is_file(&self, path: &str, env: &Env) -> RuntimeResult<bool> {
        fs::metadata(path)
            .map(|m| m.is_file())
            .map_err(|e| env.error(e.to_string()))
    }
    fn list_dir(&self, path: &str, env: &Env) -> RuntimeResult<Vec<String>> {
        let mut paths = Vec::new();
        for entry in fs::read_dir(path).map_err(|e| env.error(e.to_string()))? {
            let entry = entry.map_err(|e| env.error(e.to_string()))?;
            paths.push(entry.path().to_string_lossy().into());
        }
        Ok(paths)
    }
    fn read_file(&mut self, path: &str, env: &Env) -> RuntimeResult<Vec<u8>> {
        fs::read(path).map_err(|e| env.error(e.to_string()))
    }
    fn write_file(&mut self, path: &str, contents: Vec<u8>, env: &Env) -> RuntimeResult {
        fs::write(path, contents).map_err(|e| env.error(e.to_string()))
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
