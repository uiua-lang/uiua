use std::{
    collections::BTreeMap,
    env, fs, io,
    path::{Path, PathBuf},
};

use once_cell::sync::Lazy;
use serde::*;

#[derive(Serialize, Deserialize)]
pub struct StandFiles {
    pub main: PathBuf,
    pub files: BTreeMap<PathBuf, String>,
}

impl Default for StandFiles {
    fn default() -> Self {
        Self {
            main: "main.ua".into(),
            files: BTreeMap::new(),
        }
    }
}

impl StandFiles {
    pub fn main_code(&self) -> Option<&str> {
        self.files.get(&self.main).map(|s| s.as_str())
    }
}

pub fn build_exe(root: &Path) -> io::Result<Vec<u8>> {
    let mut files = BTreeMap::new();
    fn collect(path: PathBuf, files: &mut BTreeMap<PathBuf, String>) -> io::Result<()> {
        if path.is_dir() {
            for entry in path.read_dir()? {
                collect(entry?.path(), files)?;
            }
        } else if path.is_file() && path.extension().is_some_and(|s| s == "ua") {
            let content = std::fs::read_to_string(path.clone())?;
            files.insert(path, content);
        }
        Ok(())
    }
    collect(root.into(), &mut files)?;
    let files = StandFiles {
        main: root.to_owned(),
        files,
    };
    let files_bytes = serde_json::to_vec(&files)?;
    let mut bytes = fs::read(env::current_exe()?)?;
    bytes.extend_from_slice(&files_bytes);
    bytes.extend((files_bytes.len() as u64).to_le_bytes());
    Ok(bytes)
}

fn load_files() -> io::Result<StandFiles> {
    let mut bytes = fs::read(env::current_exe()?)?;
    let files_len = u64::from_le_bytes(bytes.split_off(bytes.len() - 8).try_into().unwrap());
    let start = bytes.len() - files_len as usize;
    let files: StandFiles = serde_json::from_slice(&bytes[start..])?;
    Ok(files)
}

pub static STAND_FILES: Lazy<StandFiles> = Lazy::new(|| load_files().unwrap_or_default());
