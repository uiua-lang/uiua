use std::{env, fs, io, path::Path};

use once_cell::sync::Lazy;

use crate::{Assembly, Compiler, NativeSys, UiuaResult};

const STAND_DATA_SIGNATURE: &[u8] = b"Uiua standalone";

pub fn build_exe(root: &Path) -> UiuaResult<Vec<u8>> {
    let asm = Compiler::with_backend(NativeSys).load_file(root)?.finish();
    // Serialize the files
    let asm_bytes = asm.to_uasm().into_bytes();
    // Append the files to the current exe
    let mut bytes = env::current_exe()
        .and_then(fs::read)
        .unwrap_or_else(|e| panic!("Unable to read current exe: {e}"));
    bytes.extend_from_slice(&asm_bytes);
    // Append the length of the serialized files and a signature
    bytes.extend((asm_bytes.len() as u64).to_le_bytes());
    bytes.extend(STAND_DATA_SIGNATURE);
    Ok(bytes)
}

fn load_asm() -> io::Result<Option<Assembly>> {
    // Read the current exe
    let bytes = fs::read(env::current_exe()?)?;
    // Check if it is a standalone exe
    let Some(bytes) = bytes.strip_suffix(STAND_DATA_SIGNATURE) else {
        return Ok(None);
    };
    // Get the length of the serialized files
    let (bytes, len_bytes) = bytes.split_at(bytes.len() - 8);
    let asm_len = u64::from_le_bytes(len_bytes.try_into().unwrap());
    let start = bytes.len() - asm_len as usize;
    // Deserialize the files
    let uasm: String = std::str::from_utf8(&bytes[start..]).unwrap().to_string();
    let asm = Assembly::from_uasm(&uasm).unwrap();
    Ok(Some(asm))
}

pub static STAND_ASM: Lazy<Option<Assembly>> = Lazy::new(|| load_asm().unwrap());
