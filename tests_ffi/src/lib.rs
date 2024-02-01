#[no_mangle]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
/// # Safety
/// `s` must be a valid pointer to a null-terminated string.
pub unsafe extern "C" fn strlen(s: *const u8) -> i32 {
    let mut len = 0;
    while unsafe { *s.offset(len) } != 0 {
        len += 1;
    }
    len as i32
}

#[test]
fn ffi_test() {
    use std::{path::Path, process::Command};

    use uiua::*;

    Command::new("cargo")
        .args(["build", "-p", "tests_ffi", "--lib"])
        .status()
        .unwrap();

    let lib_path =
        Path::new("../target/debug/ffi_lib").with_extension(std::env::consts::DLL_EXTENSION);

    let mut uiua = Uiua::with_native_sys().with_args(vec![lib_path.to_string_lossy().into_owned()]);
    uiua.run_file("test.ua").unwrap_or_else(|e| panic!("{e}"));
}
