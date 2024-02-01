#![allow(clippy::missing_safety_doc)]

#[no_mangle]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
pub unsafe extern "C" fn strlen(s: *const u8) -> i32 {
    let mut len = 0;
    while unsafe { *s.offset(len) } != 0 {
        len += 1;
    }
    len as i32
}

#[no_mangle]
pub unsafe extern "C" fn reverse(list: *mut i32, len: i32) {
    std::slice::from_raw_parts_mut(list, len as usize).reverse();
}

#[no_mangle]
pub unsafe extern "C" fn reversed(list: *const i32, len: i32) -> *const i32 {
    let slice = std::slice::from_raw_parts(list, len as usize);
    let mut reversed = slice.to_vec();
    reversed.reverse();
    reversed.as_ptr()
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
