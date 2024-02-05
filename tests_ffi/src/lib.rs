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
    reversed.leak().as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn head_tail(list: *mut i32, len: *mut i32) -> i32 {
    let slice = std::slice::from_raw_parts_mut(list, *len as usize);
    let head = slice[0];
    *len -= 1;
    slice.rotate_left(1);
    head
}

#[test]
fn ffi_test() {
    use std::{path::Path, process::Command};

    use uiua::*;

    Command::new("cargo")
        .args(["build", "-p", "tests_ffi", "--lib"])
        .status()
        .unwrap();

    #[cfg(windows)]
    let dll_path = "../target/debug/ffi_lib.dll";
    #[cfg(unix)]
    let dll_path = "../target/debug/libffi_lib.so";
    let lib_path = Path::new(dll_path);

    let mut uiua = Uiua::with_native_sys().with_args(vec![lib_path.to_string_lossy().into_owned()]);
    uiua.run_file("test.ua").unwrap_or_else(|e| panic!("{e}"));
}
