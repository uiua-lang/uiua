#![allow(clippy::missing_safety_doc)]

use std::ffi::*;

#[no_mangle]
pub extern "C" fn add(a: c_int, b: c_int) -> c_int {
    a + b
}

#[no_mangle]
pub unsafe extern "C" fn strlen(s: *const c_char) -> c_int {
    let mut len = 0;
    while unsafe { *s.offset(len) } != 0 {
        len += 1;
    }
    len as c_int
}

#[no_mangle]
pub unsafe extern "C" fn reverse(list: *mut c_int, len: c_int) {
    std::slice::from_raw_parts_mut(list, len as usize).reverse();
}

#[no_mangle]
pub unsafe extern "C" fn multi_list(
    _a: *mut c_int,
    _a_len: c_int,
    _b: *mut c_int,
    _b_len: c_int,
    c: *mut c_int,
    c_len: c_int,
) {
    for i in 0..c_len {
        *c.offset(i as isize) = i;
    }
}

#[no_mangle]
pub unsafe extern "C" fn change_string(s: *mut *const c_char) {
    let new_str = CString::new("Hello, World!").unwrap();
    *s = new_str.into_raw();
}

#[no_mangle]
pub unsafe extern "C" fn change_string_to_sum(a: c_int, b: c_int, s: *mut *const c_char) {
    let sum = a + b;
    let new_str = CString::new(format!("{a} + {b} = {sum}")).unwrap();
    *s = new_str.into_raw();
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Vec2 {
    pub x: f64,
    pub y: f64,
}

#[no_mangle]
pub unsafe extern "C" fn vec2_len(v: Vec2) -> f64 {
    (v.x * v.x + v.y * v.y).sqrt()
}

#[no_mangle]
pub unsafe extern "C" fn vec2_len_ref(v: *const Vec2) -> f64 {
    let v = &*v;
    (v.x * v.x + v.y * v.y).sqrt()
}

#[no_mangle]
pub unsafe extern "C" fn vec2_add(a: Vec2, b: Vec2) -> Vec2 {
    Vec2 {
        x: a.x + b.x,
        y: a.y + b.y,
    }
}

#[no_mangle]
pub unsafe extern "C" fn vec2_normalize(v: *mut Vec2) {
    let v = &mut *v;
    let len = (v.x * v.x + v.y * v.y).sqrt();
    if len == 0.0 {
        v.x = 0.0;
        v.y = 0.0;
    } else {
        v.x /= len;
        v.y /= len;
    }
}

#[no_mangle]
pub unsafe extern "C" fn vec2_list_sum(list: *const Vec2, len: c_int) -> Vec2 {
    let slice = std::slice::from_raw_parts(list, len as usize);
    slice.iter().fold(Vec2 { x: 0.0, y: 0.0 }, |acc, v| Vec2 {
        x: acc.x + v.x,
        y: acc.y + v.y,
    })
}

#[no_mangle]
pub unsafe extern "C" fn vec2_list_reverse(list: *mut Vec2, len: c_int) {
    let slice = std::slice::from_raw_parts_mut(list, len as usize);
    slice.reverse();
}

#[repr(C)]
pub struct Person {
    pub name: *const c_char,
    pub age: c_int,
}

#[no_mangle]
pub unsafe extern "C" fn person_new(name: *const c_char, age: c_int) -> Person {
    Person { name, age }
}

#[no_mangle]
pub unsafe extern "C" fn person_children(name: *const c_char, age: c_int) -> *const Person {
    let name = CStr::from_ptr(name).to_str().unwrap().to_string();
    let mut children = Vec::new();
    children.push(Person {
        name: CString::new(format!("{name} Jr.")).unwrap().into_raw(),
        age: age - 25,
    });
    children.push(Person {
        name: CString::new(format!("{name}ina")).unwrap().into_raw(),
        age: age - 27,
    });
    children.leak() as *mut _ as *const _
}

#[no_mangle]
pub unsafe extern "C" fn person_val_age(p: Person) -> c_int {
    p.age
}

#[no_mangle]
pub unsafe extern "C" fn person_ptr_age(p: *const Person) -> c_int {
    (*p).age
}

#[repr(C)]
pub struct TwoPeople {
    pub a: Person,
    pub b: Person,
}

#[no_mangle]
pub unsafe extern "C" fn two_people_new(a: Person, b: Person) -> TwoPeople {
    TwoPeople { a, b }
}

#[no_mangle]
pub unsafe extern "C" fn array_ptr(arr: *const c_int, len: c_int) -> *const c_int {
    let copied = std::slice::from_raw_parts(arr, len as usize).to_vec();
    copied.leak().as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn first_byte(arr: *const c_uchar) -> c_uchar {
    *arr
}

#[no_mangle]
pub unsafe extern "C" fn dummy_md5(
    m: *const c_uchar,
    len: c_int,
    out: *mut c_uchar,
) -> *const c_uchar {
    if len == 0 {
        return std::ptr::null();
    }
    let len = len as isize;
    if out.is_null() {
        let new_out = vec![0; 16].leak().as_mut_ptr();
        for i in 0..len.min(16) {
            *new_out.offset(i) = *m.offset(i);
        }
        new_out
    } else {
        for i in 0..len.min(16) {
            *out.offset(i) = *m.offset(i);
        }
        out
    }
}

#[repr(C)]
pub struct VoidStruct {
    pub a: *const c_void,
    pub b: *const c_void,
}

#[no_mangle]
pub extern "C" fn make_void_struct(_: c_int) -> VoidStruct {
    VoidStruct {
        a: std::ptr::null(),
        b: std::ptr::null(),
    }
}

#[no_mangle]
pub extern "C" fn make_void_struct_a(a: c_int) -> VoidStruct {
    VoidStruct {
        a: Box::into_raw(Box::new(a)) as *const c_void,
        b: std::ptr::null(),
    }
}

#[repr(C)]
pub struct Bytes {
    pub bytes: *const c_uchar,
}

#[no_mangle]
pub unsafe extern "C" fn bytes_first_byte(bytes: Bytes) -> c_uchar {
    first_byte(bytes.bytes)
}

#[no_mangle]
pub unsafe extern "C" fn pointerify(i: c_int) -> *const c_int {
    Box::leak(Box::new(i))
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
    #[cfg(all(unix, not(target_os = "macos")))]
    let dll_path = "../target/debug/libffi_lib.so";
    #[cfg(target_os = "macos")]
    let dll_path = "../target/debug/libffi_lib.dylib";
    let lib_path = Path::new(dll_path);

    let mut uiua = Uiua::with_native_sys().with_args(vec![lib_path.to_string_lossy().into_owned()]);
    uiua.run_file("test.ua").unwrap_or_else(|e| panic!("{e}"));
}
