#![allow(clippy::missing_safety_doc)]

use std::ffi::*;

#[no_mangle]
pub extern "C" fn add(a: c_int, b: c_int) -> c_int {
    a + b
}

#[no_mangle]
pub unsafe extern "C" fn strlen(s: *const c_char) -> c_int {
    // println!("s ptr: {:p}", s);
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
pub unsafe extern "C" fn reversed(list: *const c_int, len: c_int) -> *const c_int {
    let slice = std::slice::from_raw_parts(list, len as usize);
    let mut reversed = slice.to_vec();
    reversed.reverse();
    reversed.leak().as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn head_tail(list: *mut c_int, len: *mut c_int) -> c_int {
    // println!("list ptr b: {:p}", list);
    // println!("len ptr b: {:p}", len);
    // println!("len: {}", *len);
    // println!("first: {}", *list);
    // println!("second: {}", *list.offset(1));
    // println!("third: {}", *list.offset(2));
    let slice = std::slice::from_raw_parts_mut(list, *len as usize);
    let head = slice[0];
    slice.rotate_left(1);
    *len -= 1;
    head
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
pub unsafe fn change_string(s: *mut *const c_char) {
    let new_str = CString::new("Hello, World!").unwrap();
    *s = new_str.into_raw();
}

#[no_mangle]
pub unsafe fn change_string_to_sum(a: c_int, b: c_int, s: *mut *const c_char) {
    let sum = a + b;
    let new_str = CString::new(format!("{} + {} = {}", a, b, sum)).unwrap();
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
    // println!("v: {:?}", v);
    // println!("v.x bytes: {:?}", v.x.to_ne_bytes());
    // println!("v.y bytes: {:?}", v.y.to_ne_bytes());
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
    // println!("name ptr: {:p}", name);
    // println!("age: {}", age);
    Person { name, age }
}

#[no_mangle]
pub unsafe extern "C" fn person_new_ptr(name: *const c_char, age: c_int) -> *const Person {
    // println!("name ptr: {:p}", name);
    // println!("age: {}", age);
    Box::into_raw(Box::new(Person { name, age }))
}

#[no_mangle]
pub unsafe extern "C" fn person_val_age(p: Person) -> c_int {
    // println!("p ptr: {:p}", p);
    p.age
}

#[no_mangle]
pub unsafe extern "C" fn person_ptr_age(p: *const Person) -> c_int {
    // println!("p ptr: {:p}", p);
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

#[repr(C)]
pub struct TwoPeoplePtrs {
    pub a: *const Person,
    pub b: *const Person,
}

#[no_mangle]
pub unsafe extern "C" fn two_people_new_ptr(a: *const Person, b: *const Person) -> TwoPeoplePtrs {
    TwoPeoplePtrs { a, b }
}

#[repr(C)]
pub struct TwoInts {
    pub a: *const c_int,
    pub b: *const c_int,
}

#[no_mangle]
pub unsafe extern "C" fn two_ints_new(a: *const c_int, b: *const c_int) -> TwoInts {
    TwoInts { a, b }
}

#[no_mangle]
pub unsafe extern "C" fn array_ptr(arr: *const c_int, len: c_int) -> *const c_int {
    let copied = std::slice::from_raw_parts(arr, len as usize).to_vec();
    copied.leak().as_ptr()
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
