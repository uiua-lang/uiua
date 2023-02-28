use uiua::Runtime;

fn main() {
    let path = "test.uiua";
    let mut rt = Runtime::new();
    if let Err(e) = rt.load_file(path) {
        eprintln!("{e}");
    }
    println!("{}", rt.lua_code());
    if let Err(e) = rt.run() {
        eprintln!("{e}");
    }
}
