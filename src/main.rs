use uiua::interpret::Interpretter;

fn main() {
    let path = "test.uiua";
    let mut rt = Interpretter::new();
    if let Err(e) = rt.run_file(path) {
        eprintln!("{e}");
    }
}
