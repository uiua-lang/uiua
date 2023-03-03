use uiua::vm::Vm;

fn main() {
    let path = "test.uiua";
    let mut vm = Vm::new();
    if let Err(e) = vm.run_file(path) {
        eprintln!("{e}");
    }
}
