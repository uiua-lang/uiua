use uiua::vm::Vm;

fn main() {
    #[cfg(feature = "profile")]
    {
        puffin::set_scopes_on(true);
        let server_addr = format!("0.0.0.0:{}", puffin_http::DEFAULT_PORT);
        Box::leak(Box::new(puffin_http::Server::new(&server_addr).unwrap()));
    }

    let path = "test.uiua";
    let mut vm = Vm::new();
    if let Err(e) = vm.run_file(path) {
        eprintln!("{e}");
    }
}
