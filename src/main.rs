use uiua::compile::Compiler;

fn main() {
    #[cfg(feature = "profile")]
    {
        puffin::set_scopes_on(true);
        let server_addr = format!("0.0.0.0:{}", puffin_http::DEFAULT_PORT);
        Box::leak(Box::new(puffin_http::Server::new(&server_addr).unwrap()));
    }

    let mut compiler = Compiler::new();
    if let Err(e) = compiler.load_file("test.uiua") {
        eprintln!("{e}");
    }
    let assembly = compiler.finish();
    if let Err(e) = assembly.run() {
        eprintln!("{e}");
    }
}
