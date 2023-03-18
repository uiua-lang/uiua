use std::{fs, path::PathBuf, process::exit};

use clap::Parser;
use uiua::{compile::Compiler, parse::format_file, UiuaResult};

fn main() {
    #[cfg(feature = "profile")]
    {
        puffin::set_scopes_on(true);
        let server_addr = format!("0.0.0.0:{}", puffin_http::DEFAULT_PORT);
        Box::leak(Box::new(puffin_http::Server::new(&server_addr).unwrap()));
    }
    if let Err(e) = run() {
        eprintln!("{e}");
        exit(1);
    }
}

fn run() -> UiuaResult {
    let app = App::parse();
    if let Some(command) = app.command {
        match command {
            Command::Fmt { path } => {
                if let Some(path) = path {
                    format_file(path)?;
                } else {
                    for path in uiua_files() {
                        format_file(path)?;
                    }
                }
            }
        }
    } else {
        let path = if let Some(path) = app.path {
            path
        } else {
            let mut uiua_files = uiua_files();
            match uiua_files.len() {
                0 => {
                    eprintln!("No .ua files found in current directory");
                    exit(1);
                }
                1 => uiua_files.remove(0),
                _ => {
                    eprintln!("Multiple .ua files found in current directory");
                    exit(1);
                }
            }
        };
        let _ = format_file(&path);
        let mut compiler = Compiler::new();
        compiler.load_file(&path)?;
        let assembly = compiler.finish();
        assembly.run()?;
    }
    Ok(())
}

#[derive(Parser)]
struct App {
    #[clap(
        help = "Path to the uiua file to run",
        long_help = "Path to the uiua file to run. This can be ommitted if there is \
                     only one uiua file in the current directory."
    )]
    path: Option<PathBuf>,
    #[clap(subcommand)]
    command: Option<Command>,
}

#[derive(Parser)]
enum Command {
    #[clap(
        about = "Format a uiua file or all files in the current directory",
        long_about = "Format a uiua file or all files in the current directory, \
                      replacing named primitives with their unicode equivalents"
    )]
    Fmt { path: Option<PathBuf> },
}

fn uiua_files() -> Vec<PathBuf> {
    fs::read_dir(".")
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "ua"))
        .map(|entry| entry.path())
        .collect()
}
