use std::{
    fs,
    io::{stdout, Write},
    path::{Path, PathBuf},
    process::exit,
    sync::{Arc, Mutex},
    thread::sleep,
    time::Duration,
};

use clap::Parser;
use notify::{Event, EventKind, RecursiveMode, Watcher};
use uiua::{compile::Compiler, parse::format_file, UiuaResult};

fn main() {
    #[cfg(feature = "profile")]
    {
        puffin::set_scopes_on(true);
        let server_addr = format!("0.0.0.0:{}", puffin_http::DEFAULT_PORT);
        Box::leak(Box::new(puffin_http::Server::new(&server_addr).unwrap()));
    }
    let _ = ctrlc::set_handler(|| {
        clear_watching();
        exit(0)
    });

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
            Command::Watch => {
                let modify_paths = Arc::new(Mutex::new(Vec::new()));
                let modify_paths2 = modify_paths.clone();
                let mut watcher =
                    notify::recommended_watcher(move |res: notify::Result<Event>| match res {
                        Ok(event) => {
                            if !matches!(event.kind, EventKind::Modify(_)) {
                                return;
                            }
                            let Some(path) = event.paths.get(0) else {
                                return;
                            };
                            if path.extension().map_or(false, |ext| ext != "ua") {
                                return;
                            }
                            let Ok(mut guard) = modify_paths2.try_lock() else {
                                return;
                            };
                            guard.push(path.clone());
                        }
                        Err(e) => eprintln!("watch error{e}"),
                    })
                    .unwrap();

                watcher
                    .watch(Path::new("."), RecursiveMode::Recursive)
                    .unwrap();

                print_watching();
                let mut last_formatted = String::new();
                loop {
                    sleep(Duration::from_millis(10));
                    let mut guard = modify_paths.lock().unwrap();
                    guard.dedup();
                    for path in guard.drain(..) {
                        match format_file(&path) {
                            Ok(formatted) => {
                                if formatted != last_formatted {
                                    clear_watching();
                                    if let Err(e) = run_file(&path) {
                                        eprintln!("{e}")
                                    }
                                    print_watching();
                                }
                                last_formatted = formatted;
                            }
                            Err(e) => {
                                eprintln!("{e}")
                            }
                        }
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
        format_file(&path)?;
        run_file(&path)?;
    }
    Ok(())
}

fn run_file(path: &Path) -> UiuaResult {
    let mut compiler = Compiler::new();
    compiler.load_file(path)?;
    let assembly = compiler.finish();
    assembly.run()?;
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
    #[clap(about = "Format and run a uiua file when it changes")]
    Watch,
}

fn uiua_files() -> Vec<PathBuf> {
    fs::read_dir(".")
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "ua"))
        .map(|entry| entry.path())
        .collect()
}

const WATCHING: &str = "watching for changes...";
fn print_watching() {
    print!(
        "\n{}\n{}",
        "―".repeat(term_size::dimensions().map_or(10, |(w, _)| w)),
        WATCHING
    );
    stdout().flush().unwrap();
}
fn clear_watching() {
    print!("\r{}\n", " ".repeat(WATCHING.len()));
    stdout().flush().unwrap();
}
