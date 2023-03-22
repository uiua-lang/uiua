use std::{
    fs,
    io::{stderr, stdin, stdout, BufRead, Write},
    path::{Path, PathBuf},
    process::exit,
    sync::mpsc::channel,
    thread::sleep,
    time::Duration,
};

use clap::Parser;
use notify::{EventKind, RecursiveMode, Watcher};
use uiua::{compile::Compiler, format::format_file, value::Value, UiuaResult};

fn main() {
    #[cfg(feature = "profile")]
    {
        puffin::set_scopes_on(true);
        let server_addr = format!("0.0.0.0:{}", puffin_http::DEFAULT_PORT);
        Box::leak(Box::new(puffin_http::Server::new(&server_addr).unwrap()));
    }
    let _ = ctrlc::set_handler(|| {
        if let Ok(App {
            command: Some(Command::Watch),
            ..
        }) = App::try_parse()
        {
            clear_watching();
        }
        exit(0)
    });

    if let Err(e) = run() {
        eprintln!("{}", e.show(true));
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
                let (send, recv) = channel();
                let mut watcher = notify::recommended_watcher(send).unwrap();
                watcher
                    .watch(Path::new("."), RecursiveMode::Recursive)
                    .unwrap();
                print_watching();
                let mut last_formatted = String::new();
                loop {
                    sleep(Duration::from_millis(10));
                    if let Some(path) = recv
                        .try_iter()
                        .filter_map(Result::ok)
                        .filter(|event| matches!(event.kind, EventKind::Modify(_)))
                        .flat_map(|event| event.paths)
                        .filter(|path| path.extension().map_or(false, |ext| ext == "ua"))
                        .last()
                    {
                        match format_file(&path) {
                            Ok(formatted) => {
                                if formatted != last_formatted {
                                    clear_watching();
                                    match run_file(&path) {
                                        Ok(values) => {
                                            for value in values.into_iter().rev() {
                                                println!("{}", value.show());
                                            }
                                        }
                                        Err(e) => eprintln!("{}", e.show(true)),
                                    }
                                    print_watching();
                                }
                                last_formatted = formatted;
                            }
                            Err(e) => {
                                clear_watching();
                                eprintln!("{}", e.show(true));
                                print_watching();
                            }
                        }
                    }
                }
            }
            Command::Run => {
                let path = PathBuf::from("main.ua");
                format_file(&path)?;
                run_file(&path)?;
            }
        }
    } else {
        let mut compiler = Compiler::new();
        fn print_prompt() {
            print!("  ");
            stdout().flush().unwrap();
        }
        print_prompt();
        loop {
            let line = stdin().lock().lines().next();
            if let Some(Ok(line)) = line {
                if line.trim() == "exit" {
                    break;
                }
                match compiler.eval(&line) {
                    Ok(stack) => {
                        for value in stack {
                            println!("{}", value.show());
                        }
                    }
                    Err(e) => eprintln!("{}", e.show(true)),
                }
                print_prompt();
            } else {
                break;
            }
        }
    }
    Ok(())
}

fn run_file(path: &Path) -> UiuaResult<Vec<Value>> {
    let mut compiler = Compiler::new();
    compiler.load_file(path)?;
    let assembly = compiler.finish();
    assembly.run()
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
    Fmt {
        path: Option<PathBuf>,
    },
    #[clap(about = "Format and run a uiua file when it changes")]
    Watch,
    Run,
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
    eprint!("{}", WATCHING);
    stderr().flush().unwrap();
}
fn clear_watching() {
    eprint!(
        "\r{}",
        "―".repeat(term_size::dimensions().map_or(10, |(w, _)| w))
    );
    stderr().flush().unwrap();
}
