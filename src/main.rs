use std::{
    env, fs,
    io::{self, stderr, Write},
    path::{Path, PathBuf},
    process::{self, exit},
    sync::mpsc::channel,
    thread::sleep,
    time::Duration,
};

use clap::Parser;
use instant::Instant;
use notify::{EventKind, RecursiveMode, Watcher};
use uiua::{
    format::{format_file, FormatConfig},
    run::RunMode,
    Uiua, UiuaError, UiuaResult,
};

fn main() {
    color_backtrace::install();

    let _ = ctrlc::set_handler(|| {
        if let Ok(App { command: None, .. }) = App::try_parse() {
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
    if cfg!(feature = "profile") {
        uiua::profile::run_profile();
    } else if let Some(command) = app.command {
        let config = FormatConfig::default();
        match command {
            Command::Fmt { path } => {
                if let Some(path) = path {
                    format_file(path, &config)?;
                } else {
                    for path in uiua_files() {
                        format_file(path, &config)?;
                    }
                }
            }
            Command::Run { path, no_format } => {
                let path = path.unwrap_or_else(|| PathBuf::from("main.ua"));
                if !no_format {
                    format_file(&path, &config)?;
                }
                let mut rt = Uiua::default().mode(RunMode::Normal);
                rt.load_file(path)?;
                for value in rt.take_stack() {
                    println!("{}", value.show());
                }
            }
            Command::Test { path } => {
                let path = path.unwrap_or_else(|| PathBuf::from("main.ua"));
                format_file(&path, &config)?;
                Uiua::default().mode(RunMode::Test).load_file(path)?;
            }
            Command::Watch => {
                if let Err(e) = watch() {
                    eprintln!("Error watching file: {e}");
                }
            }
            #[cfg(feature = "lsp")]
            Command::Lsp => uiua::lsp::run_server(),
        }
    } else if let Err(e) = watch() {
        eprintln!("Error watching file: {e}");
    }
    Ok(())
}

fn watch() -> io::Result<()> {
    let main = PathBuf::from("main.ua");
    let open_path = if main.exists() {
        main
    } else if let Some(entry) = fs::read_dir("")?
        .filter_map(Result::ok)
        .find(|entry| entry.path().extension().map_or(false, |ext| ext == "ua"))
    {
        entry.path()
    } else {
        fs::write(&main, "\"Hello, World!\"")?;
        main
    };
    _ = open::that(&open_path);

    let (send, recv) = channel();
    let mut watcher = notify::recommended_watcher(send).unwrap();
    watcher
        .watch(Path::new("."), RecursiveMode::Recursive)
        .unwrap();
    let mut child: Option<process::Child> = None;
    let config = FormatConfig::default();
    let run = |path: &Path, child: &mut Option<process::Child>| -> io::Result<()> {
        if let Some(mut child) = child.take() {
            _ = child.kill();
            print_watching();
        }
        for i in 0..10 {
            match format_file(path, &config) {
                Ok(formatted) => {
                    if formatted.is_empty() {
                        clear_watching();
                        print_watching();
                        return Ok(());
                    }
                    clear_watching();
                    *child = Some(
                        process::Command::new(env::current_exe().unwrap())
                            .arg("run")
                            .arg(path)
                            .arg("--no-format")
                            .spawn()
                            .unwrap(),
                    );
                    break;
                }
                Err(UiuaError::Format(..)) => sleep(Duration::from_millis((i + 1) * 10)),
                Err(e) => {
                    clear_watching();
                    eprintln!("{}", e.show(true));
                    print_watching();
                    break;
                }
            }
        }
        Ok(())
    };
    run(&open_path, &mut child)?;
    let mut last_time = Instant::now();
    let mut ended = false;
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
            if last_time.elapsed() > Duration::from_millis(100) {
                run(&path, &mut child)?;
                ended = false;
                last_time = Instant::now();
            }
        }
        if !ended {
            if let Some(child) = &mut child {
                if child.try_wait()?.is_some() {
                    print_watching();
                    ended = true;
                }
            }
        }
    }
}

#[derive(Parser)]
struct App {
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
    #[clap(about = "Format and run a file")]
    Run {
        path: Option<PathBuf>,
        #[clap(long, help = "Don't format the file before running")]
        no_format: bool,
    },
    #[clap(about = "Format and test a file")]
    Test { path: Option<PathBuf> },
    #[clap(about = "Run a main.ua in watch mode. This is the default command")]
    Watch,
    #[cfg(feature = "lsp")]
    #[clap(about = "Run the Language Server")]
    Lsp,
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
    eprintln!(
        "\r{}",
        "―".repeat(term_size::dimensions().map_or(10, |(w, _)| w))
    );
}
