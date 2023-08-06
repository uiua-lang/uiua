use std::{
    env, fs,
    io::{self, stderr, Write},
    path::{Path, PathBuf},
    process::{self, exit},
    sync::mpsc::channel,
    thread::sleep,
    time::Duration,
};

use clap::{error::ErrorKind, Parser};
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
        if let Ok(App::Watch) | Err(_) = App::try_parse() {
            clear_watching();
        }
        exit(0)
    });

    if let Err(e) = run() {
        println!("{}", e.show(true));
        exit(1);
    }
}

fn run() -> UiuaResult {
    if cfg!(feature = "profile") {
        uiua::profile::run_profile();
        return Ok(());
    }
    match App::try_parse() {
        Ok(app) => {
            let config = FormatConfig::default();
            match app {
                App::Init => {
                    if let Some(path) = working_file_path() {
                        eprintln!("File already exists: {}", path.display());
                    } else {
                        fs::write("main.ua", "\"Hello, World!\"").unwrap();
                        _ = open::that("main.ua");
                    }
                }
                App::Fmt { path } => {
                    if let Some(path) = path {
                        format_file(path, &config)?;
                    } else {
                        for path in uiua_files() {
                            format_file(path, &config)?;
                        }
                    }
                }
                App::Run { path, no_format } => {
                    if let Some(path) = path.or_else(working_file_path) {
                        if !no_format {
                            format_file(&path, &config)?;
                        }
                        let mut rt = Uiua::default().mode(RunMode::Normal);
                        rt.load_file(path)?;
                        for value in rt.take_stack() {
                            println!("{}", value.show());
                        }
                    } else {
                        eprintln!("{NO_UA_FILE}");
                    }
                }
                App::Test { path } => {
                    if let Some(path) = path.or_else(working_file_path) {
                        format_file(&path, &config)?;
                        Uiua::default().mode(RunMode::Test).load_file(path)?;
                        println!("No failures!");
                    } else {
                        eprintln!("{NO_UA_FILE}");
                        return Ok(());
                    }
                }
                App::Watch => {
                    if let Some(path) = working_file_path() {
                        _ = open::that(&path);
                        if let Err(e) = watch(&path) {
                            eprintln!("Error watching file: {e}");
                        }
                    } else {
                        eprintln!("{NO_UA_FILE}");
                    }
                }
                #[cfg(feature = "lsp")]
                App::Lsp => uiua::lsp::run_server(),
            }
        }
        Err(e) if e.kind() == ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => {
            if let Some(path) = working_file_path() {
                _ = open::that(&path);
                if let Err(e) = watch(&path) {
                    eprintln!("Error watching file: {e}");
                }
            } else {
                _ = e.print();
                eprintln!("\n{NO_UA_FILE}");
            }
        }
        Err(e) => _ = e.print(),
    }
    Ok(())
}

const NO_UA_FILE: &str =
    "No .ua file found nearby. Initialize one in the current directory with `uiua init`";

fn working_file_path() -> Option<PathBuf> {
    let in_src = PathBuf::from("src.main.ua");
    let main = if in_src.exists() {
        in_src
    } else {
        PathBuf::from("main.ua")
    };
    Some(if main.exists() {
        main
    } else if let Some(entry) = fs::read_dir("")
        .into_iter()
        .chain(fs::read_dir("src"))
        .flatten()
        .filter_map(Result::ok)
        .find(|entry| entry.path().extension().map_or(false, |ext| ext == "ua"))
    {
        entry.path()
    } else {
        return None;
    })
}

fn watch(open_path: &Path) -> io::Result<()> {
    let (send, recv) = channel();
    let mut watcher = notify::recommended_watcher(send).unwrap();
    watcher
        .watch(Path::new("."), RecursiveMode::Recursive)
        .unwrap();

    println!("Watching for changes... (end with ctrl+C, use `uiua help` to see options)");

    let mut child: Option<process::Child> = None;
    let config = FormatConfig::default();
    let run = |path: &Path, child: &mut Option<process::Child>| -> io::Result<()> {
        if let Some(mut child) = child.take() {
            _ = child.kill();
            print_watching();
        }
        const TRIES: u8 = 10;
        for i in 0..TRIES {
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
                    return Ok(());
                }
                Err(UiuaError::Format(..)) => sleep(Duration::from_millis((i as u64 + 1) * 10)),
                Err(e) => {
                    clear_watching();
                    println!("{}", e.show(true));
                    print_watching();
                    return Ok(());
                }
            }
        }
        println!("Failed to format file after {TRIES} tries");
        Ok(())
    };
    run(open_path, &mut child)?;
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
enum App {
    #[clap(about = "Initialize a new main.ua file")]
    Init,
    #[clap(about = "Format and run a file")]
    Run {
        path: Option<PathBuf>,
        #[clap(long, help = "Don't format the file before running")]
        no_format: bool,
    },
    #[clap(about = "Format and test a file")]
    Test { path: Option<PathBuf> },
    #[clap(about = "Run a main.ua in watch mode")]
    Watch,
    #[clap(about = "Format a uiua file or all files in the current directory")]
    Fmt { path: Option<PathBuf> },
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
    println!(
        "\r{}",
        "―".repeat(term_size::dimensions().map_or(10, |(w, _)| w))
    );
}
