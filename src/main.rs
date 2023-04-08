use std::{
    fs,
    io::{self, stderr, Write},
    path::{Path, PathBuf},
    process::exit,
    sync::mpsc::channel,
    thread::sleep,
    time::Duration,
};

use clap::Parser;
use instant::Instant;
use notify::{EventKind, RecursiveMode, Watcher};
use uiua::{format::format_file, run::RunMode, Uiua, UiuaResult};

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
            Command::Run => {
                let path = PathBuf::from("main.ua");
                format_file(&path)?;
                Uiua::default().mode(RunMode::Normal).load_file(path)?;
            }
            Command::Test => {
                let path = PathBuf::from("main.ua");
                format_file(&path)?;
                Uiua::default().mode(RunMode::Test).load_file(path)?;
            }
        }
    } else if let Err(e) = watch() {
        eprintln!("Error creating watch file {e}");
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
        let path = PathBuf::from("scratch.ua");
        fs::write(&path, "Hello, World!")?;
        path
    };
    _ = open::that(&open_path);

    let (send, recv) = channel();
    let mut watcher = notify::recommended_watcher(send).unwrap();
    watcher
        .watch(Path::new("."), RecursiveMode::Recursive)
        .unwrap();
    let run = |path: &Path| match format_file(path).or_else(|_| {
        sleep(Duration::from_millis(100));
        format_file(path)
    }) {
        Ok(formatted) => {
            if formatted.is_empty() {
                clear_watching();
                print_watching();
                return;
            }
            clear_watching();
            match Uiua::default()
                .mode(RunMode::Watch)
                .load_str_path(&formatted, path)
            {
                Ok(env) => {
                    for value in env.take_stack() {
                        println!("{}", value.show());
                    }
                }
                Err(e) => eprintln!("{}", e.show(true)),
            }
            print_watching();
        }
        Err(e) => {
            clear_watching();
            eprintln!("{}", e.show(true));
            print_watching();
        }
    };
    run(&open_path);
    let mut last_time = Instant::now();
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
                run(&path);
                last_time = Instant::now();
            }
        }
    }
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
    #[clap(about = "Format and run main.ua")]
    Run,
    #[clap(about = "Format and test main.ua")]
    Test,
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
