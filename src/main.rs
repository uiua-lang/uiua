#[cfg(not(feature = "binary"))]
compile_error!("To compile the uiua interpreter binary, you must enable the `binary` feature flag");

use std::{
    env, fmt, fs,
    io::{self, stderr, Write},
    path::{Path, PathBuf},
    process::{exit, Child, Command},
    sync::mpsc::channel,
    thread::sleep,
    time::Duration,
};

use clap::{error::ErrorKind, Parser};
use colored::Colorize;
use instant::Instant;
use notify::{EventKind, RecursiveMode, Watcher};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use uiua::{
    format::{format_file, FormatConfig},
    run::RunMode,
    Uiua, UiuaError, UiuaResult,
};

fn main() {
    color_backtrace::install();

    let _ = ctrlc::set_handler(|| {
        let mut child = WATCH_CHILD.lock();
        if let Some(ch) = &mut *child {
            _ = ch.kill();
            *child = None;
            println!("# Program interrupted");
            print_watching();
        } else {
            if let Ok(App::Watch { .. }) | Err(_) = App::try_parse() {
                clear_watching_with(" ", "");
            }
            exit(0)
        }
    });

    if let Err(e) = run() {
        println!("{}", e.show(true));
        exit(1);
    }
}

static WATCH_CHILD: Lazy<Mutex<Option<Child>>> = Lazy::new(Default::default);

fn run() -> UiuaResult {
    if cfg!(feature = "profile") {
        uiua::profile::run_profile();
        return Ok(());
    }
    show_update_message();
    match App::try_parse() {
        Ok(app) => {
            let config = FormatConfig::default();
            match app {
                App::Init => {
                    if let Ok(path) = working_file_path() {
                        eprintln!("File already exists: {}", path.display());
                    } else {
                        fs::write("main.ua", "\"Hello, World!\"").unwrap();
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
                App::Run {
                    path,
                    no_format,
                    mode,
                    #[cfg(feature = "audio")]
                    audio_options,
                    args,
                } => {
                    let path = if let Some(path) = path {
                        path
                    } else {
                        match working_file_path() {
                            Ok(path) => path,
                            Err(e) => {
                                eprintln!("{}", e);
                                return Ok(());
                            }
                        }
                    };
                    if !no_format {
                        format_file(&path, &config)?;
                    }
                    let mode = mode.unwrap_or(RunMode::Normal);
                    #[cfg(feature = "audio")]
                    setup_audio(audio_options);
                    let mut rt = Uiua::with_native_sys()
                        .with_mode(mode)
                        .with_file_path(&path)
                        .with_args(args)
                        .print_diagnostics(true);
                    rt.load_file(path)?;
                    for value in rt.take_stack() {
                        println!("{}", value.show());
                    }
                }
                App::Eval {
                    code,
                    #[cfg(feature = "audio")]
                    audio_options,
                    args,
                } => {
                    #[cfg(feature = "audio")]
                    setup_audio(audio_options);
                    let mut rt = Uiua::with_native_sys()
                        .with_mode(RunMode::Normal)
                        .with_args(args)
                        .print_diagnostics(true);
                    rt.load_str(&code)?;
                    for value in rt.take_stack() {
                        println!("{}", value.show());
                    }
                }
                App::Test { path } => {
                    let path = if let Some(path) = path {
                        path
                    } else {
                        match working_file_path() {
                            Ok(path) => path,
                            Err(e) => {
                                eprintln!("{}", e);
                                return Ok(());
                            }
                        }
                    };
                    format_file(&path, &config)?;
                    Uiua::with_native_sys()
                        .with_mode(RunMode::Test)
                        .print_diagnostics(true)
                        .load_file(path)?;
                    println!("No failures!");
                }
                App::Watch {
                    no_format,
                    clear,
                    args,
                } => {
                    if let Err(e) =
                        watch(working_file_path().ok().as_deref(), !no_format, clear, args)
                    {
                        eprintln!("Error watching file: {e}");
                    }
                }
                #[cfg(feature = "lsp")]
                App::Lsp => uiua::lsp::run_server(),
            }
        }
        Err(e) if e.kind() == ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => {
            let res = match working_file_path() {
                Ok(path) => watch(Some(&path), true, false, Vec::new()),
                Err(NoWorkingFile::MultipleFiles) => watch(None, true, false, Vec::new()),
                Err(nwf) => {
                    _ = e.print();
                    eprintln!("\n{nwf}");
                    return Ok(());
                }
            };
            if let Err(e) = res {
                eprintln!("Error watching file: {e}");
            }
        }
        Err(e) => _ = e.print(),
    }
    Ok(())
}

#[derive(Debug)]
enum NoWorkingFile {
    NoFile,
    MultipleFiles,
}

impl fmt::Display for NoWorkingFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NoWorkingFile::NoFile => {
                "No .ua file found nearby. Initialize one in the \
                current directory with `uiua init`"
            }
            NoWorkingFile::MultipleFiles => {
                "No main.ua file found nearby, and multiple other \
                .ua files found. Please specify which file to run \
                with `uiua run <PATH>`"
            }
        }
        .fmt(f)
    }
}

fn working_file_path() -> Result<PathBuf, NoWorkingFile> {
    let main_in_src = PathBuf::from("src/main.ua");
    let main = if main_in_src.exists() {
        main_in_src
    } else {
        PathBuf::from("main.ua")
    };
    if main.exists() {
        Ok(main)
    } else {
        let paths: Vec<_> = fs::read_dir(".")
            .into_iter()
            .chain(fs::read_dir("src"))
            .flatten()
            .filter_map(Result::ok)
            .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "ua"))
            .map(|entry| entry.path())
            .collect();
        match paths.len() {
            0 => Err(NoWorkingFile::NoFile),
            1 => Ok(paths.into_iter().next().unwrap()),
            _ => Err(NoWorkingFile::MultipleFiles),
        }
    }
}

fn watch(
    initial_path: Option<&Path>,
    format: bool,
    clear: bool,
    args: Vec<String>,
) -> io::Result<()> {
    let (send, recv) = channel();
    let mut watcher = notify::recommended_watcher(send).unwrap();
    watcher
        .watch(Path::new("."), RecursiveMode::Recursive)
        .unwrap_or_else(|e| panic!("Failed to watch directory: {e}"));

    println!("Watching for changes... (end with ctrl+C, use `uiua help` to see options)");

    let config = FormatConfig::default();
    #[cfg(feature = "audio")]
    let audio_time = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0f64.to_bits()));
    #[cfg(feature = "audio")]
    let audio_time_clone = audio_time.clone();
    #[cfg(feature = "audio")]
    let (audio_time_socket, audio_time_port) = {
        let socket = std::net::UdpSocket::bind(("127.0.0.1", 0))?;
        let port = socket.local_addr()?.port();
        socket.set_nonblocking(true)?;
        (socket, port)
    };
    let run = |path: &Path| -> io::Result<()> {
        if let Some(mut child) = WATCH_CHILD.lock().take() {
            _ = child.kill();
            print_watching();
        }
        const TRIES: u8 = 10;
        for i in 0..TRIES {
            let formatted = if format {
                format_file(path, &config).map(|f| f.output)
            } else {
                fs::read_to_string(path).map_err(|e| UiuaError::Load(path.to_path_buf(), e.into()))
            };
            match formatted {
                Ok(input) => {
                    if input.is_empty() {
                        clear_watching();
                        print_watching();
                        return Ok(());
                    }
                    clear_watching();
                    #[cfg(feature = "audio")]
                    let audio_time =
                        f64::from_bits(audio_time_clone.load(std::sync::atomic::Ordering::Relaxed))
                            .to_string();
                    #[cfg(feature = "audio")]
                    let audio_port = audio_time_port.to_string();
                    *WATCH_CHILD.lock() = Some(
                        Command::new(env::current_exe().unwrap())
                            .arg("run")
                            .arg(path)
                            .args([
                                "--no-format",
                                "--mode",
                                "all",
                                #[cfg(feature = "audio")]
                                "--audio-time",
                                #[cfg(feature = "audio")]
                                &audio_time,
                                #[cfg(feature = "audio")]
                                "--audio-port",
                                #[cfg(feature = "audio")]
                                &audio_port,
                            ])
                            .args(&args)
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
    if let Some(path) = initial_path {
        run(path)?;
    }
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
                if clear {
                    if cfg!(target_os = "windows") {
                        Command::new("cmd").args(&["/C", "cls"]).status().unwrap();
                    } else {
                        Command::new("clear").status().unwrap();
                    }
                }
                run(&path)?;
                last_time = Instant::now();
            }
        }
        let mut child = WATCH_CHILD.lock();
        if let Some(ch) = &mut *child {
            if ch.try_wait()?.is_some() {
                print_watching();
                *child = None;
            }
            #[cfg(feature = "audio")]
            {
                let mut buf = [0; 8];
                if audio_time_socket.recv(&mut buf).is_ok_and(|n| n == 8) {
                    let time = f64::from_be_bytes(buf);
                    audio_time.store(time.to_bits(), std::sync::atomic::Ordering::Relaxed);
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
        #[clap(long, help = "Run the file in a specific mode")]
        mode: Option<RunMode>,
        #[cfg(feature = "audio")]
        #[clap(flatten)]
        audio_options: AudioOptions,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Evaluate an expression and print its output")]
    Eval {
        code: String,
        #[cfg(feature = "audio")]
        #[clap(flatten)]
        audio_options: AudioOptions,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Format and test a file")]
    Test { path: Option<PathBuf> },
    #[clap(about = "Run .ua files in the current directory when they change")]
    Watch {
        #[clap(long, help = "Don't format the file before running")]
        no_format: bool,
        #[clap(long, help = "Clear the terminal on file change")]
        clear: bool,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Format a uiua file or all files in the current directory")]
    Fmt { path: Option<PathBuf> },
    #[cfg(feature = "lsp")]
    #[clap(about = "Run the Language Server")]
    Lsp,
}

#[cfg(feature = "audio")]
#[derive(clap::Args)]
struct AudioOptions {
    #[clap(long, help = "The start time of audio streaming")]
    audio_time: Option<f64>,
    #[clap(long, help = "The port to update audio time on")]
    audio_port: Option<u16>,
}

#[cfg(feature = "audio")]
fn setup_audio(options: AudioOptions) {
    if let Some(time) = options.audio_time {
        uiua::set_audio_stream_time(time);
    }

    if let Some(port) = options.audio_port {
        if let Err(e) = uiua::set_audio_stream_time_port(port) {
            eprintln!("Failed to set audio time port: {e}");
        }
    }
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
    clear_watching_with("―", "\n")
}

fn clear_watching_with(s: &str, end: &str) {
    print!(
        "\r{}{}",
        s.repeat(term_size::dimensions().map_or(10, |(w, _)| w)),
        end,
    );
}

fn show_update_message() {
    let Ok(output) = Command::new("cargo").args(["search", "uiua"]).output() else {
        return;
    };
    let output = String::from_utf8_lossy(&output.stdout);
    let Some(remote_version) = output.split('"').nth(1) else {
        return;
    };
    fn parse_version(s: &str) -> Option<Vec<u16>> {
        let mut nums = Vec::with_capacity(3);
        for s in s.split('.') {
            if let Ok(num) = s.parse() {
                nums.push(num);
            } else {
                return None;
            }
        }
        Some(nums)
    }
    let local_version = env!("CARGO_PKG_VERSION");
    if let Some((local, remote)) = parse_version(local_version).zip(parse_version(remote_version)) {
        if local < remote {
            println!(
                "{}\n",
                format!(
                    "Update available: {local_version} → {remote_version}\n\
                Run `cargo install uiua` to update",
                )
                .bright_white()
                .bold()
            );
        }
    }
}
