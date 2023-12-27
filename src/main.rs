#[cfg(not(feature = "binary"))]
compile_error!("To compile the uiua interpreter binary, you must enable the `binary` feature flag");

use std::{
    env, fmt, fs,
    io::{self, stderr, Write},
    path::{Path, PathBuf},
    process::{exit, Child, Command, Stdio},
    sync::mpsc::channel,
    thread::sleep,
    time::Duration,
};

use clap::{error::ErrorKind, Parser};
use colored::*;
use instant::Instant;
use notify::{EventKind, RecursiveMode, Watcher};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rustyline::{error::ReadlineError, DefaultEditor};
use uiua::{
    format::{format_file, format_str, FormatConfig, FormatConfigSource},
    spans, Assembly, Compiler, NativeSys, PrimClass, RunMode, SpanKind, Uiua, UiuaError,
    UiuaResult, Value,
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
        println!("{}", e.report());
        exit(1);
    }
}

static WATCH_CHILD: Lazy<Mutex<Option<Child>>> = Lazy::new(Default::default);

fn run() -> UiuaResult {
    if cfg!(feature = "profile") {
        uiua::profile::run_profile();
        return Ok(());
    }
    #[cfg(feature = "stand")]
    if let Some(asm) = &*uiua::stand::STAND_ASM {
        let mut rt = Uiua::with_native_sys().with_args(env::args().skip(1).collect());
        rt.run_asm(asm)?;
        print_stack(&rt.take_stack(), true);
        return Ok(());
    }
    match App::try_parse() {
        Ok(app) => match app {
            App::Init => {
                if let Ok(path) = working_file_path() {
                    eprintln!("File already exists: {}", path.display());
                } else {
                    fs::write("main.ua", "\"Hello, World!\"").unwrap();
                }
            }
            App::Fmt {
                path,
                formatter_options,
            } => {
                let config = FormatConfig::from_source(
                    formatter_options.format_config_source,
                    path.as_deref(),
                )?;

                if let Some(path) = path {
                    format_single_file(path, &config, formatter_options.stdout)?;
                } else {
                    format_multi_files(&config, formatter_options.stdout)?;
                }
            }
            App::Run {
                path,
                no_format,
                no_color,
                formatter_options,
                time_instrs,
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
                #[cfg(feature = "audio")]
                setup_audio(audio_options);
                let mut rt = Uiua::with_native_sys()
                    .with_file_path(&path)
                    .with_args(args)
                    .time_instrs(time_instrs);
                if path.extension().is_some_and(|ext| ext == "uasm") {
                    let json = match fs::read_to_string(&path) {
                        Ok(json) => json,
                        Err(e) => {
                            eprintln!("Failed to read assembly: {e}");
                            return Ok(());
                        }
                    };
                    let assembly = match serde_json::from_str::<Assembly>(&json) {
                        Ok(assembly) => assembly,
                        Err(e) => {
                            eprintln!("Failed to parse assembly: {e}");
                            return Ok(());
                        }
                    };
                    rt.run_asm(assembly)?;
                } else {
                    if !no_format {
                        let config = FormatConfig::from_source(
                            formatter_options.format_config_source,
                            Some(&path),
                        )?;
                        format_file(&path, &config, false)?;
                    }
                    let mode = mode.unwrap_or(RunMode::Normal);
                    rt.compile_run(|comp| {
                        comp.mode(mode).print_diagnostics(true).load_file(&path)
                    })?;
                }
                print_stack(&rt.take_stack(), !no_color);
            }
            App::Build { path, output } => {
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
                let assembly = Compiler::with_backend(NativeSys)
                    .print_diagnostics(true)
                    .load_file(&path)?
                    .finish();
                let output = output.unwrap_or_else(|| path.with_extension("uasm"));
                let json = match serde_json::to_string(&assembly) {
                    Ok(json) => json,
                    Err(e) => {
                        eprintln!("Failed to serialize assembly: {e}");
                        return Ok(());
                    }
                };
                if let Err(e) = fs::write(output, json) {
                    eprintln!("Failed to write assembly: {e}");
                }
            }
            App::Eval {
                code,
                no_color,
                #[cfg(feature = "audio")]
                audio_options,
                args,
            } => {
                #[cfg(feature = "audio")]
                setup_audio(audio_options);
                let mut rt = Uiua::with_native_sys().with_args(args);
                rt.compile_run(|comp| {
                    comp.mode(RunMode::Normal)
                        .print_diagnostics(true)
                        .load_str(&code)
                })?;
                print_stack(&rt.take_stack(), !no_color);
            }
            App::Test {
                path,
                formatter_options,
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
                let config =
                    FormatConfig::from_source(formatter_options.format_config_source, Some(&path))?;
                format_file(&path, &config, false)?;
                let mut rt = Uiua::with_native_sys();
                rt.compile_run(|comp| {
                    comp.mode(RunMode::Test)
                        .print_diagnostics(true)
                        .load_file(path)
                })?;
                println!("No failures!");
            }
            App::Watch {
                no_format,
                no_color,
                formatter_options,
                clear,
                args,
                stdin_file,
            } => {
                if let Err(e) = watch(
                    working_file_path().ok().as_deref(),
                    !no_format,
                    !no_color,
                    formatter_options.format_config_source,
                    clear,
                    args,
                    stdin_file,
                ) {
                    eprintln!("Error watching file: {e}");
                }
            }
            #[cfg(feature = "lsp")]
            App::Lsp => uiua::run_language_server(),
            App::Repl {
                formatter_options,
                #[cfg(feature = "audio")]
                audio_options,
                args,
            } => {
                let config = FormatConfig {
                    trailing_newline: false,
                    ..FormatConfig::from_source(formatter_options.format_config_source, None)?
                };

                #[cfg(feature = "audio")]
                setup_audio(audio_options);
                let rt = Uiua::with_native_sys().with_args(args);
                let mut compiler = Compiler::new();
                compiler.mode(RunMode::Normal).print_diagnostics(true);
                repl(rt, compiler, true, config);
            }
            App::Update { main, check } => update(main, check),
            #[cfg(feature = "stand")]
            App::Stand { main, name } => {
                let main = main.unwrap_or_else(|| "main.ua".into());
                if !main.exists() {
                    eprintln!("{} does not exist", main.display());
                    exit(1);
                }
                match uiua::stand::build_exe(&main) {
                    Ok(bytes) => {
                        let name = name
                            .or_else(|| {
                                env::current_dir().ok().and_then(|p| {
                                    p.file_stem().map(|p| p.to_string_lossy().into_owned())
                                })
                            })
                            .unwrap_or_else(|| "program".into());
                        let path = PathBuf::from(name).with_extension(env::consts::EXE_EXTENSION);
                        #[allow(clippy::needless_borrows_for_generic_args)]
                        if let Err(e) = fs::write(&path, bytes) {
                            eprintln!("Failed to write executable: {e}");
                            exit(1);
                        }
                        // Set executable permissions on Unix
                        #[cfg(unix)]
                        if let Err(e) = (|| {
                            use std::os::unix::fs::PermissionsExt;
                            let mut perms = fs::metadata(&path)?.permissions();
                            perms.set_mode(0o755);
                            fs::set_permissions(&path, perms)
                        })() {
                            eprintln!("Failed to set executable permissions: {e}");
                            exit(1);
                        }
                    }
                    Err(e) => {
                        eprintln!("Failed to build executable: {e}");
                        exit(1);
                    }
                }
            }
        },
        Err(e)
            if e.kind() == ErrorKind::InvalidSubcommand
                && env::args()
                    .nth(1)
                    .is_some_and(|path| Path::new(&path).exists()) =>
        {
            let mut args: Vec<String> = env::args().collect();
            args[0] = "run".into();
            let status = Command::new(env::current_exe().unwrap())
                .args(args)
                .status()
                .unwrap();
            exit(status.code().unwrap_or(1));
        }
        Err(e) if e.kind() == ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => {
            let res = match working_file_path() {
                Ok(path) => watch(
                    Some(&path),
                    true,
                    true,
                    FormatConfigSource::SearchFile,
                    false,
                    Vec::new(),
                    None,
                ),
                Err(NoWorkingFile::MultipleFiles) => watch(
                    None,
                    true,
                    true,
                    FormatConfigSource::SearchFile,
                    false,
                    Vec::new(),
                    None,
                ),
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
    color: bool,
    format_config_source: FormatConfigSource,
    clear: bool,
    args: Vec<String>,
    stdin_file: Option<PathBuf>,
) -> io::Result<()> {
    let (send, recv) = channel();
    let mut watcher = notify::recommended_watcher(send).unwrap();
    watcher
        .watch(Path::new("."), RecursiveMode::Recursive)
        .unwrap_or_else(|e| panic!("Failed to watch directory: {e}"));

    println!("Watching for changes... (end with ctrl+C, use `uiua help` to see options)");

    let config = FormatConfig::from_source(format_config_source, initial_path).ok();
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
    let run = |path: &Path, stdin_file: Option<&PathBuf>| -> io::Result<()> {
        if let Some(mut child) = WATCH_CHILD.lock().take() {
            _ = child.kill();
            print_watching();
        }
        const TRIES: u8 = 10;
        for i in 0..TRIES {
            let formatted = if let (Some(config), true) = (&config, format) {
                format_file(path, config, false).map(|f| f.output)
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

                    let stdin_file = stdin_file.map(fs::File::open).transpose()?;

                    *WATCH_CHILD.lock() = Some(
                        Command::new(env::current_exe().unwrap())
                            .arg("run")
                            .arg(path)
                            .args((!color).then_some("--no-color"))
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
                            .stdin(stdin_file.map_or_else(Stdio::inherit, Into::into))
                            .spawn()
                            .unwrap(),
                    );
                    return Ok(());
                }
                Err(UiuaError::Format(..)) => sleep(Duration::from_millis((i as u64 + 1) * 10)),
                Err(e) => {
                    clear_watching();
                    println!("{}", e.report());
                    print_watching();
                    return Ok(());
                }
            }
        }
        println!("Failed to format file after {TRIES} tries");
        Ok(())
    };
    if let Some(path) = initial_path {
        run(path, stdin_file.as_ref())?;
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
                        _ = Command::new("cmd").args(["/C", "cls"]).status();
                    } else {
                        _ = Command::new("clear").status();
                    }
                }
                run(&path, stdin_file.as_ref())?;
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
#[clap(version)]
enum App {
    #[clap(about = "Initialize a new main.ua file")]
    Init,
    #[clap(about = "Format and run a file")]
    Run {
        path: Option<PathBuf>,
        #[clap(long, help = "Don't format the file before running")]
        no_format: bool,
        #[clap(long, help = "Don't colorize stack output")]
        no_color: bool,
        #[clap(flatten)]
        formatter_options: FormatterOptions,
        #[clap(long, help = "Emit the duration of each instruction's execution")]
        time_instrs: bool,
        #[clap(long, help = "Run the file in a specific mode")]
        mode: Option<RunMode>,
        #[cfg(feature = "audio")]
        #[clap(flatten)]
        audio_options: AudioOptions,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Build an assembly (the .uasm format is currently unstable)")]
    Build {
        path: Option<PathBuf>,
        #[clap(short, long, help = "The path to the output file")]
        output: Option<PathBuf>,
    },
    #[clap(about = "Evaluate an expression and print its output")]
    Eval {
        code: String,
        #[clap(long, help = "Don't colorize stack output")]
        no_color: bool,
        #[cfg(feature = "audio")]
        #[clap(flatten)]
        audio_options: AudioOptions,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Format and test a file")]
    Test {
        path: Option<PathBuf>,
        #[clap(flatten)]
        formatter_options: FormatterOptions,
    },
    #[clap(about = "Run .ua files in the current directory when they change")]
    Watch {
        #[clap(long, help = "Don't format the file before running")]
        no_format: bool,
        #[clap(long, help = "Don't colorize stack output")]
        no_color: bool,
        #[clap(flatten)]
        formatter_options: FormatterOptions,
        #[clap(long, help = "Clear the terminal on file change")]
        clear: bool,
        #[clap(long, help = "Read stdin from file")]
        stdin_file: Option<PathBuf>,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Format a Uiua file or all files in the current directory")]
    Fmt {
        path: Option<PathBuf>,
        #[clap(flatten)]
        formatter_options: FormatterOptions,
    },
    #[cfg(feature = "lsp")]
    #[clap(about = "Run the Language Server")]
    Lsp,
    #[clap(about = "Run the Uiua interpreter in a REPL")]
    Repl {
        #[clap(flatten)]
        formatter_options: FormatterOptions,
        #[cfg(feature = "audio")]
        #[clap(flatten)]
        audio_options: AudioOptions,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Update Uiua by installing with Cargo")]
    Update {
        #[clap(long, help = "Install from the main branch instead of crates.io")]
        main: bool,
        #[clap(long, help = "Only check for updates")]
        check: bool,
    },
    #[cfg(feature = "stand")]
    #[clap(about = "Create a standalone executable")]
    Stand {
        #[clap(help = "The main file of the program")]
        main: Option<PathBuf>,
        #[clap(short = 'o', long, help = "The name of the output executable")]
        name: Option<String>,
    },
}

#[derive(clap::Args)]
struct FormatterOptions {
    #[clap(
        long = "format-config",
        default_value_t = FormatConfigSource::SearchFile,
        help = "Select the formatter configuration source (one of search-file, default, or a path to a fmt.ua file)"
    )]
    format_config_source: FormatConfigSource,
    #[clap(
        short = 'O',
        long = "to-stdout",
        default_value_t = false,
        help = "Print result of formatted file to stdout"
    )]
    stdout: bool,
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
    #[cfg(feature = "raw_mode")]
    {
        _ = crossterm::terminal::disable_raw_mode();
    }
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

fn update(main: bool, check: bool) {
    if !main || check {
        let output = match Command::new("cargo").args(["search", "uiua"]).output() {
            Ok(output) => output,
            Err(e) => {
                eprintln!("Failed to run `cargo search uiua`: {e}");
                return;
            }
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
        if let Some((local, remote)) =
            parse_version(local_version).zip(parse_version(remote_version))
        {
            if local >= remote {
                println!("Your version of Uiua ({}) is the latest!", local_version);
                return;
            } else {
                println!(
                    "{}\n",
                    format!(
                        "Update available: {local_version} → {remote_version}\n\
                        Run `uiua update` to update\n\
                        Changelog: https://github.com/uiua-lang/uiua/blob/main/changelog.md",
                    )
                    .bright_white()
                    .bold()
                );
            }
        }
        if check {
            return;
        }
    }

    let mut args = vec!["install"];
    if main {
        args.extend(["--git", "https://github.com/uiua-lang/uiua", "uiua"]);
    } else {
        args.push("uiua");
    }
    let mut features = Vec::new();
    if cfg!(feature = "audio") {
        features.push("audio");
    }
    if cfg!(feature = "bytes") {
        features.push("bytes");
    }
    let feature_str;
    if !features.is_empty() {
        args.push("--features");
        feature_str = features.join(",");
        args.push(&feature_str);
    }
    if let Err(e) = Command::new("cargo").args(&args).spawn() {
        let full_command = format!("cargo {}", args.join(" "));
        eprintln!("Failed to run `{full_command}`: {e}");
    }
}

fn format_single_file(path: PathBuf, config: &FormatConfig, stdout: bool) -> Result<(), UiuaError> {
    let output = format_file(path, config, stdout)?.output;
    if stdout {
        println!("{output}");
    }
    Ok(())
}

fn format_multi_files(config: &FormatConfig, stdout: bool) -> Result<(), UiuaError> {
    for path in uiua_files() {
        let path_as_string = path.to_string_lossy().into_owned();
        let output = format_file(path, config, stdout)?.output;
        if stdout {
            println!("{path_as_string}");
            println!("{output}");
        }
    }
    Ok(())
}

fn print_stack(stack: &[Value], color: bool) {
    if stack.len() == 1 || !color {
        for value in stack {
            println!("{}", value.show());
        }
        return;
    }
    for (i, value) in stack.iter().enumerate() {
        const W: u8 = 255;
        const B: u8 = 200;
        let (r, g, b) = match (i + 3) % 6 {
            0 => (W, B, B),
            1 => (W, W, B),
            2 => (B, W, B),
            3 => (B, W, W),
            4 => (B, B, W),
            5 => (W, B, W),
            _ => unreachable!(),
        };
        println!("{}", value.show().truecolor(r, g, b));
    }
}

fn repl(mut rt: Uiua, mut compiler: Compiler, color: bool, config: FormatConfig) {
    let mut line_reader = DefaultEditor::new().expect("Failed to read from Stdin");
    let mut repl = |rt: &mut Uiua| -> Result<bool, UiuaError> {
        let mut code = match line_reader.readline("» ") {
            Ok(code) => code,
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => return Ok(false),
            Err(_) => panic!("Failed to read from Stdin"),
        };
        if code.is_empty() {
            return Ok(true);
        }

        match format_str(&code, &config) {
            Ok(formatted) => {
                code = formatted.output;
                _ = line_reader.add_history_entry(&code);
            }
            Err(e) => {
                _ = line_reader.add_history_entry(&code);
                return Err(e);
            }
        }

        print!("↪ ");
        println!("{}", color_code(&code));

        let backup = compiler.clone();
        let res = compiler
            .load_str(&code)
            .and_then(|comp| rt.run_asm(comp.finish()));
        print_stack(&rt.take_stack(), color);
        match res {
            Ok(asm) => {
                compiler.assembly_mut().bindings = asm.bindings;
                Ok(true)
            }
            Err(e) => {
                compiler = backup;
                Err(e)
            }
        }
    };

    println!("Uiua {} (end with ctrl+C)\n", env!("CARGO_PKG_VERSION"));
    loop {
        match repl(&mut rt) {
            Ok(true) => {}
            Ok(false) => break,
            Err(e) => {
                eprintln!("{}", e.report());
            }
        }
    }
}

fn color_code(code: &str) -> String {
    let mut colored = String::new();
    let (spans, inputs) = spans(code);
    for span in spans {
        let (r, g, b) = match span.value {
            SpanKind::Primitive(prim) => match prim.class() {
                PrimClass::Stack => (209, 218, 236),
                PrimClass::Constant => (237, 94, 36),
                _ => {
                    if let Some(margs) = prim.modifier_args() {
                        if margs == 1 {
                            (240, 195, 111)
                        } else {
                            (204, 107, 233)
                        }
                    } else {
                        match prim.args() {
                            Some(0) => (237, 94, 106),
                            Some(1) => (149, 209, 106),
                            Some(2) => (84, 176, 252),
                            _ => (255, 255, 255),
                        }
                    }
                }
            },
            SpanKind::String => (32, 249, 252),
            SpanKind::Number => (255, 136, 68),
            SpanKind::Comment => (127, 127, 127),
            SpanKind::Strand => (200, 200, 200),
            SpanKind::Ident
            | SpanKind::Signature
            | SpanKind::Whitespace
            | SpanKind::Placeholder
            | SpanKind::Delimiter => (255, 255, 255),
        };
        colored.push_str(&format!(
            "{}",
            span.span.as_str(&inputs, |s| s.truecolor(r, g, b))
        ));
    }
    colored
}
