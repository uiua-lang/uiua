#[cfg(not(feature = "binary"))]
compile_error!("To compile the uiua interpreter binary, you must enable the `binary` feature flag");

use std::{
    env, fmt, fs,
    io::{self, stderr, stdin, BufRead, Write},
    path::{Path, PathBuf},
    process::{exit, Child, Command, Stdio},
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::channel,
    },
    thread::sleep,
    time::{Duration, Instant},
};

use clap::{error::ErrorKind, Parser, Subcommand};
use colored::*;
use notify::{EventKind, RecursiveMode, Watcher};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rustyline::{error::ReadlineError, DefaultEditor};
use uiua::{
    format::{format_file, format_str, FormatConfig, FormatConfigSource},
    lsp::BindingDocsKind,
    Assembly, Compiler, NativeSys, PreEvalMode, PrimClass, Primitive, RunMode, Signature, SpanKind,
    Uiua, UiuaError, UiuaErrorKind, UiuaResult, Value,
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
            match App::try_parse() {
                Ok(App::Watch { .. }) | Err(_) => clear_watching_with(" ", ""),
                Ok(App::Repl { .. }) => {
                    if !PRESSED_CTRL_C.swap(true, Ordering::Relaxed) {
                        return;
                    }
                }
                _ => {}
            }
            exit(0);
        }
    });

    // This makes sure the terminal's original mode flags are remembered when disabling raw mode
    #[cfg(feature = "raw_mode")]
    rawrrr::save_term();

    if let Err(e) = run() {
        println!("{}", e.report());
        exit(1);
    }
}

static PRESSED_CTRL_C: AtomicBool = AtomicBool::new(false);
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
                io,
            } => {
                let config = FormatConfig::from_source(
                    formatter_options.format_config_source,
                    path.as_deref(),
                )?;

                if io {
                    let mut buffer = String::new();
                    let mut code = String::new();
                    let stdin = stdin();
                    let mut stdin = stdin.lock();
                    loop {
                        buffer.clear();
                        if stdin.read_line(&mut buffer).is_err() {
                            break;
                        }
                        if buffer.is_empty() {
                            break;
                        }
                        code.push_str(&buffer);
                    }
                    let formatted = format_str(&code, &config)?;
                    print!("{}", formatted.output);
                } else if let Some(path) = path {
                    format_single_file(path, &config)?;
                } else {
                    format_multi_files(&config)?;
                }
            }
            App::Run {
                path,
                no_format,
                no_color,
                formatter_options,
                time_instrs,
                limit,
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
                    .time_instrs(time_instrs)
                    .maybe_with_execution_limit(limit.map(Duration::from_secs_f64));
                if path.extension().is_some_and(|ext| ext == "uasm") {
                    let uasm = match fs::read_to_string(&path) {
                        Ok(json) => json,
                        Err(e) => {
                            eprintln!("Failed to read assembly: {e}");
                            return Ok(());
                        }
                    };
                    let assembly = match Assembly::from_uasm(&uasm) {
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
                        format_file(&path, &config)?;
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
                let mut assembly = Compiler::with_backend(NativeSys)
                    .print_diagnostics(true)
                    .load_file(&path)?
                    .finish();
                assembly.remove_dead_code();
                let output = output.unwrap_or_else(|| path.with_extension("uasm"));
                let uasm = assembly.to_uasm();
                if let Err(e) = fs::write(output, uasm) {
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
                let config =
                    FormatConfig::from_source(formatter_options.format_config_source, Some(&path))?;
                format_file(&path, &config)?;
                let mut rt = Uiua::with_native_sys()
                    .with_file_path(&path)
                    .with_args(args);
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
                if let Err(e) = (WatchArgs {
                    initial_path: working_file_path().ok(),
                    format: !no_format,
                    color: !no_color,
                    format_config_source: formatter_options.format_config_source,
                    clear,
                    args,
                    stdin_file,
                })
                .watch()
                {
                    eprintln!("Error watching file: {e}");
                }
            }
            #[cfg(feature = "lsp")]
            App::Lsp => uiua::lsp::run_language_server(),
            App::Repl {
                file,
                formatter_options,
                #[cfg(feature = "audio")]
                audio_options,
                clear,
                args,
            } => {
                let config = FormatConfig {
                    trailing_newline: false,
                    ..FormatConfig::from_source(formatter_options.format_config_source, None)?
                };

                #[cfg(feature = "audio")]
                setup_audio(audio_options);
                let mut rt = Uiua::with_native_sys().with_args(args);
                let mut compiler = Compiler::with_backend(NativeSys);
                compiler.mode(RunMode::Normal).print_diagnostics(true);
                if let Some(file) = file {
                    compiler.load_file(file)?;
                    rt.run_compiler(&mut compiler)?;
                }
                repl(rt, compiler, true, clear, config);
            }
            App::Update { main, check } => update(main, check),
            App::Module { command } => {
                let paths = match list_modules() {
                    Ok(paths) => paths,
                    Err(e) => {
                        eprintln!("Failed to list modules: {e}");
                        return Ok(());
                    }
                };
                match command.unwrap_or(ModuleCommand::List) {
                    ModuleCommand::List => {
                        if let Some(paths) = paths {
                            for path in paths {
                                println!("{}", path.display());
                            }
                        }
                    }
                    ModuleCommand::Update { module } => {
                        let modules = if let Some(module) = module {
                            vec![module]
                        } else if let Some(paths) = paths {
                            paths
                        } else {
                            eprintln!("No modules to update");
                            return Ok(());
                        };
                        if let Err(e) = update_modules(&modules) {
                            eprintln!("Failed to update modules: {e}");
                        }
                    }
                }
            }
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
            App::Find { path, text, raw } => find(path, text, raw)?,
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
                Ok(path) => WatchArgs {
                    initial_path: Some(path),
                    ..Default::default()
                }
                .watch(),
                Err(NoWorkingFile::MultipleFiles) => WatchArgs::default().watch(),
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

struct WatchArgs {
    initial_path: Option<PathBuf>,
    format: bool,
    color: bool,
    format_config_source: FormatConfigSource,
    clear: bool,
    args: Vec<String>,
    stdin_file: Option<PathBuf>,
}

impl Default for WatchArgs {
    fn default() -> Self {
        Self {
            initial_path: None,
            format: true,
            color: true,
            format_config_source: FormatConfigSource::SearchFile,
            clear: false,
            args: Vec::new(),
            stdin_file: None,
        }
    }
}

impl WatchArgs {
    fn watch(self) -> io::Result<()> {
        let WatchArgs {
            initial_path,
            format,
            color,
            format_config_source,
            clear,
            args,
            stdin_file,
        } = self;
        let (send, recv) = channel();
        let mut watcher = notify::recommended_watcher(send).unwrap();
        watcher
            .watch(Path::new("."), RecursiveMode::Recursive)
            .unwrap_or_else(|e| panic!("Failed to watch directory: {e}"));

        println!("Watching for changes... (end with ctrl+C, use `uiua help` to see options)");

        let config = FormatConfig::from_source(format_config_source, initial_path.as_deref()).ok();
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
            let path = if let Some(path) = std::env::current_dir()
                .ok()
                .and_then(|curr| pathdiff::diff_paths(path, curr))
            {
                path
            } else {
                path.to_path_buf()
            };
            for i in 0..TRIES {
                let formatted = if let (Some(config), true) = (&config, format) {
                    format_file(&path, config).map(|f| f.output)
                } else {
                    fs::read_to_string(&path)
                        .map_err(|e| UiuaErrorKind::Load(path.clone(), e.into()).into())
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
                            f64::from_bits(audio_time_clone.load(Ordering::Relaxed)).to_string();
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
                    Err(e) => {
                        if let UiuaErrorKind::Format(..) = e.kind {
                            sleep(Duration::from_millis((i as u64 + 1) * 10))
                        } else {
                            clear_watching();
                            println!("{}", e.report());
                            print_watching();
                            return Ok(());
                        }
                    }
                }
            }
            println!("Failed to format file after {TRIES} tries");
            Ok(())
        };
        if let Some(path) = initial_path {
            run(&path, stdin_file.as_ref())?;
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
                        audio_time.store(time.to_bits(), Ordering::Relaxed);
                    }
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
        #[clap(long, short = 'l', help = "Set an execution limit in seconds")]
        limit: Option<f64>,
        #[clap(long, help = "Run the file in a specific mode")]
        mode: Option<RunMode>,
        #[cfg(feature = "audio")]
        #[clap(flatten)]
        audio_options: AudioOptions,
        #[clap(trailing_var_arg = true, help = "Arguments to pass to the program")]
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
        #[clap(trailing_var_arg = true, help = "Arguments to pass to the program")]
        args: Vec<String>,
    },
    #[clap(about = "Format and test a file")]
    Test {
        path: Option<PathBuf>,
        #[clap(flatten)]
        formatter_options: FormatterOptions,
        #[clap(trailing_var_arg = true, help = "Arguments to pass to the program")]
        args: Vec<String>,
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
        #[clap(trailing_var_arg = true, help = "Arguments to pass to the program")]
        args: Vec<String>,
    },
    #[clap(about = "Format a Uiua file or all files in the current directory")]
    Fmt {
        path: Option<PathBuf>,
        #[clap(flatten)]
        formatter_options: FormatterOptions,
        #[clap(long, help = "Format lines read from stdin")]
        io: bool,
    },
    #[clap(about = "Find some Uiua code that matches the given unformatted text")]
    Find {
        text: String,
        #[clap(short = 'p', long, help = "The path to search")]
        path: Option<PathBuf>,
        #[clap(long, help = "Disable color and other formatting")]
        raw: bool,
    },
    #[clap(about = "Run the Uiua interpreter in a REPL")]
    Repl {
        #[clap(help = "A Uiua file to run before the REPL starts")]
        file: Option<PathBuf>,
        #[clap(flatten)]
        formatter_options: FormatterOptions,
        #[cfg(feature = "audio")]
        #[clap(flatten)]
        audio_options: AudioOptions,
        #[clap(short = 'c', long, help = "Clear the stack after each line")]
        clear: bool,
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
    #[clap(about = "Manage Git modules. Requires Git")]
    Module {
        #[clap(subcommand)]
        command: Option<ModuleCommand>,
    },
    #[cfg(feature = "stand")]
    #[clap(about = "Create a standalone executable")]
    Stand {
        #[clap(help = "The main file of the program")]
        main: Option<PathBuf>,
        #[clap(short = 'o', long, help = "The name of the output executable")]
        name: Option<String>,
    },
    #[cfg(feature = "lsp")]
    #[clap(about = "Run the Language Server")]
    Lsp,
}

#[derive(Subcommand)]
enum ModuleCommand {
    #[clap(about = "List all modules")]
    List,
    #[clap(about = "Update a module or all modules")]
    Update {
        #[clap(help = "The module to update")]
        module: Option<PathBuf>,
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

fn uiua_files(root: &Path) -> UiuaResult<Vec<PathBuf>> {
    fn rec(root: &Path, acc: &mut Vec<PathBuf>) -> UiuaResult<()> {
        for entry in fs::read_dir(root).map_err(|e| UiuaError::format(root.into(), e))? {
            let entry = entry.map_err(|e| UiuaError::format(root.into(), e))?;
            let path = entry.path();
            if path.is_dir() {
                rec(&path, acc)?;
            } else if path.extension().map_or(false, |ext| ext == "ua") {
                acc.push(path);
            }
        }
        Ok(())
    }
    let mut acc = Vec::new();
    rec(root, &mut acc)?;
    Ok(acc)
}

const WATCHING: &str = "\x1b[0mwatching for changes...";
fn print_watching() {
    #[cfg(feature = "raw_mode")]
    rawrrr::disable_raw();
    eprint!("{}", WATCHING);
    stderr().flush().unwrap();
}
fn clear_watching() {
    clear_watching_with("―", "\n")
}

fn clear_watching_with(s: &str, end: &str) {
    print!(
        "\r{}{}",
        s.repeat(terminal_size::terminal_size().map_or(10, |(w, _)| w.0 as usize)),
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
    if cfg!(feature = "webcam") {
        features.push("webcam");
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

fn format_single_file(path: PathBuf, config: &FormatConfig) -> Result<(), UiuaError> {
    format_file(path, config)?;
    Ok(())
}

fn format_multi_files(config: &FormatConfig) -> Result<(), UiuaError> {
    for path in uiua_files(".".as_ref())? {
        format_file(path, config)?;
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
        let (w, b) = if terminal_light::luma().is_ok_and(|luma| luma > 0.6) {
            (0, 35)
        } else {
            (255, 200)
        };
        let (r, g, b) = match (i + 3) % 6 {
            0 => (w, b, b),
            1 => (w, w, b),
            2 => (b, w, b),
            3 => (b, w, w),
            4 => (b, b, w),
            5 => (w, b, w),
            _ => unreachable!(),
        };
        println!("{}", value.show().truecolor(r, g, b));
    }
}

fn repl(mut env: Uiua, mut compiler: Compiler, color: bool, clear: bool, config: FormatConfig) {
    env = env.with_interrupt_hook(|| PRESSED_CTRL_C.swap(false, Ordering::Relaxed));
    compiler.pre_eval_mode(PreEvalMode::Line);
    println!(
        "Uiua {} (end with ctrl+C, type `help` for a list of commands)\n",
        env!("CARGO_PKG_VERSION")
    );
    let mut line_reader = DefaultEditor::new().expect("Failed to read from Stdin");
    loop {
        let mut code = match line_reader.readline("    ") {
            Ok(code) => {
                match code.trim() {
                    "help" => {
                        println!(
                            "\n\
                            clear - Clear the stack \n\
                            exit  - Exit the repl \n\
                            help  - Show this message \n\
                            "
                        );
                        continue;
                    }
                    "clear" | "cls" => {
                        env.take_stack();
                        println!();
                        continue;
                    }
                    "exit" => break,
                    _ => {}
                }
                code
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => break,
            Err(_) => panic!("Failed to read from Stdin"),
        };
        if code.is_empty() {
            continue;
        }

        match format_str(&code, &config) {
            Ok(formatted) => {
                code = formatted.output;
                _ = line_reader.add_history_entry(&code);
            }
            Err(e) => {
                _ = line_reader.add_history_entry(&code);
                eprintln!("{}", e.report());
                continue;
            }
        }

        let backup_comp = compiler.clone();
        let backup_stack = env.stack().to_vec();
        let res = compiler.load_str(&code).map(drop);
        println!("    {}", color_code(&code, &compiler));
        let res = res.and_then(|()| env.run_compiler(&mut compiler));

        match res {
            Ok(()) => {
                print_stack(env.stack(), color);
                if clear {
                    env.take_stack();
                }
            }
            Err(e) => {
                compiler = backup_comp;
                env.take_stack();
                for val in backup_stack {
                    env.push(val);
                }
                eprintln!("{}", e.report());
                print_stack(env.stack(), color);
            }
        }
    }
}

fn color_code(code: &str, compiler: &Compiler) -> String {
    let mut colored = String::new();
    let (spans, inputs) = uiua::lsp::spans_with_compiler(code, compiler);

    let noadic = Color::Red;
    let monadic = Color::Green;
    let monadic_mod = Color::Yellow;
    let dyadic_mod = Color::Magenta;
    let dyadic = Color::Blue;

    let for_prim = |prim: Primitive, sig: Option<Signature>| match prim.class() {
        PrimClass::Stack | PrimClass::Debug if prim.modifier_args().is_none() => None,
        PrimClass::Constant => None,
        _ => {
            if let Some(margs) = prim.modifier_args() {
                Some(if margs == 1 { monadic_mod } else { dyadic_mod })
            } else {
                match sig.map(|sig| sig.args).or(prim.args()) {
                    Some(0) => Some(noadic),
                    Some(1) => Some(monadic),
                    Some(2) => Some(dyadic),
                    _ => None,
                }
            }
        }
    };

    for span in spans {
        let color = match span.value {
            SpanKind::Primitive(prim, sig) => for_prim(prim, sig),
            SpanKind::Ident {
                docs: Some(docs), ..
            } => match docs.kind {
                BindingDocsKind::Function { sig, .. } => match sig.args {
                    0 => Some(noadic),
                    1 => Some(monadic),
                    2 => Some(dyadic),
                    _ => None,
                },
                BindingDocsKind::Modifier(margs) => Some(match margs {
                    1 => monadic_mod,
                    _ => dyadic_mod,
                }),
                _ => None,
            },
            SpanKind::String => Some(Color::Cyan),
            SpanKind::Number | SpanKind::Subscript(None, _) => Some(Color::TrueColor {
                r: 235,
                g: 136,
                b: 68,
            }),
            SpanKind::Subscript(Some(prim), n) => for_prim(prim, prim.subscript_sig(n)),
            SpanKind::Comment | SpanKind::OutputComment | SpanKind::Strand => {
                Some(Color::BrightBlack)
            }
            SpanKind::Ident { .. }
            | SpanKind::Label
            | SpanKind::Signature
            | SpanKind::Whitespace
            | SpanKind::Placeholder(_)
            | SpanKind::Delimiter
            | SpanKind::FuncDelim(_)
            | SpanKind::StackSwizzle(_)
            | SpanKind::ArraySwizzle(_) => None,
        };
        span.span.as_str(&inputs, |s| {
            colored.push_str(&if let Some(color) = color {
                s.color(color).to_string()
            } else {
                s.to_string()
            });
        })
    }
    colored
}

fn list_modules() -> io::Result<Option<Vec<PathBuf>>> {
    let Ok(entries) = fs::read_dir("uiua-modules") else {
        return Ok(None);
    };
    let mut paths = Vec::new();
    for entry in entries {
        let entry = entry?;
        let owner_path = entry.path();
        if !owner_path.is_dir() {
            continue;
        }
        let owner = PathBuf::from(owner_path.file_name().unwrap());
        for entry in fs::read_dir(owner_path)? {
            let entry = entry?;
            let repo_path = entry.path();
            if !repo_path.is_dir() {
                continue;
            }
            let repo = repo_path.file_name().unwrap();
            paths.push(owner.join(repo));
        }
    }
    paths.sort();
    Ok(Some(paths))
}

fn update_modules(modules: &[PathBuf]) -> io::Result<()> {
    let canonical: Vec<PathBuf> = modules
        .iter()
        .map(|p| Path::new("uiua-modules").join(p).canonicalize())
        .collect::<io::Result<_>>()?;
    for (path, canonical) in modules.iter().zip(canonical) {
        env::set_current_dir(&canonical)?;
        println!("{} {}", "Updating".bold().bright_green(), path.display());
        Command::new("git").args(["pull"]).spawn()?.wait()?;
    }
    Ok(())
}

fn find(path: Option<PathBuf>, mut text: String, raw: bool) -> UiuaResult {
    if raw {
        colored::control::set_override(false);
    }
    let paths = if let Some(path) = path {
        if path.is_file() {
            vec![path]
        } else if path.is_dir() {
            uiua_files(&path)?
        } else {
            return Err(UiuaError::load(
                path,
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Path is not a file or directory",
                ),
            ));
        }
    } else {
        uiua_files(".".as_ref())?
    };
    text = format_str(
        &text,
        &FormatConfig {
            trailing_newline: false,
            ..Default::default()
        },
    )?
    .output;
    for path in paths {
        let path = path
            .strip_prefix("./")
            .or_else(|_| path.strip_prefix(".\\"))
            .unwrap_or(path.as_path());
        let contents = fs::read_to_string(path).map_err(|e| UiuaError::load(path.into(), e))?;
        let mut matches = Vec::new();
        for (i, line) in contents.lines().enumerate() {
            if let Some(pos) = line.find(&text) {
                matches.push((
                    format!(
                        "{}:{}:{}",
                        path.display(),
                        i + 1,
                        line[..pos].chars().count() + 1
                    ),
                    pos,
                    line,
                ));
            }
        }
        let Some(max_len) = matches.iter().map(|(loc, ..)| loc.chars().count()).max() else {
            continue;
        };
        let width = max_len + 2;
        if !raw {
            println!("\n{}", path.display().to_string().bright_green().bold());
        }
        for (loc, pos, line) in matches {
            print!("{loc:width$}");
            print!("{}", line[..pos].bright_black());
            print!("{}", &line[pos..][..text.len()]);
            println!("{}", line[pos + text.len()..].bright_black());
        }
    }
    Ok(())
}
