#![allow(clippy::print_stdout)]
#[cfg(not(feature = "binary"))]
compile_error!("To compile the uiua interpreter binary, you must enable the `binary` feature flag");

use std::{
    env,
    error::Error,
    fmt, fs,
    io::{self, stderr, stdin, stdout, BufRead, Write},
    path::{Path, PathBuf},
    process::{exit, Child, Command, Stdio},
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::channel,
    },
    thread::sleep,
    time::{Duration, Instant},
};

use clap::{Parser, Subcommand};
use colored::*;
use notify::{event::ModifyKind, EventKind, RecursiveMode, Watcher};
use parking_lot::Mutex;
use rustyline::{error::ReadlineError, DefaultEditor};
use terminal_size::terminal_size;
use uiua::{
    format::{format_file, format_str, FormatConfig, FormatConfigSource},
    lex,
    lsp::BindingDocsKind,
    parse, print_stack, Assembly, CodeSpan, Compiler, NativeSys, PreEvalMode, PrimClass, PrimDoc,
    PrimDocFragment, PrimDocLine, Primitive, RunMode, SafeSys, SpanKind, Spans, Subscript, Token,
    Uiua, UiuaError, UiuaErrorKind, UiuaResult, CONSTANTS,
};

static PRESSED_CTRL_C: AtomicBool = AtomicBool::new(false);
static WATCH_CHILD: Mutex<Option<Child>> = Mutex::new(None);

fn fail<T>(e: UiuaError) -> T {
    eprintln!("{}", e.report());
    exit(1)
}

#[cfg(feature = "window")]
fn use_window() -> bool {
    #[cfg(feature = "window")]
    {
        uiua::window::use_window()
    }
    #[cfg(not(feature = "window"))]
    false
}

fn set_use_window(use_window: bool) {
    #[cfg(feature = "window")]
    {
        uiua::window::set_use_window(use_window || env::var("UIUA_WINDOW").is_ok());
    }
    #[cfg(not(feature = "window"))]
    if use_window {
        eprintln!(
            "{}: Window output is not supported in this environment. Compile with `-F full` or `-F window` to enable it.",
            "Warning".bright_yellow()
        );
    }
}

fn main() {
    color_backtrace::install();

    let _ = ctrlc::set_handler(|| {
        let mut child = WATCH_CHILD.lock();
        if let Some(ch) = &mut *child {
            _ = ch.kill();
            *child = None;
            eprintln!("# Program interrupted");
            print_watching();
        } else {
            match App::try_parse().ok().and_then(|app| app.command) {
                Some(Comm::Watch { .. }) | None => clear_watching_with(" ", ""),
                Some(Comm::Repl { .. }) => {
                    if !PRESSED_CTRL_C.swap(true, Ordering::Relaxed) {
                        return;
                    }
                }
                _ => {}
            }
            #[cfg(feature = "window")]
            if use_window() {
                _ = uiua::window::Request::Shutdown.send();
                sleep(Duration::from_millis(100));
            }
            exit(0);
        }
    });

    // This makes sure the terminal's original mode flags are remembered when disabling raw mode
    #[cfg(feature = "raw_mode")]
    rawrrr::save_term();

    if cfg!(feature = "profile") {
        uiua::profile::run_profile();
        return;
    }
    // Open window
    #[cfg(feature = "window")]
    if env::args().count() == 2 && env::args().nth(1).unwrap() == "window" {
        uiua::window::run_window();
        return;
    }
    // Run stand-alone
    #[cfg(feature = "stand")]
    if let Some(asm) = &*uiua::stand::STAND_ASM {
        let mut rt = Uiua::with_native_sys().with_args(env::args().skip(1).collect());
        rt.run_asm(asm.clone()).unwrap_or_else(fail);
        print_stack(&rt.take_stack(), true);
        return;
    }

    // If passing a file, just run it
    let mut args = env::args().skip(1).peekable();
    if (args.peek()).is_some_and(|arg| ["-w", "--window"].contains(&arg.as_str())) {
        set_use_window(true);
        args.next();
    }
    if let Some(path) = args.next().map(PathBuf::from).filter(|arg| {
        arg.extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("ua"))
            || arg.components().count() > 1
    }) {
        let args = args.collect();
        run(&path, args, false, None, None, None, false);
        return;
    }

    // Main command parsing
    let app = App::parse();
    match app.command {
        Some(Comm::Init) => {
            if let Ok(path) = working_file_path() {
                eprintln!("File already exists: {}", path.display());
            } else {
                fs::write("main.ua", "\"Hello, World!\"").unwrap();
            }
        }
        Some(Comm::Fmt {
            path,
            formatter_options,
            io,
        }) => {
            let config =
                FormatConfig::from_source(formatter_options.format_config_source, path.as_deref())
                    .unwrap_or_else(fail);

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
                let formatted = format_str(&code, &config).unwrap_or_else(fail);
                print!("{}", formatted.output);
            } else if let Some(path) = path {
                format_single_file(path, &config).unwrap_or_else(fail);
            } else {
                format_multi_files(&config).unwrap_or_else(fail);
            }
        }
        Some(Comm::Run {
            path,
            no_format,
            no_color,
            formatter_options,
            time_instrs,
            limit,
            mode,
            #[cfg(feature = "audio")]
            audio_options,
            window,
            args,
        }) => {
            let path = if let Some(path) = path {
                path
            } else {
                match working_file_path() {
                    Ok(path) => path,
                    Err(e) => {
                        eprintln!("{e}");
                        return;
                    }
                }
            };
            #[cfg(feature = "audio")]
            setup_audio(audio_options);
            set_use_window(window);
            run(
                &path,
                args,
                time_instrs,
                limit,
                mode,
                (!no_format).then_some(formatter_options),
                no_color,
            );
        }
        Some(Comm::Build { path, output, ast }) => {
            let path = if let Some(path) = path {
                path
            } else {
                match working_file_path() {
                    Ok(path) => path,
                    Err(e) => {
                        eprintln!("{e}");
                        return;
                    }
                }
            };
            if ast {
                let input = match fs::read_to_string(&path) {
                    Ok(input) => input,
                    Err(e) => {
                        eprintln!("Failed to read file: {e}");
                        exit(1);
                    }
                };
                let mut inputs = Default::default();
                let (items, errors, _) = parse(&input, &path, &mut inputs);
                if !errors.is_empty() {
                    eprintln!(
                        "{}",
                        UiuaErrorKind::Parse(errors, inputs.into()).error().report()
                    );
                }
                let mut s: String = "[\n  ".into();
                for (i, item) in items.into_iter().enumerate() {
                    if i > 0 {
                        s.push_str(",\n  ");
                    }
                    s.push_str(&serde_json::to_string(&item).unwrap());
                }
                s.push_str("\n]");
                if let Some(output) = output {
                    if let Err(e) = fs::write(output, s) {
                        eprintln!("Failed to write json to file: {e}");
                    }
                } else {
                    println!("{s}");
                }
            } else {
                let assembly = Compiler::with_backend(NativeSys)
                    .mode(RunMode::Normal)
                    .print_diagnostics(true)
                    .load_file(&path)
                    .unwrap_or_else(fail)
                    .finish();
                let output = output.unwrap_or_else(|| path.with_extension("uasm"));
                let uasm = assembly.to_uasm();
                if let Err(e) = fs::write(output, uasm) {
                    eprintln!("Failed to write assembly: {e}");
                }
            }
        }
        Some(Comm::Eval {
            code,
            no_color,
            experimental,
            #[cfg(feature = "audio")]
            audio_options,
            args,
        }) => {
            #[cfg(feature = "audio")]
            setup_audio(audio_options);
            let mut rt = Uiua::with_native_sys().with_args(args);
            rt.compile_run(|comp| {
                comp.mode(RunMode::Normal)
                    .experimental(experimental)
                    .print_diagnostics(true)
                    .load_str(&code)
            })
            .unwrap_or_else(fail);
            print_stack(&rt.take_stack(), !no_color);
        }
        Some(Comm::Test {
            path,
            formatter_options,
            args,
        }) => {
            let path = if let Some(path) = path {
                path
            } else {
                match working_file_path() {
                    Ok(path) => path,
                    Err(e) => {
                        eprintln!("{e}");
                        return;
                    }
                }
            };
            let config =
                FormatConfig::from_source(formatter_options.format_config_source, Some(&path))
                    .unwrap_or_else(fail);
            format_file(&path, &config).unwrap_or_else(fail);
            let mut rt = Uiua::with_native_sys()
                .with_file_path(&path)
                .with_args(args);
            let res = rt.compile_run(|comp| {
                comp.mode(RunMode::Test)
                    .print_diagnostics(true)
                    .load_file(path)
            });
            if let Err(e) = &res {
                eprintln!("{}", e.report());
            }
            rt.print_reports();
            if res.is_err() {
                exit(1);
            }
        }
        Some(Comm::Watch {
            no_format,
            no_color,
            formatter_options,
            clear,
            window,
            args,
            stdin_file,
        }) => {
            set_use_window(window);
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
        Some(Comm::Lsp) => uiua::lsp::run_language_server(),
        Some(Comm::Repl {
            file,
            formatter_options,
            #[cfg(feature = "audio")]
            audio_options,
            stack,
            experimental,
            args,
        }) => {
            let config = FormatConfig {
                trailing_newline: false,
                ..FormatConfig::from_source(formatter_options.format_config_source, None)
                    .unwrap_or_else(fail)
            };

            #[cfg(feature = "audio")]
            setup_audio(audio_options);
            let mut rt = Uiua::with_native_sys().with_args(args);
            let mut compiler = Compiler::with_backend(NativeSys);
            compiler
                .mode(RunMode::Normal)
                .print_diagnostics(true)
                .experimental(experimental);
            if let Some(file) = file {
                compiler.load_file(file).unwrap_or_else(fail);
                rt.run_compiler(&mut compiler).unwrap_or_else(fail);
            }
            repl(rt, compiler, true, stack, config);
        }
        Some(Comm::Update {
            main,
            check,
            features,
        }) => update(main, check, features),
        Some(Comm::Module { command }) => {
            let paths = match list_modules() {
                Ok(paths) => paths,
                Err(e) => {
                    eprintln!("Failed to list modules: {e}");
                    return;
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
                        return;
                    };
                    if let Err(e) = update_modules(&modules) {
                        eprintln!("Failed to update modules: {e}");
                    }
                }
            }
        }
        #[cfg(feature = "stand")]
        Some(Comm::Stand { main, name }) => {
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
        Some(Comm::Doc { name }) => doc(&name),
        Some(Comm::Check { path }) => check(path).unwrap_or_else(fail),
        Some(Comm::Find { path, text, raw }) => find(path, text, raw).unwrap_or_else(fail),
        None => {
            set_use_window(app.window);
            let res = match working_file_path() {
                Ok(path) => WatchArgs {
                    initial_path: Some(path),
                    ..Default::default()
                }
                .watch(),
                Err(NoWorkingFile::MultipleFiles) => WatchArgs::default().watch(),
                Err(_)
                    if app.window
                        || uiua_files(None, Some(2)).is_ok_and(|files| !files.is_empty()) =>
                {
                    WatchArgs::default().watch()
                }
                Err(nwf) => {
                    _ = App::try_parse_from(["uiua", "help"])
                        .map(drop)
                        .unwrap_err()
                        .print();
                    eprintln!("\n{nwf}");
                    return;
                }
            };
            if let Err(e) = res {
                eprintln!("Error watching file: {e}");
            }
        }
    }
}

fn run(
    path: &Path,
    args: Vec<String>,
    time_instrs: bool,
    limit: Option<f64>,
    mode: Option<RunMode>,
    formatter_options: Option<FormatterOptions>,
    no_color: bool,
) {
    let mut rt = Uiua::with_native_sys()
        .with_file_path(path)
        .with_args(args)
        .time_instrs(time_instrs)
        .maybe_with_execution_limit(limit.map(Duration::from_secs_f64));
    if path.extension().is_some_and(|ext| ext == "uasm") {
        let uasm = match fs::read_to_string(path) {
            Ok(json) => json,
            Err(e) => {
                eprintln!("Failed to read assembly: {e}");
                return;
            }
        };
        let assembly = match Assembly::from_uasm(&uasm) {
            Ok(assembly) => assembly,
            Err(e) => {
                eprintln!("Failed to parse assembly: {e}");
                return;
            }
        };
        rt.run_asm(assembly).unwrap_or_else(fail);
    } else {
        if let Some(formatter_options) = formatter_options {
            let config =
                FormatConfig::from_source(formatter_options.format_config_source, Some(path))
                    .unwrap_or_else(fail);
            format_file(path, &config).unwrap_or_else(fail);
        }
        let mode = mode.unwrap_or(RunMode::Normal);
        let res = rt.compile_run(|comp| comp.mode(mode).print_diagnostics(true).load_file(path));
        if let Err(e) = &res {
            print_stack(&rt.take_stack(), !no_color);
            eprintln!("{}", e.report());
        }
        rt.print_reports();
        if res.is_err() {
            exit(1);
        }
    }
    print_stack(&rt.take_stack(), !no_color);
    #[cfg(feature = "raw_mode")]
    rawrrr::disable_raw();
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
    fn watch(self) -> Result<(), Box<dyn Error>> {
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
        let mut watcher = notify::recommended_watcher(send)?;
        watcher.watch(Path::new("."), RecursiveMode::Recursive)?;

        eprintln!("Watching for changes... (end with ctrl+C, use `uiua help` to see options)");

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
                    Ok(_) => {
                        clear_watching();
                        #[cfg(feature = "audio")]
                        let audio_time =
                            f64::from_bits(audio_time_clone.load(Ordering::Relaxed)).to_string();
                        #[cfg(feature = "audio")]
                        let audio_port = audio_time_port.to_string();

                        let stdin_file = stdin_file.map(fs::File::open).transpose()?;

                        *WATCH_CHILD.lock() = Some({
                            let mut com = Command::new(env::current_exe().unwrap());
                            com.arg("run")
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
                                ]);
                            #[cfg(feature = "window")]
                            if use_window() {
                                com.arg("--window");
                            }
                            com.args(&args)
                                .stdin(stdin_file.map_or_else(Stdio::inherit, Into::into))
                                .spawn()
                                .unwrap()
                        });
                        return Ok(());
                    }
                    Err(e) => {
                        if let UiuaErrorKind::Format(..) = *e.kind {
                            sleep(Duration::from_millis((i as u64 + 1) * 10))
                        } else {
                            clear_watching();
                            eprintln!("{}", e.report());
                            print_watching();
                            return Ok(());
                        }
                    }
                }
            }
            eprintln!("Failed to format file after {TRIES} tries");
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
                .filter(|event| {
                    matches!(
                        event.kind,
                        EventKind::Modify(ModifyKind::Any | ModifyKind::Data(_))
                    )
                })
                .flat_map(|event| event.paths)
                .filter(|path| path.extension().is_some_and(|ext| ext == "ua"))
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
struct App {
    #[clap(subcommand)]
    command: Option<Comm>,
    #[clap(
        short,
        long,
        help = "Use a window for output instead of stdout. \
                Set UIUA_WINDOW=1 to always use a window."
    )]
    window: bool,
    #[clap(trailing_var_arg = true, help = "Arguments to pass to the program")]
    args: Vec<String>,
}

#[derive(Subcommand)]
enum Comm {
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
        #[clap(
            short,
            long,
            help = "Use a window for output instead of stdout. \
                    Set UIUA_WINDOW=1 to always use a window."
        )]
        window: bool,
        #[clap(trailing_var_arg = true, help = "Arguments to pass to the program")]
        args: Vec<String>,
    },
    #[clap(about = "Build an assembly (the .uasm format is currently unstable)")]
    Build {
        path: Option<PathBuf>,
        #[clap(short, long, help = "The path to the output file")]
        output: Option<PathBuf>,
        #[clap(long, help = "Parse only and emit the AST as JSON (unstable)")]
        ast: bool,
    },
    #[clap(about = "Evaluate an expression and print its output")]
    Eval {
        code: String,
        #[clap(long, help = "Don't colorize stack output")]
        no_color: bool,
        #[clap(short = 'x', long, help = "Enable experimental features")]
        experimental: bool,
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
        #[clap(
            short,
            long,
            help = "Use a window for output instead of stdout. \
                    Set UIUA_WINDOW=1 to always use a window."
        )]
        window: bool,
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
    #[clap(about = "Show the documentation for a function, modifier, or constant")]
    Doc {
        #[clap(help = "The name of the function, modifier, or constant")]
        name: String,
    },
    #[clap(about = "Check that Uiua files compile")]
    Check {
        #[clap(help = "The path to a file or directory to check")]
        path: Option<PathBuf>,
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
        #[clap(short = 's', long, help = "Don't clear the stack after each line")]
        stack: bool,
        #[clap(short = 'x', long, help = "Enable experimental features")]
        experimental: bool,
        #[clap(trailing_var_arg = true)]
        args: Vec<String>,
    },
    #[clap(about = "Update Uiua by installing with Cargo")]
    Update {
        #[clap(long, help = "Install from the main branch instead of crates.io")]
        main: bool,
        #[clap(long, help = "Only check for updates")]
        check: bool,
        #[clap(short = 'F', long, help = "Enable features")]
        features: Vec<String>,
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

fn uiua_files(path: Option<&Path>, max_depth: Option<usize>) -> UiuaResult<Vec<PathBuf>> {
    if let Some(path) = path {
        if path.is_file() {
            Ok(vec![path.into()])
        } else if path.is_dir() {
            uiua_files_in(path, max_depth)
        } else {
            Err(UiuaError::load(
                path.into(),
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Path is not a file or directory",
                ),
            ))
        }
    } else {
        uiua_files_in(".".as_ref(), max_depth)
    }
}

fn uiua_files_in(root: &Path, max_depth: Option<usize>) -> UiuaResult<Vec<PathBuf>> {
    fn rec(root: &Path, acc: &mut Vec<PathBuf>, depth: usize, max_depth: usize) -> UiuaResult<()> {
        if depth > max_depth {
            return Ok(());
        }
        for entry in fs::read_dir(root).map_err(|e| UiuaError::format(root.into(), e))? {
            let entry = entry.map_err(|e| UiuaError::format(root.into(), e))?;
            let path = entry.path();
            if path.is_dir() {
                if path
                    .file_name()
                    .is_some_and(|name| name.to_string_lossy().starts_with('.'))
                    || path
                        .to_str()
                        .is_some_and(|s| s == "./target" || s == ".\\target")
                {
                    continue;
                }
                rec(&path, acc, depth + 1, max_depth)?;
            } else if path.extension().is_some_and(|ext| ext == "ua") {
                acc.push(path);
            }
        }
        Ok(())
    }
    let mut acc = Vec::new();
    rec(root, &mut acc, 0, max_depth.unwrap_or(usize::MAX))?;
    Ok(acc)
}

const WATCHING: &str = "\x1b[0mwatching for changes...";
fn print_watching() {
    #[cfg(feature = "raw_mode")]
    rawrrr::disable_raw();
    eprint!("{WATCHING}");
    stderr().flush().unwrap();
}
fn clear_watching() {
    clear_watching_with("―", "\n")
}

fn clear_watching_with(s: &str, end: &str) {
    eprint!(
        "\r{}{}",
        s.repeat(terminal_size::terminal_size().map_or(10, |(w, _)| w.0 as usize)),
        end,
    );
}

fn update(main: bool, check: bool, mut features: Vec<String>) {
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
                println!("Your version of Uiua ({local_version}) is the latest!");
                return;
            }
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
    if cfg!(feature = "full") {
        features.push("full".into());
    }
    if cfg!(feature = "audio") {
        features.push("audio".into());
    }
    if cfg!(feature = "webcam") {
        features.push("webcam".into());
    }
    if cfg!(feature = "window") {
        features.push("window".into());
    }
    features.sort();
    features.dedup();
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
    for path in uiua_files_in(".".as_ref(), None)? {
        format_file(path, config)?;
    }
    Ok(())
}

fn repl(mut env: Uiua, mut compiler: Compiler, color: bool, stack: bool, config: FormatConfig) {
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
            Err(e) => panic!("Failed to read from Stdin: {e}"),
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
                if !stack {
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
        compiler.assembly_mut().root.clear();
    }
}

const NOADIC: Color = Color::Red;
const MONADIC: Color = Color::Green;
const DYADIC: Color = Color::Blue;
const MONADIC_MOD: Color = Color::Yellow;
const DYADIC_MOD: Color = Color::Magenta;

fn color_prim(prim: Primitive, sub: Option<&Subscript>) -> Option<Color> {
    match prim.class() {
        PrimClass::Stack | PrimClass::Debug | PrimClass::Planet
            if prim.modifier_args().is_none() =>
        {
            None
        }
        PrimClass::Constant => None,
        _ => {
            if let Some(margs) = prim.modifier_args() {
                Some(color_mod(margs))
            } else {
                match prim
                    .subscript_sig(sub)
                    .map(|sig| sig.args())
                    .or(prim.args())
                {
                    Some(n) => color_func(n),
                    _ => None,
                }
            }
        }
    }
}

fn color_func(args: usize) -> Option<Color> {
    match args {
        0 => Some(NOADIC),
        1 => Some(MONADIC),
        2 => Some(DYADIC),
        _ => None,
    }
}

fn color_mod(margs: usize) -> Color {
    if margs == 1 {
        MONADIC_MOD
    } else {
        DYADIC_MOD
    }
}

fn color_code(code: &str, compiler: &Compiler) -> String {
    let mut colored = String::new();
    let spans = Spans::with_compiler(code, compiler);

    let mut prev: Option<CodeSpan> = None;
    for span in spans.spans {
        if let Some(prev) = prev {
            if prev.end.byte_pos < span.span.start.byte_pos {
                colored
                    .push_str(&code[prev.end.byte_pos as usize..span.span.start.byte_pos as usize]);
            }
        }
        let color = match span.value {
            SpanKind::Primitive(prim, sig) => color_prim(prim, sig.as_ref()),
            SpanKind::Ident {
                docs: Some(docs), ..
            } => match docs.kind {
                BindingDocsKind::Function { sig, .. } => match sig.args() {
                    0 => Some(NOADIC),
                    1 => Some(MONADIC),
                    2 => Some(DYADIC),
                    _ => None,
                },
                BindingDocsKind::Modifier(margs) => Some(match margs {
                    1 => MONADIC_MOD,
                    _ => DYADIC_MOD,
                }),
                _ => None,
            },
            SpanKind::String | SpanKind::ImportSrc(_) => Some(Color::Cyan),
            SpanKind::Number | SpanKind::Subscript(None, _) => Some(Color::TrueColor {
                r: 235,
                g: 136,
                b: 68,
            }),
            SpanKind::Subscript(Some(prim), n) => color_prim(prim, n.as_ref()),
            SpanKind::Comment | SpanKind::OutputComment | SpanKind::Strand => {
                Some(Color::BrightBlack)
            }
            SpanKind::MacroDelim(margs) => Some(color_mod(margs)),
            SpanKind::ArgSetter(_) => Some(MONADIC),
            SpanKind::Ident { .. }
            | SpanKind::Label
            | SpanKind::Signature
            | SpanKind::Whitespace
            | SpanKind::Placeholder(_)
            | SpanKind::Delimiter
            | SpanKind::LexOrder
            | SpanKind::FuncDelim(..)
            | SpanKind::Obverse(_) => None,
        };
        span.span.as_str(&spans.inputs, |s| {
            colored.push_str(&if let Some(color) = color {
                s.color(color).to_string()
            } else {
                s.to_string()
            });
        });
        prev = Some(span.span);
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
        eprintln!("{} {}", "Updating".bold().bright_green(), path.display());
        Command::new("git").args(["pull"]).spawn()?.wait()?;
    }
    Ok(())
}

fn check(path: Option<PathBuf>) -> UiuaResult {
    let paths = uiua_files(path.as_deref(), None)?;
    let path_count = paths.len();
    let mut successes = 0;
    let width = terminal_size().map(|(w, _)| w.0 as usize).unwrap_or(60);
    for (i, path) in paths.into_iter().enumerate() {
        let message_length = format!("Checking {} ({}/{})", path.display(), i + 1, path_count)
            .chars()
            .count();
        eprint!(
            "\r{} {} ({}/{}){}",
            "Checking".bold().bright_green(),
            path.display(),
            i + 1,
            path_count,
            " ".repeat(width.saturating_sub(message_length))
        );
        stdout().flush().unwrap();
        let mut comp = Compiler::with_backend(NativeSys);
        if let Err(e) = comp.load_file(path) {
            eprintln!("\n{}", e.report());
        } else {
            successes += 1;
        }
    }
    let message = format!(
        "{successes}/{path_count} file{} compiled successfully",
        if path_count == 1 { "" } else { "s" }
    );
    let color = if successes == 0 {
        if path_count == 0 {
            Color::BrightGreen
        } else {
            Color::BrightRed
        }
    } else if successes == path_count {
        Color::BrightGreen
    } else {
        Color::BrightYellow
    };
    eprintln!(
        "\r{}{}",
        message.color(color),
        " ".repeat(width.saturating_sub(message.chars().count()))
    );
    Ok(())
}

fn find(path: Option<PathBuf>, text: String, raw: bool) -> UiuaResult {
    if raw {
        colored::control::set_override(false);
    }
    enum Needle {
        Text(String),
        Prim(Primitive),
    }
    let needle = if let Some(prim) = Primitive::from_format_name(&text).or_else(|| {
        (text.chars().count() == 1)
            .then(|| Primitive::from_glyph(text.chars().next().unwrap()))
            .flatten()
    }) {
        Needle::Prim(prim)
    } else {
        Needle::Text(
            format_str(
                &text,
                &FormatConfig {
                    trailing_newline: false,
                    ..Default::default()
                },
            )
            .map(|f| f.output)
            .unwrap_or(text),
        )
    };
    for path in uiua_files(path.as_deref(), None)? {
        let path = path
            .strip_prefix("./")
            .or_else(|_| path.strip_prefix(".\\"))
            .unwrap_or(path.as_path());
        let contents = fs::read_to_string(path).map_err(|e| UiuaError::load(path.into(), e))?;
        let mut matches = Vec::new();
        match &needle {
            Needle::Text(text) => {
                for (i, line) in contents.lines().enumerate() {
                    if let Some(pos) = line.find(text) {
                        matches.push((
                            format!(
                                "{}:{}:{}",
                                path.display(),
                                i + 1,
                                line[..pos].chars().count() + 1
                            ),
                            pos,
                            text.len(),
                            None,
                            line,
                        ));
                    }
                }
            }
            Needle::Prim(prim) => {
                let (tokens, ..) = lex(&contents, (), &mut Default::default());
                for tok in tokens {
                    let Token::Glyph(prim2) = &tok.value else {
                        continue;
                    };
                    if prim != prim2 {
                        continue;
                    }
                    let line_no = tok.span.start.line as usize - 1;
                    let line = contents.lines().nth(line_no).unwrap();
                    let pos = line
                        .chars()
                        .take(tok.span.start.col as usize - 1)
                        .map(|c| c.len_utf8())
                        .sum();
                    matches.push((
                        format!(
                            "{}:{}:{}",
                            path.display(),
                            line_no + 1,
                            line[..pos].chars().count() + 1
                        ),
                        pos,
                        (tok.span.end.byte_pos - tok.span.start.byte_pos) as usize,
                        color_prim(*prim, None),
                        line,
                    ));
                }
            }
        }
        let Some(max_len) = matches.iter().map(|(loc, ..)| loc.chars().count()).max() else {
            continue;
        };
        let width = max_len + 2;
        if !raw {
            println!("\n{}", path.display().to_string().bright_green().bold());
        }
        for (loc, pos, len, color, line) in matches {
            print!("{loc:width$}");
            print!("{}", line[..pos].bright_black());
            if let Some(color) = color {
                print!("{}", line[pos..][..len].color(color));
            } else {
                print!("{}", &line[pos..][..len]);
            }
            println!("{}", line[pos + len..].bright_black());
        }
    }
    Ok(())
}

fn doc(name: &str) {
    fn print_doc_frag(frag: &PrimDocFragment) {
        match frag {
            PrimDocFragment::Text(s) => print!("{s}"),
            PrimDocFragment::Code(s) => print!("{s}"),
            PrimDocFragment::Emphasis(s) => print!("{}", s.italic()),
            PrimDocFragment::Strong(s) => print!("{}", s.bold()),
            PrimDocFragment::Primitive { prim, named } => {
                if *named {
                    if let Some(color) = color_prim(*prim, None) {
                        print!("{}", prim.format().to_string().color(color))
                    } else {
                        print!("{}", prim.format())
                    }
                } else if let Some(color) = color_prim(*prim, None) {
                    print!("{}", prim.to_string().color(color))
                } else {
                    print!("{prim}")
                }
            }
            PrimDocFragment::Link { text, url } => {
                print!("{text}({url})")
            }
        }
    }

    fn print_doc_line(line: &PrimDocLine, comp: &Compiler) {
        match line {
            PrimDocLine::Text(vec) => {
                for frag in vec {
                    print_doc_frag(frag)
                }
                println!();
            }
            PrimDocLine::Example(prim_example) => {
                println!();
                let ex = color_code(prim_example.input(), comp);
                for line in ex.lines() {
                    println!("  {line}");
                }
                println!();
                match prim_example.output() {
                    Ok(vals) => {
                        for val in vals {
                            for line in val.lines() {
                                println!("    {line}")
                            }
                        }
                    }
                    Err(e) => {
                        let report = e.report().to_string();
                        for line in report.lines() {
                            println!("    {line}")
                        }
                    }
                }
                println!();
            }
        }
    }

    if let Some(prim) = Primitive::from_format_name(name).or_else(|| {
        let mut chars = name.chars();
        (chars.next())
            .filter(|_| chars.next().is_none())
            .and_then(Primitive::from_glyph)
    }) {
        // Primitives
        println!();
        if let Some(color) = color_prim(prim, None) {
            println!("{}", prim.format().to_string().color(color));
        } else {
            println!("{}", prim.format());
        }
        println!();
        let doc = PrimDoc::from(prim);
        for frag in &doc.short {
            print_doc_frag(frag);
        }
        println!();
        let comp = Compiler::with_backend(SafeSys::default());
        for line in &doc.lines {
            print_doc_line(line, &comp);
        }
    } else if let Some(def) = CONSTANTS.iter().find(|def| def.name == name) {
        // Constants
        for frag in def.doc_frags() {
            print_doc_frag(&frag);
        }
    } else {
        eprintln!("No documentation found for `{name}`");
    }
    stdout().flush().unwrap();
}
