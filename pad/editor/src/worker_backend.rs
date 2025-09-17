//  NOTE: This file will run a webworker environment. It should not use any browser
//        features that are not supported in webworkers (window, DOM APIs, etc.)

use crate::worker_types::{OutputItem, RunRequest, RunResponse};
use js_sys::JsString;
use serde::{Serialize, Deserialize};
use uiua::{
    BigConstant, Compiler, GitTarget, Handle, ReportFragment, SysBackend, Uiua, UiuaErrorKind,
    media::SmartOutput, now,
};

use leptos_workers::worker;
use web_sys::console;

use std::{
    io::Cursor,
    mem::take,
    sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

pub struct WebWorkerBackend {
    pub stdout: Mutex<Vec<OutputItem>>,
    pub stderr: Mutex<String>,
    pub trace: Mutex<String>,
    _editor_id: String,
    output_enabled: AtomicBool,
}

impl WebWorkerBackend {
    pub fn new(editor_id: String) -> Self {
        Self {
            _editor_id: editor_id,
            stdout: Mutex::new(Vec::new()),
            stderr: Mutex::new(String::new()),
            trace: Mutex::new(String::new()),
            output_enabled: AtomicBool::new(true),
        }
    }

    pub fn finish(&self) {
        // TODO: Any finalization if needed
    }
}

impl Default for WebWorkerBackend {
    fn default() -> Self {
        Self::new("".into())
    }
}

impl SysBackend for WebWorkerBackend {
    fn any(&self) -> &dyn std::any::Any {
        self
    }

    fn any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn output_enabled(&self) -> bool {
        self.output_enabled.load(Ordering::Relaxed)
    }

    fn set_output_enabled(&self, enabled: bool) -> bool {
        self.output_enabled.swap(enabled, Ordering::Relaxed)
    }

    fn print_str_stdout(&self, s: &str) -> Result<(), String> {
        if !self.output_enabled() {
            return Ok(());
        }

        let mut stdout = self.stdout.lock().unwrap();
        let mut lines = s.lines();
        let Some(first) = lines.next() else {
            return Ok(());
        };

        if let Some(OutputItem::String(prev)) = stdout.last_mut() {
            prev.push_str(first);
        } else {
            stdout.push(OutputItem::String(first.into()));
        }

        for line in lines {
            stdout.push(OutputItem::String(line.into()));
        }

        if s.ends_with('\n') {
            stdout.push(OutputItem::String("".into()));
        }

        Ok(())
    }

    fn print_str_stderr(&self, s: &str) -> Result<(), String> {
        if !self.output_enabled() {
            return Ok(());
        }

        self.stderr.lock().unwrap().push_str(s);
        Ok(())
    }

    fn print_str_trace(&self, s: &str) {
        if !self.output_enabled() {
            return;
        }

        self.trace.lock().unwrap().push_str(s);
    }

    fn scan_line_stdin(&self) -> Result<Option<String>, String> {
        // TODO: Implement prompt via JS interop
        return Ok(None);
    }

    fn file_exists(&self, _path: &str) -> bool {
        false
    }

    fn list_dir(&self, _path: &str) -> Result<Vec<String>, String> {
        Err("Listing directories is not supported in this environment".into())
    }

    fn is_file(&self, _path: &str) -> Result<bool, String> {
        Err("Checking if a path is a file is not supported in this environment".into())
    }

    fn delete(&self, _path: &str) -> Result<(), String> {
        Err("Deleting files is not supported in this environment".into())
    }

    fn trash(&self, _path: &str) -> Result<(), String> {
        Err("Trashing files is not supported in this environment".into())
    }

    fn read(&self, _handle: Handle, _count: usize) -> Result<Vec<u8>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }

    fn read_all(&self, _handle: Handle) -> Result<Vec<u8>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }

    fn read_lines<'a>(&self, _handle: Handle) -> Result<uiua::ReadLinesReturnFn<'a>, String> {
        Err("Reading from streams is not supported in this environment".into())
    }

    fn write(&self, _handle: Handle, _contents: &[u8]) -> Result<(), String> {
        Err("Writing to streams is not supported in this environment".into())
    }

    fn seek(&self, _handle: Handle, _offset: uiua::StreamSeek) -> Result<(), String> {
        Err("Seeking streams is not supported in this environment".into())
    }

    fn create_file(&self, _path: &std::path::Path) -> Result<Handle, String> {
        Err("Creating files is not supported in this environment".into())
    }

    fn open_file(&self, _path: &std::path::Path, _write: bool) -> Result<Handle, String> {
        Err("Opening files is not supported in this environment".into())
    }

    fn make_dir(&self, _path: &std::path::Path) -> Result<(), String> {
        Err("Creating directories is not supported in this environment".into())
    }

    fn clipboard(&self) -> Result<String, String> {
        Err("Getting the clipboard is not supported in this environment".into())
    }

    fn set_clipboard(&self, _contents: &str) -> Result<(), String> {
        Err("Setting the clipboard is not supported in this environment".into())
    }

    fn sleep(&self, seconds: f64) -> Result<(), String> {
        let start = now();
        while now() - start < seconds {}
        Ok(())
    }

    fn allow_thread_spawning(&self) -> bool {
        true
    }

    fn show_image(&self, image: image::DynamicImage, label: Option<&str>) -> Result<(), String> {
        let mut bytes = Cursor::new(Vec::new());
        image
            .write_to(&mut bytes, image::ImageFormat::Png)
            .map_err(|e| format!("Failed to show image: {e}"))?;
        self.stdout
            .lock()
            .unwrap()
            .push(OutputItem::Image(bytes.into_inner(), label.map(Into::into)));
        Ok(())
    }

    fn show_gif(&self, gif_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(OutputItem::Gif(gif_bytes, label.map(Into::into)));
        Ok(())
    }

    fn show_apng(&self, apng_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(OutputItem::Apng(apng_bytes, label.map(Into::into)));
        Ok(())
    }

    fn play_audio(&self, wav_bytes: Vec<u8>, label: Option<&str>) -> Result<(), String> {
        (self.stdout.lock().unwrap()).push(OutputItem::Audio(wav_bytes, label.map(Into::into)));
        Ok(())
    }

    fn stream_audio(&self, _f: uiua::AudioStreamFn) -> Result<(), String> {
        Err("Streaming audio is not supported in this environment".into())
    }

    fn now(&self) -> f64 {
        now()
    }

    fn close(&self, _handle: Handle) -> Result<(), String> {
        Ok(())
    }

    fn change_directory(&self, _path: &str) -> Result<(), String> {
        Err("Changing directories is not supported in this environment".into())
    }

    fn get_current_directory(&self) -> Result<String, String> {
        Err("Getting the current directory is not supported in this environment".into())
    }

    fn load_git_module(
        &self,
        _url: &str,
        _target: GitTarget,
    ) -> Result<std::path::PathBuf, String> {
        Err("Loading git modules is not supported in this environment".into())
    }

    fn breakpoint(&self, _env: &Uiua) -> Result<bool, String> {
        Err("Breakpoints are not supported in this environment".into())
    }

    fn big_constant(&self, _key: BigConstant) -> Result<std::borrow::Cow<'static, [u8]>, String> {
        Err("Retrieval of big constants is not supported in this environment".into())
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Test {
    pub a: String,
}

#[worker(UiuaRunnerX)]
pub async fn ping(test: Test) -> String {
    console::log_1(&JsString::from("Worker: Ping received"));
    "Pong from worker".into()
}

#[worker(UiuaRunner)]
#[allow(clippy::mutable_key_type)]
pub async fn run_code(req: RunRequest) -> RunResponse {
    let limit = req.execution_limit_secs.unwrap_or(2.0);

    let backend = WebWorkerBackend::new(req.editor_id);
    let mut rt = Uiua::with_backend(backend)
        .with_execution_limit(Duration::from_secs_f64(limit))
        .with_recursion_limit(50);

    let mut comp = Compiler::with_backend(WebWorkerBackend::default());
    let result = comp.load_str(&req.code).map(|c| rt.run_compiler(c));

    let mut error = None;

    let (value_lines, io) = match result {
        Ok(Ok(())) => {
            let stack = rt.take_stack_lines();
            let backend = rt.downcast_backend::<WebWorkerBackend>().unwrap();
            backend.finish();
            (stack, backend)
        }
        Ok(Err(e)) if matches!(*e.kind, UiuaErrorKind::Interrupted) => (
            rt.take_stack_lines(),
            rt.downcast_backend::<WebWorkerBackend>().unwrap(),
        ),
        Ok(Err(e)) => {
            error = Some(e);
            (
                rt.take_stack_lines(),
                rt.downcast_backend::<WebWorkerBackend>().unwrap(),
            )
        }
        Err(e) => {
            error = Some(e);
            (
                Vec::new(),
                &comp.take_backend::<WebWorkerBackend>().unwrap(),
            )
        }
    };

    // Get stdout and stderr
    let stdout = take(&mut *io.stdout.lock().unwrap());
    let stderr = take(&mut *io.stderr.lock().unwrap());
    let trace = take(&mut *io.trace.lock().unwrap());

    // Convert outputs to smart values
    let mut stack_lines = Vec::new();
    let value_count: usize = value_lines.iter().map(|line| line.len()).sum();
    let make_smart_output = if req.animation_format.as_deref() == Some("APNG") {
        SmartOutput::from_value_prefer_apng
    } else {
        SmartOutput::from_value
    };

    let mut i = 0;
    for value_line in value_lines {
        let mut stack_line = Vec::new();
        for value in value_line {
            let value = match make_smart_output(value, 24.0, io) {
                SmartOutput::Png(bytes, label) => {
                    stack_line.push(OutputItem::Image(bytes, label));
                    continue;
                }
                SmartOutput::Gif(bytes, label) => {
                    stack_line.push(OutputItem::Gif(bytes, label));
                    continue;
                }
                SmartOutput::Apng(bytes, label) => {
                    stack_line.push(OutputItem::Apng(bytes, label));
                    continue;
                }
                SmartOutput::Wav(bytes, label) => {
                    stack_line.push(OutputItem::Audio(bytes, label));
                    continue;
                }
                SmartOutput::Svg { svg, original } => {
                    stack_line.push(OutputItem::Svg(
                        svg,
                        original.meta.label.as_ref().map(Into::into),
                    ));
                    continue;
                }
                SmartOutput::Normal(value) => value,
            };

            // Otherwise, just show the value
            let class = if value_count == 1 {
                ""
            } else {
                match i % 6 {
                    0 => "output-a",
                    1 => "output-b",
                    2 => "output-c",
                    3 => "output-d",
                    4 => "output-e",
                    5 => "output-f",
                    _ => unreachable!(),
                }
            };
            stack_line.push(OutputItem::Classed(class.to_string(), value));
            i += 1;
        }
        stack_lines.push(stack_line);
    }

    // Construct output
    let mut output = Vec::new();

    let diagnostics = comp.take_diagnostics();
    let label = ((!stack_lines.is_empty()) as u8)
        + ((!stdout.is_empty()) as u8)
        + ((!stderr.is_empty()) as u8)
        + ((!trace.is_empty()) as u8)
        >= 2;

    if !trace.is_empty() {
        output.extend((trace.lines()).map(|line| vec![OutputItem::String(line.into())]));
    }

    if !stdout.is_empty() {
        if !output.is_empty() {
            output.push(vec![OutputItem::String("".into())]);
        }
        if label {
            output.push(vec![OutputItem::String("stdout:".to_string())]);
        }
        output.extend(stdout.into_iter().map(|item| vec![item]));
    }

    if !stderr.is_empty() {
        if !output.is_empty() {
            output.push(vec![OutputItem::String("".into())]);
        }

        if label {
            output.push(vec![OutputItem::String("stderr:".to_string())]);
        }

        output.extend((stderr.lines()).map(|line| vec![OutputItem::String(line.into())]));
    }

    if !stack_lines.is_empty() {
        if label {
            output.push(vec![OutputItem::Separator]);
        }

        output.extend(stack_lines);
    }

    if let Some(error) = &error {
        if !output.is_empty() {
            output.push(vec![OutputItem::String("".into())]);
        }

        const MAX_OUTPUT_BEFORE_ERROR: usize = 60;
        if output.len() >= MAX_OUTPUT_BEFORE_ERROR {
            output = output.split_off(output.len() - MAX_OUTPUT_BEFORE_ERROR);
            output[0] = vec![OutputItem::String("Previous output truncated...".into())];
        }

        let report = error.report();
        let execution_limit_reached = report.fragments.iter().any(|frag| matches!(frag, ReportFragment::Plain(s) if s.contains("Maximum execution time exceeded")));
        output.push(vec![OutputItem::Report(report)]);

        if execution_limit_reached {
            output.push(vec![OutputItem::String(
                "You can increase the execution time limit in the editor settings".into(),
            )]);
        }
    }

    if !diagnostics.is_empty() {
        if !output.is_empty() {
            output.push(vec![OutputItem::String("".into())]);
        }

        for diag in diagnostics {
            output.push(vec![OutputItem::Report(diag.report())]);
        }
    }

    output.extend(
        rt.take_reports()
            .into_iter()
            .map(|r| vec![OutputItem::Report(r)]),
    );

    RunResponse { output, error }
}
