//! Running the interpreter in a web worker

pub mod client;

use std::{
    cell::RefCell,
    collections::HashMap,
    mem::{replace, take},
    path::PathBuf,
};

use futures::channel::oneshot;
use js_sys::{Array, Object, Uint8Array};
use serde::*;
use serde_bytes::ByteBuf;
use wasm_bindgen::{JsCast, JsValue, closure::Closure};
use wasm_bindgen_futures::{JsFuture, spawn_local};
use web_sys::{DedicatedWorkerGlobalScope, MessageEvent};

use crate::{
    backend::{self, FILES, OutputItem},
    utils::{self, ChallengeDef},
};

pub type Files = Vec<(PathBuf, ByteBuf)>; // Use `ByteBuf` so they serialize nicely
/// Changes to files. Set or delete the contents.
pub type FileChanges = Vec<(PathBuf, Option<ByteBuf>)>;

#[derive(Serialize, Deserialize)]
pub enum OutputUpdate {
    Reset,
    Section(Option<String>),
    Stdout(String),
    Stderr(String),
    Trace(String),
    Item(OutputItem),
}

/// Output from an editor execution, including challenge sections.
#[derive(Default, Serialize, Deserialize)]
pub struct Output {
    /// Only challenges have multiple sections
    pub sections: Vec<OutputSection>,
    /// Extra message at the end. Used for challenges
    pub message: Option<String>,
}

#[derive(Default, Serialize, Deserialize)]
pub struct OutputSection {
    pub heading: Option<String>,
    pub stdout: Vec<OutputItem>,
    pub stderr: String,
    pub trace: String,
    pub stack: Vec<Vec<OutputItem>>,
    pub reports: Vec<OutputItem>,
    /// Limit output preceding a runtime error.
    pub truncate: bool,
}

impl Output {
    pub fn section(&mut self) -> &mut OutputSection {
        if self.sections.is_empty() {
            self.sections.push(OutputSection::default());
        }
        self.sections.last_mut().unwrap()
    }
    pub fn update(&mut self, update: OutputUpdate) {
        match update {
            OutputUpdate::Reset => *self = Self::default(),
            OutputUpdate::Section(heading) => self.sections.push(OutputSection {
                heading,
                ..Default::default()
            }),
            OutputUpdate::Stdout(s) => backend::append_stdout(&mut self.section().stdout, &s),
            OutputUpdate::Stderr(s) => self.section().stderr.push_str(&s),
            OutputUpdate::Trace(s) => self.section().trace.push_str(&s),
            OutputUpdate::Item(item) => self.section().stdout.push(item),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Settings {
    pub execution_limit: Option<f64>,
    /// Time to sample `&ast`
    pub ast_time: f64,
    pub top_at_top: bool,
    pub animation_format: String,
}

#[derive(Serialize, Deserialize)]
pub struct RunRequest {
    pub code: String,
    pub hidden: String,
    pub challenge: Option<ChallengeDef>,
    pub seed: u64,
    pub settings: Settings,
    pub files: Files,
}

#[derive(Serialize, Deserialize)]
pub enum Request {
    Run(Box<RunRequest>),
    Input(Option<String>),
}

#[derive(Serialize, Deserialize)]
pub struct RunResult {
    pub output: Output,
    pub initialized_challenge: bool,
    pub files: FileChanges,
    pub clipboard: Option<String>,
    pub bell: bool,
}

#[derive(Serialize, Deserialize)]
pub enum Response {
    Ready,
    Input(String),
    Output(OutputUpdate),
    Done(RunResult),
    Failed(String),
}

/// Check if this is a worker thread
pub fn is_worker() -> bool {
    #[cfg(target_arch = "wasm32")]
    {
        web_sys::window().is_none()
    }
    #[cfg(not(target_arch = "wasm32"))]
    false
}

/// Make a serializable value sendable to/from the worker
pub fn message(value: &impl Serialize) -> Result<(JsValue, Array), String> {
    let value = value
        .serialize(
            &serde_wasm_bindgen::Serializer::new()
                .serialize_maps_as_objects(true)
                .serialize_large_number_types_as_bigints(true),
        )
        .map_err(|e| e.to_string())?;
    fn buffers(value: &JsValue, transfer: &Array) {
        if value.is_instance_of::<Uint8Array>() {
            transfer.push(&value.unchecked_ref::<Uint8Array>().buffer());
        } else if Array::is_array(value) {
            for value in value.unchecked_ref::<Array>().iter() {
                buffers(&value, transfer);
            }
        } else if value.is_object() {
            for value in Object::values(value.unchecked_ref()).iter() {
                buffers(&value, transfer);
            }
        }
    }
    let transfer = Array::new();
    buffers(&value, &transfer);
    Ok((value, transfer))
}

#[derive(Default)]
struct WorkerState {
    /// Streaming is
    suppress_streaming: bool,
    /// Answers to stdin/clipboard prompts
    inputs: Vec<Option<String>>,
    next_input: usize,
    input_prompt: Option<String>,
    input_reply: Option<oneshot::Sender<Option<String>>>,
    /// Pending clipboard write
    clipboard: Option<String>,
    /// Whether to play the bell sound
    bell: bool,
}

impl WorkerState {
    fn reset_attempt(&mut self) {
        self.next_input = 0;
        self.input_prompt = None;
        self.clipboard.take();
        self.bell = false;
    }
}

thread_local! {
    static STATE: RefCell<WorkerState> = RefCell::default();
}

fn state<T>(f: impl FnOnce(&WorkerState) -> T) -> T {
    STATE.with(|state| f(&state.borrow()))
}
fn state_mut<T>(f: impl FnOnce(&mut WorkerState) -> T) -> T {
    STATE.with(|state| f(&mut state.borrow_mut()))
}

pub fn without_streaming_output<T>(f: impl FnOnce() -> T) -> T {
    let previous = state_mut(|state| replace(&mut state.suppress_streaming, true));
    let result = f();
    state_mut(|state| state.suppress_streaming = previous);
    result
}

impl OutputUpdate {
    pub fn send(self) {
        if !state(|state| state.suppress_streaming) && is_worker() {
            _ = Response::Output(self).send();
        }
    }
}

impl OutputItem {
    pub fn output(&self) {
        if !state(|state| state.suppress_streaming) && is_worker() {
            OutputUpdate::Item(self.clone()).send();
        }
    }
}

pub fn input(prompt: &str) -> Result<Option<String>, String> {
    state_mut(|state| {
        let index = state.next_input;
        state.next_input += 1;
        if let Some(answer) = state.inputs.get(index).cloned() {
            return Ok(answer);
        }
        state.input_prompt.get_or_insert_with(|| prompt.into());
        Err("Waiting for input".into())
    })
}

pub fn set_clipboard(contents: &str) {
    state_mut(|state| state.clipboard = Some(contents.into()));
}

pub fn bell() {
    state_mut(|state| state.bell = true);
}

impl Response {
    fn send(&self) -> Result<(), String> {
        let (value, transfer) = message(self)?;
        js_sys::global()
            .unchecked_into::<DedicatedWorkerGlobalScope>()
            .post_message_with_transfer(&value, &transfer)
            .map_err(|e| format!("Worker response failed: {e:?}"))
    }
}

async fn wait() {
    let promise = js_sys::Promise::new(&mut |resolve, reject| {
        if let Err(error) = js_sys::global()
            .unchecked_into::<DedicatedWorkerGlobalScope>()
            .set_timeout_with_callback_and_timeout_and_arguments_0(&resolve, 10)
        {
            _ = reject.call1(&JsValue::NULL, &error);
        }
    });
    _ = JsFuture::from(promise).await;
}

impl RunRequest {
    async fn execute(mut self) -> Result<RunResult, String> {
        state_mut(take);
        FILES.with(|files| files.borrow_mut().clear());
        loop {
            FILES.with(|files| {
                let files = &mut *files.borrow_mut();
                files.retain(|path, _| path.starts_with("uiua-modules"));
                files.extend(
                    (self.files.iter()).map(|(path, bytes)| (path.clone(), bytes.to_vec())),
                );
            });
            Response::Output(OutputUpdate::Reset).send()?;
            state_mut(WorkerState::reset_attempt);
            uiua::seed_random(self.seed);
            // Replays use the original files, seed, and challenge state; only recorded answers change.
            let challenge = self.challenge.clone();
            let output =
                utils::run_code(&self.code, &self.hidden, challenge.as_ref(), &self.settings);
            if let Some(prompt) = state_mut(|state| state.input_prompt.take()) {
                let (send, reply) = oneshot::channel();
                state_mut(|state| state.input_reply = Some(send));
                Response::Input(prompt).send()?;
                let answer = (reply.await).map_err(|_| "Input request canceled".to_string())?;
                state_mut(|state| state.inputs.push(answer));
                continue;
            }
            if backend::pending_fetches() > 0 {
                while backend::pending_fetches() > 0 {
                    wait().await;
                }
                continue;
            }
            let mut before: HashMap<_, _> = self.files.drain(..).collect();
            let mut changes = Vec::new();
            FILES.with(|files| {
                for (path, bytes) in files.borrow().iter() {
                    if before.remove(path).as_deref() != Some(bytes) {
                        changes.push((path.clone(), Some(ByteBuf::from(bytes.clone()))));
                    }
                }
            });
            changes.extend(before.into_keys().map(|path| (path, None)));
            let (clipboard, bell) =
                state_mut(|state| (take(&mut state.clipboard), replace(&mut state.bell, false)));
            return Ok(RunResult {
                output,
                initialized_challenge: (challenge.as_ref())
                    .is_some_and(|challenge| challenge.did_init_run.get()),
                files: changes,
                clipboard,
                bell,
            });
        }
    }
}

/// Entry point for the site's execution worker.
pub fn start() {
    crate::START_TIME.get_or_init(|| js_sys::Date::now() / 1000.0);
    let on_message = Closure::<dyn FnMut(MessageEvent)>::new(move |event: MessageEvent| {
        match serde_wasm_bindgen::from_value(event.data()) {
            Ok(Request::Run(request)) => spawn_local(async move {
                match request.execute().await {
                    Ok(result) => _ = Response::Done(result).send(),
                    Err(error) => _ = Response::Failed(error).send(),
                }
            }),
            Ok(Request::Input(answer)) => {
                if let Some(reply) = state_mut(|state| state.input_reply.take()) {
                    _ = reply.send(answer);
                }
            }
            Err(error) => {
                _ = Response::Failed(format!("Invalid execution request: {error}")).send()
            }
        }
    });
    js_sys::global()
        .unchecked_into::<DedicatedWorkerGlobalScope>()
        .set_onmessage(Some(on_message.as_ref().unchecked_ref()));
    on_message.forget();
    _ = Response::Ready.send();
}
