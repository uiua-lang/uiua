//! Worker communication code that runs from the main thread

use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use leptos::prelude::window;
use serde_bytes::ByteBuf;
use wasm_bindgen::{JsCast, closure::Closure};
use web_sys::{ErrorEvent, MessageEvent, Worker, WorkerOptions, WorkerType};

use super::*;
use crate::{
    backend::{self, FILES},
    utils::{self, ChallengeDef},
};

/// Queue a run, returning whether it was dispatched immediately.
pub(crate) fn run(
    editor: u64,
    code: String,
    hidden: String,
    challenge: Option<ChallengeDef>,
    seed: u64,
    on_output: impl Fn(OutputUpdate) + 'static,
    on_complete: impl FnOnce(Result<RunResult, String>) + 'static,
) -> bool {
    cancel(editor, false);
    CLIENT.with(|client| {
        client.borrow_mut().queue.push_back(Job {
            editor,
            request: RunRequest {
                code,
                hidden,
                challenge,
                seed,
                settings: Settings {
                    execution_limit: utils::get_limit_exec_time().then(utils::get_execution_limit),
                    fetch_requests: utils::get_fetch_requests(editor),
                    ast_time: utils::get_ast_time(),
                    top_at_top: utils::get_top_at_top(),
                    animation_format: utils::get_animation_format(),
                },
                files: Vec::new(),
            },
            on_complete: Box::new(on_complete),
            on_output: Rc::new(on_output),
        });
    });
    next_job();
    client(|client| {
        client.status == WorkerStatus::Running
            && client.queue.front().is_some_and(|job| job.editor == editor)
    })
}

/// Cancel the worker for an editor. `notify` determines whether the job's `on_complete` closure should run.
pub(crate) fn cancel(editor: u64, notify: bool) {
    let canceled = CLIENT.with(|client| {
        let client = &mut client.borrow_mut();
        if client.status == WorkerStatus::Running
            && client.queue.front().is_some_and(|job| job.editor == editor)
        {
            client.worker.take();
            client.status = WorkerStatus::Initializing;
        }
        let index = client.queue.iter().position(|job| job.editor == editor)?;
        client.queue.remove(index)
    });
    if notify && let Some(job) = canceled {
        (job.on_complete)(Err("Execution stopped".into()));
    }
    next_job();
}

#[derive(Default)]
struct Client {
    worker: Option<WorkerHandle>,
    status: WorkerStatus,
    queue: VecDeque<Job>,
}

#[derive(Default, PartialEq, Eq)]
enum WorkerStatus {
    #[default]
    Initializing,
    Idle,
    Running,
}

thread_local! {
    static CLIENT: RefCell<Client> = RefCell::default();
}

type OnComplete = Box<dyn FnOnce(Result<RunResult, String>)>;
type OnOutput = Rc<dyn Fn(OutputUpdate)>;

struct Job {
    editor: u64,
    request: RunRequest,
    on_complete: OnComplete,
    on_output: OnOutput,
}

struct WorkerHandle {
    worker: Worker,
    // Kept alive for JS
    _on_message: Closure<dyn FnMut(MessageEvent)>,
    _on_error: Closure<dyn FnMut(ErrorEvent)>,
}

impl Drop for WorkerHandle {
    fn drop(&mut self) {
        self.worker.set_onmessage(None);
        self.worker.set_onerror(None);
        self.worker.terminate();
    }
}

impl Request {
    fn send(&self, worker: &Worker) -> Result<(), String> {
        let (value, transfer) = message(self)?;
        worker
            .post_message_with_transfer(&value, &transfer)
            .map_err(|e| format!("Worker message failed: {e:?}"))
    }
}

fn fail(error: String) {
    let jobs = CLIENT.with(|client| {
        let client = &mut client.borrow_mut();
        client.worker.take();
        client.status = WorkerStatus::Initializing;
        take(&mut client.queue)
    });
    for job in jobs {
        (job.on_complete)(Err(error.clone()));
    }
}

fn client<T>(f: impl FnOnce(&Client) -> T) -> T {
    CLIENT.with(|client| f(&client.borrow()))
}

fn client_mut<T>(f: impl FnOnce(&mut Client) -> T) -> T {
    CLIENT.with(|client| f(&mut client.borrow_mut()))
}

fn next_job() {
    let res = client_mut(|client| {
        if client.status == WorkerStatus::Running || client.queue.is_empty() {
            return Ok(());
        }
        if client.worker.is_none() {
            let opts = WorkerOptions::new();
            opts.set_type(WorkerType::Module);
            let worker = Worker::new_with_options("/worker_loader.js", &opts)
                .map_err(|e| format!("Could not start execution worker: {e:?}"))?;
            let on_message = Closure::new(move |event: MessageEvent| {
                match serde_wasm_bindgen::from_value::<Response>(event.data()) {
                    Ok(response) => response.receive(),
                    Err(error) => fail(format!("Invalid worker response: {error}")),
                }
            });
            let on_error = Closure::new(move |event: ErrorEvent| {
                event.prevent_default();
                fail(format!("Execution worker failed: {}", event.message()));
            });
            worker.set_onmessage(Some(on_message.as_ref().unchecked_ref()));
            worker.set_onerror(Some(on_error.as_ref().unchecked_ref()));
            client.worker = Some(WorkerHandle {
                worker,
                _on_message: on_message,
                _on_error: on_error,
            });
        }
        if client.status != WorkerStatus::Idle {
            return Ok(());
        }
        let mut job = client.queue.pop_front().unwrap();
        job.request.files = FILES.with(|files| {
            (files.borrow().iter())
                .map(|(path, bytes)| (path.clone(), ByteBuf::from(bytes.clone())))
                .collect()
        });
        let request = Request::Run(job.request.into());
        let result = request.send(&client.worker.as_ref().unwrap().worker);
        let Request::Run(request) = request else {
            unreachable!()
        };
        job.request = *request;
        client.queue.push_front(job);
        if result.is_ok() {
            client.status = WorkerStatus::Running;
        }
        result
    });
    if let Err(error) = res {
        fail(error);
    }
}

impl Response {
    fn receive(self) {
        match self {
            Response::Ready => {
                client_mut(|client| client.status = WorkerStatus::Idle);
                next_job();
            }
            Response::Failed(error) => fail(error),
            Response::Bell => backend::weewuh(),
            Response::Input(prompt) => {
                let answer = window().prompt_with_message(&prompt).unwrap_or(None);
                let result = client_mut(|client| {
                    Request::Input(answer).send(&client.worker.as_ref().unwrap().worker)
                });
                if let Err(error) = result {
                    fail(error);
                }
            }
            Response::FetchPermission(url) => {
                let enabled = window()
                    .confirm_with_message(&format!(
                        "Attempting to fetch {url}.\n\n\
                        Enable fetch requests for this pad?"
                    ))
                    .unwrap_or(false);
                let result = client(|client| {
                    if enabled {
                        utils::set_fetch_requests(client.queue.front().unwrap().editor, true);
                    }
                    Request::FetchPermission(enabled).send(&client.worker.as_ref().unwrap().worker)
                });
                if let Err(error) = result {
                    fail(error);
                }
            }
            Response::Output(update) => {
                let on_output =
                    client(|client| client.queue.front().map(|job| job.on_output.clone()));
                if let Some(on_output) = on_output {
                    on_output(update);
                }
            }
            Response::Done(result) => {
                let job = client_mut(|client| {
                    client.status = WorkerStatus::Idle;
                    client.queue.pop_front()
                });
                let Some(job) = job else {
                    return;
                };
                FILES.with(|files| {
                    let mut files = files.borrow_mut();
                    for (path, contents) in &result.files {
                        let before = (job.request.files.iter())
                            .find(|(p, _)| p == path)
                            .map(|(_, bytes)| bytes.as_ref());
                        if files.get(path).map(Vec::as_slice) != before {
                            continue;
                        }
                        if let Some(contents) = contents {
                            files.insert(path.clone(), contents.to_vec());
                        } else {
                            files.remove(path);
                        }
                    }
                });
                if let Some(contents) = &result.clipboard {
                    _ = window().navigator().clipboard().write_text(contents);
                }
                (job.on_complete)(Ok(result));
                next_job();
            }
        }
    }
}
