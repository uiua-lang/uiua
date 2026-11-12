use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use leptos::prelude::window;
use serde_bytes::ByteBuf;
use wasm_bindgen::{JsCast, closure::Closure};
use web_sys::{ErrorEvent, MessageEvent, Worker};

use super::*;
use crate::{
    backend::{self, FILES},
    utils::{self, ChallengeDef},
};

pub(crate) fn run(
    editor: u64,
    code: String,
    hidden: String,
    challenge: Option<ChallengeDef>,
    seed: u64,
    on_output: impl Fn(OutputUpdate) + 'static,
    on_complete: impl FnOnce(Result<RunResult, String>) + 'static,
) {
    cancel(editor);
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
}

pub(crate) fn cancel(editor: u64) {
    let canceled = CLIENT.with(|client| {
        let client = &mut client.borrow_mut();
        let mut canceled = Vec::new();
        for _ in 0..client.queue.len() {
            let job = client.queue.pop_front().unwrap();
            if job.editor == editor {
                canceled.push(job.on_complete);
            } else {
                client.queue.push_back(job);
            }
        }
        if (client.active.as_ref()).is_some_and(|job| job.editor == editor) {
            canceled.push(client.active.take().unwrap().on_complete);
            client.worker.take();
            client.ready = false;
        }
        canceled
    });
    for on_complete in canceled {
        on_complete(Err("Execution stopped".into()));
    }
    next_job();
}

#[derive(Default)]
struct Client {
    worker: Option<WorkerHandle>,
    ready: bool,
    active: Option<ActiveJob>,
    queue: VecDeque<Job>,
}

thread_local! {
    static CLIENT: RefCell<Client> = RefCell::default();
}

type OnComplete = Box<dyn FnOnce(Result<RunResult, String>)>;

struct Job {
    editor: u64,
    request: RunRequest,
    on_complete: OnComplete,
    on_output: Rc<dyn Fn(OutputUpdate)>,
}

struct ActiveJob {
    editor: u64,
    files: Files,
    on_complete: OnComplete,
    on_output: Rc<dyn Fn(OutputUpdate)>,
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
    let on_completes = CLIENT.with(|client| {
        let client = &mut client.borrow_mut();
        client.worker.take();
        client.ready = false;
        let mut on_completes: Vec<_> = (client.active.take())
            .map(|job| job.on_complete)
            .into_iter()
            .collect();
        on_completes.extend(client.queue.drain(..).map(|job| job.on_complete));
        on_completes
    });
    for on_complete in on_completes {
        on_complete(Err(error.clone()));
    }
}

fn next_job() {
    let res = CLIENT.with(|client| {
        let client = &mut client.borrow_mut();
        if client.active.is_some() || client.queue.is_empty() {
            return Ok(());
        }
        if client.worker.is_none() {
            let worker = Worker::new("/worker_loader.js")
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
        if !client.ready {
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
        client.active = Some(ActiveJob {
            editor: job.editor,
            files: request.files,
            on_complete: job.on_complete,
            on_output: job.on_output,
        });
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
                CLIENT.with(|client| client.borrow_mut().ready = true);
                next_job();
            }
            Response::Failed(error) => fail(error),
            Response::Input(prompt) => {
                let answer = window().prompt_with_message(&prompt).unwrap_or(None);
                let result = CLIENT.with(|client| {
                    let client = client.borrow();
                    Request::Input(answer).send(&client.worker.as_ref().unwrap().worker)
                });
                if let Err(error) = result {
                    fail(error);
                }
            }
            Response::Output(update) => {
                let on_output = CLIENT.with(|client| {
                    let client = client.borrow();
                    client.active.as_ref().map(|job| job.on_output.clone())
                });
                if let Some(on_output) = on_output {
                    on_output(update);
                }
            }
            Response::Done(result) => {
                let job = CLIENT.with(|client| client.borrow_mut().active.take());
                let Some(job) = job else {
                    return;
                };
                FILES.with(|files| {
                    let mut files = files.borrow_mut();
                    for (path, contents) in &result.files {
                        let before = (job.files.iter())
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
                if result.bell {
                    backend::weewuh();
                }
                (job.on_complete)(Ok(result));
                next_job();
            }
        }
    }
}
