pub mod backend;

use uiua_pad_shared::{RunRequest, WorkerRequest, WorkerResponse, SerializableOutputItem};
use std::time::Duration;

use backend::WebBackend;
use uiua::{Compiler, Uiua, UiuaErrorKind, Value};
use wasm_bindgen::prelude::*;
use web_sys::{DedicatedWorkerGlobalScope, MessageEvent};

#[wasm_bindgen(start)]
pub fn worker_init() {
    console_error_panic_hook::set_once();

    let global = js_sys::global().unchecked_into::<DedicatedWorkerGlobalScope>();
    let closure = Closure::wrap(Box::new(|event: MessageEvent| {
        let result = handle_message(event.data());
        let global = js_sys::global().unchecked_into::<DedicatedWorkerGlobalScope>();
        global.post_message(&result).unwrap();
    }) as Box<dyn Fn(MessageEvent)>);

    global.set_onmessage(Some(closure.as_ref().unchecked_ref()));
    closure.forget(); // Leak the closure so it lives forever
}

/// Handle incoming message from main thread
#[wasm_bindgen]
pub fn handle_message(message: JsValue) -> JsValue {
    let request: WorkerRequest = match serde_wasm_bindgen::from_value(message) {
        Ok(req) => req,
        Err(e) => {
            let response = WorkerResponse::Error {
                id: 0,
                message: format!("Failed to parse message: {e}"),
            };
            return serde_wasm_bindgen::to_value(&response).unwrap();
        }
    };

    let response = match request {
        WorkerRequest::Run(request) => run_code(&request),
    };

    serde_wasm_bindgen::to_value(&response).unwrap()
}

fn run_code(request: &RunRequest) -> WorkerResponse {
    let limit = request.execution_limit_secs.unwrap_or(2.0);

    // Create runtime with backend
    let backend = WebBackend::default();
    let mut rt = Uiua::with_backend(backend)
        .with_execution_limit(Duration::from_secs_f64(limit))
        .with_recursion_limit(50);

    // Compile and run
    let mut comp = Compiler::with_backend(WebBackend::default());
    let result = comp
        .load_str(request.code.as_str())
        .map(|comp| rt.run_compiler(comp));

    // Collect output
    let (output, error) = match result {
        Ok(Ok(())) => {
            let stack = rt.take_stack();
            let backend = rt.downcast_backend::<WebBackend>().unwrap();
            (collect_output(backend, stack), None)
        }
        Ok(Err(e)) if matches!(*e.kind, UiuaErrorKind::Interrupted) => {
            let stack = rt.take_stack();
            let backend = rt.downcast_backend::<WebBackend>().unwrap();
            (collect_output(backend, stack), None)
        }
        Ok(Err(e)) => {
            let stack = rt.take_stack();
            let backend = rt.downcast_backend::<WebBackend>().unwrap();
            (
                collect_output(backend, stack),
                Some(e.report()),
            )
        }
        Err(e) => (Vec::new(), Some(e.report())),
    };

    WorkerResponse::RunResult {
        id: request.id,
        output,
        error,
        diagnostics: comp.take_diagnostics().into_iter().collect(),
    }
}

fn collect_output(_backend: &WebBackend, stack: Vec<Value>) -> Vec<SerializableOutputItem> {
    let mut output = Vec::new();

    // Collect final stack output
    for value in stack {
        output.push(SerializableOutputItem::String(format!("{value}")));
    }

    output
}
