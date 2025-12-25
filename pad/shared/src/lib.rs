use serde::{Deserialize, Serialize};
use uiua::{Diagnostic, Report};

#[derive(Serialize, Deserialize)]
pub struct RunRequest {
    pub id: u64,
    pub code: String,
    #[serde(default)]
    pub execution_limit_secs: Option<f64>,
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum WorkerRequest {
    Run(RunRequest),
}

/// Message types sent from worker back to main thread
#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum WorkerResponse {
    /// Result of running code
    RunResult {
        id: u64,
        output: Vec<SerializableOutputItem>,
        error: Option<Report>,
        diagnostics: Vec<Diagnostic>,
    },
    /// An error occurred
    Error { id: u64, message: String },
}

/// Serializable version of OutputItem for transfer across worker boundary
#[derive(Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum SerializableOutputItem {
    String(String),
    Image {
        data: Vec<u8>,
        label: Option<String>,
    },
    Gif {
        data: Vec<u8>,
        label: Option<String>,
    },
    Apng {
        data: Vec<u8>,
        label: Option<String>,
    },
    Audio {
        data: Vec<u8>,
        label: Option<String>,
    },
}
