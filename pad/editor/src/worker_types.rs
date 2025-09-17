use serde::{Deserialize, Serialize};
use uiua::{Report, UiuaError};

#[derive(Clone, Serialize, Deserialize)]
pub struct RunRequest {
    pub editor_id: String,
    pub code: String,
    #[serde(default)]
    pub execution_limit_secs: Option<f64>,
    pub animation_format: Option<String>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub struct RunResponse {
    pub output: Vec<Vec<OutputItem>>,
    pub error: Option<UiuaError>,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum OutputItem {
    String(String),
    Svg(String, Option<String>),
    Image(Vec<u8>, Option<String>),
    Gif(Vec<u8>, Option<String>),
    Apng(Vec<u8>, Option<String>),
    Audio(Vec<u8>, Option<String>),
    Report(Report),
    Faint(String),
    Classed(String, String),
    Separator,
}

impl OutputItem {
    pub fn is_report(&self) -> bool {
        matches!(self, OutputItem::Report(_))
    }
}
