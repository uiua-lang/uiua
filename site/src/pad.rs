use base64::engine::{general_purpose::URL_SAFE, Engine};
use leptos::*;
use leptos_router::*;

use crate::editor::*;

#[component]
pub fn Pad() -> impl IntoView {
    let mut src = use_query_map()
        .with_untracked(|params| params.get("src").cloned())
        .unwrap_or_default();
    if let Ok(decoded) = URL_SAFE.decode(src.as_bytes()) {
        src = String::from_utf8_lossy(&decoded).to_string();
    }
    view! {
        <Editor size=EditorSize::Pad example={ &src }/>
    }
}
