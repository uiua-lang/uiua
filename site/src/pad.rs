use leptos::*;
use leptos_router::*;

use crate::editor::*;

#[component]
pub fn Pad() -> impl IntoView {
    let src = move || use_query_map().with(|params| params.get("src").cloned().unwrap_or_default());
    log!("src: {}", src());
    view! {
        <Editor size=EditorSize::Pad example={ &src() }/>
    }
}
