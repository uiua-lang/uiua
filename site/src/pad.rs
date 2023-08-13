use leptos::*;
use leptos_router::*;

use crate::editor::*;

#[component]
pub fn Pad() -> impl IntoView {
    let src = use_query_map()
        .with_untracked(|params| params.get("src").cloned())
        .unwrap_or_default();
    view! {
        <Editor size=EditorSize::Pad example={ &src }/>
    }
}
