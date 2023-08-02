use leptos::*;
use leptos_router::*;

use crate::editor::*;

#[component]
pub fn Pad(cx: Scope) -> impl IntoView {
    let src =
        move || use_query_map(cx).with(|params| params.get("src").cloned().unwrap_or_default());
    log!("src: {}", src());
    view! { cx,
        <Editor size=EditorSize::Pad example={ &src() }/>
    }
}
