use leptos::*;

use crate::editor::*;

#[component]
pub fn Pad(cx: Scope) -> impl IntoView {
    view! { cx,
        <div>
        <p>"This editor saves your code on run."</p>
        <Editor size=EditorSize::Pad/>
        </div>
    }
}
