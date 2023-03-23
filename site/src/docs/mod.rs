use leptos::*;
use leptos_router::*;

use crate::editor::*;

#[component]
pub fn DocsHome(cx: Scope) -> impl IntoView {
    view! { cx,
        <h2>"Documentation"</h2>
        <h3>"Getting Started"</h3>
        <ul>
            <li><A href="basic">"Basic Operations on the Stack"</A></li>
        </ul>
    }
}

#[component]
pub fn DocsBasic(cx: Scope) -> impl IntoView {
    view! { cx,
        <div>
            <h2>"Basic Operations on the Stack"</h2>
            <Editor examples={&["+1 2"]}/>
        </div>
    }
}
