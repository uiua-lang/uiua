#![allow(clippy::needless_raw_string_hashes)]

use leptos::*;
use leptos_meta::*;
use leptos_router::*;

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct UiuismsParams {
    search: Option<String>,
}

#[component]
pub fn Uiuisms() -> impl IntoView {
    view! {
        <Title text="Uiuisms - Uiua Docs"/>
        <h1>"Uiuisms"</h1>
        <p>"This page has been removed."</p>
    }
}
