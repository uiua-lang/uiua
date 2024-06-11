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
        <ul>
            <li>"It provided a 'definitive' implementation of many algorithms where the best implementation is subjective and/or context-dependent"</li>
            <li>"It is not how I want people to approach learning Uiua"</li>
            <li>"It is not something I am interested in maintaining"</li>
        </ul>
    }
}
