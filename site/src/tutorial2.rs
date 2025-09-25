use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use uiua_editor::lang;

use crate::{markdown::Markdown, other_tutorial::OtherTutorialParams};

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct TutorialParams {
    page: Option<String>,
}

#[component]
pub fn Tutorial2() -> impl IntoView {
    move || match use_params::<OtherTutorialParams>().get() {
        Ok(params) => view! {
            <A href="/docs">"Back to Docs Home"</A>
            <br/>
            <br/>
            { params.page.view() }
            <br/>
            <br/>
            <A href="/docs">"Back to Docs Home"</A>
        }
        .into_view(),
        Err(_) => {
            let params = use_params::<TutorialParams>().get().unwrap();
            page_view(
                (params.page.as_deref())
                    .filter(|s| !s.is_empty())
                    .unwrap_or("Introduction"),
            )
            .into_view()
        }
    }
}

const TUTORIAL_NAMES: &[&str] = &[
    "Introduction",
    "Basic Data Manipulation and Formatting",
    "Math and Comparison",
];
fn next_page(name: &str) -> Option<&'static str> {
    (TUTORIAL_NAMES.iter().copied())
        .skip_while(|&n| n != name)
        .nth(1)
}
fn prev_page(name: &str) -> Option<&'static str> {
    (TUTORIAL_NAMES.iter().rev().copied())
        .skip_while(|&n| n != name)
        .nth(1)
}

fn page_view(name: &str) -> impl IntoView {
    view! {
        <Title text=format!("{name} - {} Docs", lang())/>
        <A href="/docs">"Back to Docs Home"</A>
        <br/>
        <br/>
        <TutorialNav name=name/>
        <Markdown src={format!("/tutorial2/{name}.md")}/>
        <br/>
        <br/>
        <TutorialNav name=name/>
        <br/>
        <br/>
        <A href="/docs">"Back to Docs Home"</A>
    }
}

#[component]
#[allow(clippy::needless_lifetimes)]
fn TutorialNav<'a>(name: &'a str) -> impl IntoView {
    let nm = name.to_string();
    let next = move || {
        next_page(&nm)
            .map(|p| {
                view!( <div>"Next: "<A href=format!("/tutorial2/{p}")>{p}</A>" 〉"</div>)
                    .into_view()
            })
            .unwrap_or_else(|| view!( <div/>).into_view())
    };
    let nm = name.to_string();
    let previous = move || {
        prev_page(&nm)
            .map(|p| {
                view!( <div>"〈 Previous: "<A href=format!("/tutorial2/{p}")>{p}</A></div>)
                    .into_view()
            })
            .unwrap_or_else(|| view!( <div/>).into_view())
    };

    view! {
        <div class="tutorial-nav">
            { previous }
            { next }
        </div>
    }
}
