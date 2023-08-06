mod design;
mod primitive;
mod tutorial;

use std::{collections::HashSet, iter::once};

use enum_iterator::all;
use instant::Duration;
use leptos::*;
use leptos_router::*;
use uiua::primitive::{PrimClass, Primitive};
use wasm_bindgen::JsCast;
use web_sys::{Event, EventInit, HtmlInputElement, ScrollBehavior, ScrollIntoViewOptions};

use crate::{code::*, element};
use design::*;
use primitive::*;
use tutorial::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocsPage {
    Tutorial(TutorialPage),
    Design,
    Search(String),
}

impl IntoParam for DocsPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        let value = value.unwrap_or("");
        all::<TutorialPage>()
            .find(|p| p.path() == value)
            .map(Self::Tutorial)
            .or(match value {
                "" => None,
                "design" => Some(Self::Design),
                value => Some(Self::Search(value.into())),
            })
            .ok_or_else(|| ParamsError::MissingParam(name.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct DocsParams {
    page: DocsPage,
}

#[component]
pub fn Docs(cx: Scope) -> impl IntoView {
    move || {
        let Ok(params) = use_params::<DocsParams>(cx).get() else {
            return view!(cx, <DocsHome/>).into_view(cx);
        };
        let page = params.page;
        let page_view = match page {
            DocsPage::Tutorial(tut) => view!(cx, <Tutorial page=tut/>).into_view(cx),
            DocsPage::Design => view!(cx, <Design/>).into_view(cx),
            DocsPage::Search(search) => view!(cx, <DocsHome search=search/>).into_view(cx),
        };

        view! { cx,
            <A href="/docs">"Back to Docs Home"</A>
            <br/>
            <br/>
            { page_view }
            <br/>
            <br/>
            <A href="/docs">"Back to Docs Home"</A>
        }
        .into_view(cx)
    }
}

fn scroll_to_docs_functions(options: &ScrollIntoViewOptions) {
    element::<HtmlInputElement>("function-search")
        .scroll_into_view_with_scroll_into_view_options(options);
}

#[component]
fn DocsHome(cx: Scope, #[prop(optional)] search: String) -> impl IntoView {
    let search = urlencoding::decode(&search)
        .map(|s| s.into_owned())
        .unwrap_or_default();
    let (search, _) = create_signal(cx, search);
    let (results, set_result) = create_signal(cx, None);
    let (clear_button, set_clear_button) = create_signal(cx, None);
    let (old_allowed, set_old_allowed) = create_signal(cx, Allowed::all());
    let update_search = move |text: &str| {
        // Update clear button
        set_clear_button.set(if text.is_empty() {
            None
        } else {
            // let (redirect, set_redirect) = create_signal(cx, None);
            let clear_search = move |_| {
                let search_input = element::<HtmlInputElement>("function-search");
                search_input.set_value("");
                _ = search_input.dispatch_event(
                    &Event::new_with_event_init_dict("input", EventInit::new().bubbles(true))
                        .unwrap(),
                );
            };
            Some(view!(cx, {}<button on:click=clear_search>"✕"</button>).into_view(cx))
        });

        // Derive allowed primitives
        let allowed = Allowed::from_search(text);
        if allowed == old_allowed.get() && results.get().is_some() {
            return;
        }
        if !text.is_empty() {
            scroll_to_docs_functions(ScrollIntoViewOptions::new().behavior(ScrollBehavior::Smooth));
        }
        set_old_allowed.set(allowed.clone());
        set_result.set(Some(
            if allowed.classes.is_empty() && allowed.prims.is_empty() {
                // No Results
                view!(cx, <p>"No results"</p>).into_view(cx)
            } else if allowed.prims.len() == 1
                && [PrimClass::all().count(), 1].contains(&allowed.classes.len())
            {
                // Only one result
                let prim = allowed.prims.into_iter().next().unwrap();
                scroll_to_docs_functions(
                    ScrollIntoViewOptions::new().behavior(ScrollBehavior::Instant),
                );
                view!(cx, <PrimDocs prim=prim/>).into_view(cx)
            } else {
                // Multiple results
                allowed.table(cx).into_view(cx)
            },
        ));
    };

    set_timeout(move || update_search(&search.get()), Duration::from_secs(0));
    let search_input = move |event: Event| {
        let elem: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
        update_search(&elem.value());
    };

    view! { cx,
        <h1>"Documentation"</h1>
        <h2 id="tutorial">"Tutorial"</h2>
        <p>"These are meant to be read in order:"</p>
        <ul>{ all::<TutorialPage>()
            .map(|p| view!(cx, <li><A href={format!("/docs/{}", p.path())}>{p.title()}</A></li>))
            .collect::<Vec<_>>()
        }</ul>
        <h2 id="other-docs">"Other Docs"</h2>
        <ul>
            <li><A href="/docs/design">"Design"</A>" - reasons for some of Uiua's design decisions"</li>
        </ul>
        <h2 id="functions" class="doc-functions">"Functions"</h2>
        <div class="input-div">
            "⌕ "
            <input
                id="function-search"
                type="text"
                value={ search.get() }
                on:input=search_input
                pattern="[^0-9]"
                placeholder="Search by name, glyph, or category..."/>
            { move || clear_button.get() }
        </div>
        { move|| results.get() }
        <div style="height: 50vh;"></div>
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
struct Allowed {
    classes: HashSet<PrimClass>,
    prims: HashSet<Primitive>,
}

impl Allowed {
    fn all() -> Self {
        Self {
            classes: PrimClass::all().collect(),
            prims: Primitive::all().collect(),
        }
    }
    fn from_search(search: &str) -> Self {
        let search = search.trim().to_lowercase();
        let parts: Vec<_> = search
            .split([' ', ','])
            .filter(|&part| part.chars().any(|c| !c.is_ascii_digit()))
            .collect();
        if parts.is_empty() {
            return Self::all();
        }
        let mut prims = HashSet::new();
        let all = Primitive::all;
        let prim_matching_part_exactly = |part: &str| -> Option<Primitive> {
            all().find(|p| {
                p.names().is_some_and(|n| {
                    n.text.to_lowercase() == part
                        || n.ascii.is_some_and(|a| a.to_string() == part)
                        || n.unicode.is_some_and(|u| part.chars().all(|c| c == u))
                })
            })
        };
        if let Some(prim) = prim_matching_part_exactly(&search) {
            prims.insert(prim);
        } else {
            for &part in &parts {
                if let Some(prim) = prim_matching_part_exactly(part) {
                    prims.insert(prim);
                    continue;
                }
                let matches = all()
                    .filter(|p| {
                        p.name()
                            .is_some_and(|name| name.to_lowercase().starts_with(part))
                    })
                    .chain(all().filter(|p| {
                        p.ascii()
                            .is_some_and(|simple| part.contains(&simple.to_string()))
                    }))
                    .chain(
                        all().filter(|p| p.unicode().is_some_and(|unicode| part.contains(unicode))),
                    );
                prims.extend(matches);
            }
        }
        let mut classes: HashSet<PrimClass> = PrimClass::all().collect();
        'parts: for part in &parts {
            for (pattern, pat_classes) in [
                ("stack", [PrimClass::Stack].as_slice()),
                (
                    "pervasive",
                    &[PrimClass::MonadicPervasive, PrimClass::DyadicPervasive],
                ),
                ("array", &[PrimClass::MonadicArray, PrimClass::DyadicArray]),
                (
                    "monadic",
                    &[
                        PrimClass::MonadicPervasive,
                        PrimClass::MonadicArray,
                        PrimClass::MonadicModifier,
                    ],
                ),
                (
                    "dyadic",
                    &[
                        PrimClass::DyadicPervasive,
                        PrimClass::DyadicArray,
                        PrimClass::DyadicModifier,
                    ],
                ),
                (
                    "modifier",
                    &[
                        PrimClass::MonadicModifier,
                        PrimClass::DyadicModifier,
                        PrimClass::OtherModifier,
                    ],
                ),
                ("control", &[PrimClass::Control]),
                ("misc", &[PrimClass::Misc]),
                ("constant", &[PrimClass::Constant]),
                ("system", &[PrimClass::Sys]),
                (
                    "function",
                    &[
                        PrimClass::Stack,
                        PrimClass::Control,
                        PrimClass::MonadicPervasive,
                        PrimClass::DyadicPervasive,
                        PrimClass::MonadicArray,
                        PrimClass::DyadicArray,
                        PrimClass::Misc,
                        PrimClass::Sys,
                    ],
                ),
            ] {
                if pattern.split_whitespace().any(|pat| pat.starts_with(part)) {
                    classes.retain(|class| pat_classes.contains(class));
                    continue 'parts;
                }
            }
            classes.clear();
            break;
        }
        prims.extend(classes.iter().flat_map(|p| p.primitives()));
        classes.extend(prims.iter().map(|p| p.class()));
        if classes.is_empty() && !parts.is_empty() {
            return Self::default();
        }
        if prims.is_empty() {
            prims = Primitive::all().collect();
        }
        if classes.is_empty() {
            classes = PrimClass::all().collect();
        }
        Self { classes, prims }
    }
    fn table(&self, cx: Scope) -> impl IntoView {
        let mut table_cells = Vec::new();
        for class in PrimClass::all() {
            if !self.classes.contains(&class) {
                continue;
            }
            let id = match class {
                PrimClass::Stack => "stack-functions",
                PrimClass::MonadicPervasive => "monadic-pervasive-functions",
                PrimClass::DyadicPervasive => "dyadic-pervasive-functions",
                PrimClass::MonadicArray => "monadic-array-functions",
                PrimClass::DyadicArray => "dyadic-array-functions",
                PrimClass::MonadicModifier => "monadic-modifiers",
                PrimClass::DyadicModifier => "dyadic-modifiers",
                PrimClass::OtherModifier => "other-modifiers",
                PrimClass::Control => "control-functions",
                PrimClass::Misc => "misc-functions",
                PrimClass::Constant => "constant-functions",
                PrimClass::Sys => "system-functions",
            };
            let of_class: Vec<_> = Primitive::all()
                .filter(|p| self.prims.contains(p) && p.class() == class && p.name().is_some())
                .map(|p| {
                    view! { cx, <PrimCode prim=p/> }
                })
                .collect();
            if of_class.is_empty() {
                continue;
            }
            let (header, description) = match class {
                PrimClass::Stack => ("Stack", "Modify the stack"),
                PrimClass::MonadicPervasive => {
                    ("Monadic Pervasive", "Operate on every item in an array")
                }
                PrimClass::DyadicPervasive => (
                    "Dyadic Pervasive",
                    "Operate on every pair of items in two arrays",
                ),
                PrimClass::MonadicArray => ("Monadic Array", "Operate on a single array"),
                PrimClass::DyadicArray => ("Dyadic Array", "Operate on two arrays"),
                PrimClass::MonadicModifier => (
                    "Monadic Modifiers",
                    "Apply a function in some way to an array",
                ),
                PrimClass::DyadicModifier => (
                    "Dyadic Modifiers",
                    "Apply a function in some way to two arrays",
                ),
                PrimClass::OtherModifier => ("Other Modifiers", ""),
                PrimClass::Control => ("Control", "Control the flow of execution"),
                PrimClass::Misc => ("Miscellaneous", ""),
                PrimClass::Constant => ("Constants", "Push a constant value onto the stack"),
                PrimClass::Sys => ("System", "Interact with the system"),
            };
            table_cells.push(view! { cx,
                <td id=id style="vertical-align: top;"><div>
                    <h3>{ header }</h3>
                    <p>{ description }</p>
                    <div class="primitive-list">{ of_class }</div>
                </div></td>
            });
        }

        let mut rows: Vec<_> = Vec::new();
        let mut class_iter = table_cells.into_iter();
        while let Some(first) = class_iter.next() {
            rows.push(
                view!(cx, <tr>{once(first).chain(class_iter.next()).collect::<Vec<_>>()}</tr>),
            );
        }
        view!(cx, <table>{ rows }</table>)
    }
}
