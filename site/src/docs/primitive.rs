use leptos::*;
use leptos_router::*;
use regex::Regex;
use uiua::primitive::Primitive;

use crate::{code::PrimCode, editor::Editor};

#[allow(unused_braces)]
fn parse_doc(cx: Scope, line: &str) -> impl IntoView {
    thread_local! {
        static RE: Regex = Regex::new(r"\[(.*?)\]|`(.*?)`|\*(.*?)\*|([^\[\]`\*]+)").unwrap();
    }
    RE.with(|re| {
        re.captures_iter(line)
            .map(|c| {
                let (mat, [s]) = c.extract();
                let s = s.to_string();
                if mat.starts_with('[') {
                    if let Some(prim) = Primitive::from_name(&s) {
                        view!(cx, <PrimCode prim=prim/>).into_view(cx)
                    } else {
                        view!(cx, "["{s}"]").into_view(cx)
                    }
                } else if mat.starts_with('`') {
                    view!(cx, <code>{s}</code>).into_view(cx)
                } else if mat.starts_with('*') {
                    view!(cx, <em>{s}</em>).into_view(cx)
                } else {
                    view!(cx, { s }).into_view(cx)
                }
            })
            .collect::<Vec<_>>()
    })
}

#[component]
pub fn PrimDocsPage(cx: Scope) -> impl IntoView {
    let prim_name = move || use_params_map(cx).with(|p| p.get("prim_name").cloned());
    let prim = move || {
        prim_name()
            .and_then(|name| Primitive::all().find(|p| name == format!("{p:?}").to_lowercase()))
    };

    let ex_lines = move || {
        prim()
            .and_then(|prim| prim.doc())
            .map(|doc| {
                doc.examples
                    .iter()
                    .map(|ex| {
                        view!(cx,
                            {ex.primer.lines().map(|line| view!(cx, <p>{parse_doc(cx, line)}</p>)).collect::<Vec<_>>()}
                            <Editor examples=&[&ex.input]/>
                        )
                        .into_view(cx)
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default()
    };

    let header = move || {
        if let Some(prim) = prim() {
            view!(cx, <h1><PrimCode prim=prim hide_docs=true/></h1>)
        } else {
            view!(cx, <h1>"Unknown primitive: "{ prim_name }</h1>)
        }
    };

    let body = move || {
        prim().and_then(|prim| prim.doc()).map(|doc| {
            view! { cx,
                <p style="white-space: pre-wrap">{parse_doc(cx, &doc.short)}</p>
                { ex_lines }
                <p style="white-space: pre-wrap">{parse_doc(cx, &doc.outro)}</p>
            }
        })
    };

    view! { cx,
        <div>
            <A href="/docs">"Back to Docs Home"</A>
            { header }
            { body }
            <div id="bottom-page-nav">
                <A href="/docs">"Back to Docs Home"</A>
            </div>
        </div>
    }
    .into_view(cx)
}
