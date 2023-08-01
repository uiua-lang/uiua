use leptos::*;
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
pub fn PrimDocsPage(cx: Scope, prim: Primitive) -> impl IntoView {
    let ex_lines = move || {
        prim.doc()
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

    let body = move || {
        prim.doc().map(|doc| {
            view! { cx,
                <p style="white-space: pre-wrap">{parse_doc(cx, &doc.short)}</p>
                { ex_lines }
                <p style="white-space: pre-wrap">{parse_doc(cx, &doc.outro)}</p>
            }
        })
    };

    view! { cx,
        <div>
            <h1><PrimCode prim=prim hide_docs=true/></h1>
            { body }
        </div>
    }
}
