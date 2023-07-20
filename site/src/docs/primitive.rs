use leptos::*;
use leptos_router::*;
use uiua::primitive::Primitive;

use crate::{code::PrimCode, docs::DocsHome, editor::Editor};

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct PrimsDocsParams {
    prim_name: Option<String>,
}

#[component]
pub fn PrimDocsPage(cx: Scope) -> impl IntoView {
    let (prim, doc) = match use_params::<PrimsDocsParams>(cx).get() {
        Ok(PrimsDocsParams {
            prim_name: Some(prim_name),
        }) => {
            if let Some(pair) = Primitive::ALL.into_iter().find_map(|prim| {
                prim.doc()
                    .map(|doc| (prim, doc))
                    .filter(|_| format!("{prim:?}") == prim_name)
            }) {
                pair
            } else {
                return view! { cx, <DocsHome/> }.into_view(cx);
            }
        }
        _ => return view! { cx, <DocsHome/> }.into_view(cx),
    };

    let ex_lines: Vec<_> = doc
        .examples
        .iter()
        .map(|ex| {
            view!(cx,
                <div>
                    <p>{&ex.primer}</p>
                    <Editor examples=&[&ex.input]/>
                </div>
            )
            .into_view(cx)
        })
        .collect();

    view! { cx,
        <div>
            <A href="/docs">"Back to Docs Home"</A>
            <h1><PrimCode prim=prim name=true/></h1>
            <p>{&doc.intro}</p>
            { ex_lines }
            <p>{&doc.outro}</p>
            <div id="bottom-page-nav">
                <A href="/docs">"Back to Docs Home"</A>
            </div>
        </div>
    }
    .into_view(cx)
}
