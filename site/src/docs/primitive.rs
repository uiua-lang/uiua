use leptos::*;
use uiua::primitive::{PrimDocFragment, PrimDocLine, Primitive};

use crate::{code::PrimCode, editor::Editor};

fn doc_line_fragments_to_view(cx: Scope, fragments: &[PrimDocFragment]) -> View {
    if fragments.is_empty() {
        return view!(cx, <br/>).into_view(cx);
    }
    fragments
        .iter()
        .map(|frag| match frag {
            PrimDocFragment::Text(s) => s.into_view(cx),
            PrimDocFragment::Code(s) => view!(cx, <code>{s}</code>).into_view(cx),
            PrimDocFragment::Emphasis(s) => view!(cx, <em>{s}</em>).into_view(cx),
            &PrimDocFragment::Primitive { prim, named } => {
                view!(cx, <PrimCode prim=prim glyph_only={!named}/>).into_view(cx)
            }
        })
        .collect::<Vec<_>>()
        .into_view(cx)
}

fn doc_lines_to_view(cx: Scope, lines: &[PrimDocLine]) -> impl IntoView {
    lines
        .iter()
        .map(|line| match line {
            PrimDocLine::Text(frags) => {
                view!(cx, <p style="white-space: pre-wrap">{doc_line_fragments_to_view(cx, frags)}</p>).into_view(cx)
            }
            PrimDocLine::Example(ex) => view!(cx, <Editor example={ ex.input() }/>).into_view(cx),
        })
        .collect::<Vec<_>>()
}

#[component]
pub fn PrimDocs(cx: Scope, prim: Primitive) -> impl IntoView {
    let body = move || {
        prim.doc().map(|doc| {
            view! { cx,
                <p style="white-space: pre-wrap">{doc_line_fragments_to_view(cx, &doc.short)}</p>
                { doc_lines_to_view(cx, &doc.lines) }
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
