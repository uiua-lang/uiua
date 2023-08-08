use leptos::*;
use uiua::primitive::{PrimDocFragment, PrimDocLine, Primitive};

use crate::{code::PrimCode, editor::Editor};

fn doc_line_fragments_to_view(fragments: &[PrimDocFragment]) -> View {
    if fragments.is_empty() {
        return view!( <br/>).into_view();
    }
    fragments
        .iter()
        .map(|frag| match frag {
            PrimDocFragment::Text(s) => s.into_view(),
            PrimDocFragment::Code(s) => view!( <code>{s}</code>).into_view(),
            PrimDocFragment::Emphasis(s) => view!( <em>{s}</em>).into_view(),
            &PrimDocFragment::Primitive { prim, named } => {
                view!( <PrimCode prim=prim glyph_only={!named}/>).into_view()
            }
        })
        .collect::<Vec<_>>()
        .into_view()
}

fn doc_lines_to_view(lines: &[PrimDocLine]) -> impl IntoView {
    lines
        .iter()
        .map(|line| match line {
            PrimDocLine::Text(frags) => {
                view!( <p style="white-space: pre-wrap">{doc_line_fragments_to_view( frags)}</p>)
                    .into_view()
            }
            PrimDocLine::Example(ex) => view!( <Editor example={ ex.input() }/>).into_view(),
        })
        .collect::<Vec<_>>()
}

#[component]
pub fn PrimDocs(prim: Primitive) -> impl IntoView {
    let body = move || {
        prim.doc().map(|doc| {
            view! {
                <p style="white-space: pre-wrap">{doc_line_fragments_to_view( &doc.short)}</p>
                { doc_lines_to_view( &doc.lines) }
            }
        })
    };

    view! {
        <div>
            <h1><PrimCode prim=prim hide_docs=true/></h1>
            { body }
        </div>
    }
}
