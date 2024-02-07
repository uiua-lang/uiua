use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use uiua::{PrimClass, PrimDocFragment, PrimDocLine, Primitive};

use crate::{editor::Editor, Prim};

fn doc_line_fragments_to_view(fragments: &[PrimDocFragment]) -> View {
    if fragments.is_empty() {
        return view!( <br/>).into_view();
    }
    fragments
        .iter()
        .map(|frag| match frag {
            PrimDocFragment::Text(s) => s.into_view(),
            PrimDocFragment::Code(s) => view!(<code>{s}</code>).into_view(),
            PrimDocFragment::Emphasis(s) => view!(<em>{s}</em>).into_view(),
            PrimDocFragment::Strong(s) => view!(<strong>{s}</strong>).into_view(),
            PrimDocFragment::Link { text, url } => {
                let url = url.clone();
                let text = text.clone();
                view!(<a href=url>{text}</a>).into_view()
            }
            &PrimDocFragment::Primitive { prim, named } => {
                view!(<Prim prim=prim glyph_only={!named}/>).into_view()
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
                view!(<p style="white-space: pre-wrap">{doc_line_fragments_to_view( frags)}</p>)
                    .into_view()
            }
            PrimDocLine::Example(ex) => view!(<Editor example={ ex.input() }/>).into_view(),
        })
        .collect::<Vec<_>>()
}

#[component]
pub fn PrimDocs(prim: Primitive) -> impl IntoView {
    let mut sig = String::new();
    if prim.class() == PrimClass::Constant {
        sig.push_str("Constant");
    } else if let Some(margs) = prim.modifier_args() {
        match margs {
            1 => sig.push_str("Monadic"),
            2 => sig.push_str("Dyadic"),
            3 => sig.push_str("Triadic"),
            n => sig.push_str(&format!("{n}-function")),
        }
        if let Some(args) = prim.args() {
            sig.push(' ');
            sig.push_str(&args.to_string());
            sig.push_str("-argument");
        }
        sig.push_str(" modifier");
    } else {
        match prim.args() {
            Some(0) => sig.push_str("Noadic"),
            Some(1) => sig.push_str("Monadic"),
            Some(2) => sig.push_str("Dyadic"),
            Some(3) => sig.push_str("Triadic"),
            Some(n) => sig.push_str(&format!("{n}-argument")),
            None => sig.push_str("Variadic"),
        }
        if let Some(outputs) = prim.outputs() {
            if outputs != 1 {
                sig.push_str(&format!(" {outputs}-output"));
            }
        } else {
            sig.push_str(" variable-output");
        }
        if prim.class().is_pervasive() {
            sig.push_str(" pervasive");
        }
        sig.push_str(" function");
    }
    let long_name = if let Primitive::Sys(op) = prim {
        Some(format!(" - {}", op.long_name()))
    } else {
        None
    };
    let doc = prim.doc();
    let body = view! {
        <p style="white-space: pre-wrap">{doc_line_fragments_to_view(&doc.short)}</p>
        { doc_lines_to_view(&doc.lines) }
    };

    let id = prim.name();

    let experimental = prim.is_experimental().then(|| {
        view! {
            <p><span class="experimental">"⚠️ Warning 🧪: This "{if prim.is_modifier() { "modifier" }else{ "function" }}" is experimental and may be changed or removed in the future."</span><span class="output-fainter">" Experimental features can be enabled by putting an "<code>"# Experimental!"</code>" comment at the top of a program."</span></p>
        }
    });

    view! {
        <div>
            <h1 id=id><Prim prim=prim hide_docs=true/>{ long_name }</h1>
            <p><h3>{ sig }</h3></p>
            <p>{ experimental }</p>
            { body }
        </div>
    }
}

#[component]
pub fn AllFunctions() -> impl IntoView {
    let (list, set_list) =
        create_signal(view!(<h3 class="running-text">"Generating list..."</h3>).into_view());
    set_timeout(
        move || {
            set_list.set(
                Primitive::non_deprecated()
                    .map(|p| {
                        view! {
                            <br/>
                            <hr/>
                            <PrimDocs prim=p/>
                        }
                    })
                    .collect::<Vec<_>>()
                    .into_view(),
            )
        },
        Default::default(),
    );
    view! {
        <Title text="All Functions - Uiua Docs"/>
        <h1>"All Functions"</h1>
        <p>"This is a list of every built-in function in Uiua, provided for your scrolling pleasure."</p>
        <p>"For a searchable list, see the "<A href="/docs#functions">"main docs page"</A>"."</p>
        { move || list.get() }
    }
}
