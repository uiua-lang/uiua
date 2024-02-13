use comrak::{
    nodes::{AstNode, ListType, NodeValue},
    *,
};
use leptos::*;
use uiua::Primitive;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response};

use crate::{editor::Editor, NotFound, Prim};

#[component]
#[allow(unused_braces)]
pub fn Markdown<S: Into<String>>(src: S) -> impl IntoView {
    view!(<Fetch src={src.into()} f=markdown/>)
}

#[component]
pub fn Fetch<S: Into<String>, F: Fn(&str) -> View + 'static>(src: S, f: F) -> impl IntoView {
    let src = src.into();
    let (src, _) = create_signal(src);
    let once = create_resource(
        || (),
        move |_| async move { fetch(&src.get_untracked()).await.unwrap() },
    );
    view! {{
        move || match once.get() {
            Some(text) if text.is_empty() || text.starts_with("<!DOCTYPE html>") => view!(<NotFound/>).into_view(),
            Some(text) => f(&text),
            None => view! {<h3 class="running-text">"Loading..."</h3>}.into_view(),
        }
    }}
}

async fn fetch(src: &str) -> Result<String, JsValue> {
    let mut opts = RequestInit::new();
    opts.method("GET");
    opts.mode(RequestMode::Cors);
    let request = Request::new_with_str_and_init(src, &opts)?;
    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request)).await?;
    assert!(resp_value.is_instance_of::<Response>());
    let resp: Response = resp_value.dyn_into().unwrap();
    let text = JsFuture::from(resp.text()?).await?.as_string().unwrap();
    Ok(text)
}

pub fn markdown(text: &str) -> View {
    let arena = Arena::new();
    let text = text
        .replace("```", "<code block delim>")
        .replace("``", "` `")
        .replace("<code block delim>", "```");
    let root = parse_document(&arena, &text, &ComrakOptions::default());
    node_view(root)
}

fn node_view<'a>(node: &'a AstNode<'a>) -> View {
    let children: Vec<_> = node.children().map(node_view).collect();
    match &node.data.borrow().value {
        NodeValue::Text(text) => {
            if let Some(text) = text
                .strip_prefix('[')
                .and_then(|text| text.strip_suffix(']'))
            {
                if let Some(prim) = Primitive::from_name(text) {
                    return view!(<Prim prim=prim/>).into_view();
                }
            }
            text.into_view()
        }
        NodeValue::Heading(heading) => {
            let id = leaf_text(node).map(|s| s.to_lowercase().replace(' ', "-"));
            match heading.level {
                0 | 1 => view!(<h1 id=id>{children}</h1>).into_view(),
                2 => view!(<h2 id=id>{children}</h2>).into_view(),
                3 => view!(<h3 id=id>{children}</h3>).into_view(),
                4 => view!(<h4 id=id>{children}</h4>).into_view(),
                5 => view!(<h5 id=id>{children}</h5>).into_view(),
                _ => view!(<h6 id=id>{children}</h6>).into_view(),
            }
        }
        NodeValue::List(list) => match list.list_type {
            ListType::Bullet => view!(<ul>{children}</ul>).into_view(),
            ListType::Ordered => view!(<ol>{children}</ol>).into_view(),
        },
        NodeValue::Item(_) => view!(<li>{children}</li>).into_view(),
        NodeValue::Paragraph => view!(<p>{children}</p>).into_view(),
        NodeValue::Code(code) => {
            if let Some(prim) = Primitive::from_name(&code.literal) {
                view!(<Prim prim=prim glyph_only=true/>).into_view()
            } else {
                view!(<code>{&code.literal}</code>).into_view()
            }
        }
        NodeValue::Link(link) => {
            let text = leaf_text(node).unwrap_or_default();
            let (name, _) = text.rsplit_once(' ').unwrap_or_default();
            if let Some(prim) = Primitive::from_name(name) {
                view!(<Prim prim=prim/>).into_view()
            } else {
                view!(<a href={&link.url} title={&link.title}>{text}</a>).into_view()
            }
        }
        NodeValue::Emph => view!(<em>{children}</em>).into_view(),
        NodeValue::Strong => view!(<strong>{children}</strong>).into_view(),
        NodeValue::Strikethrough => view!(<del>{children}</del>).into_view(),
        NodeValue::LineBreak => view!(<br/>).into_view(),
        NodeValue::CodeBlock(block) => {
            if (block.info.is_empty() || block.info == "uiua")
                && uiua::parse(&block.literal, (), &mut Default::default())
                    .1
                    .is_empty()
            {
                view!(<Editor example={block.literal.trim_end()}/>).into_view()
            } else {
                view!(<code class="code-block">{&block.literal}</code>).into_view()
            }
        }
        NodeValue::ThematicBreak => view!(<hr/>).into_view(),
        _ => children.into_view(),
    }
}

fn leaf_text<'a>(node: &'a AstNode<'a>) -> Option<String> {
    match &node.data.borrow().value {
        NodeValue::Text(text) => Some(text.into()),
        NodeValue::Code(code) => Some(code.literal.clone()),
        _ => node.first_child().and_then(leaf_text),
    }
}
