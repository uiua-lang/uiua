use std::borrow::Cow;

use comrak::{
    nodes::{AstNode, ListType, NodeValue},
    *,
};
use leptos::*;
use uiua::{Inputs, Primitive, Token};
use uiua_editor::{backend::fetch, lang, replace_lang_name, Editor};

use crate::{examples::LOGO, Hd, NotFound, Prim, ScrollToHash};

#[component]
#[allow(unused_braces)]
pub fn Markdown<S: Into<String>>(src: S) -> impl IntoView {
    view!(<Fetch src={src.into()} f=markdown_view/>)
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
            Some(text) if text.starts_with("<!DOCTYPE html>") => view!(<NotFound/>).into_view(),
            Some(text) => view!(<ScrollToHash/>{f(&text)}).into_view(),
            None => view! {<h3 class="running-text">"Loading..."</h3>}.into_view(),
        }
    }}
}

fn options() -> ComrakOptions<'static> {
    ComrakOptions {
        extension: ExtensionOptions::builder().table(true).build(),
        ..Default::default()
    }
}

pub fn markdown_view(text: &str) -> View {
    let arena = Arena::new();
    let text = text
        .replace("`` ` ``", "<code backtick>")
        .replace("```", "<code block delim>")
        .replace("``", "` `")
        .replace("<code block delim>", "```")
        .replace("<code backtick>", "`` ` ``");
    let root = parse_document(&arena, &text, &options());
    logging::log!("{:#?}", root);
    node_view(root)
}

#[cfg(test)]
pub fn markdown_html(text: &str) -> String {
    let arena = Arena::new();
    let text = text
        .replace("```", "<code block delim>")
        .replace("``", "` `")
        .replace("<code block delim>", "```");
    let root = parse_document(&arena, &text, &options());
    let body = format!(r#"<body><div id=top>{}</div></body>"#, node_html(root));
    let head = r#"
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="https://uiua.org/styles.css">
    "#;
    format!("<!DOCTYPE html><html><head>{}</head>{}</html>", head, body)
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
            let mut text = Cow::Borrowed(text.as_str());
            if replace_lang_name() && text.contains("Uiua") {
                text = Cow::Owned(text.replace("Uiua", lang()))
            }
            if text.contains("UIUA_VERSION") {
                text = Cow::Owned(text.replace("UIUA_VERSION", uiua::VERSION));
            }
            text.into_owned().into_view()
        }
        NodeValue::Heading(heading) => {
            let id = all_text(node).to_lowercase().replace(' ', "-");
            match heading.level {
                0 | 1 => view!(<h1 id=id>{children}</h1>).into_view(),
                2 => {
                    if id.is_empty() {
                        view!(<h2 id=id>{children}</h2>).into_view()
                    } else {
                        view!(<Hd id={&id}>{children}</Hd>).into_view()
                    }
                }
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
            let mut lit = code.literal.clone();
            if lit.contains("<backtick>") {
                lit = lit.replace("<backtick>", "`");
            }
            if lit.contains("UIUA_VERSION") {
                lit = lit.replace("UIUA_VERSION", uiua::VERSION);
            }
            let mut inputs = Inputs::default();
            let (tokens, errors, _) = uiua::lex(&lit, (), &mut inputs);
            if errors.is_empty() && lit != "---" {
                let mut frags = Vec::new();
                for token in tokens {
                    let text = token.span.as_str(&inputs, |s| s.to_string());
                    match token.value {
                        Token::Glyph(prim)
                            if prim.name() == text
                                || prim.glyph().is_some_and(|c| c.to_string() == text) =>
                        {
                            frags.push(view!(<Prim prim=prim glyph_only=true/>).into_view())
                        }
                        _ => {
                            frags = vec![view!(<code>{lit.clone()}</code>).into_view()];
                            break;
                        }
                    }
                }
                view!(<span>{frags}</span>).into_view()
            } else {
                view!(<code>{code.literal.clone()}</code>).into_view()
            }
        }
        NodeValue::Link(link) => {
            let text = leaf_text(node).unwrap_or_default();
            let name = text
                .rsplit_once(' ')
                .map(|(a, b)| {
                    if a.chars().count() > b.chars().count() {
                        a
                    } else {
                        b
                    }
                })
                .unwrap_or(&text);
            if let Some(prim) = Primitive::from_name(name).or_else(|| Primitive::from_name(&text)) {
                view!(<Prim prim=prim/>).into_view()
            } else {
                if name.chars().count() == 1 {
                    if let Some(prim) = Primitive::from_glyph(name.chars().next().unwrap()) {
                        return view!(<Prim prim=prim glyph_only=true/>).into_view();
                    }
                }
                view!(<a href={&link.url} title={&link.title}>{children}</a>).into_view()
            }
        }
        NodeValue::Emph => view!(<em>{children}</em>).into_view(),
        NodeValue::Strong => view!(<strong>{children}</strong>).into_view(),
        NodeValue::Strikethrough => view!(<del>{children}</del>).into_view(),
        NodeValue::LineBreak => view!(<br/>).into_view(),
        NodeValue::CodeBlock(block) => {
            if block.literal.trim() == "LOGO" {
                view!(<Editor example=LOGO/>).into_view()
            } else if (block.info.is_empty() || block.info.starts_with("uiua"))
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
        NodeValue::Image(image) => {
            let mut class = "";
            let mut alt = leaf_text(node).unwrap_or_default();
            if let Some(a) = alt.strip_suffix("(invert)") {
                alt = a.trim_end().into();
                class = "image-visibility";
            }
            view!(<img src={&image.url} alt={alt.clone()} title=alt class=class/>).into_view()
        }
        NodeValue::Table(_) => view!(<table class="bordered-table">{children}</table>).into_view(),
        &NodeValue::TableRow(is_header) => {
            let items = children.into_iter();
            let cells: Vec<_> = if is_header {
                items.map(|c| view!(<th>{c}</th>).into_view()).collect()
            } else {
                items.map(|c| view!(<td>{c}</td>).into_view()).collect()
            };
            view!(<tr>{cells}</tr>).into_view()
        }
        _ => children.into_view(),
    }
}

#[cfg(test)]
fn node_html<'a>(node: &'a AstNode<'a>) -> String {
    use uiua::{Compiler, SafeSys, Uiua, UiuaErrorKind, Value};
    use uiua_editor::prim_class;

    use crate::prim_html;

    let mut children_iter = node.children().map(node_html);
    let mut children = || children_iter.by_ref().collect::<String>();
    match &node.data.borrow().value {
        NodeValue::Text(text) => {
            if let Some(text) = text
                .strip_prefix('[')
                .and_then(|text| text.strip_suffix(']'))
            {
                if let Some(prim) = Primitive::from_name(text) {
                    return format!("{:?}", prim);
                }
            }
            text.clone()
        }
        NodeValue::Heading(heading) => {
            let id = all_text(node).to_lowercase().replace(' ', "-");
            format!(
                "<h{} id={:?}>{}</h{}>",
                heading.level,
                id,
                children(),
                heading.level
            )
        }
        NodeValue::List(list) => match list.list_type {
            ListType::Bullet => format!("<ul>{}</ul>", children()),
            ListType::Ordered => format!("<ol>{}</ol>", children()),
        },
        NodeValue::Item(_) => format!("<li>{}</li>", children()),
        NodeValue::Paragraph => format!("<p>{}</p>", children()),
        NodeValue::Code(code) => {
            let mut inputs = Inputs::default();
            let (tokens, errors, _) = uiua::lex(&code.literal, (), &mut inputs);
            if errors.is_empty() && code.literal != "---" {
                let mut s = "<code>".to_string();
                for token in tokens {
                    let text = token.span.as_str(&inputs, |s| s.to_string());
                    match token.value {
                        Token::Glyph(prim)
                            if prim.name() == text
                                || prim.glyph().is_some_and(|c| c.to_string() == text) =>
                        {
                            s.push_str(&prim_html(prim, true, false))
                        }
                        _ => return format!("<code>{}</code>", code.literal),
                    }
                }
                s.push_str("</code>");
                s
            } else {
                format!("<code>{}</code>", code.literal)
            }
        }
        NodeValue::Link(link) => {
            let text = leaf_text(node).unwrap_or_default();
            let name = text
                .rsplit_once(' ')
                .map(|(a, b)| {
                    if a.chars().count() > b.chars().count() {
                        a
                    } else {
                        b
                    }
                })
                .unwrap_or(&text);
            if let Some(prim) = Primitive::from_name(name).or_else(|| Primitive::from_name(&text)) {
                let symbol_class = format!("prim-glyph {}", prim_class(prim));
                let symbol = prim.to_string();
                let name = if symbol != prim.name() {
                    format!(" {}", prim.name())
                } else {
                    "".to_string()
                };
                format!(
                    r#"<a 
                        href="https://uiua.org/docs/{}" 
                        data-title={:?}
                        class="prim_code_a"
                        style="text-decoration: none;">
                        <code><span class={symbol_class:?}>{symbol}</span>{name}</code>
                    </a>"#,
                    prim.name(),
                    prim.doc().short_text()
                )
            } else {
                format!(
                    "<a href={:?} data-title={}>{}</a>",
                    link.url, link.title, text
                )
            }
        }
        NodeValue::Emph => format!("<em>{}</em>", children()),
        NodeValue::Strong => format!("<strong>{}</strong>", children()),
        NodeValue::Strikethrough => format!("<del>{}</del>", children()),
        NodeValue::LineBreak => "<br/>".into(),
        NodeValue::CodeBlock(block) => {
            let mut lines: Vec<String> = if block.literal.trim() == "LOGO" {
                LOGO
            } else {
                block.literal.as_str()
            }
            .lines()
            .map(Into::into)
            .collect();
            let max_len = lines
                .iter()
                .map(|s| {
                    s.chars()
                        .position(|c| c == '#')
                        .map(|i| i + 1)
                        .unwrap_or_else(|| s.chars().count() + 2)
                })
                .max()
                .unwrap_or(0);
            let mut comp = Compiler::with_backend(SafeSys::new());
            let mut env = Uiua::default();
            for line in &mut lines {
                let line_len = line.chars().count();
                if line_len < max_len {
                    line.push_str(&" ".repeat(max_len - line_len));
                }
                match comp.load_str(line).and_then(|comp| env.run_compiler(comp)) {
                    Ok(_) => {
                        let values = env.take_stack();
                        if !values.is_empty() && !values.iter().any(|v| v.shape.elements() > 200) {
                            let formatted: Vec<String> = values.iter().map(Value::show).collect();
                            if formatted.iter().any(|s| s.contains('\n')) {
                                line.push('\n');
                                for formatted in formatted {
                                    for fline in formatted.lines() {
                                        line.push_str(&format!("\n# {fline}"));
                                    }
                                }
                            } else {
                                line.push('#');
                                for formatted in formatted.into_iter().rev() {
                                    line.push(' ');
                                    line.push_str(&formatted);
                                }
                            }
                        }
                    }
                    Err(e)
                        if matches!(e.kind, UiuaErrorKind::Parse(..))
                            || e.to_string().contains("git modules")
                            || e.to_string().contains("was empty") =>
                    {
                        break;
                    }
                    Err(e) => line.push_str(&format!("# {e}")),
                }
            }
            let text = lines.join("\n");
            format!("<code class=\"code-block\">{text}</code>")
        }
        NodeValue::ThematicBreak => "<hr/>".into(),
        NodeValue::Image(image) => {
            let mut class = "";
            let mut alt = leaf_text(node).unwrap_or_default();
            if let Some(a) = alt.strip_suffix("(invert)") {
                alt = a.trim_end().into();
                class = "image-visibility";
            }
            format!(
                r#"<img src="{}" alt="{alt}" title="{alt}" class="{class}"/>"#,
                image.url
            )
        }
        NodeValue::Table(_) => format!(r#"<table class="bordered-table">{}</table>"#, children()),
        &NodeValue::TableRow(is_header) => {
            let tag = if is_header { "th" } else { "td" };
            children_iter
                .map(|s| format!("<{tag}>{s}</{tag}>"))
                .collect()
        }
        _ => children(),
    }
}

fn leaf_text<'a>(node: &'a AstNode<'a>) -> Option<String> {
    match &node.data.borrow().value {
        NodeValue::Text(text) => Some(text.into()),
        NodeValue::Code(code) => Some(code.literal.clone()),
        _ => node.first_child().and_then(leaf_text),
    }
}

fn all_text<'a>(node: &'a AstNode<'a>) -> String {
    let mut text = String::new();
    for child in node.children() {
        match &child.data.borrow().value {
            NodeValue::Text(s) => text.push_str(s),
            NodeValue::Code(code) => text.push_str(&code.literal),
            _ => text.push_str(&all_text(child)),
        }
    }
    text
}

#[cfg(test)]
#[test]
fn text_code_blocks() {
    use uiua_editor::backend::WebBackend;

    for entry in std::fs::read_dir("text").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        eprintln!("Testing code blocks in {:?}", path.display());
        let text = std::fs::read_to_string(path).unwrap();
        let arena = Arena::new();
        let text = text
            .replace("```", "<code block delim>")
            .replace("``", "` `")
            .replace("<code block delim>", "```");
        let root = parse_document(&arena, &text, &ComrakOptions::default());

        fn text_code_blocks<'a>(node: &'a AstNode<'a>) -> Vec<(String, bool)> {
            let mut blocks = Vec::new();
            for child in node.children() {
                match &child.data.borrow().value {
                    NodeValue::CodeBlock(block) if block.info.contains("uiua") => {
                        let should_fail = block.info.contains("should fail");
                        let literal = if block.literal.trim() == "LOGO" {
                            LOGO
                        } else {
                            block.literal.as_str()
                        };
                        blocks.push((literal.into(), should_fail))
                    }
                    _ => blocks.extend(text_code_blocks(child)),
                }
            }
            blocks
        }

        for (block, should_fail) in text_code_blocks(root) {
            eprintln!("Code block:\n{}", block);
            let mut comp = uiua::Compiler::with_backend(WebBackend::default());
            let mut env = uiua::Uiua::with_backend(WebBackend::default());
            let res = comp
                .load_str(&block)
                .and_then(|comp| env.run_compiler(comp));
            let failure_report = match res {
                Ok(_) => comp
                    .take_diagnostics()
                    .into_iter()
                    .next()
                    .map(|diag| diag.report()),
                Err(e) => Some(e.report()),
            };
            if let Some(report) = failure_report {
                if !should_fail {
                    panic!("\nBlock failed:\n{block}\n{report}")
                }
            } else if should_fail {
                panic!("\nBlock should have failed:\n{block}")
            }
        }
    }
}
