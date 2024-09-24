use std::time::Duration;

use leptos::*;
use leptos_meta::*;
use leptos_router::*;

use crate::markdown::*;

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct BlogParams {
    page: BlogParam,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BlogParam(String);
impl IntoParam for BlogParam {
    fn into_param(value: Option<&str>, _: &str) -> Result<Self, ParamsError> {
        let s = value.unwrap_or_default();
        let name = urlencoding::decode(s)
            .map(Into::into)
            .unwrap_or_else(|_| s.into());
        Ok(BlogParam(name))
    }
}

#[component]
pub fn Blog() -> impl IntoView {
    view!({
        move || match use_params::<BlogParams>().get() {
            Ok(params) => {
                if params.page.0.is_empty() {
                    view!(<BlogHome/>)
                } else {
                    view!(<BlogPage name={params.page.0}/>)
                }
            }
            Err(_) => view!(<BlogHome/>),
        }
    })
}

#[component]
fn BlogHome() -> impl IntoView {
    view! {
        <Title text="Uiua Blog"/>
        <h1>"Uiua Blog"</h1>
        <BlogRssButton/>
        {
            let list = include_str!("../blog/list.txt");
            list.lines().filter(|line| !line.is_empty() && !line.starts_with('#')).map(|line| {
                let (path, name) = line.split_once(": ").unwrap_or_default();
                let (path, _guid) = path.split_once('(').unwrap_or_default();
                let (date, name) = name.split_once(" - ").unwrap_or_default();
                let name = name.to_string();
                let date = date.to_string();
                view!(<h3><span class="output-faint">{date}" - "</span><A href={format!("/blog/{path}")}>{name}</A></h3>)
            }).collect::<Vec<_>>().into_view()
        }
    }
}

#[component]
fn BlogPage(name: String) -> impl IntoView {
    view! {
        <Title text={format!("{name} - Uiua Blog")}/>
        <A href="/blog">"Back to Blog Home"</A>
        <BlogRssButton/>
        <br/>
        <p>
            "This post is available in lightweight "
            <a href={format!("https://uiua.org/blog/{name}-html.html")}>"HTML"</a>
            " and "
            <a href={format!("https://github.com/uiua-lang/uiua/blob/main/site/blog/{name}-text.md")}>"markdown"</a>
            " formats."
        </p>
        <Markdown src={format!("/blog/{name}-text.md")}/>
        <br/>
        <br/>
        <A href="/blog">"Back to Blog Home"</A>
    }
}

#[component]
fn BlogRssButton() -> impl IntoView {
    let (copied, set_copied) = create_signal(false);
    view! {
        <button
            href="/blog/feed.rss"
            class="editor-right-button"
            style="float: right"
            data-title={move || if copied.get() { "Copied!" } else { "Copy RSS Feed" }}
            on:click=move |_| {
                set_copied.set(true);
                let host = window().location().host().unwrap();
                _ = window().navigator().clipboard().write_text(&format!("{host}/blog/feed.rss"));
                set_timeout(
                    move || set_copied.set(false),
                    Duration::from_secs(10),
                );
            }>
            <img
                src="/assets/rss.svg"
                alt="RSS Feed"
                width="32"
                height="32"
                style="border-radius: 20%"/>
        </button>
    }
}

#[cfg(test)]
#[test]
fn gen_blog_html() {
    use std::{borrow::Cow, fs};

    let list = include_str!("../blog/list.txt");
    for line in list
        .lines()
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
    {
        let (path, _) = line.split_once(": ").unwrap_or_default();
        let md_path = format!("blog/{}-text.md", path);
        let mut markdown =
            fs::read_to_string(&md_path).unwrap_or_else(|e| panic!("{md_path}: {e}"));
        let mut lines: Vec<Cow<str>> = markdown.lines().map(Cow::Borrowed).collect();
        lines.insert(
            0,
            Cow::Borrowed("[Uiua](https://uiua.org)\n\n[Blog Home](https://uiua.org/blog)"),
        );
        lines.insert(
            3,
            Cow::Owned(format!(
                "\n\n**You can read this post with full editor \
                features [here](https://uiua.org/blog/{path}).**\n\n"
            )),
        );
        markdown = lines.join("\n");
        let html = markdown_html(&markdown);
        let html_path = format!("blog/{}-html.html", path);
        fs::write(html_path, html).unwrap();
    }
}
