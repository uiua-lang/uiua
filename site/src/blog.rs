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
        // <Fetch src="https://uiua.org/blog/list.txt" f=|list| {
        //     list.lines().filter(|line| !line.is_empty() && !line.starts_with('#')).map(|name| {
        //         let (path, name) = name.split_once(": ").unwrap_or_default();
        //         let (date, name) = name.split_once(" - ").unwrap_or_default();
        //         let name = name.to_string();
        //         let date = date.to_string();
        //         view!(<h3><span class="output-faint">{date}" - "</span><A href={format!("/blog/{path}")}>{name}</A></h3>)
        //     }).collect::<Vec<_>>().into_view()
        // }/>
        {
            let list = include_str!("../blog/list.txt");
            list.lines().filter(|line| !line.is_empty() && !line.starts_with('#')).map(|line| {
                let (path, name) = line.split_once(": ").unwrap_or_default();
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
        <br/>
        <br/>
        <p>"This post is available in lightweight "<a href={format!("/blog/{name}-text.md")}>"markdown"</a>" and "<a href={format!("/blog/{name}-html.html")}>"html"</a>" formats."</p>
        <br/>
        <Markdown src={format!("/blog/{name}-text.md")}/>
        <br/>
        <br/>
        <A href="/blog">"Back to Blog Home"</A>
    }
}

#[cfg(test)]
#[test]
fn gen_blog_html() {
    use std::fs;

    let list = include_str!("../blog/list.txt");
    for line in list
        .lines()
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
    {
        let (path, _) = line.split_once(": ").unwrap_or_default();
        let md_path = format!("blog/{}-text.md", path);
        let mut markdown =
            fs::read_to_string(&md_path).unwrap_or_else(|e| panic!("{md_path}: {e}"));
        let extra = format!("You can read this post with full editor features [here](https://uiua.org/blog/{path}).");
        let insertion_pos = markdown.bytes().position(|b| b == b'\n').unwrap_or(0);
        markdown.insert_str(insertion_pos, &format!("\n{}\n\n", extra));
        let html = markdown_html(&markdown);
        let html_path = format!("blog/{}-html.html", path);
        fs::write(html_path, html).unwrap();
    }
}
