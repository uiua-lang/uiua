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
        <Fetch src="/blog/list.txt" f=|list| {
            list.lines().map(|name| {
                let (path, name) = name.split_once(": ").unwrap_or_default();
                let (date, name) = name.split_once(" - ").unwrap_or_default();
                let name = name.to_string();
                let date = date.to_string();
                view!(<h3><span class="output-faint">{date}" - "</span><A href={format!("/blog/{path}")}>{name}</A></h3>)
            }).collect::<Vec<_>>().into_view()
        }/>
    }
}

#[component]
fn BlogPage(name: String) -> impl IntoView {
    view! {
        <Title text={format!("{name} - Uiua Blog")}/>
        <A href="/blog">"Back to Blog Home"</A>
        <br/>
        <br/>
        <p>
            "Click "
            <a href={format!("https://github.com/uiua-lang/uiua/blob/main/site/blog/{name}-text.md")}>here</a>
            " for a lightweight markdown version of this page."
        </p>
        <br/>
        <Markdown src={format!("/blog/{name}-text.md")}/>
        <br/>
        <br/>
        <A href="/blog">"Back to Blog Home"</A>
    }
}
