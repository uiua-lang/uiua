mod primitive;
mod tutorial;

use std::iter::once;

use enum_iterator::all;
use leptos::*;
use leptos_router::*;
use uiua::primitive::{PrimClass, Primitive};

use crate::code::*;
use primitive::*;
use tutorial::*;

#[component]
pub fn DocsHome(cx: Scope) -> impl IntoView {
    let primitives: Vec<_> = PrimClass::all()
        .map(|class| {
            let of_class: Vec<_> = Primitive::all()
                .filter(|p| p.class() == class && p.name().is_some())
                .map(|p| {
                    view! { cx, <PrimCode prim=p/> }
                })
                .collect();
            let (header, description) = match class {
                PrimClass::Stack => ("Stack", "Modify the stack"),
                PrimClass::MonadicPervasive => {
                    ("Monadic Pervasive", "Operate on every item in an array")
                }
                PrimClass::DyadicPervasive => (
                    "Dyadic Pervasive",
                    "Operate on every pair of items in two arrays",
                ),
                PrimClass::MonadicArray => ("Monadic Array", "Operate on a single array"),
                PrimClass::DyadicArray => ("Dyadic Array", "Operate on two arrays"),
                PrimClass::MonadicModifier => (
                    "Monadic Modifiers",
                    "Apply a function in some way to an array",
                ),
                PrimClass::DyadicModifier => (
                    "Dyadic Modifiers",
                    "Apply a function in some way to two arrays",
                ),
                PrimClass::OtherModifier => ("Other Modifiers", ""),
                PrimClass::Control => ("Control", "Control the flow of execution"),
                PrimClass::Misc => ("Miscellaneous", ""),
                PrimClass::Constant => ("Constants", "Push a constant value onto the stack"),
                PrimClass::Io => ("I/O", "Do input and output"),
            };
            view! { cx,
                <td style="vertical-align: top;"><div>
                    <h3>{ header }</h3>
                    <p>{ description }</p>
                    <div class="primitive-list">{ of_class }</div>
                </div></td>
            }
        })
        .collect();

    let mut rows: Vec<_> = Vec::new();
    let mut class_iter = primitives.into_iter();
    while let Some(first) = class_iter.next() {
        rows.push(view!(cx, <tr>{once(first).chain(class_iter.next()).collect::<Vec<_>>()}</tr>));
    }

    view! { cx,
        <h2>"Documentation"</h2>
        <h2>"Tutorial"</h2>
        <p>"These are meant to be read in order:"</p>
        <ul>{ all::<TutorialPage>()
            .map(|p| view!(cx, <li><A href={p.path()}>{p.title()}</A></li>))
            .collect::<Vec<_>>()
        }</ul>
        <br/>
        <h2>"Functions"</h2>
        <table>{ rows }</table>
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocsPage {
    Tutorial(TutorialPage),
    Primitive(Primitive),
}

impl IntoParam for DocsPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        let value = value.unwrap_or("");
        all::<TutorialPage>()
            .find(|p| p.path() == value)
            .map(Self::Tutorial)
            .or_else(|| {
                Primitive::all()
                    .find(|p| format!("{p:?}").to_lowercase() == value)
                    .map(Self::Primitive)
            })
            .ok_or_else(|| ParamsError::MissingParam(name.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct DocsParams {
    page: DocsPage,
}

#[component]
pub fn Docs(cx: Scope) -> impl IntoView {
    move || {
        let Ok(params) = use_params::<DocsParams>(cx).get() else {
            return view!(cx, <Redirect path="/docs"/>).into_view(cx);
        };
        let page = params.page;
        let page_view = match page {
            DocsPage::Tutorial(tut) => view!(cx, <Tutorial page=tut/>).into_view(cx),
            DocsPage::Primitive(prim) => view!(cx, <PrimDocs prim=prim/>).into_view(cx),
        };

        view! { cx,
            <div>
                <A href="/docs">"Back to Docs Home"</A>
                <br/>
                <br/>
                { page_view }
                <A href="/docs">"Back to Docs Home"</A>
            </div>
        }
        .into_view(cx)
    }
}
