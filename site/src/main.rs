mod backend;
mod docs;
mod editor;
mod examples;
mod pad;

use std::cell::RefCell;

use leptos::*;
use leptos_router::*;
use rand::prelude::*;
use uiua::primitive::Primitive;
use wasm_bindgen::JsCast;
use web_sys::HtmlAudioElement;

use crate::{docs::*, editor::*, pad::*};

thread_local! {
    static SUBTITLE: RefCell<Option<usize>>  = RefCell::new(None);
}

pub fn main() {
    console_error_panic_hook::set_once();
    mount_to_body(|cx| view!(cx, <Site/>));
}

#[component]
pub fn Site(cx: Scope) -> impl IntoView {
    document()
        .body()
        .unwrap()
        .remove_child(&element("top"))
        .unwrap();

    view! { cx,
        <Router>
            <main>
                <div id="top">
                    <div id="header">
                        <h1><img src="/uiua-logo.png" style="height: 1em"></img>" Uiua"</h1>
                        <div id="nav">
                            <p><a href="https://github.com/kaikalii/uiua">"GitHub"</a></p>
                            <p><a href="/">"Home"</a></p>
                        </div>
                    </div>
                    <Routes>
                        <Route path="" view=|cx| view!(cx, <MainPage/>)/>
                        <Route path="docs" view=|cx| view!(cx, <Outlet/>)>
                            <Route path=":page" view=|cx| view!(cx, <DocsPage/>)/>
                            <Route path="" view=|cx| view!(cx, <DocsHome/>)/>
                        </Route>
                        <Route path="primitive" view=|cx| view!(cx, <Outlet/>)>
                            <Route path=":prim_name" view=|cx| view!(cx, <PrimDocsPage/>)/>
                        </Route>
                        <Route path="pad" view=|cx| view!(cx, <Pad/>)/>
                    </Routes>
                </div>
                <br/>
                <br/>
                <br/>
            </main>
        </Router>
    }
}

#[component]
pub fn MainPage(cx: Scope) -> impl IntoView {
    // Choose a random subtitle
    let subtitles = [
        view!(cx, <p>"A stack-oriented array programming language"</p>),
        view!(cx, <p>"An array-oriented stack programming language"</p>),
        view!(cx, <p>"A programming language for point-free enjoyers"</p>),
        view!(cx, <p>"A programming language for variable dislikers"</p>),
        view!(cx, <p>"What if APL was a FORTH?"</p>),
        view!(cx, <p>"What if FORTH was an APL?"</p>),
        view!(cx, <p>"Isn't a stack a sort of array?"</p>),
        view!(cx, <p>"It's got um...I um...arrays"</p>),
        view!(cx, <p><a href="https://youtu.be/seVSlKazsNk">"Point-Free or Die"</a></p>),
        view!(cx, <p>"Notation as a tool of thot"</p>),
        view!(cx, <p>"Do you like this page Marshall?"</p>),
        view!(cx, <p>"Conor Dyadic Hookstra"</p>),
    ];
    let index = SUBTITLE.with(|s| {
        *s.borrow_mut().get_or_insert_with(|| {
            let mut rng = SmallRng::seed_from_u64(instant::now().to_bits());
            // Prefers lower indices
            let index = rng.gen_range(0.0..(subtitles.len() as f64).cbrt());
            index.powi(3) as usize
        })
    });
    let subtitle = subtitles[index].clone();

    view! {
        cx,
        <div>
            <div id="subtitle">{ subtitle }</div>
            <div id="links">
                <p><A href="docs">"Documentation"</A></p>
                <p><A href="pad">"Pad"</A></p>
            </div>
            <Editor
                examples=examples::EXAMPLES
                size=EditorSize::Medium
                mode=EditorMode::Multiple
                help={&[
                    "Type some or all of a glyph's name, then run to format the names into glyphs.",
                    "You can run with ctrl/shift + enter.",
                ]}/>
            <br/>
            <br/>
            <MainText/>
        </div>
    }
}

#[component]
fn MainText(cx: Scope) -> impl IntoView {
    let borat = |_| {
        if let Ok(audio) = HtmlAudioElement::new_with_src("/weewah.mp3") {
            _ = audio.play();
        }
    };

    view! { cx, <div>
        <p>"Uiua ("<i>"wee-wuh "</i><button on:click=borat class="sound-button">"🔉"</button>") is a stack-oriented array programming language with a focus on tacit code (code without named values). Its semantics and primitives (and this site) are largely inspired by "<a href="https://mlochbaum.github.io/BQN/">"BQN"</a>", but it combines the array paradigm with the stack-oriented paradigm to make writing point-free code more workable."</p>
        <hr/>
        <h3>"How is Uiua like other array languages?"</h3>
        <p>"Like APL, J, BQN, and many other array languages, Uiua's primary data type is the array. Arrays are multidimensional and rank-polymorphic, meaning that many operation automatically apply to every element."</p>
        <p>"Uiua features:"</p>
        <ul>
            <li>"A rich set of primitives"</li>
            <li>"Arrays following the wonderfully simple "<a href="https://aplwiki.com/wiki/Array_model#Flat_array_theory">"Flat Array Model"</a>", with no boxing"</li>
            <li>"Basic IO facilities"</li>
            <li>"A deterministic formatter"</li>
        </ul>
        <hr/>
        <h3>"How is Uiua different from other array languages?"</h3>
        <p>"Uiua does away with the infix notation of most array languages for a more general prefix notation. While functions still take arguments, they pull those arguments from the stack and push their results back on. This enables a lot of cool stuff, particularly in the realm of tacit code."</p>
        <p>"Uiua also features:"</p>
        <ul>
            <li>"Hook and fork constructs without parenthesis"</li>
            <li>"Stack-to-array syntax for building arrays with stack operations"</li>
            <li>"Primitives specifically for stack operations"</li>
            <li>"No function overloading. Every glyph has one name and does one thing."</li>
        </ul>
        <hr/>
        <h3>"How does writing the glyphs work?"</h3>
        <p>"Unlike most array languages, Uiua does not overload primitives depending on whether they are passed one or two arguments. Functions in Uiua can take any number of arguments, but an individual function always takes the "<i>"same"</i>" number of arguments."</p>
        <p>"This ends up meaning that Uiua requires way more glyphs to have one for every primitive. There simply are not enough keys on them keyboard to type them without using a bunch of hard-to-remeber shortcuts. Also, I think it's annoying to need special editor support to be able to write code properly."</p>
        <p>"To solve these issues, Uiua has a formatter that automatically converts ASCII names and characters into glyphs. You can type the name of a glyph (or a digraph, like "<code>">="</code>" for "<PrimCode prim=Primitive::Ge glyph_only=true/>"), and the formatter will turn it into the corresponding glyph. Alternatively, the editors embedded in this site have a button for each glyph."</p>
    </div>}
}

mod code {
    use super::*;
    #[component]
    pub fn PrimCode(
        cx: Scope,
        prim: Primitive,
        #[prop(optional)] glyph_only: bool,
        #[prop(optional)] hide_docs: bool,
    ) -> impl IntoView {
        let show_name = !glyph_only;
        let class = prim_class(prim);
        let symbol = prim.to_string();
        let name = if let Some(name) = prim.name().filter(|name| show_name && symbol != *name) {
            format!(" {}", name)
        } else {
            "".to_string()
        };
        let href = if prim.doc().is_some() {
            Some(format!("/primitive/{prim:?}").to_lowercase())
        } else {
            None
        };
        let title = match (prim.doc().filter(|_| !hide_docs), show_name) {
            (Some(doc), true) => Some(doc.short.clone()),
            (Some(doc), false) => Some(format!(
                "{}: {}",
                prim.name().unwrap_or_default(),
                doc.short
            )),
            (None, true) => None,
            (None, false) => prim.name().map(Into::into),
        };
        view!(cx, <a href=href style="text-decoration: none;">
            <code class="prim-code" title=title><span class=class>{ symbol }</span>{name}</code>
        </a>)
    }
}
use code::*;

fn prim_class(prim: Primitive) -> &'static str {
    macro_rules! code_font {
        ($class:literal) => {
            concat!("code-font ", $class)
        };
    }

    if prim.antiargs().is_some() || prim.antioutputs().is_some() {
        code_font!("anti-function-button")
    } else if let Some(m) = prim.modifier_args() {
        if m == 1 {
            code_font!("modifier1-button")
        } else {
            code_font!("modifier2-button")
        }
    } else {
        match prim.args() {
            Some(0) => code_font!("noadic-function-button"),
            Some(1) => code_font!("monadic-function-button"),
            Some(2) => code_font!("dyadic-function-button"),
            Some(3) => code_font!("triadic-function-button"),
            _ => code_font!("variadic-function-button"),
        }
    }
}

#[track_caller]
fn element<T: JsCast>(id: &str) -> T {
    if let Some(elem) = document().get_element_by_id(id) {
        elem.dyn_into::<T>().unwrap()
    } else {
        panic!("#{id} not found")
    }
}
