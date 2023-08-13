mod backend;
mod docs;
mod editor;
mod examples;
mod pad;

use leptos::*;
use leptos_router::*;
use uiua::primitive::Primitive;
use wasm_bindgen::JsCast;
use web_sys::HtmlAudioElement;

use crate::{
    docs::{other::Install, tour::Tour, *},
    editor::*,
    pad::*,
};

pub fn main() {
    console_error_panic_hook::set_once();

    document()
        .body()
        .unwrap()
        .remove_child(&element("top"))
        .unwrap();

    mount_to_body(|| view!( <Site/>));
}

#[component]
pub fn Site() -> impl IntoView {
    // Choose a random subtitle
    let subtitles = [
        "A stack-oriented array programming language".into_view(),
        "An array-oriented stack programming language".into_view(),
        "A programming language for point-free enjoyers".into_view(),
        "A programming language for variable dislikers".into_view(),
        "What if APL was a FORTH?".into_view(),
        "What if FORTH was an APL?".into_view(),
        view!( "Check out "<a href="https://arraycast.com/">"The Array Cast"</a>).into_view(),
        "Isn't a stack a sort of array?".into_view(),
        "It's got um...I um...arrays".into_view(),
        view!( <a href="https://youtu.be/seVSlKazsNk">"Point-Free or Die"</a>).into_view(),
        "Notation as a tool of thot".into_view(),
        "Do you like this page Marshall?".into_view(),
        "Conor Dyadic Hookstra".into_view(),
    ];
    let local_storage = window().local_storage().unwrap().unwrap();
    let mut visits: usize = local_storage
        .get_item("visits")
        .ok()
        .flatten()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);
    let subtitle = subtitles[visits % subtitles.len()].clone();
    visits = visits.overflowing_add(1).0;
    local_storage
        .set_item("visits", &visits.to_string())
        .unwrap();

    view! {
        <Router>
            <main>
                <div id="top">
                    <div id="header">
                        <div id="header-left">
                            <h1><img src="/uiua-logo.png" style="height: 1em" alt="Uiua logo"></img>" Uiua"</h1>
                            <p id="subtitle">{ subtitle }</p>
                        </div>
                        <div id="nav">
                            <p><a href="https://github.com/uiua-lang/uiua">"GitHub"</a></p>
                            <p><a href="/">"Home"</a></p>
                        </div>
                    </div>
                    <Routes>
                        <Route path="" view=|| view!( <MainPage/>)/>
                        <Route path="docs/:page?" view=|| view!( <Docs/>)/>
                        <Route path="pad" view=|| view!( <Pad/>)/>
                        <Route path="install" view=|| view!( <Install/>)/>
                        <Route path="tour" view=|| view!( <Tour/>)/>
                        <Route path="*" view=|| view!( <NotFound/>)/>
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
pub fn MainPage() -> impl IntoView {
    view! {

        <div id="links">
            <p><A href="/install">"Installation"</A></p>
            <p><A href="/docs">"Documentation"</A></p>
            <p><A href="/tour">"Language Tour"</A></p>
            <p><A href="/pad">"Pad"</A></p>
        </div>
        <Editor
            examples=examples::EXAMPLES
            size=EditorSize::Medium
            mode=EditorMode::Multiple
            help={&[
                "Type a glyph's name, then run to format the names into glyphs.",
                "You can run with ctrl/shift + enter.",
            ]}/>
        <br/>
        <br/>
        <MainText/>
    }
}

#[component]
fn MainText() -> impl IntoView {
    let borat = |_| {
        if let Ok(audio) = HtmlAudioElement::new_with_src("/weewah.mp3") {
            _ = audio.play();
        }
    };

    view! {
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
    }
}

#[component]
fn NotFound() -> impl IntoView {
    view! {
        <h1>"Page not found"</h1>
        <Editor example="$ Where could it be?\n×101⧻⊜≠' '."/>
        <h3><A href="/">"Go home"</A></h3>
    }
}

mod code {
    use super::*;
    #[component]
    pub fn PrimCode(
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
        let href = prim
            .name()
            .map(|name| format!("/docs/{name}"))
            .unwrap_or_default();
        let mut title = match (prim.doc().filter(|_| !hide_docs), show_name) {
            (Some(doc), true) => Some(doc.short_text().into_owned()),
            (Some(doc), false) => Some(format!(
                "{}: {}",
                prim.name().unwrap_or_default(),
                doc.short_text()
            )),
            (None, true) => None,
            (None, false) => prim.name().map(Into::into),
        };
        if let Some((title, ascii)) = title.as_mut().zip(prim.ascii()) {
            *title = format!("({}) {}", ascii, title);
        }
        view!( <A href=href class="prim-code-a">
            <code class="prim-code" title=title><span class=class>{ symbol }</span>{name}</code>
        </A>)
    }
}
use code::*;

fn prim_class(prim: Primitive) -> &'static str {
    macro_rules! code_font {
        ($class:literal) => {
            concat!("code-font ", $class)
        };
    }

    if prim == Primitive::Transpose {
        code_font!("monadic-function-button trans")
    } else if let Some((m, _)) = prim.modifier_args() {
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

fn get_element<T: JsCast>(id: &str) -> Option<T> {
    document()
        .get_element_by_id(id)
        .map(|elem| elem.dyn_into().unwrap())
}

#[track_caller]
fn element<T: JsCast>(id: &str) -> T {
    if let Some(elem) = get_element(id) {
        elem
    } else {
        panic!("#{id} not found")
    }
}
