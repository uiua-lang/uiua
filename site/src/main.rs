mod backend;
mod docs;
mod editor;
mod examples;
mod other;
mod pad;
mod primitive;
mod tour;
mod tutorial;

use leptos::*;
use leptos_router::*;
use uiua::primitive::{PrimClass, Primitive};
use wasm_bindgen::JsCast;
use web_sys::HtmlAudioElement;

use crate::{docs::*, editor::*, other::*, pad::*, tour::*};

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
                <div id="top" class="top">
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
                        <Route path="" view=MainPage/>
                        <Route path="docs/:page?" view=Docs/>
                        <Route path="pad" view=Pad/>
                        <Route path="install" view=Install/>
                        <Route path="tour" view=Tour/>
                        <Route path="*" view=NotFound/>
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
    use Primitive::*;

    let borat = |_| {
        if let Ok(audio) = HtmlAudioElement::new_with_src("/weewah.mp3") {
            _ = audio.play();
        }
    };

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
        <p style="font-size: 130%">"Uiua "<span style="font-size: 70%; opacity: 0.8;">"("<i>"wee-wuh "</i><button on:click=borat class="sound-button">"🔉"</button>")"</span>" is a stack-oriented array programming language with a focus on simplicity, expressiveness, and "<a href="https://en.wikipedia.org/wiki/Tacit_programming">"tacit"</a>" code."</p>
        <div class="features">
            <div>
                <div>
                    <h2>"A Loving Union"</h2>
                    <p>"Uiua combines the stack-oriented and array-oriented paradigms in a single language. Combining these already terse paradigms results in code with a very high information density and little syntactic noise."</p>
                    <Editor example="⇌⍥(⊂/+↙2.)8⊟.1"/>
                </div>
                <div>
                    <h2>"True Arrays"</h2>
                    <p>"Uiua's one and only composite data type, the array, is based on those of APL, J, and BQN. They are multidimensional and rank-polymorphic, meaning that an operation that applies to one item also applies to many items."</p>
                    <Editor example="+1↯3_4⇡12"/>
                </div>
                <div>
                    <h2>"Rich Primitives"</h2>
                    <p>"Uiua has a lots of built-in functions for all your array manipulation needs. Just a few examples:"</p>
                    <p><PrimCode prim=Partition/>" for splitting arrays by sequential keys:"</p>
                    <Editor example=r#"⍛@ ⊜·≠@ ."Oh boy, neat!""#/>
                    <p><PrimCode prim=Select/>" for re-sequencing array items:"</p>
                    <Editor example=r#"⊏ 2_1_3_0_4 "loco!""#/>
                    <p><PrimCode prim=Under/>" for modifiying only part of an array (among other things):"</p>
                    <Editor example="⍜'↙2'×10 1_2_3_4_5"/>
                </div>
                <div>
                    <h2>"Syntactic Simplicity"</h2>
                    <p>"Uiua has a simple context-free grammar. Code runs from right to left, top to bottom, with only "<A href="/docs/functions#modifiers">"one precedence rule"</A>". As operators are to the left of their operands, Uiua code looks a little bit like a Lisp, but with fewer parentheses."</p>
                </div>
            </div>
            <div>
                <div>
                    <h2>"Friendly Glyphs"</h2>
                    <p>"Uiua uses special characters for built-in functions that remind you what they do!"</p>
                    <Editor example="⚂ # Random number"/>
                    <Editor example="⇡8 # Range up to"/>
                    <Editor example="⇌ 1_2_3_4 # Reverse"/>
                    <Editor example="⌕ 2 [0 2 5 1 2] # Find"/>
                    <p>"Unlike other array languages, Uiua does not have monadic and dyadic versions of each glyph. Every glyph does only on thing, so you don't need to parse an entire expression to know which version it is."</p>
                </div>
                <div>
                    <h2>"Unicode Formatter"</h2>
                    <p>"Uiua has the terseness and expressivity afforded by Unicode glyphs without the need for special keyboard or editor support. Instead, the language comes with a formatter that converts the names of built-in functions into glyphs."</p>
                    <Editor example="floor*10[repeatrand5]" help={&["", "Click to format ⇡⇡⇡⇡"]}/>
                </div>
                <div>
                    <h2>"Multimedia Output"</h2>
                    <p>"Uiua has built-in facilities for generating images and audio. Just make arrays of the pixel data or audio samples!"</p>
                    <Editor example="⊞(<+⇡3○⍜⊟'÷30∶)⇡100⇡300"/>
                    <Editor example="÷3/+○⊞×⊟×1.5.220×τ÷∶⇡.44100"/>
                    <p>"The Uiua logo was made with Uiua! Check example 4 at the top of the page."</p>
                </div>
                <div>
                    <h2>"System APIs"</h2>
                    <p>"Uiua has functions for spawning threads, interacting with the file system, communicating over network sockets, and "<A href="/docs/system">"more"</A>"."</p>
                </div>
            </div>
        </div>
    }
}

#[component]
fn NotFound() -> impl IntoView {
    view! {
        <h1>"Page not found"</h1>
        <Editor example="$ Where could it be?\n×101⧻⊜⧻≠@ ."/>
        <h3><A href="/">"Go home"</A></h3>
    }
}

#[component]
pub fn PrimCode(
    prim: Primitive,
    #[prop(optional)] glyph_only: bool,
    #[prop(optional)] hide_docs: bool,
) -> impl IntoView {
    let show_name = !glyph_only;
    let span_class = prim_class(prim);
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
    let code_class = if title.is_some() { "prim-code" } else { "" };
    view! {
        <A href=href class="prim-code-a">
            <code class=code_class data-title=title><span class=span_class>{ symbol }</span>{name}</code>
        </A>
    }
}

#[component]
pub fn PrimCodes<const N: usize>(prims: [Primitive; N]) -> impl IntoView {
    prims
        .into_iter()
        .map(|prim| view!(<PrimCode prim=prim glyph_only=true/>))
        .collect::<Vec<_>>()
}

fn prim_class(prim: Primitive) -> &'static str {
    macro_rules! code_font {
        ($class:literal) => {
            concat!("code-font ", $class)
        };
    }

    if prim == Primitive::Transpose {
        code_font!("monadic-function-button trans")
    } else if let PrimClass::Stack = prim.class() {
        code_font!("stack-function-button")
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
