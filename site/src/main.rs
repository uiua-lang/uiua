#![allow(non_snake_case)]

mod backend;
mod blog;
mod docs;
mod editor;
mod examples;
mod markdown;
mod other;
mod primitive;
mod tour;
mod tutorial;
mod uiuisms;

use std::time::Duration;

use base64::engine::{general_purpose::URL_SAFE, Engine};
use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use uiua::{ConstantDef, PrimClass, Primitive};
use wasm_bindgen::JsCast;
use web_sys::{HtmlAudioElement, HtmlDivElement};

use crate::{blog::*, docs::*, editor::*, other::*, tour::*, uiuisms::*};

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
    use Primitive::*;
    provide_meta_context();

    // Choose a subtitle
    let subtitles = [
        "A stack-based array programming language".into_view(),
        "An array-oriented tacit programming language".into_view(),
        "A programming language for point-free enjoyers".into_view(),
        "A programming language for variable dislikers".into_view(),
        view!("What if APL had no "<code>"ω"</code>"?").into_view(),
        view!("What if BQN had no "<code>"𝕩"</code>"?").into_view(),
        view!("What if LISP had fewer "<code>"()"</code>"s?").into_view(),
        view!("Check out "<a href="https://arraycast.com/">"The Array Cast"</a>).into_view(),
        view!(<a href="https://youtu.be/seVSlKazsNk">"Point-Free or Die"</a>).into_view(),
        view! {
            <div style="font-style: normal">
                <a href="/docs/advancedstack#planet-notation" style="text-decoration: none">"🌍🪐"</a>" "
                <code style="font-style: normal">
                    <span class="monadic-modifier">"⋅⋅⊙⋅⋅"</span>
                    <span class="stack-function">"∘"</span>
                </code>
            </div>
        }
        .into_view(),
        view! {
            <div>
                <div style="display: flex; gap: 0.5em;">
                    <div style="font-style: normal"><Prims prims=[Try, Assert]/></div>
                    " Dad! Can we go play outside?"
                </div>
                <div style="display: flex; gap: 0.5em;">
                    <div style="font-style: normal"><Prim prim=Repeat glyph_only=true/></div>
                    " Finish your code challenges first!"
                </div>
            </div>
        }
        .into_view(),
        view! {
            <div style="font-style: normal">
                <a href="/docs/advancedarray#ocean-notation" style="text-decoration: none">"🌊🪸"</a>" "
                <code style="font-style: normal">
                    <span class="ocean-function">"⋄~~"</span>
                </code>
            </div>
        }
        .into_view(),
        "It's got um...I um...arrays".into_view(),
        view! {
            <div style="font-size: 0.9em;">
                <div style="display: flex; gap: 0.5em; flex-wrap: wrap">
                    <div style="font-style: normal"><Prims prims=[Repeat, Do]/></div>
                    " Hey honey, the kids are asleep."
                    " What do you say we go"
                </div>
                <div style="display: flex; gap: 0.5em; flex-wrap: wrap">
                    " to the bedroom and "
                    <div class="spoiler">"make the language Turing-complete"</div>
                    "?"
                </div>
            </div>
        }
        .into_view(),
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
                            <h1><A id="header-uiua" href="/"><img src="/uiua-logo.png" style="height: 1em" alt="Uiua logo" />" Uiua"</A></h1>
                            <p id="subtitle">{ subtitle }</p>
                        </div>
                        <div id="nav">
                            <a class="pls-no-block" href="https://github.com/sponsors/uiua-lang">"Support Uiua's development"</a>
                            <a href="/">"Home"</a>
                        </div>
                    </div>
                    <Routes>
                        <Route path="" view=MainPage/>
                        <Route path="docs/:page?" view=Docs/>
                        <Route path="isms/:search?" view=Uiuisms/>
                        <Route path="pad" view=Pad/>
                        <Route path="embedpad" view=EmbedPad/>
                        <Route path="embed" view=Embed/>
                        <Route path="install" view=Install/>
                        <Route path="tour" view=Tour/>
                        <Route path="isms" view=Uiuisms/>
                        <Route path="rtl" view=RightToLeft/>
                        <Route path="blog/:page?" view=Blog/>
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

fn weewuh() {
    if let Ok(audio) = HtmlAudioElement::new_with_src("/wee-wuh.mp3") {
        _ = audio.play();
    }
}

#[component]
pub fn MainPage() -> impl IntoView {
    use Primitive::*;

    view! {
        <Title text="Uiua"/>
        <div id="links">
            <div>
                <A href="/install">"Installation"</A>
                <A href="/docs">"Documentation"</A>
                <A href="/tour">"Language Tour"</A>
            </div>
            <div>
                // <A href="/blog">"Blog"</A>
                <A href="/pad">"Pad"</A>
                <a href="https://discord.gg/3r9nrfYhCc">"Discord"</a>
                <a href="https://github.com/uiua-lang/uiua">"GitHub"</a>
            </div>
        </div>
        <Editor
            mode=EditorMode::Front
            help={&[
                "Type a glyph's name, then run to format the names into glyphs.",
                "You can run with ctrl/shift + enter.",
            ]}/>
        <br/>
        <p class="main-text">"Uiua "<span class="wee-wuh-span">"("<i>"wee-wuh "</i><button on:click=|_| weewuh() class="sound-button">"🔉"</button>")"</span>" is a general purpose, stack-based, array-oriented programming language with a focus on simplicity, beauty, and "<a href="https://en.wikipedia.org/wiki/Tacit_programming">"tacit"</a>" code."</p>
        <div class="features">
            <div>
                <div>
                    <h2>"A Loving Union"</h2>
                    <p>"Uiua combines the stack-based and array-oriented paradigms in a single language. Combining these already terse paradigms results in code with a very high information density and little syntactic noise."</p>
                    <Editor example="⇌[⍥⊃+⊙∘10 1 0]"/>
                </div>
                <div>
                    <h2>"True Arrays"</h2>
                    <p>"Uiua's one and only composite data type, the array, is based on those of APL, J, and BQN. They are multidimensional and rank-polymorphic, meaning that an operation that applies to one item also applies to many items."</p>
                    <Editor example="+2 ↯3_4 ⇡5"/>
                </div>
                <div>
                    <h2>"Rich Primitives"</h2>
                    <p>"Uiua has lots of built-in functions for all your array manipulation needs. Just a few examples:"</p>
                    <p><Prim prim=Partition/>" for splitting arrays by sequential keys:"</p>
                    <Editor example=r#"⬚@ ⊜∘≠@ ."Oh boy, neat!""#/>
                    <p><Prim prim=Select/>" for re-sequencing array items:"</p>
                    <Editor example=r#"⊏ 2_1_3_0_4 "loco!""#/>
                    <p><Prim prim=Under/>" for modifying only part of an array (among other things):"</p>
                    <Editor example="⍜(↙2)(×10) 1_2_3_4_5"/>
                </div>
                <div>
                    <h2>"Syntactic Simplicity"</h2>
                    <p>"Uiua has a simple, context-free, LL(2) grammar. Code runs from "<A href="/rtl">"right to left"</A>", top to bottom, with only "<A href="/docs/functions#modifiers">"one precedence rule"</A>". As operators are to the left of their operands, Uiua code reads a little bit like a Lisp, but with fewer parentheses."</p>
                </div>
                <div>
                    <h2>"System APIs"</h2>
                    <p>"Uiua has functions for spawning threads, interacting with the file system, communicating over network sockets, and "<A href="/docs/system">"more"</A>"."</p>
                </div>
            </div>
            <div>
                <div>
                    <h2>"Friendly Glyphs"</h2>
                    <p>"Uiua uses special characters for built-in functions that remind you what they do!"</p>
                    <Editor example="⚂ # Random number"/>
                    <Editor example="⇡8 # Range up to"/>
                    <Editor example="⇌ 1_2_3_4 # Reverse"/>
                    <Editor example="⌕ 0_2 [0 2 5 0 2 1] # Find"/>
                    <Editor example="⊟ 1_2_3 4_5_6 # Couple"/>
                    <p>"Unlike other array languages, Uiua does not have monadic and dyadic versions of each glyph. Every glyph does only one thing, so you don't need to parse an entire expression to know which version it is."</p>
                </div>
                <div>
                    <h2>"Unicode Formatter"</h2>
                    <p>"Uiua has the terseness and expressivity afforded by Unicode glyphs without the need for special keyboard or editor support. Instead, the language comes with a formatter that converts the names of built-in functions into glyphs."</p>
                    <Editor example="floor*10[repeatrand5]" help={&["", "Click to format ⇡⇡⇡⇡"]}/>
                </div>
                <div>
                    <h2>"Multimedia Output"</h2>
                    <p>"Uiua has built-in facilities for generating images and audio. Just make arrays of the pixel data or audio samples. You can even make GIFs!"</p>
                    {
                        if cfg!(debug_assertions) {
                            None
                        } else {
                            Some(view!{
                                <Editor example="⍉⊠<⊞+⇡3○∩(÷25)⇡240⇡80"/>
                                <Editor example="÷3/+○⊞×⊟×1.5.220×τ÷∶⇡.&asr"/>
                                <Editor example="Xy ← ⍉⍉⊞⊟.÷∶⇡.100\n\
                                    F ← ⍉◿1⊂⊃(+/÷)(÷3+1○×τ+)Xy\n\
                                    ∵F÷∶⇡.10"/>
                            })
                        }
                    }
                    <p>"The Uiua logo was made with Uiua! Check example 5 at the top of the page."</p>
                </div>
            </div>
        </div>
        <div>
            <h2>"Getting Started"</h2>
            <p>"For more examples of what Uiua code looks like and what it can do, see the examples in the editor at the top of this page."</p>
            <p>"For a quick overview of how the language works, see the "<A href="/tour">"Language Tour"</A>"."</p>
            <p>"For a full tutorial, see the "<A href="/docs#tutorial">"Tutorial"</A>"."</p>
            <p>"For a reference of all the built-in functions, the documentation has a "<A href="/docs#functions">"full list"</A>"."</p>
            <p>"For a curated list of Uiua functions for solving common problems, see "<A href="/isms">"Uiuisms"</A>"."</p>
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
pub fn Prim(
    prim: Primitive,
    #[prop(optional)] glyph_only: bool,
    #[prop(optional)] hide_docs: bool,
) -> impl IntoView {
    let span_class = prim_class(prim);
    let symbol = prim.to_string();
    let name = if !glyph_only && symbol != prim.name() {
        format!(" {}", prim.name())
    } else {
        "".to_string()
    };
    let href = format!("/docs/{}", prim.name());
    let mut title = String::new();
    if let Some(ascii) = prim.ascii() {
        title.push_str(&format!("({})", ascii));
    }
    if prim.glyph().is_some() && glyph_only {
        if !title.is_empty() {
            title.push(' ');
        }
        title.push_str(prim.name());
    }
    if let Primitive::Sys(op) = prim {
        title.push_str(op.long_name());
        title.push(':');
        title.push('\n');
    }
    if let Some(doc) = prim.doc().filter(|_| !hide_docs) {
        if glyph_only && !title.is_empty() && !matches!(prim, Primitive::Sys(_)) {
            title.push_str(": ");
        }
        title.push_str(&doc.short_text());
    }
    if title.is_empty() {
        view! {
            <a href=href class="prim-code-a">
                <code><span class=span_class>{ symbol }</span>{name}</code>
            </a>
        }
    } else {
        view! {
            <a href=href class="prim-code-a">
                <code class="prim-code" data-title=title><span class=span_class>{ symbol }</span>{name}</code>
            </a>
        }
    }
}

#[component]
pub fn Prims<const N: usize>(prims: [Primitive; N]) -> impl IntoView {
    prims
        .into_iter()
        .map(|prim| view!(<Prim prim=prim glyph_only=true/>))
        .collect::<Vec<_>>()
}

fn prim_class(prim: Primitive) -> &'static str {
    macro_rules! code_font {
        ($class:literal) => {
            concat!("code-font ", $class)
        };
    }

    match prim {
        Primitive::Identity => code_font!("stack-function"),
        Primitive::Transpose => code_font!("monadic-function trans"),
        Primitive::Both => code_font!("monadic-modifier bi"),
        prim if prim.is_ocean() => code_font!("ocean-function"),
        prim if prim.class() == PrimClass::Stack && prim.modifier_args().is_none() => {
            code_font!("stack-function")
        }
        prim => {
            if let Some(m) = prim.modifier_args() {
                if m == 1 {
                    code_font!("monadic-modifier")
                } else {
                    code_font!("dyadic-modifier")
                }
            } else {
                match prim.args() {
                    Some(0) => code_font!("noadic-function"),
                    Some(1) => code_font!("monadic-function"),
                    Some(2) => code_font!("dyadic-function"),
                    Some(3) => code_font!("triadic-function"),
                    _ => code_font!("variadic-function"),
                }
            }
        }
    }
}

#[component]
#[allow(clippy::needless_lifetimes)]
fn Const<'a>(con: &'a ConstantDef) -> impl IntoView {
    view! {
        <code class="prim-code" data-title={ con.doc }>{ con.name }</code>
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

#[component]
pub fn Pad() -> impl IntoView {
    let src = pad_src();
    view! {
        <Title text="Pad - Uiua"/>
        <Editor mode=EditorMode::Pad example={ &src }/>
    }
}

#[component]
pub fn EmbedPad() -> impl IntoView {
    set_timeout(remove_outer, Duration::ZERO);
    let src = pad_src();
    view! {
        <Editor mode=EditorMode::Pad example={ &src }/>
    }
}

#[component]
pub fn Embed() -> impl IntoView {
    set_timeout(remove_outer, Duration::ZERO);
    let src = pad_src();
    view! {
        <Editor mode=EditorMode::Example example={ &src }/>
    }
}

fn remove_outer() {
    if let Some(elem) = get_element::<HtmlDivElement>("header") {
        elem.remove();
    }
    if let Some(elem) = get_element::<HtmlDivElement>("top") {
        elem.set_id("");
    }
}

fn pad_src() -> String {
    let mut src = use_query_map()
        .with_untracked(|params| params.get("src").cloned())
        .unwrap_or_default();
    if let Ok(decoded) = URL_SAFE.decode(src.as_bytes()) {
        src = String::from_utf8_lossy(&decoded).to_string();
    } else if let Some((a, b)) = src.split_once("__") {
        if a.chars().filter(|&c| c == '_').count() == 2 {
            if let Ok(decoded) = URL_SAFE.decode(b.as_bytes()) {
                src = String::from_utf8_lossy(&decoded).to_string();
            }
        }
    }
    src
}

#[test]
fn site() {
    type Test = (
        std::path::PathBuf,
        String,
        std::thread::JoinHandle<(uiua::UiuaResult<uiua::Uiua>, bool)>,
    );
    fn recurse_dir(path: &std::path::Path, threads: &mut Vec<Test>) -> std::io::Result<()> {
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if entry.file_type()?.is_file() {
                for line in std::fs::read_to_string(&path)?.lines() {
                    if let Some(code) = line.trim().strip_prefix(r#"<Editor example=""#) {
                        let (code, should_fail) = if let Some(code) = code.strip_suffix(r#""/>"#) {
                            (code, false)
                        } else if let Some(code) = code.strip_suffix(r#""/> // Should fail"#) {
                            (code, true)
                        } else {
                            continue;
                        };
                        let code = code
                            .replace("\\\"", "\"")
                            .replace("\\\\", "\\")
                            .replace("\\n", "\n");
                        if [uiua::SysOp::AudioPlay, uiua::SysOp::GifShow]
                            .iter()
                            .any(|p| code.contains(p.name()))
                        {
                            continue;
                        }
                        threads.push((
                            path.to_path_buf(),
                            code.clone(),
                            std::thread::spawn(move || {
                                let mut env =
                                    uiua::Uiua::with_native_sys().with_mode(uiua::RunMode::All);
                                (env.load_str(&code).map(|_| env), should_fail)
                            }),
                        ));
                    }
                }
            } else if entry.file_type()?.is_dir() {
                recurse_dir(&path, threads)?;
            }
        }
        Ok(())
    }
    let mut threads = Vec::new();
    recurse_dir("src".as_ref(), &mut threads).unwrap();
    assert!(threads.len() > 50);
    for (path, code, thread) in threads {
        match thread.join().unwrap() {
            (Err(e), false) => {
                panic!(
                    "Test failed in {}\n{}\n{}",
                    path.display(),
                    code,
                    e.report()
                );
            }
            (Err(_), true) => {}
            (Ok(mut env), should_fail) => {
                if let Some(diag) = env.take_diagnostics().into_iter().next() {
                    if !should_fail {
                        panic!(
                            "Test failed in {}\n{}\n{}",
                            path.display(),
                            code,
                            diag.report()
                        );
                    }
                    continue;
                }
                if should_fail {
                    panic!("Test should have failed in {}\n{}", path.display(), code);
                }
            }
        }
    }
}
