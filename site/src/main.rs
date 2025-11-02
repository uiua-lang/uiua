#![allow(non_snake_case, clippy::empty_docs, clippy::mutable_key_type)]

mod blog;
mod docs;
mod examples;
mod idioms;
mod markdown;
mod other;
mod other_tutorial;
mod primitive;
mod tutorial;
mod uiuisms;

use std::{cell::Cell, io::Read, sync::OnceLock, time::Duration};

use base64::engine::{Engine, general_purpose::URL_SAFE};
use flate2::read::ZlibDecoder;
use js_sys::Date;
use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use rand::prelude::*;
use uiua::{ConstantDef, Primitive, SysOp, now};
use uiua_editor::{
    EDITOR_SHORTCUTS, Editor, EditorMode, Prim, binding_name_class, lang,
    utils::{
        ChallengeDef, get_april_fools_setting, get_april_fools_time, its_called_weewuh,
        set_april_fools,
    },
};
use wasm_bindgen::JsCast;
use web_sys::{Element, HtmlAudioElement};

use crate::{blog::*, docs::*, other::*, tutorial::Tutorial, uiuisms::*};

pub fn main() {
    console_error_panic_hook::set_once();

    document()
        .body()
        .unwrap()
        .remove_child(&element("top"))
        .unwrap();

    mount_to_body(|| view!( <Site/>));
}

static START_TIME: OnceLock<f64> = OnceLock::new();

#[component]
pub fn Site() -> impl IntoView {
    use Primitive::*;
    provide_meta_context();
    START_TIME.get_or_init(|| Date::now() / 1000.0);

    // Choose a subtitle
    let subtitles_common = [
        "A tacit array programming language",
        "An array-oriented tacit programming language",
        "A programming language for point-free enjoyers",
        "A programming language for variable dislikers",
        "A programming language to make you smile 😊",
    ];
    let subtitles_rare = [
        view!("Check out "<a href="https://arraycast.com/">"The Array Cast"</a>"!").into_view(),
        view!(<a href="https://youtu.be/seVSlKazsNk">"Point-Free or Die"</a>).into_view(),
        view! {
            <div style="font-style: normal">
                <a href="/tutorial/More Argument Manipulation#planet-notation" style="text-decoration: none">"🌍🪐"</a>" "
                <code style="font-style: normal">
                    <span class="monadic-modifier">"⋅⋅⊙⋅⋅"</span>
                    <span class="stack-function">"∘"</span>
                </code>
            </div>
        }
        .into_view(),
        "Abandon nominativity. Embrace relativity.".into_view(),
        view!(<div style="font-style: normal"><Prim prim=Under glyph_only=true/>"🗄️🍴"</div>).into_view(),
        "It's got um...I um...arrays".into_view(),
        view!(<p style="font-size: 0.9em; max-width: 25em;">
            "A monad is a "
            <s>"monoid in the category of endofunctors"</s>
            " a function with 1 argument"
        </p>)
        .into_view(),
    ];
    let mut visits = visits();
    let subtitle = if visits % 3 < 2 {
        subtitles_common[(visits as f64 * 2.0 / 3.0).round() as usize % subtitles_common.len()]
            .into_view()
    } else {
        subtitles_rare[visits / 3 % subtitles_rare.len()].clone()
    };
    visits = visits.overflowing_add(1).0;

    // Change the favicon to favicon-crayon.ico every 10 visits
    if visits % 10 == 0 {
        let link = document().create_element("link").unwrap();
        link.set_attribute("rel", "icon").unwrap();
        link.set_attribute("href", "/favicon-crayon.ico").unwrap();
        document().head().unwrap().append_child(&link).unwrap();
    }
    (window().local_storage().unwrap().unwrap())
        .set_item("visits", &visits.to_string())
        .unwrap();

    let date = Date::new_0();
    let logo_src = match visits % 9 {
        _ if its_called_weewuh() => "/assets/weewuh-logo.png",
        1 => "/assets/uiua-logo.png",
        3 => "/assets/uiua-logo-pride.png",
        5 => "/assets/uiua-logo-scrambledine.png",
        7 => "/assets/uiua-logo-jacob.svg",
        // Trans Day of Visibility
        _ if date.get_month() == 2 && date.get_date() == 31 => "/assets/uiua-logo-trans.png",
        // Pride month
        _ if date.get_month() == 5 => "/assets/uiua-logo-pride.png",
        // Halloween
        _ if date.get_month() == 9 && (28..=31).contains(&date.get_date()) => {
            "/assets/uiua-logo-halloween.png"
        }
        _ => "/assets/uiua-logo.png",
    };

    let toggle_april_fools_colors = move |_| {
        set_april_fools(!get_april_fools_setting());
        _ = window().location().reload();
    };

    view! {
        <Router>
            <ScrollToHash/>
            <Routes>
                <Route path="embedpad" view=EmbedPad/>
                <Route path="embed" view=Embed/>
                <Route path="*" view=move || view! {
                    <main>
                        <div id="top">
                            <div id="header">
                                <div id="header-left">
                                    <h1>
                                        <A id="header-uiua" href="/">
                                            <img src=logo_src style="height: 1em" alt={format!("{} logo", lang())} />
                                            " "{lang}
                                        </A>
                                    </h1>
                                    <p id="subtitle">{ subtitle.clone() }</p>
                                </div>
                                <div id="nav">
                                    {
                                        if get_april_fools_time() {
                                            Some(view!(
                                                <div title="Enable April Fool's colors (refresh after changing)">
                                                    "April Fool's:"
                                                    <input
                                                        type="checkbox"
                                                        checked=get_april_fools_setting
                                                        on:change=toggle_april_fools_colors
                                                    />
                                                </div>
                                            ))
                                        } else {
                                            None
                                        }
                                    }
                                    <a class="pls-no-block" href="https://github.com/sponsors/uiua-lang">"Support "{lang}"'s development"</a>
                                    <a href="/">"Home"</a>
                                </div>
                            </div>
                            <Outlet/>
                        </div>
                    </main>
                }>
                    <Route path="" view=MainPage/>
                    <Route path="tutorial/:page?" view=Tutorial/>
                    <Route path="docs/:page?" view=Docs/>
                    <Route path="isms/:search?" view=Uiuisms/>
                    <Route path="pad" view=PadPage/>
                    <Route path="install" view=Install/>
                    <Route path="tour" view=Tour/>
                    <Route path="isms" view=Uiuisms/>
                    <Route path="blog/:page?" view=Blog/>
                    <Route path="*" view=NotFound/>
                </Route>
            </Routes>
        </Router>
    }
}

fn visits() -> usize {
    (window().local_storage().unwrap().unwrap())
        .get_item("visits")
        .ok()
        .flatten()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

fn weewuh() {
    let i = (now() % 1.0 * 100.0) as u32;
    let src = match i {
        0 => "/assets/ooh-ee-ooh-ah.mp3",
        1..=4 => "/assets/wee-wah.mp3",
        _ => "/assets/wee-wuh.mp3",
    };
    if let Ok(audio) = HtmlAudioElement::new_with_src(src) {
        _ = audio.play();
    }
}

#[component]
pub fn MainPage() -> impl IntoView {
    use Primitive::*;

    let visits = visits();

    fn rich_prim(prim: Primitive, text: &'static str, example: &'static str) -> impl Fn() -> View {
        move || {
            view! {
                <p><Prim prim=prim/>" "{text}":"</p>
                <Editor example=example/>
            }
            .into_view()
        }
    }

    let mut rich_prims = vec![
        rich_prim(
            Select,
            "for re-sequencing array items",
            r#"⊏ 2_1_3_0_4 "loco!""#,
        ),
        rich_prim(
            Deduplicate,
            "for removing duplicate items",
            r#"◴ "hello, world!""#,
        ),
        rich_prim(Sort, "for... sorting", "⍆ [2 8 3 2 1 5]"),
        rich_prim(
            Keep,
            "for filtering",
            r#"▽ [1 1 1 0 0 0 0 1 0] "filter me""#,
        ),
        rich_prim(
            Where,
            "for finding the indices of things",
            "⊚≤5 [4 8 3 9 2 7 1]",
        ),
        rich_prim(Mask, "for finding subsequences", r#"⦷ "ra" "abracadabra""#),
        rich_prim(
            Partition,
            "for splitting arrays by sequential keys",
            r#"⬚@ ⊜∘⊸≠@ "Oh boy, neat!""#,
        ),
        rich_prim(
            Un,
            "for inverting the behavior of a function",
            "°(÷2+1) [1 2 3 4]",
        ),
        rich_prim(
            Under,
            "for modifying only part of an array (among other things)",
            r#"⍜(↙2|×10) 1_2_3_4_5"#,
        ),
    ];

    let indices = if visits < 4 {
        vec![0, rich_prims.len() - 3, rich_prims.len() - 1]
    } else {
        let mut rng = SmallRng::seed_from_u64(visits as u64);
        let mut indices: Vec<usize> = (0..rich_prims.len()).collect();
        indices.shuffle(&mut rng);
        indices.truncate(3);
        indices.sort_unstable();
        indices
    };
    let mut rich_prims: Vec<_> = indices
        .into_iter()
        .rev()
        .map(|i| rich_prims.remove(i)())
        .collect();
    rich_prims.reverse();

    view! {
        <Title text=lang()/>
        <div id="links">
            <div>
                <A href="/install">"Installation"</A>
                <A href="/docs">"Documentation"</A>
                <A href="/tour">"Language Tour"</A>
            </div>
            <div>
                <A href="/tutorial/Introduction" class="slow-pulse">"Tutorial"</A>
                <A href="/pad">"Pad"</A>
                <A href="/blog">"Blog"</A>
                <a href="https://discord.gg/3r9nrfYhCc">"Discord"</a>
                <a href="https://github.com/uiua-lang/uiua">"GitHub"</a>
            </div>
        </div>
        <Editor
            mode=EditorMode::Showcase
            examples=examples::EXAMPLES
                .iter()
                .map(|&ex|
                    if its_called_weewuh() {
                        match ex {
                            examples::UIUA => examples::WEEWUH,
                            examples::LOGO => examples::WEEWUH_LOGO,
                            examples::PALINDROME => examples::WEEWUH_PALINDROME,
                            _ => ex,
                        }
                    } else{
                        ex
                    }
                )
                .map(ToString::to_string)
                .collect()
            help={&[
                "Type a glyph's name, then run to format the names into glyphs.",
                "You can run with ctrl/shift + enter.",
            ]}/>
        <br/>
        <p class="main-text">{lang}" "<span class="wee-wuh-span">"("<i>"wee-wuh "</i><button on:click=|_| weewuh() class="sound-button">"🔉"</button>")"</span>" is a general purpose array-oriented programming language with a focus on simplicity, beauty, and "<a href="https://en.wikipedia.org/wiki/Tacit_programming">"tacit"</a>" code."</p>
        <p class="main-text">{lang}" lets you write code that is as short as possible while remaining readable, so you can focus on problems rather than ceremony."</p>
        <p class="main-text">"The language is not yet stable, as its design space is still being explored. However, it is already quite powerful and fun to use!"</p>
        <div class="features">
            <div>
                <div>
                    <Hd id="saying less">"Saying Less"</Hd>
                    <p>{lang}" combines the array-oriented programming paradigm with a fully tacit execution model. Combining these already terse systems results in code with a very high information density and little syntactic noise."</p>
                    <Editor example="⍥◡+9∩1"/>
                    <p>"If this code seems weird and unreadable, that's okay! It's important to remember that "<a href="https://vector-of-bool.github.io/2018/10/31/become-perl.html">"foreign ≠ confusing"</a>"."</p>
                </div>
                <div>
                    <Hd id="true-arrays">"True Arrays"</Hd>
                    <p>{lang}"'s one and only composite data type, the array, is based on those of APL, J, and BQN. They are multidimensional and rank-polymorphic, meaning that an operation that applies to one item also applies to many items."</p>
                    <Editor example="◿5 ↯3_4 ⇡12"/>
                </div>
                <div>
                    <Hd id="rich-primitives">"Rich Primitives"</Hd>
                    <p>{lang}" has lots of built-in functions for all your array manipulation needs. Just a few examples:"</p>
                    { rich_prims }
                </div>
                <div>
                    <Hd id="syntactic-simplicity">"Syntactic Simplicity"</Hd>
                    <p>{lang}" has a simple, context-free, LL(3) grammar. Code runs from right to left, top to bottom, with only "<A href="/tutorial/Modifiers and Functions#modifiers">"one precedence rule"</A>". As operators are to the left of their operands, "{lang}" code reads a little bit like a Lisp, but with fewer parentheses."</p>
                </div>
                <div>
                    <Hd id="system-apis">"System APIs"</Hd>
                    <p>{lang}" has functions for spawning threads, interacting with the file system, communicating over network sockets, and "<A href="/docs/system">"more"</A>"."</p>
                </div>
                <div>
                    <Hd id="rust-ingegration">"Rust Integration"</Hd>
                    <p>{lang}" can be embedded in Rust programs "<a href="https://docs.rs/uiua">"as a library"</a>"."</p>
                </div>
                <div>
                    <Hd id="ffi">"FFI"</Hd>
                    <p>{lang}" has experimental support for calling functions from shared libraries through "<Prim prim=Sys(SysOp::Ffi)/>"."</p>
                </div>
            </div>
            <div>
                <div>
                    <Hd id="friendly-glyphs">"Friendly Glyphs"</Hd>
                    <p>{lang}" uses special characters for built-in functions that remind you what they do!"</p>
                    <Editor example="⚂ # Random number"/>
                    <Editor example="⇡8 # Range up to"/>
                    <Editor example="⇌ 1_2_3_4 # Reverse"/>
                    <Editor example="⌕ \"ab\" \"abracabra\" # Find"/>
                    <Editor example="⊟ 1_2_3 4_5_6 # Couple"/>
                    <p>"Unlike other array languages, "{lang}" does not have monadic and dyadic versions of each glyph. Every glyph does only one thing, so you don't need to parse an entire expression to know which version it is."</p>
                </div>
                <div>
                    <Hd id="unicode-formatter">"Unicode Formatter"</Hd>
                    <p>{lang}" has the terseness and expressivity afforded by Unicode glyphs without the need for special keyboard or editor support. Instead, the language comes with a formatter that converts the names of built-in functions into glyphs."</p>
                    <Editor example="floor*10repeatrand5" help={&["", "Click to format ⇡⇡⇡            "]}/>
                </div>
                <div>
                    <Hd id="multimedia-output">"Multimedia Output"</Hd>
                    <p>{lang}" has built-in facilities for generating images and audio. Just make arrays of the pixel data or audio samples. You can even make GIFs!"</p>
                    {
                        if cfg!(debug_assertions) {
                            None
                        } else {
                            Some(view!{
                                <Editor example="⍉⊞<⊞+⇡3∿∩(÷25)⇡240⇡80"/>
                                <Editor example="÷3/+∿⊞×⊟⊸×1.5 220×τ÷⟜⇡&asr"/>
                                <Editor example="Xy ← °⍉˙⊞⊟÷⟜⇡\nF  ← ⍉◿1⊂⊃(+/÷|÷3+1∿×τ+)Xy\n≡F100 ÷⟜⇡10"/>
                            })
                        }
                    }
                    <p>"The "{lang}" logo was made with "{lang}"! Check example 5 at the top of the page."</p>
                </div>
                <div>
                    <Hd id="language-server">"Language Server"</Hd>
                    <p>"The "{lang}" interpreter has a built-in language server that uses the "<a href="https://microsoft.github.io/language-server-protocol/">"Language Server Protocol"</a>", so you can "<A href="/install#editor-support">"use it with your favorite editor"</A>"."</p>
                </div>
            </div>
        </div>
        <div>
            <Hd id="getting-started">"Getting Started"</Hd>
            <p>"For more examples of what "{lang}" code looks like and what it can do, see the examples in the editor at the top of this page."</p>
            <p>"For a quick overview of how the language works, see the "<A href="/tour">"Language Tour"</A>"."</p>
            <p>"For a full tutorial, see the "<A href="/docs#tutorial">"Tutorial"</A>"."</p>
            <p>"For a reference of all the built-in functions, the documentation has a "<A href="/docs#functions">"full list"</A>"."</p>
            <img src="assets/kala/default.svg" style="width:20em;display:block;margin:auto"/>
        </div>
    }
}

#[component]
fn NotFound() -> impl IntoView {
    view! {
        <h1>"Page not found"</h1>
        <Editor example="$ Where could it be?\n×101⧻⊜⧻⊸≠@ "/>
        <h3><A href="/">"Go home"</A></h3>
        <img src="assets/kala/confused.svg" style="width:20em;display:block;margin:auto"/>
    }
}

#[cfg(test)]
fn prim_html(prim: Primitive, glyph_only: bool, hide_docs: bool) -> String {
    use uiua::PrimDoc;

    let symbol_class = format!("prim-glyph {}", uiua_editor::prim_class(prim));
    let symbol = prim.to_string();
    let name = if !glyph_only && symbol != prim.name() {
        format!(" {}", prim.name())
    } else {
        "".to_string()
    };
    let href = format!("/docs/{}", prim.name());
    let mut title = String::new();
    if let Some(ascii) = prim.ascii() {
        title.push_str(&format!("({ascii})"));
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
    if !hide_docs {
        let doc = PrimDoc::from(prim);
        if glyph_only && !title.is_empty() && !matches!(prim, Primitive::Sys(_)) {
            title.push_str(": ");
        }
        title.push_str(&doc.short_text());
    }
    if title.is_empty() {
        format!(
            r#"<a href="{href}"class="prim-code-a">
                <code><span class="{symbol_class}">{symbol}</span>{name}</code>
            </a>"#,
        )
    } else {
        format!(
            r#"<a href="{href}" class="prim-code-a">
                <code class="prim-code" data-title="{title}"><span class="{symbol_class}">{symbol}</span>{name}</code>
            </a>"#,
        )
    }
}

#[component]
pub fn Prims<const N: usize>(
    prims: [Primitive; N],
    #[prop(optional)] show_names: bool,
) -> impl IntoView {
    prims
        .into_iter()
        .map(|prim| view!(<Prim prim=prim glyph_only={!show_names}/>))
        .collect::<Vec<_>>()
}

#[component]
#[allow(clippy::needless_lifetimes)]
fn Const<'a>(con: &'a ConstantDef) -> impl IntoView {
    let name_class = binding_name_class(con.name);
    let outer_class = if name_class.is_some() {
        "prim-code-a code-background"
    } else {
        "prim-code-a"
    };
    let inner_class = format!(
        "prim-code stack-function {}",
        name_class.unwrap_or_default()
    );
    view! {
        <a href={format!("/docs/constants#{}", con.name)} class=outer_class>
            <code
                id={con.name}
                class=inner_class
                data-title={ con.doc().lines().next().unwrap_or_default().to_string() }
            >{ con.name }</code>
        </a>
    }
}

#[track_caller]
#[allow(clippy::manual_map)]
fn get_element<T: JsCast>(id: &str) -> Option<T> {
    if let Some(elem) = document().get_element_by_id(id) {
        Some(elem.dyn_into().unwrap())
    } else {
        None
    }
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
pub fn Tour() -> impl IntoView {
    title_markdown("Language Tour", "/text/tour.md", View::default())
}

#[component]
pub fn PadPage() -> impl IntoView {
    let src = pad_src();
    let version = format!("{} {}", lang(), uiua::VERSION);
    let help = &[
        match lang() {
            "Weewuh" => "Note: Weewuh is not yet stable",
            _ => "Note: Uiua is not yet stable",
        },
        &version,
    ];
    view! {
        <Title text=format!("Pad - {}", lang())/>
        <Editor mode=EditorMode::Pad example={ &src } help=help/>
        <br/>
        <br/>
        {
            let date = Date::new_0();
            if date.get_month() == 11 || date.get_month() == 10 && date.get_date() > 15 {
                let year = date.get_full_year();
                const CODE: &str = "6680-a10280cf";
                let copy_code = move |_| {
                     _ = window().navigator().clipboard().write_text(CODE)
                };
                Some(view! {
                    <p>"Join the official "<a href={format!("https://adventofcode.com/{year}")}>"Advent of Code "{year}</a>" "{lang}" community leaderboard "<a href={format!("https://adventofcode.com/{year}/leaderboard/private")}>"here!"</a>"  "<button on:click=copy_code>"Copy code"</button></p>
                })
            } else {
                None
            }
        }
        <br/>
        <p>"You can load files into the pad by dragging and dropping them into the window."</p>
        <p>"Replace "<code>"pad"</code>" in links with "<code>"embed"</code>" or "<code>"embedpad"</code>" to embed the editor."</p>
        <p>"Keyboard shortcuts:"</p>
        <code class="code-block">
            { EDITOR_SHORTCUTS }
        </code>
        <p>"Want a pad-like experience in the native interpreter? Try the "<code>"uiua -w"</code>" command to show output in a window."</p>
        <p>"You can download the newest version of the native interpreter "<a href="https://github.com/uiua-lang/uiua/releases">"here"</a>"."</p>
    }
}

#[component]
pub fn EmbedPad() -> impl IntoView {
    let src = pad_src();
    view! {
        <Editor mode=EditorMode::Pad example={ &src }/>
    }
}

#[component]
pub fn Embed() -> impl IntoView {
    let src = pad_src();
    view! {
        <Editor mode=EditorMode::Example example={ &src }/>
    }
}

fn pad_src() -> String {
    let src = use_query_map()
        .with_untracked(|params| params.get("src").cloned())
        .unwrap_or_default();
    let mut url_decoded = src.clone().into_bytes();
    if let Some((_, encoded)) = src.split_once("__") {
        if let Ok(dec) = URL_SAFE.decode(encoded.as_bytes()) {
            // logging::log!("decoded base64 after version number");
            url_decoded = dec;
        }
    } else if let Ok(dec) = URL_SAFE.decode(src.as_bytes()) {
        // logging::log!("decoded base64 without version number");
        url_decoded = dec;
    }
    let mut decoder = ZlibDecoder::new(url_decoded.as_slice());
    let mut unzipped = String::new();
    match decoder.read_to_string(&mut unzipped) {
        Ok(_) => {
            // logging::log!("decompressed");
            unzipped
        }
        Err(_e) => {
            // logging::log!("decompression error: {_e}");
            String::from_utf8_lossy(&url_decoded).into_owned()
        }
    }
}

#[test]
fn site() {
    type Test = (
        std::path::PathBuf,
        String,
        std::thread::JoinHandle<(uiua::UiuaResult<uiua::Compiler>, bool)>,
    );
    fn recurse_dir(path: &std::path::Path, threads: &mut Vec<Test>) -> std::io::Result<()> {
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if entry.file_type()?.is_file() {
                for line in std::fs::read_to_string(&path)?.lines() {
                    let (code, should_fail) =
                        if let Some(code) = line.trim().strip_prefix(r#"<Editor example=""#) {
                            if let Some(code) = code.strip_suffix(r#""/>"#) {
                                (code, false)
                            } else if let Some(code) = code.strip_suffix(r#""/> // Should fail"#) {
                                (code, true)
                            } else {
                                continue;
                            }
                        } else if let Some(line) =
                            line.strip_prefix("            { ").and_then(|line| {
                                (line.split(", ").nth(2))
                                    .and_then(|rest| rest.strip_prefix('"'))
                                    .and_then(|rest| rest.strip_suffix("\") }"))
                            })
                        {
                            (line, false)
                        } else {
                            continue;
                        };
                    let code = code
                        .replace("\\\\n", "<escaped-newline>")
                        .replace("\\n", "\n")
                        .replace("\\\"", "\"")
                        .replace("\\\\", "\\")
                        .replace("<escaped-newline>", "\\n");
                    if code.contains("\"git:")
                        || [uiua::SysOp::AudioPlay, uiua::SysOp::GifShow]
                            .iter()
                            .any(|p| code.contains(p.name()))
                    {
                        continue;
                    }
                    threads.push((
                        path.to_path_buf(),
                        code.clone(),
                        std::thread::spawn(move || {
                            (
                                uiua::Uiua::with_backend(
                                    uiua_editor::backend::WebBackend::default(),
                                )
                                .run_str(&code),
                                should_fail,
                            )
                        }),
                    ));
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
            (Ok(mut comp), should_fail) => {
                if let Some(diag) = comp.take_diagnostics().into_iter().next() {
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

#[component]
#[allow(clippy::needless_lifetimes)]
fn Hd<'a>(id: &'a str, #[prop(optional)] class: &'a str, children: Children) -> impl IntoView {
    let id = id.to_string();
    view! {
        <h2 id={id.clone()}>
            <a class={format!("header {class}")} href={ format!("#{id}") }>{children()}</a>
        </h2>
    }
}

#[component]
#[allow(clippy::needless_lifetimes)]
fn Hd3<'a>(id: &'a str, #[prop(optional)] class: &'a str, children: Children) -> impl IntoView {
    let id = id.to_string();
    view! {
        <h3 id={id.clone()}>
            <a class={format!("header {class}")} href={ format!("#{id}") }>{children()}</a>
        </h3>
    }
}

#[cfg(test)]
#[test]
fn gen_primitives_json() {
    use serde::*;
    use std::{collections::BTreeMap, fs, ops::Not};
    use uiua::PrimDoc;

    #[derive(Serialize)]
    struct PrimDef {
        #[serde(skip_serializing_if = "Option::is_none")]
        ascii: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        glyph: Option<char>,
        #[serde(skip_serializing_if = "Option::is_none")]
        args: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        outputs: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        modifier_args: Option<usize>,
        class: String,
        description: String,
        #[serde(skip_serializing_if = "Not::not")]
        experimental: bool,
        #[serde(skip_serializing_if = "Not::not")]
        deprecated: bool,
    }

    let mut prims = BTreeMap::new();
    for prim in Primitive::all() {
        let doc = PrimDoc::from(prim);
        prims.insert(
            prim.name(),
            PrimDef {
                ascii: prim.ascii().map(|a| a.to_string()),
                glyph: prim.glyph(),
                args: prim.args(),
                outputs: prim.outputs(),
                modifier_args: prim.modifier_args(),
                class: format!("{:?}", prim.class()),
                description: doc.short_text().into(),
                experimental: prim.is_experimental(),
                deprecated: prim.is_deprecated(),
            },
        );
    }
    let json = serde_json::to_string_pretty(&prims).unwrap();
    fs::write("primitives.json", json).unwrap();
}

#[component]
fn ScrollToHash() -> impl IntoView {
    move || {
        let location = use_location();
        create_effect(move |_| {
            let hash = location.hash.get();
            if !hash.is_empty() {
                set_timeout(
                    move || {
                        let id = hash.trim_start_matches('#');
                        if let Some(elem) = get_element::<Element>(id) {
                            elem.scroll_into_view();
                        }
                    },
                    Duration::from_millis(0),
                )
            }
        });
    }
}

#[component]
pub fn Challenge<'a, P: IntoView + 'a>(
    number: u8,
    prompt: P,
    example: &'a str,
    answer: &'a str,
    tests: &'a [&'a str],
    #[prop(optional)] default: &'a str,
    #[prop(optional)] flip: bool,
    #[prop(optional)] best_answer: &'a str,
) -> impl IntoView {
    let def = ChallengeDef {
        example: example.into(),
        intended_answer: answer.into(),
        best_answer: (!best_answer.is_empty()).then(|| best_answer.into()),
        tests: tests.iter().copied().map(Into::into).collect(),
        flip,
        did_init_run: Cell::new(false),
    };
    view! {
        <div class="challenge">
            <h3>"Challenge "{number}</h3>
            <p>"Write a program that "<strong>{prompt}</strong>"."</p>
            <Editor challenge=def example=default/>
        </div>
    }
}
