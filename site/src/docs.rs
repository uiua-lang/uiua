use std::{
    collections::{HashMap, HashSet},
    iter::once,
    time::Duration,
};

use enum_iterator::all;
use leptos::{leptos_dom::helpers::location, *};
use leptos_meta::*;
use leptos_router::*;
use uiua::{PrimClass, Primitive, SysOpClass};
use uiua_editor::lang;
use wasm_bindgen::JsCast;
use web_sys::{Event, EventInit, HtmlInputElement, ScrollBehavior, ScrollIntoViewOptions};

use crate::{
    element, idioms::Idioms, markdown::Markdown, other::*, other_tutorial::OtherTutorialPage,
    primitive::*, tutorial::TUTORIAL_NAMES, uiuisms::Uiuisms, Hd, Prim, Tour,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocsPage {
    Search(String),
    Tour,
    Design,
    Technical,
    Install,
    Uiuisms,
    Changelog,
    RightToLeft,
    Constants,
    Combinators,
    Subscripts,
    Optimizations,
    FormatConfig,
    Experimental,
    Idioms,
}

impl IntoParam for DocsPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        let value = value.unwrap_or("");
        match value {
            "" => Err(ParamsError::MissingParam(name.to_string())),
            "tour" => Ok(Self::Tour),
            "design" => Ok(Self::Design),
            "technical" => Ok(Self::Technical),
            "install" => Ok(Self::Install),
            "isms" => Ok(Self::Uiuisms),
            "changelog" => Ok(Self::Changelog),
            "rtl" => Ok(Self::RightToLeft),
            "constants" => Ok(Self::Constants),
            "combinators" => Ok(Self::Combinators),
            "subscripts" => Ok(Self::Subscripts),
            "optimizations" => Ok(Self::Optimizations),
            "format-config" => Ok(Self::FormatConfig),
            "experimental" => Ok(Self::Experimental),
            "idioms" => Ok(Self::Idioms),
            value => Ok(Self::Search(value.into())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct DocsParams {
    page: DocsPage,
}

#[component]
pub fn Docs() -> impl IntoView {
    move || {
        let Ok(params) = use_params::<DocsParams>().get() else {
            return view!( <DocsHome/>).into_view();
        };
        let page = params.page;
        let page_view = match page {
            DocsPage::Search(search) => return view!( <DocsHome search=search/>).into_view(),
            DocsPage::Tour => Tour().into_view(),
            DocsPage::Design => {
                title_markdown("Design", "/text/design.md", View::default()).into_view()
            }
            DocsPage::Technical => Technical().into_view(),
            DocsPage::Install => Install().into_view(),
            DocsPage::Uiuisms => Uiuisms().into_view(),
            DocsPage::Changelog => Changelog().into_view(),
            DocsPage::RightToLeft => RightToLeft().into_view(),
            DocsPage::Constants => Constants().into_view(),
            DocsPage::Subscripts => Subscripts().into_view(),
            DocsPage::Combinators => Combinators().into_view(),
            DocsPage::Optimizations => Optimizations().into_view(),
            DocsPage::FormatConfig => title_markdown(
                "Formatter Configuration",
                "/text/format_config.md",
                View::default(),
            )
            .into_view(),
            DocsPage::Experimental => Experimental().into_view(),
            DocsPage::Idioms => Idioms().into_view(),
        };

        view! {
            <A href="/docs">"Back to Docs Home"</A>
            <br/>
            <br/>
            { page_view }
            <br/>
            <br/>
            <A href="/docs">"Back to Docs Home"</A>
        }
        .into_view()
    }
}

pub fn title_markdown(title: &str, src: &str, end: View) -> impl IntoView {
    view! {
        <Title text={format!("{} - {} Docs", lang(), title)}/>
        <Markdown src=src/>
        { end }
    }
}

fn scroll_to_docs_functions(options: &ScrollIntoViewOptions) {
    element::<HtmlInputElement>("function-search")
        .scroll_into_view_with_scroll_into_view_options(options);
}

#[component]
fn DocsHome(#[prop(optional)] search: String) -> impl IntoView {
    let search = urlencoding::decode(&search)
        .map(|s| s.into_owned())
        .unwrap_or_default();
    let (results, set_result) = create_signal(None);
    let (current_prim, set_current_prim) = create_signal(None);
    let (clear_button, set_clear_button) = create_signal(None);
    let (old_allowed, set_old_allowed) = create_signal(Allowed::all());
    let update_search = move |text: &str, update_location: bool| {
        // Update clear button
        set_clear_button.set(if text.is_empty() {
            None
        } else {
            let clear_search = move |_| {
                let search_input = element::<HtmlInputElement>("function-search");
                search_input.set_value("");
                let init = EventInit::new();
                init.set_bubbles(true);
                _ = search_input
                    .dispatch_event(&Event::new_with_event_init_dict("input", &init).unwrap());
            };
            Some(view!( {}<button on:click=clear_search>"✕"</button>).into_view())
        });

        // Derive allowed primitives
        let allowed = Allowed::from_search(text);
        if !text.is_empty() {
            let siv_options = ScrollIntoViewOptions::new();
            siv_options.set_behavior(ScrollBehavior::Smooth);
            scroll_to_docs_functions(&siv_options);
        }
        if update_location {
            let text = text.to_string();
            set_timeout(
                move || {
                    let search_input = element::<HtmlInputElement>("function-search").value();
                    if search_input == text {
                        BrowserIntegration {}.navigate(&LocationChange {
                            value: format!("/docs/{}", urlencoding::encode(&text)),
                            scroll: false,
                            replace: false,
                            ..Default::default()
                        })
                    }
                },
                Duration::from_secs(2),
            );
        }
        if allowed == old_allowed.get() && results.get().is_some() {
            return;
        }
        set_old_allowed.set(allowed.clone());
        if allowed.classes.is_empty() && allowed.prims.is_empty() {
            // No Results
            set_result.set(Some(view!( <p>"No results"</p>).into_view()));
            set_current_prim.set(None);
        } else if allowed.prims.len() == 1
            && [PrimClass::all().count(), 1].contains(&allowed.classes.len())
        {
            // Only one result
            let prim = allowed.prims.into_iter().next().unwrap();
            let siv_options = ScrollIntoViewOptions::new();
            siv_options.set_behavior(ScrollBehavior::Instant);
            scroll_to_docs_functions(&siv_options);
            set_result.set(Some(view!( <PrimDocs prim=prim/>).into_view()));
            set_current_prim.set(Some(prim));
        } else {
            // Multiple results
            set_result.set(Some(allowed.table().into_view()));
            set_current_prim.set(None);
        }
    };
    let update_title = move || {
        current_prim
            .get()
            .map(|p| format!("{} - {} Docs", lang(), p.name()))
            .unwrap_or_else(|| format!("{} Docs", lang()))
    };

    set_timeout(
        {
            let search = search.clone();
            move || update_search(&search, false)
        },
        Duration::from_secs(0),
    );
    let on_search_input = move |event: Event| {
        let elem: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
        update_search(&elem.value(), true);
    };

    view! {
        <Title text=update_title/>
        <div id="header">
            <h1>"Documentation"</h1>
            <a href="#functions" style="text-decoration: none">"↓ Jump to Functions ↓"</a>
        </div>

        <Hd id="language-tour">"Language Tour"</Hd>
        <p>"If you want to jump right in, check out the "<A href="/docs/tour">"Language Tour"</A>" for a high-level overview!"</p>
        <p>"Otherwise, read on for more detailed documentation."</p>

        <Hd id="tutorial">"Tutorial"</Hd>
        <h3><strong><em>"If you are new to "{lang}", you will likely be lost if you don't read this!"</em></strong></h3>
        <p>"These pages introduce "{lang}" concepts one at a time, each tutorial building on the previous. They go into much more depth than the language tour."</p>
        <p>"They are meant to be read in order, but feel free to skip around!"</p>
        <ul>{ TUTORIAL_NAMES.iter()
            .map(|&p| view!(<li><A href={format!("/tutorial/{p}")}>{p}</A></li>))
            .collect::<Vec<_>>()
        }</ul>

        <Hd id="other-tutorials">"Other Tutorials"</Hd>
        <p>"These tutorials cover more specific topics. They assume you have read the main tutorial above, but they can be read in any order."</p>
        <ul>{ all::<OtherTutorialPage>()
            .map(|p| view!( <li><A href={format!("/tutorial/{}", p.path())}>{p.title()}</A>" - "{p.description()}</li>))
            .collect::<Vec<_>>()
        }</ul>

        <Hd id="other-docs">"Other Docs"</Hd>
        <ul>
            <li><A href="/docs/install">"Installation"</A>" - how to install and use "{lang}"'s interpreter"</li>
            <li><A href="/docs/changelog">"Changelog"</A>" - what's new in each version"</li>
            <li><A href="/docs/constants">"Constants"</A>" - a list of the shadowable constants"</li>
            <li><A href="/docs/subscripts">"Subscripts"</A>" - a list of subscript-compatible functions"</li>
            <li><A href="/docs/format-config">"Formatter Configuration"</A>" - how to configure the "{lang}" formatter"</li>
            <li><A href="/docs/optimizations">"Optimizations"</A>" - a list of optimizations in the interpreter"</li>
            <li><A href="/docs/experimental">"Experimental Features"</A>" - an overview of experimental features"</li>
            <li><A href="/docs/idioms">"Idioms"</A>" - commonly useful, non-obvious idioms. Also "<A href="/docs/idioms#aliases">"aliases"</A>"."</li>
        </ul>

        <Hd id="other-pages">"Other Pages"</Hd>
        <ul>
            <li><A href="/docs/design">"Design"</A>" - reasons for some of "{lang}"'s design decisions"</li>
            <li><A href="/docs/rtl">"Right-to-Left"</A>" - the answer to the most-asked question about "{lang}"'s design gets its own page"</li>
            <li><A href="/docs/technical">"Technical Details"</A>" - notes on the implementation of the "{lang}" interpreter and this website"</li>
            <li><A href="/docs/combinators">"Combinators"</A>" - a list of common combinators implemented in "{lang}</li>
            <li><a href="https://tankorsmash.unison-services.cloud/s/uiuisms-service/">{
                match lang() {
                    "Weewuh" => "Weewisms",
                    _ => "Uiuisms",
                }
            }</a>" - a community catalog of many common "{lang}" snippets"</li>
            <li>
                <A href="/primitives.json" on:click = |_| _ = location().set_href("/primitives.json")>"Primitives JSON"</A>
                " - a JSON file of all the primitives, for tooling and other projects"</li>
        </ul>

        <Hd id="functions" class="doc-functions">"Functions"</Hd>
        <div id="function-search-wrapper">
            <div class="input-div">
                "⌕ "
                <input
                    id="function-search"
                    type="text"
                    value=search
                    on:input=on_search_input
                    pattern="[^0-9]"
                    placeholder="Search by name, glyph, or category..."/>
                { move || clear_button.get() }
            </div>
        </div>
        { move || results.get() }
        <div style="height: 85vh;"></div>
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
struct Allowed {
    classes: HashSet<PrimClass>,
    prims: HashSet<Primitive>,
}

fn aliases() -> HashMap<&'static str, &'static [Primitive]> {
    use Primitive::*;
    [
        ("filter", &[Keep] as &[_]),
        ("search", &[Find, Mask]),
        ("intersect", &[MemberOf]),
        ("split", &[Partition]),
        ("while", &[Do]),
        ("for", &[Repeat]),
        ("invert", &[Un]),
        ("encode", &[Bits, Base]),
        ("decode", &[Bits, Base]),
        ("prefixes", &[Tuples]),
        ("suffixes", &[Tuples]),
        ("flatten", &[Deshape]),
        ("choose", &[Tuples]),
        ("permute", &[Tuples]),
        ("window", &[Stencil]),
    ]
    .into_iter()
    .flat_map(|(alias, prims)| (3..=alias.len()).map(move |len| (&alias[..len], prims)))
    .collect()
}

thread_local! {
    static ALIASES: HashMap<&'static str, &'static [Primitive]> = aliases();
}

impl Allowed {
    fn all() -> Self {
        Self {
            classes: PrimClass::all().collect(),
            prims: Primitive::all().collect(),
        }
    }
    fn from_search(search: &str) -> Self {
        let search = search.trim().to_lowercase();
        let parts: Vec<_> = search
            .split([' ', ','])
            .filter(|&part| part.chars().any(|c| !c.is_ascii_digit()))
            .collect();
        if parts.is_empty() {
            return Self::all();
        }
        let mut prims = HashSet::new();
        let all = Primitive::all;
        let prim_matching_part_exactly = |part: &str| -> Option<Primitive> {
            Primitive::non_deprecated().find(|p| {
                p.name().to_lowercase() == part
                    || p.ascii().is_some_and(|a| a.to_string() == part)
                    || p.glyph().is_some_and(|u| part.chars().all(|c| c == u))
            })
        };
        for part in &parts {
            ALIASES.with(|aliases| {
                if let Some(prim) = aliases.get(part) {
                    prims.extend(prim.iter().copied());
                }
            });
        }
        if let Some(prim) = prim_matching_part_exactly(&search) {
            prims.insert(prim);
            return Self {
                classes: [prim.class()].into(),
                prims,
            };
        } else {
            for &part in &parts {
                if let Some(prim) = prim_matching_part_exactly(part) {
                    prims.insert(prim);
                    continue;
                }
                let matches = all()
                    .filter(|p| p.name().to_lowercase().starts_with(part))
                    .chain(all().filter(|p| {
                        p.ascii()
                            .is_some_and(|simple| part.contains(&simple.to_string()))
                    }))
                    .chain(
                        all().filter(|p| p.glyph().is_some_and(|unicode| part.contains(unicode))),
                    );
                prims.extend(matches);
            }
        }
        let mut classes: HashSet<PrimClass> = PrimClass::all().collect();
        let system_classes: Vec<PrimClass> = SysOpClass::all().map(PrimClass::Sys).collect();
        let mut function_classes: Vec<PrimClass> = system_classes.clone();
        function_classes.extend([
            PrimClass::Arguments,
            PrimClass::MonadicPervasive,
            PrimClass::DyadicPervasive,
            PrimClass::MonadicArray,
            PrimClass::DyadicArray,
            PrimClass::Misc,
        ]);
        'parts: for part in &parts {
            for (pattern, pat_classes) in [
                ("arguments", [PrimClass::Arguments].as_slice()),
                (
                    "pervasive pervade",
                    &[PrimClass::MonadicPervasive, PrimClass::DyadicPervasive],
                ),
                ("array", &[PrimClass::MonadicArray, PrimClass::DyadicArray]),
                (
                    "monadic",
                    &[PrimClass::MonadicPervasive, PrimClass::MonadicArray],
                ),
                (
                    "dyadic",
                    &[PrimClass::DyadicPervasive, PrimClass::DyadicArray],
                ),
                (
                    "modifier",
                    &[
                        PrimClass::AggregatingModifier,
                        PrimClass::IteratingModifier,
                        PrimClass::OtherModifier,
                    ],
                ),
                ("aggregating", &[PrimClass::AggregatingModifier]),
                ("iterating", &[PrimClass::IteratingModifier]),
                ("other", &[PrimClass::OtherModifier]),
                ("misc", &[PrimClass::Misc]),
                ("constant", &[PrimClass::Constant]),
                ("system", &system_classes),
                ("function", &function_classes),
                ("images", &[PrimClass::Sys(SysOpClass::Media)]),
                ("gifs", &[PrimClass::Sys(SysOpClass::Media)]),
                ("audio", &[PrimClass::Sys(SysOpClass::Media)]),
                ("tcp", &[PrimClass::Sys(SysOpClass::Tcp)]),
                ("env", &[PrimClass::Sys(SysOpClass::Env)]),
                ("command", &[PrimClass::Sys(SysOpClass::Command)]),
                ("filesystem", &[PrimClass::Sys(SysOpClass::Filesystem)]),
                ("stream", &[PrimClass::Sys(SysOpClass::Stream)]),
                ("stdio", &[PrimClass::Sys(SysOpClass::StdIO)]),
                ("thread", &[PrimClass::Thread]),
                ("map", &[PrimClass::Map]),
                ("encoding encode", &[PrimClass::Encoding]),
                ("ffi", &[PrimClass::Sys(SysOpClass::Ffi)]),
                ("misc", &[PrimClass::Sys(SysOpClass::Misc)]),
            ] {
                if pattern.split_whitespace().any(|pat| pat.starts_with(part)) {
                    classes.retain(|class| pat_classes.contains(class));
                    continue 'parts;
                }
            }
            classes.clear();
            break;
        }
        prims.extend(classes.iter().flat_map(|p| p.primitives()));
        classes.extend(prims.iter().map(|p| p.class()));
        if classes.is_empty() && !parts.is_empty() {
            return Self::default();
        }
        if prims.is_empty() {
            prims = Primitive::all().collect();
        }
        if classes.is_empty() {
            classes = PrimClass::all().collect();
        }
        Self { classes, prims }
    }
    fn table(&self) -> impl IntoView {
        let mut table_cells = Vec::new();
        for class in PrimClass::all() {
            if !self.classes.contains(&class) {
                continue;
            }
            let id = match class {
                PrimClass::Arguments => "arguments",
                PrimClass::Constant => "constant-functions",
                PrimClass::MonadicPervasive => "monadic-pervasive-functions",
                PrimClass::DyadicPervasive => "dyadic-pervasive-functions",
                PrimClass::MonadicArray => "monadic-array-functions",
                PrimClass::DyadicArray => "dyadic-array-functions",
                PrimClass::AggregatingModifier => "aggregating-modifiers",
                PrimClass::IteratingModifier => "iterating-modifiers",
                PrimClass::InversionModifier => "inversion-modifiers",
                PrimClass::Comptime => "comptime-modifiers",
                PrimClass::OtherModifier => "other-modifiers",
                PrimClass::Debug => "debug",
                PrimClass::Thread => "threads",
                PrimClass::Map => "map-functions",
                PrimClass::Encoding => "encoding",
                PrimClass::Algorithm => "algorithms",
                PrimClass::Rng => "rng",
                PrimClass::Time => "time",
                PrimClass::Environment => "environment",
                PrimClass::Misc => "misc-functions",
                PrimClass::Sys(_) => "system-functions",
            };
            let of_class: Vec<_> = Primitive::all()
                .filter(|p| self.prims.contains(p) && p.class() == class)
                .map(|p| {
                    let exp = if p.is_experimental() {
                        Some(view!(<span class="experimental-icon" data-title="Experimental!">"🧪"</span>))
                    } else {
                        None
                    };
                    let style = if p.is_deprecated() {
                        "text-decoration: line-through;"
                    } else {
                        ""
                    };
                    if let Primitive::Sys(sysop) = p {
                        view!(<div style="display: flex;">
                            <div style="min-width: 7em; display: flex; align-items: center;">
                                <div style=style><Prim prim=p/></div>{exp}
                            </div>
                            {sysop.long_name()}
                        </div>)
                        .into_view()
                    } else {
                        view!(<div style="display: flex; align-items: center;">
                            <div style=style><Prim prim=p/></div>{exp}
                        </div>)
                        .into_view()
                    }
                })
                .collect();
            if of_class.is_empty() {
                continue;
            }
            let (header, description) = match class {
                PrimClass::Arguments => ("Arguments".into_view(), "Manipulate function arguments"),
                PrimClass::Constant => ("Constants".into_view(), "Symbolic constants"),
                PrimClass::MonadicPervasive => (
                    "Monadic Pervasive".into_view(),
                    "Operate on every element in an array",
                ),
                PrimClass::DyadicPervasive => (
                    "Dyadic Pervasive".into_view(),
                    "Operate on every pair of elements in two arrays",
                ),
                PrimClass::MonadicArray => {
                    ("Monadic Array".into_view(), "Operate on a single array")
                }
                PrimClass::DyadicArray => ("Dyadic Array".into_view(), "Operate on two arrays"),
                PrimClass::IteratingModifier => (
                    "Iterating Modifiers".into_view(),
                    "Iterate and apply a function to an array or arrays",
                ),
                PrimClass::AggregatingModifier => (
                    "Aggregating Modifiers".into_view(),
                    "Apply a function to aggregate an array",
                ),
                PrimClass::InversionModifier => (
                    "Inversion Modifiers".into_view(),
                    "Work with the inverses of functions",
                ),
                PrimClass::Comptime => ("Comptime".into_view(), "Do things at compile time"),
                PrimClass::OtherModifier => ("Other Modifiers".into_view(), ""),
                PrimClass::Debug => ("Debug".into_view(), "Debug your code"),
                PrimClass::Thread => ("Thread".into_view(), "Work with OS threads"),
                PrimClass::Map => ("Map".into_view(), "Use arrays as hash maps"),
                PrimClass::Encoding => (
                    "Encoding".into_view(),
                    "Convert to and from different encodings",
                ),
                PrimClass::Algorithm => ("Algorithms".into_view(), "Useful, specific algorithms"),
                PrimClass::Rng => (
                    "Random Number Generation".into_view(),
                    "Generate random numbers",
                ),
                PrimClass::Time => ("Time".into_view(), "Work with time"),
                PrimClass::Environment => (
                    "Environment".into_view(),
                    "Get information about the environment",
                ),
                PrimClass::Misc => ("Miscellaneous".into_view(), ""),
                PrimClass::Sys(class) => match class {
                    SysOpClass::Filesystem => (
                        "System - Filesystem".into_view(),
                        "Work with files and directories",
                    ),
                    SysOpClass::StdIO => (
                        "System - Standard I/O".into_view(),
                        "Read and write standard input and output",
                    ),
                    SysOpClass::Env => {
                        ("System - Environment".into_view(), "Query the environment")
                    }
                    SysOpClass::Stream => (
                        "System - Streams".into_view(),
                        "Read from and write to streams",
                    ),
                    SysOpClass::Command => ("System - Commands".into_view(), "Execute commands"),
                    SysOpClass::Media => ("System - Media".into_view(), "Present media"),
                    SysOpClass::Tcp => ("System - TCP".into_view(), "Work with TCP sockets"),
                    SysOpClass::Udp => ("System - UDP".into_view(), "Work with UDP sockets"),
                    SysOpClass::Ffi => ("System - FFI".into_view(), "Foreign function interface"),
                    SysOpClass::Misc => ("System - Misc".into_view(), ""),
                },
            };
            table_cells.push(view! {
                <td id=id style="vertical-align: top;"><div>
                    <h3>{ header }</h3>
                    <p>{ description }</p>
                    <div class="primitive-list">{ of_class }</div>
                </div></td>
            });
        }

        let mut rows: Vec<_> = Vec::new();
        let mut class_iter = table_cells.into_iter();
        while let Some(first) = class_iter.next() {
            rows.push(view!( <tr>{once(first).chain(class_iter.next()).collect::<Vec<_>>()}</tr>));
        }
        view!( <table>{ rows }</table>)
    }
}
