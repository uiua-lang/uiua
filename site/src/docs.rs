use std::{
    collections::{HashMap, HashSet},
    iter::once,
    time::Duration,
};

use enum_iterator::all;
use leptos::{prelude::*, *};
use leptos_meta::*;
use leptos_router::{components::*, hooks::*, params::*};
use uiua::{PrimClass, Primitive, SysOpClass};
use uiua_editor::lang;
use wasm_bindgen::JsCast;
use web_sys::{Event, EventInit, HtmlInputElement, ScrollBehavior, ScrollIntoViewOptions};

use crate::{
    Hd, Prim, Tour, element, markdown::Markdown, other::*, other_tutorial::OtherTutorialPage,
    primitive::*, tutorial::TUTORIAL_NAMES, uiuisms::Uiuisms,
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
    Constants,
    Combinators,
    Subscripts,
    Optimizations,
    FormatConfig,
    Experimental,
    Idioms,
    Aliases,
}

impl ParamToString for DocsPage {
    fn param_to_string(&self, _: &str) -> Option<String> {
        Some(match self {
            Self::Search(search) => search.clone(),
            Self::Uiuisms => "isms".into(),
            Self::FormatConfig => "format-config".into(),
            page => format!("{page:?}").to_lowercase(),
        })
    }
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
            "constants" => Ok(Self::Constants),
            "combinators" => Ok(Self::Combinators),
            "subscripts" => Ok(Self::Subscripts),
            "optimizations" => Ok(Self::Optimizations),
            "format-config" => Ok(Self::FormatConfig),
            "experimental" => Ok(Self::Experimental),
            "idioms" => Ok(Self::Idioms),
            "aliases" => Ok(Self::Aliases),
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
            return view!( <DocsHome/>).into_any();
        };
        let page = params.page;
        let page_view = match page {
            DocsPage::Search(search) => return view!( <DocsHome search=search/>).into_any(),
            DocsPage::Tour => Tour().into_any(),
            DocsPage::Design => {
                title_markdown("Design", "/text/design.md", ().into_any()).into_any()
            }
            DocsPage::Technical => Technical().into_any(),
            DocsPage::Install => Install().into_any(),
            DocsPage::Uiuisms => Uiuisms().into_any(),
            DocsPage::Changelog => Changelog().into_any(),
            DocsPage::Constants => Constants().into_any(),
            DocsPage::Subscripts => Subscripts().into_any(),
            DocsPage::Combinators => Combinators().into_any(),
            DocsPage::Optimizations => Optimizations().into_any(),
            DocsPage::FormatConfig => title_markdown(
                "Formatter Configuration",
                "/text/format_config.md",
                ().into_any(),
            )
            .into_any(),
            DocsPage::Experimental => Experimental().into_any(),
            DocsPage::Idioms => Idioms().into_any(),
            DocsPage::Aliases => Aliases().into_any(),
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
        .into_any()
    }
}

pub fn title_markdown(title: &str, src: &str, end: AnyView) -> impl IntoView {
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
    let (results, set_result) = signal(None::<Allowed>);
    let (current_prim, set_current_prim) = signal(None);
    let (clear_button, set_clear_button) = signal(false);
    let (old_allowed, set_old_allowed) = signal(Allowed::all());
    let update_search = move |text: &str, update_location: bool| {
        // Update clear button
        set_clear_button.set(!text.is_empty());

        // Derive allowed primitives
        let allowed = Allowed::from_search(text);
        if !text.is_empty() {
            let siv_options = ScrollIntoViewOptions::new();
            siv_options.set_behavior(ScrollBehavior::Smooth);
            scroll_to_docs_functions(&siv_options);
        }
        if update_location {
            let text = text.to_string();
            _ = set_timeout(
                move || {
                    let search_input = element::<HtmlInputElement>("function-search").value();
                    if search_input == text {
                        _ = window().history().unwrap().push_state_with_url(
                            &wasm_bindgen::JsValue::NULL,
                            "",
                            Some(&format!("/docs/{}", urlencoding::encode(&text))),
                        );
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
            set_result.set(Some(allowed));
            set_current_prim.set(None);
        } else if allowed.prims.len() == 1
            && [PrimClass::all().count(), 1].contains(&allowed.classes.len())
        {
            // Only one result
            let prim = *allowed.prims.iter().next().unwrap();
            let siv_options = ScrollIntoViewOptions::new();
            siv_options.set_behavior(ScrollBehavior::Instant);
            scroll_to_docs_functions(&siv_options);
            set_result.set(Some(allowed));
            set_current_prim.set(Some(prim));
        } else {
            // Multiple results
            set_result.set(Some(allowed));
            set_current_prim.set(None);
        }
    };
    let update_title = move || {
        current_prim
            .get()
            .map(|p| format!("{} - {} Docs", lang(), p.name()))
            .unwrap_or_else(|| format!("{} Docs", lang()))
    };

    _ = set_timeout(
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
            <li><A href="/docs/idioms">"Idioms"</A>" - commonly useful, non-obvious idioms"</li>
            <li><A href="/docs/aliases">"Aliases"</A>" - patterns that get converted by the formatter"</li>
        </ul>

        <Hd id="other-pages">"Other Pages"</Hd>
        <ul>
            <li><A href="/docs/design">"Design"</A>" - reasons for some of "{lang}"'s design decisions"</li>
            <li><A href="/docs/technical">"Technical Details"</A>" - notes on the implementation of the "{lang}" interpreter and this website"</li>
            <li><A href="/docs/combinators">"Combinators"</A>" - a list of common combinators implemented in "{lang}</li>
            <li><a href="https://tankorsmash.unison-services.cloud/s/uiuisms-service/">{
                match lang() {
                    "Weewuh" => "Weewisms",
                    _ => "Uiuisms",
                }
            }</a>" - a community catalog of many common "{lang}" snippets"</li>
            <li>
                <a href="/primitives.json" target="_self">"Primitives JSON"</a>
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
                { move || clear_button.get().then(|| view! {
                    <button on:click=move |_| {
                        let search_input = element::<HtmlInputElement>("function-search");
                        search_input.set_value("");
                        let init = EventInit::new();
                        init.set_bubbles(true);
                        _ = search_input.dispatch_event(&Event::new_with_event_init_dict("input", &init).unwrap());
                    }>"✕"</button>
                }) }
            </div>
        </div>
        { move || results.get().map(|allowed| {
            if allowed.classes.is_empty() && allowed.prims.is_empty() {
                view!(<p>"No results"</p>).into_any()
            } else if allowed.prims.len() == 1
                && [PrimClass::all().count(), 1].contains(&allowed.classes.len())
            {
                view!(<PrimDocs prim=*allowed.prims.iter().next().unwrap()/>).into_any()
            } else {
                allowed.table().into_any()
            }
        }) }
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
                        PrimClass::MappingModifier,
                        PrimClass::IteratingModifier,
                        PrimClass::OtherModifier,
                    ],
                ),
                ("mapping", &[PrimClass::MappingModifier]),
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
                PrimClass::GeometricAlgebra => "geometric-algebra",
                PrimClass::DyadicPervasive => "dyadic-pervasive-functions",
                PrimClass::MonadicArray => "monadic-array-functions",
                PrimClass::DyadicArray => "dyadic-array-functions",
                PrimClass::MappingModifier => "mapping-modifiers",
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
                        .into_any()
                    } else {
                        view!(<div style="display: flex; align-items: center;">
                            <div style=style><Prim prim=p/></div>{exp}
                        </div>)
                        .into_any()
                    }
                })
                .collect();
            if of_class.is_empty() {
                continue;
            }
            let (header, description) = match class {
                PrimClass::Arguments => ("Arguments".into_any(), "Manipulate function arguments"),
                PrimClass::Constant => ("Constants".into_any(), "Symbolic constants"),
                PrimClass::MonadicPervasive => (
                    "Monadic Pervasive".into_any(),
                    "Operate on every element in an array",
                ),
                PrimClass::DyadicPervasive => (
                    "Dyadic Pervasive".into_any(),
                    "Operate on every pair of elements in two arrays",
                ),
                PrimClass::MonadicArray => {
                    ("Monadic Array".into_any(), "Operate on a single array")
                }
                PrimClass::DyadicArray => ("Dyadic Array".into_any(), "Operate on two arrays"),
                PrimClass::MappingModifier => (
                    "Mapping Modifiers".into_any(),
                    "Apply a function to separate rows of an array or arrays",
                ),
                PrimClass::IteratingModifier => (
                    "Iterating Modifiers".into_any(),
                    "Apply a function to iteratively transform an array or arrays",
                ),
                PrimClass::InversionModifier => (
                    "Inversion Modifiers".into_any(),
                    "Work with the inverses of functions",
                ),
                PrimClass::Comptime => ("Comptime".into_any(), "Do things at compile time"),
                PrimClass::OtherModifier => ("Other Modifiers".into_any(), ""),
                PrimClass::Debug => ("Debug".into_any(), "Debug your code"),
                PrimClass::Thread => ("Thread".into_any(), "Work with OS threads"),
                PrimClass::GeometricAlgebra => {
                    ("Geometric Algebra".into_any(), "Operate on multivectors")
                }
                PrimClass::Map => ("Map".into_any(), "Use arrays as hash maps"),
                PrimClass::Encoding => (
                    "Encoding".into_any(),
                    "Convert to and from different encodings",
                ),
                PrimClass::Algorithm => ("Algorithms".into_any(), "Useful, specific algorithms"),
                PrimClass::Rng => (
                    "Random Number Generation".into_any(),
                    "Generate random numbers",
                ),
                PrimClass::Time => ("Time".into_any(), "Work with time"),
                PrimClass::Environment => (
                    "Environment".into_any(),
                    "Get information about the environment",
                ),
                PrimClass::Misc => ("Miscellaneous".into_any(), ""),
                PrimClass::Sys(class) => match class {
                    SysOpClass::Filesystem => (
                        "System - Filesystem".into_any(),
                        "Work with files and directories",
                    ),
                    SysOpClass::StdIO => (
                        "System - Standard I/O".into_any(),
                        "Read and write standard input and output",
                    ),
                    SysOpClass::Env => ("System - Environment".into_any(), "Query the environment"),
                    SysOpClass::Stream => (
                        "System - Streams".into_any(),
                        "Read from and write to streams",
                    ),
                    SysOpClass::Command => ("System - Commands".into_any(), "Execute commands"),
                    SysOpClass::Media => ("System - Media".into_any(), "Present media"),
                    SysOpClass::Tcp => ("System - TCP".into_any(), "Work with TCP sockets"),
                    SysOpClass::Udp => ("System - UDP".into_any(), "Work with UDP sockets"),
                    SysOpClass::Ffi => ("System - FFI".into_any(), "Foreign function interface"),
                    SysOpClass::Misc => ("System - Misc".into_any(), ""),
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
