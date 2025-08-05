use leptos::*;
use leptos_meta::*;
use leptos_router::A;
use uiua::{PrimComponent, Primitive};
use uiua_editor::{lang, Editor};

use crate::{Hd, Prim};

fn idioms() -> impl IntoView {
    let src = include_str!("../text/idioms.ua");
    let mut idioms = Vec::new();
    for text in src.split("\n\n").flat_map(|s| s.split("\r\n\r\n")) {
        let mut comment = String::new();
        let mut code = String::new();
        let mut lines = text.split('\n').filter(|line| !line.is_empty());
        for (i, line) in lines.by_ref().enumerate() {
            if line.starts_with("# ") {
                if i > 0 {
                    comment.push('\n');
                }
                comment.push_str(line.trim_start_matches("# "));
            } else {
                code.push_str(line);
                for line in lines {
                    code.push('\n');
                    code.push_str(line);
                }
                break;
            }
        }
        idioms.push(view!(<tr>
            <td style="text-wrap: wrap">{comment}</td>
            <td><Editor example={&code}/></td>
        </tr>));
    }
    idioms
}

fn aliases() -> impl IntoView {
    Primitive::non_deprecated()
        .filter(|p| !p.aliases().is_empty())
        .map(|p| {
            view!(<tr><td><Prim prim=p/></td><td>{ 
                (p.aliases().iter())
                    .map(|&s| view!(<code>{s}</code>" "))
                    .collect::<Vec<_>>() 
            }{
                (p.aliases().iter().any(|a| a.contains('&')) || matches!(p, Primitive::Sys(_)))
                    .then(|| view!(<span style="opacity: 0.7">"Temporary"</span>))
            }</td></tr>)
        })
        .collect::<Vec<_>>()
}

fn multi_aliases() -> impl IntoView {
    Primitive::multi_aliases()
        .iter()
        .map(|(name, prims)| {
            view!(<tr>
                <td><code>{*name}</code></td>
                <td>{
                    prims
                        .iter()
                        .map(|(p, _)| match p {
                            PrimComponent::Prim(p) => view!(<Prim prim=*p/>" ").into_view(),
                            p => view!(<code>{p.name()}</code>).into_view(),
                        })
                        .collect::<Vec<_>>()
                }</td>
            </tr>)
        })
        .collect::<Vec<_>>()
}

#[component]
pub fn Idioms() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Idioms - {} Docs", lang())/>
        <h1>"Idioms"</h1>
        <p>"This page contains short "{lang}" idioms that may be non-obvious from simply knowing the primitives themselves."</p>
        <p>"There are also lists of "<A href="#aliases">"aliases"</A>" below."</p>
        <p>"You can contribute to this list by submitting a PR to the repo. The list is defined "<a href="https://github.com/uiua-lang/uiua/blob/main/site/text/idioms.ua">"here"</a>". Keep in mind these are meant to be both short and useful. Idioms above 10 characters in length (without inputs), or which are not useful for everyday tasks, will not be accepted."</p>
        <table class="bordered-table" style="width: 100%; table-layout: fixed">
            <colgroup>
                <col style="width: 50%"/>
                <col style="width: max(50%, 10em)"/>
            </colgroup>
            { idioms() }
        </table>

        <div class="wider">
            <div>
                <Hd id="aliases">"Aliases"</Hd>
                <p>"Some primitives have aliases that are not prefixes of the primitive's name, not unique, or less than 3 characters long. These aliases are listed in the table below."</p>
                <table class="bordered-table" style="width: 100%">
                    <tr><th>"Primitive"</th><th>"Aliases"</th></tr>
                    { aliases() }
                </table>
            </div>
            <div>
                <Hd id="multi-aliases">"Multi-aliases"</Hd>
                <p>"Some words serve as an alias for a sequence of primitives. Each primitive is typically represented by a single character in the word."</p>
                <Editor example="dor 10 # Try formatting!"/>
                <p>"These words are listed in the table below."</p>
                <table class="bordered-table" style="width: 100%">
                    <tr><th>"Word"</th><th>"Primitives"</th></tr>
                    { multi_aliases() }
                </table>

                <Hd id="planet-aliases">"Planet Aliases"</Hd>
                <p>"As discussed in the "<A href="/tutorial/morestack#planet-notation">"Planet Notation"</A>" tutorial, sequences of some primitives can be represented by sequences of corresponding characters. These compose arbitrarily long."</p>
                <p>"The rules are:"</p>
                <table class="bordered-table all-centered-table" style="width: 100%">
                    <tr><th>"Where"</th><th>"Characters"</th><th>"Formats to"</th></tr>
                    <tr><td>"Inner Sequence"</td><td><code>"d"</code>" / "<code>"g"</code></td><td><Prim prim=Dip/>" / "<Prim prim=Gap/></td></tr>
                    <tr><td>"Ending"</td><td><code>"i"</code>" / "<code>"p"</code>" / "<code>"f"</code></td><td><Prim prim=Identity/>" / "<Prim prim=Pop/>" / "<Prim prim=Fix/></td></tr>
                    <tr><td>"Beginning"</td><td><code>"f"</code></td><td><Prim prim=Fork/></td></tr>
                </table>
                <p>"Some examples:"</p>
                <Editor example="[dgdi] 1 2 3 4 # Try formatting!"/>
                <Editor example="[fggiddp] 1 2 3"/>
                <Editor example="{ddf} 1 2 3"/>
            </div>
        </div>
    }
}
