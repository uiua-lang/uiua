use leptos::*;
use leptos_router::*;
use uiua::ops::Primitive;

use crate::editor::*;

#[component]
pub fn DocsHome(cx: Scope) -> impl IntoView {
    view! { cx,
        <h2>"Documentation"</h2>
        <h3>"Getting Started"</h3>
        <ul>
            <li><A href="basic">"Basic Stack Operations and Formatting"</A></li>
        </ul>
    }
}

#[component]
pub fn DocsBasic(cx: Scope) -> impl IntoView {
    let primitive_table: Vec<_> = Primitive::ALL
        .into_iter()
        .filter_map(|p| {
            if let (Some(name), Some(ascii), Some(glyph)) = (p.ident(), p.ascii(), p.unicode()) {
                Some(view! { cx,
                    <tr>
                        <td><code>{ name }</code></td>
                        <td><code>{ ascii.to_string() }</code></td>
                        <td><code>{ glyph }</code></td>
                    </tr>
                })
            } else {
                None
            }
        })
        .collect();

    view! { cx,
        <div>
            <h2>"Basic Stack Operations and Formatting"</h2>
            <p>"In Uiua, all operations operate on a global stack. Each line is evaluated from right to left. A number simply pushes its value onto the stack:"</p>
            <Editor examples={&["5", "1 2 3"]}/>
            <p>"Operators pop values off the stack and push their results."</p>
            <Editor examples={&["+1 2", "+1 ×2 3"]}/>
            <p>"Most Uiua primitives use special unicode characters. To type multiplication and division signs, you can use "<code>"*"</code>" and "<code>"%"</code>" respectively. Then, run the code to format."</p>
            <Editor examples={&["# Click Run to format!\n%6 *3 8"]}/>
            <p>"Most primitives have names you can type rather than symbols. Formatting works on these too. "<b>"This is the primary way of entering Uiua's glyphs."</b></p>
            <Editor examples={&["max sqrt 10 mod 10 pow 2 8", "* `1 `2"]}/>
            <p>"You don't have to type the whole name, just enough to disambiguate it from others"</p>
            <Editor examples={&["(cei ceil ceili ceilin ceiling)"]}/>
            <p>"On this site, you can also click the ↧ symbol on any editor to show a pallete of all the Uiua glyphs. You can then click on any glyph to insert it into the editor."</p>
            <p>"Here is a table of all the glyphs that are typed with ASCII characters that get converted to glyphs:"</p>
            <table>
                <tr>
                    <th>"Name"</th>
                    <th>"ASCII"</th>
                    <th>"Glyph"</th>
                </tr>
                {primitive_table}
                <tr>
                    <td>"negative number"</td>
                    <td><code>"`"</code></td>
                    <td><code>"¯"</code></td>
                </tr>
            </table>
        </div>
    }
}
