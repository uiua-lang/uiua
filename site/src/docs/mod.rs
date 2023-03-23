use std::fmt::Display;

use leptos::*;
use leptos_router::*;
use uiua::ops::Primitive;

use crate::{code::*, editor::*};

#[component]
pub fn DocsHome(cx: Scope) -> impl IntoView {
    view! { cx,
        <h2>"Documentation"</h2>
        <h2>"Tutorial"</h2>
        <ul>
            <p>"These are meant to be read in order:"</p>
            <li><A href="basic">"Basic Stack Operations and Formatting"</A></li>
            <li><A href="math">"Math and Comparison"</A></li>
        </ul>
    }
}

#[component]
pub fn DocsBasic(cx: Scope) -> impl IntoView {
    let primitive_table: Vec<_> = Primitive::ALL
        .into_iter()
        .filter_map(|p| {
            if let (Some(name), Some(ascii), Some(_)) = (p.ident(), p.ascii(), p.unicode()) {
                Some(view! { cx,
                    <tr>
                        <td><code>{ name }</code></td>
                        <td><code>{ ascii.to_string() }</code></td>
                        <td><PrimCode prim=p/></td>
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
            <h3>"The Stack"</h3>
            <hr/>
            <p>"In Uiua, all operations operate on a global stack. Each line is evaluated from right to left. A number simply pushes its value onto the stack:"</p>
            <Editor examples={&["5", "1 2 3"]} help={&["", "Click the arrows to see more examples"]}/>
            <p>"Operators pop values off the stack and push their results."</p>
            <Editor examples={&["3", "2 3", "×2 3", "1 ×2 3", "+1 ×2 3"]}/>
            <br/>
            <h3>"Formatting"</h3>
            <hr/>
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
            <p>"As noted in the table, negative number literals are typed with the "<code>"`"</code>" character. This is because "<code>"-"</code>" is used for subtraction."</p>
            <Editor examples={&["+`1 `2"]}/>
        </div>
    }
}

fn maybe_code<T: Display>(cx: Scope, val: Option<T>) -> impl IntoView {
    if let Some(val) = val {
        view! { cx, <code>{ val.to_string() }</code> }.into_view(cx)
    } else {
        view! { cx, "" }.into_view(cx)
    }
}

fn primitive_rows(cx: Scope, prims: impl IntoIterator<Item = Primitive>) -> Vec<impl IntoView> {
    prims
        .into_iter()
        .map(|p| {
            let name = p.ident();
            let glyph = p.unicode();
            let ascii = p
                .ascii()
                .map(|s| s.to_string())
                .or_else(|| glyph.filter(|c| c.is_ascii()).map(|c| c.to_string()));
            let args = p.args();
            view! { cx,
                <tr>
                    <td>{maybe_code(cx, name)}</td>
                    <td>{maybe_code(cx, ascii)}</td>
                    <td><PrimCode prim=p/></td>
                    <td>{maybe_code(cx, args)}</td>
                </tr>
            }
        })
        .collect()
}

#[component]
pub fn DocsMath(cx: Scope) -> impl IntoView {
    use Primitive::*;
    let math_table = primitive_rows(
        cx,
        [
            Add, Sub, Mul, Div, Mod, Pow, Neg, Abs, Ceil, Floor, Round, Sqrt, Sign,
        ],
    );
    let comp_table = primitive_rows(cx, [Eq, Ne, Lt, Gt, Le, Ge, Min, Max, Floor, Ceil, Round]);

    view! { cx,
        <div>
            <h2>"Math and Comparison"</h2>
            <p>"Uiua supports all the basic math operations:"</p>
            <table>
                <tr>
                    <th>"Name"</th>
                    <th>"ASCII"</th>
                    <th>"Glyph"</th>
                    <th>"Arguments"</th>
                </tr>
                {math_table}
            </table>
            <p>"Uiua also supports comparison, min/max, and rounding operations:"</p>
            <table>
                <tr>
                    <th>"Name"</th>
                    <th>"ASCII"</th>
                    <th>"Glyph"</th>
                    <th>"Arguments"</th>
                </tr>
                {comp_table}
            </table>
            <p>"Most of these are used mostly how you might think:"</p>
            <Editor examples={&["+2 5", "↥2 5", "ⁿ2 5", "⌈2.5", "√4"]}/>
            <p>"One thing to note is that non-commutative operators work backwards:"</p>
            <Editor examples={&["-2 5", "<2 5", "÷2 5"]}/>
            <p>"Uiua has no boolean types. Comparison operators return "<code>0</code>" for false and "<code>1</code>" for true:"</p>
            <Editor examples={&["=2 5", "=2 2"]}/>
            <p>"Because of how stack operations work, you can delay operations until after all the arguments are on the stack:"</p>
            <Editor examples={&["++1 2 3", "×××2 3 4 5"]}/>
            <p>"This is not special syntax. All the numbers are pushed to the stack, then the operators work on them."</p>
            <p>"Remember that you can type the names of operators and then run to format them:"</p>
            <Editor examples={&["# Click Run to format!\nmax sqrt 2 mod 10 abs `31"]}/>
        </div>
    }
}
