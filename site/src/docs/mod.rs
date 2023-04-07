use std::fmt::Display;

use leptos::*;
use leptos_router::*;
use uiua::primitive::Primitive;

use crate::{code::*, editor::*};

#[component]
pub fn DocsHome(cx: Scope) -> impl IntoView {
    view! { cx,
        <h2>"Documentation"</h2>
        <h2>"Tutorial"</h2>
        <ul>
            <p>"These are meant to be read in order:"</p>
            {TutorialPage::ALL.iter().map(|p| view!(cx, <li><A href={p.path()}>{p.title()}</A></li>)).collect::<Vec<_>>()}
        </ul>
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TutorialPage {
    Basic,
    Math,
    Arrays,
}

impl TutorialPage {
    const ALL: [Self; 3] = [Self::Basic, Self::Math, Self::Arrays];
    fn path(&self) -> String {
        format!("{self:?}").to_lowercase()
    }
    fn title(&self) -> &'static str {
        match self {
            Self::Basic => "Basic Stack Operations and Formatting",
            Self::Math => "Math and Comparison",
            Self::Arrays => "Arrays",
        }
    }
}

impl IntoParam for TutorialPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        TutorialPage::ALL
            .iter()
            .find(|p| p.path() == value.unwrap_or(""))
            .copied()
            .ok_or_else(|| ParamsError::MissingParam(name.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct DocsParams {
    page: TutorialPage,
}

#[component]
pub fn DocsPage(cx: Scope) -> impl IntoView {
    let page = match use_params::<DocsParams>(cx).get() {
        Ok(page) => page.page,
        Err(_) => return view! { cx, <DocsHome/> }.into_view(cx),
    };
    let page_view = move || match page {
        TutorialPage::Basic => view! { cx, <TutorialBasic/> }.into_view(cx),
        TutorialPage::Math => view! { cx, <TutorialMath/> }.into_view(cx),
        TutorialPage::Arrays => view! { cx, <TutorialArrays/> }.into_view(cx),
    };

    view! { cx,
        <div>
            <A href="/docs">"Back to Docs Home"</A>
            { page_view }
            <div id="bottom-page-nav">
                <A href="/docs">"Back to Docs Home"</A>
            </div>
        </div>
    }
    .into_view(cx)
}

#[component]
pub fn TutorialBasic(cx: Scope) -> impl IntoView {
    let primitive_table: Vec<_> = Primitive::ALL
        .into_iter()
        .filter_map(|p| {
            if let (Some(name), Some(ascii), Some(_)) = (p.name(), p.ascii(), p.unicode()) {
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
            <h2>"The Stack"</h2>
            <hr/>
            <p>"In Uiua, all operations operate on a global stack. Each line is evaluated from right to left. A number simply pushes its value onto the stack:"</p>
            <Editor examples={&["5", "1 2 3"]} help={&["", "Click the arrows to see more examples"]}/>
            <p>"Operators pop values off the stack and push their results."</p>
            <Editor examples={&["3", "2", "×", "1", "+"]} progressive=true help={&["", "Really, try the arrows"]}/>
            <p>"In the editor, items that end up on the "<i>"top"</i>" of the stack are shown at the "<i>"bottom"</i>" of the output. This is so consecutive lines of code show their outputs in the correct order:"</p>
            <Editor examples={&["5\n+1 2\n\"Hello, World!\"\n+1 'a'"]}/>
            <br/>
            <h2>"Formatting"</h2>
            <hr/>
            <p>"Most Uiua primitives use special unicode characters. To type multiplication and division signs, you can use "<code>"*"</code>" and "<code>"%"</code>" respectively. Then, run the code to format."</p>
            <Editor examples={&["# Click Run to format!\n%6 *3 8"]} help={&["", "⇡Click⇡"]}/>
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
            let name = p.name();
            let glyph = p.unicode();
            let ascii = p
                .ascii()
                .map(|s| s.to_string())
                .or_else(|| glyph.filter(|c| c.is_ascii()).map(|c| c.to_string()));
            view! { cx,
                <tr>
                    <td>{maybe_code(cx, name)}</td>
                    <td>{maybe_code(cx, ascii)}</td>
                    <td><PrimCode prim=p/></td>
                    <td>{view!(cx, <code>{p.args()}</code>)}</td>
                </tr>
            }
        })
        .collect()
}

#[component]
pub fn TutorialMath(cx: Scope) -> impl IntoView {
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
            <p>"Uiua supports all the basic math operations as well as comparison, min/max, and rounding:"</p>
            <div style="display: flex; justify-content: space-evenly;">
                <table>
                    <tr>
                        <th>"Name"</th>
                        <th>"ASCII"</th>
                        <th>"Glyph"</th>
                        <th>"Args"</th>
                    </tr>
                    {math_table}
                </table>
                <table>
                    <tr>
                        <th>"Name"</th>
                        <th>"ASCII"</th>
                        <th>"Glyph"</th>
                        <th>"Args"</th>
                    </tr>
                    {comp_table}
                </table>
            </div>
            <p>"Most of these are used mostly how you might think:"</p>
            <Editor examples={&["+2 5", "↥2 5", "ⁿ2 5", "⌈2.5", "√4"]}/>
            <p>"One thing to note is that non-commutative operators work backwards:"</p>
            <Editor examples={&["-2 5", "<2 5", "÷2 5"]}/>
            <p>"Uiua has no boolean types. Comparison operators return "<code>0</code>" for false and "<code>1</code>" for true:"</p>
            <Editor examples={&["=2 5", "=2 2"]}/>
            <p>"Because of how stack operations work, you can delay operations until after all the arguments are on the stack:"</p>
            <Editor examples={&["4", "3", "2", "1", "+", "+",  "×"]} progressive=true/>
            <p>"This is not special syntax. All the numbers are pushed to the stack, then the operators work on them."</p>
            <p>"Remember that you can type the names of operators and then run to format them:"</p>
            <Editor examples={&["# Click Run to format!\nmax sqrt 2 mod 10 abs `31"]} help={&["", "⇡Click⇡"]}/>
        </div>
    }
}

#[component]
pub fn TutorialArrays(cx: Scope) -> impl IntoView {
    use Primitive::*;
    view! { cx,
        <div>
            <h2>"Arrays"</h2>
            <p>"Uiua is, first and foremost, an array language. The only composite data type is the multimensional array. Arrays have a lot of nice properties, and the primitive oeprations of the language are designed to make it easy to work with them. If you've only ever programmed in non-array languages, then this will be a completely foreign paradigm. In most array languages, most data structures and control flow are replaced with operations on arrays."</p>
            <h2>"Creating Arrays"</h2>
            <p>"Other than with functions, Uiua has two ways to create arrays. They are called "<i>"strand notation"</i>" and "<i>"stack notation"</i>"."</p>
            <p><b>"Strand notation"</b>" uses underscores to connect elements:"</p>
            <Editor examples={&["1_2_3","\"Hello\"_\"World\"",  "0_π_⍉_5_(+1)"]}/>
            <p>"Strand notation is good when you want to create short and/or simple arrays. For longer or more complex arrays, you can use stack notation."</p>
            <p><b>"Stack notation"</b>" uses brackets to group elements:"</p>
            <Editor examples={&["[1 2 3]", "[¯5 'a' 0 (+)]"]}/>
            <p>"What's cool about stack notation is that it is "<i>"not"</i>" just a way to list elements. The code between the brackets runs from right to left as it normally would. When it is done, any items on the stack higher than when it started are put into the array. This gives you some cool ways to create arrays:"</p>
            <Editor examples={&["[...5]", "[×2.×2.×2.×2 .2]", "[+1 2 +3 4]"]}/>
            <p>"Of course, you can also use stack notation to make multidimensional arrays:"</p>
            <Editor examples={&["[[1 2 3] [4 5 6]]", "[...[1 2 3]]"]}/>
            <br/>
            <h2><PrimCode prim=Shape name=true/>", "<PrimCode prim=Len name=true/>", and "<PrimCode prim=Rank name=true/></h2>
            <hr/>
            <p>"Other than their data, arrays also have a property called their "<b>"shape"</b>". Shape is a list of non-negative integers that describes the array's size along each of its axes."</p>
            <p>"We can get the array's shape with the "<PrimCode prim=Shape name=true/>" primitive. It's a triangle because a triangle is a shape."</p>
            <Editor examples={&["△[1 2 3]", "△5", "△[[1 2 3] [4 5 6]]", "△[...[1 2 3]]"]}/>
            <p>"From shape we can derive two closely-related properties called "<b>"length"</b>" and "<b>"rank"</b>"."</p>
            <p><PrimCode prim=Len name=true/>" is the number of "<i>"major cells"</i>" of the array. This is the number of elements for a 1D array and the number of rows for a 2D array. Length is always equal to the first number in the shape (or 1 if the shape is empty)."</p>
            <p><PrimCode prim=Rank name=true/>" is the number of dimensions of the array. It is defined as the length of the shape."</p>
            <Editor examples={&["△[1 2 3]\n≢[1 2 3]\n∴[1 2 3]", "# ∴ is equivalent to ≢△\n=∴[1 2 3]≢△[1 2 3]"]}/>
            <p>"When creating multidimensional arrays, stack notation applies a step called "<i>"normalization"</i>". If all the items pushed to the stack have the same shape, they will combine into an array with a higher rank. Different shapes result in an error."</p>
            <Editor examples={&["[[1 2] [3 4]]", "[[1 2] [3 4 5]]"]}/>
            <br/>
            <h2>"Pervasion"</h2>
            <hr/>
            <p>"Most operations that apply to scalars are what is called "<i>"pervasive"</i>" when it comes to arrays. This means that the operations automatically applies to every item in the array."</p>
            <Editor examples={&["+1 1_2_3\n√[4 9 16]\n+1_2_3 4_5_6"]}/>
            <p>"When doing a pervasive operation on two arrays, their shape "<i>"prefixes"</i>" must match."</p>
            <Editor examples={&["+[1 2] [3 4 5]", "△10_20\n△[3_4_5 6_7_8]\n+10_20 [3_4_5 6_7_8]"]}/>
        </div>
    }
}
