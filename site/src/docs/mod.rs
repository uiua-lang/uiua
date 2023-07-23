mod primitive;

use std::{fmt::Display, iter::once};

use enum_iterator::{all, Sequence};
use leptos::*;
use leptos_router::*;
use uiua::primitive::{PrimClass, Primitive};

use crate::{code::*, editor::*};

pub use primitive::*;

#[component]
pub fn DocsHome(cx: Scope) -> impl IntoView {
    let primitives: Vec<_> = PrimClass::all()
        .map(|class| {
            let of_class: Vec<_> = Primitive::all()
                .filter(|p| p.class() == class && p.name().is_some())
                .map(|p| {
                    view! { cx, <PrimCode prim=p/> }
                })
                .collect();
            let (header, description) = match class {
                PrimClass::Stack => ("Stack", "Modify the stack"),
                PrimClass::MonadicPervasive => {
                    ("Monadic Pervasive", "Operate on every item in an array")
                }
                PrimClass::DyadicPervasive => (
                    "Dyadic Pervasive",
                    "Operate on every pair of items in two arrays",
                ),
                PrimClass::MonadicArray => ("Monadic Array", "Operate on a single array"),
                PrimClass::DyadicArray => ("Dyadic Array", "Operate on two arrays"),
                PrimClass::MonadicModifier => (
                    "Monadic Modifiers",
                    "Apply a function in some way to an array",
                ),
                PrimClass::DyadicModifier => (
                    "Dyadic Modifiers",
                    "Apply a function in some way to two arrays",
                ),
                PrimClass::OtherModifier => ("Other Modifiers", ""),
                PrimClass::Control => ("Control", "Control the flow of execution"),
                PrimClass::Misc => ("Miscellaneous", ""),
                PrimClass::Constant => ("Constants", "Push a constant value onto the stack"),
                PrimClass::Io => ("I/O", "Do input and output"),
            };
            view! { cx,
                <td style="vertical-align: top;"><div>
                    <h3>{ header }</h3>
                    <p>{ description }</p>
                    <div class="primitive-list">{ of_class }</div>
                </div></td>
            }
        })
        .collect();

    let mut rows: Vec<_> = Vec::new();
    let mut class_iter = primitives.into_iter();
    while let Some(first) = class_iter.next() {
        rows.push(view!(cx, <tr>{once(first).chain(class_iter.next()).collect::<Vec<_>>()}</tr>));
    }

    view! { cx,
        <h2>"Documentation"</h2>
        <h2>"Tutorial"</h2>
        <ul>
            <p>"These are meant to be read in order:"</p>
            { all::<TutorialPage>()
                .map(|p| view!(cx, <li><A href={p.path()}>{p.title()}" "{p.additional_title(cx)}</A></li>))
                .collect::<Vec<_>>()
            }
        </ul>
        <br/>
        <h2>"Functions"</h2>
        <table>{ rows }</table>
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Sequence)]
#[repr(u8)]
pub enum TutorialPage {
    Basic,
    Math,
    Arrays,
    Bindings,
    Functions,
}

impl TutorialPage {
    fn path(&self) -> String {
        format!("{self:?}").to_lowercase()
    }
    fn title(&self) -> &'static str {
        match self {
            Self::Basic => "Basic Stack Operations and Formatting",
            Self::Math => "Math and Comparison",
            Self::Arrays => "Arrays",
            Self::Bindings => "Bindings",
            Self::Functions => "Functions",
        }
    }
    fn additional_title(&self, cx: Scope) -> impl IntoView {
        use Primitive::*;
        match self {
            Self::Arrays => view!(cx,
                "("
                <PrimCode prim=Len/>
                <PrimCode prim=Rank/>
                <PrimCode prim=Shape/>
                <PrimCode prim=Fill/>
                ")")
            .into_view(cx),
            _ => view!(cx, {}).into_view(cx),
        }
    }
}

impl IntoParam for TutorialPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        all::<TutorialPage>()
            .find(|p| p.path() == value.unwrap_or(""))
            .ok_or_else(|| ParamsError::MissingParam(name.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct DocsParams {
    page: TutorialPage,
}

#[component]
pub fn DocsPage(cx: Scope) -> impl IntoView {
    move || {
        let Ok(params) = use_params::<DocsParams>(cx).get() else {
            return view! { cx, <Redirect path="/docs"/> }.into_view(cx);
        };
        let page = params.page;
        let page_view = match page {
            TutorialPage::Basic => view! { cx, <TutorialBasic/> }.into_view(cx),
            TutorialPage::Math => view! { cx, <TutorialMath/> }.into_view(cx),
            TutorialPage::Arrays => view! { cx, <TutorialArrays/> }.into_view(cx),
            TutorialPage::Bindings => view! { cx, <TutorialBindings/> }.into_view(cx),
            TutorialPage::Functions => view! { cx, <TutorialFunctions/> }.into_view(cx),
        };

        view! { cx,
            <div>
                <A href="/docs">"Back to Docs Home"</A>
                <br/>
                <br/>
                <TutorialNav page=page/>
                { page_view }
                <br/>
                <br/>
                <TutorialNav page=page/>
            </div>
        }
        .into_view(cx)
    }
}

#[component]
fn TutorialNav(cx: Scope, page: TutorialPage) -> impl IntoView {
    let next = move || {
        page.next()
            .map(|p| {
                view!(cx, <div><A href=format!("/docs/{}", p.path())>{p.title()}</A>" 〉"</div>)
                    .into_view(cx)
            })
            .unwrap_or_else(|| view!(cx, "").into_view(cx))
    };
    let previous = move || {
        page.previous()
            .map(|p| {
                view!(cx, <div>"〈 "<A href=format!("/docs/{}", p.path())>{p.title()}</A></div>)
                    .into_view(cx)
            })
            .unwrap_or_else(|| view!(cx, "").into_view(cx))
    };

    view! { cx,
        <div class="tutorial-nav">
            { previous }
            { next }
        </div>
    }
}

#[component]
fn TutorialBasic(cx: Scope) -> impl IntoView {
    let primitive_table: Vec<_> = Primitive::all()
        .filter_map(|p| {
            if let (Some(name), Some(ascii), Some(_)) = (p.name(), p.ascii(), p.unicode()) {
                Some(view! { cx,
                    <tr>
                        <td><code>{ name }</code></td>
                        <td><code>{ ascii.to_string() }</code></td>
                        <td><PrimCode prim=p glyph_only=true/></td>
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
                    <td><PrimCode prim=p glyph_only=true/></td>
                    <td>{view!(cx, <code>{p.args()}</code>)}</td>
                </tr>
            }
        })
        .collect()
}

#[component]
fn TutorialMath(cx: Scope) -> impl IntoView {
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
                <table class="bordered-table">
                    <tr>
                        <th>"Name"</th>
                        <th>"ASCII"</th>
                        <th>"Glyph"</th>
                        <th>"Args"</th>
                    </tr>
                    {math_table}
                </table>
                <table class="bordered-table">
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
fn TutorialArrays(cx: Scope) -> impl IntoView {
    use Primitive::*;
    view! { cx,
        <div>
            <h2>"Arrays"</h2>
            <p>"Uiua is, first and foremost, an array language. The only composite data type is the multimensional array. Arrays have a lot of nice properties, and the primitive oeprations of the language are designed to make it easy to work with them. If you've only ever programmed in non-array languages, then this will be a completely foreign paradigm. In most array languages, most data structures and control flow are replaced with operations on arrays."</p>
            <h2>"Creating Arrays"</h2>
            <p>"Other than with functions, Uiua has two ways to create arrays. They are called "<i>"strand notation"</i>" and "<i>"stack notation"</i>"."</p>
            <p><b>"Strand notation"</b>" uses underscores to connect elements:"</p>
            <Editor examples={&["1_2_3","\"Hello\"_\"World\"",  "0_π_2.3_5_∞"]}/>
            <p>"Strand notation is good when you want to create short and/or simple arrays. For longer or more complex arrays, you can use stack notation."</p>
            <p><b>"Stack notation"</b>" uses brackets to group elements:"</p>
            <Editor examples={&["[1 2 3]", "[¯5 37 42 π]"]}/>
            <p>"What's cool about stack notation is that it is "<i>"not"</i>" just a way to list elements. The code between the brackets runs from right to left as it normally would. When it is done, any items on the stack higher than when it started are put into the array. This gives you some cool ways to create arrays:"</p>
            <Editor examples={&["[...5]", "[×2.×2.×2.×2 .2]", "[+1 2 +3 4]"]}/>
            <p>"Of course, you can also use stack notation to make multidimensional arrays:"</p>
            <Editor examples={&["[[1 2 3] [4 5 6]]", "[...[1 2 3]]"]}/>
            <br/>
            <h2><PrimCode prim=Shape/>", "<PrimCode prim=Len/>", and "<PrimCode prim=Rank/></h2>
            <hr/>
            <p>"Other than their data, arrays also have a property called their "<b>"shape"</b>". Shape is a list of non-negative integers that describes the array's size along each of its axes."</p>
            <p>"We can get the array's shape with the "<PrimCode prim=Shape/>" primitive. It's a triangle because a triangle is a shape."</p>
            <Editor examples={&["△[1 2 3]", "△5", "△[[1 2 3] [4 5 6]]", "△[...[1 2 3]]"]}/>
            <p>"From shape we can derive two closely-related properties called "<b>"length"</b>" and "<b>"rank"</b>"."</p>
            <p><PrimCode prim=Len/>" is the number of "<i>"major cells"</i>" of the array. This is the number of elements for a 1D array and the number of rows for a 2D array. Length is always equal to the first number in the shape (or 1 if the shape is empty)."</p>
            <p><PrimCode prim=Rank/>" is the number of dimensions of the array. It is defined as the length of the shape."</p>
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
            <br/>
            <h2><PrimCode prim=Fill/>" and the Flat Array Model"</h2>
            <hr/>
            <p>"Most modern array languages allow arrays to contain arrays. The simplest model for this is the "<a href="https://aplwiki.com/wiki/Box">"Boxed Array Model"</a>" used in J, where an array may be an array of \"boxes\", which can contain any value but which must be boxed and unboxed. APL uses the "<a href="https://aplwiki.com/wiki/Array_model#Nested_array_theory">"Nested Array Model"</a>", making nested arrays a little easier to access. BQN uses the "<a href="https://mlochbaum.github.io/BQN/doc/based.html">"Based Array Model"</a>"."</p>
            <p>"Uiua does not use any of these. It sticks to the Flat Array Model of the original APL. In Uiua, "<b>"you cannot nest arrays inside other arrays"</b>"."</p>
            <p>"This may seem like a regression, but this model was picked for two reasons. For one, it is easier to make array algorithms fast when you only have to check their type once. More importantly than the implementation, however, is that the Flat Array Model makes array primitives easier to reason about. In Uiua, you never have to wonder whether a function like "<PrimCode prim=Windows/>" or "<PrimCode prim=Partition/>" returns an array or an array of arrays. The answer is always the same."</p>
            <p>"If you have worked with other array languages, you may be thinking, \"Sure, the Flat Array Model is simple, but it is very limited. How do you represent arrays with subarrays of different lengths?\" The answer is "<b>"fill values"</b>"."</p>
            <p>"Many operations that work on multiple arrays will return an error if the arrays have mismatched lengths or shapes. The most basic is stack notation, like in the error in the example above."</p>
            <p>"The "<PrimCode prim=Fill/>" function sets an array to use "<i>"fill values"</i>" to make array shapes match so that operations like this succeed:"</p>
            <Editor examples={&["[∘[1 2] [3 4 5]]", "[[1 2 3] ∘[4] ∘[5 6]]"]}/>
            <p>"Only one of the arrays needs to be fill-marked for this to work. The fill flag does not carry to result arrays."</p>
            <p>"Every array type is not fill by default, except for character arrays:"</p>
            <Editor examples={&["[\"Hello\" \"my\" \"friend\"]"]} help={&["Notice the lack of the fill function"]}/>
        </div>
    }
}

#[component]
fn TutorialBindings(cx: Scope) -> impl IntoView {
    view! { cx,
        <div>
            <h2>"Bindings"</h2>
            <p>"Bindings are global names that can be given to Uiua values. They are denoted with "<code>"←"</code>", which the formatter will convert from "<code>"="</code>" when appropriate."</p>
            <Editor examples={&["a = 3\nb = 5\n+ a b"]} help={&["", "Try running to format the ="]}/>
            <p>"Valid binding names can be made up of any sequence of uppercase or lowercase alphabetic characters OR a single non-alphanumeric character that is not already used for a Uiua function."</p>
            <p>"Unlike most programming languages, binding names in Uiua "<i>"cannot"</i>" contain numbers or underscores."</p>
            <Editor examples={&["numone ← 1\nNuMtWo ← 2\n😀 ← \"happy\"", "variable_1 ← 5"]}/>
            <p>"If the formatter would coerce a binding name to a Uiua function glyph, simply change its capitalization."</p>
            <Editor examples={&["part = 5", "Part ← 5\n×2 Part"]} help={&["", "Run to format and reveal why this does not work"]}/>
            <p>"If you start a binding with a captial letter or an unused glyph, it will bind the right side as a function."</p>
            <Editor examples={&["TimesThree ← ×3\nTimesThree 7", "👋 ← ⊂\"Hello, \"\n👋 \"World!\""]}/>
        </div>
    }
}

#[component]
fn TutorialFunctions(cx: Scope) -> impl IntoView {
    view! { cx,
        <div>
              <p>"TODO!"</p>
        </div>
    }
}
