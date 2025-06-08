use std::fmt::Display;

use enum_iterator::{all, Sequence};
use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use uiua::{Primitive, SysOp, EXAMPLE_UA};
use uiua_editor::*;

use crate::{
    markdown::Markdown, other_tutorial::OtherTutorialParams, title_markdown, Challenge, Hd, Hd3,
    Prim, Prims,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Sequence)]
pub enum TutorialPage {
    Introduction,
    Basic,
    Math,
    Arrays,
    Bindings,
    Functions,
    MoreStack,
    Types,
    Inverses,
    ControlFlow,
    PatternMatching,
    MoreArray,
    ThinkingWithArrays,
    Macros,
    TacitCode,
    Modules,
    DataDefs,
    Testing,
}

impl TutorialPage {
    pub fn path(&self) -> String {
        format!("{self:?}").to_lowercase()
    }
    pub fn title(&self) -> &'static str {
        match self {
            Self::Introduction => "Introduction",
            Self::Basic => "Basic Stack Operations and Formatting",
            Self::Math => "Math and Comparison",
            Self::Arrays => "Arrays",
            Self::Types => "Types",
            Self::Bindings => "Bindings",
            Self::Functions => "Modifiers and Functions",
            Self::MoreStack => "More Stack Manipulation",
            Self::Inverses => "Inverses",
            Self::ControlFlow => "Control Flow",
            Self::PatternMatching => "Pattern Matching",
            Self::MoreArray => "More Array Manipulation",
            Self::ThinkingWithArrays => "Thinking With Arrays",
            Self::Macros => "Macros",
            Self::TacitCode => "Tacit Code",
            Self::Modules => "Modules",
            Self::DataDefs => "Data Definitions",
            Self::Testing => "Testing",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct TutorialParams {
    page: TutorialPage,
}

#[component]
pub fn Tutorial() -> impl IntoView {
    let page_view = |page: TutorialPage| {
        let tut_view = match page {
            TutorialPage::Introduction => title_markdown(
                &format!("Introduction - {} Docs", lang()),
                "/text/introduction.md",
                View::default(),
            )
            .into_view(),
            TutorialPage::Basic => TutorialBasic().into_view(),
            TutorialPage::Math => TutorialMath().into_view(),
            TutorialPage::Arrays => TutorialArrays().into_view(),
            TutorialPage::Types => TutorialTypes().into_view(),
            TutorialPage::Bindings => TutorialBindings().into_view(),
            TutorialPage::Functions => TutorialFunctions().into_view(),
            TutorialPage::ControlFlow => TutorialControlFlow().into_view(),
            TutorialPage::MoreStack => TutorialMoreStack().into_view(),
            TutorialPage::Inverses => TutorialInverses().into_view(),
            TutorialPage::PatternMatching => TutorialPatternMatching().into_view(),
            TutorialPage::MoreArray => TutorialMoreArray().into_view(),
            TutorialPage::ThinkingWithArrays => TutorialThinkingWithArrays().into_view(),
            TutorialPage::Macros => TutorialMacros().into_view(),
            TutorialPage::TacitCode => TutorialTacitCode().into_view(),
            TutorialPage::Modules => TutorialModules().into_view(),
            TutorialPage::DataDefs => {
                title_markdown("Data Definitions", "/text/data_defs.md", View::default())
                    .into_view()
            }
            TutorialPage::Testing => TutorialTesting().into_view(),
        };
        view! {
            <A href="/docs">"Back to Docs Home"</A>
            <br/>
            <br/>
            <TutorialNav page=page/>
            { tut_view }
            <br/>
            <br/>
            <TutorialNav page=page/>
            <br/>
            <br/>
            <A href="/docs">"Back to Docs Home"</A>
        }
    };
    move || match use_params::<TutorialParams>().get() {
        Ok(params) => page_view(params.page).into_view(),
        Err(_) => match use_params::<OtherTutorialParams>().get() {
            Ok(params) => view! {
                <A href="/docs">"Back to Docs Home"</A>
                <br/>
                <br/>
                { params.page.view() }
                <br/>
                <br/>
                <A href="/docs">"Back to Docs Home"</A>
            }
            .into_view(),
            Err(_) => page_view(TutorialPage::Introduction).into_view(),
        },
    }
}

impl IntoParam for TutorialPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        match value {
            Some("custommodifiers") => Ok(TutorialPage::Macros),
            Some("advancedstack") => Ok(TutorialPage::MoreStack),
            Some("advancedarray") => Ok(TutorialPage::MoreArray),
            Some(val) => all::<TutorialPage>()
                .find(|p| p.path() == val)
                .ok_or_else(|| ParamsError::MissingParam(name.to_string())),
            None => Ok(TutorialPage::Introduction),
        }
    }
}

#[component]
fn TutorialNav(page: TutorialPage) -> impl IntoView {
    let next = move || {
        page.next()
            .map(|p| {
                view!( <div>"Next: "<A href=format!("/tutorial/{}", p.path())>{p.title()}</A>" 〉"</div>)
                    .into_view()
            })
            .unwrap_or_else(|| view!( <div/>).into_view())
    };
    let previous = move || {
        page.previous()
            .map(|p| {
                view!( <div>"〈 Previous: "<A href=format!("/tutorial/{}", p.path())>{p.title()}</A></div>)
                    .into_view()
            })
            .unwrap_or_else(|| view!( <div/>).into_view())
    };

    view! {
        <div class="tutorial-nav">
            { previous }
            { next }
        </div>
    }
}

#[component]
fn TutorialBasic() -> impl IntoView {
    use Primitive::*;

    let primitive_table: Vec<_> = Primitive::non_deprecated()
        .filter_map(|p| {
            if p.is_experimental() {
                return None;
            }
            let (ascii, unicode) = (p.ascii()?, p.glyph()?);
            if ascii.to_string() != unicode.to_string() {
                return Some(view! {
                    <tr>
                        <td><code>{ p.name() }</code></td>
                        <td><code>{ ascii.to_string() }</code></td>
                        <td><Prim prim=p glyph_only=true/></td>
                    </tr>
                });
            }
            None
        })
        .collect();

    view! {
        <Title text=format!("Basic Stack Operations and Formatting - {} Docs", lang())/>
        <h1>"Basic Stack Operations and Formatting"</h1>
        <Hd id="the-stack">"The Stack"</Hd>
        <p>"In "{lang}", all operations operate on a global stack. Lines of code are evaluated from "<A href="/docs/rtl">"right to left"</A>", top to bottom."</p>
        <p>"A number simply pushes its value onto the stack."</p>
        <Editor example="5"/>
        <Editor example="1 2 3"/>
        <p>"Operators pop values off the stack and push their results."</p>
        <p>"For example, "<Prim prim=Primitive::Add glyph_only=true/>" pops two values off the stack and pushes their sum."</p>
        <Editor example="+ 1 2"/>
        <p><Prim prim=Primitive::Mul glyph_only=true/>", of course, multiplies the two values instead."</p>
        <Editor example="+ 1 × 2 3" help={&["", "Try the arrows to see how the stack changes with each operation."]}/>
        <p>"In the editor, items that end up on the "<em>"top"</em>" of the stack are shown at the "<em>"bottom"</em>" of the output. This is so that consecutive lines of code show their outputs in the correct order."</p>
        <Editor example="5\n+1 2\n\"Hello, World!\"\n+1 @a"/>
        <p>"This orientation can be changed in the editor's settings. Click the ⚙️ icon in the top right corner of the editor to see them."</p>
        <p>"Operations can span multiple lines. Every line uses the same stack!"</p>
        <Editor example="1 2\n+\n5\n×"/>

        <Hd id="comments">"Comments"</Hd>
        <p>"Comments are denoted with "<code>"#"</code>" and run to the end of the line."</p>
        <Editor example="5 # This is a comment"/>
        <p>{lang}" does not have multiline comments."</p>

        <Hd id="formatting">"Formatting"</Hd>
        <p>"Most "{lang}" built-in functions use special Unicode characters. To type multiplication and division signs, you can use "<code>"*"</code>" and "<code>"%"</code>" respectively. Then, run the code to format the ASCII characters into Unicode."</p>
        <Editor example="# Click Run to format!\n%6 *3 8" help={&["", "⇡⇡⇡⇡ Click   "]}/>
        <p>"Most built-in functions have names you can type rather than symbols. Formatting works on these too. "<em><strong>"This is the primary way of entering "{lang}"'s glyphs."</strong></em></p>
        <p>"Try formatting the lines below by clicking "<strong>"Run"</strong>"."</p>
        <Editor example="max sqrt 10 mod 10 pow 2 8"/>
        <Editor example="abs +`1 `2"/>
        <p>"You don't have to type the whole name, just enough to disambiguate it from others."</p>
        <Editor example="cei 1.5\nceil 1.5\nceili 1.5\nceilin 1.5\nceiling 1.5"/>
        <p>"You don't even have to remove spaces between built-in function names. The formatter will figure it out!"</p>
        <Editor example="roundsqrtpi"/>
        <p>"On this site, you can also click the "<span class="material-symbols-rounded">"keyboard_arrow_down"</span>" symbol on any editor to show a palette of all the "{lang}" glyphs. You can then click on any glyph to insert it into the editor."</p>
        <p>"Here is a table of all the glyphs that are typed with ASCII characters that get converted to glyphs."</p>
        <table class="left-header-table">
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
        <Editor example="+ `1 `2"/>
        <p>"The formatter will align consecutive end-of-line comments. Try it out!"</p>
        <Editor example="%2 8 # Line\n@x # these\n1 # up"/>

        <Hd id="output-comments">"Output Comments"</Hd>
        <p>"A comment that starts with additional "<code>"#"</code>"s is an "<em>"output comment"</em>". The formatter replaces the text of an output comment with as many values from the stack as there are extra "<code>"#"</code>"s."</p>
        <p>"Click Run to try it out!"</p>
        <Editor example="1 2 4\n####\n+\n###\n+\n##"/>
        <p>"Output comments on the same line as other code show the values at the top of the stack after the code is run. This is useful for debugging."</p>
        <Editor example="+1 2 ##\n×3 4 ##"/>

        <Hd id="stack-functions">"Stack Functions"</Hd>
        <p>"There are a few functions that work on the stack itself. Some of these are critical and can be found scattered across all "{lang}" code."</p>
        <Hd id="dup"><Prim prim=Dup/></Hd>
        <p><Prim prim=Dup/>" duplicates the top item on the stack."</p>
        <p>"In general, functions do not leave their arguments on the stack. If you want to reuse a value, the most basic way is to "<Prim prim=Dup/>" it first."</p>
        <p>"For example, if you wanted to square a number, you could "<Prim prim=Dup/>" it, then "<Prim prim=Mul/>"."</p>
        <Editor example="×.4"/>
        <p><Prim prim=Dup/>" is often used in the examples on this site to show both the input and output of a function."</p>
        <Editor example="√.144"/>
        <br/>
        <Hd id="backward"><Prim prim=Backward/></Hd>
        <p><Prim prim=Backward/>" swaps the arguments to a function that takes two values."</p>
        <blockquote>"Note that "<Prim prim=Backward/>" is not actually a function itself. It is something called a "<em>"modifier"</em>". We'll talk about modifiers in more depth in a "<A href="/tutorial/functions#modifiers">"later section"</A>"."</blockquote>
        <p><Prim prim=Backward/>" is useful when you want to call a function that takes two arguments, but the arguments are on the stack in the wrong order."</p>
        <p>"For example, if you wanted to get the reciprocal of a number, you would "<Prim prim=Div/>" "<code>"1"</code>" by it. But, if the number is already on the stack, you would need to use "<Prim prim=Backward/>"."</p>
        <Editor example=" ÷1 5\n˜÷1 5"/>
        <Editor example="    -2 5\nback-2 5"/>
        <br/>
        <Hd id="pop"><Prim prim=Pop/></Hd>
        <p><Prim prim=Pop/>" removes the top item from the stack."</p>
        <p>"This is useful when you want to discard a value that you do not need."</p>
        <Editor example="1 pop 2 3 4 ◌ 5 6"/>
        <Hd id="stack"><Prim prim=Stack/></Hd>
        <p><Prim prim=Stack/>" prints the entire stack."</p>
        <p>"It also attaches line and column numbers."</p>
        <p>"This is useful for debugging by inspecting the stack."</p>
        <Editor example="√+ ? .+ ? 1 ×3 4"/>
        <p><Prim prim=Stack/>" is compatible with a language feature called "<em>"subscripts"</em>". If you add a little subscript number to the right of it, it will only print that many values from the stack."</p>
        <p>"We'll talk more about subscripts later. For now, know that you can add subscripts to "<Prim prim=Stack/>" by adding consecutive "<code>"?"</code>"s. The formatter will convert it for you!"</p>
        <Editor example="+1 ?? × ??? 4 ?? ×. -3 5 # Try formatting!"/>

        <Hd id="challenges">"Challenges"</Hd>
        <p>"At the end of most sections of this tutorial, there will be a few challenges to test your understanding."</p>
        <p>"The code you write will be run on multiple inputs and tested for correctness."</p>
        <p>"Each challenge has an example input and output followed by some test cases. There is also a hidden test case that your code is checked against, so make sure to think about edge cases!"</p>
        <p>"Remember that you can click the "<span class="material-symbols-rounded">"keyboard_arrow_down"</span>" on the right side of the editor to see a list of all the glyphs."</p>
        <p>"Answers are available, but "<strong>"try to solve the challenges yourself first!"</strong></p>
        <p>"Some challenges have additional answers that use functions and concepts not yet covered in the tutorial, but which are more idiomatic."</p>
        <br/>

        <Challenge
            number=1
            prompt="adds 3 numbers"
            example="1 2 3"
            answer="++"
            tests={&["0 10 1", "10 ¯1 5", "0 5 1"]}
            hidden="6 7 8"/>

        <Challenge
            number=2
            prompt="divides the first number by the second"
            example="5 10"
            answer="˜÷"
            tests={&["6 24", "2 100", "17 51"]}
            hidden="8 32"/>

        <Challenge
            number=3
            prompt="subtracts the second number from the first then squares the result"
            example="10 1"
            answer="×.˜-"
            tests={&["5 3", "9 2", "5 6"]}
            hidden="6 7"/>
    }
}

fn maybe_code<T: Display>(val: Option<T>) -> impl IntoView {
    if let Some(val) = val {
        view! {  <code>{ val.to_string() }</code> }.into_view()
    } else {
        view! {  "" }.into_view()
    }
}

fn primitive_rows(prims: impl IntoIterator<Item = Primitive>) -> Vec<impl IntoView> {
    prims
        .into_iter()
        .map(|p| {
            let ascii = p
                .ascii()
                .map(|s| s.to_string())
                .or_else(|| p.glyph().filter(|c| c.is_ascii()).map(|c| c.to_string()));
            view! {
                <tr>
                    <td><Prim prim=p/></td>
                    <td>{maybe_code( ascii)}</td>
                    <td>{view!( <code>{p.args()}</code>)}</td>
                </tr>
            }
        })
        .collect()
}

#[component]
fn TutorialMath() -> impl IntoView {
    use Primitive::*;
    let math_table = primitive_rows([
        Add, Sub, Mul, Div, Modulus, Pow, Log, Neg, Abs, Sqrt, Sign, Sin, Atan,
    ]);
    let comp_table = primitive_rows([Eq, Ne, Lt, Gt, Le, Ge, Min, Max, Floor, Ceil, Round]);

    view! {
        <Title text=format!("Math and Comparison - {} Docs", lang())/>
        <h1>"Math and Comparison"</h1>
        <p>{lang}" supports all the basic math operations as well as comparison, min/max, and rounding."</p>
        <div id="ascii-glyphs">
            <table class="bordered-table">
                <tr>
                    <th>"Function"</th>
                    <th>"ASCII"</th>
                    <th>"Args"</th>
                </tr>
                {math_table}
            </table>
            <table class="bordered-table">
                <tr>
                    <th>"Function"</th>
                    <th>"ASCII"</th>
                    <th>"Args"</th>
                </tr>
                {comp_table}
            </table>
        </div>
        <p>"Most of these are used mostly how you might think."</p>
        <Editor example="+2 5"/>
        <Editor example="↥2 5"/>
        <Editor example="ⁿ2 5"/>
        <Editor example="⌈2.5"/>
        <Editor example="√4"/>
        <p>{lang}" has no boolean type. Comparison operators return "<code>0</code>" for false and "<code>1</code>" for true."</p>
        <Editor example="=2 5"/>
        <Editor example="=2 2"/>
        <p>"One thing to note is that non-commutative operators work backwards."</p>
        <p>"This is so you can think of the operator and the second number as a single unit."</p>
        <Editor example="-2 5" help={&["", "What is 5 \"minus 2\"?"]}/>
        <Editor example="<2 5" help={&["", "Is 5 \"less than 2\"?"]}/>
        <Editor example="÷2 5" help={&["", "What is 5 \"divided by 2\"?"]}/>
        <p>"Because of how stack operations work, you can delay operations until after all the arguments are on the stack."</p>
        <Editor example="×++1 2 3 4" help={&["", "Click the arrows to see how the expression is built up"]}/>
        <p>"This is not special syntax. All the numbers are pushed to the stack, then the operators work on them."</p>
        <p>"Remember that you can type the names of operators and then run to format them."</p>
        <Editor example="# Click Run to format!\nmax sqrt2 mod10 abs`31" help={&["", "⇡⇡⇡⇡ Click   "]}/>

        <Hd id="adicity">"Adicity"</Hd>
        <p>"Some programming languages use the terms \"unary\" and \"binary\" to refer to functions that take one or two arguments respectively. While these are the Latin terms, many array languages, including "{lang}", prefer to use the Greek terms \"monadic\" and \"dyadic\"."</p>
        <p>"As you read "{lang}"'s documentation, you will see these terms used to describe functions (and modifiers)."</p>
        <p>"For example, "<Prim prim=Sqrt/>" is a monadic function, and "<Prim prim=Add/>" is a dyadic function."</p>
        <p>"On this site, "<span class="monadic-function">"monadic"</span>" functions are in "<span class="monadic-function">"green"</span>" and "<span class="dyadic-function">"dyadic"</span>" functions are in "<span class="dyadic-function">"blue"</span>"."</p>
        <p>"Some documentation may also reference functions which are "<span class="noadic-function">"noadic"</span>", "<span class="triadic-function">"triadic"</span>", or "<span class="tetradic-function">"tetradic"</span>". These are the words for functions that take 0, 3, or 4 arguments respectively. The word "<span class="noadic-function">"noadic"</span>" is not common outside of "{lang}", but it is chosen because such a function takes "<em>"no"</em>" arguments."</p>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="for arguments A, B, and C, computes (A + B) × C"
            example="1 2 3"
            answer="×+"
            tests={&["2 2 2", "5 7 2", "3 ¯1 ¯1"]}
            hidden="6 7 8"/>

        <Challenge
            number=2
            prompt="calculates the equation √(A² + B), where A is the first argument and B is the second"
            example="3 16"
            answer="√+×."
            best_answer="⍜°√+"
            tests={&["12 81", "12 25", "6 64"]}
            hidden="5 3"/>
    }
}

#[component]
fn TutorialArrays() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Arrays - {} Docs", lang())/>
        <h1>"Arrays"</h1>
        <p>{lang}" is, first and foremost, an array language. The only composite data type is the multidimensional array. Arrays have a lot of nice properties, and the language's built-in functions are designed to make it easy to work with them. If you've only ever programmed in non-array languages, then this will be a completely foreign paradigm. In most array languages, most data structures and control flow are replaced with operations on arrays."</p>

        <Hd id="creating-arrays">"Creating Arrays"</Hd>
        <p>"Other than with functions, "{lang}" has two ways to create arrays. They are called "<em>"strand notation"</em>" and "<em>"stack notation"</em>"."</p>
        <p><strong>"Strand notation"</strong>" uses underscores to connect elements."</p>
        <Editor example="1_2_3"/>
        <Editor example="\"Hello\"_\"World\""/>
        <p>"Strand notation is good when you want to create short and/or simple arrays. For longer or more complex arrays, you can use stack notation."</p>
        <p><strong>"Stack notation"</strong>" uses "<code>"[]"</code>" brackets to group elements."</p>
        <Editor example="[1 2 3]"/>
        <Editor example="[¯5 37 42 π]"/>
        <p>"What's cool about stack notation is that it is "<em>"not"</em>" just a way to list elements. The code between the brackets runs from right to left as it normally would. When it is done, any items on the stack higher than when it started are put into the array. This gives you some cool ways to create arrays."</p>
        <p>"Remember that "<Prim prim=Dup/>" duplicates the top item on the stack."</p>
        <Editor example="[...5]"/>
        <Editor example="[×2.×2.×2.×2 .2]"/>
        <Editor example="[+1 2 +3 4]"/>
        <p>"Any functions inside the brackets will \"pull in\" their arguments from outside if there are not enough inside."</p>
        <Editor example="[+] 1 9"/>
        <Editor example="[...] 7"/>
        <Editor example="[+×2] 20 2"/>
        <p>"You can also use stack notation to make multidimensional arrays."</p>
        <Editor example="[1_2_3 4_5_6]"/>
        <Editor example="[...[1 2 3]]"/>
        <p>"Unlike strand notation, stack notation may span multiple lines. The lines are still executed right-to-left, but they are executed bottom-to-top so that the arrays come out the same way they look in the code."</p>
        <Editor example="\
[1 2 3
 4 5 6
 7 8 9]"/>
        <Editor example="\
[[1 2 3]
 [4 5 6]
 [7 8 9]]"/>

        <Hd id="shape-len"><Prim prim=Shape/>" and "<Prim prim=Len/></Hd>
        <p>"Other than their data, arrays also have a property called their "<strong>"shape"</strong>". Shape is a list of non-negative integers that describes the array's size along each of its axes."</p>
        <p>"We can get the array's shape with the "<Prim prim=Shape/>" function. It's a triangle because a triangle is a shape."</p>
        <Editor example="△[1 2 3]"/>
        <Editor example="△5"/>
        <Editor example="△[[1 2 3] [4 5 6]]"/>
        <Editor example="△[...[1 2 3]]"/>
        <p>"Arrays with 0 dimensions (an empty "<Prim prim=Shape/>") are called "<strong>"scalars"</strong>"."</p>
        <p>"Arrays with 1 dimension are often called "<strong>"lists"</strong>" or "<strong>"vectors"</strong>"."</p>
        <p>"Arrays with 2 dimensions are often called "<strong>"tables"</strong>" or "<strong>"matrices"</strong>"."</p>
        <p>"While there are not common names for arrays with 3 or more dimensions, "{lang}" supports arrays with an arbitrary number of axes."</p>
        <p>"The first element of the shape is the number of "<em>"rows"</em>" of the array. "<em>"Rows"</em>" does not refer just to the rows of a matrix or table. It is the groups of elements along the leading axis of the array. For lists, this is just the individual elements. For matrices, it is the rows as you might traditionally think of them. But arrays with a higher number of dimensions have rows as well. For example, in an array with 3 dimensions, each row is a matrix."</p>
        <p>"From shape we can derive two closely-related properties called "<strong>"length"</strong>" and "<strong>"rank"</strong>"."</p>
        <p><Prim prim=Len/>" is the number of rows in the array. Length is always equal to the first number in the shape (or 1 if the shape is empty)."</p>
        <p><strong>"Rank"</strong>" is the number of dimensions of the array. It is equivalent to the "<Prim prim=Len/>" of the "<Prim prim=Shape/>"."</p>
        <Editor example=" △[1_2_3 4_5_6 7_8_9]\n ⧻[1_2_3 4_5_6 7_8_9]\n⧻△[1_2_3 4_5_6 7_8_9] # Rank"/>

        <Hd id="output">"Pretty Array Output"</Hd>
        <p>"The online editor and native interpreter both pretty-print any values that remain on the stack when a program is finished. (This can be invoked manually using the "<Prim prim=Sys(SysOp::Show)/>" or "<Prim prim=Pretty/>" functions.)"</p>
        <p>"To understand how the pretty-printed output corresponds to the actual array, we can use "<Prim prim=Reshape/>" to create a multidimensional array. "<Prim prim=Reshape/>" uses its first argument as a new shape for its second argument."</p>
        <p>"Here, we create a "<Prim prim=Range/>" array of all the numbers up to "<code>"24"</code>" and turn it into a 3-dimensional array with the shape "<code>"[2 3 4]"</code>"."</p>
        <Editor example="↯2_3_4 ⇡24"/>
        <p>"Notice there are "<code>"2"</code>" big cells, each with "<code>"3"</code>" rows of "<code>"4"</code>" elements."</p>
        <p>"This expands to any number of dimensions. The elements of the last axis are always laid out horizontally. The rows of the second-to-last axis are always laid out vertically. The third-to-last axis is horizontal, the fourth-to-last is vertical, etc."</p>
        <p>"We can see here that the shape "<code>"[2 3 4 5]"</code>" appears almost like a 2×3 matrix of 4×5 matrices."</p>
        <Editor example="↯2_3_4_5 ⇡120"/>

        <Hd id="pervasion">"Pervasion"</Hd>
        <p>"Most operations that apply to scalars are what is called "<em>"pervasive"</em>" when it comes to arrays. This means that the operation automatically applies to every item in the array."</p>
        <Editor example="+1 1_2_3"/>
        <Editor example="√[4 9 16]"/>
        <Editor example="+1_2_3 4_5_6"/>
        <p>"When doing a pervasive operation on two arrays, the shape of one array must be the "<em>"prefix"</em>" of the shape of the other. This means that all the numers in one shape must be at the beginning of the other shape."</p>
        <p>"Here, neither of the shapes "<code>"[2]"</code>" or "<code>"[3]"</code>" are prefixes of the other."</p>
        <Editor example="+[1 2] [3 4 5]"/> // Should fail
        <p>"But here, the shape of the first array ("<code>"[2]"</code>") is a prefix of the shape of the second array ("<code>"[2 3]"</code>")."</p>
        <Editor example="△10_20\n      △[3_4_5 6_7_8]\n+10_20 [3_4_5 6_7_8]"/>
        <p>"If you want to do some pervasive operation on arrays whose shapes do not match, you can set a default value with "<Prim prim=Fill/>". Any places where the shapes don't match will be filled in with that value."</p>
        <Editor example="⬚10+ [1 2] [3 4 5 6 7]"/>
        <p><Prim prim=Fill/>" can be used in a lot of other cases. See its documentation for more."</p>
        <p>"Pervasive operations are optimized in the interpreter to be very fast. You should prefer to use them whenever possible."</p>

        <Hd id="useful-array-operations">"Useful Array Operations"</Hd>
        <p>"You don't need to memorize all of these right now. This is just a brief introduction to some of the array operations so that you won't be surprised when you see them later."</p>
        <p>"If you ever see a glyph that you don't recognize in an example, you can mouse over it in the editor to learn its name."</p>
        <p>"You can ctrl/⌘-click any glyph in the editor to see its documentation."</p>
        <p>"You can also click the names of functions in the site text to see their documentation."</p>
        <p><Prim prim=Couple/>" turns two arrays into rows of a new array."</p>
        <Editor example="⊟ 1_2_3 [4 5 6]"/>
        <p><Prim prim=Join/>" joins two arrays end-to-end."</p>
        <Editor example="⊂ 1_2_3 4"/>
        <Editor example="⊂ 1_2_3 [4 5 6 7]"/>
        <p><Prim prim=First/>" and "<Prim prim=Last/>" get the first or last row of an array."</p>
        <Editor example="⊢ [4 7 1]"/>
        <Editor example="⊢ [1_2 3_4 5_6]"/>
        <Editor example="⊣ \"hello\""/>
        <p><Prim prim=Reverse/>" reverses the rows of an array."</p>
        <Editor example="⇌ [4 7 1]"/>
        <Editor example="⇌ [1_2 3_4 5_6]"/>
        <p><Prim prim=Rotate/>" rotates the rows of an array by some amount."</p>
        <Editor example="↻2 [1 2 3 4 5]"/>
        <p><Prim prim=Deshape/>" flattens an array into a 1D array."</p>
        <Editor example="♭ . [1_2 3_4 5_6]"/>
        <p><Prim prim=Take/>" and "<Prim prim=Drop/>" isolate part of an array."</p>
        <Editor example="↙3 [1 2 3 4 5]\n↘3 [1 2 3 4 5]"/>
        <p>"Negative values work from the end."</p>
        <Editor example="↙¯3 [1 2 3 4 5]\n↘¯3 [1 2 3 4 5]"/>
        <p><Prim prim=Pick/>" indexes an array. Longer indices index deeper into the array."</p>
        <p>{lang}" is 0-indexed."</p>
        <Editor example="⊡2 [3 8 4 1]"/>
        <Editor example="⊡1   [1_2_3 4_5_6]\n⊡1_1 [1_2_3 4_5_6]"/>
        <p><Prim prim=Select/>" uses a list of indices to select rows of an array."</p>
        <Editor example="⊏ [0 2 1 1 2] ↯3_3⇡9"/>
        <Editor example="⊏ [3 5 0 1 7 8 9 5 1 2 5 3 10] \"their sinks\""/>
        <p><Prim prim=Transpose/>" rotates the axes of an array. This is useful for changing which axis other functions will work on."</p>
        <Editor example="⍉ . [1_2_3 4_5_6]"/>

        <Hd id="array-model">"The Array Model"</Hd>
        <p>"For curious array aficionados, "{lang}" uses an array model resembling "<a href="https://aplwiki.com/wiki/Box">"J's Boxed array model"</a>"."</p>
        <p>"All arrays are flat and homogenous. Arrays always have a rectangular shape, meaning that all rows along an axis always have the same length. Different types of data, like numbers and characters, cannot be mixed in the same array."</p>
        <p>"However, there is an escape hatch for when you really want jagged, nested, or mixed-type arrays. In "{lang}", an array of heterogeneous values can be simulated with an array of "<em>"boxes"</em>"."</p>
        <p>"The array below cannot be constructed normally because its rows have different "<Prim prim=Shape/>"s."</p>
        <Editor example="[1_2 3 [4 5 6] [7]]"/> // Should fail
        <p>"By using "<Prim prim=Box/>", we can turn any value into a "<strong>"box"</strong>" that contains that value. We can then put these boxes into an array together."</p>
        <Editor example="[□1_2 □3 □[4 5 6] □[7]]"/>
        <p>"The "<Prim prim=Box/>"ed items in the array are separated by "<code>"│"</code>"s. Scalars are marked with a "<code>"∙"</code>" to distinguish them from single-row lists."</p>
        <p>"This is "<em>"not"</em>" an array of numbers. It is an array of boxes! Notice that the "<Prim prim=Shape/>" of the array has nothing to do with the shape of any of the boxed values."</p>
        <Editor example="△ [□1_2 □3 □[4 5 6] □[7]]"/>
        <p><Prim prim=Un/><Prim prim=Box/>" extracts a "<Prim prim=Box/>"ed value."</p>
        <Editor example="°□ .□[1 2 3]"/>
        <p>"The "<code>"□"</code>"in front of the list indicates that it is "<Prim prim=Box/>"ed."</p>
        <p>"Having to write "<Prim prim=Box glyph_only=true/>" everywhere is annoying, and so..."</p>

        <Hd id="nested-arrays">"Nested Arrays"</Hd>
        <p>{lang}" has a special syntax for making arrays where every item is "<Prim prim=Box/>"ed."</p>
        <p>"Using "<code>"{}"</code>"s instead of "<code>"[]"</code>"s for stack array notation will automatically "<Prim prim=Box/>" every item."</p>
        <Editor example="{1_2 3 [4 5 6] [7]}"/>
        <p>"This is very useful for making lists of strings."</p>
        <Editor example=r#"["Uiua" "APL" "J" "BQN" "K" "Q"] # Fails"#/>
        <Editor example=r#"{"Uiua" "APL" "J" "BQN" "K" "Q"} # Works!"#/>
        <p>"Functions that require their arguments to have matching types may require "<Prim prim=Box/>"ing an argument."</p>
        <p>"For example, to check if a string is in a list of "<Prim prim=Box/>"ed strings with "<Prim prim=MemberOf/>", you would need to "<Prim prim=Box/>" the string first."</p>
        <Editor example=
r#"Langs ← {"Uiua" "APL" "J" "BQN" "K" "Q"}
∊ Langs □"APL""#/>
        <p>"Pervasive functions work through boxes and preserve the maximum "<Prim prim=Box/>" depth of their arguments."</p>
        <Editor example="¯ 1\n¯ □1\n¯ □□1"/>
        <Editor example="+1 4\n+1 □4\n+1 □□4\n+□□1 □4"/>
        <Editor example="×10 {1_2_3 4_5 6}"/>
        <p>"There is an exception for comparison functions, which compare lexicographically."</p>
        <Editor example=r#"=  [1 2 3]  [1 2 5]
= □[1 2 3] □[1 2 5]
>  [1 2 3]  [1 2 5]
> □[1 2 3] □[1 2 5]
>  "banana"  "orange"
> □"banana" □"orange""#/>
        <p>"Non-pervasive functions often require "<Prim prim=Un/><Prim prim=Box/>"ing the arguments to get at the value you want."</p>
        <p>"Consider this difference:"</p>
        <Editor example="△    ⊢{1_2_3 5_6}\n△ °□ ⊢{1_2_3 5_6}"/>

        <p>"For more about working with box arrays, see "<Prim prim=Box/>"'s documentation."</p>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="adds an array to its reverse"
            example="1_2_5"
            answer="+⇌."
            tests={&["3_1_7", "↯2_4⇡8", "5"]}
            hidden="0_1_2_6"/>

        <Challenge
            number=2
            prompt="creates a matrix of 0's with as many rows as the first argument and as many columns as the second argument"
            example="3 4"
            answer="˜↯0⊟"
            tests={&["2 7", "3 3", "1 8"]}
            hidden="1 1"/>

        <Challenge
            number=3
            prompt="adds a 1-row leading axis to an array"
            example="[1 2 3]"
            answer="↯⊂1△."
            best_answer="¤"
            tests={&["1_3_1_5", "5", "↯2_3⇡6"]}
            hidden="1_1_1_1_1"/>

        <Challenge
            number=4
            prompt="prepends the first row of the first argument to the second argument"
            example="[1 2 3] 4_5_6"
            answer="⊂⊢"
            tests={&["3_3 2_2", "[1_2_3 4_5_6] +10↯3_3⇡9", "[2 4 3] [9 9 9 9 9 1]"]}
            hidden="↯2_3_4⇡24 ↯3_4⇡12"/>

        <Challenge
            number=5
            prompt="removes the first and last rows from an array"
            example="1_2_3_4"
            answer="↘¯1↘1"
            tests={&["[27 9 3 1]", "↯4_3⇡12"]}
            hidden="[5]"/>

        <Challenge
            number=6
            prompt="prepends an array as an item to a list of boxed arrays"
            example="\"Hi\" {\"how\" \"are\" \"ya\"}"
            answer="⊂□"
            tests={&["1_2_3 {4_5 [6]}", "[] {[] []}"]}
            hidden="1 {2 3}"/>
    }
}

#[component]
fn TutorialBindings() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Bindings - {} Docs", lang())/>
        <h1>"Bindings"</h1>
        <p>"Bindings are global names that can be given to "{lang}" values. They are denoted with "<code>"←"</code>", which the formatter will convert from "<code>"="</code>" when appropriate."</p>
        <Editor example="a = 3\nb ← 5\n+ a b" help={&["", "Try running to format the ="]}/>
        <p>"Valid binding names can be made up of any sequence of uppercase or lowercase alphabetic characters OR a single non-alphanumeric character that is not already used for a "{lang}" function or syntax."</p>
        <Editor example="NumOne ← 1\nNumTwo ← 2\n😀 ← \"happy\""/>
        <p><em>"Warning"</em>": It is not guaranteed that any particular non-alphanumeric character will not be used for a built-in function in the future. Use them at your own risk. Emojis are safe though."</p>
        <p>"Unlike most programming languages, binding names in "{lang}" "<em>"cannot"</em>" contain numbers or underscores."</p>
        <Editor example="Variable_1 ← 5"/> // Should fail
        <p>"Bindings "<em>"can"</em>" contain subscript numbers. These will format from "<code>"__"</code>" followed by some digits. Try formatting the example below!"</p>
        <Editor example="X__1 = 5\nSha__256 = \"TODO\""/>
        <p>"Subscripts are only allowed at the end of a binding name."</p>
        <p><strong>"Bindings are case-sensitive."</strong></p>
        <p>"The parser can sometimes mistake all-lowercase binding names for unformatted built-in functions."</p>
        <p>"Here, the parser thinks that "<code>"part"</code>" is "<Prim prim=Partition/>"."</p>
        <Editor example="part = 5" help={&["", "Run to format and reveal why this does not work"]}/>
        <p>"Binding names with 2 or more characters should be "<A href="https://en.wikipedia.org/wiki/Camel_case">"PascalCase (also known as upper CamelCase)"</A>" to avoid this issue."</p>
        <Editor example="Part = 5\n*2 Part"/>
        <p>"Bindings run the code to the right of the "<code>"←"</code>", then pop the top value off the stack and bind it to the name on the left."</p>
        <p>"Note, though, that an empty right side is perfectly valid! This means you can bind values that were created on previous lines."</p>
        <Editor example="×6 7\nAnswer ←\n[Answer]"/>

        <Hd id="binding-functions">"Binding Functions"</Hd>
        <p>"If the code on the right side of the "<code>"←"</code>" requires more than 0 values to be on the stack, then instead of evaluating its right side immediately, the right side will be bound as a function."</p>
        <p>"This is how you make named functions in "{lang}"."</p>
        <Editor example="F ← +1\nF 5"/>
        <Editor example="Cube ← ××..\nCube 6"/>
        <Editor example="👋 ← ⊂\"Hello, \"\n👋 \"World!\""/>
        <p>"If the code on the right side takes 0 arguments but you still want it to be a function, it must be surrounded by "<code>"()"</code>"s."</p>
        <p>"Notice how the first example here gives the same value every time, while the second one does not."</p>
        <Editor example="F ← ⚂\nF F F"/>
        <Editor example="F ← (⚂)\nF F F"/>
        <p>"The "<A href="/tutorial/functions">"next section"</A>" discusses functions in more detail."</p>
    }
}

#[component]
fn TutorialFunctions() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Modifiers and Functions - {} Docs", lang())/>
        <h1>"Modifiers and Functions"</h1>

        <Hd id="modifiers">"Modifiers"</Hd>
        <p>"Modifiers are functions that take other functions as arguments. If you immediately follow a modifier with its function arguments, the functions will be called inside the modifier rather than outside."</p>
        <p>"For example, "<Prim prim=Reduce/>" applies a function \"between\" all rows of an array."</p>
        <p><Prims prims={[Reduce, Add]}/>" is therefore the sum of all the rows of an array."</p>
        <Editor example="/+ 1_2_3_4"/>
        <p><Prim prim=Scan/>" is similar, but it returns all the intermediate results."</p>
        <Editor example="\\+ 1_2_3_4"/>
        <p><Prim prim=Rows/>" applies a function to each row of an array."</p>
        <p>"For example, "<Prims prims=[Reduce, Add]/>" adds each row of a matrix to the next, effectively summing along the columns."</p>
        <p><Prims prims=[Rows, Reduce, Add]/>" sums each row itself."</p>
        <Editor example="    [1_2_3 4_5_6 7_8_9]\n /+ [1_2_3 4_5_6 7_8_9]\n≡/+ [1_2_3 4_5_6 7_8_9]"/>
        <p><Prim prim=Table/>" applies a function between all combinations of rows of two arrays. This is sometimes called the "<em>"outer product"</em>"."</p>
        <Editor example="⊞+ [5 6 7 8] [10 20 30 40]"/>
        <p>"In the same way that \"monadic\" and \"dyadic\" functions refer to functions that take one or two array arguments respectively, \"monadic\" and \"dyadic\" "<em>"modifiers"</em>" refer to modifiers that take one or two "<em>"functions"</em>" respectively."</p>
        <p>"On this site, monadic modifiers are in "<span class="monadic-modifier">"yellow"</span>" and dyadic modifiers are in "<span class="dyadic-modifier">"purple"</span>"."</p>
        <p>"The main docs page has "<A href="/docs/modifier">"a list"</A>" of all of the built-in modifiers."</p>

        <Hd id="inline-functions">"Inline Functions"</Hd>
        <p>"In addition to creating a new function with a capitalized binding name, as discussed in the "<A href="/tutorial/bindings">"previous section"</A>", functions in "{lang}" can also be created by surrounding code with "<code>"()"</code>"s."</p>
        <p>"This is usually only necessary when you need to call multiple functions within a modifier."</p>
        <p>"For example, if you wanted to add each row of an array to its reverse, you could use "<Prim prim=Add/><Prim prim=Reverse/><Prim prim=Dup/>"."</p>
        <Editor example="≡(+⇌.) .[2_5_3 0_2_1 0_0_2]"/>
        <p>"Inline functions may span multiple lines. Unlike multiline stack notation arrays, which run bottom-to-top, multiline inline functions run top-to-bottom as other code does."</p>
        <Editor example="\
X ← (
  ⊞=.⇡ # First this line runs
  ↥⇌.  # Then this one
)
X 5"/>
        <p>"Output comments inside inline functions will show the values on the stack for each time the function is called. Try it out!"</p>
        <Editor example="\
F ← (
  ### Run to see values here!
  +×. ##
)
F 3 5
F 2 9
F 10 11"/>

        <Hd id="local-bindings">"A Note on Local Bindings"</Hd>
        <p>"Bindings in "{lang}" can "<em>"only"</em>" be global. There is no way to give a name to a value within an inline function."</p>
        <p>"This is a deliberate design decision. It forces you to write tacit code, a.k.a. code with functions that do not mention their arguments. "{lang}" is designed to make writing tacit code as workable as possible. "<em>"How"</em>" it does this will be discussed in "<A href="/tutorial/morestack">"later"</A>" "<A href="/tutorial/advancedarray">"sections"</A>"."</p>

        <Hd id="format-strings">"Format Strings"</Hd>
        <p>"Prefixing a string with a "<code>"$"</code>" creates a format string. A format string is a special kind of function. It takes an argument for each "<code>"_"</code>" in the string and replaces it with the stringified version."</p>
        <Editor example="\"World\"\n$\"Hello, _!\""/>
        <Editor example="Greet ← $\"Hello, _!\"\nGreet \"user\""/>
        <Editor example="x ← 5\n$\"x = _\" x"/>
        <Editor example="$\"_, _, and _\" 1 2 3"/>
        <p>"If you need to use a literal "<code>"_"</code>", you can escape them with "<code>"\\"</code>"."</p>
        <Editor example="$\"\\__\\_\" 27"/>
        <p>"Raw strings can be made format strings by adding an additional "<code>"$"</code>"."</p>
        <Editor example="◡+ 1 2\n&p $$ What are two numbers that add up to _?\n   $$ _ and _ do!"/>
        <p><code>"_"</code>"s still need to be escaped in raw format strings."</p>
        <Editor example="1 2 3\n$$ _\\__\\__"/>
        <p>"Because format strings are just functions, you can use them with modifiers like "<Prim prim=Reduce/>". This is a common way to join a list of "<Prim prim=Box/>"ed strings."</p>
        <Editor example="/$\"_ _\" {\"Separated\" \"by\" \"spaces\"}"/>
        <p>"The "<A href="/tutorial/strings">"Working with Strings"</A>" tutorial has a "<A href="/tutorial/strings#format-string-tricks">"section"</A>" with more format string tricks."</p>

        <Hd id="stack-signatures">"Stack Signatures"</Hd>
        <p>"Bindings and inline functions can have a "<em>"stack signature"</em>" declared with a "<code>"|"</code>" followed by 1 or 2 numbers separated by a "<code>"."</code>". The first number is the number of arguments the function pops from the stack. The second number is the number of values the function pushes to the stack."</p>
        <p>"The second number is optional. If it is not given, it is assumed to be 1."</p>
        <p>"In bindings, the "<code>"|"</code>" comes after the "<code>"←"</code>". In inline functions, it comes after the "<code>"("</code>"."</p>
        <Editor example="TimesThree ← |1.1 ×3\nTimesThree 7"/>
        <Editor example="TimesThree ← |1   ×3\nTimesThree 7"/>
        <Editor example="≡(|2.1 ⊟.×) 1_2_3 4_5_6"/>
        <p>"Stack signatures are useful for documenting functions to make sure that they are used correctly."</p>
        <p>"A signature declaration is "<em>"required"</em>" if the function's signature cannot be inferred. The compiler can usually infer a function's signature unless you are doing something weird that it cannot reason about."</p>
        <p>"A declared signature always overrides the inferred signature. However, if they do not match, a warning will be emitted."</p>
        <Editor example="F ← |3.2 ⊟+1\nF 1 4"/> // Should fail
        <p>"In that example, the net stack change of the declared signature of "<code>"|3.2"</code>" is actually the same of that of the inferred signature of "<code>"|2.1"</code>". Either way, the function will cause the stack to have one less value."</p>
        <p>"If this is "<em>"not"</em>" the case, the declared signature will be "<em>"made"</em>" to be correct by, after the function has run, either popping extra arguments or pushing extra outputs below on the stack."</p>
        <p>"For example, even though this function has signature "<code>"|2.1"</code>", the declared signature of "<code>"|2.2"</code>" causes an extra debug output to be pushed. This debug output is a boxed string. This is done to ensure that the function does in fact have 2 outputs."</p>
        <Editor example="F ← |2.2 ⊟+1\nF 1 4 5"/> // Should fail
        <p>"Conversely, if the function needs to have more "<em>"arguments"</em>", such as here where we declare it to have signature "<code>"|4.2"</code>", some extra arguments will be popped from the stack. In this case, it is the "<code>"6"</code>" that is popped. We can see that this does in fact make the function turn 4 values on the stack into 2."</p>
        <Editor example="F ← |4.2 ⊟+1\nF 1 4 5 6"/> // Should fail
        <p><strong>"The point of allowing this is that functions which are only partially written can still be run and debugged. But finished code should never have warnings!"</strong></p>
        <p>"If the compiler cannot derive the stack signature of a function and you give it one which is "<em>"wrong"</em>", the function will throw an error at runtime."</p>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="calculates the product of the first n positive integers"
            example="5"
            answer="/×+1⇡"
            tests={&["10", "0", "6"]}
            hidden="3"/>

        <Challenge
            number=2
            prompt="adds each column of a matrix to the next"
            example="[1_2_3 4_5_6]"
            answer="≡/+"
            tests={&["[6_9_1_2 3_0_0_1 2_3_4_5]", "[2_2_2_2_5]"]}
            hidden="↯2_3_4_5⇡120"/>

        <Challenge
            number=3
            prompt="wraps a string in brackets"
            example="\"Hello, World!\""
            answer="$\"[_]\""
            tests={&["\"Uiua\"", "\"🙃\""]}
            hidden="\"tomorrow\""/>
    }
}

#[component]
fn TutorialMoreStack() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("More Stack Manipulation - {} Docs", lang())/>
        <h1>"More Stack Manipulation"</h1>
        <p>{lang}" does not have local variables. With only "<Prim prim=Dup/>" and "<Prim prim=Backward/>", how do you work with more than 2 values at a time?"</p>

        <Hd id="fork"><Prim prim=Fork/></Hd>
        <p><Prim prim=Fork/>" is a dyadic modifier that takes 2 functions and calls them both on the same set of arguments. The number of arguments used is the maximum of the two functions."</p>
        <Editor example="[⊃+× 3 5]"/>
        <p>"If one of the functions takes more arguments than the other, the function with fewer arguments uses the top-most values."</p>
        <Editor example="⊃×⇌ [1 2 3] 10"/>
        <p>"What's powerful about "<Prim prim=Fork/>" is that it can be chained to use as many functions as you want."</p>
        <Editor example="[⊃⊃⊃+-×÷ 5 8]"/>
        <p><Prim prim=Fork/>" is also good because it does not require that its values be in an array together, so they can be different shapes or types."</p>
        <Editor example="⊃+- 1 @b"/>
        <Editor example="⊃⊃⊃↻↙↘⊡ 2 [1 2 3 4 5]"/>
        <p>"We'll see just how important "<Prim prim=Fork/>" is later in this section."</p>

        <Hd id="both"><Prim prim=Both/></Hd>
        <p><Prim prim=Both/>" is a monadic modifier and a sort of complement to "<Prim prim=Fork/>". While "<Prim prim=Fork/>" calls multiple functions on the same set of arguments, "<Prim prim=Both/>" calls a "<em>"single"</em>" function on "<em>"multiple"</em>" sets of arguments."</p>
        <Editor example="∩⇌ [1 2 3] [4 5 6]"/>
        <p>"Chaining "<Prim prim=Both/>" doubles the number of arguments each time."</p>
        <Editor example="∩∩⇌ [1 2 3] [4 5 6] [7 8 9] [10 11 12]"/>

        <Hd id="bracket"><Prim prim=Bracket/></Hd>
        <p>"To round off the trio, we have "<Prim prim=Bracket/>", which is a dyadic modifier that calls each of its functions on a different set of arguments."</p>
        <Editor example="[⊓+× 1 2 3 4]"/>
        <p><Prim prim=Bracket/>" too can be chained. Each additional function is called on arguments deeper in the stack."</p>
        <Editor example="[⊓⊓⊓+¯×. 1 2 3 4 5 6]"/>

        <Hd id="function-packs">"Function Packs"</Hd>
        <p>"All dyadic modifiers allow a special notation with a single set of "<code>"()"</code>"s with a "<code>"|"</code>" in the middle separating the functions. This is called a "<em>"function pack"</em>"."</p>
        <Editor example="⊓(+|×) 1 2 3 4"/>
        <p>"While all dyadic modifiers can use function packs, "<Prim prim=Fork/>" and "<Prim prim=Bracket/>" allow more than 2 functions to be used. This can sometimes be shorter and/or more readable than chaining the modifier."</p>
        <Editor example="[⊃(+|-|×|÷) 5 8]"/>
        <Editor example="[⊓(+1|×|÷2) 5 10 12 22]"/>

        <Hd id="dip"><Prim prim=Dip/></Hd>
        <p>"The "<Prim prim=Dip/>" modifier temporarily pops the top value on the stack, calls its function, then pushes the value back."</p>
        <Editor example="[⊙+ 1 2 3]"/>
        <p><Prim prim=Dip/>" can be chained to dig deeper into the stack, though try not to dig "<em>"too"</em>" deep, as it makes code harder to read."</p>
        <Editor example="[⊙⊙⊙⊙⊙⊙+ 1 2 3 4 5 6 7 8]"/>
        <p>"One use of "<Prim prim=Dip/>" is to collect values from the stack into an array. Here, a chain of "<Prim prim=Dip/>"s are terminated with "<Prim prim=Identity/>"."</p>
        <Editor example="[⊙⊙⊙∘] 1 2 3 4 5"/>
        <Editor example="{⊙⊙∘} 1 2_3 \"wow\""/>
        <p>"However, you do not typically need to do this because of..."</p>

        <Hd id="Subscripts">"Subscripts"</Hd>
        <p>"Subscripts are a special syntax that allows you to augment some functions and modifiers with a number."</p>
        <p>"Subscripts are typed with "<code>"__"</code>" followed by some digits. The formatter will turn them into subscript digit characters. A leading negative sign is allowed."</p>
        <p>"Several functions and modifiers are supported, but we'll only cover some stack-related ones here. You can find a full list of subscript-compatible functions "<A href="/docs/subscripts">"here"</A>"."</p>
        <p>"Subscripted "<Prim prim=Both/>" calls its function on N sets of arguments."</p>
        <Editor example="[∩+ 1 2 3 4]\n[∩__3+ 1 2 3 4 5 6]\n[∩__4+ 1 2 3 4 5 6 7 8] # Try formatting!"/>
        <p>"Subscripted "<Prim prim=Couple/>" collects N values from the stack into an array."</p>
        <Editor example="⊟₄ 1 2 3 4 5"/>
        <p><Prim prim=Box/>" has similar behavior, but it boxes each value."</p>
        <Editor example="□₃ 5 \"Hi!\" [1 2 3]"/>

        <Hd id="planet-notation">"🌍 Planet Notation 🪐"</Hd>
        <p><Prim prim=Gap/>" "<em>"discards"</em>" the top value on the stack and calls its function."</p>
        <Editor example="⋅+ 1 2 3"/>
        <p>"But wait, "<Prim prim=Pop/>" exists! Why would you need this?"</p>
        <p>"The main reason for "<Prim prim=Dip/>" and "<Prim prim=Gap/>" to exist is to be chained with "<Prim prim=Identity/>", often inside of "<Prim prim=Fork/>". They act as a sort of boolean selector to choose which arguments to keep and which to discard in a branch."</p>
        <p>"This is called "<em>"planet notation"</em>" because it looks like the planets in a solar system chart."</p>
        <p>"For example, let's say you want to "<Prim prim=Mul/>" the 2nd and 4th arguments on the stack and discard the rest:"</p>
        <Editor example="×⋅⊙⋅∘ 1 2 3 4"/>
        <p>"Notice how the circles correspond to the stack arguments we want."</p>
        <p>"Maybe you want to "<Prim prim=Add/>" 3 numbers but keep the second 2 on the stack:"</p>
        <Editor example="[⊃⋅⊙∘(++)] 2 5 10"/>
        <p>"You can read "<Prim prim=Gap glyph_only=true/><Prim prim=Dip glyph_only=true/><Prim prim=Identity glyph_only=true/>" as \"discard argument 1, keep argument 2, keep argument 3.\""</p>
        <p>"If you only wanted to keep argument 2, you simply make the expression shorter:"</p>
        <Editor example="[⊃⋅∘(++)] 2 5 10"/>
        <p>"For a more useful example, let's do a complex mathematical expression. We will implement this function (shown here in mathematical notation):"</p>
        <code class="code-block">"f(a,b,c,x) = (a+x)(bx-c)"</code>
        <p>"We'll start with the "<code>"(a + x)"</code>" part. We can grab "<code>"a"</code>" and "<code>"x"</code>" with "<Prim prim=Dip/>" and "<Prim prim=Identity/>", and ignore "<code>"b"</code>" and "<code>"c"</code>" with "<Prim prim=Gap/>"."</p>
        <Editor example="+⊙⋅⋅∘ 1 2 3 4"/>
        <p>"Next, we'll do the "<code>"(bx-c)"</code>" part. We can grab each term with "<Prim prim=Fork/>"."</p>
        <Editor example="-⊃(⋅⋅∘)(×⋅⊙⋅∘) 1 2 3 4"/>
        <p>"The first pair of "<code>"()"</code>"s is not actually necessary, so let's remove them."</p>
        <Editor example="-⊃⋅⋅∘(×⋅⊙⋅∘) 1 2 3 4"/>
        <p>"Finally, we can combine the two parts with another "<Prim prim=Fork/>"."</p>
        <Editor example="×⊃(+⊙⋅⋅∘)(-⊃⋅⋅∘(×⋅⊙⋅∘)) 1 2 3 4"/>
        <p>"If you like, you can factor out the "<Prim prim=Gap/>" in the second part"</p>
        <Editor example="×⊃(+⊙⋅⋅∘)⋅(-⊃⋅∘(×⊙⋅∘)) 1 2 3 4"/>
        <p>"Alternatively, you can use a function pack."</p>
        <Editor example="×⊃(+⊙⋅⋅∘|-⊃⋅⋅∘(×⋅⊙⋅∘)) 1 2 3 4"/>
        <p>"And there you have it! A readable syntax juggling lots of values without any names!"</p>
        <p>"It's annoying to write long lists of names like "<code>"gapdipgapgapide"</code>", so those three functions (plus "<Prim prim=Pop/>") have a special rule in the parser that allows you to write them with only 1 character as long as there are at least 2 characters in the sequence. Also, 'i' and 'p' for "<Prim prim=Identity/>" and "<Prim prim=Pop/>" only work if they are the last character."</p>
        <p>"Try it out!"</p>
        <Editor example="+gdggi 1 2 3 4 5"/>
        <Editor example="+dggdp 1 2 3 4 5"/>

        <Hd id="on-and-by"><Prim prim=On/>" and "<Prim prim=By/></Hd>
        <p>"As you write more "{lang}" code, you'll find that there is a kind of pattern you'll encounter over and over again. It involves calling a function, then calling another function that re-uses an argument to the first function."</p>
        <p>"One simple example is getting "<code>"n"</code>" numbers between "<code>"0"</code>" and "<code>"1"</code>". One way you may think to solve this is with "<Prim prim=Dup/>" and "<Prim prim=Backward/>"."</p>
        <Editor example="˜÷⇡. 5"/>
        <p>"This solution works, but it's not quite idiomatic."</p>
        <p>"When the first function you call is dyadic, it can get a little trickier. For example, if you wanted to get all the integers between two numbers, you may try the following:"</p>
        <Editor example="+⊃∘(⇡-) 3 8"/> // Should fail
        <p>"As the style diagnostic tells you, there is a better way."</p>
        <p>"The "<Prim prim=On/>" modifier calls a function but keeps its first argument on top of the stack. This can be used in both of the above examples."</p>
        <Editor example="÷⟜⇡ 5"/>
        <Editor example="+⟜(⇡-) 3 8"/>
        <p>"Having a single glyph for something that can be written as simply "<Prims prims=[Fork, Identity]/>" may seem unnecessary, but you'll find that because the pattern is so common, it makes code easier to both read and write."</p>
        <p>"The "<Prim prim=By/>" modifier is similar. Instead of keeping the first argument on top of the stack, it keeps the last argument below the function's outputs."</p>
        <p>"You can read more about these modifiers in the "<A href="/tutorial/evenmorestack">"Even More Stack Manipulation"</A>" tutorial."</p>
        <Editor example="÷⊸⧻ [1 2 3 4]"/>
        <Editor example="▽⊸> 5 [1 8 4 9 2 8 4]"/>

        <Hd id="even-more">"Even More Stack Manipulation"</Hd>
        <p>"But wait, there's more! For even more stack manipulation techniques, see the "<A href="/tutorial/evenmorestack">"Even More Stack Manipulation"</A>" tutorial."</p>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="moves the 4th value on the stack to the top"
            example="1 2 3 4"
            answer="⊃⋅⋅⋅∘⊙⊙∘"
            best_answer="⤙⊙⊙⊙◌"
            tests={&["@x [1 2 3] □5 27"]}
            hidden="3 3 3 4"/>

        <Challenge
            number=2
            prompt="adds the second argument to the third and divides by the first"
            example="2 3 5"
            answer="÷⊙+"
            tests={&["1 2 3", "5 10 15"]}
            hidden="2 3 4"/>

        <Challenge
            number=3
            prompt="finds both the sum and product of three arguments"
            example="4 5 6"
            answer="⊃(++|××)"
            tests={&["10 10 10", "1_2 3_4 5"]}
            hidden="[1 2 3] 4 [5 6 7]"/>

        <Challenge
            number=4
            prompt="collects 9 values from the stack evenly into 3 arrays"
            example="1 2 3 4 5 6 7 8 9"
            answer="∩₃⊟₃"
            tests={&["@G @o @o @d @  @j @o @b @!", "...×2..+1...5"]}
            hidden="1 2 3 4 5 6 7 8 9"/>

        <Challenge
            number=5
            prompt="for numbers A, B, C, and D calculates (A+C)×(B+D)"
            example="1 2 3 4"
            answer="×⊃(+⊙⋅∘|+⋅⊙⋅∘)"
            best_answer="×˜∩+"
            tests={&["10 ¯3 1 0", "3 ¯7 2 2"]}
            hidden="1_2 3_4 5_6 7"/>
    }
}

#[component]
fn TutorialTypes() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Types - {} Docs", lang())/>
        <h1>"Types"</h1>
        <p>"Every value in "{lang}" is an array. However, different arrays on the stack can have different "<em>"types"</em>" of items. Every element of an array is always the same type. Unlike some other array programming languages, "{lang}" arrays cannot have elements of different types."</p>
        <p>"There are four types of arrays:"</p>
        <ul>
            <li><strong>"Number"</strong></li>
            <li><strong>"Complex"</strong></li>
            <li><strong>"Character"</strong></li>
            <li><strong>"Box"</strong></li>
        </ul>

        <Hd id="numbers">"Numbers"</Hd>
        <p>"Numbers are decimal numbers with floating precision. They use the IEEE-754 double-precision floating-point format."</p>
        <Editor example="[5 6e3 e 0 3.2 3/4 ¯1.1 π ∞ 3π/2]"/>
        <p>"As you can see, numbers can be written as integers, with decimals, and as fractions. They can also contain numeric constants like "<code>"π"</code>", "<code>"e"</code>", and "<code>"∞"</code>"."</p>
        <p>"Most math operations can only be applied to numbers."</p>
        <p>"In cases where a number with a fractional part has repeating decimals, or when floating-point errors create tiny differences, the number will be shown with repeated decimal digits replaced by a "<code>"…"</code>"."</p>
        <Editor example="1/3\n1/12\n1/24\n+ 0.1 0.2"/>
        <p>"Even though numbers can have a fractional part, many built-in functions require whole numbers. These functions will return an error if given a non-whole number."</p>
        <p>"One such example is "<Prim prim=Pick/>"."</p>
        <Editor example="⊡ 2 [4 7 9 1 0]"/>
        <Editor example="⊡ 3.1 [4 7 9 1 0]"/> // Should fail
        <p>"If you want to convert a number to a whole number, you can use "<Prim prim=Floor/>", "<Prim prim=Ceil/>", or "<Prim prim=Round/>"."</p>

        <Hd id="complex-numbers">"Complex Numbers"</Hd>
        <p>"Complex numbers can be created with the "<Prim prim=Complex/>" function."</p>
        <Editor example="ℂ 3 5"/>
        <Editor example="ℂ [1 2 3] [4 5 6]"/>
        <p>"While complex numbers support all the same math operations as normal numbers, they are a distinct type and cannot be used in place of normal numbers."</p>
        <p>"You can convert a complex number to a normal number with "<Prim prim=Abs/>"."</p>
        <Editor example="⌵ ℂ3 4"/>
        <p>"You can normalize a complex number to a unit vector with "<Prim prim=Sign/>"."</p>
        <Editor example="± ℂ3 4"/>
        <p><Prim prim=Sqrt/>" only returns a complex number if it is called on a complex number. Beware of floating-point errors."</p>
        <Editor example="√  ¯4\n√ℂ0¯4"/>
        <p>"See "<Prim prim=Complex/>"'s docs for more details."</p>
        <p>"Comparing complex numbers for equality returns a normal number."</p>
        <Editor example="= i ℂ0 1\n= i ℂ1 1"/>
        <p>"Comparing complex numbers for order returns a component-wise comparison."</p>
        <Editor example="< i ℂ¯1 1\n≥ i ℂ1 1"/>
        <p>"In cases where a complex array has no elements with an imaginary part, it will be displayed in output with a "<code>"ℂ"</code>" marker."</p>
        <Editor example="ℂ0 5\nℂ0 [1 2 3]\nℂ0 [1_2 3_4]"/>
        <p>"Complex numbers can be written as literals by suffixing the real part with "<code>"r"</code>" and/or the imaginary part with "<code>"i"</code>"."</p>
        <Editor example="[3r4i 5r2i 2ri i/5 rπi]"/>

        <Hd id="characters">"Characters"</Hd>
        <p>"Characters are represented as 32-bit Unicode codepoints."</p>
        <p>"Character literals, denoted with a preceding "<code>"@"</code>", create rank 0 (scalar) character arrays."</p>
        <Editor example="@a @b"/>
        <Editor example="[@u @i @u @a]"/> // Should fail
        <p>"Characters like newline or null need to be escaped with "<code>"\\"</code>", but spaces do not."</p>
        <Editor example="@\\r @\\0 @ "/>
        <p>"If you don't like the significant whitespace of "<code>"@ "</code>", "<code>"@\\s"</code>" is also space."</p>
        <p>"As noted in the advice diagnostic above, string literals, delimited by "<code>"\""</code>"s, create rank-1 character arrays."</p>
        <Editor example="△.\"Hello, World!\""/>
        <p>"You can make raw strings, which do not require escaping, with a "<code>"$"</code>" followed by a space."</p>
        <p><Prim prim=Sys(SysOp::Print)/>" pretty-prints a value."</p>
        <Editor example="&p $ \"How are you?\" she asked."/>
        <p>"Raw strings that follow each other form multi-line strings."</p>
        <Editor example="$ Hello\n$ World!"/>
        <p>"This style of string is useful when your string contains a lot of quotes that you don't want to escape."</p>
        <Editor example="$ And then she was like, \"No way!\"\n$ And I was like, \"Way...\""/>
        <p>"Characters in character or string literals can also be specified with 2 or 4 hex digits by using escape codes "<code>"\\x"</code>" and "<code>"\\u"</code>" respectively."</p>
        <Editor example="\"\\x41\\x42\\x43\""/>
        <Editor example="@\\u2665"/>
        <p>"Longer (or shorter) sequences can be specified between "<code>"{}"</code>"s after a "<code>"\\u"</code>"."</p>
        <Editor example="@\\u{1f600}"/>
        <p>"Note that these escape sequences do not work in raw strings."</p>
        <br/>

        <Hd id="character-arithmetic">"Character Arithmetic"</Hd>
        <p>"Characters and numbers exist in an "<a href="https://en.wikipedia.org/wiki/Affine_space">"affine space"</a>", the same as in "<a href="https://mlochbaum.github.io/BQN/doc/arithmetic.html#character-arithmetic">"BQN"</a>"."</p>
        {
            let number = || view!(<span class="number-literal">"number"</span>);
            let character = || view!(<span class="string-literal-span">"character"</span>);
            view! {
                <p>"You can "<Prim prim=Add/>" "{number}"s and "{character}"s to get another "{character}"."</p>
                <p>"You can "<Prim prim=Sub/>" a "{number}" from a "{character}" to get another "{character}"."</p>
                <p>"You can "<Prim prim=Sub/>" two "{character}"s to get a "{number}"."</p>
                <p>"You can "<Prim prim=Mul/>" or "<Prim prim=Div/>" a "{character}" by a "{number}" to possibly toggle its case."</p>
                <p><em>"No"</em>" other dyadic arithmetic operations can be done on "{character}"s."</p>
            }
        }
        <Editor example="+1 @a"/>
        <Editor example="-8 \"Uiua\""/>
        <Editor example="-@a @z"/>
        <Editor example="× [1 ¯5 0 ¯2] \"uiua\""/>
        <Editor example="+@a @b"/> // Should fail
        <p><Prim prim=Sign/>" gives the case of a character. It gives "<code>"1"</code>" for uppercase, "<code>"¯1"</code>" for lowercase, and "<code>"0"</code>" for caseless characters."</p>
        <Editor example="± \"Hello, World!\""/>
        <p><Prim prim=Abs/>" uppercases a character."</p>
        <Editor example="⌵ \"Hello, World!\""/>
        <p><Prim prim=Neg/>" toggles the case of a character."</p>
        <Editor example="¯ \"Hello, World!\""/>
        <p>"Use "<Prim prim=Neg/>" and "<Prim prim=Abs/>" together to lowercase a character."</p>
        <Editor example="¯⌵ \"Hello, World!\""/>

        <Hd id="boxes">"Boxes"</Hd>
        <p>"Boxes are containers that can wrap an array of any type or shape. Multiple boxes can be put in the same array, no matter their contents."</p>
        <p>"Boxes can be created either by using the "<Prim prim=Box/>" function or with boxing array notation between "<code>"{}"</code>"s."</p>
        <Editor example="□5"/>
        <Editor example="□[1 2 3]"/>
        <Editor example="□\"Hello!\""/>
        <Editor example="{\"cat\" 5}"/>

        <Hd id="type-agreement">"Type agreement"</Hd>
        <p id="type-agreement">"For functions that work on the structure of arrays rather than their values, the types of the arrays must match."</p>
        <Editor example="⊂ 1_2 3"/>
        <Editor example="⊟ \"Hello\" \"World\""/>
        <Editor example="⊟ 1_2_3 \"dog\""/> // Should fail
        <p>"There is an exception for boxes. Any box can be put in an array with a non-box. In this case, the non-box will be "<Prim prim=Box/>"ed first."</p>
        <Editor example="⊟ 5 □[1 2 3]"/>

        <Hd id="empty-arrays">"Empty Arrays"</Hd>
        <p>"The type of an array that is constructed with no elements depends on the syntax used to construct it. Its shape is always "<code>"[0]"</code>"."</p>
        <p>"We can use the "<Prim prim=Type/>" function to get the type of an array. "<code>"0"</code>" corresponds to real numbers, "<code>"1"</code>" to characters, "<code>"2"</code>" to boxes, and "<code>"3"</code>" to complex numbers."</p>
        <Editor example="type []"/>
        <Editor example="type \"\""/>
        <Editor example="type {}"/>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="increments the first character of a string"
            example="\"`rray\""
            answer="⊂⊃(+1⊢|↘1)"
            best_answer="⍜⊢+₁"
            tests={&["\"Xou're\"", "\"coing\"", "\"freat!\""]}
            hidden="\"abc\""/>
    }
}

#[component]
fn TutorialInverses() -> impl IntoView {
    view! {
        <Title text=format!("Inverses - {} Docs", lang())/>

        <Markdown src="/text/inverses.md"/>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="adds 100 to the 2nd and 4th rows of an array"
            example="[1 2 3 4 5]"
            answer="⍜(⊏1_3|+100)"
            tests={&["↯4_3⇡12", "⊚10"]}
            hidden="↯4_3_4⇡24"/>

        <Challenge
            number=2
            prompt="transposes an array so that the last axis becomes the first"
            example="[[1_2_3 4_5_6] [7_8_9 10_11_12]]"
            answer="°⍉"
            tests={&["↯2_4⇡8", "↯2_2_4⇡16"]}
            hidden="↯2_2_2_2⇡16"/>

        <Challenge
            number=3
            prompt="multiplies the first column of a matrix by 10"
            example="[1_2 3_4]"
            answer="≡⍜⊢(×10)"
            best_answer="⍜(⊢⍉|×10)"
            tests={&["+1↯3_3⇡9", "↯2_4 1"]}
            hidden="[1_2]"/>
    }
}

#[component]
fn TutorialControlFlow() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Control Flow - {} Docs", lang())/>
        <h1>"Control Flow"</h1>
        <p>{lang}", and array languages in general, require much less control flow than other programming languages. Most operations that would be loops in other languages are simply operations on arrays. Because boolean operations return numbers, a lot of checks that would be done with "<code>"if"</code>" statements in other languages become mathematical or indexing operations in array languages."</p>
        <p>"For example, if you wanted to split an array of numbers into an array of odds and an array of evens, you might do it like this in a language like Python:"</p>
        <code class="code-block">"\
def splitArray(array):
    even = []
    odd = []
    for i in array:
        if i % 2 == 0:
            even.append(i)
        else:
            odd.append(i)
    return even, odd

splitArray([1, 2, 3, 7, 2, 4, 5])"</code>
        <p>"In "{lang}", it is much simpler, and there are no "<code>"if"</code>"s or "<code>"for"</code>"s to be found:"</p>
        <Editor example="F ← ∩▽¬◡⊙∘⊸◿2\nF [1 2 3 7 2 4 5]"/>
        <p>"That being said, not every problem lends itself to array operations. "{lang}" has a few methods for handling such cases."</p>

        <Hd id="repeat-do">"Looping with "<Prim prim=Repeat/>" and "<Prim prim=Do/></Hd>
        <p>"The "<Prim prim=Repeat/>" modifier takes a function and a number and calls the function that many times."</p>
        <p>"Here, we "<Prim prim=Mul/>" a number by "<code>"2"</code>", "<code>"10"</code>" times."</p>
        <Editor example="⍥(×2)10 5"/>
        <Editor example="⍥/+2 ↯3_3⇡9"/>
        <p>"If the function has more outputs than arguments, the extra outputs are collected into arrays."</p>
        <p><Prim prim=Repeat/><Prim prim=Rand/>" is a common pattern for generating a list of random numbers."</p>
        <Editor example="⁅⍥⚂5"/>
        <p><Prim prim=Repeat/>" is also useful for conditionally calling a function. Because booleans in "{lang}" are just numbers, "<Prim prim=Repeat/>"ing with a boolean value will call a function "<code>"0"</code>" or "<code>"1"</code>" times."</p>
        <Editor example="F ← ⍥(×10)<10.\nF 5\nF 12"/>
        <p><Prim prim=Repeat/>"'s glyph is a combination of a circle, representing a loop, and the 𝄇 symbol from musical notation."</p>
        <p>"The "<Prim prim=Do/>" modifier takes a loop function and a condition function. It repeatedly calls the loop function as long as the condition function returns "<code>"1"</code>"."</p>
        <Editor example="⍢(×2|<1000) 1"/>
        <Editor example="◌⍢(⊃(×2)⊂|<100) 1 []"/>
        <p>"While "<Prim prim=Do/>" is very powerful, it should only be used when necessary."</p>
        <p><Prim prim=Do/>" is the only way to do an infinite loop in "{lang}". To do so, simply use "<code>"1"</code>" as the condition function."</p>

        <Hd id="try">"Catching errors with "<Prim prim=Try/></Hd>
        <p>"The "<Prim prim=Try/>" modifier takes two functions. If the first function throws an error, the second function is called to handle it."</p>
        <p>"The function must have the same number of outputs."</p>
        <p>"The handler function can take at most 1 more agument that the first function."</p>
        <p>"We can see how this works by using it with "<Prim prim=Parse/>"."</p>
        <p>"If the handler function has 0 arguments, then it is simply called. This is a nice way to provide default values in the event of a failure."</p>
        <Editor example="⍣⋕0 \"5\"\n⍣⋕0 \"dog\""/>
        <p>"If the handler function has 1 argument, then the original argument is passed to it."</p>
        <Editor example="⍣⋕∘ \"5\"\n⍣⋕∘ \"dog\""/>
        <p>"If the handler function takes 1 more argument than the first function, then the error is also passed to it."</p>
        <Editor example="⍣⋕{⊙∘} \"5\"\n⍣⋕{⊙∘} \"dog\""/>
        <p>"You can read about more uses of "<Prim prim=Try/>" in its documentation."</p>

        <Hd id="switch"><Prim prim=Switch/></Hd>
        <p>"The "<Prim prim=Switch/>" modifier uses a selector to choose one of its functions to call."</p>
        <Editor example="⨬(3)(5) 0\n⨬(3)(5) 1"/>
        <p>"The selector goes above the arguments on the stack."</p>
        <p>"Here, we "<Prim prim=Add/>" if the selector is "<code>"0"</code>" and "<Prim prim=Sub/>" if the selector is "<code>"1"</code>"."</p>
        <Editor example="⨬+- 0 3 5\n⨬+- 1 3 5"/>
        <p>"Non-scalar selectors are allowed. They allow a different function to be evaluated for each row of the input array(s)."</p>
        <Editor example="⨬+- [1 0 1] [1 2 3] [4 5 6]"/>
        <p><Prim prim=Switch/>" can use a "<A href="/tutorial/morestack#function-packs">"function pack"</A>" to select from more functions."</p>
        <Editor example="⨬(+|-|×|÷) [1 2 0 3] [...2] [...5]"/>
        <Editor example="⨬(×10|+1|⨬¯∘ =2.) ◿3. [2 9 4 0 8 3]"/>
        <p>"With "<Prim prim=IndexOf/>", "<Prim prim=Switch/>" can be used to implement behavior similar to "<code>"switch"</code>" statements in other languages."</p>
        <Editor example="F ← (\n  ⊗□⊙{\"foo\" \"bar\" \"baz\"}\n  ⨬(+1|×10|÷2|¯)\n)\nF \"foo\" 5\nF \"bar\" 5\nF \"baz\" 5\nF \"wow\" 5"/>
        <p>"Each branch can have a signature specified. For the overall "<Prim prim=Switch/>" to have a valid signature, all branches must either change the height of the stack by the same amount "<em>"or"</em>" return the same number of outputs."</p>
        <Editor example="F ← ⨬(|2 ×||3.2 ⊃(++)×)\n[F 0 2 3 4]\n[F 1 2 3 4]"/>
        <p>"Signatures in "<Prim prim=Switch/>" functions are a bit messy, so try to avoid them when possible."</p>
        <p>"Because a second "<code>"|"</code>" immediately after another indicates a signature, branches that do nothing must contain "<Prim prim=Identity/>"."</p>
        <Editor example="F ← ⨬(+5|∘|÷10)/+⊸⊞>5_10\n[F2 F6 F200]\nF[2 6 200]"/>

        <Hd id="recursion">"Recursion"</Hd>
        <p>"A bound function that refers to its own name is a "<a href="https://en.wikipedia.org/wiki/Recursion_(computer_science)">"recursive function"</a>". A function that calls itself can easily recurse infinitely, so it is important to have a "<em>"base case"</em>" that stops the recursion when a condition is met. Switch functions are great for this."</p>
        <p>"As a simple example, here is a function that calculates the factorial of a number. Note that you should not actually do this, as "<Prims prims=[Reduce, Mul, Add]/><code>"1"</code><Prims prims=[Range]/>" is shorter, faster, and more idiomatic."</p>
        <Editor example="Fact ← |1 ⨬(×Fact-1.|1)<2.\nFact 5"/>
        <p>"The base case is when the input is "<code>"1"</code>". In this case, the function returns "<code>"1"</code>". Otherwise, it multiplies the input by the result of calling itself with the input decremented by "<code>"1"</code>"."</p>
        <p>"Recursive functions are required to have signatures declared."</p>
        <p>"Recursion is only recommended if a particular problem "<em>"really"</em>" calls for it. Recursion in "{lang}" can be slow, and there is a limit to how deep you can recur."</p>
        <p>"It is usually better to use either array-based methods or iteration with "<Prim prim=Repeat/>" or "<Prim prim=Do/>"."</p>

        <Hd id="assert"><Prim prim=Assert/></Hd>
        <p>"The "<Prim prim=Assert/>" function takes any value and a condition. If the condition is anything but "<code>"1"</code>", the value is thrown as an error that can be caught with "<Prim prim=Try/>"."</p>
        <Editor example="F ← ⍣(¯⍤10≤10.)⋅∘\nF 5\nF 12"/>
        <p>"If the "<Prim prim=Assert/>"ed value is never caught, it becomes an error."</p>
        <Editor example="F ← ¯⍤\"Too big!\"≤10.\nF 5\nF 12"/> // Should fail
        <p>"The error message above may not be useful to the user, as it refers to the code of the function itself. You can use the "<code>"# Track caller!"</code>" semantic comment to refer to the call site instead."</p>
        <Editor example="F ← ¯⍤\"Too big!\"≤10. # Track caller!\nF 5\nF 12"/> // Should fail
        <p>"You can read more about "<code>"# Track caller!"</code>" "<A href="/tutorial/documentation#track-caller">"here"</A>"."</p>
        <p>"Using "<Prim prim=Assert/>" for this purpose will be covered more in the "<A href="/tutorial/testing">"section on testing"</A>"."</p>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="pushes \"small\" if a number is less than 10, \"medium\" if it is less than 100, and \"large\" otherwise"
            example="17"
            answer=r#"⨬("small"|"medium"|"large")/+≥[10 100]"#
            tests={&["3", "50", "2357"]}
            hidden="10"/>

        <Challenge
            number=2
            prompt="multiplies an array by its reverse until any element is greater than 1000"
            example="[1.5 8 2]"
            answer="⍢(×⇌.|≤1000/↥)"
            tests={&["[1 2 3]", "[¯6 5 1]"]}
            hidden="7"/>
    }
}

#[component]
fn TutorialPatternMatching() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Pattern Matching - {} Docs", lang())/>
        <h1>"Pattern Matching"</h1>
        <p>{lang}" has a powerful mechanism for matching patterns in arrays to conditionally extract data."</p>

        <Hd id="un-patterns"><Prim prim=Un/>" Patterns"</Hd>
        <p><Prim prim=Un/>" can be applied to a constant value to form a function that throws an error if the top value on the stack does not match the constant."</p>
        <Editor example="°5 5"/>
        <Editor example="°5 3"/> // Should fail
        <p>"This works for arrays as well."</p>
        <Editor example="°[1 2 3] [1 2 3]"/>
        <Editor example="°[1 2 3] [4 5 6]"/> // Should fail
        <p>"This is not very useful on its own, but it can be composed with other inverses to form more complex patterns."</p>
        <p>"A primary pattern of note is using stack array notation with planet notation to form patterns that match arrays with certain values and extract the others."</p>
        <Editor example="°[1⊙3] [1 2 3]"/>
        <Editor example="°[1⊙3] [4 5 6]"/> // Should fail
        <p>"These can be arbitrarily nested."</p>
        <Editor example="°[1 2⊙⊙(5∘)] [1 2 3 4 5 6]"/>
        <p><Prim prim=Un/><Prim prim=Join/>" with a constant can also be used to match arrays with a certain prefix."</p>
        <Editor example="°(⊂1) [1 2 3]"/>
        <Editor example="°(⊂1) [4 5 6]"/> // Should fail
        <Editor example="°(⊂1_2) [1 2 3]"/>
        <p>"To match a suffix, you can use "<Prim prim=Backward/>"."</p>
        <Editor example="°(˜⊂3) [1 2 3]"/>

        <Hd id="with-try">"Matching multiple patterns with "<Prim prim=Try/></Hd>
        <p>"Single patterns are of limited usefulness on their own. Because they throw errors when matching fails, you can attempt to match additional patterns using "<Prim prim=Try/>"."</p>
        <p><Prim prim=Try/>" accepts arbitrarily long function packs, so you can match as many patterns as you want in a simple way."</p>
        <p>"In this example, we run different code depending on which pattern matches."</p>
        <Editor example="F ← ⍣(×10°[1⊙3]|°(⊂5)|⇌)\nF [5 6 7]\nF [1 2 3]\nF \"abc\""/>
        <p>"Having more or longer patterns may be easier to read if each pattern gets its own line."</p>
        <Editor example="F ← ⍣(\n  ×10 °[1⊙3]\n| °(⊂5)\n| ⇌\n)"/>

        <Hd id="format-string-patterns">"Format String Patterns"</Hd>
        <p><Prim prim=Un/>" works with format strings to extract substrings where the "<code>"_"</code>"s are. While the "<Prim prim=Regex/>" function is available, it is often more complex than is necessary. In these cases, format string patterns are more appropriate."</p>
        <Editor example="°$\"_, _, _\" \"1, 2, 3\""/>
        <Editor example="°$\"_, _, _\" \"1, 2, 3, 4, 5\""/>
        <Editor example="°$\"Hello, _!\" \"Hello, World!\""/>
        <p>"Multiline format strings can be inverted as well."</p>
        <Editor example="\"Hello\\nWorld!\"\n°$$ Hello\n $$ _!"/>
        <p>"Inverting the "<Prim prim=Reduce/>" of a dyadic format string will split by a delimiter."</p>
        <Editor example="°/$\"_ - _\" \"a - bcd - ef\""/>
        <p>"More precisely, format string patterns form a regex that replaces all "<code>"_"</code>"s from the format string with "<code>"(.+?|.*)"</code>", where "<code>"."</code>" also matches newlines."</p>

        <Hd id="case">"Propagating errors with "<Prim prim=Case/></Hd>
        <p>"Consider a function which attempts to match multiple patterns. After a pattern matches, each branch has some code to run."</p>
        <p>"For example, this function attempts to parse a couple different expected string formats, then "<Prim prim=Parse/>"s and "<Prim prim=Select/>"s the result in some way."</p>
        <Editor example="F ← ⍣(  ⊏⋕ °$\"_: _\"| ˜⊏⊙⋕ °$\"_ - _\"| ∘)\nF \"1: abc\"\nF \"def - 2\""/>
        <p>"But what happens if we give an input that matches the pattern but fails elsewhere in the branch?"</p>
        <Editor example="F ← ⍣(⊏⋕ °$\"_: _\"|˜⊏⊙⋕ °$\"_ - _\"|∘)\nF \"r: xyz\"  # Can't parse\nF \"ghi - 3\" # Out of bounds"/>
        <p>"In both those cases, a pattern match succeeds, but either the "<Prim prim=Parse/>" or "<Prim prim=Select/>" fails. This causes the "<Prim prim=Try/>" to move on to the next branch silently, causing what may be unexpected behavior!"</p>
        <p>"The "<Prim prim=Case/>" modifier can be used to make the branch fail properly. "<Prim prim=Case/>" simply calls its function. However, in the event that the function errors, the error can escape a single "<Prim prim=Try/>"."</p>
        <p>"Wrapping the code after a pattern match in "<Prim prim=Case/>" will make the error propagate properly."</p>
        <Editor example="F ← ⍣(\n  ⍩(⊏⋕) °$\"_: _\"\n| ⍩(˜⊏⊙⋕) °$\"_ - _\"\n| ∘)\nF \"ghi - 3\""/> // Should fail

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt=view!("removes a leading "<code>"0"</code>" from an array of numbers or prepends a "<code>"0"</code>" if it is missing")
            example="[0 1 2 3]"
            answer="⍣°(⊂0)(⊂0)"
            tests={&["[4 0 9]", "[0 0 3 4]"]}
            hidden="[0 0 0 0]"/>

        <Challenge
            number=2
            prompt=view!("splits a string on the first "<code>"-"</code>" and returns the two parts")
            example="\"hello-world\""
            answer="°$\"_-_\""
            tests={&["\"foo-bar\"", "\"1-2-3\""]}
            hidden="\"-\""/>

        <Challenge
            number=3
            prompt=view!("matches the argument against string prefixes "<code>"a"</code>", "<code>"bc"</code>", or "<code>"def"</code>" and adds "<code>"1"</code>", "<code>"2"</code>", or "<code>"3"</code>" to the second argument respectively, or adds "<code>"10"</code>" otherwise")
            example="\"definate\" 5"
            answer="⍣(+1 ◌°$\"a_\"|+2 ◌°$\"bc_\"|+3 ◌°$\"def_\"|+10 ◌)"
            tests={&["\"abc\" 1", "\"bcause\" [1 2 3]"]}
            hidden="\"wow\" 4"/>
    }
}

#[component]
fn TutorialMoreArray() -> impl IntoView {
    title_markdown(
        "More Array Manipulation",
        "/text/more_array.md",
        view! {
            <Hd id="challenges">"Challenges"</Hd>

            <Challenge
                number=1
                prompt="adds the first argument list to each row of the second argument matrix"
                example="1_2_3 [4_5_6 7_8_9]"
                answer="+¤"
                tests={&["10_20 ↯4_2⇡8", "\"Wow\" ¯[10_0_10 19_14_19]"]}
                hidden="1_2 [3_4]"/>

            <Challenge
                number=2
                prompt="joins the first argument to each list in the second argument"
                example="0 +1°△3_4"
                answer="≡₁⌞⊂"
                tests={&["0 [1 2 3]", r#"@| ⬚@\s[["Hey""there""buddy"] [@a "bc" "def"]]"#, "η_π_τ ⇡2_2_2"]}
                hidden="3 5"/>
        }.into_view(),
    )
}

#[component]
fn TutorialThinkingWithArrays() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Thinking with Arrays - {} Docs", lang())/>
        <h1>"Thinking with Arrays"</h1>
        <p>"So far, we've covered the mechanics of working with arrays in "{lang}". However, if you are new to the array paradigm, it may not be clear how to use arrays to solve problems."</p>
        <p>"This section covers some of the common functions and modifiers that pop up when solving many different problems."</p>

        <Hd id="masks-and-keep">"Masks and "<Prim prim=Keep/></Hd>
        <p>"Many languages have some sort of "<code>"filter"</code>" function that takes a predicate and a list and returns a list of all the elements that satisfy the predicate. In array languages, we take a different approach."</p>
        <p>"First, we create a "<em>"mask"</em>" array. A mask array is an array of "<code>"0"</code>"s and "<code>"1"</code>"s where "<code>"1"</code>"s represent the rows that satisfy the predicate. For pervasive functions, this is extremely simple."</p>
        <p>"For example, if we wanted to create a mask of all numbers greater that 4, we simply treat the whole array as a single unit."</p>
        <Editor example=">4. [2 8 3 9 1 7 2]"/>
        <p>"The "<Prim prim=Keep/>" function takes a mask array and an array and returns an array of all the rows that have a "<code>"1"</code>" in the mask. This is essentially a filter."</p>
        <Editor example="▽ >4. [2 8 3 9 1 7 2]"/>
        <p><Prim prim=Keep/>" also works with "<Prim prim=Under/>" so that you can modify the rows that have a "<code>"1"</code>" in the mask."</p>
        <Editor example="⍜▽(×10) >4. [2 8 3 9 1 7 2]"/>
        <p><Prim prim=Keep/>" has a few other use cases with non-masks. See its documentation for more."</p>

        <Hd id="where"><Prim prim=Where/></Hd>
        <p>"The "<Prim prim=Where/>" function converts a mask array into an array of indices where the mask is "<code>"1"</code>"."</p>
        <Editor example="⊚. >4. [2 8 3 9 1 7 2]"/>
        <p>"This works with multi-dimensional arrays as well."</p>
        <Editor example="⊚. >4. [2_8_3 9_1_7]"/>
        <p><Prim prim=Un/><Prim prim=Where/>" converts an array of indices into a mask array."</p>
        <Editor example="°⊚ [3 9 5 8]"/>
        <Editor example="°⊚ [1_2 3_4]"/>
        <p><Prim prim=Select/><Prim prim=Where/>" is equivalent to "<Prim prim=Keep/>" (at least for boolean predicates)."</p>
        <Editor example="⊏⊚ =0◿2. [2 8 3 9 1 7 2]"/>
        <Editor example="▽  =0◿2. [2 8 3 9 1 7 2]"/>

        <Hd id="scan"><Prim prim=Scan/></Hd>
        <p>"The "<Prim prim=Scan/>" modifier is similar to "<Prim prim=Reduce/>", but it returns an array of all the intermediate results."</p>
        <Editor example="/+ [1 2 3 4]\n\\+ [1 2 3 4]"/>
        <p>"This can be useful when used on a mask."</p>
        <p>"For example, if we wanted to get the first word of a string, we could start by creating a mask of all the non-space characters."</p>
        <p>"Then we can use "<Prim prim=Scan/><Prim prim=Mul/>" to zero the mask after the first word."</p>
        <p>"Finally, we can use "<Prim prim=Keep/>" to apply the mask and get the first word."</p>
        <p>"Use the arrows to see how the mask changes."</p>
        <Editor example=r#"▽ \× ≠@ . "What's the first word?""#/>

        <Hd id="fill"><Prim prim=Fill/></Hd>
        <p>"Recall that the "<Prim prim=Fill/>" modifier sets a \"fill value\" that can be used by certain functions."</p>
        <p>"One common use is to set a default value that will be used when the shapes of arrays do not match."</p>
        <Editor example="⬚0+ 10_20 3_4_5_6"/>
        <p>"For example, if you wanted to logical OR two masks with different shapes, you could use "<Prim prim=Fill/>" with a different fill value depending on what you want to do with the mismatched parts."</p>
        <Editor example="⬚0↥ 1_0_0_1_0 0_1_0\n⬚1↥ 1_0_0_1_0 0_1_0"/>
        <p>"Another interesting use is a "<Prim prim=Fill/>"ed "<Prim prim=Rotate/>". Instead of wrapping values around, it fills in one side of the array with the fill value."</p>
        <Editor example="  ↻¯2 [1 2 3 4 5]\n⬚0↻¯2 [1 2 3 4 5]"/>

        <Hd id="partition"><Prim prim=Partition/></Hd>
        <p><Prim prim=Partition/>" is a powerful modifier that splits up an array based on a list of consecutive keys. Before explaining it further, let's look at a simple example of a very common use case: splitting a string into words."</p>
        <Editor example=r#"⊜□ ≠@ . "Look at that!""#/>
        <p>"First, we create a mask of all the non-space characters. Then, "<Prim prim=Partition/>" calls "<Prim prim=Box/>" on each section of the string that corresponds to a run of "<code>"1"</code>"s in the mask."</p>
        <p>"Here is another example using "<Prim prim=Partition/><Prim prim=Box/>" with the inputs explicitly defined."</p>
        <Editor example="[1 2 3 4 5 6 7 8]\n[1 1 0 5 6 6 0 1]\n⊜□"/>
        <p>"Notice that "<code>"0"</code>"s in the keys array cause the corresponding sections of the input array to be skipped, so "<code>"3"</code>" and "<code>"7"</code>" are omitted from the output."</p>
        <p>"We use "<Prim prim=Box/>" here because the resulting sections have different lengths. If we expect the sections to have the same lengths, we can use "<Prim prim=Identity/>" instead."</p>
        <Editor example="[1 2 3 4 5 6 7 8]\n[1 1 2 2 0 0 3 3]\n⊜∘"/>
        <p><Prim prim=Partition/>" is very useful when working with strings. See the "<A href="/tutorial/strings">"Strings tutorial"</A>" for more."</p>
        <p>"A hint for one of the challenges below: "<Prim prim=Partition/>" works with "<Prim prim=Under/>"!"</p>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="negates each number in a list that is not a multiple of 3"
            example="[1 2 3 4 5 6 7 8 9]"
            answer="⍜▽¯ ≠0⊸◿3"
            tests={&["[3 0 1 8]", "[3 6 9 12 15 18 21 25 27]"]}
            hidden="[3 6 9 4]"/>

        <Challenge
            number=2
            prompt="returns the last word of a string"
            example=r#""What's the last word?""#
            answer=r#"▽ ⍜⇌\× ⊸≠@ "#
            tests={&[r#""Um, I um, arrays""#, r#""I like trains""#]}
            hidden=r#"Wow"#/>

        <Challenge
            number=3
            prompt="for every multiple of 3 in a list, multiplies the following number by 10"
            example="[1 2 3 4 5 6 7]"
            answer="⍜▽(×10) ⬚0↻¯1 =0⊸◿3"
            tests={&["[2 9 3 8 7 1]", "[3 3 3 3]"]}
            hidden="[]"/>

        <Challenge
            number=4
            prompt="given a matrix of 0s an 1s, only keeps the 1s that have even x and y coordinates"
            example="[1_1_0 0_1_1 0_1_1]"
            answer="⍜⊚(▽≡/×¬⊸◿2)"
            tests={&["↯3_4 1_0_1", "↯4_4 1_0_0_1_0"]}
            hidden="[1_1 1_1]"/>

        <Challenge
            number=5
            prompt="reverses each word in a string but keeps the words in the same order"
            example=r#""get in the racecar""#
            answer="⍜⊜□≡⇌ ⊸≠@ "
            tests={&[r#""arrays are neat""#, r#""wow mom""#]}
            hidden=r#""Wow, mom!""#/>
    }
}

#[component]
fn TutorialMacros() -> impl IntoView {
    view! {
        <Title text=format!("Macros - {} Docs", lang())/>

        <Markdown src="/text/macros.md"/>

        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="creates a macro called F! which calls its function on each row of an array, reverses each row, and reverses the whole array"
            example="F!(⊂.) ↯3_4⇡12"
            answer="F! ← ⇌≡(⇌^0)"
            default="F! ← ^0"
            flip=true
            tests={&["F!(↯3) [1_2_3 4_5_6]", "F!(⊟.) 1_2 3_4"]}
            hidden="5"/>

        <Challenge
            number=2
            prompt="creates a macro called F‼ which calls its first function, then its second, then its first again"
            example="F‼⇌(⊂10) [1 2 3]"
            answer="F‼ ← ^0^1^0"
            default="F‼ ← ^0"
            flip=true
            tests={&["F‼⇌⍉ [1_2 3_4]", "F‼⊂⇌ 1_2 3_4 5_6"]}
            hidden="5"/>
    }
}

#[component]
fn TutorialTacitCode() -> impl IntoView {
    view! {
        { title_markdown("Tacit Code", "/text/tacit_code.md", View::default()).into_view() }

        <br/>
        <br/>
        <hr/>
        <p>"This is the end of the tutorial that pertains to writing "<em>"programs"</em>"."</p>
        <p>"If you want to use "{lang}" to write "<em>"software"</em>", then read on for the sections on modules and testing."</p>
        <p>"If you don't care about that stuff and want to learn more about the language, you can check out:"</p>
        <EndOfTutorialList/>
    }
}

#[component]
fn EndOfTutorialList() -> impl IntoView {
    view! {
        <ul>
            <li><A href="/docs#functions">"The list of all functions"</A></li>
            <li><A href="/docs#other-tutorials">"Other tutorials about more specific topics"</A></li>
            <li><A href="/docs#other-docs">"Other language topics"</A></li>
            <li>"The online "<A href="/pad">"pad"</A>" for writing longer code"</li>
        </ul>
    }
}

#[component]
fn TutorialModules() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Modules - {} Docs", lang())/>
        <h1>"Modules"</h1>
        <p>"Modules are a way to organize your code in "{lang}". They can either be defined in a scope, or imported from a file. Any "{lang}" file can be used as a module."</p>
        <p>"Modules can be compared to namespaces in other languages."</p>

        <Hd id="scoped-modules">"Scoped Modules"</Hd>
        <p>"Scoped modules are defined between a "<code>"┌─╴"</code>" and a "<code>"└─╴"</code>"."</p>
        <p>"Both delimiters format from "<code>"---"</code>"."</p>
        <p>" The "<code>"┌─╴"</code>" should be immediately followed by a name for the module. Module names follow the same rules as other bindings."</p>
        <p>"Names from inside the module can be referenced by following the module name with a "<code>"~"</code>"."</p>
        <Editor example="---Mod\n  A ← 5\n  F ← +1\n  G ← F F\n---\nMod~G Mod~A" help={&["", "Try formatting!"]}/>
        <p>"Bindings defined inside a scoped module are only visible inside the module."</p>
        <Editor example="┌─╴Mod\n  A ← 5\n  F ← +1\n  G ← F F\n└─╴\nG A"/> // Should fail
        <p>"Names from inside the module can be "<em>"made"</em>" visible by following the module name with a "<code>"~"</code>" and a list of the names to make visible."</p>
        <Editor example="┌─╴Mod ~ A G\n  A ← 5\n  F ← +1\n  G ← F F\n└─╴\nG A"/>
        <p>"Note that these names are brought into the module's parent scope as private bindings, so they cannot be referenced from outside the parent."</p>
        <Editor example="┌─╴Mod\n  ┌─╴SubMod ~ X\n    X ← 5\n  └─╴\n└─╴\nMod~X"/> // Should fail
        <p>"Names must be re-bound in the parent to be made visible."</p>
        <Editor example="┌─╴Mod\n  ┌─╴SubMod\n    X ← 5\n  └─╴\n  X ← SubMod~X\n└─╴\nMod~X"/>
        <p>"Names defined above the module can be referenced inside it."</p>
        <Editor example="B ← 5\n┌─╴Mod\n  C ← ×2 B\n└─╴\nMod~C"/>

        <Hd id="modules-as-functions">"Modules as Functions"</Hd>
        <p>"If a module defines a function called "<code>"Call"</code>" or "<code>"New"</code>", it can be called as a function. This can be useful if your module defines functions that all work on the same kind of data, like methods in some other languages."</p>
        <Editor example="┌─╴Foo\n  New ← {⊓$Bar$Baz}\n  Format ← /$\"_ _\"\n└─╴\nFoo \"Hi!\" 5\nFoo~Format ."/>

        <Hd id="module-import-macros">"Module Import Macros"</Hd>
        <p>"If the name of a module is referenced as a macro (with a trailing "<code>"!"</code>"), names defined in the module will be available in the macro's scope."</p>
        <p>"This is useful if you need to refer to a bunch of bindings from a module without having to prefix them with the module name."</p>
        <Editor example="┌─╴Foo\n  New ← {⊓$Bar$Baz}\n  Format ← /$\"_ _\"\n  Incr ← ⍜⊣(+1)\n└─╴\nFoo!(Format Incr New) \"Oh\" 10"/>

        <Hd id="web-files">"Files on the Website"</Hd>
        <p>"Using files as modules involves loading files from the file system."</p>
        <p>"This website has a virtual file system. You can write to virtual files with "<Prim prim=Sys(SysOp::FWriteAll)/>". You can also drag and drop files from your computer into the editor to make them available to import."</p>
        <p>"There is also a test module that can always be imported as "<code>"example.ua"</code>". Its contents is:"</p>
        <Editor example=EXAMPLE_UA/>

        <Hd id="importing">"Importing Modules"</Hd>
        <p>"Modules can be imported by file path with "<code>"~"</code>"."</p>
        <Editor example="~ \"example.ua\""/>
        <p>"This is not very useful on its own. We can bind items from the module in the current scope by listing them after the file path, separated by an additional "<code>"~"</code>"."</p>
        <Editor example="~ \"example.ua\" ~ Increment Square\n\nIncrement Square 3"/>
        <p>"If we have a lot of items to import, we can use multiple lines."</p>
        <Editor example="~ \"example.ua\"\n~ Increment Square\n~ Span\n~ Foo Bar\n\nIncrement Square Foo\nSpan 4 10"/>
        <p>"The formatter will automatically indent the imports if they are on multiple lines. It will also alphabetize them. Try it out!"</p>

        <Hd id="binding">"Binding Modules"</Hd>
        <p>"If we put a name before the import, we can bind the module to that name."</p>
        <p>"We can then reference items from that module anywhere using a "<code>"~"</code>"."</p>
        <Editor example="Ex ~ \"example.ua\"\n\nEx~Increment 10"/>
        <p>"This can be mixed and matched with the other import syntax."</p>
        <Editor example="Ex ~ \"example.ua\" ~ Increment Square\n\nEx~Double Square 3\nEx~Mac!×\nIncrement Ex~Bar"/>

        <Hd id="aliasing">"Aliasing Modules"</Hd>
        <p>"If you want to be able to refer to an item from a module with a different name, simply make a binding with the new name."</p>
        <Editor example="Ex ~ \"example.ua\"\nSqr ← Ex~Square\nSp ← Ex~Span\n\nSp⟜Sqr 3"/>
        <p>"These bindings will also get indented by the formatter if they immediately follow the import. Try formatting the above code!"</p>
        <p>"You can also re-bind the module itself."</p>
        <Editor example="Ex ~ \"example.ua\"\nLocalEx ← Ex\nLocalEx~Square 7"/>

        <Hd id="visibility">"Visibility"</Hd>
        <p>"All bindings in a module bound with the normal "<code>"←"</code>" arrow are public and can be used by importers of the module."</p>
        <p>"However, modules imported in modules, as well as their same-name imports (the names on lines that start with "<code>"~"</code>"), are private."</p>
        <p>"You may have noticed in the example file that one binding uses a special "<code>"↚"</code>" arrow. This indicates that the binding is private."</p>
        <p>"Private bindings cannot be accessed from outside the file in which they are defined."</p>
        <Editor example="~ \"example.ua\" ~ RangeDiff"/> // Should fail
        <p>"To enter this arrow, you can put a "<code>"~"</code>" after a binding's normal "<code>"←"</code>" or "<code>"="</code>"."</p>
        <p>"Try formatting the following example to see how this works."</p>
        <Editor example="A = +1\nB ← +2\nC =~ +3\nD ←~ +4"/>
        <Hd3 id="private-imports">"Private Imports"</Hd3>
        <p>"Imports can be made private by using a "<code>"≁"</code>" instead of the first "<code>"~"</code>". "<code>"≁"</code>" formats from "<code>"~~"</code>"."</p>
        <Editor example="# Try formatting!\n┌─╴M\n  Ex ~~ \"example\"\n└─╴\nM~Ex~Foo"/> // Should fail
        <Hd3 id="private-scoped-modules">"Private Scoped Modules"</Hd3>
        <p>"Scoped modules can be made private with special delimiters "<code>"┌╶╶"</code>" and "<code>"└╶╶"</code>". These format from the normal delimiters or "<code>"---"</code>"s followed by a "<code>"~"</code>"."</p>
        <Editor example="┌─╴A\n  ┌╶╶B\n    C ← 5\n  └╶╶\n└─╴\nA~B~C"/> // Should fail
        <Editor example = "# Try formatting!\n---A\n  ---~M\n    F = +1\n  ---\n---\nA~M~F 5"/> // Should fail
        <p>"The formatter will automatically change the closing delimiter to match its corresponding opening delimiter."</p>

        <Hd id="git-modules">"Git Modules"</Hd>
        <p>"Modules can be imported from Git repositories. Instead of a path, use a URL prefixed with "<code>"git:"</code>"."</p>
        <p>"The "{lang}" GitHub organization hosts an example module at "<a href="https://github.com/uiua-lang/example-module">"https://github.com/uiua-lang/example-module"</a>". The protocol specification can be omitted."</p>
        <Editor example="~ \"git: github.com/uiua-lang/example-module\" ~ Upscale\nUpscale 3 [1_2 3_4]"/>
        <p>"On the site, code is pulled from a "<code>"lib.ua"</code>" file at the root of the repository. Loading other files on the site is not supported."</p>
        <p>"To use Git modules in the "<A href="/docs/install">"native interpreter"</A>", you must have Git installed. The repository is cloned and the "<code>"lib.ua"</code>" file is loaded as the module's contents. Code from other files can be made available by importing them as modules in the "<code>"lib.ua"</code>" file."</p>
        <p>"The native interpreter also supports adding an additional "<code>"branch: <branch-name>"</code>" or "<code>"commit: <commit-hash>"</code>" specifier after the URL."</p>
        <p>"The "<code>"uiua module"</code>" command can be used to list or update Git modules."</p>
        <p>"You can find a curated list of "{lang}" modules "<a href="https://github.com/uiua-lang/uiua-modules">"here"</a>"."</p>
    }
}

#[component]
fn TutorialTesting() -> impl IntoView {
    view! {
        <Title text=format!("Testing - {} Docs", lang())/>
        <Markdown src="/text/testing.md"/>

        <br/>
        <br/>
        <hr/>
        <p>"Hooray! You've reached the end of the tutorial!"</p>
        <p>"To keep going with "{lang}", you can check out:"</p>
        <EndOfTutorialList/>
    }
}
