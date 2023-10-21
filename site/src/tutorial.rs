use std::fmt::Display;

use enum_iterator::{all, Sequence};
use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use uiua::{example_ua, primitive::Primitive, SysOp};

use crate::{editor::*, Prim, PrimCodes};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Sequence)]
pub enum TutorialPage {
    Basic,
    Math,
    Arrays,
    Types,
    Bindings,
    Functions,
    ControlFlow,
    AdvancedStack,
    AdvancedArray,
    CustomModifiers,
    Modules,
    Testing,
}

impl TutorialPage {
    pub fn path(&self) -> String {
        format!("{self:?}").to_lowercase()
    }
    pub fn title(&self) -> &'static str {
        match self {
            Self::Basic => "Basic Stack Operations and Formatting",
            Self::Math => "Math and Comparison",
            Self::Arrays => "Arrays",
            Self::Types => "Types",
            Self::Bindings => "Bindings",
            Self::Functions => "Modifiers and Functions",
            Self::ControlFlow => "Control Flow",
            Self::AdvancedStack => "Advanced Stack Manipulation",
            Self::AdvancedArray => "Advanced Array Manipulation",
            Self::CustomModifiers => "Custom Modifiers",
            Self::Modules => "Modules",
            Self::Testing => "Testing",
        }
    }
}

#[component]
pub fn Tutorial(page: TutorialPage) -> impl IntoView {
    let tut_view = match page {
        TutorialPage::Basic => TutorialBasic().into_view(),
        TutorialPage::Math => TutorialMath().into_view(),
        TutorialPage::Arrays => TutorialArrays().into_view(),
        TutorialPage::Types => TutorialTypes().into_view(),
        TutorialPage::Bindings => TutorialBindings().into_view(),
        TutorialPage::Functions => TutorialFunctions().into_view(),
        TutorialPage::ControlFlow => TutorialControlFlow().into_view(),
        TutorialPage::AdvancedStack => TutorialAdvancedStack().into_view(),
        TutorialPage::AdvancedArray => TutorialAdvancedArray().into_view(),
        TutorialPage::CustomModifiers => TutorialCustomModifiers().into_view(),
        TutorialPage::Modules => TutorialModules().into_view(),
        TutorialPage::Testing => TutorialTesting().into_view(),
    };
    view! {
        <TutorialNav page=page/>
        { tut_view }
        <br/>
        <br/>
        <TutorialNav page=page/>
    }
}

impl IntoParam for TutorialPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        all::<TutorialPage>()
            .find(|p| p.path() == value.unwrap_or(""))
            .ok_or_else(|| ParamsError::MissingParam(name.to_string()))
    }
}

#[component]
fn TutorialNav(page: TutorialPage) -> impl IntoView {
    let next = move || {
        page.next()
            .map(|p| {
                view!( <div><A href=format!("/docs/{}", p.path())>{p.title()}</A>" 〉"</div>)
                    .into_view()
            })
            .unwrap_or_else(|| view!( <div/>).into_view())
    };
    let previous = move || {
        page.previous()
            .map(|p| {
                view!( <div>"〈 "<A href=format!("/docs/{}", p.path())>{p.title()}</A></div>)
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

    let primitive_table: Vec<_> = Primitive::all()
        .filter_map(|p| {
            if let (Some(name), Some(ascii), Some(unicode)) = (p.name(), p.ascii(), p.glyph()) {
                if ascii.to_string() != unicode.to_string() {
                    return Some(view! {
                        <tr>
                            <td><code>{ name }</code></td>
                            <td><code>{ ascii.to_string() }</code></td>
                            <td><Prim prim=p glyph_only=true/></td>
                        </tr>
                    });
                }
            }
            None
        })
        .collect();

    view! {
        <Title text="Basic Stack Operations and Formatting - Uiua Docs"/>
        <h1>"Basic Stack Operations and Formatting"</h1>
        <h2 id="the-stack">"The Stack"</h2>
        <p>"In Uiua, all operations operate on a global stack. Lines of code are evaluated from "<A href="/docs/rtl">"right to left"</A>", top to bottom."</p>
        <p>"A number simply pushes its value onto the stack."</p>
        <Editor example="5"/>
        <Editor example="1 2 3"/>
        <p>"Operators pop values off the stack and push their results."</p>
        <p>"For example, "<Prim prim=Primitive::Add glyph_only=true/>" pops two values off the stack and pushes their sum."</p>
        <Editor example="+ 1 2"/>
        <p><Prim prim=Primitive::Mul glyph_only=true/>", of course, multiplies the two values instead."</p>
        <Editor examples={&["+ ", "1 ", "× ", "2 ", "3"]} help={&["", "Try the arrows to see how the stack changes with each operation."]}/>
        <p>"In the editor, items that end up on the "<em>"top"</em>" of the stack are shown at the "<em>"bottom"</em>" of the output. This is so that consecutive lines of code show their outputs in the correct order."</p>
        <Editor example="5\n+1 2\n\"Hello, World!\"\n+1 @a"/>
        <p>"This orientation can be changed in the editor's settings. Click the ⚙️ icon in the top right corner of the editor to see them."</p>
        <p>"Operations can span multiple lines. Every line uses the same stack!"</p>
        <Editor examples={&["1 2 ", "+ ", "5 ", "×"]} progress_lines=true/>

        <h2 id="comments">"Comments"</h2>
        <p>"Comments are denoted with "<code>"#"</code>" and run to the end of the line."</p>
        <Editor example="5 # This is a comment"/>
        <p>"Uiua does not have multiline comments."</p>

        <h2 id="formatting">"Formatting"</h2>
        <p>"Most Uiua built-in functions use special unicode characters. To type multiplication and division signs, you can use "<code>"*"</code>" and "<code>"%"</code>" respectively. Then, run the code to format the ASCII characters into unicode."</p>
        <Editor example="# Click Run to format!\n%6 *3 8" help={&["", "⇡Click⇡"]}/>
        <p>"Most built-in functions have names you can type rather than symbols. Formatting works on these too. "<em><strong>"This is the primary way of entering Uiua's glyphs."</strong></em></p>
        <p>"Try formatting the lines below by clicking "<strong>"Run"</strong>"."</p>
        <Editor examples=&{["max ", "sqrt ", "10 ", "mod ", "10 ", "pow ", "2 ", "8"]}/>
        <Editor example="abs +`1 `2"/>
        <p>"You don't have to type the whole name, just enough to to disambiguate it from others."</p>
        <Editor example="cei 1.5\nceil 1.5\nceili 1.5\nceilin 1.5\nceiling 1.5"/>
        <p>"You don't even have to remove spaces between built-in function names. The formatter will figure it out!"</p>
        <Editor example="roundsqrtpi"/>
        <p>"On this site, you can also click the ↧ symbol on any editor to show a pallete of all the Uiua glyphs. You can then click on any glyph to insert it into the editor."</p>
        <p>"Here is a table of all the glyphs that are typed with ASCII characters that get converted to glyphs."</p>
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
        <Editor example="+ `1 `2"/>

        <h2 id="stack-functions">"Stack Functions"</h2>
        <p>"There are a few functions that work on the stack itself. Some of these are critical and can be found scattered across all Uiua code."</p>
        <h2><Prim prim=Dup/></h2>
        <p><Prim prim=Dup/>" duplicates the top item on the stack."</p>
        <p>"In general, functions do not leave their arguments on the stack. If you want to reuse a value, you must "<Prim prim=Dup/>" it first."</p>
        <p>"For example, if you wanted to square a number, you could "<Prim prim=Dup/>" it, then "<Prim prim=Mul/>"."</p>
        <Editor example="×.4"/>
        <p><Prim prim=Dup/>" is often used in the examples on this site to show both the input and output of a function."</p>
        <Editor example="√.144"/>
        <br/>
        <h2><Prim prim=Flip/></h2>
        <p><Prim prim=Flip/>" swaps the top two items on the stack."</p>
        <p>"This is useful when you want to call a function that takes two arguments, but the arguments are on the stack in the wrong order."</p>
        <p>"For example, if you wanted to get the reciprocal of a number, you would "<Prim prim=Div/>" "<code>"1"</code>" by it. But, if the number is already on the stack, you would need to use "<Prim prim=Flip/>"."</p>
        <Editor example="÷1 5"/>
        <Editor example="÷∶1 5"/>
        <Editor example="∶1 2 3 4 5"/>
        <br/>
        <h2><Prim prim=Over/></h2>
        <p><Prim prim=Over/>" is like "<Prim prim=Dup/>", but it duplicates the second item on the stack instead of the first."</p>
        <Editor example=",1 2 3 4"/>
        <Editor example="+×, 3 5"/>
        <br/>
        <h2><Prim prim=Pop/></h2>
        <p><Prim prim=Pop/>" removes the top item from the stack."</p>
        <p>"This is useful when you want to discard a value that you do not need."</p>
        <Editor examples={&["1 ", "; ", "2 ", "3 ", "4 ", "; ", "5 ", "6"]}/>
        <h2><Prim prim=Trace/></h2>
        <p><Prim prim=Trace/>" prints the top item on the stack without popping it."</p>
        <p>"It also attaches line and column numbers."</p>
        <p>"This is useful for debugging by inpecting the stack."</p>
        <Editor example="+1 ⸮ ×4 trace ×. -3 5"/>
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
        Add, Sub, Mul, Div, Mod, Pow, Log, Neg, Abs, Ceil, Floor, Round, Sqrt, Sign,
    ]);
    let comp_table = primitive_rows([
        Eq, Ne, Lt, Gt, Le, Ge, Min, Max, Floor, Ceil, Round, Sin, Atan,
    ]);

    view! {
        <Title text="Math and Comparison - Uiua Docs"/>
        <h1>"Math and Comparison"</h1>
        <p>"Uiua supports all the basic math operations as well as comparison, min/max, and rounding."</p>
        <div id="ascii-glyphs" style="display: flex; justify-content: space-evenly;">
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
        <p>"One thing to note is that non-commutative operators work backwards."</p>
        <p>"This is so you can think of the operator and the second number as a single unit."</p>
        <Editor example="-2 5" help={&["", "What is 5 \"minus 2\"?"]}/>
        <Editor example="<2 5" help={&["", "Is 5 \"less than 2\"?"]}/>
        <Editor example="÷2 5" help={&["", "What is 5 \"divided by 2\"?"]}/>
        <p>"Uiua has no boolean types. Comparison operators return "<code>0</code>" for false and "<code>1</code>" for true."</p>
        <Editor example="=2 5"/>
        <Editor example="=2 2"/>
        <p>"Because of how stack operations work, you can delay operations until after all the arguments are on the stack."</p>
        <Editor examples={&["×", "+", "+", "1 ", "2 ", "3 ", "4"]} help={&["", "Click the arrows to see how the expression is built up"]}/>
        <p>"This is not special syntax. All the numbers are pushed to the stack, then the operators work on them."</p>
        <p>"Remember that you can type the names of operators and then run to format them."</p>
        <Editor example="# Click Run to format!\nmax sqrt2 mod10 abs`31" help={&["", "⇡Click⇡"]}/>

        <h2 id="adicity">"Adicity"</h2>
        <p>"Some programming languages use the terms \"unary\" and \"binary\" to refer to functions that take one or two arguments respectively. While these are the Latin terms, many array languages, including Uiua, prefer to use the Greek terms \"monadic\" and \"dyadic\"."</p>
        <p>"As you read Uiua's documentation, you will see these terms used to describe functions (and modifiers)."</p>
        <p>"For example, "<Prim prim=Sqrt/>" is a monadic function, and "<Prim prim=Add/>" is a dyadic function."</p>
        <p>"On this site, monadic functions are in "<span class="monadic-function">"green"</span>" and dyadic functions are in "<span class="dyadic-function">"blue"</span>"."</p>
    }
}

#[component]
fn TutorialArrays() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Arrays - Uiua Docs"/>
        <h1>"Arrays"</h1>
        <p>"Uiua is, first and foremost, an array language. The only composite data type is the multimensional array. Arrays have a lot of nice properties, and the language's built-in functions are designed to make it easy to work with them. If you've only ever programmed in non-array languages, then this will be a completely foreign paradigm. In most array languages, most data structures and control flow are replaced with operations on arrays."</p>

        <h2 id="creating-arrays">"Creating Arrays"</h2>
        <p>"Other than with functions, Uiua has two ways to create arrays. They are called "<em>"strand notation"</em>" and "<em>"stack notation"</em>"."</p>
        <p><strong>"Strand notation"</strong>" uses underscores to connect elements."</p>
        <Editor example="1_2_3"/>
        <Editor example="\"Hello\"_\"World\""/>
        <p>"Strand notation is good when you want to create short and/or simple arrays. For longer or more complex arrays, you can use stack notation."</p>
        <p><strong>"Stack notation"</strong>" uses brackets to group elements."</p>
        <Editor example="[1 2 3]"/>
        <Editor example="[¯5 37 42 π]"/>
        <p>"What's cool about stack notation is that it is "<em>"not"</em>" just a way to list elements. The code between the brackets runs from right to left as it normally would. When it is done, any items on the stack higher than when it started are put into the array. This gives you some cool ways to create arrays."</p>
        <p>"Remember that "<Prim prim=Dup/>" duplicates the top item on the stack."</p>
        <Editor example="[...5]"/>
        <Editor example="[×2.×2.×2.×2 .2]"/>
        <Editor example="[+1 2 +3 4]"/>
        <p>"You can also use stack notation to make multidimensional arrays."</p>
        <Editor example="[1_2_3 4_5_6]"/>
        <Editor example="[...[1 2 3]]"/>
        <p>"More precisely, stack notation "<Prim prim=Couple/>"s the first two stack items created between the "<code>"[]"</code>"s and "<Prim prim=Join/>"s the rest to that coupling."</p>
        <p>"Unlike strand notation, stack notation may span multiple lines. The lines are still executed right-to-left, but they are executed bottom-to-top so that the arrays come out the same way they look in the code."</p>
        <Editor example="\
[1 2 3
 4 5 6
 7 8 9]"/>
        <Editor example="\
[[1 2 3]
 [4 5 6]
 [7 8 9]]"/>

        <h2 id="shape-len"><Prim prim=Shape/>" and "<Prim prim=Len/></h2>
        <p>"Other than their data, arrays also have a property called their "<strong>"shape"</strong>". Shape is a list of non-negative integers that describes the array's size along each of its axes."</p>
        <p>"We can get the array's shape with the "<Prim prim=Shape/>" function. It's a triangle because a triangle is a shape."</p>
        <Editor example="△[1 2 3]"/>
        <Editor example="△5"/>
        <Editor example="△[[1 2 3] [4 5 6]]"/>
        <Editor example="△[...[1 2 3]]"/>
        <p>"From shape we can derive two closely-related properties called "<strong>"length"</strong>" and "<strong>"rank"</strong>"."</p>
        <p><Prim prim=Len/>" is the number of "<em>"rows"</em>" of the array. This is the number of elements for a 1D array and the number of rows for a 2D array. Length is always equal to the first number in the shape (or 1 if the shape is empty)."</p>
        <p><strong>"Rank"</strong>" is the number of dimensions of the array. It is equivalent to the "<Prim prim=Len/>" of the "<Prim prim=Shape/>"."</p>
        <Editor example=" △[1_2_3 4_5_6]\n ⧻[1_2_3 4_5_6]\n⧻△[1_2_3 4_5_6]"/>

        <h2 id="pervasion">"Pervasion"</h2>
        <p>"Most operations that apply to scalars are what is called "<em>"pervasive"</em>" when it comes to arrays. This means that the operation automatically applies to every item in the array."</p>
        <Editor example="+1 1_2_3"/>
        <Editor example="√[4 9 16]"/>
        <Editor example="+1_2_3 4_5_6"/>
        <p>"When doing a pervasive operation on two arrays, their shape "<em>"prefixes"</em>" must match."</p>
        <Editor example="+[1 2] [3 4 5]"/> // Should fail
        <Editor example="△10_20\n      △[3_4_5 6_7_8]\n+10_20 [3_4_5 6_7_8]"/>
        <p>"If you want to do some pervasive operation on arrays whose shapes do not match, you can set a default value with "<Prim prim=Fill/>". Any places where the shapes don't match will be filled in with that value."</p>
        <Editor example="⬚-0 [1 2] [3 4 5 6 7]"/>
        <p><Prim prim=Fill/>" can be used in a lot of other cases. See its documentation for more."</p>

        <h2 id="useful-array-operations">"Useful Array Operations"</h2>
        <p>"You don't need to memorize all of these right now. This is just a brief introduction to some of the array operations so that you won't be surprised when you see them later."</p>
        <p>"If you ever see a glyph that you don't recognize in an example, you can mouse over it in the editor to learn its name. You can also click the names of functions in the site text to see their documentation."</p>
        <p><Prim prim=Range/>" creates an array of all the natural numbers less than a maximum."</p>
        <Editor example="⇡10"/>
        <p><Prim prim=First/>" gets the first row of an array."</p>
        <Editor example="⊢ [4 7 1]"/>
        <Editor example="⊢ [1_2 3_4 5_6]"/>
        <p><Prim prim=Reverse/>" reverses the rows of an array."</p>
        <Editor example="⇌ [4 7 1]"/>
        <Editor example="⇌ [1_2 3_4 5_6]"/>
        <p><Prim prim=Rotate/>" rotates the rows of an array by some amount."</p>
        <Editor example="↻2 [1 2 3 4 5]"/>
        <p><Prim prim=Deshape/>" flattens an array into a 1D array."</p>
        <Editor example="♭ .[1_2 3_4 5_6]"/>
        <p><Prim prim=Reshape/>" changes the shape of an array while keeping the elements in the same order."</p>
        <Editor example="↯3_3 .⇡9"/>
        <p><Prim prim=Take/>" and "<Prim prim=Drop/>" isolate part of an array."</p>
        <Editor example="↙3 [1 2 3 4 5]\n↘3 [1 2 3 4 5]"/>

        <h2 id="array-model">"The Array Model"</h2>
        <p>"For curious array afficionados, Uiua uses an array model resembling "<a href="https://aplwiki.com/wiki/Box">"J's Boxed array model"</a>"."</p>
        <p>"All arrays are flat and homogenous. Arrays always have a rectangular shape. Different types of data, like numbers and characters, cannot be mixed in the same array."</p>
        <p>"However, there is an escape hatch for when you really want jagged, nested, or mixed-type arrays. In Uiua, an array of heterogenous values can be simulated with an array of "<em>"boxes"</em>"."</p>
        <Editor example="[1 2 [7 8 9]]"/> // Should fail
        <p>"By using "<Prim prim=Box/>", we can turn any value into a box that contains that value. We can then put these boxes into an array together."</p>
        <Editor example="[□1 □2 □[7 8 9]]"/>
        <p>"The "<code>"⟦⟧"</code>"s indicate that a list is "<Prim prim=Box/>"ed."</p>
        <p><Prim prim=Unbox/>" extracts a "<Prim prim=Box/>"ed value."</p>
        <Editor example="⊔ .□[1 2 3]"/>
        <p>"To get "<Prim prim=Box/>"ed array values back on the stack, we can use "<Prim prim=Reduce/><Prim prim=Unbox/>"."</p>
        <Editor example="/⊔[□1 □2 □[7 8 9]]"/>
        <p>"Having to write "<Prim prim=Box glyph_only=true/>" everywhere is annoying, and so..."</p>

        <h2 id="nested-arrays">"Nested Arrays"</h2>
        <p>"Uiua has a special syntax for making arrays where every item is "<Prim prim=Box/>"ed."</p>
        <p>"Using "<code>"{}"</code>"s instead of "<code>"[]"</code>"s for stack array notation will automatically "<Prim prim=Box/>" every item."</p>
        <Editor example="{1 2 [7 8 9]}"/>
        <p>"This is very useful for making lists of strings."</p>
        <Editor example=r#"Langs ← .["Uiua" "APL" "J" "BQN" "K" "Q"]"#/>
        <Editor example=r#"Langs ← .{"Uiua" "APL" "J" "BQN" "K" "Q"}"#/>
        <p>"The "<code>"⌜⌟"</code>"s indicate that a string is "<Prim prim=Box/>"ed."</p>
        <p>"Many simple functions will work on "<Prim prim=Box/>" elements without needing to use "<Prim prim=Unbox/>"."</p>
        <Editor example=
r#"Langs ← {"Uiua" "APL" "J" "BQN" "K" "Q"}
⧻⊢Langs
+1Langs"#/>
        <p>"However, more complex functions usually need both operands to be the same type, so you must either "<Prim prim=Unbox/>" the boxed elements or "<Prim prim=Box/>" the normal ones."</p>
        <p>"For example, to check if a string is in the list with "<Prim prim=Member/>", you would need to "<Prim prim=Box/>" the string first."</p>
        <Editor example=
r#"Langs ← {"Uiua" "APL" "J" "BQN" "K" "Q"}
∊ □"APL" Langs"#/>

        <p>"For more about working with box arrays, see "<Prim prim=Box/>"'s documentation."</p>
    }
}

#[component]
fn TutorialTypes() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Types - Uiua Docs"/>
        <h1>"Types"</h1>
        <p>"Every value in Uiua is an array. However, different arrays on the stack can have different "<em>"types"</em>" of items. Every element of an array is always the same type. Unlike some other array programming languages, Uiua arrays cannot have elements of different types."</p>
        <p>"There are only three types of arrays:"</p>
        <ul>
            <li><strong>"Number"</strong></li>
            <li><strong>"Character"</strong></li>
            <li><strong>"Function"</strong></li>
        </ul>

        <h2 id="numbers">"Numbers"</h2>
        <p>"Numbers are decimal numbers with floating precision. They use a 64-bit floating-point representation."</p>
        <Editor example="[5 0 3.2 ¯1.1 π ∞]"/>
        <p>"Most math operations can only be applied to numbers."</p>
        <p>"Even though numbers can have a fractional part, many built-in functions require whole numbers. These functions will return an error if given a non-whole number."</p>
        <p>"One such example is "<Prim prim=Pick/>"."</p>
        <Editor example="⊡ 2 [4 7 9 1 0]"/>
        <Editor example="⊡ 3.1 [4 7 9 1 0]"/> // Should fail
        <p>"If you want to convert a number to a whole number, you can use "<Prim prim=Floor/>", "<Prim prim=Ceil/>", or "<Prim prim=Round/>"."</p>

        <h2 id="characters">"Characters"</h2>
        <p>"Characters are represented as 32-bit Unicode codepoints."</p>
        <p>"Character literals, denoted with a preceding "<code>"@"</code>", create rank 0 (scalar) character arrays."</p>
        <Editor example="@a @b"/>
        <Editor example="[@u @i @u @a]"/>
        <p>"Characters like newline or null need to be escaped with "<code>"\\"</code>", but a space does not."</p>
        <Editor example="[@\\r @\\0 @ ]"/>
        <p>"If you don't like the significant whitespace of "<code>"@ "</code>", "<code>"@\\s"</code>" is also space."</p>
        <p>"String literals, delimited by "<code>"\""</code>"s, create rank 1 character arrays."</p>
        <Editor example="△.\"Hello, World!\""/>
        <p>"You can make strings span multiple lines with a "<code>"$"</code>" followed by a space on each line."</p>
        <p>"These do not require "<code>"\""</code>"s."</p>
        <p><Prim prim=Sys(SysOp::Print)/>" pretty-prints a value."</p>
        <Editor example="&p $ Hello, \n   $ World!"/>
        <p>"This style of string is also useful when your string contains a lot of quotes that you don't want to escape."</p>
        <Editor example="$ An then she was like, \"No way!\"\n$ And I was like, \"Way...\""/>
        <br/>

        <h2 id="character-arithmetic">"Character Arithmetic"</h2>
        <p>"Characters and numbers exist in an "<a href="https://en.wikipedia.org/wiki/Affine_space">"affine space"</a>", the same as in "<a href="https://mlochbaum.github.io/BQN/doc/arithmetic.html#character-arithmetic">"BQN"</a>"."</p>
        {
            let number = || view!(<span class="number-literal-span">"number"</span>);
            let character = || view!(<span class="string-literal-span">"character"</span>);
            view! {
                <p>"You can "<Prim prim=Add/>" "{number}"s and "{character}"s to get another "{character}"."</p>
                <p>"You can "<Prim prim=Sub/>" a "{number}" from a "{character}" to get another "{character}"."</p>
                <p>"You can "<Prim prim=Sub/>" two "{character}"s to get a "{number}"."</p>
                <p><em>"No"</em>" other arithmetic operations can be done on "{character}"s."</p>
            }
        }
        <Editor example="+1 @a"/>
        <Editor example="-8 \"Uiua\""/>
        <Editor example="-@a @z"/>
        <Editor example="+@a @b"/> // Should fail

        <h2 id="boxes">"Boxes"</h2>
        <p>"Boxes are containers that can wrap an array of any type or shape. Multiple boxes can be put in the same array, no matter their contents."</p>
        <p>"Boxes can be created either by using the "<Prim prim=Box/>" function or with boxing array notation between "<code>"{}"</code>"s."</p>
        <Editor example="□5"/>
        <Editor example="□[1 2 3]"/>
        <Editor example="□\"Hello!\""/>
        <Editor example="{\"cat\" 5}"/>

        <h2>"Type agreement"</h2>
        <p id="type-agreement">"For functions that work on the structure of arrays rather than their values, the types of the arrays must match."</p>
        <Editor example="⊂ 1_2 3"/>
        <Editor example="⊟ \"Hello\" \"World\""/>
        <Editor example="⊟ 1_2_3 \"dog\""/> // Should fail
        <p>"There is an exception for functions. Any function that pushes one value onto the stack can be put in an array with a non-function. In this case, the non-function will be turned into a function, similar to "<Prim prim=Box/>"."</p>
        <Editor example="⊟ 5 □[1 2 3]"/>

        <h2 id="empty-arrays">"Empty Arrays"</h2>
        <p>"The type of an array that is constructed with no elements depends on the syntax used to construct it. Its shape is always "<code>"[0]"</code>"."</p>
        <p>"We can use the "<Prim prim=Type/>" function to get the type of an array. "<code>"0"</code>" corresponds to numbers, "<code>"1"</code>" to characters, and "<code>"2"</code>" to functions."</p>
        <Editor example="type []"/>
        <Editor example="type \"\""/>
        <Editor example="type {}"/>
    }
}

#[component]
fn TutorialBindings() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Bindings - Uiua Docs"/>
        <h1>"Bindings"</h1>
        <p>"Bindings are global names that can be given to Uiua values. They are denoted with "<code>"←"</code>", which the formatter will convert from "<code>"="</code>" when appropriate."</p>
        <Editor example="a = 3\nb ← 5\n+ a b" help={&["", "Try running to format the ="]}/>
        <p>"Valid binding names can be made up of any sequence of uppercase or lowercase alphabetic characters OR a single non-alphanumeric character that is not already used for a Uiua function."</p>
        <p>"Binding names longer than 2 characters should be TitleCase."</p>
        <Editor example="NumOne ← 1\nNuMtWo ← 2\n😀 ← \"happy\""/>
        <p><em>"Warning"</em>": It is not guaranteed that any particular non-alphanumeric character will not be used for a built-in function in the future. Use them at your own risk. Emojis are safe though."</p>
        <p>"Unlike most programming languages, binding names in Uiua "<em>"cannot"</em>" contain numbers or underscores."</p>
        <Editor example="Variable_1 ← 5"/> // Should fail
        <p>"Bindings are case-sensitive."</p>
        <p>"The parser can sometimes mistake all-lowercase binding names for unformatted built-in functions."</p>
        <p>"Here, the parser thinks that "<code>"part"</code>" is "<Prim prim=Partition/>"."</p>
        <Editor example="part = 5" help={&["", "Run to format and reveal why this does not work"]}/>
        <p>"To fix this issue, simply change the binding's capitalization."</p>
        <Editor example="Part = 5\n*2 Part"/>
        <p>"Bindings run the code to the right of the "<code>"←"</code>", then pop the top value off the stack and bind it to the name on the left."</p>
        <p>"Note, though, that an empty right side is perfectly valid! This means you can bind values that were create on previous lines."</p>
        <Editor example="×6 7\nAnswer ←\n[Answer]"/>

        <h2 id="binding-functions">"Binding Functions"</h2>
        <p>"If the code on the right side of the "<code>"←"</code>" looks like a function, then instead of evaluating its right side immediately, the right side will be bound as a function."</p>
        <p>"This is how you make named functions in Uiua."</p>
        <Editor example="TimesThree ← ×3\nTimesThree 7"/>
        <Editor example="👋 ← ⊂\"Hello, \"\n👋 \"World!\""/>
        <Editor example="⍨ ← ∶\n⍪ ← ⊂\n⍳ ← ⇡\n⍪⍨⍳3⍳5 # Truly an abomination"/>
        <p>"The "<A href="/docs/functions">"next section"</A>" discusses functions in more detail."</p>
    }
}

#[component]
fn TutorialFunctions() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Modifiers and Functions - Uiua Docs"/>
        <h1>"Modifiers and Functions"</h1>

        <h2 id="modifiers">"Modifiers"</h2>
        <p>"Modifiers are functions that take other functions as arguments. If you immediately follow a modifier with its function arguments, the functions will be called inside the modifier rather than outside."</p>
        <p>"For example, "<Prim prim=Reduce/>" applies a function \"between\" all rows of an array."</p>
        <p><PrimCodes prims={[Reduce, Add]}/>" is therefore the sum of all the rows of an array."</p>
        <Editor example="/+ 1_2_3_4"/>
        <p><Prim prim=Scan/>" is similar, but it returns all the intermediate results."</p>
        <Editor example="\\+ 1_2_3_4"/>
        <p><Prim prim=Table/>" applies a function between all combinations of elements of two arrays. This is sometimes called the "<em>"outer product"</em>"."</p>
        <Editor example="⊞+ [5 6 7 8] [10 20 30 40]"/>
        <p>"In the same way that \"monadic\" and \"dyadic\" functions refer to functions that take one or two arguments respectively, \"monadic\" and \"dyadic\" "<em>"modifiers"</em>" refer to modifiers that take one or two "<em>"functions"</em>" respectively."</p>
        <p>"On this site, monadic modifiers are in "<span class="monadic-modifier">"yellow"</span>" and dyadic modifiers are in "<span class="dyadic-modifier">"purple"</span>"."</p>
        <p>"The main docs page has "<A href="/docs/modifier">"a list"</A>" of all of the built-in modifiers."</p>

        <h2 id="inline-functions">"Inline Functions"</h2>
        <p>"In addition to creating a new function with a capitalized binding name, as discussed in the "<A href="/docs/bindings">"previous section"</A>", functions in Uiua can also be created by surrounding code with "<code>"()"</code>"s."</p>
        <p>"This is usually only necessary when you need to call multiple functions within a modifier."</p>
        <p>"For example, if you wanted to get the last element of each row of an array, you could use "<Prim prim=Rows/>"."</p>
        <Editor example="≡(⊢⇌) .[2_5_3 0_2_1 0_0_2]"/>
        <p>"If you wanted to rotate every row of an array backwards by a fixed amount, you could use "<Prim prim=Distribute/>"."</p>
        <Editor example="∺(↻¯) 2 [1_2_3 4_5_6 7_8_9]"/>
        <p>"If you want to make an inline function with exactly 2 terms, you can use the "<Prim prim=Bind/>" modifier instead of "<code>"()"</code>"s and save 1 character of space!"</p>
        <Editor example="/(-∶) 1_2_3_4_5\n/'-∶ 1_2_3_4_5"/>
        <p>"This looks nice with modifiers that take multiple functions like "<Prim prim=Under/>"."</p>
        <Editor example="⍜'↙3⇌ [1 2 3 4 5]"/>
        <Editor example="⍜'↻3'⊂π 1_2_3_4_5"/>
        <p>"Inline functions may span multiple lines. Unlike multiline stack notation arrays, which run bottom-to-top, multiline inline functions run top-to-bottom as other code does."</p>
        <Editor example="\
X ← (
  ⊞=.⇡
  ↥⇌.
)
X 5"/>

        <h2 id="local-bindings">"A Note on Local Bindings"</h2>
        <p>"Bindings in Uiua can "<em>"only"</em>" be global. There is no way to give a name to a value within an inline function. A "<code>"←"</code>" inside "<code>"()"</code>"s is a syntax error."</p>
        <p>"This is a deliberate design decision. It forces you to write tacit code, a.k.a. code with functions that do not mention their arguments. Uiua is designed to make writing tacit code as workable as possible."</p>

        <h2 id="format-strings">"Format Strings"</h2>
        <p>"Prefixing a string with "<code>"$"</code>", creates a format string. A format string is a function that is called immediately. It takes an argument for each "<code>"_"</code>" in the string and replaces it with the stringified version."</p>
        <Editor example="$\"Hello, _!\" \"World\""/>
        <Editor example="Greet ← $\"Hello, _!\"\nGreet \"user\""/>
        <Editor example="x ← 5\n$\"x = _\" x"/>
        <Editor example="$\"_, _, and _\" 1 2 3"/>
        <p>"If you need to use a literal "<code>"_"</code>", you can escape them with "<code>"\\"</code>"."</p>
        <Editor example="$\"\\__\\_\" 27"/>
        <p>"Multi-line strings are implicitly format strings."</p>
        <Editor example="⊃⊙∘+ 1 2\n&p $ Do you know what _ + _ is?\n   $ It's _!"/>

        <h2 id="stack-signatures">"Stack Signatures"</h2>
        <p>"Bindings and inline functions can have a "<em>"stack signature"</em>" declared with a "<code>"|"</code>" followed by 1 or 2 numbers seperated by a "<code>"."</code>". The first number is the number of arguments the function pops from the stack. The second number is the number of values the function pushes to the stack."</p>
        <p>"The second number is optional. If it is not given, it is assumed to be 1."</p>
        <p>"In bindings, the "<code>"|"</code>" comes after the "<code>"←"</code>". In inline functions, it comes after the "<code>"("</code>"."</p>
        <Editor example="TimesThree ← |1.1 ×3\nTimesThree 7"/>
        <Editor example="TimesThree ← |1   ×3\nTimesThree 7"/>
        <Editor example="∵(|2.1 ⊟.×) 1_2_3 4_5_6"/>
        <p>"Stack signatures are useful for documenting functions to make sure that they are used correctly."</p>
        <p>"A signature declaration is "<em>"required"</em>" if the function's signature cannot be infered. The compiler can usually infer a function's signature unless you are doing something weird or using "<Prim prim=Break/>"."</p>
        <p>"In addition, an error is thrown if a function's signature can be inferred and the inferred signature does not match the declared signature. This can help validate that a function is correct."</p>
        <Editor example="≡(|2 ↻.) 1_2_3 ↯3_3⇡9"/> // Should fail
        <p><strong>"WARNING"</strong>": If the compiler cannot derive the stack signature of a function and you give it one which is "<em>"wrong"</em>", your code may no longer compile in future versions of the language."</p>
    }
}

#[component]
fn TutorialControlFlow() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Control Flow - Uiua Docs"/>
        <h1>"Control Flow"</h1>
        <p>"Uiua, and array languages in generall, require much less control flow than other programming languages. Most operations that would be loops in other languages are simply operations on arrays. Because boolean operations, return numbers, a lot of checks that would be done with "<code>"if"</code>" statements in other languages become mathematical or indexing operations in array languages."</p>
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
        <p>"In Uiua, it is much simpler, and there are no "<code>"if"</code>"s or "<code>"for"</code>"s to be found:"</p>
        <Editor example="f ← ∩▽¬,,=0◿2.\nf [1 2 3 7 2 4 5]"/>
        <p>"That being said, not every problem lends itself to array operations. Uiua has a few methods for handling such cases."</p>

        <h2 id="repeat">"Looping with "<Prim prim=Repeat/></h2>
        <p>"The "<Prim prim=Repeat/>" modifier takes a function and a number and calls the function that many times."</p>
        <Editor example="⍥(×2)10 5"/>
        <Editor example="⍥/+2 ↯3_3⇡9"/>
        <Editor example="⁅[⍥⚂5]"/>
        <p>"You can loop forever by using "<Prim prim=Infinity/>". You can break out of an infinite (or finite) loop with "<Prim prim=Break/>"."</p>
        <Editor example="⍥(⎋>1000.×2)∞ 1"/>
        <p>"This requires a signature declaration if done in a function."</p>
        <Editor example="f ← |1 ⍥(⎋>1000.×2)∞\nf 5"/>
        <p><Prim prim=Repeat/>"'s glyph is a combination of a circle, representing a loop, and the 𝄇 symbol from musical notation."</p>

        <h2 id="try">"Catching errors with "<Prim prim=Try/></h2>
        <p>"The "<Prim prim=Try/>" modifier takes two functions. If the first function throws an error, the second function is called with the same arguments plus an error message."</p>
        <p>"We can see how this works by using it with "<Prim prim=Parse/>"."</p>
        <p>"If the parsing fails, we "<Prim prim=Box/>" "<Prim prim=Both/>" the argument and the error message and put them in an array."</p>
        <Editor example="f ← ⍣parse[∩□]\nf \"5\"\nf \"dog\""/>
        <p>"If we don't care about an error and just want to supply a default value, we can use "<Prim prim=Gap/>" to discard the argument and error message. "<Prim prim=Gap/>" is similar to "<Prim prim=Pop/>", except it is a modifier instead of function."</p>
        <Editor example="f ← ⍣parse⋅⋅0\nf \"5\"\nf \"dog\""/>

        <h2 id="switch">"Switch Functions"</h2>
        <p>"Switch functions are inline functions that choose a branch based on an index. Like normal inline functions, they are surrounded by "<code>"()"</code>"s. Branches are separated by "<code>"|"</code>"s."</p>
        <p>"Unlike normal inline functions, switch functions can appear anywhere in code and are called immediately."</p>
        <Editor example="(3|5) 0"/>
        <Editor example="(3|5) 1"/>
        <p>"Switch functions can have as many branches as you want, and they can also be nested."</p>
        <Editor example="≡((×10|+1|(∘|¯)=2.) ◿3.) [2 9 4 0 8 3]"/>

        <h2 id="if"><Prim prim=If/></h2>
        <p>"The "<Prim prim=If/>" modifier is similar to a switch function, but it only has two branches. The true branch is the first function and the false branch is the second"</p>
        <Editor example="f ← ?+×\nf 0 3 5\nf 1 3 5"/>
        <p>"This may seem useless when switch functions exist, and for scalar conditions, apart from being slightly shorter, is is. However, "<Prim prim=If/>"'s condition can be a list."</p>
        <Editor example="?+- [1 0 1] [2 2 2] [4 4 4]"/>
        <p>"While "<Prim prim=If/>" can be chained, it is usually preferable to use a switch function instead."</p>

        <h2 id="assert"><Prim prim=Assert/></h2>
        <p>"The "<Prim prim=Assert/>" function takes any value and a condition. If the condition is anything but "<code>"1"</code>", the value is thrown as an error that can be caught with "<Prim prim=Try/>"."</p>
        <Editor example="f ← ⍣(¯⍤10≤10.);\nf 5\nf 12"/>
        <p>"If the "<Prim prim=Assert/>"ed value is never caught, it becomes an error."</p>
        <Editor example="f ← ¯⍤\"too big!\"≤10.\nf 5\nf 12"/> // Should fail
        <p>"Using "<Prim prim=Assert/>" for this purpose will be covered more in the "<A href="/docs/testing">"section on testing"</A>"."</p>
    }
}

#[component]
fn TutorialAdvancedStack() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Advanced Stack Manipulation - Uiua Docs"/>
        <h1>"Advanced Stack Manipulation"</h1>
        <p>"Uiua does not have local variables. With only "<Prim prim=Dup/>", "<Prim prim=Flip/>", and "<Prim prim=Over/>", how do you work with more than 2 values at a time?"</p>

        <h2 id="fork"><Prim prim=Fork/></h2>
        <p><Prim prim=Fork/>" is a dyadic modifier that takes 2 functions and calls them both on the same set of arguments. The number of arguments used is the maximum of the two functions."</p>
        <Editor example="[⊃+× 3 5]"/>
        <p>"If one of the functions take more arguments than the other, the function with fewer arguments uses the top-most values."</p>
        <Editor example="⊃×⇌ [1 2 3] 10"/>
        <p>"What's powerful about "<Prim prim=Fork/>" is that it can be chained to use as many functions as you want."</p>
        <Editor example="[⊃⊃⊃+-×÷ 5 8]"/>
        <p><Prim prim=Fork/>" is also good because it does not require that its values be in an array together, so they can be different shapes or types."</p>
        <Editor example="⊃+- 1 @b"/>
        <Editor example="⊃⊃⊃↻↙↘⊡ 2 [1 2 3 4 5]"/>
        <p>"We'll see just how important "<Prim prim=Fork/>" is later is this section."</p>

        <h2 id="both"><Prim prim=Both/></h2>
        <p><Prim prim=Both/>" is a monadic modifier and a sort of complement to "<Prim prim=Fork/>". While "<Prim prim=Fork/>" calls multiple functions on the same set of arguments, "<Prim prim=Both/>" calls a "<em>"single"</em>" function on "<em>"multiple"</em>" sets of arguments."</p>
        <Editor example="∩⇌ [1 2 3] [4 5 6]"/>
        <p>"Chaining "<Prim prim=Both/>" doubles the number of arguments each time."</p>
        <Editor example="∩∩⇌ [1 2 3] [4 5 6] [7 8 9] [10 11 12]"/>

        <h2 id="bracket"><Prim prim=Bracket/></h2>
        <p>"To round off the trio, we have "<Prim prim=Bracket/>", which is a dyadic modifier that calls each of its functions on a different set of arguments."</p>
        <Editor example="[⊓+× 1 2 3 4]"/>
        <p><Prim prim=Bracket/>" too can be chained. Each additional function is called on arguments deeper in the stack."</p>
        <Editor example="[⊓⊓⊓+¯×. 1 2 3 4 5 6]"/>

        <h2 id="dip-and-gap"><Prim prim=Dip/>" and "<Prim prim=Gap/></h2>
        <p>"The "<Prim prim=Dip/>" modifier temporarily pops the top value on the stack, calls its function, then pushes the value back."</p>
        <Editor example="[⊙+ 1 2 3]"/>
        <p><Prim prim=Dip/>" can be chained to dig deeper into the stack."</p>
        <Editor example="[⊙⊙⊙⊙⊙⊙+ 1 2 3 4 5 6 7 8]"/>
        <p><Prim prim=Gap/>" "<em>"discards"</em>" the top value on the stack and calls its function."</p>
        <p>"But wait, "<Prim prim=Pop/>" exists! Why would you need this?"</p>

        <h2 id="planet-notation">"🌍 Planet Notation 🪐"</h2>
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
        <p>"If you like, you can factor out the "<Prim prim=Gap/>" in the second part."</p>
        <Editor example="×⊃(+⊙⋅⋅∘)⋅(-⊃⋅∘(×⊙⋅∘)) 1 2 3 4"/>
        <p>"And there you have it! A readable syntax juggling lots of values without any names!"</p>
        <p>"If you "<em>"really"</em>" want it to be shorter, you can use "<Prim prim=Bind/>" instead of "<code>"()"</code>"s, but let's not."</p>
        <p>"It's annoying to write long lists of names like "<code>"gapdipgapgapide"</code>", so those three functions have a special rule in the parser that allows you to write them with only 2 letters."</p>
        <p>"Try it out!"</p>
        <Editor example="+gadigagaid 1 2 3 4 5"/>
        <p>"In general, planet notation as complex as the mathematical function example above should only be used when it is necessary. For that examples like that with 4+ values, it is. However, when working with fewer values, you can get very far with just "<Prim prim=Dup/>" and "<Prim prim=Flip/>". Maybe sprinkle some "<Prim prim=Over/>"s and "<Prim prim=Dip/>"s in there too."</p>
    }
}

#[component]
fn TutorialAdvancedArray() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Advanced Array Manipulation - Uiua Docs"/>
        <h1>"Advanced Array Manipulation"</h1>
        <p>"Sometime the operation you need to perform on an array is more complicated than something like "<Prim prim=Reduce/>", "<Prim prim=Rows/>", or "<Prim prim=Table/>" allows."</p>

        <h2 id="rank-lists">"Rank Lists"</h2>
        <p>"Uiua has 4 special modifiers that allow you to specify the ranks of arrays that you want to operate on."</p>
        <p>"Specified ranks are passed as a single number or a list of numbers."</p>
        <p>"Ranks can be specified as any integer, or "<Prim prim=Infinity/>"."</p>
        <br/>
        <p>"To see how this works, we can use "<Prim prim=Box/>" with the most basic rank-generic modifier, "<Prim prim=Level/>"."</p>
        <p>"In the examples below, pay attention to which parts of the array end up in boxes"</p>
        <Editor example="≑0□ ↯2_3_4⇡24"/>
        <Editor example="≑1□ ↯2_3_4⇡24"/>
        <Editor example="≑2□ ↯2_3_4⇡24"/>
        <Editor example="≑3□ ↯2_3_4⇡24"/>
        <Editor example="≑∞□ ↯2_3_4⇡24"/>
        <Editor example="≑¯1□ ↯2_3_4⇡24"/>
        <Editor example="≑¯2□ ↯2_3_4⇡24"/>
        <Editor example="≑¯3□ ↯2_3_4⇡24"/>
        <p>"As you can see, non-negative ranks refer to the rank of the argument that will be passed to the modifier's function, while negative ranks refer to that many ranks "<em>"less"</em>" than the rank of the argument array."</p>
        <p><Prim prim=Infinity/>" refers to the exact rank of the argument array."</p>

        <h2 id="level"><Prim prim=Level/></h2>
        <p><Prim prim=Level/>" iterates over all arrays of ranks specified in its rank list."</p>
        <p>"Here, we join all rank 1 arrays from the first arguments with all rank 2 arrays from the second."</p>
        <Editor example="≑1_2⊂ ,, ↯2_3 π ↯2_2_3⇡12"/>
        <p>"This is useful when you want to reference a fixed value while iterating over each row of an array."</p>
        <p>"Here, we fix "<code>"2"</code>" and "<code>"0_3_1"</code>" as things that will not be iterated over by specifying their ranks as "<Prim prim=Infinity/>"."</p>
        <Editor example="≑∞_∞_¯1(↻⊙⊏) 2 0_3_1 ↯3_4⇡12"/>

        <h2 id="ocean-notation">"🌊 Ocean Notation 🪸"</h2>
        <p>"It can sometimes be verbose to specify rank lists, and having numbers that refer to ranks in code next to numbers that refer to, well, "<em>"numbers"</em>", can be confusing."</p>
        <p>"For this reason, there exists a special set of functions that constructs rank lists. We call these functions "<em>"ocean functions"</em>", and their use is called "<em>"ocean notation"</em>"."</p>
        <p>"Each ocean function "<Prim prim=Join/>"s a value to a list. They are as follows:"</p>
        <table>
        <tr><th>"Function"</th><th>"Rank Item"</th></tr>
        {
            Primitive::all()
                .filter_map(|p| p.ocean_constant().map(|c| (p, c)))
                .map(|(prim, c)| {
                    view! {
                        <tr>
                            <td><Prim prim=prim/></td>
                            <td><code>{uiua::array::Array::from(c).show()}</code></td>
                        </tr>
                    }
                })
                .collect::<Vec<_>>()
        }
        </table>
        <p>"The example above with "<Prim prim=Level/>" can be rewritten using "<Prim prim=Rock/>" and "<Prim prim=Surface/>"."</p>
        <Editor example="≑⋄⋄~(↻⊙⊏) 2 0_3_1 ↯3_4⇡12"/>
        <p>"If you wanted to factor in the rows of another array, you could simply add another "<Prim prim=Surface/>"."</p>
        <Editor example="≑⋄⋄~~(↻⊙'⊏⊙'⊂∶) 2 0_3_1 ↯3_4⇡12 ↯3_2⇡6"/>
        <p>"Ocean functions are syntactically special. While they are not modifiers, adjacent ocean functions are parsed as a single unit so they do not have to be surrounded with "<code>"()"</code>"s. They are otherwise normal function."</p>
        <p>"Ocean functions work to specify rank lists because if a rank-generic modifier's first argument is a monadic function, it will push an empty list for the function to work on. Ocean functions are monadic, so a chain of ocean functions form a monadic function."</p>

        <h2 id="combinate"><Prim prim=Combinate/></h2>
        <p><Prim prim=Combinate/>" is a rank-generic version of "<Prim prim=Table/>" and "<Prim prim=Cross/>". It functions similarly to "<Prim prim=Level/>", except instead of calling its function on every tuple of matching rows, it calls it on every "<em>"combination"</em>" of rows."</p>
        <Editor example="◳⋄~~(▽⊙⊂) 3 [1 2 3] [4 5 6]"/>
        <Editor example="◳∸≃(+×10) ,, ↯2_2⇡4 ↯2_4⇡8"/>

        <h2 id="fold"><Prim prim=Fold/></h2>
        <p><Prim prim=Fold/>" uses fixed rank values as accumulators. Its function's outputs set the new value of the accumulator for the next iteration."</p>
        <p>"One basic use is to reduce with a default value."</p>
        <Editor example="∧⋄~+ 10 [1 2 3]"/>
        <Editor example="∧⋄~+ 10 []"/>
        <p>"Here is an example that implements a simple stack instruction set. The initially empty stack is marked as an accumulator with "<Prim prim=Rock/>"."</p>
        <p><code>"0"</code>" duplicates the top stack value, "<code>"1"</code>" pushes a 1, "<code>"2"</code>" adds the top 2 stack values, and "<code>"3"</code>" subtracts the top 2 stack values."</p>
        <Editor example="\
Add ← ⊂/+⊃↙↘2
Sub ← ⊂/-⊃↙↘2
f ← (⊂⊢.|⊂1|Add|Sub)∶
∧⋄~f [] [1 0 2 0 2 0 2 1 3]"/>
    }
}

#[component]
fn TutorialCustomModifiers() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Custom Modifiers - Uiua Docs"/>
        <h1>"Custom Modifiers"</h1>
        <p>"Defining your own functions that work on arrays is pretty easy. Just a name, a "<code>"←"</code>" and you're done."</p>
        <p>"But what if you want to define functions that use other functions?"</p>

        <h2 id="placeholders-and-bangs">"Placeholders and "<code>"!"</code>"s"</h2>
        <p>"Anywhere you can put a built-in or inline function, you can also put a "<code>"^"</code>". This is called a "<em>"placeholder"</em>"."</p>
        <p>"Any named function with "<code>"^"</code>"s in it becomes a modifier."</p>
        <p>"However, there is one additional requirement: custom modifiers must have names that end in as many "<code>"!"</code>"s as the number of functions they take."</p>
        <p>"Lets look at a simple example using "<Prim prim=Reduce/>". It reduces a function over the numbers up to the given range."</p>
        <Editor example="\
ReduceRange! ← |1 /^+1⇡
ReduceRange!+5
ReduceRange!×4"/>
        <p>"Custom modifiers "<em>"must"</em>" have stack signatures declared."</p>
        <p>"A custom modifier can take as many functions as you want."</p>
        <Editor example="\
F!!! ← |2 ⊂/^⊃^^
F!!!+×⊂ [1 2 3][4 5 6]"/>
        <p>"Each "<code>"^"</code>" refers to a different function. If you want to use that function more than once in the modifier, you'll have to get creative."</p>
        <p>"Here, we reduce with the same function multiple times by using "<Prim prim=Repeat/>"."</p>
        <Editor example="\
ReduceAll! ← |1 ⍥(|1 /^)⧻△.
ReduceAll!+[1_2_3 4_5_6]"/>

        <br/>
        <br/>
        <hr/>
        <p>"This is the end of the tutorial that pertains to writing "<em>"programs"</em>"."</p>
        <p>"If you want to use Uiua to write "<em>"software"</em>", then read on for the sections on modules and testing."</p>
        <p>"If you don't care about that stuff and want to learn more about the language, you can check out:"</p>
        <ul>
            <li><A href="/docs#functions">"The list of all functions"</A></li>
            <li><A href="/docs#other-docs">"Other language topics"</A></li>
            <li>"The online "<A href="/pad">"pad"</A>" for writing longer code"</li>
            <li><A href="/docs/isms">"Uiuisms"</A>", a currated list of common operations"</li>
        </ul>
    }
}

#[component]
fn TutorialModules() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Modules - Uiua Docs"/>
        <h1>"Modules"</h1>
        <p>"Modules are a way to organize your code in Uiua. Any Uiua file can be a used as a module."</p>

        <h2 id="import"><Prim prim=Sys(SysOp::Import)/></h2>
        <p>"The "<Prim prim=Sys(SysOp::Import)/>" function allows you to import items from modules. It expects a file path and a binding name from that file, both as strings."</p>
        <p>"There is no file system here on the website, but there is a test module that can always be imported as "<code>"example.ua"</code>". It's contents is:"</p>
        <Editor example={&example_ua(|ex| ex.clone())}/>
        <p>"The "<code>"Increment"</code>" function defined in the example module can be imported with "<Prim prim=Sys(SysOp::Import)/>" then immediately bound so that it can be used locally."</p>
        <Editor example="Inc ← &i \"example.ua\" \"Increment\"\nInc 5"/>
        <p>"Lets import and use them all."</p>
        <Editor example="\
Inc ← &i \"example.ua\" \"Increment\"
Dub ← &i \"example.ua\" \"Double\"
Sqr ← &i \"example.ua\" \"Square\"
Inc Sqr Dub 5"/>
        <p>"This is a little verbose, so we can make a function that imports a given item."</p>
        <Editor example="\
ex ← &i \"example.ua\"
Inc ← ex \"Increment\"
Dub ← ex \"Double\"
Sqr ← ex \"Square\"
Inc Sqr Dub 5"/>

    }
}

#[component]
fn TutorialTesting() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Testing - Uiua Docs"/>
        <h1>"Testing"</h1>
        <h2 id="test-scopes">"Test Scopes"</h2>
        <p>"Test scopes are regions of code that are delimited with "<code>"---"</code>"s at the top level of a file. They are meant to be used with "<Prim prim=Assert/>"."</p>
        <Editor example="Square ← ×.\n---\n⍤.=9 Square 3\n⍤.=225 Square 15\n---"/>
        <p><Prim prim=Assert/>" will return an error when its second argument is anything other than "<code>"1"</code>"."</p>
        <Editor example="Square ← ×.\n---\n⍤.=25 Square 4\n---"/> // Should fail
        <p>"The first argument to "<Prim prim=Assert/>" is the value that will be thrown if the assertion fails. In the examples above, we have simply been "<Prim prim=Dup/>"ing the test value. We can throw a message instead."</p>
        <Editor example=r#"Square ← ×.
---
⍤"3² is not 9!" =9 Square 3
⍤"4² is not 25!" =25 Square 4
---"#/>
        <p>"One nice pattern for writing tests is to put the expected result before the test computation and use"<Prim prim=Assert glyph_only=true/><Prim prim=Flip glyph_only=true/><Prim prim=Match glyph_only=true/><Prim prim=Over glyph_only=true/>"."</p>
        <p>"If the result does not match the expectation, that incorrect result will be thrown."</p>
        <Editor example="---\n⍤∶≍, 4 +2 2 # Passes\n---"/>
        <Editor example="---\n⍤∶≍, [2 3 5] +1 [1 2 3]\n--- #  ↓↓↓↓↓↓↓"/> // Should fail

        <h2 id="run-modes">"Run Modes"</h2>
        <p>"Whether tests will run or not depends on how you run the code."</p>
        <p>"On this website, both test and non-test code will always be run."</p>
        <p>"However, if you use the "<A href="/docs/install">"native interpreter"</A>", you have a few options."</p>
        <p><code>"uiua watch"</code>" will run all code, including tests."</p>
        <p><code>"uiua run"</code>" will only run non-test code."</p>
        <p><code>"uiua test"</code>" will only run test code, but also any non-test bindings and any non-test code which makes imports."</p>
    }
}
