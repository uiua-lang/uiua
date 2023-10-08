use std::fmt::Display;

use enum_iterator::{all, Sequence};
use leptos::*;
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
    AdvancedStack,
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
            Self::AdvancedStack => "Advanced Stack Manipulation",
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
        TutorialPage::AdvancedStack => TutorialAdvancedStack().into_view(),
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
        <Editor example="(cei ceil ceili ceilin ceiling)"/>
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
        <Editor example="+1 ~ ×4 ~ ×. -3 5"/>
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
    }
}

#[component]
fn TutorialArrays() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Arrays"</h1>
        <p>"Uiua is, first and foremost, an array language. The only composite data type is the multimensional array. Arrays have a lot of nice properties, and the language's built-in functions are designed to make it easy to work with them. If you've only ever programmed in non-array languages, then this will be a completely foreign paradigm. In most array languages, most data structures and control flow are replaced with operations on arrays."</p>

        <h2 id="creating-arrays">"Creating Arrays"</h2>
        <p>"Other than with functions, Uiua has two ways to create arrays. They are called "<em>"strand notation"</em>" and "<em>"stack notation"</em>"."</p>
        <p><strong>"Strand notation"</strong>" uses underscores to connect elements."</p>
        <Editor example="1_2_3"/>
        <Editor example="\"Hello\"_\"World\""/>
        <Editor example="+_-_×_÷"/>
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
        <Editor example="⬚0- [1 2] [3 4 5 6 7]"/>
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
        <p>"However, there is an escape hatch for when you really want jagged, nested, or mixed-type arrays. In Uiua, an array of heterogenous values can be simulated with an array of functions. These functions can be used similarly to J's boxes."</p>
        <Editor example="[1 2 [7 8 9]]"/> // Should fail
        <p>"By using "<Prim prim=Box/>", we can turn any value into a function that pushes that value onto the stack. We can then put these functions into an array together."</p>
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

        <h2 id="functions">"Functions"</h2>
        <p>"Functions are usually used as scalars, but they are still arrays. Most array operations that work on number and character arrays work on arrays of functions as well."</p>
        <p>"Functions will be discussed more in some "<A href="/docs/bindings#binding-functions">"later"</A>" "<A href="/docs/functions">"sections"</A>"."</p>

        <h2>"Type agreement"</h2>
        <p id="type-agreement">"For functions that work on the structure of arrays rather than their values, the types of the arrays must match."</p>
        <Editor example="⊂ 1_2 3"/>
        <Editor example="⊟ \"Hello\" \"World\""/>
        <Editor example="⊟ 1_2_3 \"dog\""/> // Should fail
        <p>"There is an exception for functions. Any function that pushes one value onto the stack can be put in an array with a non-function. In this case, the non-function will be turned into a function, similar to "<Prim prim=Box/>"."</p>
        <Editor example="⊟ 5 (+1 2)"/>

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
        <h1>"Modifiers and Functions"</h1>

        <h2 id="modifiers">"Modifiers"</h2>
        <p>"Modifiers are functions that take other functions. If you immediately follow a modifier with its function arguments, the functions will be called inside the modifier rather than outside."</p>
        <p>"For example, "<Prim prim=Reduce/>" applies a function \"between\" all rows of an array."</p>
        <p><PrimCodes prims={[Reduce, Add]}/>" is therefore the sum of all the rows of an array."</p>
        <Editor example="/+ 1_2_3_4"/>
        <p><Prim prim=Scan/>" is similar, but it returns all the intermediate results."</p>
        <Editor example="\\+ 1_2_3_4"/>
        <p><Prim prim=Table/>" applies a function between all combinations of elements of two arrays. This is sometimes called the "<em>"outer product"</em>"."</p>
        <Editor example="⊞+ [5 6 7 8] [10 20 30 40]"/>
        <p>"The main docs page has "<A href="/docs/modifier">"a list"</A>" of all of the built-in modifiers."</p>

        <h2 id="inline-functions">"Inline Functions"</h2>
        <p>"In addition to creating a new function with a capitalized binding name, as discussed in the "<A href="/docs/bindings">"previous section"</A>", functions in Uiua can also be created by surrounding code with "<code>"()"</code>"s."</p>
        <p>"This is usually only necessary when you need to call multiple functions within a modifier."</p>
        <p>"For example, if you wanted to get the last element of each row of an array, you could use "<Prim prim=Rows/>"."</p>
        <Editor example="≡(⊢⇌) .[2_5_3 0_2_1 0_0_2]"/>
        <p>"If you wanted to rotate every row of an array by a fixed amount, you could use "<Prim prim=Distribute/>"."</p>
        <Editor example="∺↻ 2 [1_2_3 4_5_6 7_8_9]"/>
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

        <h2 id="terminating-modifiers">"Terminating Modifiers"</h2>
        <p>"Sometimes you don't want to parse the function(s) following a modifier as being part of the modifier."</p>
        <p>"In these cases, you can use "<code>"|"</code>" to terminate the modifier. Functions after the "<code>"|"</code>" will be considered \"outside\" of it."</p>
        <p>"This is useful when you want the function passed to your modifier to be dynamic."</p>
        <p>"Ignore the "<code>"|1"</code>" for now. It will be explained shortly."</p>
        <Editor example="\
f ← |1 /|:[1 2 3 4 5]
f(+)
f(×)
f(↥)"/>

        <h2 id="stack-signatures">"Stack Signatures"</h2>
        <p>"Bindings and inline functions can have a "<em>"stack signature"</em>" declared with a "<code>"|"</code>" followed by 1 or 2 numbers seperated by a "<code>"."</code>". The first number is the number of arguments the function pops from the stack. The second number is the number of values the function pushes to the stack."</p>
        <p>"The second number is optional. If it is not given, it is assumed to be 1."</p>
        <p>"In bindings, the "<code>"|"</code>" comes after the "<code>"←"</code>". In inline functions, it comes after the "<code>"("</code>"."</p>
        <Editor example="TimesThree ← |1.1 ×3\nTimesThree 7"/>
        <Editor example="TimesThree ← |1   ×3\nTimesThree 7"/>
        <Editor example="∵(|2.1 ⊟.×) 1_2_3 4_5_6"/>
        <p>"Stack signatures are useful for documenting functions to make sure that they are used correctly."</p>
        <p>"A signature declaration is "<em>"required"</em>" if the function's signature cannot be infered. The compiler can usually infer a function's signature unless you are doing something weird with higher-order functions or fiddling with function arrays, or if you are using "<Prim prim=Recur/>"sion."</p>
        <Editor example="∺(⊞|∶,)+_-⇡3"/> // Should fail
        <p>"Simply add a signature declaration to fix this."</p>
        <Editor example="∺(|2 ⊞|∶,)+_-⇡3"/> // Should fail
        <p>"In addition, an error is thrown if a function's signature can be inferred and the inferred signature does not match the declared signature. This can help validate that a function is correct."</p>
        <Editor example="≡(|2 ↻.) 1_2_3 ↯3_3⇡9"/> // Should fail
        <p><strong>"WARNING"</strong>": If the compiler cannot derive the stack signature of a function and you give it one which is "<em>"wrong"</em>", your code may no longer compile in future versions of the language."</p>
    }
}

#[component]
fn TutorialAdvancedStack() -> impl IntoView {
    use Primitive::*;
    view! {
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
fn TutorialModules() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Modules"</h1>
        <p>"Modules are a way to organize your code in Uiua."</p>

        <h2 id="scopes">"Scopes"</h2>
        <p>"Scopes are a way to create a temporary namespace for bindings that are only used in a small part of your code. Only the names that you want to escape a scope are usable outside it."</p>
        <p>"Scopes begin and end with triple hyphens "<code>"---"</code>". All names declared inside a scope are not available outside of it."</p>
        <Editor example="---\nFoo ← 5\n---\nFoo # Foo is not available here"/> // Should fail
        <p>"Values pushed to the stack inside a scope remain on the stack after the scope ends."</p>
        <p>"You can bind values that were pushed to the stack inside an ended scope by using a "<code>"←"</code>" with nothing on the right side."</p>
        <Editor example="---\na ← 3\nb ← 5\n+ a b\n× a b\n---\nc ← \nd ←\nc_d"/>
        <p>"Note that scopes can only be created at the top level of a file (but between other top-level items), and they cannot be nested."</p>

        <h2 id="modules-and-use">"Modules and "<Prim prim=Use/></h2>
        <p><Prim prim=Use/>" is a very special function that extracts a function from a "<em>module</em>"."</p>
        <p>"A module is an array of "<em>"named"</em>" functions."</p>
        <p>"The only way to name a function is to bind it. Named functions can be put into arrays by stranding or surrounding the names with "<code>"()"</code>"."</p>
        <p><Prim prim=Use/>" takes the name of the function as a string and the module and returns the function."</p>
        <p>"This allows you to export functions by name from a scope."</p>
        <Editor example=r#"---
PlusFive ← +5
Twin ← ⊟.
PlusFive_Twin
---
Mod ←
tw ← use "Twin" Mod
pf ← use "PlusFive" Mod

tw pf 3"#/>

        <h2 id="import">"Importing with "<Prim prim=Sys(SysOp::Import)/></h2>
        <p>"Finally, we reach the point of all of this. You can import other files as scopes with "<Prim prim=Sys(SysOp::Import)/>"."</p>
        <p>"System functions like "<Prim prim=Sys(SysOp::Import)/>" are prefixed with "<code>"&"</code>" so that the names of your own functions don't collide with them."</p>
        <p>"The website's editor has an example file that you can import called "<code>"example.ua"</code>". Its contents is:"</p>
        <Editor example={ &example_ua(|ex| ex.clone()) }/>
        <p>"You can import it with "<Prim prim=Sys(SysOp::Import)/>" and then "<Prim prim=Use/>" to extract the functions."</p>
        <Editor example=r#"ex ← &i "example.ua"
Square ← use "Square" ex
Double ← use "Double" ex
Increment ← use "Increment" ex

Increment Square Double 5"#/>
        <p><Prim prim=Sys(SysOp::Import)/>" only imports a given file once and caches the results. Subsequent imports of the same file (from anywhere) will not run the file's code again, but they "<em>"will"</em>" push its stack values again."</p>
        <p>"In this example, we make some code that prints a message and then generates a random number. We then write the code to a file and import it 3 times. Notice that the message is only printed once, and the same number is returned every time."</p>
        <Editor example="\
Code ← $ &p \"Loading module\"
       $ rand
&fwa \"test.ua\" Code
⍥(&i \"test.ua\")3"/>
    }
}

#[component]
fn TutorialTesting() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Testing"</h1>
        <h2 id="test-scopes">"Test Scopes"</h2>
        <p>"Similar to the "<A href="/docs/modules">"scopes discussed in the previous section"</A>", Uiua has "<em>"test scopes"</em>"."</p>
        <p>"Instead of "<code>"---"</code>", test scopes begin and end with "<code>"~~~"</code>"."</p>
        <p>"Test scopes are meant to be used with "<Prim prim=Assert/>"."</p>
        <Editor example="Square ← ×.\n~~~\n⍤.=9 Square 3\n⍤.=225 Square 15\n~~~"/>
        <p><Prim prim=Assert/>" will return an error when its second argument is anything other than "<code>"1"</code>"."</p>
        <Editor example="Square ← ×.\n~~~\n⍤.=25 Square 4\n~~~"/>
        <p>"The first argument to "<Prim prim=Assert/>" is the value that will be thrown if the assertion fails. In the examples above, we have simply been "<Prim prim=Dup/>"ing the test value. We can throw a message instead."</p>
        <Editor example=r#"Square ← ×.
~~~
⍤"3² is not 9!" =9 Square 3
⍤"4² is not 25!" =25 Square 4
~~~"#/>

        <h2 id="run-modes">"Run Modes"</h2>
        <p>"Whether tests will run or not depends on how you run the code."</p>
        <p>"On this website, both test and non-test code will always be run."</p>
        <p>"However, if you use the "<A href="/docs/install">"native interpreter"</A>", you have a few options."</p>
        <p><code>"uiua watch"</code>" will run all code, including tests."</p>
        <p><code>"uiua run"</code>" will only run non-test code."</p>
        <p><code>"uiua test"</code>" will only run test code, but also any non-test bindings and any non-test code which makes imports."</p>
    }
}
