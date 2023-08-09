use std::fmt::Display;

use enum_iterator::{all, Sequence};
use leptos::*;
use leptos_router::*;
use uiua::{primitive::Primitive, SysOp, Uiua};

use crate::{code::*, editor::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Sequence)]
#[repr(u8)]
pub enum TutorialPage {
    Basic,
    Math,
    Arrays,
    Types,
    Bindings,
    Functions,
    Modules,
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
            Self::Modules => "Modules",
        }
    }
}

#[component]
pub fn Tutorial(page: TutorialPage) -> impl IntoView {
    let tut_view = match page {
        TutorialPage::Basic => view! {  <TutorialBasic/> }.into_view(),
        TutorialPage::Math => view! {  <TutorialMath/> }.into_view(),
        TutorialPage::Arrays => view! {  <TutorialArrays/> }.into_view(),
        TutorialPage::Types => view! {  <TutorialTypes/> }.into_view(),
        TutorialPage::Bindings => view! {  <TutorialBindings/> }.into_view(),
        TutorialPage::Functions => view! {  <TutorialFunctions/> }.into_view(),
        TutorialPage::Modules => view! {  <TutorialModules/> }.into_view(),
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
            if let (Some(name), Some(ascii), Some(_)) = (p.name(), p.ascii(), p.unicode()) {
                Some(view! {
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

    view! {
        <h1>"Basic Stack Operations and Formatting"</h1>
        <h2 id="the-stack">"The Stack"</h2>
        <p>"In Uiua, all operations operate on a global stack. Lines of code are evaluated from right-to-left, top-to-bottom"</p>
        <br/>
        <p>"A number simply pushes its value onto the stack."</p>
        <Editor example="5"/>
        <Editor example="1 2 3"/>
        <p>"Operators pop values off the stack and push their results."</p>
        <p>"For example, "<PrimCode prim=Primitive::Add glyph_only=true/>" pops two values off the stack and pushes their sum."</p>
        <Editor example="+ 1 2"/>
        <p><PrimCode prim=Primitive::Mul glyph_only=true/>", of course, multiplies the two values instead."</p>
        <Editor examples={&["+", "1", "×", "2", "3"]} help={&["", "Try the arrows to see how the stack changes with each operation."]}/>
        <p>"In the editor, items that end up on the "<em>"top"</em>" of the stack are shown at the "<em>"bottom"</em>" of the output. This is so that consecutive lines of code show their outputs in the correct order."</p>
        <Editor example="5\n+1 2\n\"Hello, World!\"\n+1 'a'"/>
        <p>"Operations can span multiple lines. Every line uses the same stack!"</p>
        <Editor examples={&["1 2", "+", "5", "×"]} progress_lines=true/>

        <h2 id="comments">"Comments"</h2>
        <p>"Comments are denoted with "<code>"#"</code>" and run to the end of the line."</p>
        <Editor example="5 # This is a comment"/>
        <p>"Uiua does not have multiline comments."</p>

        <h2 id="formatting">"Formatting"</h2>
        <p>"Most Uiua built-in functions use special unicode characters. To type multiplication and division signs, you can use "<code>"*"</code>" and "<code>"%"</code>" respectively. Then, run the code to format the ASCII characters into unicode."</p>
        <Editor example="# Click Run to format!\n%6 *3 8" help={&["", "⇡Click⇡"]}/>
        <p>"Most built-in functions have names you can type rather than symbols. Formatting works on these too. "<em><b>"This is the primary way of entering Uiua's glyphs."</b></em></p>
        <p>"Try formatting the lines below by clicking "<b>"Run"</b>"."</p>
        <Editor examples=&{["max", "sqrt", "10", "mod", "10", "pow", "2", "8"]}/>
        <Editor example="abs +`1 `2"/>
        <p>"You don't have to type the whole name, just enough to disambiguate it from others"</p>
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
        <p>"There are a few functions that work on the stack itself. Some of these are critical and be found scattered across all Uiua code."</p>
        <h2><PrimCode prim=Dup/></h2>
        <p><PrimCode prim=Dup/>" duplicates the top item on the stack."</p>
        <p>"In general, functions do not leave their arguments on the stack. If you want to reuse a value, you must "<PrimCode prim=Dup/>" it first."</p>
        <p>"For example, if you wanted to square a number, you could "<PrimCode prim=Dup/>" it, then "<PrimCode prim=Mul/>"."</p>
        <Editor example="×.4"/>
        <br/>
        <h2><PrimCode prim=Flip/></h2>
        <p><PrimCode prim=Flip/>" swaps the top two items on the stack."</p>
        <p>"This is useful when you want to call a function that takes two arguments, but the arguments are on the stack in the wrong order."</p>
        <p>"For example, if you wanted to get the reciprocal of a number, you would "<PrimCode prim=Div/>" "<code>"1"</code>" by it. But, if the number is already on the stack, you would need to use "<PrimCode prim=Flip/>"."</p>
        <Editor example="÷1 5"/>
        <Editor example="÷~1 5"/>
        <Editor example="~1 2 3 4 5"/>
        <br/>
        <h2><PrimCode prim=Over/></h2>
        <p><PrimCode prim=Over/>" is like "<PrimCode prim=Dup/>", but it duplicates the second item on the stack instead of the first."</p>
        <Editor example=",1 2 3 4"/>
        <Editor example="+×, 3 5"/>
        <br/>
        <h2><PrimCode prim=Pop/></h2>
        <p><PrimCode prim=Pop/>" removes the top item from the stack."</p>
        <p>"This is useful when you want to discard a value that you do not need."</p>
        <Editor examples={&["1", ";", "2", "3", "4", ";", "5", "6"]}/>
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
            let glyph = p.unicode();
            let ascii = p
                .ascii()
                .map(|s| s.to_string())
                .or_else(|| glyph.filter(|c| c.is_ascii()).map(|c| c.to_string()));
            view! {
                <tr>
                    <td><PrimCode prim=p/></td>
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
        <Editor examples={&["×", "+", "+", "1", "2", "3", "4"]} help={&["", "Click the arrows to see how the expression is built up"]}/>
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
        <p><b>"Strand notation"</b>" uses underscores to connect elements."</p>
        <Editor example="1_2_3"/>
        <Editor example="\"Hello\"_\"World\""/>
        <Editor example="+_-_×_÷"/>
        <p>"Strand notation is good when you want to create short and/or simple arrays. For longer or more complex arrays, you can use stack notation."</p>
        <p><b>"Stack notation"</b>" uses brackets to group elements."</p>
        <Editor example="[1 2 3]"/>
        <Editor example="[¯5 37 42 π]"/>
        <p>"What's cool about stack notation is that it is "<em>"not"</em>" just a way to list elements. The code between the brackets runs from right to left as it normally would. When it is done, any items on the stack higher than when it started are put into the array. This gives you some cool ways to create arrays."</p>
        <p>"Note that "<PrimCode prim=Dup/>" duplicates the top item on the stack."</p>
        <Editor example="[...5]"/>
        <Editor example="[×2.×2.×2.×2 .2]"/>
        <Editor example="[+1 2 +3 4]"/>
        <p>"Of course, you can also use stack notation to make multidimensional arrays."</p>
        <Editor example="[1_2_3 4_5_6]"/>
        <Editor example="[...[1 2 3]]"/>
        <p>"More preceisely, stack notation "<PrimCode prim=Couple/>"s the first two stack items created between the "<code>"[]"</code>" and "<PrimCode prim=Join/>"s the to that coupling."</p>
        <p>"Unlike strand notation, stack notation may span multiple lines. The lines are still executed in the normal order."</p>
        <Editor example="\
[1 2 3
 4 5 6
 7 8 9]"/>
        <Editor example="\
[[1 2 3]
 [4 5 6]
 [7 8 9]]"/>
        <p>"If you want the rows to be in the order that they appear in the code, use "<PrimCode prim=Reverse/>"."</p>
        <Editor example="\
⇌[[1 2 3]
  [4 5 6]
  [7 8 9]]"/>

        <h2 id="shape-len-rank"><PrimCode prim=Shape/>", "<PrimCode prim=Len/>", and "<PrimCode prim=Rank/></h2>
        <p>"Other than their data, arrays also have a property called their "<b>"shape"</b>". Shape is a list of non-negative integers that describes the array's size along each of its axes."</p>
        <p>"We can get the array's shape with the "<PrimCode prim=Shape/>" function. It's a triangle because a triangle is a shape."</p>
        <Editor example="△[1 2 3]"/>
        <Editor example="△5"/>
        <Editor example="△[[1 2 3] [4 5 6]]"/>
        <Editor example="△[...[1 2 3]]"/>
        <p>"From shape we can derive two closely-related properties called "<b>"length"</b>" and "<b>"rank"</b>"."</p>
        <p><PrimCode prim=Len/>" is the number of "<em>"major cells"</em>" of the array. This is the number of elements for a 1D array and the number of rows for a 2D array. Length is always equal to the first number in the shape (or 1 if the shape is empty)."</p>
        <p><PrimCode prim=Rank/>" is the number of dimensions of the array."</p>
        <Editor example="△[1 2 3]\n⧻[1 2 3]\n∴[1 2 3]"/>
        <p><PrimCode prim=Rank/>" is equivalent to the "<PrimCode prim=Len/>" of the "<PrimCode prim=Shape/>"."</p>
        <Editor example="= ∴[1 2 3] ⧻△[1 2 3]"/>

        <h2 id="pervasion">"Pervasion"</h2>
        <p>"Most operations that apply to scalars are what is called "<em>"pervasive"</em>" when it comes to arrays. This means that the operations automatically applies to every item in the array."</p>
        <Editor example="+1 1_2_3\n√[4 9 16]\n+1_2_3 4_5_6"/>
        <p>"When doing a pervasive operation on two arrays, their shape "<em>"prefixes"</em>" must match."</p>
        <Editor example="+[1 2] [3 4 5]"/>
        <Editor example="△10_20\n△[3_4_5 6_7_8]\n+10_20 [3_4_5 6_7_8]"/>

        <h2 id="flat-array-model">"The Flat Array Model"</h2>
        <p>"Most modern array languages allow arrays to contain arrays. The simplest model for this is the "<a href="https://aplwiki.com/wiki/Box">"Boxed Array Model"</a>" used in J, where an array may be an array of \"boxes\", which can contain any value but which must be boxed and unboxed. APL uses the "<a href="https://aplwiki.com/wiki/Array_model#Nested_array_theory">"Nested Array Model"</a>", making nested arrays a little easier to access. BQN uses the "<a href="https://mlochbaum.github.io/BQN/doc/based.html">"Based Array Model"</a>"."</p>
        <p>"Uiua does not use any of these. It sticks to the Flat Array Model of the original APL. In Uiua, "<b>"you cannot nest arrays inside other arrays"</b>"."</p>
        <p>"This may seem like a regression, but this model was picked for two reasons. For one, it is easier to make array algorithms fast when you only have to check their type once. More importantly than the implementation, however, is that the Flat Array Model makes array functions easier to reason about. In Uiua, you never have to wonder whether a function like "<PrimCode prim=Windows/>" or "<PrimCode prim=Partition/>" return an array or an array of arrays. The answer is always the same."</p>
        <p>"If you have worked with other array languages, you may be thinking, \"Sure, the Flat Array Model is simple, but it is very limited. How do you represent arrays with subarrays of different lengths?\" The answer is "<b>"fill values"</b>"."</p>
        <p>"Many operations that work on multiple arrays will insert fill values when shapes do not match. The most basic is stack notation, like in the error in the example above."</p>
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
            <li><b>"Number"</b></li>
            <li><b>"Character"</b></li>
            <li><b>"Function"</b></li>
        </ul>

        <h2 id="numbers">"Numbers"</h2>
        <p>"Numbers are decimal numbers with floating precision. They are represented as 64-bit floating-point."</p>
        <Editor example="[5 0 3.2 ¯1.1 π ∞]"/>
        <p>"Most math operations can only be applied to numbers."</p>
        <p>"Even though numbers can have a fractional part, many built-in functions require whole numbers. These functions will return an error if given a non-whole number."</p>
        <p>"One such example is "<PrimCode prim=Pick/>"."</p>
        <Editor example="⊡ 2 [4 7 9 1 0]"/>
        <Editor example="⊡ 3.1 [4 7 9 1 0]"/>
        <p>"If you want to convert a number to a whole number, you can use "<PrimCode prim=Floor/>", "<PrimCode prim=Ceil/>", or "<PrimCode prim=Round/>"."</p>

        <h2 id="characters">"Characters"</h2>
        <p>"Characters are represented as 32-bit Unicode codepoints."</p>
        <p>"Character literals, delimited by "<code>"'"</code>"s, create "<PrimCode prim=Rank/><code>"0"</code>" character arrays."</p>
        <Editor example="'a' 'b'"/>
        <Editor example="['u' 'i' 'u' 'a']"/>
        <p>"String literals, delimited by "<code>"\""</code>"s, create "<PrimCode prim=Rank/><code>"1"</code>" character arrays."</p>
        <Editor example="\"Hello, World!\""/>
        <p>"You can make strings span multiple lines with a "<code>"$"</code>" followed by a space on each line."</p>
        <p>"These do not require "<code>"\""</code>"s."</p>
        <Editor example="print $ Hello, \n      $ World!"/>
        <p>"This style of string is also useful when your string contains a lot of quotes that you don't want to escape."</p>
        <Editor example="$ An then she was like, \"No way!\"\n$ And I was like, \"Way...\""/>
        <br/>

        <h2 id="character-arithmetic">"Character Arithmetic"</h2>
        <p>"Characters and numbers exist in an "<a href="https://en.wikipedia.org/wiki/Affine_space">"affine space"</a>", the same as in "<a href="https://mlochbaum.github.io/BQN/doc/arithmetic.html#character-arithmetic">"BQN"</a>"."</p>
        <p>"You can "<PrimCode prim=Add/>" "<span class="number-literal-span">"numbers"</span>" and "<span class="string-literal-span">"characters"</span>" to get another character."</p>
        <p>"You can "<PrimCode prim=Sub/>" a "<span class="number-literal-span">"number"</span>" from a character to get another character."</p>
        <p>"You can "<PrimCode prim=Sub/>" two "<span class="string-literal-span">"characters"</span>" to get a "<span class="number-literal-span">"number"</span>"."</p>
        <p><em>"No"</em>" other arithmetic operations can be done on "<span class="string-literal-span">"characters"</span>"."</p>
        <Editor example="+1 'a'"/>
        <Editor example="-'a' 'z'"/>
        <Editor example="+'a' 'b'"/>

        <h2 id="functions">"Functions"</h2>
        <p>"Functions are usually used as scalars, but they are still arrays. Most array operations that work on number and character arrays work on arrays of functions as well."</p>
        <p>"Functions will be discussed more in a "<A href="/docs/functions">"later section"</A>"."</p>

        <h2>"Type agreement"</h2>
        <p id="type-agreement">"For functions that work on the structure of arrays rather than their values, the types of the arrays must match."</p>
        <Editor example="⊂ 1_2 3"/>
        <Editor example="⊟ \"Hello\" \"World\""/>
        <Editor example="⊟ 1_2_3 \"dog\""/>
    }
}

#[component]
fn TutorialBindings() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Bindings"</h1>
        <p>"Bindings are global names that can be given to Uiua values. They are denoted with "<code>"←"</code>", which the formatter will convert from "<code>"="</code>" when appropriate."</p>
        <Editor example="a = 3\nb = 5\n+ a b" help={&["", "Try running to format the ="]}/>
        <p>"Valid binding names can be made up of any sequence of uppercase or lowercase alphabetic characters OR a single non-alphanumeric character that is not already used for a Uiua function."</p>
        <p>"Unlike most programming languages, binding names in Uiua "<em>"cannot"</em>" contain numbers or underscores."</p>
        <Editor example="numone ← 1\nNuMtWo ← 2\n😀 ← \"happy\""/>
        <Editor example="variable_1 ← 5"/>
        <p>"Bindings are case-insensitive."</p>
        <Editor example="foo ← 5\n+ FOO fOo"/>
        <p>"The compiler can somtimes mistake all-lowercase binding names for unformatted built-in functions."</p>
        <p>"Here, the compiler thinks that "<code>"part"</code>" is "<PrimCode prim=Partition/>"."</p>
        <Editor example="part = 5" help={&["", "Run to format and reveal why this does not work"]}/>
        <p>"To fix this issue, simply change the binding's capitalization."</p>
        <Editor example="Part ← 5\n×2 Part"/>
        <p>"Binding lines run the code right of the "<code>"←"</code>", then pop the top value off the stack and bind it to the name on the left."</p>
        <p>"Note, though, that an empty right side is perfectly valid! This means you can bind values that were create on previous lines."</p>
        <Editor example="×6 7\nanswer ←\n[answer]"/>

        <h2 id="binding-functions">"Binding Functions"</h2>
        <p>"If you start a binding with a captial letter or an unused glyph, then instead of evaluating its right side immediately, the right side will be bound as a function."</p>
        <p>"This is the primary way to make named functions in Uiua."</p>
        <Editor example="TimesThree ← ×3\nTimesThree 7"/>
        <Editor example="👋 ← ⊂\"Hello, \"\n👋 \"World!\""/>
        <Editor example="⍨ ← ~\n⍪ ← ⊂\n⍳ ← ⇡\n⍪⍨⍳3⍳5 # Truly an abomination"/>
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
        <p>"For example, "<PrimCode prim=Reduce/>" applies a function \"between\" all rows of an array."</p>
        <Editor example="/+ 1_2_3_4"/>
        <p><PrimCode prim=Scan/>" is similar, but it returns all the intermediate results."</p>
        <Editor example="\\+ 1_2_3_4"/>
        <p>"The main docs page has "<A href="/docs#monadic-modifiers">"a list"</A>" of all of the built-in modifiers."</p>

        <h2 id="inline-functions">"Inline Functions"</h2>
        <p>"In addition to creating a new function with a capitalized binding name, as discussed in the "<A href="/docs/bindings">"previous sections"</A>", functions in Uiua can also be created with "<code>"(...)"</code>"."</p>
        <p>"This is usually only necessary when you need to call multiple functions within a modifier."</p>
        <p>"For example, if you wanted to make an array that pairs each element of an array with its reciprocal, you could use "<PrimCode prim=Each/>"."</p>
        <Editor example="∵(⊂÷~1.) 1_2_4_5"/>
        <p>"Or, if you wanted to get the last element of each row of an array, you could use "<PrimCode prim=Rows/>"."</p>
        <Editor example="A ← [2_5_3 0_2_1 0_0_2]\nA\n≡(⊢⇌)A"/>

        <h2 id="dfns">"Dfns"</h2>
        <p>"A dfn (pronounced \"dee-fun\") is a block of code that can bind values locally to single-character names."</p>
        <p>"Dfns are created with "<code>"{...}"</code>"."</p>
        <p>"Unlike functions created with "<code>"(...)"</code>", dfns are called imediately, much like a built-in function."</p>
        <p>"The number of arguments that a dfn takes is determined by which single-character lowercase ASCII names it refers to. This means you can use any of 26 names: "<code>"a"</code>", "<code>"b"</code>", "<code>"c"</code>", ... "<code>"z"</code>". A dfn that only refers to "<code>"a"</code>" takes 1 argument. A dfn that refers to "<code>"z"</code>" takes 26."</p>
        <p>"As an example, you could use a dfn to manually implement "<PrimCode prim=Flip/>" (don't do this)."</p>
        <Editor example="[1 2]\n[~ 1 2]\n[{b a} 1 2]"/>
        <p>"Dfns are particularly useful when you have to juggle three or more arguments."</p>
        <p>"If you tried to implement the "<a href="https://en.wikipedia.org/wiki/Quadratic_formula">quadratic formula</a>" with only stack operations, you would have a very hard time. Thankfully, dfns make it pretty simple."</p>
        <Editor example="Quadratic ← {÷ ×2a -b ⊟¯. √- ××4a c ⁿ2 b}\nQuadratic 1 2 0"/>
        <p>"Dfns are also required if you want to use "<PrimCode prim=Recur/>". The dfn that contains the "<PrimCode prim=Recur/>" will be the thing that recurs."</p>
        <p>"Here is a recursive fibonacci function."</p>
        <Editor example="{:(+ ↬-1a ↬-2a)_a <2a} 10"/>
        <p>"It is an error to access a dfn's argument outside of the dfn. This is only possible if you smuggle out the argument reference inside a function."</p>
        <Editor example=":{(a)}5"/>
        <p>"Dfns that immediately follow a modifier are called as the modifier's function."</p>
        <Editor example="\\{-a ×a b} 1_2_3_4_5"/>

        <h2 id="format-strings">"Format Strings"</h2>
        <p>"Prefixing a string with "<code>"$"</code>", creates a format string. A format string is a function that, like a dfn, is called immediately. It takes an argument for each "<code>"_"</code>" in the string and replaces it with the stringified version."</p>
        <Editor example="$\"Hello, _!\" \"World\""/>
        <Editor example="Greet ← $\"Hello, _!\"\nGreet \"user\""/>
        <Editor example="x ← 5\n$\"x = _\" x"/>
        <Editor example="$\"_, _, and _\" 1 2 3"/>
        <p>"If you need to use a literal "<code>"_"</code>", you can escape them with "<code>"\\"</code>"."</p>
        <Editor example="$\"\\__\\_\" 27"/>
        <p>"Multi-line strings are implicitly format strings."</p>
        <Editor example="{a b +a b}1 2\nprint $ Do you know what _ + _ is?\n      $ It's _!"/>
    }
}

#[component]
fn TutorialModules() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Modules"</h1>
        <p>"Modules are a way to organize your code in Uiua."</p>

        <h2 id="scopes">"s"</h2>
        <p>"s are a way to create a temporary namespace for bindings that are only used in a small part of your code. Only the names that you want to escape a scope are usable outside it."</p>
        <p>"s begin and end with triple hyphens "<code>"---"</code>". All names declared inside a scope are not available outside of it."</p>
        <Editor example="---\nfoo ← 5\n---\nfoo # foo is not available here"/>
        <p>"Values pushed to the stack inside a scope remain on the stack after the scope ends."</p>
        <p>"You can bind values that were pushed to the stack inside an ended scope by using a "<code>"←"</code>" with nothing on the right side."</p>
        <Editor example="---\na ← 3\nb ← 5\n+ a b\n× a b\n---\nc ← \nd ←\nc_d"/>
        <p>"Note that scopes can only be created at the top level of a file, and they cannot be nested."</p>

        <h2 id="modules-and-use">"Modules and "<PrimCode prim=Use/></h2>
        <p><PrimCode prim=Use/>" is a very special function that extracts a function from a "<em>module</em>"."</p>
        <p>"A module is an array of "<em>"named"</em>" functions."</p>
        <p>"The only way to name a function is to bind it. Named functions can be put into arrays by stranding or surrounding the names with "<code>"()"</code>"."</p>
        <p><PrimCode prim=Use/>" takes the name of the function as a string and the module and returns the function."</p>
        <p>"If you want to give the function a name in the current scope, the name on the left side of the "<code>"←"</code>" must "<em>"not"</em>" be capitalized. (If you do capitalize it, you will end up with a function that returns a function.)"</p>
        <p>"This allows you to export functions by name from a scope."</p>
        <Editor example=r#"---
PlusFive ← +5
Twin ← ⊟.
PlusFive_Twin
---
mymodule ←
twin ← use "twin" mymodule
plusfive ← use "PlusFive" mymodule

Twin PlusFive 3"#/>

        <h2 id="import"><PrimCode prim=Sys(SysOp::Import)/></h2>
        <p>"Finally, we reach the point of all of this. You can import other files as scopes with "<PrimCode prim=Sys(SysOp::Import)/>"."</p>
        <p>"The website's editor has an example file that you can import called "<code>"example.ua"</code>". Its contents is:"</p>
        <Editor example=Uiua::DEFAULT_EXAMPLE_UA/>
        <p>"You can import it with "<PrimCode prim=Sys(SysOp::Import)/>" and then "<PrimCode prim=Use/>" to extract the functions."</p>
        <p>"By using "<PrimCode prim=Dup/>" on the imported module, you can repetedly extract functions from it. Notice the lack of a "<PrimCode prim=Dup glyph_only=true/>" after the last "<PrimCode prim=Use/>"."</p>
        <Editor example=r#"import "example.ua"
square ← use "square".
double ← use "double".
increment ← use "increment"

increment square double 5"#/>
        <p><PrimCode prim=Sys(SysOp::Import)/>" only imports a given file once and caches the results. Subsequent imports of the same file (from anywhere) will not run the file's code again, but they will push its stack values again."</p>
        <p>"In this example, we make some code that prints a message and then generates a random number. We then write the code to a file and import it 3 times. Notice that the message is only printed once, and the same number is returned every time."</p>
        <Editor example=r#"code ← "print \"Loading module\"\nrand"
FWriteAll "test.ua" code
⍥(import "test.ua")3"#/>
    }
}
