use leptos::*;
use leptos_router::*;
use uiua::primitive::Primitive;

use crate::{code::PrimCode, editor::Editor};

#[component]
pub fn Tour() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Uiua Language Tour"</h1>

        <h2 id="the-union-of-two-paradigms">"The Union of Two Paradigms"</h2>
        <p>"Uiua is a programming language that incorporates two of the less-common programming paradigms: "<b>"array-oriented"</b>" and "<b>"stack-oriented"</b>"."</p>
        <p>"An "<b>"array-oriented"</b>" language is one where the primary data structure is the array. In array languages many operations that can apply to a single value can also apply to every value in an array. This is known as "<em>"rank-polymorphism"</em>"."</p>
        <p>"A "<b>"stack-oriented"</b>" language is one where all operations manipulate a global stack of values. Functions pop values off the top of the stack, perform their calculation, then push the values back on to the stack."</p>
        <p>"In Uiua, functions work on a global stack of arrays."</p>
        <p>"Thats enough introduction, lets see some code!"</p>
        <Editor examples={&["+", "1", "×", "2 ", "⇡", "10"]}/>
        <p>"Uiua code runs from right to left, top to bottom. Operators are put to the "<em>"left"</em>" of their arguments, rather that in between."</p>
        <p>"This program makes an array of all the numbers less than 10, multiplies each one by 2, then adds 1 to each one."</p>
        <p>"If you want to see how that works step-by-step, try clicking the arrows beside the Run button."</p>
        <p>"Now, I can already hear you asking, "<em>"\"Wait, what is that funny arrow? How am I supposed to type the multiplication sign?\""</em></p>
        <p>"Unlike some other array languages, Uiua does not require a special keyboard or an editor with custom keybindings. Instead, you can type either the ASCII symbol or the name of a built-in function, then the Uiua formatter will convert it to the correct unicode glyph."</p>
        <p>"In this case, the ASCII symbol for multiplication is "<code>"*"</code>" and the name of the funny arrow is "<PrimCode prim=Range/>"."</p>
        <p>"On this website, you can format by clicking "<b>"Run"</b>" or by pressing "<b>"Ctrl+Enter"</b>" with the cursor in the text area. Try it out!"</p>
        <Editor example="+1*2 range10" help={&["", "Click! ⇡⇡⇡⇡"]}/>
        <p>"You don't even have to type the whole name of a built-in function, just enough to disambiguate it from the others."</p>
        <Editor example="rang10"/>
        <p>"If you're ever not sure what a glyph is called, you can hover over it to see its name."</p>
        <p>"Click the "<code>"↧"</code>" on the right of the editor to see a list of all the built-in functions."</p>

        <h2 id="the-stack">"The Stack"</h2>
        <p>"A number in Uiua code pushes its value to the stack. On the website's editor, the values on "<em>"top"</em>" of the stack are displayed at the "<em>"bottom"</em>". This is so that sequential lines of code show their result in the correct order."</p>
        <Editor example="10 11\n'c'\n+1 2\n\"Hello, World!\"\n# By the way, comments start with #"/>
        <p>"If you like, you can put values on the stack first, then operate on them."</p>
        <Editor examples={&["×", "+", "+ ", "1 ", "2 ", "3 ", "4"]}/>
        <p><PrimCode prim=Dup/>" duplicates the top value on the stack."</p>
        <Editor examples={&["×", ".", "3"]}/>
        <p>"For math function where the order matters, like "<PrimCode prim=Sub/>" and "<PrimCode prim=Div/>", what would normally be the second argument comes first. This is so you can think of fragments like "<PrimCode prim=Sub glyph_only=true/><code>"2"</code>" as a single unit."</p>
        <p><PrimCode prim=Flip/>" swaps the top two values on the stack."</p>
        <Editor example="-3 10\n-~3 10"/>
        <p>"By the way, since "<code>"-"</code>" is for "<PrimCode prim=Sub/>", use "<code>"`"</code>" for negative numbers. The formatter will turn in into a nice "<code>"¯"</code>"."</p>
        <Editor example="`10"/>

        <h2 id="arrays">"Arrays"</h2>
        <p>"So far, we have only talked about the stack part of Uiua. Now, lets talk about the most important part: Arrays!"</p>
        <p>"An array is a rectangular collection of elements arranged along some number of axis."</p>
        <p>"An array with no axes is called a scalar. All the numbers is the axmples above are scalars."</p>
        <p>"An array with one axis is called a list."</p>
        <p>"An array with two axes is called a table."</p>
        <p>"You can make simple lists by putting "<code>"_"</code>" between the elements."</p>
        <Editor example="1_2_3_4"/>
        <p>"You can also just surround them with "<code>"[]"</code>"s."</p>
        <Editor example="[5 6 7 8]"/>
        <p>"But wait! You can put whatever code you want between the brackets! The code runs from right to left as normal, and any values pushed to the stack get put in the array!"</p>
        <Editor example="[÷2 . -36 . ×4 100]"/>
        <p>"If you put arrays inside others, you can make arrays with multiple dimensions."</p>
        <Editor example="[1_2_3 [4 5 6] 7_8_9]"/>
        <Editor example="[×3. 4_5_6]"/>
        <p>"Operations that are "<em>"pervasive"</em>" apply to every element of an array or every pair of elements between two arrays. All the math operators are pervasive!"</p>
        <Editor example="√[4 9 16]"/>
        <Editor example="×2 [1 2 3]"/>
        <Editor example="+ 1_2_3 4_5_6"/>
        <Editor example="× 2_10 [1_2_3 4_5_6]"/>
        <p>"Array have a "<PrimCode prim=Shape/>" that describes how many elements they have along each axis."</p>
        <p>"The "<PrimCode prim=First/>" item in the "<PrimCode prim=Shape/>" is also called the "<PrimCode prim=Len/>"."</p>
        <p>"The "<PrimCode prim=Len/>" of the "<PrimCode prim=Shape/>" is the "<PrimCode prim=Rank/>"."</p>
        <Editor example="△5\n△[]\n△[9 1 6]\n△[4_π_9 1_5_∞]"/>
        <Editor example="a ← [1_2_3_4 5_6_7_8 9_10_11_12]\n△a\n⧻a\n∴a"/>
        <p>"If you want to type that fancy "<code>"←"</code>" so you can gives names to arrays, you can type "<code>"="</code>" after a name at the start of a line, and the formatter will convert it for you."</p>
        <Editor example="x = 5\n+x x"/>
        <p><code>"←"</code>" just pops the first thing off the stack and assigns it to the name on the left, so if there is already a value on the stack, you don't actually need anything on the right."</p>
        <Editor example="×2 [2 3 4]\nx ←\nx"/>
        <p>"Names are case-"<em>"in"</em>"sensitive and can only contain letters."</p>
        <Editor example="value ← ÷2 [2 3 4]\nVaLuE"/>

        <h2 id="basic-array-operations">"Basic Array Operations"</h2>
        <p>"You can reverse an array's rows with "<PrimCode prim=Reverse/>"."</p>
        <Editor example="rev[1 2 3] # Run to format!"/>
        <Editor example="⇌[1_2_3 4_5_6]"/>
        <p>"You can concatenate two arrays with "<PrimCode prim=Join/>"."</p>
        <Editor example="⊂1 [2 3 4]\n⊂[1 2 3] [4 5 6]"/>
        <p>"You can make two arrays the rows of a new array with "<PrimCode prim=Couple/>"."</p>
        <Editor example="⊟[1 2 3] [4 5 6]"/>
        <p>"You can get the first element of an array with "<PrimCode prim=First/>"."</p>
        <Editor example="⊢[1 2 3]"/>
        <Editor example="fir[1_2_3 4_5_6]"/>
        <p><PrimCode prim=Take/>" and "<PrimCode prim=Drop/>" can be used to get just part of an array."</p>
        <Editor example="↙3 [1 2 3 4 5]\n↘3 [1 2 3 4 5]"/>
        <p><PrimCode prim=Transpose/>" rotates the axes of an array."</p>
        <Editor example="trans.[1_2_3 4_5_6] # 🏳️‍⚧️"/>

        <h2 id="functions">"Functions"</h2>
        <p>"If you bind a name with "<code>"←"</code>", and the thing on the right doesn't have enough arguments, the code will not be run until the name is used."</p>
        <Editor example="f ← +1\nf5"/>
        <Editor example="👋 ← ⊂\"Hello, \"\n👋\"World\""/>

        <h2 id="modifiers">"Modifiers"</h2>
        <p>"Modifiers (called operators or adverbs in some other array languages) are functions that take other functions as arguments. The built-in modifiers are parsed so that if their function argument(s) immediately follow them, the function is run inside the modifier rather than before it."</p>
        <p><PrimCode prim=Reduce/>" is a modifier many array-language afficionados will be familiar with. It takes its function and applies it to the first two rows of an array, then to each result and the next element."</p>
        <p>"One basic use of "<PrimCode prim=Reduce/>" is to sum an array."</p>
        <Editor example="/+ [1 2 3 4 5]"/>
        <p>"It works on multi-dimensional arrays too! In this case, it adds each row to the next."</p>
        <Editor example="/+ .[1_2_3 4_5_6 7_8_9]"/>
        <p><PrimCode prim=Rows/>" applies a function to each row of an array."</p>
        <Editor example="x ← [1_2_3 4_5_6 7_8_9]\n  x\n ⇌x\n≡⇌x"/>
        <br/>
        <h3 id="inline-functions">"Inline Functions"</h3>
        <p>"If you need a more complex function for a modifier, you can make an inline function by surrounding code with "<code>"()"</code>"s."</p>
        <p>"Let's use "<PrimCode prim=Each/>" to get the sum of all the numbers up to each element of an array."</p>
        <Editor example="∵(/+ ⇡ +1) .[1_2_3 4_5_6 7_8_9]"/>
        <p>"There are bunch of other modifiers that are useful in different situations. You can find a "<A href="/docs/modifier">"list of them"</A>" on the main docs page."</p>
    }
}
