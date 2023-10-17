use leptos::*;
use leptos_router::*;
use uiua::{primitive::Primitive, SysOp};

use crate::{editor::Editor, examples::LOGO, Prim};

#[component]
pub fn Tour() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Uiua Language Tour"</h1>

        <h2 id="the-union-of-two-paradigms">"The Union of Two Paradigms"</h2>
        <p>"Uiua is a programming language that incorporates two of the less-common programming paradigms: "<b>"array-oriented"</b>" and "<b>"stack-based"</b>"."</p>
        <p>"An "<b>"array-oriented"</b>" language is one where the primary data structure is the array. In array languages, many operations that can apply to a single value can also apply to every value in an array. This is known as "<em>"rank-polymorphism"</em>"."</p>
        <p>"A "<b>"stack-based"</b>" language is one where all operations manipulate a global stack of values. Functions pop values off the top of the stack, perform their calculation, then push the results back on to the stack."</p>
        <p>"In Uiua, functions work on a global stack of arrays."</p>
        <p>"That's enough introduction, let's see some code!"</p>
        <Editor examples={&["+", "1", "×", "2 ", "⇡", "10"]}/>
        <p>"Uiua code runs from "<A href="../rtl">"right to left"</A>", top to bottom. Operators are put to the "<em>"left"</em>" of their arguments, rather than in-between."</p>
        <p>"This program makes an array of all the numbers less than 10, multiplies each one by 2, then adds 1 to each."</p>
        <p>"If you want to see how that works step-by-step, try clicking the arrows beside the Run button."</p>
        <p>"Now, I can already hear you asking, "<em>"\"Wait, what is that funny arrow? How am I supposed to type the multiplication sign?\""</em></p>
        <p>"Unlike some other array languages, Uiua does not require a special keyboard configuration or an editor with custom keybindings. Instead, you can type either the ASCII symbol or the name of a built-in function, then the Uiua formatter will convert it to the correct unicode glyph."</p>
        <p>"In this case, the ASCII symbol for multiplication is "<code>"*"</code>" and the name of the funny arrow is "<Prim prim=Range/>"."</p>
        <p>"On this website, you can format by clicking "<b>"Run"</b>" or by pressing "<b>"Ctrl+Enter"</b>" with the cursor in the text area. Try it out!"</p>
        <Editor example="+1*2 range10" help={&["", "Click! ⇡⇡⇡⇡"]}/>
        <p>"You don't even have to type the whole name of a built-in function, just enough to disambiguate it from the others."</p>
        <Editor example="rang10"/>
        <p>"If you're ever not sure what a glyph is called, you can hover over it to see its name."</p>
        <p>"Click the "<code>"↧"</code>" on the right of the editor to see a list of all the built-in functions."</p>

        <h2 id="the-stack">"The Stack"</h2>
        <p>"A number in Uiua code pushes its value to the stack. On the website's editor, the values on "<em>"top"</em>" of the stack are displayed at the "<em>"bottom"</em>". This is so that sequential lines of code show their result in the correct order."</p>
        <Editor example="10 11\n@c\n+1 2\n\"Hello, World!\"\n# By the way, comments start with #"/>
        <p>"If you like, you can put values on the stack first, then operate on them."</p>
        <Editor examples={&["×", "+", "+ ", "1 ", "2 ", "3 ", "4"]}/>
        <p><Prim prim=Dup/>" duplicates the top value on the stack."</p>
        <Editor examples={&["×", ".", "3"]}/>
        <p><Prim prim=Dup/>" is often used in the examples on this site to show both the input and output of a function."</p>
        <Editor example="√.225"/>
        <p>"For math functions where the order matters, like "<Prim prim=Sub/>" and "<Prim prim=Div/>", what would normally be the second argument is instead the first. This is so you can think of fragments like "<Prim prim=Sub glyph_only=true/><code>"2"</code>" as a single unit."</p>
        <p>"If you want them to work the other way, you can use "<Prim prim=Flip/>", which swaps the top two values on the stack."</p>
        <Editor example="-3 10\n-∶3 10"/>
        <p>"By the way, since "<code>"-"</code>" is for "<Prim prim=Sub/>", use "<code>"`"</code>" for negative numbers. The formatter will turn in into a nice "<code>"¯"</code>"."</p>
        <Editor example="`10"/>
        <p>"You can inspect the top value on the stack at any point with "<Prim prim=Trace/>"."</p>
        <Editor example="+1~×2~×.-3 5"/>

        <h2 id="arrays">"Arrays"</h2>
        <p>"So far, we have only talked about the stack part of Uiua. Now, let's talk about the most important part: Arrays!"</p>
        <p>"An array is a rectangular collection of elements arranged along some number of axes."</p>
        <p>"An array with no axes is called a scalar. All the numbers in the examples above are scalars."</p>
        <p>"An array with one axis is often called a list or a vector. An array with two axes is often called a table or a matrix."</p>
        <p>"You can make simple lists by putting "<code>"_"</code>"s between the elements."</p>
        <Editor example="1_2_3_4"/>
        <p>"You can also just surround them with "<code>"[]"</code>"s."</p>
        <Editor example="[5 6 7 8]"/>
        <p>"But wait! You can put whatever code you want between the brackets! The code runs from right to left as normal, and any values pushed to the stack get put in the array!"</p>
        <Editor example="[×3 . -2 . 10]"/>
        <p>"If you put arrays inside others, you can make arrays with multiple dimensions."</p>
        <Editor example="[1_2_3 [4 5 6] 7_8_9]"/>
        <Editor example="[×3. 4_5_6]"/>
        <p>"Some operations are "<em>"pervasive"</em>", which means they apply to every element of an array or every pair of elements between two arrays. All the math operators are pervasive!"</p>
        <Editor example="√[4 9 16]"/>
        <Editor example="×2 [1 2 3]"/>
        <Editor example="+ 1_2_3 4_5_6"/>
        <Editor example="× 2_10 [1_2_3 4_5_6]"/>
        <p>"Arrays have a "<Prim prim=Shape/>" that describes how many elements they have along each axis."</p>
        <Editor example="△5\n△[]\n△[9 1 6]\n△[4_π_9 1_5_∞]"/>
        <p>"The rank of an array is the number of axes it has."</p>
        <p>"The "<Prim prim=Len/>" is the number of rows it has along its first axis."</p>
        <Editor example="a ← [1_2_3_4 5_6_7_8 9_10_11_12]\n△a\n⧻a\n⧻△a # rank"/>
        <p>"If you want to type that fancy "<code>"←"</code>" so you can give names to arrays, you can type "<code>"="</code>" after a name at the start of a line, and the formatter will convert it for you."</p>
        <Editor example="x = 5\n+x x"/>
        <p><code>"←"</code>" just pops the first thing off the stack and assigns it to the name on the left, so if there is already a value on the stack, you don't actually need anything on the right."</p>
        <Editor example="×2 [2 3 4]\nx ←\nx"/>
        <p>"Names are case-sensitive and can only contain letters."</p>

        <h2 id="basic-array-operations">"Basic Array Operations"</h2>
        <p>"You can reverse an array's rows with "<Prim prim=Reverse/>"."</p>
        <Editor example="rev[1 2 3] # Run to format!"/>
        <Editor example="⇌[1_2_3 4_5_6]"/>
        <p>"You can concatenate two arrays with "<Prim prim=Join/>"."</p>
        <Editor example="⊂1 [2 3 4]\n⊂[1 2 3] [4 5 6]"/>
        <p>"You can make two arrays the rows of a new array with "<Prim prim=Couple/>"."</p>
        <Editor example="⊟[1 2 3] [4 5 6]"/>
        <p>"You can get the first element of an array with "<Prim prim=First/>"."</p>
        <Editor example="⊢[1 2 3]"/>
        <Editor example="fir[1_2_3 4_5_6]"/>
        <p><Prim prim=Take/>" and "<Prim prim=Drop/>" can be used to get just part of an array."</p>
        <Editor example="↙3 [1 2 3 4 5]\n↘3 [1 2 3 4 5]"/>
        <p><Prim prim=Reshape/>" changes the shape of an array while keeping the elements in the same order."</p>
        <Editor example="↯3_3 .⇡9"/>
        <p><Prim prim=Transpose/>" rotates the axes of an array."</p>
        <Editor example="trans.[1_2_3 4_5_6]"/>
        <p>"Uiua has a lot of built-in functions like these. You can explore their documentation on the "<A href="/docs#functions">"main docs page"</A>"."</p>

        <h2 id="functions">"Functions"</h2>
        <p>"If you bind a name with "<code>"←"</code>" and the code on the right does not have enough arguments to run, the code will be bound as a function and will not run until the name is used."</p>
        <Editor example="f ← +1\nf5"/>
        <Editor example="👋 ← ⊂\"Hello, \"\n👋\"World\""/>

        <h2 id="modifiers">"Modifiers"</h2>
        <p>"Modifiers (called operators or adverbs in some other array languages) are functions that take other functions as arguments. The built-in modifiers are parsed so that if their function argument(s) immediately follow them, the function is run inside the modifier rather than before it."</p>
        <p><Prim prim=Reduce/>" is a modifier many array-language aficionados will be familiar with. It takes its function and applies it \"between\" the items of an array."</p>
        <p>"One basic use of "<Prim prim=Reduce/>" is to sum an array."</p>
        <Editor example="/+ [1 2 3 4 5]"/>
        <p>"It works on multi-dimensional arrays too! In this case, it adds each row to the next."</p>
        <Editor example="/+ .[1_2_3 4_5_6 7_8_9]"/>
        <p><Prim prim=Rows/>" applies a function to each row of an array."</p>
        <Editor example="x ← [1_2_3 4_5_6]\n  x\n ⇌x\n≡⇌x"/>
        <p><Prim prim=Rows/>" also works "<em>"between"</em>" two arrays if it is given a dyadic function like "<Prim prim=Join/>"."</p>
        <Editor example="≡⊂ [1_2 3_4] [5_6 7_8]"/>
        <p>"There are a bunch of other modifiers that are useful in different situations. You can find a "<A href="/docs/modifier">"list of them"</A>" on the main docs page."</p>

        <h2 id="inline-functions">"Inline Functions"</h2>
        <p>"If you need a more complex function for a modifier, you can make an inline function by surrounding code with "<code>"()"</code>"s."</p>
        <p>"Let's use "<Prim prim=Each/>" to get the sum of all the numbers up to each element of an array."</p>
        <p>"For "<Prim prim=Each/>" element, we'll "<Prim prim=Add/><code>"1"</code>", get the "<Prim prim=Range/>" up to that number, then "<Prim prim=Reduce/>" it with "<Prim prim=Add/>"."</p>
        <Editor example="∵(/+ ⇡ +1) .[1_2_3 4_5_6 7_8_9]"/>
        <p>"Small inline functions with only 2 terms can also be replaced with the "<Prim prim=Bind/>" modifier."</p>
        <p>"Here, we use "<Prim prim=First/>" after "<Prim prim=Reverse/>" to get the last element of each row."</p>
        <Editor example="≡'⊢⇌ .[1_2_3 4_5_6 7_8_9]"/>

        <h2 id="fill-and-nested-arrays"><Prim prim=Fill/>" and Nested Arrays"</h2>
        <p>"Here is an array that cannot be constructed normally because its rows have different "<Prim prim=Len/>"s."</p>
        <Editor example="[1 2_3_4 5_6]"/> // Should fail
        <p>"One way to make this array work is to use the "<Prim prim=Fill/>" modifier. You give it a fill value and a function or array that would fail with mismatched shapes, and it will fill in the missing values with the fill value."</p>
        <Editor example="⬚0[1 2_3_4 5_6]"/>
        <p><Prim prim=Fill/>" works with lots of functions. Another one is "<Prim prim=Take/>" when the amount you are taking is more than the length of the array."</p>
        <Editor example="⬚π↙5 [1 2 3]"/>
        <br/>
        <p><Prim prim=Fill/>" is nice, but you don't always want to fill in the missing elements. Sometimes you need to mix values of different shapes or types in an array. To understand Uiua's solution to this problem, you must first understand its "<em>"array model"</em>"."</p>
        <p>"Uiua's array model is similar to that of J. Arrays must be rectangular and cannot mix types. However, the "<Prim prim=Box/>" function can turn any value into a function that pushes that value to the stack. That value can then be extracted with "<Prim prim=Unbox/>". This is similar to J's boxes."</p>
        <Editor example="[□1 □2_3_4 □5_6]"/>
        <p>"Having to use "<Prim prim=Box/>" on every value is kind of annoying, so there is a special syntax for "<Prim prim=Box/>" arrays that uses "<code>"{}"</code>"s instead of "<code>"[]"</code>"s."</p>
        <Editor example="{1 2_3_4 5_6}"/>
        <p>"Many simple functions work on "<Prim prim=Box/>"ed elements without needing to "<Prim prim=Unbox/>" them."</p>
        <Editor example="{1 2_3_4 5_6}\n∵⇌.\n∵⧻."/>
        <Editor example="+5 {1 2_3_4 5_6}"/>
        <p>"For more complex operations, though, you'll need to use "<Prim prim=Unbox/>". Using it with "<Prim prim=Under/>" will re-"<Prim prim=Box/>" the result."</p>
        <Editor example="{\"dog\" \"cat\" \"fish\"}\n∵⍜⊔(⊂∶⇌.)."/>

        <h2 id="multimedia">"Multimedia"</h2>
        <p>"Uiua can natively generate images, audio, and GIFs."</p>
        <p>"On this site, simply leaving an array on the stack that "<em>"looks"</em>" like image or audio data will display it."</p>
        <h3>"Images"</h3>
        <p>"Image data can either be a rank 2 array of grayscale pixel data or a rank 3 array of grayscale with alpha, RGB, or RGBA pixel data."</p>
        <p>"This minimal example uses three different functions on x/y coordinates to generate RGB values and make a pretty gradient."</p>
        <Editor example="⍉≑α(|2 ⊞|⊙.)+_-_× ÷∶⇡.100"/>
        <p>"The Uiua logo is made with Uiua itself!"</p>
        <Editor example=LOGO/>
        <h3>"Audio"</h3>
        <p>"Audio data is just an array of numbers between -1 and 1. The numbers are interpreted as samples of a waveform."</p>
        <p>"This example plays a series of notes."</p>
        <Editor example="\
↯4[0 2 4 7 12 9 7 4]
×220 ⁿ∶2÷12
÷2 ○×τ ♭⊞× ∶÷∶⇡⁅÷8 .&asr"/>
        <h3>"GIFs"</h3>
        <p>"Any array whose rows can all be turned into images can be turned into a GIF."</p>
        <p>"On this site, arrays that look like they should be GIFs will be displayed as GIFs. You can see some on the "<A href="/">"main page"</A>"."</p>
        <p>"GIFs can be explicitly rendered with the "<Prim prim={Sys(SysOp::GifShow)}/>" function."</p>

        <h2 id="next-steps">"Next Steps"</h2>
        <p>"If you want a more in-depth introduction to Uiua, you can check out the "<A href="/docs/basic">"tutorial"</A>"."</p>
        <p>"For information on installing the native Uiua interpreter, see the "<A href="/install">"install page"</A>"."</p>
        <p>"For information on specific functions and modifiers, see the "<A href="/docs#functions">"functions section"</A>" of the main docs page."</p>
        <p>"To see some cool examples, click through the editor at the top of the "<A href="/">"home page"</A>". There are also some interesting, longer examples in the "<a href="https://github.com/uiua-lang/uiua/tree/main/examples">" main Uiua repository on GitHub"</a>"."</p>
        <p>"Check out "<A href="../isms">"Uiuisms"</A>" for a curated list of Uiua functions for solving common problems."</p>
    }
}
