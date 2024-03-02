use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use uiua::{Primitive, SysOp, CONSTANTS};

use crate::{editor::Editor, markdown::markdown, Const, Prim, Prims};

#[component]
pub fn Design() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Design - Uiua Docs"/>
        <h1 id="design">"Design"</h1>
        <p>"This page explains the reasons for some of Uiua's design decisions."</p>
        <p>"It serves as a "<a href="https://news.knowledia.com/US/en/articles/more-software-projects-need-defenses-of-design-85ea9e23ffd85f5fde5a2d3d42001393cbce169a">"defense of design"</a>"."</p>

        <h2 id="stack-basing">"Stack Basing"</h2>
        <h3>"Combinators"</h3>
        <p>"When I first started developing Uiua, it was neither stack-based nor array-oriented. What it "<em>"did"</em>" focus a lot on was "<em>"combinators"</em>". I had this whole hierarchy of language-level operators that let you construct arbitrarily complex combinators relatively succinctly."</p>
        <p>"I discovered what a lot of others have discovered when delving deep into tacit code: it's really hard to read and write and reason about."</p>
        <p>"Eventually, I moved to a stack-based model and discovered that you can write almost any 1 or 2 argument combinator with just "<Prim prim=Dup/>", "<Prim prim=Over/>", and "<Prim prim=Flip/>"."</p>
        <p>"Of course, I also made the discovery that juggling 3 or more values on the stack also imposes a high cognitive load on the developer. This is especially true if you try to "<em>"rotate"</em>" the stack like you could with the now-removed functions"<code>"roll"</code>" and "<code>"unroll"</code>". "<Prim prim=Dip/>" replaced the rolling functions as it is more general and easier to reason about, and eventually grew into "<A href="/tutorial/advancedstack#planet-notation">"Planet Notation"</A>"."</p>
        <br/>
        <h3>"Expressions"</h3>
        <p>"Long tacit expressions in most array languages can get very unwieldy. Because binary operations are infix, you have to parse the tree structure in your head before you can start determining the order of operations."</p>
        <p>"For example, in BQN, you can trim matches from the beginning of a string with "<a style="text-decoration: none;" href="https://mlochbaum.github.io/bqncrate/?q=Remove%20cells%20that%20appear%20in%20x%20from%20beginning%20of%20y#"><code>"x(∧`∘∊˜¬⊸/⊢)y"</code></a>". "</p>
        <p>"In contrast, here is their equivalent in Uiua, implemented the same way:"</p>
        <Editor example="Trim ← ▽¬\\×∊,"/>
        <p>
            "You'll notice that stack basing simplifies the expression in a few ways:"
            <ul>
                <li>"There is no Uiua code corresponding to the BQN combinators "<code>"∘"</code>" and "<code>"⊸"</code>". Function composition is implicit."</li>
                <li>"Functions are executed right-to-left instead of in a tree ordering."</li>
                <li>"The expression does not require "<code>"()"</code>"s. In fact, no Uiua expression requires explicit grouping. "<code>"()"</code>" is used to make inline functions instead."</li>
            </ul>
        </p>
        <p>"I think this clarity makes writing long tacit expressions much more workable."</p>

        <h2 id="array-model">"The Array Model"</h2>
        <p>"Uiua's array model went through a lot of iterations during development. At first, it used a flat, vector-based model ala K and Q. Then, I switched to BQN's Based array model. That was really complicated to implement primitives for, so I tried something else."</p>
        <p>"I switched to a flat array model with \"fill elements\". While arrays could not be nested, operations which would create nested arrays in other languages would instead create jagged arrays with special fill elements at the end of some rows. While this worked, the code was scattered everywhere with checks for fill elements, because they had to propagate through everything. It also had the unfortunate effect of making byte arrays take up 2 bytes of space, since a bit had to be used to indicate whether the byte was a fill element or not. Also, a lot of operations, such as "<Prim prim=Transpose/>", don't really make a lot of sense with jagged arrays."</p>
        <p>"Finally, I switched to the current model, which resembles J's Boxed array model. While you can do something resembling J's "<code>"box <"</code>" using "<Prim prim=Box/>" (and "<code>"open >"</code>" with "<Prim prim=Un/><Prim prim=Box/>"), I designed functions like "<Prim prim=Partition/>" and "<Prim prim=Group/>" to allow selecting uniformly-shaped rows from a non-uniform list in an effort to minimize interaction with jagged data."</p>
        <p>"The fact that the stack is always available also makes putting non-uniform data in arrays less necessary."</p>

        <h2 id="glyphs">"The Glyphs"</h2>
        <p>"Most of Uiua's glyphs were chosen for one of a few reasons:"</p>
        <ul>
            <li>"It is a common mathematical symbol, such as "<Prim prim=Add/>", "<Prim prim=Sub/>", and "<Prim prim=Pi/>"."</li>
            <li>"It is a very commonly used function and should create little line noise, such as "<Prim prim=Dup/>" and "<Prim prim=Flip/>"."</li>
            <li>"It is used in other array languages, such as "<Prim prim=Reduce/>", "<Prim prim=Scan/>", and "<Prim prim=Transpose/>"."</li>
            <li>"It kind of reminds me of what it does. Some of my favorites are "<Prim prim=Table/>", "<Prim prim=Reshape/>", "<Prim prim=Rotate/>", "<Prim prim=Deshape/>", and "<Prim prim=Find/>"."</li>
            <li>"Its function is kind of abstract, but there are other related functions, so they all use related glyphs. For example, "<Prim prim=Fold/>" has this nice symmetry with "<Prim prim=Reduce/>" and "<Prim prim=Scan/>". The indexing/finding/grouping functions like"<Prim prim=Classify/>", "<Prim prim=Group/>", "<Prim prim=Deduplicate/>", etc are all circles."</li>
            <li>"I think they look like cute little guys: "<Prim prim=Assert/>" and "<Prim prim=Try/></li>
        </ul>
        <p>"An additional constraint is that every glyph must be present in the "<a href="https://dejavu-fonts.github.io">"DejaVu Sans Mono"</a>" font, which is the best-looking free monospace font I could find that supports the largest number of glyphs."</p>

        <h2 id="no-local-variables">"No Local Variables"</h2>
        <p>"Forbidding general local variables has a few benefits:"</p>
        <ul>
            <li>"I don't have to implement them (score!)"</li>
            <li>"It forces you to write (often beautiful) tacit code, which I would argue Uiua enables better than almost any other programming language."</li>
            <li>"It frees you from the burden of naming things."</li>
        </ul>

        <h2 id="identifiers-and-formatting">"Identifiers and Formatting"</h2>
        <p>"I made the decision to have a formatter that turns names into Unicode glyphs about as soon as I started using Unicode glyphs. I did not want to require special keyboard or editor support like APL and BQN do."</p>
        <p>"The advantage of a file-watching formatter is that the only feature your editor needs is the ability to automatically reload files if they change on disk. You don't need special keybinds or plugins or anything."</p>
        <p>"The other nice thing about a formatter is that it makes it easier to get started with the language. You do not have to memorize a bunch of keyboard shortcuts to type the glyphs. You just need to learn their names."</p>

        <h2 id="inspiration">"Inspiration"</h2>
        <h3>"BQN"</h3>
        <p>"The main language that inspired Uiua is "<a href="https://mlochbaum.github.io/BQN/">BQN</a>". While I had heard about APL before, BQN was my first real exposure to the power of the array paradigm. I think the language is an astounding feat of engineering. Marshall is both a genius and a great communicator."</p>
        <p>"However, as you can read above, a lot of Uiua's design decisions are responses to things I "<em>"didn't"</em>" like about BQN. There were a bunch of little pain-points that I thought I could improve on."</p>
        <p>"A lot of the behavior of Uiua's built-in functions (and the choice of which built-ins to include) is inspired by BQN's primitives. Just a few examples are "<Prim prim=Transpose/>", "<Prim prim=Classify/>", "<Prim prim=Group/>", and "<Prim prim=Take/>"."</p>
        <p>"Another thing that was largely inspired by BQN is this website! BQN's site is excellent. I really like the way it is organized and the way it presents the language. I particularly liked the built-in editor, so I made my own version for Uiua that has syntax highlighting and history, which I reuse in all the tutorials and examples."</p>
        <br/>
        <h3>"The Array Cast"</h3>
        <p>"During the period of Uiua's development, I spent a lot of time listening to "<a href="https://arraycast.com/">"The Array Cast"</a>", a podcast about array languages. The conversations about the design and implementation of APL, J, K, Q, and BQN are both inspirational and informative. The guys have such a depth and breadth of knowledge on the topic. I really recommend giving it a listen."</p>
        <p>"Thanks to "<a href = "https://github.com/codereport">"Con"</a><a href="https://www.youtube.com/@code_report">"or"</a>", Bob, Stephen, Adám, "<a href="https://github.com/mlochbaum">"Marshall"</a>", Richard, and all the guests."</p>
    }
}

#[component]
pub fn Technical() -> impl IntoView {
    view! {
        <Title text="Technical Details - Uiua Docs"/>
        <h1>"Technical Details"</h1>

        <h2>"The Interpreter"</h2>
        <p>"The Uiua interpreter is written in Rust."</p>
        <p>"Uiua code is compiled into a simple bytecode assembly. This assembly is then usually immediately executed by the interpreter."</p>
        <p>"Built-in functions are implemented in Rust so they can be as fast as possible. User-defined functions are passed around as chunks of bytecode."</p>

        <h2>"Arrays"</h2>
        <p>"Values on the stack are implemented as Rust "<code>"enum"</code>"s, where each variant contains a different array type."</p>
        <p>"While the language itself only has 4 types, the interpreter can have 1 extra: a byte array. IO streams and some operations which have boolean results return byte arrays for space efficiency. This optimization is only enabled if the interpreter is built with the "<code>"bytes"</code>" feature."</p>
        <p>"Array elements are stored in a reference-counted contiguous-memory container I call a "<em>"CowSlice"</em>" or clone-on-write slice. When an array is modified, its data is only copied if it is shared with another array. In addition, pulling out the rows of an array only increments the reference count of the data, and the row arrays have modified shapes and data offsets."</p>
        <p>"Array shapes are stored in a special array type that only allocates when there are more than 3 items."</p>

        <h2>"The Website"</h2>
        <p>"The Uiua website is written using the "<a href="https://leptos.dev/">Leptos</a>" framework and hosted on GitHub pages."</p>
        <p>"Leptos compiles to webassembly, which allows the entire Uiua interpreter to be compiled and used by the site's editor."</p>
        <p>"The online editor is implemented as a "<code>"contenteditable"</code>" div with lots of custom behaviors."</p>
    }
}

#[component]
pub fn StackIdioms() -> impl IntoView {
    view! {
        <Title text="Stack Idioms - Uiua Docs"/>
        <h1>"Common Stack Idioms"</h1>
        <p>"This page contains some common stack idioms that you may find useful."</p>
        <p>"They are presented as rearrangements of characters which are then grouped into an array so that you can see the result."</p>
        <Editor example="[. @A]"/>
        <Editor example="[: @A@B]"/>
        <Editor example="[, @A@B]"/>
        <Editor example="[◌ @A@B]"/> // Should fail
        <Editor example="[,, @A@B]"/>
        <Editor example="[⟜: @A@B]"/>
        <Editor example="[⊙. @A@B]"/>
        <Editor example="[⊙◌ @A@B]"/>
        <Editor example="[⊙: @A@B@C]"/>
        <Editor example="[⊙, @A@B@C]"/>
    }
}

#[component]
pub fn Install() -> impl IntoView {
    view! {
        <Title text="Installation - Uiua Docs"/>
        <h2>"Installing Uiua"</h2>
        <p>"If your OS is supported, then the newest version of the Uiua interpreter can be downloaded from the "<a href="https://github.com/uiua-lang/uiua/releases">"releases"</a>" page."</p>
        <p>"Otherwise, the native Uiua interpreter can be installed via Cargo."</p>
        <p>"This requires a "<a href="https://www.rust-lang.org/tools/install">"Rust"</a>" installation (>=1.75)."</p>
        <p>"Once you have that, run the following command:"</p>
        <code class="code-block">"cargo install uiua"</code>
        <p>"The following optional features are available (enabled by passing "<code>"--features <feature>"</code>"):"</p>
        <ul>
            <li>
                <p><code>"audio"</code>" - Enables audio system functions."</p>
                <p>"On Linux, this may require installing some dependencies:"</p>
                <code class="code-block">"apt install libasound2-dev libudev-dev pkg-config"</code>
            </li>
        </ul>
        <p>"If you want the most recent development version of Uiua, you can install from the git repository."</p>
        <code class="code-block">"cargo install --git https://github.com/uiua-lang/uiua uiua"</code>

        <h2 id="fonts">"Fonts"</h2>
        <p>"Uiua was originally designed to be used with stock "<a href="https://dejavu-fonts.github.io">"DejaVu Sans Mono"</a>". However, it now supports two custom fonts:"</p>
        <ul>
            <li><a href="https://github.com/uiua-lang/uiua/raw/main/site/Uiua386.ttf">"Uiua386"</a>" - inspired by APL386. Thanks to Gifti for making it!"</li>
            <li>"Jonathan Perret's"<a href="https://github.com/jonathanperret/uiua386color">"Uiua386Color"</a>" - a colored version of Uiua386"</li>
            <li><a href="https://github.com/uiua-lang/uiua/raw/main/site/DejaVuSansMono.ttf">"DejaVuSansMono"</a>" - a modified version"</li>
        </ul>

        <h2 id="editor-support">"Editor Support"</h2>
        <p>"An official "<a href="https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode">"Uiua language extension for VSCode"</a>" is available."</p>
        <p>"For Neovim, Apeiros-46B maintains "<a href="https://github.com/Apeiros-46B/nvim/blob/main/after/syntax/uiua.vim">"syntax"</a>" and "<a href="https://github.com/Apeiros-46B/nvim/blob/main/after/ftplugin/uiua.lua">"LSP"</a>" scripts."</p>
        <p>"For Vim, sputnick1124 maintains a "<a href="https://github.com/sputnick1124/uiua.vim">"Uiua plugin"</a>"."</p>
        <p>"For Emacs, crmsnbleyd maintains a "<a href="https://github.com/crmsnbleyd/uiua-ts-mode">"Uiua mode"</a>"."</p>
        <p>"These require Uiua to be installed and in your "<code>"PATH"</code>"."</p>

        <h2>"Basic Usage"</h2>
        <p>"Running just "<code>"uiua"</code>" will display the help message if there are no "<code>".ua"</code>" files in the directory."</p>
        <p>"You can initialize a "<code>"main.ua"</code>" with "<code>"uiua init"</code>"."</p>
        <p>"Once a "<code>".ua"</code>" file exists, running "<code>"uiua"</code>" will begin watching the directory for changes. If you edit and save a "<code>".ua"</code>" file, the interpreter will automatically format and run it."</p>
        <p>"You should configure you editor so that it automatically reloads files if they change on disk. This will allow you to see the formatted file as soon as it is saved."</p>
        <p>"Use "<code>"uiua <PATH>"</code>" or "<code>"uiua run [PATH]"</code>" to format and run a file without watching it."</p>
        <p>"Use "<code>"uiua fmt [PATH]"</code>" to format a file without running it."</p>
        <p>"Use "<code>"uiua test [PATH]"</code>" to run tests."</p>
    }
}

#[component]
pub fn RightToLeft() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Right-to-Left - Uiua Docs"/>
        <h2>"Right-to-Left"</h2>
        <p>"One of the most asked questions about Uiua is \"Why does code execute right-to-left?\" It's a valid question. Every other stack-oriented language I know goes left-to-right."</p>
        <p>"The simple answer is that while Uiua is stack-"<em>"based"</em>", it is not stack-"<em>"oriented"</em>"."</p>
        <p>"The misunderstanding is largely my own fault. The initial version of the website said \"stack-oriented\" everywhere and made references to FORTH. I have since rectified this."</p>
        <p>"When you write Uiua code the stack should just be a tool, a convention. It's how you pass values around. "<strong>"The stack should not guide how you think about solving problems in Uiua."</strong></p>
        <p>"Uiua is about composing arrays. The stack makes it possible to do this without naming local variables. This is the entire reason for its presence in the language. In particular, the stack can be used to construct arbitrary combinators and data flows. It is an extremely powerful mechanism for this purpose."</p>
        <p>"You should not think of Uiua syntax like a FORTH. You should think of it like any of the numerous other languages that put functions before their arguments. This group includes languages of vastly different kinds, like C, Haskell, and Lisp."</p>
        <p>"The left side of an expression is "<em>"not"</em>" the end or the beginning. It is the "<em>"root"</em>". The expression is a tree with branches that converge and diverge in different ways. It is not a list of instructions."</p>
        <p>"This allows us to separate the execution model from the mental model. With a separate mental model, why does it matter which direction the code executes? Why can't the root be on the right?"</p>
        <p>"Of course, "<em>"now"</em>" the decision is arbitrary. I'm used to languages that put the root on the left, so that is what I chose."</p>
        <hr/>
        <p>"Enough with the philosophical. There are also some syntactic reasons that left-to-right execution would be weird."</p>
        <p>"Consider some mathematical expressions:"</p>
        <Editor example="√4\n-3 5"/>
        <p>"The square root looks almost just like it does in mathematical notation. It would not be so if the "<Prim prim=Sqrt glyph_only=true/>" were to the right of the number. Similar problems arise with "<Prim prim=Neg glyph_only=true/>" and "<Prim prim=Not glyph_only=true/>"."</p>
        <p><code>"-3"</code>" has this nice quality where it kind of becomes its own little monadic function that also has a syntactic similarity to mathematical notation. You could do something similar if the language went the other way, with "<code>"5-"</code>", but subtracting is a more common and intuitive operation than subtracting from."</p>
        <p>"Consider the simple "<Prim prim=First/>" function:"</p>
        <Editor example="⊢[1 2 3]"/>
        <p>"The "<Prim prim=First glyph_only=true/>" glyph was chosen because it looks like it indicates the left side of a span (imagine some longer "<code>"⊢–––––⊣"</code>"). If it had to go on the right, there is no glyph that would indicate it quite so nicely. "<code>"⊣"</code>" has a similar aesthetic when put at the end, but that would indicate the last item rather than the first."</p>
    }
}

#[component]
pub fn Audio() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Audio Output - Uiua Docs"/>
        <h1>"Audio Output"</h1>
        <p>"Uiua has a built-in support for audio output."</p>

        <h2 id="basic-synthesis">"Basic Synthesis"</h2>
        <p>"In the online editor, you need only make an array that looks like audio samples."</p>
        <p>"Audio samples must be either a rank 1 where each element is a sample or a rank 2 array where each row is a channel."</p>
        <p>"The samples must be between "<code>"-1"</code>" and "<code>"1"</code>". We use the "<Prim prim=Sys(SysOp::AudioSampleRate)/>" system function to get the sample rate of the audio output."</p>
        <p>"For a minimal example, here is a simple 1 second sawtooth wave:"</p>
        <Editor example="÷2 ◿1×220 ÷⟜⇡&asr"/>
        <p>"First, we make a range of numbers from 0 to 1 by getting the "<Prim prim=Range/>" up to the sample rate and dividing it by that much. This array represents the time at each sample."</p>
        <p>"Then, we multiply the time by 220, the frequency of an A3 note, and take the "<Prim prim=Mod/>"1 of that. This gives us a nice pure sawtooth wave."</p>
        <p>"Finally, the wave is a little loud on its own, so we "<Prim prim=Div/>" it by 2."</p>
        <br/>
        <p>"For longer time arrays, "<Prim prim=Mul/>" the number of samples by the number of seconds you want before calling "<Prim prim=Range/>" but after "<Prim prim=Dup/>"."</p>
        <Editor example="÷2◿1×220÷:⇡ ×3 .&asr"/>
        <p>"If you "<Prim prim=Mul/>" by a non-integer, you may need to use "<Prim prim=Round/>" to prevent an error."</p>
        <Editor example="÷2◿1×220÷:⇡ ⁅×0.5 .&asr"/>

        <h2 id="notes">"Notes"</h2>
        <p>"My favorite way to make multiple notes is to "<Prim prim=Table/>" different frequencies with the time array."</p>
        <p>"Then, if you want a chord, you can use "<Prim prim=Reduce glyph_only=true/><Prim prim=Add glyph_only=true/>" to add them together."</p>
        <p>"If you want sequence instead, you can use "<Prim prim=Reduce glyph_only=true/><Prim prim=Join glyph_only=true/>"."</p>
        <p>"You can calculate freqencies "<code>"f"</code>" that are a certain number of half-steps "<code>"n"</code>" from another with the formula "<code>"f×2^(n/12)"</code>" which can be written in Uiua as"<code><Prim prim=Mul glyph_only=true/>"f"<Prim prim=Pow glyph_only=true/><Prim prim=Flip glyph_only=true/>"2"<Prim prim=Div glyph_only=true/>"12 n"</code>"."</p>
        <p>"In this example, we make both a chord and a sequence from the same notes. We use "<Prim prim=Sin glyph_only=true/><Prim prim=Mul glyph_only=true/><Prim prim=Tau glyph_only=true/>" to make a sine wave instead of a saw wave."</p>
        <Editor example="\
f ← ×220ⁿ:2÷12 [0 4 7]
s ← ○×τ⊞×f ÷⟜⇡&asr
÷⧻f/+s
÷⧻f/⊂s"/>

        <h2 id="native-audio">"Native Audio"</h2>
        <p>"If running code in the native Uiua interpreter, arrays will not be automatically turned into audio."</p>
        <p>"Instead, you must use the "<Prim prim=Sys(SysOp::AudioPlay)/>" system function to play it."</p>
        <p><Prim prim=Sys(SysOp::AudioPlay)/>" should work fine on the website as well, but it is not necessary."</p>
        <Editor example="&ap÷2×¬◿1×4:±○×τ×55.÷:⇡×2. &asr"/>
    }
}

#[component]
pub fn ImagesAndGifs() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Images and GIFs - Uiua Docs"/>
        <h1>"Images and GIFs"</h1>
        <p>"Uiua has built-in support for generating images and GIFs."</p>

        <h2 id="images">"Images"</h2>
        <p>"Creating an image is as simple as creating an array of pixel data."</p>
        <p>"To start, we can create a list of numbers from 0 to 1 by dividing a "<Prim prim=Range/>" by its length."</p>
        <Editor example="÷⟜⇡10"/>
        <p>"If we make the list a bit bigger and "<Prim prim=Table/>" it with itself, we can form a square array of numbers."</p>
        <p>"Let's see what that looks like with different functions used in the "<Prim prim=Table/>"."</p>
        <Editor example="⊞<. ÷⟜⇡100"/>
        <Editor example="⊞×. ÷⟜⇡100"/>
        <Editor example="⊞◿. ÷⟜⇡100"/>
        <p>"We can use a bit of math(s) to make more interesting patterns. Here, we use a "<Prim prim=Dip/>" and "<Prim prim=Identity/>" in the "<Prim prim=Table/>" to turn each list into its own image. See what happens if you remove "<Prim prim=Lt/>"."</p>
        <Editor example="< ⊞⊙∘ -1/2 : ×0.2○×τ . ÷⟜⇡100"/>
        <p>"So far, these images have all been rank-2 arrays of grayscale pixel data, but rank-3 arrays allow for multiple color channels!"</p>
        <p>"The last axis is always the colors. In this example, we create an array with "<Prim prim=Shape/>" "<code>"[100 100 2]"</code>". Because there are only 2 color channels, the image will be interpreted as grayscale with an alpha channel."</p>
        <Editor example="⊞⊟. ÷⟜⇡100"/>
        <p>"With 3 or 4 color channels, you can create full-color images."</p>
        <Editor example="⊞(⊂⊂.). ÷⟜⇡100 # RGB"/>
        <Editor example="⊞(⊂.⊂). ÷⟜⇡100 # RGBA"/>
        <p>"In the examples above, the image array is constructed in such a way that the color channels are already the last axis. To create an image by combining color channels, it may be necessary to use "<Prim prim=Transpose/>"."</p>
        <Editor example="< ⊞⊙∘ -0.4 : ×0.2○×τ . ÷⟜⇡100\n[⍉.⇌.]\n△. # Not a valid image shape\n⍉:\n△. # Valid image shape"/>
        <p>"Of course, images need not be sqaure."</p>
        <Editor example="⊞< :+1/2÷3○×τ: ∩(÷100⇡) 100 300"/>

        <h2 id="gifs">"GIFs"</h2>
        <p>"To create a GIF, simply create an array where every row is an image."</p>
        <p>"Here, we define a function that takes a frame parameter and generates an image, then evaluate it for each value in a range."</p>
        <Editor example="F ← <⊞×. ÷⟜⇡100 ÷2+1○×τ\n∵F÷⟜⇡30"/>

        <h2 id="system-functions">"System Functions"</h2>
        <p>"If you use the native interpreter, arrays will not be automatically converted into images or GIFs like they are on the website. To generate them, you must explicitly call certain system functions."</p>
        <p>"You can find lists of "<A href="/docs/imag">"image"</A>" and "<A href="/docs/gif">"GIF"</A>" system functions on the main docs page."</p>
        <p>"One system function that is particularly useful on the website is "<Prim prim=Sys(SysOp::GifShow)/>", which lets you set the framerate of a GIF."</p>
        <Editor example="÷2+1○×τ÷⟜⇡30\n∵(⍉[⍉..]⊞× ⟜(+○) ÷⟜⇡80)\n&gifs 30"/>
    }
}

#[component]
pub fn Constants() -> impl IntoView {
    use Primitive::*;
    let constants = CONSTANTS
        .iter()
        .filter(|con| !con.doc.trim().is_empty())
        .map(|con| view!(<p><Const con=con/>" - "{ con.doc }</p>))
        .collect::<Vec<_>>();
    view! {
        <Title text="Constants - Uiua Docs"/>
        <h1>"Constants"</h1>
        <p>"These constants are available in every scope. However, unlike formattable constants like "<Prim prim=Pi/>", these constants can be shadowed within a scope."</p>
        <Editor example="e\ne ← 5\ne"/>
        <br/>
        <div>
        { constants }
        </div>
    }
}

#[component]
pub fn Optimizations() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Optimizations - Uiua Docs"/>
        <h1>"Optimizations"</h1>
        <p>"The Uiua interpreter contains a number of optimizations that you can take advantage of to improve the performance of your code."</p>

        <h2>"Pervasive Functions"</h2>
        <p>"All pervasive functions run on arrays in hot loops that should have performance comparable to an implementation in a languages like C or Rust. This includes all mathematical and comparison functions."</p>
        <p>"The interpreter does its best to re-use allocated memory when possible instead of copying. Arrays are reference-counted, so an array's memory is only copied when it is modified "<em>"and"</em>" a duplicate exists somewhere. "<Prim prim=Dup/>" and "<Prim prim=Over/>" do not copy actual array memory. They only copy pointers and increment reference counts."</p>
        <p>"In this example, only the last line results in a copy:"</p>
        <Editor no_run=true example="+1 ⇡10\n×. ⇡10\n×+1⇡10⇡10\n+1.⇡10"/>
        <p>"Using pervasive functions whenever possible, on the largest arrays possible, is the best way to get good performance out of Uiua."</p>

        <h2>"Iterating Modifiers"</h2>
        <p>"The modifiers "<Prim prim=Reduce/>", "<Prim prim=Scan/>", and "<Prim prim=Table/>" have special-case optimizations when used with certain functions. These optimizations eliminate all interpreter overhead while the loops are running, and are therefore very fast."</p>
        <p>"This table shows which combinations are optimized:"</p>
        <table class="bordered-table cell-centered-table">
            <tr>
                <th/>
                <th><Prims prims=[Add, Sub, Mul, Div, Mod, Atan, Min, Max]/></th>
                <th><Prims prims=[Eq, Ne]/></th>
                <th><Prims prims=[Lt, Le, Gt, Ge]/></th>
                <th><Prim prim=Join glyph_only=true/></th>
                <th><Prims prims=[Couple, Complex]/></th>
            </tr>
            <tr><th><Prim prim=Table/></th> <td>"✔"</td> <td>"✔"</td> <td>"✔"</td> <td>"✔"</td> <td>"✔"</td></tr>
            <tr><th><Prim prim=Reduce/></th> <td>"✔"</td> <td></td>  <td></td> <td>"✔"</td> <td></td></tr>
            <tr><th><Prim prim=Scan/></th> <td>"✔"</td>  <td>"✔"</td> <td></td> <td></td> <td></td></tr>
        </table>

        <h2>"Complexity"</h2>
        <p>"Some combinations of functions are special-cased in the interpreter to run in less time complexity or in fewer operations than is implied by each function individually."</p>
        <p>"This table shows how various combinations of functions are optimized:"</p>
        <table class="bordered-table cell-centered-table">
            <tr><th>"Functions"</th><th style="text-align: center">"Naive Implementation"</th><th>"Optimized Implementation"</th></tr>
            <tr><th><Prims prims=[First, Reverse]/></th><td>"O(n)"</td><td>"O(1)"</td></tr>
            <tr><th><Prims prims=[First, Rise]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Reverse, Rise]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Fall]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Reverse, Fall]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Where]/></th><td>"O(n)"</td><td>"O(where the first non-zero is)"</td></tr>
            <tr><th><Prims prims=[Select, Rise, Dup]/></th><td>"Create intermediate "<Prim prim=Rise/>" array"</td><td>"Just sort"</td></tr>
            <tr><th><Prims prims=[Select, Fall, Dup]/></th><td>"Create intermediate "<Prim prim=Fall/>" array"</td><td>"Just sort"</td></tr>
            <tr><th><Prims prims=[Dip, Dip, Dip]/>"…"</th><td><Prim prim=Dip/>" n times"</td><td>"Single "<Prim prim=Dip/>" of n values"</td></tr>
            <tr><th><Prims prims=[Transpose, Transpose, Transpose]/>"…"</th><td><Prim prim=Transpose/>" n times"</td><td>"Single "<Prim prim=Transpose/></td></tr>
            <tr><th><Prims prims=[Rows, Transpose]/></th><td><Prim prim=Transpose/>" each row"</td><td>"Single "<Prim prim=Transpose/></td></tr>
            <tr><th><Prims prims=[Rows, Reduce]/><code>"F"</code><Prims prims=[Windows]/></th><td>"Make "<Prim prim=Windows/>" then "<Prim prim=Reduce/>" each row"</td><td>"Apply "<code>"F"</code>" to adjacent rows"</td></tr>
        </table>
    }
}

#[component]
pub fn Changelog() -> impl IntoView {
    view! {
        <Title text="Changelog - Uiua Docs"/>
        { markdown(include_str!("../../changelog.md")) }
    }
}

#[component]
pub fn Combinators() -> impl IntoView {
    use Primitive::*;
    let combinators = [
        (
            view!(<Prim prim=Identity/>).into_view(),
            ("∘", 1, "I", "Identity"),
        ),
        (
            view!(<Prims prims=[Dip, Pop]/>).into_view(),
            ("⊙◌", 2, "K", "Kestrel"),
        ),
        (
            view!(<Prims prims=[Pop]/>" or "<Prims prims=[Gap, Identity]/>).into_view(),
            ("◌\n⋅∘", 2, "KI", "Kite"),
        ),
        (
            view!(<Prim prim=Dup/>).into_view(),
            ("⊂.", 1, "W", "Warbler"),
        ),
        (
            view!(<Prim prim=Flip/>).into_view(),
            ("⊂:", 2, "C", "Cardinal"),
        ),
        (View::default(), ("⊢⇌", 1, "B", "Bluebird")),
        (View::default(), ("⇌⊂", 2, "B1", "Blackbird")),
        (
            view!(<Prim prim=On/>).into_view(),
            ("⊂⟜¯", 1, "S", "Starling"),
        ),
        (
            view!(<Prim prim=Dup/>).into_view(),
            ("≍⇌.", 1, "Σ", "Violet Starling"),
        ),
        (view!(<Prim prim=Dip/>).into_view(), ("⊟⊙⇌", 2, "D", "Dove")),
        (View::default(), ("⊟⇌", 2, "Δ", "Zebra Dove")),
        (
            view!(<Prim prim=Fork/>).into_view(),
            ("⊟⊃¯⇌", 1, "Φ", "Phoenix"),
        ),
        (view!(<Prim prim=Both/>).into_view(), ("⊂∩□", 2, "Ψ", "Psi")),
        (
            view!(<Prim prim=Bracket/>).into_view(),
            ("⊟⊓¯⇌", 2, "D2", "Dovekie"),
        ),
        (
            view!(<Prim prim=On/>).into_view(),
            ("⊟⟜+", 2, "N", "Eastern Nicator"),
        ),
        (
            view!(<Prim prim=Over/>).into_view(),
            ("⊟+,", 2, "ν", "Western Nicator"),
        ),
        (
            view!(<Prim prim=Dip/>).into_view(),
            ("⊟⊙+", 3, "E", "Eagle"),
        ),
        (View::default(), ("⊟+", 3, "ε", "Golden Eagle")),
        (
            view!(<Prim prim=Fork/>).into_view(),
            ("⊟⊃¯+", 2, "X", "Eastern Kingbird"),
        ),
        (
            view!(<Prim prim=Fork/>).into_view(),
            ("⊟⊃+¯", 2, "χ", "Western Kingbird"),
        ),
        (
            view!(<Prim prim=Bracket/>).into_view(),
            ("⊟⊓¯+", 3, "R", "Eastern Parotia"),
        ),
        (
            view!(<Prim prim=Bracket/>).into_view(),
            ("⊟⊓+¯", 3, "ρ", "Western Parotia"),
        ),
        (
            view!(<Prim prim=Fork/>).into_view(),
            ("⊟⊃+-", 2, "Φ1", "Pheasant"),
        ),
        (
            view!(<Prim prim=Bracket/>).into_view(),
            ("⊟⊓-+", 4, "Ê", "Bald Eagle"),
        ),
    ];
    let combinators = combinators
        .into_iter()
        .map(|(code, (example, inputs, symbol, bird))| {
            let mut ex = String::new();
            for (i, line) in example.lines().enumerate() {
                if i > 0 {
                    ex.push('\n');
                }
                ex.push_str(line);
                if !line.starts_with('#') {
                    for i in 0..inputs {
                        let a = i * 3 + 1;
                        ex.push_str(&format!(" {}_{}_{}", a, a + 1, a + 2));
                    }
                    ex.push_str("  ");
                }
            }
            let diagram = format!("/combinators/{symbol}.svg");
            let note = ["N", "ν", "X", "χ", "R", "ρ"].contains(&symbol).then(|| {
                view! {
                    <sup>" "<span
                        style="text-decoration: underline dotted; font-size: 0.8em; cursor: help;"
                        title="N, ν, X, χ, R, and ρ are not standard named combinators. They are included here because Uiua can express them easily.">
                        "*"
                    </span></sup>
                }
            });
            let symbol = if let Some(sym) = symbol.strip_suffix(|c: char| c.is_ascii_digit()) {
                let sub = symbol.chars().rev().take_while(char::is_ascii_digit).collect::<String>();
                view!({ sym }<sub>{ sub }</sub>).into_view()
            } else {
                symbol.into_view()
            };
            view! {
                <tr>
                    <td>{ symbol }{ note }</td>
                    <td>{ bird }</td>
                    <td>{ code }</td>
                    <td><Editor example={&ex} nonprogressive=true/></td>
                    <td><img src={diagram} alt={bird} class="combinator-diagram"/></td>
                </tr>
            }
        })
        .collect::<Vec<_>>();
    view! {
        <Title text="Combinators - Uiua Docs"/>
        <h1>"Combinators"</h1>
        <p>"This page contains a list of implementations of common combinators in Uiua. While it's not really necessary to know these to write Uiua programs, you may find the information interesting."</p>
        <p>"A combinator is a function that only refers to its arguments. "<a href="https://en.wikipedia.org/wiki/Combinatory_logic">"Combinatory logic"</a>" is the branch of logic that deals with combinators."</p>
        <p>"Ever since Raymond Smullyan's book "<a href="https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird">"To Mock a Mockingbird"</a>", people have been calling combinators by bird names. These bird names are included in the table."</p>
        <h2 id="reading">"Reading the Table"</h2>
        <p>"Each entry in the table contains a diagram of the combinator. The letters "<code>"F"</code>", "<code>"G"</code>", and "<code>"H"</code>" represent the first, second, and third functions involved in the combinator. The letters "<code>"a"</code>", "<code>"b"</code>", "<code>"c"</code>", and "<code>"d"</code>" represent the arguments."</p>
        <p>"For the purpose of the examples, "<code>"a"</code>" is always the array "<code>"1_2_3"</code>", "<code>"b"</code>" is always the array "<code>"4_5_6"</code>", etc."</p>
        <p>"The left-most function in the example stands in for "<code>"F"</code>", the \"top-most\" function in the combinator."</p>
        <br/>
        <hr/>
        <br/>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th title="Symbol">"Sym."</th>
                <th>"Bird"</th>
                <th>"Code"</th>
                <th>"Example"</th>
                <th>"Diagram"</th>
            </tr>
            { combinators }
        </table>
        <p>"This page is inspired by the "<a href="https://mlochbaum.github.io/BQN/doc/birds.html">"similar page"</a>" on the BQN website. The diagrams are also inspired by "<a href="https://mlochbaum.github.io/BQN/doc/tacit.html#combinators">"BQN's combinator diagrams"</a>"."</p>
        <p>"I referenced "<a href="https://combinatorylogic.com/table.html">"these"</a>" "<a href="https://www.angelfire.com/tx4/cus/combinator/birds.html">"lists"</a>" of combinators when making this page."</p>
    }
}
