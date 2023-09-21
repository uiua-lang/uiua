use leptos::*;
use uiua::{primitive::Primitive, SysOp};

use crate::{editor::Editor, PrimCode};

#[component]
pub fn Design() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1 id="design">"Design"</h1>
        <p>"This page explains the reasons for some of Uiua's design decisions."</p>
        <p>"It serves as a "<a href="https://news.knowledia.com/US/en/articles/more-software-projects-need-defenses-of-design-85ea9e23ffd85f5fde5a2d3d42001393cbce169a">"defense of design"</a>"."</p>

        <h2 id="stack-orientation">"Stack Orientation"</h2>
        <h3>"Combinators"</h3>
        <p>"When I first started developing Uiua, it was neither stack-oriented nor array-oriented. What it "<em>"did"</em>" focus a lot on was "<em>"combinators"</em>". I had this whole heirarchy of language-level operators that let you construct arbitrarily complex combinators relatively succinctly."</p>
        <p>"I discovered what a lot of others have discovered when delving deep into tacit code: it's really hard to read and write and reason about."</p>
        <p>"Eventually, I moved to a stack-oriented model and discovered that you can write almost any 1 or 2 argument combinator with just "<PrimCode prim=Dup/>", "<PrimCode prim=Over/>", and "<PrimCode prim=Flip/>"."</p>
        <p>"Of course, I also made the discovery that juggling 3 or more values on the stack also imposes a high cognitive load on the developer, but I added "<PrimCode prim=Roll/>" and "<PrimCode prim=Unroll/>" anyway, because they are super useful. "<PrimCode prim=Fork/>" is based on the fork structure from other array languages, and it inspired my own invention, "<PrimCode prim=Trident/>"."</p>
        <br/>
        <h3>"Expressions"</h3>
        <p>"Long tacit expressions in most array languages can get very unwieldy. Because binary operations are infix, you have to parse the tree structure in your head before you can start determining the order of operations."</p>
        <p>"For example, in BQN, you can trim matches from the beginning of a string with "<a style="text-decoration: none;" href="https://mlochbaum.github.io/bqncrate/?q=Remove%20cells%20that%20appear%20in%20x%20from%20beginning%20of%20y#"><code>"x(∧`∘∊˜¬⊸/⊢)y"</code></a>". "</p>
        <p>"In contrast, here is there equivalent in Uiua, implemented the same way:"</p>
        <Editor example="Trim ← ‡¬\\×∊,"/>
        <p>
            "You'll notice that stack orientation simplifies the expression in a few ways:"
            <ul>
                <li>"There is no Uiua code corresponding to the BQN combinators "<code>"∘"</code>" and "<code>"⊸"</code>". Function composition is implicit."</li>
                <li>"Functions are executed right-to-left instead of in a tree ordering."</li>
                <li>"The expression does not require "<code>"()"</code>"s. In fact, no Uiua expression requires explicit grouping. "<code>"()"</code>" is used to make inline functions instead."</li>
            </ul>
        </p>
        <p>"I think this clarity makes writing long tacit expression much more workable."</p>

        <h2 id="array-model">"The Array Model"</h2>
        <p>"Uiua's array model went through a lot of iterations during development. At first, it used a flat, vector-based model ala K and Q. Then, I switched to BQN's Based array model. That was really complicated to implement primitives for, so I tried something else."</p>
        <p>"I switched to a flat array model with \"fill elements\". While arrays could not be nested, operations which would create nested arrays in other languages would instead create jagged arrays with special fill elements at the end of some rows. While this worked, the code was scattered everywhere with checks for fill elements, because they had to propogate through everything. It also had the unfortunate effect of making byte arrays take up 2 bytes of space, since a bit had to be used to indicate whether the byte was a fill element or not. Also, a lot of operations, such as "<PrimCode prim=Transpose/>", don't really make a lot of sense with jagged arrays."</p>
        <p>"Finally, I switched to the current model, which resembles J's Boxed array model. While you can do something resembling J's "<code>"box <"</code>" using "<PrimCode prim=Constant/>" (and "<code>"open >"</code>" with "<PrimCode prim=Call/>"), I designed functions like "<PrimCode prim=Partition/>" and "<PrimCode prim=Group/>" to allow selecting uniformly-shaped rows from a non-uniform list in an effort to minimize interaction with jagged data."</p>
        <p>"The fact that the stack is always available also makes putting non-uniform data in arrays less necessary."</p>

        <h2 id="array-model">"The Glyphs"</h2>
        <p>"Most of Uiua's glyphs were chosen for one of a few reasons:"</p>
        <ul>
            <li>"It is a common mathematical symbol, such as "<PrimCode prim=Add/>", "<PrimCode prim=Sub/>", and "<PrimCode prim=Pi/>"."</li>
            <li>"It is a very commonly used function and should create little line noise, such as "<PrimCode prim=Dup/>" and "<PrimCode prim=Flip/>"."</li>
            <li>"It is used in other array languages, such as "<PrimCode prim=Reduce/>", "<PrimCode prim=Scan/>", and "<PrimCode prim=Transpose/>"."</li>
            <li>"It kind of reminds me of what it does. Some of my favorites are "<PrimCode prim=Table/>", "<PrimCode prim=Reshape/>", "<PrimCode prim=Rotate/>", "<PrimCode prim=Deshape/>", "<PrimCode prim=Find/>", and "<PrimCode prim=Recur/>"."</li>
            <li>"Its function is kind of abstract, but there are other related functions, so they all use related glyphs. For example, "<PrimCode prim=Fold/>" has this nice symmetry with "<PrimCode prim=Reduce/>" and "<PrimCode prim=Scan/>". The indexing/finding/grouping functions like"<PrimCode prim=Classify/>", "<PrimCode prim=Group/>", "<PrimCode prim=Deduplicate/>", etc are all circles."</li>
            <li>"I think they look like cute little guys: "<PrimCode prim=Assert/>" and "<PrimCode prim=Try/></li>
        </ul>

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
        <p>"However, as you can read above, a lot of Uiua's design decisions are responses to things I "<em>"didn't"</em>" like about BQN. There were a bunch of little pain-points that I though I could improve on."</p>
        <p>"A lot of the behavior of Uiua's built-in functions (and the choice of which built-ins to include) is inspired by BQN's primitives. Just a few examples are "<PrimCode prim=Transpose/>", "<PrimCode prim=Classify/>", "<PrimCode prim=Group/>", and "<PrimCode prim=Take/>"."</p>
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
        <h1>"Technical Details"</h1>

        <h2>"The Interpreter"</h2>
        <p>"The Uiua interpreter is written in Rust."</p>
        <p>"An entire file is first parsed into an abstract syntax tree. Then, each line is, one after the other, compiled to a simple bytecode and executed."</p>
        <p>"Built-in functions are implemented in Rust so they can be as fast as possible. User defined functions are passed around as chunks of bytecode."</p>

        <h2>"Arrays"</h2>
        <p>"Values on the stack are implemented as Rust "<code>"enum"</code>"s, where each variant contains a different array type."</p>
        <p>"While the language itself only has 3 types, the interpreter has 1 extra: a byte array. IO streams and operations which have boolean results return byte arrays for space efficiency."</p>
        <p>"Array elements are stored in a reference-counted contiguous-memory container I call a "<em>"CowSlice"</em>" or clone-on-write slice. When an array is modified, its data is only copied if it is shared with another array. In addition, pulling out the rows of an array only increments the reference count of the data, and the row arrays have modified shapes and data offsets."</p>
        <p>"Array shapes are stored in a special array type that only allocates when there are more than 3 items."</p>

        <h2>"The Website"</h2>
        <p>"The Uiua website is written using the "<a href="https://leptos.dev/">Leptos</a>" framework and hosted on GitHub pages."</p>
        <p>"Leptos compiles to webassembly, which allows the entire Uiua interpreter to be compiled and used by the site's editor."</p>
        <p>"The online editor is implemented as a "<code>"contenteditable"</code>" div with lots of custom behaviors."</p>
    }
}

#[component]
pub fn Install() -> impl IntoView {
    view! {
        <h2>"Installing Uiua"</h2>
        <p>"Currently, the native Uiua interpreter must be built from source."</p>
        <p>"You will need a "<a href="https://www.rust-lang.org/tools/install">"Rust"</a>" installation, as well as Git."</p>
        <p>"Once you have those, run the following commands:"</p>
        <code class="code-block">
r#"git clone https://github.com/uiua-lang/uiua
cd uiua
cargo install --path ."#
        </code>
        <p>"On Linux, you may need to install some dependencies first:"</p>
        <code class="code-block">"apt install libasound2-dev libudev-dev pkg-config"</code>

        <h2>"Basic Usage"</h2>
        <p>"Running just "<code>"uiua"</code>" will display the help message if there are no "<code>".ua"</code>" files in the directory."</p>
        <p>"You can initialize a "<code>"main.ua"</code>" with "<code>"uiua init"</code>"."</p>
        <p>"Once a "<code>".ua"</code>" file exists, running "<code>"uiua"</code>" will begin watching the directory for changes. If you edit and save a "<code>".ua"</code>" file, the interpreter will automatically format and run it."</p>
        <p>"You should configure you editor so that it automatically reloads files if they change on disk. This will allow you to see the formatted file as soon as it is saved."</p>
        <p>"Use "<code>"uiua run"</code>" to format and run a file without watching it."</p>
        <p>"Use "<code>"uiua fmt"</code>" to format a file without running it."</p>
        <p>"Use "<code>"uiua test"</code>" to run tests."</p>

        <h2>"Font"</h2>
        <p>"The Uiua glyphs were chosen to be compatible specifically with "<a href="https://dejavu-fonts.github.io/Download.html">"DejaVu Sans Mono"</a>". It is recommended that you install this font to get the best experience with Uiua."</p>

        <h2>"Editor Support"</h2>
        <p>"A "<a href="https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode">"Uiua language extension for VSCode"</a>" is available."</p>
        <p>"It requires Uiua to be installed and in your "<code>"PATH"</code>"."</p>
    }
}

#[component]
pub fn Audio() -> impl IntoView {
    use Primitive::*;
    view! {
        <h1>"Audio Output"</h1>
        <p>"Uiua has a built-in support for audio output."</p>

        <h2 id="basic-synthesis">"Basic Synthesis"</h2>
        <p>"In the online editor, you need only make an array that looks like audio samples."</p>
        <p>"Audio samples must be eiter a "<PrimCode prim=Rank/><code>"1"</code>" array with at least 1000 elements or a "<PrimCode prim=Rank/><code>"2"</code>" array with a row for each audio channel and at least 1000 elements in each row."</p>
        <p>"The samples must be between "<code>"-1"</code>" and "<code>"1"</code>". They will be played at a sample rate of 44100 Hz."</p>
        <p>"For a minimal example, here is a simple 1 second sawtooth wave:"</p>
        <Editor examples={&["%2", "◿1×220", "÷∶⇡.44100"]}/>
        <p>"First, we make a range of 44100 numbers from 0 to 1 by getting the "<PrimCode prim=Range/>" up to 44100 and dividing it by that much. This array represents the time at each sample."</p>
        <p>"Then, we multiply the time by 220, the frequency of an A3 note, and take the "<PrimCode prim=Mod/>"1 of that. This gives us a nice pure sawtooth wave."</p>
        <p>"Finally, the wave is a little loud on its own, so we "<PrimCode prim=Div/>" it by 2."</p>
        <br/>
        <p>"For longer time arrays, "<PrimCode prim=Mul/>" the number of sample by the number of seconds you want before calling "<PrimCode prim=Range/>" but after "<PrimCode prim=Dup/>"."</p>
        <Editor example="%2◿1×220÷∶⇡ ×3 .44100"/>
        <p>"If you "<PrimCode prim=Mul/>" by a non-integer, you may need to use "<PrimCode prim=Round/>" to prevent an error."</p>
        <Editor example="%2◿1×220÷∶⇡ ⁅×0.5 .44100"/>

        <h2 id="notes">"Notes"</h2>
        <p>"My favorite way to make multiple notes is to "<PrimCode prim=Table/>" different frequencies with the time array."</p>
        <p>"Then, if you want a chord, you can use "<PrimCode prim=Reduce glyph_only=true/><PrimCode prim=Add glyph_only=true/>" to add them together."</p>
        <p>"If you want sequence instead, you can use "<PrimCode prim=Reduce glyph_only=true/><PrimCode prim=Join glyph_only=true/>"."</p>
        <p>"You can calculate freqencies "<code>"f"</code>" that are a certain number of half-steps "<code>"n"</code>" from another with the formula "<code>"f×2^(n/12)"</code>" which can be written in Uiua as"<code><PrimCode prim=Mul glyph_only=true/>"f"<PrimCode prim=Pow glyph_only=true/><PrimCode prim=Flip glyph_only=true/>"2"<PrimCode prim=Div glyph_only=true/>"12 n"</code>"."</p>
        <p>"In this example, we make both a chord and a sequence from the same notes. We use "<PrimCode prim=Sin glyph_only=true/><PrimCode prim=Mul glyph_only=true/><PrimCode prim=Tau glyph_only=true/>" to make a sine wave instead of a saw wave."</p>
        <Editor example="\
f ← ×220ⁿ∶2÷12 [0 4 7]
÷∶⇡.44100
s ← ○×τ⊞×f
÷⧻f/+s
÷⧻f/⊂s"/>

        <h2 id="native-audio">"Native Audio"</h2>
        <p>"If running code in the native Uiua interpreter, arrays will not be automatically turned into audio."</p>
        <p>"Instead, you must use the "<PrimCode prim=Sys(SysOp::AudioPlay)/>" system function to play it."</p>
        <p><PrimCode prim=Sys(SysOp::AudioPlay)/>" should fine on the website as well, but it is not necessary."</p>
        <Editor example="&ap÷2×¬◿1×4∶±○×τ×55.÷∶⇡×2. 44100"/>
    }
}
