use std::collections::BTreeMap;

use leptos::*;
use leptos_meta::*;
use uiua::{ConstClass, Primitive, CONSTANTS};
use uiua_editor::Editor;

use crate::{
    markdown::{markdown_view, Markdown},
    primitive::doc_line_fragments_to_view,
    Const, Hd, Prim, Prims,
};

#[component]
pub fn Technical() -> impl IntoView {
    view! {
        <Title text="Technical Details - Uiua Docs"/>
        <h1>"Technical Details"</h1>

        <Hd id="the-interpreter">"The Interpreter"</Hd>
        <p>"The Uiua interpreter is written in Rust."</p>
        <p>"Uiua code is compiled into a simple bytecode assembly. This assembly is then usually immediately executed by the interpreter."</p>
        <p>"Built-in functions are implemented in Rust so they can be as fast as possible. User-defined functions are passed around as chunks of bytecode."</p>

        <Hd id="arrays">"Arrays"</Hd>
        <p>"Values on the stack are implemented as Rust "<code>"enum"</code>"s, where each variant contains a different array type."</p>
        <p>"While the language itself only has 4 types, the interpreter can have 1 extra: a byte array. IO streams and some operations which have boolean results return byte arrays for space efficiency."</p>
        <p>"Array elements are stored in a reference-counted contiguous-memory container I call a "<em>"CowSlice"</em>" or clone-on-write slice. When an array is modified, its data is only copied if it is shared with another array. In addition, pulling out the rows of an array only increments the reference count of the data, and the row arrays have modified shapes and data offsets."</p>
        <p>"Array shapes are stored in a special array type that only allocates when there are more than 3 items."</p>

        <Hd id="website">"The Website"</Hd>
        <p>"The Uiua website is written using the "<a href="https://leptos.dev/">Leptos</a>" framework and hosted on GitHub pages."</p>
        <p>"Leptos compiles to webassembly, which allows the entire Uiua interpreter to be compiled and used by the site's editor."</p>
        <p>"The online editor is implemented as a "<code>"contenteditable"</code>" div with lots of custom behaviors."</p>
    }
}

#[component]
pub fn Install() -> impl IntoView {
    view! {
        <Title text="Installation - Uiua Docs"/>
        <Hd id="installing-uiua">"Installing Uiua"</Hd>
        <p><strong>"If your OS is supported, then the newest version of the Uiua interpreter can be downloaded from the "<a href="https://github.com/uiua-lang/uiua/releases">"releases"</a>" page."</strong></p>
        <p>"Otherwise, the native Uiua interpreter can be installed via Cargo."</p>
        <p>"This requires a "<a href="https://www.rust-lang.org/tools/install">"Rust"</a>" installation (>=1.78)."</p>
        <p>"Once you have that, run one of the following commands:"</p>
        <code class="code-block">"cargo install uiua -F full"</code>
        <code class="code-block">"cargo install uiua"</code>
        <p>"On Linux, this may require installing some dependencies:"</p>
        <code class="code-block">"apt install libx11-dev"</code>
        <p>"The "<code>"-F full"</code>" flag enables all optional features. If you need more control over which features are installed, enable only the only the ones you want by passing "<code>"--features <feature>"</code>"):"</p>
        <ul>
            <li>
                <p><code>"audio"</code>" - Enables audio system functions."</p>
                <p>"On Linux, this may require installing some dependencies:"</p>
                <code class="code-block">"apt install libasound2-dev libudev-dev pkg-config"</code>
            </li>
            <li>
                <p><code>"webcam"</code>" - Enables webcam system functions."</p>
                <p>"On Linux, this may require installing some dependencies:"</p>
                <code class="code-block">"apt install libjpeg-dev"</code>
            </li>
        </ul>
        <p>"If you want the most recent development version of Uiua, you can install from the git repository."</p>
        <code class="code-block">"cargo install --git https://github.com/uiua-lang/uiua uiua"</code>

        <Hd id="fonts">"Fonts"</Hd>
        <p>"Uiua supports a few custom fonts, but "<a href="https://github.com/uiua-lang/uiua/blob/main/src/algorithm/Uiua386.ttf">"Uiua386"</a>" is the primary one."</p>
        <ul>
            <li><a href="https://github.com/uiua-lang/uiua/blob/main/src/algorithm/Uiua386.ttf">"Uiua386"</a>" - inspired by APL386. Thanks to Gifti for making it!"</li>
            <li>"Jonathan Perret's "<a href="https://github.com/jonathanperret/uiua386color">"Uiua386Color"</a>" - a colored version of Uiua386"</li>
            <li><a href="https://github.com/uiua-lang/uiua/raw/main/site/DejaVuSansMono.ttf">"DejaVuSansMono"</a>" - a modified version"</li>
        </ul>
        <p>"Uiua was originally designed to be used with stock "<a href="https://dejavu-fonts.github.io">"DejaVu Sans Mono"</a>", but further development and glyph choices target Uiua386."</p>

        <Hd id="editor-support">"Editor Support"</Hd>
        <p>"An official "<a href="https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode">"Uiua language extension for VSCode"</a>" is available."</p>
        <p>"For Vim/Neovim, Apeiros-46B maintains a "<a href="https://github.com/Apeiros-46B/uiua.vim">"Uiua syntax highlighting plugin"</a>"."</p>
        <p>"For Vim, sputnick1124 maintains a "<a href="https://github.com/sputnick1124/uiua.vim">"Uiua plugin"</a>"."</p>
        <p>"Language support in Neovim is easy with "<a href="https://github.com/neovim/nvim-lspconfig">"nvim-lspconfig"</a>"."</p>
        <p>"For Emacs, crmsnbleyd maintains a "<a href="https://github.com/crmsnbleyd/uiua-ts-mode">"Uiua mode"</a>"."</p>
        <p>"These require Uiua to be installed and in your "<code>"PATH"</code>"."</p>

        <Hd id="basic-usage">"Basic Usage"</Hd>
        <p>"Running just "<code>"uiua"</code>" will display the help message if there are no "<code>".ua"</code>" files in the directory."</p>
        <p>"You can initialize a "<code>"main.ua"</code>" with "<code>"uiua init"</code>"."</p>
        <p>"Once a "<code>".ua"</code>" file exists, running "<code>"uiua"</code>" will begin watching the directory for changes. If you edit and save a "<code>".ua"</code>" file, the interpreter will automatically format and run it."</p>
        <p>"You should configure you editor so that it automatically reloads files if they change on disk. This will allow you to see the formatted file as soon as it is saved."</p>
        <p>"Use "<code>"uiua <PATH>"</code>" or "<code>"uiua run [PATH]"</code>" to format and run a file without watching it."</p>
        <p>"Use "<code>"uiua fmt [PATH]"</code>" to format a file without running it."</p>
        <p>"Use "<code>"uiua test [PATH]"</code>" to run tests."</p>
        <p>"Use "<code>"uiua module update"</code>" to update Git modules."</p>

        <Hd id="local-site">"Running the Site Locally"</Hd>
        <p>"This website is a static, single-page application. As such, it can be build and run locally, without connecting to a server."</p>
        <p>"To do this, you will need "<a href="https://www.rust-lang.org/tools/install">"Rust"</a>" installed. You will also need the "<a href="https://github.com/uiua-lang/uiua">"Uiua repository"</a>" cloned locally."</p>
        <p>"You need to have both "<a href="https://trunkrs.dev">"Trunk"</a>" and the "<code>"wasm32-unknown-unknown"</code>" target installed. To get both of these, run:"</p>
        <code class="code-block">"cargo install trunk\nrustup target add wasm32-unknown-unknown"</code>
        <p>"Then, from the root of the Uiua repository, run:"</p>
        <code class="code-block">"cd site\ntrunk serve"</code>
        <p>"The site will be available at "<code>"http://localhost:8080"</code>"."</p>
    }
}

#[component]
pub fn RightToLeft() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Right-to-Left - Uiua Docs"/>
        <Hd id="right-to-left">"Right-to-Left"</Hd>
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
pub fn Constants() -> impl IntoView {
    use ConstClass::*;
    use Primitive::*;
    let mut by_class: BTreeMap<ConstClass, Vec<View>> = BTreeMap::new();
    for con in CONSTANTS.iter().filter(|con| !con.doc.trim().is_empty()) {
        let view = view!(<tr>
            <td><Const con=con/></td>
            <td><div class="const-desc">{
                doc_line_fragments_to_view(&con.doc_frags())
            }</div></td>
        </tr>)
        .into_view();
        by_class.entry(con.class).or_default().push(view);
    }
    let class_cols = [
        vec![Math, Time, Color, Flags],
        vec![External, Media, System, Fun],
    ];
    let cols = class_cols.map(|col_classes| {
        let mut tables = Vec::new();
        for class in col_classes {
            let mut rows = Vec::new();
            for con in CONSTANTS.iter().filter(|con| con.class == class) {
                let view = view!(<tr>
                    <td><Const con=con/></td>
                    <td><div class="const-desc">{
                        doc_line_fragments_to_view(&con.doc_frags())
                    }</div></td>
                </tr>)
                .into_view();
                rows.push(view);
            }
            tables.push(view!(
                <h3>{format!("{class:?}")}</h3>
                <table class="bordered-table" style="width: 100%">{ rows }</table>
            ));
        }
        view!(<div>{tables}</div>)
    });
    view! {
        <Title text="Constants - Uiua Docs"/>
        <h1>"Constants"</h1>
        <p>"These constants are available in every scope. However, unlike formattable constants like "<Prim prim=Pi/>", these constants can be shadowed within a scope."</p>
        <Editor example="e\ne ← 5\ne"/>
        <br/>
        <div>
        <div class="features">{ cols }</div>
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

        <Hd id="pervasive-functions">"Pervasive Functions"</Hd>
        <p>"All pervasive functions run on arrays in hot loops that should have performance comparable to an implementation in a languages like C or Rust. This includes all mathematical and comparison functions."</p>
        <p>"The interpreter does its best to re-use allocated memory when possible instead of copying. Arrays are reference-counted, so an array's memory is only copied when it is modified "<em>"and"</em>" a duplicate exists somewhere. "<Prim prim=Dup/>" and "<Prim prim=Over/>" do not copy actual array memory. They only copy pointers and increment reference counts."</p>
        <p>"In this example, only the last line results in a copy:"</p>
        <Editor no_run=true example="+1 ⇡10\n×. ⇡10\n×+1⇡10⇡10\n+1.⇡10"/>
        <p>"Using pervasive functions whenever possible, on the largest arrays possible, is the best way to get good performance out of Uiua."</p>

        <Hd id="iterating-modifiers">"Iterating Modifiers"</Hd>
        <p>"The modifiers "<Prim prim=Reduce/>", "<Prim prim=Scan/>", and "<Prim prim=Table/>" have special-case optimizations when used with certain functions. These optimizations eliminate all interpreter overhead while the loops are running, and are therefore very fast."</p>
        <p>"This table shows which combinations are optimized:"</p>
        <table class="bordered-table cell-centered-table">
            <tr>
                <th/>
                <th><Prims prims=[Add, Sub, Mul, Div, Modulus, Atan, Min, Max]/></th>
                <th><Prims prims=[Eq, Ne]/></th>
                <th><Prims prims=[Lt, Le, Gt, Ge]/></th>
                <th><Prim prim=Join glyph_only=true/></th>
                <th><Prims prims=[Couple, Complex]/></th>
            </tr>
            <tr><th><Prim prim=Table/></th> <td>"✔"</td> <td>"✔"</td> <td>"✔"</td> <td>"✔"</td> <td>"✔"</td></tr>
            <tr><th><Prim prim=Reduce/></th> <td>"✔"</td> <td></td>  <td></td> <td>"✔"</td> <td></td></tr>
            <tr><th><Prim prim=Scan/></th> <td>"✔"</td>  <td>"✔"</td> <td></td> <td></td> <td></td></tr>
        </table>

        <p>"The pattern "<Prims prims=[Reduce]/><code>"F"</code><Prims prims=[Table]/><code>"G"</code>" is optimized to use much less memory and run much faster than the naive implementation. This only occurs when both functions have signature "<code>"|2.1"</code>". Rather than creating the entire table and then reducing it, each reduced row is generated as it is needed."</p>
        <p>"On top of this, particular combinations of "<code>"F"</code>" and "<code>"G"</code>" are optimized to eliminate all interpreter overhead. All combinations of the following functions are optimized:"</p>
        <table class="bordered-table cell-centered-table">
            <tr><th><Prim prim=Reduce/></th><td><Prims prims=[Add, Mul, Min, Max]/></td></tr>
            <tr><th><Prim prim=Table/></th><td><Prims prims=[Add, Sub, Mul, Div, Modulus, Atan, Eq, Ne, Lt, Le, Gt, Ge, Min, Max, Complex, Join, Couple]/></td></tr>
        </table>

        <Hd id="rows"><Prim prim=Rows/></Hd>
        <p>"When used inside "<Prim prim=Rows/>", some functions have special-case implementations that operate on the whole array at once. This avoids the interpreter overhead inherent to "<Prim prim=Rows/>"."</p>
        <p>"In addition to all pervasive functions, the following functions are optimized when used inside "<Prim prim=Rows/>":"</p>
        <div style="display: flex; gap: 1em">
            <table class="bordered-table cell-centered-table">
                <tr><td><Prim prim=Deshape/></td></tr>
                <tr><td><Prim prim=Reverse/></td></tr>
                <tr><td><Prim prim=Transpose/></td></tr>
                <tr><td><Prim prim=Classify/></td></tr>
                <tr><td><Prim prim=Fix/></td></tr>
                <tr><td><Prim prim=Box/></td></tr>
                <tr><td><Prim prim=First/></td></tr>
                <tr><td><Prims prims=[First, Reverse] show_names=true/></td></tr>
            </table>
            <table class="bordered-table cell-centered-table">
                <tr><td><Prims prims=[Gap, Rand] show_names=true/></td></tr>
                <tr><td><Prims prims=[On, Rand] show_names=true/></td></tr>
                <tr><td><Prims prims=[By, Rand] show_names=true/></td></tr>
                <tr><td><Prim prim=Gap/><code>"constant"</code></td></tr>
                <tr><td><Prim prim=On/><code>"constant"</code></td></tr>
                <tr><td><Prim prim=By/><code>"constant"</code></td></tr>
                <tr><td><Prims prims=[Select, Rise, Dup]/>" / "<Prims prims=[Select, By, Rise]/></td></tr>
                <tr><td><Prims prims=[Select, Fall, Dup]/>" / "<Prims prims=[Select, By, Fall]/></td></tr>
                <tr><td><Prims prims=[Un, Couple] show_names=true/></td></tr>
                <tr><td><Prims prims=[Un, Join] show_names=true/></td></tr>
                <tr><td><Prim prim=Rotate/></td></tr>
                <tr><td><Prim prim=Reduce/></td></tr>
            </table>
        </div>
        <p>"This optimization applies not just to "<Prim prim=Rows/>", but also "<Prim prim=Each/>", "<Prims prims=[Rows, Rows]/>", "<Prims prims=[Rows, Rows, Rows]/>", etc."</p>

        <Hd id="complexity">"Complexity"</Hd>
        <p>"Some combinations of functions are special-cased in the interpreter to run in less time complexity or in fewer operations than is implied by each function individually."</p>
        <p>"This table shows how various combinations of functions are optimized:"</p>
        <table class="bordered-table cell-centered-table">
            <tr><th>"Functions"</th><th style="text-align: center">"Naive Implementation"</th><th>"Optimized Implementation"</th></tr>
            <tr><th><Prims prims=[First, Reverse]/></th><td>"O(n)"</td><td>"O(1)"</td></tr>
            <tr><th><Prims prims=[First, Rise]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Reverse, Rise]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Fall]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Reverse, Fall]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Where]/></th><td>"O(n)"</td><td>"Stop at first non-zero from front"</td></tr>
            <tr><th><Prims prims=[First, Reverse, Where]/></th><td>"O(n)"</td><td>"Stop at first non-zero from back"</td></tr>
            <tr><th><Prims prims=[Select, Rise, Dup]/>" / "<Prims prims=[Select, By, Rise]/></th><td>"Create intermediate "<Prim prim=Rise/>" array"</td><td>"Just sort"</td></tr>
            <tr><th><Prims prims=[Select, Fall, Dup]/>" / "<Prims prims=[Select, By, Fall]/></th><td>"Create intermediate "<Prim prim=Fall/>" array"</td><td>"Just sort"</td></tr>
            <tr><th><Prims prims=[Dip, Dip, Dip]/>"…"</th><td><Prim prim=Dip/>" n times"</td><td>"Single "<Prim prim=Dip/>" of n values"</td></tr>
            <tr><th><Prims prims=[Transpose, Transpose, Transpose]/>"…"</th><td><Prim prim=Transpose/>" n times"</td><td>"Single "<Prim prim=Transpose/></td></tr>
            <tr><th><Prims prims=[Rows, Reduce]/><code>"F"</code><Prims prims=[Windows]/></th><td>"Make "<Prim prim=Windows/>" then "<Prim prim=Reduce/>" each row"</td><td>"Apply "<code>"F"</code>" to adjacent rows"</td></tr>
            <tr><th><Prims prims=[Rows, Box, Windows]/></th><td>"Make "<Prim prim=Windows/>" then "<Prim prim=Box/>" each row"</td><td><Prim prim=Box/>" each window"</td></tr>
            <tr><th><Prims prims=[Len, Where]/></th><td>"Make "<Prim prim=Where/>" then get "<Prim prim=Len/></td><td>"Just count"</td></tr>
            <tr><th><Prims prims=[MemberOf, Range]/></th><td>"Make "<Prim prim=Range/>" then check "<Prim prim=MemberOf/></td><td>"Just check bounds"</td></tr>
            <tr><th><Prims prims=[MemberOf, Rerank]/><code>"1"</code><Prims prims=[Range]/></th><td>"Make "<Prim prim=Range/>" then "<Prim prim=Rerank/>" then check "<Prim prim=MemberOf/></td><td>"Just check bounds"</td></tr>
            <tr><th><Prim prim=First glyph_only=true/>"/"<Prim prim=Last glyph_only=true/> <Prims prims=[Un, Sort]/></th><td><Prims prims=[Un, Sort] show_names=true/>" then take the "<Prim prim=First/>"/"<Prim prim=Last/></td><td>"Just pick a random row"</td></tr>
        </table>

        <Hd id="other-optimizations">"Other Optimizations"</Hd>
        <ul>
            <li><Prim prim=Rows/>", "<Prim prim=Each/>", "<Prim prim=Table/>", "<Prim prim=Group/>", "<Prim prim=Partition/>", and "<Prim prim=Inventory/>" are all optimized when a "<Prim prim=Fork/>" or "<Prim prim=Bracket/>" is at the top level of their function. For example, "<Prims prims=[Table, Fork]/><code>"F"</code><code>"G"</code>" is optimized to "<Prims prims=[Fork, Table]/><code>"F"</code><Prims prims=[Table]/><code>"G"</code>"."</li>
            <li><Prim prim=Group/>" and "<Prim prim=Partition/>" are optimized to be fast with "<Prim prim=Len/>", "<Prim prim=First/>", "<Prims prims=[First, Reverse]/>", "<Prims prims=[Dip, Pop]/>", and "<Prims prims=[Gap, Identity]/>"."</li>
            <li><Prim prim=Repeat/>" with a simple-enough function and a small constant count will compile to an unrolled loop."</li>
        </ul>
    }
}

#[component]
pub fn Changelog() -> impl IntoView {
    view! {
        <Title text="Changelog - Uiua Docs"/>
        { markdown_view(include_str!("../../changelog.md")) }
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
            view!(<Prim prim=By/>).into_view(),
            ("⊟⊸+", 2, "ν", "Western Nicator"),
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
            let symbol_view = if let Some(sym) = symbol.strip_suffix(|c: char| c.is_ascii_digit()) {
                let sub = symbol.chars().rev().take_while(char::is_ascii_digit).collect::<String>();
                view!({ sym }<sub>{ sub }</sub>).into_view()
            } else {
                symbol.into_view()
            };
            let onclick = {
                let diagram = diagram.clone();
                move |_| {
                    window().open_with_url_and_target(&diagram, "_blank").unwrap();
                }
            };
            view! {
                <tr>
                    <td>{ symbol_view }{ note }</td>
                    <td>{ bird }</td>
                    <td>{ code }</td>
                    <td><Editor example={&ex} nonprogressive=true/></td>
                    <td><div style="position: relative">
                        <object
                            data={diagram}
                            type="image/svg+xml"
                            aria-label={bird}
                            class="combinator-diagram"/>
                        <button
                            class="editor-right-button"
                            style="position: absolute; top: 0; right: 0;"
                            data-title="Open SVG"
                            on:click=onclick>
                            "🔗"
                        </button>
                    </div></td>
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
        <Hd id="reading">"Reading the Table"</Hd>
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
                <th style="width: 100%">"Example"</th>
                <th>"Diagram"</th>
            </tr>
            { combinators }
        </table>
        <p>"This page is inspired by the "<a href="https://mlochbaum.github.io/BQN/doc/birds.html">"similar page"</a>" on the BQN website. The diagrams are also inspired by "<a href="https://mlochbaum.github.io/BQN/doc/tacit.html#combinators">"BQN's combinator diagrams"</a>"."</p>
        <p>"I referenced "<a href="https://combinatorylogic.com/table.html">"these"</a>" "<a href="https://www.angelfire.com/tx4/cus/combinator/birds.html">"lists"</a>" of combinators when making this page."</p>
    }
}

#[component]
pub fn Experimental() -> impl IntoView {
    use Primitive::*;

    fn subscript<'a>(prim: Primitive, meaning: &'a str, example: &'a str) -> impl IntoView + 'a {
        view!(<tr>
            <td><Prim prim=prim/></td>
            <td>{ meaning.to_string() }</td>
            <td><Editor example=example nonprogressive=true/></td>
        </tr>)
    }

    view! {
        <Title text="Experimental Features - Uiua Docs"/>
        <h1>"Experimental Features"</h1>
        <p>"Uiua has a number of features that are considered experimental. They are available in the interpreter for testing, but may be removed or changed in the future."</p>
        <p>"Using experimental features requires an "<code>"# Experimental!"</code>" comment to be placed at the top of a Uiua source file."</p>

        <Hd id="functions-modifiers">"Experimental Functions and Modifiers"</Hd>
        <ul>{
            Primitive::non_deprecated().filter(Primitive::is_experimental).map(|prim| {
                view! { <li><Prim prim=prim/></li> }
            }).collect::<Vec<_>>()
        }</ul>

        <Hd id="subscript-modifiers">"Subscript Modifiers"</Hd>
        <p>"By suffixing some functions or modifiers with a subscript number, their behavior can be modified."</p>
        <p>"Subscript numbers are typed with a "<code>"__"</code>" followed by some digits. The formatter will turn them into subscript digit characters."</p>
        <p>"The following functions and modifiers are supported:"</p>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th>"Primitive"</th>
                <th>"Meaning"</th>
                <th style="width: 100%">"Example"</th>
            </tr>
            <tr>
                <td>"Any "<span class="dyadic-function">"dyadic"</span>" pervasive function"</td>
                <td>"Constant first argument"</td>
                <td><Editor example="# Experimental!\n⊃+₁×₂ [1 2 3]" nonprogressive=true/></td>
            </tr>
            <tr>
                <td><Prims prims=[Select, Pick, Take, Drop, Join, Rerank, Rotate, Orient, Windows]/></td>
                <td>"Constant first argument"</td>
                <td><Editor example="# Experimental!\n⊃↙₂↻₃ [1 2 3 4 5]" nonprogressive=true/></td>
            </tr>
            { vec![
                subscript(Couple, "Group N arrays as rows", "# Experimental!\n{⊟₃ 1 2 3 4 5}"),
                subscript(Box, "Group N arrays as boxed rows", "# Experimental!\n□₃ 1_2_3 5 \"wow\""),
                subscript(Transpose, "Repeat", "# Experimental!\n△ ⍉₃ °△1_2_3_4_5"),
                subscript(Sqrt, "Nth root", "# Experimental!\n√₃ [8 27 125]"),
                subscript(Round, "To N decimal places", "# Experimental!\n⁅₃ π"),
                subscript(Floor, "To N decimal places", "# Experimental!\n⌊₃ π"),
                subscript(Ceil, "To N decimal places", "# Experimental!\n⌈₃ π"),
                subscript(Both, "Apply to N argument sets", "# Experimental!\n[∩₃+ 1 2 3 4 5 6]"),
                subscript(Repeat, "Repetition count", "# Experimental!\n⍥₅(⊂⟜/+) [1 2]"),
                subscript(Tuples, "Tuple size", "# Experimental!\n⧅₂< ⇡4"),
                subscript(Stack, "Print top N values", "# Experimental!\n?₂ 1 2 3 4")
            ] }
        </table>

        <Markdown src="/text/experimental.md"/>
    }
}
