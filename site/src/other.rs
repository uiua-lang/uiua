use leptos::*;
use leptos_meta::*;
use uiua::{ConstClass, Primitive, SysOp, CONSTANTS};
use uiua_editor::{lang, Editor};

use crate::{
    markdown::{markdown_view, Markdown},
    primitive::doc_line_fragments_to_view,
    Const, Hd, Prim, Prims,
};

#[component]
pub fn Technical() -> impl IntoView {
    view! {
        <Title text=format!("Technical Details - {} Docs", lang())/>
        <h1>"Technical Details"</h1>

        <Hd id="the-interpreter">"The Interpreter"</Hd>
        <p>"The "{lang}" interpreter is written in Rust."</p>
        <p>{lang}" code is compiled into an IR tree assembly. This assembly is then usually immediately executed by the interpreter."</p>
        <p>"Built-in functions are implemented in Rust so they can be as fast as possible. User-defined functions are passed around as IR trees."</p>

        <Hd id="arrays">"Arrays"</Hd>
        <p>"Values are implemented as Rust "<code>"enum"</code>"s, where each variant contains a different array type."</p>
        <p>"While the language itself only has 4 types, the interpreter can have 1 extra: a byte array. IO streams and some operations which have boolean results return byte arrays for space efficiency."</p>
        <p>"Array elements are stored in a reference-counted contiguous-memory container I call a "<em>"CowSlice"</em>" or clone-on-write slice. When an array is modified, its data is only copied if it is shared with another array. In addition, pulling out the rows of an array only increments the reference count of the data, and the row arrays have modified shapes and data offsets."</p>
        <p>"Array shapes are stored in a special array type that only allocates when there are more than 3 items."</p>

        <Hd id="website">"The Website"</Hd>
        <p>"The "{lang}" website is written using the "<a href="https://leptos.dev/">Leptos</a>" framework and hosted on GitHub pages."</p>
        <p>"Leptos compiles to webassembly, which allows the entire "{lang}" interpreter to be compiled and used by the site's editor."</p>
        <p>"The online editor is implemented as a "<code>"contenteditable"</code>" div with lots of custom behaviors."</p>
    }
}

#[component]
pub fn Install() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text=format!("Installation - {} Docs", lang())/>
        <Hd id="installing-uiua">"Installing "{lang}""</Hd>
        <p><strong>"If your OS is supported, then the newest version of the "{lang}" interpreter can be downloaded from the "<a href="https://github.com/uiua-lang/uiua/releases">"releases"</a>" page."</strong></p>
        <p>"Otherwise, the native "{lang}" interpreter can be installed via Cargo."</p>
        <p>"This requires a "<a href="https://www.rust-lang.org/tools/install">"Rust"</a>" installation (>=1.83)."</p>
        <p>"Once you have that, run one of the following commands:"</p>
        <code class="code-block">"cargo install uiua -F full"</code>
        <code class="code-block">"cargo install uiua"</code>
        <p>"On Linux, this may require installing some dependencies:"</p>
        <code class="code-block">"apt install libx11-dev libffi-dev"</code>
        <p>"The "<code>"-F full"</code>" flag enables all optional features. If you need more control over which features are installed, enable only the only the ones you want by passing "<code>"--features <feature>"</code>"):"</p>
        <ul>
            <li><code>"full"</code>" - Enables all optional features below."</li>
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
            <li>
                <p><code>"window"</code>" - Enables output in a window."</p>
            </li>
        </ul>
        <p>"If you want the most recent development version of "{lang}", you can install from the git repository."</p>
        <code class="code-block">"cargo install --git https://github.com/uiua-lang/uiua uiua"</code>

        <Hd id="fonts">"Fonts"</Hd>
        <p><a href="https://github.com/uiua-lang/uiua/blob/main/src/assets/Uiua386.ttf">{lang}"386"</a>" is the only font that is updated alongside the language itself. It is a heavily modified version of BQN386. Thanks to Gifti for doing the original redesign!"</p>
        <p>"Other fonts will work, but they may look odd in some cases, especially if they do not have every glyph."</p>

        <Hd id="editor-support">"Editor Support"</Hd>
        <p>"An official "<a href="https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode">{lang}" language extension for VSCode"</a>" is available."</p>
        <p>"For Vim/Neovim, Apeiros-46B maintains a "<a href="https://github.com/Apeiros-46B/uiua.vim">{lang}" syntax highlighting plugin"</a>"."</p>
        <p>"For Vim, sputnick1124 maintains a "<a href="https://github.com/sputnick1124/uiua.vim">{lang}" plugin"</a>"."</p>
        <p>"Language support in Neovim is easy with "<a href="https://github.com/neovim/nvim-lspconfig">"nvim-lspconfig"</a>"."</p>
        <p>"For Emacs, crmsnbleyd maintains a "<a href="https://github.com/crmsnbleyd/uiua-ts-mode">{lang}" mode"</a>"."</p>
        <p>"For Kakoune, ThaCuber maintains a "<a href="https://github.com/thacuber2a03/highlighters.kak/blob/main/uiua.kak">"syntax highlighting module"</a>"."</p>
        <p>"For Nano, cqn-brwpna9 manatins a "<a href="https://github.com/cqn-brwpna9/uiua-nano-config">"nanorc for uiua"</a>"."</p>
        <p>"These require "{lang}" to be installed and in your "<code>"PATH"</code>"."</p>

        <Hd id="basic-usage">"Basic Usage"</Hd>
        <p>"Running just "<code>"uiua"</code>" will display the help message if there are no "<code>".ua"</code>" files in the directory."</p>
        <p>"You can initialize a "<code>"main.ua"</code>" with "<code>"uiua init"</code>"."</p>
        <p>"Once a "<code>".ua"</code>" file exists, running "<code>"uiua"</code>" will begin watching the directory for changes. If you edit and save a "<code>".ua"</code>" file, the interpreter will automatically format and run it."</p>
        <p>"You should configure you editor so that it automatically reloads files if they change on disk. This will allow you to see the formatted file as soon as it is saved."</p>
        <p>"Use "<code>"uiua <PATH>"</code>" or "<code>"uiua run [PATH]"</code>" to format and run a file without watching it."</p>
        <p>"Use "<code>"uiua fmt [PATH]"</code>" to format a file without running it."</p>
        <p>"Use "<code>"uiua test [PATH]"</code>" to run tests."</p>
        <p>"Use "<code>"uiua module update"</code>" to update Git modules."</p>

        <Hd id="the-output-window">"The Output Window"</Hd>
        <p>"If you download the native interpreter from the "<a href="https://github.com/uiua-lang/uiua/releases">"releases page"</a>", or if you build it from source with either the "<code>"window"</code>" or "<code>"full"</code>" features, you will have the option to show the output of a program (and certain system functions) in a window."</p>
        <p>"The window shows not only basic arrays, but images, gifs, and audio as well."</p>
        <p>"The window will be shown if you run any of the following commands:"</p>
        <code class="code-block">"\
uiua -w
uiua run -w <PATH>
uiua watch -w"
        </code>
        <p>"It can also be enabled by default if you set the "<code>"UIUA_WINDOW"</code>" environment variable to "<code>"1"</code>"."</p>
        <p>"The remaining arguments after a program finishes will be shown in the window. In addition, the "<Prim prim=Sys(SysOp::Show)/>", "<Prim prim=Sys(SysOp::ImShow)/>", "<Prim prim=Sys(SysOp::GifShow)/>", and "<Prim prim=Sys(SysOp::AudioPlay)/>" functions will all show their output in the window."</p>

        <Hd id="local-site">"Running the Site Locally"</Hd>
        <p>"This website is a static, single-page application. As such, it can be build and run locally, without connecting to a server."</p>
        <p>"To do this, you will need "<a href="https://www.rust-lang.org/tools/install">"Rust"</a>" installed. You will also need the "<a href="https://github.com/uiua-lang/uiua">{lang}" repository"</a>" cloned locally."</p>
        <p>"You need to have both "<a href="https://trunkrs.dev">"Trunk"</a>" and the "<code>"wasm32-unknown-unknown"</code>" target installed. To get both of these, run:"</p>
        <code class="code-block">"cargo install trunk\nrustup target add wasm32-unknown-unknown"</code>
        <p>"Then, from the root of the "{lang}" repository, run:"</p>
        <code class="code-block">"cd site\ntrunk serve"</code>
        <p>"The site will be available at "<code>"http://localhost:8080"</code>"."</p>
    }
}

#[component]
pub fn Constants() -> impl IntoView {
    use ConstClass::*;
    use Primitive::*;
    let class_cols = [
        vec![Math, Time, Color, Flags],
        vec![External, Media, Spatial, System, Fun],
    ];
    let cols = class_cols.map(|col_classes| {
        let mut tables = Vec::new();
        for class in col_classes {
            let mut rows = Vec::new();
            for con in CONSTANTS.iter().filter(|con| con.class == class) {
                let style = if con.is_deprecated() {
                    "text-decoration: line-through;"
                } else {
                    ""
                };
                let view = view!(<tr>
                    <td><div style=style><Const con=con/></div></td>
                    <td><div class="const-desc">{
                        doc_line_fragments_to_view(&con.doc_frags())
                    }</div></td>
                </tr>)
                .into_view();
                rows.push(view);
            }
            tables.push(view!(
                <h3>{format!("{class:?}")}</h3>
                <table id={format!("{class:?}")} class="bordered-table" style="width: 100%">{ rows }</table>
            ));
        }
        view!(<div>{tables}</div>)
    });
    view! {
        <Title text=format!("Constants - {} Docs", lang())/>
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
        <Title text=format!("Optimizations - {} Docs", lang())/>
        <h1>"Optimizations"</h1>
        <p>"The "{lang}" interpreter contains a number of optimizations that you can take advantage of to improve the performance of your code."</p>

        <Hd id="pervasive-functions">"Pervasive Functions"</Hd>
        <p>"All pervasive functions run on arrays in hot loops that should have performance comparable to an implementation in a languages like C or Rust. This includes all mathematical and comparison functions."</p>
        <p>"The interpreter does its best to re-use allocated memory when possible instead of copying. Arrays are reference-counted, so an array's memory is only copied when it is modified "<em>"and"</em>" a duplicate exists somewhere. Duplicating an array with "<Prim prim=On/>"/"<Prim prim=By/>"/etc does not copy actual array memory. It only copies pointers and increments reference counts."</p>
        <p>"In this example, only the last line results in a copy:"</p>
        <Editor no_run=true example="+1 ⇡10\n˙× ⇡10\n×+1⇡10⇡10\n+1.⇡10"/>
        <p>"Using pervasive functions whenever possible, on the largest arrays possible, is the best way to get good performance out of "{lang}"."</p>

        <Hd id="iterating-modifiers">"Iterating Modifiers"</Hd>
        <p>"The modifiers "<Prim prim=Reduce/>", "<Prim prim=Scan/>", and "<Prim prim=Table/>" have special-case optimizations when used with certain functions. These optimizations eliminate all interpreter overhead while the loops are running, and are therefore very fast."</p>
        <p>"This table shows which combinations are optimized:"</p>
        <table class="bordered-table cell-centered-table">
            <tr>
                <th/>
                <th><Prims prims=[Add, Sub, Mul, Div, Modulo, Atan, Min, Max]/></th>
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
            <tr><th><Prim prim=Table/></th><td><Prims prims=[Add, Sub, Mul, Div, Modulo, Atan, Eq, Ne, Lt, Le, Gt, Ge, Min, Max, Complex, Join, Couple]/></td></tr>
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
                <tr><td><Prim prim=Last/></td></tr>
                <tr><td><Prims prims=[Sort]/>" / "<Prims prims=[Select, By, Rise]/></td></tr>
                <tr><td><Prims prims=[Reverse, Sort]/>" / "<Prims prims=[Select, By, Fall]/></td></tr>
            </table>
            <table class="bordered-table cell-centered-table">
                <tr><td><Prims prims=[Gap, Rand] show_names=true/></td></tr>
                <tr><td><Prims prims=[On, Rand] show_names=true/></td></tr>
                <tr><td><Prims prims=[By, Rand] show_names=true/></td></tr>
                <tr><td><Prim prim=Gap/><code>"constant"</code></td></tr>
                <tr><td><Prim prim=On/><code>"constant"</code></td></tr>
                <tr><td><Prim prim=By/><code>"constant"</code></td></tr>
                <tr><td><Prims prims=[Un, Couple] show_names=true/></td></tr>
                <tr><td><Prims prims=[Un, Join] show_names=true/></td></tr>
                <tr><td><Prim prim=Rotate/></td></tr>
                <tr><td><Prim prim=Reduce/></td></tr>
            </table>
        </div>
        <p>"This optimization applies not just to "<Prim prim=Rows/>", but also "<Prim prim=Stencil/>", "<Prims prims=[Rows, Rows]/>", "<Prims prims=[Rows, Rows, Rows]/>", etc."</p>

        <Hd id="complexity">"Complexity"</Hd>
        <p>"Some combinations of functions are special-cased in the interpreter to run in less time complexity or in fewer operations than is implied by each function individually."</p>
        <p>"This table shows how various combinations of functions are optimized:"</p>
        <table class="bordered-table cell-centered-table">
            <tr><th>"Functions"</th><th style="text-align: center">"Naive Implementation"</th><th>"Optimized Implementation"</th></tr>
            <tr><th><Prims prims=[First, Rise]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[Last, Rise]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Fall]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[Last, Fall]/></th><td>"O(nlogn)"</td><td>"O(n)"</td></tr>
            <tr><th><Prims prims=[First, Where]/></th><td>"O(n)"</td><td>"Stop at first non-zero from front"</td></tr>
            <tr><th><Prims prims=[Last, Where]/></th><td>"O(n)"</td><td>"Stop at first non-zero from back"</td></tr>
            <tr><th><Prims prims=[Select, By, Rise]/></th><td>"Create intermediate "<Prim prim=Rise/>" array"</td><td>"Just sort"</td></tr>
            <tr><th><Prims prims=[Select, By, Fall]/></th><td>"Create intermediate "<Prim prim=Fall/>" array"</td><td>"Just sort"</td></tr>
            <tr><th><Prims prims=[Reverse, Sort]/></th><td>"Sort then reverse"</td><td>"Sort backwards"</td></tr>
            <tr><th><Prims prims=[Dip, Dip, Dip]/>"…"</th><td><Prim prim=Dip/>" n times"</td><td>"Single "<Prim prim=Dip/>" of n values"</td></tr>
            <tr><th><Prims prims=[Transpose, Transpose, Transpose]/>"…"</th><td><Prim prim=Transpose/>" n times"</td><td>"Single "<Prim prim=Transpose/></td></tr>
            <tr><th><Prims prims=[Len, Where]/></th><td>"Make "<Prim prim=Where/>" then get "<Prim prim=Len/></td><td>"Just count"</td></tr>
            <tr><th><Prims prims=[MemberOf, Range]/></th><td>"Make "<Prim prim=Range/>" then check "<Prim prim=MemberOf/></td><td>"Just check bounds"</td></tr>
            <tr><th><Prims prims=[MemberOf, Deshape]/><code>"₂"</code><Prims prims=[Range]/></th><td>"Make "<Prim prim=Range/>" then "<Prim prim=Deshape/>" then check "<Prim prim=MemberOf/></td><td>"Just check bounds"</td></tr>
            <tr><th><Prim prim=First glyph_only=true/>"/"<Prim prim=Last glyph_only=true/> <Prims prims=[Un, Sort]/></th><td><Prims prims=[Un, Sort] show_names=true/>" then take the "<Prim prim=First/>"/"<Prim prim=Last/></td><td>"Just pick a random row"</td></tr>
            <tr><th><Prims prims=[Match, By, Rotate]/><code>"1"</code></th><td>"Make a copy, "<Prim prim=Rotate/>", and "<Prim prim=Match/></td><td>"Just check that every row is the same"</td></tr>
            <tr><th><Prims prims=[Reduce, Mul, Stencil, Match]/></th><td><Prim prim=Stencil/>" then "<Prim prim=Reduce/></td><td>"Just check that every row is the same"</td></tr>
            <tr><th><Prims prims=[Abs, Complex]/></th><td>"Make intermediate "<Prim prim=Complex/>" array then get "<Prim prim=Abs/></td><td>"Directly compute the magnitude"</td></tr>
        </table>

        <Hd id="sortedness-flags">"Sortedness Flags"</Hd>
        <p>"The interpreter can sometimes mark arrays as being sorted up and/or down. These flags allow certain short-circuiting or optimized behavior for some algorithms."</p>
        <p>"Sortedness flags are set when:"</p>
        <ul>
            <li>"An inline array with all-constant values (i.e "<code>"1_2_3"</code>" or "<code>"[7 6 6 4]"</code>") is already sorted at compile time."</li>
            <li>"The "<Prim prim=Sort/>" function is used."</li>
            <li>"The "<Prim prim=Where/>" function is used."</li>
            <li>"A range is generated with "<Prim prim=Range/>" or "<Prim prim=Un/><Prim prim=Shape/>"."</li>
            <li>"An array is passed to "<Prims prims=[Scan, Min]/>" or "<Prims prims=[Scan, Max]/>"."</li>
            <li>"Two arrays are "<Prim prim=Couple/>"d."</li>
            <li><Prim prim=Reshape/>" is called with at least one scalar argument."</li>
        </ul>
        <p>"The default behavior of most primitives is to clear sortedness flags. However, sortedness flags are maintained in these cases:"</p>
        <ul>
            <li>"A sorted array is "<Prim prim=Neg/>"d."</li>
            <li>"A sorted array is "<Prim prim=Sign/>"ed."</li>
            <li>"A sorted array is "<Prim prim=Floor/>"ed, "<Prim prim=Ceil/>"ed, or "<Prim prim=Round/>"ed."</li>
            <li>"Two sorted arrays are "<Prim prim=Add/>"ed."</li>
            <li>"A sorted array is passed to "<Prim prim=Add/>" or "<Prim prim=Sub/>" along with a scalar."</li>
            <li>"A sorted array is "<Prim prim=Mul/>"d or "<Prim prim=Div/>"d with a scalar. The sort flags will be swapped if the scalar is negative."</li>
            <li>"A sorted array is passed to "<Prim prim=Min/>" or "<Prim prim=Max/>" along with a scalar."</li>
            <li>"A sorted array is "<Prim prim=Reverse/>"d. The sort flags will be swapped."</li>
            <li>"A sorted array is "<Prim prim=Deduplicate/>"d or "<Prim prim=Classify/>"d."</li>
            <li>"A sorted array is "<Prim prim=Un/><Prim prim=Keep/>"d."</li>
            <li>"Two sorted arrays are passed to "<Prim prim=Select/>"."</li>
            <li>"A sorted array is passed as the second argument to "<Prim prim=Keep/>", "<Prim prim=Take/>", or "<Prim prim=Drop/>" (without a "<Prim prim=Fill/>" value)."</li>
            <li>"Two sorted arrays are "<Prim prim=Join/>"ed appropriately."</li>
        </ul>
        <p>"Sortedness flags are used to improve the performance of:"</p>
        <ul>
            <li><Prim prim=Sort/></li>
            <li><Prim prim=Rise/>" and "<Prim prim=Fall/></li>
            <li><Prim prim=First/><Prim prim=Rise/>" and "<Prim prim=Last/><Prim prim=Fall/></li>
            <li><Prim prim=Deduplicate/>" and "<Prim prim=Classify/></li>
            <li><Prims prims=[Reduce, Min]/>" and "<Prims prims=[Reduce, Max]/>" on rank-1 arrays"</li>
            <li><Prims prims=[Scan, Min]/>" and "<Prims prims=[Scan, Max]/>" on rank-1 arrays"</li>
            <li><Prim prim=MemberOf/>" and "<Prim prim=IndexIn/>" when searching for a single row"</li>
            <li>"The "<a href="#complexity">"above"</a>" optimizations for checking if all rows of an array are the same"</li>
            <li>"More may be added in the future"</li>
        </ul>

        <Hd id="other-optimizations">"Other Optimizations"</Hd>
        <ul>
            <li><Prim prim=Group/>" and "<Prim prim=Partition/>" are optimized to be fast with "<Prim prim=Len/>", "<Prim prim=First/>", "<Prim prim=Last/>"."</li>
            <li>
                <Prim prim=Tuples/>" with the following functions:"
                <ul>
                    <li><Prim prim=Lt/></li>
                    <li><Prim prim=Le/></li>
                    <li><Prim prim=Gt/></li>
                    <li><Prim prim=Ge/></li>
                    <li><Prim prim=Ne/></li>
                    <li><Prim prim=Eq/></li>
                    <li><Prim prim=Match/></li>
                    <li><Prims prims=[Gap, Gap]/><code>"constant"</code></li>
                    <li><Prims prims=[Gap, Len]/></li>
                </ul>
            </li>
            <li>
                "The following splitting patterns are optimized for monadic function "<code>"F"</code>":"
                <ul>
                    <li><Prims prims=[Partition]/><code>"F"</code><Prims prims=[By, Ne]/></li>
                    <li><Prims prims=[Partition]/><code>"F"</code><Prims prims=[Not, By, Mask]/></li>
                </ul>
            </li>
        </ul>
    }
}

#[component]
pub fn Changelog() -> impl IntoView {
    view! {
        <Title text=format!("Changelog - {} Docs", lang())/>
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
            view!(<Prim prim=Slf/>).into_view(),
            ("˙⊂", 1, "W", "Warbler"),
        ),
        (
            view!(<Prim prim=Backward/>).into_view(),
            ("˜⊂", 2, "C", "Cardinal"),
        ),
        (View::default(), ("□⊣", 1, "B", "Bluebird")),
        (View::default(), ("⇌⊂", 2, "B1", "Blackbird")),
        (
            view!(<Prim prim=On/>).into_view(),
            ("⊂⟜¯", 1, "S", "Starling"),
        ),
        (
            view!(<Prim prim=By/>).into_view(),
            ("≍⊸⇌", 1, "Σ", "Violet Starling"),
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
                        class="help-note"
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
        <Title text=format!("Combinators - {} Docs", lang())/>
        <h1>"Combinators"</h1>
        <p>"This page contains a list of implementations of common combinators in "{lang}". While it's not really necessary to know these to write "{lang}" programs, you may find the information interesting."</p>
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
pub fn Subscripts() -> impl IntoView {
    use Primitive::*;

    fn subscript<'a>(prim: Primitive, meaning: &'a str, example: &'a str) -> impl IntoView + 'a {
        view!(<tr>
            <td><Prim prim=prim/></td>
            <td>{ meaning.to_string() }</td>
            <td><Editor example=example nonprogressive=true/></td>
        </tr>)
    }

    let numeric = vec![
        subscript(Couple, "Group N arrays as rows", "{⊟₃ 1 2 3 4 5}"),
        subscript(Join, "Join N arrays", "⊂₃ 1 2_3 4"),
        subscript(Box, "Group N arrays as boxed rows", "□₃ 1_2_3 5 \"wow\""),
        subscript(Deshape, "Change rank", "♭₂ ⇡ 2_2"),
        subscript(
            Transpose,
            "Only first N axes",
            "# Experimental!\n△ ⍉₃ °△1_2_3_4_5",
        ),
        subscript(Sqrt, "Nth root", "√₃ [8 27 125]"),
        subscript(Neg, "Turn one Nth in the complex plane", "⁅₃ ⍥₄⊸¯₄ 1"),
        subscript(Exp, "Base N exponential", "ₑ₂ 8"),
        subscript(Round, "To N decimal places", "⁅₃ π"),
        subscript(Floor, "To N decimal places", "# Experimental!\n⌊₄ π\n⌊₄ τ"),
        subscript(Ceil, "To N decimal places", "# Experimental!\n⌈₄ π\n⌈₄ τ"),
        subscript(First, "First N values", "⊢₂ \"hello\""),
        subscript(Last, "Last N values", "⊣₂ \"hello\""),
        subscript(Bits, "Force N bits", "⋯₄ [1 2 3]"),
        subscript(Parse, "Parse in base N", "⋕₁₆ \"beef\""),
        subscript(Keep, "Keep along N axes", "▽₂ 1.5 [1_2_3_4 5_6_7_8]"),
        subscript(Rand, "Random integer", "⚂₁₀₀"),
        subscript(Len, "Length of the Nth axis", "⧻₁ °△2_3_4_5"),
        subscript(Shape, "Shape of the first N axes", "△₂ °△2_3_4_5"),
        subscript(Range, "Start offset", "⇡₁ 5"),
        subscript(
            Occurrences,
            "Within first N occurrences",
            "⧆₁ \"lego helmet\"",
        ),
        subscript(Classify, "Rank-N subrows", "⊛₀ [\"hello\"\"world\"]"),
        subscript(On, "First N values", "{⟜₂[⊙⊙∘] 1 2 3}"),
        subscript(By, "Last N values", "{⊸₂[⊙⊙∘] 1 2 3}"),
        subscript(With, "Last N values", "{⤙₂[⊙⊙∘] 1 2 3}"),
        subscript(Off, "First N values", "{⤚₂[⊙⊙∘] 1 2 3}"),
        subscript(Both, "Apply to N argument sets", "[∩₃+ 1 2 3 4 5 6]"),
        subscript(Reach, "Include first N values", "∪₂⊟₃ 1 2 3 4"),
        subscript(Rows, "Apply to rank-N subarrays", "≡₁□ °△2_3_4"),
        subscript(Inventory, "Apply to rank-N subarrays", "⍚₁⇡ °△2_3"),
        subscript(Repeat, "Repetition count", "⍥₅(⊂⟜/+) [1 2]"),
        subscript(Tuples, "Tuple size", "⧅₂< ⇡4"),
        subscript(Stencil, "Window size", "⧈₃∘ ⇡6"),
        subscript(Args, "Print top N values", "?₂ 1 2 3 4"),
    ];

    let sided = vec![
        subscript(
            Join,
            "Use the left or right argument as the list",
            "⊂⌟ 1 2_3 ⊂⌟ 1_2 3 ⊂⌟ 1_2 3_4\n⊂⌞ 1 2_3 ⊂⌞ 1_2 3 ⊂⌞ 1_2 3_4"
        ),
        subscript(
            Both,
            "Use left-most or right-most argument twice",
            "[∩⌞⊟ @a@b@c]\n[∩⌟⊟ @a@b@c]",
        ),
        subscript(
            Bracket,
            "Use left-most or right-most argument twice",
            "{⊓⌞⊟□₂ @a@b@c}\n{⊓⌟⊟□₂ @a@b@c}",
        ),
        subscript(
            Rows,
            "Fix left-most or right-most argument",
            "≡⌞⊂ 1_2_3 4_5_6\n≡⌟⊂ 1_2_3 4_5_6",
        ),
        subscript(
            Inventory,
            "Fix left-most or right-most argument",
            "⍚⌞⊂ 1_2_3 4_5_6\n⍚⌟⊂ 1_2_3 4_5_6",
        ),
        subscript(
            Slf,
            "Duplicate only the first or last argument",
            "# Experimental!\n˙⌞⊟₃ 1 2\n˙⌟⊟₃ 1 2",
        ),
        subscript(
            Backward,
            "Flip the first or last pair of arguments",
            "# Experimental!\n˜⌞⊟₃ 1 2 3\n˜⌟⊟₃ 1 2 3",
        ),
        subscript(
            Reach,
            "Call an additional function on the skipped arguments",
            "# Experimental!\n{∪⌞¯⊟ 1 2 3}\n{∪⌟⊟¯ 1 2 3}",
        ),
        subscript(
            Under,
            "Apply the undoing function to later arguments",
            "# Experimental!\n⍜⌟¯°+ ¯1.25"
        ),
        subscript(
            Fill,
            "Fill from the left instead of the right",
            "# Experimental!\n ⬚0[1_2_3 4_5 6]\n⬚⌟0[1_2_3 4_5 6]\n⬚⌞0[1_2_3 4_5 6]",
        ),
        subscript(
            EncodeBytes,
            "Choose little or big endian",
            "# Experimental!\nbytes⌞ \"u64\" 1234567890 # Little endian\nbytes⌟ \"u64\" 1234567890 # Big endian"
        ),
    ];

    let mixed = vec![
        subscript(
            Both,
            "Apply to N sets of arguments but some arguments N times",
            "{∩₃⌞₂⊟₃} 1 2 3 4 5\n{∩₃⌟₂⊟₃} 1 2 3 4 5",
        ),
        subscript(
            Reach,
            "Call an additional function on N skipped arguments",
            "# Experimental!\n{∪₂⌟⊟₃⊟ 1 2 3 4 5}\n{∪₂⌞⊟⊟₃ 1 2 3 4 5}",
        ),
        subscript(
            Rows,
            "Apply to rank-N subarrays and fix left-most or right-most argument(s)",
            "≡₁⌞₂(⊂⊂) 1_2 3_4 [5_6 7_8]\n≡₁⌟₂(⊂⊂) [1_2 3_4] 5_6 7_8",
        ),
        subscript(
            Inventory,
            "Apply to subarrays N deep and fix left-most or right-most argument(s)",
            "⍚₁⌞₂(⊂⊂) 1_2 3_4 [5_6 7_8]\n⍚₁⌟₂(⊂⊂) [1_2 3_4] 5_6 7_8",
        ),
    ];

    view! {
        <Title text=format!("Subscripts - {} Docs", lang())/>
        <h1>"Subscripts"</h1>
        <p>"By suffixing some functions or modifiers with a subscript number, their behavior can be modified."</p>
        <p>"Subscript numbers are typed with a "<code>","</code>" followed by some digits. The formatter will turn them into subscript digit characters."</p>
        <p>"The following functions and modifiers are supported. Not all are stable."</p>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th>"Primitive"</th>
                <th>"Meaning"</th>
                <th style="width: 100%">"Example"</th>
            </tr>
            <tr>
                <td>"Any "<span class="dyadic-function">"dyadic"</span>" pervasive function"</td>
                <td>"Constant first argument"</td>
                <td><Editor example="⊃+₁×₂ [1 2 3]" nonprogressive=true/></td>
            </tr>
            <tr>
                <td><Prims prims=[Select, Pick, Take, Drop, Rotate, Orient, Base]/></td>
                <td>"Constant first argument"</td>
                <td><Editor example="⊃↙₂↻₃ [1 2 3 4 5]" nonprogressive=true/></td>
            </tr>
            { numeric }
        </table>

        <Hd id="sided">"Sided Subscripts"</Hd>
        <p>"Sided subscripts augment a modifier or function in a way that can be thought of as having a \"side\"."</p>
        <p>"Sided subscripts are typed like normal subscripts with "<code>","</code>", but followed by "<code>"<"</code>" for left or "<code>">"</code>" for right. The formatter will turn them into "<code>"⌞"</code>" and "<code>"⌟"</code>" respectively."</p>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th>"Primitive"</th>
                <th>"Meaning"</th>
                <th style="width: 60%">"Example"</th>
            </tr>
            <tr>
                <td>"Any "<span class="dyadic-function">"dyadic"</span>" pervasive function"</td>
                <td>"Fix left-most or right-most argument"</td>
                <td><Editor example="# Experimental!\n+⌞ ⊸×100 1_2_3\n+⌟ ⊸×100 1_2_3" nonprogressive=true/></td>
            </tr>
            { sided }
        </table>

        <Hd id="mixed">"Mixed Subscripts"</Hd>
        <p>"Some modifiers that can take both numeric and sided subscripts can take them "<em>"both"</em>" at the same time. The numeric subscript is written first, followed by the sided subscript. Additionally, the sided subscript can be followed by "<em>"another"</em>" number to specify how many times to apply the sided behavior. This second number is called the \"side quntifier\"."</p>
        <table class="header-centered-table cell-centered-table" style="width: 100%">
            <tr>
                <th>"Primitive"</th>
                <th>"Meaning"</th>
                <th style="width: 60%">"Example"</th>
            </tr>
            { mixed }
        </table>
    }
}

#[component]
pub fn Experimental() -> impl IntoView {
    view! {
        <Title text=format!("Experimental Features - {} Docs", lang())/>
        <h1>"Experimental Features"</h1>
        <p>{lang}" has a number of features that are considered experimental. They are available in the interpreter for testing, but may be removed or changed in the future."</p>
        <p>"Using experimental features requires an "<code>"# Experimental!"</code>" comment to be placed at the top of a "{lang}" source file."</p>

        <Hd id="functions-modifiers">"Experimental Functions and Modifiers"</Hd>
        <ul>{
            Primitive::non_deprecated().filter(Primitive::is_experimental).map(|prim| {
                view! { <li><Prim prim=prim/></li> }
            }).collect::<Vec<_>>()
        }</ul>

        <Markdown src="/text/experimental.md"/>

        <Markdown src="/text/ga.md"/>
    }
}
