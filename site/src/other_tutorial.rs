use enum_iterator::{all, Sequence};
use leptos::*;
use leptos_meta::Title;
use leptos_router::*;
use uiua::{Primitive, SysOp};
use uiua_editor::Editor;

use crate::{title_markdown, Challenge, Hd, Prim, Prims};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Sequence)]
pub enum OtherTutorialPage {
    CodeTactility,
    Documentation,
    Strings,
    Ranges,
    EvenMoreStack,
    FilesAndStreams,
    Audio,
    Images,
}

impl OtherTutorialPage {
    pub fn path(&self) -> String {
        format!("{self:?}").to_lowercase()
    }
    pub fn view(&self) -> View {
        match self {
            Self::Strings => {
                title_markdown("Strings", "/text/strings.md", strings_challenges).into_view()
            }
            Self::FilesAndStreams => {
                title_markdown("Files and Streams", "/text/files_and_streams.md", ()).into_view()
            }
            Self::Audio => Audio().into_view(),
            Self::Images => ImagesAndGifs().into_view(),
            Self::Documentation => Documentation().into_view(),
            Self::CodeTactility => {
                title_markdown("Code Tactility", "/text/code_tactility.md", ()).into_view()
            }
            Self::Ranges => title_markdown("Ranges", "/text/ranges.md", ()).into_view(),
            Self::EvenMoreStack => EvenMoreStack().into_view(),
        }
    }
    pub fn title(&self) -> &'static str {
        match self {
            OtherTutorialPage::Strings => "Strings",
            OtherTutorialPage::FilesAndStreams => "Files and Streams",
            OtherTutorialPage::EvenMoreStack => "Even More Stack Manipulation",
            OtherTutorialPage::Audio => "Audio",
            OtherTutorialPage::Images => "Images and GIFs",
            OtherTutorialPage::Documentation => "Documenting Code",
            OtherTutorialPage::CodeTactility => "Code Tactility",
            OtherTutorialPage::Ranges => "Ranges",
        }
    }
    pub fn description(&self) -> &'static str {
        match self {
            OtherTutorialPage::Strings => "how to manipulate strings",
            OtherTutorialPage::FilesAndStreams => "how to read and write files and streams",
            OtherTutorialPage::EvenMoreStack => "more ways to manipulate the stack",
            OtherTutorialPage::Audio => "how to generate and play audio",
            OtherTutorialPage::Images => "how to generate images and GIFs",
            OtherTutorialPage::Documentation => "how to document Uiua code",
            OtherTutorialPage::CodeTactility => "tools for debugging and editing code",
            OtherTutorialPage::Ranges => "various ways to generate ranges",
        }
    }
}

impl IntoParam for OtherTutorialPage {
    fn into_param(value: Option<&str>, name: &str) -> Result<Self, ParamsError> {
        all::<OtherTutorialPage>()
            .find(|p| p.path() == value.unwrap_or(""))
            .ok_or_else(|| ParamsError::MissingParam(name.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct OtherTutorialParams {
    pub page: OtherTutorialPage,
}

#[component]
pub fn Audio() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Audio Output - Uiua Docs"/>
        <h1>"Audio Output"</h1>
        <p>"Uiua has a built-in support for audio output."</p>

        <Hd id="basic-synthesis">"Basic Synthesis"</Hd>
        <p>"In the online editor, you need only make an array that looks like audio samples."</p>
        <p>"Audio samples must be either a rank 1 where each element is a sample or a rank 2 array where each row is a channel."</p>
        <p>"The samples must be between "<code>"-1"</code>" and "<code>"1"</code>". We use the "<Prim prim=Sys(SysOp::AudioSampleRate)/>" system function to get the sample rate of the audio output."</p>
        <p>"For a minimal example, here is a simple 1 second sawtooth wave:"</p>
        <Editor example="÷2 ◿1×220 ÷⟜⇡&asr"/>
        <p>"First, we make a range of numbers from 0 to 1 by getting the "<Prim prim=Range/>" up to the sample rate and dividing it by that much. This array represents the time at each sample."</p>
        <p>"Then, we multiply the time by 220, the frequency of an A3 note, and take the "<Prim prim=Modulus/>"1 of that. This gives us a nice pure sawtooth wave."</p>
        <p>"Finally, the wave is a little loud on its own, so we "<Prim prim=Div/>" it by 2."</p>
        <br/>
        <p>"For longer time arrays, "<Prim prim=Mul/>" the number of samples by the number of seconds you want before calling "<Prim prim=Range/>" but after "<Prim prim=Dup/>"."</p>
        <Editor example="÷2◿1×220÷:⇡ ×3 .&asr"/>
        <p>"If you "<Prim prim=Mul/>" by a non-integer, you may need to use "<Prim prim=Round/>" to prevent an error."</p>
        <Editor example="÷2◿1×220÷:⇡ ⁅×0.5 .&asr"/>

        <Hd id="notes">"Notes"</Hd>
        <p>"My favorite way to make multiple notes is to "<Prim prim=Table/>" different frequencies with the time array."</p>
        <p>"Then, if you want a chord, you can use "<Prim prim=Reduce glyph_only=true/><Prim prim=Add glyph_only=true/>" to add them together."</p>
        <p>"If you want sequence instead, you can use "<Prim prim=Reduce glyph_only=true/><Prim prim=Join glyph_only=true/>"."</p>
        <p>"You can calculate freqencies "<code>"f"</code>" that are a certain number of half-steps "<code>"n"</code>" from another with the formula "<code>"f×2^(n/12)"</code>" which can be written in Uiua as"<Prims prims=[Mul]/><code>"f"</code><Prims prims=[Pow]/><Prims prims=[Flip]/><code>"2"</code><Prims prims=[Div]/><code>"12 n"</code>"."</p>
        <p>"In this example, we make both a chord and a sequence from the same notes. We use "<Prim prim=Sin glyph_only=true/><Prim prim=Mul glyph_only=true/><Prim prim=Tau glyph_only=true/>" to make a sine wave instead of a saw wave."</p>
        <Editor example="\
f ← ×220ⁿ:2÷12 [0 4 7]
s ← ∿×τ⊞×f ÷⟜⇡&asr
÷⧻f/+s
÷⧻f/⊂s"/>

        <Hd id="native-audio">"Native Audio"</Hd>
        <p>"If running code in the native Uiua interpreter, arrays will not be automatically turned into audio."</p>
        <p>"Instead, you must use the "<Prim prim=Sys(SysOp::AudioPlay)/>" system function to play it."</p>
        <p><Prim prim=Sys(SysOp::AudioPlay)/>" should work fine on the website as well, but it is not necessary."</p>
        <Editor example="&ap÷2×¬◿1×4:±∿×τ×55.÷:⇡×2. &asr"/>
    }
}

#[component]
pub fn ImagesAndGifs() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Images and GIFs - Uiua Docs"/>
        <h1>"Images and GIFs"</h1>
        <p>"Uiua has built-in support for generating images and GIFs."</p>

        <Hd id="images">"Images"</Hd>
        <p>"Creating an image is as simple as creating an array of pixel data."</p>
        <p>"To start, we can create a list of numbers from 0 to 1 by dividing a "<Prim prim=Range/>" by its length."</p>
        <Editor example="÷⟜⇡10"/>
        <p>"If we make the list a bit bigger and "<Prim prim=Table/>" it with itself, we can form a square array of numbers."</p>
        <p>"Let's see what that looks like with different functions used in the "<Prim prim=Table/>"."</p>
        <Editor example="⊞<. ÷⟜⇡100"/>
        <Editor example="⊞×. ÷⟜⇡100"/>
        <Editor example="⊞◿. ÷⟜⇡100"/>
        <p>"We can use a bit of math(s) to make more interesting patterns. Here, we use a "<Prim prim=Dip/>" and "<Prim prim=Identity/>" in the "<Prim prim=Table/>" to turn each list into its own image. See what happens if you remove "<Prim prim=Lt/>"."</p>
        <Editor example="< ⊞⊙∘ -1/2 : ×0.2∿×τ . ÷⟜⇡100"/>
        <p>"So far, these images have all been rank-2 arrays of grayscale pixel data, but rank-3 arrays allow for multiple color channels!"</p>
        <p>"The last axis is always the colors. In this example, we create an array with "<Prim prim=Shape/>" "<code>"[100 100 2]"</code>". Because there are only 2 color channels, the image will be interpreted as grayscale with an alpha channel."</p>
        <Editor example="⊞⊟. ÷⟜⇡100"/>
        <p>"With 3 or 4 color channels, you can create full-color images."</p>
        <Editor example="⊞(⊂⊂.). ÷⟜⇡100 # RGB"/>
        <Editor example="⊞(⊂.⊂). ÷⟜⇡100 # RGBA"/>
        <p>"In the examples above, the image array is constructed in such a way that the color channels are already the last axis. To create an image by combining color channels, it may be necessary to use "<Prim prim=Transpose/>"."</p>
        <Editor example="< ⊞⊙∘ -0.4 : ×0.2∿×τ . ÷⟜⇡100\n[⍉.⇌.]\n△. # Not a valid image shape\n⍉:\n△. # Valid image shape"/>
        <p>"Of course, images need not be sqaure."</p>
        <Editor example="⊞< :+1/2÷3∿×τ: ∩(÷100⇡) 100 300"/>
        <p>"The "<code>"Logo"</code>" constant is a quick way to get the Uiua logo as an image."</p>
        <Editor example="Logo"/>
        <p>"The "<Prim prim=Keep/>" function can be used to scale an image vertically. "<Prims prims=[Rows, Keep]/>" scales it horizontally. Non-integer scales are allowed."</p>
        <Editor example="▽ 0.5 Logo"/>
        <Editor example="≡▽ 2 Logo"/>
        <Editor example="▽⟜≡▽ 0.7 Logo"/>

        <Hd id="gifs">"GIFs"</Hd>
        <p>"To create a GIF, simply create an array where every row is an image."</p>
        <p>"Here, we define a function that takes a frame parameter and generates an image, then evaluate it for each value in a range."</p>
        <Editor example="F ← <⊞×. ÷⟜⇡100 ÷2+1∿×τ\n∵F÷⟜⇡30"/>

        <Hd id="system-functions">"System Functions"</Hd>
        <p>"If you use the native interpreter, arrays will not be automatically converted into images or GIFs like they are on the website. To generate them, you must explicitly call certain system functions."</p>
        <p>"You can find lists of "<A href="/docs/imag">"image"</A>" and "<A href="/docs/gif">"GIF"</A>" system functions on the main docs page."</p>
        <p>"One system function that is particularly useful on the website is "<Prim prim=Sys(SysOp::GifShow)/>", which lets you set the framerate of a GIF."</p>
        <Editor example="÷2+1∿×τ÷⟜⇡30\n∵(⍉[⍉..]⊞× ⟜(+∿) ÷⟜⇡80)\n&gifs 30"/>
    }
}

fn strings_challenges() -> impl IntoView {
    view! {
        <Hd id="challenges">"Challenges"</Hd>

        <Challenge
            number=1
            prompt="counts the number of times a string appears in another string"
            example="\"ab\" \"abracadabra\""
            answer="/+⌕"
            tests={&["\"123\" \"12345678\"", "\"()\" \"(()(())()(()()))\""]}
            hidden="\"5\" \"dog\""/>

        <Challenge
            number=2
            prompt="finds the first and last number in a string and adds them together"
            example="\"1foo2bar3\""
            answer="+⊃⊢⊣ ⋕♭regex\"\\\\d+\""
            tests={&["\"What is 1 + 2?\"", "\"99 bottles of beer on the wall, 99 bottles of beer\"", "\"(555) 555-5555\""]}
            best_answer="+⊃⊢⊣ ⊜⋕⊸∊+@0⇡10"
            hidden="\"123\""/>
    }
}

#[component]
pub fn Documentation() -> impl IntoView {
    view! {
        <Title text="Documenting Code - Uiua Docs"/>
        <h1>"Documenting Code"</h1>
        <p>"Uiua interprets comments in certain contexts as documentation."</p>
        <p>"For example, writing a comment directly above a binding will make it the documentation for that binding. A binding's documentation will be show when hovering over any references to it, both on this site and when using the language server "<A href="/docs/install#editor-support">"in your native editor"</A>"."</p>
        <p>"Hover over any of the instances of the name "<code>"Avg"</code>" in the example below to see the documentation."</p>
        <Editor example="# Get the average of an array\nAvg ← ÷⧻⟜/+\nAvg [1 2 7 6]"/>
        <p>"Multiple lines of documentation can be written by using multiple comments."</p>
        <Editor example="# Remove the first instance of one array from another\n# The first array must be one rank lower than the second\nRemFirst ← ⍜↻(↘1)⊸⊗\nRemFirst 1_2 [3_5 1_2 0_2 1_2]"/>
        <p>"If both your binding code and your documentation are short, you can write them on the same line."</p>
        <Editor example="Avg ← ÷⧻⟜/+ # Average of an array\nAvg [1_2 3_4 5_9]"/>
        <p>"If you start a line in a comment with "<code>"?"</code>", subsequent words will be interpreted as argument names."</p>
        <p>"These are handled separately from the rest of the comment, and they will be checked against a function's signature."</p>
        <Editor example="# Remove the first instance of one array from another\n# ? Needle Haystack\nRemFirst ← ⍜↻(↘1)⊸⊗"/>
        <p>"These names should follow the same conventions as binding names."</p>
        <Editor example="# Do the thing\n# ? x y\nFoo ← ≡↻⇡⧻⟜¤"/> // Should fail
        <p>"The "<code>"?"</code>" is similar to the "<Prim prim=Primitive::Stack/>" function because the arguments indicate the intended state of the stack before the function is called."</p>
        <p>"If you also want to give names to a function's outputs, you can list them in front of the "<code>"?"</code>". This lets you read the comment signature right-to-left, the same way as normal Uiua code."</p>
        <Editor example="# Quotient Remainder ? Divisor Dividend\nDivRem ← ⌊⊃÷◿\nDivRem 3 7"/>
        <p>"These kinds of comments can also be put at the end of lines. The signature of the line will be checked against the signature specified in the comment."</p>
        <Editor example="1 2 # A B ?\n+⌵  # Sum ? A B\n⇡+5 # Res ? Foo Bar"/> // Should fail
        <p>"These can be put in functions as well."</p>
        <Editor example="# Get the average of a list\n# ? List\nAvg ← (\n  ⟜/+ # List Sum ? List\n  ⧻   # Length ? List\n  ÷   # Avg ? Length List\n)"/>

        <Hd id="track-caller"><code>"# Track caller!"</code></Hd>

        <p>"When writing code that other people might use, it is important that error messages point to a useful place. When a function errors, the error message often refers to the base built-in function that emitted the error."</p>
        <Editor example="F ← +@a\nF 5\nF @b"/> // Should fail
        <p>"However, that position in the code might not be useful to the person trying to figure out where they went wrong."</p>
        <p>"The "<code>"# Tracker caller!"</code>" semantic comment tells a function to emit errors at its call site rather than from within. It can be placed either inside the function's body, or on the line just above it."</p>
        <Editor example="F ← +@a # Track caller!\nF 5\nF @b"/> // Should fail
        <p>"Notice the difference in the error messages."</p>

        <Hd id="deprecated"><code>"# Deprecated!"</code></Hd>

        <p>"The "<code>"# Deprecated!"</code>" semantic comment causes a warning to be emitted when the function is referenced. It can be placed either inside the function's body, or on the line just above it."</p>
        <p>"This is useful for making it clear to users that a function is no longer supported, but without breaking their code."</p>
        <Editor example="# Deprecated!\nF ← +2\nF 5"/> // Should fail
        <p>"You can note a suggested replacement after the "<code>"!"</code>". This will appear in the warning message."</p>
        <Editor example="F ← +2 # Deprecated! Use G instead\nG ← ×2\nF 5"/> // Should fail
        <p><code>"# Deprecated!"</code>" works for constants, macros, and modules as well."</p>
        <Editor example="# Deprecated! Don't use it!\n┌─╴Foo\n  # Deprecated! It's no good!\n  Bar ← 5\n└─╴\nFoo~Bar"/> // Should fail
    }
}

#[component]
fn EvenMoreStack() -> impl IntoView {
    use Primitive::*;
    view! {
        <Title text="Even More Stack Manipulation - Uiua Docs"/>
        <h1>"Even More Stack Manipulation"</h1>
        <p>"In the "<A href="/tutorial/morestack">"More Stack Manipulation"</A>" tutorial, we learned about various ways of working with the stack including "<Prim prim=Fork/>", "<Prim prim=Bracket/>", "<Prim prim=Both/>", and "<Prim prim=Dip/>". We even touched briefly on "<Prim prim=On/>" and "<Prim prim=By/>"."</p>
        <p>"But "<Prim prim=On/>" and "<Prim prim=By/>" are just two modifiers in a larger category."</p>

        <Hd id="with-and-off"><Prim prim=With/>" and "<Prim prim=Off/></Hd>
        <p>"Consider that "<Prim prim=On/>" keeps the "<strong>"first"</strong>" argument "<strong>"above"</strong>" the outputs of a function, and "<Prim prim=By/>" keeps the "<strong>"last"</strong>" argument "<strong>"below"</strong>" the outputs of a function. We can then conceive of similar modifiers that fill in other possible combinations."</p>
        <p><Prim prim=With/>" keeps the "<strong>"last"</strong>" argument "<strong>"above"</strong>" the outputs of a function, and "<Prim prim=Off/>" keeps the "<strong>"first"</strong>" argument "<strong>"below"</strong>" the outputs of a function."</p>
        <p>"This table shows how these six modifiers are related:"</p>
        <table class="bordered-table header-centered-table">
            <tr>
                <td colspan="2">"Keep _ argument"</td>
                <th></th>
                <th></th>
            </tr>
            <tr>
                <th>"the first"</th>
                <th>"the last"</th>
                <th>"every"</th>
                <th></th>
                <th></th>
            </tr>
            <tr>
                <td><Prim prim=On/></td>
                <td><Prim prim=With/></td>
                <td><Prim prim=Above/><span title="Experimental">"🧪"</span></td>
                <th>"above"</th>
                <td rowspan="2">"_ the outputs"</td>
            </tr>
            <tr>
                <td><Prim prim=Off/></td>
                <td><Prim prim=By/></td>
                <td><Prim prim=Below/></td>
                <th>"below"</th>
            </tr>
        </table>
        <p>"That's the theory, but how should we actually use and think about these modifiers?"</p>

        <Hd id="on"><Prim prim=On/></Hd>
        <p>"Use "<Prim prim=On/>" when you want to transform an array with some other array, but reuse the first."</p>
        <p>"One common example of this is in upscaling an image. We want to use "<Prim prim=Keep/>" to scale the array along 2 different axes, so we can use "<Prim prim=On/>" preserve the scale to be reused."</p>
        <Editor example="▽⟜≡▽ 3 [1_2 3_4]"/>
        <Editor example="▽⟜≡▽ 0.2 Logo"/>
        <p>"Another common example is getting N numbers between 0 and 1. We want to "<Prim prim=Div/>" a "<Prim prim=Range/>" by its length, so we use "<Prim prim=On/>" to reuse the N."</p>
        <Editor example="÷⟜⇡5"/>

        <Hd id="by"><Prim prim=By/></Hd>
        <p>"Use "<Prim prim=By/>" when you want to operate on an array based on some value "<em>"derived"</em>" from that array."</p>
        <p>"For example, if we want to "<Prim prim=Keep/>" only odd numbers in an array, we derive the mask while preserving the array with "<Prim prim=By/>"."</p>
        <Editor example="▽⊸◿2 [2 3 8 4 9 1]"/>
        <p>"This works the same if we wanted to keep all numbers above or below a certain value."</p>
        <Editor example="▽⊸≥4 [2 3 8 4 9 1]"/>

        <Hd id="off"><Prim prim=Off/></Hd>
        <p>"Use "<Prim prim=Off/>" in similar situations to "<Prim prim=By/>", but when the derived calculation also relies on values farther down the stack."</p>
        <p>"For example, if we wanted to keep all indices in an array that have "<code>"1"</code>"s in some mask, we can use "<Prim prim=Off/>" to make sure everything ends up where we want it."</p>
        <Editor example="▽⤚⊏ [1 2 3 4 5] [0 1 0 0 1 1 0 1]"/>

        <Hd id="with"><Prim prim=With/></Hd>
        <p><Prim prim=With/>" has fewer common patterns that come up often, (other than "<Prims prims=[Assert, With, Match]/>" for "<A href="/tutorial/testing">"testing"</A>"), but it can still be useful in some situations."</p>
        <p>"Its name was chosen because after using it, you end up with an array "<em>"with"</em>" a transformed version of it."</p>
        <Editor example="⊟⤙↻1 [1 2 3 4 5] # Couple *with* rotation"/>

        <Hd id="below"><Prim prim=Below/></Hd>
        <p>"Use "<Prim prim=Below/>" when you want to call a function without disturbing the stack "<em>"at all"</em>"."</p>
        <p><Prim prim=By/>" is generally prefered for monadic functions on a single argument, but "<Prim prim=Below/>" is useful for the more general case."</p>
        <p>"For example, here, we use "<Prim prim=Below/>" to retrieve the "<Prim prim=Shape/>"s of two arrays so that we can scale one array to the size of the other."</p>
        <Editor example="∧(⍉▽)÷◡∩△ [1_2 3_4] °△4_6"/>
        <p>"For a simpler example, we can use "<Prim prim=Below/>" with "<Prim prim=Gap/>" to call a function on a value further down the stack."</p>
        <Editor example="◡⋅⧻ 1_2_3 \"hello!\""/>

        <hr/>

        <p>"Mastering these stack manipulation modifiers takes time and practice. When you end up with a convoluted bit of stack manipulation code, try to see if you can simplify it by using one of these modifiers."</p>
        <p>"Often, even simple patterns can be simplified further. For example, "<Prim prim=Flip/><Prim prim=On/>" is usually just "<Prim prim=Off/>"."</p>
        <Editor example="▽<2:⟜⊡ [1_2 0_1] [0_1_2 3_4_5]\n▽<2 ⤚⊡ [1_2 0_1] [0_1_2 3_4_5]"/>
        <p>"When you reduce a pattern to its simplest form, you can often gain a better view of the flow of data through the program."</p>
    }
}
