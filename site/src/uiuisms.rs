#![allow(clippy::needless_raw_string_hashes)]

use leptos::*;
use leptos_meta::*;
use leptos_router::*;
use wasm_bindgen::JsCast;
use web_sys::{Event, HtmlInputElement};

use crate::Editor;

#[derive(Debug, Clone, PartialEq, Eq, Params)]
pub struct UiuismsParams {
    search: Option<String>,
}

#[component]
pub fn Uiuisms() -> impl IntoView {
    let initial_search = use_params::<UiuismsParams>()
        .get()
        .map(|p| p.search)
        .unwrap_or_default()
        .unwrap_or_default();
    let (search, _) = create_signal(initial_search);
    let (body, set_body) = create_signal(Vec::new());
    let items: Vec<(Uiuism, _)> = UIUISMS.with(|uiuism| {
        uiuism
            .iter()
            .map(|item| {
                (
                    item.clone(),
                    view!(<Editor example={&item.code} no_run=true />),
                )
            })
            .collect()
    });
    let update_search = move |search_text: &str| {
        let search_text = search_text.to_lowercase();
        set_body.set(
            items
                .iter()
                .filter(|(item, _)| {
                    search_text.is_empty()
                        || item.code.contains(&search_text)
                        || item.description.to_lowercase().contains(&search_text)
                })
                .map(|(item, view)| {
                    view! {
                        <div class="uiuism-item">
                            <div style="width: 29%">{ &item.description }</div>
                            <div style="width: 69%">{ view }</div>
                        </div>
                    }
                })
                .collect(),
        );
    };
    update_search(&search.get());
    let on_search_input = move |event: Event| {
        let elem: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
        update_search(&elem.value());
    };
    view! {
        <Title text="Uiuisms - Uiua Docs"/>
        <h1>"Uiuisms"</h1>
        <p>"This is a curated list of Uiua functions for solving common problems."</p>
        <p>"You can add more by contributing in the "<a href="https://github.com/uiua-lang/uiua">"repo"</a>" to "<a href="https://github.com/uiua-lang/uiua/blob/main/site/src/uiuisms.rs#L115">"this list"</a>"."</p>
        <div class="input-div">
            <input
                type="text"
                placeholder="Search"
                value={ search.get() }
                on:input=on_search_input />
        </div>
        <br/>
        { body }
    }
}

#[derive(Clone)]
struct Uiuism {
    code: String,
    description: String,
}

macro_rules! uiuisms {
    ($(#[doc = $desc:literal] $code:literal),* $(,)?) => {
        thread_local! {
            static UIUISMS: Vec<Uiuism> = vec![
                $(
                    Uiuism {
                        code: $code.to_string(),
                        description: $desc.to_string(),
                    }
                ),*
            ];
        }

        #[test]
        fn uiuisms() {
            for code in [$($code),*] {
                println!("Testing Uiuism:\n{code}");
                match uiua::Uiua::with_backend(crate::backend::WebBackend::default()).run_str(code) {
                    Ok(mut comp) => {
                        if let Some(diag) = comp.take_diagnostics().into_iter().next() {
                            panic!("Uiuism failed\n{code}\n{}", diag.report());
                        }
                    }
                    Err(e) => panic!("Uiuism failed\n{code}\n{}", e.report()),
                }
            }
        }
    };
}

uiuisms!(
    /// Reverse each row of an array
    "≡⇌ [1_2_3 4_5_6]",
    /// Get the sum of an array
    "/+ [1 2 3 4 5]",
    /// Get the product of an array
    "/× [1 2 3 4 5]",
    /// Get the maximum value in an array
    "/↥ [1 4 5 3 2]",
    /// Get the minimum value in an array
    "/↧ [1 4 5 3 2]",
    /// Sort an array
    "⊏⍏. [1 4 5 3 2]",
    /// Get n numbers between 0 and 1 exclusive
    "÷⟜⇡ 10",
    /// Get n numbers between 0 and 1 inclusive
    "÷-1⟜⇡ 11",
    /// Create a zero matrix
    "↯⟜⊚ 5",
    /// Create an identity matrix
    "⊞=.⇡ 5",
    /// Create an X matrix
    "↥⇌.⊞=.⇡ 5",
    /// Create a zero matrix of the same shape as another
    "≠. [1_1 2_2]",
    /// Create a 0xN matrix
    "↯0⊚ 2",
    /// Check if character is numeric [0-9]
    r#"×⊃≥≤@0,@9 "hi-5!""#,
    /// Check if character is letter [a-zA-Z]
    r#"↥∩×⊃(≥@a|≤@z|≥@A|≤@Z) "oiCu812""#,
    /// Check if an array is a palindrome
    r#"≍⇌. "racecar""#,
    /// Convert a number to a string
    r#"$"_" 17"#,
    /// Convert a string to a number
    r#"⋕ "42""#,
    /// Convert a string to a list of code points
    r#"-@\0 "Uiua""#,
    /// Convert a list of code points to a string
    r#"+@\0 [85 105 117 97]"#,
    /// Convert a list to a string
    r#"/$"_, _" [10 20 30 40]"#,
    /// Parse a string as a base 2 number
    r#"°⋯⇌-@0 "110""#,
    /// Parse a string as a base 3 <= X <= 9 number
    r#"/+×ⁿ⇌⇡⧻,⊙-:@0 3 "210""#,
    /// Interleave two arrays
    "♭⍉⊟ [1 2 3 4] [5 6 7 8]",
    /// Intersperse an item between the rows of an array
    "↘1♭≡⊂ π [1 2 3 4]",
    /// Get the cycling windows of an array
    "◫⊙⊂⊃⊙∘(↙-1) 3 ⇡7",
    /// Split an array at an index
    "⊃↙↘ 3 [1 2 3 4 5]",
    /// Split an array by a delimiter
    r#"⊜□≠, @, "split,this,up""#,
    /// Split an array by a delimiter with fill elements
    r#"⬚@ ⊜∘≠, @, "split,this,up""#,
    /// Split an array by a delimiter keeping empty segments
    r#"⊕□⍜▽¯:\+.=, @, "split,this,,up""#,
    /// Split an array into groups of contiguous equal elements
    "⊜□. [1 1 1 2 2 1 1 4]",
    /// Join a list of boxed strings
    r#"/◇⊂ {"a" "bc" "def"}"#,
    /// Find the nth fibonacci number
    "◌⍥(+⟜:)⊙.:1 10",
    /// Find the GCD of two numbers
    "◌⍢⊃◿∘± 35 360",
    /// Remove all instances of an element from a list
    "▽≠, 4 [1 4 2 0 5 4 3]",
    /// Remove first instance of an element from an array
    "⍜↻(↘1)⊗⊙. 4 [1 4 2 0 5 4 3]",
    /// Remove the nth element from an array
    "⍜↻(↘1) 4 [1 4 2 0 5 4 3]",
    /// Remove all instances of a row from an array
    "▽¬≡≍¤⊙. 2_0 [1_4 2_0 5_3 2_0]",
    /// Filter by a fixed predicate
    "▽ =0◿2 . ⇡10",
    /// Find the most common row in an array
    r#"⊏⊢⍖°⊚⊛:◴. "Hello World!""#,
    /// Convert a string to uppercase
    r#"-×32×≥@a,≤@z. "These are Words""#,
    /// Convert a string to lowercase
    r#"+×32×≥@A,≤@Z. "These are Words""#,
    /// Check if a string is in a list of strings
    r#"∊□ "uiua" {"apl" "bqn" "uiua"}"#,
    /// Trim leading whitespace
    r#"▽\↥≠@ . "   ← remove these""#,
    /// Trim trailing whitespace
    r#"▽⍜⇌\↥≠@ . "remove these →   ""#,
    /// Trim prefix-matching characters from a set
    r#"▽¬\×∊, "abc" "ccab ← remove this""#,
    /// Trim suffix-matching characters from a set
    r#"▽¬⍜⇌\×∊, "abc" "remove this → bcaa""#,
    /// Trim whitespace
    r#"▽×⍜(⊟⇌)≡\↥.≠@ . "  abc xyz   ""#,
    /// Upscale a 2d matrix
    "30 [0_1 1_0]\n▽▽⧻⟜:⍉▽▽⧻,⟜:",
    /// Upscale a colored image
    "30 [[0_0_1 0_1_0] [1_0_0 0_0_0]]\n°⍉▽▽⧻⟜:⍉▽▽⧻,⟜:",
    /// Linearly interpolate between two values
    "⍜-× 10 0 0.2\n⍜⊙-× 0.2 10 0",
    /// Set the value of an array at an index
    "⍜⊏◌ 2 1_2_3_4 10",
    /// Create a matrix of random 0s or 1s
    "⁅⊞⋅⋅⚂.⊚ 5",
    /// Create a matrix of random numbers
    "⌊×⊞⋅⋅⚂.⊚ 5 10",
    /// Arithmetic mean
    "÷⊃⧻/+ [85 105 117 97]",
    /// Dot product
    "/+× [1 2 3] [4 ¯5 6]",
    /// Cross product
    "1_2_3 4_5_6\n↻1-∩(×↻1)⊃:⊙∘",
    /// Matrix product
    "[7_8_9 10_11_12] [1_2 3_4 5_6]\n⍜⍉⊞(/+×)",
    /// Matrix power (Also works with scalars)
    "4 [1_2 3_4]\n⊙◌⍥(⊞(/+×)⊙⍉,):⊞=.⇡⬚1⊢△,",
    /// Repeat a function and collect intermediate results into an array
    "[⍥(×2.)] 10 1",
    /// Pad an array with 0s
    "[1_2_3 4_5_6]\n↻⊟.¯1⬚0↙+2△."
    /// Complex conjugate
    "⍜°ℂ¯ +i1"
    /// Cosine
    "○+η 1"
    /// Tangent
    "÷:°∠ 1"
    /// Factorial
    "/×+1⇡ 5"
    /// Rank of an array
    "⧻△ [[1 2 3 4][5 6 7 8][9 10 11 12]]"
    /// Main diagonal of an array
    "⊡≡↯⊃⧻(⇡/↧)△. [[1 2 3 4][5 6 7 8][9 10 11 12]]"
    /// Convert from base, input in little endian
    "/+×ⁿ⇡⧻, 3 0_2_1_1"
    /// Corner element of an array
    "⊢♭ [[1 2 3 4][5 6 7 8][9 10 11 12]]"
    /// Test if a positive integer is prime
    "=1⧻°/× 5"
    /// Inverse modulo m, returns m if it does not exist
    "⊗1◿:×⇡, 3 7"
    /// Multiplicative order modulo m
    "⊡1⊚=1◿:ⁿ⇡, 2 7"
    /// Continued fraction to decimal
    "/(+÷:1)⇌ 2_1_2_1_1_4_1_1_6"
    /// Complex argument
    "∠°ℂ i"
    /// Successive differences
    "↘1-↻¯1. 2_3_5_9_4"
    /// Binomial coefficient
    "÷××∩∩(/×+1⇡)1⊃-⊙∘ 5 3"
    /// Replace all of one element in a list with another
    "⍜▽⋅∘⊃=∘ [1 5 8 2] 5 3"
    /// Boxed powerset
    "≡(□▽)☇1⇡↯:2⊃⧻¤ [1 5 8 2]"
    /// Irrational number to n terms of continued fraction
    "⊙◌⍥(⊂⊙⊃⌊(÷:1◿1)):[] 10 e"
);
