#![allow(clippy::needless_raw_string_hashes)]

use leptos::*;
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
        <h1>"Uiuisms"</h1>
        <p>"This is a curated list of Uiua functions for solving common problems."</p>
        <p>"You can add more by contributing in the "<a href="https://github.com/uiua-lang/uiua">"repo"</a>" to "<a href="https://github.com/uiua-lang/uiua/blob/main/site/src/uiuisms.rs#L107">"this list"</a>"."</p>
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
                if let Err(e) = uiua::Uiua::with_native_sys().load_str(code) {
                    panic!("Uiuism failed\n{code}\n{e}");
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
    "⊏⌂. [1 4 5 3 2]",
    /// Get n numbers between 0 and 1 inclusive
    "÷-1∶⇡. 11",
    /// Get n numbers between 0 and 1 exclusive
    "÷∶⇡. 10",
    /// Create an identity matrix
    "⊞=.⇡ 5",
    /// Create an X matrix
    "↥⇌.⊞=.⇡ 5",
    /// Check if an array is a palindrome
    r#"≅⇌. "racecar""#,
    /// Convert a number to a string
    r#"$"_" 17"#,
    /// Convert a string to a list of code points
    r#"-@\0 "Uiua""#,
    /// Convert a list of code points to a string
    r#"+@\0 [85 105 117 97]"#,
    /// Find the indices of all 1s
    "▽∶⇡⧻. [0 1 0 0 1]",
    /// Interleave two arrays
    "♭⍉⊟ [1 2 3 4] [5 6 7 8]",
    /// Intersperse an item between the rows of an array
    "↘¯1♭∺⊂∶ π [1 2 3 4]",
    /// Split an array by a delimiter
    r#"⊜□≠, @, "split,this,up""#,
    /// Split an array by a delimiter with fill elements
    r#"⍛@ ⊜·≠, @, "split,this,up""#,
    /// Split an array into groups of contiguous equal elements
    "⊜□. [1 1 1 2 2 1 1 4]",
    /// Find the nth fibonacci number
    ";⍥(+,∶)→.∶1 10",
    /// Remove all instances of an element from a list
    "▽≠, 4 [1 4 2 0 5 4 3]",
    /// Remove all instances of a row from an array
    "▽∺'¬≅, 2_0 [1_4 2_0 5_3 2_0]",
    /// Filter by a fixed predicate
    "▽ =0◿2 . ⇡10",
    /// Filter by a dynamic predicate
    "▽!→. (=0◿2) ⇡10",
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
    /// Trim matching prefix
    r#"▽¬\×∊, "thing" "thing← remove this""#,
    /// Trim matching suffix
    r#"▽¬⍜⇌\×∊, "thing" "remove this →thing""#,
    /// Trim whitespace
    r#"▽×⍜'⊟⇌≡\↥.≠@ . "  abc xyz   ""#,
    /// Upscale a matrix
    "▽↯⧻,/÷△.⍉▽↯⧻,30 [0_1 1_0]",
    /// Dot product
    "/+× [1 2 3] [4 ¯5 6]",
    /// Matrix product
    "⊠(/+×)→⍉ [1_2 3_4 5_6] [7_8_9 10_11_12]"
);
