use std::cell::RefCell;

use leptos::{ev::Event, *};
use rand::prelude::*;
use uiua::{compile::Compiler, format::format_str, ops::Primitive, UiuaResult};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::HtmlTextAreaElement;

const EXAMPLES: &[&str] = &[
    r#"‡=¯1 ≡/-◫2 ≤'A' ≍' ' ."Um, I um...arrays""#,
    "# Click Run to format!\nfirst repeat (\\+ rev)0_1 10",
    "↯~⇡/×.2_3_4",
    "Avg = ÷⇀~/+.\nAvg 0_2_1_5",
    "{÷×2 a -b ⚇¯.√-××4 a c ⁿ2 b}1 2 0",
    r#"⊟⸪(≅⇌.).["uiua" "racecar" "wow" "cool!"]"#,
    "‡⸪(=2 /+=⌊.÷+1 ⇡.).+1 ⇡60",
    "⍉↯4_4[...1 .2 .3 ...4 .5 .6]",
    "⸪(⊡~·_:_÷_≡_⍋ ⁅÷23)⊞×.-10 ⇡20",
    "# Change this ↓ to a 0\n!\"Oh no bad!\" 1\nprintln \"All is well\"",
];

#[cfg(test)]
#[test]
fn test_examples() {
    for example in EXAMPLES {
        Compiler::new()
            .eval(example)
            .unwrap_or_else(|e| panic!("Example failed:\n{example}\n{e}"));
    }
}

thread_local! {
    static SUBTITLE: RefCell<Option<usize>>  = RefCell::new(None);
}

pub fn main() {
    mount_to_body(|cx| view! { cx, <App/> })
}

#[component]
pub fn App(cx: Scope) -> impl IntoView {
    // Choose a random subtitle
    let subtitles = [
        view! {cx, <p>"A stack-oriented array programming language"</p>},
        view! {cx, <p>"An array-oriented stack programming language"</p>},
        view! {cx, <p>"A programming language for point-free enjoyers"</p>},
        view! {cx, <p>"A programming language for variable dislikers"</p>},
        view! {cx, <p>"What if APL was a FORTH?"</p>},
        view! {cx, <p>"What if FORTH was an APL?"</p>},
        view! {cx, <p>"Isn't a stack a sort of array?"</p>},
        view! {cx, <p>"It's got um...I um...arrays"</p>},
        view! {cx, <p><a href="https://youtu.be/seVSlKazsNk">"Point-Free or Die"</a></p>},
        view! {cx, <p>"Notation as a tool of thot"</p>},
        view! {cx, <p>"Do you like this page Marshall?"</p>},
        view! {cx, <p>"Conor Dyadic Hookstra"</p>},
    ];
    let index = SUBTITLE.with(|s| {
        *s.borrow_mut().get_or_insert_with(|| {
            let mut rng = SmallRng::seed_from_u64(instant::now().to_bits());
            // Prefers lower indices
            let index = rng.gen_range(0.0..(subtitles.len() as f64).cbrt());
            index.powi(3) as usize
        })
    });
    let subtitle = subtitles[index].clone();

    let (_, default_output) = run_code(EXAMPLES[0], false).expect_throw("First example failed");

    let (_, set_example) = create_signal(cx, 0);
    let (code, set_code) = create_signal(cx, EXAMPLES[0].to_string());
    let (output, set_output) = create_signal(cx, default_output);
    let (error, set_error) = create_signal(cx, String::new());

    // Run the code
    let run = move |format: bool| {
        set_output.set(String::new());
        set_error.set(String::new());
        let code_string = code_element().value();
        match run_code(&code_string, format) {
            Ok((formatted, stack)) => {
                code_element().set_value(&formatted);
                set_code.set(formatted);
                set_output.set(stack);
            }
            Err(e) => {
                log!("{}", e.show(false));
                set_error.set(e.show(false))
            }
        }
    };

    // Replace the selected text in the editor with the given string
    let replace_code = move |inserted: &str| {
        let elem = code_element();
        if let (Ok(Some(start)), Ok(Some(end))) = (elem.selection_start(), elem.selection_end()) {
            let (start, end) = (start.min(end) as usize, start.max(end) as usize);
            let text: String = code
                .get()
                .chars()
                .take(start)
                .chain(inserted.chars())
                .chain(code.get().chars().skip(end))
                .collect();
            code_element().set_value(&text);
            set_code.set(text);
        };
    };

    // Go to the next example
    let next_example = move |_| {
        set_example.update(|e| {
            *e = (*e + 1) % EXAMPLES.len();
            set_code.set(EXAMPLES[*e].to_string());
            code_element().set_value(EXAMPLES[*e]);
            run(false);
        })
    };
    // Go to the previous example
    let prev_example = move |_| {
        set_example.update(|e| {
            *e = (*e + EXAMPLES.len() - 1) % EXAMPLES.len();
            set_code.set(EXAMPLES[*e].to_string());
            code_element().set_value(EXAMPLES[*e]);
            run(false);
        })
    };

    // Run the code when Ctrl+Enter or Shift+Enter is pressed
    window_event_listener("keydown", move |event| {
        let event = event.dyn_ref::<web_sys::KeyboardEvent>().unwrap_throw();
        if event.key() == "Enter" && (event.ctrl_key() || event.shift_key()) {
            run(true);
        }
    });

    // Update the code when the textarea is changed
    let code_input = move |event: Event| {
        let text_area: HtmlTextAreaElement =
            event.target().unwrap_throw().dyn_into().unwrap_throw();
        set_code.set(text_area.value());
    };
    let mut glyph_buttons: Vec<_> = Primitive::ALL
        .iter()
        .filter_map(|p| {
            let name = p.name();
            let text = name
                .unicode
                .map(Into::into)
                .or_else(|| name.ascii.map(|s| s.to_string()))?;
            let extra = name
                .unicode
                .is_some()
                .then(|| name.ascii.map(|s| s.to_string()))
                .flatten()
                .unwrap_or_default();
            let title = format!(
                "{}{}",
                name.ident.unwrap_or_default(),
                format_args!("\n{extra}")
            );
            let onclick = move |_| replace_code(&p.to_string());
            let class = format!(
                "glyph-button {}",
                if p.is_modifier() {
                    "modifier-button"
                } else {
                    match p.args() {
                        Some(1) => "monadic-function-button",
                        Some(2) => "dyadic-function-button",
                        Some(3) => "triadic-function-button",
                        Some(4) => "tetradic-function-button",
                        _ => "noadic-function-button",
                    }
                }
            );
            Some(view! { cx,
                <button class=class title=title on:click=onclick>{ text }</button>
            })
        })
        .collect();

    for (glyph, title) in [
        ("_", "strand"),
        ("[]", "array"),
        ("()", "function"),
        ("|", "function separator"),
        ("¯", "negative\n`"),
        ("'", "character"),
        ("\"", "string"),
        ("=", "binding"),
        ("#", "comment"),
    ] {
        let onclick = move |_| replace_code(glyph);
        glyph_buttons.push(view! { cx,
            <button class="glyph-button" title=title on:click=onclick>{glyph}</button>
        });
    }

    view! {
        cx,
        <div id="top">
            <h1>{ "Uiua" }</h1>
            <div id="subtitle">
                { subtitle }
                <p><a href="https://github.com/kaikalii/uiua">{"GitHub"}</a></p>
            </div>
            <div id="editor">
                <div id="glyph-buttons">{ glyph_buttons }</div>
                <textarea class="code" id="code" spellcheck="false" on:input=code_input>{ move || code.get() }</textarea>
                <div id="output" class="code">
                    <div id="code-buttons">
                        <button id="run-button" class="code-button" on:click=move |_| run(true)>{ "Run" }</button>
                        <button id="prev-example" class="code-button" on:click=prev_example title="Previous example">{ "<" } </button>
                        <button id="next-example" class="code-button" on:click=next_example title="Next example">{ ">" } </button>
                    </div>
                    { move || output.get() }
                </div>
                <p id="error" class="code">{ move || error.get() }</p>
            </div>
            <div id="editor-help">
                <p class="editor-help-item">{ "Type some or all of a glyph's name, then run to format the names into glyphs." }</p>
                <p class="editor-help-item">{ "You can run with ctrl/shift + enter." }</p>
            </div>
            <br/>
            <br/>
            <MainText/>
        </div>
    }
}

/// Returns the output and the formatted code
fn run_code(code: &str, format_first: bool) -> UiuaResult<(String, String)> {
    let formatted = if format_first {
        format_str(code)?
    } else {
        code.to_string()
    };
    let mut compiler = Compiler::new();
    compiler.load_str(&formatted)?;
    let assembly = compiler.finish();
    let (values, output) = assembly.run_piped()?;
    let mut s = String::new();
    if !output.is_empty() {
        if !values.is_empty() {
            s.push_str("Output:\n");
        }
        s.push_str(&output);
        if !values.is_empty() {
            s.push_str("\n\nStack:\n");
        }
    }
    for val in values.into_iter().rev() {
        s.push_str(&val.show());
        s.push('\n');
    }
    Ok((formatted, s))
}

#[component]
fn MainText(cx: Scope) -> impl IntoView {
    view! { cx, <div>
        <p>"Uiua ("<i><a href="weewah.mp3">"wee-wuh"</a></i>") is a stack-oriented array programming language with a focus on tacit code (code without named values). Its semantics and primitives (and this site) are largely inspired by "<a href="https://mlochbaum.github.io/BQN/">"BQN"</a>", but it combines the array paradigm with the stack-oriented paradigm to make writing point-free code more workable."</p>
        <hr/>
        <h3>"How is Uiua like other array languages?"</h3>
        <p>"Like APL, J, BQN, and many other array languages, Uiua's primary data type is the array. Arrays are multidimensional and rank-polymorphic, meaning that many operation automatically apply to every element."</p>
        <p>"Uiua features:"</p>
        <ul>
            <li>"A rich set of primitives"</li>
            <li>"Arrays following the "<a href="https://mlochbaum.github.io/BQN/doc/based.html">"Based Array Model"</a></li>
            <li>"Basic IO facilities"</li>
            <li>"A deterministic formatter"</li>
        </ul>
        <hr/>
        <h3>"How is Uiua different from other array languages?"</h3>
        <p>"Uiua does away with the infix notation of most array languages for a more general prefix notation. While functions still take arguments, they pull those arguments from the stack and push their results back on. This enables a lot of cool stuff, particularly in the realm of tacit code."</p>
        <p>"Uiua also features:"</p>
        <ul>
            <li>"Hook and fork constructs without parenthesis"</li>
            <li>"Stack-to-array syntax for building arrays with stack operations"</li>
            <li>"Special syntax for constructing lists of functions"</li>
            <li>"Functions that work with lists of functions to form complex combinators"</li>
        </ul>
        <hr/>
        <h3>"How does writing the glyphs work?"</h3>
        <p>"Unlike most array languages, Uiua does not overload primitives depending on whether they are passed one or two arguments. Functions in Uiua can take any number of arguments, but an individual function always takes the "<i>"same"</i>" number of arguments."</p>
        <p>"This ends up meaning that Uiua requires way more glyphs to have one for every primitive. There simply are not enough keys on them keyboard to type them without using a bunch of hard-to-remeber shortcuts. Also, I think it's annoying to need special editor support to be able to write code properly."</p>
        <p>"To solve these issues, Uiua has a formatter that automatically converts ASCII names and characters into glyphs. You can type the name of a glyph (or a digraph, like >= for ≥), and the formatter will turn it into the corresponding glyph. Alternatively, the editor on the homepage has a button for each glyph."</p>
    </div>}
}

fn element<T: JsCast>(id: &str) -> T {
    document()
        .get_element_by_id(id)
        .unwrap_throw()
        .dyn_into::<T>()
        .unwrap_throw()
}

fn code_element() -> HtmlTextAreaElement {
    element("code")
}
