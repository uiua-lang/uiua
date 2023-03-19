use gloo::{events::EventListener, utils::document};
use uiua::{compile::Compiler, format::format, value::Value, UiuaResult};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::HtmlTextAreaElement;
use yew::prelude::*;

const DEFAULT_CODE: &str = r#"⌗=¯1≡/-🗗2≤'A'∾' '."Um, I um ...arrays""#;

#[function_component]
fn App() -> Html {
    let (_, default_output) = run_code(DEFAULT_CODE).unwrap_throw();

    let code = use_state(|| DEFAULT_CODE.to_string());
    let output = use_state(move || default_output);
    let error = use_state(String::new);

    // Run the code
    let run = {
        let code = code.clone();
        let output = output.clone();
        let error = error.clone();
        move || {
            output.set(String::new());
            error.set(String::new());
            match run_code(&code) {
                Ok((formatted, stack)) => {
                    code.set(formatted);
                    if let Some(elem) = element::<HtmlTextAreaElement>("code") {
                        elem.set_value(&code);
                    }
                    output.set(stack);
                }
                Err(e) => error.set(e.to_string()),
            }
        }
    };

    // Run the code when the button is clicked
    let onclick = {
        let run = run.clone();
        move |_| run()
    };

    // Run the code when Ctrl+Enter or Shift+Enter is pressed
    use_effect(move || {
        let listener = EventListener::new(&document(), "keydown", move |event| {
            let event = event.dyn_ref::<web_sys::KeyboardEvent>().unwrap_throw();
            if event.key() == "Enter" && (event.ctrl_key() || event.shift_key()) {
                run();
            }
        });
        || drop(listener)
    });

    // Update the code when the textarea is changed
    let oninput = {
        let code = code.clone();
        move |e: InputEvent| {
            let event: Event = e.dyn_into().unwrap_throw();
            let text_area: HtmlTextAreaElement =
                event.target().unwrap_throw().dyn_into().unwrap_throw();
            code.set(text_area.value());
        }
    };

    html! {
        <div id="top" class="centered-div">
            <h1>{ "Uiua" }</h1>
            <h4>{ "A stack-oriented array programming language" }</h4>
            <div id="editor">
                <textarea class="code" id="code" spellcheck="false" {oninput} value={ (*code).clone() }></textarea>
                <div id="output" class="code">
                    <button id="run-button" {onclick}>{ "Run" }</button>
                    { (*output).clone() }
                </div>
                <p id="error" class="code">{ (*error).clone() }</p>
            </div>
        </div>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}

fn element<T: JsCast>(id: &str) -> Option<T> {
    Some(
        document()
            .get_element_by_id(id)?
            .dyn_into::<T>()
            .unwrap_throw(),
    )
}

fn run_code(code: &str) -> UiuaResult<(String, String)> {
    let formatted = format(code, "")?;
    let mut compiler = Compiler::new();
    compiler.load(code, "")?;
    let assembly = compiler.finish();
    let values = assembly.run()?;
    let mut s = String::new();
    for val in values.into_iter().rev() {
        s.push_str(&val.show());
    }
    Ok((formatted, s))
}
