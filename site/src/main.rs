use uiua::{compile::Compiler, format::format};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::{window, HtmlTextAreaElement};
use yew::prelude::*;

const DEFAULT_CODE: &str = r#"⌗=¯1≡/-🗗2≤'A'∾' '."Um, I um ...arrays""#;

#[function_component]
fn App() -> Html {
    let code = use_state(|| DEFAULT_CODE.to_string());
    let output = use_state(String::new);
    let error = use_state(String::new);
    let onclick = {
        let code = code.clone();
        let output = output.clone();
        let error = error.clone();
        move |_| {
            output.set(String::new());
            error.set(String::new());
            match format(&code, "") {
                Ok(formatted) => {
                    code.set(formatted);
                    window()
                        .unwrap_throw()
                        .document()
                        .unwrap_throw()
                        .get_element_by_id("code")
                        .unwrap_throw()
                        .dyn_into::<HtmlTextAreaElement>()
                        .unwrap_throw()
                        .set_value(&code);
                }
                Err(e) => {
                    error.set(e.to_string());
                    return;
                }
            }
            let mut compiler = Compiler::new();
            if let Err(e) = compiler.load(&code, "") {
                error.set(e.to_string());
                return;
            }
            let assembly = compiler.finish();
            match assembly.run() {
                Ok(values) => {
                    let mut s = String::new();
                    for val in values {
                        s.push_str(&val.show());
                        s.push('\n');
                    }
                    output.set(s);
                }
                Err(e) => error.set(e.to_string()),
            }
        }
    };

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
        <div class="centered-div">
            <h1>{ "Uiua" }</h1>
            <h4>{ "A stack-oriented array programming language" }</h4>
            <textarea id="code" {oninput} value={ (*code).clone() }></textarea>
            <button {onclick}>{ "Run" }</button>
            <p>{ (*output).clone() }</p>
            <p>{ (*error).clone() }</p>
        </div>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
