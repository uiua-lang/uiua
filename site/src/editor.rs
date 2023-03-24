use instant::Duration;
use leptos::*;
use uiua::{format::format_str, ops::Primitive, Assembly, UiuaResult};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::{Event, HtmlDivElement, HtmlTextAreaElement};

use crate::{backend::WebBackend, prim_class};

#[derive(Debug, Clone, Copy, Default)]
pub enum EditorSize {
    #[default]
    Small,
    Medium,
}

#[component]
pub fn Editor(
    cx: Scope,
    #[prop(optional)] examples: &'static [&'static str],
    #[prop(optional)] size: EditorSize,
    #[prop(optional)] help: &'static [&'static str],
) -> impl IntoView {
    let examples = if examples.is_empty() { &[""] } else { examples };
    let code_em = examples.iter().map(|e| e.lines().count()).max().unwrap() as f32 * 1.3 + 0.5;

    let (code_id, _) = create_signal(cx, format!("code{:p}", examples));
    let (output_id, _) = create_signal(cx, format!("output{:p}", examples));

    let code_element = move || -> HtmlTextAreaElement { element(&code_id.get()) };
    let output_element = move || -> HtmlDivElement { element(&output_id.get()) };

    let (example, set_example) = create_signal(cx, 0);
    let (code, set_code) = create_signal(cx, examples[0].to_string());
    let (output, set_output) = create_signal(cx, String::new());

    // Run the code
    let run = move |format: bool| {
        set_output.set(String::new());
        let code_elem = code_element();
        let code_string = code_elem.value();
        let input = if format {
            if let Ok(formatted) = format_str(&code_string) {
                let formatted = formatted.trim();
                code_elem.set_value(formatted);
                set_code.set(formatted.to_string());
                formatted.into()
            } else {
                code_string
            }
        } else {
            code_string
        };
        match run_code(&input) {
            Ok(stack) => {
                set_output.set(stack);
                _ = output_element().style().remove_property("color");
            }
            Err(e) => {
                log!("{}", e.show(false));
                set_output.set(e.show(false));
                _ = output_element().style().set_property("color", "#f33");
            }
        }
    };

    // Replace the selected text in the editor with the given string
    let replace_code = move |inserted: &str| {
        let elem = code_element();
        if let (Ok(Some(start)), Ok(Some(end))) = (elem.selection_start(), elem.selection_end()) {
            let (start, end) = (start.min(end), start.max(end) as usize);
            let text: String = code
                .get()
                .chars()
                .take(start as usize)
                .chain(inserted.chars())
                .chain(code.get().chars().skip(end))
                .collect();
            elem.set_value(&text);
            _ = elem.focus();
            _ = elem.set_selection_range(start + 1, start + 1);
            set_code.set(text);
        };
    };

    // Go to the next example
    let next_example = move |_| {
        set_example.update(|e| {
            *e = (*e + 1) % examples.len();
            set_code.set(examples[*e].to_string());
            code_element().set_value(examples[*e]);
            run(false);
        })
    };
    // Go to the previous example
    let prev_example = move |_| {
        set_example.update(|e| {
            *e = (*e + examples.len() - 1) % examples.len();
            set_code.set(examples[*e].to_string());
            code_element().set_value(examples[*e]);
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

    set_timeout(move || run(false), Duration::from_millis(0));

    // Update the code when the textarea is changed
    let code_input = move |event: Event| {
        let text_area: HtmlTextAreaElement =
            event.target().unwrap_throw().dyn_into().unwrap_throw();
        set_code.set(text_area.value());
    };
    let mut glyph_buttons: Vec<_> = Primitive::ALL
        .iter()
        .filter_map(|p| {
            let text = p
                .unicode()
                .map(Into::into)
                .or_else(|| p.ascii().map(|s| s.to_string()))?;
            let extra = p
                .unicode()
                .is_some()
                .then(|| p.ascii().map(|s| s.to_string()))
                .flatten()
                .unwrap_or_default();
            let title = format!(
                "{}{}",
                p.ident().unwrap_or_default(),
                format_args!("\n{extra}")
            );
            let onclick = move |_| replace_code(&p.to_string());
            let class = format!("glyph-button {}", prim_class(*p));
            Some(view! { cx,
                <button class=class title=title on:click=onclick>{ text }</button>
            })
        })
        .collect();

    for (glyph, title) in [
        ("_", "strand"),
        ("[]", "array"),
        ("()", "function"),
        ("{}", "ref function"),
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

    let (editor_class, code_class) = match size {
        EditorSize::Small => ("small-editor", "small-code"),
        EditorSize::Medium => ("medium-editor", "medium-code"),
    };

    let example_arrow_style = if examples.len() <= 1 {
        "display:none"
    } else {
        ""
    };

    let (show_glyphs, set_show_glyphs) = create_signal(
        cx,
        match size {
            EditorSize::Small => false,
            EditorSize::Medium => true,
        },
    );

    let show_glyphs_text = move || if show_glyphs.get() { "↥" } else { "↧" };
    let show_glyphs_title = move || {
        if show_glyphs.get() {
            "Hide glyphs"
        } else {
            "Show glyphs"
        }
    };
    let toggle_show_glyphs = move |_| set_show_glyphs.update(|s| *s = !*s);

    let glyph_buttons_style = move || {
        if show_glyphs.get() {
            ""
        } else {
            "display:none"
        }
    };

    let glyph_button_bottom = glyph_buttons.split_off(glyph_buttons.len() / 2);

    let example_text =
        move || (examples.len() > 1).then(|| format!("{}/{}", example.get() + 1, examples.len()));

    view! { cx,
        <div>
            <div id="editor" class=editor_class>
                <div style=glyph_buttons_style>
                    <div class="glyph-buttons">{glyph_buttons}</div>
                    <div class="glyph-buttons">{glyph_button_bottom}</div>
                </div>
                <div id="code-area">
                    <div id="code-right-side">
                        <button
                            id="glyphs-toggle-button"
                            title=show_glyphs_title
                            on:click=toggle_show_glyphs>{show_glyphs_text}</button>
                        <div id="example-tracker">{example_text}</div>
                    </div>
                    <textarea
                        id={code_id.get()}
                        spellcheck="false"
                        class={format!("code {code_class}")}
                        style={format!("height: {code_em}em")}
                        on:input=code_input>{ move || code.get() }</textarea>
                </div>
                <div id={output_id.get()} class="output">
                    <div id="output-text">
                        { move || output.get() }
                    </div>
                    <div id="code-buttons">
                        <button id="run-button" class="code-button" on:click=move |_| run(true)>{ "Run" }</button>
                        <button
                            id="prev-example"
                            class="code-button"
                            style=example_arrow_style
                            on:click=prev_example title="Previous example">{ "<" } </button>
                            <button
                            id="next-example"
                            class="code-button"
                            style=example_arrow_style
                            on:click=next_example title="Next example">{ ">" } </button>
                    </div>
                </div>
            </div>
            <div id="editor-help">
                { help.iter().map(|s| view! { cx, <p>{*s}</p> }).collect::<Vec<_>>() }
            </div>
        </div>
    }
}

/// Returns the output and the formatted code
fn run_code(code: &str) -> UiuaResult<String> {
    let (values, io) = Assembly::load_str(code)?.run_with_backend(WebBackend::default())?;
    let output = io.stdout;
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
    for val in values {
        s.push_str(&val.show());
        s.push('\n');
    }
    Ok(s)
}

fn element<T: JsCast>(id: &str) -> T {
    document()
        .get_element_by_id(id)
        .unwrap_throw()
        .dyn_into::<T>()
        .unwrap_throw()
}
