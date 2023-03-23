use leptos::*;
use uiua::{compile::Compiler, format::format_str, ops::Primitive, UiuaResult};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::{Event, HtmlDivElement, HtmlTextAreaElement};

#[derive(Debug, Default)]
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
) -> impl IntoView {
    let examples = if examples.is_empty() { &[""] } else { examples };
    let (_, default_output) = run_code(examples[0], false).expect_throw("First example failed");

    let (_, set_example) = create_signal(cx, 0);
    let (code, set_code) = create_signal(cx, examples[0].to_string());
    let (output, set_output) = create_signal(cx, default_output);

    // Run the code
    let run = move |format: bool| {
        set_output.set(String::new());
        let code_string = code_element().value();
        match run_code(&code_string, format) {
            Ok((formatted, stack)) => {
                code_element().set_value(&formatted);
                set_code.set(formatted);
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
            let class = format!(
                "glyph-button {}",
                if let Some(m) = p.modifier_args() {
                    if m == 1 {
                        "modifier1-button"
                    } else {
                        "modifier2-button"
                    }
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

    let code_class = match size {
        EditorSize::Small => "small-code",
        EditorSize::Medium => "medium-code",
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

    view! { cx,
        <div id="editor">
            <div id="glyph-buttons" style=glyph_buttons_style>{ glyph_buttons }</div>
            <div id="code-area">
                <button
                    id="glyphs-toggle-button"
                    title=show_glyphs_title
                    on:click=toggle_show_glyphs>{show_glyphs_text}</button>
                <textarea class={code_class} id="code" spellcheck="false" on:input=code_input>{ move || code.get() }</textarea>
            </div>
            <div id="output">
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

fn output_element() -> HtmlDivElement {
    element("output")
}
