use std::{rc::Rc, time::Duration};

use base64::{engine::general_purpose::STANDARD, Engine};
use image::ImageOutputFormat;
use leptos::{ev::keydown, *};
use uiua::{
    format::format_str, primitive::Primitive, value::Value, value_to_image_bytes,
    value_to_wav_bytes, Uiua, UiuaResult,
};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::{Event, HtmlAudioElement, HtmlDivElement, HtmlImageElement, HtmlTextAreaElement};

use crate::{backend::WebBackend, prim_class};

#[derive(Debug, Clone, Copy, Default)]
pub enum EditorSize {
    #[default]
    Small,
    Medium,
    Pad,
}

#[component]
pub fn Editor(
    cx: Scope,
    #[prop(optional)] examples: &'static [&'static str],
    #[prop(optional)] size: EditorSize,
    #[prop(optional)] help: &'static [&'static str],
    #[prop(optional)] progressive: bool,
) -> impl IntoView {
    let id = format!("{:?}", cx.id);
    let examples: Vec<String> = if examples.is_empty() {
        vec![String::new()]
    } else if progressive {
        examples
            .iter()
            .map(|s| s.to_string())
            .scan(String::new(), |acc, s| {
                *acc = format!("{s} {acc}");
                Some(format_str(acc).unwrap_or_else(|_| acc.clone()))
            })
            .collect()
    } else {
        examples.iter().map(|s| s.to_string()).collect()
    };
    let code_max_lines = if let EditorSize::Pad = size {
        10
    } else {
        examples.iter().map(|e| e.lines().count()).max().unwrap()
    };
    let code_height_em = code_max_lines as f32 * 1.2 + 0.6;

    let (code_id, _) = create_signal(cx, format!("code{id}"));
    let (output_id, _) = create_signal(cx, format!("output{id}"));
    let (image_id, _) = create_signal(cx, format!("image{id}"));
    let (audio_id, _) = create_signal(cx, format!("audio{id}"));
    let (glyph_doc_id, _) = create_signal(cx, format!("glyphdoc{id}"));

    let code_element = move || -> HtmlTextAreaElement { element(&code_id.get()) };
    let output_element = move || -> HtmlDivElement { element(&output_id.get()) };
    let image_element = move || -> HtmlImageElement { element(&image_id.get()) };
    let audio_element = move || -> HtmlAudioElement { element(&audio_id.get()) };
    let glyph_doc_element = move || -> HtmlDivElement { element(&glyph_doc_id.get()) };

    let get_saved_code = || {
        window()
            .local_storage()
            .ok()
            .flatten()
            .and_then(|storage| storage.get_item("code").ok().flatten())
    };
    let code = (matches!(size, EditorSize::Pad))
        .then(get_saved_code)
        .flatten()
        .unwrap_or_else(|| examples[0].to_string());

    let (example, set_example) = create_signal(cx, 0);
    let (code, set_code) = create_signal(cx, code);
    let (output, set_output) = create_signal(cx, String::new());

    // Run the code
    let run = move |format: bool| {
        set_output.set(String::new());
        let code_elem = code_element();
        let code_string = code_elem.value();
        let input = if format {
            if let Ok(formatted) = format_str(&code_string) {
                let formatted = formatted.trim();
                let range = (code_elem.selection_start(), code_elem.selection_end());
                code_elem.set_value(formatted);
                if let (Ok(Some(start)), Ok(Some(end))) = range {
                    _ = code_elem.set_selection_range(start, end);
                }
                set_code.set(formatted.to_string());
                formatted.into()
            } else {
                code_string
            }
        } else {
            code_string
        };
        match run_code(&input) {
            Ok(output) => {
                // Show text output
                set_output.set(output.text());
                _ = output_element().style().remove_property("color");
                // Show image output
                if let Some(image_bytes) = output.image_bytes {
                    _ = image_element().style().remove_property("display");
                    let encoded = STANDARD.encode(image_bytes);
                    image_element().set_src(&format!("data:image/png;base64,{encoded}"));
                } else {
                    _ = image_element().style().set_property("display", "none");
                    image_element().set_src("");
                }
                // Show audio output
                if let Some(audio_bytes) = output.audio_bytes {
                    _ = audio_element().style().remove_property("display");
                    let encoded = STANDARD.encode(audio_bytes);

                    audio_element().set_src(&format!("data:audio/wav;base64,{encoded}"));
                } else {
                    _ = audio_element().style().set_property("display", "none");
                    audio_element().set_src("");
                }
                // Save the code for Pad editor
                if let EditorSize::Pad = size {
                    if let Ok(Some(storage)) = window().local_storage() {
                        _ = storage.set_item("code", &input);
                    }
                }
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
    let next_example = {
        let examples = examples.clone();
        move |_| {
            set_example.update(|e| {
                *e = (*e + 1) % examples.len();
                set_code.set(examples[*e].to_string());
                code_element().set_value(&examples[*e]);
                run(false);
            })
        }
    };
    // Go to the previous example
    let prev_example = {
        let examples = examples.clone();
        move |_| {
            set_example.update(|e| {
                *e = (*e + examples.len() - 1) % examples.len();
                set_code.set(examples[*e].to_string());
                code_element().set_value(&examples[*e]);
                run(false);
            })
        }
    };

    // Run the code when Ctrl+Enter or Shift+Enter is pressed
    window_event_listener(keydown, move |event| {
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
    let (glyph_doc, set_glyph_doc) = create_signal(cx, "");
    let onmouseleave = move |_| {
        _ = glyph_doc_element().style().set_property("display", "none");
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
            let title = format!("{}\n{}", p.name().unwrap_or_default(), extra);
            let onclick = move |_| replace_code(&p.to_string());
            let onmouseover = move |_| {
                if let Some(doc) = p.doc() {
                    set_glyph_doc.set(doc);
                    _ = glyph_doc_element().style().remove_property("display");
                }
            };
            let class = format!("glyph-button {}", prim_class(*p));
            Some(view! { cx,
                <button
                    class=class
                    title=title
                    on:click=onclick
                    on:mouseover=onmouseover
                    on:mouseleave=onmouseleave>{ text }</button>
            })
        })
        .collect();

    for (glyph, title) in [
        ("_", "strand"),
        ("[]", "array"),
        ("()", "function"),
        ("{}", "dfn"),
        ("¯", "negative\n`"),
        ("'", "character"),
        ("\"", "string"),
        ("←", "binding"),
        ("#", "comment"),
    ] {
        let onclick = move |_| replace_code(glyph);
        glyph_buttons.push(view! { cx,
            <button class="glyph-button" title=title on:click=onclick>{glyph}</button>
        });
    }

    let (editor_class, code_class) = match size {
        EditorSize::Small => ("small-editor", "small-code"),
        EditorSize::Medium | EditorSize::Pad => ("medium-editor", "medium-code"),
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
            EditorSize::Medium | EditorSize::Pad => true,
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

    let examples_len = examples.len();
    let example_text =
        move || (examples_len > 1).then(|| format!("{}/{}", example.get() + 1, examples_len));

    let next_button_class = move || {
        if example.get() == examples_len - 1 {
            "code-button"
        } else {
            "code-button important-button"
        }
    };

    // This ensures the output of the first example is shown
    set_timeout(move || run(false), Duration::from_millis(0));

    view! { cx,
        <div>
            <div id="editor" class=editor_class>
                <div style=glyph_buttons_style>
                    <div class="glyph-buttons">{glyph_buttons}</div>
                </div>
                <div id="code-area">
                    <div id="code-right-side">
                        <button
                            id="glyphs-toggle-button"
                            title=show_glyphs_title
                            on:click=toggle_show_glyphs>{show_glyphs_text}</button>
                        <div id="example-tracker">{example_text}</div>
                    </div>
                    <div id={glyph_doc_id.get()} class="glyph-doc" style="display: none">{move || glyph_doc.get()}</div>
                    <textarea
                        id={code_id.get()}
                        spellcheck="false"
                        class={format!("code {code_class}")}
                        style={format!("height: {code_height_em}em")}
                        on:input=code_input>{ move || code.get() }</textarea>
                </div>
                <div id={output_id.get()} class="output">
                    <div id="output-text">{ move || output.get() }</div>
                    <div id="code-buttons">
                        <button id="run-button" class="code-button" on:click=move |_| run(true)>{ "Run" }</button>
                        <button
                            id="prev-example"
                            class="code-button"
                            style=example_arrow_style
                            on:click=prev_example title="Previous example">{ "<" } </button>
                        <button
                            id="next-example"
                            class=next_button_class
                            style=example_arrow_style
                            on:click=next_example title="Next example">{ ">" } </button>
                    </div>
                </div>
                <div>
                    <img id=move || image_id.get() src=""/>
                    <audio id=move || audio_id.get() src="" controls autoplay/>
                </div>
            </div>
            <div id="editor-help">
                { help.iter().map(|s| view! { cx, <p>{*s}</p> }).collect::<Vec<_>>() }
            </div>
        </div>
    }
}

struct RunOutput {
    stdout: String,
    stack: Vec<Rc<Value>>,
    image_bytes: Option<Vec<u8>>,
    audio_bytes: Option<Vec<u8>>,
}

impl RunOutput {
    fn text(&self) -> String {
        let mut s = String::new();
        if !self.stdout.is_empty() {
            if !self.stack.is_empty() {
                s.push_str("Output:\n");
            }
            s.push_str(&self.stdout);
            if !self.stack.is_empty() {
                s.push_str("\n\nStack:\n");
            }
        }
        for val in &self.stack {
            s.push_str(&val.show());
            s.push('\n');
        }
        s
    }
}

/// Returns the output and the formatted code
fn run_code(code: &str) -> UiuaResult<RunOutput> {
    let io = WebBackend::default();
    let mut values = Uiua::with_backend(&io).load_str(code)?.take_stack();
    let image_bytes = io.image_bytes.into_inner().or_else(|| {
        for i in 0..values.len() {
            let value = &values[i];
            if let Ok(bytes) = value_to_image_bytes(value, ImageOutputFormat::Png) {
                if value.shape().iter().product::<usize>() > 100 {
                    values.remove(i);
                    return Some(bytes);
                }
            }
        }
        None
    });
    let audio_bytes = io.audio_bytes.into_inner().or_else(|| {
        for i in 0..values.len() {
            let value = &values[i];
            if let Ok(bytes) = value_to_wav_bytes(value) {
                if value.len() > 1000 {
                    values.remove(i);
                    return Some(bytes);
                }
            }
        }
        None
    });
    Ok(RunOutput {
        stdout: io.stdout.into_inner(),
        stack: values,
        image_bytes,
        audio_bytes,
    })
}

fn element<T: JsCast>(id: &str) -> T {
    document()
        .get_element_by_id(id)
        .unwrap_throw()
        .dyn_into::<T>()
        .unwrap_throw()
}
