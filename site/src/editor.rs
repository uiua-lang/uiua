use std::{iter, time::Duration};

use base64::{engine::general_purpose::STANDARD, Engine};
use image::ImageOutputFormat;
use leptos::{ev::keydown, *};
use uiua::{
    format::format_str, primitive::Primitive, value::Value, value_to_image_bytes,
    value_to_wav_bytes, Uiua, UiuaResult,
};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Event, HtmlAudioElement, HtmlDivElement, HtmlImageElement, MouseEvent, Node};

use crate::{backend::WebBackend, element, prim_class};

#[derive(Debug, Clone, Copy, Default)]
pub enum EditorSize {
    #[default]
    Small,
    Medium,
    Pad,
}

#[derive(Default)]
pub enum EditorMode {
    #[default]
    Progressive,
    Multiple,
}

#[component]
#[allow(clippy::needless_lifetimes)]
pub fn Editor<'a>(
    cx: Scope,
    #[prop(optional)] example: &'a str,
    #[prop(optional)] examples: &'a [&'a str],
    #[prop(optional)] size: EditorSize,
    #[prop(optional)] help: &'static [&'static str],
    #[prop(optional)] mode: EditorMode,
) -> impl IntoView {
    let id = format!("{:?}", cx.id);
    let examples = Some(example)
        .filter(|ex| !ex.is_empty() || examples.is_empty())
        .into_iter()
        .chain(examples.iter().copied());
    let examples: Vec<String> = match mode {
        EditorMode::Progressive => {
            let mut examples: Vec<_> = examples
                .scan(String::new(), |acc, s| {
                    *acc = format!("{s} {acc}");
                    Some(acc.clone())
                })
                .collect();
            examples.rotate_right(1);
            examples
        }
        EditorMode::Multiple => examples.map(Into::into).collect(),
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

    let code_element = move || -> HtmlDivElement { element(&code_id.get()) };
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
        .unwrap_or_else(|| examples.get(0).cloned().unwrap_or_else(|| example.into()));

    let (example, set_example) = create_signal(cx, 0);
    let (code, set_code) = create_signal(cx, code);
    let (output, set_output) = create_signal(cx, String::new());

    // Run the code
    let run = move |format: bool| {
        set_output.set(String::new());
        let code_elem = code_element();
        let mut code_string = code_elem.inner_text();
        if code_string == "<uninitialized>" {
            code_string = code.get();
        }
        let input = if format {
            if let Ok(formatted) = format_str(&code_string) {
                let formatted = formatted.trim();
                let range = get_cursor_position(&code_elem);
                code_elem.set_inner_html(&code_to_html(formatted));
                if let Some((start, end)) = range {
                    _ = set_selection_range(&code_elem, start, end);
                }
                set_code.set(formatted.to_string());
                formatted.into()
            } else {
                code_elem.set_inner_html(&code_to_html(&code_string));
                code_string
            }
        } else {
            code_elem.set_inner_html(&code_to_html(&code_string));
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
                set_output.set(e.show(false));
                _ = output_element().style().set_property("color", "#f33");
            }
        }
    };

    // Replace the selected text in the editor with the given string
    let replace_code = move |inserted: &str| {
        let elem = code_element();
        if let Some((start, end)) = get_cursor_position(&elem) {
            let (start, end) = (start.min(end), start.max(end) as usize);
            let text: String = code
                .get()
                .chars()
                .take(start as usize)
                .chain(inserted.chars())
                .chain(code.get().chars().skip(end))
                .collect();
            elem.set_inner_html(&code_to_html(&text));
            _ = elem.focus();
            _ = set_selection_range(&elem, start + 1, start + 1);
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
                code_element().set_inner_html(&code_to_html(&examples[*e]));
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
                code_element().set_inner_html(&code_to_html(&examples[*e]));
                run(false);
            })
        }
    };

    // Run the code when Ctrl+Enter or Shift+Enter is pressed
    window_event_listener(keydown, move |event| {
        let event = event.dyn_ref::<web_sys::KeyboardEvent>().unwrap();
        let focused = || {
            event
                .target()
                .and_then(|t| t.dyn_into::<HtmlDivElement>().ok())
                .is_some_and(|t| t.id() == code_id.get())
        };
        if event.key() == "Enter" && (event.ctrl_key() || event.shift_key()) && focused() {
            run(true);
        }
    });

    // Update the code when the textarea is changed
    let code_input = move |event: Event| {
        let text_area: HtmlDivElement = event.target().unwrap().dyn_into().unwrap();
        set_code.set(text_area.inner_text());
    };

    // Glyph hover doc
    let (glyph_doc, set_glyph_doc) = create_signal(cx, String::new());
    let onmouseleave = move |_| {
        _ = glyph_doc_element().style().set_property("display", "none");
    };

    // Glyph buttons
    let mut glyph_buttons: Vec<_> = Primitive::all()
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
            let onclick = move |event: MouseEvent| {
                if event.ctrl_key() {
                    // Redirect to the docs page
                    _ = window()
                        .location()
                        .set_href(&format!("/docs/{p:?}").to_lowercase());
                } else {
                    replace_code(&p.to_string());
                }
            };
            let onmouseover = move |_| {
                if let Some(doc) = p.doc() {
                    set_glyph_doc.set(doc.short.clone());
                    _ = glyph_doc_element().style().remove_property("display");
                }
            };
            let class = format!("glyph-button glyph-title {}", prim_class(p));
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

    // Additional code buttons
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

    // Select a class for the editor and code area
    let (editor_class, code_class) = match size {
        EditorSize::Small => ("small-editor", "small-code"),
        EditorSize::Medium | EditorSize::Pad => ("medium-editor", "medium-code"),
    };

    // Hide the example arrows if there is only one example
    let example_arrow_style = if examples.len() <= 1 {
        "display:none"
    } else {
        ""
    };

    // Show or hide the glyph buttons
    let (show_glyphs, set_show_glyphs) = create_signal(
        cx,
        match size {
            EditorSize::Small => false,
            EditorSize::Medium | EditorSize::Pad => true,
        },
    );

    // Glyphs toggle button
    let show_glyphs_text = move || if show_glyphs.get() { "↥" } else { "↧" };
    let show_glyphs_title = move || {
        if show_glyphs.get() {
            "Hide glyphs"
        } else {
            "Show glyphs"
        }
    };
    let toggle_show_glyphs = move |_| set_show_glyphs.update(|s| *s = !*s);

    // Hide the glyph buttons if the editor is small
    let glyph_buttons_style = move || {
        if show_glyphs.get() {
            ""
        } else {
            "display:none"
        }
    };

    // Show the example number if there are multiple examples
    let examples_len = examples.len();
    let example_text =
        move || (examples_len > 1).then(|| format!("{}/{}", example.get() + 1, examples_len));

    // Select a class for the next example button
    let next_button_class = move || {
        if example.get() == examples_len - 1 {
            "code-button"
        } else {
            "code-button important-button"
        }
    };

    // This ensures the output of the first example is shown
    set_timeout(move || run(false), Duration::from_millis(0));

    // Render
    view! { cx,
        <div id="editor-wrapper">
            <div id="editor">
                <div style=glyph_buttons_style>
                    <div class="glyph-buttons">{glyph_buttons}</div>
                </div>
                <div class=editor_class>
                    <div id="code-area">
                        <div id="code-right-side">
                            <button
                                id="glyphs-toggle-button"
                                title=show_glyphs_title
                                on:click=toggle_show_glyphs>{show_glyphs_text}</button>
                            <div id="example-tracker">{example_text}</div>
                        </div>
                        <div id={glyph_doc_id.get()} class="glyph-doc" style="display: none">
                            { move || glyph_doc.get() }
                            <div class="glyph-doc-ctrl-click">"Ctrl+click for more info"</div>
                        </div>
                        // The text area
                        <div
                            id={code_id.get()}
                            contenteditable="true"
                            spellcheck="false"
                            class={format!("code {code_class}")}
                            style={format!("height: {code_height_em}em")}
                            on:input=code_input>
                            "<uninitialized>"
                        </div>
                    </div>
                    <div id={output_id.get()} class="output">
                        <div id="output-text">{ move || output.get() }</div>
                        <div id="code-buttons">
                            <button class="code-button" on:click=move |_| run(true)>{ "Run" }</button>
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
                </div>
                <div>
                    <img id=move || image_id.get() src=""/>
                    <audio id=move || audio_id.get() src="" style="display:none" controls autoplay/>
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
    stderr: String,
    stack: Vec<Value>,
    image_bytes: Option<Vec<u8>>,
    audio_bytes: Option<Vec<u8>>,
}

impl RunOutput {
    fn text(&self) -> String {
        let mut s = String::new();
        let groups = (!self.stack.is_empty()) as u8
            + (!self.stdout.is_empty()) as u8
            + (!self.stderr.is_empty()) as u8;
        if !self.stdout.is_empty() {
            if groups >= 2 {
                s.push_str("stdout:\n");
            }
            s.push_str(&self.stdout);
            if !self.stdout.ends_with('\n') {
                s.push('\n');
            }
        }
        if !self.stderr.is_empty() {
            if groups >= 2 {
                if !s.is_empty() {
                    s.push('\n');
                }
                s.push_str("stderr:\n");
            }
            s.push_str(&self.stderr);
            if !self.stderr.ends_with('\n') {
                s.push('\n');
            }
        }
        if !self.stack.is_empty() {
            if groups >= 2 {
                if !s.is_empty() {
                    s.push('\n');
                }
                s.push_str("stack:\n");
            }
            for val in &self.stack {
                s.push_str(&val.show());
                s.push('\n');
            }
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
        stderr: io.stderr.into_inner(),
        stack: values,
        image_bytes,
        audio_bytes,
    })
}

fn children_of(node: &Node) -> impl Iterator<Item = Node> {
    let mut curr = node.first_child();
    iter::from_fn(move || {
        let node = curr.take()?;
        curr = node.next_sibling();
        Some(node)
    })
}

fn get_cursor_position(parent: &HtmlDivElement) -> Option<(u32, u32)> {
    let sel = window().get_selection().ok()??;
    let (anchor_node, anchor_offset) = (sel.anchor_node()?, sel.anchor_offset());
    let (focus_node, focus_offset) = (sel.focus_node()?, sel.focus_offset());
    if anchor_node == ****parent {
        Some((
            anchor_offset.min(focus_offset),
            anchor_offset.max(focus_offset),
        ))
    } else if !parent.contains(Some(&anchor_node)) || !parent.contains(Some(&focus_node)) {
        None
    } else {
        let mut start = None;
        let mut end = None;
        let mut curr = 0;
        for node in children_of(parent) {
            if node.contains(Some(&anchor_node)) {
                start = Some(curr + anchor_offset);
            }
            if node.contains(Some(&focus_node)) {
                end = Some(curr + focus_offset);
            }
            // Increment curr by the length of the text in the node
            let elem = node.dyn_ref::<HtmlDivElement>().unwrap();
            let len = elem.inner_text().chars().count() as u32;
            curr += len + 1;
        }
        let (start, end) = (start.min(end), start.max(end));
        start.zip(end)
    }
}

fn terminal_child(node: &Node) -> Option<Node> {
    let mut curr = node.first_child()?;
    while let Some(next) = curr.first_child() {
        curr = next;
    }
    Some(curr)
}

fn set_selection_range(elem: &HtmlDivElement, mut start: u32, mut end: u32) -> Result<(), JsValue> {
    for node in children_of(elem) {
        let elem = node.dyn_ref::<HtmlDivElement>().unwrap();
        let text_len = elem.inner_text().chars().count() as u32 + 1;
        if start >= text_len {
            start -= text_len;
            end -= text_len;
        } else {
            start = start.min(text_len);
            end = end.min(text_len);
            let range = document().create_range()?;
            range.set_start(&terminal_child(&node).unwrap(), start)?;
            range.set_end(&terminal_child(&node).unwrap(), end)?;
            let sel = window().get_selection()?.unwrap();
            sel.remove_all_ranges()?;
            sel.add_range(&range)?;
            break;
        }
    }
    Ok(())
}

fn code_to_html(code: &str) -> String {
    code.lines()
        .map(|l| {
            format!(
                r#"<div>
    <span class="code-span">{l}</span>
</div>"#,
            )
        })
        .collect()
}
