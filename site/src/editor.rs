use std::{iter, time::Duration};

use base64::{engine::general_purpose::STANDARD, Engine};
use image::ImageOutputFormat;
use leptos::{ev::keydown, *};
use uiua::{
    format::format_str,
    primitive::{PrimClass, Primitive},
    value::Value,
    value_to_image_bytes, value_to_wav_bytes, Uiua, UiuaResult,
};
use wasm_bindgen::JsCast;
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
    let code_height_em = code_max_lines as f32 * 1.2;

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

    // Track line count
    let (line_count, set_line_count) = create_signal(cx, 1);

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
        // Get code
        let code_elem = code_element();
        let mut code_string = code_elem.inner_text();
        if code_string == "<uninitialized>" {
            code_string = code.get();
        }
        let range = get_code_cursor(&code_elem);

        // Format code
        let input = if format {
            if let Ok(formatted) = format_str(&code_string) {
                let formatted = formatted.trim();
                code_elem.set_inner_html(&code_to_html(formatted, false));
                set_code.set(formatted.to_string());
                formatted.into()
            } else {
                code_elem.set_inner_html(&code_to_html(&code_string, false));
                code_string
            }
        } else {
            code_elem.set_inner_html(&code_to_html(&code_string, false));
            code_string
        };
        if let Some((start, end)) = range {
            set_code_cursor(&code_elem, start, end);
        }
        set_line_count.set(children_of(&code_elem).count());

        // Run code
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
        if let Some((start, end)) = get_code_cursor(&elem) {
            let (start, end) = (start.min(end), start.max(end) as usize);
            let text: String = code
                .get()
                .chars()
                .take(start as usize)
                .chain(inserted.chars())
                .chain(code.get().chars().skip(end))
                .collect();
            elem.set_inner_html(&code_to_html(&text, false));
            _ = elem.focus();
            set_code_cursor(&elem, start + 1, start + 1);
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
                code_element().set_inner_html(&code_to_html(&examples[*e], false));
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
                code_element().set_inner_html(&code_to_html(&examples[*e], false));
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
            event.prevent_default();
            event.stop_propagation();
            run(true);
        }
    });

    // Update the code when the textarea is changed
    let code_input = move |event: Event| {
        let event = event.dyn_into::<web_sys::InputEvent>().unwrap();
        let parent = code_element();
        let child: HtmlDivElement = event.target().unwrap().dyn_into().unwrap();
        if parent.contains(Some(&child)) {
            log!("text before update: {:?}", parent.inner_text());
            if let Some((start, _)) = get_code_cursor(&parent) {
                let text = parent.inner_text();
                let before_c = if start == 0 {
                    None
                } else {
                    text.chars().nth(start as usize - 1)
                };
                let after_c = text.chars().nth(start as usize);
                log!("chars at cursor: {:?}|{:?}", before_c, after_c);
                let ty = event.input_type();
                let smaller = matches!(ty.as_str(), "deleteContentBackward");
                parent.set_inner_html(&code_to_html(&text, smaller));
                match ty.as_str() {
                    "insertText" | "deleteContentForward" => {
                        set_code_cursor(&parent, start, start);
                    }
                    "deleteContentBackward" => {
                        let target = if before_c == Some('\n') {
                            start - 1
                        } else {
                            start
                        };
                        set_code_cursor(&parent, target, target);
                    }
                    "insertParagraph" => {
                        set_code_cursor(&parent, start + 1, start + 1);
                    }
                    ty => log!("Unhandled input type: {:?}", ty),
                }
            }
            set_line_count.set(children_of(&parent).count());
            set_code.set(parent.inner_text());
        }
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
    let editor_class = match size {
        EditorSize::Small => "small-editor",
        EditorSize::Medium | EditorSize::Pad => "medium-editor",
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

    // Line numbers
    let line_numbers = move |cx| {
        (0..line_count.get().max(1))
            .map(|i| {
                view!(cx, <div>
                    <span class="code-span line-number">{i + 1}</span>
                </div>)
            })
            .collect::<Vec<_>>()
    };

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
                        <div class="code">
                            <div class="line-numbers">
                                { move || line_numbers(cx) }
                            </div>
                            // The text entry area
                            <div
                                id={code_id.get()}
                                contenteditable="true"
                                spellcheck="false"
                                class="code-entry"
                                style={format!("height: {code_height_em}em;")}
                                on:input=code_input>
                                "<uninitialized>"
                            </div>
                        </div>
                    </div>
                    <div class="output-frame">
                        <div id={output_id.get()} class="output">
                            <div id="output-text">{ move || output.get() }</div>
                            <img id=move || image_id.get() style="border-radius: 1em;" src=""/>
                            <audio id=move || audio_id.get() src="" style="display:none" controls autoplay/>
                        </div>
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

fn get_code_cursor(parent: &HtmlDivElement) -> Option<(u32, u32)> {
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
        for node in children_of(parent).flat_map(|n| children_of(&n)) {
            if node.contains(Some(&anchor_node)) {
                start = Some(curr + anchor_offset);
            }
            if node.contains(Some(&focus_node)) {
                end = Some(curr + focus_offset);
            }
            // Increment curr by the length of the text in the node
            let len = node.text_content().unwrap().chars().count() as u32;
            curr += len;
        }
        let (start, end) = (start.min(end), start.max(end));
        start.zip(end)
    }
}

fn set_code_cursor(elem: &HtmlDivElement, mut start: u32, mut end: u32) {
    log!("set_code_cursor({}, {})", start, end);
    log!("element inner text: {:?}", elem.inner_text());
    let max_len = elem.inner_text().chars().count() as u32;
    start = start.min(max_len);
    end = end.min(max_len);

    let nodes: Vec<Node> = children_of(elem)
        .flat_map(|n| children_of(&n))
        .flat_map(|n| children_of(&n))
        .collect();

    let node_count = nodes.len();
    for (i, node) in nodes.into_iter().enumerate() {
        let text_content = node.text_content().unwrap();
        let text_len = text_content.chars().count() as u32;
        if start >= text_len && i != node_count - 1 {
            start -= text_len;
            end -= text_len;
        } else {
            log!("ended on: {:?}", text_content);
            start = start.min(text_len);
            end = end.min(text_len);
            log!("start: {}, end: {}", start, end);
            let range = document().create_range().unwrap();
            range.set_start(&node, start).unwrap();
            range.set_end(&node, end).unwrap();
            let sel = window().get_selection().unwrap().unwrap();
            sel.remove_all_ranges().unwrap();
            sel.add_range(&range).unwrap();
            return;
        }
    }
}

fn code_to_html(code: &str, smaller: bool) -> String {
    use uiua::{lex::Span, lsp::*};

    log!("code_to_html: {:?}", code);

    let mut html = "<div>".to_string();

    let chars: Vec<char> = code.chars().collect();

    let push_unspanned = |html: &mut String, mut target: usize, curr: &mut usize| {
        target = target.min(chars.len());
        if *curr >= target {
            return;
        }
        html.push_str(r#"<span class="code-span">"#);
        let mut unspanned = String::new();
        while *curr < target {
            if chars[*curr] == '\n' {
                html.push_str(&unspanned);
                unspanned.clear();
                html.push_str("</span></div><div><span class=\"code-span\">");
                *curr += 1;
                let mut newline_count: usize = !smaller as usize;
                while *curr < target && chars[*curr] == '\n' {
                    newline_count += 1;
                    *curr += 1;
                }
                for _ in 0..newline_count / 2 {
                    html.push_str("\n</span></div><div><span class=\"code-span\">");
                }
                continue;
            }
            unspanned.push(chars[*curr]);
            *curr += 1;
        }
        html.push_str(&unspanned);
        html.push_str("</span>");
    };

    let mut end = 0;
    for span in spans(code) {
        let kind = span.value;
        if let Span::Code(span) = span.span {
            push_unspanned(&mut html, span.start.pos, &mut end);

            let text: String = chars[span.start.pos..span.end.pos].iter().collect();
            let color_class = match kind {
                SpanKind::Primitive(prim) => match prim.class() {
                    PrimClass::Stack => "{}",
                    _ => prim_class(prim),
                },
                SpanKind::Number => "number-literal-span",
                SpanKind::String => "string-literal-span",
                SpanKind::Comment => "comment-span",
            };
            html.push_str(&format!(
                r#"<span class="code-span {color_class}">{text}</span>"#,
            ));

            end = span.end.pos;
        } else {
            unreachable!("parsed span is not a code span")
        }
    }

    push_unspanned(&mut html, code.len(), &mut end);

    html.push_str("</div>");

    html = html
        .replace("<span class=\"code-span\"></span>", "")
        .replace("<div></div>", "");
    if html.is_empty() {
        html = "<div><span class=\"code-span\"> </span></div>".to_string();
    }
    html
}
