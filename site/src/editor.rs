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
                    if !acc.is_empty() {
                        acc.insert(0, ' ');
                    }
                    acc.insert_str(0, s);
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

    let (initial_code, set_initial_code) = create_signal(
        cx,
        Some(examples.get(0).cloned().unwrap_or_else(|| example.into())),
    );

    let (example, set_example) = create_signal(cx, 0);
    let (output, set_output) = create_signal(cx, String::new());

    let code_text = move || code_text(&code_id.get());
    let get_code_cursor = move || get_code_cursor(&code_id.get());
    let set_code_cursor = move |start, end| set_code_cursor(&code_id.get(), start, end);
    let (copied_link, set_copied_link) = create_signal(cx, false);
    let (copied_code, set_copied_code) = create_signal(cx, false);

    let set_code_html = move |code: &str| {
        set_code_html(&code_id.get(), code);
        set_line_count.set(children_of(&code_element()).count());
        set_copied_link.set(false);
        set_copied_code.set(false);
    };

    // Run the code
    let run = move |format: bool| {
        // Get code
        let mut code_text = code_text();
        if let Some(code) = initial_code.get() {
            code_text = code;
            set_initial_code.set(None);
        }
        let range = get_code_cursor();

        // Format code
        let input = if format {
            if let Ok(formatted) = format_str(&code_text) {
                let formatted = formatted.trim();
                set_code_html(formatted);
                formatted.into()
            } else {
                set_code_html(&code_text);
                code_text
            }
        } else {
            set_code_html(&code_text);
            code_text
        };
        if let Some((start, end)) = range {
            set_code_cursor(start, end);
        }

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
            }
            Err(e) => {
                set_output.set(e.show(false));
                _ = output_element().style().set_property("color", "#f33");
            }
        }
    };

    // Replace the selected text in the editor with the given string
    let replace_code = move |inserted: &str| {
        if let Some((start, end)) = get_code_cursor() {
            let (start, end) = (start.min(end), start.max(end) as usize);
            let code = code_text();
            let text: String = code
                .chars()
                .take(start as usize)
                .chain(inserted.chars())
                .chain(code.chars().skip(end))
                .collect();
            set_code_html(&text);
            let offset = inserted.chars().count() as u32;
            set_code_cursor(start + offset, start + offset);
        };
    };

    // Remove a code range
    let remove_code = move |start: u32, end: u32| {
        if start == end {
            return;
        }
        let (start, end) = (start.min(end), start.max(end) as usize);
        let code = code_text();
        let text: String = code
            .chars()
            .take(start as usize)
            .chain(code.chars().skip(end))
            .collect();
        set_code_html(&text);
        set_code_cursor(start, start);
    };

    // Go to the next example
    let next_example = {
        let examples = examples.clone();
        move |_| {
            set_example.update(|e| {
                *e = (*e + 1) % examples.len();
                set_code_html(&examples[*e]);
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
                set_code_html(&examples[*e]);
                run(false);
            })
        }
    };

    // Handle key events
    window_event_listener(keydown, move |event| {
        let event = event.dyn_ref::<web_sys::KeyboardEvent>().unwrap();
        let focused = event
            .target()
            .and_then(|t| t.dyn_into::<HtmlDivElement>().ok())
            .is_some_and(|t| t.id() == code_id.get());
        if !focused {
            return;
        }
        let mut handled = true;
        match event.key().as_str() {
            "Enter" => {
                if event.ctrl_key() || event.shift_key() {
                    run(true);
                } else {
                    replace_code("\n");
                }
            }
            "Backspace" => {
                let (start, end) = get_code_cursor().unwrap();
                if start == end {
                    if start > 0 {
                        remove_code(start - 1, start);
                    }
                } else {
                    remove_code(start, end);
                }
            }
            "Delete" => {
                let (start, end) = get_code_cursor().unwrap();
                if start == end {
                    remove_code(start, start + 1);
                } else {
                    remove_code(start, end);
                }
            }
            "c" if event.ctrl_key() => {
                let (start, end) = get_code_cursor().unwrap();
                let (start, end) = (start.min(end), start.max(end));
                let code = code_text();
                let text: String = code
                    .chars()
                    .skip(start as usize)
                    .take((end - start) as usize)
                    .collect();
                log!("copy: {:?}", text);
                _ = window().navigator().clipboard().unwrap().write_text(&text);
            }
            "x" if event.ctrl_key() => {
                let (start, end) = get_code_cursor().unwrap();
                let (start, end) = (start.min(end), start.max(end));
                let code = code_text();
                let text: String = code
                    .chars()
                    .skip(start as usize)
                    .take((end - start) as usize)
                    .collect();
                log!("cut: {:?}", text);
                _ = window().navigator().clipboard().unwrap().write_text(&text);
                remove_code(start, end);
            }
            key => {
                log!("key: {:?}", key);
                handled = false;
            }
        }
        if handled {
            event.prevent_default();
            event.stop_propagation();
        }
    });

    // Handle paste evens
    let code_paste = move |event: Event| {
        let event = event.dyn_into::<web_sys::ClipboardEvent>().unwrap();
        event.prevent_default();
        event.stop_propagation();
        let text = event.clipboard_data().unwrap().get_data("text").unwrap();
        log!("paste: {:?}", text);
        replace_code(&text);
    };

    // Update the code when the textarea is changed
    let code_input = move |event: Event| {
        let event = event.dyn_into::<web_sys::InputEvent>().unwrap();
        let parent = code_element();
        let child: HtmlDivElement = event.target().unwrap().dyn_into().unwrap();
        if !parent.contains(Some(&child)) {
            return;
        }
        if let Some((start, _)) = get_code_cursor() {
            set_code_html(&code_text());
            set_code_cursor(start, start);
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

    // Copy a link to the code
    let copy_link = move |_| {
        let url = format!(
            "https://uiua.org/pad?src={}",
            urlencoding::encode(&code_text())
        );
        _ = window().navigator().clipboard().unwrap().write_text(&url);
        window()
            .history()
            .unwrap()
            .push_state_with_url(
                &JsValue::NULL,
                "",
                Some(&format!("/pad?src={}", urlencoding::encode(&code_text()))),
            )
            .unwrap();
        set_copied_link.set(true);
    };
    let copy_link_title = move || {
        if copied_link.get() {
            "Copied!"
        } else {
            "Copy a link to this code"
        }
    };

    // Copy the code
    let copy_code = move |_| {
        let code = code_text();
        _ = window().navigator().clipboard().unwrap().write_text(&code);
        set_copied_code.set(true);
    };
    let copy_code_title = move || {
        if copied_code.get() {
            "Copied!"
        } else {
            "Copy this code"
        }
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
                                class="editor-right-button"
                                title=copy_code_title
                                on:click=copy_code>
                                "📋"
                            </button>
                            <button
                                class="editor-right-button"
                                title=copy_link_title
                                on:click=copy_link>
                                "🔗"
                            </button>
                            <button
                                id="glyphs-toggle-button"
                                class="editor-right-button"
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
                                on:input=code_input
                                on:paste=code_paste>
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

fn children_of(node: &Node) -> impl Iterator<Item = Node> {
    let mut curr = node.first_child();
    iter::from_fn(move || {
        let node = curr.take()?;
        curr = node.next_sibling();
        Some(node)
    })
}

fn code_text(id: &str) -> String {
    let parent = element::<HtmlDivElement>(id);
    let mut text = String::new();
    for (i, div_node) in children_of(&parent).enumerate() {
        if i > 0 {
            text.push('\n');
        }
        for span_node in children_of(&div_node) {
            let fragment = span_node.text_content().unwrap();
            // log!("text fragment: {:?}", fragment);
            text.push_str(&fragment);
        }
    }
    // log!("code text: {:?}", text);

    text
}

fn get_code_cursor(id: &str) -> Option<(u32, u32)> {
    let parent = element::<HtmlDivElement>(id);
    let sel = window().get_selection().ok()??;
    let (anchor_node, anchor_offset) = (sel.anchor_node()?, sel.anchor_offset());
    let (focus_node, focus_offset) = (sel.focus_node()?, sel.focus_offset());
    if !parent.contains(Some(&anchor_node)) || !parent.contains(Some(&focus_node)) {
        return None;
    }
    let mut start = anchor_offset.min(focus_offset);
    let mut end = anchor_offset.max(focus_offset);
    let mut curr = 0;
    for (i, div_node) in children_of(&parent).enumerate() {
        if i > 0 {
            curr += 1;
        }
        for span_node in children_of(&div_node) {
            if span_node.contains(Some(&anchor_node)) {
                start = curr + anchor_offset;
            }
            if span_node.contains(Some(&focus_node)) {
                end = curr + focus_offset;
            }
            // Increment curr by the length of the text in the node
            let len = span_node.text_content().unwrap().chars().count() as u32;
            curr += len;
        }
    }
    // log!("get_code_cursor -> {:?}, {:?}", start, end);
    Some((start, end))
}

fn set_code_cursor(id: &str, mut start: u32, mut end: u32) {
    let elem = element::<HtmlDivElement>(id);
    // log!("set_code_cursor({}, {})", start, end);
    let code = code_text(id);
    let max_len = code.chars().count() as u32;
    start = start.min(max_len);
    end = end.min(max_len);

    let mut text_len = 0;
    let mut last_node = None;
    'divs: for (i, div_node) in children_of(&elem).enumerate() {
        if i > 0 {
            if start > 0 {
                start -= 1;
                end -= 1;
                text_len = 1;
                last_node = Some(div_node.first_child().unwrap());
            } else {
                break 'divs;
            }
        }
        for span_node in children_of(&div_node) {
            if let Some(node) = span_node.first_child() {
                text_len = node.text_content().unwrap().chars().count() as u32;
                last_node = Some(node);
            } else {
                text_len = 0;
                last_node = Some(span_node);
            }
            if start > text_len {
                start -= text_len;
                end -= text_len;
            } else {
                break 'divs;
            }
        }
    }
    if let Some(text_node) = last_node {
        // log!("ended on: {:?}", text_content);
        start = start.min(text_len);
        end = end.min(text_len);
        let range = document().create_range().unwrap();
        range.set_start(&text_node, start).unwrap();
        range.set_end(&text_node, end).unwrap();
        let sel = window().get_selection().unwrap().unwrap();
        sel.remove_all_ranges().unwrap();
        sel.add_range(&range).unwrap();
        elem.focus().unwrap();
        get_code_cursor(id);
    }
}

fn set_code_html(id: &str, code: &str) {
    use uiua::lsp::*;

    // log!("code_to_html: {:?}", code);

    let elem = element::<HtmlDivElement>(id);
    if code.is_empty() {
        elem.set_inner_html("<div class=\"code-line\"><br/></div>");
        return;
    }

    let mut html = "<div class=\"code-line\">".to_string();

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
                // log!("unspanned: {:?}", unspanned);
                html.push_str(&unspanned);
                unspanned.clear();
                // log!("newline");
                html.push_str("</span></div><div class=\"code-line\">");
                *curr += 1;
                while *curr < target && chars[*curr] == '\n' {
                    html.push_str("<br/></div><div class=\"code-line\">");
                    *curr += 1;
                }
                html.push_str("<span class=\"code-span\">");
                continue;
            }
            unspanned.push(chars[*curr]);
            *curr += 1;
        }
        // log!("unspanned: {:?}", unspanned);
        html.push_str(&unspanned);
        html.push_str("</span>");
    };

    let mut end = 0;
    for span in spans(code) {
        let kind = span.value;
        let span = span.span;
        push_unspanned(&mut html, span.start.char_pos, &mut end);

        let text: String = chars[span.start.char_pos..span.end.char_pos]
            .iter()
            .collect();
        // log!("spanned: {:?}", text);
        let color_class = match kind {
            SpanKind::Primitive(prim) => match prim.class() {
                PrimClass::Stack => "{}",
                _ => prim_class(prim),
            },
            SpanKind::Number => "number-literal-span",
            SpanKind::String => "string-literal-span",
            SpanKind::Comment => "comment-span",
        };

        html.push_str(&if let SpanKind::Primitive(prim) = kind {
            let name = prim.name().unwrap_or_default();
            if let Some(doc) = prim.doc() {
                let title = format!("{}: {}", name, doc.short);
                format!(
                    r#"<span 
                        class="code-span 
                        code-hover {color_class}" 
                        title={title:?}>{text}</span>"#
                )
            } else {
                format!(
                    r#"<span 
                        class="code-span code-hover {color_class}" 
                        title={name:?}>{text}</span>"#
                )
            }
        } else {
            format!(r#"<span class="code-span {color_class}">{text}</span>"#)
        });

        end = span.end.char_pos;
    }

    push_unspanned(&mut html, code.len(), &mut end);

    html.push_str("</div>");

    html = html
        .replace(
            "<div class=\"code-line\"><span class=\"code-span\"></span></div>",
            "<div class=\"code-line\"><br/></div>",
        )
        .replace("<span class=\"code-span\"></span>", "");

    // if html.is_empty() {
    //     html = "<div class=\"code-line\"><span class=\"code-span\"> </span></div>".to_string();
    // }

    elem.set_inner_html(&html);
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
