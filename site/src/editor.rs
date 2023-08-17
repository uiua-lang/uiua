use std::{
    cell::{Cell, RefCell},
    iter,
    mem::{replace, take},
    rc::Rc,
    time::Duration,
};

use base64::{engine::general_purpose::STANDARD, Engine};
use image::ImageOutputFormat;
use leptos::{ev::keydown, *};
use leptos_router::{use_navigate, NavigateOptions};
use uiua::{
    format::{format_str, FormatConfig},
    primitive::{PrimClass, Primitive},
    run::RunMode,
    value_to_image_bytes, value_to_wav_bytes, Uiua,
};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Event, HtmlDivElement, MouseEvent, Node};

use crate::{
    backend::{OutputItem, WebBackend},
    element, prim_class,
};

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

thread_local! {
    static ID: Cell<u64> = Cell::new(0);
}

#[component]
#[allow(clippy::needless_lifetimes)]
pub fn Editor<'a>(
    #[prop(optional)] example: &'a str,
    #[prop(optional)] examples: &'a [&'a str],
    #[prop(optional)] size: EditorSize,
    #[prop(optional)] help: &'static [&'static str],
    #[prop(optional)] mode: EditorMode,
    #[prop(optional)] progress_lines: bool,
) -> impl IntoView {
    let id = ID.with(|id| {
        let i = id.get();
        id.set(i + 1);
        i
    });
    let examples = Some(example)
        .filter(|ex| !ex.is_empty() || examples.is_empty())
        .into_iter()
        .chain(examples.iter().copied());
    let examples: Vec<String> = match mode {
        EditorMode::Progressive => {
            let mut examples: Vec<_> = if progress_lines {
                examples
                    .scan(String::new(), |acc, s| {
                        if !acc.is_empty() {
                            acc.push('\n');
                        }
                        acc.push_str(s);
                        Some(acc.clone())
                    })
                    .collect()
            } else {
                examples
                    .rev()
                    .scan(String::new(), |acc, s| {
                        acc.insert_str(0, s);
                        Some(acc.clone())
                    })
                    .collect()
            };
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

    let code_id = move || format!("code{id}");
    let glyph_doc_id = move || format!("glyphdoc{id}");

    let code_element = move || -> HtmlDivElement { element(&code_id()) };
    let glyph_doc_element = move || -> HtmlDivElement { element(&glyph_doc_id()) };

    // Track line count
    let (line_count, set_line_count) = create_signal(1);

    let (initial_code, set_initial_code) = create_signal(Some(
        examples.get(0).cloned().unwrap_or_else(|| example.into()),
    ));

    let (example, set_example) = create_signal(0);
    let (output, set_output) = create_signal(View::default());

    let code_text = move || code_text(&code_id());
    let get_code_cursor = move || get_code_cursor_impl(&code_id());
    let (copied_link, set_copied_link) = create_signal(false);
    let (copied_code, set_copied_code) = create_signal(false);

    /// Handles set the code in the editor, setting the cursor, and managing the history
    struct State {
        code_id: String,
        set_line_count: WriteSignal<usize>,
        set_copied_link: WriteSignal<bool>,
        set_copied_code: WriteSignal<bool>,
        past: RefCell<Vec<Record>>,
        future: RefCell<Vec<Record>>,
        curr: RefCell<Record>,
    }

    #[derive(Debug)]
    struct Record {
        code: String,
        before: (u32, u32),
        after: (u32, u32),
    }

    #[derive(Clone, Copy)]
    enum Cursor {
        Set(u32, u32),
        Keep,
        Ignore,
    }

    impl State {
        fn set_code(&self, code: &str, cursor: Cursor) {
            let after = match cursor {
                Cursor::Set(start, end) => (start, end),
                Cursor::Keep => get_code_cursor_impl(&self.code_id).unwrap_or_else(|| {
                    let len = code.chars().count() as u32;
                    (len, len)
                }),
                Cursor::Ignore => {
                    let len = code.chars().count() as u32;
                    (len, len)
                }
            };
            let maybe_before = get_code_cursor_impl(&self.code_id);
            let before = maybe_before
                .or_else(|| self.past.borrow().last().map(|r| r.after))
                .unwrap_or(after);
            let new_curr = Record {
                code: code.into(),
                before,
                after,
            };
            let prev = replace(&mut *self.curr.borrow_mut(), new_curr);
            let changed = prev.code != code;
            if changed {
                self.past.borrow_mut().push(prev);
                self.future.borrow_mut().clear();
            }
            set_code_html(&self.code_id, code);
            if matches!(cursor, Cursor::Ignore) {
                if let Some(before) = maybe_before {
                    self.set_cursor(before);
                }
            } else {
                self.set_cursor(after);
            }
            if changed {
                self.set_changed();
            } else {
                self.set_line_count();
            }
        }
        fn set_cursor(&self, to: (u32, u32)) {
            set_code_cursor(&self.code_id, to.0, to.1);
        }
        fn set_code_html(&self, code: &str) {
            set_code_html(&self.code_id, code);
        }
        fn set_changed(&self) {
            self.set_copied_link.set(false);
            self.set_copied_code.set(false);
            self.set_line_count();
        }
        fn set_line_count(&self) {
            self.set_line_count
                .set(children_of(&element(&self.code_id)).count());
        }
        fn clear_history(&self) {
            self.past.borrow_mut().clear();
            self.future.borrow_mut().clear();
        }
        fn undo(&self) {
            let prev = self.past.borrow_mut().pop();
            if let Some(prev) = prev {
                self.set_code_html(&prev.code);
                let mut curr = self.curr.borrow_mut();
                self.set_cursor(curr.before);
                self.future.borrow_mut().push(replace(&mut *curr, prev));
                self.set_changed();
            }
        }
        fn redo(&self) {
            if let Some(next) = self.future.borrow_mut().pop() {
                self.set_code_html(&next.code);
                let mut curr = self.curr.borrow_mut();
                self.set_cursor(next.after);
                self.past.borrow_mut().push(replace(&mut *curr, next));
                self.set_changed();
            }
        }
    }

    let state = Rc::new(State {
        code_id: code_id(),
        set_line_count,
        set_copied_link,
        set_copied_code,
        past: Default::default(),
        future: Default::default(),
        curr: {
            let code = initial_code.get_untracked().unwrap();
            let len = code.chars().count() as u32;
            Record {
                code,
                before: (len, len),
                after: (len, len),
            }
        }
        .into(),
    });
    let (state, _) = create_signal(state);
    let state = move || state.get();

    // Run the code
    let run = move |format: bool, set_cursor: bool| {
        // Get code
        let mut code_text = code_text();
        let mut cursor = if set_cursor {
            Cursor::Keep
        } else {
            Cursor::Ignore
        };
        if let Some(code) = initial_code.get() {
            code_text = code;
            set_initial_code.set(None);
            cursor = Cursor::Ignore;
        }

        // Format code
        let input = if format {
            if let Ok(formatted) = format_str(
                &code_text,
                &FormatConfig {
                    trailing_newline: false,
                    ..Default::default()
                },
            ) {
                state().set_code(&formatted, cursor);
                formatted
            } else {
                state().set_code(&code_text, cursor);
                code_text
            }
        } else {
            state().set_code(&code_text, cursor);
            code_text
        };

        // Run code
        let output = run_code(&input);
        let mut allow_autoplay = !matches!(size, EditorSize::Small);
        set_output.set(
            output
                .into_iter()
                .map(|item| match item {
                    OutputItem::String(s) => view!(<div class="output-item">{s}</div>).into_view(),
                    OutputItem::Image(bytes) => {
                        let encoded = STANDARD.encode(bytes);
                        view!(<div><img class="output-image" src={format!("data:image/png;base64,{encoded}")} /></div>).into_view()
                    }
                    OutputItem::Audio(bytes) => {
                        let encoded = STANDARD.encode(bytes);
                        let src = format!("data:audio/wav;base64,{}", encoded);
                        if allow_autoplay {
                            allow_autoplay = false;
                            view!(<div><audio controls autoplay src=src/></div>).into_view()
                        } else {
                            view!(<div><audio controls src=src/></div>).into_view()
                        }
                    }
                    OutputItem::Error(error) => {
                        view!(<div class="output-item output-error">{error}</div>).into_view()
                    }
                })
                .collect::<Vec<_>>()
                .into_view(),
        );
    };

    // Replace the selected text in the editor with the given string
    let replace_code = move |inserted: &str| {
        if let Some((start, end)) = get_code_cursor() {
            let (start, end) = (start.min(end), start.max(end) as usize);
            let code = code_text();
            let new: String = code
                .chars()
                .take(start as usize)
                .chain(inserted.chars())
                .chain(code.chars().skip(end))
                .collect();
            let offset = inserted.chars().count() as u32;
            state().set_code(&new, Cursor::Set(start + offset, start + offset));
        };
    };

    // Remove a code range
    let remove_code = move |start: u32, end: u32| {
        if start == end {
            return;
        }
        let (start, end) = (start.min(end), start.max(end) as usize);
        let code = code_text();
        let new: String = code
            .chars()
            .take(start as usize)
            .chain(code.chars().skip(end))
            .collect();
        state().set_code(&new, Cursor::Set(start, start));
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
            state().set_code(&code_text(), Cursor::Set(start, start));
        }
    };

    // Handle key events
    window_event_listener(keydown, move |event| {
        let event = event.dyn_ref::<web_sys::KeyboardEvent>().unwrap();
        let focused = event
            .target()
            .and_then(|t| t.dyn_into::<HtmlDivElement>().ok())
            .is_some_and(|t| t.id() == code_id());
        if !focused {
            return;
        }
        let mut handled = true;
        /// For determining if ctrl+backspace/delete should remove a sequence of characters
        fn char_class(c: char) -> u8 {
            if c.is_ascii_alphabetic() {
                1
            } else if c.is_ascii_digit() {
                2
            } else if c == '\n' {
                3
            } else if c == '\'' {
                4
            } else if c == '"' {
                5
            } else if "()[]{}".contains(c) {
                7
            } else {
                0
            }
        }
        let key = event.key();
        let key = key.as_str();
        match key {
            "Enter" => {
                if event.ctrl_key() || event.shift_key() {
                    run(true, true);
                } else {
                    replace_code("\n");
                }
            }
            "Backspace" => {
                let (start, end) = get_code_cursor().unwrap();
                if start == end {
                    if start > 0 {
                        let mut removal_count = 1;
                        if event.ctrl_key() {
                            removal_count = 0;
                            let code = code_text();
                            let chars: Vec<_> = code.chars().take(start as usize).collect();
                            let last_char = *chars.last().unwrap();
                            let class = char_class(last_char);
                            let mut encountered_space = false;
                            for &c in chars.iter().rev() {
                                if c.is_whitespace() && c != '\n'
                                    || char_class(c) == class && !encountered_space
                                {
                                    removal_count += 1;
                                } else {
                                    break;
                                }
                                encountered_space |= c.is_whitespace();
                            }
                        }
                        remove_code(start - removal_count, start);
                    }
                } else {
                    remove_code(start, end);
                }
            }
            "Delete" => {
                let (start, end) = get_code_cursor().unwrap();
                if start == end {
                    let mut removal_count = 1;
                    if event.ctrl_key() {
                        removal_count = 0;
                        let code = code_text();
                        let chars: Vec<_> = code.chars().skip(end as usize).collect();
                        let first_char = *chars.first().unwrap();
                        let class = char_class(first_char);
                        let mut encountered_space = false;
                        for &c in chars.iter() {
                            if c.is_whitespace() && c != '\n'
                                || char_class(c) == class && !encountered_space
                            {
                                removal_count += 1;
                            } else {
                                break;
                            }
                            encountered_space |= c.is_whitespace();
                        }
                    }
                    remove_code(start, start + removal_count);
                } else {
                    remove_code(start, end);
                }
            }
            "Tab" => {
                replace_code("\t");
            }
            "a" if event.ctrl_key() => {
                let code = code_text();
                log!("{:?}", code);
                state().set_code(&code, Cursor::Set(0, code.chars().count() as u32));
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
                _ = window().navigator().clipboard().unwrap().write_text(&text);
                remove_code(start, end);
            }
            "z" if event.ctrl_key() => state().undo(),
            "y" if event.ctrl_key() => state().redo(),
            "/" if event.ctrl_key() => {
                let code = code_text();
                let (start, end) = get_code_cursor().unwrap();
                let (start, end) = (start.min(end), start.max(end));
                let (start_line, _) = line_col(&code, start as usize);
                let (end_line, _) = line_col(&code, end as usize);
                let mut lines: Vec<String> = code.lines().map(Into::into).collect();
                let range = &mut lines[start_line - 1..end_line];
                if range.iter().all(|line| line.trim().starts_with('#')) {
                    // Toggle comments off
                    for line in range {
                        if line.starts_with("# ") {
                            line.replace_range(0..2, "");
                        } else {
                            *line = line.trim_start_matches('#').into();
                        }
                    }
                } else {
                    // Toggle comments on
                    for line in range {
                        line.insert_str(0, "# ");
                    }
                }
                let new_code = lines.join("\n");
                state().set_code(&new_code, Cursor::Set(start, end));
            }
            "\"" | "(" | "[" | "{" => {
                // Surround the selected text with delimiters
                let (start, end) = get_code_cursor().unwrap();
                if start != end {
                    let open = key;
                    let close = match open {
                        "\"" => "\"",
                        "(" => ")",
                        "[" => "]",
                        "{" => "}",
                        _ => unreachable!(),
                    };
                    let (start, end) = (start.min(end), start.max(end));
                    let code = code_text();
                    let mut chars = code.chars();
                    let mut new_code = String::new();
                    new_code.extend(chars.by_ref().take(start as usize));
                    new_code.push_str(open);
                    new_code.extend(chars.by_ref().take((end - start) as usize));
                    new_code.push_str(close);
                    new_code.extend(chars);
                    state().set_code(&new_code, Cursor::Set(start + 1, end + 1));
                } else {
                    handled = false;
                }
            }
            key @ ("ArrowUp" | "ArrowDown") if event.alt_key() => {
                let (_, end) = get_code_cursor().unwrap();
                let code = code_text();
                let (line, col) = line_col(&code, end as usize);
                let line_index = line - 1;
                let up = key == "ArrowUp";
                let mut lines: Vec<String> = code.lines().map(Into::into).collect();
                if up && line_index > 0 || !up && line_index < lines.len() - 1 {
                    let swap_index = if up { line_index - 1 } else { line_index + 1 };
                    lines.swap(line_index, swap_index);
                    let swapped: String = lines.join("\n");
                    let mut new_end = 0;
                    for (i, line) in lines.iter().enumerate() {
                        if i == swap_index {
                            let line_len = line.chars().count();
                            if col < line_len {
                                new_end += col as u32;
                                new_end -= 1;
                            } else {
                                new_end += line_len as u32;
                            }
                            break;
                        }
                        new_end += line.chars().count() as u32 + 1;
                    }
                    state().set_code(&swapped, Cursor::Set(new_end, new_end));
                }
            }
            "ArrowLeft" | "ArrowRight" if event.alt_key() => {}
            _ => handled = false,
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
        replace_code(&text);
    };

    // Go to the next example
    let next_example = {
        let examples = examples.clone();
        move |_| {
            set_example.update(|e| {
                *e = (*e + 1) % examples.len();
                state().set_code(&examples[*e], Cursor::Ignore);
                state().clear_history();
                run(false, false);
            })
        }
    };
    // Go to the previous example
    let prev_example = {
        let examples = examples.clone();
        move |_| {
            set_example.update(|e| {
                *e = (*e + examples.len() - 1) % examples.len();
                state().set_code(&examples[*e], Cursor::Ignore);
                state().clear_history();
                run(false, false);
            })
        }
    };

    // Glyph hover doc
    let (glyph_doc, set_glyph_doc) = create_signal(String::new());
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
            let mut title = p.name().unwrap_or_default().to_string();
            if let Some(ascii) = p.ascii() {
                title = format!("({}) {}", ascii, title);
            }
            let onclick = move |event: MouseEvent| {
                if event.ctrl_key() {
                    // Redirect to the docs page
                    use_navigate()(
                        &format!("/docs/{}", p.name().unwrap_or_default()),
                        NavigateOptions::default(),
                    );
                } else {
                    replace_code(&p.to_string());
                }
            };
            let onmouseover = move |_| {
                if let Some(doc) = p.doc() {
                    set_glyph_doc.set(doc.short_text().into_owned());
                    _ = glyph_doc_element().style().remove_property("display");
                }
            };
            Some(
                view! {
                    <button
                        class="glyph-button glyph-title"
                        data-title=title
                        on:click=onclick
                        on:mouseover=onmouseover
                        on:mouseleave=onmouseleave>
                        <div class={prim_class(p)}>{ text }</div>
                    </button>
                }
                .into_view(),
            )
        })
        .collect();

    // Additional code buttons
    for (glyph, title, class) in [
        ("_", "strand", "strand-span"),
        ("[]", "array", ""),
        ("()", "function", ""),
        ("{}", "dfn", ""),
        ("¯", "negative\n`", "number-literal-span"),
        ("@", "character", "string-literal-span"),
        ("\"", "string", "string-literal-span"),
        ("←", "binding", ""),
        ("#", "comment", "comment-span"),
    ] {
        let onclick = move |_| replace_code(glyph);
        let class = format!("glyph-button {class}");
        glyph_buttons.push(
            view! {
                <button class=class data-title=title on:click=onclick>{glyph}</button>
            }
            .into_view(),
        );
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
    let (show_glyphs, set_show_glyphs) = create_signal(match size {
        EditorSize::Small => false,
        EditorSize::Medium | EditorSize::Pad => true,
    });

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
    set_timeout(move || run(false, false), Duration::from_millis(0));

    // Line numbers
    let line_numbers = move || {
        (0..line_count.get().max(1))
            .map(|i| {
                view!( <div>
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
        if let EditorSize::Pad = size {
            window()
                .history()
                .unwrap()
                .push_state_with_url(
                    &JsValue::NULL,
                    "",
                    Some(&format!("/pad?src={}", urlencoding::encode(&code_text()))),
                )
                .unwrap();
        }
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
    view! {
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
                                data-title=copy_code_title
                                on:click=copy_code>
                                "📋"
                            </button>
                            <button
                                class="editor-right-button"
                                data-title=copy_link_title
                                on:click=copy_link>
                                "🔗"
                            </button>
                            <button
                                id="glyphs-toggle-button"
                                class="editor-right-button"
                                data-title=show_glyphs_title
                                on:click=toggle_show_glyphs>{show_glyphs_text}</button>
                            <div id="example-tracker">{example_text}</div>
                        </div>
                        <div id={glyph_doc_id} class="glyph-doc" style="display: none">
                            { move || glyph_doc.get() }
                            <div class="glyph-doc-ctrl-click">"Ctrl+click for more info"</div>
                        </div>
                        <div class="code">
                            <div class="line-numbers">
                                { line_numbers }
                            </div>
                            // The text entry area
                            <div
                                id={code_id}
                                contenteditable="true"
                                spellcheck="false"
                                class="code-entry"
                                style={format!("height: {code_height_em}em;")}
                                on:input=code_input
                                on:paste=code_paste>
                                "Loading..."
                            </div>
                        </div>
                    </div>
                    <div class="output-frame">
                        <div class="output">
                            { move || output.get() }
                        </div>
                        <div id="code-buttons">
                            <button class="code-button" on:click=move |_| run(true, false)>{ "Run" }</button>
                            <button
                                id="prev-example"
                                class="code-button"
                                style=example_arrow_style
                                on:click=prev_example>{ "<" } </button>
                            <button
                                id="next-example"
                                class=next_button_class
                                style=example_arrow_style
                                on:click=next_example>{ ">" } </button>
                        </div>
                    </div>
                </div>
            </div>
            <div id="editor-help">
                { help.iter().map(|s| view!(<p>{*s}</p>)).collect::<Vec<_>>() }
            </div>
        </div>
    }
}

fn line_col(s: &str, pos: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in s.chars().enumerate() {
        if i == pos {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
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
    if text.is_empty() {
        return parent.inner_text();
    }

    text
}

fn get_code_cursor_impl(id: &str) -> Option<(u32, u32)> {
    let parent = element::<HtmlDivElement>(id);
    let sel = window().get_selection().ok()??;
    let (anchor_node, anchor_offset) = (sel.anchor_node()?, sel.anchor_offset());
    let (focus_node, focus_offset) = (sel.focus_node()?, sel.focus_offset());
    if !parent.contains(Some(&anchor_node)) || !parent.contains(Some(&focus_node)) {
        return None;
    }
    let mut start = anchor_offset;
    let mut end = focus_offset;
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

fn set_code_cursor(id: &str, start: u32, end: u32) {
    // log!("set_code_cursor({}, {})", start, end);

    let elem = element::<HtmlDivElement>(id);
    let find_pos = |mut pos: u32| {
        let mut text_len;
        let mut last_node = None;
        'divs: for (i, div_node) in children_of(&elem).enumerate() {
            if i > 0 {
                if pos > 0 {
                    pos -= 1;
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
                if pos > text_len {
                    pos -= text_len;
                } else {
                    break 'divs;
                }
            }
        }
        (last_node, pos)
    };
    let (start_node, mut start_len) = find_pos(start);
    let (end_node, mut end_len) = find_pos(end);
    if let Some((start_node, end_node)) = start_node.zip(end_node) {
        // log!("ended on: {:?}", text_content);
        start_len = start_len.min(start_node.text_content().unwrap().chars().count() as u32);
        end_len = end_len.min(end_node.text_content().unwrap().chars().count() as u32);
        let range = document().create_range().unwrap();
        range.set_start(&start_node, start_len).unwrap();
        range.set_end(&end_node, end_len).unwrap();
        let sel = window().get_selection().unwrap().unwrap();
        sel.remove_all_ranges().unwrap();
        sel.add_range(&range).unwrap();
        elem.focus().unwrap();
        get_code_cursor_impl(id);
    }
}

fn set_code_html(id: &str, code: &str) {
    use uiua::lsp::*;

    // log!("set_code_html({:?})", code);

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
            SpanKind::Strand => "strand-span",
        };

        html.push_str(&match kind {
            SpanKind::Primitive(prim) => {
                let name = prim.name().unwrap_or_default();
                if let Some(doc) = prim.doc() {
                    let mut title = format!("{}: {}", name, doc.short_text());
                    if let Some(ascii) = prim.ascii() {
                        title = format!("({}) {}", ascii, title);
                    }
                    format!(
                        r#"<span 
                            class="code-span code-hover {color_class}" 
                            data-title={title:?}>{text}</span>"#
                    )
                } else {
                    format!(
                        r#"<span 
                            class="code-span code-hover {color_class}" 
                            data-title={name:?}>{text}</span>"#
                    )
                }
            }
            SpanKind::String => {
                let title = if text.starts_with('@') {
                    "character"
                } else {
                    "string"
                };
                format!(
                    r#"<span
                            class="code-span code-hover {color_class}" 
                            data-title={title}>{text}</span>"#
                )
            }
            _ => format!(r#"<span class="code-span {color_class}">{text}</span>"#),
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

/// Returns the output and the formatted code
fn run_code(code: &str) -> Vec<OutputItem> {
    let io = WebBackend::default();
    let mut env = Uiua::with_backend(io).with_mode(RunMode::All);
    let mut error = None;
    let values = match env.load_str(code) {
        Ok(env) => env.take_stack(),
        Err(e) => {
            error = Some(e);
            env.take_stack()
        }
    };
    let io = env.downcast_backend::<WebBackend>().unwrap();
    let stdout = take(&mut *io.stdout.lock().unwrap());
    let mut stack = Vec::new();
    for value in values {
        if let Ok(bytes) = value_to_image_bytes(&value, ImageOutputFormat::Png) {
            if value.shape().iter().product::<usize>() > 100 {
                stack.push(OutputItem::Image(bytes));
                continue;
            }
        }
        if value.row_count() > 1000 {
            if let Ok(bytes) = value_to_wav_bytes(&value) {
                stack.push(OutputItem::Audio(bytes));
                continue;
            }
        }
        for line in value.show().lines() {
            stack.push(OutputItem::String(line.to_string()));
        }
    }
    let stderr = take(&mut *io.stderr.lock().unwrap());

    let label =
        ((!stack.is_empty()) as u8) + ((!stdout.is_empty()) as u8) + ((!stderr.is_empty()) as u8)
            >= 2;
    let mut output = Vec::new();
    if !stdout.is_empty() {
        if label {
            output.push(OutputItem::String("stdout:".to_string()));
        }
        output.extend(stdout);
    }
    if !stderr.is_empty() {
        if label {
            output.push(OutputItem::String("stderr:".to_string()));
        }
        output.extend(stderr.lines().map(|line| OutputItem::String(line.into())));
    }
    if !stack.is_empty() {
        if label {
            output.push(OutputItem::String("stack:".to_string()));
        }
        output.extend(stack);
    }
    if let Some(error) = error {
        output.push(OutputItem::Error(error.show(false)));
    }
    output
}
