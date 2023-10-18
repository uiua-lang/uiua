use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    iter,
    mem::{replace, take},
    rc::Rc,
    str::FromStr,
    time::Duration,
};

use base64::engine::{
    general_purpose::{STANDARD, URL_SAFE},
    Engine,
};
use image::ImageOutputFormat;
use leptos::{ev::keydown, *};
use leptos_router::{use_navigate, BrowserIntegration, History, LocationChange, NavigateOptions};
use uiua::{
    format::{format_str, FormatConfig},
    image_to_bytes,
    lex::is_ident_char,
    primitive::Primitive,
    run::RunMode,
    value_to_gif_bytes, value_to_image, value_to_wav_bytes, DiagnosticKind, SysBackend, Uiua,
};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{
    Event, HtmlBrElement, HtmlDivElement, HtmlInputElement, HtmlSelectElement, HtmlStyleElement,
    KeyboardEvent, MouseEvent, Node,
};

use crate::{
    backend::{OutputItem, WebBackend},
    element, prim_class, Prim,
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

/// An editor for Uiua code
#[component]
pub fn Editor<'a>(
    #[prop(optional)] example: &'a str,
    #[prop(optional)] examples: &'a [&'a str],
    #[prop(optional)] size: EditorSize,
    #[prop(optional)] help: &'a [&'a str],
    #[prop(optional)] mode: EditorMode,
    #[prop(optional)] progress_lines: bool,
    #[prop(optional)] no_run: bool,
) -> impl IntoView {
    let no_run = no_run || example.contains("&sl");
    let id = ID.with(|id| {
        let i = id.get();
        id.set(i + 1);
        i
    });
    let help: Vec<String> = help.iter().map(|s| s.to_string()).collect();
    // Initialize all the examples
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
    let (settings_open, set_settings_open) = create_signal(false);

    /// Handles setting the code in the editor, setting the cursor, and managing the history
    struct State {
        code_id: String,
        set_line_count: WriteSignal<usize>,
        set_copied_link: WriteSignal<bool>,
        past: RefCell<Vec<Record>>,
        future: RefCell<Vec<Record>>,
        curr: RefCell<Record>,
    }

    /// A record of a code change
    #[derive(Debug)]
    struct Record {
        code: String,
        before: (u32, u32),
        after: (u32, u32),
    }

    /// Ways to set the cursor
    #[derive(Clone, Copy)]
    enum Cursor {
        Set(u32, u32),
        Keep,
        Ignore,
    }

    impl State {
        /// Set the code and cursor
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

    // Initialize the state
    let state = Rc::new(State {
        code_id: code_id(),
        set_line_count,
        set_copied_link,
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
                let cursor = if let Some((start, end)) = get_code_cursor() {
                    let (new_start_start, new_start_end) = formatted.map_char_pos(start as usize);
                    let (new_end_start, new_end_end) = formatted.map_char_pos(end as usize);
                    let (new_start, new_end) = if get_right_to_left() {
                        (new_start_start, new_end_start)
                    } else {
                        (new_start_end, new_end_end)
                    };
                    Cursor::Set(new_start as u32, new_end as u32)
                } else {
                    cursor
                };
                state().set_code(&formatted.output, cursor);
                formatted.output
            } else {
                state().set_code(&code_text, cursor);
                code_text
            }
        } else {
            state().set_code(&code_text, cursor);
            code_text
        };

        // Update URL
        {
            let encoded = URL_SAFE.encode(&input);
            if let EditorSize::Pad = size {
                BrowserIntegration {}.navigate(&LocationChange {
                    value: format!("/pad?src={encoded}"),
                    scroll: false,
                    replace: true,
                    ..Default::default()
                });
            }
        }

        // Run code
        set_output.set(view!(<div class="running-text">"Running"</div>).into_view());
        set_timeout(
            move || {
                let output = run_code(&input);
                let mut allow_autoplay = !matches!(size, EditorSize::Small);
                let render_output_item = |item| match item {
                    OutputItem::String(s) => {
                        if s.is_empty() {
                            view!(<div class="output-item"><br/></div>).into_view()
                        } else {
                            view!(<div class="output-item">{s}</div>).into_view()
                        }
                    }
                    OutputItem::Image(bytes) => {
                        let encoded = STANDARD.encode(bytes);
                        view!(<div><img class="output-image" src={format!("data:image/png;base64,{encoded}")} /></div>).into_view()
                    }
                    OutputItem::Gif(bytes) => {
                        let encoded = STANDARD.encode(bytes);
                        view!(<div><img class="output-image" src={format!("data:image/gif;base64,{encoded}")} /></div>).into_view()
                    }
                    OutputItem::Audio(bytes) => {
                        let encoded = STANDARD.encode(bytes);
                        let src = format!("data:audio/wav;base64,{}", encoded);
                        if allow_autoplay {
                            allow_autoplay = false;
                            view!(<div><audio class="output-audio" controls autoplay src=src/></div>).into_view()
                        } else {
                            view!(<div><audio class="output-audio" controls src=src/></div>)
                                .into_view()
                        }
                    }
                    OutputItem::Error(error) => {
                        view!(<div class="output-item output-error">{error}</div>).into_view()
                    }
                    OutputItem::Diagnostic(message, kind) => {
                        let class = match kind {
                            DiagnosticKind::Warning => "output-warning",
                            DiagnosticKind::Advice => "output-advice",
                            DiagnosticKind::Style => "output-style",
                        };
                        let class = format!("output-item {class}");
                        view!(<div class=class>{message}</div>).into_view()
                    }
                    OutputItem::Separator => {
                        view!(<div class="output-item"><hr/></div>).into_view()
                    }
                };
                let items: Vec<_> = output.into_iter().map(render_output_item).collect();
                set_output.set(items.into_view());
            },
            Duration::ZERO,
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

    // Surround the selected text with delimiters
    let surround_code = move |open: char, close: char| {
        let (start, end) = get_code_cursor().unwrap();
        let (start, end) = (start.min(end), start.max(end));
        let code = code_text();
        let mut chars = code.chars();
        let mut new_code = String::new();
        new_code.extend(chars.by_ref().take(start as usize));
        new_code.push(open);
        new_code.extend(chars.by_ref().take((end - start) as usize));
        new_code.push(close);
        new_code.extend(chars);
        state().set_code(&new_code, Cursor::Set(start + 1, end + 1));
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

    let on_mac = window()
        .navigator()
        .user_agent()
        .unwrap()
        .to_lowercase()
        .contains("mac");
    let os_ctrl = move |event: &KeyboardEvent| {
        if on_mac {
            event.meta_key()
        } else {
            event.ctrl_key()
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
            if is_ident_char(c) {
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
                if os_ctrl(event) || event.shift_key() {
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
                        if os_ctrl(event) {
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
                    if os_ctrl(event) {
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
            // Select all
            "a" if os_ctrl(event) => {
                let code = code_text();
                state().set_code(&code, Cursor::Set(0, code.chars().count() as u32));
            }
            // Copy
            "c" if os_ctrl(event) => {
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
            // Cut
            "x" if os_ctrl(event) => {
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
            // Undo
            "z" if os_ctrl(event) => state().undo(),
            // Redo
            "y" if os_ctrl(event) => state().redo(),
            // Toggle line comment
            "/" if os_ctrl(event) => {
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
            // Handle double quote delimiters
            "\"" => {
                let (start, end) = get_code_cursor().unwrap();
                let code = code_text();
                if start != end
                    || code
                        .chars()
                        .nth(start as usize)
                        .map_or(true, |c| c.is_whitespace())
                {
                    surround_code('"', '"');
                } else if start == end && code_text().chars().nth(start as usize) == Some('"') {
                    state().set_cursor((start + 1, start + 1));
                } else {
                    replace_code(key);
                }
            }
            // Handle open delimiters
            "(" | "[" | "{" => {
                // Surround the selected text with delimiters
                let (open, close) = match key {
                    "\"" => ('"', '"'),
                    "(" => ('(', ')'),
                    "[" => ('[', ']'),
                    "{" => ('{', '}'),
                    _ => unreachable!(),
                };
                let (start, end) = get_code_cursor().unwrap();
                if start != end
                    || code_text()
                        .chars()
                        .nth(start as usize)
                        .map_or(true, |c| c.is_whitespace())
                {
                    surround_code(open, close);
                } else {
                    replace_code(key);
                }
            }
            // Handle close delimiters
            ")" | "]" | "}" => {
                let (start, end) = get_code_cursor().unwrap();
                let close = key.chars().next().unwrap();
                if start == end && code_text().chars().nth(start as usize) == Some(close) {
                    state().set_cursor((start + 1, start + 1));
                } else {
                    handled = false;
                }
            }
            // Line swapping with alt+up/down
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
            // Intercept forward/back keyboard navigation
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
    let (glyph_doc, set_glyph_doc) = create_signal(View::default());
    let onmouseleave = move |_| {
        _ = glyph_doc_element().style().set_property("display", "none");
    };

    // Glyph buttons
    // These are the buttons that appear above the editor and allow the user to insert glyphs
    let mut glyph_buttons: Vec<_> = Primitive::non_deprecated()
        .filter_map(|p| {
            let text = p
                .glyph()
                .map(Into::into)
                .or_else(|| p.ascii().map(|s| s.to_string()))?;
            let mut title = p.name().unwrap_or_default().to_string();
            if let Some(ascii) = p.ascii() {
                title = format!("({}) {}", ascii, title);
            }
            // Navigate to the docs page on ctrl/shift+click
            let onclick = move |event: MouseEvent| {
                if !on_mac && event.ctrl_key() || on_mac && event.meta_key() {
                    // Open the docs page
                    window()
                        .open_with_url_and_target(
                            &format!("/docs/{}", p.name().unwrap_or_default()),
                            "_blank",
                        )
                        .unwrap();
                } else if event.shift_key() {
                    // Redirect to the docs page
                    use_navigate()(
                        &format!("/docs/{}", p.name().unwrap_or_default()),
                        NavigateOptions::default(),
                    );
                } else {
                    replace_code(&p.to_string());
                }
            };
            // Show the glyph doc on mouseover
            let onmouseover = move |_| {
                if let Some(doc) = p.doc() {
                    set_glyph_doc.set(
                        view! {
                            <Prim prim=p/>
                            <br/>
                            { doc.short_text().into_owned() }
                        }
                        .into_view(),
                    );
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
    for (glyph, title, class, surround, doc) in [
        ("_", "strand", "strand-span", None, "arrays#creating-arrays"),
        (
            "[]",
            "array",
            "",
            Some(('[', ']')),
            "arrays#creating-arrays",
        ),
        (
            "{}",
            "box array",
            "",
            Some(('{', '}')),
            "arrays#nested-arrays",
        ),
        (
            "()",
            "function",
            "",
            Some(('(', ')')),
            "functions#inline-functions",
        ),
        ("¯", "negative (`)", "number-literal-span", None, ""),
        (
            "@",
            "character",
            "string-literal-span",
            None,
            "types#characters",
        ),
        (
            "$",
            "format/multiline string",
            "string-literal-span",
            None,
            "functions#format-strings",
        ),
        (
            "\"",
            "string",
            "string-literal-span",
            Some(('"', '"')),
            "types#characters",
        ),
        ("←", "binding (=)", "", None, "bindings"),
        (
            "|",
            "signature / terminate modifier",
            "",
            None,
            "functions#stack-signatures",
        ),
        ("#", "comment", "comment-span", None, "basic#comments"),
    ] {
        let class = format!("glyph-button {class}");
        // Navigate to the docs page on ctrl/shift+click
        let onclick = move |event: MouseEvent| {
            if !doc.is_empty() && (!on_mac && event.ctrl_key() || on_mac && event.meta_key()) {
                // Open the docs page
                window()
                    .open_with_url_and_target(&format!("/docs/{doc}"), "_blank")
                    .unwrap();
            } else if !doc.is_empty() && event.shift_key() {
                // Redirect to the docs page
                use_navigate()(&format!("/docs/{doc}"), NavigateOptions::default());
            } else if let Some((open, close)) = surround {
                surround_code(open, close);
            } else {
                replace_code(glyph)
            }
        };
        // Show the doc on mouseover
        let onmouseover = move |_| {
            if !doc.is_empty() {
                set_glyph_doc.set(view!(<code>{ glyph }</code>" "{ title }).into_view());
                _ = glyph_doc_element().style().remove_property("display");
            }
        };
        glyph_buttons.push(
            view! {
                <button
                    class=class
                    data-title=title
                    on:click=onclick
                    on:mouseover=onmouseover
                    on:mouseleave=onmouseleave>
                    {glyph}
                </button>
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
    set_timeout(
        move || {
            if no_run {
                let code = initial_code.get().unwrap();
                set_initial_code.set(None);
                state().set_code(&code, Cursor::Ignore);
            } else {
                run(false, false)
            }
        },
        Duration::from_millis(0),
    );

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
        let encoded = URL_SAFE.encode(code_text());
        let url = format!("https://uiua.org/pad?src={encoded}");
        _ = window().navigator().clipboard().unwrap().write_text(&url);
        if let EditorSize::Pad = size {
            window()
                .history()
                .unwrap()
                .push_state_with_url(&JsValue::NULL, "", Some(&format!("/pad?src={encoded}")))
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

    // Toggle settings
    let toggle_settings_open = move |_| {
        set_settings_open.update(|s| *s = !*s);
    };
    let toggle_settings_title = move || {
        if settings_open.get() {
            "Hide settings"
        } else {
            "Show settings"
        }
    };

    // Settings
    let settings_style = move || {
        if settings_open.get() {
            ""
        } else {
            "display:none"
        }
    };
    let on_execution_limit_change = move |event: Event| {
        let event = event.dyn_into::<web_sys::InputEvent>().unwrap();
        let input: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
        let limit = input.value().parse().unwrap_or(2.0);
        set_execution_limit(limit);
    };
    let toggle_right_to_left = move |_| {
        set_right_to_left(!get_right_to_left());
    };
    let on_select_font = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        let name = input.value();
        set_font_name(&name);
    };
    let on_select_font_size = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        let size = input.value();
        set_font_size(&size);
    };
    set_font_name(&get_font_name());
    set_font_size(&get_font_size());

    // Render
    view! {
        <div id="editor-wrapper">
            <div id="editor">
                <div style=glyph_buttons_style>
                    <div class="glyph-buttons">{glyph_buttons}</div>
                </div>
                <div id="settings" style=settings_style>
                    <div title="The maximum number of seconds a program can run for">
                        "Execution limit:"
                        <input
                            type="number"
                            min="0.01"
                            max="1000000"
                            width="3em"
                            value=get_execution_limit
                            on:input=on_execution_limit_change/>
                        "s"
                    </div>
                    <div title="Place the cursor on the left of the current token when formatting">
                        "Format left:"
                        <input
                            type="checkbox"
                            checked=get_right_to_left
                            on:change=toggle_right_to_left/>
                    </div>
                    <div>
                        "Font size:"
                        <select
                            on:change=on_select_font_size>
                            <option value="0.6em" selected={get_font_size() == "0.6em"}>"Scalar"</option>
                            <option value="0.8em" selected={get_font_size() == "0.8em"}>"Small"</option>
                            <option value="1em" selected={get_font_size() == "1em"}>"Normal"</option>
                            <option value="1.2em" selected={get_font_size() == "1.2em"}>"Big"</option>
                            <option value="1.4em" selected={get_font_size() == "1.4em"}>"Rank 3"</option>
                        </select>
                    </div>
                    <div>
                        "Font:"
                        <select
                            on:change=on_select_font>
                            <option value="DejaVuSansMono" selected={get_font_name() == "DejaVuSansMono"}>"DejaVuSansMono"</option>
                            <option value="Uiua386" selected={get_font_name() == "Uiua386"}>"Uiua386"</option>
                        </select>
                    </div>
                </div>
                <div class=editor_class>
                    <div id="code-area">
                        <div id={glyph_doc_id} class="glyph-doc" style="display: none">
                            { move || glyph_doc.get() }
                            <div class="glyph-doc-ctrl-click">"Shift+click for more info (Ctrl+click for new tab)"</div>
                        </div>
                        <div id="code-right-side">
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
                                on:click=toggle_show_glyphs>{show_glyphs_text}
                            </button>
                            <button
                                class="editor-right-button"
                                data-title=toggle_settings_title
                                on:click=toggle_settings_open>
                                "⚙️"
                            </button>
                            <div id="example-tracker">{example_text}</div>
                        </div>
                        <div class="code sized-code">
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
                        <div class="output sized-code">
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
                { help.iter().map(|s| view!(<p>{s}</p>)).collect::<Vec<_>>() }
            </div>
            <div id="editor-help">
            {
                if let EditorSize::Pad = size {
                    Some("Note: Uiua is not yet stable")
                } else {
                    None
                }
            }
            </div>
        </div>
    }
}

fn get_local_var<T>(name: &str, default: impl FnOnce() -> T) -> T
where
    T: FromStr,
    T::Err: std::fmt::Display,
{
    window()
        .local_storage()
        .unwrap()
        .unwrap()
        .get_item(name)
        .ok()
        .flatten()
        .and_then(|s| {
            s.parse()
                .map_err(|e| logging::log!("Error parsing local var {name:?} = {s:?}: {e}"))
                .ok()
        })
        .unwrap_or_else(default)
}

fn set_local_var<T>(name: &str, value: T)
where
    T: ToString,
{
    window()
        .local_storage()
        .unwrap()
        .unwrap()
        .set_item(name, &value.to_string())
        .unwrap();
}

fn get_execution_limit() -> f64 {
    get_local_var("execution-limit", || 2.0)
}
fn set_execution_limit(limit: f64) {
    set_local_var("execution-limit", limit);
}

fn get_right_to_left() -> bool {
    get_local_var("right-to-left", || false)
}
fn set_right_to_left(rtl: bool) {
    set_local_var("right-to-left", rtl);
}

fn get_font_name() -> String {
    get_local_var("font-name", || "DejaVuSansMono".into())
}
fn set_font_name(name: &str) {
    set_local_var("font-name", name);
    update_style();
}

fn get_font_size() -> String {
    get_local_var("font-size", || "1em".into())
}
fn set_font_size(size: &str) {
    set_local_var("font-size", size);
    update_style();
}

fn update_style() {
    let font_name = get_font_name();
    let font_size = get_font_size();
    // Remove the old style
    let head = &document().head().unwrap();
    if let Some(item) = head.get_elements_by_tag_name("style").item(0) {
        head.remove_child(&item).unwrap();
    }
    // Add the new style
    let new_style = document()
        .create_element("style")
        .unwrap()
        .dyn_into::<HtmlStyleElement>()
        .unwrap();
    new_style.set_inner_text(&format!(
        "@font-face {{ font-family: 'Code Font'; src: url('/{font_name}.ttf') format('truetype'); }}\n\
        .sized-code {{ font-size: {font_size}; }} }}"
    ));
    document().head().unwrap().append_child(&new_style).unwrap();
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

    // log!("code_text -> {:?}", text);

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
        // This is the case when you click on an empty line
        if children_of(&div_node).count() == 1
            && div_node
                .first_child()
                .unwrap()
                .dyn_into::<HtmlBrElement>()
                .is_ok()
        {
            if div_node.contains(Some(&anchor_node)) {
                start = curr + anchor_offset;
            }
            if div_node.contains(Some(&focus_node)) {
                end = curr + focus_offset;
            }
            continue;
        }
        // This is the normal case
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
                if !unspanned.is_empty() {
                    // log!("unspanned: {:?}", unspanned);
                    html.push_str(&escape_html(&unspanned));
                    unspanned.clear();
                }
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
        if !unspanned.is_empty() {
            // log!("unspanned: {:?}", unspanned);
            html.push_str(&escape_html(&unspanned));
        }
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
        // log!("spanned: {:?} {:?}", kind, text);
        let color_class = match kind {
            SpanKind::Primitive(prim) => prim_class(prim),
            SpanKind::Number => "number-literal-span",
            SpanKind::String => "string-literal-span",
            SpanKind::Comment => "comment-span",
            SpanKind::Strand => "strand-span",
            _ => "",
        };

        if !text.is_empty() && text.chars().all(|c| c == '\n') {
            html.push_str("</div>");
            for _ in 0..text.chars().count() - 1 {
                html.push_str("<div class=\"code-line\"><br/></div>");
            }
            html.push_str("<div class=\"code-line\">");
        } else {
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
                            data-title={title:?}>{}</span>"#,
                            escape_html(&text)
                        )
                    } else {
                        format!(
                            r#"<span 
                            class="code-span code-hover {color_class}" 
                            data-title={name:?}>{}</span>"#,
                            escape_html(&text)
                        )
                    }
                }
                SpanKind::String => {
                    if text == "@ " {
                        format!(
                            r#"<span
                                class="code-span code-hover {color_class}" 
                                data-title="space character">@</span><span
                                class="code-span code-hover {color_class} space-character" 
                                data-title="space character"> </span>"#
                        )
                    } else {
                        let title = if text.starts_with('@') {
                            "character"
                        } else {
                            "string"
                        };
                        format!(
                            r#"<span
                                class="code-span code-hover {color_class}" 
                                data-title={title}>{}</span>"#,
                            escape_html(&text)
                        )
                    }
                }
                _ => format!(
                    r#"<span class="code-span {color_class}">{}</span>"#,
                    escape_html(&text)
                ),
            });
        }

        end = span.end.char_pos;
    }

    push_unspanned(&mut html, code.len(), &mut end);

    html.push_str("</div>");

    html = html
        .replace(
            "<div class=\"code-line\"><span class=\"code-span\"></span></div>",
            "<div class=\"code-line\"><br/></div>",
        )
        .replace(
            "<div class=\"code-line\"></div>",
            "<div class=\"code-line\"><br/></div>",
        )
        .replace("<span class=\"code-span\"></span>", "");

    // log!("html: {}", html);

    elem.set_inner_html(&html);
}

fn escape_html(s: &str) -> Cow<str> {
    if s.contains(['&', '<', '>', '"', '\''].as_ref()) {
        let mut escaped = String::with_capacity(s.len());
        for c in s.chars() {
            match c {
                '&' => escaped.push_str("&amp;"),
                '<' => escaped.push_str("&lt;"),
                '>' => escaped.push_str("&gt;"),
                '"' => escaped.push_str("&quot;"),
                '\'' => escaped.push_str("&#x27;"),
                _ => escaped.push(c),
            }
        }
        Cow::Owned(escaped)
    } else {
        Cow::Borrowed(s)
    }
}

/// Run code and return the output
fn run_code(code: &str) -> Vec<OutputItem> {
    let io = WebBackend::default();
    // Run
    let mut env = Uiua::with_backend(io)
        .with_mode(RunMode::All)
        .with_execution_limit(Duration::from_secs_f64(get_execution_limit()));
    let mut error = None;
    let values = match env.load_str(code) {
        Ok(()) => env.take_stack(),
        Err(e) => {
            error = Some(e);
            env.take_stack()
        }
    };
    let diagnotics = env.take_diagnostics();
    // Get stdout and stderr
    let io = env.downcast_backend::<WebBackend>().unwrap();
    let stdout = take(&mut *io.stdout.lock().unwrap());
    let mut stack = Vec::new();
    for value in values {
        // Try to convert the value to audio
        if value.shape().last().is_some_and(|&n| n >= 1000) {
            if let Ok(bytes) = value_to_wav_bytes(&value, io.audio_sample_rate()) {
                stack.push(OutputItem::Audio(bytes));
                continue;
            }
        }
        // Try to convert the value to an image
        if let Ok(image) = value_to_image(&value) {
            if image.width() > 25 && image.height() > 25 {
                if let Ok(bytes) = image_to_bytes(&image, ImageOutputFormat::Png) {
                    stack.push(OutputItem::Image(bytes));
                    continue;
                }
            }
        }
        // Try to convert the value to a gif
        if let Ok(bytes) = value_to_gif_bytes(&value, 16.0) {
            match value.shape() {
                &[_, h, w] | &[_, h, w, _] if h >= 25 && w >= 25 => {
                    stack.push(OutputItem::Gif(bytes));
                    continue;
                }
                _ => {}
            }
        }
        // Otherwise, just show the value
        for line in value.show().lines() {
            stack.push(OutputItem::String(line.to_string()));
        }
    }
    let stderr = take(&mut *io.stderr.lock().unwrap());
    let trace = take(&mut *io.trace.lock().unwrap());

    // Construct output
    let label = ((!stack.is_empty()) as u8)
        + ((!stdout.is_empty()) as u8)
        + ((!stderr.is_empty()) as u8)
        + ((!trace.is_empty()) as u8)
        >= 2;
    let mut output = Vec::new();
    if !trace.is_empty() {
        output.extend(trace.lines().map(|line| OutputItem::String(line.into())));
    }
    if !stdout.is_empty() {
        if !output.is_empty() {
            output.push(OutputItem::String("".into()));
        }
        if label {
            output.push(OutputItem::String("stdout:".to_string()));
        }
        output.extend(stdout);
    }
    if !stderr.is_empty() {
        if !output.is_empty() {
            output.push(OutputItem::String("".into()));
        }
        if label {
            output.push(OutputItem::String("stderr:".to_string()));
        }
        output.extend(stderr.lines().map(|line| OutputItem::String(line.into())));
    }
    if !stack.is_empty() {
        if label {
            output.push(OutputItem::Separator);
        }
        output.extend(stack);
    }
    if let Some(error) = error {
        if !output.is_empty() {
            output.push(OutputItem::String("".into()));
        }
        const MAX_OUTPUT_BEFORE_ERROR: usize = 60;
        if output.len() >= MAX_OUTPUT_BEFORE_ERROR {
            output = output.split_off(output.len() - MAX_OUTPUT_BEFORE_ERROR);
            output[0] = OutputItem::String("Previous output truncated...".into());
        }
        let formatted = error.show(false);
        let execution_limit_reached = formatted.contains("Maximum execution time exceeded");
        output.push(OutputItem::Error(formatted));
        if execution_limit_reached {
            output.push(OutputItem::String(
                "You can increase the execution time limit in the editor settings".into(),
            ));
        }
    }
    if !diagnotics.is_empty() {
        if !output.is_empty() {
            output.push(OutputItem::String("".into()));
        }
        for diag in diagnotics {
            output.push(OutputItem::Diagnostic(diag.show(false), diag.kind));
        }
    }
    output
}
