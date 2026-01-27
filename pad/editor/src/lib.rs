pub mod backend;
pub mod utils;

use std::{
    borrow::Cow,
    cell::Cell,
    iter::{once, repeat_n},
    mem::take,
    path::PathBuf,
    rc::Rc,
    sync::Arc,
    time::Duration,
};

use base64::engine::{Engine, general_purpose::STANDARD};

use ev::mousemove;
use leptos::{
    ev::{keydown, keyup},
    *,
};

use leptos_router::{BrowserIntegration, History, LocationChange, NavigateOptions, use_navigate};
use uiua::{
    IgnoreError, PrimClass, PrimDoc, Primitive, Signature, Subscript, SysOp, Token,
    format::{FormatConfig, format_str},
    is_ident_char, lex,
    lsp::{BindingDocs, BindingDocsKind},
    now, seed_random,
};
use wasm_bindgen::{JsCast, JsValue, closure::Closure};
use web_sys::{
    DragEvent, Event, FileList, FileReader, HtmlAnchorElement, HtmlBodyElement, HtmlDivElement,
    HtmlInputElement, HtmlSelectElement, HtmlTextAreaElement, MouseEvent,
};

use utils::*;
use utils::{element, format_insert_file_code, get_ast_time};

use backend::{OutputItem, WebBackend, delete_file, drop_file};
use js_sys::Date;
use std::sync::OnceLock;

use leptos_use::use_resize_observer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EditorMode {
    #[default]
    Example,
    Showcase,
    Pad,
}

thread_local! {
    static ID: Cell<u64> = const { Cell::new(0) };
}

static START_TIME: OnceLock<f64> = OnceLock::new();

/// An editor for Uiua code
#[component]
pub fn Editor<'a>(
    #[prop(optional)] example: &'a str,
    #[prop(optional)] hidden: &'a str,
    #[prop(optional)] mode: EditorMode,
    #[prop(optional)] help: &'a [&'a str],
    #[prop(optional)] no_run: bool,
    #[prop(optional)] challenge: Option<ChallengeDef>,
    #[prop(optional)] nonprogressive: bool,
    #[prop(optional)] examples: Option<Vec<String>>,
    #[prop(optional)] kala: &'a str,
) -> impl IntoView {
    START_TIME.get_or_init(|| Date::now() / 1000.0);

    let no_run = no_run
        || mode == EditorMode::Pad && !get_autorun()
        || ["&sl", "&httpsw", "send", "recv", "&ffi", "&clip"]
            .iter()
            .any(|name| example.contains(name));
    let id = ID.with(|id| {
        let i = id.get();
        id.set(i + 1);
        i
    });
    let help: Vec<String> = help.iter().map(|s| s.to_string()).collect();
    // Initialize all the examples
    let examples = match mode {
        EditorMode::Example if !nonprogressive => progressive_strings(example),
        EditorMode::Showcase => examples.unwrap_or_default(),
        _ => vec![example.into()],
    };
    let code_max_lines = if let EditorMode::Pad = mode {
        6
    } else if let Some(chal) = &challenge {
        chal.intended_answer.lines().count()
    } else {
        examples
            .iter()
            .map(|e| e.lines().filter(|line| !line.is_empty()).count())
            .max()
            .unwrap()
    };
    let code_height_em = code_max_lines as f32 * 1.25;

    let editor_wrapper_id = move || format!("editor{id}");
    let code_id = move || format!("code{id}");
    let code_area_id = move || format!("code-area{id}");
    let code_outer_id = move || format!("code-outer{id}");
    let line_numbers_id = move || format!("line-numbers{id}");
    let overlay_id = move || format!("overlay{id}");
    let glyph_doc_id = move || format!("glyphdoc{id}");
    let hover_id = move || format!("hover{id}");
    let input_id = move || format!("input{id}");

    let editor_wrapper_element = move || -> HtmlDivElement { element(&editor_wrapper_id()) };
    let code_element = move || -> HtmlTextAreaElement { element(&code_id()) };
    let glyph_doc_element = move || -> HtmlDivElement { element(&glyph_doc_id()) };

    // Track line count
    let (line_count, set_line_count) = create_signal(1);

    let initial_code_str = if mode == EditorMode::Example && !nonprogressive {
        examples.last()
    } else {
        examples.first()
    }
    .cloned()
    .unwrap_or_else(|| example.into());
    let (initial_code, set_initial_code) = create_signal(Some(initial_code_str.clone()));

    let (example, set_example) = create_signal(if mode == EditorMode::Example && !nonprogressive {
        examples.len().saturating_sub(1)
    } else {
        0
    });
    let (diag_output, set_diag_output) = create_signal(View::default());
    let (output, set_output) = create_signal(View::default());
    let (token_count, set_token_count) = create_signal(0);

    // let code_text = move || code_text(&code_id());
    let get_code_cursor = move || get_code_cursor(&code_id());
    let (copied_link, set_copied_link) = create_signal(false);
    let (settings_open, set_settings_open) = create_signal(false);
    let (fullscreen_enabled, set_fullscreen_enabled) = create_signal(false);
    let update_token_count = move |code: &str| {
        set_token_count.set(
            lex(code, (), &mut Default::default())
                .0
                .into_iter()
                .filter(|tok| {
                    !matches!(&tok.value, Token::Spaces | Token::Newline | Token::Comment)
                })
                .count(),
        )
    };
    let get_code = move || get_code(&code_id());
    let (overlay, set_overlay) = create_signal(String::new());

    let hidden = hidden.to_string();

    // Initialize the state
    let state = State {
        code_id: code_id(),
        code_area_id: code_area_id(),
        code_outer_id: code_outer_id(),
        line_numbers_id: line_numbers_id(),
        editor_wrapper_id: editor_wrapper_id(),
        set_overlay,
        set_line_count,
        set_copied_link,
        past: Vec::new(),
        future: Vec::new(),
        challenge,
        loading_module: false,
        hidden: hidden.clone(),
        curr: {
            let code = initial_code.get_untracked().unwrap();
            let len = code.chars().count() as u32;
            Record {
                code,
                before: (len, len),
                after: (len, len),
            }
        },
    };
    let (get_state, state) = create_signal(state);

    // Get the code with output comments cleaned up
    let clean_code = move || {
        let hidden = get_state.get().hidden;
        let code = if hidden.is_empty() {
            get_code()
        } else {
            format!(
                "{}\n\n# ^^^ HIDDEN ^^^\n\n{}",
                get_state.get().hidden,
                get_code()
            )
        };
        let mut cleaned = String::new();
        let mut in_output_comment = false;
        for line in code.lines() {
            if line.trim().starts_with("##") {
                if !in_output_comment {
                    let mut chars = line.chars();
                    cleaned.extend(chars.by_ref().take_while(|c| c.is_whitespace()));
                    cleaned.push('#');
                    cleaned.extend(chars.take_while(|c| *c == '#'));
                    in_output_comment = true;
                    cleaned.push('\n');
                }
            } else {
                cleaned.push_str(line);
                cleaned.push('\n');
                in_output_comment = false;
            }
        }
        cleaned
    };

    // Format the code
    let format = move |do_format: bool, set_cursor: bool| -> (String, u64) {
        // Get code
        let mut code_text = get_code();
        let mut cursor = if set_cursor {
            Cursor::Keep
        } else {
            Cursor::Ignore
        };
        if let Some(code) = initial_code.get_untracked() {
            code_text = code;
            set_initial_code.set(None);
            cursor = Cursor::Ignore;
        }

        update_token_count(&code_text);

        // Format code
        let seed = now().to_bits();
        seed_random(seed);
        let input = if do_format {
            if let Ok(formatted) = format_str(
                &code_text,
                &FormatConfig {
                    trailing_newline: mode == EditorMode::Pad,
                    backend: Some(Arc::new(WebBackend::default())),
                    ..Default::default()
                },
            ) {
                let cursor = if let Some((start, end)) = get_code_cursor() {
                    let (new_start_start, new_start_end) = formatted.map_char_pos(start);
                    let (new_end_start, new_end_end) = formatted.map_char_pos(end);
                    let (new_start, new_end) = if get_right_to_left() {
                        (new_start_start, new_end_start)
                    } else {
                        (new_start_end, new_end_end)
                    };
                    Cursor::Set(new_start, new_end)
                } else {
                    cursor
                };
                state.update(|state| state.set_code(&formatted.output, cursor));
                formatted.output
            } else {
                state.update(|state| state.set_code(&code_text, cursor));
                code_text.clone()
            }
        } else {
            state.update(|state| state.set_code(&code_text, cursor));
            code_text.clone()
        };

        // Update title
        if let EditorMode::Pad = mode {
            let title = derive_title(&input);
            (window().document().unwrap()).set_title(&format!("{} - {title}", lang()));
        }

        // Update URL
        {
            let encoded = url_encode_code(&clean_code());
            if let EditorMode::Pad = mode {
                BrowserIntegration {}.navigate(&LocationChange {
                    value: format!("/pad?src={encoded}"),
                    scroll: false,
                    replace: true,
                    ..Default::default()
                });
            }
        }

        (input, seed)
    };

    // Run the code
    let run = move |do_format: bool, set_cursor: bool| {
        // Format code
        let (input, seed) = format(do_format, set_cursor);
        state.update(|s| s.update_line_number_width());

        // Run code
        set_output.set(view! { <div class="running-text">"Running"</div> }.into_view());
        let allow_autoplay = !matches!(mode, EditorMode::Example) && get_autoplay();
        let render_output_item = move |item| match item {
            OutputItem::String(s) => {
                if s.is_empty() {
                    view!().into_view()
                } else {
                    view! { <div class="output-item">{s}</div> }.into_view()
                }
            }
            OutputItem::Classed(class, s) => {
                let class = format!("output-item {class}");
                view! { <div class=class>{s}</div> }.into_view()
            }
            OutputItem::Faint(s) => {
                view! { <div class="output-item output-fainter">{s}</div> }.into_view()
            }
            OutputItem::Image(bytes, label) => {
                let encoded = STANDARD.encode(bytes);
                view! {
                    <div class="output-media-wrapper">
                        <div class="output-image-label">{label}</div>
                        <img class="output-image" src=format!("data:image/png;base64,{encoded}") />
                    </div>
                }
                .into_view()
            }
            OutputItem::Gif(bytes, label) => {
                let encoded = STANDARD.encode(bytes);
                view! {
                    <div class="output-media-wrapper">
                        <div class="output-image-label">{label}</div>
                        <img class="output-image" src=format!("data:image/gif;base64,{encoded}") />
                    </div>
                }
                .into_view()
            }
            OutputItem::Apng(bytes, label) => {
                let encoded = STANDARD.encode(bytes);
                view! {
                    <div class="output-media-wrapper">
                        <div class="output-image-label">{label}</div>
                        <img class="output-image" src=format!("data:image/png;base64,{encoded}") />
                    </div>
                }
                .into_view()
            }
            OutputItem::Audio(bytes, label) => {
                let encoded = STANDARD.encode(bytes);
                let src = format!("data:audio/wav;base64,{encoded}");
                let label = label.map(|s| format!("{s}:"));
                if allow_autoplay {
                    view! {
                        <div class="output-media-wrapper">
                            <div class="output-item output-audio-label">{label}</div>
                            <audio class="output-audio" controls autoplay src=src />
                        </div>
                    }
                    .into_view()
                } else {
                    view! {
                        <div class="output-media-wrapper">
                            <div class="output-item output-audio-label">{label}</div>
                            <audio class="output-audio" controls src=src />
                        </div>
                    }
                    .into_view()
                }
            }
            OutputItem::Svg(s, label) => view! {
                <div class="output-media-wrapper">
                    <div class="output-image-label">{label}</div>
                    <img
                        class="output-image"
                        src=format!("data:image/svg+xml;utf8, {}", urlencoding::encode(&s))
                    />
                </div>
            }
            .into_view(),
            OutputItem::Report(report) => report_view(&report).into_view(),
            OutputItem::Separator => view! {
                <div class="output-item">
                    <hr />
                </div>
            }
            .into_view(),
        };
        let flex_wrap = if get_top_at_top() {
            "flex-wrap: wrap;"
        } else {
            "flex-wrap: wrap-reverse;"
        };
        let render_output_items = move |items: Vec<OutputItem>| match items.as_slice() {
            [] => View::default(),
            [OutputItem::Separator] => view!(<div class="output-item">
                    <hr />
                </div>)
            .into_view(),
            [OutputItem::String(s)] if s.is_empty() => View::default(),
            _ => view!(<div class="output-values" style=flex_wrap>
                    {(items.into_iter())
                        .map(|item| {
                            render_output_item(item)
                                .into_view()
                        })
                        .collect::<Vec<_>>()
                    }
                </div>)
            .into_view(),
        };
        set_timeout(
            move || {
                state.update(|st| {
                    seed_random(seed);
                    let output = st.run_code(&input);
                    if take(&mut st.loading_module) {
                        set_timeout(
                            move || {
                                state.update(|st| {
                                    seed_random(seed);
                                    let output = st.run_code(&input);
                                    let (diags, items): (Vec<_>, Vec<_>) = output
                                        .into_iter()
                                        .partition(|line| line.len() == 1 && line[0].is_report());
                                    let items: Vec<_> =
                                        items.into_iter().map(render_output_items).collect();
                                    let diags: Vec<_> = diags
                                        .into_iter()
                                        .map(|line| {
                                            render_output_item(line.into_iter().next().unwrap())
                                        })
                                        .collect();
                                    set_output.set(items.into_view());
                                    set_diag_output.set(diags.into_view());
                                });
                            },
                            Duration::from_millis(200),
                        );
                    } else {
                        let (diags, items): (Vec<_>, Vec<_>) = output
                            .into_iter()
                            .partition(|line| line.len() == 1 && line[0].is_report());
                        let items: Vec<_> = items.into_iter().map(render_output_items).collect();
                        let diags: Vec<_> = diags
                            .into_iter()
                            .map(|line| render_output_item(line.into_iter().next().unwrap()))
                            .collect();
                        set_output.set(items.into_view());
                        set_diag_output.set(diags.into_view());
                    }
                });
            },
            Duration::ZERO,
        );
    };

    // Replace the selected text in the editor with the given string
    let replace_code = move |state: &mut State, inserted: &str| {
        let Some((start, end)) = get_code_cursor() else {
            return;
        };
        let mut inserted = Cow::Borrowed(inserted);
        if inserted.contains('\r') {
            inserted = inserted.replace('\r', "").into();
        }
        if inserted.contains('\t') {
            inserted = inserted.replace('\t', "  ").into();
        }
        let (start, end) = (start.min(end), start.max(end) as usize);
        // logging::log!("replace start: {start}, end: {end}");
        let code = get_code();
        // logging::log!("code: {code:?}");
        // logging::log!("insert: {inserted:?}");
        let new: String = code
            .chars()
            .take(start as usize)
            .chain(inserted.chars())
            .chain(code.chars().skip(end))
            .collect();
        let offset = inserted.chars().count() as u32;
        state.set_code(&new, Cursor::Set(start + offset, start + offset));
        _ = code_element().focus();
    };

    // Remove a code range
    let remove_code = move |state: &mut State, start: u32, end: u32| {
        // logging::log!("remove start: {start}, end: {end}");
        if start == end {
            return;
        }
        let (start, end) = (start.min(end), start.max(end) as usize);
        let code = get_code();
        let new: String = code
            .chars()
            .take(start as usize)
            .chain(code.chars().skip(end))
            .collect();
        state.set_code(&new, Cursor::Set(start, start));
        _ = code_element().focus();
    };

    // Surround the selected text with delimiters
    let surround_code = move |state: &mut State, open: char, close: char| {
        let (start, end) = get_code_cursor().unwrap();
        let (start, end) = (start.min(end), start.max(end));
        let code = get_code();
        let mut chars = code.chars();
        let mut new_code = String::new();
        new_code.extend(chars.by_ref().take(start as usize));
        new_code.push(open);
        new_code.extend(chars.by_ref().take((end - start) as usize));
        new_code.push(close);
        new_code.extend(chars);
        state.set_code(&new_code, Cursor::Set(start + 1, end + 1));
        _ = code_element().focus();
    };

    // Insert an # Experimental! comment at the top of the code
    let insert_experimental = move || {
        state.update(|state| {
            let code = get_code();
            if code.starts_with("# Experimental!\n") || code == "# Experimental!" {
                return;
            }
            let new_code = format!("# Experimental!\n{code}");
            let cursor = if let Some((start, end)) = get_code_cursor() {
                Cursor::Set(start + 16, end + 16)
            } else {
                Cursor::Ignore
            };
            state.set_code(&new_code, cursor);
        });
    };

    // Remove an # Experimental! comment from the top of the code
    let remove_experimental = move || {
        state.update(|state| {
            let code = get_code();
            if let Some(new_code) = code.strip_prefix("# Experimental!\n") {
                let cursor = if let Some((start, end)) = get_code_cursor() {
                    Cursor::Set(start.saturating_sub(16), end.saturating_sub(16))
                } else {
                    Cursor::Ignore
                };
                state.set_code(new_code, cursor);
            } else if code == "# Experimental!" {
                state.set_code("", Cursor::Set(0, 0));
            }
        });
    };

    // Update textarea line height
    let update_rounded_line_height = move || {
        state.update(|state| state.update_rounded_line_height());
    };
    let line_height_sampler_ref = NodeRef::new();
    use_resize_observer(line_height_sampler_ref, move |_, _| {
        // logging::log!("sample code line resized on pad {id}");
        update_rounded_line_height();
    });

    // Handle key events
    window_event_listener(mousemove, move |event| {
        if let Some(overlay_element) = get_element::<HtmlDivElement>(&overlay_id()) {
            let pointer_events = if event.ctrl_key() && !on_mac() || event.meta_key() && on_mac() {
                "all"
            } else {
                "none"
            };
            overlay_element
                .style()
                .set_property("pointer-events", pointer_events)
                .unwrap();
        }
    });
    window_event_listener(keyup, move |event| {
        let event = event.dyn_ref::<web_sys::KeyboardEvent>().unwrap();
        update_ctrl(event);
        // let key = event.key();
        // logging::log!("release: {key:?}");
    });
    window_event_listener(keydown, move |event| {
        let event = event.dyn_ref::<web_sys::KeyboardEvent>().unwrap();
        update_ctrl(event);
        let key = event.key();
        let key = key.as_str();
        // logging::log!("press: {key:?}");

        if (key == "Control" && !on_mac() || key == "Meta" && on_mac())
            && let Some(overlay_element) = get_element::<HtmlDivElement>(&overlay_id())
        {
            overlay_element
                .style()
                .set_property("pointer-events", "all")
                .unwrap();
        }

        let focused = event
            .target()
            .and_then(|t| t.dyn_into::<HtmlTextAreaElement>().ok())
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

        match key {
            "Enter" => {
                let ctrl = os_ctrl(event);
                let shift = event.shift_key();
                if ctrl || shift {
                    if get_run_on_format() || shift {
                        run(true, true);
                    } else {
                        format(true, true);
                    }
                } else {
                    let (start, _) = get_code_cursor().unwrap();
                    state.update(|state| {
                        let code = get_code();
                        let left_char = if start > 0 {
                            code.chars().nth(start as usize - 1)
                        } else {
                            None
                        };
                        let right_char = code.chars().nth(start as usize);
                        let (start_line, start_col) = line_col(&code, start as usize);
                        let curr_line = code.lines().nth(start_line - 1).unwrap_or_default();
                        let curr_line_indent = curr_line
                            .chars()
                            .take(start_col - 1)
                            .take_while(|c| c.is_whitespace())
                            .count();
                        let line_start = code
                            .lines()
                            .take(start_line - 1)
                            .map(|line| line.chars().count() + 1)
                            .sum::<usize>();
                        let line_before_left: String = code
                            .chars()
                            .take(start.saturating_sub(1) as usize)
                            .skip(line_start)
                            .collect();
                        let before_is_stringy =
                            line_before_left.ends_with('@') || line_before_left.ends_with("$ ");
                        let indent = curr_line_indent
                            + 2 * left_char.is_some_and(|c| "({[".contains(c) && !before_is_stringy)
                                as usize;
                        replace_code(state, &format!("\n{}", " ".repeat(indent)));
                        let (start, _) = get_code_cursor().unwrap();
                        if right_char.is_some_and(|c| ")}]".contains(c) && !before_is_stringy) {
                            replace_code(
                                state,
                                &format!("\n{}", " ".repeat(indent.saturating_sub(2))),
                            );
                            state.set_cursor((start, start));
                        }
                    });
                }
            }
            "Backspace" => {
                let (mut start, end) = get_code_cursor().unwrap();
                // logging::log!("backspace start: {start}, end: {end}");
                state.update(|state| {
                    if start == end {
                        if start > 0 {
                            let mut removal_count = 1;
                            let code = get_code();
                            if os_ctrl(event) {
                                removal_count = 0;
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
                            } else if let Some(char_after) = code.chars().nth(start as usize) {
                                let last_char = code.chars().nth(start as usize - 1).unwrap();
                                for (open, close) in
                                    [('(', ')'), ('[', ']'), ('{', '}'), ('"', '"')]
                                {
                                    if last_char == open {
                                        if char_after == close {
                                            start += 1;
                                            removal_count += 1;
                                        }
                                        break;
                                    }
                                }
                            }
                            remove_code(state, start - removal_count, start);
                        }
                    } else {
                        remove_code(state, start, end);
                    }
                });
            }
            "Delete" if event.shift_key() => {
                // Delete lines between cursor start and end
                state.update(|state| {
                    let (start, end) = get_code_cursor().unwrap();
                    let (start, end) = (start.min(end), start.max(end));
                    let code = get_code();
                    let (start_line, _) = line_col(&code, start as usize);
                    let (end_line, _) = line_col(&code, end as usize);
                    let new_code: String = code
                        .lines()
                        .enumerate()
                        .filter_map(|(i, line)| {
                            if i < start_line - 1 || i >= end_line {
                                if i == 0 || start_line == 1 && i == end_line {
                                    Some(line.into())
                                } else {
                                    Some(format!("\n{line}"))
                                }
                            } else {
                                None
                            }
                        })
                        .collect();
                    state.set_code(&new_code, Cursor::Set(start, start));
                });
            }
            "Delete" => {
                let (start, end) = get_code_cursor().unwrap();
                state.update(|state| {
                    if start == end {
                        let mut removal_count = 1;
                        if os_ctrl(event) {
                            removal_count = 0;
                            let code = get_code();
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
                        remove_code(state, start, start + removal_count);
                    } else {
                        remove_code(state, start, end);
                    }
                });
            }
            "Tab" => state.update(|state| replace_code(state, "  ")),
            // Select all
            "a" if os_ctrl(event) => {
                state.update(|state| {
                    let code = get_code();
                    state.set_code(&code, Cursor::Set(0, code.chars().count() as u32))
                });
            }
            // Copy line
            "c" if os_ctrl(event) => {
                let (start, end) = get_code_cursor().unwrap();
                if start == end {
                    let code = get_code();
                    let (line, _) = line_col(&code, start as usize);
                    if let Some(line) = code.split('\n').nth(line - 1) {
                        _ = window().navigator().clipboard().write_text(line);
                    }
                } else {
                    handled = false;
                }
            }
            // Cut
            "x" if os_ctrl(event) => {
                let (start, end) = get_code_cursor().unwrap();
                let (mut start, mut end) = (start.min(end), start.max(end));
                let code = get_code();
                let text = if start == end {
                    let (line, _) = line_col(&code, start as usize);
                    let text = code.split('\n').nth(line - 1).unwrap_or("").to_string();
                    start = (code.split('\n').take(line - 1))
                        .map(|line| line.chars().count() + 1)
                        .sum::<usize>() as u32;
                    end = start + text.chars().count() as u32 + 1;
                    text
                } else {
                    code.chars()
                        .skip(start as usize)
                        .take((end - start) as usize)
                        .collect()
                };
                _ = window().navigator().clipboard().write_text(&text);
                state.update(|state| remove_code(state, start, end));
            }
            // Redo
            "y" if os_ctrl(event) => state.update(|state| state.redo()),
            "z" | "Z" if os_ctrl(event) && event.shift_key() => state.update(|state| state.redo()),
            // Undo
            "z" if os_ctrl(event) => state.update(|state| state.undo()),
            // Remove # Experimental! comment
            "e" | "E" if os_ctrl(event) && event.shift_key() => remove_experimental(),
            // Insert # Experimental! comment
            "e" if os_ctrl(event) => insert_experimental(),
            // Toggle line comment or multiline string
            "/" | "4" if os_ctrl(event) => {
                state.update(|state| {
                    let code = get_code();
                    let (start, end) = get_code_cursor().unwrap();
                    let (start, end) = (start.min(end), start.max(end));
                    // If the selection spans multiple lines, but reaches its final line through only the selection of the newline character, exclude that final line
                    let offset_end = if start != end
                        && code.chars().nth((end as usize).saturating_sub(1)) == Some('\n')
                    {
                        end - 1
                    } else {
                        end
                    };
                    let (start_line, start_col) = line_col(&code, start as usize);
                    let (end_line, end_col) = line_col(&code, offset_end as usize);

                    let mut lines: Vec<String> = code.split('\n').map(Into::into).collect();
                    let range = &mut lines[start_line - 1..end_line];
                    let comment = key == "/";
                    let prefix = if comment { '#' } else { '$' };

                    // How much to offset the ends of the selection by to account for the change in the number of characters
                    let mut start_diff = 0;
                    let mut end_diff = 0;

                    let last_index = range.len() - 1;
                    if range
                        .iter()
                        .map(|line| line.trim())
                        .all(|line| (comment && line.is_empty()) || line.starts_with(prefix))
                    {
                        // Toggle comments off
                        for (i, line) in range.iter_mut().enumerate() {
                            let old_len = line.len() as i32;
                            let space_count = line.chars().take_while(|c| *c == ' ').count();
                            *line = repeat_n(' ', space_count)
                                .chain({
                                    let line_ = line.trim().trim_start_matches(prefix);
                                    line_.strip_prefix(' ').unwrap_or(line_).chars()
                                })
                                .collect();

                            if i == 0 {
                                start_diff -= (old_len - line.len() as i32)
                                    .min(start_col.saturating_sub(space_count + 1) as i32);
                            }

                            let mut this_end_diff = old_len - line.len() as i32;
                            if i == last_index {
                                this_end_diff = this_end_diff
                                    .min(end_col.saturating_sub(space_count + 1) as i32);
                            }
                            end_diff -= this_end_diff;
                        }
                    } else {
                        // Toggle comments on
                        let spot = range
                            .iter()
                            // Skip blank lines when commenting
                            .filter(|line| !comment || !line.trim().is_empty())
                            .map(|line| line.chars().take_while(|c| " \t".contains(*c)).count())
                            .min()
                            .unwrap_or(0);
                        for (i, line) in range
                            .iter_mut()
                            .enumerate()
                            .filter(|(_, line)| !comment || !line.trim().is_empty())
                        {
                            line.insert(spot, ' ');
                            line.insert(spot, prefix);

                            if i == 0 && start_col > spot {
                                start_diff += 2
                            }

                            if i != last_index || end_col > spot {
                                end_diff += 2;
                            }
                        }
                    }
                    let new_code = lines.join("\n");
                    state.set_code(
                        &new_code,
                        Cursor::Set(
                            (start as i32 + start_diff) as u32,
                            (end as i32 + end_diff) as u32,
                        ),
                    );
                });
            }
            // Handle double quote delimiters
            "\"" => {
                let (start, end) = get_code_cursor().unwrap();
                state.update(|state| {
                    let code = get_code();
                    let can_couple = code
                        .chars()
                        .nth(start as usize)
                        .is_none_or(|c| c.is_whitespace() || "(){}[]".contains(c));
                    let just_one = code.chars().nth((start as usize).saturating_sub(1))
                        == Some('@')
                        || start == end && {
                            let before = code.chars().take(start as usize).collect::<String>();
                            let count = (before.chars().rev())
                                .take_while(|&c| c != '\n')
                                .filter(|&c| c == '"')
                                .count();
                            count % 2 == 1
                        };
                    if (start != end || can_couple) && !just_one {
                        surround_code(state, '"', '"');
                    } else if start == end && code.chars().nth(start as usize) == Some('"') {
                        state.set_cursor((start + 1, start + 1));
                    } else {
                        replace_code(state, key);
                    }
                });
            }
            // Handle open delimiters
            "(" | "[" | "{" if !os_ctrl(event) => {
                // Surround the selected text with delimiters
                let (open, close) = match key {
                    "\"" => ('"', '"'),
                    "(" => ('(', ')'),
                    "[" => ('[', ']'),
                    "{" => ('{', '}'),
                    _ => unreachable!(),
                };
                let (start, end) = get_code_cursor().unwrap();
                let code = get_code();
                state.update(|state| {
                    let can_couple = code
                        .chars()
                        .nth(start as usize)
                        .is_none_or(|c| c.is_whitespace() || "(){}[]".contains(c));
                    let at_behind =
                        code.chars().nth((start as usize).saturating_sub(1)) == Some('@');
                    if (start != end || can_couple) && !at_behind {
                        surround_code(state, open, close);
                    } else {
                        replace_code(state, key);
                    }
                });
            }
            // Handle close delimiters
            ")" | "]" | "}" if !event.meta_key() => {
                let (start, end) = get_code_cursor().unwrap();
                let code = get_code();
                let close = key.chars().next().unwrap();
                state.update(|state| {
                    if start == end && code.chars().nth(start as usize) == Some(close) {
                        state.set_cursor((start + 1, start + 1));
                    } else {
                        replace_code(state, key);
                    }
                });
            }
            // Line swapping with alt+up/down
            key @ ("ArrowUp" | "ArrowDown") if event.alt_key() => {
                let (_, end) = get_code_cursor().unwrap();
                state.update(|state| {
                    let code = get_code();
                    let (line, col) = line_col(&code, end as usize);
                    let line_index = line - 1;
                    let up = key == "ArrowUp";
                    let mut lines: Vec<String> = code.split('\n').map(Into::into).collect();
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
                        state.set_code(&swapped, Cursor::Set(new_end, new_end));
                    }
                });
            }
            // Intercept forward/back keyboard navigation
            "ArrowLeft" | "ArrowRight" if !on_mac() && event.alt_key() => {}
            // Chrome 😠
            "Unidentified" => {
                set_timeout(
                    move || get_state.get().refresh_code(),
                    Duration::from_millis(0),
                );
            }
            // Normal key input
            key if key.chars().count() == 1 && !os_ctrl(event) && !event.alt_key() => {
                state.update(|state| replace_code(state, key));
            }
            _ => handled = false,
        }
        if handled {
            event.prevent_default();
            event.stop_propagation();
            update_token_count(&get_code());
        }
    });

    // Handle composition events
    let update_composition = move |_| {
        set_timeout(
            move || get_state.get().refresh_code(),
            Duration::from_millis(0),
        );
    };
    window_event_listener(ev::compositionstart, update_composition);
    window_event_listener(ev::compositionupdate, update_composition);
    window_event_listener(ev::compositionend, update_composition);

    // Handle paste evens
    let code_paste = move |event: Event| {
        let event = event.dyn_into::<web_sys::ClipboardEvent>().unwrap();
        event.prevent_default();
        event.stop_propagation();
        let text = event.clipboard_data().unwrap().get_data("text").unwrap();
        state.update(|state| replace_code(state, &text));
    };

    // Handle change events
    let code_input = move |_| {
        set_timeout(
            move || {
                state.update(|state| {
                    state.refresh_code();
                    state.track_change();
                })
            },
            Duration::from_millis(0),
        );
    };

    // Handle mouse events
    let code_mouse_move = move |event: MouseEvent| {
        let (mouse_x, mouse_y) = (event.client_x(), event.client_y());
        let overlay: HtmlDivElement = element(&overlay_id());
        let children = overlay.children();
        let mut subchildren = (0..children.length())
            .map(|i| children.item(i).unwrap())
            .flat_map(|child| {
                let children = child.children();
                (0..children.length()).map(move |i| children.item(i).unwrap())
            });
        let hover_elem: HtmlDivElement = element(&hover_id());
        let Some((data_title, rect)) = subchildren
            .find(|child| {
                let rect = child.get_bounding_client_rect();
                rect.left() <= mouse_x as f64
                    && mouse_x as f64 <= rect.right()
                    && rect.top() <= mouse_y as f64
                    && mouse_y as f64 <= rect.bottom()
            })
            .and_then(|child| {
                child
                    .get_attribute("data-title")
                    .map(|title| (title, child.get_bounding_client_rect()))
            })
        else {
            hover_elem.style().set_property("display", "none").unwrap();
            return;
        };
        // Set hover elem pos to mouse pos
        let style = hover_elem.style();
        let left = rect.left();
        let top = rect.bottom();
        _ = style.set_property("left", &format!("{left}px"));
        _ = style.set_property("top", &format!("{top}px"));
        // Set hover elem text to hovered data-title
        hover_elem.set_inner_text(&data_title);
        // Show hover elem
        hover_elem.style().set_property("display", "block").unwrap();
    };

    // Handle mouse leave events
    let code_mouse_leave = move |_| {
        let hover_elem: HtmlDivElement = element(&hover_id());
        hover_elem.style().set_property("display", "none").unwrap();
    };

    // Go to the next example
    let next_example = {
        let examples = examples.clone();
        move |_| {
            set_example.update(|e| {
                *e = (*e + 1) % examples.len();
                state.update(|state| {
                    state.set_code(&examples[*e], Cursor::Ignore);
                    state.clear_history();
                });
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
                state.update(|state| {
                    state.set_code(&examples[*e], Cursor::Ignore);
                    state.clear_history();
                });
                run(false, false);
            })
        }
    };

    let on_toggle_experimental = move |_| {
        let code = get_code();
        if code.starts_with("# Experimental!\n") || code == "# Experimental!" {
            remove_experimental();
        } else {
            insert_experimental();
        }
    };
    let toggle_experimental_button = view! {
        <button
            class="info-button"
            data-title="Toggle # Experimental!"
            on:click=on_toggle_experimental
        >
            "🧪"
        </button>
    }
    .into_view();

    // Glyph hover doc
    let (glyph_doc, set_glyph_doc) = create_signal(View::default());
    let onmouseleave = move |_| {
        _ = glyph_doc_element().style().set_property("display", "none");
    };

    // Glyph buttons
    // These are the buttons that appear above the editor and allow the user to insert glyphs
    let make_glyph_button = move |prim: Primitive| {
        let text = prim
            .glyph()
            .map(Into::into)
            .or_else(|| prim.ascii().map(|s| s.to_string()))?;
        let mut title = prim.name().to_string();
        if let Some(ascii) = prim.ascii() {
            title = format!("({ascii}) {title}");
        }
        // Navigate to the docs page on ctrl/shift+click
        let onclick = move |event: MouseEvent| {
            if os_ctrl(&event) {
                // Open the docs page
                window()
                    .open_with_url_and_target(&format!("/docs/{}", prim.name()), "_blank")
                    .unwrap();
            } else if event.shift_key() {
                // Redirect to the docs page
                use_navigate()(
                    &format!("/docs/{}", prim.name()),
                    NavigateOptions::default(),
                );
            } else {
                state.update(|state| replace_code(state, &prim.to_string()));
            }
        };
        // Show the glyph doc on mouseover
        let doc = PrimDoc::from(prim);
        let onmouseover = move |_| {
            set_glyph_doc.set(
                view! {
                    <Prim prim=prim />
                    {prim
                        .is_experimental()
                        .then(|| {
                            view! {
                                <span class="experimental" style="font-size: 0.8em;">
                                    "⚠️ Experimental"
                                </span>
                            }
                        })}
                    <br />
                    {doc.short_text().into_owned()}
                }
                .into_view(),
            );
            _ = glyph_doc_element().style().remove_property("display");
        };
        Some(
            view! {
                <button
                    class="glyph-button glyph-title"
                    data-title=title
                    on:click=onclick
                    on:mouseover=onmouseover
                    on:mouseleave=onmouseleave
                >
                    <div class=prim_class(prim)>{text}</div>
                </button>
            }
            .into_view(),
        )
    };

    // Show or hide the glyph buttons
    let (show_glyphs, set_show_glyphs) = create_signal(match mode {
        EditorMode::Example => false,
        EditorMode::Showcase | EditorMode::Pad => true,
    });

    let glyph_toggle_experimental_button = toggle_experimental_button.clone();

    let glyph_buttons_container = move || {
        show_glyphs.get().then(|| {
            let shown_prims = Primitive::non_deprecated().filter(|prim| !prim.is_experimental());
            let insertion_point = shown_prims
                .clone()
                .enumerate()
                .find(|(_, p)| p.class() == PrimClass::MonadicArray)
                .unwrap()
                .0
                - 2;
            let mut glyph_buttons: Vec<_> = shown_prims.filter_map(make_glyph_button).collect();

            // Additional code buttons
            let syntax_buttons = [
                ("¯", "(`) negative", number_class(), None, ""),
                (
                    "_",
                    "strand",
                    "strand-span",
                    None,
                    "tutorial/Arrays#creating-arrays",
                ),
                (
                    "[]",
                    "array",
                    "",
                    Some(('[', ']')),
                    "tutorial/Arrays#creating-arrays",
                ),
                (
                    "{}",
                    "box array",
                    "",
                    Some(('{', '}')),
                    "tutorial/Arrays#nested-arrays",
                ),
                (
                    "()",
                    "function",
                    "",
                    Some(('(', ')')),
                    "tutorial/Modifiers and Functions#inline-functions",
                ),
                (
                    "@",
                    "character",
                    string_class(),
                    None,
                    "tutorial/Types#characters",
                ),
                (
                    "$",
                    "format/multiline string",
                    string_class(),
                    None,
                    "tutorial/Modifiers and Functions#format-strings",
                ),
                (
                    "\"",
                    "string",
                    string_class(),
                    Some(('"', '"')),
                    "tutorial/Types#characters",
                ),
                ("!", "macro", "", None, "tutorial/Macros"),
                ("^", "placeholder", "", None, "tutorial/Macros"),
                ("←", "(=) binding", "", None, "tutorial/Bindings"),
                ("~", "module", "", None, "tutorial/Modules"),
                (
                    "|",
                    "signature",
                    "",
                    None,
                    "tutorial/Modifiers and Functions#stack-signatures",
                ),
                (
                    "#",
                    "comment",
                    "comment-span",
                    None,
                    "tutorial/Basic Argument Manipulation and Formatting#comments",
                ),
            ];
            for (i, (glyph, title, class, surround, doc)) in syntax_buttons.into_iter().enumerate()
            {
                let class = format!("glyph-button {class}");
                // Navigate to the docs page on ctrl/shift+click
                let onclick = move |event: MouseEvent| {
                    if !doc.is_empty() && os_ctrl(&event) {
                        // Open the docs page
                        window()
                            .open_with_url_and_target(&format!("/{doc}"), "_blank")
                            .unwrap();
                    } else if !doc.is_empty() && event.shift_key() {
                        // Redirect to the docs page
                        use_navigate()(&format!("/{doc}"), NavigateOptions::default());
                    } else if let Some((open, close)) = surround {
                        state.update(|state| surround_code(state, open, close));
                    } else {
                        state.update(|state| replace_code(state, glyph))
                    }
                };
                // Show the doc on mouseover
                let onmouseover = move |_| {
                    if !doc.is_empty() {
                        set_glyph_doc.set(
                            view! {
                                <code>{glyph}</code>
                                " "
                                {title}
                            }
                            .into_view(),
                        );
                        _ = glyph_doc_element().style().remove_property("display");
                    }
                };
                glyph_buttons.insert(
                    insertion_point + i,
                    view! {
                        <button
                            class=class
                            data-title=title
                            on:click=onclick
                            on:mouseover=onmouseover
                            on:mouseleave=onmouseleave
                        >
                            {glyph}
                        </button>
                    }
                    .into_view(),
                );
            }

            // Additional functions combobox
            let mut options = Vec::new();
            let mut named_prims: Vec<Primitive> = Primitive::non_deprecated()
                .filter(|prim| {
                    prim.glyph().is_none() && (get_show_experimental() || !prim.is_experimental())
                })
                .collect();
            named_prims.sort_by(|a, b| {
                (a.name().starts_with('&').cmp(&b.name().starts_with('&')))
                    .then_with(|| a.name().cmp(b.name()))
            });
            let max_name_len = named_prims
                .iter()
                .map(|prim| prim.name().chars().count())
                .max()
                .unwrap();
            for prim in named_prims {
                let class = format!("{} named-function-button", prim_class(prim));
                let doc = PrimDoc::from(prim);
                let mut desc = doc.short_text().to_string();
                if desc.chars().count() > 30 {
                    desc = desc.chars().take(29).chain(['…']).collect();
                }
                let text = format!("{:max_name_len$} - {desc}", prim.name()).replace(' ', " ");
                options.push(view! {
                    <option class=class value=prim.name()>
                        {text}
                    </option>
                });
            }
            let additional_on_change = move |event: Event| {
                let select: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
                let name = select.value();
                state.update(|state| replace_code(state, &name));
                select.set_value("");
            };
            glyph_buttons.push(
                view! {
                    <select
                        id="additional-functions"
                        class="additional-functions"
                        on:change=additional_on_change
                    >
                        <option value="" disabled=true selected=true>
                            "…"
                        </option>
                        {options}
                    </select>
                }
                .into_view(),
            );
            glyph_buttons.swap(insertion_point, insertion_point - 1);

            let experimental_glyph_buttons: Vec<_> = once(glyph_toggle_experimental_button.clone())
                .chain(
                    Primitive::non_deprecated()
                        .filter(|prim| prim.is_experimental())
                        .filter_map(make_glyph_button),
                )
                .collect();

            view! { <div>
                <div class="glyph-buttons">{glyph_buttons}</div>
                <div class="experimental-glyph-buttons">{experimental_glyph_buttons}</div>
            </div> }
        })
    };

    // Select a class for the editor and code area
    let editor_class = move || {
        let editor_size = match mode {
            EditorMode::Example => "small-editor",
            EditorMode::Showcase | EditorMode::Pad => "medium-editor",
        };

        let editor_layout = match fullscreen_enabled.get() {
            true => "fullscreen-editor",
            false => "normal-editor",
        };

        format!("editor {editor_size} {editor_layout}")
    };

    // Hide the example arrows if there is only one example
    let example_arrow_style = if examples.len() <= 1 {
        "display:none"
    } else {
        ""
    };

    // Glyphs toggle button
    let show_glyphs_icon = move || {
        if show_glyphs.get() {
            view! { <span class="material-symbols-rounded">"keyboard_arrow_up"</span> }
        } else {
            view! { <span class="material-symbols-rounded">"keyboard_arrow_down"</span> }
        }
    };

    let show_glyphs_title = move || {
        if show_glyphs.get() {
            "Hide glyphs"
        } else {
            "Show glyphs"
        }
    };
    let toggle_show_glyphs = move |_| set_show_glyphs.update(|s| *s = !*s);

    let fullscreen_button_icon = move || {
        if fullscreen_enabled.get() {
            view! { <span class="material-symbols-rounded">"close_fullscreen"</span> }
        } else {
            view! { <span class="material-symbols-rounded">"open_in_full"</span> }
        }
    };

    let fullscreen_button_title = move || {
        if fullscreen_enabled.get() {
            "Collapse editor"
        } else {
            "Expand editor"
        }
    };

    let toggle_fullscreen = move |_: MouseEvent| {
        set_fullscreen_enabled.update(|s| {
            *s = !*s;
            let _ = document()
                .body()
                .unwrap()
                .unchecked_into::<HtmlBodyElement>()
                .style()
                .set_property(
                    "overflow",
                    match *s {
                        true => "hidden",
                        false => "auto",
                    },
                );
            if !*s {
                set_timeout(
                    move || state.update(|state| state.update_line_number_width()),
                    Duration::ZERO,
                );
            }
        })
    };

    // Show the example number if there are multiple examples
    let examples_len = examples.len();
    let example_tracker_element = move || {
        (examples_len > 1)
            .then(|| format!("{}/{}", example.get() + 1, examples_len))
            .map(|text| view! { <span id="example-tracker">{text}</span> })
    };

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
                state.update(|state| state.set_code(&code, Cursor::Ignore));
            } else {
                run(false, false);
            }
        },
        Duration::from_millis(0),
    );

    let (drag_message, set_drag_message) = create_signal("");

    // Get file drop events
    window_event_listener(leptos_dom::ev::dragover, move |event: DragEvent| {
        let event = event.dyn_into::<web_sys::DragEvent>().unwrap();
        event.prevent_default();
        event.stop_propagation();
        let files = event.data_transfer().unwrap().files().unwrap();
        if files.length() > 0 {
            set_drag_message.set("Drop file to load");
        }
    });
    window_event_listener(leptos_dom::ev::dragleave, move |event: DragEvent| {
        let event = event.dyn_into::<web_sys::DragEvent>().unwrap();
        event.prevent_default();
        event.stop_propagation();
        set_drag_message.set("");
    });

    let handle_load_files = move |files: FileList| {
        let total_files = files.length();
        let processed_files = Rc::new(Cell::new(0));

        for i in 0..total_files {
            let file = files.get(i).unwrap();
            let file_name = file.name();
            let reader = FileReader::new().unwrap();
            reader.read_as_array_buffer(&file).unwrap();

            let processed_files = Rc::clone(&processed_files);
            let on_load = Closure::wrap(Box::new(move |event: Event| {
                let event = event.dyn_into::<web_sys::ProgressEvent>().unwrap();
                let reader = event.target().unwrap().dyn_into::<FileReader>().unwrap();
                let bytes = reader
                    .result()
                    .unwrap()
                    .dyn_into::<js_sys::ArrayBuffer>()
                    .unwrap();
                let bytes = js_sys::Uint8Array::new(&bytes);
                let path = PathBuf::from(&file_name);
                drop_file(path.clone(), bytes.to_vec());
                set_drag_message.set("");

                processed_files.set(processed_files.get() + 1);
                if processed_files.get() == total_files {
                    run(true, false);
                }
            }) as Box<dyn FnMut(_)>);

            reader
                .add_event_listener_with_callback("load", on_load.as_ref().unchecked_ref())
                .unwrap();
            on_load.forget();
        }
    };

    let listener = window_event_listener(leptos_dom::ev::drop, move |event: DragEvent| {
        let event = event.dyn_into::<web_sys::DragEvent>().unwrap();
        event.prevent_default();
        event.stop_propagation();
        let files = event.data_transfer().unwrap().files().unwrap();
        if files.length() == 0 {
            return;
        }
        handle_load_files(files);
    });

    let h = &get_state.get().hidden;
    let hidden_lines = if h.is_empty() {
        0
    } else {
        h.split('\n').count()
    };
    // Line numbers
    let line_numbers = move || {
        (0..line_count.get().max(1))
            .map(|i| {
                view! {
                    <div class="code-line">
                        <span class="code-span">{i + 1 + hidden_lines}</span>
                    </div>
                }
            })
            .collect::<Vec<_>>()
    };

    // Copy a link to the code
    let copy_link_impl = move |markdown: bool| {
        let encoded = url_encode_code(&clean_code());
        let url = format!("https://uiua.org/pad?src={encoded}");
        let to_copy = if markdown {
            let text =
                if let Some((start, end)) = get_code_cursor().filter(|(start, end)| start != end) {
                    let st = start.min(end);
                    let en = start.max(end);
                    let code: String = get_code()
                        .chars()
                        .skip(st as usize)
                        .take((en - st) as usize)
                        .collect();
                    format!("`{code}`")
                } else {
                    "this".into()
                };
            format!("[{text}]({url})")
        } else {
            url
        };
        _ = window().navigator().clipboard().write_text(&to_copy);
        if let EditorMode::Pad = mode {
            window()
                .history()
                .unwrap()
                .push_state_with_url(&JsValue::NULL, "", Some(&format!("/pad?src={encoded}")))
                .unwrap();
        }
        set_copied_link.set(true);
    };
    let copy_link = move |event: MouseEvent| copy_link_impl(event.shift_key());
    let copy_markdown_link = move |_| copy_link_impl(true);
    let copy_link_title = move || {
        if copied_link.get() {
            "Copied!"
        } else {
            "Copy a link to this code (hold shift for markdown)"
        }
    };

    // Let the user download the code as a `.ua` file
    // They can choose where to save it
    let download_code = move |_| {
        let code = get_code();
        let doc = window().document().unwrap();
        let anchor = doc.create_element("a").unwrap();
        anchor
            .set_attribute(
                "href",
                &format!(
                    "data:text/plain;charset=utf-8,{}",
                    urlencoding::encode(&code)
                ),
            )
            .unwrap();
        let name = format!("{}.ua", derive_title(&code));
        anchor.set_attribute("download", &name).unwrap();
        anchor.set_attribute("style", "display: none").unwrap();
        doc.body().unwrap().append_child(&anchor).unwrap();
        anchor.dyn_ref::<HtmlAnchorElement>().unwrap().click();
        set_timeout(move || anchor.remove(), Duration::from_millis(0));
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
    let on_ast_time_change = move |event: Event| {
        let event = event.dyn_into::<web_sys::InputEvent>().unwrap();
        let input: HtmlInputElement = event.target().unwrap().dyn_into().unwrap();
        let time = input.value().parse().unwrap_or(10.0);
        set_ast_time(time);
    };
    let toggle_right_to_left = move |_| {
        set_right_to_left(!get_right_to_left());
    };
    let toggle_autorun = move |_| set_autorun(!get_autorun());
    let toggle_autoplay = move |_| set_autoplay(!get_autoplay());
    let toggle_show_experimental = move |_| {
        set_show_experimental(!get_show_experimental());
    };
    let toggle_run_on_format = move |_| set_run_on_format(!get_run_on_format());
    let toggle_inlay_values = move |_| {
        set_timeout(
            move || get_state.get().refresh_code(),
            Duration::from_millis(0),
        );
        set_inlay_values(!get_inlay_values());
    };
    let on_select_font = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        let name = input.value();
        set_font_name(&name);
        update_rounded_line_height();
    };
    let on_select_font_size = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        let size = input.value();
        set_font_size(&size);
        update_rounded_line_height();
    };
    let on_select_top_at_top = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        let orientation = input.value() == "true";
        set_top_at_top(orientation);
        run(false, false);
    };
    let on_select_animation_format = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        set_animation_format(&input.value());
        run(false, false);
    };
    let on_select_gayness = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        set_gayness(input.value().as_str().into());
    };
    let toggle_rgb_bindings = move |_| {
        set_timeout(
            move || get_state.get().refresh_code(),
            Duration::from_millis(0),
        );
        set_rgb_bindings(!get_rgb_bindings());
    };
    set_font_name(&get_font_name());
    set_font_size(&get_font_size());

    // File upload dialog opening
    let upload_file_dialog = move |_| {
        let input_element: HtmlInputElement = element(&input_id());
        // This produces an error in the console for some reason, but it works
        input_element.dyn_ref::<HtmlInputElement>().unwrap().click();
    };

    let files_selected = move |event: Event| {
        let target = event.target().unwrap();
        let input_element = target.dyn_ref::<HtmlInputElement>().unwrap();
        let files = input_element.files().unwrap();
        handle_load_files(files);
        input_element.set_value("");
    };

    let get_files_to_display = move || {
        // This is a hack to make this closure reactive.
        // The file list updates every time output changes.
        // The code runs immediately after dropping a file, so the output changes too.
        // Plus it handles cases where files are created/deleted after the code runs.
        let _ = output.get();

        let excluded_files = ["example.txt", "example.ua", "primitives.json"];
        backend::FILES.with(|files| {
            files
                .borrow()
                .iter()
                .filter(|(path, _)| {
                    !excluded_files.contains(&path.to_str().unwrap())
                        && !path.starts_with("uiua-modules/")
                })
                .map(|(path, _)| path.clone())
                .collect::<Vec<_>>()
        })
    };

    let file_tab_display = move || {
        let files = get_files_to_display().clone();
        if files.is_empty() {
            return view! { <div></div> };
        }

        let file_list = get_files_to_display()
            .into_iter()
            .map(|path| {
                let path_clone = path.clone();
                let on_delete = move |event: MouseEvent| {
                    event.stop_propagation();
                    delete_file(&path_clone);
                    run(true, false);
                };

                let path_clone = path.clone();
                let on_download = move |event: MouseEvent| {
                    event.stop_propagation();
                    let content = backend::FILES.with(|files| {
                        files
                            .borrow()
                            .iter()
                            .find(|(p, _)| *p == &path_clone)
                            .map(|(_, code)| code.clone())
                            .unwrap_or_default()
                    });
                    let doc = window().document().unwrap();
                    let anchor = doc.create_element("a").unwrap();
                    anchor
                        .set_attribute(
                            "href",
                            &format!(
                                "data:application/octet-stream;base64,{}",
                                STANDARD.encode(content)
                            ),
                        )
                        .unwrap();
                    anchor
                        .set_attribute("download", &path_clone.to_string_lossy())
                        .unwrap();
                    anchor.set_attribute("style", "display: none").unwrap();
                    doc.body().unwrap().append_child(&anchor).unwrap();
                    anchor.dyn_ref::<HtmlAnchorElement>().unwrap().click();
                    set_timeout(move || anchor.remove(), Duration::from_millis(0));
                };

                let path_clone = path.clone();
                let on_insert = move |_: MouseEvent| {
                    let content = backend::FILES.with(|files| {
                        files
                            .borrow()
                            .iter()
                            .find(|(p, _)| *p == &path_clone)
                            .map(|(_, code)| code.clone())
                            .unwrap_or_default()
                    });
                    state.update(|state| {
                        let to_insert = format_insert_file_code(&path_clone, content);
                        replace_code(state, &to_insert);
                    });
                    run(true, false);
                };

                let path = path.to_string_lossy().into_owned();
                view! {
                    <div class="pad-file-tab" on:click=on_insert>
                        {path}
                        <span
                            class="material-symbols-rounded pad-file-tab-button"
                            on:click=on_download
                        >
                            download
                        </span>
                        <span
                            class="material-symbols-rounded pad-file-tab-button"
                            on:click=on_delete
                        >
                            delete
                        </span>
                    </div>
                }
            })
            .collect_view();

        view! { <div class="pad-files">{file_list}</div> }
    };

    let (splitter_dragging, set_splitter_dragging) = create_signal(false);
    let (splitter_ratio, set_splitter_ratio) = create_signal(0.5);

    let start_dragging_splitter = move |event: MouseEvent| {
        event.prevent_default();
        event.stop_propagation();
        set_splitter_dragging.set(true);
    };

    let draggable_splitter_class = move || {
        if splitter_dragging.get() {
            "draggable-splitter active"
        } else {
            "draggable-splitter"
        }
    };

    let editor_style = move || {
        if !fullscreen_enabled.get() {
            return "".to_string();
        }

        let ratio = splitter_ratio.get();
        format!(
            "grid-template-columns: {}fr 0.25em {}fr",
            ratio,
            1.0 - ratio
        )
    };

    let mouse_move_listener =
        window_event_listener(leptos_dom::ev::mousemove, move |event: MouseEvent| {
            if splitter_dragging.get() {
                let mouse_x = event.client_x();
                let editor_width = editor_wrapper_element().client_width();
                let ratio = (mouse_x as f64 / editor_width as f64).clamp(0.1, 0.9);
                set_splitter_ratio.set(ratio);
            }
        });

    let mouse_up_listener = window_event_listener(leptos_dom::ev::mouseup, move |_: MouseEvent| {
        set_splitter_dragging.set(false);
    });

    on_cleanup(move || {
        mouse_move_listener.remove();
        mouse_up_listener.remove();
        listener.remove();
    });

    // Render
    view! { <div id="editor-wrapper">
        <div>
            <div id=editor_wrapper_id class=editor_class style=editor_style>
                {glyph_buttons_container}
                {file_tab_display}
                <div id="settings" style=settings_style>
                    <div id="settings-left">
                        <div title="The maximum number of seconds a program can run for">
                            "Exec limit:"
                            <input
                                type="number"
                                min="0.01"
                                max="1000000"
                                width="3em"
                                value=get_execution_limit
                                on:input=on_execution_limit_change
                            /> "s"
                        </div>
                        <div title="The maximum number of seconds of audio &ast will generate">
                            <Prim prim=Primitive::Sys(SysOp::AudioStream) />
                            " time:"
                            <input
                                type="number"
                                min="1"
                                max="600"
                                width="3em"
                                value=get_ast_time
                                on:input=on_ast_time_change
                            />
                            "s"
                        </div>
                        <div title="Place the cursor on the left of the current token when formatting">
                            "Format left:"
                            <input
                                type="checkbox"
                                checked=get_right_to_left
                                on:change=toggle_right_to_left
                            />
                        </div>
                        <div title="Automatically run pad links">
                            "Autorun links:"
                            <input type="checkbox" checked=get_autorun on:change=toggle_autorun />
                        </div>
                        <div title="Automatically play audio">
                            "Autoplay audio:"
                            <input type="checkbox" checked=get_autoplay on:change=toggle_autoplay />
                        </div>
                        <div title="Default format for displaying animation arrays">
                            "Animation:" <select on:change=on_select_animation_format>
                                <option value="GIF" selected={get_animation_format() == "GIF"}>
                                    "GIF"
                                </option>
                                <option value="APNG" selected={get_animation_format() == "APNG"}>
                                    "APNG"
                                </option>
                            </select>
                        </div>
                        <div title="Show experimental primitive glyphs">
                            "Show experimental:"
                            <input
                                type="checkbox"
                                checked=get_show_experimental
                                on:change=toggle_show_experimental
                            />
                        </div>
                        <div title="Run and format together">
                            "Run on format:"
                            <input
                                type="checkbox"
                                checked=get_run_on_format
                                on:change=toggle_run_on_format
                            />
                        </div>
                        <div title="Enable LGBTQ+ colors">
                            "🏳️‍🌈:"
                            <select on:change=on_select_gayness>
                                <option value={Gayness::Gray.str()} selected={get_gayness() == Gayness::Gray}>
                                    {Gayness::Gray.str()}
                                </option>
                                <option value={Gayness::None.str()} selected={get_gayness() == Gayness::None}>
                                    {Gayness::None.str()}
                                </option>
                                <option value={Gayness::Ally.str()} selected={get_gayness() == Gayness::Ally}>
                                    {Gayness::Ally.str()}
                                </option>
                                <option value={Gayness::VeryGay.str()} selected={get_gayness() == Gayness::VeryGay}>
                                    {Gayness::VeryGay.str()}
                                </option>
                            </select>
                        </div>
                        <div title="Color constant [r g b] bindings according to their value">
                            "RGB bindings:"
                            <input
                                type="checkbox"
                                checked=get_rgb_bindings
                                on:change=toggle_rgb_bindings
                            />
                        </div>
                        <div title="Show line values to the right of the code">
                            "Show values:"
                            <input
                                type="checkbox"
                                checked=get_inlay_values
                                on:change=toggle_inlay_values
                            />
                        </div>
                        <div>
                            "Args:" <select on:change=on_select_top_at_top>
                                <option value="false" selected=get_top_at_top()>
                                    "First at bottom"
                                </option>
                                <option value="true" selected=get_top_at_top()>
                                    "First at top"
                                </option>
                            </select>
                        </div>
                        <div>
                            "Font size:" <select on:change=on_select_font_size>
                                <option value="0.6em" selected=get_font_size() == "0.6em">
                                    "Scalar"
                                </option>
                                <option value="0.8em" selected=get_font_size() == "0.8em">
                                    "Small"
                                </option>
                                <option value="1em" selected=get_font_size() == "1em">
                                    "Normal"
                                </option>
                                <option value="1.2em" selected=get_font_size() == "1.2em">
                                    "Big"
                                </option>
                                <option value="1.4em" selected=get_font_size() == "1.4em">
                                    "Rank 3"
                                </option>
                            </select>
                        </div>
                        <div>
                            "Font:" <select on:change=on_select_font>
                                <option value="Uiua386" selected=get_font_name() == "Uiua386">
                                    {format!("{}386", lang())}
                                </option>
                                <option value="TerminusUiua_14" selected=get_font_name() == "TerminusUiua_14">
                                    "TerminusUiua 14"
                                </option>
                                <option value="TerminusUiua_16" selected=get_font_name() == "TerminusUiua_16">
                                    "TerminusUiua 16"
                                </option>
                                <option value="TerminusUiua_24" selected=get_font_name() == "TerminusUiua_24">
                                    "TerminusUiua 24"
                                </option>
                                <option value="Pixua" selected=get_font_name() == "Pixua">
                                    "Pixua"
                                </option>
                            </select>
                        </div>
                        <button on:click=download_code>"Download Code"</button>
                        <button on:click=copy_markdown_link>"Copy Markdown"</button>
                    </div>
                    <div id="settings-right">
                        <div style="display: flex; gap: 0.2em;">
                            {toggle_experimental_button}
                            <button class="info-button" data-title=EDITOR_SHORTCUTS disabled>
                                "🛈"
                            </button>
                        </div>
                        <div style="margin-right: 0.1em">
                            "Tokens: " {move || token_count.get()}
                        </div>
                    </div>
                </div>

                <div class="editor-area">
                    <div id=glyph_doc_id class="glyph-doc" style="display: none">
                        {move || glyph_doc.get()}
                        <div class="glyph-doc-ctrl-click">
                            "Shift+click for more info (Ctrl/⌘+click for new tab)"
                        </div>
                    </div>

                    <div id="code-right-side">
                        <button
                            id="glyphs-toggle-button"
                            class="editor-right-button"
                            data-title=show_glyphs_title
                            on:click=toggle_show_glyphs
                        >
                            {show_glyphs_icon}
                        </button>

                        <button
                            class="editor-right-button"
                            data-title=toggle_settings_title
                            on:click=toggle_settings_open
                        >
                            <span class="material-symbols-rounded">settings</span>
                        </button>

                        <button
                            class="editor-right-button"
                            data-title=copy_link_title
                            on:click=copy_link
                        >
                            <span class="material-symbols-rounded">link</span>
                        </button>

                        {(mode == EditorMode::Pad)
                            .then(|| {
                                Some(
                                    view! {
                                        <button
                                            class="editor-right-button"
                                            data-title="Upload file"
                                            on:click=upload_file_dialog
                                        >
                                            <span class="material-symbols-rounded">upload</span>
                                        </button>

                                        <button
                                            class="editor-right-button expand-editor-button"
                                            data-title=fullscreen_button_title
                                            on:click=toggle_fullscreen
                                        >
                                            {fullscreen_button_icon}
                                        </button>
                                    },
                                )
                            })}

                        {example_tracker_element}
                    </div>

                    <div
                        id=code_area_id
                        class="code-area"
                        style=format!("height: {}em;", code_height_em + 1.25 / 2.0)
                    >
                        <div id=code_outer_id class="code code-outer sized-code">
                            <div id=line_numbers_id class="line-numbers">
                                <div
                                    class="code-line hidden-line-height-sampler"
                                    node_ref=line_height_sampler_ref
                                >
                                    <span class="code-span">"0"</span>
                                </div>
                                {line_numbers}
                            </div>
                            <div class="code-and-overlay">
                                // ///////////////////////
                                // The text entry area //
                                // ///////////////////////
                                <textarea
                                    id=code_id
                                    class="code-entry"
                                    autocorrect="false"
                                    autocapitalize="off"
                                    spellcheck="false"
                                    translate="no"
                                    on:paste=code_paste
                                    on:input=code_input
                                    on:mousemove=code_mouse_move
                                    on:mouseleave=code_mouse_leave
                                    value=initial_code_str
                                ></textarea>
                                // ///////////////////////
                                <div id=overlay_id class="code-overlay">
                                    {move || gen_code_view(&code_id(), &overlay.get(), &hidden)}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>

                <div id=hover_id class="code-hover" />

                <div class="output-frame">
                    <div class="output-lines">
                        <div class="output-diagnostics sized-code">{move || diag_output.get()}</div>
                        <div class="output-wrapper">
                            <div id=format!("output-{id}") class="output sized-code">
                                {move || output.get()}
                                {move || {
                                    get_state
                                        .get()
                                        .challenge
                                        .as_ref()
                                        .map(|chal| {
                                            let intended = chal.intended_answer.clone();
                                            let click_intended = move |_| {
                                                get_state.get().set_code(&intended, Cursor::Ignore);
                                            };
                                            view! {
                                                <div>
                                                    <hr />
                                                    <button
                                                        class="glyph-button"
                                                        data-title="Show intended answer"
                                                        on:click=click_intended
                                                    >
                                                        "📖"
                                                    </button>
                                                    {chal
                                                        .best_answer
                                                        .clone()
                                                        .map(|ans| {
                                                            let click_ans = move |_| {
                                                                get_state.get().set_code(&ans, Cursor::Ignore);
                                                            };
                                                            view! {
                                                                <button
                                                                    class="glyph-button"
                                                                    data-title="Show idiomatic answer"
                                                                    on:click=click_ans
                                                                >
                                                                    "💡"
                                                                </button>
                                                            }
                                                        })}
                                                </div>
                                            }
                                        })
                                }}
                            </div>
                        </div>
                    </div>
                    <div id="code-buttons">
                        <button
                            class="code-button format-button run-format-button"
                            on:click=move |_| {
                                format(true, false);
                            }
                            data-title=" ctrl Enter - Format        \nshift Enter - Format and Run"
                        >
                            {"Format"}
                        </button>
                        <button
                            class="code-button"
                            on:click=move |_| run(get_run_on_format(), false)
                        >
                            {"Run"}
                        </button>
                        <button
                            id="prev-example"
                            class="code-button"
                            aria-label="Previous"
                            style=example_arrow_style
                            on:click=prev_example
                        >
                            <svg
                                xmlns="http://www.w3.org/2000/svg"
                                width="1em"
                                height="1em"
                                viewBox="0 0 24 24"
                                fill="none"
                                stroke="currentColor"
                                stroke-width="2"
                                stroke-linecap="round"
                                stroke-linejoin="round"
                            >
                                <path d="M19 12H5"/>
                                <path d="M11 18l-6-6 6-6"/>
                            </svg>
                        </button>
                        <button
                            id="next-example"
                            class=next_button_class
                            aria-label="Next"
                            style=example_arrow_style
                            on:click=next_example
                        >
                            <svg
                                xmlns="http://www.w3.org/2000/svg"
                                width="1em"
                                height="1em"
                                viewBox="0 0 24 24"
                                fill="none"
                                stroke="currentColor"
                                stroke-width="2"
                                stroke-linecap="round"
                                stroke-linejoin="round"
                            >
                                <path d="M5 12h14"/>
                                <path d="M13 6l6 6-6 6"/>
                            </svg>
                        </button>
                    </div>
                </div>

                <div class=draggable_splitter_class on:mousedown=start_dragging_splitter></div>

                {move || {
                    let message = drag_message.get();
                    (!message.is_empty()).then(|| view! { <div id="drag-message">{message}</div> })
                }}
            </div>
            <div id="editor-help">
                {help.iter().map(|s| view! { <p>{s}</p> }).collect::<Vec<_>>()}
            </div>
            <input
                id=input_id
                type="file"
                multiple="multiple"
                style="display: none"
                on:change=files_selected
            />
        </div>
        { (!kala.is_empty()).then(||
            view!(<img src=format!("/assets/kala/{kala}.png") class="editor-side-icon"/>)
        ) }
    </div> }
}

#[component]
pub fn Prim(
    prim: Primitive,
    #[prop(optional)] glyph_only: bool,
    #[prop(optional)] hide_docs: bool,
) -> impl IntoView {
    let symbol_class = format!("prim-glyph {}", prim_class(prim));
    let symbol = prim.to_string();
    let name = if !glyph_only && symbol != prim.name() {
        format!(" {}", prim.name())
    } else {
        "".to_string()
    };
    let href = format!("/docs/{}", prim.name());
    let mut title = String::new();
    if let Some(ascii) = prim.ascii() {
        title.push_str(&format!("({ascii})"));
    }
    if prim.glyph().is_some() && glyph_only {
        if !title.is_empty() {
            title.push(' ');
        }
        title.push_str(prim.name());
    }
    if let Primitive::Sys(op) = prim {
        title.push_str(op.long_name());
        title.push(':');
        title.push('\n');
    }
    if !hide_docs {
        let doc = PrimDoc::from(prim);
        if glyph_only && !title.is_empty() && !matches!(prim, Primitive::Sys(_)) {
            title.push_str(": ");
        }
        title.push_str(&doc.short_text());
    }
    if title.is_empty() {
        view! {
            <a href=href class="prim-code-a">
                <code>
                    <span class=symbol_class>{symbol}</span>
                    {name}
                </code>
            </a>
        }
    } else {
        view! {
            <a href=href class="prim-code-a">
                <code class="prim-code" data-title=title>
                    <span class=symbol_class>{symbol}</span>
                    {name}
                </code>
            </a>
        }
    }
}

pub fn prim_class(prim: Primitive) -> &'static str {
    prim_sig_class(prim, None)
}

macro_rules! code_font {
    ($class:literal) => {
        concat!("code-font ", $class)
    };
}

pub(crate) use code_font;

fn number_class() -> &'static str {
    match get_gayness() {
        Gayness::VeryGay => "text-gradient number-lesbian",
        Gayness::Gray => "",
        _ => "number-literal",
    }
}

fn string_class() -> &'static str {
    match get_gayness() {
        Gayness::VeryGay => "text-gradient bright-rainbow",
        Gayness::Gray => "",
        _ => "string-literal-span",
    }
}

fn module_class() -> &'static str {
    match get_gayness() {
        Gayness::Gray => "",
        _ => "module",
    }
}

fn comment_class() -> &'static str {
    match get_gayness() {
        Gayness::VeryGay => "text-gradient graynbow",
        _ => "comment-span",
    }
}

fn sig_class(sig: Signature) -> &'static str {
    if get_gayness() == Gayness::Gray {
        return code_font!("");
    }
    match sig.args() {
        0 => code_font!("noadic-function"),
        1 if very_gay() => code_font!("text-gradient monadic-aro"),
        1 => code_font!("monadic-function"),
        2 if very_gay() => code_font!("text-gradient dyadic-gay"),
        2 => code_font!("dyadic-function"),
        3 => code_font!("triadic-function"),
        4 => code_font!("tetradic-function"),
        5 => code_font!("pentadic-function"),
        _ => code_font!("hexadic-function"),
    }
}

fn modifier_class(margs: usize) -> &'static str {
    if get_gayness() == Gayness::Gray {
        return code_font!("");
    }
    match margs {
        0 | 1 if very_gay() => code_font!("text-gradient monadic-pan"),
        0 | 1 => code_font!("monadic-modifier"),
        2 if very_gay() => code_font!("text-gradient dyadic-fluid"),
        2 => code_font!("dyadic-modifier"),
        _ => code_font!("triadic-modifier"),
    }
}

fn prim_sig_class(prim: Primitive, subscript: Option<&Subscript>) -> &'static str {
    if get_gayness() == Gayness::Gray {
        return code_font!("");
    }
    match prim {
        Primitive::Identity => code_font!("stack-function"),
        Primitive::Transpose if at_least_a_little_gay() => {
            code_font!("monadic-function trans text-gradient")
        }
        Primitive::Both if at_least_a_little_gay() => {
            match subscript.and_then(Subscript::n).unwrap_or(2) {
                0 => code_font!("monadic-function aroace text-gradient"),
                1 => code_font!("monadic-function aro text-gradient"),
                2 => code_font!("monadic-modifier bi text-gradient"),
                _ => code_font!("dyadic-function pan text-gradient"),
            }
        }
        Primitive::Couple => match subscript.and_then(Subscript::n).unwrap_or(2) {
            0 if at_least_a_little_gay() => {
                code_font!("monadic-function aroace text-gradient")
            }
            1 if at_least_a_little_gay() => code_font!("monadic-function aro text-gradient"),
            2 => sig_class((2, 1).into()),
            3 if very_gay() => code_font!("dyadic-function poly text-gradient"),
            n if n > 3 && at_least_a_little_gay() => {
                code_font!("dyadic-function poly text-gradient")
            }
            n => sig_class((n.unsigned_abs() as usize, 1).into()),
        },
        Primitive::Rand if very_gay() => code_font!("text-gradient random"),
        Primitive::IndexOf => code_font!("text-gradient caution"),
        prim if matches!(prim.class(), PrimClass::Arguments | PrimClass::Debug)
            && prim.modifier_args().is_none() =>
        {
            code_font!("stack-function")
        }
        prim if prim.class() == PrimClass::Constant && very_gay() => {
            code_font!("text-gradient number-lesbian")
        }
        prim if prim.class() == PrimClass::Constant => code_font!("number-literal"),
        prim => {
            if let Some(m) = prim.subscript_margs(subscript) {
                modifier_class(m)
            } else {
                prim.subscript_sig(subscript)
                    .or(prim.sig())
                    .map(sig_class)
                    .unwrap_or(code_font!(""))
            }
        }
    }
}

pub fn binding_name_class(name: &str) -> Option<&'static str> {
    Some(match name {
        "Trans" | "Transgender" => code_font!("trans text-gradient"),
        "Bi" | "Bisexual" => code_font!("bi text-gradient"),
        "Pan" | "Pansexual" => code_font!("pan text-gradient"),
        "Rainbow" | "LGBT" | "Lgbt" => code_font!("rainbow text-gradient"),
        "Gay" => code_font!("gay text-gradient"),
        "Lesbian" => code_font!("lesbian text-gradient"),
        "Ace" | "Asexual" => code_font!("ace text-gradient"),
        "Aro" | "Aromantic" => code_font!("aro text-gradient"),
        "AroAce" => code_font!("aroace text-gradient"),
        "Agender" => code_font!("agender text-gradient"),
        "Nb" | "Enby" | "Nonbinary" | "NonBinary" => code_font!("nb text-gradient"),
        "Fluid" | "Genderfluid" | "GenderFluid" => code_font!("fluid text-gradient"),
        "Queer" | "Genderqueer" | "GenderQueer" => code_font!("queer text-gradient"),
        "Poly" | "Polyamorous" => code_font!("poly text-gradient"),
        _ => return None,
    })
}

fn binding_class(name: &str, docs: &BindingDocs) -> &'static str {
    binding_name_class(name).unwrap_or_else(|| match docs.kind {
        BindingDocsKind::Constant(_) => code_font!(""),
        BindingDocsKind::Function { sig, .. } => sig_class(sig),
        BindingDocsKind::Modifier(margs) => modifier_class(margs),
        BindingDocsKind::Module { .. } => code_font!("module"),
        BindingDocsKind::Error => code_font!("output-error"),
    })
}

fn binding_style(docs: &BindingDocs) -> String {
    if !get_rgb_bindings() {
        return String::new();
    }
    if let BindingDocsKind::Constant(Some(val)) = &docs.kind
        && let Ok(nums) = val.as_nums(&IgnoreError, "")
        && let [r, g, b] = *nums
        && [r, g, b].iter().all(|c| (0.0..=1.0).contains(c))
    {
        return format!(
            "color: rgb({}, {}, {})",
            (r * 255.0).round(),
            (g * 255.0).round(),
            (b * 255.0).round(),
        );
    }
    String::new()
}

pub const EDITOR_SHORTCUTS: &str = " shift Enter   - Run + Format
ctrl/⌘ Click   - Open glyph docs
ctrl/⌘ /       - Toggle line comment
ctrl/⌘ 4       - Toggle multiline string
   alt Up/Down - Swap lines
 shift Delete  - Delete lines
ctrl/⌘ Z       - Undo
ctrl/⌘ Y       - Redo
ctrl/⌘ E       - Insert # Experimental! comment
ctrl/⌘ shift E - Remove # Experimental! comment";

pub fn replace_lang_name() -> bool {
    cfg!(target_arch = "wasm32") && its_called_weewuh()
}

pub fn lang() -> &'static str {
    #[cfg(target_arch = "wasm32")]
    if its_called_weewuh() {
        "Weewuh"
    } else {
        "Uiua"
    }
    #[cfg(not(target_arch = "wasm32"))]
    "Uiua"
}
