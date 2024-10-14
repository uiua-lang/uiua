pub mod utils;
pub mod backend;

use std::{cell::Cell, iter::repeat, mem::take, path::PathBuf, time::Duration};

use base64::engine::{general_purpose::STANDARD, Engine};

use ev::mousemove;
use leptos::{
    ev::{keydown, keyup},
    *,
};

use leptos_router::{use_navigate, BrowserIntegration, History, LocationChange, NavigateOptions};
use uiua::{
    format::{format_str, FormatConfig},
    is_ident_char, lex, now, seed_random, Primitive, SysOp, Token,
    PrimClass, Signature,
    lsp::{BindingDocs, BindingDocsKind},
};
use wasm_bindgen::{closure::Closure, JsCast, JsValue};
use web_sys::{
    DragEvent, Event, FileReader, HtmlAnchorElement, HtmlDivElement, HtmlInputElement,
    HtmlSelectElement, HtmlTextAreaElement, MouseEvent,
};

use utils::*;
use utils::{get_ast_time, element};

use js_sys::Date;
use std::sync::OnceLock;
use backend::{OutputItem, drop_file};

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
    #[prop(optional)] mode: EditorMode,
    #[prop(optional)] help: &'a [&'a str],
    #[prop(optional)] no_run: bool,
    #[prop(optional)] challenge: Option<ChallengeDef>,
    #[prop(optional)] nonprogressive: bool,
    #[prop(optional)] examples: Option<Vec<String>>,
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
        EditorMode::Showcase => examples.unwrap_or(vec![]),
        _ => vec![example.into()],
    };
    let code_max_lines = if let EditorMode::Pad = mode {
        10
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

    let code_id = move || format!("code{id}");
    let code_outer_id = move || format!("code-outer{id}");
    let overlay_id = move || format!("overlay{id}");
    let glyph_doc_id = move || format!("glyphdoc{id}");
    let hover_id = move || format!("hover{id}");

    let code_element = move || -> HtmlTextAreaElement { element(&code_id()) };
    #[allow(unused)]
    let code_outer_element = move || -> HtmlDivElement { element(&code_outer_id()) };
    let glyph_doc_element = move || -> HtmlDivElement { element(&glyph_doc_id()) };

    // Track line count
    let (line_count, set_line_count) = create_signal(1);

    let initial_code_str = examples.first().cloned().unwrap_or_else(|| example.into());
    let (initial_code, set_initial_code) = create_signal(Some(initial_code_str.clone()));

    let (example, set_example) = create_signal(0);
    let (diag_output, set_diag_output) = create_signal(View::default());
    let (output, set_output) = create_signal(View::default());
    let (token_count, set_token_count) = create_signal(0);

    // let code_text = move || code_text(&code_id());
    let get_code_cursor = move || get_code_cursor(&code_id());
    let (copied_link, set_copied_link) = create_signal(false);
    let (settings_open, set_settings_open) = create_signal(false);
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

    // Initialize the state
    let state = State {
        code_id: code_id(),
        code_outer_id: code_outer_id(),
        set_overlay,
        set_line_count,
        set_copied_link,
        past: Vec::new(),
        future: Vec::new(),
        challenge,
        loading_module: false,
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
        let code = get_code();
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
            (window().document().unwrap()).set_title(&format!("Uiua - {title}"));
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

        // Run code
        set_output.set(view!(<div class="running-text">"Running"</div>).into_view());
        let allow_autoplay = !matches!(mode, EditorMode::Example) && get_autoplay();
        let render_output_item = move |item| match item {
            OutputItem::String(s) => {
                if s.is_empty() {
                    view!(<div class="output-item"><br/></div>).into_view()
                } else {
                    view!(<div class="output-item">{s}</div>).into_view()
                }
            }
            OutputItem::Classed(class, s) => {
                let class = format!("output-item {class}");
                view!(<div class=class>{s}</div>).into_view()
            }
            OutputItem::Faint(s) => {
                view!(<div class="output-item output-fainter">{s}</div>).into_view()
            }
            OutputItem::Image(bytes, label) => {
                let encoded = STANDARD.encode(bytes);
                view!(<div class="output-media-wrapper">
                    <div class="output-image-label">{label}</div>
                    <img class="output-image" src={format!("data:image/png;base64,{encoded}")} />
                </div>)
                .into_view()
            }
            OutputItem::Gif(bytes, label) => {
                let encoded = STANDARD.encode(bytes);
                view!(<div class="output-media-wrapper">
                    <div class="output-image-label">{label}</div>
                    <img class="output-image" src={format!("data:image/gif;base64,{encoded}")} />
                </div>)
                .into_view()
            }
            OutputItem::Audio(bytes, label) => {
                let encoded = STANDARD.encode(bytes);
                let src = format!("data:audio/wav;base64,{}", encoded);
                let label = label.map(|s| format!("{s}:"));
                if allow_autoplay {
                    view!(<div class="output-media-wrapper">
                        <div class="output-item output-audio-label">{label}</div>
                        <audio class="output-audio" controls autoplay src=src/>
                    </div>)
                    .into_view()
                } else {
                    view!(<div class="output-media-wrapper">
                        <div class="output-item output-audio-label">{label}</div>
                        <audio class="output-audio" controls src=src/>
                    </div>)
                    .into_view()
                }
            }
            OutputItem::Svg(s) => view!(<div><img
                    class="output-image"
                    src={format!("data:image/svg+xml;utf8, {}", urlencoding::encode(&s))}/>
                </div>)
            .into_view(),
            OutputItem::Report(report) => report_view(&report).into_view(),
            OutputItem::Separator => view!(<div class="output-item"><hr/></div>).into_view(),
        };
        set_timeout(
            move || {
                state.update(|st| {
                    seed_random(seed);
                    let output = st.run_code(&input);
                    if take(&mut st.loading_module) {
                        set_timeout(
                            move || {
                                state.update(|state| {
                                    seed_random(seed);
                                    let output = state.run_code(&input);
                                    let (diags, items): (Vec<_>, Vec<_>) =
                                        output.into_iter().partition(OutputItem::is_report);
                                    let items: Vec<_> =
                                        items.into_iter().map(render_output_item).collect();
                                    let diags: Vec<_> =
                                        diags.into_iter().map(render_output_item).collect();
                                    set_output.set(items.into_view());
                                    set_diag_output.set(diags.into_view());
                                });
                            },
                            Duration::from_millis(200),
                        );
                    } else {
                        let (diags, items): (Vec<_>, Vec<_>) =
                            output.into_iter().partition(OutputItem::is_report);
                        let items: Vec<_> = items.into_iter().map(render_output_item).collect();
                        let diags: Vec<_> = diags.into_iter().map(render_output_item).collect();
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
        let inserted = inserted.replace('\r', "");
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
            if code.starts_with("# Experimental!") {
                return;
            }
            let new_code = format!("# Experimental!\n{}", code);
            let cursor = if let Some((start, end)) = get_code_cursor() {
                if start == 0 {
                    Cursor::Set(16, 16)
                } else {
                    Cursor::Set(start + 16, end + 16)
                }
            } else {
                Cursor::Ignore
            };
            state.set_code(&new_code, cursor);
        });
    };

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

        if key == "Control" && !on_mac() || key == "Meta" && on_mac() {
            if let Some(overlay_element) = get_element::<HtmlDivElement>(&overlay_id()) {
                overlay_element
                    .style()
                    .set_property("pointer-events", "all")
                    .unwrap();
            }
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
                        let (start_line, _) = line_col(&code, start as usize);
                        let curr_line = code.lines().nth(start_line - 1).unwrap_or_default();
                        let curr_line_indent =
                            curr_line.chars().take_while(|c| c.is_whitespace()).count();
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
                                    Some(format!("\n{}", line))
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
            "Tab" => {
                state.update(|state| replace_code(state, "  "));
            }
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
            // Insert # Experimental! comment
            "e" if os_ctrl(event) => insert_experimental(),
            // Toggle line comment
            "/" | "4" if os_ctrl(event) => {
                state.update(|state| {
                    let code = get_code();
                    let (start, end) = get_code_cursor().unwrap();
                    let (start, end) = (start.min(end), start.max(end));
                    let (start_line, _) = line_col(&code, start as usize);
                    let (end_line, _) = line_col(&code, end as usize);
                    let mut lines: Vec<String> = code.split('\n').map(Into::into).collect();
                    let range = &mut lines[start_line - 1..end_line];
                    let prefix = if key == "/" { '#' } else { '$' };
                    if range.iter().all(|line| line.trim().starts_with(prefix)) {
                        // Toggle comments off
                        for line in range {
                            let space_count = line.chars().take_while(|c| *c == ' ').count();
                            *line = repeat(' ')
                                .take(space_count)
                                .chain(
                                    line.trim()
                                        .trim_start_matches(prefix)
                                        .trim_start_matches(' ')
                                        .chars(),
                                )
                                .collect();
                        }
                    } else {
                        // Toggle comments on
                        for line in range {
                            let spot = line.chars().take_while(|c| " \t".contains(*c)).count();
                            line.insert(spot, ' ');
                            line.insert(spot, prefix);
                        }
                    }
                    let new_code = lines.join("\n");
                    state.set_code(&new_code, Cursor::Set(start, end));
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
                        .map_or(true, |c| c.is_whitespace() || "(){}[]".contains(c));
                    let at_behind =
                        code.chars().nth((start as usize).saturating_sub(1)) == Some('@');
                    if (start != end || can_couple) && !at_behind {
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
                        .map_or(true, |c| c.is_whitespace() || "(){}[]".contains(c));
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
            "ArrowLeft" | "ArrowRight" if event.alt_key() => {}
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

    // Glyph hover doc
    let (glyph_doc, set_glyph_doc) = create_signal(View::default());
    let onmouseleave = move |_| {
        _ = glyph_doc_element().style().set_property("display", "none");
    };

    // Glyph buttons
    // These are the buttons that appear above the editor and allow the user to insert glyphs
    let make_glyph_button = |prim: Primitive| {
        let text = prim
            .glyph()
            .map(Into::into)
            .or_else(|| prim.ascii().map(|s| s.to_string()))?;
        let mut title = prim.name().to_string();
        if let Some(ascii) = prim.ascii() {
            title = format!("({}) {}", ascii, title);
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
        let onmouseover = move |_| {
            set_glyph_doc.set(
                view! {
                    <Prim prim=prim/>
                    { prim.is_experimental().then(||
                        view! {
                            <span class="experimental" style="font-size: 0.8em;">
                                "⚠️ Experimental"
                            </span>
                        }
                    ) }
                    <br/>
                    { prim.doc().short_text().into_owned() }
                }
                .into_view(),
            );
            _ = glyph_doc_element().style().remove_property("display");
        };
        let mut class = "glyph-button glyph-title".to_string();
        if prim.is_experimental() {
            class.push_str(" experimental-glyph-button");
        }
        Some(
            view! {
                <button
                    class=class
                    data-title=title
                    on:click=onclick
                    on:mouseover=onmouseover
                    on:mouseleave=onmouseleave>
                    <div class={prim_class(prim)}>{ text }</div>
                </button>
            }
            .into_view(),
        )
    };
    let mut glyph_buttons: Vec<_> = Primitive::non_deprecated()
        .filter_map(make_glyph_button)
        .collect();

    // Additional code buttons
    for (glyph, title, class, surround, doc) in [
        (
            "_",
            "strand",
            "strand-span",
            None,
            "tutorial/arrays#creating-arrays",
        ),
        (
            "[]",
            "array",
            "",
            Some(('[', ']')),
            "tutorial/arrays#creating-arrays",
        ),
        (
            "{}",
            "box array",
            "",
            Some(('{', '}')),
            "tutorial/arrays#nested-arrays",
        ),
        (
            "()",
            "function",
            "",
            Some(('(', ')')),
            "tutorial/functions#inline-functions",
        ),
        ("¯", "(`) negative", "number-literal", None, ""),
        (
            "@",
            "character",
            "string-literal-span",
            None,
            "tutorial/types#characters",
        ),
        (
            "$",
            "format/multiline string",
            "string-literal-span",
            None,
            "tutorial/functions#format-strings",
        ),
        (
            "\"",
            "string",
            "string-literal-span",
            Some(('"', '"')),
            "tutorial/types#characters",
        ),
        ("!", "macro", "", None, "tutorial/macros"),
        ("^", "placeholder", "", None, "tutorial/custommodifiers"),
        ("←", "(=) binding", "", None, "tutorial/bindings"),
        (
            "↚",
            "(=~) private binding",
            "",
            None,
            "tutorial/modules#visibility",
        ),
        ("~", "module", "", None, "tutorial/modules"),
        (
            "|",
            "signature",
            "",
            None,
            "tutorial/functions#stack-signatures",
        ),
        (
            "#",
            "comment",
            "comment-span",
            None,
            "tutorial/basic#comments",
        ),
    ] {
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
        let mut desc = prim.doc().short_text().to_string();
        if desc.chars().count() > 30 {
            desc = desc.chars().take(29).chain(['…']).collect();
        }
        let text = format!("{:max_name_len$} - {desc}", prim.name()).replace(' ', " ");
        options.push(view!(<option
            class=class
            value={prim.name()}>
            {text}
        </option>));
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
                on:change=additional_on_change>
                <option value="" disabled=true selected=true>"…"</option>
                { options }
            </select>
        }
        .into_view(),
    );

    // Select a class for the editor and code area
    let editor_class = match mode {
        EditorMode::Example => "small-editor",
        EditorMode::Showcase | EditorMode::Pad => "medium-editor",
    };

    // Hide the example arrows if there is only one example
    let example_arrow_style = if examples.len() <= 1 {
        "display:none"
    } else {
        ""
    };

    // Show or hide the glyph buttons
    let (show_glyphs, set_show_glyphs) = create_signal(match mode {
        EditorMode::Example => false,
        EditorMode::Showcase | EditorMode::Pad => true,
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
    let listener = window_event_listener(leptos_dom::ev::drop, move |event: DragEvent| {
        let event = event.dyn_into::<web_sys::DragEvent>().unwrap();
        event.prevent_default();
        event.stop_propagation();
        let files = event.data_transfer().unwrap().files().unwrap();
        if files.length() == 0 {
            return;
        }
        let file = files.get(0).unwrap();
        let file_name = file.name();
        let reader = FileReader::new().unwrap();
        reader.read_as_array_buffer(&file).unwrap();
        let on_load = Closure::wrap(Box::new(move |event: Event| {
            // Log file contents
            let event = event.dyn_into::<web_sys::ProgressEvent>().unwrap();
            let reader = event.target().unwrap().dyn_into::<FileReader>().unwrap();
            let bytes = reader
                .result()
                .unwrap()
                .dyn_into::<js_sys::ArrayBuffer>()
                .unwrap();
            let bytes = js_sys::Uint8Array::new(&bytes);
            let byte_count = bytes.length();
            let path = PathBuf::from(&file_name);
            drop_file(path.clone(), bytes.to_vec());
            set_drag_message.set("");
            state.update(|state| {
                let code = get_code();
                if code.trim().is_empty() {
                    let function = if path.extension().is_some_and(|ext| ext == "ua") {
                        "~"
                    } else if path
                        .extension()
                        .map_or(true, |ext| ["txt", "md"].iter().any(|e| e == &ext))
                    {
                        "&fras"
                    } else {
                        "&frab"
                    };
                    state.set_code(
                        &if byte_count < 10000 {
                            format!("{function} {file_name:?}\n")
                        } else {
                            format!("# {byte_count} bytes\n# {function} {file_name:?}\n")
                        },
                        Cursor::Ignore,
                    )
                }
            });
            run(true, false);
        }) as Box<dyn FnMut(_)>);
        reader
            .add_event_listener_with_callback("load", on_load.as_ref().unchecked_ref())
            .unwrap();
        on_load.forget();
    });

    on_cleanup(move || listener.remove());

    // Line numbers
    let line_numbers = move || {
        (0..line_count.get().max(1))
            .map(|i| {
                view!( <div class="code-line">
                    <span class="code-span">{i + 1}</span>
                </div>)
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
    let on_select_top_at_top = move |event: Event| {
        let input: HtmlSelectElement = event.target().unwrap().dyn_into().unwrap();
        let orientation = input.value() == "true";
        set_top_at_top(orientation);
        run(false, false);
    };
    set_font_name(&get_font_name());
    set_font_size(&get_font_size());
    let on_insert_experimental = move |_| insert_experimental();

    // Render
    view! {
        <div id="editor-wrapper">
            <div id="editor">
                <div style=glyph_buttons_style>
                    <div class="glyph-buttons">{glyph_buttons}</div>
                </div>
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
                                on:input=on_execution_limit_change/>
                            "s"
                        </div>
                        <div title="The maximum number of seconds of audio &ast will generate">
                            <Prim prim=Primitive::Sys(SysOp::AudioStream) />" time:"
                            <input
                                type="number"
                                min="1"
                                max="600"
                                width="3em"
                                value=get_ast_time
                                on:input=on_ast_time_change/>
                            "s"
                        </div>
                        <div title="Place the cursor on the left of the current token when formatting">
                            "Format left:"
                            <input
                                type="checkbox"
                                checked=get_right_to_left
                                on:change=toggle_right_to_left/>
                        </div>
                        <div title="Automatically run pad links">
                            "Autorun links:"
                            <input
                                type="checkbox"
                                checked=get_autorun
                                on:change=toggle_autorun/>
                        </div>
                        <div title="Automatically play audio">
                            "Autoplay audio:"
                            <input
                                type="checkbox"
                                checked=get_autoplay
                                on:change=toggle_autoplay/>
                        </div>
                        <div title="Show experimental primitive glyphs">
                            "Show experimental:"
                            <input
                                type="checkbox"
                                checked=get_show_experimental
                                on:change=toggle_show_experimental/>
                        </div>
                        <div title="Run and format together">
                            "Run on format:"
                            <input
                                type="checkbox"
                                checked=get_run_on_format
                                on:change=toggle_run_on_format/>
                        </div>
                        <div>
                            "Stack:"
                            <select
                                on:change=on_select_top_at_top>
                                <option value="false" selected=get_top_at_top()>"Top at bottom"</option>
                                <option value="true" selected=get_top_at_top()>"Top at top"</option>
                            </select>
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
                                <option value="DejaVuSansMono" selected={get_font_name() == "DejaVuSansMono"}>"DejaVu"</option>
                                <option value="Uiua386" selected={get_font_name() == "Uiua386"}>"Uiua386"</option>
                            </select>
                        </div>
                        <button on:click=download_code>"Download Code"</button>
                        <button on:click=copy_markdown_link>"Copy Markdown"</button>
                    </div>
                    <div id="settings-right">
                        <div style="display: flex; gap: 0.2em;">
                            <button
                                class="info-button"
                                data-title="Add # Experimental"
                                on:click=on_insert_experimental>
                                "🧪"
                            </button>
                            <button
                                class="info-button"
                                data-title=EDITOR_SHORTCUTS
                                disabled>
                                "🛈"
                            </button>
                        </div>
                        <div style="margin-right: 0.1em">
                            "Tokens: "
                            { move || token_count.get() }
                        </div>
                    </div>
                </div>
                <div class=editor_class>
                    <div id="code-area">
                        <div id={glyph_doc_id} class="glyph-doc" style="display: none">
                            { move || glyph_doc.get() }
                            <div class="glyph-doc-ctrl-click">"Shift+click for more info (Ctrl/⌘+click for new tab)"</div>
                        </div>
                        <div
                            id=code_outer_id
                            class="code code-outer sized-code"
                            style={format!("height: {}em;", code_height_em + 1.25 / 2.0)}>
                            <div class="line-numbers">
                                { line_numbers }
                            </div>
                            <div class="code-and-overlay">
                                /////////////////////////
                                // The text entry area //
                                /////////////////////////
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
                                    value=initial_code_str>
                                </textarea>
                                /////////////////////////
                                <div
                                    id=overlay_id
                                    class="code-overlay">
                                    { move || gen_code_view(&overlay.get()) }
                                </div>
                            </div>
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
                    </div>
                    <div id=hover_id class="code-hover"/>
                    <div class="output-frame">
                        <div class="output-lines">
                            <div class="output-diagnostics">
                                { move || diag_output.get() }
                            </div>
                            <div class="output-wrapper">
                                <div id=format!("output-{id}") class="output sized-code">
                                    { move || output.get() }
                                    { move || get_state.get().challenge.as_ref().map(|chal| {
                                        let intended = chal.intended_answer.clone();
                                        let click_intended = move|_| {
                                            get_state.get().set_code(&intended, Cursor::Ignore);
                                        };
                                        view! {
                                        <div>
                                            <hr/>
                                            <button
                                                class="glyph-button"
                                                data-title="Show intended answer"
                                                on:click=click_intended>
                                                "📖"
                                            </button>
                                            {chal.best_answer.clone().map(|ans| {
                                                let click_ans = move|_| {
                                                    get_state.get().set_code(&ans, Cursor::Ignore);
                                                };
                                                view! {
                                                    <button
                                                        class="glyph-button"
                                                        data-title="Show idiomatic answer"
                                                        on:click=click_ans>
                                                        "💡"
                                                    </button>
                                                }
                                            })}
                                        </div>
                                    }})}
                                </div>
                            </div>
                        </div>
                        <div id="code-buttons">
                            <button
                                class="code-button format-button run-format-button"
                                on:click=move |_| {format(true, false);}
                                data-title=" ctrl Enter - Format        \nshift Enter - Format and Run"
                            >{ "Format" }</button>
                            <button class="code-button" on:click=move |_| run(get_run_on_format(), false)>{ "Run" }</button>
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
                { move || {
                    let message = drag_message.get();
                    (!message.is_empty()).then(|| view!(<div id="drag-message">{ message }</div>))
                } }
            </div>
            <div id="editor-help">
                { help.iter().map(|s| view!(<p>{s}</p>)).collect::<Vec<_>>() }
            </div>
        </div>
    }
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
        title.push_str(&format!("({})", ascii));
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
        let doc = prim.doc();
        if glyph_only && !title.is_empty() && !matches!(prim, Primitive::Sys(_)) {
            title.push_str(": ");
        }
        title.push_str(&doc.short_text());
    }
    if title.is_empty() {
        view! {
            <a href=href class="prim-code-a">
                <code><span class=symbol_class>{ symbol }</span>{name}</code>
            </a>
        }
    } else {
        view! {
            <a href=href class="prim-code-a">
                <code class="prim-code" data-title=title><span class=symbol_class>{ symbol }</span>{name}</code>
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

fn sig_class(sig: Signature) -> &'static str {
    match sig.args {
        0 => code_font!("noadic-function"),
        1 => code_font!("monadic-function"),
        2 => code_font!("dyadic-function"),
        3 => code_font!("triadic-function"),
        4 => code_font!("tetradic-function"),
        _ => code_font!(""),
    }
}

fn prim_sig_class(prim: Primitive, sig: Option<Signature>) -> &'static str {
    match prim {
        Primitive::Identity => code_font!("stack-function"),
        Primitive::Transpose => code_font!("monadic-function trans text-gradient"),
        Primitive::Both => code_font!("monadic-modifier bi text-gradient"),
        Primitive::Member => code_font!("dyadic-function caution text-gradient"),
        Primitive::Couple => match sig.map(|sig| sig.args) {
            None | Some(2) => code_font!("dyadic-function"),
            Some(0) => code_font!("monadic-function aroace text-gradient"),
            Some(1) => code_font!("monadic-function aro text-gradient"),
            Some(_) => code_font!("dyadic-function poly text-gradient"),
        },
        prim if matches!(prim.class(), PrimClass::Stack | PrimClass::Debug)
            && prim.modifier_args().is_none() =>
        {
            code_font!("stack-function")
        }
        prim if prim.class() == PrimClass::Constant => code_font!("number-literal"),
        prim => {
            if let Some(m) = prim.modifier_args() {
                match m {
                    0 | 1 => code_font!("monadic-modifier"),
                    2 => code_font!("dyadic-modifier"),
                    _ => code_font!("triadic-modifier"),
                }
            } else {
                sig.or(prim.signature())
                    .map(sig_class)
                    .unwrap_or(code_font!(""))
            }
        }
    }
}

fn binding_class(name: &str, docs: &BindingDocs) -> &'static str {
    match name {
        "Trans" | "Transgender" => code_font!("trans text-gradient"),
        "Bi" | "Bisexual" => code_font!("bi text-gradient"),
        "Pan" | "Pansexual" => code_font!("pan text-gradient"),
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
        _ => match docs.kind {
            BindingDocsKind::Constant(_) => code_font!(""),
            BindingDocsKind::Function { sig, .. } => sig_class(sig),
            BindingDocsKind::Modifier(margs) => match margs {
                1 => code_font!("monadic-modifier"),
                2 => code_font!("dyadic-modifier"),
                _ => code_font!("triadic-modifier"),
            },
            BindingDocsKind::Module { .. } => code_font!("module"),
        },
    }
}

pub const EDITOR_SHORTCUTS: &str = " shift Enter   - Run + Format
  hold ctrl/⌘  - Show hovered glyph's name
ctrl/⌘ Click   - Open glyph docs
ctrl/⌘ /       - Toggle line comment
ctrl/⌘ 4       - Toggle multiline string
   alt Up/Down - Swap lines
 shift Delete  - Delete lines
ctrl/⌘ Z       - Undo
ctrl/⌘ Y       - Redo
ctrl/⌘ E       - Insert # Experimental! comment";