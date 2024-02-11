use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    iter,
    mem::{replace, take},
    str::FromStr,
    time::Duration,
};

use base64::engine::{general_purpose::URL_SAFE, Engine};
use image::ImageOutputFormat;
use leptos::*;

use uiua::{
    ast::Item, image_to_bytes, spans, value_to_gif_bytes, value_to_image, value_to_wav_bytes,
    Compiler, DiagnosticKind, Inputs, Report, ReportFragment, ReportKind, SpanKind, SysBackend,
    Uiua, UiuaResult, Value,
};
use wasm_bindgen::JsCast;
use web_sys::{HtmlBrElement, HtmlDivElement, HtmlStyleElement, Node};

use crate::{
    backend::{OutputItem, WebBackend},
    binding_class,
    editor::Editor,
    element, prim_class,
};

/// Handles setting the code in the editor, setting the cursor, and managing the history
pub struct State {
    pub code_id: String,
    pub set_line_count: WriteSignal<usize>,
    pub set_copied_link: WriteSignal<bool>,
    pub past: RefCell<Vec<Record>>,
    pub future: RefCell<Vec<Record>>,
    pub curr: RefCell<Record>,
    pub challenge: Option<ChallengeDef>,
}

/// A record of a code change
#[derive(Debug)]
pub struct Record {
    pub code: String,
    pub before: (u32, u32),
    pub after: (u32, u32),
}

/// Ways to set the cursor
#[derive(Clone, Copy)]
pub enum Cursor {
    Set(u32, u32),
    Keep,
    Ignore,
}

impl State {
    /// Set the code and cursor
    pub fn set_code(&self, code: &str, cursor: Cursor) {
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
    pub fn set_cursor(&self, to: (u32, u32)) {
        set_code_cursor(&self.code_id, to.0, to.1);
    }
    pub fn set_code_html(&self, code: &str) {
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
    pub fn clear_history(&self) {
        self.past.borrow_mut().clear();
        self.future.borrow_mut().clear();
    }
    pub fn undo(&self) {
        let prev = self.past.borrow_mut().pop();
        if let Some(prev) = prev {
            self.set_code_html(&prev.code);
            let mut curr = self.curr.borrow_mut();
            self.set_cursor(curr.before);
            self.future.borrow_mut().push(replace(&mut *curr, prev));
            self.set_changed();
        }
    }
    pub fn redo(&self) {
        if let Some(next) = self.future.borrow_mut().pop() {
            self.set_code_html(&next.code);
            let mut curr = self.curr.borrow_mut();
            self.set_cursor(next.after);
            self.past.borrow_mut().push(replace(&mut *curr, next));
            self.set_changed();
        }
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

pub fn get_execution_limit() -> f64 {
    get_local_var("execution-limit", || 2.0)
}
pub fn set_execution_limit(limit: f64) {
    set_local_var("execution-limit", limit);
}

pub fn get_ast_time() -> f64 {
    get_local_var("&ast-time", || 30.0)
}
pub fn set_ast_time(time: f64) {
    set_local_var("&ast-time", time);
}

pub fn get_right_to_left() -> bool {
    get_local_var("right-to-left", || false)
}
pub fn set_right_to_left(rtl: bool) {
    set_local_var("right-to-left", rtl);
}

pub fn get_top_at_top() -> bool {
    get_local_var("top-at-top", || false)
}
pub fn set_top_at_top(top_at_top: bool) {
    set_local_var("top-at-top", top_at_top);
}

pub fn get_font_name() -> String {
    get_local_var("font-name", || "Uiua386".into())
}
pub fn set_font_name(name: &str) {
    set_local_var("font-name", name);
    update_style();
}

pub fn get_font_size() -> String {
    get_local_var("font-size", || "1em".into())
}
pub fn set_font_size(size: &str) {
    set_local_var("font-size", size);
    update_style();
}

pub fn get_autorun() -> bool {
    get_local_var("autorun", || true)
}

pub fn set_autorun(autorun: bool) {
    set_local_var("autorun", autorun);
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

pub fn line_col(s: &str, pos: usize) -> (usize, usize) {
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

pub fn code_text(id: &str) -> String {
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

pub fn get_code_cursor_impl(id: &str) -> Option<(u32, u32)> {
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
    let mut found_start = false;
    let mut found_end = false;
    for (i, div_node) in children_of(&parent).enumerate() {
        if i > 0 {
            curr += 1;
        }
        if children_of(&div_node).count() == 1
            && div_node
                .first_child()
                .unwrap()
                .dyn_into::<HtmlBrElement>()
                .is_ok()
        {
            // This is the case when you click on an empty line
            if div_node.contains(Some(&anchor_node)) {
                start = curr + anchor_offset;
                found_start = true;
            }
            if div_node.contains(Some(&focus_node)) {
                end = curr + focus_offset;
                found_end = true;
            }
        } else {
            // This is the normal case
            for span_node in children_of(&div_node) {
                let text_content = span_node.text_content().unwrap();
                // logging::log!("text_content: {:?}", text_content);
                let len = text_content.chars().count() as u32;
                if span_node.contains(Some(&anchor_node)) {
                    let anchor_char_offset =
                        utf16_offset_to_char_offset(&text_content, anchor_offset);
                    start = curr + anchor_char_offset;
                    found_start = true;
                    // logging::log!(
                    //     "start change: curr: {}, offset: {}",
                    //     curr,
                    //     anchor_char_offset
                    // );
                    // logging::log!("start -> {:?}", start);
                }
                if span_node.contains(Some(&focus_node)) {
                    let focus_char_offset =
                        utf16_offset_to_char_offset(&text_content, focus_offset);
                    end = curr + focus_char_offset;
                    found_end = true;
                    // logging::log!("end change: curr: {}, offset: {}", curr, focus_char_offset);
                    // logging::log!("end -> {:?}", end);
                }
                // Increment curr by the length of the text in the node
                // logging::log!("len {} -> {}", curr, curr + len);
                curr += len;
            }
        }
    }
    // This case occurs when double clicking a line in Firefox
    if !found_start || !found_end {
        let text_content = parent.inner_text();
        let start_offset = text_content
            .lines()
            .take(start as usize)
            .map(|s| s.chars().count() + 1)
            .sum::<usize>();
        let end_offset = text_content
            .lines()
            .take(end as usize)
            .map(|s| s.chars().count() + 1)
            .sum::<usize>()
            - 1;
        if !found_start {
            start = utf16_offset_to_char_offset(&text_content, start_offset as u32);
        }
        if !found_end {
            end = utf16_offset_to_char_offset(&text_content, end_offset as u32);
        }
    }
    // logging::log!("get_code_cursor -> {:?}, {:?}", start, end);
    Some((start, end))
}

fn utf16_offset_to_char_offset(s: &str, utf16_offset: u32) -> u32 {
    let mut char_offset = 0;
    let mut utf16_index = 0;
    for c in s.chars() {
        if utf16_index == utf16_offset {
            break;
        }
        utf16_index += c.len_utf16() as u32;
        char_offset += 1;
    }
    char_offset
}

fn char_offset_to_utf16_offset(s: &str, char_offset: u32) -> u32 {
    let mut utf16_offset = 0;
    for c in s.chars().take(char_offset as usize) {
        utf16_offset += c.len_utf16() as u32;
    }
    utf16_offset
}

fn set_code_cursor(id: &str, start: u32, end: u32) {
    // logging::log!("set_code_cursor({}, {})", start, end);

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
        let start_text = &start_node.text_content().unwrap();
        let end_text = &end_node.text_content().unwrap();
        // logging::log!("start: {start_len} of {start_text:?}");
        start_len = char_offset_to_utf16_offset(
            start_text,
            start_len.min(start_text.chars().count() as u32),
        );
        end_len =
            char_offset_to_utf16_offset(end_text, end_len.min(end_text.chars().count() as u32));
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
    // logging::log!("{:#?}", spans(code));
    for span in spans(code).0 {
        let kind = span.value;
        let span = span.span;
        push_unspanned(&mut html, span.start.char_pos as usize, &mut end);

        let text: String = chars[span.start.char_pos as usize..span.end.char_pos as usize]
            .iter()
            .collect();
        // logging::log!("spanned: {:?} {:?}", kind, text);
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
            for (i, text) in text.lines().enumerate() {
                if i > 0 {
                    html.push_str("</div><div class=\"code-line\">");
                }
                html.push_str(&match kind {
                    SpanKind::Primitive(prim) => {
                        let name = prim.name();
                        let mut title = format!("{}: {}", name, prim.doc().short_text());
                        if let Some(ascii) = prim.ascii() {
                            title = format!("({}) {}", ascii, title);
                        }
                        format!(
                            r#"<span 
                            class="code-span code-hover {color_class}" 
                            data-title={title:?}>{}</span>"#,
                            escape_html(text)
                        )
                    }
                    SpanKind::Ident(None) if text == "i" => r#"<span 
                            class="code-span code-hover noadic-function" 
                            data-title="i: The imaginary unit">i</span>"#
                        .into(),
                    SpanKind::Ident(None) if text == "e" => r#"<span 
                            class="code-span code-hover noadic-function" 
                            data-title="e: Euler's constant">e</span>"#
                        .into(),
                    SpanKind::Ident(ref docs) => {
                        if let Some(docs) = docs {
                            let mut title = docs
                                .signature
                                .map(|sig| sig.to_string())
                                .unwrap_or_default();
                            let class = if let Some(sig) = docs.signature {
                                let margs = text.chars().rev().take_while(|c| *c == '!').count();
                                binding_class(text, sig, margs, docs.constant)
                            } else {
                                ""
                            };
                            if let Some(comment) = &docs.comment {
                                if !title.is_empty() {
                                    title.push(' ');
                                }
                                title.push_str(comment);
                            }
                            format!(
                                r#"<span 
                                    class="code-span code-hover {class}" 
                                    data-title="{title}">{}</span>"#,
                                escape_html(text)
                            )
                        } else {
                            format!(
                                r#"<span class="code-span {color_class}">{}</span>"#,
                                escape_html(text)
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
                                escape_html(text)
                            )
                        }
                    }
                    SpanKind::Signature | SpanKind::Placeholder => format!(
                        r#"<span 
                        class="code-span code-hover {color_class}" 
                        data-title="{}">{}</span>"#,
                        format!("{kind:?}").to_lowercase(),
                        escape_html(text)
                    ),
                    SpanKind::Label => format!(
                        r#"<span 
                        class="code-span code-hover {color_class}"
                        data-title="label">{}</span>"#,
                        escape_html(text)
                    ),
                    _ => format!(
                        r#"<span class="code-span {color_class}">{}</span>"#,
                        escape_html(text)
                    ),
                });
            }
        }

        end = span.end.char_pos as usize;
    }

    push_unspanned(&mut html, code.chars().count(), &mut end);

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

fn init_rt() -> Uiua {
    Uiua::with_backend(WebBackend::default())
        .with_execution_limit(Duration::from_secs_f64(get_execution_limit()))
}

fn just_values(code: &str) -> UiuaResult<Vec<Value>> {
    let mut rt = init_rt();
    rt.run_str(code)?;
    Ok(rt.take_stack())
}

fn challenge_code(input: &str, test: &str, flip: bool) -> String {
    if flip {
        format!("{input}\n{test}")
    } else {
        format!("{test}\n{input}")
    }
}

impl State {
    /// Run code and return the output
    pub fn run_code(&self, code: &str) -> Vec<OutputItem> {
        if let Some(chal) = &self.challenge {
            let mut example = run_code_single(&challenge_code(
                &chal.intended_answer,
                &chal.example,
                chal.flip,
            ));
            example.insert(0, OutputItem::Faint(format!("Example: {}", chal.example)));
            let mut output_sections = vec![example];
            let mut correct = true;
            for test in &chal.tests {
                let answer =
                    || just_values(&challenge_code(&chal.intended_answer, test, chal.flip));
                let user_input = challenge_code(code, test, chal.flip);
                let user_output = || just_values(&user_input);
                correct = correct
                    && match (answer(), user_output()) {
                        (Ok(answer), Ok(users)) => answer == users,
                        (Err(answer), Err(users)) => answer.to_string() == users.to_string(),
                        _ => false,
                    };
                let mut output = run_code_single(&user_input);
                output.insert(0, OutputItem::Faint(format!("Input: {test}")));
                output_sections.push(output);
            }
            let hidden_answer = || {
                just_values(&challenge_code(
                    &chal.intended_answer,
                    &chal.hidden,
                    chal.flip,
                ))
            };
            let hidden_user_output = || just_values(&challenge_code(code, &chal.hidden, chal.flip));
            let hidden_correct = match (hidden_answer(), hidden_user_output()) {
                (Ok(answer), Ok(users)) => answer == users,
                (Err(answer), Err(users)) => answer.to_string() == users.to_string(),
                _ => false,
            };
            let mut output = if chal.did_init_run.get() {
                vec![OutputItem::String(if correct {
                    if hidden_correct {
                        "✅ Correct!".into()
                    } else {
                        "❌ Incorrect (on edge case)".into()
                    }
                } else {
                    "❌ Incorrect".into()
                })]
            } else {
                Vec::new()
            };
            chal.did_init_run.set(true);
            for section in output_sections {
                output.push(OutputItem::Separator);
                output.extend(section);
            }
            output
        } else {
            run_code_single(code)
        }
    }
}

fn run_code_single(code: &str) -> Vec<OutputItem> {
    // Run
    let mut rt = init_rt();
    let mut error = None;
    let mut comp = Compiler::new();
    let mut values = match comp
        .load_str(code)
        .and_then(|comp| rt.run_asm(comp.finish()))
    {
        Ok(_) => rt.take_stack(),
        Err(e) => {
            error = Some(e);
            rt.take_stack()
        }
    };
    if get_top_at_top() {
        values.reverse();
    }
    let diagnostics = comp.take_diagnostics();
    let io = rt.downcast_backend::<WebBackend>().unwrap();
    // Get stdout and stderr
    let stdout = take(&mut *io.stdout.lock().unwrap());
    let mut stack = Vec::new();
    let value_count = values.len();
    for (i, value) in values.into_iter().enumerate() {
        // Try to convert the value to audio
        if value.shape().last().is_some_and(|&n| n >= 44100 / 4) {
            if let Ok(bytes) = value_to_wav_bytes(&value, io.audio_sample_rate()) {
                stack.push(OutputItem::Audio(bytes));
                continue;
            }
        }
        // Try to convert the value to an image
        const MIN_AUTO_IMAGE_DIM: usize = 30;
        if let Ok(image) = value_to_image(&value) {
            if image.width() >= MIN_AUTO_IMAGE_DIM as u32
                && image.height() >= MIN_AUTO_IMAGE_DIM as u32
            {
                if let Ok(bytes) = image_to_bytes(&image, ImageOutputFormat::Png) {
                    stack.push(OutputItem::Image(bytes));
                    continue;
                }
            }
        }
        // Try to convert the value to a gif
        if let Ok(bytes) = value_to_gif_bytes(&value, 16.0) {
            match value.shape().dims() {
                &[f, h, w] | &[f, h, w, _]
                    if h >= MIN_AUTO_IMAGE_DIM && w >= MIN_AUTO_IMAGE_DIM && f >= 5 =>
                {
                    stack.push(OutputItem::Gif(bytes));
                    continue;
                }
                _ => {}
            }
        }
        // Otherwise, just show the value
        let class = if value_count == 1 {
            ""
        } else {
            match i % 6 {
                0 => "output-a",
                1 => "output-b",
                2 => "output-c",
                3 => "output-d",
                4 => "output-e",
                5 => "output-f",
                _ => unreachable!(),
            }
        };
        for line in value.show().lines() {
            stack.push(OutputItem::Classed(class, line.to_string()));
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
        let report = error.report();
        let execution_limit_reached = report.fragments.iter().any(|frag| matches!(frag, ReportFragment::Plain(s) if s.contains("Maximum execution time exceeded")));
        output.push(OutputItem::Report(report));
        if execution_limit_reached {
            output.push(OutputItem::String(
                "You can increase the execution time limit in the editor settings".into(),
            ));
        }
    }
    if !diagnostics.is_empty() {
        if !output.is_empty() {
            output.push(OutputItem::String("".into()));
        }
        for diag in diagnostics {
            output.push(OutputItem::Report(diag.report()));
        }
    }
    output
}

pub fn report_view(report: &Report) -> impl IntoView {
    let class = match report.kind {
        ReportKind::Error => "output-report output-error",
        ReportKind::Diagnostic(DiagnosticKind::Warning) => "output-report output-warning",
        ReportKind::Diagnostic(DiagnosticKind::Advice) => "output-report output-advice",
        ReportKind::Diagnostic(DiagnosticKind::Style) => "output-report output-style",
    };
    let mut newline_indices = Vec::new();
    for (i, frag) in report.fragments.iter().enumerate() {
        if matches!(frag, ReportFragment::Newline) {
            newline_indices.push(i);
        }
    }
    let mut snip_range = None;
    if newline_indices.len() > 20 {
        snip_range = Some(newline_indices[12]..newline_indices[newline_indices.len() - 8]);
    }
    let mut frags = Vec::new();
    for (i, frag) in report.fragments.iter().enumerate() {
        if let Some(range) = &snip_range {
            if range.contains(&i) {
                if range.start == i {
                    let omitted_count = newline_indices.len() - 20;
                    frags.push(view!(<br/>).into_view());
                    frags.push(view!(<br/>).into_view());
                    frags.push(view! {
                        <span class="output-report">{format!("     ...{omitted_count} omitted...")}</span>
                    }.into_view());
                    frags.push(view!(<br/>).into_view());
                }
                continue;
            }
        }
        frags.push(match frag {
            ReportFragment::Plain(s) => view!(<span class="output-report">{s}</span>).into_view(),
            ReportFragment::Faint(s) => {
                view!(<span class="output-report output-faint">{s}</span>).into_view()
            }
            ReportFragment::Fainter(s) => {
                view!(<span class="output-report output-fainter">{s}</span>).into_view()
            }
            ReportFragment::Colored(s) => view!(<span class=class>{s}</span>).into_view(),
            ReportFragment::Newline => view!(<br/>).into_view(),
        });
    }
    view! {
        <div style="font-family: inherit">{frags}</div>
    }
}

pub struct ChallengeDef {
    pub example: String,
    pub intended_answer: String,
    pub best_answer: Option<String>,
    pub tests: Vec<String>,
    pub hidden: String,
    flip: bool,
    did_init_run: Cell<bool>,
}

#[component]
pub fn Challenge<'a>(
    number: u8,
    prompt: &'a str,
    example: &'a str,
    answer: &'a str,
    tests: &'a [&'a str],
    hidden: &'a str,
    #[prop(optional)] default: &'a str,
    #[prop(optional)] flip: bool,
    #[prop(optional)] best_answer: &'a str,
) -> impl IntoView {
    let def = ChallengeDef {
        example: example.into(),
        intended_answer: answer.into(),
        best_answer: (!best_answer.is_empty()).then(|| best_answer.into()),
        tests: tests.iter().copied().map(Into::into).collect(),
        hidden: hidden.into(),
        flip,
        did_init_run: Cell::new(false),
    };
    let (main_part, rest) = if let Some((a, b)) = prompt.split_once('.') {
        (a.to_string(), b.to_string())
    } else {
        (prompt.to_string(), String::new())
    };
    view! {
        <div class="challenge">
            <h3>"Challenge "{number}</h3>
            <p>"Write a program that "<strong>{main_part}</strong>"."{rest}</p>
            <Editor challenge=def example=default/>
        </div>
    }
}

pub fn progressive_strings(input: &str) -> Vec<String> {
    let mut inputs = Inputs::default();
    let (items, errors, _) = uiua::parse(input, (), &mut inputs);
    if !errors.is_empty() {
        return vec![input.into()];
    }
    let mut lines: Vec<Vec<String>> = Vec::new();
    for item in items {
        match item {
            Item::Words(lns) => {
                for ln in lns {
                    let mut line: Vec<String> = Vec::new();
                    for word in ln {
                        if word.value.is_code() {
                            line.push(word.span.as_str(&inputs, |s| s.into()));
                        } else if let Some(last) = line.last_mut() {
                            word.span.as_str(&inputs, |s| last.push_str(s));
                        } else {
                            line.push(word.span.as_str(&inputs, |s| s.into()));
                        }
                    }
                    lines.push(line);
                }
            }
            Item::Binding(binding) => {
                lines.push(vec![binding.span().as_str(&inputs, |s| s.into())])
            }
            Item::TestScope(items) => lines.push(vec![items.span.as_str(&inputs, |s| s.into())]),
        }
    }
    let mut strings = Vec::new();
    let mut curr_total = String::new();
    for line in lines {
        let mut curr_line = String::new();
        for frag in line.into_iter().rev() {
            curr_line.insert_str(0, &frag);
            strings.push(format!("{curr_total}{curr_line}"));
        }
        curr_total.push_str(&curr_line);
        curr_total.push('\n');
    }
    if strings.is_empty() {
        strings.push("".into());
    }
    strings.rotate_right(1);
    strings[0] = input.into();
    strings
}

pub fn url_encode_code(code: &str) -> String {
    format!(
        "{}__{}",
        uiua::VERSION.replace('.', "_"),
        URL_SAFE.encode(code)
    )
}
