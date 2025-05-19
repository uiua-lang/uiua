use base64::engine::{general_purpose::URL_SAFE, Engine};
use js_sys::Date;
use leptos::*;
use std::path::Path;
use std::{
    borrow::Cow,
    cell::Cell,
    collections::HashMap,
    mem::{replace, take},
    str::FromStr,
    time::Duration,
};
use uiua::UiuaErrorKind;

use uiua::{
    ast::Item,
    lsp::{BindingDocsKind, ImportSrc},
    media::SmartOutput,
    Compiler, DiagnosticKind, Inputs, Primitive, Report, ReportFragment, ReportKind, SpanKind,
    Spans, Uiua, UiuaError, UiuaResult, Value,
};
use unicode_segmentation::UnicodeSegmentation;
use wasm_bindgen::JsCast;
use web_sys::{
    DomRect, Event, HtmlDivElement, HtmlSpanElement, HtmlStyleElement, HtmlTextAreaElement,
    KeyboardEvent, MouseEvent,
};

use crate::binding_style;
use crate::{
    backend::{OutputItem, WebBackend},
    binding_class, code_font, modifier_class, prim_sig_class,
};

#[derive(Clone)]
pub struct ChallengeDef {
    pub example: String,
    pub intended_answer: String,
    pub best_answer: Option<String>,
    pub tests: Vec<String>,
    pub hidden: String,
    pub flip: bool,
    pub did_init_run: Cell<bool>,
}

/// Handles setting the code in the editor, setting the cursor, and managing the history
#[derive(Clone)]
pub struct State {
    pub code_id: String,
    pub code_area_id: String,
    pub code_outer_id: String,
    pub line_numbers_id: String,
    pub editor_wrapper_id: String,
    pub set_overlay: WriteSignal<String>,
    pub set_line_count: WriteSignal<usize>,
    pub set_copied_link: WriteSignal<bool>,
    pub past: Vec<Record>,
    pub future: Vec<Record>,
    pub curr: Record,
    pub challenge: Option<ChallengeDef>,
    pub loading_module: bool,
}

/// A record of a code change
#[derive(Debug, Clone)]
pub struct Record {
    pub code: String,
    pub before: (u32, u32),
    pub after: (u32, u32),
}

/// Ways to set the cursor
#[derive(Debug, Clone, Copy)]
pub enum Cursor {
    Set(u32, u32),
    Keep,
    Ignore,
}

impl State {
    /// Set the code and cursor
    pub fn set_code(&mut self, code: &str, cursor: Cursor) {
        // logging::log!("set_code({:?}, {:?})", code, cursor);
        let maybe_before = get_code_cursor(&self.code_id);
        let after = match cursor {
            Cursor::Set(start, end) => (start, end),
            Cursor::Keep => maybe_before.unwrap_or_else(|| {
                let len = code.chars().count() as u32;
                (len, len)
            }),
            Cursor::Ignore => {
                let len = code.chars().count() as u32;
                (len, len)
            }
        };
        let before = maybe_before
            .or_else(|| self.past.last().map(|r| r.after))
            .unwrap_or(after);
        let new_curr = Record {
            code: code.into(),
            before,
            after,
        };
        let prev = replace(&mut self.curr, new_curr);
        let changed = prev.code != code;
        if changed {
            self.past.push(prev);
            self.future.clear();
        }
        self.set_code_element(code);
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
    pub fn track_change(&mut self) {
        let Some(cursor) = get_code_cursor(&self.code_id) else {
            return;
        };
        let code = get_code(&self.code_id);
        let before = self.past.last().map(|r| r.after).unwrap_or(cursor);
        let new_curr = Record {
            code: code.clone(),
            before,
            after: cursor,
        };
        let prev = replace(&mut self.curr, new_curr);
        if prev.code != code {
            self.past.push(prev);
            self.future.clear();
            self.set_changed();
        }
    }
    fn set_code_element(&self, code: &str) {
        // logging::log!("set code: {code:?}");
        if let Some(cursor) = get_code_cursor(&self.code_id) {
            self.set_cursor(cursor);
        }
        self.set_overlay.set(code.into());
        let area = element::<HtmlTextAreaElement>(&self.code_id);
        let code_area = element::<HtmlDivElement>(&self.code_area_id);
        let outer = element::<HtmlDivElement>(&self.code_outer_id);

        let height = format!("{}em", code.split('\n').count().max(1) as f32 * 1.25 + 0.75);
        code_area
            .style()
            .set_property("--normal-min-height", &height)
            .unwrap();

        let rect = &virtual_rect(&area, code);
        let width = rect.width();
        let new_width = format!("max(calc({width}px + 1em),100%)");
        area.style().set_property("width", &new_width).unwrap();

        let outer_new_width = format!("max(calc({width}px + 3.4em),100%)");
        outer
            .style()
            .set_property("width", &outer_new_width)
            .unwrap();

        area.set_value(code);
    }
    pub fn refresh_code(&self) {
        let code = get_code(&self.code_id);
        self.set_code_element(&code);
    }
    pub fn set_cursor(&self, (start, end): (u32, u32)) {
        // logging::log!("set_cursor({start}, {end})");
        let area = element::<HtmlTextAreaElement>(&self.code_id);
        let content = area.value();
        let start = char_offset_to_utf16_offset(&content, start);
        let end = char_offset_to_utf16_offset(&content, end);
        area.set_selection_range(start, end).unwrap();

        let outer = element::<HtmlDivElement>(&self.code_outer_id);

        let Some(cursor_position) = area.selection_end().unwrap() else {
            return;
        };
        let code = get_code(&self.code_id);
        let (line, col) = line_col(&code, cursor_position as usize);
        let horiz_text = code
            .lines()
            .nth(line - 1)
            .unwrap_or("")
            .chars()
            .take(col - 1)
            .collect::<String>();
        let relative_x = virtual_rect(&area, &horiz_text).width();
        let area_rect = virtual_rect(&area, &code);
        let outer_rect = outer.get_bounding_client_rect();
        let x = area_rect.left() + relative_x;
        if x > outer_rect.right() {
            outer.set_scroll_left(outer.scroll_width());
        } else if x < outer_rect.left() {
            outer.set_scroll_left(0);
        }
    }
    fn set_changed(&self) {
        self.set_copied_link.set(false);
        self.set_line_count();
    }
    pub fn update_line_number_width(&self) {
        let line_numbers = element::<HtmlDivElement>(&self.line_numbers_id);
        let editor = element::<HtmlDivElement>(&self.editor_wrapper_id);
        let line_numbers_width = line_numbers.get_bounding_client_rect().width();
        let line_numbers_width_str = format!("{}px", line_numbers_width);
        editor
            .style()
            .set_property("--line-numbers-width", &line_numbers_width_str)
            .unwrap();
    }
    pub fn set_line_count(&self) {
        self.set_line_count
            .set(get_code(&self.code_id).split('\n').count().max(1));

        let state = self.clone();
        set_timeout(
            move || state.update_line_number_width(),
            Duration::from_millis(0),
        );
    }
    pub fn clear_history(&mut self) {
        self.past.clear();
        self.future.clear();
    }
    pub fn undo(&mut self) {
        let prev = self.past.pop();
        if let Some(prev) = prev {
            self.set_code_element(&prev.code);
            self.set_cursor(self.curr.before);
            self.future.push(replace(&mut self.curr, prev));
            self.set_changed();
        }
    }
    pub fn redo(&mut self) {
        if let Some(next) = self.future.pop() {
            self.set_code_element(&next.code);
            self.set_cursor(next.after);
            self.past.push(replace(&mut self.curr, next));
            self.set_changed();
        }
    }
}

fn virtual_rect(area: &HtmlTextAreaElement, text: &str) -> DomRect {
    let temp_span = (document().create_element("span"))
        .unwrap()
        .unchecked_into::<HtmlSpanElement>();
    let style = temp_span.style();
    style.set_property("visibility", "hidden").unwrap();
    style.set_property("white-space", "pre").unwrap();
    let area_style = &window().get_computed_style(area).unwrap().unwrap();
    let area_font = area_style.get_property_value("font").unwrap();
    temp_span.style().set_property("font", &area_font).unwrap();
    let mut text = Cow::Borrowed(text);
    if text.ends_with('\n') {
        text.to_mut().push(' ');
    }
    temp_span.set_inner_text(&text);
    document().body().unwrap().append_child(&temp_span).unwrap();
    let rect = temp_span.get_bounding_client_rect();
    document().body().unwrap().remove_child(&temp_span).unwrap();
    rect
}

pub fn get_code(id: &str) -> String {
    element::<HtmlTextAreaElement>(id).value()
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

pub fn get_code_cursor(id: &str) -> Option<(u32, u32)> {
    let area = element::<HtmlTextAreaElement>(id);
    let content = area.value();
    let start = area.selection_start().unwrap()?;
    let end = area.selection_end().unwrap()?;
    let start = utf16_offset_to_char_offset(&content, start);
    let end = utf16_offset_to_char_offset(&content, end);
    // logging::log!("get_code_cursor -> {start}, {end}");
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

#[derive(Debug)]
enum CodeFragment {
    Unspanned(String),
    Br,
    Span(String, SpanKind),
    Ghost(String, Option<String>),
}

struct CodeLines {
    frags: Vec<Vec<CodeFragment>>,
}

impl CodeLines {
    fn line(&mut self) -> &mut Vec<CodeFragment> {
        self.frags.last_mut().unwrap()
    }
    fn frag(&mut self) -> &mut CodeFragment {
        self.line().last_mut().unwrap()
    }
    fn push_str(&mut self, s: &str) {
        match self.frag() {
            CodeFragment::Unspanned(ref mut unspanned) => unspanned.push_str(s),
            _ => self.line().push(CodeFragment::Unspanned(s.to_string())),
        }
    }
    fn new_line(&mut self) {
        if self.line().is_empty() {
            self.line().push(CodeFragment::Br);
        }
        self.frags.push(Vec::new());
    }
}

pub fn on_mac() -> bool {
    window()
        .navigator()
        .user_agent()
        .unwrap()
        .to_lowercase()
        .contains("mac")
}

pub fn os_ctrl(event: &Event) -> bool {
    if let Some(event) = event.dyn_ref::<KeyboardEvent>() {
        if on_mac() {
            event.meta_key()
        } else {
            event.ctrl_key()
        }
    } else if let Some(event) = event.dyn_ref::<MouseEvent>() {
        if on_mac() {
            event.meta_key()
        } else {
            event.ctrl_key()
        }
    } else {
        false
    }
}

pub fn update_ctrl(event: &Event) {
    document()
        .body()
        .unwrap()
        .set_class_name(if os_ctrl(event) { "ctrl-pressed" } else { "" });
}

fn build_code_lines(id: &str, code: &str) -> CodeLines {
    let mut lines = CodeLines {
        frags: vec![Vec::new()],
    };

    let chars: Vec<&str> = code.graphemes(true).collect();

    let push_unspanned = |lines: &mut CodeLines, mut target: usize, curr: &mut usize| {
        target = target.min(chars.len());
        if *curr >= target {
            return;
        }
        lines.line().push(CodeFragment::Unspanned(String::new()));
        let mut unspanned = String::new();
        while *curr < target {
            if chars[*curr] == "\n" {
                if !unspanned.is_empty() {
                    // logging::log!("unspanned: `{}`", unspanned);
                    lines.push_str(&unspanned);
                    unspanned.clear();
                }
                // logging::log!("newline");
                lines.new_line();
                *curr += 1;
                while *curr < target && chars[*curr] == "\n" {
                    lines.new_line();
                    *curr += 1;
                }
                lines.line().push(CodeFragment::Unspanned(String::new()));
                continue;
            }
            unspanned.push_str(chars[*curr]);
            *curr += 1;
        }
        if !unspanned.is_empty() {
            // logging::log!("unspanned: `{}`", unspanned);
            lines.push_str(&unspanned);
        }
        lines.line().push(CodeFragment::Unspanned(String::new()));
    };

    let mut end = 0;
    let spans = Spans::with_backend(code, WebBackend::new(id, code));
    for span in spans.spans {
        let kind = span.value;
        let span = span.span;
        push_unspanned(&mut lines, span.start.char_pos as usize, &mut end);

        let text: String = chars
            [span.start.char_pos as usize..(span.end.char_pos as usize).min(chars.len())]
            .iter()
            .copied()
            .collect();
        // logging::log!("spanned: {:?} `{}`", kind, text);

        if !text.is_empty() && text.chars().all(|c| c == '\n') {
            lines.new_line();
            for _ in 0..text.chars().count() - 1 {
                lines.new_line();
            }
        } else {
            for (i, text) in text.lines().enumerate() {
                if i > 0 {
                    lines.new_line();
                }
                lines
                    .line()
                    .push(CodeFragment::Span(text.into(), kind.clone()));
            }
        }

        end = span.end.char_pos as usize;
    }

    push_unspanned(&mut lines, chars.len(), &mut end);

    for line in &mut lines.frags {
        line.retain(|frag| !matches!(frag, CodeFragment::Unspanned(s) if s.is_empty()));
    }

    if get_inlay_values() {
        let line_lengths: Vec<usize> = code.split('\n').map(|line| line.chars().count()).collect();
        let max_val_line_len = line_lengths
            .iter()
            .enumerate()
            .filter(|(i, _)| spans.top_level_values.contains_key(&(i + 1)))
            .map(|(_, len)| *len)
            .max()
            .unwrap_or(0);

        fn make_long(long: String) -> Option<String> {
            let mut long_height = 0;
            let mut long_width = 0;
            for line in long.split('\n') {
                let line_len = line.chars().count();
                long_width = long_width.max(line_len);
                long_height += 1;
            }
            (long_width < 100 && long_height < 25).then_some(long)
        }

        for (i, line) in lines.frags.iter_mut().enumerate() {
            let Some(values) = spans.top_level_values.get(&(i + 1)) else {
                continue;
            };
            for (j, val) in values.iter().rev().enumerate() {
                let (short, long) = if val.rank() > 1 || val.shape.elements() > 1000 {
                    (val.shape_string(), make_long(val.show()))
                } else {
                    let long = val.show();
                    let short = if long.split('\n').count() > 1 || long.len() > 20 {
                        val.shape_string()
                    } else {
                        long.clone()
                    };
                    (short, make_long(long))
                };
                let padding = if j == 0 {
                    max_val_line_len - line_lengths[i]
                } else {
                    0
                };
                let short = format!("  {:padding$}{short}", "");
                line.push(CodeFragment::Ghost(short, long));
            }
        }
    }

    lines
}

pub fn gen_code_view(id: &str, code: &str) -> View {
    fn pair_aliases() -> HashMap<(Primitive, Primitive), &'static str> {
        use Primitive::*;
        [
            (
                (Un, Select),
                "unselect: Enumerate the length of an array, preserving the array",
            ),
            (
                (Un, Pick),
                "unpick: Enumerate the shape of an array, preserving the array",
            ),
            (
                (Un, Orient),
                "unorient: Enumerate the rank of an array, preserving the array",
            ),
            ((Un, Where), "unwhere: Convert lists of indices into a mask"),
            (
                (Un, Shape),
                "unshape: Create an array of incrementing indices with the given shape",
            ),
            ((Un, Sin), "unsine: Get the arcsine of a number"),
            (
                (Un, Atan),
                "unatangent: Get the sine and cosine of a number",
            ),
            (
                (Un, Complex),
                "uncomplex: Get the real part and imaginary part of a number",
            ),
            ((Un, Parse), "unparse: Format a number as a string"),
            ((Un, Sqrt), "unsqrt: Square a number"),
            ((Un, Fix), "unfix: Remove a length-1 axis from an array"),
            ((Un, Sort), "unsort: Shuffle an array"),
            ((Un, Couple), "uncouple: Split an array into its two rows"),
            ((Un, Bits), "unbits: Decode an array from bits (LSB-first)"),
            ((Un, Utf8), "unutf₈: Convert UTF-8 bytes to a string"),
            ((Un, Csv), "uncsv: Decode a CSV string to an array"),
            (
                (Un, Transpose),
                "untranspose: Bring the last axis of an array to the front",
            ),
            ((Un, Box), "unbox: Get the array out of a box"),
            (
                (Un, Join),
                "unjoin: Split an array into its first row and the rest",
            ),
            (
                (Un, Keep),
                "unkeep: Deduplicate adjacent rows and get the duplicate counts",
            ),
            ((Anti, Drop), "antidrop: Pad an array"),
            ((Anti, Pow), "antipower: Get the nth root of a number"),
            ((Anti, Log), "antilogarithm: Get the nth power of a number"),
            (
                (Anti, Select),
                "antiselect: Puts rows into the corresponding indices",
            ),
            (
                (Anti, Pick),
                "antipick: Puts elements into the corresponding indices",
            ),
            (
                (Anti, Repeat),
                "antirepeat: Repeat a function's inverse a number of times",
            ),
            (
                (Anti, Orient),
                "antiorient: Reorder axes by specifying where to send them",
            ),
            (
                (Anti, Join),
                "antijoin: Trim one array from the start of another",
            ),
        ]
        .into()
    }

    thread_local! {
        static PAIR_ALIASES: HashMap<(Primitive, Primitive), &'static str> = pair_aliases();
    }

    fn add_prim_view(
        prim: Primitive,
        text: String,
        title: String,
        color_class: &str,
        frag_views: &mut Vec<View>,
    ) {
        let class = format!("code-span code-underline {}", color_class);
        let onmouseover = move |event: web_sys::MouseEvent| update_ctrl(&event);
        let onclick = move |event: web_sys::MouseEvent| {
            if os_ctrl(&event) {
                window()
                    .open_with_url_and_target(&format!("/docs/{}", prim.name()), "_blank")
                    .unwrap();
            }
        };
        let view = view! {
            <span class=class data-title=title on:mouseover=onmouseover on:click=onclick>
                {text}
            </span>
        }
        .into_view();
        frag_views.push(view);
    }

    // logging::log!("gen_code_view({code:?})");
    let CodeLines { frags } = build_code_lines(id, code);
    let mut line_views = Vec::new();
    for line in frags {
        if line.is_empty() {
            line_views.push(view! {
                <div class="code-line">
                    <br />
                </div>
            });
            continue;
        }
        let mut frag_views = Vec::new();
        let mut frags = line.into_iter().peekable();
        while let Some(frag) = frags.next() {
            match frag {
                CodeFragment::Unspanned(s) => {
                    // logging::log!("unspanned escaped: `{}`", s);
                    frag_views.push(view! { <span class="code-span">{s}</span> }.into_view())
                }
                CodeFragment::Ghost(short, Some(long)) => frag_views.push(
                    view! {
                        <span class="code-span value-hint" data-title=long>
                            {short}
                        </span>
                    }
                    .into_view(),
                ),
                CodeFragment::Ghost(short, None) => frag_views
                    .push(view! { <span class="code-span value-hint">{short}</span> }.into_view()),
                CodeFragment::Br => frag_views.push(view! { <br /> }.into_view()),
                CodeFragment::Span(text, kind) => {
                    let color_class = match &kind {
                        SpanKind::Primitive(prim, sig) => prim_sig_class(*prim, *sig),
                        SpanKind::Obverse(_) => prim_sig_class(Primitive::Obverse, None),
                        SpanKind::Number if very_gay() => "text-gradient number-lesbian",
                        SpanKind::Number => "number-literal",
                        SpanKind::String | SpanKind::ImportSrc(_) if very_gay() => {
                            "text-gradient bright-rainbow"
                        }
                        SpanKind::String | SpanKind::ImportSrc(_) => "string-literal-span",
                        SpanKind::Comment | SpanKind::OutputComment if very_gay() => {
                            "text-gradient graynbow"
                        }
                        SpanKind::Comment | SpanKind::OutputComment => "comment-span",
                        SpanKind::Strand => "strand-span",
                        SpanKind::Subscript(None, _) => "number-literal",
                        SpanKind::Subscript(Some(prim), n) => prim_sig_class(*prim, *n),
                        SpanKind::MacroDelim(margs) => modifier_class(*margs),
                        _ => "",
                    };
                    match kind {
                        SpanKind::Primitive(Primitive::On, _)
                            if frags.peek().is_some_and(|frag| {
                                matches!(
                                    frag,
                                    CodeFragment::Span(_, SpanKind::Primitive(Primitive::By, _))
                                )
                            }) =>
                        {
                            let Some(CodeFragment::Span(next_text, _)) = frags.next() else {
                                unreachable!()
                            };
                            let title = format!(
                                "{}{}: Call a function keeping its first and last \
                                arguments on either side of the outputs",
                                Primitive::On.name(),
                                Primitive::By.name()
                            );
                            let class = format!(
                                "code-span code-underline {}",
                                if at_least_a_little_gay() {
                                    code_font!("nb2 text-gradient")
                                } else {
                                    "monadic-modifier"
                                }
                            );
                            let onmouseover = move |event: web_sys::MouseEvent| update_ctrl(&event);
                            let onclick = move |event: web_sys::MouseEvent| {
                                if os_ctrl(&event) {
                                    window()
                                        .open_with_url_and_target(
                                            "/tutorial/morestack#on-and-by",
                                            "_blank",
                                        )
                                        .unwrap();
                                }
                            };
                            let text = format!("{text}{next_text}");
                            let view = view! {
                                <span
                                    class=class
                                    data-title=title
                                    on:mouseover=onmouseover
                                    on:click=onclick
                                >
                                    {text}
                                </span>
                            }
                            .into_view();
                            frag_views.push(view)
                        }
                        SpanKind::Primitive(prim, _)
                            if frags.peek().is_some_and(|frag| {
                                matches!(frag, CodeFragment::Span(_, SpanKind::Primitive(next, _))
                                    if PAIR_ALIASES.with(|map| map.contains_key(&(prim, *next))))
                            }) =>
                        {
                            let Some(CodeFragment::Span(
                                next_text,
                                SpanKind::Primitive(next_prim, next_sig),
                            )) = frags.next()
                            else {
                                unreachable!()
                            };
                            let next_color_class = prim_sig_class(next_prim, next_sig);
                            let title =
                                PAIR_ALIASES.with(|map| *map.get(&(prim, next_prim)).unwrap());
                            let title = format!("(compound) {title}");
                            for (prim, text, color_class) in [
                                (prim, text, color_class),
                                (next_prim, next_text, next_color_class),
                            ] {
                                add_prim_view(
                                    prim,
                                    text,
                                    title.clone(),
                                    color_class,
                                    &mut frag_views,
                                );
                            }
                        }
                        SpanKind::Primitive(prim, _) => {
                            let name = prim.name();
                            let mut title = format!("{}: {}", name, prim.doc().short_text());
                            if let Some(ascii) = prim.ascii() {
                                title = format!("({}) {}", ascii, title);
                            }
                            add_prim_view(prim, text, title, color_class, &mut frag_views);
                        }
                        SpanKind::Obverse(set_inverses) => {
                            let prim = Primitive::Obverse;
                            let name = prim.name();
                            let mut title = format!("{}: {}", name, prim.doc().short_text());
                            if !set_inverses.is_empty() {
                                title.push('\n');
                                title.push_str(&set_inverses.to_string());
                            }
                            add_prim_view(prim, text, title, color_class, &mut frag_views);
                        }
                        SpanKind::String | SpanKind::ImportSrc(ImportSrc::File(_)) => {
                            let class = format!("code-span {color_class}");
                            if text == "@ " {
                                let space_class =
                                    format!("code-span space-character {}", color_class);
                                frag_views.push(
                                    view! {
                                        <span class=class data-title="space character">
                                            @
                                        </span>
                                        <span class=space_class data-title="space character">
                                            " "
                                        </span>
                                    }
                                    .into_view(),
                                )
                            } else {
                                let title = if text.starts_with('@') {
                                    "character"
                                } else {
                                    "string"
                                };
                                frag_views.push(
                                    view! {
                                        <span class=class data-title=title>
                                            {text}
                                        </span>
                                    }
                                    .into_view(),
                                )
                            }
                        }
                        SpanKind::ImportSrc(ImportSrc::Git(path)) => {
                            let title = "Git module path (Ctrl+Click to open)";
                            let class = format!("code-span code-underline {}", color_class);
                            let onmouseover = move |event: web_sys::MouseEvent| update_ctrl(&event);
                            let onclick = move |event: web_sys::MouseEvent| {
                                if os_ctrl(&event) {
                                    window().open_with_url_and_target(&path, "_blank").unwrap();
                                }
                            };
                            let view = view! {
                                <span
                                    class=class
                                    data-title=title
                                    on:mouseover=onmouseover
                                    on:click=onclick
                                >
                                    {text}
                                </span>
                            }
                            .into_view();
                            frag_views.push(view);
                        }
                        SpanKind::Signature => {
                            let class = format!("code-span {color_class}");
                            let title = format!("{kind:?}").to_lowercase();
                            frag_views.push(
                                view! {
                                    <span class=class data-title=title>
                                        {text}
                                    </span>
                                }
                                .into_view(),
                            )
                        }
                        SpanKind::Placeholder(_) => {
                            let class = format!("code-span {color_class}");
                            let title = "placeholder";
                            frag_views.push(
                                view! {
                                    <span class=class data-title=title>
                                        {text}
                                    </span>
                                }
                                .into_view(),
                            )
                        }
                        SpanKind::Label => {
                            let label = text.trim_start_matches('$');
                            let mut components = [0f32; 3];
                            const MIN: f32 = 0.2;
                            const MAX: f32 = 0.8;
                            let first = label.bytes().next();
                            for (i, c) in label.bytes().map(|c| c.to_ascii_lowercase()).enumerate()
                            {
                                let j = (i + first.unwrap().to_ascii_lowercase() as usize) % 3;
                                let mul = 1.0 - (i / 3 % 3) as f32 * 0.333;
                                let t = mul * (c.saturating_sub(b'a') as f32 % 26.0 / 26.0);
                                let target = MIN + (MAX - MIN) * t;
                                components[j] = components[j].max(target);
                            }
                            // Normalize to a pastel color
                            for c in &mut components {
                                *c = (*c).mul_add(0.5, 0.5);
                            }
                            let components = components.map(|c| (c * 255.0).round() as u8);
                            let style = format!(
                                "color: rgb({}, {}, {})",
                                components[0], components[1], components[2]
                            );
                            frag_views.push(
                                view! {
                                    <span class="code-span" style=style data-title="label">
                                        {text}
                                    </span>
                                }
                                .into_view(),
                            )
                        }
                        SpanKind::FuncDelim(sig, set_inverses) => {
                            let class = format!("code-span {color_class}");
                            let mut title = sig.to_string();
                            if !set_inverses.is_empty() {
                                title.push('\n');
                                title.push_str(&set_inverses.to_string());
                            }
                            frag_views.push(
                                view! {
                                    <span class=class data-title=title>
                                        {text}
                                    </span>
                                }
                                .into_view(),
                            )
                        }
                        SpanKind::Subscript(prim, _) => {
                            let class = format!("code-span {color_class}");
                            let title = if let Some(prim) = prim {
                                format!("subscript for {}", prim.format())
                            } else {
                                "subscript".into()
                            };
                            frag_views.push(
                                view! {
                                    <span class=class data-title=title>
                                        {text}
                                    </span>
                                }
                                .into_view(),
                            )
                        }
                        SpanKind::Ident {
                            docs: Some(docs), ..
                        } => {
                            let mut title = String::new();
                            match &docs.kind {
                                BindingDocsKind::Function { sig, .. } => {
                                    title.push_str(&sig.to_string())
                                }
                                BindingDocsKind::Constant(Some(value)) => {
                                    title.push_str(&value.shape_string())
                                }
                                _ => (),
                            }

                            let private = if docs.is_public
                                || matches!(docs.kind, BindingDocsKind::Module { .. })
                            {
                                ""
                            } else {
                                if !title.is_empty() {
                                    title.push(' ');
                                }
                                title.push_str("(private) ");
                                "private-binding"
                            };
                            if let Some(escape) = &docs.escape {
                                title.push('\n');
                                title.push_str(escape);
                                if docs.meta.comment.is_some() {
                                    title.push('\n');
                                }
                            }
                            if let Some(comment) = &docs.meta.comment {
                                if !title.is_empty() && !title.ends_with('\n') {
                                    if comment.text.contains('\n') && comment.sig.is_none() {
                                        title.push('\n');
                                    } else {
                                        title.push(' ');
                                    }
                                }
                                if let Some(sig) = &comment.sig {
                                    title.push_str(&sig.to_string());
                                    title.push('\n');
                                }
                                title.push_str(&comment.text.replace("\n\n\n", "\n"));
                            } else {
                                match docs.kind {
                                    BindingDocsKind::Constant(None) => title.push_str("constant"),
                                    BindingDocsKind::Module { .. } => title.push_str("module"),
                                    BindingDocsKind::Modifier(_) => title.push_str("macro"),
                                    BindingDocsKind::Error => title.push_str("error"),
                                    _ => {}
                                }
                            }
                            if let Some(counts) = &docs.meta.counts {
                                title.push('\n');
                                title.push_str(&counts.to_string());
                            }
                            let class =
                                format!("code-span {} {}", binding_class(&text, &docs), private);
                            let style = binding_style(&docs);
                            frag_views.push(
                                view! {
                                    <span class=class style=style data-title=title>
                                        {text}
                                    </span>
                                }
                                .into_view(),
                            )
                        }
                        SpanKind::LexOrder => {
                            let class = format!("code-span {color_class}");
                            let title = "lexical order";
                            frag_views.push(
                                view! {
                                    <span class=class data-title=title>
                                        {text}
                                    </span>
                                }
                                .into_view(),
                            )
                        }
                        _ => {
                            let class = format!("code-span {color_class}");
                            frag_views.push(view! { <span class=class>{text}</span> }.into_view())
                        }
                    }
                }
            }
        }
        line_views.push(view! { <div class="code-line">{frag_views}</div> })
    }
    line_views.into_view()
}

fn init_rt(id: &str, code: &str) -> Uiua {
    Uiua::with_backend(WebBackend::new(id, code))
        .with_execution_limit(Duration::from_secs_f64(get_execution_limit()))
        .with_recursion_limit(50)
}

fn just_values(id: &str, code: &str) -> UiuaResult<Vec<Value>> {
    let mut rt = init_rt(id, code);
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
    pub fn run_code(&mut self, code: &str) -> Vec<OutputItem> {
        if let Some(chal) = &self.challenge {
            let mut example = run_code_single(
                &self.code_id,
                &challenge_code(&chal.intended_answer, &chal.example, chal.flip),
            )
            .0;
            example.insert(0, OutputItem::Faint(format!("Example: {}", chal.example)));
            let mut output_sections = vec![example];
            let mut correct = true;
            for test in &chal.tests {
                let answer = || {
                    just_values(
                        &self.code_id,
                        &challenge_code(&chal.intended_answer, test, chal.flip),
                    )
                };
                let user_input = challenge_code(code, test, chal.flip);
                let user_output = || just_values(&self.code_id, &user_input);
                correct = correct
                    && match (answer(), user_output()) {
                        (Ok(answer), Ok(users)) => answer == users,
                        (Err(answer), Err(users)) => answer.to_string() == users.to_string(),
                        _ => false,
                    };
                let mut output = run_code_single(&self.code_id, &user_input).0;
                output.insert(0, OutputItem::Faint(format!("Input: {test}")));
                output_sections.push(output);
            }
            let hidden_answer = || {
                just_values(
                    &self.code_id,
                    &challenge_code(&chal.intended_answer, &chal.hidden, chal.flip),
                )
            };
            let hidden_user_output = || {
                just_values(
                    &self.code_id,
                    &challenge_code(code, &chal.hidden, chal.flip),
                )
            };
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
            let (output, error) = run_code_single(&self.code_id, code);
            self.loading_module = false;
            if let Some(error) = error {
                if error.to_string().contains("Waiting for module") {
                    self.loading_module = true;
                }
            }
            output
        }
    }
}

#[allow(clippy::mutable_key_type)]
fn run_code_single(id: &str, code: &str) -> (Vec<OutputItem>, Option<UiuaError>) {
    // Run
    let mut rt = init_rt(id, code);
    let mut error = None;
    let mut comp = Compiler::with_backend(WebBackend::new(id, code));
    let comp_backend;
    let res = comp.load_str(code).map(|comp| rt.run_compiler(comp));
    let (mut values, io) = match res {
        Ok(Ok(())) => {
            let stack = rt.take_stack();
            let backend = rt.downcast_backend::<WebBackend>().unwrap();
            backend.finish();
            (stack, backend)
        }
        Ok(Err(e)) if matches!(e.kind, UiuaErrorKind::Interrupted) => (
            rt.take_stack(),
            rt.downcast_backend::<WebBackend>().unwrap(),
        ),
        Ok(Err(e)) => {
            error = Some(e);
            (
                rt.take_stack(),
                rt.downcast_backend::<WebBackend>().unwrap(),
            )
        }
        Err(e) => {
            error = Some(e);
            comp_backend = comp.take_backend::<WebBackend>().unwrap();
            (Vec::new(), &comp_backend)
        }
    };
    if get_top_at_top() {
        values.reverse();
    }
    let diagnostics = comp.take_diagnostics();
    // Get stdout and stderr
    let stdout = take(&mut *io.stdout.lock().unwrap());
    let mut stack = Vec::new();
    let value_count = values.len();
    let make_smart_output = if get_animation_format() == "APNG" {
        SmartOutput::from_value_prefer_apng
    } else {
        SmartOutput::from_value
    };
    for (i, value) in values.into_iter().enumerate() {
        let value = match make_smart_output(value, 24.0, io) {
            SmartOutput::Png(bytes, label) => {
                stack.push(OutputItem::Image(bytes, label));
                continue;
            }
            SmartOutput::Gif(bytes, label) => {
                stack.push(OutputItem::Gif(bytes, label));
                continue;
            }
            SmartOutput::Apng(bytes, label) => {
                stack.push(OutputItem::Apng(bytes, label));
                continue;
            }
            SmartOutput::Wav(bytes, label) => {
                stack.push(OutputItem::Audio(bytes, label));
                continue;
            }
            SmartOutput::Svg { svg, original } => {
                stack.push(OutputItem::Svg(
                    svg,
                    original.meta.label.as_ref().map(Into::into),
                ));
                continue;
            }
            SmartOutput::Normal(value) => value,
        };
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
        for line in value.lines() {
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
    if let Some(error) = &error {
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
    output.extend(rt.take_reports().into_iter().map(OutputItem::Report));
    (output, error)
}

pub fn report_view(report: &Report) -> impl IntoView {
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
    let mut frag_lines = vec![(Vec::new(), false)];
    for (i, frag) in report.fragments.iter().enumerate() {
        let (frags, wrap) = frag_lines.last_mut().unwrap();
        if let Some(range) = &snip_range {
            if range.contains(&i) {
                if range.start == i {
                    let omitted_count = newline_indices.len() - 20;
                    frags.push(view! { <br /> }.into_view());
                    frags.push(view! { <br /> }.into_view());
                    frags.push(view! { <span class="output-report">{format!("     ...{omitted_count} omitted...")}</span> }.into_view());
                    frags.push(view! { <br /> }.into_view());
                }
                continue;
            }
        }
        frags.push(match frag {
            ReportFragment::Plain(s) => {
                view! { <span class="output-report">{s}</span> }.into_view()
            }
            ReportFragment::Faint(s) => {
                view! { <span class="output-report output-faint">{s}</span> }.into_view()
            }
            ReportFragment::Fainter(s) => {
                view! { <span class="output-report output-fainter">{s}</span> }.into_view()
            }
            ReportFragment::Colored(s, kind) => {
                if frags.is_empty() {
                    *wrap = true;
                }
                let class = match kind {
                    ReportKind::Error => "output-report output-error",
                    ReportKind::Diagnostic(DiagnosticKind::Warning) => {
                        "output-report output-warning"
                    }
                    ReportKind::Diagnostic(DiagnosticKind::Advice) => "output-report output-advice",
                    ReportKind::Diagnostic(DiagnosticKind::Style) => "output-report output-style",
                    ReportKind::Diagnostic(DiagnosticKind::Info) => "output-report output-info",
                };
                view! { <span class=class>{s}</span> }.into_view()
            }
            ReportFragment::Newline => {
                frag_lines.push((Vec::new(), false));
                continue;
            }
        });
    }
    let mut frags = Vec::new();
    for (line, wrap) in frag_lines {
        let class = if wrap {
            "output-line output-wrap-line"
        } else {
            "output-line"
        };
        frags.push(view! { <div class=class>{line}</div> }.into_view());
    }
    view! { <div style="font-family: inherit">{frags}</div> }
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
            Item::Words(ln) => {
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
            Item::Binding(binding) => {
                lines.push(vec![binding.span().as_str(&inputs, |s| s.into())])
            }
            Item::Data(defs) => lines.push(
                defs.iter()
                    .map(|data| data.span().as_str(&inputs, |s| s.into()))
                    .collect(),
            ),
            Item::Module(items) => lines.push(vec![items.span.as_str(&inputs, |s| s.into())]),
            Item::Import(import) => lines.push(vec![import.span().as_str(&inputs, |s| s.into())]),
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

pub fn get_autoplay() -> bool {
    get_local_var("autoplay", || true)
}
pub fn set_autoplay(autoplay: bool) {
    set_local_var("autoplay", autoplay)
}

pub fn get_show_experimental() -> bool {
    get_local_var("show-experimental", || false)
}
pub fn set_show_experimental(show_experimental: bool) {
    set_local_var("show-experimental", show_experimental);
    update_style();
}

pub fn get_run_on_format() -> bool {
    get_local_var("run-on-format", || true)
}
pub fn set_run_on_format(run_on_format: bool) {
    set_local_var("run-on-format", run_on_format);
    update_style();
}

pub fn get_inlay_values() -> bool {
    get_local_var("inlay-values", || false)
}
pub fn set_inlay_values(inlay_values: bool) {
    set_local_var("inlay-values", inlay_values);
}

pub fn get_animation_format() -> String {
    get_local_var("animation-format", || "GIF".into())
}
pub fn set_animation_format(animation_format: &str) {
    set_local_var("animation-format", animation_format);
}

pub fn get_april_fools_setting() -> bool {
    get_local_var("april-fools", || true)
}
pub fn get_april_fools_time() -> bool {
    let date = Date::new_0();
    date.get_month() == 3 && date.get_date() == 1
}
pub fn set_april_fools(enabled: bool) {
    set_local_var("april-fools", enabled);
    update_style();
}
fn get_april_fools() -> bool {
    #[cfg(target_arch = "wasm32")]
    {
        get_april_fools_setting() && get_april_fools_time()
    }
    #[cfg(not(target_arch = "wasm32"))]
    false
}
pub fn very_gay() -> bool {
    get_gayness() == Gayness::VeryGay || get_april_fools()
}
pub fn at_least_a_little_gay() -> bool {
    get_gayness() >= Gayness::Ally || get_april_fools()
}
pub fn its_called_weewuh() -> bool {
    get_april_fools()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Gayness {
    None,
    Ally,
    VeryGay,
}
impl Gayness {
    pub const fn str(&self) -> &'static str {
        match self {
            Gayness::None => "None 😢",
            Gayness::Ally => "Ally",
            Gayness::VeryGay => "Very Gay",
        }
    }
}
impl From<&str> for Gayness {
    fn from(s: &str) -> Self {
        if s == Gayness::None.str() {
            Gayness::None
        } else if s == Gayness::VeryGay.str() {
            Gayness::VeryGay
        } else {
            Gayness::Ally
        }
    }
}
pub fn get_gayness() -> Gayness {
    #[cfg(target_arch = "wasm32")]
    {
        get_local_var("gayness", || Gayness::Ally.str().to_owned())
            .as_str()
            .into()
    }
    #[cfg(not(target_arch = "wasm32"))]
    Gayness::Ally
}
pub fn set_gayness(gayness: Gayness) {
    set_local_var("gayness", gayness.str());
    _ = window().location().reload();
}

fn update_style() {
    let font_name = get_font_name();
    let font_size = get_font_size();
    let show_experimental = if get_show_experimental() {
        "flex"
    } else {
        "none"
    };
    let run_on_format = if get_run_on_format() { "none" } else { "block" };
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
        .sized-code {{ font-size: {font_size}; }}\n\
        .experimental-glyph-buttons {{ display: {show_experimental}; }}\n\
        .format-button {{ display: {run_on_format}; }}",
    ));
    document().head().unwrap().append_child(&new_style).unwrap();
}

pub fn derive_title(input: &str) -> &str {
    if let Some(line) =
        (input.lines()).find(|line| line.starts_with('#') && !line.starts_with("# Experimental!"))
    {
        line[1..].trim()
    } else if let Some(line) =
        (input.lines()).find(|line| !line.trim().is_empty() && !line.starts_with("# Experimental!"))
    {
        line.trim()
    } else {
        "Pad"
    }
}

#[track_caller]
#[allow(clippy::manual_map)]
pub fn get_element<T: JsCast>(id: &str) -> Option<T> {
    if let Some(elem) = document().get_element_by_id(id) {
        Some(elem.dyn_into().unwrap())
    } else {
        None
    }
}

#[track_caller]
pub fn element<T: JsCast>(id: &str) -> T {
    if let Some(elem) = get_element(id) {
        elem
    } else {
        panic!("#{id} not found")
    }
}

pub fn format_insert_file_code(path: &Path, content: Vec<u8>) -> String {
    let function = match path.extension().and_then(|ext| ext.to_str()) {
        Some("ua") => "~",
        Some("txt") | Some("md") | Some("json") | None => "&fras",
        _ => "&frab",
    };

    let file_name = path.to_string_lossy().into_owned();
    let byte_count = content.len();
    if byte_count < 10000 {
        format!("{function} {file_name:?}\n")
    } else {
        format!("# {byte_count} bytes\n# {function} {file_name:?}\n")
    }
}
