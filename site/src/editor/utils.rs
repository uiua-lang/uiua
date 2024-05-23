use std::{
    cell::Cell,
    iter,
    mem::{replace, take},
    str::FromStr,
    time::Duration,
};

use base64::engine::{general_purpose::URL_SAFE, Engine};
use image::ImageOutputFormat;
use leptos::*;

use uiua::{
    ast::Item,
    image_to_bytes,
    lsp::{spans_with_backend, BindingDocsKind},
    value_to_gif_bytes, value_to_image, value_to_wav_bytes, Compiler, DiagnosticKind, Inputs,
    Report, ReportFragment, ReportKind, SpanKind, SysBackend, Uiua, UiuaError, UiuaResult, Value,
};
use unicode_segmentation::UnicodeSegmentation;
use wasm_bindgen::JsCast;
use web_sys::{
    Comment, Event, HtmlStyleElement, HtmlTextAreaElement, KeyboardEvent, MouseEvent, Node,
};

use crate::{
    backend::{OutputItem, WebBackend},
    binding_class,
    editor::Editor,
    element, prim_class, sig_class,
};

/// Handles setting the code in the editor, setting the cursor, and managing the history
#[derive(Clone)]
pub struct State {
    pub code_id: String,
    pub set_line_count: WriteSignal<usize>,
    pub set_copied_link: WriteSignal<bool>,
    pub past: Vec<Record>,
    pub future: Vec<Record>,
    pub curr: Record,
    pub challenge: Option<ChallengeDef>,
    pub loading_module: Cell<bool>,
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
        let maybe_before = get_code_cursor_impl(&self.code_id);
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
    fn set_code_element(&mut self, code: &str) {
        logging::log!("set code: {code:?}");
        let area = element::<HtmlTextAreaElement>(&self.code_id);
        area.set_value(code);
        area.style().set_property("height", "auto").unwrap();
        let scroll_height = area.scroll_height();
        area.style()
            .set_property("height", &format!("{}px", scroll_height))
            .unwrap();
    }
    pub fn set_cursor(&self, to: (u32, u32)) {
        set_code_cursor(&self.code_id, to.0, to.1);
    }
    fn set_changed(&self) {
        self.set_copied_link.set(false);
        self.set_line_count();
    }
    fn set_line_count(&self) {
        self.set_line_count
            .set(get_code(&self.code_id).split('\n').count().max(1));
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

pub fn get_code(id: &str) -> String {
    element::<HtmlTextAreaElement>(id).value()
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

fn update_style() {
    let font_name = get_font_name();
    let font_size = get_font_size();
    let show_experimental = if get_show_experimental() {
        "block"
    } else {
        "none"
    };
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
        .experimental-glyph-button {{ display: {show_experimental}; }}",
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
    .filter(|node| node.dyn_ref::<Comment>().is_none())
}

pub fn get_code_cursor_impl(id: &str) -> Option<(u32, u32)> {
    let area = element::<HtmlTextAreaElement>(id);
    let start = area.selection_start().unwrap()?;
    let end = area.selection_end().unwrap()?;
    Some((start, end))
}

fn set_code_cursor(id: &str, start: u32, end: u32) {
    // logging::log!("set_code_cursor({}, {})", start, end);
    let area = element::<HtmlTextAreaElement>(id);
    area.set_selection_range(start, end).unwrap();
}

#[derive(Debug)]
enum CodeFragment {
    Unspanned(String),
    Br,
    Span(String, SpanKind),
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

fn build_code_lines(code: &str) -> CodeLines {
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
    for span in spans_with_backend(code, WebBackend::default()).0 {
        let kind = span.value;
        let span = span.span;
        push_unspanned(&mut lines, span.start.char_pos as usize, &mut end);

        let text: String = chars[span.start.char_pos as usize..span.end.char_pos as usize]
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

    lines
}

pub fn gen_code_view(code: &str) -> View {
    // logging::log!("gen_code_view({code:?})");
    let CodeLines { frags } = build_code_lines(code);
    let mut line_views = Vec::new();
    for line in frags {
        if line.is_empty() {
            line_views.push(view!(<div class="code-line"><br/></div>));
            continue;
        }
        let mut frag_views = Vec::new();
        for frag in line {
            match frag {
                CodeFragment::Unspanned(s) => {
                    // logging::log!("unspanned escaped: `{}`", s);
                    frag_views.push(view!(<span class="code-span">{s}</span>).into_view())
                }
                CodeFragment::Br => frag_views.push(view!(<br/>).into_view()),
                CodeFragment::Span(text, kind) => {
                    let color_class = match &kind {
                        SpanKind::Primitive(prim) => prim_class(*prim),
                        SpanKind::Number => "number-literal",
                        SpanKind::String => "string-literal-span",
                        SpanKind::Comment | SpanKind::OutputComment => "comment-span",
                        SpanKind::Strand => "strand-span",
                        SpanKind::StackSwizzle(sw) => sig_class(sw.signature()),
                        SpanKind::ArraySwizzle(sw) => sig_class(sw.signature()),
                        _ => "",
                    };
                    match kind {
                        SpanKind::Primitive(prim) => {
                            let name = prim.name();
                            let mut title = format!("{}: {}", name, prim.doc().short_text());
                            if let Some(ascii) = prim.ascii() {
                                title = format!("({}) {}", ascii, title);
                            }
                            let class =
                                format!("code-span code-hover code-underline {}", color_class);
                            let onmouseover = move |event: web_sys::MouseEvent| update_ctrl(&event);
                            let onclick = move |event: web_sys::MouseEvent| {
                                if os_ctrl(&event) {
                                    window()
                                        .open_with_url_and_target(
                                            &format!("/docs/{}", prim.name()),
                                            "_blank",
                                        )
                                        .unwrap();
                                }
                            };
                            let view = view!(<span
                                    class=class
                                    data-title=title
                                    on:mouseover=onmouseover
                                    on:click=onclick>{text}</span>)
                            .into_view();
                            frag_views.push(view)
                        }
                        SpanKind::String => {
                            let class = format!("code-span code-hover {}", color_class);
                            if text == "@ " {
                                let space_class =
                                    format!("code-span code-hover space-character {}", color_class);
                                frag_views.push(
                                    view!(
                                        <span class=class data-title="space character">@</span>
                                        <span class=space_class data-title="space character">" "</span>
                                    ).into_view(),
                                )
                            } else {
                                let title = if text.starts_with('@') {
                                    "character"
                                } else {
                                    "string"
                                };
                                frag_views.push(
                                    view!(<span class=class data-title=title>{text}</span>)
                                        .into_view(),
                                )
                            }
                        }
                        SpanKind::Signature => {
                            let class = format!("code-span code-hover {}", color_class);
                            let title = format!("{kind:?}").to_lowercase();
                            frag_views.push(
                                view!(<span class=class data-title=title>{text}</span>).into_view(),
                            )
                        }
                        SpanKind::Placeholder(op) => {
                            let class = format!("code-span code-hover {}", color_class);
                            let title = format!("placeholder {}", op.name());
                            frag_views.push(
                                view!(<span class=class data-title=title>{text}</span>).into_view(),
                            )
                        }
                        SpanKind::StackSwizzle(sw) => {
                            let class = format!("code-span code-hover {}", color_class);
                            let title = format!("stack swizzle {}", sw.signature());
                            frag_views.push(
                                view!(<span class=class data-title=title>{text}</span>).into_view(),
                            )
                        }
                        SpanKind::ArraySwizzle(sw) => {
                            let class = format!("code-span code-hover {}", color_class);
                            let title = format!("array swizzle {}", sw.signature());
                            frag_views.push(
                                view!(<span class=class data-title=title>{text}</span>).into_view(),
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
                                let t = mul * (c.saturating_sub(b'a') as f32 / 26.0);
                                let target = MIN + (MAX - MIN) * t;
                                components[j] = components[j].max(target);
                            }
                            // Normalize to a pastel color
                            for c in &mut components {
                                *c = 0.5 + 0.5 * *c;
                            }
                            let components = components.map(|c| (c * 255.0).round() as u8);
                            let style = format!(
                                "color: rgb({}, {}, {})",
                                components[0], components[1], components[2]
                            );
                            frag_views.push(
                                view!(<span class="code-span code-hover" style=style data-title="label">{text}</span>)
                                    .into_view(),
                            )
                        }
                        SpanKind::FuncDelim(sig) => {
                            let class = format!("code-span code-hover {}", color_class);
                            let title = sig.to_string();
                            frag_views.push(
                                view!(<span class=class data-title=title>{text}</span>).into_view(),
                            )
                        }
                        SpanKind::Ident(Some(docs)) => {
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

                            let private =
                                if docs.is_public || matches!(docs.kind, BindingDocsKind::Module) {
                                    ""
                                } else {
                                    if !title.is_empty() {
                                        title.push(' ');
                                    }
                                    title.push_str("(private) ");
                                    "private-binding"
                                };
                            if let Some(comment) = &docs.comment {
                                if !title.is_empty() {
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
                                    BindingDocsKind::Module => title.push_str("module"),
                                    BindingDocsKind::Modifier(_) => title.push_str("macro"),
                                    _ => {}
                                }
                            }
                            let class = format!(
                                "code-span code-hover {} {}",
                                binding_class(&text, &docs),
                                private
                            );
                            frag_views.push(
                                view!(<span class=class data-title=title>{text}</span>).into_view(),
                            )
                        }
                        _ => {
                            let class = format!("code-span {color_class}");
                            frag_views.push(view!(<span class=class>{text}</span>).into_view())
                        }
                    }
                }
            }
        }
        line_views.push(view!(<div class="code-line">{frag_views}</div>))
    }
    line_views.into_view()
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
            ))
            .0;
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
                let mut output = run_code_single(&user_input).0;
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
            let (output, error) = run_code_single(code);
            self.loading_module.set(false);
            if let Some(error) = error {
                if error.to_string().contains("Waiting for module") {
                    self.loading_module.set(true);
                }
            }
            output
        }
    }
}

fn run_code_single(code: &str) -> (Vec<OutputItem>, Option<UiuaError>) {
    // Run
    let mut rt = init_rt();
    let mut error = None;
    let mut comp = Compiler::with_backend(WebBackend::default());
    let comp_backend;
    let (mut values, io) = match comp.load_str(code).map(|comp| rt.run_compiler(comp)) {
        Ok(Ok(_)) => (
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
        // Try to convert the value to SVG
        if let Ok(mut str) = value.as_string(&rt, "") {
            if str.starts_with("<svg") && str.ends_with("</svg>") {
                if !str.contains("xmlns") {
                    str = str.replacen("<svg", "<svg xmlns=\"http://www.w3.org/2000/svg\"", 1);
                }
                stack.push(OutputItem::Svg(str));
                continue;
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
            ReportFragment::Colored(s, kind) => {
                let class = match kind {
                    ReportKind::Error => "output-report output-error",
                    ReportKind::Diagnostic(DiagnosticKind::Warning) => {
                        "output-report output-warning"
                    }
                    ReportKind::Diagnostic(DiagnosticKind::Advice) => "output-report output-advice",
                    ReportKind::Diagnostic(DiagnosticKind::Style) => "output-report output-style",
                    ReportKind::Diagnostic(DiagnosticKind::Info) => "output-report output-info",
                };
                view!(<span class=class>{s}</span>).into_view()
            }
            ReportFragment::Newline => view!(<br/>).into_view(),
        });
    }
    view! {
        <div style="font-family: inherit">{frags}</div>
    }
}

#[derive(Clone)]
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
pub fn Challenge<'a, P: IntoView + 'static>(
    number: u8,
    prompt: P,
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
    view! {
        <div class="challenge">
            <h3>"Challenge "{number}</h3>
            <p>"Write a program that "<strong>{prompt}</strong>"."</p>
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
