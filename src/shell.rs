use std::{
    borrow::Cow,
    env, fs, io,
    path::{MAIN_SEPARATOR_STR, Path, PathBuf},
    process::Command,
};

use reedline::*;

pub fn shell() {
    let mut keybindings = default_emacs_keybindings();
    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("completion_menu".to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );
    let edit_mode = Box::new(Emacs::new(keybindings));

    let mut line_editor = Reedline::create()
        .with_completer(Box::new(UiuaCompleter))
        .with_menu(ReedlineMenu::EngineCompleter(Box::new(
            ColumnarMenu::default().with_name("completion_menu"),
        )))
        .with_edit_mode(edit_mode);

    let prompt = UiuaPrompt::default();
    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                if buffer.trim().is_empty() {
                    continue;
                }
                let mut commands = vec![vec![String::new()]];
                let mut in_string = None;
                let mut escaped = false;
                for c in buffer.chars() {
                    let sub = commands.last_mut().unwrap();
                    let arg = sub.last_mut().unwrap();
                    match c {
                        '\\' if in_string.is_some() && escaped => arg.push('\\'),
                        '\\' if in_string.is_some() => escaped = true,
                        '\'' | '"' | '`' if !escaped && in_string == Some(c) => {
                            in_string = None;
                            sub.push(String::new());
                        }
                        '\'' | '"' | '`' if in_string.is_none() => in_string = Some(c),
                        c if in_string.is_none() && c.is_whitespace() && !arg.is_empty() => {
                            sub.push(String::new())
                        }
                        c if in_string.is_some() && c.is_whitespace() => arg.push(c),
                        c => {
                            arg.push(c);
                            escaped = false;
                        }
                    }
                }
                let mut children = Vec::new();
                for sub in &mut commands {
                    sub.pop_if(|s| s.is_empty());
                    let command = sub.remove(0);
                    match Command::new(&command).args(sub).spawn() {
                        Ok(child) => children.push(child),
                        Err(e) => match e.kind() {
                            io::ErrorKind::NotFound => eprintln!("`{command}` not found"),
                            _ => eprintln!("{e}"),
                        },
                    }
                }
                for mut child in children {
                    _ = child.wait();
                }
            }
            Ok(Signal::CtrlD | Signal::CtrlC | Signal::ExternalBreak(_)) => {
                break;
            }
            Ok(_) => {}
            Err(_) => {}
        }
    }
}

struct UiuaPrompt(DefaultPrompt);
impl Default for UiuaPrompt {
    fn default() -> Self {
        UiuaPrompt(DefaultPrompt {
            left_prompt: DefaultPromptSegment::WorkingDirectory,
            right_prompt: DefaultPromptSegment::Empty,
        })
    }
}
impl Prompt for UiuaPrompt {
    fn render_prompt_left(&self) -> Cow<'_, str> {
        self.0.render_prompt_left()
    }
    fn render_prompt_right(&self) -> Cow<'_, str> {
        self.0.render_prompt_right()
    }
    fn render_prompt_indicator(&self, prompt_mode: PromptEditMode) -> Cow<'_, str> {
        self.0.render_prompt_indicator(prompt_mode)
    }
    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        self.0.render_prompt_multiline_indicator()
    }
    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<'_, str> {
        self.0
            .render_prompt_history_search_indicator(history_search)
    }
}

struct UiuaCompleter;
impl Completer for UiuaCompleter {
    fn complete(&mut self, line: &str, _pos: usize) -> CompletionResult {
        let mut suggestions = Vec::new();
        let prefix = if let Some(path) = line.split_whitespace().last() {
            PathBuf::from(path)
        } else {
            env::current_dir().unwrap_or_default()
        };
        let mut span = Span::new(line.len(), line.len());
        if let Ok(read_dir) = fs::read_dir(&prefix) {
            for entry in read_dir.flatten() {
                let path = entry.path();
                if path.starts_with(&prefix)
                    && let Some(file_name) = path.file_name().and_then(|name| name.to_str())
                {
                    let slash = if path.ends_with(MAIN_SEPARATOR_STR) {
                        ""
                    } else {
                        MAIN_SEPARATOR_STR
                    };
                    suggestions.push(Suggestion {
                        value: format!("{slash}{file_name}"),
                        display_override: Some(file_name.into()),
                        span,
                        append_whitespace: path.is_file(),
                        ..Default::default()
                    })
                }
            }
        } else {
            let parent = prefix
                .parent()
                .filter(|p| *p != "")
                .unwrap_or_else(|| Path::new("."));
            if let Ok(read_dir) = fs::read_dir(parent)
                && let Some(file_prefix) = prefix.file_name().and_then(|name| name.to_str())
            {
                for entry in read_dir.flatten() {
                    let path = entry.path();
                    if let Some(file_name) = path
                        .file_name()
                        .and_then(|name| name.to_str())
                        .filter(|name| name.starts_with(file_prefix))
                    {
                        let slash = if path.is_dir() {
                            MAIN_SEPARATOR_STR
                        } else {
                            ""
                        };
                        let value = if parent == "." {
                            span.start -= file_prefix.len();
                            format!(".{MAIN_SEPARATOR_STR}{file_name}")
                        } else {
                            format!("{}{slash}", &file_name[file_prefix.len()..])
                        };
                        suggestions.push(Suggestion {
                            value,
                            display_override: Some(file_name.into()),
                            span,
                            append_whitespace: path.is_file(),
                            ..Default::default()
                        })
                    }
                }
            }
        }
        CompletionResult::Fresh {
            suggestions: suggestions.into(),
            partial: None,
        }
    }
}
