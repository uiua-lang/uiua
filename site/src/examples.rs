use std::collections::HashMap;

use include_dir::{Dir, include_dir};
use uiua_editor::examples::{PadExample, PadExampleCategory, parse_example};

pub const LOGO: &str = include_str!("examples/logo.ua");
static EXAMPLES_DIRECTORY: Dir = include_dir!("site/src/examples");

const EXAMPLE_CATEGORY_ORDER: &[&str] = &["Basics", "Image", "Animation", "Audio"];

pub fn get_examples() -> Vec<PadExample> {
    EXAMPLES_DIRECTORY
        .files()
        .map(|file| {
            let file_path = file.path().to_string_lossy();
            let content = file
                .contents_utf8()
                .unwrap_or_else(|| panic!("Invalid example file {file_path}"));
            parse_example(file_path.into(), content)
        })
        .collect()
}

pub fn get_categorized_examples() -> Vec<PadExampleCategory> {
    let mut categories: HashMap<String, Vec<PadExample>> = HashMap::new();
    for example in get_examples() {
        categories
            .entry(example.category.clone())
            .or_default()
            .push(example);
    }
    let mut categories: Vec<PadExampleCategory> = categories
        .into_iter()
        .map(|(title, items)| PadExampleCategory { title, items })
        .collect();
    categories.sort_by_key(|cat| {
        (EXAMPLE_CATEGORY_ORDER.iter())
            .position(|&title| title == cat.title)
            .unwrap_or(usize::MAX)
    });
    for category in &mut categories {
        (category.items)
            .sort_by(|a, b| (a.precedence.cmp(&b.precedence)).then_with(|| a.title.cmp(&b.title)));
    }
    categories
}

#[cfg(test)]
#[test]
fn test_examples() {
    use uiua_editor::backend::WebBackend;
    for example in get_examples() {
        let PadExample { path, content, .. } = example;
        match uiua::Uiua::with_backend(WebBackend::default()).run_str(&content) {
            Ok(mut comp) => {
                if let Some(diag) = comp.take_diagnostics().into_iter().next() {
                    panic!("Example failed:\n{path}\n{content}\n{diag}");
                }
            }
            Err(e) => panic!("Example failed:\n{path}\n{content}\n{e}"),
        }
    }
}
