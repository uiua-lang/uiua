use include_dir::{Dir, include_dir};
use uiua_editor::examples::{PadExample, PadExampleCategory, parse_example};

pub const LOGO: &str = include_str!("examples/logo.ua");
static EXAMPLES_DIRECTORY: Dir = include_dir!("site/src/examples");

const EXAMPLE_CATEGORY_ORDER: &[&str] = &["Basics", "Image", "Audio", "Animation"];

pub fn get_examples() -> Vec<PadExample> {
    EXAMPLES_DIRECTORY
        .files()
        .map(|file| {
            let file_path = file.path().to_string_lossy();
            let content = file
                .contents_utf8()
                .unwrap_or_else(|| panic!("Invalid example file {file_path}"));

            let parsed = parse_example(content);
            let meta = |key: &str| {
                parsed
                    .metadata
                    .get(key)
                    .unwrap_or_else(|| panic!("Missing `{key}` in example {file_path}"))
                    .to_string()
            };

            PadExample {
                path: file_path.to_string(),
                title: meta("title"),
                category: meta("category"),
                content: parsed.content,
            }
        })
        .collect()
}

pub fn get_categorized_examples() -> Vec<PadExampleCategory> {
    let mut categories: Vec<PadExampleCategory> = Vec::new();
    let mut unrecognized: Vec<PadExample> = Vec::new();

    for category in EXAMPLE_CATEGORY_ORDER {
        categories.push(PadExampleCategory {
            title: category.to_string(),
            items: vec![],
        });
    }

    for example in get_examples() {
        match categories.iter_mut().find(|c| c.title == example.category) {
            Some(category) => category.items.push(example),
            None => unrecognized.push(example),
        }
    }

    if !unrecognized.is_empty() {
        categories.push(PadExampleCategory {
            title: "Unrecognized category".to_string(),
            items: unrecognized,
        });
    }

    for category in &mut categories {
        category.items.sort_by(|a, b| a.title.cmp(&b.title));
    }

    categories
}

#[cfg(test)]
#[test]
fn test_examples() {
    use uiua_editor::backend::WebBackend;

    for example in get_examples() {
        let example_path = example.path;
        let example_content = example.content;

        match uiua::Uiua::with_backend(WebBackend::default()).run_str(&example_content) {
            Ok(mut comp) => {
                if let Some(diag) = comp.take_diagnostics().into_iter().next() {
                    panic!("Example failed:\n{example_path}\n{example_content}\n{diag}");
                }
            }
            Err(e) => panic!("Example failed:\n{example_path}\n{example_content}\n{e}"),
        }
    }
}
