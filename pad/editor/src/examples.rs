use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct PadExampleCategory {
    pub title: String,
    pub items: Vec<PadExample>,
}

#[derive(Clone, Debug)]
pub struct PadExample {
    pub path: String,
    pub title: String,
    pub category: String,
    pub content: String,
}

pub struct ExampleFile {
    pub metadata: HashMap<String, String>,
    pub content: String,
}

pub fn parse_example(original_content: &str) -> ExampleFile {
    let full_content = original_content.replace("\r\n", "\n");
    let mut metadata = HashMap::new();
    let mut lines = full_content.lines();
    let mut content_start = 0;

    if let Some(first) = lines.next()
        && first.trim() == "# ---"
    {
        let mut offset = first.len() + 1;
        for line in lines {
            offset += line.len() + 1;
            let trimmed = line.trim();

            if trimmed == "# ---" {
                content_start = offset.min(full_content.len());
                break;
            }

            if let Some(rest) = trimmed.strip_prefix('#') {
                if let Some((key, value)) = rest.split_once(':') {
                    metadata.insert(
                        key.trim().to_string(),
                        value.trim().to_string(),
                    );
                }
            }
        }
    }

    ExampleFile {
        metadata,
        content: full_content[content_start..].to_string(),
    }
}
