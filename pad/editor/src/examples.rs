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
    /// Order within a category
    pub precedence: u16,
}

pub fn parse_example(path: String, original_content: &str) -> PadExample {
    let mut content = original_content.replace("\r\n", "\n");
    let mut lines = content.lines();
    let title;
    let mut category = "Misc".into();
    let mut precedence = u16::MAX;

    if let Some(line) = lines.next()
        && let Some(first_com) = line.strip_prefix("# ")
    {
        title = if let Some((ti, cat)) = first_com.split_once(" :: ") {
            if let Some((cat, prec)) = cat.split_once(" :: ") {
                category = cat.into();
                if let Ok(prec) = prec.parse() {
                    precedence = prec;
                }
            } else {
                category = cat.into();
            }
            ti.into()
        } else {
            first_com.into()
        };
        content.drain(0..=line.len());
    } else {
        panic!("Example file missing title:\n{original_content}");
    }
    while content.ends_with('\n') {
        content.pop();
    }

    PadExample {
        path,
        title,
        category,
        content,
        precedence,
    }
}
