use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt::Display,
    fs::{self, OpenOptions},
    io::{Read, Write},
    ops::Not,
    path::{Component, Path, PathBuf},
    str::FromStr,
};

use clap::{Parser, Subcommand};
use either::Either;
use serde::Serialize;
use serde_json::json;
use site::markdown::markdown_html;
use uiua::{PrimClass, PrimDoc, Primitive};

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Generate primitives.json file
    PrimitivesJson {
        /// Where to write the generated JSON
        #[arg(default_value_t)]
        output: StdioFile,
    },
    /// Generate uiua.tmLanguage.json file
    SublimeGrammar {
        /// Where to write the generated JSON
        #[arg(default_value_t)]
        output: StdioFile,
    },
    /// Generate formatter configuration documentation
    FormatConfigDocs {
        /// Where to write the generated documentation
        #[arg(default_value_t)]
        output: StdioFile,
    },
    /// Render a Markdown page to HTML
    BlogPageHtml {
        /// Slug of the page
        slug: String,
        /// Markdown file
        #[arg(default_value_t)]
        source: StdioFile,
        /// HTML file
        #[arg(default_value_t)]
        dest: StdioFile,
    },
    /// Render multiple blog pages to HTML
    BlogHtml {
        /// Directory containing source and target files
        dir: PathBuf,
        /// Page list; defaults to `list.txt` inside the provided directory
        list: Option<StdioFile>,
    },
}

#[derive(Clone, Debug, Default)]
enum StdioFile {
    #[default]
    Stdio,
    File(PathBuf),
}

impl FromStr for StdioFile {
    type Err = <PathBuf as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "-" {
            Ok(Self::Stdio)
        } else {
            s.parse().map(Self::File)
        }
    }
}

impl Display for StdioFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdio => write!(f, "-"),
            Self::File(path_buf) => {
                if let Some(Component::Normal(_)) = path_buf.components().next() {
                    write!(f, "./{}", path_buf.display())
                } else {
                    write!(f, "{}", path_buf.display())
                }
            }
        }
    }
}

impl StdioFile {
    fn writer(&self) -> std::io::Result<impl Write> {
        match self {
            Self::Stdio => Ok(Either::Left(std::io::stdout())),
            Self::File(path) => OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)
                .map(Either::Right),
        }
    }
    fn reader(&self) -> std::io::Result<impl Read> {
        match self {
            StdioFile::Stdio => Ok(Either::Left(std::io::stdin())),
            StdioFile::File(path) => OpenOptions::new().read(true).open(path).map(Either::Right),
        }
    }
}

fn main() -> anyhow::Result<()> {
    let Cli { command } = Cli::parse();
    match command {
        Command::PrimitivesJson { output } => {
            serde_json::to_writer_pretty(output.writer()?, &primitives())?;
        }
        Command::SublimeGrammar { output } => {
            serde_json::to_writer_pretty(output.writer()?, &sublime_grammar())?;
        }
        Command::FormatConfigDocs { output } => {
            output
                .writer()?
                .write_all(uiua::format::format_cfg_docs().as_bytes())?;
        }
        Command::BlogPageHtml { source, dest, slug } => {
            let mut buf = String::new();
            source.reader()?.read_to_string(&mut buf)?;
            dest.writer()?
                .write_all(process_md(&buf, &slug).as_bytes())?;
        }
        Command::BlogHtml { dir, list } => {
            let mut buf = String::new();
            list.unwrap_or_else(|| StdioFile::File(dir.join("list.txt")))
                .reader()?
                .read_to_string(&mut buf)?;
            gen_blog_html(&dir, &buf)?;
        }
    }
    Ok(())
}

fn primitives() -> impl Serialize {
    #[derive(Serialize)]
    struct PrimDef {
        #[serde(skip_serializing_if = "Option::is_none")]
        ascii: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        glyph: Option<char>,
        #[serde(skip_serializing_if = "Option::is_none")]
        args: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        outputs: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        modifier_args: Option<usize>,
        class: String,
        description: String,
        #[serde(skip_serializing_if = "Not::not")]
        experimental: bool,
        #[serde(skip_serializing_if = "Not::not")]
        deprecated: bool,
    }

    Primitive::all()
        .map(|prim| {
            (
                prim.name(),
                PrimDef {
                    ascii: prim.ascii().map(|a| a.to_string()),
                    glyph: prim.glyph(),
                    args: prim.args(),
                    outputs: prim.outputs(),
                    modifier_args: prim.modifier_args(),
                    class: format!("{:?}", prim.class()),
                    description: PrimDoc::from(prim).short_text().into_owned(),
                    experimental: prim.is_experimental(),
                    deprecated: prim.is_deprecated(),
                },
            )
        })
        .collect::<BTreeMap<_, _>>()
}

fn sublime_grammar() -> impl Serialize {
    fn gen_group<'a>(
        prims: impl IntoIterator<Item = &'a Primitive, IntoIter: Clone>,
        additional: &str,
    ) -> String {
        let prims = prims.into_iter();
        let glyphs = prims
            .clone()
            .flat_map(|p| {
                p.glyph().map(|x| x.to_string()).into_iter().chain(
                    p.ascii()
                        .into_iter()
                        .map(|x| x.to_string())
                        .filter(|x| x.len() == 1),
                )
            })
            .map(|x| match x.as_str() {
                r"\" | "-" | "*" | "^" => format!(r"\{x}"),
                _ => x,
            })
            .collect::<String>();
        let names = prims
            .map(|p| {
                let name = p.name();
                let min_len = if name.starts_with('&') {
                    name.len()
                } else {
                    (2..=name.len())
                        .find(|&n| Primitive::from_format_name(&name[..n]) == Some(*p))
                        .unwrap()
                };
                let mut chars = name.chars();
                let pref: String = chars.by_ref().take(min_len).collect();
                let re: String = chars
                    .clone()
                    .flat_map(|c| ['(', c])
                    .chain(std::iter::repeat_n([')', '?'], chars.count()).flatten())
                    .collect();
                format!("{pref}{re}")
            })
            .collect::<Vec<_>>()
            .join("|");
        format!(r#"[{glyphs}]|(?<![a-zA-Z$])({names})(?![a-zA-Z]){additional}"#)
    }

    #[derive(Default, Debug)]
    struct PrimGroups {
        stack: Vec<Primitive>,
        funcs: [Vec<Primitive>; 4],
        mods: [Vec<Primitive>; 3],
    }

    let PrimGroups { stack, funcs, mods } = Primitive::non_deprecated()
        .filter(|x| x.class() != PrimClass::Constant)
        .fold(PrimGroups::default(), |mut acc, cur| {
            match (cur.args(), cur.modifier_args(), cur.class()) {
                (_, Some(x), _) => acc.mods.get_mut(x),
                (Some(_), None, PrimClass::Stack | PrimClass::Debug | PrimClass::Planet) => {
                    Some(&mut acc.stack)
                }
                (Some(x), None, _) => acc.funcs.get_mut(x),
                (None, None, _) => panic!("Expected {cur:?} to be a function or a modifier"),
            }
            .unwrap_or_else(|| {
                panic!(
                    "Couldn't fit {cur:?} (takes {:?} args and {:?} modifier args)",
                    cur.args(),
                    cur.modifier_args()
                )
            })
            .push(cur);
            acc
        });
    assert_eq!(mods[0], [], "noadic modifiers don't exist");
    let stack_functions = gen_group(&stack, "");
    let noadic_functions = gen_group(&funcs[0], "");
    let monadic_functions = gen_group(&funcs[1], "|⋊[a-zA-Z]*");
    let dyadic_functions = gen_group(&funcs[2], "");
    let triadic_functions = gen_group(&funcs[3], "");
    let monadic_modifiers = gen_group(&mods[1], "");
    let dyadic_modifiers = gen_group(&mods[2], "");

    json!({
        "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
        "name": "Uiua",
        "firstLineMatch": r#"^#!/.*\buiua\b"#,
        "scopeName": "source.uiua",
        "fileTypes": ["ua"],
        "patterns": [
            { "include": "#comments" },
            { "include": "#strings-multiline-format" },
            { "include": "#strings-multiline" },
            { "include": "#strings-format" },
            { "include": "#strings-normal" },
            { "include": "#characters" },
            { "include": "#labels" },
            { "include": "#module_delim" },
            { "include": "#strand" },
            { "include": "#stack" },
            { "include": "#noadic" },
            { "include": "#monadic" },
            { "include": "#dyadic" },
            { "include": "#triadic" },
            { "include": "#mod1" },
            { "include": "#mod2" },
            { "include": "#idents" },
            { "include": "#numbers" },
        ],
        "repository": {
            "idents": {
                "name": "variable.parameter.uiua",
                "match": r#"\b[a-zA-Z]+(₋?[₀₁₂₃₄₅₆₇₈₉]|,`?\d+)*[!‼]*\b"#,
            },
            "comments": {
                "name": "comment.line.uiua",
                "match": r#"(#.*$|$[a-zA-Z]*)"#,
            },
            "strings-normal": {
                "name": "constant.character.escape",
                "begin": "\"",
                "end": "\"",
                "patterns": [
                    {
                        "name": "string.quoted",
                        "match": r#"\\[\\"0nrt]"#,
                    },
                ],
            },
            "strings-format": {
                "name": "constant.character.escape",
                "begin": r#"\$""#,
                "end": r#"""#,
                "patterns": [
                    {
                        "name": "string.quoted",
                        "match": r#"\\[\\"0nrt_]"#,
                    },
                    {
                        "name": "constant.numeric",
                        "match": r#"(?<!\\)_"#,
                    },
                ]
            },
            "strings-multiline": {
                "name": "constant.character.escape",
                "begin": r#"\$ "#,
                "end": "$",
            },
            "strings-multiline-format": {
                "name": "constant.character.escape",
                "begin": r#"\$\$ "#,
                "end": "$",
                "patterns": [
                    {
                        "name": "constant.numeric",
                        "match": r#"(?<!\\)_"#,
                    },
                ],
            },
            "characters": {
                "name": "constant.character.escape",
                "match": r#"@(\\(x[0-9A-Fa-f]{2}|u[0-9A-Fa-f]{4}|.)|.)"#,
            },
            "labels": {
                "name": "label.uiua",
                "match": r#"\$[a-zA-Z]*"#,
            },
            "numbers": {
                "name": "constant.numeric.uiua",
                "match": r#"[`¯]?(\d+|η|π|τ|∞|eta|pi|tau|inf(i(n(i(t(y)?)?)?)?)?)([./]\d+|e[+-]?\d+)?"#,
            },
            "strand": {
                "name": "comment.line",
                "match": "(_|‿)",
            },
            "module_delim": {
                "match": "---",
            },
            "stack": {
                "match": stack_functions,
            },
            "noadic": {
                "name": "entity.name.tag.uiua",
                "match": noadic_functions,
            },
            "monadic": {
                "name": "string.quoted",
                "match": monadic_functions,
            },
            "dyadic": {
                "name": "entity.name.function.uiua",
                "match": dyadic_functions,
            },
            // TODO: Separate entity?
            "triadic": {
                "name": "entity.name.function.uiua",
                "match": triadic_functions,
            },
            "mod1": {
                "name": "entity.name.type.uiua",
                "match": monadic_modifiers,
            },
            "mod2": {
                "name": "keyword.control.uiua",
                "match": dyadic_modifiers,
            },
        },
    })
}
fn process_md(content: &str, slug: &str) -> String {
    let mut lines: Vec<Cow<str>> = content.lines().map(Cow::Borrowed).collect();
    lines.insert(
        0,
        Cow::Borrowed("[Uiua](https://uiua.org)\n\n[Blog Home](https://uiua.org/blog)"),
    );
    lines.insert(
        3,
        Cow::Owned(format!(
            "\n\n**You can read this post with full editor \
                features [here](https://uiua.org/blog/{slug}).**\n\n"
        )),
    );
    markdown_html(&lines.join("\n"))
}

fn gen_blog_html(dir: &Path, list: &str) -> std::io::Result<()> {
    for slug in list
        .lines()
        .filter(|line| !(line.is_empty() || line.starts_with('#')))
        .map(|x| {
            x.split_once('(')
                .expect("slug should be followed by an opening parenthesis")
                .0
        })
    {
        let md_path = dir.join(format!("{slug}-text.md"));
        let html_path = dir.join(format!("{slug}-html.html"));
        eprintln!(
            "{slug}: {} -> {}...",
            md_path.display(),
            html_path.display()
        );
        let markdown = fs::read_to_string(&md_path)?;
        fs::write(html_path, process_md(&markdown, slug))?;
    }
    Ok(())
}
