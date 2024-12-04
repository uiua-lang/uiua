use leptos::*;
use leptos_meta::*;
use uiua_editor::Editor;

fn idioms() -> impl IntoView {
    let src = include_str!("../text/idioms.ua");
    let mut idioms = Vec::new();
    for text in src.split("\n\n").flat_map(|s| s.split("\r\n\r\n")) {
        logging::log!("{text:?}");
        let mut comment = String::new();
        let mut code = String::new();
        let mut lines = text.split('\n').filter(|line| !line.is_empty());
        for (i, line) in lines.by_ref().enumerate() {
            if line.starts_with("# ") {
                if i > 0 {
                    comment.push('\n');
                }
                comment.push_str(line.trim_start_matches("# "));
            } else {
                code.push_str(line);
                for line in lines {
                    code.push('\n');
                    code.push_str(line);
                }
                break;
            }
        }
        idioms.push(view!(<tr>
            <td style="text-wrap: wrap"><pre>{comment}</pre></td>
            <td style="width: 62%"><Editor example={&code}/></td>
        </tr>));
    }
    idioms
}

#[component]
pub fn Idioms() -> impl IntoView {
    view! {
        <Title text="Idioms - Uiua Docs"/>
        <h1>"Idioms"</h1>
        <p>"This page contains short Uiua idioms that may be non-obvious from simply knowing the primitives themselves."</p>
        <p>"You can contribute to this list by submitted a PR to the repo. The list is defined "<a href="https://github.com/uiua-lang/uiua/blob/main/site/text/idioms.ua">"here"</a>". Keep in mind these are meant to be both short and useful. Idioms above 10 character in length (without inputs), or which are not useful for everyday tasks, will not be accepted."</p>
        <table class="bordered-table" style="width: 100%">
            { idioms() }
        </table>
    }
}
