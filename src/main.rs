use uiua::lex::lex;

fn main() {
    let path = "test.uiua";
    let input = std::fs::read_to_string(path).unwrap();
    for token in lex(&input, path.as_ref()).unwrap_or_else(|e| panic!("{e}")) {
        println!("{token:?}");
    }
}
