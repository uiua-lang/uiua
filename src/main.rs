use uiua::{lex::lex, parse::parse};

fn main() {
    let path = "test.uiua";
    let input = std::fs::read_to_string(path).unwrap();
    println!("tokens:");
    match lex(&input, path.as_ref()) {
        Ok(tokens) => {
            for token in tokens {
                println!("{token:?}");
            }
        }
        Err(error) => {
            println!("{error}");
        }
    };
    println!("\nAST:");
    match parse(&input, path.as_ref()) {
        Ok(items) => {
            for item in items {
                println!("{item:?}");
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{error}");
            }
        }
    }
}
