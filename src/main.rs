use uiua::{ir::Ir, lex::lex, parse::parse};

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
    let (items, errors) = parse(&input, path.as_ref());
    for item in items {
        println!("{item:#?}");
    }
    for error in errors {
        println!("{error}");
    }
    println!("\nIR:");
    let mut ir = Ir::default();
    if let Err(e) = ir.compile(&input, path.as_ref()) {
        for e in e {
            println!("{e}");
        }
    }
    println!("{ir:#?}");
}
