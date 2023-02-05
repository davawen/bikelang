use crate::{
    ast::{parse_ast, parse_func_def},
    token::tokenize, utility::Inspect,
};

mod utility;
mod ast;
mod analysis;
// mod ir;
mod token;

// fn parse(mut tokens: &[Token]) -> Node {
//     /// Returns the index of the matching brace or parenthesis
//     /// The start token should be included in `tokens`
//
//     let mut out = Vec::new();
//
//     while !tokens.is_empty() {
//     }
//
//     out
// }

const SOURCE: &str = r#"
func main(i32 a, i32 b) -> void {
    a = another("hi!");
} 

func another(str val) -> i32 {
}
"#;

fn main() {
    let tokens = tokenize(SOURCE);
    println!("{tokens:#?}");

    let ast = parse_ast(&tokens);
    println!("{ast:#?}");

    let mut app = analysis::App::new();

    app.insert_declarations(ast).my_inspect_err(|e| eprintln!("{e}")).expect("Couldn't parse function declarations");
    println!("{app:#?}");

    app.type_check().my_inspect_err(|e| eprintln!("{e}")).unwrap();

    // app.integrate_definitions().expect("Couldn't parse function bodies");
    // println!("{app:#?}");
}
