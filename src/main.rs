use crate::{
    ast::parse_ast,
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
    print#(val);
}
"#;

fn main() {
    let tokens = tokenize(SOURCE);
    println!("{tokens:#?}");

    let ast = parse_ast(&tokens).log_err().unwrap();
    println!("{ast:#?}");

    let mut app = analysis::App::new();

    app.insert_declarations(ast).log_err().expect("Couldn't parse function declarations");
    println!("{app:#?}");

    app.type_check().log_err().unwrap();

    // app.integrate_definitions().expect("Couldn't parse function bodies");
    // println!("{app:#?}");
}
