use crate::{
    ast::{parse_ast, parse_func_def},
    token::tokenize, ir::App,
};

mod ast;
mod ir;
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
func main(int a, int b) -> float {
    int c = another(10 + 10);
} 

func another(int val) -> int {
    print#("Hello!");

    0;
}
"#;

fn main() {
    let tokens = tokenize(SOURCE);
    println!("{tokens:#?}");

    let ast = parse_ast(&tokens);
    println!("{ast:#?}");

    let mut app = App::new();

    app.get_declarations(&ast).expect("Couldn't parse function declarations");
    println!("{app:#?}");

    app.integrate_definitions().expect("Couldn't parse function bodies");
    println!("{app:#?}");
}
