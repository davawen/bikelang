use std::fs;

use crate::{
    ast::parse_ast,
    token::tokenize, utility::Inspect,
};

mod utility;
mod ast;
mod analysis;
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
func main() -> void {
    i32 idx = 0;
    loop {
        if idx < 50 {
            print#("Small number: ", idx, "\n");
        };
        if idx >= 50 {
            if idx < 100 {
                print#("Medium Number: ", idx, "\n");
            }
        };
        if idx >= 100 {
            print#("Big number: ", idx, "\n")
        };
        if idx >= 150 {
            asm#("jmp .label1")
        };
        idx = idx + 3;
    }
} 
"#;
    // print#("The number is: ", a);
/*

func another(str val) -> i32 {
    val = "Hiya!";
    print#(val);

    0
}*/

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let tokens = tokenize(SOURCE);
    println!("{tokens:#?}");

    let ast = parse_ast(&tokens).log_err()?;
    println!("{ast:#?}");

    let mut app = analysis::App::new();

    app.insert_declarations(ast).log_err()?;
    // println!("{app:#?}");

    app.type_check().log_err()?;

    let ir = ir::Ir::from_app(app);
    println!("{ir:#?}");
    println!("{}", ir.generate_asm());

    fs::write("out.asm", ir.generate_full())?;

    println!("$ nasm -f elf64 out.asm");
    let out = std::process::Command::new("nasm")
        .args(["-f", "elf64", "out.asm"])
        .output().expect("failed to assemble");

    if !out.status.success() {
        println!("error: {}", String::from_utf8(out.stderr)?);
    }

    println!("$ ld out.o");
    let out = std::process::Command::new("ld")
        .arg("out.o")
        .output().expect("failed to link");

    if !out.status.success() {
        println!("error: {}", String::from_utf8(out.stderr)?);
    }

    println!("Compilation successful!");
    Ok(())
}
