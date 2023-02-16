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
mod typed;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() == 1 {
        return Err("no arguments given".into());
    }

    let source = String::from_utf8(fs::read(&args[1])?)?;

    let tokens = tokenize(&source);
    println!("{tokens:#?}");

    let ast = parse_ast(&tokens).log_err()?;
    // println!("{ast:#?}");

    let mut app = analysis::App::new();

    app.insert_declarations(ast).log_err()?;
    println!("{app:#?}");

    app.type_check().log_err()?;

    let mut ir = ir::Ir::from_app(app);
    ir.optimize();

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
