#![feature(box_syntax, box_patterns)]

use std::fs;

use crate::{
    token::Lexer, utility::Inspect, ast::parse_ast,
};

mod utility;
mod ast;
// mod old_ast;
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

    let mut lexer = Lexer::new(&source);

    let ast = parse_ast(&mut lexer);
    // println!("{ast:#?}");

    let mut app = analysis::App::new();

    app.insert_declarations(ast).log_err()?;
    let app = app.type_check().log_err()?;

    println!("{app:#?}");

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
