#![feature(box_syntax, box_patterns)]

use std::fs;
use clap::{ Parser, ArgAction };

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

#[derive(Parser)]
struct Args {
    /// Show the generated AST
    #[arg(short)]
    ast: bool,

    /// Show the type analysis
    #[arg(short = 't')]
    analyze: bool,

    /// Show the intermediate representation
    #[arg(short)]
    ir: bool,

    /// Print the resulting assembly
    #[arg(short = 's')]
    asm: bool,
    prog: String
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let source = String::from_utf8(fs::read(args.prog).expect("A valid file"))?;

    let mut lexer = Lexer::new(&source);

    let ast = parse_ast(&mut lexer);

    if args.ast { println!("{ast:#?}") }

    let mut app = analysis::App::new();

    app.insert_declarations(ast).log_err()?;
    let app = app.type_check().log_err()?;

    if args.analyze { println!("{app:#?}") }

    let mut ir = ir::Ir::from_app(app);
    ir.optimize();

    if args.ir { println!("{ir:#?}") }
    if args.asm { println!("{}", ir.generate_asm()) }

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
