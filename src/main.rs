#![feature(box_syntax, box_patterns)]

use std::fs;
use clap::Parser;

use crate::{
    token::Lexer, utility::Inspect, ast::parse_ast, error::ToCompilerError,
};

mod utility;
mod error;
mod ast;
mod analysis;
// mod ir;
mod token;
mod typed;

#[derive(Parser)]
struct Args {
    /// Show lexed tokens and exit
    #[arg(short)]
    tokens: bool,

    /// Show the generated AST
    #[arg(short)]
    ast: bool,

    /// Show the type analysis
    #[arg(short = 'z')]
    analyze: bool,

    /// Show the intermediate representation
    #[arg(short)]
    ir: bool,

    /// Print the resulting assembly
    #[arg(short = 's')]
    asm: bool,
    prog: String
}

fn compile(args: Args, source: &str) -> error::Result<String> {
    let mut lexer = Lexer::new(source);

    if args.tokens {
        loop {
            let t = lexer.next();
            if t.token == token::Token::Eof {
                return Ok(String::new()); 
            }

            println!("{}: {:?}", &source[t.bounds], t.token)
        }
    }

    let ast = parse_ast(&mut lexer)?;

    if args.ast { println!("{ast:#?}") }

    let mut app = analysis::App::new();

    app.insert_declarations(ast)?;
    let app = app.type_check()?;

    if args.analyze { println!("{app:#?}") }
    Ok(String::new())
    //
    // let mut ir = ir::Ir::from_app(app);
    // ir.optimize();
    //
    // if args.ir { println!("{ir:#?}") }
    // if args.asm { println!("{}", ir.generate_asm()) }
    //
    // Ok(ir.generate_full())
}

fn main() {
    let args = Args::parse();
    let source = {
        let source = String::from_utf8(fs::read(&args.prog).expect("couldn't open file")).expect("invalid utf-8");

        // Very roundabout copy-heavy way to replace tabs with spaces but I couldn't find an easier one :(
        // Needed for nice error messages (need to replace any other multi-width characters)
        let mut out = String::with_capacity(source.capacity());
        for c in source.chars() {
            match c {
                '\t' => out.push_str("    "),
                c => out.push(c)
            }
        }
        out
    };

    match compile(args, &source) {
        Ok(asm) => {
            if asm.is_empty() { return; }

            fs::write("out.asm", asm).expect("couldn't write assembly to file");

            println!("$ nasm -f elf64 out.asm");
            let out = std::process::Command::new("nasm")
                .args(["-f", "elf64", "out.asm"])
                .output().expect("failed to execute nasm");

            if !out.status.success() {
                eprintln!("assembling error:\n{}", String::from_utf8(out.stderr).expect("invalid utf-8"));
                return;
            }

            println!("$ ld out.o");
            let out = std::process::Command::new("ld")
                .arg("out.o")
                .output().expect("failed to execute ld");

            if !out.status.success() {
                eprintln!("link error:\n{}", String::from_utf8(out.stderr).expect("invalid utf-8"));
                return;
            }

            println!("Compilation successful!");
        }
        Err(e) => {
            e.print(&source);
        }
    }
}
