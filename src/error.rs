use std::fmt::Display;

use crate::{ast::{AstError, self, analysis::AnalysisError}, typed::TypeError, token, utility::Bounds};

#[derive(Debug)]
pub struct CompilerError {
    error: ErrorType,
    bounds: Bounds
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("compile error!\n")
    }
}
impl std::error::Error for CompilerError {}

#[derive(Debug)]
enum ErrorType {
    Ast(AstError),
    Analysis(AnalysisError),
    Type(TypeError)
}

impl CompilerError {
    pub fn print(&self, source: &str) {
        println!("\x1b[31mcompile error\x1b[0m: {}",
            match &self.error {
                ErrorType::Ast(e) => format!("{e}"),
                ErrorType::Analysis(e) => format!("{e}"),
                ErrorType::Type(e) => format!("{e}")
            }
        );

        // Print is allowed to be horribly inefficient as it should only ever be called once
        let line_numbers: Vec<_> = source.char_indices().filter(|&(i, c)| i == 0 || c == '\n').map(|(i, _)| i).collect();
        let get_line_number = |idx: usize| {
            match line_numbers.binary_search(&idx) {
                Ok(idx) => idx + 1,
                Err(idx) => idx
            }
        };

        let line_start = source[..=self.bounds.start]
            .rfind(|x| x == '\n')
            .map(|i| i + 1) // don't include the newline
            .unwrap_or(0);

        let line_end = source[self.bounds.start..]
            .find(|x| x == '\n')
            .map(|i| i + self.bounds.start)
            .unwrap_or(source.len());

        let cutoff_end = self.bounds.end.min(line_end);

        println!("     |");
        println!("{: >4} | {}", get_line_number(line_start), &source[line_start..line_end]);
        println!("     | {}{}", " ".repeat(self.bounds.start - line_start), "^".repeat(cutoff_end - self.bounds.start));
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

pub trait ToCompilerError where Self: Sized {
    type Out;
    fn at(self, bounds: Bounds) -> Self::Out;

    fn at_item(self, item: &token::Item) -> Self::Out {
        self.at(item.bounds)
    }

    fn at_ast(self, ast: &ast::Ast) -> Self::Out {
        self.at(ast.bounds)
    }
}

impl ToCompilerError for AstError {
    type Out = CompilerError;
    fn at(self, bounds: Bounds) -> Self::Out {
        CompilerError {
            error: ErrorType::Ast(self),
            bounds
        }
    }
}

impl ToCompilerError for AnalysisError {
    type Out = CompilerError;
    fn at(self, bounds: Bounds) -> Self::Out {
        CompilerError {
            error: ErrorType::Analysis(self),
            bounds
        }
    }
}

impl ToCompilerError for TypeError {
    type Out = CompilerError;
    fn at(self, bounds: Bounds) -> Self::Out {
        CompilerError {
            error: ErrorType::Type(self),
            bounds
        }
    }
}

impl<T, E: ToCompilerError<Out = CompilerError>> ToCompilerError for std::result::Result<T, E> {
    type Out = Result<T>;
    fn at(self, bounds: Bounds) -> Self::Out {
        self.map_err(|e| e.at(bounds))
    }
}
