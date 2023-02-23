use std::fmt::Display;
use thiserror::Error;

use crate::{ast::{AstError, self}, typed::TypeError/* , analysis::AnalysisError */, token};

#[derive(Debug, Error)]
enum AnalysisError {

}

#[derive(Debug)]
pub struct CompilerError {
    error: ErrorType,
    start: usize,
    end: usize
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

        let line_start = source[..=self.start]
            .rfind(|x| x == '\n')
            .map(|i| i + 1) // don't include the newline
            .unwrap_or(0);

        let line_end = source[self.start..]
            .find(|x| x == '\n')
            .map(|i| i + self.start)
            .unwrap_or(source.len());

        let cutoff_end = self.end.min(line_end);

        println!("     |");
        print!("{: >4} | ", get_line_number(line_start));
        println!("{}", &source[line_start..line_end]);
        println!("     | {}{}", " ".repeat(self.start - line_start), "^".repeat(cutoff_end - self.start));
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

pub trait ToCompilerError where Self: Sized {
    type Out;
    fn at(self, start: usize, end: usize) -> Self::Out;

    fn at_item(self, item: &token::Item) -> Self::Out {
        self.at(item.start, item.end)
    }

    fn at_ast(self, ast: &ast::Ast) -> Self::Out {
        self.at(ast.start, ast.end)
    }
}

impl ToCompilerError for AstError {
    type Out = CompilerError;
    fn at(self, start: usize, end: usize) -> Self::Out {
        CompilerError {
            error: ErrorType::Ast(self),
            start,
            end,
        }
    }
}

impl ToCompilerError for AnalysisError {
    type Out = CompilerError;
    fn at(self, start: usize, end: usize) -> Self::Out {
        CompilerError {
            error: ErrorType::Analysis(self),
            start,
            end,
        }
    }
}

impl ToCompilerError for TypeError {
    type Out = CompilerError;
    fn at(self, start: usize, end: usize) -> Self::Out {
        CompilerError {
            error: ErrorType::Type(self),
            start,
            end,
        }
    }
}

impl<T, E: ToCompilerError<Out = CompilerError>> ToCompilerError for std::result::Result<T, E> {
    type Out = Result<T>;
    fn at(self, start: usize, end: usize) -> Self::Out {
        self.map_err(|e| e.at(start, end))
    }
}
