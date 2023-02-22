use std::fmt::Display;

use crate::{ast::AstError, typed::TypeError, analysis::AnalysisError, token};

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
        match &self.error {
            ErrorType::Ast(e) => println!("error: {e}"),
            ErrorType::Analysis(e) => println!("error: {e}"),
            ErrorType::Type(e) => println!("error: {e}")
        }

        let line_start = source[..=self.start]
            .rfind(|x| x == '\n')
            .map(|i| i + 1) // don't include the newline
            .unwrap_or(0);

        let line_end = source[self.end..]
            .find(|x| x == '\n')
            .map(|i| i + self.end)
            .unwrap_or(source.len());

        print!("\n\x1b[1F");
        // Print until "selection", print indication markers (^^^), go back to where you where and print until the end of the line
        // Necessary to handle multi-width characters like tabs
        println!("at: {}\x1b[1B{}\x1b[1A\x1b[{}D{}\n",
            &source[line_start..self.start],
            "^".repeat(self.end-self.start),
            self.end-self.start,
            &source[self.start..line_end]
        );
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

pub trait ToCompilerError where Self: Sized {
    type Out;
    fn hydrate(self, start: usize, end: usize) -> Self::Out;

    fn at_item(self, item: &token::Item) -> Self::Out {
        self.hydrate(item.start, item.end)
    }
}

impl ToCompilerError for AstError {
    type Out = CompilerError;
    fn hydrate(self, start: usize, end: usize) -> Self::Out {
        CompilerError {
            error: ErrorType::Ast(self),
            start,
            end,
        }
    }
}

impl ToCompilerError for AnalysisError {
    type Out = CompilerError;
    fn hydrate(self, start: usize, end: usize) -> Self::Out {
        CompilerError {
            error: ErrorType::Analysis(self),
            start,
            end,
        }
    }
}

impl ToCompilerError for TypeError {
    type Out = CompilerError;
    fn hydrate(self, start: usize, end: usize) -> Self::Out {
        CompilerError {
            error: ErrorType::Type(self),
            start,
            end,
        }
    }
}

impl<T, E: ToCompilerError<Out = CompilerError>> ToCompilerError for std::result::Result<T, E> {
    type Out = Result<T>;
    fn hydrate(self, start: usize, end: usize) -> Self::Out {
        self.map_err(|e| e.hydrate(start, end))
    }
}
