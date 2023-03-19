use thiserror::Error;

use crate::{typed::Type, token::{Token, Lexer, Operation, Dir, self, Item}, error::{Result, ToCompilerError}, utility::Bounds};

pub mod node;
pub mod analysis;
pub mod parse;
mod format;

pub use node::*;

#[derive(Debug, Clone)]
pub struct Ast<T = Node> {
    pub node: T,
    pub bounds: Bounds
}

#[derive(Debug, Error)]
pub enum AstError {
    #[error("operation {0:?} cannot be used {1}")]
    WrongOperation(Operation, &'static str),
    #[error("expected {0}, got {1:?}")]
    Expected(&'static str, Token),
    #[error("expected {0:?}, got {1:?}")]
    ExpectedToken(Token, Token),
    #[error("unexpected token {1:?}: {0}")]
    UnexpectedToken(&'static str, Token),
    #[error("expected {0}, got {1:?}")]
    ExpectedNode(&'static str, Node),
    #[error("unknown intrisic \"{0}\"")]
    UnknownIntrisic(String)
}

impl<T> Ast<T> {
    fn new(bounds: Bounds, node: T) -> Self {
        Ast {
            node,
            bounds
        }
    }

    pub fn extend(self, bounds: Bounds) -> Self {
        self.bounds.extend(bounds);
        self
    }
}
