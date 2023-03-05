use thiserror::Error;

use crate::{typed::Type, token::{Token, Lexer, Operation, Dir, self, Item}, error::{Result, ToCompilerError}, utility::Bounds};

pub mod parse;
mod format;

#[derive(Debug, Clone)]
pub struct Ast {
    pub node: Node,
    pub bounds: Bounds
}

#[derive(Debug, Clone)]
pub enum Node {
    FuncDef {
        name: String,
        parameter_list: Vec<Ast>,
        return_type: Type,
        body: Box<Ast>,
    },
    Call {
        name: String,
        argument_list: Vec<Ast>,
        return_type: Type
    },
    UnaryExpr {
        op: UnaryOperation,
        ty: Type,
        value: Box<Ast>
    },
    Expr {
        op: BinaryOperation,
        ty: Type,
        lhs: Box<Ast>,
        rhs: Box<Ast>
    },
    If {
        condition: Box<Ast>,
        body: Box<Ast>,
        else_body: Option<Box<Ast>>,
        ty: Type
    },
    Loop {
        body: Box<Ast>
    },
    Break,
    Return(Box<Ast>),
    Intrisic(Intrisic),
    Statement(Box<Ast>),
    /// Converts it's expression to another type
    Convert(Box<Ast>, Type), 
    Empty,
    Block(Vec<Ast>, Type),
    Number(i64, Type),
    StringLiteral(String),
    BoolLiteral(bool),
    Identifier(String, Type),
    Definition {
        typename: Type,
        name: String,
    },
}

#[derive(Debug, Clone)]
pub enum Intrisic {
    Asm(Box<Ast>),
    Print(Vec<Ast>)
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperation {
    Negation,
    Deref,
    AddressOf,
    LogicalNot
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperation {
    Assignment,

    Equals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Lesser,
    LesserOrEquals,

    LogicalAnd,
    LogicalOr,
    LogicalXor,

    Add,
    Sub,
    Mul,
    Div,
    Modulus
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

impl BinaryOperation {
    pub fn is_comparison(&self) -> bool {
        use BinaryOperation::*;
        matches!(self, Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals)
    }

    pub fn is_arithmetic(&self) -> bool {
        use BinaryOperation::*;
        matches!(self, Add | Sub | Mul | Div | Modulus)
    }

    pub fn is_logic(&self) -> bool {
        use BinaryOperation::*;
        matches!(self, LogicalAnd | LogicalOr | LogicalXor)
    }
}

impl Node {
    fn ast(self, bounds: Bounds) -> Ast {
        Ast {
            node: self,
            bounds
        }
    }

    fn ast_from(self, value: Item) -> Ast {
        self.ast(value.bounds)
    }
}

impl Ast {
    fn new(bounds: Bounds, node: Node) -> Self {
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
