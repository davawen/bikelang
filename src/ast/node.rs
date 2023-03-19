use crate::{typed::Type, utility::Bounds, token::Item};

use super::Ast;

/// Invariant: ALL types in nodes are uninitialized (set to `Type::Void`) until `analysis` phase
#[derive(Debug, Clone)]
pub enum Node {
    FuncDef {
        name: String,
        parameter_list: Vec<Ast>,
        return_type: Ast<TypeNode>,
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
    Convert(Box<Ast>, Ast<TypeNode>, Type), 
    Empty,
    Block { 
        inner: Vec<Ast>,
        ty: Type
    },
    Number(i64, Type),
    StringLiteral(String),
    BoolLiteral(bool),
    Identifier(String, Type),
    Definition {
        typename: Ast<TypeNode>,
        ty: Type,
        name: String,
    },
}

#[derive(Debug, Clone)]
pub enum TypeNode {
    Typename(String),
    Ptr(Box<Ast<TypeNode>>)
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
    pub fn ast(self, bounds: Bounds) -> Ast {
        Ast {
            node: self,
            bounds
        }
    }

    pub fn ast_from(self, value: Item) -> Ast {
        self.ast(value.bounds)
    }
}
