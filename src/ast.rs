use thiserror::Error;

use crate::{typed::Type, token::{Token, Lexer, Operation, Dir, self, Item}, error::{Result, ToCompilerError}};

#[derive(Debug, Clone)]
pub enum Node {
    FuncDef {
        name: String,
        parameter_list: Vec<Node>,
        return_type: Type,
        body: Box<Node>,
    },
    Call {
        name: String,
        parameter_list: Vec<Node>,
        return_type: Type
    },
    UnaryExpr {
        op: UnaryOperation,
        ty: Type,
        value: Box<Node>
    },
    Expr {
        op: BinaryOperation,
        ty: Type,
        lhs: Box<Node>,
        rhs: Box<Node>
    },
    If {
        condition: Box<Node>,
        body: Box<Node>
    },
    Loop {
        body: Box<Node>
    },
    Break,
    Return(Box<Node>),
    Intrisic(Intrisic),
    Statement(Box<Node>),
    Empty,
    Block(Vec<Node>, Type),
    Number(i64, Type),
    StringLiteral(String),
    Identifier(String, Type),
    Definition {
        typename: Type,
        name: String,
    },
}

#[derive(Debug, Clone)]
pub enum Intrisic {
    Asm(Box<Node>),
    Print(Vec<Node>)
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
    #[error("Operation {0:?} cannot be used {1}")]
    WrongOperation(Operation, &'static str),
    #[error("Expected {0}, got {1:?}")]
    Expected(&'static str, Token),
    #[error("Expected {0:?}, got {1:?}")]
    ExpectedToken(Token, Token),
    #[error("Expected {0}, got {1:?}")]
    ExpectedNode(&'static str, Node),
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

impl Item {
    fn left_binding_power(&self) -> u32 {
        use Token::*;
        use Operation::*;
        match self.token {
            Op(op) => match op {
                Assignment => 10,
                LogicalAnd | LogicalOr | LogicalXor => 20,
                Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => 30,
                Plus | Minus => 40,
                Times | Div | Modulus => 50,
                Exclamation | AddressOf => unreachable!()
            }
            Paren(Dir::Left) => 70, // operator ()
            Hash => 70,
            Word(_) => 5, 
            Brace(Dir::Left) => 1, // allow stopping at opening brace
            Paren(Dir::Right) | Brace(Dir::Right) | Keyword(_) | Comma | Semicolon | Eof => 0,
            _ => unreachable!("{self:#?}")
        }
    }

    fn nud(self, lexer: &mut Lexer) -> Result<Node> {
        use Token::*;
        let node = match self.token {
            Number(n) => Node::Number(n, Type::Void),
            StringLiteral(s) => Node::StringLiteral(s),
            Word(name) => Node::Identifier(name, Type::Void),
            Op(op) => {
                let (op, power) = match op {
                    Operation::Minus => (UnaryOperation::Negation, 60),
                    Operation::Times => (UnaryOperation::Deref, 60),
                    Operation::AddressOf => (UnaryOperation::AddressOf, 60),
                    Operation::Exclamation => (UnaryOperation::LogicalNot, 60),
                    _ => unreachable!()
                };
                Node::UnaryExpr {
                    op,
                    ty: Type::Void,
                    value: box expression(lexer, power)?
                }
            }
            Keyword(keyword) => {
                use token::Keyword::*;
                match keyword {
                    Func => {
                        let name = lexer.next();
                        let Token::Word(name) = name.token
                            else { return Err(AstError::Expected("a function name", name.token)).hydrate(name.start, name.end) };

                        lexer.expect(Token::Paren(Dir::Left))?;
                        let parameter_list = parameter_list(lexer)?;
                        let return_type = if lexer.peek().token == Token::Arrow {
                            lexer.next();
                            Type::from_node(expression(lexer, 1)?).unwrap()
                        } else {
                            Type::Void
                        };

                        Node::FuncDef {
                            name,
                            parameter_list,
                            body: box expression(lexer, 0)?,
                            return_type
                        }
                    }
                    If => {
                        let condition = expression(lexer, 1)?;
                        Node::If {
                            condition: box condition,
                            body: box expression(lexer, 0)?
                        }
                    }
                    Loop => {
                        Node::Loop {
                            body: box expression(lexer, 0)?
                        }
                    }
                    Break => Node::Break,
                    Return => Node::Return(box expression(lexer, 0)?)
                }
            }
            Paren(Dir::Left) => { // parenthesis for grouping operations
                let ex = expression(lexer, 0)?;
                lexer.expect(Paren(Dir::Right))?;
                ex
            }
            Brace(Dir::Left) => {
                Node::Block(parse_block(lexer)?, Type::Void)
            }
            Semicolon => Node::Empty,
            _ => unreachable!("{self:#?}")
        };

        Ok(node)
    }

    fn led(self, lexer: &mut Lexer, left: Node) -> Result<Node> {
        use Token::*;
        let node = match self.token {
            Op(op) => {
                use Operation::*;
                let (op, power) = match op {
                    Assignment      => (BinaryOperation::Assignment, 9),
                    LogicalAnd      => (BinaryOperation::LogicalAnd, 21),
                    LogicalOr       => (BinaryOperation::LogicalOr, 21),
                    LogicalXor      => (BinaryOperation::LogicalXor, 21),
                    Equals          => (BinaryOperation::Equals, 31),
                    NotEquals       => (BinaryOperation::NotEquals, 31),
                    Greater         => (BinaryOperation::Greater, 31),
                    GreaterOrEquals => (BinaryOperation::GreaterOrEquals, 31),
                    Lesser          => (BinaryOperation::Lesser, 31),
                    LesserOrEquals  => (BinaryOperation::LesserOrEquals, 31),
                    Plus            => (BinaryOperation::Add, 41),
                    Minus           => (BinaryOperation::Sub, 41),
                    Times           => (BinaryOperation::Mul, 51),
                    Div             => (BinaryOperation::Div, 51),
                    Modulus         => (BinaryOperation::Modulus, 51),
                    Exclamation | AddressOf => return Err(AstError::WrongOperation(op, "as an infix operator")).at_item(&self)
                };

                Node::Expr {
                    op,
                    ty: Type::Void,
                    lhs: box left,
                    rhs: box expression(lexer, power)?
                }
            }
            Word(name) => { // got (type) variable
                Node::Definition {
                    typename: Type::from_node(left).unwrap(),
                    name
                }
            }
            Paren(Dir::Left) => { // parenthesis operator = function call
                let Node::Identifier(name, _) = left 
                    else { return Err(AstError::ExpectedNode("a function name", left)).at_item(&self) };

                Node::Call {
                    name,
                    return_type: Type::Void,
                    parameter_list: parameter_list(lexer)?
                }
            }
            Hash => {
                let Node::Identifier(name, _) = left 
                    else { return Err(AstError::ExpectedNode("an intrisic name", left)).at_item(&self) };

                lexer.expect(Token::Paren(Dir::Left))?;
                let list = parameter_list(lexer)?;
                let intrisic = match name.as_str() {
                    "asm" => Intrisic::Asm(box list.into_iter().next().unwrap()),
                    "print" => Intrisic::Print(list),
                    _ => unreachable!("Wrong intrisic {name}")
                };
                Node::Intrisic(intrisic)
            }
            _ => unreachable!("{self:#?}")
        };

        Ok(node)
    }
}

fn parameter_list(lexer: &mut Lexer) -> Result<Vec<Node>> {
    let mut list = Vec::new();
    if lexer.peek().token != Token::Paren(Dir::Right) {
        loop {
            list.push(expression(lexer, 0)?);
            if lexer.peek().token != Token::Comma {
                break;
            }
            lexer.expect(Token::Comma)?;
        }
    }
    lexer.expect(Token::Paren(Dir::Right))?;
    Ok(list)
}

pub fn parse_block(lexer: &mut Lexer) -> Result<Vec<Node>> {
    let mut block = Vec::new();
    while lexer.peek().token != Token::Brace(Dir::Right) {
        let expr = expression(lexer, 0)?;
        let next = lexer.peek();
        if next.token == Token::Semicolon {
            block.push(Node::Statement(box expr));
            lexer.next();
        }
        else {
            block.push(expr);
        }
    }
    lexer.expect(Token::Brace(Dir::Right))?;

    Ok(block)
}

fn expression(lexer: &mut Lexer, rbp: u32) -> Result<Node> {
    let mut t = lexer.next();
    let mut left = t.nud(lexer)?;

    while lexer.peek().left_binding_power() > rbp {
        t = lexer.next();

        left = t.led(lexer, left)?;
    }

    Ok(left)
}

pub fn parse_ast(lexer: &mut Lexer) -> Result<Node> {
    let mut root = Vec::new();
    while lexer.peek().token != Token::Eof {
        root.push(expression(lexer, 0)?);
    }
    Ok(Node::Block(root, Type::Void))
}
