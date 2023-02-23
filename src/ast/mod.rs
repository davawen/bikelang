use thiserror::Error;

use crate::{typed::Type, token::{Token, Lexer, Operation, Dir, self, Item}, error::{Result, ToCompilerError}, utility::Bounds};

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
        parameter_list: Vec<Ast>,
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
        body: Box<Ast>
    },
    Loop {
        body: Box<Ast>
    },
    Break,
    Return(Box<Ast>),
    Intrisic(Intrisic),
    Statement(Box<Ast>),
    Empty,
    Block(Vec<Ast>, Type),
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

impl Item {
    fn left_binding_power(&self) -> Result<u32> {
        use Token::*;
        use Operation::*;
        let out = match self.token {
            Op(op) => match op {
                Assignment => 10,
                LogicalAnd | LogicalOr | LogicalXor => 20,
                Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => 30,
                Plus | Minus => 40,
                Times | Div | Modulus => 50,
                Exclamation | AddressOf => return Err(AstError::WrongOperation(op, "as an infix operator")).at_item(self) 
            }
            Paren(Dir::Left) => 70, // operator ()
            Hash => 70,
            Word(_) => 5, 
            Brace(Dir::Left) => 1, // allow stopping at opening brace
            Paren(Dir::Right) | Brace(Dir::Right) | Keyword(_) | Comma | Semicolon | Eof => 0,
            _ => unreachable!("{self:#?}")
        };
        Ok(out)
    }

    fn nud(self, lexer: &mut Lexer) -> Result<Ast> {
        use Token::*;
        let node = match self.token {
            Number(n) => Node::Number(n, Type::Void).ast(self.bounds),
            StringLiteral(s) => Node::StringLiteral(s).ast(self.bounds),
            Word(name) => Node::Identifier(name, Type::Void).ast(self.bounds),
            Op(op) => {
                let (op, power) = match op {
                    Operation::Minus => (UnaryOperation::Negation, 60),
                    Operation::Times => (UnaryOperation::Deref, 60),
                    Operation::AddressOf => (UnaryOperation::AddressOf, 60),
                    Operation::Exclamation => (UnaryOperation::LogicalNot, 60),
                    _ => return Err(AstError::WrongOperation(op, "as a prefix operator")).at_item(&self)
                };

                let value = box expression(lexer, power)?;
                Ast::new(
                    self.bounds.with_end_of(value.bounds),
                    Node::UnaryExpr {
                        op,
                        ty: Type::Void,
                        value
                    }
                )
            }
            Keyword(keyword) => {
                use token::Keyword::*;
                match keyword {
                    Func => {
                        let name = lexer.next();
                        let Token::Word(name) = name.token
                            else { return Err(AstError::Expected("a function name", name.token)).at(name.bounds) };

                        lexer.expect(Token::Paren(Dir::Left))?;
                        let (parameter_list, _) = parameter_list(lexer)?;
                        let return_type = if lexer.peek().token == Token::Arrow {
                            lexer.next();
                            Type::from_node(expression(lexer, 1)?.node).unwrap()
                        } else {
                            Type::Void
                        };

                        let body = box expression(lexer, 0)?;
                        Ast::new(
                            self.bounds.with_end_of(body.bounds),
                            Node::FuncDef {
                                name,
                                parameter_list,
                                body,
                                return_type
                            }
                        )
                    }
                    If => {
                        let condition = box expression(lexer, 1)?;
                        let body = box expression(lexer, 0)?;
                        Ast::new(
                            self.bounds.with_end_of(body.bounds),
                            Node::If {
                                condition,
                                body
                            }
                        )
                    }
                    Loop => {
                        let body = box expression(lexer, 0)?;
                        Ast::new(
                            self.bounds.with_end_of(body.bounds),
                            Node::Loop {
                                body
                            }
                        )
                    }
                    Break => Node::Break.ast(self.bounds),
                    Return => {
                        let expr = box expression(lexer, 0)?;
                        Ast::new(self.bounds.with_end_of(expr.bounds), Node::Return(expr))
                    }
                }
            }
            Paren(Dir::Left) => { // parenthesis for grouping operations
                let inner = expression(lexer, 0)?;
                let rparen = lexer.expect(Paren(Dir::Right))?;

                inner.extend(self.bounds.with_end_of(rparen.bounds))
            }
            Brace(Dir::Left) => {
                let (inner, rtoken) = parse_block(lexer)?;
                Node::Block(inner, Type::Void).ast(self.bounds.with_end_of(rtoken.bounds))
            }
            Semicolon => Node::Empty.ast_from(self),
            _ => unreachable!("{self:#?}")
        };

        Ok(node)
    }

    fn led(self, lexer: &mut Lexer, left: Ast) -> Result<Ast> {
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
                    Exclamation | AddressOf => unreachable!() // error emitted in left_binding_power()
                };

                let rhs = box expression(lexer, power)?;
                Ast::new(
                    left.bounds.with_end_of(rhs.bounds),
                    Node::Expr {
                        op,
                        ty: Type::Void,
                        lhs: box left,
                        rhs
                    }
                )
            }
            Word(name) => { // got (type) variable
                Node::Definition {
                    typename: Type::from_node(left.node).unwrap(),
                    name
                }.ast(left.bounds.with_end_of(self.bounds))
            }
            Paren(Dir::Left) => { // parenthesis operator = function call
                let Node::Identifier(name, _) = left.node
                    else { return Err(AstError::ExpectedNode("a function name", left.node)).at(left.bounds)};

                let (parameter_list, rtoken) = parameter_list(lexer)?;
                Node::Call {
                    name,
                    return_type: Type::Void,
                    parameter_list
                }.ast(left.bounds.with_end_of(rtoken.bounds))
            }
            Hash => {
                let Node::Identifier(name, _) = left.node
                    else { return Err(AstError::ExpectedNode("an intrisic name", left.node)).at(left.bounds) };

                lexer.expect(Token::Paren(Dir::Left))?;
                let (list, rtoken) = parameter_list(lexer)?;

                let intrisic = match name.as_str() {
                    "asm" => Intrisic::Asm(box list.into_iter().next().unwrap()),
                    "print" => Intrisic::Print(list),
                    _ => return Err(AstError::UnknownIntrisic(name)).at(left.bounds)
                };
                Node::Intrisic(intrisic).ast(left.bounds.with_end_of(rtoken.bounds))
            }
            _ => unreachable!("{self:#?}")
        };

        Ok(node)
    }
}

fn parameter_list(lexer: &mut Lexer) -> Result<(Vec<Ast>, Item)> {
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
    
    Ok((list, lexer.expect(Token::Paren(Dir::Right))?))
}

pub fn parse_block(lexer: &mut Lexer) -> Result<(Vec<Ast>, Item)> {
    let mut block = Vec::new();
    while lexer.peek().token != Token::Brace(Dir::Right) {
        let expr = expression(lexer, 0)?;
        let next = lexer.peek();

        if next.token == Token::Semicolon {
            block.push(Ast::new(expr.bounds.with_end_of(next.bounds), Node::Statement(box expr)));
            lexer.next();
        }
        else {
            block.push(expr);
        }
    }
    
    Ok((block, lexer.expect(Token::Brace(Dir::Right))?))
}

fn expression(lexer: &mut Lexer, rbp: u32) -> Result<Ast> {
    let mut t = lexer.next();
    let mut left = t.nud(lexer)?;

    while lexer.peek().left_binding_power()? > rbp {
        t = lexer.next();

        left = t.led(lexer, left)?;
    }

    Ok(left)
}

pub fn parse_ast(lexer: &mut Lexer) -> Result<Ast> {
    let mut root = Vec::new();
    while lexer.peek().token != Token::Eof {
        root.push(expression(lexer, 0)?);
    }

    let bounds = Bounds {
        start: root.first().map(|x| x.bounds.start).unwrap_or(0),
        end: root.last().map(|x| x.bounds.end).unwrap_or(0) 
    };
    Ok(Node::Block(root, Type::Void).ast(bounds))
}