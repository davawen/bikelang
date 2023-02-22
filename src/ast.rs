use crate::{typed::Type, token::{Token, Lexer, Operation, Dir, self}};

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

impl Token {
    fn left_binding_power(&self) -> u32 {
        use Token::*;
        use Operation::*;
        match self {
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

    fn nud(self, lexer: &mut Lexer) -> Node {
        use Token::*;
        match self {
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
                    value: box expression(lexer, power)
                }
            }
            Keyword(keyword) => {
                use token::Keyword::*;
                match keyword {
                    Func => {
                        let Token::Word(name) = lexer.next() else { panic!("No function name") };
                        lexer.expect(Token::Paren(Dir::Left));
                        let parameter_list = parameter_list(lexer);
                        let return_type = if lexer.peek() == &Token::Arrow {
                            lexer.next();
                            Type::from_node(expression(lexer, 1)).unwrap()
                        } else {
                            Type::Void
                        };

                        Node::FuncDef {
                            name,
                            parameter_list,
                            body: box expression(lexer, 0),
                            return_type
                        }
                    }
                    If => {
                        let condition = expression(lexer, 1);
                        Node::If {
                            condition: box condition,
                            body: box expression(lexer, 0)
                        }
                    }
                    Loop => {
                        Node::Loop {
                            body: box expression(lexer, 0)
                        }
                    }
                    Break => Node::Break,
                    Return => Node::Return(box expression(lexer, 0))
                }
            }
            Paren(Dir::Left) => { // parenthesis for grouping operations
                let ex = expression(lexer, 0);
                lexer.expect(Paren(Dir::Right));
                ex
            }
            Brace(Dir::Left) => {
                Node::Block(parse_block(lexer), Type::Void)
            }
            Semicolon => Node::Empty,
            _ => unreachable!("{self:#?}")
        }
    }

    fn led(self, lexer: &mut Lexer, left: Node) -> Node {
        use Token::*;
        match self {
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
                    Exclamation | AddressOf => unreachable!()
                };

                Node::Expr {
                    op,
                    ty: Type::Void,
                    lhs: box left,
                    rhs: box expression(lexer, power)
                }
            }
            Word(name) => { // got (type) variable
                Node::Definition {
                    typename: Type::from_node(left).unwrap(),
                    name
                }
            }
            Paren(Dir::Left) => { // parenthesis operator = function call
                let Node::Identifier(name, _) = left else { unreachable!("{left:#?}") };

                Node::Call {
                    name,
                    return_type: Type::Void,
                    parameter_list: parameter_list(lexer)
                }
            }
            Hash => {
                let Node::Identifier(name, _) = left else { unreachable!("{left:#?}") };

                lexer.expect(Token::Paren(Dir::Left));
                let list = parameter_list(lexer);
                let intrisic = match name.as_str() {
                    "asm" => Intrisic::Asm(box list.into_iter().next().unwrap()),
                    "print" => Intrisic::Print(list),
                    _ => unreachable!("Wrong intrisic {name}")
                };
                Node::Intrisic(intrisic)
            }
            _ => unreachable!("{self:#?}")
        }
    }
}

fn parameter_list(lexer: &mut Lexer) -> Vec<Node> {
    let mut list = Vec::new();
    if lexer.peek() != &Token::Paren(Dir::Right) {
        loop {
            list.push(expression(lexer, 0));
            if lexer.peek() != &Token::Comma {
                break;
            }
            lexer.expect(Token::Comma);
        }
    }
    lexer.expect(Token::Paren(Dir::Right));
    list
}

pub fn parse_block(lexer: &mut Lexer) -> Vec<Node> {
    let mut block = Vec::new();
    while lexer.peek() != &Token::Brace(Dir::Right) {
        let expr = expression(lexer, 0);
        let next = lexer.peek();
        if next == &Token::Semicolon {
            block.push(Node::Statement(box expr));
            lexer.next();
        }
        else {
            block.push(expr);
        }
    }
    lexer.expect(Token::Brace(Dir::Right));

    block
}

fn expression(lexer: &mut Lexer, rbp: u32) -> Node {
    let mut t = lexer.next();
    let mut left = t.nud(lexer);

    while lexer.peek().left_binding_power() > rbp {
        t = lexer.next();

        left = t.led(lexer, left);
    }

    left
}

pub fn parse_ast(lexer: &mut Lexer) -> Node {
    let mut root = Vec::new();
    while lexer.peek() != &Token::Eof {
        root.push(expression(lexer, 0));
    }
    Node::Block(root, Type::Void)
}
