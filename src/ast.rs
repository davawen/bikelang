use itertools::Itertools;
use thiserror::Error;

use crate::{token::{self, Dir, Token, Keyword}, typed::{Type, TypeError}};

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
    },
    UnaryExpr {
        op: UnaryOperation,
        value: Box<Node>
    },
    Expr {
        op: BinaryOperation,
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
    Block(Vec<Node>),
    Number(i32),
    StringLiteral(String),
    Identifier(String),
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
pub enum ASTError {
    #[error("Unexpected token {0:?}")]
    UnexpectedToken(Token),
    #[error("Expected token {0:?}")]
    ExpectedToken(Token),
    #[error("Malformed intrisic: {0}")]
    MalformedIntrisic(&'static str),
    #[error("Intrisic {0} isn't defined in the language")]
    UknownInstric(String),
    #[error("Malformed expression")]
    InvalidExpression,
    #[error("{0}")]
    Type(TypeError)
}

impl From<TypeError> for ASTError {
    fn from(value: TypeError) -> Self {
        ASTError::Type(value)
    }
}

type Result<T> = std::result::Result<T, ASTError>;

impl UnaryOperation {
    fn from_op(op: token::Operation) -> Result<Self> {
        use token::Operation::*;
        match op {
            LogicalNot => Ok(UnaryOperation::LogicalNot),
            _ => Err(ASTError::InvalidExpression)
        }
    }
}

impl BinaryOperation {
    fn from_op(op: token::Operation) -> Result<Self> {
        macro_rules! map {
            ($($x:ident),+) => {
                match op {
                    $(token::Operation::$x => Ok(BinaryOperation::$x),)+
                    _ => Err(ASTError::InvalidExpression)
                }
            };
        }

        map!(
            Assignment,
            Equals, NotEquals, Greater, GreaterOrEquals, Lesser, LesserOrEquals,
            LogicalAnd, LogicalOr, LogicalXor,
            Add, Sub, Mul, Div, Modulus
        )
    }

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

impl Intrisic {
    fn from_parameters(name: &str, parameters: Vec<Node>) -> Result<Self> {
        match name {
            "asm" => {
                if let Some(s) = parameters.into_iter().next() {
                    Ok(Self::Asm(Box::new(s)))
                }
                else {
                    Err(ASTError::MalformedIntrisic("no argument given to asm intrisic"))
                }
            }
            "print" => {
                if !parameters.is_empty() {
                    Ok(Self::Print(parameters))
                }
                else {
                    Err(ASTError::MalformedIntrisic("no argument given to print intrisic"))
                }
            }
            name => Err(ASTError::UknownInstric(name.to_owned()))
        }
    }
}

fn parse_lhs(tokens: &[Token]) -> Result<Node> {
    match tokens {
        [typename @ .., Token::Word(name)] => {
            if typename.is_empty() {
                parse_expr(tokens)
            }
            else {
                Ok(Node::Definition {
                    typename: Type::from_tokens(typename)?,
                    name: name.clone()
                })
            }
        }
        _ => parse_expr(tokens)
    }
}

fn parse_expr(tokens: &[Token]) -> Result<Node> {
    fn parse_following_or_return_inner(
        tokens: &[Token],
        end_inner: usize,
        inner_expression: Node,
    ) -> Result<Node> {
        if let Some(Token::Op(op)) = tokens.get(end_inner + 1) {
            let rhs = &tokens[(end_inner + 2)..];

            Ok(Node::Expr {
                lhs: Box::new(inner_expression),
                rhs: Box::new(parse_expr(rhs)?),
                op: BinaryOperation::from_op(*op)?,
            })
        }
        // Else just return the content of the parenthesis
        else {
            Ok(inner_expression)
        }
    }

    match tokens {
        [Token::Number(num)] => Ok(Node::Number(*num)),
        [Token::StringLiteral(s)] => Ok(Node::StringLiteral(s.clone())),
        [Token::Word(name)] => Ok(Node::Identifier(name.clone())),
        [Token::Word(name), h @ Token::Paren(Dir::Left), following @ ..] | 
        [Token::Word(name), h @ Token::Hash, Token::Paren(Dir::Left), following @ ..] => {
            let matching = find_matching(following, Token::Paren(Dir::Left), false).ok_or(ASTError::ExpectedToken(Token::Paren(Dir::Right)))?;

            let parameter_list = &following[..matching];

            let arg_list = parse_parameter_list(parameter_list)?;

            let inner_expression = if matches!(h, Token::Hash) {
                    Node::Intrisic(
                        Intrisic::from_parameters(name, arg_list)?
                    )
                }
                else {
                    Node::Call {
                        name: name.clone(),
                        parameter_list: arg_list,
                    }
                };

            parse_following_or_return_inner(following, matching, inner_expression)
        }
        [Token::Keyword(Keyword::If), following @ ..] => {
            let (open_body, _) = following.iter().enumerate().find(|(_, token)| matches!(token, Token::Brace(Dir::Left)))
                .ok_or(ASTError::ExpectedToken(Token::Brace(Dir::Left)))?;

            Ok(Node::If {
                condition: Box::new(parse_expr(&following[..open_body])?),
                body: Box::new(parse_expr(&following[open_body..])?)
            })
        }
        [Token::Keyword(Keyword::Loop), following @ ..] => {
            Ok(Node::Loop { body: Box::new(parse_expr(following)?) })
        }
        [Token::Keyword(Keyword::Break)] => Ok(Node::Break),
        [Token::Keyword(Keyword::Return), return_expr @ ..] => {
            Ok(Node::Return( Box::new(parse_expr(return_expr)?) ))
        }
        [Token::Paren(Dir::Left), following @ ..] => {
            let matching = find_matching(following, Token::Paren(Dir::Left), false).ok_or(ASTError::ExpectedToken(Token::Paren(Dir::Right)))?;

            let inner_expression = parse_expr(&following[..matching])?;
            parse_following_or_return_inner(following, matching, inner_expression)
        }
        [Token::Brace(Dir::Left), following @ ..] => {
            let matching = find_matching(following, Token::Brace(Dir::Left), false).ok_or(ASTError::ExpectedToken(Token::Paren(Dir::Right)))?;

            let block = parse_block(&following[..matching])?;
            parse_following_or_return_inner(following, matching, block)
        }
        [Token::Op(op), _value] => Ok(Node::UnaryExpr { 
            op: UnaryOperation::from_op(*op)?, 
            value: Box::new(parse_expr(&tokens[1..])?)
        }),
        [_lhs, Token::Op(op), _rhs] => Ok(Node::Expr {
            lhs: Box::new(parse_expr(&tokens[..1])?),
            rhs: Box::new(parse_expr(&tokens[2..])?),
            op: BinaryOperation::from_op(*op)?,
        }),
        tokens => {
            let mut scope = 0;

            // Get the operation with the highest precedence not in any parenthesises
            let op = tokens
                .iter()
                .enumerate()
                .flat_map(|(idx, token)| {
                    match token {
                        Token::Paren(Dir::Left) => scope += 1,
                        Token::Paren(Dir::Right) => scope -= 1,
                        Token::Op(op) if scope == 0 => return Some((idx, op)),
                        _ => ()
                    };

                    None
                }
                )
                .max_by(|a, b| a.1.precedence().cmp(&b.1.precedence()));

            if let Some((idx, op)) = op {
                // println!("Out of {tokens:?},\nthe biggest operation is {op:?}");
                let lhs = if let token::Operation::Assignment = op {
                    parse_lhs(&tokens[..idx])?
                } else {
                    parse_expr(&tokens[..idx])?
                };

                Ok(Node::Expr {
                    lhs: Box::new(lhs),
                    rhs: Box::new(parse_expr(&tokens[(idx + 1)..])?),
                    op: BinaryOperation::from_op(*op)?,
                })
            } else {
                Err(ASTError::InvalidExpression)
            }
        }
    }
}

/// Find the matching token to the one given
///
/// * `tokens`: The input tokens
/// * `token`: The token to match
/// * `included`: Wether the starting token is included in the input range
fn find_matching(tokens: &[Token], token: Token, included: bool) -> Option<usize> {
    let mut scope = i32::from(!included);
    for (idx, tok) in tokens.iter().enumerate() {
        scope += match (&token, tok) {
            (Token::Paren(_), Token::Paren(dir)) | (Token::Brace(_), Token::Brace(dir)) => {
                match dir {
                    Dir::Left => 1,
                    Dir::Right => -1,
                }
            }
            _ => 0,
        };

        if scope == 0 {
            return Some(idx);
        }
    }
    None
}

/// Parses every comma separated expression in `inner_tokens`.
///
/// * `inner_tokens`: Given tokens
fn parse_parameter_list(inner_tokens: &[Token]) -> Result<Vec<Node>> {
    let mut scope = 0;

    inner_tokens
        .split(|t| {
            match t {
                Token::Paren(Dir::Left) => scope += 1,
                Token::Paren(Dir::Right) => scope -= 1,
                _ => ()
            };

            scope <= 0 && matches!(t, Token::Comma)
        })
        .filter(|t| !t.is_empty())
        .map(parse_lhs)
        .collect()
}

fn parse_block(tokens: &[Token]) -> Result<Node> {
    let mut out = Vec::new();

    // println!("aaa");
    let mut start_idx = 0;
    let mut it = tokens.iter().enumerate();
    while let Some((idx, token)) = it.next() {
        if let Token::Semicolon = token {
            let line = &tokens[start_idx..idx];
            if !line.is_empty() {
                out.push(Node::Statement(Box::new(parse_expr(line)?)));
            }
            start_idx = idx + 1;
        }
        else if let Token::Brace(Dir::Left) = token { // skip underlyinh scopes
            if let Some(end_idx) = find_matching(&tokens[idx..], Token::Brace(Dir::Left), true) {
                // NOTE: end_idx is relative to tokens[idx..]
                it.by_ref().nth(end_idx - 1);
            } else {
                return Err(ASTError::ExpectedToken(Token::Brace(Dir::Right)));
            }
        }
    }

    // Get last expression
    if start_idx < tokens.len() {
        let line = &tokens[start_idx..];
        if !line.is_empty() {
            out.push(parse_expr(line)?);
        }
    }

    Ok(Node::Block(out))
}

pub fn parse_func_def(mut tokens: &[Token]) -> Result<Option<(Node, &[Token])>> {
    if let (Token::Keyword(Keyword::Func), Token::Word(name)) = (&tokens[0], &tokens[1]) {
        tokens = &tokens[2..];

        // println!("{tokens:#?}");

        let Token::Paren(Dir::Left) = tokens[0] else { panic!("No parameter list after function definition") };

        let matching =
            find_matching(tokens, Token::Paren(Dir::Left), true).expect("End to parameter list");

        let parameter_list = parse_parameter_list(&tokens[1..matching])?;

        tokens = &tokens[(matching + 1)..];

        let (open_brace, _) = tokens.iter().find_position(|x| matches!(x, Token::Brace(Dir::Left)))
            .ok_or(ASTError::ExpectedToken(Token::Brace(Dir::Left)))?;

        let return_type = if let Token::Arrow = &tokens[0] {
            Type::from_tokens(&tokens[1..open_brace])?
        } else {
            Type::Void
        };

        tokens = &tokens[(open_brace + 1)..];

        let body = if let Some(end_idx) = find_matching(tokens, Token::Brace(Dir::Left), false) {
            let body = parse_block(&tokens[..end_idx])?;
            tokens = &tokens[(end_idx + 1)..];
            body
        } else {
            // panic!("No brace after function definition")
            return Err(ASTError::ExpectedToken(Token::Brace(Dir::Right)));
        };

        Ok(Some((
            Node::FuncDef {
                name: name.clone(),
                parameter_list,
                return_type,
                body: Box::new(body),
            },
            tokens,
        )))
    } else {
        Ok(None)
    }
}

pub fn parse_ast(mut tokens: &[Token]) -> Result<Node> {
    let mut root = Vec::new();
    while !matches!(tokens[0], Token::Eof) && !tokens.is_empty() {
        if let Some((func, out_tokens)) = parse_func_def(tokens)? {
            root.push(func);
            tokens = out_tokens;
        } else {
            eprintln!("{tokens:#?}");
            panic!("Couldn't find a valid statement");
        }
    }

    Ok(Node::Block(root))
}
