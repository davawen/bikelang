use indexmap::IndexMap;
use itertools::Itertools;
use thiserror::Error;

use crate::token::{Dir, Operation, Token};

#[derive(Debug, Clone)]
pub enum Node {
    FuncDef {
        name: String,
        parameter_list: Vec<Node>,
        return_type: String,
        body: Box<Node>,
    },
    Definition {
        typename: String,
        name: String,
    },
    Expr {
        op: Operation,
        lhs: Box<Node>,
        rhs: Box<Node>
    },
    Statement(Box<Node>),
    Intrisic(Intrisic),
    Call {
        name: String,
        parameter_list: Vec<Node>,
    },
    Block(Vec<Node>),
    Number(i32),
    StringLiteral(String),
    Identifier(String),
}

#[derive(Debug, Clone)]
pub enum Intrisic {
    Asm(Box<Node>),
    Print(Vec<Node>)
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
    InvalidExpression
}

type Result<T> = std::result::Result<T, ASTError>;

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
            },
            "print" => {
                if parameters.len() > 0 {
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
                op: *op,
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
        [Token::Word(name)] => Ok(Node::Identifier(name.clone())),
        [Token::Word(typename), Token::Word(name)] => Ok(Node::Definition {
            typename: typename.clone(),
            name: name.clone(),
        }),
        [Token::Paren(Dir::Left), following @ ..] => {
            let matching = find_matching(following, Token::Paren(Dir::Left), false).ok_or(ASTError::ExpectedToken(Token::Paren(Dir::Right)))?;

            let inner_expression = parse_expr(&following[..matching])?;
            parse_following_or_return_inner(following, matching, inner_expression)
        },
        [Token::Brace(Dir::Left), following @ ..] => {
            let matching = find_matching(following, Token::Brace(Dir::Left), false).ok_or(ASTError::ExpectedToken(Token::Paren(Dir::Right)))?;

            let block = parse_block(&following[..matching])?;
            parse_following_or_return_inner(following, matching, block)
        },
        [_lhs, Token::Op(op), _rhs] => Ok(Node::Expr {
            lhs: Box::new(parse_expr(&tokens[..1])?),
            rhs: Box::new(parse_expr(&tokens[2..])?),
            op: *op,
        }),
        tokens => {
            let op = tokens
                .iter()
                .enumerate()
                .find(|(_, token)| matches!(token, Token::Op(_)));

            if let Some((idx, Token::Op(op))) = op {
                Ok(Node::Expr {
                    lhs: Box::new(parse_expr(&tokens[0..idx])?),
                    rhs: Box::new(parse_expr(&tokens[(idx + 1)..])?),
                    op: *op,
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
        .map(parse_expr)
        .collect()
}

fn parse_block(mut tokens: &[Token]) -> Result<Node> {
    let mut out = Vec::new();

    // println!("aaa");
    let mut start_idx = 0;
    let mut it = tokens.iter().enumerate();
    while let Some((idx, token)) = it.next() {
        if let Token::Semicolon = token {
            let line = &tokens[start_idx..idx];
            if line.len() > 0 {
                out.push(Node::Statement(Box::new(parse_expr(line)?)));
            }
            start_idx = idx + 1;
        }
        else if idx == tokens.len()-1 {
            let line = &tokens[start_idx..=idx];
            if line.len() > 0 {
                out.push(parse_expr(line)?);
            }
        }
        else if let Token::Brace(Dir::Left) = token {
            if let Some(end_idx) = find_matching(&tokens[idx..], Token::Brace(Dir::Left), true) {
                // NOTE: end_idx is relative to tokens[idx..]
                it.by_ref().skip(end_idx - 1).next(); // - 1 to account for the next() call
            } else {
                return Err(ASTError::ExpectedToken(Token::Brace(Dir::Right)));
            }
        }
    }

    Ok(Node::Block(out))
}

pub fn parse_func_def(mut tokens: &[Token]) -> Result<Option<(Node, &[Token])>> {
    if let (Token::Func, Token::Word(name)) = (&tokens[0], &tokens[1]) {
        tokens = &tokens[2..];

        // println!("{tokens:#?}");

        let Token::Paren(Dir::Left) = tokens[0] else { panic!("No parameter list after function definition") };

        let matching =
            find_matching(tokens, Token::Paren(Dir::Left), true).expect("End to parameter list");

        let parameter_list = parse_parameter_list(&tokens[1..matching])?;

        tokens = &tokens[(matching + 1)..];

        let return_type = if let (Token::Arrow, Token::Word(return_type)) = (&tokens[0], &tokens[1])
        {
            tokens = &tokens[2..];
            return_type.clone()
        } else {
            "void".to_owned()
        };

        let Token::Brace(Dir::Left) = tokens[0] else {
            return Err(ASTError::ExpectedToken(Token::Brace(Dir::Left)));
        };
        tokens = &tokens[1..];

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
