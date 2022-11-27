#![allow(unused)]

use std::iter::Peekable;

use itertools::Itertools;
use enum_as_inner::EnumAsInner;

#[derive(Clone, Copy, Debug, Default)]
enum Dir {
    #[default]
    Left,
    Right 
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Assignment,
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Clone, Debug, EnumAsInner)]
enum Token {
    Func,
    Paren(Dir),
    Brace(Dir),
    Semicolon,
    Comma,
    Arrow,

    Op(Operation),

    Identifier(String),
    Number(i32)
}

fn tokenize(source: &str) -> Vec<Token> {
    use Dir::*;

    let mut word = String::new();
    let mut chars = source.chars().peekable();
    let mut out = Vec::new();

    while let Some(c) = chars.next() {
        let token = match c {
            c if c.is_alphanumeric() => {
                word.push(c);
                None
            }
            _ => {
                let word_token = match word.as_str() {
                    "" => None,
                    "->" => Some(Token::Arrow),
                    "func" => Some(Token::Func),
                    _ => {
                        match word.parse::<i32>() {
                            Ok(num) => Some(Token::Number(num)),
                            Err(_) => Some(Token::Identifier(word))
                        }
                    }
                };
                if let Some(token) = word_token { out.push(token); }
                word = String::new();

                match c {
                    '(' => Some(Token::Paren(Left)),
                    ')' => Some(Token::Paren(Right)),
                    '{' => Some(Token::Brace(Left)),
                    '}' => Some(Token::Brace(Right)),
                    ';' => Some(Token::Semicolon),
                    ',' => Some(Token::Comma),
                    '=' => Some(Token::Op(Operation::Assignment)),
                    '+' => Some(Token::Op(Operation::Add)),
                    '-' => {
                        if let Some('>') = chars.peek() {
                            chars.next();
                            Some(Token::Arrow)
                        }
                        else { Some(Token::Op(Operation::Sub)) }
                    },
                    '*' => Some(Token::Op(Operation::Mul)),
                    '/' => Some(Token::Op(Operation::Div)),
                    _ => None
                }
            }
        };

        if let Some(token) = token { out.push(token); }
    }

    out
}

impl Operation {
    fn precedence(&self) -> usize {
        use Operation::*;
        match self {
            Mul | Div => 1,
            Add | Sub => 2,
            Assignment => 3,
        }
    }
}

#[derive(Debug, Clone)]
enum Node {
    FuncDef{ name: String, parameter_list: Vec<Node>, return_type: String, body: Box<Node> },
    Definition{ typename: String, name: String },
    Expr{ lhs: Box<Node>, rhs: Box<Node>, op: Operation },
    Call{ name: String, parameter_list: Vec<Node> },
    Block(Vec<Node>),
    Number(i32),
    Identifier(String)
}

fn parse_expr(mut tokens: &[Token]) -> Node {
    if let [Token::Number(num)] = tokens {
        Node::Number(*num)
    }
    else if let [Token::Identifier(name)] = tokens {
        Node::Identifier(name.clone())
    }
    else if let [Token::Identifier(typename), Token::Identifier(name)] = tokens {
        Node::Definition { typename: typename.clone(), name: name.clone() }
    }
    else if let [lhs, Token::Op(op), rhs] = tokens {
        Node::Expr {
            lhs: Box::new(parse_expr(&tokens[0..1])),
            rhs: Box::new(parse_expr(&tokens[2..3])),
            op: *op
        }
    }
    else if let Some((op_idx, op)) = tokens.iter().enumerate()
        .filter(|(_, t)| matches!(t, Token::Op(_)) )
        .map(|(i, t)| (i, t.as_op().unwrap()))
        .max_by_key(|(_, op)| op.precedence() )
    {
        Node::Expr {
            lhs: Box::new(parse_expr(&tokens[..op_idx])),
            rhs: Box::new(parse_expr(&tokens[(op_idx+1)..])),
            op: *op
        }
    }
    else {
        println!("{tokens:#?}");
        unreachable!()
    }
}

fn parse_line(mut tokens: &[Token]) -> Option<Node> {
    assert!(matches!(tokens.last().unwrap(), Token::Semicolon));
    tokens = &tokens[..(tokens.len()-1)];

    Some(parse_expr(tokens))
}

fn find_matching(tokens: &[Token], token: Token) -> Option<usize> {
    let mut scope = 0;
    for (idx, tok) in tokens.iter().enumerate() {
        if let Token::Brace(_) = token {
            if let Token::Brace(Dir::Left) = tok { scope += 1 }
            else if let Token::Brace(Dir::Right) = tok { scope -= 1 };
        }
        else if let Token::Paren(_) = token {
            if let Token::Paren(Dir::Left) = tok { scope += 1 }
            else if let Token::Paren(Dir::Right) = tok { scope -= 1 };
        }

        if scope == 0 { return Some(idx); }
    }
    None
}

fn parse_block(mut tokens: &[Token]) -> Node {
    assert!(matches!(tokens[0], Token::Brace(Dir::Left)));
    assert!(matches!(tokens.last().unwrap(), Token::Brace(Dir::Right)));

    tokens = &tokens[1..(tokens.len()-1)];
    let mut out = Vec::new();

    let mut start_idx = 0;
    for (idx, token) in tokens.iter().enumerate() {
        if let Token::Semicolon = token {
            if let Some(node) = parse_line(&tokens[start_idx..=idx]) {
                out.push(node);
            }
            start_idx = idx+1;
        }
    }

    Node::Block(out)
}

fn parse_func_def(mut tokens: &[Token]) -> Node {
    if let (Token::Func, Token::Identifier(name)) = (&tokens[0], &tokens[1]) {
        tokens = &tokens[2..];

        let mut parameter_list = Vec::new();
        if let Token::Paren(Dir::Left) = tokens[0] {
            let start_idx = 1;
            let mut end_idx = usize::MAX;

            for (idx, token) in tokens[0..].iter().enumerate() { 
                if let Token::Comma = token {
                    let arg = parse_expr(&tokens[start_idx..idx]);

                    let Node::Definition { .. } = arg else { panic!("Wrong stuff in parameter list") };
                    parameter_list.push(arg);
                }
                else if let Token::Paren(Dir::Right) = token { 
                    end_idx = idx;
                    break; 
                }
            };
            if(end_idx == usize::MAX) { panic!("No end of parameter list"); }

            tokens = &tokens[(end_idx+1)..];
        }
        else { panic!("No parameter list after function definition") }

        let return_type = if let (Token::Arrow, Token::Identifier(return_type)) = ( &tokens[0], &tokens[1] ) {
            tokens = &tokens[2..];
            return_type
        }
        else { panic!("No return type") };

        let body = if let Some(end_idx) = find_matching(tokens, Token::Brace(Dir::Left)) {
            parse_block(&tokens[0..=end_idx])
        }
        else { panic!("No brace after function definition") };

        Node::FuncDef { name: name.clone(), parameter_list, return_type: return_type.clone(), body: Box::new(body) }
    }
    else { panic!("") }
}

// fn parse(mut tokens: &[Token]) -> Node {
//     /// Returns the index of the matching brace or parenthesis
//     /// The start token should be included in `tokens`
//
//     let mut out = Vec::new();
//
//     while !tokens.is_empty() {
//     }
//
//     out
// }

const SOURCE: &str = "
func main() -> int {
    int a = 32 + 10 * 2 / 4;
    int b = a;

    0;
}
";

fn main() {
    let tokens = tokenize(SOURCE);
    // println!("{tokens:#?}");

    let ast = parse_func_def(&tokens);
    println!("Source:{SOURCE}\nAst:\n{ast:#?}");
}
