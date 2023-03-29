use std::str::FromStr;

use crate::{ast::AstError, error::{Result, ToCompilerError}, utility::Bounds};

#[derive(Debug, Clone)]
pub struct Item {
    pub token: Token,
    pub bounds: Bounds
}

impl Item {
    const EOF: Self = Item { token: Token::Eof, bounds: Bounds::ZERO };
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Dir {
    #[default]
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Paren(Dir),
    Brace(Dir),
    Semicolon,
    Comma,
    Hash,
    Arrow,

    Op(Operation),

    Word(String),
    Number(i64),
    StringLiteral(String),

    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Let,
    Func,
    If,
    Else,
    Loop,
    Break,
    Return,
    True,
    False,
    TypeKeyword,
    Struct
}

impl FromStr for Keyword {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let k = match s {
            "let" => Keyword::Let,
            "func" => Keyword::Func,
            "if" => Keyword::If,
            "else" => Keyword::Else,
            "loop" => Keyword::Loop,
            "break" => Keyword::Break,
            "return" => Keyword::Return,
            "true" => Keyword::True,
            "false" => Keyword::False,
            "type" => Keyword::TypeKeyword,
            "struct" => Keyword::Struct,
            _ => return Err(())
        };

        Ok(k)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    Assignment,

    Equals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Lesser,
    LesserOrEquals,

    Exclamation,
    LogicalAnd,
    LogicalOr,
    LogicalXor,

    AddressOf,
    Dot,

    Plus,
    Minus,
    Times,
    Div,
    Modulus
}

impl From<Keyword> for Token {
    fn from(value: Keyword) -> Self {
        Token::Keyword(value)
    }
}

pub fn get_string_literal(source: &mut impl Iterator<Item = (usize, char)>) -> String {
    let mut out = String::new();

    while let Some((_, c)) = source.next()  {
        let c = match c {
            '\\' => { 
                match source.next().unwrap().1 {
                    'n' => '\n',
                    '\\' => '\\',
                    '"' => '"',
                    b => panic!("Uknown escape sequence: \\{b}")
                } 
            },
            '"' => break,
            _ => c
        };

        out.push(c);
    }

    out
}

pub struct Lexer {
    tokens: Vec<Item>
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        use Dir::*;

        let mut word = String::new();
        let mut chars = source.char_indices().peekable();
        let mut out = Vec::new();
        let mut start = 0;

        while let Some((mut idx, c)) = chars.next() {
            let token = match c {
                c if c.is_alphanumeric() || c == '_' => {
                    word.push(c);
                    None
                }
                _ => {
                    let word_token = match word.as_str() {
                        "" => None,
                        _ if let Ok(key) = word.parse::<Keyword>() => {
                            Some(key.into())
                        }
                        _ => match word.parse::<i64>() {
                            Ok(num) => Some(Token::Number(num)),
                            Err(_) => Some(Token::Word(word)),
                        },
                    };

                    if let Some(token) = word_token {
                        out.push(Item {
                            token,
                            bounds: Bounds { start, end: idx}
                        });
                        start = idx;
                    }
                    word = String::new();

                    macro_rules! then_or {
                        ($then:expr, $($c:expr, $or:expr),+) => {
                            match chars.peek() {
                                $(Some((_, $c)) => {
                                    chars.next();
                                    idx += 1;
                                    $or
                                })+
                                _ => $then
                            }
                        };
                    }

                    let t = match c {
                        '(' => Some(Token::Paren(Left)),
                        ')' => Some(Token::Paren(Right)),
                        '{' => Some(Token::Brace(Left)),
                        '}' => Some(Token::Brace(Right)),
                        ';' => Some(Token::Semicolon),
                        ',' => Some(Token::Comma),
                        '#' => Some(Token::Hash),
                        '"' => Some(Token::StringLiteral(get_string_literal(chars.by_ref()))),
                        '-' => then_or!(
                            Some(Token::Op(Operation::Minus)),
                            '>', Some(Token::Arrow)
                        ),
                        '=' => then_or!(
                            Some(Token::Op(Operation::Assignment)), 
                            '=', Some(Token::Op(Operation::Equals))
                        ),
                        '!' => then_or!(
                            Some(Token::Op(Operation::Exclamation)),
                            '=', Some(Token::Op(Operation::NotEquals))
                        ),
                        '>' => then_or!(
                            Some(Token::Op(Operation::Greater)),
                            '=', Some(Token::Op(Operation::GreaterOrEquals))
                        ),
                        '<' => then_or!(
                            Some(Token::Op(Operation::Lesser)),
                            '=', Some(Token::Op(Operation::LesserOrEquals))
                        ),
                        '&' => then_or!(Some(Token::Op(Operation::AddressOf)), '&', Some(Token::Op(Operation::LogicalAnd))),
                        '.' => Some(Token::Op(Operation::Dot)),
                        '|' => then_or!(None, '|', Some(Token::Op(Operation::LogicalOr))),
                        '^' => then_or!(None, '^', Some(Token::Op(Operation::LogicalXor))),
                        '+' => Some(Token::Op(Operation::Plus)),
                        '*' => Some(Token::Op(Operation::Times)),
                        '/' => then_or! {
                            Some(Token::Op(Operation::Div)),
                            '/',
                            {
                                chars.by_ref().skip_while(|&x| x.1 != '\n').peekable().peek();
                                None
                            }
                        },
                        '%' => Some(Token::Op(Operation::Modulus)),
                        _ => None,
                    };

                    if t.is_none() {
                        start = idx+1;
                    }
                    t
                }
            };

            if let Some(token) = token {
                out.push(Item {
                    token,
                    bounds: Bounds { start, end: idx+1 }
                });
                start = idx+1;
            }
        }

        out.reverse();

        Lexer { tokens: out }
    }

    pub fn next(&mut self) -> Item {
        self.tokens.pop().unwrap_or(Item::EOF)
    }

    pub fn expect(&mut self, tok: Token) -> Result<Item> {
        let n = self.next();
        if n.token == tok {
            Ok(n)
        } else {
            Err(AstError::ExpectedToken(tok, n.token)).at(n.bounds)
        }
    }

    // pub fn expect_fn<F: FnOnce(&Token) -> bool>(&mut self, f: F) -> Result<Item> {
    //     let n = self.next();
    //     if f(&n.token) {
    //         Ok(n)
    //     } else {
    //         Err(AstError::UnexpectedToken("at expect_fn", n.token)).at(n.bounds)
    //     }
    // }

    pub fn peek(&self) -> &Item {
        self.tokens.last().unwrap_or(&Item::EOF)
    }
}

