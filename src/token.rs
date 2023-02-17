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
    Func,
    If,
    Loop,
    Break,
    Return
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

pub fn get_string_literal(source: &mut impl Iterator<Item = char>) -> String {
    let mut out = String::new();

    while let Some(c) = source.next()  {
        let c = match c {
            '\\' => { 
                match source.next().unwrap() {
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
    tokens: Vec<Token>
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        use Dir::*;

        let mut word = String::new();
        let mut chars = source.chars().peekable();
        let mut out = Vec::new();

        while let Some(c) = chars.next() {
            let token = match c {
                c if c.is_alphanumeric() || c == '_' => {
                    word.push(c);
                    None
                }
                _ => {
                    let word_token = match word.as_str() {
                        "" => None,
                        "func" => Some(Keyword::Func.into()),
                        "if" => Some(Keyword::If.into()),
                        "loop" => Some(Keyword::Loop.into()),
                        "break" => Some(Keyword::Break.into()),
                        "return" => Some(Keyword::Return.into()),
                        _ => match word.parse::<i64>() {
                            Ok(num) => Some(Token::Number(num)),
                            Err(_) => Some(Token::Word(word)),
                        },
                    };
                    if let Some(token) = word_token {
                        out.push(token);
                    }
                    word = String::new();

                    macro_rules! then_or {
                        ($then:expr, $($c:expr, $or:expr),+) => {
                            match chars.peek() {
                                $(Some($c) => {
                                    chars.next();
                                    $or
                                })+
                                _ => $then
                            }
                        };
                    }

                    match c {
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
                        '&' => then_or!(None, '&', Some(Token::Op(Operation::LogicalAnd))),
                        '|' => then_or!(None, '|', Some(Token::Op(Operation::LogicalOr))),
                        '^' => then_or!(None, '^', Some(Token::Op(Operation::LogicalXor))),
                        '+' => Some(Token::Op(Operation::Plus)),
                        '*' => Some(Token::Op(Operation::Times)),
                        '/' => {
                            if let Some('/') = chars.peek() {
                                chars.by_ref().skip_while(|&x| x != '\n').peekable().peek();
                                None
                            } else {
                                Some(Token::Op(Operation::Div))
                            }
                        }
                        '%' => Some(Token::Op(Operation::Modulus)),
                        _ => None,
                    }
                }
            };

            if let Some(token) = token {
                out.push(token);
            }
        }

        out.reverse();

        Lexer { tokens: out }
    }

    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    pub fn expect(&mut self, tok: Token) {
        if self.next() != tok {
            panic!("Wrong token");
        }
    }

    pub fn peek(&self) -> &Token {
        self.tokens.last().unwrap_or(&Token::Eof)
    }
}

