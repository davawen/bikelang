#[derive(Clone, Copy, Debug, Default)]
pub enum Dir {
    #[default]
    Left,
    Right,
}

#[derive(Debug, Clone)]
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
    Number(i32),
    StringLiteral(String),

    Eof,
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Func,
    If,
    Loop,
    Break
}

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Assignment,

    Equals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Lesser,
    LesserOrEquals,

    Add,
    Sub,
    Mul,
    Div,
}

impl Operation {
    pub fn precedence(&self) -> u32 {
        use Operation::*;
        match self {
            Mul | Div => 1,
            Add | Sub => 2,
            Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => 3,
            Assignment => 4
        }
    }

    pub fn is_comparison(&self) -> bool {
        use Operation::*;
        matches!(self, Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals)
    }

    pub fn is_arithmetic(&self) -> bool {
        use Operation::*;
        matches!(self, Add | Sub | Mul | Div)
    }
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

pub fn tokenize(source: &str) -> Vec<Token> {
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
                    _ => match word.parse::<i32>() {
                        Ok(num) => Some(Token::Number(num)),
                        Err(_) => Some(Token::Word(word)),
                    },
                };
                if let Some(token) = word_token {
                    out.push(token);
                }
                word = String::new();

                match c {
                    '(' => Some(Token::Paren(Left)),
                    ')' => Some(Token::Paren(Right)),
                    '{' => Some(Token::Brace(Left)),
                    '}' => Some(Token::Brace(Right)),
                    ';' => Some(Token::Semicolon),
                    ',' => Some(Token::Comma),
                    '#' => Some(Token::Hash),
                    '"' => Some(Token::StringLiteral(get_string_literal(chars.by_ref()))),
                    '-' => {
                        if let Some('>') = chars.peek() {
                            chars.next();
                            Some(Token::Arrow)
                        } else {
                            Some(Token::Op(Operation::Sub))
                        }
                    },
                    '=' => {
                        if let Some('=') = chars.peek() {
                            chars.next();
                            Some(Token::Op(Operation::Equals))
                        } else {
                            Some(Token::Op(Operation::Assignment))
                        }
                    },
                    '!' => {
                        if let Some('=') = chars.peek() {
                            chars.next();
                            Some(Token::Op(Operation::NotEquals))
                        }
                        else { None }
                    },
                    '>' => {
                        if let Some('=') = chars.peek() {
                            chars.next();
                            Some(Token::Op(Operation::GreaterOrEquals))
                        }
                        else { Some(Token::Op(Operation::Greater)) }
                    },
                    '<' => {
                        if let Some('=') = chars.peek() {
                            chars.next();
                            Some(Token::Op(Operation::LesserOrEquals))
                        }
                        else { Some(Token::Op(Operation::Lesser)) }
                    },
                    '+' => Some(Token::Op(Operation::Add)),
                    '*' => Some(Token::Op(Operation::Mul)),
                    '/' => {
                        if let Some('/') = chars.peek() {
                            chars.by_ref().skip_while(|&x| x != '\n').peekable().peek();
                            None
                        } else {
                            Some(Token::Op(Operation::Div))
                        }
                    },
                    _ => None,
                }
            }
        };

        if let Some(token) = token {
            out.push(token);
        }
    }

    out.push(Token::Eof);

    out
}
