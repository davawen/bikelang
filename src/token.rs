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
    Break,
    Return
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

    LogicalNot,
    LogicalAnd,
    LogicalOr,
    LogicalXor,

    Add,
    Sub,
    Mul,
    Div,
    Modulus
}

impl Operation {
    pub fn precedence(&self) -> u32 {
        use Operation::*;
        match self {
            LogicalNot => 1,
            Mul | Div | Modulus => 2,
            Add | Sub => 3,
            Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => 4,
            LogicalAnd | LogicalOr | LogicalXor => 5,
            Assignment => 6
        }
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
                    "return" => Some(Keyword::Return.into()),
                    _ => match word.parse::<i32>() {
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
                        Some(Token::Op(Operation::Sub)),
                        '>', Some(Token::Arrow)
                    ),
                    '=' => then_or!(
                        Some(Token::Op(Operation::Assignment)), 
                        '=', Some(Token::Op(Operation::Equals))
                    ),
                    '!' => then_or!(
                        Some(Token::Op(Operation::LogicalNot)),
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
                    '+' => Some(Token::Op(Operation::Add)),
                    '*' => Some(Token::Op(Operation::Mul)),
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

    out.push(Token::Eof);

    out
}
