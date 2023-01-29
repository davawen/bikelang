#[derive(Clone, Copy, Debug, Default)]
pub enum Dir {
    #[default]
    Left,
    Right,
}

#[derive(Clone, Debug)]
pub enum Token {
    Func,
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

pub fn get_string_literal(source: &mut impl Iterator<Item = char>) -> String {
    source.take_while(|&c| c != '"').collect()
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
                    "func" => Some(Token::Func),
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
                    '=' => Some(Token::Op(Operation::Assignment)),
                    '+' => Some(Token::Op(Operation::Add)),
                    '*' => Some(Token::Op(Operation::Mul)),
                    '/' => {
                        if let Some('/') = chars.peek() {
                            // Comment
                            chars.by_ref().skip_while(|&x| x != '\n').for_each(|_| ());
                            None
                        } else {
                            Some(Token::Op(Operation::Div))
                        }
                    }
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

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Assignment,
    Add,
    Sub,
    Mul,
    Div,
}
