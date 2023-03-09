use std::default::default;

use super::*;

/// Parsing mode
trait Mode {
    type Output;

    fn lbp(&self, item: &Item) -> Result<u32>;

    fn nud(&self, item: Item, lexer: &mut Lexer) -> Result<Self::Output>;
    fn led(&self, item: Item, lexer: &mut Lexer, left: Self::Output) -> Result<Self::Output>;
}

struct ExpressionMode;

impl Mode for ExpressionMode {
    type Output = Ast;

    fn lbp(&self, item: &Item) -> Result<u32> {
        use Token::*;
        use Operation::*;
        let out = match &item.token {
            Op(op) => match op {
                Assignment => 10,
                LogicalAnd | LogicalOr | LogicalXor => 20,
                Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => 30,
                Plus | Minus => 40,
                Times | Div | Modulus => 50,
                Exclamation | AddressOf => return Err(AstError::WrongOperation(*op, "as an infix operator")).at_item(item) 
            }
            Paren(Dir::Left) => 70, // operator ()
            Hash => 70,
            Keyword(token::Keyword::Else) => 70, // bind elses to if-s;
            Brace(Dir::Left) => 1, // allow stopping at opening brace
            Paren(Dir::Right) | Brace(Dir::Right) | Keyword(_) | Comma | Semicolon | Eof => 0,
            token => return Err(AstError::UnexpectedToken("unbindable token used infix", token.clone())).at_item(item)
        };
        Ok(out)
    }

    fn nud(&self, item: Item, lexer: &mut Lexer) -> Result<Self::Output>{
        use Token::*;
        let node = match item.token {
            Number(n) => Node::Number(n, Type::Void).ast(item.bounds),
            StringLiteral(s) => Node::StringLiteral(s).ast(item.bounds),
            Word(name) => Node::Identifier(name, Type::Void).ast(item.bounds),
            Op(Operation::Lesser) => { // parse type conversion
                let convert_to = pratt(&TypeMode, lexer, 0)?;
                lexer.expect(Token::Op(Operation::Greater))?;

                let operand = box pratt(self, lexer, 60)?;

                Ast::new(
                    item.bounds.with_end_of(operand.bounds),
                    Node::Convert(operand, convert_to)
                )
            }
            Op(op) => {
                let (op, power) = match op {
                    Operation::Minus => (UnaryOperation::Negation, 60),
                    Operation::Times => (UnaryOperation::Deref, 60),
                    Operation::AddressOf => (UnaryOperation::AddressOf, 60),
                    Operation::Exclamation => (UnaryOperation::LogicalNot, 60),
                    _ => return Err(AstError::WrongOperation(op, "as a prefix operator")).at_item(&item)
                };

                let value = box pratt(self, lexer, power)?;
                Ast::new(
                    item.bounds.with_end_of(value.bounds),
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
                    Let => {
                        let typename = pratt(&TypeMode, lexer, 0)?;
                        let name = lexer.next();
                        let rbounds = name.bounds;
                        let Token::Word(name) = name.token else {
                            return Err(AstError::ExpectedToken(Token::Word("variable-name".to_owned()), name.token)).at(rbounds)
                        };

                        Node::Definition {
                            typename,
                            name
                        }.ast(item.bounds.with_end_of(rbounds))
                    }
                    Func => {
                        let name = lexer.next();
                        let Token::Word(name) = name.token
                            else { return Err(AstError::Expected("a function name", name.token)).at(name.bounds) };

                        lexer.expect(Token::Paren(Dir::Left))?;
                        let parameter_list = parameter_list(lexer)?;
                        let return_type = if lexer.peek().token == Token::Arrow {
                            lexer.next();
                            pratt(&TypeMode, lexer, 0)?
                        } else {
                            Type::Void
                        };

                        let body = box parse_block(lexer.next(), lexer)?;
                        Ast::new(
                            item.bounds.with_end_of(body.bounds),
                            Node::FuncDef {
                                name,
                                parameter_list,
                                body,
                                return_type
                            }
                        )
                    }
                    If => {
                        let condition = box pratt(self, lexer, 1)?;
                        let body = box parse_block(lexer.next(), lexer)?;
                        Ast::new(
                            item.bounds.with_end_of(body.bounds),
                            Node::If {
                                condition,
                                body,
                                else_body: None,
                                ty: Type::Void
                            }
                        )
                    }
                    Loop => {
                        let body = box parse_block(lexer.next(), lexer)?;
                        Ast::new(
                            item.bounds.with_end_of(body.bounds),
                            Node::Loop {
                                body
                            }
                        )
                    }
                    Break => Node::Break.ast(item.bounds),
                    Return => {
                        let expr = box pratt(self, lexer, 0)?;
                        Ast::new(item.bounds.with_end_of(expr.bounds), Node::Return(expr))
                    }
                    True => Ast::new(item.bounds, Node::BoolLiteral(true)),
                    False => Ast::new(item.bounds, Node::BoolLiteral(false)),
                    Else => return Err(AstError::UnexpectedToken("missing an if clause before else", Keyword(keyword))).at(item.bounds)
                }
            }
            Paren(Dir::Left) => { // parenthesis for grouping operations
                let inner = pratt(self, lexer, 0)?;
                let rparen = lexer.expect(Paren(Dir::Right))?;

                inner.extend(item.bounds.with_end_of(rparen.bounds))
            }
            Brace(Dir::Left) => {
                parse_block(item, lexer)?
            }
            Semicolon => Node::Empty.ast_from(item),
            _ => unreachable!("{item:#?}")
        };

        Ok(node)
    } 

    fn led(&self, item: Item, lexer: &mut Lexer, left: Ast) -> Result<Self::Output> {
        use Token::*;
        let node = match item.token {
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

                let rhs = box pratt(self, lexer, power)?;
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
            Paren(Dir::Left) => { // parenthesis operator = function call
                let Node::Identifier(name, _) = left.node
                    else { return Err(AstError::ExpectedNode("a function name", left.node)).at(left.bounds)};

                let (argument_list, rtoken) = argument_list(lexer)?;
                Node::Call {
                    name,
                    return_type: Type::Void,
                    argument_list
                }.ast(left.bounds.with_end_of(rtoken.bounds))
            }
            Hash => { // intrisic 'function call'
                let Node::Identifier(name, _) = left.node
                    else { return Err(AstError::ExpectedNode("an intrisic name", left.node)).at(left.bounds) };

                lexer.expect(Token::Paren(Dir::Left))?;
                let (list, rtoken) = argument_list(lexer)?;

                let intrisic = match name.as_str() {
                    "asm" => Intrisic::Asm(box list.into_iter().next().unwrap()),
                    "print" => Intrisic::Print(list),
                    _ => return Err(AstError::UnknownIntrisic(name)).at(left.bounds)
                };
                Node::Intrisic(intrisic).ast(left.bounds.with_end_of(rtoken.bounds))
            }
            Keyword(token::Keyword::Else) => {
                let Node::If { condition, body, else_body: _, ty } = left.node
                    else { return Err(AstError::ExpectedNode("if condition", left.node)).at(left.bounds) };

                let else_body = match lexer.peek().token {
                    Keyword(token::Keyword::If) => {
                        box pratt(self, lexer, 0)?
                    },
                    _ => box parse_block(lexer.next(), lexer)?
                };
                Ast::new(
                    left.bounds.with_end_of(else_body.bounds),
                    Node::If { condition, body, else_body: Some(else_body), ty }
                )
            }
            _ => unreachable!("{item:#?}")
        };

        Ok(node)
    } 
}

struct TypeMode;
impl Mode for TypeMode {
    type Output = Type;

    fn lbp(&self, item: &Item) -> Result<u32> {
        use Token::*;
        // > to close type conversions
        // stop on left-binding word = declaration
        let out = match &item.token {
            Op(Operation::Greater) | Comma | Word(_) | Brace(Dir::Left) => 0,
            token => return Err(AstError::UnexpectedToken("cannot use token in type declaration", token.clone())).at_item(item)
        };
        Ok(out)
    }

    fn nud(&self, item: Item, lexer: &mut Lexer) -> Result<Self::Output> {
        use Token::*;
        let node = match item.token {
            Op(Operation::Times) => {
                let inner = pratt(self, lexer, 60)?;
                inner.into_ptr()
            },
            Word(typename) => {
                Type::from_str(&typename).at(item.bounds)?
            }
            token => return Err(AstError::UnexpectedToken("cannot create type from token", token)).at(item.bounds)
        };

        Ok(node)
    }
    
    fn led(&self, item: Item, _lexer: &mut Lexer, _left: Self::Output) -> Result<Self::Output> {
        Err(AstError::UnexpectedToken("no left binding token in type declarations", item.token)).at(item.bounds)
    }
}


impl Item {
    fn lbp<T>(&self, mode: &dyn Mode<Output = T>) -> Result<u32> {
        mode.lbp(self)
    }

    fn nud<T>(self, mode: &dyn Mode<Output = T>, lexer: &mut Lexer) -> Result<T> {
        mode.nud(self, lexer)
    }

    fn led<T>(self, mode: &dyn Mode<Output = T>, lexer: &mut Lexer, left: T) -> Result<T> {
        mode.led(self, lexer, left)
    }
}

/// Parses a list of function parameters in the form `type var, type var`
/// Always returns a list of definitions
fn parameter_list(lexer: &mut Lexer) -> Result<Vec<Ast>> {
    let mut list = Vec::new();
    if lexer.peek().token != Token::Paren(Dir::Right) {
        loop {
            let definition = {
                let start = lexer.peek().bounds; // Types don't have bounds so get it manually
                
                let typename = pratt(&TypeMode, lexer, 0)?;
                let name = lexer.next();
                let rbounds = name.bounds;
                let Token::Word(name) = name.token else {
                    return Err(AstError::ExpectedToken(Token::Word("variable-name".to_owned()), name.token)).at(rbounds)
                };

                Node::Definition {
                    typename,
                    name
                }.ast(start.with_end_of(rbounds))
            };

            list.push(definition);
            if lexer.peek().token != Token::Comma {
                break;
            }
            lexer.expect(Token::Comma)?;
        }
    }
    lexer.expect(Token::Paren(Dir::Right))?;

    Ok(list)
}

/// Parses a list of comma separated expressions
/// Returns the closing parenthesis of the argument list
fn argument_list(lexer: &mut Lexer) -> Result<(Vec<Ast>, Item)> {
    let mut list = Vec::new();
    if lexer.peek().token != Token::Paren(Dir::Right) {
        loop {
            list.push(pratt(&ExpressionMode, lexer, 0)?);
            if lexer.peek().token != Token::Comma {
                break;
            }
            lexer.expect(Token::Comma)?;
        }
    }
    
    Ok((list, lexer.expect(Token::Paren(Dir::Right))?))
}

/// Parses a block and returns an Ast::Block
fn parse_block(start_brace: Item, lexer: &mut Lexer) -> Result<Ast> {
    if start_brace.token != Token::Brace(Dir::Left) {
        return Err(AstError::ExpectedToken(Token::Brace(Dir::Left), start_brace.token)).at(start_brace.bounds)
    }

    let (inner, rtoken) = parse_block_vec(lexer)?;
    Ok(Node::Block { inner, ty: default() }.ast(start_brace.bounds.with_end_of(rtoken.bounds)))
}

/// Parses a block including its ending brace
/// Returns the list of expressions inside of it, as well as its closing brace for bounds check
fn parse_block_vec(lexer: &mut Lexer) -> Result<(Vec<Ast>, Item)> {
    let mut block = Vec::new();
    while lexer.peek().token != Token::Brace(Dir::Right) {
        let expr = pratt(&ExpressionMode, lexer, 0)?;
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

fn pratt<T>(mode: &dyn Mode<Output = T>, lexer: &mut Lexer, rbp: u32) -> Result<T> {
    let mut t = lexer.next();
    let mut left = t.nud(mode, lexer)?;

    while lexer.peek().lbp(mode)? > rbp {
        t = lexer.next();

        left = t.led(mode, lexer, left)?;
    }

    Ok(left)
}

pub fn parse_ast(lexer: &mut Lexer) -> Result<Ast> {
    let mut root = Vec::new();
    while lexer.peek().token != Token::Eof {
        root.push(pratt(&ExpressionMode, lexer, 0)?);
    }

    let bounds = Bounds {
        start: root.first().map(|x| x.bounds.start).unwrap_or(0),
        end: root.last().map(|x| x.bounds.end).unwrap_or(0) 
    };
    Ok(Node::Block { inner: root, ty: default() }.ast(bounds))
}
