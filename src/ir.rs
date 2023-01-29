use std::collections::HashMap;

use crate::{ast, token};

#[derive(Debug)]
pub struct App<'a> {
    functions: HashMap<String, Function<'a>>,
}

#[derive(Debug)]
pub struct Function<'a> {
    name: String,
    return_type: String,
    variables: HashMap<String, Variable>,
    instructions: FunctionBody<'a>,
}

#[derive(Debug)]
enum FunctionBody<'a> {
    Parsed(Vec<Instruction>),
    /// This is the unparsed ast body of the function
    Raw(&'a Vec<ast::Node>),
}

#[derive(Debug)]
enum Instruction {
    VariableStore(String, Box<Instruction>),
    VariableLoad(String),
    Constant(Constant),
    Call {
        func: String,
        parameters: Vec<Instruction>
    },
    Intrisic(Intrisic),
    Nop
}

#[derive(Debug)]
enum Intrisic {
    Asm(Constant),
    Print(Constant),
    Add(Box<Instruction>, Box<Instruction>),
    Sub(Box<Instruction>, Box<Instruction>),
    Mul(Box<Instruction>, Box<Instruction>),
    Div(Box<Instruction>, Box<Instruction>),
}

#[derive(Debug)]
enum Constant {
    Number(i32),
    Literal(String)
}

#[derive(Debug)]
struct Variable {
    name: String,
    typename: String,
}

#[derive(Debug)]
pub enum Error {
    WrongNodeType,
    MalformedIntrisic,
    UknownInstric(String),
    FunctionAlreadyParsed,
}

pub type Result<T> = std::result::Result<T, Error>;

impl Intrisic {
    fn from_node(name: &str, parameters: &[ast::Node]) -> Result<Self> {
        match name {
            "asm" => {
                if let [ast::Node::StringLiteral(s)] = parameters {
                    Ok(Self::Asm(Constant::Literal(s.clone())))
                }
                else {
                    Err(Error::MalformedIntrisic)
                }
            },
            "print" => {
                if let [ast::Node::StringLiteral(s)] = parameters {
                    Ok(Self::Print(Constant::Literal(s.clone())))
                }
                else {
                    Err(Error::MalformedIntrisic)
                }
            }
            name => Err(Error::UknownInstric(name.to_owned()))
        }
    }

    fn from_op(lhs: &ast::Node, rhs: &ast::Node, op: token::Operation) -> Result<Self> {
        use token::Operation;
        match op {
            Operation::Add => Ok(Intrisic::Add(
                Box::new(integrate_node(lhs)?),
                Box::new(integrate_node(rhs)?)
            )),
            Operation::Sub => Ok(Intrisic::Sub(
                Box::new(integrate_node(lhs)?),
                Box::new(integrate_node(rhs)?)
            )),
            Operation::Mul => Ok(Intrisic::Mul(
                Box::new(integrate_node(lhs)?),
                Box::new(integrate_node(rhs)?)
            )),
            Operation::Div => Ok(Intrisic::Div(
                Box::new(integrate_node(lhs)?),
                Box::new(integrate_node(rhs)?)
            )),
            _ => Err(Error::MalformedIntrisic)
        }
    }
}

fn integrate_node(node: &ast::Node) -> Result<Instruction> {
    match node {
        ast::Node::Expr { lhs, rhs, op } => match op {
            token::Operation::Assignment => {
                let (ast::Node::Identifier(var) | ast::Node::Definition { name: var, .. }) = &**lhs else { Err(Error::WrongNodeType)? };

                Ok(Instruction::VariableStore(
                    var.clone(),
                    Box::new(integrate_node(rhs)?),
                ))
            }
            &op => Ok(Instruction::Intrisic(Intrisic::from_op(lhs, rhs, op)?)),
        },
        ast::Node::Call { name, intrisic: false, parameter_list } => Ok(Instruction::Call {
            func: name.clone(),
            parameters: parameter_list.iter().map(integrate_node).collect::<Result<Vec<_>>>()?
        }),
        ast::Node::Call { name, intrisic: true, parameter_list } => Ok(Instruction::Intrisic(
            Intrisic::from_node(name, parameter_list)?
        )),
        ast::Node::Identifier(var) => Ok(Instruction::VariableLoad(var.clone())),
        &ast::Node::Number(n) => Ok(Instruction::Constant(Constant::Number(n))),
        ast::Node::StringLiteral(s) => Ok(Instruction::Constant(Constant::Literal(s.clone()))),
        ast::Node::Definition{ .. } => Ok(Instruction::Nop),
        _ => unimplemented!(),
    }
}

impl<'a> Function<'a> {
    /// Returns a Some(Ok(Function)) if `node` is a correctly defined function,
    /// a Some(Err(Function)) if it still represents a function but is ill-defined,
    /// or a None if `node` doesn't represent a function.
    ///
    /// * `node`: Given node
    pub fn get_declaration(node: &'a ast::Node) -> Option<Result<Function<'a>>> {
        let ast::Node::FuncDef { name, return_type, parameter_list, body } = node
            else { None? };

        let mut variables = HashMap::new();

        for v in parameter_list {
            let ast::Node::Definition { name, typename } = v else { return Some(Err(Error::WrongNodeType)) };

            variables.insert(
                name.clone(),
                Variable {
                    name: name.clone(),
                    typename: typename.clone(),
                },
            );
        }

        let ast::Node::Block(body) = &**body else { return Some(Err(Error::WrongNodeType)) };

        for expr in body {
            if let ast::Node::Expr { lhs, .. } = expr {
                if let ast::Node::Definition { name, typename } = &**lhs {
                    if let Some(v) = variables.insert(
                        name.clone(),
                        Variable {
                            name: name.clone(),
                            typename: typename.clone(),
                        },
                    ) {
                        panic!("Variable redefinition")
                    }
                }
            }
        }

        Some(Ok(Self {
            name: name.clone(),
            return_type: return_type.clone(),
            variables,
            instructions: FunctionBody::Raw(body),
        }))
    }

    /// Used to postpone the interpretation of the function's source until everything in the file is declared
    pub fn integrate_body(&mut self) -> Result<()> {
        let FunctionBody::Raw(body) = self.instructions else { Err(Error::FunctionAlreadyParsed)? };

        let mut ins = Vec::new();

        for node in body {
            ins.push(integrate_node(node)?);
        }

        self.instructions = FunctionBody::Parsed(ins);

        Ok(())
    }
}

impl<'a> App<'a> {
    pub fn new() -> Self {
        App {
            functions: HashMap::new(),
        }
    }

    pub fn get_declarations(&mut self, root: &'a ast::Node) -> Result<()> {
        let ast::Node::Block(root) = root else { Err(Error::WrongNodeType)? };

        for statement in root.iter().flat_map(Function::get_declaration) {
            let statement = statement?;
            self.functions.insert(statement.name.clone(), statement);
        }

        Ok(())
    }

    pub fn integrate_definitions(&mut self) -> Result<()> {
        for f in self.functions.values_mut() {
            f.integrate_body()?;
        }

        Ok(())
    }
}
