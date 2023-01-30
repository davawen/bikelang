use std::collections::HashMap;
use indexmap::IndexMap;
use thiserror::Error;

use crate::{ast, token};

#[derive(Debug)]
pub struct App<'a> {
    functions: HashMap<String, Function<'a>>,
}

#[derive(Debug)]
pub struct Function<'a> {
    name: String,
    return_type: String,
    variables: IndexMap<String, Variable>,
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
    VariableStore(usize, Value),
    StoreIntrisic(usize, Intrisic),
    Intrisic(Intrisic),
    Nop
}

#[derive(Debug)]
enum Intrisic {
    Asm(Value),
    Print(Value),
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
}

#[derive(Debug)]
enum Value {
    Number(i32),
    Literal(String),
    VariableLoad(usize),
    Call {
        func: String,
        parameters: Vec<Value>
    },
    NoValue
}

#[derive(Debug)]
struct Variable {
    typename: String,
}

#[derive(Debug, Error)]
pub enum IrError {
    #[error("Wrong node type used, exepected {0}, got {1:?}")]
    WrongNodeType(&'static str, ast::Node),
    #[error("Wrong intrisic used")]
    MalformedIntrisic,
    #[error("Intrisic {0} isn't known by the language")]
    UknownInstric(String),
    #[error("Unknown variable {0} isn't defined anywhere in the function")]
    UknownVariable(String),
    #[error("Parsing function {0} again")]
    FunctionAlreadyParsed(String),
    #[error("Redefinition of {0} {1}")]
    Redefinition(&'static str, String)
}

pub type Result<T> = std::result::Result<T, IrError>;

impl Intrisic {
    fn from_node(name: &str, parameters: &[ast::Node]) -> Result<Self> {
        match name {
            "asm" => {
                if let [ast::Node::StringLiteral(s)] = parameters {
                    Ok(Self::Asm(Value::Literal(s.clone())))
                }
                else {
                    Err(IrError::MalformedIntrisic)
                }
            },
            "print" => {
                if let [ast::Node::StringLiteral(s)] = parameters {
                    Ok(Self::Print(Value::Literal(s.clone())))
                }
                else {
                    Err(IrError::MalformedIntrisic)
                }
            }
            name => Err(IrError::UknownInstric(name.to_owned()))
        }
    }

    fn from_op(op: token::Operation, lhs: Value, rhs: Value) -> Result<Self> {
        use token::Operation;
        match op {
            Operation::Add => Ok(Intrisic::Add(lhs, rhs)),
            Operation::Sub => Ok(Intrisic::Sub(lhs, rhs)),
            Operation::Mul => Ok(Intrisic::Mul(lhs, rhs)),
            Operation::Div => Ok(Intrisic::Div(lhs, rhs)),
            _ => Err(IrError::MalformedIntrisic)
        }
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

        let mut variables = IndexMap::new();

        for v in parameter_list {
            let ast::Node::Definition { name, typename } = v else { return Some(Err(IrError::WrongNodeType("An argument definition", v.clone()))) };

            variables.insert(
                name.clone(),
                Variable {
                    typename: typename.clone(),
                },
            );
        }

        let ast::Node::Block(body) = &**body else { return Some(Err(IrError::WrongNodeType("A function body", *body.clone()))) };

        for expr in body {
            if let ast::Node::Expr { lhs, .. } = expr {
                if let ast::Node::Definition { name, typename } = &**lhs {
                    if let Some(v) = variables.insert(
                        name.clone(),
                        Variable {
                            typename: typename.clone(),
                        },
                    ) {
                        return Some(Err(IrError::Redefinition("variable", name.clone())));
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

    fn fold_node(ins: &mut Vec<Instruction>, vars: &mut IndexMap<String, Variable>, temporary_count: &mut i32, node: &ast::Node) -> Result<Value> {
        match node {
            ast::Node::Expr { lhs, rhs, op } => match op {
                token::Operation::Assignment => {
                    let (ast::Node::Identifier(var) | ast::Node::Definition { name: var, .. }) = &**lhs
                    else { 
                        return Err(IrError::WrongNodeType("A variable name or definition", *lhs.clone()));
                    };
                    let var = vars.get_index_of(var).ok_or(IrError::UknownVariable(var.clone()))?;

                    let rhs = Self::fold_node(ins, vars, temporary_count, rhs)?;
                    ins.push(Instruction::VariableStore(var, rhs));

                    Ok(Value::NoValue)
                }
                &op => {
                    let lhs = Self::fold_node(ins, vars, temporary_count, lhs)?;
                    let rhs = Self::fold_node(ins, vars, temporary_count, rhs)?;

                    let temporary = format!("tmp{temporary_count}");
                    let (temporary, _) = vars.insert_full(temporary, Variable { typename: "bruh".to_owned() });
                    *temporary_count += 1;

                    ins.push(Instruction::StoreIntrisic(temporary, Intrisic::from_op(op, lhs, rhs)?));
                    Ok(Value::VariableLoad(temporary))
                },
            },
            ast::Node::Call { name, intrisic: false, parameter_list } => Ok(Value::Call {
                func: name.clone(),
                parameters: parameter_list.iter().map(|n| Self::fold_node(ins, vars, temporary_count, n)).collect::<Result<Vec<_>>>()?
            }),
            ast::Node::Call { name, intrisic: true, parameter_list } => {
                ins.push(Instruction::Intrisic(Intrisic::from_node(name, parameter_list)?));
                Ok(Value::NoValue)
            },
            ast::Node::Identifier(var) => Ok(
                Value::VariableLoad(vars.get_index_of(var).ok_or(IrError::UknownVariable(var.clone()))?)
            ),
            &ast::Node::Number(n) => Ok(Value::Number(n)),
            ast::Node::StringLiteral(s) => Ok(Value::Literal(s.clone())),
            _ => Err(IrError::WrongNodeType("A defined node", node.clone())),
        }
    }

    /// Used to postpone the interpretation of the function's source until everything in the file is declared
    pub fn integrate_body(&mut self) -> Result<()> {
        let FunctionBody::Raw(body) = self.instructions else { Err(IrError::FunctionAlreadyParsed(self.name.clone()))? };

        let mut ins = Vec::new();
        let mut temporary = 0;

        for node in body {
            Self::fold_node(&mut ins, &mut self.variables, &mut temporary, node).expect("Valid AST input");
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
        let ast::Node::Block(root) = root else { Err(IrError::WrongNodeType("A list of top-level statements", root.clone()))? };

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
