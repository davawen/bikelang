use std::collections::HashMap;

use indexmap::IndexMap;
use thiserror::Error;

use crate::{ast::Node, utility::PushIndex};

#[derive(Debug)]
pub struct App {
    function_definitions: HashMap<String, Function>,
    function_bodies: Vec<FunctionBody>
}

#[derive(Debug)]
pub struct Function {
    return_type: String,
    variables: HashMap<String, Type>,
    body: FunctionBodyIndex,
}

pub type FunctionBody = Vec<Node>;
pub type FunctionBodyIndex = usize;

#[derive(Debug)]
struct Type {
    name: String,
}

#[derive(Debug, Clone, Error)]
pub enum AnalysisError {
    #[error("Wrong node type used, exepected {0}, got {1:?}")]
    WrongNodeType(&'static str, Node),
    #[error("Redefinition of {0} {1}")]
    Redefinition(&'static str, String)
}

pub type Result<T> = std::result::Result<T, AnalysisError>;

impl Function {
    fn is_declaration(node: &Node) -> bool {
        matches!(node, Node::FuncDef { .. })
    }

    /// Inserts a function declaration and body into an app
    fn insert(app: &mut App, node: Node) -> Result<()> {
        let Node::FuncDef { name, return_type, parameter_list, body } = node
            else { return Err(AnalysisError::WrongNodeType("No function definition given...", node)) };

        let mut variables = HashMap::new();

        for v in parameter_list {
            let Node::Definition { name, typename } = v else { return Err(AnalysisError::WrongNodeType("An argument definition", v.clone())) };

            variables.insert(
                name.clone(),
                Type {
                    name: typename.clone(),
                },
            );
        }

        let Node::Block(body) = *body else { return Err(AnalysisError::WrongNodeType("A function body", *body)) };

        for expr in &body {
            if let Node::Expr { lhs, .. } = expr {
                if let Node::Definition { name, typename } = &**lhs {
                    if let Some(_) = variables.insert(
                        name.clone(),
                        Type {
                            name: typename.clone(),
                        },
                    ) {
                        return Err(AnalysisError::Redefinition("variable", name.clone()));
                    }
                }
            }
        }

        let body = app.function_bodies.push_idx(body);

        app.function_definitions.insert(name, Self {
            return_type: return_type.clone(),
            variables,
            body
        });

        Ok(())
    }
}

impl App {
    pub fn new() -> Self {
        Self {
            function_definitions: HashMap::new(),
            function_bodies: Vec::new()
        }
    }
    
    pub fn get_declarations(&mut self, root: Node) -> Result<()> {
        let Node::Block(root) = root else { Err(AnalysisError::WrongNodeType("A list of top-level statements", root.clone()))? };

        for statement in root {
            if Function::is_declaration(&statement) {
                Function::insert(self, statement)?;
            }
        }

        Ok(())
    }
}
