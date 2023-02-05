use std::collections::HashMap;
use indexmap::{IndexMap, indexmap};
use thiserror::Error;
use derive_more::{Deref, DerefMut};

use crate::{ast::Node, utility::PushIndex, token::Operation};

#[derive(Debug)]
pub struct App {
    function_definitions: IndexMap<String, Function>,
    function_bodies: Vec<FunctionBody>,
    types: IndexMap<String, Type>,
}

#[derive(Debug)]
pub struct Function {
    return_type: TypeIndex,
    variables: IndexMap<String, TypeIndex>,
    arguments: Vec<VariableIndex>,
    body: FunctionBodyIndex,
}

#[derive(Debug, Deref, DerefMut)]
pub struct FunctionBody {
    #[deref]
    #[deref_mut]
    body: Vec<Node>,
    definition: FunctionIndex
}

#[derive(Debug, Clone)]
struct Type {
    /// Size of the type in bytes
    size: u32
}

pub type FunctionIndex = usize;
pub type FunctionBodyIndex = usize;
pub type VariableIndex = usize;
pub type TypeIndex = usize;

#[derive(Debug, Clone, Error)]
pub enum AnalysisError {
    #[error("Wrong node type used, exepected {0}, got {1:?}")]
    WrongNodeType(&'static str, Node),
    #[error("Redefinition of {0} {1}")]
    Redefinition(&'static str, String),
    #[error("Unknown {0} {1}")]
    Unknown(&'static str, String),
    #[error("Expected type {1}, got {2}: {0}")]
    MismatchedType(&'static str, String, String)
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

        let return_type = app.get_type(&return_type)?;

        let mut variables = IndexMap::new();
        let mut arguments = Vec::new();

        for v in parameter_list {
            let Node::Definition { name, typename } = v else { return Err(AnalysisError::WrongNodeType("An argument definition", v)) };

            let (idx, _) = variables.insert_full(name, app.get_type(&typename)?);
            arguments.push(idx);
        }

        let Node::Block(body) = *body else { return Err(AnalysisError::WrongNodeType("A function body", *body)) };

        for expr in &body {
            if let Node::Expr { lhs, .. } = expr {
                if let Node::Definition { name, typename } = &**lhs {
                    if !variables.contains_key(name) {
                        variables.insert(name.clone(), app.get_type(typename)?);
                    } else {
                        return Err(AnalysisError::Redefinition("variable", name.clone()));
                    }
                }
            }
        }

        let body = app.function_bodies.push_idx(FunctionBody{ body, definition: 0 });

        let (definition, _) = app.function_definitions.insert_full(
            name,
            Self {
                return_type,
                variables,
                arguments,
                body,
            },
        );

        app.function_bodies[body].definition = definition;

        Ok(())
    }
}

impl FunctionBody {
    fn get_type(&self, app: &App, node: &Node) -> Result<TypeIndex> {
        match node {
            Node::Number(_) => app.get_type("i32"),
            Node::StringLiteral(_) => app.get_type("str"),
            Node::Identifier(name) | Node::Definition { name, .. } => {
                app.function_definitions[self.definition]
                    .variables.get(name).copied()
                    .ok_or(AnalysisError::Unknown("variable", name.to_owned()))
            },
            Node::Block(nodes) => {
                if let Some(node) = nodes.last() {
                    self.get_type(app, node)
                }
                else { app.get_type("void") }
            },
            Node::Intrisic(_intrisic) => {
                Ok(app.get_type("void")?)
            }
            Node::Call { name, parameter_list, .. } => {
                let func = app.function_definitions.get(name)
                    .ok_or(AnalysisError::Unknown("function", name.clone()))?;

                for (arg, param) in func.arguments.iter().copied().map(|a| func.variables[a]) // map argument index into type index
                    .zip(parameter_list.iter())
                {
                    let param = self.get_type(app, param)?;
                    if param != arg {
                        return Err(AnalysisError::MismatchedType("wrong argument to function", app.get_type_name(arg).to_owned(), app.get_type_name(param).to_owned()))
                    }
                }

                Ok(func.return_type)
            },
            Node::Expr { lhs, rhs, op: Operation::Assignment } => {
                if !matches!(**lhs, Node::Identifier(_) | Node::Definition { .. }) { 
                    return Err(AnalysisError::WrongNodeType("a variable definition", *lhs.clone())) 
                }
                let expected_type = self.get_type(app, &**lhs)?;
                let rhs_type = self.get_type(app, &**rhs)?;

                if rhs_type == expected_type {
                    app.get_type("void")
                }
                else {
                    Err(AnalysisError::MismatchedType("cannot assign value to variable", app.get_type_name(expected_type).to_owned(), app.get_type_name(rhs_type).to_owned()))
                }
            },
            Node::Expr { lhs, rhs, op: _ } => {
                let i32_type = app.get_type("i32")?;
                let lhs = self.get_type(app, &**lhs)?;
                let rhs = self.get_type(app, &**rhs)?;

                if lhs == i32_type && rhs == i32_type {
                    Ok(i32_type)
                }
                else {
                    Err(AnalysisError::MismatchedType("math operations only apply to numbers", app.get_type_name(lhs).to_owned(), app.get_type_name(rhs).to_owned()))
                }
            },
            Node::FuncDef { .. } => {
                Err(AnalysisError::WrongNodeType("something that isn't a function definition what the fuck", node.clone()))
            }
        }
    }

    fn type_check(&self, app: &App) -> Result<()> {
        for line in self.iter() {
            self.get_type(app, line)?;
        }
        Ok(())
    }
}

impl App {
    fn get_type(&self, name: &str) -> Result<TypeIndex> {
        self.types.get_index_of(name).ok_or(AnalysisError::Unknown("type", name.to_owned()))
    }

    /// Assume the type index exists in the map
    fn get_type_name(&self, idx: TypeIndex) -> &str {
        self.types.get_index(idx).unwrap().0
    }

    pub fn new() -> Self {
        Self {
            function_definitions: IndexMap::new(),
            function_bodies: Vec::new(),
            types: indexmap!{
                "i32".to_owned() => Type { size: 4 },
                "f32".to_owned() => Type { size: 4 },
                "str".to_owned() => Type { size: 8 },
                "void".to_owned() => Type { size: 0 }
            },
        }
    }

    pub fn insert_declarations(&mut self, root: Node) -> Result<()> {
        let Node::Block(root) = root else { Err(AnalysisError::WrongNodeType("A list of top-level statements", root.clone()))? };

        for statement in root {
            if Function::is_declaration(&statement) {
                Function::insert(self, statement)?;
            }
        }

        Ok(())
    }

    pub fn type_check(&self) -> Result<()> {
        for function in &self.function_bodies {
            function.type_check(self)?;
        }
        Ok(())
    }
}
