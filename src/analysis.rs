use indexmap::IndexMap;
use thiserror::Error;
use derive_more::{Deref, DerefMut};

use crate::{ast::{Node, Intrisic, BinaryOperation, UnaryOperation}, utility::PushIndex};

#[derive(Debug)]
pub struct App {
    pub function_definitions: IndexMap<String, Function>,
    pub function_bodies: Vec<FunctionBody>,
    // pub types: IndexMap<String, Type>,
}

#[derive(Debug)]
pub struct Function {
    pub return_type: Type,
    pub variables: IndexMap<String, Type>,
    pub arguments: Vec<VariableIndex>,
    pub body: FunctionBodyIndex,
}

#[derive(Debug, Deref, DerefMut)]
pub struct FunctionBody {
    #[deref]
    #[deref_mut]
    pub body: Vec<Node>,
    pub definition: FunctionIndex
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Integer32,
    Float32,
    Boolean,
    String,
    Void,
    // For later :)
    // Struct(TypeIndex)
}

pub type FunctionIndex = usize;
pub type FunctionBodyIndex = usize;
pub type VariableIndex = usize;
// pub type TypeIndex = usize;

#[derive(Debug, Clone, Error)]
pub enum AnalysisError {
    #[error("Wrong node type used, exepected {0}, got {1:?}")]
    WrongNodeType(&'static str, Node),
    #[error("Redefinition of {0} {1}")]
    Redefinition(&'static str, String),
    #[error("Unknown {0} {1}")]
    Unknown(&'static str, String),
    #[error("Expected type {1:?}, got {2:?}: {0}")]
    MismatchedType(&'static str, Type, Type),
    #[error("Wrong number of arguments given to function {0}: expected {1}, got {2}.")]
    WrongArgumentNumber(String, usize, usize)
}

pub type Result<T> = std::result::Result<T, AnalysisError>;

impl Type {
    /// Size of the type in bytes
    pub fn size(&self) -> u32 {
        use Type::*;
        match self {
            Integer32 => 4,
            Float32 => 4,
            Boolean => 1,
            String => 8,
            Void => 0
        }
    }

    pub fn from_str(name: &str) -> Result<Self> {
        use Type::*;
        match name {
            "i32" => Ok(Integer32),
            "f32" => Ok(Float32),
            "bool" => Ok(Boolean),
            "str" => Ok(String),
            "void" => Ok(Void),
            _ => Err(AnalysisError::Unknown("type", name.to_owned()))
        }
    }

    pub fn expect(self, expected: Self, msg: &'static str) -> Result<Self> {
        if self == expected {
            Ok(self)
        } else {
            Err(AnalysisError::MismatchedType(msg, expected, self))
        }
    }
}

impl Function {
    fn is_declaration(node: &Node) -> bool {
        matches!(node, Node::FuncDef { .. })
    }

    /// Inserts a function declaration and body into an app
    fn insert(app: &mut App, node: Node) -> Result<()> {
        let Node::FuncDef { name, return_type, parameter_list, body } = node
            else { return Err(AnalysisError::WrongNodeType("No function definition given...", node)) };

        let return_type = Type::from_str(&return_type)?;

        let mut variables = IndexMap::new();
        let mut arguments = Vec::new();

        for v in parameter_list {
            let Node::Definition { name, typename } = v else { return Err(AnalysisError::WrongNodeType("An argument definition", v)) };

            let (idx, _) = variables.insert_full(name, Type::from_str(&typename)?);
            arguments.push(idx);
        }

        let Node::Block(body) = *body else { return Err(AnalysisError::WrongNodeType("A function body", *body)) };

        fn get_variable_definitions(variables: &mut IndexMap<String, Type>, node: &Node) -> Result<()> {
            match node {
                Node::Definition { name, typename } => {
                    if !variables.contains_key(name) {
                        variables.insert(name.clone(), Type::from_str(typename)?);
                    } else {
                        return Err(AnalysisError::Redefinition("variable", name.clone()));
                    }
                }
                Node::UnaryExpr { value, .. } => {
                    get_variable_definitions(variables, value)?;
                }
                Node::Expr { lhs, rhs, .. } => {
                    get_variable_definitions(variables, lhs)?;
                    get_variable_definitions(variables, rhs)?;
                }
                Node::Statement(x) => {
                    get_variable_definitions(variables, x)?;
                }
                Node::Block(body) => {
                    for expr in body {
                        get_variable_definitions(variables, expr)?;
                    }
                }
                Node::If { body, .. } | Node::Loop { body } => {
                    get_variable_definitions(variables, body)?;
                }
                Node::FuncDef { .. } | Node::Call { .. } | Node::Break | Node::Intrisic(_) | Node::Identifier(_) | Node::Number(_) | Node::StringLiteral(_) => ()
            };
            Ok(())
        }

        for expr in &body {
            get_variable_definitions(&mut variables, expr)?;
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

impl Node {
    /// Returns the type associated with this node by recursively computing it
    ///
    /// * `app`: Global application state (functions and types)
    /// * `definition`: Definition of the current function
    pub fn get_type(&self, app: &App, definition: &Function) -> Result<Type> {
        match self {
            Node::Number(_) => Ok(Type::Integer32),
            Node::StringLiteral(_) => Ok(Type::String),
            Node::Identifier(name) | Node::Definition { name, .. } => {
                definition
                    .variables.get(name).copied()
                    .ok_or(AnalysisError::Unknown("variable", name.to_owned()))
            }
            Node::Intrisic(intrisic) => {
                match intrisic {
                    Intrisic::Asm(x) => {
                        if !matches!(&**x, Node::StringLiteral(_)) {
                            return Err(AnalysisError::WrongNodeType("a compile-time string literal", (**x).clone()));
                        }
                    },
                    Intrisic::Print(args) => {
                        for ty in args.iter().map(|node| node.get_type(app, definition)) {
                            let ty = ty?;
                            if !matches!(ty, Type::String | Type::Integer32) {
                                return Err(AnalysisError::MismatchedType("print intrisic cannot format the given type (expect str or i32)", Type::String, ty));
                            }
                        }
                    }
                }

                Ok(Type::Void)
            }
            Node::Call { name, parameter_list, .. } => {
                let func = app.function_definitions.get(name)
                    .ok_or(AnalysisError::Unknown("function", name.clone()))?;

                if func.arguments.len() != parameter_list.len() {
                    return Err(AnalysisError::WrongArgumentNumber(name.clone(), func.arguments.len(), parameter_list.len()))
                }

                for (param, arg) in func.arguments.iter().map(|&a| func.variables[a]) // map argument index into type index
                    .zip(parameter_list.iter())
                {
                    let arg = arg.get_type(app, definition)?;
                    if arg != param {
                        return Err(AnalysisError::MismatchedType("wrong argument type to function", param, arg));
                    }
                }

                Ok(func.return_type)
            }
            Node::UnaryExpr { op, value } => {
                let value = value.get_type(app, definition)?;
                use UnaryOperation::*;
                match op {
                    LogicalNot => value.expect(Type::Boolean, "logical operations only apply to booleans")
                }
            }
            Node::Expr { lhs, rhs, op: BinaryOperation::Assignment } => {
                if !matches!(**lhs, Node::Identifier(_) | Node::Definition { .. }) { 
                    return Err(AnalysisError::WrongNodeType("a variable definition", *lhs.clone())) 
                }
                let expected_type = lhs.get_type(app, definition)?;
                let rhs_type = rhs.get_type(app, definition)?;

                if rhs_type == expected_type {
                    Ok(Type::Void)
                }
                else {
                    Err(AnalysisError::MismatchedType("cannot assign value to variable", expected_type, rhs_type))
                }
            },
            Node::Expr { lhs, rhs, op } => {
                let lhs = lhs.get_type(app, definition)?;
                let rhs = rhs.get_type(app, definition)?;

                use BinaryOperation::*;
                match op {
                    Add | Sub | Div | Mul | Modulus => {
                        lhs.expect(Type::Integer32, "math operations only apply to numbers")?;
                        rhs.expect(Type::Integer32, "math operations only apply to numbers")
                    }
                    Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => if lhs == rhs {
                        Ok(Type::Boolean)
                    } else {
                        Err(AnalysisError::MismatchedType("cannot compare different types", lhs, rhs))
                    }
                    LogicalAnd | LogicalOr | LogicalXor => {
                        lhs.expect(Type::Boolean, "logical operations only apply to booleans")?;
                        rhs.expect(Type::Boolean, "logical operations only apply to booleans")
                    }
                    Assignment => unreachable!()
                }
                
            }
            Node::If { condition, body } => {
                let condition = condition.get_type(app, definition)?;

                condition.expect(Type::Boolean, "if statement condition needs to be boolean")?;

                body.get_type(app, definition)
            }
            Node::Loop { body } => {
                body.get_type(app, definition)?;
                Ok(Type::Void)
            }
            Node::Break => Ok(Type::Void),
            Node::Block(nodes) => {
                let mut nodes = nodes.iter().peekable();
                while let Some(node) = nodes.next() {
                    if nodes.peek().is_none() {
                        return node.get_type(app, definition);
                    }
                    node.get_type(app, definition)?;
                }
                Ok(Type::Void) 
            }
            Node::Statement( inner ) => {
                inner.get_type(app, definition)?;
                Ok(Type::Void)
            }
            Node::FuncDef { .. } => {
                Err(AnalysisError::WrongNodeType("something that isn't a function definition what the fuck", self.clone()))
            }
        }
    }
}

impl FunctionBody {
    fn type_check(&self, app: &App) -> Result<()> {
        for statement in self.iter() {
            statement.get_type(app, &app.function_definitions[self.definition])?;
        }
        Ok(())
    }
}

impl App {
    // fn get_type(&self, name: &str) -> Result<TypeIndex> {
    //     self.types.get_index_of(name).ok_or(AnalysisError::Unknown("type", name.to_owned()))
    // }
    //
    // /// Assume the type index exists in the map
    // fn get_type_name(&self, idx: TypeIndex) -> &str {
    //     self.types.get_index(idx).unwrap().0
    // }

    pub fn new() -> Self {
        Self {
            function_definitions: IndexMap::new(),
            function_bodies: Vec::new(),
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
