use indexmap::IndexMap;
use thiserror::Error;
use derive_more::{Deref, DerefMut};

use crate::{ast::{Node, Intrisic, BinaryOperation, UnaryOperation}, utility::{PushIndex, Transmit}, typed::{TypeError, Type, SuperType}, super_type_or};

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
    #[error("Wrong number of arguments given to function {0}: expected {1}, got {2}.")]
    WrongArgumentNumber(String, usize, usize),
    #[error("Number {0} too big for storage in {1:?}.")]
    NumberTooBig(i64, Type),
    #[error("{0}")]
    Type(TypeError)
}

impl From<TypeError> for AnalysisError {
    fn from(value: TypeError) -> Self {
        AnalysisError::Type(value)
    }
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

        let mut variables = IndexMap::new();
        let mut arguments = Vec::new();

        for v in parameter_list {
            let Node::Definition { name, typename } = v else { return Err(AnalysisError::WrongNodeType("An argument definition", v)) };

            let (idx, _) = variables.insert_full(name, typename);
            arguments.push(idx);
        }

        let Node::Block(body, _) = *body else { return Err(AnalysisError::WrongNodeType("A function body", *body)) };

        fn get_variable_definitions(variables: &mut IndexMap<String, Type>, node: &Node) -> Result<()> {
            match node {
                Node::Definition { name, typename } => {
                    if !variables.contains_key(name) {
                        variables.insert(name.clone(), typename.clone());
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
                Node::Block(body, _) | Node::Call { parameter_list: body, .. } => {
                    for expr in body {
                        get_variable_definitions(variables, expr)?;
                    }
                }
                Node::If { body, .. } | Node::Loop { body } | Node::Return(body) => {
                    get_variable_definitions(variables, body)?;
                }
                Node::FuncDef { .. } | Node::Break | Node::Intrisic(_) | Node::Identifier(..) | Node::Number(..) | Node::StringLiteral(_) | Node::Empty => ()
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
    /// * `expect`: Type that's expected from the node, for simple coercions
    pub fn set_type(&mut self, app: &App, definition: &Function, expect: Option<&Type>) -> Result<Type> {
        match self {
            Node::Number(value, ty) => {
                *ty = Type::Integer32;
                if let Some(expect) = expect {
                    if SuperType::Integer.verify(expect) || (SuperType::Ptr.verify(expect) && *value == 0) {
                        *ty = expect.clone();
                    }
                }

                if *value < 2_i64.saturating_pow(ty.size()*8) {
                    Ok(ty.clone())
                } else {
                    Err(AnalysisError::NumberTooBig(*value, ty.clone()))
                }
            },
            Node::StringLiteral(_) => Ok(Type::String),
            Node::Identifier(name, typename) => {
                *typename = definition
                    .variables.get(name).cloned()
                    .ok_or(AnalysisError::Unknown("variable", name.to_owned()))?;
                Ok(typename.clone())
            }
            Node::Definition { name, typename } => Ok(typename.clone()),
            Node::Intrisic(intrisic) => {
                match intrisic {
                    Intrisic::Asm(x) => {
                        if !matches!(&**x, Node::StringLiteral(_)) {
                            return Err(AnalysisError::WrongNodeType("a compile-time string literal", (**x).clone()));
                        }
                    },
                    Intrisic::Print(args) => {
                        for ty in args.iter_mut().map(|node| node.set_type(app, definition, None)) {
                            let ty = ty?;
                            ty.expect(
                                super_type_or!(Type::String, SuperType::Integer),
                                "print intrisic cannot format the given type"
                            )?;
                        }
                    }
                }

                Ok(Type::Void)
            }
            Node::Call { name, parameter_list, return_type } => {
                let func = app.function_definitions.get(name)
                    .ok_or(AnalysisError::Unknown("function", name.clone()))?;

                if func.arguments.len() != parameter_list.len() {
                    return Err(AnalysisError::WrongArgumentNumber(name.clone(), func.arguments.len(), parameter_list.len()))
                }

                for (param, arg) in func.arguments.iter().map(|&a| func.variables[a].clone()) // map argument index into type index
                    .zip(parameter_list.iter_mut())
                {
                    let arg = arg.set_type(app, definition, Some(&param))?;
                    arg.expect(param.into(), "wrong argument type to function")?;
                }

                *return_type = func.return_type.clone();
                Ok(func.return_type.clone())
            }
            Node::UnaryExpr { op, ty, value } => {
                let value = value.set_type(app, definition, expect)?;

                use UnaryOperation::*;
                *ty = match op {
                    LogicalNot => value.expect(Type::Boolean.into(), "logical operations only apply to booleans"),
                    Negation => value.expect(SuperType::Number, "math operations only operate on numbers"),
                    Deref => match value {
                        Type::Ptr(t) => Ok(*t),
                        value => value.expect(SuperType::Ptr, "dereference takes a pointer")
                    }
                }?;
                
                Ok(ty.clone())
            }
            Node::Expr { op: BinaryOperation::Assignment, ty: _, lhs, rhs } => {
                match &**lhs {
                    Node::Identifier(..) | Node::Definition { .. } | Node::UnaryExpr { op: UnaryOperation::Deref, .. } => (),
                    _ => {
                        Err(AnalysisError::WrongNodeType("given node isn't assignable to", (**lhs).clone()))?;
                    }
                }

                let expected_type = lhs.set_type(app, definition, None)?;
                let rhs_type = rhs.set_type(app, definition, Some(&expected_type))?;

                rhs_type.expect(expected_type.into(), "cannot assign value to variable")?;
                Ok(Type::Void)
            },
            Node::Expr { op, ty, lhs, rhs } => {
                let lhs = lhs.set_type(app, definition, expect)?;
                let rhs = rhs.set_type(app, definition, expect)?;

                use BinaryOperation::*;
                *ty = match op {
                    Add | Sub | Div | Mul | Modulus => {
                        lhs.expect_ref(super_type_or!(SuperType::Number, SuperType::Ptr), "math operations only apply to numbers")?;
                        rhs.expect_ref(super_type_or!(SuperType::Number, SuperType::Ptr), "math operations only apply to numbers")?;

                        let ptr = SuperType::Ptr;
                        Ok(
                            if ptr.verify(&lhs) { lhs }
                            else if ptr.verify(&rhs) { rhs }
                            else { [ lhs, rhs ].into_iter().max_by_key(|x| x.size()).unwrap() }
                        )
                    }
                    Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => {
                        rhs.expect(lhs.into(), "cannot compare different types")?;
                        Ok(Type::Boolean)
                    }
                    LogicalAnd | LogicalOr | LogicalXor => {
                        lhs.expect(Type::Boolean.into(), "logical operations only apply to booleans")?;
                        rhs.expect(Type::Boolean.into(), "logical operations only apply to booleans")
                    }
                    Assignment => unreachable!()
                }?;

                Ok(ty.clone())
            }
            Node::If { condition, body } => {
                let condition = condition.set_type(app, definition, None)?;
                condition.expect(Type::Boolean.into(), "if statement condition needs to be boolean")?;

                body.set_type(app, definition, None)?;
                Ok(Type::Void)
            }
            Node::Loop { body } => {
                body.set_type(app, definition, None)?;
                Ok(Type::Void)
            }
            Node::Break => Ok(Type::Void),
            Node::Return( expr ) => {
                let out = expr.set_type(app, definition, Some(&definition.return_type))?;
                out.expect(definition.return_type.clone().into(), "return type doesn't correspond to the given function")?;

                Ok(Type::Void)
            }
            Node::Block(body, ty) => {
                let mut body = body.iter_mut().peekable();
                while let Some(node) = body.next() {
                    if body.peek().is_none() {
                        *ty = node.set_type(app, definition, expect)?;
                    }
                    node.set_type(app, definition, None)?;
                }

                Ok(ty.clone()) 
            }
            Node::Statement( inner ) => {
                inner.set_type(app, definition, None)?;
                Ok(Type::Void)
            }
            Node::Empty => Ok(Type::Void),
            Node::FuncDef { .. } => {
                Err(AnalysisError::WrongNodeType("something that isn't a function definition what the fuck", self.clone()))
            }
        }
    }

    pub fn get_type(&self) -> &Type {
        match self {
            Node::Call { return_type, .. } => return_type,
            Node::UnaryExpr { ty, .. } => ty,
            Node::Expr { ty, .. } => ty,
            Node::Block(_, ty) => ty,
            Node::Number(_, ty) => ty,
            Node::StringLiteral(_) => &Type::String,
            Node::Identifier(_, ty) => ty,
            _ => &Type::Void
        }
    }
}

impl FunctionBody {
    fn type_check(&mut self, app: &App) -> Result<()> {
        for statement in self.body.iter_mut() {
            statement.set_type(app, &app.function_definitions[self.definition], None)?;
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
        let Node::Block(root, _) = root else { Err(AnalysisError::WrongNodeType("A list of top-level statements", root.clone()))? };

        for statement in root {
            if Function::is_declaration(&statement) {
                Function::insert(self, statement)?;
            }
        }

        Ok(())
    }

    pub fn type_check(mut self) -> Result<Self> {
        // We do a little trickery
        // Allow borrowing the function definitions without borrowing self
        // We don't need the bodies during type checking anyway
        let this = App {
            function_definitions: self.function_definitions,
            function_bodies: Vec::new()
        };

        for function in &mut self.function_bodies {
            function.type_check(&this)?;
        }

        self.function_definitions = this.function_definitions;
        Ok(self)
    }
}
