use std::{ops::Not, fmt::Display};

use indexmap::IndexMap;
use itertools::Itertools;
use thiserror::Error;
use derive_more::{Deref, DerefMut};

use crate::{ast::{Node, Intrisic, BinaryOperation, UnaryOperation, Ast}, utility::{PushIndex, Transmit}, typed::{TypeError, Type, SuperType, TypeDescriptor}, super_type_or, error::{Result, ToCompilerError}};

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
    pub parameters: Vec<VariableIndex>,
    pub body: FunctionBodyIndex,
}

#[derive(Debug, Deref, DerefMut)]
pub struct FunctionBody {
    #[deref]
    #[deref_mut]
    pub body: Vec<Ast>,
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
}

impl Function {
    fn is_declaration(node: &Node) -> bool {
        matches!(node, Node::FuncDef { .. })
    }

    /// Inserts a function declaration and body into an app
    fn insert(app: &mut App, definition: Ast) -> Result<()> {
        let Node::FuncDef { name, return_type, parameter_list, box body } = definition.node
            else { return Err(AnalysisError::WrongNodeType("no function definition given", definition.node)).at(definition.bounds) };

        let mut variables = IndexMap::new();
        let mut parameters = Vec::new();

        for v in parameter_list {
            let Node::Definition { name, typename } = v.node
                else { return Err(AnalysisError::WrongNodeType("an argument definition", v.node)).at(v.bounds) };

            let (idx, _) = variables.insert_full(name, typename);
            parameters.push(idx);
        }

        let Node::Block(body, _) = body.node 
            else { return Err(AnalysisError::WrongNodeType("a function body", body.node)).at(body.bounds) };

        fn get_variable_definitions(variables: &mut IndexMap<String, Type>, ast: &Ast) -> Result<()> {
            match &ast.node {
                Node::Definition { name, typename } => {
                    if !variables.contains_key(name) {
                        variables.insert(name.clone(), typename.clone());
                    } else {
                        return Err(AnalysisError::Redefinition("variable", name.clone())).at_ast(ast);
                    }
                }
                Node::UnaryExpr { value, .. } => {
                    get_variable_definitions(variables, value)?;
                }
                Node::Expr { lhs, rhs, .. } => {
                    get_variable_definitions(variables, lhs)?;
                    get_variable_definitions(variables, rhs)?;
                }
                Node::Statement(box expr) | Node::Convert(box expr, _) => {
                    get_variable_definitions(variables, expr)?;
                }
                Node::Block(body, _) | Node::Call { argument_list: body, .. } => {
                    for expr in body {
                        get_variable_definitions(variables, expr)?;
                    }
                }
                Node::If { body, .. } | Node::Loop { body } | Node::Return(body) => {
                    get_variable_definitions(variables, body)?;
                }
                Node::FuncDef { .. } | Node::Break | Node::Intrisic(_) | Node::Identifier(..) | Node::Number(..) | Node::StringLiteral(_) | Node::BoolLiteral(_) | Node::Empty => ()
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
                parameters,
                body,
            },
        );

        app.function_bodies[body].definition = definition;

        Ok(())
    }
}

impl Ast {
    /// Returns the type associated with this node by recursively computing it
    ///
    /// * `app`: Global application state (functions and types)
    /// * `definition`: Definition of the current function
    /// * `expect`: Type that's expected from the node, for simple coercions
    pub fn set_type(&mut self, app: &App, definition: &Function, expect: Option<&Type>) -> Result<TypeDescriptor> {
        match &mut self.node {
            Node::Number(value, ty) => {
                *ty = Type::Int32;
                if let Some(expect) = expect {
                    if SuperType::Integer.verify(expect) || (SuperType::Ptr.verify(expect) && *value == 0) {
                        *ty = expect.clone();
                    }
                }

                if *value < 2_i64.saturating_pow(ty.size()*8) {
                    Ok(ty.clone().into())
                } else {
                    Err(AnalysisError::NumberTooBig(*value, ty.clone())).at_ast(self)
                }
            }
            Node::StringLiteral(_) => Ok(Type::string().into()),
            Node::BoolLiteral(_) => Ok(Type::Boolean.into()),
            Node::Identifier(name, typename) => {
                *typename = definition
                    .variables.get(name).cloned()
                    .ok_or(AnalysisError::Unknown("variable", name.to_owned()))
                    .at(self.bounds)?;

                Ok(typename.clone().addressable())
            }
            Node::Definition { name: _, typename } => Ok(typename.clone().addressable()),
            Node::Intrisic(intrisic) => {
                match intrisic {
                    Intrisic::Asm(box x) => {
                        if matches!(x.node, Node::StringLiteral(_)).not() {
                            return Err(AnalysisError::WrongNodeType("a compile-time string literal", x.node.clone()))
                                .at_ast(x);
                        }
                    },
                    Intrisic::Print(args) => {
                        for (bounds, ty) in args.iter_mut().map(|node| (node.bounds, node.set_type(app, definition, None))) {
                            let ty = ty?;
                            ty.expect_ref(
                                super_type_or!(Type::string(), SuperType::Integer),
                                "print intrisic cannot format the given type"
                            ).at(bounds)?;
                        }
                    }
                }

                Ok(Type::Void.into())
            }
            Node::Call { name, argument_list, return_type } => {
                let func = app.function_definitions.get(name)
                    .ok_or(AnalysisError::Unknown("function", name.clone()))
                    .at(self.bounds)?;

                if func.parameters.len() != argument_list.len() {
                    return Err(AnalysisError::WrongArgumentNumber(name.clone(), func.parameters.len(), argument_list.len()))
                        .at_ast(self)
                }

                for (param, arg) in func.parameters.iter().map(|&a| func.variables[a].clone()) // map argument index into type index
                    .zip(argument_list.iter_mut())
                {
                    let arg_type = arg.set_type(app, definition, Some(&param))?;
                    arg_type.ty.expect_ref(param.into(), "wrong argument type to function").at_ast(arg)?;
                }

                *return_type = func.return_type.clone();
                Ok(func.return_type.clone().into())
            }
            Node::UnaryExpr { op, ty, value } => {
                let value = value.set_type(app, definition, expect)?;

                use UnaryOperation::*;
                let descriptor = match op {
                    LogicalNot => value.expect(Type::Boolean.into(), "logical operations only apply to booleans"),
                    Negation => value.expect(SuperType::Number, "math operations only operate on numbers"),
                    Deref => match value.ty {
                        Type::Ptr(box t) => Ok(t.addressable()),
                        value => value.expect(SuperType::Ptr, "dereference takes a pointer").transmit()
                    }
                    AddressOf => match value.has_address {
                        true => Ok(value.ty.into_ptr().into()),
                        false => Err(TypeError::InvalidOperation("can't take address of r-value", value.ty))
                    }
                }.at(self.bounds)?;
                *ty = descriptor.ty.clone();

                Ok(descriptor)
            }
            Node::Expr { op: BinaryOperation::Assignment, ty: _, box lhs, box rhs } => {
                match &lhs.node {
                    Node::Identifier(..) | Node::Definition { .. } | Node::UnaryExpr { op: UnaryOperation::Deref, .. } => (),
                    node => {
                        return Err(AnalysisError::WrongNodeType("given node isn't assignable to", node.clone())).at_ast(lhs);
                    }
                }

                let expected_type = lhs.set_type(app, definition, None)?;
                let rhs_type = rhs.set_type(app, definition, Some(&expected_type.ty))?;

                rhs_type.expect(expected_type.ty.into(), "cannot assign value to variable").at_ast(rhs)?;
                Ok(Type::Void.into())
            },
            Node::Expr { op, ty, box lhs, box rhs } => {
                use BinaryOperation::*;
                let descriptor = match op {
                    Add | Sub | Div | Mul | Modulus => {
                        let left = lhs.set_type(app, definition, expect)?;
                        let right = rhs.set_type(app, definition, expect)?;

                        left.expect_ref(super_type_or!(SuperType::Number, SuperType::Ptr), "math operations only apply to numbers").at_ast(lhs)?;
                        right.expect_ref(super_type_or!(SuperType::Number, SuperType::Ptr), "math operations only apply to numbers").at_ast(rhs)?;

                        if left.ty.size() != right.ty.size() {
                            return Err(TypeError::MismatchedSize("cannot operate on operands of different sizes", left.ty, right.ty).at_ast(self));
                        }

                        let ptr = SuperType::Ptr;

                        if ptr.verify(&left.ty) { left }
                        else if ptr.verify(&right.ty) { right }
                        else { left }
                        
                    }
                    Equals | NotEquals | Greater | GreaterOrEquals | Lesser | LesserOrEquals => {
                        let left = lhs.set_type(app, definition, None)?;
                        let right = rhs.set_type(app, definition, Some(&left.ty))?;

                        right.expect(left.ty.into(), "cannot compare different types").at(self.bounds)?;
                        Type::Boolean.into()
                    }
                    LogicalAnd | LogicalOr | LogicalXor => {
                        let left = lhs.set_type(app, definition, Some(&Type::Boolean))?;
                        let right = rhs.set_type(app, definition, Some(&Type::Boolean))?;

                        left.expect(Type::Boolean.into(), "logical operations only apply to booleans").at_ast(lhs)?;
                        right.expect(Type::Boolean.into(), "logical operations only apply to booleans").at_ast(rhs)?
                    }
                    Assignment => unreachable!()
                };
                *ty = descriptor.ty.clone();

                Ok(descriptor)
            }
            Node::Convert(box expr, ty) => {
                let expr_type = expr.set_type(app, definition, Some(ty))?;

                let output = ty.clone(); // Weird ownership shit?

                expr_type.ty.is_convertible_to(ty).at_ast(self)?;

                Ok(output.into())
            }
            Node::If { box condition, box body, else_body } => {
                let cond_type = condition.set_type(app, definition, None)?;
                cond_type.expect(Type::Boolean.into(), "if statement condition needs to be boolean").at_ast(condition)?;

                body.set_type(app, definition, None)?;
                if let Some(box else_body) = else_body {
                    else_body.set_type(app, definition, None)?;
                }
                Ok(Type::Void.into())
            }
            Node::Loop { box body } => {
                body.set_type(app, definition, None)?;
                Ok(Type::Void.into())
            }
            Node::Break => Ok(Type::Void.into()),
            Node::Return( box expr ) => {
                let out = expr.set_type(app, definition, Some(&definition.return_type))?;
                out.expect(
                    definition.return_type.clone().into(),
                    "return type doesn't correspond to the given function"
                ).at_ast(expr)?;

                Ok(Type::Void.into())
            }
            Node::Block(body, ty) => {
                let mut body = body.iter_mut().peekable();
                while let Some(node) = body.next() {
                    if body.peek().is_none() {
                        *ty = node.set_type(app, definition, expect)?.ty;
                    }
                    node.set_type(app, definition, None)?;
                }

                Ok(ty.clone().into()) 
            }
            Node::Statement( inner ) => {
                inner.set_type(app, definition, None)?;
                Ok(Type::Void.into())
            }
            Node::Empty => Ok(Type::Void.into()),
            Node::FuncDef { .. } => {
                Err(AnalysisError::WrongNodeType("something that isn't a function definition what the fuck", self.node.clone())).at_ast(self)
            }
        }
    }

    pub fn get_type(&self) -> Type {
        match &self.node {
            Node::Call { return_type, .. } => return_type,
            Node::UnaryExpr { ty, .. } => ty,
            Node::Expr { ty, .. } => ty,
            Node::Block(_, ty) => ty,
            Node::Convert(_, ty) => ty,
            Node::Number(_, ty) => ty,
            Node::Identifier(_, ty) => ty,
            Node::BoolLiteral(_) => &Type::Boolean,
            Node::StringLiteral(_) => return Type::string(),
            Node::Statement(..) | Node::FuncDef { .. } | Node::Loop { .. } | Node::If { .. }
                | Node::Return(..) | Node::Break | Node::Intrisic(..) | Node::Empty
                | Node::Definition { .. } => &Type::Void
        }.clone()
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

    pub fn insert_declarations(&mut self, root: Ast) -> Result<()> {
        let Node::Block(root, _) = root.node
            else { return Err(AnalysisError::WrongNodeType("A list of top-level statements", root.node.clone())).at_ast(&root) };

        for statement in root {
            if Function::is_declaration(&statement.node) {
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

impl Display for App {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const WHITE: &str = "\x1b[0m";
        const CYAN: &str = "\x1b[36m";
        const GRAY: &str = "\x1b[90m";

        for (name, func) in &self.function_definitions {
            writeln!(f, "FUNC {name}")?;

            if func.return_type != Type::Void {
                writeln!(f, "{GRAY}├ {WHITE}RETURNS -> {CYAN}{:?}", func.return_type)?;
            }
            writeln!(f, "{GRAY}├ {WHITE}VARIABLES")?;
            for (idx, (name, ty)) in func.variables.iter().enumerate() {
                let is_last = idx == func.variables.len()-1;
                let connector = if is_last { '╰' } else { '├' };

                let is_argument = if func.parameters.contains(&idx) {
                    "(ARGUMENT)"
                } else { "" };

                writeln!(f, "{GRAY}│ {connector} {WHITE}{name} -> {CYAN}{ty:?} {is_argument}")?;
            }
            writeln!(f, "{GRAY}╰ {WHITE}BODY")?;
            let body = self.function_bodies[func.body]
                .body.iter()
                .map(|x| format!("{x}"))
                .collect_vec();

            let body_len = body.len();
            for (idx, arm) in body.into_iter().enumerate() {
                let is_last = idx == body_len-1;

                let mut it = arm.split_inclusive('\n');
                let first = it.next().unwrap();

                let connector = if is_last { '╰' } else { '├' };
                let separator = if is_last { ' ' } else { '│' };

                write!(f, "  {GRAY}{connector} {WHITE}{first}")?;
                for line in it {
                    write!(f, "  {GRAY}{separator} {WHITE}{line}")?;
                }
            }

            writeln!(f)?;
        }

        Ok(())
    }
}
