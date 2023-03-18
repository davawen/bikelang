use std::{ops::Not, fmt::Display};

use indexmap::IndexMap;
use itertools::Itertools;
use thiserror::Error;
use derive_more::{Deref, DerefMut};

use crate::{utility::{PushIndex, Transmit}, typed::{TypeError, Type, SuperType, TypeDescriptor, TypeHolder}, super_type_or, error::{Result, ToCompilerError}, scope::{ScopeTrait, ScopeStack}, };
use super::{Ast, node::{Node, Intrisic, BinaryOperation, UnaryOperation}};

pub type FunctionIndex = usize;
pub type FunctionBodyIndex = usize;
pub type VariableIndex = usize;

#[derive(Debug)]
pub struct App {
    pub function_definitions: IndexMap<String, Function>,
    pub function_bodies: Vec<FunctionBody>,
    pub types: TypeHolder,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: FunctionBodyIndex,
}

#[derive(Debug, Deref, DerefMut)]
pub struct FunctionBody {
    #[deref]
    #[deref_mut]
    pub body: Vec<Ast>,
    pub definition: FunctionIndex
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub variables: IndexMap<String, Type>,
    deferred_variable: Option<(String, Type)>,
    deferred: bool
}

impl Scope {
    /// Don't directly add the next declaration
    fn defer(&mut self) {
        self.deferred = true;
    }

    /// Declare the deferred variable if any
    fn apply_defer(&mut self) {
        if self.deferred {
            self.deferred = false;
            if let Some((name, var)) = self.deferred_variable.take() {
                self.insert(name, var);
            }
        }
    }
}

impl ScopeTrait for Scope {
    type VariableType = Type;
    type Key = VariableIndex;

    fn get_variable(&self, name: &str) -> Option<&Self::VariableType> {
        self.variables.get(name)
    }

    /// # Panics
    /// This function panics if the scope is deferred, but a deferred variable is already set 
    /// Such as to not create invalid indices and to not lose a declaration
    /// NOTE: No `unsafe`, but pretty unsafe stuff is hapenning
    fn insert(&mut self, name: String, var: Self::VariableType) -> Self::Key {
        if !self.deferred {
            self.variables.insert_full(name, var).0
        } else {
            if self.deferred_variable.is_some() {
                panic!("Trying to insert a variable({name}, {var:?}) when another one is already deferred({:?})", self.deferred_variable)
            }

            self.deferred_variable = Some((name, var));
            // Index will be valid once deferred is applied
            // Although this mean an index cannot technically be considered valid after an insert
            // But it's fine in its usage and will panic instead of giving wrong results
            self.variables.len()
        }
    }

    fn get_index(&self, idx: Self::Key) -> &Self::VariableType {
        &self.variables[idx]
    }
}

#[derive(Debug, Clone, Error)]
pub enum AnalysisError {
    #[error("Wrong node type used, exepected {0}, got {1:?}")]
    WrongNodeType(&'static str, Node),
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

        let parameters = parameter_list.into_iter().map(|p| {
            if let Node::Definition { name, typename, ty: _ } = p.node {
                Ok((name, Type::from_node(&typename, &app.types).at(p.bounds)?))
            } else { 
                Err(AnalysisError::WrongNodeType("an argument definition", p.node)).at(p.bounds)
            }
        }).collect::<Result<_>>()?;

        let Node::Block { inner: body, ty: _ } = body.node 
            else { return Err(AnalysisError::WrongNodeType("a function body", body.node)).at(body.bounds) };

        let body = app.function_bodies.push_idx(FunctionBody{ body, definition: 0 });

        let (definition, _) = app.function_definitions.insert_full(
            name,
            Self {
                parameters,
                return_type: Type::from_node(&return_type, &app.types).at(definition.bounds)?,
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
    pub fn set_type(&mut self, app: &App, definition: &Function, scopes: &mut ScopeStack<Scope>, expect: Option<&Type>) -> Result<TypeDescriptor> {
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
                *typename = scopes.get(name).cloned()
                    .ok_or(AnalysisError::Unknown("variable", name.to_owned()))
                    .at(self.bounds)?;

                Ok(typename.clone().addressable())
            }
            // This place defines variables in the scope!
            Node::Definition { name, typename, ty } => {
                *ty = Type::from_node(typename, &app.types).at(self.bounds)?;

                scopes.insert(name.clone(), ty.clone());

                Ok(ty.clone().addressable())
            },
            Node::Intrisic(intrisic) => {
                match intrisic {
                    Intrisic::Asm(box x) => {
                        if matches!(x.node, Node::StringLiteral(_)).not() {
                            return Err(AnalysisError::WrongNodeType("a compile-time string literal", x.node.clone()))
                                .at_ast(x);
                        }
                    },
                    Intrisic::Print(args) => {
                        for (bounds, ty) in args.iter_mut().map(|node| (node.bounds, node.set_type(app, definition, scopes, None))) {
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

                let parameters = &func.parameters;
                if parameters.len() != argument_list.len() {
                    return Err(AnalysisError::WrongArgumentNumber(name.clone(), parameters.len(), argument_list.len()))
                        .at_ast(self)
                }

                for (param, arg) in parameters.iter().map(|(_, b)| b).cloned().zip(argument_list.iter_mut())
                {
                    let arg_type = arg.set_type(app, definition, scopes, Some(&param))?;
                    arg_type.ty.expect_ref(param.into(), "wrong argument type to function").at_ast(arg)?;
                }

                *return_type = func.return_type.clone();
                Ok(func.return_type.clone().into())
            }
            Node::UnaryExpr { op, ty, value } => {
                let value = value.set_type(app, definition, scopes, expect)?;

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
                scopes.top_mut().defer();

                let expected_type = lhs.set_type(app, definition, scopes, None)?;
                let rhs_type = rhs.set_type(app, definition, scopes, Some(&expected_type.ty))?;

                scopes.top_mut().apply_defer();
                
                // Parse definition after expression
                // Can't assign to r-value
                if !expected_type.has_address {
                    return Err(AnalysisError::WrongNodeType("given node isn't assignable to", lhs.node.clone())).at_ast(lhs);
                }

                rhs_type.expect(expected_type.ty.into(), "cannot assign value to variable").at_ast(rhs)?;
                Ok(Type::Void.into())
            },
            Node::Expr { op, ty, box lhs, box rhs } => {
                use BinaryOperation::*;
                let descriptor = match op {
                    Add | Sub | Div | Mul | Modulus => {
                        let left = lhs.set_type(app, definition, scopes, expect)?;
                        let right = rhs.set_type(app, definition, scopes, expect)?;

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
                        let left = lhs.set_type(app, definition, scopes, None)?;
                        let right = rhs.set_type(app, definition, scopes, Some(&left.ty))?;

                        right.expect(left.ty.into(), "cannot compare different types").at(self.bounds)?;
                        Type::Boolean.into()
                    }
                    LogicalAnd | LogicalOr | LogicalXor => {
                        let left = lhs.set_type(app, definition, scopes, Some(&Type::Boolean))?;
                        let right = rhs.set_type(app, definition, scopes, Some(&Type::Boolean))?;

                        left.expect(Type::Boolean.into(), "logical operations only apply to booleans").at_ast(lhs)?;
                        right.expect(Type::Boolean.into(), "logical operations only apply to booleans").at_ast(rhs)?
                    }
                    Assignment => unreachable!()
                };
                *ty = descriptor.ty.clone();

                Ok(descriptor)
            }
            Node::Convert(box expr, typename, ty) => {
                *ty = Type::from_node(typename, &app.types).at(self.bounds)?;

                let expr_type = expr.set_type(app, definition, scopes, Some(ty))?;

                let output = ty.clone(); // Weird ownership shit?

                expr_type.ty.is_convertible_to(ty).at_ast(self)?;

                Ok(output.into())
            }
            Node::If { box condition, box body, else_body, ty } => {
                let cond_type = condition.set_type(app, definition, scopes, None)?;
                cond_type.expect(Type::Boolean.into(), "if statement condition needs to be boolean").at_ast(condition)?;

                // simple if branches don't return anything
                // if-else branches have to have matching types

                let body_type = body.set_type(app, definition, scopes, expect)?;
                if let Some(box else_body) = else_body {
                    let else_type = else_body.set_type(app, definition, scopes, expect)?;

                    else_type.expect_ref(body_type.ty.into(), "divergent if statement types").at_ast(else_body)?;
                    *ty = else_type.ty;
                }

                Ok(ty.clone().into())
            }
            Node::Loop { box body } => {
                body.set_type(app, definition, scopes, None)?;
                Ok(Type::Void.into())
            }
            Node::Break => Ok(Type::Void.into()),
            Node::Return( box expr ) => {
                let out = expr.set_type(app, definition, scopes, Some(&definition.return_type))?;
                out.expect(
                    definition.return_type.clone().into(),
                    "return type doesn't correspond to the given function"
                ).at_ast(expr)?;

                Ok(Type::Void.into())
            }
            // Creates new scopes
            Node::Block { inner: body, ty } => {
                scopes.push(Scope::default());

                if let Some((last, body)) = body.split_last_mut() {
                    for node in body {
                        node.set_type(app, definition, scopes, None)?;
                    }
                    *ty = last.set_type(app, definition, scopes, expect)?.ty;
                }

                scopes.pop();

                Ok(ty.clone().into()) 
            }
            Node::Statement( inner ) => {
                inner.set_type(app, definition, scopes, None)?;
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
            Node::Block { inner: _, ty } => ty,
            Node::Convert(_, _, ty) => ty,
            Node::Number(_, ty) => ty,
            Node::Identifier(_, ty) => ty,
            Node::BoolLiteral(_) => &Type::Boolean,
            Node::StringLiteral(_) => return Type::string(),
            Node::If { ty, .. } => ty,
            Node::Statement(..) | Node::FuncDef { .. } | Node::Loop { .. }
                | Node::Return(..) | Node::Break | Node::Intrisic(..) | Node::Empty
                | Node::Definition { .. } => &Type::Void
        }.clone()
    }
}

impl FunctionBody {
    fn type_check(&mut self, app: &App) -> Result<()> {
        let definition = &app.function_definitions[self.definition];

        let mut stack = ScopeStack::from_top(
            Scope {
                variables: IndexMap::from_iter(definition.parameters.iter().cloned()),
                deferred_variable: None,
                deferred: false
            }
        );

        for statement in self.body.iter_mut() {
            statement.set_type(app, definition, &mut stack, None)?;
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
            types: TypeHolder::with_builtins()
        }
    }

    pub fn insert_declarations(&mut self, root: Ast) -> Result<()> {
        let Node::Block { inner: root, ty: _ } = root.node
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
        // Although it means you don't have access to the function bodies while iterating, but that's not necessarly a problem
        let mut bodies = std::mem::take(&mut self.function_bodies);

        for function in &mut bodies {
            function.type_check(&self)?;
        }

        self.function_bodies = bodies;
        Ok(self)
    }
}

impl Display for App {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::utility::color::{GRAY, WHITE, CYAN, GREEN};

        writeln!(f, "TYPES")?;
        for (idx, (name, ty)) in self.types.types.iter().enumerate() {
            let connector = if idx == self.types.types.len()-1 { '╰' } else { '├' } ;

            writeln!(f, "{GRAY}{connector} {GREEN}\"{name}\" = {CYAN}{ty:?}")?;
        }
        writeln!(f, "{WHITE}")?;

        for (name, func) in &self.function_definitions {
            writeln!(f, "FUNC {name}")?;

            if func.return_type != Type::Void {
                writeln!(f, "{GRAY}├ {WHITE}RETURNS -> {CYAN}{:?}", func.return_type)?;
            }
            writeln!(f, "{GRAY}├ {WHITE}PARAMETERS")?;
            let parameters = &func.parameters;
            for (idx, (name, ty)) in parameters.iter().enumerate() {
                let is_last = idx == parameters.len()-1;
                let connector = if is_last { '╰' } else { '├' };

                writeln!(f, "{GRAY}│ {connector} {WHITE}{name} -> {CYAN}{ty:?}")?;
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
