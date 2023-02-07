use indexmap::IndexMap;

use crate::{ast, token::{self, Operation}, analysis, utility::PushIndex};

use super::*;

impl Intrisic {
    fn from_op(op: token::Operation, lhs: Value, rhs: Value) -> Self {
        match op {
            Operation::Add => Intrisic::Add(lhs, rhs),
            Operation::Sub => Intrisic::Sub(lhs, rhs),
            Operation::Mul => Intrisic::Mul(lhs, rhs),
            Operation::Div => Intrisic::Div(lhs, rhs),
            _ => unreachable!()
        }
    }
}

impl Function {
    /// Invariant: app's function_bodies isn't valid!
    fn fold_node(&mut self, ir: &mut Ir, app: &analysis::App, func: &analysis::Function, node: ast::Node) -> Value {
        match node {
            ast::Node::Expr { lhs, rhs, op: Operation::Assignment } => {
                let (ast::Node::Identifier(var) | ast::Node::Definition { name: var, .. }) = *lhs
                else { 
                    unreachable!()
                };

                let var = func.variables.get_index_of(&var).unwrap();

                let rhs = self.fold_node(ir, app, func, *rhs);
                self.instructions.push(Instruction::VariableStore(var, rhs));

                Value::NoValue
            },
            ast::Node::Expr { lhs, rhs, op } => {
                let lhs = self.fold_node(ir, app, func, *lhs);
                let rhs = self.fold_node(ir, app, func, *rhs);

                // Operations should be between i32s only
                let temporary = self.variables.push_idx(Type { size: 4 });

                self.instructions.push(Instruction::StoreIntrisic(temporary, Intrisic::from_op(op, lhs, rhs)));
                Value::VariableLoad(temporary)
            },
            ast::Node::Call { name, parameter_list } => Value::Call {
                func: app.function_definitions.get_index_of(&name).unwrap(),
                parameters: parameter_list.into_iter().map(|n| self.fold_node(ir, app, func, n)).collect()
            },
            ast::Node::Intrisic(i) => {
                match i {
                    ast::Intrisic::Asm(str) => {
                        let str = self.fold_node(ir, app, func, *str);
                        self.instructions.push(Instruction::Intrisic(Intrisic::Asm(str)));
                    },
                    ast::Intrisic::Print(values) => {
                        for node in values {
                            // Desugar print intrisic
                            let print = match node.get_type(app, func).unwrap() {
                                analysis::Type::String => Intrisic::PrintString(self.fold_node(ir, app, func, node)),
                                analysis::Type::Integer32 => Intrisic::PrintNumber(self.fold_node(ir, app, func, node)),
                                _ => unreachable!()
                            };

                            self.instructions.push(Instruction::Intrisic(print));
                        }
                    }
                }
                // self.instructions.push(Instruction::Intrisic(Intrisic::from_node(name, parameter_list)?));
                Value::NoValue
            },
            ast::Node::Block(nodes) => {
                let mut it = nodes.into_iter().peekable();
                while let Some(node) = it.next() {
                    if it.peek().is_none() { // if last node
                        return self.fold_node(ir, app, func, node);
                    }
                    else {
                        self.fold_node(ir, app, func, node);
                    }
                }

                Value::NoValue
            },
            ast::Node::Statement(inner) => {
                self.fold_node(ir, app, func, *inner);
                Value::NoValue
            }
            ast::Node::Identifier(var) => Value::VariableLoad(func.variables.get_index_of(&var).unwrap()),
            ast::Node::Number(n) => Value::Number(n),
            ast::Node::StringLiteral(s) => Value::Literal(ir.push_literal(s)),
            _ => unreachable!("this ast node shouldn't be given to ir generation, got: {node:#?}"),
        }
    }

    fn new(name: String, definition: &analysis::Function) -> Self {
        Self {
            name,
            variables: definition.variables.values().map(|&x| x.into()).collect(),
            instructions: Vec::new()
        }
    }

    fn generate_ir(&mut self, ir: &mut Ir, app: &analysis::App, definition: &analysis::Function, body: analysis::FunctionBody) {
        for node in body.body {
            self.fold_node(ir, app, definition, node);
        }
    }
}

impl Ir {
    pub fn from_app(mut app: analysis::App) -> Self {
        let mut this = Self {
            functions: Vec::new(),
            literals: Vec::new()
        };

        let bodies = app.function_bodies;
        app.function_bodies = Vec::new();

        for body in bodies {
            let (name, definition) = app.function_definitions.get_index(body.definition).unwrap();
            let mut func = Function::new(name.clone(), definition);
            func.generate_ir(&mut this, &app, definition, body);

            this.functions.push(func);
        }

        this
    }

    pub fn push_literal(&mut self, value: String) -> LiteralIndex {
        self.literals.push_idx(value)
    }
}