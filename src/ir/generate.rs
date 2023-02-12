use crate::{ast, token::{self, Operation}, analysis, utility::PushIndex};

use super::*;

impl Arithmetic {
    fn from_op(op: Operation, lhs: Value, rhs: Value) -> Self {
        use Operation::*;
        match op {
            Add => Arithmetic::Add(lhs, rhs),
            Sub => Arithmetic::Sub(lhs, rhs),
            Mul => Arithmetic::Mul(lhs, rhs),
            Div => Arithmetic::Div(lhs, rhs),
            _ => unreachable!()
        }
    }
}

impl Comparison { 
    fn from_op(op: Operation, lhs: Value, rhs: Value) -> Self {
        use Operation::*;
        match op {
            Equals => Comparison::Eq(lhs, rhs),
            NotEquals => Comparison::Neq(lhs, rhs),
            Greater => Comparison::Gt(lhs, rhs),
            GreaterOrEquals => Comparison::Ge(lhs, rhs),
            Lesser => Comparison::Lt(lhs, rhs),
            LesserOrEquals => Comparison::Le(lhs, rhs),
            _ => unreachable!("Operation {op:?} cannot be converted to a jump")
        }
    }

    fn inverse(self) -> Self {
        use Comparison::*;
        match self {
            Unconditional => Never,
            Never => Unconditional,
            NotZero(v) => Zero(v),
            Zero(v) => NotZero(v),
            Eq(l, r) => Neq(l, r),
            Neq(l, r) => Eq(l, r),
            Gt(l, r) => Le(l, r),
            Ge(l, r) => Lt(l, r),
            Lt(l, r) => Ge(l, r),
            Le(l, r) => Gt(l, r)
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
            }
            ast::Node::Expr { lhs, rhs, op } => {
                let lhs = self.fold_node(ir, app, func, *lhs);
                let rhs = self.fold_node(ir, app, func, *rhs);

                let temporary;

                let ins = match op {
                    op if op.is_arithmetic() => {
                        temporary = self.add_variable(4);
                        Instruction::StoreOperation(temporary, Arithmetic::from_op(op, lhs, rhs))
                    },
                    op if op.is_comparison() => {
                        temporary = self.add_variable(1);
                        Instruction::StoreComparison(temporary, Comparison::from_op(op, lhs, rhs))
                    },
                    _ => unreachable!()
                };
                
                self.instructions.push(ins);
                Value::VariableLoad(temporary)
            }
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
            }
            ast::Node::If { condition, body } => {
                let idx = self.add_label();
                let jump = if let ast::Node::Expr { op, lhs, rhs } = *condition {
                    let lhs = self.fold_node(ir, app, func, *lhs);
                    let rhs = self.fold_node(ir, app, func, *rhs);
                    Comparison::from_op(op, lhs, rhs).inverse() // Skip the function's body if the condition is NOT true
                } else {
                    let value = self.fold_node(ir, app, func, *condition);
                    Comparison::NotZero(value).inverse()
                };

                self.instructions.push(Instruction::Jump(idx, jump));

                self.fold_node(ir, app, func, *body);
                self.instructions.push(Instruction::Label(idx));
                
                Value::NoValue
            }
            ast::Node::Loop { body } => {
                let loop_start = self.add_label();
                let loop_end = self.add_label();

                self.instructions.push(Instruction::Label(loop_start));
                self.fold_node(ir, app, func, *body);

                self.instructions.push(Instruction::Jump(loop_start, Comparison::Unconditional));
                self.instructions.push(Instruction::Label(loop_end));


                Value::NoValue
            }
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
            }
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

    fn add_variable(&mut self, size: u32) -> VariableIndex {
        let total_offset = self.variables.last().map_or(0, |v| v.total_offset) + size;
        self.variables.push_idx(VariableOffset { size, total_offset })
    }

    fn add_label(&mut self) -> LabelIndex {
        let idx = self.label_num;
        self.label_num += 1;
        idx
    }

    fn new(name: String, definition: &analysis::Function) -> Self {
        let mut this = Self {
            name,
            variables: Vec::new(),
            instructions: Vec::new(),
            label_num: 0
        };

        for v in definition.variables.values() {
            this.add_variable(v.size());
        }

        this
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
