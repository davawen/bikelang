use crate::{ast::{self, BinaryOperation, UnaryOperation}, analysis, utility::PushIndex};

use super::*;

impl Arithmetic {
    fn from_op(op: BinaryOperation, lhs: Value, rhs: Value) -> Self {
        macro_rules! map {
            ($($l:ident => $r:ident),+) => {
                match op {
                    $(BinaryOperation::$l => Arithmetic::$r(lhs, rhs),)+
                    _ => unreachable!()
                }
            };
        }

        map! {
            Add => Add,
            Sub => Sub,
            Mul => Mul,
            Div => Div,
            Modulus => Modulus,
            LogicalAnd => And,
            LogicalOr => Or,
            LogicalXor => Xor
        }
    }
}

impl Comparison { 
    fn from_op(op: BinaryOperation, lhs: Value, rhs: Value) -> Self {
        use BinaryOperation::*;
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
    /// Invariant: app's function_bodies can't be used!
    fn fold_node(&mut self, ir: &mut Ir, app: &analysis::App, func: &analysis::Function, scope: &[Scope], node: ast::Node) -> Value {
        match node {
            ast::Node::Expr { lhs, rhs, op: BinaryOperation::Assignment } => {
                let (ast::Node::Identifier(var) | ast::Node::Definition { name: var, .. }) = *lhs
                else { 
                    todo!("Can only assign values to variables for now")
                };

                let var = func.variables.get_index_of(&var).unwrap();

                let rhs = self.fold_node(ir, app, func, scope, *rhs);
                self.instructions.push(Instruction::VariableStore(var, rhs));

                Value::NoValue
            }
            ast::Node::UnaryExpr { op, value } => {
                let value = self.fold_node(ir, app, func, scope, *value);
                let temporary = self.add_temporary(1);

                use UnaryOperation::*;
                let ins = match op {
                    LogicalNot => Instruction::StoreOperation(temporary, Arithmetic::Not(value))
                };

                self.instructions.push(ins);
                Value::VariableLoad(temporary)
            }
            ast::Node::Expr { lhs, rhs, op } => {
                let lhs = self.fold_node(ir, app, func, scope, *lhs);
                let rhs = self.fold_node(ir, app, func, scope, *rhs);

                let temporary;

                let ins = match op {
                    op if op.is_arithmetic() => {
                        temporary = self.add_temporary(4);
                        Instruction::StoreOperation(temporary, Arithmetic::from_op(op, lhs, rhs))
                    }
                    op if op.is_logic() => {
                        temporary = self.add_temporary(1);
                        Instruction::StoreOperation(temporary, Arithmetic::from_op(op, lhs, rhs))
                    }
                    op if op.is_comparison() => {
                        temporary = self.add_temporary(1);
                        Instruction::StoreComparison(temporary, Comparison::from_op(op, lhs, rhs))
                    }
                    _ => unreachable!()
                };
                
                self.instructions.push(ins);
                Value::VariableLoad(temporary)
            }
            ast::Node::Call { name, parameter_list } => {
                let (idx, _, called) = app.function_definitions.get_full(&name).unwrap();
                let call = Instruction::Call {
                    func: idx,
                    return_type: called.return_type,
                    parameters: parameter_list.into_iter().map(|n| self.fold_node(ir, app, func, scope, n)).collect()
                };
                self.instructions.push(call);

                let temporary = self.add_temporary(called.return_type.size());
                self.instructions.push(Instruction::VariableStore(temporary, Value::LastCall { size: called.return_type.size() }));

                Value::VariableLoad(temporary)
            }
            ast::Node::Intrisic(i) => {
                match i {
                    ast::Intrisic::Asm(str) => {
                        let str = self.fold_node(ir, app, func, scope, *str);
                        self.instructions.push(Instruction::Intrisic(Intrisic::Asm(str)));
                    },
                    ast::Intrisic::Print(values) => {
                        for node in values {
                            // Desugar print intrisic
                            let print = match node.get_type(app, func).unwrap() {
                                analysis::Type::String => Intrisic::PrintString(self.fold_node(ir, app, func, scope, node)),
                                analysis::Type::Integer32 => Intrisic::PrintNumber(self.fold_node(ir, app, func, scope, node)),
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

                let jump = match *condition { 
                    ast::Node::Expr { op, lhs, rhs } if op.is_comparison() => {
                        let lhs = self.fold_node(ir, app, func, scope, *lhs);
                        let rhs = self.fold_node(ir, app, func, scope, *rhs);
                        Comparison::from_op(op, lhs, rhs).inverse() // Skip the function's body if the condition is NOT true
                    } 
                    _ => {
                        let value = self.fold_node(ir, app, func, scope, *condition);
                        Comparison::NotZero(value).inverse()
                    }
                };

                let scope = &[scope, &[Scope::If { end_label: idx }]].concat();

                self.instructions.push(Instruction::Jump(idx, jump));
                self.fold_node(ir, app, func, scope, *body);
                self.instructions.push(Instruction::Label(idx));
                
                Value::NoValue
            }
            ast::Node::Loop { body } => {
                let loop_start = self.add_label();
                let loop_end = self.add_label();

                let scope = &[scope, &[Scope::Loop { start_label: loop_start, end_label: loop_end }]].concat();

                self.instructions.push(Instruction::Label(loop_start));
                self.fold_node(ir, app, func, scope, *body);

                self.instructions.push(Instruction::Jump(loop_start, Comparison::Unconditional));
                self.instructions.push(Instruction::Label(loop_end));

                Value::NoValue
            }
            ast::Node::Break => {
                let Some(Scope::Loop { end_label, .. }) = scope.iter().rfind(|s| matches!(s, Scope::Loop { .. }))
                    else { unreachable!() };

                self.instructions.push(Instruction::Jump(*end_label, Comparison::Unconditional));

                Value::NoValue
            }
            ast::Node::Return(inner) => {
                let inner = self.fold_node(ir, app, func, scope, *inner);
                self.instructions.push(Instruction::VariableStore(self.return_variable.unwrap(), inner));
                self.instructions.push(Instruction::Ret);

                Value::NoValue
            }
            ast::Node::Block(nodes) => {
                let scope = &[scope, &[Scope::Block]].concat();
                let mut it = nodes.into_iter().peekable();
                while let Some(node) = it.next() {
                    if it.peek().is_none() { // if last node
                        return self.fold_node(ir, app, func, scope, node);
                    }
                    else {
                        self.fold_node(ir, app, func, scope, node);
                    }
                }

                Value::NoValue
            }
            ast::Node::Statement(inner) => {
                self.fold_node(ir, app, func, scope, *inner);
                Value::NoValue
            }
            ast::Node::Identifier(var) => Value::VariableLoad(func.variables.get_index_of(&var).unwrap()),
            ast::Node::Number(n) => Value::Number(n),
            ast::Node::StringLiteral(s) => Value::Literal(ir.push_literal(s)),
            ast::Node::FuncDef { .. } | ast::Node::Definition { .. } => unreachable!("this ast node shouldn't be given to ir generation, got: {node:#?}"),
        }
    }

    fn add_temporary(&mut self, size: u32) -> VariableIndex {
        let total_offset = if let Some(VariableOffset { total_offset, argument: false, .. }) = self.variables.last() {
            total_offset + size
        } else {
            size
        };

        self.variables.push_idx(VariableOffset { size, total_offset, argument: false })
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
            label_num: 0,
            return_variable: None
        };

        let num_args = definition.arguments.len();
        let mut total_offset = 16;
        for v in definition.variables.values().take(num_args) {
            this.variables.push(VariableOffset { size: v.size(), total_offset, argument: true});
            total_offset += v.size();
        }

        let return_size = definition.return_type.size();
        if return_size > 0 {
            this.return_variable = Some(this.variables.push_idx(VariableOffset { 
                size: return_size, total_offset, argument: true 
            }));
        }

        for v in definition.variables.values().skip(num_args) {
            this.add_temporary(v.size());
        }

        this
    }

    fn generate_ir(&mut self, ir: &mut Ir, app: &analysis::App, definition: &analysis::Function, body: analysis::FunctionBody) {
        for node in body.body {
            self.fold_node(ir, app, definition, &[], node);
        }
        self.instructions.push(Instruction::Ret);
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
