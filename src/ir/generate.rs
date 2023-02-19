use crate::{ast::{self, BinaryOperation, UnaryOperation}, analysis, utility::PushIndex, typed::{Type, SuperType}};

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
            ast::Node::Expr { op: BinaryOperation::Assignment, ty: _, lhs, rhs } => {
                let address = match *lhs {
                    ast::Node::Identifier(var, _) | ast::Node::Definition { name: var, .. } => {
                        let var = self.named_variables[&var];
                        var.into()
                    }
                    ast::Node::UnaryExpr { op: UnaryOperation::Deref, ty, value: var, .. } => {
                        let var = self.fold_node(ir, app, func, scope, *var);
                        Address::Ptr(var, ty.size())
                    }
                    _ => unreachable!("Cannot assign to {lhs:?}"),
                };

                let rhs = self.fold_node(ir, app, func, scope, *rhs);
                self.instructions.push(Instruction::VariableStore(address, rhs));

                Value::NoValue
            }
            ast::Node::UnaryExpr { op, ty, value } => {
                let value = self.fold_node(ir, app, func, scope, *value);

                let temporary = self.add_temporary(ty.size());
                use UnaryOperation::*;
                let op = match op {
                    LogicalNot => Arithmetic::Not(value),
                    Deref => Arithmetic::Deref(value, ty.size()),
                    AddressOf => Arithmetic::AddressOf(value),
                    Negation => Arithmetic::Negate(value)
                };

                self.instructions.push(Instruction::StoreOperation(temporary.into(), op));
                Value::VariableLoad(temporary)
            }
            ast::Node::Expr { op, ty, lhs, rhs } => {
                let lhs = self.fold_node(ir, app, func, scope, *lhs);
                let rhs = self.fold_node(ir, app, func, scope, *rhs);

                let temporary = self.add_temporary(ty.size());

                let ins = match op {
                    op if op.is_arithmetic() => {
                        Instruction::StoreOperation(temporary.into(), Arithmetic::from_op(op, lhs, rhs))
                    }
                    op if op.is_logic() => {
                        Instruction::StoreOperation(temporary.into(), Arithmetic::from_op(op, lhs, rhs))
                    }
                    op if op.is_comparison() => {
                        Instruction::StoreComparison(temporary.into(), Comparison::from_op(op, lhs, rhs))
                    }
                    _ => unreachable!()
                };
                
                self.instructions.push(ins);
                Value::VariableLoad(temporary)
            }
            ast::Node::Call { name, parameter_list, return_type } => {
                let idx = app.function_definitions.get_index_of(&name).unwrap();

                let return_size = return_type.size();
                let call = Instruction::Call {
                    func: idx,
                    return_type,
                    parameters: parameter_list.into_iter().map(|n| self.fold_node(ir, app, func, scope, n)).collect()
                };
                self.instructions.push(call);

                if return_size > 0 {
                    let temporary = self.add_temporary(return_size);
                    self.instructions.push(
                        Instruction::VariableStore(temporary.into(), Value::LastCall { size: return_size })
                    );

                    Value::VariableLoad(temporary)
                }
                else {
                    Value::NoValue
                }
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
                            let print = match node.get_type().clone() {
                                Type::Ptr(box Type::UInt8) => Intrisic::PrintString(self.fold_node(ir, app, func, scope, node)),
                                ty if SuperType::Integer.verify(&ty) => Intrisic::PrintNumber(self.fold_node(ir, app, func, scope, node), ty),
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
                    ast::Node::Expr { op, ty: _, lhs, rhs} if op.is_comparison() => {
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
                self.instructions.push(Instruction::VariableStore(self.return_variable.unwrap().into(), inner));
                self.instructions.push(Instruction::Ret);

                Value::NoValue
            }
            ast::Node::Block(nodes, _) => {
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
            ast::Node::Empty => Value::NoValue,
            ast::Node::Identifier(var, _) => Value::VariableLoad(self.named_variables[&var]),
            ast::Node::Number(n, ty) => Value::Number(n, ty.size()),
            ast::Node::StringLiteral(s) => Value::Literal(ir.push_literal(s)),
            ast::Node::Definition { .. } => Value::NoValue,
            ast::Node::FuncDef { .. } => unreachable!("this ast node shouldn't be given to ir generation, got: {node:#?}"),
        }
    }

    fn add_temporary(&mut self, size: u32) -> VariableKey {
        let total_offset = match self.last_variable.map(|x| self.variables[x]) {
            Some(VariableOffset { total_offset, argument: false, .. }) => total_offset + size,
            _ => size
        };

        let k = self.variables.insert(VariableOffset { size, total_offset, argument: false });
        self.last_variable = Some(k);
        k
    }

    fn add_label(&mut self) -> LabelIndex {
        let idx = self.label_num;
        self.label_num += 1;
        idx
    }

    fn new(name: String, definition: &analysis::Function) -> Self {
        let mut this = Self {
            name,
            variables: SlotMap::new(),
            named_variables: HashMap::new(),
            last_variable: None,
            return_variable: None,
            instructions: Vec::new(),
            label_num: 0,
        };

        let num_args = definition.arguments.len();
        let mut total_offset = 16;
        for (name, v) in definition.variables.iter().take(num_args) {
            let k = this.variables.insert(VariableOffset { size: v.size(), total_offset, argument: true});
            this.named_variables.insert(name.clone(), k);

            total_offset += v.size();
        }

        let return_size = definition.return_type.size();
        if return_size > 0 {
            this.return_variable = Some(this.variables.insert(VariableOffset { 
                size: return_size, total_offset, argument: true 
            }));
        }

        for (name, v) in definition.variables.iter().skip(num_args) {
            let k = this.add_temporary(v.size());
            this.named_variables.insert(name.clone(), k);
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
