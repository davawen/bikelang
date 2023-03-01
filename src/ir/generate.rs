use itertools::Itertools;

use crate::{ast::{self, BinaryOperation, UnaryOperation}, analysis, utility::PushIndex, typed::{Type, SuperType}};

use super::*;

impl Arithmetic {
    fn from_op(op: BinaryOperation, lhs: Register, rhs: Value) -> Self {
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
    fn fold_node(&mut self, ir: &mut Ir, app: &analysis::App, func: &analysis::Function, scope: &[Scope], ast: ast::Ast) -> Value {
        match ast.node {
            ast::Node::Expr { op: BinaryOperation::Assignment, ty: _, box lhs, box rhs } => {
                let address = match lhs.node {
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

                let rhs = self.fold_node(ir, app, func, scope, rhs);

                address.free_register(ir);
                rhs.free_register(ir);

                self.instructions.push(Instruction::VariableStore(address, rhs));

                Value::NoValue
            }
            ast::Node::UnaryExpr { op, ty, value: box inner } => {
                let mut value;

                use UnaryOperation::*;
                let op = match op {
                    AddressOf => {
                        let ast::Node::Identifier(name, ty) = inner.node 
                            else { unreachable!("Trying to take address of literal") };
                        let reg = Register::get(ir, 8);
                        value = Value::Register(reg);
                        Arithmetic::AddressOf(reg, self.named_variables[&name])
                    },
                    op => {
                        value = self.fold_node(ir, app, func, scope, inner);
                        let reg = if let Value::Register(reg) = value {
                            reg
                        } else  {
                            let reg = Register::get(ir, value.size());
                            self.instructions.push(Instruction::Load(reg, value));
                            value = Value::Register(reg);
                            reg
                        };
                        match op {
                            Deref => {
                                value = Value::Register(reg.kind.with_size(ty.size()));
                                Arithmetic::Deref(reg, ty.size())
                            }
                            LogicalNot => Arithmetic::Not(reg),
                            Negation => Arithmetic::Negate(reg),
                            AddressOf => unreachable!()
                        }
                    }
                };

                self.instructions.push(Instruction::StoreOperation(op));
                value
            }
            ast::Node::Expr { op, ty, box lhs, box rhs } => {
                let lhs = self.fold_node(ir, app, func, scope, lhs);
                let rhs = self.fold_node(ir, app, func, scope, rhs);

                let (out, ins) = match op {
                    op if op.is_arithmetic() || op.is_logic() => {
                        let reg = match lhs {
                            Value::Register(reg) => reg,
                            _ => {
                                let reg = Register::get(ir, ty.size());
                                self.instructions.push(Instruction::Load(reg, lhs));
                                reg
                            }
                        };
                        // result of operation is stored in register to the left

                        (reg, Instruction::StoreOperation(Arithmetic::from_op(op, reg, rhs)))
                    }
                    op if op.is_comparison() => {
                        // Check if we're comparing constants (not authorized x:)
                        let lhs =  if let (Value::Number(..) | Value::Boolean(_), Value::Number(..) | Value::Boolean(_)) = (lhs, rhs) {
                            let reg = Register::get(ir, lhs.size());
                            self.instructions.push(Instruction::Load(reg, lhs));
                            Value::Register(reg)
                        } else {
                            lhs
                        };

                        let reg = Register::get(ir, ty.size());
                        lhs.free_register(ir); // result of comparison is stored in new register
                        (reg, Instruction::StoreComparison(reg, Comparison::from_op(op, lhs, rhs)))
                    }
                    _ => unreachable!()
                };
                rhs.free_register(ir);
                
                self.instructions.push(ins);
                Value::Register(out)
            }
            ast::Node::Call { name, parameter_list, return_type } => {
                let idx = app.function_definitions.get_index_of(&name).unwrap();

                let return_size = return_type.size();
                let parameters: Vec<_> = parameter_list.into_iter().map(|n| self.fold_node(ir, app, func, scope, n)).collect();
                for p in &parameters {
                    p.free_register(ir);
                }

                // Save used registers
                for reg in ir.registers_in_use() {
                    self.instructions.push(Instruction::Save(reg.with_size(8)));
                }

                let call = Instruction::Call {
                    func: idx,
                    return_type,
                    parameters
                };
                self.instructions.push(call);

                // Restore used registers
                let mut restore = ir.registers_in_use().rev().map( |reg| {
                    Instruction::Restore(reg.with_size(8))
                }).collect_vec();

                // Get return value 
                let out = if return_size > 0 {
                    let reg = Register::get(ir, return_size);
                    // It's at the top of the stack
                    self.instructions.push(Instruction::Restore(reg));

                    Value::Register(reg)
                }
                else {
                    Value::NoValue
                };

                self.instructions.append(&mut restore);

                out
            }
            ast::Node::Intrisic(i) => {
                match i {
                    ast::Intrisic::Asm(box str) => {
                        let str = self.fold_node(ir, app, func, scope, str);
                        self.instructions.push(Instruction::Intrisic(Intrisic::Asm(str)));
                    },
                    ast::Intrisic::Print(values) => {
                        for node in values {
                            // Desugar print intrisic
                            let print = match node.get_type().clone() {
                                Type::Ptr(box Type::UInt8) => Intrisic::PrintString(self.fold_node(ir, app, func, scope, node)),
                                ty if SuperType::Integer.verify(&ty) => {
                                    let num = self.fold_node(ir, app, func, scope, node);
                                    num.free_register(ir);
                                    Intrisic::PrintNumber(num, ty)
                                }
                                _ => unreachable!()
                            };

                            self.instructions.push(Instruction::Intrisic(print));
                        }
                    }
                }
                // self.instructions.push(Instruction::Intrisic(Intrisic::from_node(name, parameter_list)?));
                Value::NoValue
            }
            ast::Node::If { box condition, box body } => {
                let idx = self.add_label();

                let jump = match condition.node { 
                    ast::Node::Expr { op, ty: _, lhs, rhs} if op.is_comparison() => {
                        let lhs = self.fold_node(ir, app, func, scope, *lhs);
                        let rhs = self.fold_node(ir, app, func, scope, *rhs);

                        // Allow using registers after this instruction
                        // Note: this is fine because the comparison is pushed right after
                        lhs.free_register(ir);
                        rhs.free_register(ir);

                        Comparison::from_op(op, lhs, rhs).inverse() // Skip the function's body if the condition is NOT true
                    } 
                    _ => {
                        let value = self.fold_node(ir, app, func, scope, condition);
                        value.free_register(ir);

                        Comparison::NotZero(value).inverse()
                    }
                };

                let scope = &[scope, &[Scope::If { end_label: idx }]].concat();

                self.instructions.push(Instruction::Jump(idx, jump));

                self.fold_node(ir, app, func, scope, body).free_register(ir);
                self.instructions.push(Instruction::Label(idx));
                
                Value::NoValue
            }
            ast::Node::Loop { box body } => {
                let loop_start = self.add_label();
                let loop_end = self.add_label();

                let scope = &[scope, &[Scope::Loop { start_label: loop_start, end_label: loop_end }]].concat();

                self.instructions.push(Instruction::Label(loop_start));
                self.fold_node(ir, app, func, scope, body).free_register(ir);

                self.instructions.push(Instruction::Jump(loop_start, Comparison::Unconditional));
                self.instructions.push(Instruction::Label(loop_end));

                Value::NoValue
            }
            ast::Node::Break => {
                let Some(Scope::Loop { end_label, .. }) = scope.iter().rfind(|s| matches!(s, Scope::Loop { .. }))
                    else { unreachable!("Break called outside of a loop") };

                self.instructions.push(Instruction::Jump(*end_label, Comparison::Unconditional));

                Value::NoValue
            }
            ast::Node::Return(inner) => {
                let inner = self.fold_node(ir, app, func, scope, *inner);
                inner.free_register(ir);
                if let Some(return_variable) = self.return_variable {
                    self.instructions.push(Instruction::VariableStore(return_variable.into(), inner));
                }
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
                        self.fold_node(ir, app, func, scope, node).free_register(ir);
                    }
                }

                Value::NoValue
            }
            ast::Node::Statement(inner) => {
                self.fold_node(ir, app, func, scope, *inner).free_register(ir);
                Value::NoValue
            }
            ast::Node::Identifier(name, ty) => {
                let reg = Register::get(ir, ty.size());
                self.instructions.push(Instruction::VariableLoad(reg, self.named_variables[&name].into()));
                Value::Register(reg)
            }
            ast::Node::Number(n, ty) => Value::Number(n, ty.size()),
            ast::Node::StringLiteral(s) => Value::Literal(ir.push_literal(s)),
            ast::Node::BoolLiteral(b) => Value::Boolean(b),
            ast::Node::Definition { .. } => Value::NoValue,
            ast::Node::Empty => Value::NoValue,
            ast::Node::FuncDef { .. } => unreachable!("this ast node shouldn't be given to ir generation, got: {self:#?}"),
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

    fn get_var(&self, name: &str) -> (VariableKey, VariableOffset) {
        let var = self.named_variables[name];
        ( var, self.variables[var] )
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
            literals: Vec::new(),
            used_registers: EnumMap::default()
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

    /// Returns all registers currently in use
    fn registers_in_use(&mut self) -> impl DoubleEndedIterator<Item = RegisterKind> + '_ {
        self.used_registers.iter_mut().filter(|(_, x)| **x).map(|(x, _)| x)
    }
}

impl Register {
    fn get(ir: &mut Ir, size: u32) -> Self {
        let (kind, used) = ir.used_registers.iter_mut()
            .find(|(r, used)| r.is_usable() && !**used)
            .expect("Couldn't find an unused register");

        *used = true;
        Self { kind, size }
    }

    fn free_register(self, ir: &mut Ir) {
        ir.used_registers[self.kind] = false;
    }
}

impl RegisterKind {
    fn is_usable(&self) -> bool {
        use RegisterKind::*;
        matches!(self, Rax | Rcx | Rdx | R8 | R9 | R10 | R11)
    }
}

impl Value {
    /// Frees the register in value if it is one
    fn free_register(&self, ir: &mut Ir) {
        if let Value::Register(reg) = self {
            reg.free_register(ir);
        }
    }
}

impl Address {
    fn free_register(&self, ir: &mut Ir) {
        if let Address::Ptr(v, _) = self {
            v.free_register(ir);
        }
    }
}
