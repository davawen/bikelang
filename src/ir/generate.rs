use itertools::Itertools;

use crate::{ast::{self, analysis, BinaryOperation, UnaryOperation}, typed::{Type, SuperType}};

use super::{*, scope::{VariableOffset, ScopeIndex, ScopeKind}};

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
    /// Invariant: app's function definition's scopes are invalid! (moved into IR scopes)
    fn fold_node(&mut self, ir: &mut Ir, app: &analysis::App, func: &analysis::Function, scope: ScopeIndex, ast: ast::Ast) -> Value {
        match ast.node {
            ast::Node::Expr { op: BinaryOperation::Assignment, ty: _, box lhs, box rhs } => {
                let address = match lhs.node {
                    ast::Node::Identifier(var, _) | ast::Node::Definition { name: var, .. } => {
                        let scope = &self.scopes[scope];
                        let var = scope.get(&var, &self.scopes);
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
                        let ast::Node::Identifier(name, _ty) = inner.node 
                            else { unreachable!("Trying to take address of literal") };
                        let reg = Register::get(ir, 8);
                        value = Value::Register(reg);

                        let scope = &self.scopes[scope];
                        Arithmetic::AddressOf(reg, scope.get(&name, &self.scopes))
                    },
                    op => {
                        value = self.fold_node(ir, app, func, scope, inner);
                        let reg = value.as_register(ir, &mut self.instructions);
                        value = Value::Register(reg);
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
                        let reg = lhs.as_register(ir, &mut self.instructions);

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
            ast::Node::Convert(box expr, ty) => {
                // Only do sign-extend with signed->signed conversions
                // Which skips conversion entirely in same-sized signed->unsigned conversions
                // unsigned->bigger signed = You want to keep the upper values
                // signed->unsigned = Either you only have positive values or you want to interpret two's complement anyway

                // If you have an unsigned -> bigger unsigned conversion, a load will zero-extend it automatically,
                // but if you're reusing the register passed to you, that's both useless and unauthorized by x86.

                let inner_type = expr.get_type();

                let inner = self.fold_node(ir, app, func, scope, expr);

                let reg = Register::get(ir, ty.size());

                let ins = if SuperType::Signed.verify_both(&ty, &inner_type) {
                    if inner_type.size() == ty.size() {
                        Instruction::Load(reg, inner)
                    } else {
                        Instruction::LoadSignExtend(reg, inner)
                    }
                } else {
                    Instruction::Load(reg, inner)
                };
                self.instructions.push(ins);
                inner.free_register(ir);

                Value::Register(reg)
            }
            ast::Node::Call { name, argument_list, return_type } => {
                let idx = app.function_definitions.get_index_of(&name).unwrap();

                let return_size = return_type.size();
                let arguments: Vec<_> = argument_list.into_iter().map(|n| self.fold_node(ir, app, func, scope, n)).collect();
                for p in &arguments {
                    p.free_register(ir);
                }

                // Save used registers
                for reg in ir.registers_in_use() {
                    self.instructions.push(Instruction::Save(reg.with_size(8)));
                }

                let call = Instruction::Call {
                    func: idx,
                    return_type,
                    arguments
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
            ast::Node::If { box condition, box body, else_body, ty } => {
                let jump_comparison = match condition.node { 
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

                let end_body_idx = self.add_label();
                self.instructions.push(Instruction::Jump(end_body_idx, jump_comparison));

                let body_result = self.fold_node(ir, app, func, scope, body);

                // If there is an else, jump to the end of the branch when you've finished executing the body
                if let Some(box else_body) = else_body {
                    let reg = body_result.as_register(ir, &mut self.instructions);

                    let end_if_idx = self.add_label();
                    self.instructions.push(Instruction::Jump(end_if_idx, Comparison::Unconditional));

                    self.instructions.push(Instruction::Label(end_body_idx)); // go here if condition was false
                    let else_result = self.fold_node(ir, app, func, scope, else_body);
                    else_result.free_register(ir);
                    self.instructions.push(Instruction::Load(reg, else_result));

                    self.instructions.push(Instruction::Label(end_if_idx));

                    Value::Register(reg)
                } else {
                    self.instructions.push(Instruction::Label(end_body_idx));

                    Value::NoValue
                }
            }
            ast::Node::Loop { box body } => {
                let loop_start = self.add_label();
                let loop_end = self.add_label();

                // Set scope kind of inner block
                if let &ast::Node::Block { scope: loop_scope, .. } = &body.node { 
                    self.scopes[loop_scope].kind = ScopeKind::Loop { end_label: loop_end };
                } else { unreachable!() };

                self.instructions.push(Instruction::Label(loop_start));
                self.fold_node(ir, app, func, scope, body).free_register(ir);

                self.instructions.push(Instruction::Jump(loop_start, Comparison::Unconditional));
                self.instructions.push(Instruction::Label(loop_end));

                Value::NoValue
            }
            ast::Node::Break => {
                let scope = &self.scopes[scope];
                let Some(ScopeKind::Loop { end_label, .. }) = scope.search(&self.scopes, |k| matches!(k, ScopeKind::Loop { .. }))
                    else { unreachable!("Break called outside of a loop") };

                self.instructions.push(Instruction::Jump(*end_label, Comparison::Unconditional));

                Value::NoValue
            }
            ast::Node::Return(inner) => {
                let inner = self.fold_node(ir, app, func, scope, *inner);
                inner.free_register(ir);
                if let Some(return_variable) = self.return_variable {
                    self.instructions.push(Instruction::VariableStore(VariableId(0, return_variable).into(), inner));
                }
                self.instructions.push(Instruction::Ret);

                Value::NoValue
            }
            ast::Node::Block { inner: nodes, scope, ty: _ } => {
                // let scope = &[scope, &[Scope::Block]].concat();
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
                let scope = &self.scopes[scope];
                let reg = Register::get(ir, ty.size());
                self.instructions.push(Instruction::VariableLoad(reg, scope.get(&name, &self.scopes).into()));
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

    fn add_label(&mut self) -> LabelIndex {
        let idx = self.label_num;
        self.label_num += 1;
        idx
    }

    fn new(name: String, definition: &mut analysis::Function) -> Self {
        // Translate definition scopes into ir scopes
        let ast_scopes = std::mem::replace(&mut definition.scopes, Vec::new());

        // Calculate maximum stack offset
        let stack_offset = ast_scopes[1..].iter().map(|s| s.offset + s.size).max().unwrap();

        let mut ast_scopes = ast_scopes.into_iter().enumerate();

        let mut scopes = Vec::with_capacity(ast_scopes.len());

        let (_, parameter_scope) = ast_scopes.next().unwrap();
        let parameter_offset = parameter_scope.size + 16;

        let mut parameter_scope = Scope::from_parameters(parameter_scope, 0);

        let return_size = definition.return_type.size();
        let return_variable = if return_size > 0 {
            let r = parameter_scope.variables.insert( VariableOffset { 
                size: return_size, offset: parameter_offset, argument: true 
            });
            Some(r)
        } else { None };

        scopes.push(parameter_scope);
        scopes.extend(ast_scopes.map(|(idx, s)| Scope::from_ast_scope(s, idx)));

        Self {
            name,
            scopes,
            stack_offset,
            return_variable,
            instructions: Vec::new(),
            label_num: 0,
        }
    }

    fn generate_ir(&mut self, ir: &mut Ir, app: &analysis::App, definition: &analysis::Function, body: analysis::FunctionBody) {
        for node in body.body {
            self.fold_node(ir, app, definition, 1, node);
        }
        self.instructions.push(Instruction::Ret);
    }
}

impl Ir {
    pub fn from_app(mut app: analysis::App) -> Self {
        let mut this = Self {
            functions: Vec::new(),
            literals: IndexSet::new(),
            used_registers: EnumMap::default()
        };

        let bodies = app.function_bodies;
        app.function_bodies = Vec::new();

        for body in bodies {
            let (name, definition) = app.function_definitions.get_index_mut(body.definition).unwrap();
            let mut func = Function::new(name.clone(), definition);

            let definition = app.function_definitions.get_index(body.definition).unwrap().1;
            func.generate_ir(&mut this, &app, definition, body);

            this.functions.push(func);
        }

        this
    }

    pub fn push_literal(&mut self, value: String) -> LiteralIndex {
        self.literals.insert_full(value).0
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

    /// Returns the inner register if it's a Value::Register, else get a new one and put the value inside of it
    fn as_register(self, ir: &mut Ir, instructions: &mut Vec<Instruction>) -> Register {
        match self {
            Value::Register(reg) => reg,
            _ => {
                let reg = Register::get(ir, self.size());
                instructions.push(Instruction::Load(reg, self));
                reg
            }
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
