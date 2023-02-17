use super::*;

impl Value {
    fn used_variable(&self) -> Option<VariableKey> {
        if let Value::VariableLoad(key) = self {
            Some(*key)
        } else { None }
    }
}

impl Arithmetic {
    fn as_values(&self) -> Vec<&Value> {
        use Arithmetic::*;
        match self {
            Not(x) | Negate(x) | Deref(x, _) => vec![x],
            Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) | Modulus(a, b) | And(a, b) | Or(a, b) | Xor(a, b) => vec![a, b]
        }
    }
}

impl Comparison {
    fn as_values(&self) -> Vec<&Value> {
        use Comparison::*;
        match self {
            Unconditional | Never => vec![],
            NotZero(x) | Zero(x) => vec![x],
            Eq(a, b) | Neq(a, b) | Gt(a, b) | Ge(a, b) | Lt(a, b) | Le(a, b) => vec![a, b],
        }
    }
}

impl Intrisic {
    fn as_value(&self) -> &Value {
        use Intrisic::*;
        match self {
            Asm(v) => v,
            PrintNumber(v, _) => v,
            PrintString(v) => v
        }
    }
}

impl Instruction {
    fn used_variables(&self) -> Vec<VariableKey> {
        match self {
            Instruction::VariableStore(key, v) => [ Some(*key), v.used_variable() ].into_iter().flatten().collect(),
            Instruction::StoreOperation(key, v) => {
                [ *key ].into_iter().chain(v.as_values().into_iter().flat_map(|x| x.used_variable())).collect()
            }
            Instruction::StoreComparison(key, cmp) => {
                [ *key ].into_iter().chain(cmp.as_values().into_iter().flat_map(|x| x.used_variable())).collect()
            }
            Instruction::Jump(_, cmp) => {
                cmp.as_values().into_iter().flat_map(|x| x.used_variable()).collect()
            },
            Instruction::Intrisic(i) => i.as_value().used_variable().map_or(vec![], |x| vec![x]),
            Instruction::Call { parameters, .. } => parameters.iter().flat_map(|x| x.used_variable()).collect(),
            _ => vec![]
        }
    }
}

impl Function {
    /// Returns wether any instructions was removed
    fn remove_with_mask(&mut self, mask: Vec<usize>) -> bool {
        if mask.is_empty() { return false };

        for idx in mask.into_iter().rev() {
            self.instructions.remove(idx);
        }

        true
    }

    fn double_returns(&mut self) -> bool {
        let mut mask = Vec::new();
        let mut it = self.instructions.iter().enumerate().peekable();

        while let Some((_, ins)) = it.next() {
            if let (Instruction::Ret, Some((idx, Instruction::Ret))) = (ins, it.peek()) {
                mask.push(*idx);
            }
        }

        self.remove_with_mask(mask)
    }

    fn useless_stores(&mut self) -> bool {
        let mut mask = Vec::new();

        let mut it = self.instructions.iter_mut().enumerate().peekable();
        while let Some((_, ins)) = it.next() {
            if let Instruction::StoreOperation(temp, _)
            | Instruction::StoreComparison(temp, _)
            | Instruction::VariableStore(temp, Value::LastCall { .. }) = ins
            {
                if let Some((idx, Instruction::VariableStore(var, Value::VariableLoad(load)))) = it.peek() {
                    if load == temp {
                        *temp = *var;
                        mask.push(*idx);
                    }
                }
            }
        }

        self.remove_with_mask(mask)
    }

    fn unused_variables(&mut self) -> bool {
        let mut used: HashMap<_, _> = self.variables.keys().map(|k| (k, false)).collect();

        for ins in &self.instructions {
            for key in ins.used_variables() { 
                *used.get_mut(&key).unwrap() = true;
            };

        }

        let mut changed = false;
        let mut changed_last = false;
        for (key, used) in used.into_iter() {
            if !used {
                self.variables.remove(key);
                if let Some(last) = self.last_variable {
                    if last == key {
                        changed_last = true;
                        self.last_variable = None;
                    }
                }
                changed = true;
            }
        }

        // Search new last
        if changed_last {
            self.last_variable = self.variables.iter()
                .filter(|(_, o)| !o.argument)
                .max_by_key(|(_, o)| o.total_offset)
                .map(|(key, _)| key)
        }

        changed
    }


    pub fn optimize(&mut self) {

        self.double_returns();

        let mut changed = true;
        while changed {
            changed = false;

            changed |= self.useless_stores();
            changed |= self.unused_variables();
        }
    }
}

impl Ir {
    pub fn optimize(&mut self) {
        for func in &mut self.functions {
            func.optimize();
        }
    }
}
