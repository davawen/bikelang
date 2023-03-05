use std::{fmt::Display, ops::Not};

use itertools::Itertools;

use super::*;
use crate::utility::color::*;

type ReverseNames<'a> = &'a [HashMap<VariableKey, String>];

fn format_variable(var: &VariableId, variable_names: ReverseNames ) -> String {
    fmtools::format!(
        "(" {var.1:?} " "
        if let Some(name) = variable_names[var.0].get(&var.1) {
            {GREEN}"\""{name}"\" "{WHITE}
        }
        ")"
    )
}

impl Address {
    fn format(&self, variable_names: ReverseNames) -> String {
        match self {
            Address::Ptr(ptr, size) => format!("(*{ptr} $ {CYAN}{size} bytes{WHITE})"),
            Address::Variable(var) => format_variable(var, variable_names)
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmtools::write!(f, 
            match self {
                Value::Number(n, size) => "("{ORANGE}{n}{WHITE}" $ "{CYAN}{size}" bytes"{WHITE}")",
                Value::Literal(literal) => "( "{GREEN}"literal "{literal}{WHITE}" )",
                Value::Boolean(b) => ""{ORANGE}{b}{WHITE},
                Value::Register(reg) => ""{reg},
                Value::NoValue => ""
            }
        )
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{PINK}{}{WHITE}", self.as_str().to_uppercase())
    }
}

impl Arithmetic {
    fn format(&self, variable_names: ReverseNames) -> String {
        use Arithmetic::*;
        fmtools::format!(
            match self {
                Add(a, b) => "ADD "{b}" TO "{a},
                Sub(a, b) => "REMOVE "{b}" FROM "{a},
                Mul(a, b) => "MULTIPLY "{a}" BY "{b},
                Div(a, b) => "DIVIDE "{a}" BY "{b},
                Modulus(a, b) => "REMAINDER OF "{a}" BY "{b},
                And(a, b) => "LOGICAL AND "{a}" WITH "{b},
                Or(a, b) => "LOGICAL OR "{a}" WITH "{b},
                Xor(a, b) => "LOGICAL XOR "{a}" WITH "{b},
                Not(a) => "LOGICAL INVERSE "{a},
                Negate(a) => "NEGATE "{a},
                Deref(a, _) => "DEREFERENCE "{a},
                AddressOf(reg, var) => "PUT ADDRESS OF "{format_variable(var, variable_names)}" INTO "{reg}
            }
        )
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Comparison::*;
        fmtools::write!(f, 
            match self {
                Unconditional => "ALWAYS",
                Never => "NEVER",
                NotZero(v) => ""{v}" IS NOT ZERO",
                Zero(v) => ""{v}" IS ZERO",
                Eq(a, b) => ""{a}" == "{b},
                Neq(a, b) => ""{a}" != "{b},
                Gt(a, b) => ""{a}" > "{b},
                Ge(a, b) => ""{a}" >= "{b},
                Lt(a, b) => ""{a}" < "{b},
                Le(a, b) => ""{a}" <= "{b},
            }
        )
    }
}

impl Display for Intrisic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmtools::write!(f, 
            match self {
                Intrisic::Asm(_) => "INLINE ASSEMBLY",
                Intrisic::PrintNumber(n, _) => "PRINT NUMBER "{n},
                Intrisic::PrintString(s) => "PRINT STRING "{s}
            }
        )
    }
}

impl Instruction {
    fn format(&self, functions: &[Function], variable_names: ReverseNames) -> String {
        use Instruction::*;
        match self {
            VariableStore(var, val) => format!("{YELLOW}STORE{WHITE} {val} INTO {}", var.format(variable_names)),
            VariableLoad(reg, var) => format!("{YELLOW}LOAD{WHITE} {} INTO {reg}", var.format(variable_names)),
            Load(reg, val) => format!("{YELLOW}LOAD{WHITE} {val} into {reg}"),
            LoadSignExtend(reg, val) => format!("{YELLOW}LOAD {PURPLE}SIGN-EXTEND{WHITE} {val} into {reg}"),
            Save(reg) => format!("{BLUE}SAVE{WHITE} {reg}"),
            Restore(reg) => format!("{BLUE}RESTORE{WHITE} {reg}"),
            StoreOperation(op) => op.format(variable_names),
            StoreComparison(reg, comp) => format!("{YELLOW}LOAD{WHITE} {comp} into {reg}"),
            Label(idx) => format!("{RED}LABEL {idx}{WHITE}:"),
            Jump(idx, comp) => fmtools::format!(
                {PURPLE}"JUMP"{WHITE}" TO LABEL "{idx}" "
                if matches!(comp, Comparison::Unconditional).not() {
                    {PURPLE}"IF"{WHITE}" "{comp}
                }
            ),
            Intrisic(i) => format!("{PINK}INTRISIC{WHITE} {i}"),
            Call { func, arguments, return_type: _ } => fmtools::format!(
                {BLUE}"CALL "{functions[*func].name}{WHITE}" WITH "
                for p in arguments {
                    {p}", "
                }
            ),
            Ret => format!("{PURPLE}RETURN{WHITE}")
        }
    }
}

impl Display for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        fmtools::write!(f, 
            for func in &self.functions {
                "FUNCTION "{BLUE}{func.name}{WHITE}"\n"
                "  VARIABLES:\n"
                let variables_names = func.scopes.iter().map(|s| {
                    s.named_variables.iter().map(|(a, b)| (*b, a.clone())).collect::<HashMap<_, _>>()
                }).collect_vec();

                for (var, offset) in &func.scopes[0].variables {
                    "    "{GREEN}
                    if let Some(name) = variables_names[0].get(&var) {
                        '"'{name}"\": "
                    }
                    else if Some(var) == func.return_variable {
                        "\"return\": "
                    }
                    else {
                        {var:?}
                    }
                    {WHITE}
                    "size: "{CYAN}{offset.size}{WHITE}", offset: "{CYAN}{offset.offset}
                    if offset.argument {
                        {BLUE}" (argument)"
                    }
                    {WHITE}"\n"
                }
                "  INSTRUCTIONS:\n"
                for (idx, ins) in func.instructions.iter().enumerate() {
                    "  "{idx + 1: >3}" | "{ins.format(&self.functions, &variables_names)}"\n"
                }
            }
            "LITERALS:\n"
            for (idx, literal) in self.literals.iter().enumerate() {
                "  literal "{idx}": "{GREEN}{literal:?}{WHITE}"\n"
            }
        )?;

        Ok(())
    }
}
