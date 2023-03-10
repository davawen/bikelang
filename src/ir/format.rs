use std::{fmt::Display, ops::Not};

use super::*;
use crate::utility::color::*;

fn format_variable(var: &VariableOffset) -> String {
    fmtools::format!{
        "( " 
        if var.argument {
            "BASE + "{var.offset}
        } else {
            "BASE - "{var.offset + var.size}
        }
        " )"
    }
    // fmtools::format!(
    //     "(" {var:?} " "
    //     if let Some(name) = variable_names[var.0].get(&var.1) {
    //         {GREEN}"\""{name}"\" "{WHITE}
    //     }
    //     ")"
    // )
}

// fn format_scope(scope: &Scope, scopes: &[Scope]) -> String {
//     scope.variables
// }

impl Address {
    fn format(&self) -> String {
        match self {
            Address::Ptr(ptr, size) => format!("(*{ptr} $ {CYAN}{size} bytes{WHITE})"),
            Address::Variable(var) => format_variable(var)
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
    fn format(&self) -> String {
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
                AddressOf(reg, var) => "PUT ADDRESS OF "{format_variable(var)}" INTO "{reg}
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
    fn format(&self, functions: &[Function]) -> String {
        use Instruction::*;
        match self {
            VariableStore(var, val) => format!("{YELLOW}STORE{WHITE} {val} INTO {}", var.format()),
            VariableLoad(reg, var) => format!("{YELLOW}LOAD{WHITE} {} INTO {reg}", var.format()),
            Load(reg, val) => format!("{YELLOW}LOAD{WHITE} {val} into {reg}"),
            LoadSignExtend(reg, val) => format!("{YELLOW}LOAD {PURPLE}SIGN-EXTEND{WHITE} {val} into {reg}"),
            Save(reg) => format!("{BLUE}SAVE{WHITE} {reg}"),
            Restore(reg) => format!("{BLUE}RESTORE{WHITE} {reg}"),
            StoreOperation(op) => op.format(),
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

        fmtools::write!{f, 
            for func in &self.functions {
                "FUNCTION "{BLUE}{func.name}{WHITE}"\n"
                "  STACK SPACE RESERVED: "
                {func.stack_offset}
                "\n"
                "  INSTRUCTIONS:\n"
                for (idx, ins) in func.instructions.iter().enumerate() {
                    "  "{idx + 1: >3}" | "{ins.format(&self.functions)}"\n"
                }
            }
            "LITERALS:\n"
            for (idx, literal) in self.literals.iter().enumerate() {
                "  literal "{idx}": "
                {GREEN}
                if literal.len() < 40 {
                    {literal:?}
                } else {
                    let (literal, rest) = split_at_nearest_char_boundary(literal, 40);
                    {literal:?} "..."{rest.chars().count()}" more characters"
                }
                {WHITE}
                "\n"
            }
        }?;

        Ok(())
    }
}

/// Splits a given string at the nearest char boundary to the left
///
/// * `s`: input string
/// * `idx`: byte index
fn split_at_nearest_char_boundary(s: &str, mut idx: usize) -> (&str, &str) {
    while !s.is_char_boundary(idx) {
        if idx == 0 { panic!("Invalid utf-8 string given") }

        idx -= 1;
    }

    s.split_at(idx)
}
