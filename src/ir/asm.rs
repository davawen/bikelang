use super::*;

fn variable_operand(func: &Function, idx: VariableIndex) -> String {
    let var = &func.variables[idx];
    format!(
    "{} [rbp-{}]",
    match var.size {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
        _ => unreachable!("Invalid word size")
    },
    var.total_offset)
}

#[derive(Debug, Clone, Copy)]
enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
}

impl Register {
    fn as_str(self, size: u32) -> &'static str {
        macro_rules! by_size {
            ($v8:literal $v4:literal $v2:literal $v1: literal) => {
                match size {
                    1 => $v1,
                    2 => $v2,
                    4 => $v4,
                    8 => $v8,
                    _ => unreachable!("Invalid variable size")
                }
            }
        }

        match self {
            Register::Rax => by_size!("rax" "eax" "ax" "al"),
            Register::Rbx => by_size!("rbx" "ebx" "bx" "bl"),
            Register::Rcx => by_size!("rcx" "ecx" "cx" "cl"),
            Register::Rdx => by_size!("rdx" "edx" "dx" "dl"),
            Register::Rsi => by_size!("rsi" "esi" "si" "sil"),
            Register::Rdi => by_size!("rdi" "edi" "di" "dil"),
            Register::Rbp => by_size!("rbp" "ebp" "bp" "bpl"),
            Register::Rsp => by_size!("rsp" "esp" "sp" "spl"),
            Register::R8  => by_size!("r8"  "r8d"  "r8w"  "r8b" ),
            Register::R9  => by_size!("r9"  "r9d"  "r9w"  "r9b" ),
            Register::R10 => by_size!("r10" "r10d" "r10w" "r10b"),
            Register::R11 => by_size!("r11" "r11d" "r11w" "r11b"),
            Register::R12 => by_size!("r12" "r12d" "r12w" "r12b"),
            Register::R13 => by_size!("r13" "r13d" "r13w" "r13b"),
            Register::R14 => by_size!("r14" "r14d" "r14w" "r14b"),
            Register::R15 => by_size!("r15" "r15d" "r15w" "r15b"),
        }
    }
}

impl Value {
    fn as_operand(&self, func: &Function) -> String {
        match self {
            Value::Number(x) => x.to_string(),
            Value::VariableLoad(idx) => variable_operand(func, *idx),
            _ => unreachable!("blegh: {self:#?}")
        }
    }

    fn move_to(&self, func: &Function, reg: Register) -> String {
        format!("mov {}, {}", reg.as_str(self.size(func)), self.as_operand(func))
    }

    fn size(&self, func: &Function) -> u32 {
        match self {
            Value::Number(_) => 4,
            Value::VariableLoad(idx) => func.variables[*idx].size,
            _ => unreachable!("value blegh: {self:#?}")
        }
    }
}

// Intrisics will put their value into rax
impl Intrisic {
    /// Returns the code the intrisic will do and the output register that was used
    fn generate_asm(&self, func: &Function) -> (String, &'static str) {
        match self {
            Intrisic::Add(lhs, rhs) => {
                let l_reg = Register::Rax.as_str(lhs.size(func));
                let r_reg = Register::R8.as_str(lhs.size(func));

                (format!(
                    "mov {l_reg}, {}\nmov {r_reg}, {}\nadd {l_reg}, {r_reg}\n",
                    lhs.as_operand(func),
                    rhs.as_operand(func)
                ), l_reg)
            },
            _ => unreachable!("Intrisics not all set lol: {self:#?}")
        }
    }
}

impl Instruction {
    fn generate_asm(&self, func: &Function) -> String {
        match self {
            Instruction::VariableStore(idx, value) => {
                if let Value::VariableLoad(_) = value {
                    let reg = Register::Rax.as_str(value.size(func));
                    format!("mov {reg}, {}\nmov {}, {reg}\n", value.as_operand(func), variable_operand(func, *idx))
                } else {
                    format!("mov {}, {}\n", variable_operand(func, *idx), value.as_operand(func))
                }
            },
            Instruction::StoreIntrisic(idx, intrisic) => {
                let (code, reg) = intrisic.generate_asm(func);
                format!("{code}\nmov {}, {reg}\n", variable_operand(func, *idx))
            },
            Instruction::Intrisic(i) => {
                i.generate_asm(func).0
            }
            _ => unreachable!("instructions not all set lmao: {self:#?}")
        }
    }
}

impl Function {
    fn generate_asm(&self) -> String {
        let mut out = format!(
"{}:
    ; enter stack frame
    push rbp
    mov rbp, rsp
    sub rsp, {}
\n", 
            self.name,
            self.variables.last().map_or(0, |v| v.total_offset)
        );

        for ins in &self.instructions {
            let ins = ins.generate_asm(self);
            for x in ins.split('\n') {
                out.push_str("    ");
                out.push_str(x.trim());
                out.push('\n');
            }
        }

        out.push_str(
            "
    ; leave stack frame
    mov rsp, rbp
    pop rbp
    ret
"
        );

        out
    }
}

impl Ir {
    pub fn generate_asm(&self) -> String {
        let mut out = String::new();

        for func in &self.functions {
            out.push_str(&func.generate_asm());
            out.push('\n');
        }

        out
    }
}
