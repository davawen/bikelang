use itertools::Itertools;

use super::*;

fn variable_operand(func: &Function, idx: VariableIndex) -> String {
    let var = &func.variables[idx];
    format!(
    "{} [rbp-{}]",
    match var.size {
        1 => "BYTE",
        2 => "WORD",
        4 => "DWORD",
        8 => "QWORD",
        _ => unreachable!("Invalid word size")
    },
    var.total_offset)
}

#[allow(unused)]
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
            _ => todo!("value blegh: {self:#?}")
        }
    }
}

// Intrisics will put their value into rax
impl Arithmetic {
    /// Returns the code the intrisic will do and the output register that was used
    fn generate_asm(&self, func: &Function) -> (&'static str, String) {
        use Arithmetic::*;
        match self {
            Add(lhs, rhs) | Sub(lhs, rhs) | Mul(lhs, rhs) => {
                let rax = Register::Rax.as_str(lhs.size(func));

                let op = match self {
                    Add(..) => "add",
                    Sub(..) => "sub",
                    Mul(..) => "imul",
                    _ => unreachable!()
                };

                (rax, format!(
                    "mov {rax}, {}\n{op} {rax}, {}\n",
                    lhs.as_operand(func),
                    rhs.as_operand(func)
                ))
            },
            Div(lhs, rhs) => {
                let rax = Register::Rax.as_str(lhs.size(func));

                // Signed dived of rdx:rax = make sure rdx is null
                (rax, format!(
                    "mov {rax}, {}\nxor rdx, rdx\nidivq {}",
                    lhs.as_operand(func),
                    rhs.as_operand(func)
                ))
            },
        }
    }
}

impl Intrisic {
    fn generate_asm(&self, ir: &Ir, func: &Function) -> String {
        use Intrisic::*;
        match self {
            Asm(Value::Literal(inner)) => {
                ir.literals[*inner].clone()
            }
            PrintNumber(v) => {
                let rax = Register::Rax.as_str(v.size(func));
                format!("mov {rax}, {}\ncall __builtin_print_number4", v.as_operand(func))
            },
            PrintString(Value::Literal(idx)) => {
                let literal = &ir.literals[*idx];
                format!("
mov rax, 1
mov rdi, 1
mov rsi, user_str{}
mov rdx, {}
syscall
                    ",
                    idx,
                    literal.len()
                )
            }
            _ => todo!("Intrisics not all set lol: {self:#?}")
        }
    }
}

impl Instruction {
    fn generate_asm(&self, ir: &Ir, func: &Function) -> String {
        match self {
            Instruction::VariableStore(idx, value) => {
                if let Value::VariableLoad(_) = value {
                    let reg = Register::Rax.as_str(value.size(func));
                    format!("mov {reg}, {}\nmov {}, {reg}\n", value.as_operand(func), variable_operand(func, *idx))
                } else {
                    format!("mov {}, {}\n", variable_operand(func, *idx), value.as_operand(func))
                }
            },
            Instruction::StoreOperation(idx, op) => {
                let (reg, code) = op.generate_asm(func);
                format!("{code}\nmov {}, {reg}\n", variable_operand(func, *idx))
            },
            Instruction::Intrisic(i) => {
                i.generate_asm(ir, func)
            }
            _ => unreachable!("instructions not all set lmao: {self:#?}")
        }
    }
}

impl Function {
    fn generate_asm(&self, ir: &Ir) -> String {
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
            let ins = ins.generate_asm(ir, self);
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
    pub fn builtins(&self) -> String {
        let mut out = String::new();

        for size in [1, 2, 4, 8] {
            let rax = Register::Rax.as_str(size);
            let rdx = Register::Rdx.as_str(size);
            let r8 = Register::R8.as_str(size);
            out.push_str(&format!("
__builtin_print_number{size}:
    mov rcx, 0
    .loop:
        xor {rdx}, {rdx} ; nullify rdx

        mov {r8}, 10
        idiv {r8} ; divide assembled register {rdx}:{rax} by {r8}
        ; quotient goes in {rax}, remainder goes in {rdx}

        add {rdx}, 0x30 ; offset remainder to ASCII '0'

        ; push printable remainder to stack
        sub rsp, 1
        mov BYTE [rsp], dl ; ASCII characters are one byte wide, so we only want that

        ; continue until remaining number is 0
        inc rcx
        cmp rax, 0
        jnz .loop

    push rcx ; save rcx for cleanup

    ; the stack grows downwards, so the characters pushed in reverse order are now in the good order
    lea rsi, [rsp+8] 
    mov rdx, rcx
    mov rax, 1
    mov rdi, 1
    syscall

    pop rcx
    add rsp, rcx ; cleanup
    ret
"));
        }

        out
    }

    pub fn generate_asm(&self) -> String {
        let mut out = String::new();

        for func in &self.functions {
            out.push_str(&func.generate_asm(self));
            out.push('\n');
        }

        out
    }

    pub fn generate_full(&self) -> String {
        let mut data = String::new();
        for (idx, literal) in self.literals.iter().enumerate() {
            data.push_str(&format!(
                "user_str{}: db {}\n",
                idx,
                literal.as_bytes().iter().join(", ")
            ))
        }

        format!(
            "
global _start
section .text
; builtin functions
{}
; user program:
{}
_start:
    call main

    mov rax, 60
    xor rdi, rdi
    syscall ; sys_exit

section .data
{}
",
            self.builtins(),
            self.generate_asm(),
            data
        )
    }
}
