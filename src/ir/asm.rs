use itertools::Itertools;

use super::*;

fn word_size(size: u32) -> &'static str {
    match size {
        1 => "BYTE",
        2 => "WORD",
        4 => "DWORD",
        8 => "QWORD",
        _ => unreachable!("Invalid word size: {size}")
    }
}

fn variable_operand(func: &Function, key: VariableKey) -> String {
    let var = &func.variables[key];
    format!(
        "{} [rbp{}{}]",
        word_size(var.size),
        if var.argument { '+' } else { '-' },
        var.total_offset
    )
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


// Addresses use r8 as an intermediary register
impl Address {
    fn as_operand(&self, func: &Function) -> (String, String) {
        match self {
            Address::Variable(key) => (String::new(), variable_operand(func, *key)),
            Address::Ptr(v, pointed_size) => {
                let (r8, move_to) = v.move_to(Register::R8, func);
                (move_to + "\n", format!("{} [{r8}]", word_size(*pointed_size)))
            }
        }
    }
}

impl Value {
    fn as_operand(&self, func: &Function) -> String {
        match self {
            &Value::Number(x, size) => format!("{} {x}", word_size(size)),
            &Value::VariableLoad(idx) => variable_operand(func, idx),
            &Value::LastCall { size } => format!("{} [rsp]", word_size(size)),
            &Value::Boolean(x) => if x { "BYTE 1".to_owned() } else { "BYTE 0".to_owned() },
            Value::NoValue | Value::Literal(_) => unreachable!("blegh: {self:#?}")
        }
    }

    fn move_to(&self, reg: Register, func: &Function) -> (&'static str, String) {
        let reg = reg.as_str(self.size(func));
        (reg, format!("mov {reg}, {}", self.as_operand(func)))
    }

    fn is_memory_access(&self) -> bool {
        match self {
            Value::Number(..) | Value::Boolean(_) => false,
            Value::VariableLoad(_) | Value::LastCall { .. } => true,
            Value::NoValue | Value::Literal(_) => unreachable!("{self:#?}")
        }
    }

    fn direct_move_or_pass(&self, destination: String, func: &Function) -> String {
        if self.is_memory_access() {
            let (reg, move_to) = self.move_to(Register::Rax, func);
            format!("{move_to}\nmov {destination}, {reg}\n")
        } else {
            format!("mov {destination}, {}\n", self.as_operand(func))
        }
    }

    fn size(&self, func: &Function) -> u32 {
        match self {
            &Value::Number(_, size) => size,
            Value::VariableLoad(idx) => func.variables[*idx].size,
            Value::Boolean(_) => 1,
            &Value::LastCall { size } => size,
            Value::NoValue | Value::Literal(_) => todo!("value blegh: {self:#?}")
        }
    }
}

// Intrisics will put their value into rax
impl Arithmetic {
    /// Returns the code the intrisic will do and the output register that was used
    fn generate_asm(&self, func: &Function) -> (&'static str, String) {
        use Arithmetic::*;
        match self {
            Add(lhs, rhs) | Sub(lhs, rhs) | Mul(lhs, rhs) | And(lhs, rhs) | Or(lhs, rhs) | Xor(lhs, rhs) => {
                let rax = Register::Rax.as_str(lhs.size(func));

                let op = match self {
                    Add(..) => "add",
                    Sub(..) => "sub",
                    Mul(..) => "imul",
                    And(..) => "and",
                    Or(..) => "or",
                    Xor(..) => "xor",
                    _ => unreachable!()
                };

                (rax, format!(
                    "mov {rax}, {}\n{op} {rax}, {}\n",
                    lhs.as_operand(func),
                    rhs.as_operand(func)
                ))
            }
            Div(lhs, rhs) | Modulus(lhs, rhs)  => {
                let rax = Register::Rax.as_str(lhs.size(func));
                let r8 = Register::R8.as_str(lhs.size(func));

                // Signed dived of rdx:rax = make sure rdx is null
                (rax, format!(
                    "xor rdx, rdx\nmov {rax}, {}\nmov {r8}, {}\nidiv {r8}{}\n",
                    lhs.as_operand(func),
                    rhs.as_operand(func),
                    if matches!(self, Modulus(..)) { "\nmov rax, rdx" } else { "" }
                ))
            }
            Not(v) => {
                let size = v.size(func);
                let rax = Register::Rax.as_str(size);
                (rax, format!(
                    "mov {rax}, {}\nxor {rax}, 1\n", 
                    v.as_operand(func)
                ))
            }
            Negate(v) => {
                let size = v.size(func);
                let rax = Register::Rax.as_str(size);
                (rax, format!(
                    "mov {rax}, {}\nneg {rax}\n",
                    v.as_operand(func)
                ))
            }
            Deref(v, size) => {
                let rax_out = Register::Rax.as_str(*size);
                (rax_out, format!(
                    "mov rax, {}\nmov {rax_out}, {} [rax]\n",
                    v.as_operand(func),
                    word_size(*size)
                ))
            }
            AddressOf(v) => {
                let Value::VariableLoad(v) = v else { unreachable!("Trying to take address of r-value") };
                let rax = Register::Rax.as_str(8);
                (rax, format!("lea rax, {}\n", variable_operand(func, *v)))
            }
        }
    }
}

impl Comparison {
    fn generate_asm(&self, func: &Function) -> String {
        use Comparison::*;
        let (rax, left, comparison) = match self {
            Unconditional | Never => return "".to_owned(),
            NotZero(v) | Zero(v) => {
                let rax = Register::Rax.as_str(v.size(func));
                ( rax, v, format!("test {rax}, {rax}") ) 
            },
            Eq(l, r) | Neq(l, r) | Gt(l, r) | Ge(l, r) | Lt(l, r) | Le(l, r) => {
                let rax = Register::Rax.as_str(l.size(func));
                ( rax, l, format!("cmp {rax}, {}", r.as_operand(func)))
            }
        };

        format!("mov {rax}, {}\n{comparison}", left.as_operand(func))
    }

    fn as_store(&self) -> &'static str {
        use Comparison::*;
        match self {
            NotZero(..) => "setnz",
            Zero(..) => "setz",
            Eq(..) => "sete",
            Neq(..) => "setne",
            Gt(..) => "setg",
            Ge(..) => "setge",
            Lt(..) => "setl",
            Le(..) => "setle",
            Unconditional | Never => unreachable!()
        }
    }

    fn as_jump(&self) -> &'static str {
        use Comparison::*;
        match self {
            Unconditional => "jmp",
            Never => "",
            NotZero(..) => "jnz",
            Zero(..) => "jz",
            Eq(..) => "je",
            Neq(..) => "jne",
            Gt(..) => "jg",
            Ge(..) => "jge",
            Lt(..) => "jl",
            Le(..) => "jle"
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
            PrintNumber(v, size) => {
                let rax = Register::Rax.as_str(v.size(func));
                format!("mov {rax}, {}\ncall __builtin_print_number{size}", v.as_operand(func))
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
            Instruction::VariableStore(var, value) => {
                let (load_var, operand) = var.as_operand(func);
                format!("{load_var}{}", value.direct_move_or_pass(operand, func))
            }
            Instruction::StoreOperation(var, op) => {
                let (reg, code) = op.generate_asm(func);
                let (load_var, operand) = var.as_operand(func);
                format!("{code}{load_var}mov {operand}, {reg}\n")
            }
            Instruction::StoreComparison(var, comp) => {
                let comparison = comp.generate_asm(func);
                let store = comp.as_store();
                let (load_var, operand) = var.as_operand(func);
                format!("{comparison}\n{load_var}{store} {operand}\n", )
            }
            Instruction::Label(label_idx) => {
                format!(".label{label_idx}:")
            }
            Instruction::Jump(label_idx, comp) => {
                let comparison = comp.generate_asm(func);
                let jump = comp.as_jump();
                format!("{comparison}\n{jump} .label{label_idx}\n")
            }
            Instruction::Intrisic(i) => {
                i.generate_asm(ir, func)
            }
            Instruction::Call { func: func_idx, parameters, return_type } => {
                format! {
                    "{}{}\ncall {}\nadd rsp, {}",
                    if return_type.size() > 0 { format!("sub rsp, {}\n", return_type.size()) } else { "".to_owned() },
                    parameters.iter().rev().map(|p| {
                        format!("sub rsp, {}\n{}", p.size(func), p.direct_move_or_pass("[rsp]".to_owned(), func))
                    }).join("\n"),
                    ir.functions[*func_idx].name,
                    parameters.iter().map(|p| p.size(func)).sum::<u32>()
                }
            }
            Instruction::Ret => "mov rsp, rbp\npop rbp\nret\n".to_owned()
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
            self.last_variable.map_or(0, |v| self.variables[v].total_offset)
        );

        for ins in &self.instructions {
            let ins = ins.generate_asm(ir, self);
            for x in ins.split('\n') {
                out.push_str("    ");
                out.push_str(x.trim());
                out.push('\n');
            }
        }

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
            let r9 = Register::R9.as_str(size);
            out.push_str(&format!("
__builtin_print_number{size}:
    mov {r9}, {rax} ; save full number for later
    test {rax}, {rax}
    jns .positive

    neg {rax}

    .positive:
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
    
    test {r9}, {r9}
    jns .positive1
    sub rsp, 1
    mov BYTE [rsp], '-' ; show negative numbers
    inc rcx

    .positive1:
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
