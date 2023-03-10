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

fn variable_operand(_func: &Function, var: VariableOffset) -> String {
    // let var = &func.scopes[key.0].variables[key.1];
    fmtools::format!(
        {word_size(var.size)} " "
        "[rbp"
        if var.argument {
            '+' {var.offset}
        } else {
            '-' {var.offset + var.size}
        }
        "]"
    )
}

impl Register {
    pub fn as_str(self) -> &'static str {
        macro_rules! by_size {
            ($v8:literal $v4:literal $v2:literal $v1: literal) => {
                match self.size {
                    1 => $v1,
                    2 => $v2,
                    4 => $v4,
                    8 => $v8,
                    _ => unreachable!("Invalid variable size")
                }
            }
        }

        use RegisterKind::*;
        match self.kind {
            Rax => by_size!("rax" "eax" "ax" "al"),
            Rbx => by_size!("rbx" "ebx" "bx" "bl"),
            Rcx => by_size!("rcx" "ecx" "cx" "cl"),
            Rdx => by_size!("rdx" "edx" "dx" "dl"),
            Rsi => by_size!("rsi" "esi" "si" "sil"),
            Rdi => by_size!("rdi" "edi" "di" "dil"),
            Rbp => by_size!("rbp" "ebp" "bp" "bpl"),
            Rsp => by_size!("rsp" "esp" "sp" "spl"),
            R8  => by_size!("r8"  "r8d"  "r8w"  "r8b" ),
            R9  => by_size!("r9"  "r9d"  "r9w"  "r9b" ),
            R10 => by_size!("r10" "r10d" "r10w" "r10b"),
            R11 => by_size!("r11" "r11d" "r11w" "r11b"),
            R12 => by_size!("r12" "r12d" "r12w" "r12b"),
            R13 => by_size!("r13" "r13d" "r13w" "r13b"),
            R14 => by_size!("r14" "r14d" "r14w" "r14b"),
            R15 => by_size!("r15" "r15d" "r15w" "r15b"),
        }
    }
}


// Addresses use r8 as an intermediary register
impl Address {
    fn as_operand(&self, func: &Function) -> String {
        match self {
            Address::Variable(key) => variable_operand(func, *key),
            Address::Ptr(v, pointed_size) => {
                let Value::Register(reg) = v else { unreachable!("Trying to copy value into literal") };
                format!("{1} [{0}]", reg.as_str(), word_size(*pointed_size))
            }
        }
    }
}

impl Value {
    fn as_operand(&self) -> String {
        match self {
            &Value::Number(x, size) => format!("{} {x}", word_size(size)),
            &Value::Boolean(x) => if x { "BYTE 1".to_owned() } else { "BYTE 0".to_owned() },
            &Value::Register(reg) => reg.as_str().to_owned(),
            Value::Literal(idx) => format!("user_str{idx}"),
            Value::NoValue => unreachable!("blegh: {self:#?}")
        }
    }

    // fn direct_move_or_pass(&self, destination: String, func: &Function) -> String {
    //     if self.is_memory_access() {
    //         let (reg, move_to) = self.move_to(Register::Rax, func);
    //         format!("{move_to}\nmov {destination}, {reg}\n")
    //     } else {
    //         format!("mov {destination}, {}\n", self.as_operand(func))
    //     }
    // }
}

impl Arithmetic {
    /// Returns the code the intrisic will do and the output register that was used
    fn generate_asm(&self, func: &Function) -> (&'static str, String) {
        use Arithmetic::*;
        match self {
            Add(lhs, rhs) | Sub(lhs, rhs) | Mul(lhs, rhs) | And(lhs, rhs) | Or(lhs, rhs) | Xor(lhs, rhs) => {
                let op = match self {
                    Add(..) => "add",
                    Sub(..) => "sub",
                    Mul(..) => "imul",
                    And(..) => "and",
                    Or(..) => "or",
                    Xor(..) => "xor",
                    _ => unreachable!()
                };
                let reg = lhs.as_str();
                (reg, format!(
                    "{op} {reg}, {}\n", rhs.as_operand()
                ))
            }
            Div(lhs, rhs) | Modulus(lhs, rhs)  => {
                let size = lhs.size;

                let is_rax = matches!(lhs, Register { kind: RegisterKind::Rax, .. });

                let rax = RegisterKind::Rax.with_size(size).as_str();
                let r8 = RegisterKind::R8.with_size(size).as_str();

                let lhs = lhs.as_str();

                // Signed dived of rdx:rax = make sure rdx is null
                // Save their values to the stack! (could check if they're actually used to remove this step)
                (rax, fmtools::format!(
                    if !is_rax {
                        "push rax\n"
                        "mov "{rax}", "{lhs}"\n"
                    }
                    "push rdx\n"
                    "push r8\n"
                    "xor rdx, rdx\n"
                    "mov "{r8}", "{rhs.as_operand()}"\n"
                    "idiv "{r8}"\n"
                    if let Modulus(..) = self {
                        "mov rax, rdx\n"
                    }
                    "pop r8\n"
                    "pop rdx\n"
                    if !is_rax {
                        "mov "{lhs}", "{rax}"\n"
                        "pop rax\n"
                    }
                ))
            }
            Not(reg) => {
                let reg = reg.as_str();
                (reg, format!( "xor {reg}, 1\n" ))
            }
            Negate(reg) => {
                let reg = reg.as_str();
                (reg, format!( "neg {reg}\n"))
            }
            &Deref(reg, out_size) => {
                let reg_out = reg.kind.with_size(out_size).as_str();
                let reg = reg.as_str();
                (reg_out, format!(
                    "mov {reg_out}, {} [{reg}]\n",
                    word_size(out_size)
                ))
            }
            &AddressOf(reg, var) => {
                let reg = reg.as_str();
                (reg, format!("lea {reg}, {}\n", variable_operand(func, var)))
            }
        }
    }
}

impl Comparison {
    fn generate_asm(&self, func: &Function) -> String {
        use Comparison::*;
        match self {
            Unconditional | Never => "".to_owned(),
            NotZero(v) | Zero(v) => {
                let v = v.as_operand();
                format!("test {v}, {v}")  
            },
            Eq(l, r) | Neq(l, r) | Gt(l, r) | Ge(l, r) | Lt(l, r) | Le(l, r) => {
                let l = l.as_operand();
                let r = r.as_operand();
                format!("cmp {l}, {r}")
            }
        }
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
    fn generate_asm(&self, ir: &Ir) -> String {
        use Intrisic::*;
        match self {
            Asm(Value::Literal(inner)) => {
                ir.literals[*inner].clone()
            }
            PrintNumber(v, ty) => {
                let is_rax = matches!(v, Value::Register(Register{ kind: RegisterKind::Rax, .. }));
                fmtools::format!(
                    // If given value is not rax, need to save and restore its value
                    if !is_rax {
                        let rax = RegisterKind::Rax.with_size(v.size()).as_str();
                        "push rax\n" // save value of rax
                        "mov "{rax}", "{v.as_operand()}"\n"
                    }

                    // Extend size of rax if it's signed
                    if typed::SuperType::Signed.verify(ty) {
                        let size = ty.size();
                        if size == 1 { "cbw\n" }
                        if size <= 2 { "cwde\n" }
                        if size <= 4 { "cdqe\n" }

                        "call __builtin_print_number_signed\n"
                    } else {
                        "call __builtin_print_number_unsigned\n"
                    }
                    if !is_rax {
                        "pop rax\n" // get back value of rax
                    }
                )
            },
            PrintString(Value::Literal(idx)) => {
                let literal = &ir.literals[*idx];
                fmtools::format!(
                    "mov rax, 1\n"
                    "mov rdi, 1\n"
                    "mov rsi, user_str"{idx}"\n"
                    "mov rdx, "{literal.len()}"\n"
                    "syscall\n"
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
                // No memory access in values = no problems c:
                format!("mov {}, {}", var.as_operand(func), value.as_operand())
            }
            Instruction::VariableLoad(reg, var) => {
                format!("mov {}, {}", reg.as_str(), var.as_operand(func))
            }
            Instruction::Load(reg, value) => {
                // mov with 32 bit or greater operands automatically do zero-extend
                if reg.size == value.size() {
                    format!("mov {}, {}", reg.as_str(), value.as_operand())
                } else if value.size() == 4 { 
                    // mov zero extend is weird as shit for 32bit operands fuck this shit fuch life fuck me https://www.felixcloutier.com/x86/movzx
                    format!("mov {}, {}", reg.kind.with_size(4).as_str(), value.as_operand())
                } else {
                    format!("movzx {}, {}", reg.as_str(), value.as_operand())
                }
            }
            Instruction::LoadSignExtend(reg, value) => {
                format!("movsx {}, {}", reg.as_str(), value.as_operand())
            }
            Instruction::Save(reg) => {
                format!("sub rsp, {}\nmov {} [rsp], {}", reg.size, word_size(reg.size), reg.as_str())
            }
            Instruction::Restore(reg) => {
                format!("mov {}, {} [rsp]\n add rsp, {}", reg.as_str(), word_size(reg.size), reg.size)
            }
            Instruction::StoreOperation(op) => {
                op.generate_asm(func).1
            }
            Instruction::StoreComparison(var, comp) => {
                let comparison = comp.generate_asm(func);
                let store = comp.as_store();
                let var = var.as_str();
                format!("{comparison}\n{store} {var}\n")
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
                i.generate_asm(ir)
            }
            Instruction::Call { func: func_idx, arguments, return_type } => {
                let param_size = arguments.iter().map(Value::size).sum::<u32>();
                let return_size = return_type.size();

                let mut offset = param_size;
                let offsets = arguments.iter().map(|v| {
                    offset -= v.size();
                    offset
                }).collect_vec();

                fmtools::format! {
                    "sub rsp, "{param_size + return_size}"\n"
                    for (p, offset) in arguments.iter().zip(&offsets) {
                        let size = p.size();
                        "mov "{word_size(size)}" [rsp + "{offset}"], "
                        {p.as_operand()}"\n"
                    }
                    "call "{ir.functions[*func_idx].name}"\n"
                    "add rsp, "{param_size}"\n"
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
            self.stack_offset
        );

        for (idx, ins) in self.instructions.iter().enumerate() {
            out.push_str(&format!("    ; {}\n", idx + 1));
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

        for signed in [false, true] {
            let name = if signed { "signed" } else { "unsigned" };

            let negate = if signed { "
    mov r9, rax ; save full number for later
    test rax, rax
    jns .positive

    neg rax

    .positive:
"           } else { "" };

            let add_minus = if signed { "
    test r9, r9
    jns .positive1
    sub rsp, 1
    mov BYTE [rsp], '-' ; show negative numbers
    inc rcx

    .positive1:
"           } else { "" };

            out.push_str(&format!("
__builtin_print_number_{name}:
    {negate}
    mov rcx, 0
    .loop:
        xor rdx, rdx ; nullify rdx

        mov r8, 10
        div r8 ; divide assembled register rdx:rax by r8
        ; quotient goes in rax, remainder goes in rdx

        add rdx, 0x30 ; offset remainder to ASCII '0'

        ; push printable remainder to stack
        sub rsp, 1
        mov BYTE [rsp], dl ; ASCII characters are one byte wide, so we only want that

        ; continue until remaining number is 0
        inc rcx
        test rax, rax
        jnz .loop
    
    {add_minus}
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
