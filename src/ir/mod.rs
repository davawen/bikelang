use std::collections::HashMap;

use enum_map::{EnumMap, Enum};
use slotmap::SlotMap;

use crate::typed;

mod generate;
mod asm;
mod format;
// mod optimize;

#[derive(Debug)]
pub struct Ir {
    functions: Vec<Function>,
    literals: Vec<String>,
    used_registers: EnumMap<RegisterKind, bool>
}

type FunctionIndex = usize;
type VariableKey = slotmap::DefaultKey;
type LiteralIndex = usize;
type LabelIndex = usize;

#[derive(Debug)]
struct Function {
    name: String,
    variables: SlotMap<VariableKey, VariableOffset>,
    named_variables: HashMap<String, VariableKey>,
    /// Last variable added to the `variables` slotmap
    last_variable: Option<VariableKey>,
    return_variable: Option<VariableKey>,

    instructions: Vec<Instruction>,
    label_num: usize
}

#[derive(Debug)]
enum Instruction {
    /// Store value into either a memory address or a stack variable
    VariableStore(Address, Value),
    /// Load the value from a stack variable or a memory address into a register
    VariableLoad(Register, Address),
    /// Copies a value into a register
    Load(Register, Value),
    /// Saves the immediate value of a register to the top of the stack
    Save(Register),
    /// Restores the immediate value of a register at the top of the stack
    Restore(Register),
    /// Stores the result of the operation in the first operand (which MUST BE A REGISTER)
    StoreOperation(Arithmetic),
    StoreComparison(Register, Comparison),
    Label(LabelIndex),
    Jump(LabelIndex, Comparison),
    Intrisic(Intrisic),
    Call {
        func: FunctionIndex,
        parameters: Vec<Value>,
        return_type: typed::Type
    },
    Ret
}

#[derive(Debug)]
enum Address {
    Variable(VariableKey),
    Ptr(Value, u32)
}

impl From<VariableKey> for Address {
    fn from(value: VariableKey) -> Self {
        Address::Variable(value)
    }
}

#[derive(Debug)]
enum Comparison {
    Unconditional,
    Never,
    NotZero(Value),
    Zero(Value),
    Eq(Value, Value),
    Neq(Value, Value),
    Gt(Value, Value),
    Ge(Value, Value),
    Lt(Value, Value),
    Le(Value, Value)
}

#[derive(Debug)]
enum Arithmetic {
    Add(Register, Value),
    Sub(Register, Value),
    Mul(Register, Value),
    Div(Register, Value),
    Modulus(Register, Value),
    And(Register, Value),
    Or(Register, Value),
    Xor(Register, Value),
    Not(Register),
    Negate(Register),
    Deref(Register, u32),
    AddressOf(Register, VariableKey)
}

#[derive(Debug)]
enum Intrisic {
    Asm(Value),
    PrintNumber(Value, typed::Type),
    PrintString(Value)
}

#[derive(Debug, Clone, Copy)]
enum Value {
    Number(i64, u32),
    Literal(LiteralIndex),
    Boolean(bool),
    Register(Register),
    NoValue
}

#[allow(unused)]
#[derive(Debug, Clone, Copy, Enum)]
enum RegisterKind {
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

#[derive(Debug, Clone, Copy)]
struct Register {
    pub kind: RegisterKind,
    pub size: u32
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum Scope {
    Block,
    If { end_label: LabelIndex },
    Loop { start_label: LabelIndex, end_label: LabelIndex }
}

#[derive(Debug, Clone, Copy)]
pub struct VariableOffset {
    /// The size of this variable in bytes
    pub size: u32,
    /// The total offset of this variable from the stack base
    pub total_offset: u32,
    /// Wether this variable is an argument (is it stored in this stack frame or in the parent one)
    pub argument: bool
}

impl Value {
    fn size(&self) -> u32 {
        match self {
            &Value::Number(_, size) => size,
            Value::Boolean(_) => 1,
            &Value::Register(Register { size, .. }) => size,
            Value::NoValue | Value::Literal(_) => todo!("value blegh: {self:#?}")
        }
    }
}

impl RegisterKind {
    fn with_size(self, size: u32) -> Register {
        Register { kind: self, size }
    }
}
