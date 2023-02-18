use std::collections::HashMap;

use slotmap::SlotMap;

use crate::typed;

mod generate;
mod asm;
mod optimize;

#[derive(Debug)]
pub struct Ir {
    functions: Vec<Function>,
    literals: Vec<String>
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
    last_variable: Option<VariableKey>,
    return_variable: Option<VariableKey>,

    instructions: Vec<Instruction>,
    label_num: usize
}

#[derive(Debug)]
enum Instruction {
    VariableStore(Address, Value),
    StoreOperation(Address, Arithmetic),
    StoreComparison(Address, Comparison),
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
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Modulus(Value, Value),
    And(Value, Value),
    Or(Value, Value),
    Xor(Value, Value),
    Not(Value),
    Negate(Value),
    Deref(Value, u32)
}

#[derive(Debug)]
enum Intrisic {
    Asm(Value),
    PrintNumber(Value, u32),
    PrintString(Value)
}

#[derive(Debug)]
enum Value {
    Number(i64, u32),
    Literal(LiteralIndex),
    Boolean(bool),
    VariableLoad(VariableKey),
    LastCall { size: u32 },
    NoValue
}

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
