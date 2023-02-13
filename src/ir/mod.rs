mod generate;
mod asm;

#[derive(Debug)]
pub struct Ir {
    functions: Vec<Function>,
    literals: Vec<String>
}

#[derive(Debug)]
struct Function {
    name: String,
    variables: Vec<VariableOffset>,
    instructions: Vec<Instruction>,
    label_num: usize
}

#[derive(Debug)]
enum Instruction {
    VariableStore(VariableIndex, Value),
    StoreOperation(VariableIndex, Arithmetic),
    StoreComparison(VariableIndex, Comparison),
    Label(LabelIndex),
    Jump(LabelIndex, Comparison),
    Intrisic(Intrisic)
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
    Not(Value)
}

#[derive(Debug)]
enum Intrisic {
    Asm(Value),
    PrintNumber(Value),
    PrintString(Value)
}

#[derive(Debug)]
enum Value {
    Number(i32),
    Literal(LiteralIndex),
    Boolean(bool),
    VariableLoad(VariableIndex),
    Call {
        func: FunctionIndex,
        parameters: Vec<Value>
    },
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
    pub total_offset: u32
}

type FunctionIndex = usize;
type VariableIndex = usize;
type LiteralIndex = usize;
type LabelIndex = usize;
