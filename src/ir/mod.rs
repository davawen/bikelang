use thiserror::Error;

use crate::{ast, token::{self, Operation}, analysis, utility::PushIndex};

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
}

#[derive(Debug)]
enum Instruction {
    VariableStore(VariableIndex, Value),
    StoreIntrisic(VariableIndex, Intrisic),
    Intrisic(Intrisic),
    Nop
}

#[derive(Debug)]
enum Intrisic {
    Asm(Value),
    PrintNumber(Value),
    PrintString(Value),
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
}

#[derive(Debug)]
enum Value {
    Number(i32),
    Literal(LiteralIndex),
    VariableLoad(VariableIndex),
    Call {
        func: FunctionIndex,
        parameters: Vec<Value>
    },
    NoValue
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
