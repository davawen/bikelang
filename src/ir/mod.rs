use thiserror::Error;

use crate::{ast, token::{self, Operation}, analysis, utility::PushIndex};

mod generate;

#[derive(Debug)]
pub struct Ir {
    functions: Vec<Function>,
    literals: Vec<String>
}

#[derive(Debug)]
struct Function {
    name: String,
    variables: Vec<Type>,
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
pub struct Type {
    pub size: u32
}

impl From<analysis::Type> for Type {
    fn from(value: analysis::Type) -> Self {
        Self {
            size: value.size()
        }
    }
}

type FunctionIndex = usize;
type VariableIndex = usize;
type LiteralIndex = usize;
