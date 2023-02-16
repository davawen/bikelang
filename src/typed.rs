use thiserror::Error;

use crate::token::{Token, self};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer32,
    Float32,
    Boolean,
    String,
    Ptr(Box<Type>),
    Void,
    // For later :)
    // Struct(TypeIndex)
}

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Unknown type {0} used.")]
    Unknown(String),
    #[error("Expected type {1:?}, got {2:?}: {0}")]
    Mismatched(&'static str, Type, Type),
}

pub type Result<T> = std::result::Result<T, TypeError>;

impl Type {
    /// Size of the type in bytes
    pub fn size(&self) -> u32 {
        use Type::*;
        match self {
            Integer32 => 4,
            Float32 => 4,
            Boolean => 1,
            String => 8,
            Ptr(_) => 8,
            Void => 0
        }
    }

    pub fn from_str(name: &str) -> Result<Self> {
        use Type::*;
        match name {
            "i32" => Ok(Integer32),
            "f32" => Ok(Float32),
            "bool" => Ok(Boolean),
            "str" => Ok(String),
            "void" => Ok(Void),
            _ => Err(TypeError::Unknown(name.to_owned()))
        }
    }

    pub fn from_tokens(tokens: &[Token]) -> Result<Self> {
        match tokens {
            [Token::Word(typename)] => Self::from_str(typename),
            [Token::Op(token::Operation::Mul), inner @ ..] => Ok(
                Self::Ptr(Box::new(Self::from_tokens(inner)?))
            ),
            _ => Err(TypeError::Unknown(format!("{tokens:?}")))
        }
    }

    pub fn expect(self, expected: Self, msg: &'static str) -> Result<Self> {
        if self == expected {
            Ok(self)
        } else {
            Err(TypeError::Mismatched(msg, expected, self))
        }
    }
}
