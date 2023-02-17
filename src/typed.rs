use thiserror::Error;

use crate::ast::{Node, UnaryOperation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UInt8,
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
    Mismatched(&'static str, SuperType, Type),
}

pub type Result<T> = std::result::Result<T, TypeError>;

impl Type {
    /// Size of the type in bytes
    pub fn size(&self) -> u32 {
        use Type::*;
        match self {
            UInt8 => 1,
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
            "u8" => Ok(UInt8),
            "i32" => Ok(Integer32),
            "f32" => Ok(Float32),
            "bool" => Ok(Boolean),
            "str" => Ok(String),
            "void" => Ok(Void),
            _ => Err(TypeError::Unknown(name.to_owned()))
        }
    }

    pub fn from_node(node: Node) -> Result<Self> {
        match node {
            Node::Identifier( typename, _) => Self::from_str(&typename),
            Node::UnaryExpr { op: UnaryOperation::Deref, value, .. } => Ok(
                Self::Ptr(Box::new(Self::from_node(*value)?))
            ),
            _ => Err(TypeError::Unknown(format!("{node:?}")))
        }
    }

    pub fn expect(self, expected: SuperType, msg: &'static str) -> Result<Self> {
        if expected.verify(&self) {
            Ok(self)
        } else {
            Err(TypeError::Mismatched(msg, expected, self))
        }
    }

    /// Expectes a type in place
    pub fn expect_ref(&self, expected: SuperType, msg: &'static str) -> Result<()> {
        if expected.verify(self) {
            Ok(())
        } else {
            Err(TypeError::Mismatched(msg, expected, self.clone()))
        }
    }
}

#[derive(Debug, Clone)]
pub enum SuperType {
    As(Type),
    Or(Box<SuperType>, Box<SuperType>),
    Signed,
    Unsigned,
    Integer,
    Float,
    Number,
    Ptr
}

impl From<Type> for SuperType {
    fn from(value: Type) -> Self {
        SuperType::As(value)
    }
}

#[macro_export]
macro_rules! super_type_or {
    ($a:expr, $b:expr) => {
        SuperType::Or(Box::new($a.into()), Box::new($b.into()))
    };
    ($a:expr, $b:expr $(, $more:expr)+) => {
        SuperType::Or(Box::new($a.into()), Box::new(super_type_or!($b $(, $more)+)))
    };
}

impl SuperType {
    pub fn verify(&self, ty: &Type) -> bool {
        use SuperType::*;
        match self {
            As(t)      => t == ty,
            Or(t1, t2) => t1.verify(ty) || t2.verify(ty),
            Signed     => matches!(ty, Type::Integer32),
            Unsigned   => matches!(ty, Type::UInt8),
            Integer    => Signed.verify(ty) || Unsigned.verify(ty),
            Float      => matches!(ty, Type::Float32),
            Number     => Integer.verify(ty) || Float.verify(ty),
            Ptr        => matches!(ty, Type::Ptr(_))
        }
    }
}
