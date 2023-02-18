use thiserror::Error;

use crate::ast::{Node, UnaryOperation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UInt8,
    Integer32,
    Float32,
    Boolean,
    Ptr(Box<Type>),
    Void,
    // For later :)
    // Struct(TypeIndex)
}

#[derive(Debug, Clone)]
pub struct TypeDescriptor {
    pub ty: Type,
    pub has_address: bool
}

impl PartialEq for TypeDescriptor {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}
impl Eq for TypeDescriptor {}

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("Unknown type {0} used.")]
    Unknown(String),
    #[error("Expected type {1:?}, got {2:?}: {0}")]
    Mismatched(&'static str, SuperType, Type),
    #[error("Invalid operation on {1:?}: {0}")]
    InvalidOperation(&'static str, Type)
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
            Ptr(_) => 8,
            Void => 0
        }
    }

    pub fn from_str(name: &str) -> Result<Self> {
        use Type::*;
        let ty = match name {
            "u8" => UInt8,
            "i32" => Integer32,
            "f32" => Float32,
            "bool" => Boolean,
            "str" => Self::string(),
            "void" => Void,
            _ => return Err(TypeError::Unknown(name.to_owned()))
        };
        Ok(ty)
    }

    pub fn from_node(node: Node) -> Result<Self> {
        match node {
            Node::Identifier( typename, _) => Self::from_str(&typename),
            Node::UnaryExpr { op: UnaryOperation::Deref, value, .. } => Ok(
                Type::Ptr(box Self::from_node(*value)?)
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

    pub fn ptr(ty: Type) -> Self {
        Type::Ptr(box ty)
    }
    pub fn string() -> Self {
        Self::ptr(Type::UInt8)
    }

    pub fn addressable(self) -> TypeDescriptor {
        TypeDescriptor::from(self).addressable()
    }
}

impl TypeDescriptor {
    pub fn new(ty: Type, has_address: bool) -> Self {
        Self { ty, has_address }
    }

    pub fn addressable(mut self) -> Self {
        self.has_address = true;
        self
    }

    pub fn expect(self, expected: SuperType, msg: &'static str) -> Result<Self> {
        if expected.verify(&self.ty) {
            Ok(self)
        } else {
            Err(TypeError::Mismatched(msg, expected, self.ty))
        }
    }

    pub fn expect_ref(&self, expected: SuperType, msg: &'static str) -> Result<()> {
        self.ty.expect_ref(expected, msg)
    }
}

impl From<Type> for TypeDescriptor {
    fn from(ty: Type) -> Self {
        Self {
            ty,
            has_address: false
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
        SuperType::Or(box $a.into(), box $b.into())
    };
    ($a:expr, $b:expr $(, $more:expr)+) => {
        SuperType::Or(box $a.into(), box super_type_or!($b $(, $more)+))
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
