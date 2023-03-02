use thiserror::Error;

use crate::ast::{Node, UnaryOperation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UInt8,
    Int8,
    UInt32,
    Int32,
    UInt64,
    Int64,
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
    pub has_address: bool,
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
    #[error("Mismatched size between {1:?}({} bytes) and {2:?}({} bytes): {0}", .1.size(), .2.size())]
    MismatchedSize(&'static str, Type, Type),
    #[error("Invalid operation on {1:?}: {0}")]
    InvalidOperation(&'static str, Type),
}

pub type Result<T> = std::result::Result<T, TypeError>;

impl Type {
    /// Size of the type in bytes
    pub fn size(&self) -> u32 {
        use Type::*;
        match self {
            UInt8 | Int8 => 1,
            UInt32 | Int32 => 4,
            UInt64 | Int64 => 8,
            Float32 => 4,
            Boolean => 1,
            Ptr(_) => 8,
            Void => 0,
        }
    }

    pub fn from_str(name: &str) -> Result<Self> {
        use Type::*;
        let ty = match name {
            "u8" => UInt8,
            "i8" => Int8,
            "u32" => UInt32,
            "i32" => Int32,
            "u64" => UInt64,
            "i64" => Int64,
            "f32" => Float32,
            "bool" => Boolean,
            "str" => Self::string(),
            "void" => Void,
            _ => return Err(TypeError::Unknown(name.to_owned())),
        };
        Ok(ty)
    }

    pub fn from_node(node: Node) -> Result<Self> {
        match node {
            Node::Identifier(typename, _) => Self::from_str(&typename),
            Node::UnaryExpr {
                op: UnaryOperation::Deref,
                value,
                ..
            } => Ok(Type::Ptr(box Self::from_node(value.node)?)),
            _ => Err(TypeError::Unknown(format!("{node:?}"))),
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

    /// Alias for string type: `*u8`
    pub fn string() -> Self {
        Type::UInt8.into_ptr()
    }

    /// Creates a Type::Ptr from to this Type
    pub fn into_ptr(self) -> Self {
        Type::Ptr(box self)
    }
    /// Creates a TypeDescriptor from this Type which allows you to take its address
    pub fn addressable(self) -> TypeDescriptor {
        TypeDescriptor::from(self).addressable()
    }

    pub fn is_convertible_to(&self, dest: &Self) -> Result<()> {
        let integer = SuperType::Integer;

        if integer.verify_both(self, dest) {
            // You can convert numbers to bigger numbers
            if self.size() > dest.size() {
                return Err(TypeError::MismatchedSize("cannot convert number to smaller one", self.clone(), dest.clone()));
            }

            Ok(())
        } else if SuperType::verify_symetric(&Type::UInt64.into(), &SuperType::Ptr, self, dest) {
            // You can convert pointers to u64s and vice-versa
            Ok(())
        } else {
            Err(TypeError::Mismatched(
                "cannot convert types",
                self.clone().into(),
                dest.clone(),
            ))
        }
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
            has_address: false,
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
    Ptr,
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
            As(t) => t == ty,
            Or(t1, t2) => t1.verify(ty) || t2.verify(ty),
            Signed => matches!(ty, Type::Int8 | Type::Int32 | Type::Int64),
            Unsigned => matches!(ty, Type::UInt8 | Type::UInt32 | Type::UInt64),
            Integer => Signed.verify(ty) || Unsigned.verify(ty),
            Float => matches!(ty, Type::Float32),
            Number => Integer.verify(ty) || Float.verify(ty),
            Ptr => matches!(ty, Type::Ptr(_)),
        }
    }

    pub fn verify_any(&self, ty1: &Type, ty2: &Type) -> bool {
        self.verify(ty1) || self.verify(ty2)
    }

    pub fn verify_both(&self, ty1: &Type, ty2: &Type) -> bool {
        self.verify(ty1) && self.verify(ty2)
    }

    /// Checks if `(super1, super2)` matches either `(ty1, ty2)` or `(ty2, ty1)`
    pub fn verify_symetric(super1: &Self, super2: &Self, ty1: &Type, ty2: &Type) -> bool {
        (super1.verify(ty1) && super2.verify(ty2)) || (super1.verify(ty2) && super2.verify(ty1))
    }
}
