use crate::extensions::{
    ConcreteExtension, GenericExtension, NoGenericArgsGenericExtension, SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::program::GenericArg;
use crate::super_extension;

super_extension! {
    pub enum IntegerExtension {
        Operation(OperationGeneric),
        Const(ConstGeneric),
        Ignore(IgnoreGeneric),
        Duplicate(DuplicateGeneric),
        JumpNotZero(JumpNotZeroGeneric),
        UnwrapNonZero(UnwrapNonZeroGeneric)
    }, IntegerConcrete
}

/// Possible operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Extension for operations on integers.
pub struct OperationGeneric {
    operator: Operator,
}
impl GenericExtension for OperationGeneric {
    type Concrete = OperationConcrete;
    fn id() -> Option<GenericExtensionId> {
        None
    }
    fn new() -> Option<Self> {
        None
    }
    fn by_id(id: &GenericExtensionId) -> Option<Self> {
        if id == &"int_add".into() {
            return Some(OperationGeneric { operator: Operator::Add });
        }
        if id == &"int_sub".into() {
            return Some(OperationGeneric { operator: Operator::Sub });
        }
        if id == &"int_mul".into() {
            return Some(OperationGeneric { operator: Operator::Mul });
        }
        if id == &"int_div".into() {
            return Some(OperationGeneric { operator: Operator::Div });
        }
        if id == &"int_mod".into() {
            return Some(OperationGeneric { operator: Operator::Mod });
        }
        None
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [] => {
                Ok(OperationConcrete::Binary(BinaryOperationConcrete { _operator: self.operator }))
            }
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedTemplateArg)
                } else {
                    Ok(OperationConcrete::Const(OperationWithConstConcrete {
                        _operator: self.operator,
                        _c: *c,
                    }))
                }
            }
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

/// Binary int operations.
pub struct BinaryOperationConcrete {
    _operator: Operator,
}
/// Operations between a int and a const.
pub struct OperationWithConstConcrete {
    _operator: Operator,
    _c: i64,
}
pub enum OperationConcrete {
    Binary(BinaryOperationConcrete),
    Const(OperationWithConstConcrete),
}
impl ConcreteExtension for OperationConcrete {}

/// Extension for creating a constant int.
pub struct ConstGeneric {}
impl GenericExtension for ConstGeneric {
    type Concrete = ConstConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_const".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(ConstConcrete { _c: *c }),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

pub struct ConstConcrete {
    _c: i64,
}
impl ConcreteExtension for ConstConcrete {}

/// Extension for ignoring an int.
pub struct IgnoreGeneric {}
impl NoGenericArgsGenericExtension for IgnoreGeneric {
    type Concrete = IgnoreConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_ignore".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        IgnoreConcrete {}
    }
}

pub struct IgnoreConcrete {}
impl ConcreteExtension for IgnoreConcrete {}

/// Extension for duplicating an int.
pub struct DuplicateGeneric {}
impl NoGenericArgsGenericExtension for DuplicateGeneric {
    type Concrete = DuplicateConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_dup".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        DuplicateConcrete {}
    }
}

pub struct DuplicateConcrete {}
impl ConcreteExtension for DuplicateConcrete {}

/// Extension for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
pub struct JumpNotZeroGeneric {}
impl NoGenericArgsGenericExtension for JumpNotZeroGeneric {
    type Concrete = JumpNotZeroConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_jump_nz".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        JumpNotZeroConcrete {}
    }
}

pub struct JumpNotZeroConcrete {}
impl ConcreteExtension for JumpNotZeroConcrete {}

/// Extension for unwrapping a non-zero int back into a regular int.
pub struct UnwrapNonZeroGeneric {}
impl NoGenericArgsGenericExtension for UnwrapNonZeroGeneric {
    type Concrete = UnwrapNonZeroConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("int_unwrap_nz".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        UnwrapNonZeroConcrete {}
    }
}

pub struct UnwrapNonZeroConcrete {}
impl ConcreteExtension for UnwrapNonZeroConcrete {}
