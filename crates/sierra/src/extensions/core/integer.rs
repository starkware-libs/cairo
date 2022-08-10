use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtension, GenericExtensionBox,
    NoGenericArgsGenericExtension, SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::program::GenericArg;

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
struct OperationGeneric {
    operator: Operator,
}
impl GenericExtension for OperationGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [] => Ok(Box::new(BinaryOperationConcrete { _operator: self.operator })),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedTemplateArg)
                } else {
                    Ok(Box::new(OperationWithConstConcrete { _operator: self.operator, _c: *c }))
                }
            }
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

/// Binary int operations.
struct BinaryOperationConcrete {
    _operator: Operator,
}
impl ConcreteExtension for BinaryOperationConcrete {}

/// Operations between a int and a const.
struct OperationWithConstConcrete {
    _operator: Operator,
    _c: i64,
}
impl ConcreteExtension for OperationWithConstConcrete {}

/// Extension for creating a constant int.
struct ConstGeneric {}
impl GenericExtension for ConstGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(Box::new(ConstConcrete { _c: *c })),
            _ => Err(SpecializationError::UnsupportedTemplateArg),
        }
    }
}

struct ConstConcrete {
    _c: i64,
}
impl ConcreteExtension for ConstConcrete {}

/// Extension for ignoring an int.
struct IgnoreGeneric {}
impl NoGenericArgsGenericExtension for IgnoreGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(IgnoreConcrete {})
    }
}

struct IgnoreConcrete {}
impl ConcreteExtension for IgnoreConcrete {}

/// Extension for duplicating an int.
struct DuplicateGeneric {}
impl NoGenericArgsGenericExtension for DuplicateGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(DuplicateConcrete {})
    }
}

struct DuplicateConcrete {}
impl ConcreteExtension for DuplicateConcrete {}

/// Extension for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
struct JumpNotZeroGeneric {}
impl NoGenericArgsGenericExtension for JumpNotZeroGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(JumpNotZeroConcrete {})
    }
}

struct JumpNotZeroConcrete {}
impl ConcreteExtension for JumpNotZeroConcrete {}

/// Extension for unwrapping a non-zero int back into a regular int.
struct UnwrapNonZeroGeneric {}
impl NoGenericArgsGenericExtension for UnwrapNonZeroGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnwrapNonZeroConcrete {})
    }
}

struct UnwrapNonZeroConcrete {}
impl ConcreteExtension for UnwrapNonZeroConcrete {}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 10] {
    [
        ("int_add".into(), Box::new(OperationGeneric { operator: Operator::Add })),
        ("int_sub".into(), Box::new(OperationGeneric { operator: Operator::Sub })),
        ("int_mul".into(), Box::new(OperationGeneric { operator: Operator::Mul })),
        ("int_div".into(), Box::new(OperationGeneric { operator: Operator::Div })),
        ("int_mod".into(), Box::new(OperationGeneric { operator: Operator::Mod })),
        ("int_const".into(), Box::new(ConstGeneric {})),
        ("int_ignore".into(), Box::new(IgnoreGeneric {})),
        ("int_dup".into(), Box::new(DuplicateGeneric {})),
        ("int_jump_nz".into(), Box::new(JumpNotZeroGeneric {})),
        ("int_unwrap_nz".into(), Box::new(UnwrapNonZeroGeneric {})),
    ]
}
