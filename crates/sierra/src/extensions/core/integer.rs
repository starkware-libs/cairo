use crate::define_extension_hierarchy;
use crate::extensions::{
    ConcreteExtension, GenericExtension, NamedExtension, NoGenericArgsGenericExtension,
    SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::program::GenericArg;

define_extension_hierarchy! {
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
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

/// Extension for operations on integers.
pub struct OperationGeneric {
    pub operator: Operator,
}
impl GenericExtension for OperationGeneric {
    type Concrete = OperationConcrete;
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
                Ok(OperationConcrete::Binary(BinaryOperationConcrete { operator: self.operator }))
            }
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(OperationConcrete::Const(OperationWithConstConcrete {
                        operator: self.operator,
                        c: *c,
                    }))
                }
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct BinaryOperationConcrete {
    pub operator: Operator,
}
impl ConcreteExtension for BinaryOperationConcrete {}

/// Operations between a int and a const.
pub struct OperationWithConstConcrete {
    pub operator: Operator,
    pub c: i64,
}
pub enum OperationConcrete {
    Binary(BinaryOperationConcrete),
    Const(OperationWithConstConcrete),
}
impl ConcreteExtension for OperationWithConstConcrete {}

impl ConcreteExtension for OperationConcrete {}

/// Extension for creating a constant int.
#[derive(Default)]
pub struct ConstGeneric {}
impl NamedExtension for ConstGeneric {
    type Concrete = ConstConcrete;
    const NAME: &'static str = "int_const";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(ConstConcrete { c: *c }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct ConstConcrete {
    pub c: i64,
}
impl ConcreteExtension for ConstConcrete {}

/// Extension for ignoring an int.
#[derive(Default)]
pub struct IgnoreGeneric {}
impl NoGenericArgsGenericExtension for IgnoreGeneric {
    type Concrete = IgnoreConcrete;
    const NAME: &'static str = "int_ignore";
    fn specialize(&self) -> Self::Concrete {
        IgnoreConcrete {}
    }
}

pub struct IgnoreConcrete {}
impl ConcreteExtension for IgnoreConcrete {}

/// Extension for duplicating an int.
#[derive(Default)]
pub struct DuplicateGeneric {}
impl NoGenericArgsGenericExtension for DuplicateGeneric {
    type Concrete = DuplicateConcrete;
    const NAME: &'static str = "int_dup";

    fn specialize(&self) -> Self::Concrete {
        DuplicateConcrete {}
    }
}

pub struct DuplicateConcrete {}
impl ConcreteExtension for DuplicateConcrete {}

/// Extension for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
#[derive(Default)]
pub struct JumpNotZeroGeneric {}
impl NoGenericArgsGenericExtension for JumpNotZeroGeneric {
    type Concrete = JumpNotZeroConcrete;
    const NAME: &'static str = "int_jump_nz";

    fn specialize(&self) -> Self::Concrete {
        JumpNotZeroConcrete {}
    }
}

pub struct JumpNotZeroConcrete {}
impl ConcreteExtension for JumpNotZeroConcrete {}

/// Extension for unwrapping a non-zero int back into a regular int.
#[derive(Default)]
pub struct UnwrapNonZeroGeneric {}
impl NoGenericArgsGenericExtension for UnwrapNonZeroGeneric {
    type Concrete = UnwrapNonZeroConcrete;
    const NAME: &'static str = "int_unwrap_nz";

    fn specialize(&self) -> Self::Concrete {
        UnwrapNonZeroConcrete {}
    }
}

pub struct UnwrapNonZeroConcrete {}
impl ConcreteExtension for UnwrapNonZeroConcrete {}
