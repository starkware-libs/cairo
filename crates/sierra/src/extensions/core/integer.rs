use super::mem::DeferredGeneric;
use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{
    ConcreteLibFunc, ConcreteType, GenericLibFunc, NamedLibFunc, NamedType,
    NoGenericArgsGenericLibFunc, NoGenericArgsGenericType, NonBranchConcreteLibFunc,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId};
use crate::program::GenericArg;
use crate::{define_concrete_libfunc_hierarchy, define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum IntegerGenericType {
        Basic(IntegerBasicGeneric),
        NonZero(IntegerNonZeroGeneric),
    }, IntegerConcreteType
}

/// Type for int.
#[derive(Default)]
pub struct IntegerBasicGeneric {}
impl NoGenericArgsGenericType for IntegerBasicGeneric {
    type Concrete = IntegerBasicConcrete;
    const NAME: &'static str = "int";
}
#[derive(Default)]
pub struct IntegerBasicConcrete {}
impl ConcreteType for IntegerBasicConcrete {}

/// Type for non-zero int.
#[derive(Default)]
pub struct IntegerNonZeroGeneric {}
impl NoGenericArgsGenericType for IntegerNonZeroGeneric {
    type Concrete = IntegerNonZeroConcrete;
    const NAME: &'static str = "NonZeroInt";
}
#[derive(Default)]
pub struct IntegerNonZeroConcrete {}
impl ConcreteType for IntegerNonZeroConcrete {}

define_libfunc_hierarchy! {
    pub enum IntegerLibFunc {
        Operation(OperationGeneric),
        Const(ConstGeneric),
        Ignore(IgnoreGeneric),
        Duplicate(DuplicateGeneric),
        JumpNotZero(JumpNotZeroGeneric),
        UnwrapNonZero(UnwrapNonZeroGeneric),
    }, IntegerConcrete
}

fn get_int_types(
    context: &SpecializationContext<'_>,
) -> Result<(ConcreteTypeId, ConcreteTypeId), SpecializationError> {
    let int_type = context.get_concrete_type(IntegerBasicGeneric::id(), &[])?;
    Ok((int_type.clone(), context.get_wrapped_concrete_type(DeferredGeneric::id(), int_type)?))
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

/// Libfunc for operations on integers.
pub struct OperationGeneric {
    pub operator: Operator,
}
impl GenericLibFunc for OperationGeneric {
    type Concrete = OperationConcrete;
    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
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
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (int_type, deferred_int_type) = get_int_types(&context)?;
        match args {
            [] => Ok(OperationConcrete::Binary(BinaryOperationConcrete {
                operator: self.operator,
                int_type,
                deferred_int_type,
            })),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(OperationConcrete::Const(OperationWithConstConcrete {
                        operator: self.operator,
                        c: *c,
                        int_type,
                        deferred_int_type,
                    }))
                }
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct BinaryOperationConcrete {
    pub operator: Operator,
    pub int_type: ConcreteTypeId,
    pub deferred_int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for BinaryOperationConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone(), self.int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.deferred_int_type.clone()]
    }
}

/// Operations between a int and a const.
pub struct OperationWithConstConcrete {
    pub operator: Operator,
    pub c: i64,
    pub int_type: ConcreteTypeId,
    pub deferred_int_type: ConcreteTypeId,
}
define_concrete_libfunc_hierarchy! {
    pub enum OperationConcrete {
        Binary(BinaryOperationConcrete),
        Const(OperationWithConstConcrete),
    }
}

impl NonBranchConcreteLibFunc for OperationWithConstConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.deferred_int_type.clone()]
    }
}

/// LibFunc for creating a constant int.
#[derive(Default)]
pub struct ConstGeneric {}
impl NamedLibFunc for ConstGeneric {
    type Concrete = ConstConcrete;
    const NAME: &'static str = "int_const";
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => {
                let int_type = context.get_concrete_type(IntegerBasicGeneric::id(), &[])?;
                let deferred_int_type = context
                    .get_concrete_type(DeferredGeneric::id(), &[GenericArg::Type(int_type)])?;
                Ok(ConstConcrete { c: *c, deferred_int_type })
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct ConstConcrete {
    pub c: i64,
    pub deferred_int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for ConstConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.deferred_int_type.clone()]
    }
}

/// LibFunc for ignoring an int.
#[derive(Default)]
pub struct IgnoreGeneric {}
impl NoGenericArgsGenericLibFunc for IgnoreGeneric {
    type Concrete = IgnoreConcrete;
    const NAME: &'static str = "int_ignore";
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(IgnoreConcrete { int_type: context.get_concrete_type(IntegerBasicGeneric::id(), &[])? })
    }
}

pub struct IgnoreConcrete {
    pub int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for IgnoreConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// LibFunc for duplicating an int.
#[derive(Default)]
pub struct DuplicateGeneric {}
impl NoGenericArgsGenericLibFunc for DuplicateGeneric {
    type Concrete = DuplicateConcrete;
    const NAME: &'static str = "int_dup";

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(DuplicateConcrete {
            int_type: context.get_concrete_type(IntegerBasicGeneric::id(), &[])?,
        })
    }
}

pub struct DuplicateConcrete {
    pub int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for DuplicateConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone(), self.int_type.clone()]
    }
}

/// LibFunc for jump non-zero on an int's value, and returning a non-zero wrapped int in case of
/// success.
#[derive(Default)]
pub struct JumpNotZeroGeneric {}
impl NoGenericArgsGenericLibFunc for JumpNotZeroGeneric {
    type Concrete = JumpNotZeroConcrete;
    const NAME: &'static str = "int_jump_nz";

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(JumpNotZeroConcrete {
            int_type: context.get_concrete_type(IntegerBasicGeneric::id(), &[])?,
            non_zero_int_type: context.get_concrete_type(IntegerNonZeroGeneric::id(), &[])?,
        })
    }
}

pub struct JumpNotZeroConcrete {
    pub int_type: ConcreteTypeId,
    pub non_zero_int_type: ConcreteTypeId,
}
impl ConcreteLibFunc for JumpNotZeroConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        vec![/* success= */ vec![self.non_zero_int_type.clone()], /* failure= */ vec![]]
    }
    fn fallthrough(&self) -> Option<usize> {
        Some(1)
    }
}

/// LibFunc for unwrapping a non-zero int back into a regular int.
#[derive(Default)]
pub struct UnwrapNonZeroGeneric {}
impl NoGenericArgsGenericLibFunc for UnwrapNonZeroGeneric {
    type Concrete = UnwrapNonZeroConcrete;
    const NAME: &'static str = "int_unwrap_nz";

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(UnwrapNonZeroConcrete {
            int_type: context.get_concrete_type(IntegerBasicGeneric::id(), &[])?,
            non_zero_int_type: context.get_concrete_type(IntegerNonZeroGeneric::id(), &[])?,
        })
    }
}

pub struct UnwrapNonZeroConcrete {
    pub int_type: ConcreteTypeId,
    pub non_zero_int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for UnwrapNonZeroConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.non_zero_int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
}
