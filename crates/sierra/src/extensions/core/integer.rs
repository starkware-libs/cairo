use super::mem::DeferredType;
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
    pub enum IntegerType {
        Basic(BasicIntegerType),
        NonZero(NonZeroIntegerType),
    }, IntegerConcreteType
}

/// Type for int.
#[derive(Default)]
pub struct BasicIntegerType {}
impl NoGenericArgsGenericType for BasicIntegerType {
    type Concrete = BasicIntegerConcreteType;
    const NAME: &'static str = "int";
}
#[derive(Default)]
pub struct BasicIntegerConcreteType {}
impl ConcreteType for BasicIntegerConcreteType {}

/// Type for non-zero int.
#[derive(Default)]
pub struct NonZeroIntegerType {}
impl NoGenericArgsGenericType for NonZeroIntegerType {
    type Concrete = NonZeroIntegerConcreteType;
    const NAME: &'static str = "NonZeroInt";
}
#[derive(Default)]
pub struct NonZeroIntegerConcreteType {}
impl ConcreteType for NonZeroIntegerConcreteType {}

define_libfunc_hierarchy! {
    pub enum IntegerLibFunc {
        Operation(OperationLibFunc),
        Const(ConstLibFunc),
        Ignore(IgnoreLibFunc),
        Duplicate(DuplicateLibFunc),
        JumpNotZero(JumpNotZeroLibFunc),
        UnwrapNonZero(UnwrapNonZeroLibFunc),
    }, IntegerConcrete
}

fn get_int_types(
    context: &SpecializationContext<'_>,
) -> Result<(ConcreteTypeId, ConcreteTypeId), SpecializationError> {
    let int_type = context.get_concrete_type(BasicIntegerType::id(), &[])?;
    Ok((int_type.clone(), context.get_wrapped_concrete_type(DeferredType::id(), int_type)?))
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
pub struct OperationLibFunc {
    pub operator: Operator,
}
impl GenericLibFunc for OperationLibFunc {
    type Concrete = OperationConcreteLibFunc;
    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
        if id == &"int_add".into() {
            return Some(OperationLibFunc { operator: Operator::Add });
        }
        if id == &"int_sub".into() {
            return Some(OperationLibFunc { operator: Operator::Sub });
        }
        if id == &"int_mul".into() {
            return Some(OperationLibFunc { operator: Operator::Mul });
        }
        if id == &"int_div".into() {
            return Some(OperationLibFunc { operator: Operator::Div });
        }
        if id == &"int_mod".into() {
            return Some(OperationLibFunc { operator: Operator::Mod });
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
            [] => Ok(OperationConcreteLibFunc::Binary(BinaryOperationConcreteLibFunc {
                operator: self.operator,
                int_type,
                non_zero_int_type: context.get_concrete_type(NonZeroIntegerType::id(), &[])?,
                deferred_int_type,
            })),
            [GenericArg::Value(c)] => {
                if matches!(self.operator, Operator::Div | Operator::Mod) && *c == 0 {
                    Err(SpecializationError::UnsupportedGenericArg)
                } else {
                    Ok(OperationConcreteLibFunc::Const(OperationWithConstConcreteLibFunc {
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

pub struct BinaryOperationConcreteLibFunc {
    pub operator: Operator,
    pub int_type: ConcreteTypeId,
    pub non_zero_int_type: ConcreteTypeId,
    pub deferred_int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for BinaryOperationConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![
            self.int_type.clone(),
            if matches!(self.operator, Operator::Div | Operator::Mod) {
                self.non_zero_int_type.clone()
            } else {
                self.int_type.clone()
            },
        ]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.deferred_int_type.clone()]
    }
}

/// Operations between a int and a const.
pub struct OperationWithConstConcreteLibFunc {
    pub operator: Operator,
    pub c: i64,
    pub int_type: ConcreteTypeId,
    pub deferred_int_type: ConcreteTypeId,
}
define_concrete_libfunc_hierarchy! {
    pub enum OperationConcreteLibFunc {
        Binary(BinaryOperationConcreteLibFunc),
        Const(OperationWithConstConcreteLibFunc),
    }
}

impl NonBranchConcreteLibFunc for OperationWithConstConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.deferred_int_type.clone()]
    }
}

/// LibFunc for creating a constant int.
#[derive(Default)]
pub struct ConstLibFunc {}
impl NamedLibFunc for ConstLibFunc {
    type Concrete = ConstConcreteLibFunc;
    const NAME: &'static str = "int_const";
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => {
                let int_type = context.get_concrete_type(BasicIntegerType::id(), &[])?;
                let deferred_int_type =
                    context.get_concrete_type(DeferredType::id(), &[GenericArg::Type(int_type)])?;
                Ok(ConstConcreteLibFunc { c: *c, deferred_int_type })
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct ConstConcreteLibFunc {
    pub c: i64,
    pub deferred_int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for ConstConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.deferred_int_type.clone()]
    }
}

/// LibFunc for ignoring an int.
#[derive(Default)]
pub struct IgnoreLibFunc {}
impl NoGenericArgsGenericLibFunc for IgnoreLibFunc {
    type Concrete = IgnoreConcreteLibFunc;
    const NAME: &'static str = "int_ignore";
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(IgnoreConcreteLibFunc {
            int_type: context.get_concrete_type(BasicIntegerType::id(), &[])?,
        })
    }
}

pub struct IgnoreConcreteLibFunc {
    pub int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for IgnoreConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
}

/// LibFunc for duplicating an int.
#[derive(Default)]
pub struct DuplicateLibFunc {}
impl NoGenericArgsGenericLibFunc for DuplicateLibFunc {
    type Concrete = DuplicateConcreteLibFunc;
    const NAME: &'static str = "int_dup";

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(DuplicateConcreteLibFunc {
            int_type: context.get_concrete_type(BasicIntegerType::id(), &[])?,
        })
    }
}

pub struct DuplicateConcreteLibFunc {
    pub int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for DuplicateConcreteLibFunc {
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
pub struct JumpNotZeroLibFunc {}
impl NoGenericArgsGenericLibFunc for JumpNotZeroLibFunc {
    type Concrete = JumpNotZeroConcreteLibFunc;
    const NAME: &'static str = "int_jump_nz";

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(JumpNotZeroConcreteLibFunc {
            int_type: context.get_concrete_type(BasicIntegerType::id(), &[])?,
            non_zero_int_type: context.get_concrete_type(NonZeroIntegerType::id(), &[])?,
        })
    }
}

pub struct JumpNotZeroConcreteLibFunc {
    pub int_type: ConcreteTypeId,
    pub non_zero_int_type: ConcreteTypeId,
}
impl ConcreteLibFunc for JumpNotZeroConcreteLibFunc {
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
pub struct UnwrapNonZeroLibFunc {}
impl NoGenericArgsGenericLibFunc for UnwrapNonZeroLibFunc {
    type Concrete = UnwrapNonZeroConcreteLibFunc;
    const NAME: &'static str = "int_unwrap_nz";

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(UnwrapNonZeroConcreteLibFunc {
            int_type: context.get_concrete_type(BasicIntegerType::id(), &[])?,
            non_zero_int_type: context.get_concrete_type(NonZeroIntegerType::id(), &[])?,
        })
    }
}

pub struct UnwrapNonZeroConcreteLibFunc {
    pub int_type: ConcreteTypeId,
    pub non_zero_int_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for UnwrapNonZeroConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.non_zero_int_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.int_type.clone()]
    }
}
