use crate::define_libfunc_hierarchy;
use crate::extensions::core::integer::Operator;
use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{
    ConcreteType, GenericLibFunc, NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType,
    NonBranchConcreteLibFunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum FeltLibFunc {
        Operation(FeltOperationLibFunc),
        Duplicate(FeltDuplicateLibFunc),
    }, FeltConcrete
}

fn get_felt_type(
    context: &SpecializationContext<'_>,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt_type = context.get_concrete_type(FeltType::id(), &[])?;
    Ok(felt_type)
}

/// Type for felt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct FeltType {}
impl NoGenericArgsGenericType for FeltType {
    type Concrete = FeltConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("felt");
}
#[derive(Default)]
pub struct FeltConcreteType {}
impl ConcreteType for FeltConcreteType {}

/// Libfunc for operations on felts.
pub struct FeltOperationLibFunc {
    pub operator: Operator,
}
impl GenericLibFunc for FeltOperationLibFunc {
    type Concrete = FeltBinaryOperationConcreteLibFunc;
    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
        const FELT_ADD: GenericLibFuncId = GenericLibFuncId::new_inline("felt_add");
        const FELT_MUL: GenericLibFuncId = GenericLibFuncId::new_inline("felt_mul");
        match id {
            id if id == &FELT_ADD => Some(FeltOperationLibFunc { operator: Operator::Add }),
            id if id == &FELT_MUL => Some(FeltOperationLibFunc { operator: Operator::Mul }),
            _ => None,
        }
    }
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let felt_type = get_felt_type(&context)?;
        match args {
            [] => Ok(FeltBinaryOperationConcreteLibFunc { operator: self.operator, felt_type }),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }
    }
}

pub struct FeltBinaryOperationConcreteLibFunc {
    pub operator: Operator,
    pub felt_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for FeltBinaryOperationConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.felt_type.clone(), self.felt_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.felt_type.clone()]
    }
}

/// LibFunc for duplicating a felt.
#[derive(Default)]
pub struct FeltDuplicateLibFunc {}
impl NoGenericArgsGenericLibFunc for FeltDuplicateLibFunc {
    type Concrete = FeltDuplicateConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("felt_dup");

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(FeltDuplicateConcreteLibFunc { felt_type: get_felt_type(&context)? })
    }
}

pub struct FeltDuplicateConcreteLibFunc {
    pub felt_type: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for FeltDuplicateConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.felt_type.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.felt_type.clone(), self.felt_type.clone()]
    }
}
