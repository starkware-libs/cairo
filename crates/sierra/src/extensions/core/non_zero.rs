use super::as_single_type;
use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, NonBranchConcreteLibFunc, SpecializationError,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

/// Type wrapping a value as non zero.
#[derive(Default)]
pub struct NonZeroType {}
impl NamedType for NonZeroType {
    type Concrete = NonZeroConcreteType;
    const NAME: &'static str = "NonZero";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(NonZeroConcreteType { ty: as_single_type(args)? })
    }
}
pub struct NonZeroConcreteType {
    pub ty: ConcreteTypeId,
}
impl ConcreteType for NonZeroConcreteType {}

/// LibFunc for unwrapping a NonZero<T> back into a T.
#[derive(Default)]
pub struct UnwrapNonZeroLibFunc {}
impl NamedLibFunc for UnwrapNonZeroLibFunc {
    type Concrete = UnwrapNonZeroConcreteLibFunc;
    const NAME: &'static str = "unwrap_nz";

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(UnwrapNonZeroConcreteLibFunc {
            ty: ty.clone(),
            non_zero_ty: context.get_wrapped_concrete_type(NonZeroType::id(), ty)?,
        })
    }
}

pub struct UnwrapNonZeroConcreteLibFunc {
    pub ty: ConcreteTypeId,
    pub non_zero_ty: ConcreteTypeId,
}
impl NonBranchConcreteLibFunc for UnwrapNonZeroConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.non_zero_ty.clone()]
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        vec![self.ty.clone()]
    }
}
