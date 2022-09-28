use super::as_single_type;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyConcreteLibFunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::types::{TypeInfo, TypeSpecializationContext};
use crate::extensions::{
    ConcreteType, NamedLibFunc, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type wrapping a value as non zero.
#[derive(Default)]
pub struct NonZeroType {}
impl NamedType for NonZeroType {
    type Concrete = NonZeroConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("NonZero");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(NonZeroConcreteType { info: context.get_type_info_as_result(ty.clone())?, ty })
    }
}
pub struct NonZeroConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}
impl ConcreteType for NonZeroConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

/// LibFunc for unwrapping a NonZero<T> back into a T.
#[derive(Default)]
pub struct UnwrapNonZeroLibFunc {}
impl NamedLibFunc for UnwrapNonZeroLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("unwrap_nz");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![context.get_wrapped_concrete_type(NonZeroType::id(), ty.clone())?],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known,
        ))
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc { signature: self.specialize_signature(&context, args)? })
    }
}
