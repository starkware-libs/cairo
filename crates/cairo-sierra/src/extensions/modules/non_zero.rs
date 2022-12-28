use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyGenericLibFunc,
    SignatureSpecializationContext,
};
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type wrapping a value as non zero.
#[derive(Default)]
pub struct NonZeroTypeWrapped {}
impl GenericTypeArgGenericType for NonZeroTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("NonZero");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if !wrapped_info.storable {
            Err(SpecializationError::UnsupportedGenericArg)
        } else {
            Ok(TypeInfo { long_id, ..wrapped_info })
        }
    }
}
pub type NonZeroType = GenericTypeArgGenericTypeWrapper<NonZeroTypeWrapped>;

/// LibFunc for unwrapping a `NonZero<T>` back into a T.
#[derive(Default)]
pub struct UnwrapNonZeroLibFunc {}
impl SignatureOnlyGenericLibFunc for UnwrapNonZeroLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("unwrap_nz");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibFuncSignature::new_non_branch(
            vec![context.get_wrapped_concrete_type(NonZeroType::id(), ty.clone())?],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
