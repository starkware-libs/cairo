use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{
    args_as_single_type, NamedType, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Type wrapping a value as non zero.
#[derive(Default)]
pub struct NonZeroTypeWrapped {}
impl GenericTypeArgGenericType for NonZeroTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("NonZero");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { zero_sized, storable, droppable, duplicatable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable {
            Ok(TypeInfo { long_id, zero_sized, storable, droppable, duplicatable })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type NonZeroType = GenericTypeArgGenericTypeWrapper<NonZeroTypeWrapped>;

/// Returns the type `NonZero<T>` for a given type `T`.
pub fn nonzero_ty(
    context: &dyn SignatureSpecializationContext,
    ty: &ConcreteTypeId,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_wrapped_concrete_type(NonZeroType::id(), ty.clone())
}

/// Libfunc for unwrapping a `NonZero<T>` back into a T.
#[derive(Default)]
pub struct UnwrapNonZeroLibfunc {}
impl SignatureOnlyGenericLibfunc for UnwrapNonZeroLibfunc {
    const STR_ID: &'static str = "unwrap_non_zero";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![nonzero_ty(context, &ty)?],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
