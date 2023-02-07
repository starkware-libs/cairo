use std::marker::PhantomData;

use super::non_zero::nonzero_ty;
use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::GenericTypeId;

/// Trait for implementing a IsZero library function for a type.
pub trait IsZeroTraits: Default {
    /// The is_zero library function id.
    const IS_ZERO: &'static str;
    /// The id of the generic type to implement the library functions for.
    const GENERIC_TYPE_ID: GenericTypeId;
}

/// Libfunc for checking whether the given value is zero or not, and returning a non-zero wrapped
/// value in case of success.
#[derive(Default)]
pub struct IsZeroLibfunc<TIsZeroTraits: IsZeroTraits> {
    _phantom: PhantomData<TIsZeroTraits>,
}
impl<TIsZeroTraits: IsZeroTraits> NoGenericArgsGenericLibfunc for IsZeroLibfunc<TIsZeroTraits> {
    const STR_ID: &'static str = TIsZeroTraits::IS_ZERO;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TIsZeroTraits::GENERIC_TYPE_ID, &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(ty.clone())],
            branch_signatures: vec![
                // Zero.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // NonZero.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: nonzero_ty(context, &ty)?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
