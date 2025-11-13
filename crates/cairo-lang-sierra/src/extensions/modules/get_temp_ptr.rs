//! Libfunc for computing the address of a variable based on type size.
//!
//! This is useful for creating references to variables when ap change is unknown,
//! particularly for Box optimizations where we want to reference the boxed value location.

use crate::extensions::felt252::Felt252Type;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureAndTypeGenericLibfunc,
    SignatureSpecializationContext, WrapSignatureAndTypeGenericLibfunc,
};
use crate::extensions::{NamedType, OutputVarReferenceInfo, SpecializationError};
use crate::ids::ConcreteTypeId;

/// Libfunc for computing address of T by getting ap - |T| and returning it as a felt252.
///
/// This computes the address where a value of type T would be located after
/// the libfunc execution completes. Useful for creating references to variables
/// when ap change is unknown (e.g., for Box optimizations).
#[derive(Default)]
pub struct GetTempPtrLibfuncWrapped {}

impl SignatureAndTypeGenericLibfunc for GetTempPtrLibfuncWrapped {
    const STR_ID: &'static str = "get_temp_ptr";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError> {
        // The actual offset is computed in CASM based on type size
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(Felt252Type::id(), &[])?,
                // The result is a deferred expression since it's computed from ap
                ref_info: OutputVarReferenceInfo::Deferred(
                    crate::extensions::lib_func::DeferredOutputKind::Generic,
                ),
            }],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

pub type GetTempPtrLibfunc = WrapSignatureAndTypeGenericLibfunc<GetTempPtrLibfuncWrapped>;
