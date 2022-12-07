use std::marker::PhantomData;

use super::non_zero::NonZeroType;
use crate::extensions::lib_func::{
    BranchSignature, LibFuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibFunc, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Trait for implementing a JumpNotZero library function for a type.
pub trait JumpNotZeroTraits: Default {
    /// The jump not zero library function id.
    const JUMP_NOT_ZERO: GenericLibFuncId;
    /// The id of the generic type to implement the library functions for.
    const GENERIC_TYPE_ID: GenericTypeId;
}

/// LibFunc for jump non-zero on some value, and returning a non-zero wrapped value in case of
/// success.
#[derive(Default)]
pub struct JumpNotZeroLibFunc<TJumpNotZeroTraits: JumpNotZeroTraits> {
    _phantom: PhantomData<TJumpNotZeroTraits>,
}
impl<TJumpNotZeroTraits: JumpNotZeroTraits> NoGenericArgsGenericLibFunc
    for JumpNotZeroLibFunc<TJumpNotZeroTraits>
{
    const ID: GenericLibFuncId = TJumpNotZeroTraits::JUMP_NOT_ZERO;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(TJumpNotZeroTraits::GENERIC_TYPE_ID, &[])?;
        Ok(LibFuncSignature {
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
                        ty: context.get_wrapped_concrete_type(NonZeroType::id(), ty)?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
