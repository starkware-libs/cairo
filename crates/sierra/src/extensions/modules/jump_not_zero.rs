use std::marker::PhantomData;

use super::non_zero::NonZeroType;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputBranchInfo, OutputVarInfo, SierraApChange,
    SignatureOnlyConcreteLibFunc, SignatureSpecializationContext, SpecializationContext,
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
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = TJumpNotZeroTraits::JUMP_NOT_ZERO;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = context.get_concrete_type_as_result(TJumpNotZeroTraits::GENERIC_TYPE_ID, &[])?;
        Ok(LibFuncSignature {
            input_types: vec![ty.clone()],
            output_info: vec![
                // Success:
                OutputBranchInfo {
                    vars: vec![OutputVarInfo {
                        ty: context.get_wrapped_concrete_type(NonZeroType::id(), ty)?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known,
                },
                // Failure:
                OutputBranchInfo { vars: vec![], ap_change: SierraApChange::Known },
            ],
            fallthrough: Some(1),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: <Self as NoGenericArgsGenericLibFunc>::specialize_signature(
                self,
                context.upcast(),
            )?,
        })
    }
}
