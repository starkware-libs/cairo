use std::marker::PhantomData;

use super::non_zero::NonZeroType;
use crate::extensions::lib_func::{
    BranchReferenceInfo, LibFuncSignature, SignatureOnlyConcreteLibFunc, SpecializationContext,
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

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = context.get_concrete_type(TJumpNotZeroTraits::GENERIC_TYPE_ID, &[])?;
        Ok(SignatureOnlyConcreteLibFunc {
            signature: LibFuncSignature {
                input_types: vec![ty.clone()],
                output_types: vec![
                    // Success:
                    vec![context.get_wrapped_concrete_type(NonZeroType::id(), ty)?],
                    // Failure:
                    vec![],
                ],
                fallthrough: Some(1),
                output_ref_info: vec![
                    // Success:
                    BranchReferenceInfo(vec![OutputVarReferenceInfo::SameAsParam { param_idx: 0 }]),
                    // Failure:
                    BranchReferenceInfo(vec![]),
                ],
            },
        })
    }
}
