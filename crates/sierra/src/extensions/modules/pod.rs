use std::marker::PhantomData;

use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyConcreteLibFunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibFunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Trait for implementing drop and duplicate for a Plain Old Data type.
pub trait PodTraits: Default {
    /// The drop library function id.
    const DROP: GenericLibFuncId;
    /// The duplicate library function id.
    const DUPLICATE: GenericLibFuncId;
    /// The id of the generic type to implement the library functions for.
    const GENERIC_TYPE_ID: GenericTypeId;
}

/// LibFunc for ignoring a plain old data object.
#[derive(Default)]
pub struct DropLibFunc<TPodTraits: PodTraits> {
    _phantom: PhantomData<TPodTraits>,
}
impl<TPodTraits: PodTraits> NoGenericArgsGenericLibFunc for DropLibFunc<TPodTraits> {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = TPodTraits::DROP;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![context.get_concrete_type_as_result(TPodTraits::GENERIC_TYPE_ID, &[])?],
            vec![],
            SierraApChange::Known,
        ))
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

/// LibFunc for duplicating a plain old data object.
#[derive(Default)]
pub struct DuplicateLibFunc<TPodTraits: PodTraits> {
    _phantom: PhantomData<TPodTraits>,
}
impl<TPodTraits: PodTraits> NoGenericArgsGenericLibFunc for DuplicateLibFunc<TPodTraits> {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = TPodTraits::DUPLICATE;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = context.get_concrete_type_as_result(TPodTraits::GENERIC_TYPE_ID, &[])?;
        Ok(LibFuncSignature::new_non_branch(
            vec![ty.clone()],
            vec![
                OutputVarInfo {
                    ty: ty.clone(),
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
                OutputVarInfo {
                    ty,
                    ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                },
            ],
            SierraApChange::Known,
        ))
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
