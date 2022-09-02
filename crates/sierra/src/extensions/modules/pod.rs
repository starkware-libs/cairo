use std::marker::PhantomData;

use crate::extensions::lib_func::{
    LibFuncSignature, SignatureOnlyConcreteLibFunc, SpecializationContext,
};
use crate::extensions::{NoGenericArgsGenericLibFunc, SpecializationError};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Trait for implementing ignore and duplicate for a Plain Old Data type.
pub trait PodTraits: Default {
    /// The ignore library function id.
    const IGNORE: GenericLibFuncId;
    /// The duplicate library function id.
    const DUPLICATE: GenericLibFuncId;
    /// The id of the generic type to implement the library functions for.
    const GENERIC_TYPE_ID: GenericTypeId;
}

/// LibFunc for ignoring a plain old data object.
#[derive(Default)]
pub struct IgnoreLibFunc<TPodTraits: PodTraits> {
    _phantom: PhantomData<TPodTraits>,
}
impl<TPodTraits: PodTraits> NoGenericArgsGenericLibFunc for IgnoreLibFunc<TPodTraits> {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = TPodTraits::IGNORE;
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: LibFuncSignature::new_non_branch(
                vec![context.get_concrete_type(TPodTraits::GENERIC_TYPE_ID, &[])?],
                vec![],
            ),
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

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = context.get_concrete_type(TPodTraits::GENERIC_TYPE_ID, &[])?;
        Ok(SignatureOnlyConcreteLibFunc {
            signature: LibFuncSignature::new_non_branch(vec![ty.clone()], vec![ty.clone(), ty]),
        })
    }
}
