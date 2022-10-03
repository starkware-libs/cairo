use super::as_single_type;
use crate::extensions::lib_func::{
    LibFuncSignature, SierraApChange, SignatureOnlyConcreteLibFunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::{NamedLibFunc, SpecializationError};
use crate::ids::GenericLibFuncId;
use crate::program::GenericArg;

/// LibFunc for ignoring a plain old data object.
#[derive(Default)]
pub struct DropLibFunc {}
impl NamedLibFunc for DropLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("drop");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(generic_args)?;
        let info = context.get_type_info_as_result(ty.clone())?;
        if info.droppable {
            Ok(LibFuncSignature::new_non_branch(vec![ty], vec![], SierraApChange::Known))
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: self.specialize_signature(context.upcast(), generic_args)?,
        })
    }
}
