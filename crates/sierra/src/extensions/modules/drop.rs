use super::as_single_type;
use crate::extensions::lib_func::{
    LibFuncSignature, SierraApChange, SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
};
use crate::extensions::SpecializationError;
use crate::ids::GenericLibFuncId;
use crate::program::GenericArg;

/// LibFunc for ignoring a plain old data object.
#[derive(Default)]
pub struct DropLibFunc {}
impl SignatureOnlyGenericLibFunc for DropLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("drop");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(generic_args)?;
        let info = context.get_type_info(ty.clone())?;
        if info.droppable {
            Ok(LibFuncSignature::new_non_branch(vec![ty], vec![], SierraApChange::Known(0)))
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
