use crate::extensions::lib_func::{
    LibFuncSignature, SierraApChange, SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
};
use crate::extensions::{args_as_single_type, SpecializationError};
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
        let ty = args_as_single_type(generic_args)?;
        let info = context.get_type_info(ty.clone())?;
        if info.droppable {
            Ok(LibFuncSignature::new_non_branch(
                vec![ty],
                vec![],
                SierraApChange::Known { new_vars_only: true },
            ))
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
