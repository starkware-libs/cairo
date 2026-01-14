use crate::extensions::lib_func::{
    LibfuncSignature, ParamSignature, SierraApChange, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext,
};
use crate::extensions::{SpecializationError, args_as_single_type};
use crate::program::GenericArg;

/// Libfunc for ignoring a plain old data object.
#[derive(Default)]
pub struct DropLibfunc {}
impl SignatureOnlyGenericLibfunc for DropLibfunc {
    const STR_ID: &'static str = "drop";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(generic_args)?;
        let info = context.get_type_info(ty)?;
        if info.droppable {
            Ok(LibfuncSignature::new_non_branch_ex(
                vec![ParamSignature {
                    ty: ty.clone(),
                    allow_deferred: true,
                    allow_add_const: true,
                    allow_const: true,
                }],
                vec![],
                SierraApChange::Known { new_vars_only: true },
            ))
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
