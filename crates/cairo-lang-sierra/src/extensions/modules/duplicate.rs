use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange, SignatureOnlyGenericLibfunc,
    SignatureSpecializationContext,
};
use crate::extensions::{args_as_single_type, OutputVarReferenceInfo, SpecializationError};
use crate::program::GenericArg;

/// Libfunc for duplicating an object.
#[derive(Default)]
pub struct DupLibfunc {}
impl SignatureOnlyGenericLibfunc for DupLibfunc {
    const STR_ID: &'static str = "dup";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(generic_args)?;
        let info = context.get_type_info(ty.clone())?;
        if !info.duplicatable {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let output_info = OutputVarInfo {
            ty: ty.clone(),
            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
        };
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature::new(ty).with_allow_const()],
            vec![output_info.clone(), output_info],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
