use super::as_single_type;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyConcreteLibFunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{NamedLibFunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::GenericLibFuncId;
use crate::program::GenericArg;

/// LibFunc for duplicating an object.
#[derive(Default)]
pub struct DupLibFunc {}
impl NamedLibFunc for DupLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dup");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(generic_args)?;
        let info = context.get_type_info(ty.clone())?;
        if !info.duplicatable {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

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
        generic_args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibFunc {
            signature: self.specialize_signature(context.upcast(), generic_args)?,
        })
    }
}
