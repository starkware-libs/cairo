use super::as_single_type;
use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureOnlyConcreteLibFunc,
    SignatureSpecializationContext, SignatureSpecializationContextEx, SpecializationContext,
};
use crate::extensions::{NamedLibFunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::GenericLibFuncId;
use crate::program::GenericArg;

/// LibFunc for duplicating an object.
#[derive(Default)]
pub struct DuplicateLibFunc {}
impl NamedLibFunc for DuplicateLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("dup");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        generic_args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = as_single_type(generic_args)?;
        let info = context.get_type_info_as_result(ty.clone())?;
        if info.duplicatable {
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
