use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SignatureBasedConcreteLibFunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::{NamedLibFunc, OutputVarReferenceInfo, SpecializationError};
use crate::ids::GenericLibFuncId;
use crate::program::{Function, GenericArg};

/// LibFunc used to call user functions.
#[derive(Default)]
pub struct FunctionCallLibFunc {}
impl NamedLibFunc for FunctionCallLibFunc {
    type Concrete = FunctionCallConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("function_call");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        match args {
            [GenericArg::UserFunc(function_id)] => {
                let signature = context.get_function_signature(function_id)?;
                let ap_change = context.get_function_ap_change(function_id)?;
                Ok(LibFuncSignature::new_non_branch(
                    signature.param_types.clone(),
                    signature
                        .ret_types
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| OutputVarInfo {
                            ty: ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(i) },
                        })
                        .collect(),
                    ap_change,
                ))
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::UserFunc(function_id)] => Ok(Self::Concrete {
                function: context.get_function(function_id)?,
                signature: self.specialize_signature(context.upcast(), args)?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct FunctionCallConcreteLibFunc {
    pub function: Function,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for FunctionCallConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}
