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
                let function = context.get_function_signature(function_id)?;
                Ok(LibFuncSignature::new_non_branch(
                    function.param_types.clone(),
                    function
                        .ret_types
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| OutputVarInfo {
                            ty: ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: i },
                        })
                        .collect(),
                ))
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::UserFunc(function_id)] => {
                let function = context
                    .functions
                    .get(function_id)
                    .ok_or_else(|| SpecializationError::MissingFunction(function_id.clone()))?;
                Ok(Self::Concrete {
                    function: function.clone(),
                    signature: self.specialize_signature(&context, args)?,
                })
            }
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
