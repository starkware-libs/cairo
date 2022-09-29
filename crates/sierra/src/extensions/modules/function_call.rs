use crate::extensions::lib_func::{
    LibFuncSignature, OutputVarInfo, SierraApChange, SignatureBasedConcreteLibFunc,
    SignatureSpecializationContext, SpecializationContext,
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
                let function = context.get_function_signature_as_result(function_id)?;
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
                    SierraApChange::NotImplemented,
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
                function: context.get_function_as_result(function_id)?,
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
