use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SignatureBasedConcreteLibfunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::{NamedLibfunc, OutputVarReferenceInfo, SpecializationError};
use crate::program::{Function, GenericArg};

/// Libfunc used to call user functions.
#[derive(Default)]
pub struct FunctionCallLibfunc {}
impl NamedLibfunc for FunctionCallLibfunc {
    type Concrete = FunctionCallConcreteLibfunc;
    const STR_ID: &'static str = "function_call";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        match args {
            [GenericArg::UserFunc(function_id)] => {
                let signature = context.get_function_signature(function_id)?;
                let ap_change = context.get_function_ap_change(function_id)?;
                Ok(LibfuncSignature::new_non_branch(
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

pub struct FunctionCallConcreteLibfunc {
    pub function: Function,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for FunctionCallConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
