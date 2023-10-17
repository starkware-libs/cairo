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
        let mut curr_stack_idx = 0;
        let mut get_stack_idx = || {
            let idx = curr_stack_idx;
            curr_stack_idx += 1;
            idx
        };

        match args {
            [GenericArg::UserFunc(function_id)] => {
                let signature = context.get_function_signature(function_id)?;
                let ap_change = context.get_function_ap_change(function_id)?;
                Ok(LibfuncSignature::new_non_branch(
                    signature.param_types.clone(),
                    signature
                        .ret_types
                        .iter()
                        .map(|ty| {
                            Ok(OutputVarInfo {
                                ty: ty.clone(),
                                ref_info: if context.get_type_info(ty.clone())?.zero_sized {
                                    OutputVarReferenceInfo::ZeroSized
                                } else {
                                    OutputVarReferenceInfo::NewTempVar { idx: get_stack_idx() }
                                },
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?,
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
