use crate::extensions::lib_func::{
    LibFuncSignature, SignatureBasedConcreteLibFunc, SpecializationContext,
};
use crate::extensions::{NamedLibFunc, SpecializationError};
use crate::ids::GenericLibFuncId;
use crate::program::{Function, GenericArg};

/// LibFunc used to call user functions.
#[derive(Default)]
pub struct FunctionCallLibFunc {}
impl NamedLibFunc for FunctionCallLibFunc {
    type Concrete = FunctionCallConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("function_call");
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
                    signature: LibFuncSignature::new_non_branch(
                        function.params.iter().map(|p| p.ty.clone()).collect(),
                        function.ret_types.clone(),
                    ),
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
