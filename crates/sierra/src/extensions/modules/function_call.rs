use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{NamedLibFunc, NonBranchConcreteLibFunc, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericLibFuncId};
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
                Ok(Self::Concrete { function: function.clone() })
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct FunctionCallConcreteLibFunc {
    pub function: Function,
}
impl NonBranchConcreteLibFunc for FunctionCallConcreteLibFunc {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        self.function.params.iter().map(|p| p.ty.clone()).collect()
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        self.function.ret_types.clone()
    }
}
