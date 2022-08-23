use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{NamedLibFunc, NonBranchConcreteLibFunc, SpecializationError};
use crate::ids::ConcreteTypeId;
use crate::program::{Function, GenericArg};

/// LibFunc used to call user functions.
#[derive(Default)]
pub struct FunctionCallGeneric {}
impl NamedLibFunc for FunctionCallGeneric {
    type Concrete = FunctionCallConcrete;
    const NAME: &'static str = "function_call";
    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Func(function_id)] => {
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

pub struct FunctionCallConcrete {
    pub function: Function,
}
impl NonBranchConcreteLibFunc for FunctionCallConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        self.function.params.iter().map(|p| p.ty.clone()).collect()
    }
    fn output_types(&self) -> Vec<ConcreteTypeId> {
        self.function.ret_types.clone()
    }
}
