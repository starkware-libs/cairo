use crate::extensions::{ConcreteLibFunc, NamedLibFunc, SpecializationError};
use crate::ids::FunctionId;
use crate::program::GenericArg;

/// LibFunc used to call user functions.
#[derive(Default)]
pub struct FunctionCallGeneric {}
impl NamedLibFunc for FunctionCallGeneric {
    type Concrete = FunctionCallConcrete;
    const NAME: &'static str = "function_call";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Func(function_id)] => {
                Ok(Self::Concrete { function_id: function_id.clone() })
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct FunctionCallConcrete {
    pub function_id: FunctionId,
}
impl ConcreteLibFunc for FunctionCallConcrete {
    fn input_types(&self) -> Vec<crate::ids::ConcreteTypeId> {
        todo!("implemented when function declaration is looked up")
    }
    fn output_types(&self) -> Vec<Vec<crate::ids::ConcreteTypeId>> {
        todo!("implemented when function declaration is looked up")
    }
    fn fallthrough(&self) -> Option<usize> {
        Some(0)
    }
}
