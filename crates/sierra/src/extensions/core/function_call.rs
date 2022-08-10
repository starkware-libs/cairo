use crate::extensions::{ConcreteExtension, NamedExtension, SpecializationError};
use crate::program::GenericArg;

#[derive(Default)]
pub struct FunctionCallGeneric {}
impl NamedExtension for FunctionCallGeneric {
    type Concrete = FunctionCallConcrete;
    const NAME: &'static str = "function_call";
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Func(_)] => Ok(Self::Concrete {}),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct FunctionCallConcrete {}
impl ConcreteExtension for FunctionCallConcrete {}
