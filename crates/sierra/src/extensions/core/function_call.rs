use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtension, GenericExtensionBox,
    SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::mem_cell::MemCell;
use crate::program::GenericArg;

struct FunctionCallGeneric {}
impl GenericExtension for FunctionCallGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [GenericArg::Func(_)] => Ok(Box::new(FunctionCallConcrete {})),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

struct FunctionCallConcrete {}
impl ConcreteExtension for FunctionCallConcrete {
    fn simulate(
        &self,
        _inputs: Vec<Vec<MemCell>>,
    ) -> Result<(Vec<Vec<MemCell>>, usize), crate::extensions::InputError> {
        unreachable!("simulation of function calls should happen from outside the function call")
    }
}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 1] {
    [("call_function".into(), Box::new(FunctionCallGeneric {}))]
}
