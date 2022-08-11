use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, ConcreteTypeRegistry, FunctionRegistry,
    GenericExtension, GenericExtensionBox, SpecializationError,
};
use crate::ids::GenericExtensionId;
use crate::mem_cell::MemCell;
use crate::program::{Function, GenericArg};

struct FunctionCallGeneric {}
impl GenericExtension for FunctionCallGeneric {
    fn specialize(
        &self,
        function_registry: &FunctionRegistry,
        type_registry: &ConcreteTypeRegistry,
        args: &[GenericArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [GenericArg::Func(id)] => {
                let function = function_registry
                    .get(id)
                    .cloned()
                    .ok_or_else(|| SpecializationError::UsedUnregisteredFunction(id.clone()))?;
                let mut input_type_sizes = vec![];
                for param in &function.params {
                    input_type_sizes.push(
                        type_registry.get(&param.ty).map(|info| info.size).ok_or_else(|| {
                            SpecializationError::UsedUnregisteredType(param.ty.clone())
                        })?,
                    )
                }
                let mut output_type_sizes = vec![];
                for ty in &function.ret_types {
                    output_type_sizes.push(
                        type_registry
                            .get(ty)
                            .map(|info| info.size)
                            .ok_or_else(|| SpecializationError::UsedUnregisteredType(ty.clone()))?,
                    )
                }
                Ok(Box::new(FunctionCallConcrete {
                    _function: function,
                    _input_type_sizes: input_type_sizes,
                    _output_type_sizes: output_type_sizes,
                }))
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

struct FunctionCallConcrete {
    _function: Function,
    _input_type_sizes: Vec<usize>,
    _output_type_sizes: Vec<usize>,
}
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
