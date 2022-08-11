use crate::extensions::{
    ConcreteTypeRegistry, FunctionRegistry, NamedExtension, NonBranchConcreteExtension,
    SpecializationError,
};
use crate::program::{Function, GenericArg};

#[derive(Default)]
pub struct FunctionCallGeneric {}
impl NamedExtension for FunctionCallGeneric {
    type Concrete = FunctionCallConcrete;
    const NAME: &'static str = "function_call";
    fn specialize(
        &self,
        function_registry: &FunctionRegistry,
        type_registry: &ConcreteTypeRegistry,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
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
                Ok(FunctionCallConcrete { function, input_type_sizes, output_type_sizes })
            }
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct FunctionCallConcrete {
    pub function: Function,
    pub input_type_sizes: Vec<usize>,
    pub output_type_sizes: Vec<usize>,
}
impl NonBranchConcreteExtension for FunctionCallConcrete {
    fn input_types(&self) -> Vec<crate::ids::ConcreteTypeId> {
        self.function.params.iter().map(|p| p.ty.clone()).collect()
    }
    fn output_types(&self) -> Vec<crate::ids::ConcreteTypeId> {
        self.function.ret_types.clone()
    }
}
