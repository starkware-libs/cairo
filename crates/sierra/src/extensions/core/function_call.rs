use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtension, GenericExtensionBox,
    SpecializationError,
};
use crate::program::{GenericArg, GenericExtensionId};

struct FunctionCallGeneric {}
impl GenericExtension for FunctionCallGeneric {
    fn specialize(&self, args: &[GenericArg]) -> Result<ConcreteExtensionBox, SpecializationError> {
        match args {
            [GenericArg::Func(_)] => Ok(Box::new(UnconditionalJumpConcrete {})),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

struct UnconditionalJumpConcrete {}
impl ConcreteExtension for UnconditionalJumpConcrete {}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 1] {
    [("call_function".into(), Box::new(FunctionCallGeneric {}))]
}
