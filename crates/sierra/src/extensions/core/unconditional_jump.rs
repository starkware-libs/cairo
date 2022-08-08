use super::unpack_inputs;
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtensionBox, InputsError,
    NoGenericArgsGenericExtension,
};
use crate::mem_cell::MemCell;
use crate::program::GenericExtensionId;

struct UnconditionalJumpGeneric {}
impl NoGenericArgsGenericExtension for UnconditionalJumpGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnconditionalJumpConcrete {})
    }
}

struct UnconditionalJumpConcrete {}
impl ConcreteExtension for UnconditionalJumpConcrete {
    fn simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<(Vec<Vec<MemCell>>, usize), InputsError> {
        unpack_inputs::<0>(inputs)?;
        Ok((vec![], 0))
    }
}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 1] {
    [("jump".into(), Box::new(UnconditionalJumpGeneric {}))]
}
