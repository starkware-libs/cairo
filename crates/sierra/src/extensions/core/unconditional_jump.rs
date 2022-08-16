use super::unpack_inputs;
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtensionBox, InputError,
    NoGenericArgsGenericExtension,
};
use crate::ids::GenericExtensionId;
use crate::mem_cell::MemCell;

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
    ) -> Result<(Vec<Vec<MemCell>>, usize), InputError> {
        unpack_inputs::<0>(inputs)?;
        Ok((vec![], 0))
    }
}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 1] {
    [("jump".into(), Box::new(UnconditionalJumpGeneric {}))]
}
