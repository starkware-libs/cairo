use super::unpack_inputs;
use crate::extensions::{ConcreteExtension, InputError, NoGenericArgsGenericExtension};
use crate::ids::GenericExtensionId;
use crate::mem_cell::MemCell;

pub struct UnconditionalJumpGeneric {}
impl NoGenericArgsGenericExtension for UnconditionalJumpGeneric {
    type Concrete = UnconditionalJumpConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("jump".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        UnconditionalJumpConcrete {}
    }
}

pub struct UnconditionalJumpConcrete {}
impl ConcreteExtension for UnconditionalJumpConcrete {
    fn simulate(
        &self,
        inputs: Vec<Vec<MemCell>>,
    ) -> Result<(Vec<Vec<MemCell>>, usize), InputError> {
        unpack_inputs::<0>(inputs)?;
        Ok((vec![], 0))
    }
}
