use super::unpack_inputs;
use crate::extensions::{ConcreteExtension, InputError, NoGenericArgsGenericExtension};
use crate::mem_cell::MemCell;

#[derive(Default)]
pub struct UnconditionalJumpGeneric {}
impl NoGenericArgsGenericExtension for UnconditionalJumpGeneric {
    type Concrete = UnconditionalJumpConcrete;
    const NAME: &'static str = "jump";
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
