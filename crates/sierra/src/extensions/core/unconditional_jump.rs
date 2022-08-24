use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{ConcreteLibFunc, NoGenericArgsGenericLibFunc};
use crate::ids::ConcreteTypeId;

#[derive(Default)]
pub struct UnconditionalJumpGeneric {}
impl NoGenericArgsGenericLibFunc for UnconditionalJumpGeneric {
    type Concrete = UnconditionalJumpConcrete;
    const NAME: &'static str = "jump";
    fn specialize(&self, _context: SpecializationContext<'_>) -> Self::Concrete {
        UnconditionalJumpConcrete {}
    }
}

pub struct UnconditionalJumpConcrete {}
impl ConcreteLibFunc for UnconditionalJumpConcrete {
    fn input_types(&self) -> Vec<ConcreteTypeId> {
        vec![]
    }
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        vec![]
    }
    fn fallthrough(&self) -> Option<usize> {
        None
    }
}
