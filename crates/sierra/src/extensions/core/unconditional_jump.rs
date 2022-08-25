use crate::extensions::lib_func::SpecializationContext;
use crate::extensions::{ConcreteLibFunc, NoGenericArgsGenericLibFunc, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericLibFuncId};

#[derive(Default)]
pub struct UnconditionalJumpLibFunc {}
impl NoGenericArgsGenericLibFunc for UnconditionalJumpLibFunc {
    type Concrete = UnconditionalJumpConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("jump");
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(UnconditionalJumpConcreteLibFunc {})
    }
}

pub struct UnconditionalJumpConcreteLibFunc {}
impl ConcreteLibFunc for UnconditionalJumpConcreteLibFunc {
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
