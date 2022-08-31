use crate::extensions::lib_func::{LibFuncSignature, SpecializationContext};
use crate::extensions::{NoGenericArgsGenericLibFunc, SpecializationError};
use crate::ids::GenericLibFuncId;

#[derive(Default)]
pub struct UnconditionalJumpLibFunc {}
impl NoGenericArgsGenericLibFunc for UnconditionalJumpLibFunc {
    type Concrete = LibFuncSignature;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("jump");
    fn specialize(
        &self,
        _context: SpecializationContext<'_>,
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(LibFuncSignature { input_types: vec![], output_types: vec![], fallthrough: None })
    }
}
