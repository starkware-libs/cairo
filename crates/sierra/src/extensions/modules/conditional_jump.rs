use super::as_single_type;
use super::non_zero::NonZeroType;
use crate::extensions::lib_func::{
    LibFuncSignature, SignatureOnlyConcreteLibFunc, SpecializationContext,
};
use crate::extensions::{NamedLibFunc, NamedType, SpecializationError};
use crate::ids::GenericLibFuncId;
use crate::program::GenericArg;

/// LibFunc for jump non-zero<T>, returns  non-zero<T> in case of success.
#[derive(Default)]
pub struct JumpNotZeroLibFunc {}
impl NamedLibFunc for JumpNotZeroLibFunc {
    type Concrete = SignatureOnlyConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("jump_nz");

    fn specialize(
        &self,
        context: SpecializationContext<'_>,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = as_single_type(args)?;
        Ok(SignatureOnlyConcreteLibFunc {
            signature: LibFuncSignature {
                input_types: vec![ty.clone()],
                output_types: vec![
                    // success=
                    vec![context.get_wrapped_concrete_type(NonZeroType::id(), ty)?],
                    // failure=
                    vec![],
                ],
                fallthrough: Some(1),
            },
        })
    }
}
