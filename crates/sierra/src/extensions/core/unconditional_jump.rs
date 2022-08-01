use super::validate_no_args;
use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, Extension, ExtensionBox, SpecializationError,
};
use crate::program::{ExtensionId, TemplateArg};

struct UnconditionalJumpExtension {}
impl Extension for UnconditionalJumpExtension {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<ConcreteExtensionBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(UnconditionalJump {}))
    }
}

struct UnconditionalJump {}
impl ConcreteExtension for UnconditionalJump {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 1] {
    [(ExtensionId::Name("jump".into()), Box::new(UnconditionalJumpExtension {}))]
}
