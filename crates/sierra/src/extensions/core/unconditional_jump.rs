use super::validate_no_args;
use crate::extensions::{
    Specialization, SpecializationBox, SpecializationError, Specializer, SpecializerBox,
};
use crate::program::{Identifier, TemplateArg};

struct UnconditionalJumpSpecializer {}
impl Specializer for UnconditionalJumpSpecializer {
    fn specialize(
        &self,
        tmpl_args: &[TemplateArg],
    ) -> Result<SpecializationBox, SpecializationError> {
        validate_no_args(tmpl_args)?;
        Ok(Box::new(UnconditionalJump {}))
    }
}

struct UnconditionalJump {}
impl Specialization for UnconditionalJump {}

pub(super) fn extensions() -> [(Identifier, SpecializerBox); 1] {
    [(Identifier::Name("jump".into()), Box::new(UnconditionalJumpSpecializer {}))]
}
