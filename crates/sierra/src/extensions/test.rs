use test_case::test_case;

use super::{ExtensionError, Extensions, SpecializationError};
use crate::program::{ConcreteTypeId, ExtensionId, TemplateArg};

fn type_arg(name: &str) -> TemplateArg {
    TemplateArg::Type(ConcreteTypeId::Name(name.into()))
}

#[test_case("NoneExistent", vec![] => Err(SpecializationError::UnsupportedLibCallName); "NoneExistent")]
#[test_case("jump", vec![] => Ok(()); "jump")]
#[test_case("jump", vec![type_arg("T")] => Err(SpecializationError::WrongNumberOfTemplateArgs); "jump<T>")]
fn find_specialization(id: &str, tmpl_args: Vec<TemplateArg>) -> Result<(), SpecializationError> {
    Extensions::default().specialize(&ExtensionId::Name(id.into()), &tmpl_args).map(|_| ()).map_err(
        |error| match error {
            ExtensionError::Specialization { extension_id: _, error } => error,
        },
    )
}
