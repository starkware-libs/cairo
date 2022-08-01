use std::collections::HashMap;

use itertools::chain;

use super::{ExtensionBox, SpecializationError};
use crate::program::{ExtensionId, TemplateArg};

mod unconditional_jump;

pub(super) fn all_core_extensions() -> HashMap<ExtensionId, ExtensionBox> {
    chain!(unconditional_jump::extensions().into_iter(),).collect()
}

fn validate_no_args(tmpl_args: &[TemplateArg]) -> Result<(), SpecializationError> {
    if tmpl_args.is_empty() { Ok(()) } else { Err(SpecializationError::WrongNumberOfTemplateArgs) }
}
