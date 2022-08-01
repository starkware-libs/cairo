use std::collections::HashMap;

use itertools::chain;

use super::{SpecializationError, SpecializerBox};
use crate::program::{Identifier, TemplateArg};

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

pub(super) fn all_core_extensions() -> HashMap<Identifier, SpecializerBox> {
    chain!(
        gas::extensions().into_iter(),
        integer::extensions().into_iter(),
        mem::extensions().into_iter(),
        unconditional_jump::extensions().into_iter(),
    )
    .collect()
}

fn validate_no_args(tmpl_args: &[TemplateArg]) -> Result<(), SpecializationError> {
    if tmpl_args.is_empty() { Ok(()) } else { Err(SpecializationError::WrongNumberOfTemplateArgs) }
}
