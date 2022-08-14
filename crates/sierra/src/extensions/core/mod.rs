use std::collections::HashMap;

use itertools::chain;

use super::GenericExtensionBox;
use crate::ids::GenericExtensionId;

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

pub(super) fn all_core_extensions() -> HashMap<GenericExtensionId, GenericExtensionBox> {
    chain!(
        gas::extensions().into_iter(),
        integer::extensions().into_iter(),
        mem::extensions().into_iter(),
        unconditional_jump::extensions().into_iter(),
    )
    .collect()
}
