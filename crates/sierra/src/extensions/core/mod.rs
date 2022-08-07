use std::collections::HashMap;

use itertools::chain;

use super::ExtensionBox;
use crate::program::ExtensionId;

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

pub(super) fn all_core_extensions() -> HashMap<ExtensionId, ExtensionBox> {
    chain!(
        gas::extensions().into_iter(),
        integer::extensions().into_iter(),
        mem::extensions().into_iter(),
        unconditional_jump::extensions().into_iter(),
    )
    .collect()
}
