use std::collections::HashMap;

use itertools::chain;

use super::{GenericExtensionBox, GenericTypeBox};
use crate::program::ids::{GenericExtensionId, GenericTypeId};

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

pub(super) fn all_core_types() -> HashMap<GenericTypeId, GenericTypeBox> {
    chain!(gas::types().into_iter(), integer::types().into_iter(),).collect()
}
