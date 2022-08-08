use std::collections::HashMap;

use array_init::array_init;
use itertools::chain;

use super::{GenericExtensionBox, InputsError};
use crate::mem_cell::MemCell;
use crate::program::GenericExtensionId;

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

fn unpack_inputs<const N: usize>(
    mut inputs: Vec<Vec<MemCell>>,
) -> Result<[MemCell; N], InputsError> {
    if inputs.len() != N {
        Err(InputsError::WrongNumberOfArgs)
    } else if inputs.iter().any(|input| input.len() != 1) {
        Err(InputsError::MemoryLayoutMismatch)
    } else {
        Ok(array_init(|i| inputs[i].remove(0)))
    }
}
