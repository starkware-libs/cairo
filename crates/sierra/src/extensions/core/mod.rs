use std::collections::HashMap;

use array_init::array_init;
use itertools::chain;

use super::{GenericExtensionBox, GenericTypeBox, InputError};
use crate::ids::{GenericExtensionId, GenericTypeId};
use crate::mem_cell::MemCell;

mod function_call;
mod gas;
mod integer;
mod mem;
mod unconditional_jump;

pub(super) fn all_core_extensions() -> HashMap<GenericExtensionId, GenericExtensionBox> {
    chain!(
        function_call::extensions().into_iter(),
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

/// Unpacking inputs from a vector of vectors memcells into an array of memcell of the given
/// constant size.
fn unpack_inputs<const N: usize>(
    mut inputs: Vec<Vec<MemCell>>,
) -> Result<[MemCell; N], InputError> {
    if inputs.len() != N {
        Err(InputError::WrongNumberOfArgs)
    } else if inputs.iter().any(|input| input.len() != 1) {
        // TODO(oziv): Currently we only support internal vectors to be of size 1.
        Err(InputError::MemoryLayoutMismatch)
    } else {
        Ok(array_init(|i| inputs[i].remove(0)))
    }
}

/// Unpacks and repacks the arguments, validating the number and size of arguments.
fn single_cell_identity<const N: usize>(
    inputs: Vec<Vec<MemCell>>,
) -> Result<Vec<Vec<MemCell>>, InputError> {
    let cells = unpack_inputs::<N>(inputs)?;
    Ok(cells.into_iter().map(|cell| vec![cell]).collect())
}
