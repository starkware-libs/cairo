use array_init::array_init;

use self::gas::GasExtension;
use self::integer::IntegerExtension;
use self::mem::MemExtension;
use self::unconditional_jump::UnconditionalJumpGeneric;
use super::{GenericExtension, InputError};
use crate::define_extension_hierarchy;
use crate::mem_cell::MemCell;

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

define_extension_hierarchy! {
    pub enum CoreExtension {
        Gas(GasExtension),
        Integer(IntegerExtension),
        Mem(MemExtension),
        UnconditionalJump(UnconditionalJumpGeneric)
    }, CoreConcrete
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
