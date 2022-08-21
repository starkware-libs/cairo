use array_init::array_init;

use super::mem_cell::MemCell;
use super::ExtensionSimulationError;
use crate::extensions::core::gas::GasConcrete::{GetGas, RefundGas};
use crate::extensions::core::gas::{GetGasConcrete, RefundGasConcrete};
use crate::extensions::core::integer::IntegerConcrete::{
    Const, Duplicate, Ignore, JumpNotZero, Operation, UnwrapNonZero,
};
use crate::extensions::core::integer::{
    BinaryOperationConcrete, ConstConcrete, OperationConcrete, OperationWithConstConcrete, Operator,
};
use crate::extensions::core::mem::MemConcrete::{
    AlignTemps, AllocLocals, Move, Rename, StoreLocal, StoreTemp,
};
use crate::extensions::CoreConcrete::{self, Gas, Integer, Mem, UnconditionalJump};

/// Simulates the run of a single extension.
pub fn simulate(
    extension: &CoreConcrete,
    inputs: Vec<Vec<MemCell>>,
) -> Result<(Vec<Vec<MemCell>>, usize), ExtensionSimulationError> {
    match extension {
        Gas(GetGas(GetGasConcrete { count })) => {
            let [MemCell { value: gas_counter }] = unpack_inputs::<1>(inputs)?;
            if gas_counter >= *count {
                // Have enough gas - return reduced counter and jump to success branch.
                Ok((vec![vec![(gas_counter - count).into()]], 0))
            } else {
                // Don't have enough gas - return the same counter and jump to failure branch.
                Ok((vec![vec![gas_counter.into()]], 1))
            }
        }
        Gas(RefundGas(RefundGasConcrete { count })) => {
            let [MemCell { value: gas_counter }] = unpack_inputs::<1>(inputs)?;
            Ok((vec![vec![(gas_counter + count).into()]], 0))
        }
        Integer(Const(ConstConcrete { c })) => {
            unpack_inputs::<0>(inputs)?;
            Ok((vec![vec![(*c).into()]], 0))
        }
        Integer(Operation(OperationConcrete::Binary(BinaryOperationConcrete { operator }))) => {
            let [MemCell { value: lhs }, MemCell { value: rhs }] = unpack_inputs::<2>(inputs)?;
            Ok((
                vec![vec![
                    match operator {
                        Operator::Add => lhs + rhs,
                        Operator::Sub => lhs - rhs,
                        Operator::Mul => lhs * rhs,
                        Operator::Div => lhs / rhs,
                        Operator::Mod => lhs % rhs,
                    }
                    .into(),
                ]],
                0,
            ))
        }
        Integer(Operation(OperationConcrete::Const(OperationWithConstConcrete {
            operator,
            c,
        }))) => {
            let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
            Ok((
                vec![vec![
                    match operator {
                        Operator::Add => value + c,
                        Operator::Sub => value - c,
                        Operator::Mul => value * c,
                        Operator::Div => value / c,
                        Operator::Mod => value % c,
                    }
                    .into(),
                ]],
                0,
            ))
        }
        Integer(Duplicate(_)) => {
            let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
            Ok((vec![vec![value.into()], vec![value.into()]], 0))
        }
        Integer(Ignore(_)) => {
            unpack_inputs::<1>(inputs)?;
            Ok((vec![], 0))
        }
        Integer(JumpNotZero(_)) => {
            let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
            if value != 0 {
                // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                // given value.
                Ok((vec![vec![value.into()]], 0))
            } else {
                // Zero - jumping to the failure branch.
                Ok((vec![], 1))
            }
        }
        Integer(UnwrapNonZero(_))
        | Mem(Move(_))
        | Mem(Rename(_))
        | Mem(StoreLocal(_))
        | Mem(StoreTemp(_)) => Ok((single_cell_identity::<1>(inputs)?, 0)),
        Mem(AlignTemps(_)) | Mem(AllocLocals(_)) | UnconditionalJump(_) => {
            unpack_inputs::<0>(inputs)?;
            Ok((vec![], 0))
        }
    }
}

/// Unpacking inputs from a vector of vectors memcells into an array of memcell of the given
/// constant size.
fn unpack_inputs<const N: usize>(
    mut inputs: Vec<Vec<MemCell>>,
) -> Result<[MemCell; N], ExtensionSimulationError> {
    if inputs.len() != N {
        Err(ExtensionSimulationError::WrongNumberOfArgs)
    } else if inputs.iter().any(|input| input.len() != 1) {
        // TODO(oziv): Currently we only support internal vectors to be of size 1.
        Err(ExtensionSimulationError::MemoryLayoutMismatch)
    } else {
        Ok(array_init(|i| inputs[i].remove(0)))
    }
}

/// Unpacks and repacks the arguments, validating the number and size of arguments.
fn single_cell_identity<const N: usize>(
    inputs: Vec<Vec<MemCell>>,
) -> Result<Vec<Vec<MemCell>>, ExtensionSimulationError> {
    let cells = unpack_inputs::<N>(inputs)?;
    Ok(cells.into_iter().map(|cell| vec![cell]).collect())
}
