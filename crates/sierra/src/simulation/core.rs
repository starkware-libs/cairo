use array_init::array_init;

use super::mem_cell::MemCell;
use super::LibFuncSimulationError;
use crate::extensions::core::CoreConcreteLibFunc::{
    self, Felt, FunctionCall, Gas, Integer, Mem, UnconditionalJump, UnwrapNonZero,
};
use crate::extensions::function_call::FunctionCallConcreteLibFunc;
use crate::extensions::gas::GasConcreteLibFunc::{GetGas, RefundGas};
use crate::extensions::gas::{GetGasConcreteLibFunc, RefundGasConcreteLibFunc};
use crate::extensions::integer::IntegerConcrete::{Const, Duplicate, Ignore, Operation};
use crate::extensions::integer::{
    BinaryOperationConcreteLibFunc, ConstConcreteLibFunc, OperationConcreteLibFunc,
    OperationWithConstConcreteLibFunc, Operator,
};
use crate::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, AllocLocals, Rename, StoreLocal, StoreTemp,
};
use crate::ids::FunctionId;
use crate::simulation::CoreConcreteLibFunc::JumpNotZero;

/// Simulates the run of a single libfunc. Returns the memory reperesentations of the outputs, and
/// the chosen branch given the inputs. A function that provides the simulation of running a user
/// function is also provided for the case where the extensions needs to simulate it.
pub fn simulate<
    F: Fn(&FunctionId, Vec<Vec<MemCell>>) -> Result<Vec<Vec<MemCell>>, LibFuncSimulationError>,
>(
    libfunc: &CoreConcreteLibFunc,
    inputs: Vec<Vec<MemCell>>,
    simulate_function: F,
) -> Result<(Vec<Vec<MemCell>>, usize), LibFuncSimulationError> {
    match libfunc {
        Felt(_) => todo!("felt operations are yet to be simulated."),
        FunctionCall(FunctionCallConcreteLibFunc { function, .. }) => {
            Ok((simulate_function(&function.id, inputs)?, 0))
        }
        Gas(GetGas(GetGasConcreteLibFunc { count, .. })) => {
            let [MemCell { value: gas_counter }] = unpack_inputs::<1>(inputs)?;
            if gas_counter >= *count {
                // Have enough gas - return reduced counter and jump to success branch.
                Ok((vec![vec![(gas_counter - count).into()]], 0))
            } else {
                // Don't have enough gas - return the same counter and jump to failure branch.
                Ok((vec![vec![gas_counter.into()]], 1))
            }
        }
        Gas(RefundGas(RefundGasConcreteLibFunc { count, .. })) => {
            let [MemCell { value: gas_counter }] = unpack_inputs::<1>(inputs)?;
            Ok((vec![vec![(gas_counter + count).into()]], 0))
        }
        Integer(Const(ConstConcreteLibFunc { c, .. })) => {
            unpack_inputs::<0>(inputs)?;
            Ok((vec![vec![(*c).into()]], 0))
        }
        Integer(Operation(OperationConcreteLibFunc::Binary(BinaryOperationConcreteLibFunc {
            operator,
            ..
        }))) => {
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
        Integer(Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        ))) => {
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
        JumpNotZero(_) => {
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
        UnwrapNonZero(_) | Mem(Rename(_)) | Mem(StoreLocal(_)) | Mem(StoreTemp(_)) => {
            Ok((single_cell_identity::<1>(inputs)?, 0))
        }
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
) -> Result<[MemCell; N], LibFuncSimulationError> {
    if inputs.len() != N {
        Err(LibFuncSimulationError::WrongNumberOfArgs)
    } else if inputs.iter().any(|input| input.len() != 1) {
        // TODO(oziv): Currently we only support internal vectors to be of size 1.
        Err(LibFuncSimulationError::MemoryLayoutMismatch)
    } else {
        Ok(array_init(|i| inputs[i].remove(0)))
    }
}

/// Unpacks and repacks the arguments, validating the number and size of arguments.
fn single_cell_identity<const N: usize>(
    inputs: Vec<Vec<MemCell>>,
) -> Result<Vec<Vec<MemCell>>, LibFuncSimulationError> {
    let cells = unpack_inputs::<N>(inputs)?;
    Ok(cells.into_iter().map(|cell| vec![cell]).collect())
}
