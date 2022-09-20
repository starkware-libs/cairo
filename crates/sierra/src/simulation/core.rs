use array_init::array_init;

use super::mem_cell::MemCell;
use super::LibFuncSimulationError;
use crate::extensions::arithmetic::{
    BinaryOperationConcreteLibFunc, ConstConcreteLibFunc, OperationConcreteLibFunc,
    OperationWithConstConcreteLibFunc, Operator,
};
use crate::extensions::core::CoreConcreteLibFunc::{
    self, ApTracking, Felt, FunctionCall, Gas, Integer, Mem, UnconditionalJump, UnwrapNonZero,
};
use crate::extensions::felt::FeltConcrete;
use crate::extensions::function_call::FunctionCallConcreteLibFunc;
use crate::extensions::gas::GasConcreteLibFunc::{GetGas, RefundGas};
use crate::extensions::gas::{GetGasConcreteLibFunc, RefundGasConcreteLibFunc};
use crate::extensions::integer::IntegerConcrete;
use crate::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use crate::ids::FunctionId;

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
        Integer(libfunc) => simulate_integer_libfunc(libfunc, inputs),
        Felt(libfunc) => simulate_felt_libfunc(libfunc, inputs),
        UnwrapNonZero(_) | Mem(Rename(_)) | Mem(StoreLocal(_)) | Mem(StoreTemp(_)) => {
            Ok((single_cell_identity::<1>(inputs)?, 0))
        }
        Mem(AlignTemps(_)) | Mem(FinalizeLocals(_)) | UnconditionalJump(_) | ApTracking(_) => {
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

/// Simulate integer library functions.
fn simulate_integer_libfunc(
    libfunc: &IntegerConcrete,
    inputs: Vec<Vec<MemCell>>,
) -> Result<(Vec<Vec<MemCell>>, usize), LibFuncSimulationError> {
    match libfunc {
        IntegerConcrete::Const(ConstConcreteLibFunc { c, .. }) => {
            unpack_inputs::<0>(inputs)?;
            Ok((vec![vec![(*c).into()]], 0))
        }
        IntegerConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => {
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
        IntegerConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => {
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
        IntegerConcrete::Duplicate(_) => {
            let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
            Ok((vec![vec![value.into()], vec![value.into()]], 0))
        }
        IntegerConcrete::Drop(_) => {
            unpack_inputs::<1>(inputs)?;
            Ok((vec![], 0))
        }
        IntegerConcrete::JumpNotZero(_) => {
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
    }
}

/// Simulate felt library functions.
fn simulate_felt_libfunc(
    libfunc: &FeltConcrete,
    inputs: Vec<Vec<MemCell>>,
) -> Result<(Vec<Vec<MemCell>>, usize), LibFuncSimulationError> {
    match libfunc {
        FeltConcrete::Const(ConstConcreteLibFunc { c, .. }) => {
            unpack_inputs::<0>(inputs)?;
            Ok((vec![vec![(*c).into()]], 0))
        }
        // TODO(orizi): Fix MemCell to based on felt and fix operation here.
        FeltConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => {
            let [MemCell { value: lhs }, MemCell { value: rhs }] = unpack_inputs::<2>(inputs)?;
            Ok((
                vec![vec![
                    match operator {
                        Operator::Add => lhs + rhs,
                        Operator::Sub => lhs - rhs,
                        Operator::Mul => lhs * rhs,
                        Operator::Div => todo!("Support full felt operations."),
                        Operator::Mod => 0,
                    }
                    .into(),
                ]],
                0,
            ))
        }
        // TODO(orizi): Fix MemCell to based on felt and fix operation here.
        FeltConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => {
            let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
            Ok((
                vec![vec![
                    match operator {
                        Operator::Add => value + c,
                        Operator::Sub => value - c,
                        Operator::Mul => value * c,
                        Operator::Div => todo!("Support full felt operations."),
                        Operator::Mod => value % c,
                    }
                    .into(),
                ]],
                0,
            ))
        }
        FeltConcrete::Duplicate(_) => {
            let [MemCell { value }] = unpack_inputs::<1>(inputs)?;
            Ok((vec![vec![value.into()], vec![value.into()]], 0))
        }
        FeltConcrete::Drop(_) => {
            unpack_inputs::<1>(inputs)?;
            Ok((vec![], 0))
        }
        FeltConcrete::JumpNotZero(_) => {
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
    }
}
