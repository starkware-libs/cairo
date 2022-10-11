use utils::extract_matches;

use super::value::CoreValue;
use super::LibFuncSimulationError;
use crate::extensions::arithmetic::{
    BinaryOperationConcreteLibFunc, ConstConcreteLibFunc, OperationConcreteLibFunc,
    OperationWithConstConcreteLibFunc, Operator,
};
use crate::extensions::array::ArrayConcreteLibFunc;
use crate::extensions::core::CoreConcreteLibFunc::{
    self, ApTracking, Array, Drop, Dup, Felt, FunctionCall, Gas, Integer, Mem, UnconditionalJump,
    UnwrapNonZero,
};
use crate::extensions::felt::FeltConcrete;
use crate::extensions::function_call::FunctionCallConcreteLibFunc;
use crate::extensions::gas::GasConcreteLibFunc::{GetGas, RefundGas};
use crate::extensions::integer::IntegerConcrete;
use crate::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use crate::ids::FunctionId;

/// Simulates the run of a single libfunc. Returns the value reperesentations of the outputs, and
/// the chosen branch given the inputs. A function that provides the simulation of running a user
/// function is also provided for the case where the extensions needs to simulate it.
pub fn simulate<
    GetStatementGasInfo: Fn() -> Option<i64>,
    SimulateFunction: Fn(&FunctionId, Vec<CoreValue>) -> Result<Vec<CoreValue>, LibFuncSimulationError>,
>(
    libfunc: &CoreConcreteLibFunc,
    inputs: Vec<CoreValue>,
    get_statement_gas_info: GetStatementGasInfo,
    simulate_function: SimulateFunction,
) -> Result<(Vec<CoreValue>, usize), LibFuncSimulationError> {
    match libfunc {
        Drop(_) => match &inputs[..] {
            [_] => Ok((vec![], 0)),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Dup(_) => match &inputs[..] {
            [value] => Ok((vec![value.clone(), value.clone()], 0)),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FunctionCall(FunctionCallConcreteLibFunc { function, .. }) => {
            Ok((simulate_function(&function.id, inputs)?, 0))
        }
        Gas(GetGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibFuncSimulationError::UnresolvedStatementGasInfo)?;
            let gas_counter = match &inputs[..] {
                [CoreValue::GasBuiltin(value)] => Ok(value),
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }?;
            if *gas_counter >= count {
                // Have enough gas - return reduced counter and jump to success branch.
                Ok((vec![CoreValue::GasBuiltin(gas_counter - count)], 0))
            } else {
                // Don't have enough gas - return the same counter and jump to failure branch.
                Ok((vec![CoreValue::GasBuiltin(*gas_counter)], 1))
            }
        }
        Gas(RefundGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibFuncSimulationError::UnresolvedStatementGasInfo)?;
            let gas_counter = match &inputs[..] {
                [CoreValue::GasBuiltin(value)] => Ok(value),
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }?;
            Ok((vec![CoreValue::GasBuiltin(gas_counter + count)], 0))
        }
        Array(ArrayConcreteLibFunc::New(_)) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Array(vec![])], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        Array(ArrayConcreteLibFunc::Append(_)) => match &inputs[..] {
            [CoreValue::Array(_), _] => {
                let mut iter = inputs.into_iter();
                let mut arr = extract_matches!(iter.next().unwrap(), CoreValue::Array);
                arr.push(iter.next().unwrap());
                Ok((vec![CoreValue::Array(arr)], 0))
            }
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Integer(libfunc) => simulate_integer_libfunc(libfunc, &inputs),
        Felt(libfunc) => simulate_felt_libfunc(libfunc, &inputs),
        UnwrapNonZero(_) => match &inputs[..] {
            [CoreValue::NonZero(value)] => Ok((vec![*value.clone()], 0)),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Mem(Rename(_) | StoreTemp(_)) | CoreConcreteLibFunc::Box(_) => {
            if inputs.len() == 1 {
                Ok((inputs, 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        Mem(AlignTemps(_)) | Mem(FinalizeLocals(_)) | UnconditionalJump(_) | ApTracking(_) => {
            if inputs.is_empty() {
                Ok((inputs, 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        Mem(StoreLocal(_)) => match &inputs[..] {
            [CoreValue::Uninitialized, other] => Ok((vec![other.clone()], 0)),
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Mem(AllocLocal(_)) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uninitialized], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
    }
}

/// Simulate integer library functions.
fn simulate_integer_libfunc(
    libfunc: &IntegerConcrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibFuncSimulationError> {
    match libfunc {
        IntegerConcrete::Const(ConstConcreteLibFunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Integer(*c)], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        IntegerConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::Integer(lhs), CoreValue::Integer(rhs)],
                Operator::Add | Operator::Sub | Operator::Mul,
            ) => Ok((
                vec![CoreValue::Integer(match operator {
                    Operator::Add => lhs + rhs,
                    Operator::Sub => lhs - rhs,
                    Operator::Mul => lhs * rhs,
                    _ => unreachable!("Arm only handles these cases."),
                })],
                0,
            )),
            (
                [CoreValue::Integer(lhs), CoreValue::NonZero(non_zero)],
                Operator::Div | Operator::Mod,
            ) => {
                if let CoreValue::Integer(rhs) = **non_zero {
                    Ok((
                        vec![CoreValue::Integer(match operator {
                            Operator::Div => lhs / rhs,
                            Operator::Mod => lhs % rhs,
                            _ => unreachable!("Arm only handles these cases."),
                        })],
                        0,
                    ))
                } else {
                    Err(LibFuncSimulationError::MemoryLayoutMismatch)
                }
            }
            ([_, _], _) => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        IntegerConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::Integer(value)] => Ok((
                vec![CoreValue::Integer(match operator {
                    Operator::Add => value + c,
                    Operator::Sub => value - c,
                    Operator::Mul => value * c,
                    Operator::Div => value / c,
                    Operator::Mod => value % c,
                })],
                0,
            )),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        IntegerConcrete::JumpNotZero(_) => {
            match inputs {
                [CoreValue::Integer(value)] if *value != 0 => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![CoreValue::NonZero(Box::new(CoreValue::Integer(*value)))], 0))
                }
                [CoreValue::Integer(value)] if *value == 0 => {
                    // Zero - jumping to the failure branch.
                    Ok((vec![], 1))
                }
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
    }
}

/// Simulate felt library functions.
fn simulate_felt_libfunc(
    libfunc: &FeltConcrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibFuncSimulationError> {
    match libfunc {
        FeltConcrete::Const(ConstConcreteLibFunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Felt(*c as i128)], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        FeltConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::Felt(lhs), CoreValue::Felt(rhs)],
                Operator::Add | Operator::Sub | Operator::Mul,
            ) => Ok((
                vec![CoreValue::Felt(match operator {
                    Operator::Add => lhs + rhs,
                    Operator::Sub => lhs - rhs,
                    Operator::Mul => lhs * rhs,
                    _ => unreachable!("Arm only handles these cases."),
                })],
                0,
            )),
            (
                [CoreValue::Felt(_lhs), CoreValue::NonZero(non_zero)],
                Operator::Div | Operator::Mod,
            ) => {
                if let CoreValue::Felt(_rhs) = **non_zero {
                    Ok((
                        vec![CoreValue::Integer(match operator {
                            Operator::Div => todo!("Support full felt operations."),
                            Operator::Mod => 0,
                            _ => unreachable!("Arm only handles these cases."),
                        })],
                        0,
                    ))
                } else {
                    Err(LibFuncSimulationError::MemoryLayoutMismatch)
                }
            }
            ([_, _], _) => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::Felt(value)] => Ok((
                vec![CoreValue::Felt(match operator {
                    Operator::Add => value + *c as i128,
                    Operator::Sub => value - *c as i128,
                    Operator::Mul => value * *c as i128,
                    Operator::Div => todo!("Support full felt operations."),
                    Operator::Mod => 0,
                })],
                0,
            )),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::JumpNotZero(_) => {
            match inputs {
                [CoreValue::Felt(value)] if *value != 0 => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![CoreValue::NonZero(Box::new(CoreValue::Felt(*value)))], 0))
                }
                [CoreValue::Felt(value)] if *value == 0 => {
                    // Zero - jumping to the failure branch.
                    Ok((vec![], 1))
                }
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
    }
}
