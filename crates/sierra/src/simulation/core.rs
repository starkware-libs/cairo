use super::value::Value;
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
    SimulateFunction: Fn(&FunctionId, Vec<Value>) -> Result<Vec<Value>, LibFuncSimulationError>,
>(
    libfunc: &CoreConcreteLibFunc,
    inputs: Vec<Value>,
    get_statement_gas_info: GetStatementGasInfo,
    simulate_function: SimulateFunction,
) -> Result<(Vec<Value>, usize), LibFuncSimulationError> {
    match libfunc {
        FunctionCall(FunctionCallConcreteLibFunc { function, .. }) => {
            Ok((simulate_function(&function.id, inputs)?, 0))
        }
        Gas(GetGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibFuncSimulationError::UnresolvedStatementGasInfo)?;
            let gas_counter = match &inputs[..] {
                [Value::Int(value)] => Ok(value),
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }?;
            if *gas_counter >= count {
                // Have enough gas - return reduced counter and jump to success branch.
                Ok((vec![Value::Int(gas_counter - count)], 0))
            } else {
                // Don't have enough gas - return the same counter and jump to failure branch.
                Ok((vec![Value::Int(*gas_counter)], 1))
            }
        }
        Gas(RefundGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibFuncSimulationError::UnresolvedStatementGasInfo)?;
            let gas_counter = match &inputs[..] {
                [Value::Int(value)] => Ok(value),
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }?;
            Ok((vec![Value::Int(gas_counter + count)], 0))
        }
        Integer(libfunc) => simulate_integer_libfunc(libfunc, &inputs),
        Felt(libfunc) => simulate_felt_libfunc(libfunc, &inputs),
        UnwrapNonZero(_) | Mem(Rename(_) | StoreTemp(_)) | CoreConcreteLibFunc::Ref(_) => {
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
            [Value::Transient, other] => Ok((vec![other.clone()], 0)),
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Mem(AllocLocal(_)) => {
            if inputs.is_empty() {
                Ok((vec![Value::Transient], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
    }
}

/// Simulate integer library functions.
fn simulate_integer_libfunc(
    libfunc: &IntegerConcrete,
    inputs: &[Value],
) -> Result<(Vec<Value>, usize), LibFuncSimulationError> {
    match libfunc {
        IntegerConcrete::Const(ConstConcreteLibFunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![Value::Int(*c)], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        IntegerConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match inputs {
            [Value::Int(lhs), Value::Int(rhs)] => Ok((
                vec![Value::Int(match operator {
                    Operator::Add => lhs + rhs,
                    Operator::Sub => lhs - rhs,
                    Operator::Mul => lhs * rhs,
                    Operator::Div => lhs / rhs,
                    Operator::Mod => lhs % rhs,
                })],
                0,
            )),
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        IntegerConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [Value::Int(value)] => Ok((
                vec![Value::Int(match operator {
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
        IntegerConcrete::Duplicate(_) => match inputs {
            [Value::Int(value)] => Ok((vec![Value::Int(*value), Value::Int(*value)], 0)),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        IntegerConcrete::Drop(_) => match inputs {
            [Value::Int(_)] => Ok((vec![], 0)),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        IntegerConcrete::JumpNotZero(_) => {
            match inputs {
                [Value::Int(value)] if *value != 0 => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![Value::Int(*value)], 0))
                }
                [Value::Int(value)] if *value == 0 => {
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
    inputs: &[Value],
) -> Result<(Vec<Value>, usize), LibFuncSimulationError> {
    match libfunc {
        FeltConcrete::Const(ConstConcreteLibFunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![Value::Felt(*c as i128)], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        FeltConcrete::Operation(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match inputs {
            [Value::Felt(lhs), Value::Felt(rhs)] => Ok((
                vec![Value::Felt(match operator {
                    Operator::Add => lhs + rhs,
                    Operator::Sub => lhs - rhs,
                    Operator::Mul => lhs * rhs,
                    Operator::Div => todo!("Support full felt operations."),
                    Operator::Mod => lhs % rhs,
                })],
                0,
            )),
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        // TODO(orizi): Fix MemCell to based on felt and fix operation here.
        FeltConcrete::Operation(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [Value::Felt(value)] => Ok((
                vec![Value::Felt(match operator {
                    Operator::Add => value + *c as i128,
                    Operator::Sub => value - *c as i128,
                    Operator::Mul => value * *c as i128,
                    Operator::Div => todo!("Support full felt operations."),
                    Operator::Mod => value % *c as i128,
                })],
                0,
            )),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::Duplicate(_) => match inputs {
            [Value::Felt(value)] => Ok((vec![Value::Felt(*value), Value::Felt(*value)], 0)),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::Drop(_) => match inputs {
            [Value::Felt(_)] => Ok((vec![], 0)),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::JumpNotZero(_) => {
            match inputs {
                [Value::Felt(value)] if *value != 0 => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![Value::Felt(*value)], 0))
                }
                [Value::Felt(value)] if *value == 0 => {
                    // Zero - jumping to the failure branch.
                    Ok((vec![], 1))
                }
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
    }
}
