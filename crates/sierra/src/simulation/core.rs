use utils::extract_matches;

use super::value::CoreValue;
use super::LibFuncSimulationError;
use crate::extensions::array::ArrayConcreteLibFunc;
use crate::extensions::core::CoreConcreteLibFunc::{
    self, ApTracking, Array, Drop, Dup, Enum, Felt, FunctionCall, Gas, Integer, Mem,
    UnconditionalJump, UnwrapNonZero,
};
use crate::extensions::enm::{EnumConcreteLibFunc, EnumInitConcreteLibFunc};
use crate::extensions::felt::FeltConcrete;
use crate::extensions::function_call::FunctionCallConcreteLibFunc;
use crate::extensions::gas::GasConcreteLibFunc::{BurnGas, GetGas, RefundGas};
use crate::extensions::integer::IntegerConcrete;
use crate::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use crate::extensions::wrapping_arithmetic::{
    BinaryOperationConcreteLibFunc, ConstConcreteLibFunc, OperationConcreteLibFunc,
    OperationWithConstConcreteLibFunc, Operator,
};
use crate::ids::FunctionId;

/// Simulates the run of a single libfunc. Returns the value representations of the outputs, and
/// the chosen branch given the inputs.
///
/// `simulate_function` is a function that simulates running of a user function. It is provided here
/// for the case where the extensions need to use it.
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
        Gas(BurnGas(_)) => {
            get_statement_gas_info().ok_or(LibFuncSimulationError::UnresolvedStatementGasInfo)?;
            Ok((vec![], 0))
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
        Enum(EnumConcreteLibFunc::Init(EnumInitConcreteLibFunc { index, .. })) => {
            match &inputs[..] {
                [input] => {
                    // We don't verify here that the input type matches the signature.
                    Ok((vec![CoreValue::Enum { value: Box::new(input.clone()), index: *index }], 0))
                }
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
        Enum(EnumConcreteLibFunc::Match(_)) => match &inputs[..] {
            [CoreValue::Enum { value, index }] => Ok((vec![*value.clone()], *index)),
            [_] => Err(LibFuncSimulationError::WrongArgType),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
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
                Ok((vec![CoreValue::Uint128(*c as u128)], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        IntegerConcrete::WrappingOp(OperationConcreteLibFunc::Binary(
            BinaryOperationConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)],
                Operator::Add | Operator::Sub | Operator::Mul,
            ) => Ok((
                vec![CoreValue::Uint128(match operator {
                    Operator::Add => lhs.wrapping_add(*rhs),
                    Operator::Sub => lhs.wrapping_sub(*rhs),
                    Operator::Mul => lhs.wrapping_mul(*rhs),
                    _ => unreachable!("Arm only handles these cases."),
                })],
                0,
            )),
            (
                [CoreValue::Uint128(lhs), CoreValue::NonZero(non_zero)],
                Operator::Div | Operator::Mod,
            ) => {
                if let CoreValue::Uint128(rhs) = **non_zero {
                    Ok((
                        vec![CoreValue::Uint128(match operator {
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
        IntegerConcrete::WrappingOp(OperationConcreteLibFunc::Const(
            OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::Uint128(value)] => Ok((
                vec![CoreValue::Uint128(match operator {
                    Operator::Add => value.wrapping_add(*c as u128),
                    Operator::Sub => value.wrapping_sub(*c as u128),
                    Operator::Mul => value.wrapping_mul(*c as u128),
                    Operator::Div => value / *c as u128,
                    Operator::Mod => value % *c as u128,
                })],
                0,
            )),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        IntegerConcrete::JumpNotZero(_) => {
            match inputs {
                [CoreValue::Uint128(value)] if *value == 0 => {
                    // Zero - jumping to the failure branch.
                    Ok((vec![], 0))
                }
                [CoreValue::Uint128(value)] if *value != 0 => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![CoreValue::NonZero(Box::new(CoreValue::Uint128(*value)))], 1))
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
                        vec![CoreValue::Uint128(match operator {
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
                [CoreValue::Felt(value)] if *value == 0 => {
                    // Zero - jumping to the failure branch.
                    Ok((vec![], 0))
                }
                [CoreValue::Felt(value)] if *value != 0 => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![CoreValue::NonZero(Box::new(CoreValue::Felt(*value)))], 1))
                }
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
    }
}
