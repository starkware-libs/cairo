use utils::extract_matches;

use super::value::CoreValue;
use super::LibFuncSimulationError;
use crate::extensions::array::ArrayConcreteLibFunc;
use crate::extensions::core::CoreConcreteLibFunc::{
    self, ApTracking, Array, Drop, Dup, Enum, Felt, FunctionCall, Gas, Mem, Uint128,
    UnconditionalJump, UnwrapNonZero,
};
use crate::extensions::enm::{EnumConcreteLibFunc, EnumInitConcreteLibFunc};
use crate::extensions::felt::{
    FeltBinaryOperationConcreteLibFunc, FeltConcrete, FeltConstConcreteLibFunc,
    FeltOperationConcreteLibFunc, FeltOperationWithConstConcreteLibFunc, FeltOperator,
};
use crate::extensions::function_call::FunctionCallConcreteLibFunc;
use crate::extensions::gas::GasConcreteLibFunc::{BurnGas, GetGas, RefundGas};
use crate::extensions::integer::{
    IntOperator, Uint128BinaryOperationConcreteLibFunc, Uint128Concrete,
    Uint128ConstConcreteLibFunc, Uint128OperationConcreteLibFunc,
    Uint128OperationWithConstConcreteLibFunc,
};
use crate::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
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
        Uint128(libfunc) => simulate_integer_libfunc(libfunc, &inputs),
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
    libfunc: &Uint128Concrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibFuncSimulationError> {
    match libfunc {
        Uint128Concrete::Const(Uint128ConstConcreteLibFunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uint128(*c)], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Binary(
            Uint128BinaryOperationConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)],
                IntOperator::WrappingAdd | IntOperator::WrappingSub | IntOperator::WrappingMul,
            ) => Ok((
                vec![CoreValue::Uint128(match operator {
                    IntOperator::WrappingAdd => lhs.wrapping_add(*rhs),
                    IntOperator::WrappingSub => lhs.wrapping_sub(*rhs),
                    IntOperator::WrappingMul => lhs.wrapping_mul(*rhs),
                    _ => unreachable!("Arm only handles these cases."),
                })],
                0,
            )),
            (
                [CoreValue::Uint128(lhs), CoreValue::NonZero(non_zero)],
                IntOperator::Div | IntOperator::Mod,
            ) => {
                if let CoreValue::Uint128(rhs) = **non_zero {
                    Ok((
                        vec![CoreValue::Uint128(match operator {
                            IntOperator::Div => lhs / rhs,
                            IntOperator::Mod => lhs % rhs,
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
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Const(
            Uint128OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::Uint128(value)] => Ok((
                vec![CoreValue::Uint128(match operator {
                    IntOperator::WrappingAdd => value.wrapping_add(*c),
                    IntOperator::WrappingSub => value.wrapping_sub(*c),
                    IntOperator::WrappingMul => value.wrapping_mul(*c),
                    IntOperator::Div => value / *c,
                    IntOperator::Mod => value % *c,
                })],
                0,
            )),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::JumpNotZero(_) => {
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
        FeltConcrete::Const(FeltConstConcreteLibFunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Felt(*c as i128)], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        FeltConcrete::Operation(FeltOperationConcreteLibFunc::Binary(
            FeltBinaryOperationConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::Felt(lhs), CoreValue::Felt(rhs)],
                FeltOperator::Add | FeltOperator::Sub | FeltOperator::Mul,
            ) => Ok((
                vec![CoreValue::Felt(match operator {
                    FeltOperator::Add => lhs + rhs,
                    FeltOperator::Sub => lhs - rhs,
                    FeltOperator::Mul => lhs * rhs,
                    _ => unreachable!("Arm only handles these cases."),
                })],
                0,
            )),
            ([CoreValue::Felt(_lhs), CoreValue::NonZero(non_zero)], FeltOperator::Div) => {
                if let CoreValue::Felt(_rhs) = **non_zero {
                    todo!("Support felt_div operation.")
                } else {
                    Err(LibFuncSimulationError::MemoryLayoutMismatch)
                }
            }
            ([_, _], _) => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::Operation(FeltOperationConcreteLibFunc::Const(
            FeltOperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::Felt(value)] => Ok((
                vec![CoreValue::Felt(match operator {
                    FeltOperator::Add => value + *c,
                    FeltOperator::Sub => value - *c,
                    FeltOperator::Mul => value * *c,
                    FeltOperator::Div => todo!("Support full felt operations."),
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
