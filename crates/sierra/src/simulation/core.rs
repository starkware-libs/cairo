use std::collections::HashMap;

use num_bigint::ToBigInt;
use num_traits::Zero;
use utils::extract_matches;

use super::value::CoreValue;
use super::LibFuncSimulationError;
use crate::extensions::array::ArrayConcreteLibFunc;
use crate::extensions::boolean::BoolConcreteLibFunc;
use crate::extensions::core::CoreConcreteLibFunc::{
    self, ApTracking, Array, Bitwise, Bool, BranchAlign, Drop, Dup, Enum, Felt, FunctionCall, Gas,
    Mem, Struct, Uint128, UnconditionalJump, UnwrapNonZero,
};
use crate::extensions::dict_felt_to::DictFeltToConcreteLibFunc;
use crate::extensions::enm::{EnumConcreteLibFunc, EnumInitConcreteLibFunc};
use crate::extensions::felt::{
    FeltBinaryOpConcreteLibFunc, FeltBinaryOperationConcreteLibFunc, FeltBinaryOperator,
    FeltConcrete, FeltConstConcreteLibFunc, FeltOperationWithConstConcreteLibFunc,
    FeltUnaryOpConcreteLibFunc, FeltUnaryOperationConcreteLibFunc, FeltUnaryOperator,
};
use crate::extensions::function_call::FunctionCallConcreteLibFunc;
use crate::extensions::gas::GasConcreteLibFunc::{GetGas, RefundGas};
use crate::extensions::mem::MemConcreteLibFunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use crate::extensions::strct::StructConcreteLibFunc;
use crate::extensions::uint128::{
    IntOperator, Uint128BinaryOperationConcreteLibFunc, Uint128Concrete,
    Uint128ConstConcreteLibFunc, Uint128OperationConcreteLibFunc,
    Uint128OperationWithConstConcreteLibFunc,
};
use crate::ids::FunctionId;

// TODO(spapini): Proper errors when converting from bigint to u128.
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
        Bitwise(_) => match &inputs[..] {
            [CoreValue::Uint128(a), CoreValue::Uint128(b)] => Ok((
                vec![
                    CoreValue::Uint128(a & b),
                    CoreValue::Uint128(a | b),
                    CoreValue::Uint128(a ^ b),
                ],
                0,
            )),
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
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
                [CoreValue::RangeCheck, CoreValue::GasBuiltin(value)] => Ok(value),
                [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }?;
            if *gas_counter >= count {
                // Have enough gas - return reduced counter and jump to success branch.
                Ok((vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(gas_counter - count)], 0))
            } else {
                // Don't have enough gas - return the same counter and jump to failure branch.
                Ok((vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(*gas_counter)], 1))
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
        BranchAlign(_) => {
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
        Array(ArrayConcreteLibFunc::At(_)) => match &inputs[..] {
            [CoreValue::RangeCheck, CoreValue::Array(_), CoreValue::Uint128(_)] => {
                let mut iter = inputs.into_iter();
                iter.next(); // Ignore range check.
                let arr = extract_matches!(iter.next().unwrap(), CoreValue::Array);
                let idx = extract_matches!(iter.next().unwrap(), CoreValue::Uint128) as usize;
                match arr.get(idx).cloned() {
                    Some(element) => {
                        Ok((vec![CoreValue::RangeCheck, CoreValue::Array(arr), element], 0))
                    }
                    None => Ok((vec![CoreValue::RangeCheck, CoreValue::Array(arr)], 1)),
                }
            }
            [_, _, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Array(ArrayConcreteLibFunc::Len(_)) => match &inputs[..] {
            [CoreValue::Array(_)] => {
                let arr = extract_matches!(inputs.into_iter().next().unwrap(), CoreValue::Array);
                let len = arr.len();
                Ok((vec![CoreValue::Array(arr), CoreValue::Uint128(len as u128)], 0))
            }
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Uint128(libfunc) => simulate_integer_libfunc(libfunc, &inputs),
        Bool(libfunc) => simulate_bool_libfunc(libfunc, &inputs),
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
        Struct(StructConcreteLibFunc::Construct(_)) => Ok((vec![CoreValue::Struct(inputs)], 0)),
        Struct(StructConcreteLibFunc::Deconstruct(_)) => match &inputs[..] {
            [CoreValue::Struct(_)] => {
                // Extracting the values instead of cloning them, as the match is on a reference.
                Ok((extract_matches!(inputs.into_iter().next().unwrap(), CoreValue::Struct), 0))
            }
            [_] => Err(LibFuncSimulationError::WrongArgType),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        CoreConcreteLibFunc::DictFeltTo(DictFeltToConcreteLibFunc::New(_)) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Dict(HashMap::new())], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        CoreConcreteLibFunc::DictFeltTo(DictFeltToConcreteLibFunc::Read(_)) => {
            match &inputs[..] {
                [CoreValue::Dict(map), CoreValue::Felt(key)] => {
                    // Returns 0 as a defualt value.
                    // TODO(Gil): correct this behaviour when dict behaviour is decided on key not
                    // found.
                    Ok((vec![map.get(key).map_or(CoreValue::Felt(0.into()), |x| x.clone())], 0))
                }
                [_, _] => Err(LibFuncSimulationError::WrongArgType),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
        CoreConcreteLibFunc::DictFeltTo(DictFeltToConcreteLibFunc::Write(_)) => match &inputs[..] {
            [CoreValue::Dict(_), CoreValue::Felt(_), _] => {
                let mut iter = inputs.into_iter();
                let mut dict = extract_matches!(iter.next().unwrap(), CoreValue::Dict);
                let key = extract_matches!(iter.next().unwrap(), CoreValue::Felt);
                dict.insert(key, iter.next().unwrap());
                Ok((vec![CoreValue::Dict(dict)], 0))
            }
            [_, _, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        CoreConcreteLibFunc::DictFeltTo(DictFeltToConcreteLibFunc::Squash(_)) => {
            match &inputs[..] {
                [CoreValue::RangeCheck, CoreValue::Dict(_)] => {
                    let mut iter = inputs.into_iter();
                    iter.next();
                    // Returning the same dict since it is exactly the same as the squashed one.
                    let dict = extract_matches!(iter.next().unwrap(), CoreValue::Dict);
                    Ok((vec![CoreValue::RangeCheck, CoreValue::Dict(dict)], 0))
                }
                [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
        CoreConcreteLibFunc::Pedersen(_) => {
            unimplemented!("Simulation of the Pedersen hash function is not implemented yet.");
        }
        CoreConcreteLibFunc::BuiltinCost(_) => {
            todo!("Simulation of the builtin cost functionality is not implemented yet.")
        }
        &CoreConcreteLibFunc::StarkNet(_) => {
            unimplemented!("Simulation of the StarkNet functionalities is not implemented yet.")
        }
    }
}

/// Simulate boolean library functions.
fn simulate_bool_libfunc(
    libfunc: &BoolConcreteLibFunc,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibFuncSimulationError> {
    match libfunc {
        BoolConcreteLibFunc::And(_) => match inputs {
            [CoreValue::Enum { index: a_index, .. }, CoreValue::Enum { index: b_index, .. }] => {
                // The variant index defines the true/false "value". Index zero is false.
                Ok((
                    vec![CoreValue::Enum {
                        value: Box::new(CoreValue::Struct(vec![])),
                        index: usize::from(*a_index == 1_usize && *b_index == 1_usize),
                    }],
                    0,
                ))
            }
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        BoolConcreteLibFunc::Not(_) => match inputs {
            [CoreValue::Enum { index, .. }] => {
                // The variant index defines the true/false "value". Index zero is false.
                Ok((
                    vec![CoreValue::Enum {
                        value: Box::new(CoreValue::Struct(vec![])),
                        index: 1_usize - *index,
                    }],
                    0,
                ))
            }
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
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
        Uint128Concrete::FromFelt(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Felt(value)] => Ok(match u128::try_from(value) {
                Ok(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], 0),
                Err(_) => (vec![CoreValue::RangeCheck], 1),
            }),
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::ToFelt(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(value)] => {
                Ok((vec![CoreValue::Felt(value.to_bigint().unwrap())], 0))
            }
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Binary(
            Uint128BinaryOperationConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::RangeCheck, CoreValue::Uint128(lhs), CoreValue::NonZero(non_zero)],
                IntOperator::DivMod,
            ) => {
                if let CoreValue::Uint128(rhs) = **non_zero {
                    Ok((
                        vec![
                            CoreValue::RangeCheck,
                            CoreValue::Uint128(lhs / rhs),
                            CoreValue::Uint128(lhs % rhs),
                        ],
                        0,
                    ))
                } else {
                    Err(LibFuncSimulationError::MemoryLayoutMismatch)
                }
            }
            (
                [CoreValue::RangeCheck, CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)],
                IntOperator::OverflowingAdd
                | IntOperator::OverflowingSub
                | IntOperator::OverflowingMul,
            ) => {
                let (value, overflow) = match operator {
                    IntOperator::OverflowingAdd => lhs.overflowing_add(*rhs),
                    IntOperator::OverflowingSub => lhs.overflowing_sub(*rhs),
                    IntOperator::OverflowingMul => lhs.overflowing_mul(*rhs),
                    _ => unreachable!("Arm only handles these cases."),
                };
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], usize::from(overflow)))
            }
            ([_, _, _], _) => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::Operation(Uint128OperationConcreteLibFunc::Const(
            Uint128OperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(value)] => Ok(match operator {
                IntOperator::OverflowingAdd
                | IntOperator::OverflowingSub
                | IntOperator::OverflowingMul => {
                    let (value, overflow) = match operator {
                        IntOperator::OverflowingAdd => value.overflowing_add(*c),
                        IntOperator::OverflowingSub => value.overflowing_sub(*c),
                        IntOperator::OverflowingMul => value.overflowing_mul(*c),
                        _ => unreachable!("Arm only handles these cases."),
                    };
                    (vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], usize::from(overflow))
                }
                IntOperator::DivMod => (
                    vec![
                        CoreValue::RangeCheck,
                        CoreValue::Uint128(value / *c),
                        CoreValue::Uint128(value % *c),
                    ],
                    0,
                ),
            }),
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
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
        Uint128Concrete::LessThan(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(a), CoreValue::Uint128(b)] => {
                // "False" branch (branch 0) is the case a >= b.
                // "True" branch (branch 1) is the case a < b.
                Ok((vec![CoreValue::RangeCheck], usize::from(a < b)))
            }
            [_, _, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::Equal(_) => match inputs {
            [CoreValue::Uint128(a), CoreValue::Uint128(b)] => {
                // "False" branch (branch 0) is the case a != b.
                // "True" branch (branch 1) is the case a == b.
                Ok((vec![], usize::from(a == b)))
            }
            [_, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::LessThanOrEqual(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(a), CoreValue::Uint128(b)] => {
                // "False" branch (branch 0) is the case a > b.
                // "True" branch (branch 1) is the case a <= b.
                Ok((vec![CoreValue::RangeCheck], usize::from(a <= b)))
            }
            [_, _, _] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
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
                Ok((vec![CoreValue::Felt(c.to_bigint().unwrap())], 0))
            } else {
                Err(LibFuncSimulationError::WrongNumberOfArgs)
            }
        }
        FeltConcrete::UnaryOperation(FeltUnaryOperationConcreteLibFunc::Unary(
            FeltUnaryOpConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            ([CoreValue::Felt(val)], FeltUnaryOperator::Neg) => {
                Ok((vec![CoreValue::Felt(-val)], 0))
            }
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibFunc::Binary(
            FeltBinaryOpConcreteLibFunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::Felt(lhs), CoreValue::Felt(rhs)],
                FeltBinaryOperator::Add | FeltBinaryOperator::Sub | FeltBinaryOperator::Mul,
            ) => Ok((
                vec![CoreValue::Felt(match operator {
                    FeltBinaryOperator::Add => lhs + rhs,
                    FeltBinaryOperator::Sub => lhs - rhs,
                    FeltBinaryOperator::Mul => lhs * rhs,
                    _ => unreachable!("Arm only handles these cases."),
                })],
                0,
            )),
            ([CoreValue::Felt(_lhs), CoreValue::NonZero(non_zero)], FeltBinaryOperator::Div) => {
                if let CoreValue::Felt(_rhs) = *non_zero.clone() {
                    todo!("Support felt_div operation.")
                } else {
                    Err(LibFuncSimulationError::MemoryLayoutMismatch)
                }
            }
            ([_, _], _) => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibFunc::Const(
            FeltOperationWithConstConcreteLibFunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::Felt(value)] => Ok((
                vec![CoreValue::Felt(match operator {
                    FeltBinaryOperator::Add => value + c.clone(),
                    FeltBinaryOperator::Sub => value - c.clone(),
                    FeltBinaryOperator::Mul => value * c.clone(),
                    FeltBinaryOperator::Div => todo!("Support full felt operations."),
                })],
                0,
            )),
            [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::JumpNotZero(_) => {
            match inputs {
                [CoreValue::Felt(value)] if value.is_zero() => {
                    // Zero - jumping to the failure branch.
                    Ok((vec![], 0))
                }
                [CoreValue::Felt(value)] if !value.is_zero() => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![CoreValue::NonZero(Box::new(CoreValue::Felt(value.clone())))], 1))
                }
                [_] => Err(LibFuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibFuncSimulationError::WrongNumberOfArgs),
            }
        }
    }
}
