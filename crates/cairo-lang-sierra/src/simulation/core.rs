use std::collections::HashMap;
use std::str::FromStr;

use cairo_lang_utils::extract_matches;
use num_bigint::{BigInt, ToBigInt};
use num_traits::{ToPrimitive, Zero};

use super::value::CoreValue;
use super::LibfuncSimulationError;
use crate::extensions::array::ArrayConcreteLibfunc;
use crate::extensions::boolean::BoolConcreteLibfunc;
use crate::extensions::core::CoreConcreteLibfunc::{
    self, ApTracking, Array, Bool, BranchAlign, Drop, Dup, Ec, Enum, Felt252, FunctionCall, Gas,
    Mem, Sint128, Sint16, Sint32, Sint64, Sint8, Struct, Uint128, Uint16, Uint32, Uint64, Uint8,
    UnconditionalJump, UnwrapNonZero,
};
use crate::extensions::ec::EcConcreteLibfunc;
use crate::extensions::enm::{EnumConcreteLibfunc, EnumInitConcreteLibfunc};
use crate::extensions::felt252::{
    Felt252BinaryOpConcreteLibfunc, Felt252BinaryOperationConcrete, Felt252BinaryOperator,
    Felt252Concrete, Felt252ConstConcreteLibfunc, Felt252OperationWithConstConcreteLibfunc,
};
use crate::extensions::felt252_dict::Felt252DictConcreteLibfunc;
use crate::extensions::function_call::FunctionCallConcreteLibfunc;
use crate::extensions::gas::GasConcreteLibfunc::{
    BuiltinWithdrawGas, GetAvailableGas, GetBuiltinCosts, RedepositGas, WithdrawGas,
};
use crate::extensions::int::unsigned::{
    Uint16Concrete, Uint32Concrete, Uint64Concrete, Uint8Concrete,
};
use crate::extensions::int::unsigned128::Uint128Concrete;
use crate::extensions::int::{IntConstConcreteLibfunc, IntOperator};
use crate::extensions::mem::MemConcreteLibfunc::{
    AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use crate::extensions::structure::StructConcreteLibfunc;
use crate::ids::FunctionId;

// TODO(orizi): This def is duplicated.
/// Returns the Beta value of the Starkware elliptic curve.
fn get_beta() -> BigInt {
    BigInt::from_str("3141592653589793238462643383279502884197169399375105820974944592307816406665")
        .unwrap()
}

// TODO(spapini): Proper errors when converting from bigint to u128.
/// Simulates the run of a single libfunc. Returns the value representations of the outputs, and
/// the chosen branch given the inputs.
///
/// `simulate_function` is a function that simulates running of a user function. It is provided here
/// for the case where the extensions need to use it.
pub fn simulate<
    GetStatementGasInfo: Fn() -> Option<i64>,
    SimulateFunction: Fn(&FunctionId, Vec<CoreValue>) -> Result<Vec<CoreValue>, LibfuncSimulationError>,
>(
    libfunc: &CoreConcreteLibfunc,
    inputs: Vec<CoreValue>,
    get_statement_gas_info: GetStatementGasInfo,
    simulate_function: SimulateFunction,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        Drop(_) => match &inputs[..] {
            [_] => Ok((vec![], 0)),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Dup(_) => match &inputs[..] {
            [value] => Ok((vec![value.clone(), value.clone()], 0)),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Ec(libfunc) => match libfunc {
            EcConcreteLibfunc::TryNew(_) => match &inputs[..] {
                [CoreValue::Felt252(x), CoreValue::Felt252(y)] => {
                    // If the point is on the curve use the fallthrough branch and return the point.
                    if y * y == x * x * x + x + get_beta() {
                        Ok((vec![CoreValue::EcPoint(x.clone(), y.clone())], 0))
                    } else {
                        Ok((vec![], 1))
                    }
                }
                [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            },
            EcConcreteLibfunc::UnwrapPoint(_) => match &inputs[..] {
                [CoreValue::EcPoint(x, y)] => {
                    Ok((vec![CoreValue::Felt252(x.clone()), CoreValue::Felt252(y.clone())], 0))
                }
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            },
            _ => unimplemented!(),
        },
        FunctionCall(FunctionCallConcreteLibfunc { function, .. }) => {
            Ok((simulate_function(&function.id, inputs)?, 0))
        }
        Gas(WithdrawGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibfuncSimulationError::UnresolvedStatementGasInfo)?;
            let gas_counter = match &inputs[..] {
                [CoreValue::RangeCheck, CoreValue::GasBuiltin(value)] => Ok(value),
                [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }?;
            if *gas_counter >= count {
                // Have enough gas - return reduced counter and jump to success branch.
                Ok((vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(gas_counter - count)], 0))
            } else {
                // Don't have enough gas - return the same counter and jump to failure branch.
                Ok((vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(*gas_counter)], 1))
            }
        }
        Gas(RedepositGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibfuncSimulationError::UnresolvedStatementGasInfo)?;
            let gas_counter = match &inputs[..] {
                [CoreValue::GasBuiltin(value)] => Ok(value),
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }?;
            Ok((vec![CoreValue::GasBuiltin(gas_counter + count)], 0))
        }
        Gas(GetAvailableGas(_)) => {
            let gas_counter = match &inputs[..] {
                [CoreValue::GasBuiltin(value)] => Ok(value),
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }?;
            Ok((
                vec![CoreValue::GasBuiltin(*gas_counter), CoreValue::Uint128(*gas_counter as u128)],
                0,
            ))
        }
        Gas(BuiltinWithdrawGas(_) | GetBuiltinCosts(_)) => {
            unimplemented!("Simulation of the builtin cost functionality is not implemented yet.")
        }
        BranchAlign(_) => {
            get_statement_gas_info().ok_or(LibfuncSimulationError::UnresolvedStatementGasInfo)?;
            Ok((vec![], 0))
        }
        Array(ArrayConcreteLibfunc::New(_)) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Array(vec![])], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Array(ArrayConcreteLibfunc::Append(_)) => match &inputs[..] {
            [CoreValue::Array(_), _] => {
                let mut iter = inputs.into_iter();
                let mut arr = extract_matches!(iter.next().unwrap(), CoreValue::Array);
                arr.push(iter.next().unwrap());
                Ok((vec![CoreValue::Array(arr)], 0))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Array(ArrayConcreteLibfunc::PopFront(_) | ArrayConcreteLibfunc::PopFrontConsume(_)) => {
            match &inputs[..] {
                [CoreValue::Array(_)] => {
                    let mut iter = inputs.into_iter();
                    let mut arr = extract_matches!(iter.next().unwrap(), CoreValue::Array);
                    if arr.is_empty() {
                        Ok((vec![CoreValue::Array(arr)], 1))
                    } else {
                        let front = arr.remove(0);
                        if matches!(libfunc, Array(ArrayConcreteLibfunc::PopFrontConsume(_))) {
                            Ok((vec![front], 0))
                        } else {
                            Ok((vec![CoreValue::Array(arr), front], 0))
                        }
                    }
                }
                [_] => Err(LibfuncSimulationError::WrongArgType),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
        Array(ArrayConcreteLibfunc::Get(_)) => match &inputs[..] {
            [CoreValue::RangeCheck, CoreValue::Array(_), CoreValue::Uint64(_)] => {
                let mut iter = inputs.into_iter();
                iter.next(); // Ignore range check.
                let arr = extract_matches!(iter.next().unwrap(), CoreValue::Array);
                let idx = extract_matches!(iter.next().unwrap(), CoreValue::Uint64) as usize;
                match arr.get(idx).cloned() {
                    Some(element) => Ok((vec![CoreValue::RangeCheck, element], 0)),
                    None => Ok((vec![CoreValue::RangeCheck], 1)),
                }
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Array(ArrayConcreteLibfunc::Slice(_)) => match &inputs[..] {
            [
                CoreValue::RangeCheck,
                CoreValue::Array(_),
                CoreValue::Uint32(_),
                CoreValue::Uint32(_),
            ] => {
                let mut iter = inputs.into_iter();
                iter.next(); // Ignore range check.
                let arr = extract_matches!(iter.next().unwrap(), CoreValue::Array);
                let start = extract_matches!(iter.next().unwrap(), CoreValue::Uint32) as usize;
                let length = extract_matches!(iter.next().unwrap(), CoreValue::Uint32) as usize;
                match arr.get(start..(start + length)) {
                    Some(elements) => {
                        Ok((vec![CoreValue::RangeCheck, CoreValue::Array(elements.to_vec())], 0))
                    }
                    None => Ok((vec![CoreValue::RangeCheck], 1)),
                }
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Array(ArrayConcreteLibfunc::Len(_)) => match &inputs[..] {
            [CoreValue::Array(_)] => {
                let arr = extract_matches!(inputs.into_iter().next().unwrap(), CoreValue::Array);
                let len = arr.len();
                Ok((vec![CoreValue::Uint64(len as u64)], 0))
            }
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Array(ArrayConcreteLibfunc::SnapshotPopFront(_)) => todo!(),
        Array(ArrayConcreteLibfunc::SnapshotPopBack(_)) => todo!(),
        Uint8(libfunc) => simulate_u8_libfunc(libfunc, &inputs),
        Uint16(libfunc) => simulate_u16_libfunc(libfunc, &inputs),
        Uint32(libfunc) => simulate_u32_libfunc(libfunc, &inputs),
        Uint64(libfunc) => simulate_u64_libfunc(libfunc, &inputs),
        Uint128(libfunc) => simulate_u128_libfunc(libfunc, &inputs),
        Sint8(_) | Sint16(_) | Sint32(_) | Sint64(_) | Sint128(_) => {
            unimplemented!("Simulation of signed integer libfuncs is not implemented yet.")
        }
        Bool(libfunc) => simulate_bool_libfunc(libfunc, &inputs),
        Felt252(libfunc) => simulate_felt252_libfunc(libfunc, &inputs),
        UnwrapNonZero(_) => match &inputs[..] {
            [CoreValue::NonZero(value)] => Ok((vec![*value.clone()], 0)),
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Mem(Rename(_) | StoreTemp(_)) | CoreConcreteLibfunc::Box(_) => {
            if inputs.len() == 1 {
                Ok((inputs, 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Mem(FinalizeLocals(_)) | UnconditionalJump(_) | ApTracking(_) => {
            if inputs.is_empty() {
                Ok((inputs, 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Mem(StoreLocal(_)) => match &inputs[..] {
            [CoreValue::Uninitialized, other] => Ok((vec![other.clone()], 0)),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Mem(AllocLocal(_)) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uninitialized], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Enum(EnumConcreteLibfunc::Init(EnumInitConcreteLibfunc { index, .. })) => {
            match &inputs[..] {
                [input] => {
                    // We don't verify here that the input type matches the signature.
                    Ok((vec![CoreValue::Enum { value: Box::new(input.clone()), index: *index }], 0))
                }
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
        Enum(EnumConcreteLibfunc::Match(_) | EnumConcreteLibfunc::SnapshotMatch(_)) => {
            match &inputs[..] {
                [CoreValue::Enum { value, index }] => Ok((vec![*value.clone()], *index)),
                [_] => Err(LibfuncSimulationError::WrongArgType),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
        Struct(StructConcreteLibfunc::Construct(_)) => Ok((vec![CoreValue::Struct(inputs)], 0)),
        Struct(
            StructConcreteLibfunc::Deconstruct(_) | StructConcreteLibfunc::SnapshotDeconstruct(_),
        ) => match &inputs[..] {
            [CoreValue::Struct(_)] => {
                // Extracting the values instead of cloning them, as the match is on a reference.
                Ok((extract_matches!(inputs.into_iter().next().unwrap(), CoreValue::Struct), 0))
            }
            [_] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        CoreConcreteLibfunc::Felt252Dict(Felt252DictConcreteLibfunc::New(_)) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Dict(HashMap::new())], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        CoreConcreteLibfunc::Felt252Dict(Felt252DictConcreteLibfunc::Squash(_)) => {
            match &inputs[..] {
                [CoreValue::RangeCheck, CoreValue::Dict(_)] => {
                    let mut iter = inputs.into_iter();
                    iter.next();
                    // Returning the same dict since it is exactly the same as the squashed one.
                    let dict = extract_matches!(iter.next().unwrap(), CoreValue::Dict);
                    Ok((vec![CoreValue::RangeCheck, CoreValue::Dict(dict)], 0))
                }
                [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
        CoreConcreteLibfunc::Pedersen(_) => {
            unimplemented!("Simulation of the Pedersen hash function is not implemented yet.");
        }
        CoreConcreteLibfunc::Poseidon(_) => {
            unimplemented!("Simulation of the Poseidon hash function is not implemented yet.");
        }
        CoreConcreteLibfunc::StarkNet(_) => {
            unimplemented!("Simulation of the StarkNet functionalities is not implemented yet.")
        }
        CoreConcreteLibfunc::Nullable(_) => {
            unimplemented!("Simulation of nullable is not implemented yet.")
        }
        CoreConcreteLibfunc::Debug(_) => {
            if inputs.len() == 1 {
                let arr = extract_matches!(&inputs[0], CoreValue::Array);
                let mut bytes = Vec::new();
                for limb in arr {
                    let limb = extract_matches!(limb, CoreValue::Felt252);
                    // TODO(spapini): What to do with the sign?
                    let (_sign, limb_bytes) = limb.to_bytes_be();
                    // Currently, we ignore leading zeros. That might need to change.
                    bytes.extend(limb_bytes);
                }
                if let Ok(s) = String::from_utf8(bytes) {
                    print!("{s}");
                } else {
                    println!("Not utf8");
                }
                Ok((vec![], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        CoreConcreteLibfunc::SnapshotTake(_) => match &inputs[..] {
            [value] => Ok((vec![value.clone(), value.clone()], 0)),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        CoreConcreteLibfunc::Cast(_) => unimplemented!(),
        CoreConcreteLibfunc::Felt252DictEntry(_) => unimplemented!(),
        CoreConcreteLibfunc::Uint256(_) => unimplemented!(),
        CoreConcreteLibfunc::Uint512(_) => unimplemented!(),
        CoreConcreteLibfunc::Bytes31(_) => unimplemented!(),
    }
}

/// Simulate boolean library functions.
fn simulate_bool_libfunc(
    libfunc: &BoolConcreteLibfunc,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        BoolConcreteLibfunc::And(_) => match inputs {
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
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        BoolConcreteLibfunc::Not(_) => match inputs {
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
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        BoolConcreteLibfunc::Xor(_) => match inputs {
            [CoreValue::Enum { index: a_index, .. }, CoreValue::Enum { index: b_index, .. }] => {
                // The variant index defines the true/false "value". Index zero is false.
                Ok((
                    vec![CoreValue::Enum {
                        value: Box::new(CoreValue::Struct(vec![])),
                        index: usize::from(*a_index != *b_index),
                    }],
                    0,
                ))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        BoolConcreteLibfunc::Or(_) => match inputs {
            [CoreValue::Enum { index: a_index, .. }, CoreValue::Enum { index: b_index, .. }] => {
                let (a, b) = (*a_index, *b_index);
                // The variant index defines the true/false "value". Index zero is false.
                Ok((
                    vec![CoreValue::Enum {
                        value: Box::new(CoreValue::Struct(vec![])),
                        index: usize::from(a + b > 0),
                    }],
                    0,
                ))
            }
            [_, _] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        BoolConcreteLibfunc::ToFelt252(_) => match inputs {
            [CoreValue::Enum { index, .. }] => {
                // The variant index defines the true/false "value". Index zero is false.
                Ok((vec![CoreValue::Felt252(BigInt::from(*index))], 0))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
    }
}

/// Simulate u128 library functions.
fn simulate_u128_libfunc(
    libfunc: &Uint128Concrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        Uint128Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uint128(*c)], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Uint128Concrete::FromFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Felt252(value)] => Ok(match u128::try_from(value) {
                Ok(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], 0),
                Err(_) => (vec![CoreValue::RangeCheck], 1),
            }),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::ToFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(value)] => {
                Ok((vec![CoreValue::Felt252(value.to_bigint().unwrap())], 0))
            }
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::Operation(libfunc) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)] => {
                let (value, overflow) = match libfunc.operator {
                    IntOperator::OverflowingAdd => lhs.overflowing_add(*rhs),
                    IntOperator::OverflowingSub => lhs.overflowing_sub(*rhs),
                };
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], usize::from(overflow)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::Divmod(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(lhs), CoreValue::NonZero(non_zero)] => {
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
                    Err(LibfuncSimulationError::MemoryLayoutMismatch)
                }
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::GuaranteeMul(_) | Uint128Concrete::MulGuaranteeVerify(_) => {
            unimplemented!()
        }
        Uint128Concrete::IsZero(_) => {
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
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
        Uint128Concrete::SquareRoot(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(value)] => {
                let root = BigInt::from(*value).sqrt();
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint128(root.to_u128().unwrap())], 0))
            }
            [_, _] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::Equal(_) => match inputs {
            [CoreValue::Uint128(a), CoreValue::Uint128(b)] => {
                // "False" branch (branch 0) is the case a != b.
                // "True" branch (branch 1) is the case a == b.
                Ok((vec![], usize::from(a == b)))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::ByteReverse(_) => todo!("ByteReverse"),
        Uint128Concrete::Bitwise(_) => match inputs {
            [CoreValue::Uint128(a), CoreValue::Uint128(b)] => Ok((
                vec![
                    CoreValue::Uint128(a & b),
                    CoreValue::Uint128(a | b),
                    CoreValue::Uint128(a ^ b),
                ],
                0,
            )),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
    }
}

/// Simulate u8 library functions.
fn simulate_u8_libfunc(
    libfunc: &Uint8Concrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        Uint8Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uint8(*c)], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Uint8Concrete::Operation(libfunc) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint8(lhs), CoreValue::Uint8(rhs)] => {
                let (value, overflow) = match libfunc.operator {
                    IntOperator::OverflowingAdd => lhs.overflowing_add(*rhs),
                    IntOperator::OverflowingSub => lhs.overflowing_sub(*rhs),
                };
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint8(value)], usize::from(overflow)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint8Concrete::SquareRoot(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint8(value)] => {
                let root = BigInt::from(*value).sqrt();
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint8(root.to_u8().unwrap())], 0))
            }
            [_, _] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint8Concrete::Equal(_) => match inputs {
            [CoreValue::Uint8(a), CoreValue::Uint8(b)] => {
                // "False" branch (branch 0) is the case a != b.
                // "True" branch (branch 1) is the case a == b.
                Ok((vec![], usize::from(a == b)))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint8Concrete::ToFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint8(value)] => {
                Ok((vec![CoreValue::Felt252(value.to_bigint().unwrap())], 0))
            }
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint8Concrete::FromFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Felt252(value)] => Ok(match u8::try_from(value) {
                Ok(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint8(value)], 0),
                Err(_) => (vec![CoreValue::RangeCheck], 1),
            }),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint8Concrete::IsZero(_) => unimplemented!(),
        Uint8Concrete::Divmod(_) => unimplemented!(),
        Uint8Concrete::Bitwise(_) => unimplemented!(),
        Uint8Concrete::WideMul(_) => match inputs {
            [CoreValue::Uint8(lhs), CoreValue::Uint8(rhs)] => {
                Ok((vec![CoreValue::Uint16(u16::from(*lhs) * u16::from(*rhs))], 0))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
    }
}

/// Simulate u16 library functions.
fn simulate_u16_libfunc(
    libfunc: &Uint16Concrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        Uint16Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uint16(*c)], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Uint16Concrete::Operation(libfunc) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint16(lhs), CoreValue::Uint16(rhs)] => {
                let (value, overflow) = match libfunc.operator {
                    IntOperator::OverflowingAdd => lhs.overflowing_add(*rhs),
                    IntOperator::OverflowingSub => lhs.overflowing_sub(*rhs),
                };
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint16(value)], usize::from(overflow)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint16Concrete::SquareRoot(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint16(value)] => {
                let root = BigInt::from(*value).sqrt();
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint16(root.to_u16().unwrap())], 0))
            }
            [_, _] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint16Concrete::Equal(_) => match inputs {
            [CoreValue::Uint16(a), CoreValue::Uint16(b)] => {
                // "False" branch (branch 0) is the case a != b.
                // "True" branch (branch 1) is the case a == b.
                Ok((vec![], usize::from(a == b)))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint16Concrete::ToFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint16(value)] => {
                Ok((vec![CoreValue::Felt252(value.to_bigint().unwrap())], 0))
            }
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint16Concrete::FromFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Felt252(value)] => Ok(match u16::try_from(value) {
                Ok(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint16(value)], 0),
                Err(_) => (vec![CoreValue::RangeCheck], 1),
            }),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint16Concrete::IsZero(_) => unimplemented!(),
        Uint16Concrete::Divmod(_) => unimplemented!(),
        Uint16Concrete::Bitwise(_) => unimplemented!(),
        Uint16Concrete::WideMul(_) => match inputs {
            [CoreValue::Uint16(lhs), CoreValue::Uint16(rhs)] => {
                Ok((vec![CoreValue::Uint32(u32::from(*lhs) * u32::from(*rhs))], 0))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
    }
}

/// Simulate u32 library functions.
fn simulate_u32_libfunc(
    libfunc: &Uint32Concrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        Uint32Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uint32(*c)], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Uint32Concrete::Operation(libfunc) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint32(lhs), CoreValue::Uint32(rhs)] => {
                let (value, overflow) = match libfunc.operator {
                    IntOperator::OverflowingAdd => lhs.overflowing_add(*rhs),
                    IntOperator::OverflowingSub => lhs.overflowing_sub(*rhs),
                };
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint32(value)], usize::from(overflow)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint32Concrete::SquareRoot(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint32(value)] => {
                let root = BigInt::from(*value).sqrt();
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint32(root.to_u32().unwrap())], 0))
            }
            [_, _] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint32Concrete::Equal(_) => match inputs {
            [CoreValue::Uint32(a), CoreValue::Uint32(b)] => {
                // "False" branch (branch 0) is the case a != b.
                // "True" branch (branch 1) is the case a == b.
                Ok((vec![], usize::from(a == b)))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint32Concrete::ToFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint32(value)] => {
                Ok((vec![CoreValue::Felt252(value.to_bigint().unwrap())], 0))
            }
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint32Concrete::FromFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Felt252(value)] => Ok(match u32::try_from(value) {
                Ok(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint32(value)], 0),
                Err(_) => (vec![CoreValue::RangeCheck], 1),
            }),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint32Concrete::IsZero(_) => unimplemented!(),
        Uint32Concrete::Divmod(_) => unimplemented!(),
        Uint32Concrete::Bitwise(_) => unimplemented!(),
        Uint32Concrete::WideMul(_) => match inputs {
            [CoreValue::Uint32(lhs), CoreValue::Uint32(rhs)] => {
                Ok((vec![CoreValue::Uint64(u64::from(*lhs) * u64::from(*rhs))], 0))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
    }
}

/// Simulate u64 library functions.
fn simulate_u64_libfunc(
    libfunc: &Uint64Concrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        Uint64Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uint64(*c)], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Uint64Concrete::Operation(libfunc) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint64(lhs), CoreValue::Uint64(rhs)] => {
                let (value, overflow) = match libfunc.operator {
                    IntOperator::OverflowingAdd => lhs.overflowing_add(*rhs),
                    IntOperator::OverflowingSub => lhs.overflowing_sub(*rhs),
                };
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint64(value)], usize::from(overflow)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint64Concrete::SquareRoot(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint64(value)] => {
                let root = BigInt::from(*value).sqrt();
                Ok((vec![CoreValue::RangeCheck, CoreValue::Uint64(root.to_u64().unwrap())], 0))
            }
            [_, _] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint64Concrete::Equal(_) => match inputs {
            [CoreValue::Uint64(a), CoreValue::Uint64(b)] => {
                // "False" branch (branch 0) is the case a != b.
                // "True" branch (branch 1) is the case a == b.
                Ok((vec![], usize::from(a == b)))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint64Concrete::ToFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint64(value)] => {
                Ok((vec![CoreValue::Felt252(value.to_bigint().unwrap())], 0))
            }
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint64Concrete::FromFelt252(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Felt252(value)] => Ok(match u64::try_from(value) {
                Ok(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint64(value)], 0),
                Err(_) => (vec![CoreValue::RangeCheck], 1),
            }),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint64Concrete::IsZero(_) => unimplemented!(),
        Uint64Concrete::Divmod(_) => unimplemented!(),
        Uint64Concrete::Bitwise(_) => unimplemented!(),
        Uint64Concrete::WideMul(_) => match inputs {
            [CoreValue::Uint64(lhs), CoreValue::Uint64(rhs)] => {
                Ok((vec![CoreValue::Uint128(u128::from(*lhs) * u128::from(*rhs))], 0))
            }
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
    }
}

/// Simulate felt252 library functions.
fn simulate_felt252_libfunc(
    libfunc: &Felt252Concrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        Felt252Concrete::Const(Felt252ConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Felt252(c.to_bigint().unwrap())], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Felt252Concrete::BinaryOperation(Felt252BinaryOperationConcrete::WithVar(
            Felt252BinaryOpConcreteLibfunc { operator, .. },
        )) => match (inputs, operator) {
            (
                [CoreValue::Felt252(lhs), CoreValue::Felt252(rhs)],
                Felt252BinaryOperator::Add
                | Felt252BinaryOperator::Sub
                | Felt252BinaryOperator::Mul,
            ) => Ok((
                vec![CoreValue::Felt252(match operator {
                    Felt252BinaryOperator::Add => lhs + rhs,
                    Felt252BinaryOperator::Sub => lhs - rhs,
                    Felt252BinaryOperator::Mul => lhs * rhs,
                    _ => unreachable!("Arm only handles these cases."),
                })],
                0,
            )),
            (
                [CoreValue::Felt252(_lhs), CoreValue::NonZero(non_zero)],
                Felt252BinaryOperator::Div,
            ) => {
                if let CoreValue::Felt252(_rhs) = *non_zero.clone() {
                    todo!("Support felt252_div operation.")
                } else {
                    Err(LibfuncSimulationError::MemoryLayoutMismatch)
                }
            }
            ([_, _], _) => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Felt252Concrete::BinaryOperation(Felt252BinaryOperationConcrete::WithConst(
            Felt252OperationWithConstConcreteLibfunc { operator, c, .. },
        )) => match inputs {
            [CoreValue::Felt252(value)] => Ok((
                vec![CoreValue::Felt252(match operator {
                    Felt252BinaryOperator::Add => value + c.clone(),
                    Felt252BinaryOperator::Sub => value - c.clone(),
                    Felt252BinaryOperator::Mul => value * c.clone(),
                    Felt252BinaryOperator::Div => todo!("Support full felt252 operations."),
                })],
                0,
            )),
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Felt252Concrete::IsZero(_) => {
            match inputs {
                [CoreValue::Felt252(value)] if value.is_zero() => {
                    // Zero - jumping to the failure branch.
                    Ok((vec![], 0))
                }
                [CoreValue::Felt252(value)] if !value.is_zero() => {
                    // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                    // given value.
                    Ok((vec![CoreValue::NonZero(Box::new(CoreValue::Felt252(value.clone())))], 1))
                }
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
    }
}
