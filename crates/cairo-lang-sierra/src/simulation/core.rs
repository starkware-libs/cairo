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
    self, ApTracking, Array, Bitwise, Bool, BranchAlign, Drop, Dup, Ec, Enum, Felt, FunctionCall,
    Gas, Mem, Struct, Uint128, Uint8, UnconditionalJump, UnwrapNonZero,
};
use crate::extensions::dict_felt_to::DictFeltToConcreteLibfunc;
use crate::extensions::ec::EcConcreteLibfunc;
use crate::extensions::enm::{EnumConcreteLibfunc, EnumInitConcreteLibfunc};
use crate::extensions::felt::{
    FeltBinaryOpConcreteLibfunc, FeltBinaryOperationConcreteLibfunc, FeltBinaryOperator,
    FeltConcrete, FeltConstConcreteLibfunc, FeltOperationWithConstConcreteLibfunc,
};
use crate::extensions::function_call::FunctionCallConcreteLibfunc;
use crate::extensions::gas::GasConcreteLibfunc::{GetGas, RefundGas};
use crate::extensions::mem::MemConcreteLibfunc::{
    AlignTemps, AllocLocal, FinalizeLocals, Rename, StoreLocal, StoreTemp,
};
use crate::extensions::strct::StructConcreteLibfunc;
use crate::extensions::uint::{IntOperator, Uint8Concrete, UintConstConcreteLibfunc};
use crate::extensions::uint128::Uint128Concrete;
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
        Bitwise(_) => match &inputs[..] {
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
                [CoreValue::Felt(x), CoreValue::Felt(y)] => {
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
                    Ok((vec![CoreValue::Felt(x.clone()), CoreValue::Felt(y.clone())], 0))
                }
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            },
            _ => unimplemented!(),
        },
        FunctionCall(FunctionCallConcreteLibfunc { function, .. }) => {
            Ok((simulate_function(&function.id, inputs)?, 0))
        }
        Gas(GetGas(_)) => {
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
        Gas(RefundGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibfuncSimulationError::UnresolvedStatementGasInfo)?;
            let gas_counter = match &inputs[..] {
                [CoreValue::GasBuiltin(value)] => Ok(value),
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }?;
            Ok((vec![CoreValue::GasBuiltin(gas_counter + count)], 0))
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
        Array(ArrayConcreteLibfunc::PopFront(_)) => match &inputs[..] {
            [CoreValue::Array(_)] => {
                let mut iter = inputs.into_iter();
                let mut arr = extract_matches!(iter.next().unwrap(), CoreValue::Array);
                if arr.is_empty() {
                    Ok((vec![CoreValue::Array(arr)], 1))
                } else {
                    let front = arr.remove(0);
                    Ok((vec![CoreValue::Array(arr), front], 0))
                }
            }
            [_] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Array(ArrayConcreteLibfunc::At(_)) => match &inputs[..] {
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
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Array(ArrayConcreteLibfunc::Len(_)) => match &inputs[..] {
            [CoreValue::Array(_)] => {
                let arr = extract_matches!(inputs.into_iter().next().unwrap(), CoreValue::Array);
                let len = arr.len();
                Ok((vec![CoreValue::Array(arr), CoreValue::Uint128(len as u128)], 0))
            }
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint8(libfunc) => simulate_u8_libfunc(libfunc, &inputs),
        Uint128(libfunc) => simulate_u128_libfunc(libfunc, &inputs),
        Bool(libfunc) => simulate_bool_libfunc(libfunc, &inputs),
        Felt(libfunc) => simulate_felt_libfunc(libfunc, &inputs),
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
        Mem(AlignTemps(_)) | Mem(FinalizeLocals(_)) | UnconditionalJump(_) | ApTracking(_) => {
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
        Enum(EnumConcreteLibfunc::Match(_)) => match &inputs[..] {
            [CoreValue::Enum { value, index }] => Ok((vec![*value.clone()], *index)),
            [_] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Struct(StructConcreteLibfunc::Construct(_)) => Ok((vec![CoreValue::Struct(inputs)], 0)),
        Struct(StructConcreteLibfunc::Deconstruct(_)) => match &inputs[..] {
            [CoreValue::Struct(_)] => {
                // Extracting the values instead of cloning them, as the match is on a reference.
                Ok((extract_matches!(inputs.into_iter().next().unwrap(), CoreValue::Struct), 0))
            }
            [_] => Err(LibfuncSimulationError::WrongArgType),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        CoreConcreteLibfunc::DictFeltTo(DictFeltToConcreteLibfunc::New(_)) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Dict(HashMap::new())], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        CoreConcreteLibfunc::DictFeltTo(DictFeltToConcreteLibfunc::Read(_)) => {
            match &inputs[..] {
                [CoreValue::Dict(map), CoreValue::Felt(key)] => {
                    // Returns 0 as a defualt value.
                    // TODO(Gil): correct this behaviour when dict behaviour is decided on key not
                    // found.
                    Ok((vec![map.get(key).map_or(CoreValue::Felt(0.into()), |x| x.clone())], 0))
                }
                [_, _] => Err(LibfuncSimulationError::WrongArgType),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
        CoreConcreteLibfunc::DictFeltTo(DictFeltToConcreteLibfunc::Write(_)) => match &inputs[..] {
            [CoreValue::Dict(_), CoreValue::Felt(_), _] => {
                let mut iter = inputs.into_iter();
                let mut dict = extract_matches!(iter.next().unwrap(), CoreValue::Dict);
                let key = extract_matches!(iter.next().unwrap(), CoreValue::Felt);
                dict.insert(key, iter.next().unwrap());
                Ok((vec![CoreValue::Dict(dict)], 0))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        CoreConcreteLibfunc::DictFeltTo(DictFeltToConcreteLibfunc::Squash(_)) => {
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
        CoreConcreteLibfunc::BuiltinCost(_) => {
            unimplemented!("Simulation of the builtin cost functionality is not implemented yet.")
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
                    let limb = extract_matches!(limb, CoreValue::Felt);
                    // TODO(spapini): What to do with the sign?
                    let (_sign, limb_bytes) = limb.to_bytes_be();
                    // Currently, we ignore leading zeros. That might need to change.
                    bytes.extend(limb_bytes);
                }
                if let Ok(s) = String::from_utf8(bytes) {
                    print!("{}", s);
                } else {
                    println!("Not utf8");
                }
                Ok((vec![], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
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
        BoolConcreteLibfunc::Equal(_) => match inputs {
            [CoreValue::Enum { index: a_index, .. }, CoreValue::Enum { index: b_index, .. }] => {
                // The variant index defines the true/false "value". Index zero is false.
                Ok((vec![], usize::from(*a_index == *b_index)))
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
        Uint128Concrete::Const(UintConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Uint128(*c)], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        Uint128Concrete::FromFelt(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Felt(value)] => Ok(match u128::try_from(value) {
                Ok(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], 0),
                Err(_) => (vec![CoreValue::RangeCheck], 1),
            }),
            [_, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        Uint128Concrete::ToFelt(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(value)] => {
                Ok((vec![CoreValue::Felt(value.to_bigint().unwrap())], 0))
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
        Uint128Concrete::DivMod(_) => match inputs {
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
        Uint128Concrete::WideMul(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)] => {
                let result = BigInt::from(*lhs) * BigInt::from(*rhs);
                let u128_limit = BigInt::from(u128::MAX) + BigInt::from(1);
                Ok((
                    vec![
                        CoreValue::RangeCheck,
                        CoreValue::Uint128(
                            (result.clone() / u128_limit.clone()).to_u128().unwrap(),
                        ),
                        CoreValue::Uint128((result % u128_limit).to_u128().unwrap()),
                    ],
                    0,
                ))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
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
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
        Uint128Concrete::LessThan(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(a), CoreValue::Uint128(b)] => {
                // "False" branch (branch 0) is the case a >= b.
                // "True" branch (branch 1) is the case a < b.
                Ok((vec![CoreValue::RangeCheck], usize::from(a < b)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
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
        Uint128Concrete::LessThanOrEqual(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint128(a), CoreValue::Uint128(b)] => {
                // "False" branch (branch 0) is the case a > b.
                // "True" branch (branch 1) is the case a <= b.
                Ok((vec![CoreValue::RangeCheck], usize::from(a <= b)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
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
        Uint8Concrete::Const(UintConstConcreteLibfunc { c, .. }) => {
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
        Uint8Concrete::LessThan(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint8(a), CoreValue::Uint8(b)] => {
                // "False" branch (branch 0) is the case a >= b.
                // "True" branch (branch 1) is the case a < b.
                Ok((vec![CoreValue::RangeCheck], usize::from(a < b)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
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
        Uint8Concrete::LessThanOrEqual(_) => match inputs {
            [CoreValue::RangeCheck, CoreValue::Uint8(a), CoreValue::Uint8(b)] => {
                // "False" branch (branch 0) is the case a > b.
                // "True" branch (branch 1) is the case a <= b.
                Ok((vec![CoreValue::RangeCheck], usize::from(a <= b)))
            }
            [_, _, _] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
    }
}

/// Simulate felt library functions.
fn simulate_felt_libfunc(
    libfunc: &FeltConcrete,
    inputs: &[CoreValue],
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    match libfunc {
        FeltConcrete::Const(FeltConstConcreteLibfunc { c, .. }) => {
            if inputs.is_empty() {
                Ok((vec![CoreValue::Felt(c.to_bigint().unwrap())], 0))
            } else {
                Err(LibfuncSimulationError::WrongNumberOfArgs)
            }
        }
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibfunc::Binary(
            FeltBinaryOpConcreteLibfunc { operator, .. },
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
                    Err(LibfuncSimulationError::MemoryLayoutMismatch)
                }
            }
            ([_, _], _) => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
        },
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibfunc::Const(
            FeltOperationWithConstConcreteLibfunc { operator, c, .. },
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
            [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
            _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
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
                [_] => Err(LibfuncSimulationError::MemoryLayoutMismatch),
                _ => Err(LibfuncSimulationError::WrongNumberOfArgs),
            }
        }
    }
}
