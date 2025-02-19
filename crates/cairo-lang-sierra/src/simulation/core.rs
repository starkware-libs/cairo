use std::collections::HashMap;

use cairo_lang_utils::extract_matches;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, ToPrimitive, Zero};
use starknet_types_core::felt::{Felt as Felt252, NonZeroFelt as NonZeroFelt252};

use super::LibfuncSimulationError;
use super::value::CoreValue;
use crate::extensions::array::ArrayConcreteLibfunc;
use crate::extensions::boolean::BoolConcreteLibfunc;
use crate::extensions::core::CoreConcreteLibfunc;
use crate::extensions::ec::EcConcreteLibfunc;
use crate::extensions::enm::{EnumConcreteLibfunc, EnumInitConcreteLibfunc};
use crate::extensions::felt252::{
    Felt252BinaryOpConcreteLibfunc, Felt252BinaryOperationConcrete, Felt252BinaryOperator,
    Felt252Concrete, Felt252ConstConcreteLibfunc, Felt252OperationWithConstConcreteLibfunc,
};
use crate::extensions::felt252_dict::Felt252DictConcreteLibfunc;
use crate::extensions::function_call::SignatureAndFunctionConcreteLibfunc;
use crate::extensions::gas::GasConcreteLibfunc;
use crate::extensions::int::unsigned::{
    Uint8Concrete, Uint16Concrete, Uint32Concrete, Uint64Concrete,
};
use crate::extensions::int::unsigned128::Uint128Concrete;
use crate::extensions::int::{IntConstConcreteLibfunc, IntOperator};
use crate::extensions::mem::MemConcreteLibfunc;
use crate::extensions::structure::StructConcreteLibfunc;
use crate::ids::FunctionId;

/// Helper macro to take the inputs and return an error if the number of inputs is wrong, or the
/// type of the expected inputs is wrong. Usage:
///
/// ```ignore
/// take_inputs!(let [CoreValue::Felt252(x), CoreValue::Uint128(y), z] = inputs);
/// ```
macro_rules! take_inputs {
    (let $assigned_value:pat = $inputs:ident) => {
        let $assigned_value = take_inputs($inputs)? else {
            return Err(LibfuncSimulationError::WrongArgType);
        };
    };
}

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
    Ok(match libfunc {
        CoreConcreteLibfunc::Drop(_) => {
            let [_] = take_inputs(inputs)?;
            (vec![], 0)
        }
        CoreConcreteLibfunc::Dup(_) => {
            let [value] = take_inputs(inputs)?;
            (vec![value.clone(), value], 0)
        }
        CoreConcreteLibfunc::Ec(libfunc) => {
            match libfunc {
                EcConcreteLibfunc::TryNew(_) => {
                    take_inputs!(let [CoreValue::Felt252(x), CoreValue::Felt252(y)] = inputs);
                    const BETA: Felt252 = Felt252::from_hex_unchecked(
                        "0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89",
                    );
                    // If the point is on the curve use the fallthrough branch and return the
                    // point.
                    if y * y == x * x * x + x + BETA {
                        (vec![CoreValue::EcPoint(x, y)], 0)
                    } else {
                        (vec![], 1)
                    }
                }
                EcConcreteLibfunc::UnwrapPoint(_) => {
                    take_inputs!(let [CoreValue::EcPoint(x, y)] = inputs);
                    (vec![CoreValue::Felt252(x), CoreValue::Felt252(y)], 0)
                }
                _ => unimplemented!(),
            }
        }
        CoreConcreteLibfunc::FunctionCall(SignatureAndFunctionConcreteLibfunc {
            function, ..
        })
        | CoreConcreteLibfunc::CouponCall(SignatureAndFunctionConcreteLibfunc {
            function, ..
        }) => (simulate_function(&function.id, inputs)?, 0),
        CoreConcreteLibfunc::Gas(GasConcreteLibfunc::WithdrawGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibfuncSimulationError::UnresolvedStatementGasInfo)?;
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::GasBuiltin(gas_counter)] = inputs);
            if gas_counter >= count {
                // Have enough gas - return reduced counter and jump to success branch.
                (vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(gas_counter - count)], 0)
            } else {
                // Don't have enough gas - return the same counter and jump to failure branch.
                (vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(gas_counter)], 1)
            }
        }
        CoreConcreteLibfunc::Gas(GasConcreteLibfunc::RedepositGas(_)) => {
            let count = get_statement_gas_info()
                .ok_or(LibfuncSimulationError::UnresolvedStatementGasInfo)?;
            take_inputs!(let [CoreValue::GasBuiltin(gas_counter)] = inputs);
            (vec![CoreValue::GasBuiltin(gas_counter + count)], 0)
        }
        CoreConcreteLibfunc::Gas(GasConcreteLibfunc::GetAvailableGas(_)) => {
            take_inputs!(let [CoreValue::GasBuiltin(gas_counter)] = inputs);
            (vec![CoreValue::GasBuiltin(gas_counter), CoreValue::Uint128(gas_counter as u128)], 0)
        }
        CoreConcreteLibfunc::Gas(
            GasConcreteLibfunc::BuiltinWithdrawGas(_)
            | GasConcreteLibfunc::GetBuiltinCosts(_)
            | GasConcreteLibfunc::GetUnspentGas(_),
        ) => {
            unimplemented!("Simulation of the builtin cost functionality is not implemented yet.")
        }
        CoreConcreteLibfunc::BranchAlign(_) => {
            let [] = take_inputs(inputs)?;
            get_statement_gas_info().ok_or(LibfuncSimulationError::UnresolvedStatementGasInfo)?;
            (vec![], 0)
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::New(_)) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Array(vec![])], 0)
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::SpanFromTuple(_)) => {
            take_inputs!(let [CoreValue::Struct(members)] = inputs);
            (vec![CoreValue::Array(members)], 0)
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::TupleFromSpan(_)) => todo!(),
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::Append(_)) => {
            take_inputs!(let [CoreValue::Array(mut arr), element] = inputs);
            arr.push(element);
            (vec![CoreValue::Array(arr)], 0)
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::PopFront(_)) => {
            take_inputs!(let [CoreValue::Array(mut arr)] = inputs);
            if arr.is_empty() {
                (vec![CoreValue::Array(arr)], 1)
            } else {
                let front = arr.remove(0);
                (vec![CoreValue::Array(arr), front], 0)
            }
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::PopFrontConsume(_)) => {
            take_inputs!(let [CoreValue::Array(mut arr)] = inputs);
            if arr.is_empty() { (vec![CoreValue::Array(arr)], 1) } else { (vec![arr.remove(0)], 0) }
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::Get(_)) => {
            take_inputs!(
                let [CoreValue::RangeCheck, CoreValue::Array(arr), CoreValue::Uint32(idx)] = inputs
            );
            match arr.get(idx as usize).cloned() {
                Some(element) => (vec![CoreValue::RangeCheck, element], 0),
                None => (vec![CoreValue::RangeCheck], 1),
            }
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::Slice(_)) => {
            take_inputs!(let [
                CoreValue::RangeCheck,
                CoreValue::Array(arr),
                CoreValue::Uint32(start),
                CoreValue::Uint32(length),
            ] = inputs);
            match arr.get(start as usize..(start + length) as usize) {
                Some(elements) => {
                    (vec![CoreValue::RangeCheck, CoreValue::Array(elements.to_vec())], 0)
                }
                None => (vec![CoreValue::RangeCheck], 1),
            }
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::Len(_)) => {
            take_inputs!(let [CoreValue::Array(arr)] = inputs);
            (vec![CoreValue::Uint32(arr.len() as u32)], 0)
        }
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::SnapshotPopFront(_)) => todo!(),
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::SnapshotPopBack(_)) => todo!(),
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::SnapshotMultiPopFront(_)) => todo!(),
        CoreConcreteLibfunc::Array(ArrayConcreteLibfunc::SnapshotMultiPopBack(_)) => todo!(),
        CoreConcreteLibfunc::Uint8(libfunc) => simulate_u8_libfunc(libfunc, inputs)?,
        CoreConcreteLibfunc::Uint16(libfunc) => simulate_u16_libfunc(libfunc, inputs)?,
        CoreConcreteLibfunc::Uint32(libfunc) => simulate_u32_libfunc(libfunc, inputs)?,
        CoreConcreteLibfunc::Uint64(libfunc) => simulate_u64_libfunc(libfunc, inputs)?,
        CoreConcreteLibfunc::Uint128(libfunc) => simulate_u128_libfunc(libfunc, inputs)?,
        CoreConcreteLibfunc::Sint8(_)
        | CoreConcreteLibfunc::Sint16(_)
        | CoreConcreteLibfunc::Sint32(_)
        | CoreConcreteLibfunc::Sint64(_)
        | CoreConcreteLibfunc::Sint128(_) => {
            unimplemented!("Simulation of signed integer libfuncs is not implemented yet.")
        }
        CoreConcreteLibfunc::Bool(libfunc) => simulate_bool_libfunc(libfunc, inputs)?,
        CoreConcreteLibfunc::Felt252(libfunc) => simulate_felt252_libfunc(libfunc, inputs)?,
        CoreConcreteLibfunc::UnwrapNonZero(_) => (inputs, 0),
        CoreConcreteLibfunc::Mem(
            MemConcreteLibfunc::Rename(_) | MemConcreteLibfunc::StoreTemp(_),
        )
        | CoreConcreteLibfunc::Box(_) => {
            let [value] = take_inputs(inputs)?;
            (vec![value], 0)
        }
        CoreConcreteLibfunc::Mem(MemConcreteLibfunc::FinalizeLocals(_))
        | CoreConcreteLibfunc::UnconditionalJump(_)
        | CoreConcreteLibfunc::ApTracking(_) => {
            let [] = take_inputs(inputs)?;
            (vec![], 0)
        }
        CoreConcreteLibfunc::Mem(MemConcreteLibfunc::StoreLocal(_)) => {
            take_inputs!(let [CoreValue::Uninitialized, value] = inputs);
            (vec![value], 0)
        }
        CoreConcreteLibfunc::Mem(MemConcreteLibfunc::AllocLocal(_)) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Uninitialized], 0)
        }
        CoreConcreteLibfunc::Enum(EnumConcreteLibfunc::Init(EnumInitConcreteLibfunc {
            index,
            ..
        })) => {
            let [value] = take_inputs(inputs)?;
            // We don't verify here that the input type matches the signature.
            (vec![CoreValue::Enum { value: Box::new(value), index: *index }], 0)
        }
        CoreConcreteLibfunc::Enum(
            EnumConcreteLibfunc::Match(_) | EnumConcreteLibfunc::SnapshotMatch(_),
        ) => {
            take_inputs!(let [CoreValue::Enum { value, index }] = inputs);
            (vec![*value], index)
        }
        CoreConcreteLibfunc::Enum(EnumConcreteLibfunc::FromBoundedInt(_)) => todo!(),
        CoreConcreteLibfunc::Struct(StructConcreteLibfunc::Construct(_)) => {
            (vec![CoreValue::Struct(inputs)], 0)
        }
        CoreConcreteLibfunc::Struct(
            StructConcreteLibfunc::Deconstruct(_) | StructConcreteLibfunc::SnapshotDeconstruct(_),
        ) => {
            take_inputs!(let [CoreValue::Struct(members)] = inputs);
            (members, 0)
        }
        CoreConcreteLibfunc::Felt252Dict(Felt252DictConcreteLibfunc::New(_)) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Dict(HashMap::new())], 0)
        }
        CoreConcreteLibfunc::Felt252Dict(Felt252DictConcreteLibfunc::Squash(_)) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Dict(dict)] = inputs);
            // Returning the same dict since it is exactly the same as the squashed one.
            (vec![CoreValue::RangeCheck, CoreValue::Dict(dict)], 0)
        }
        CoreConcreteLibfunc::Felt252SquashedDict(_) => {
            unimplemented!("Simulation of Felt252SquashedDict is not implemented yet.");
        }
        CoreConcreteLibfunc::Pedersen(_) => {
            unimplemented!("Simulation of the Pedersen hash function is not implemented yet.");
        }
        CoreConcreteLibfunc::Poseidon(_) => {
            unimplemented!("Simulation of the Poseidon hash function is not implemented yet.");
        }
        CoreConcreteLibfunc::Starknet(_) => {
            unimplemented!("Simulation of the Starknet functionalities is not implemented yet.")
        }
        CoreConcreteLibfunc::Nullable(_) => {
            unimplemented!("Simulation of nullable is not implemented yet.")
        }
        CoreConcreteLibfunc::Debug(_) => {
            take_inputs!(let [CoreValue::Array(arr)] = inputs);
            let mut bytes = Vec::new();
            for limb in arr {
                let limb = extract_matches!(limb, CoreValue::Felt252);
                bytes.extend(limb.to_bytes_be());
            }
            if let Ok(s) = String::from_utf8(bytes) {
                print!("{s}");
            } else {
                println!("Not utf8");
            }
            (vec![], 0)
        }
        CoreConcreteLibfunc::SnapshotTake(_) => {
            let [value] = take_inputs(inputs)?;
            (vec![value.clone(), value], 0)
        }
        CoreConcreteLibfunc::Cast(_) => unimplemented!(),
        CoreConcreteLibfunc::Felt252DictEntry(_) => unimplemented!(),
        CoreConcreteLibfunc::Uint256(_) => unimplemented!(),
        CoreConcreteLibfunc::Uint512(_) => unimplemented!(),
        CoreConcreteLibfunc::Bytes31(_) => unimplemented!(),
        CoreConcreteLibfunc::Const(_) => unimplemented!(),
        CoreConcreteLibfunc::Coupon(_) => unimplemented!(),
        CoreConcreteLibfunc::BoundedInt(_) => unimplemented!(),
        CoreConcreteLibfunc::Circuit(_) => unimplemented!(),
        CoreConcreteLibfunc::IntRange(_) => unimplemented!(),
        CoreConcreteLibfunc::Blake(_) => unimplemented!(),
    })
}

/// Simulate boolean library functions.
fn simulate_bool_libfunc(
    libfunc: &BoolConcreteLibfunc,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    Ok(match libfunc {
        BoolConcreteLibfunc::And(_) => {
            take_inputs!(let [
                CoreValue::Enum { index: a_index, .. },
                CoreValue::Enum { index: b_index, .. },
            ] = inputs);
            (
                vec![CoreValue::Enum {
                    value: Box::new(CoreValue::Struct(vec![])),
                    index: usize::from(a_index == 1 && b_index == 1),
                }],
                0,
            )
        }
        BoolConcreteLibfunc::Not(_) => {
            take_inputs!(let [CoreValue::Enum { index, .. }] = inputs);
            (
                vec![CoreValue::Enum {
                    value: Box::new(CoreValue::Struct(vec![])),
                    index: 1_usize - index,
                }],
                0,
            )
        }
        BoolConcreteLibfunc::Xor(_) => {
            take_inputs!(let [
                CoreValue::Enum { index: a_index, .. },
                CoreValue::Enum { index: b_index, .. },
            ] = inputs);
            (
                vec![CoreValue::Enum {
                    value: Box::new(CoreValue::Struct(vec![])),
                    index: usize::from(a_index != b_index),
                }],
                0,
            )
        }
        BoolConcreteLibfunc::Or(_) => {
            take_inputs!(let [
                CoreValue::Enum { index: a_index, .. },
                CoreValue::Enum { index: b_index, .. },
            ] = inputs);
            (
                vec![CoreValue::Enum {
                    value: Box::new(CoreValue::Struct(vec![])),
                    index: usize::from(a_index + b_index > 0),
                }],
                0,
            )
        }
        BoolConcreteLibfunc::ToFelt252(_) => {
            take_inputs!(let [CoreValue::Enum { index, .. }] = inputs);
            (vec![CoreValue::Felt252(Felt252::from(index))], 0)
        }
    })
}

/// Simulate u128 library functions.
fn simulate_u128_libfunc(
    libfunc: &Uint128Concrete,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    Ok(match libfunc {
        Uint128Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Uint128(*c)], 0)
        }
        Uint128Concrete::FromFelt252(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Felt252(value)] = inputs);
            match value.to_u128() {
                Some(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], 0),
                None => (vec![CoreValue::RangeCheck], 1),
            }
        }
        Uint128Concrete::ToFelt252(_) => {
            take_inputs!(let [CoreValue::Uint128(value)] = inputs);
            (vec![CoreValue::Felt252(Felt252::from(value))], 0)
        }
        Uint128Concrete::Operation(libfunc) => {
            take_inputs!(let [
                CoreValue::RangeCheck, CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)
            ] = inputs);
            let (value, overflow) = match libfunc.operator {
                IntOperator::OverflowingAdd => lhs.overflowing_add(rhs),
                IntOperator::OverflowingSub => lhs.overflowing_sub(rhs),
            };
            (vec![CoreValue::RangeCheck, CoreValue::Uint128(value)], usize::from(overflow))
        }
        Uint128Concrete::Divmod(_) => {
            take_inputs!(let [
                CoreValue::RangeCheck, CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)
            ] = inputs);
            (
                vec![
                    CoreValue::RangeCheck,
                    CoreValue::Uint128(lhs / rhs),
                    CoreValue::Uint128(lhs % rhs),
                ],
                0,
            )
        }
        Uint128Concrete::GuaranteeMul(_) => {
            take_inputs!(let [CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)] = inputs);
            let (limb1, limb0) = (BigInt::from(lhs) * rhs).div_rem(&BigInt::one().pow(128));
            (
                vec![
                    CoreValue::Uint128(limb0.to_u128().unwrap()),
                    CoreValue::Uint128(limb1.to_u128().unwrap()),
                    CoreValue::U128MulGuarantee,
                ],
                0,
            )
        }
        Uint128Concrete::MulGuaranteeVerify(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::U128MulGuarantee] = inputs);
            (vec![CoreValue::RangeCheck], 0)
        }
        Uint128Concrete::IsZero(_) => {
            take_inputs!(let [CoreValue::Uint128(value)] = inputs);
            if value.is_zero() { (vec![], 0) } else { (vec![CoreValue::Uint128(value)], 1) }
        }
        Uint128Concrete::SquareRoot(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint128(value)] = inputs);
            let root = BigInt::from(value).sqrt();
            (vec![CoreValue::RangeCheck, CoreValue::Uint64(root.to_u64().unwrap())], 0)
        }
        Uint128Concrete::Equal(_) => {
            take_inputs!(let [CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)] = inputs);
            // "False" branch (branch 0) is the case a != b.
            // "True" branch (branch 1) is the case a == b.
            (vec![], usize::from(lhs == rhs))
        }
        Uint128Concrete::ByteReverse(_) => todo!("ByteReverse"),
        Uint128Concrete::Bitwise(_) => {
            take_inputs!(let [
                CoreValue::Bitwise, CoreValue::Uint128(lhs), CoreValue::Uint128(rhs)
            ] = inputs);
            (
                vec![
                    CoreValue::Bitwise,
                    CoreValue::Uint128(lhs & rhs),
                    CoreValue::Uint128(lhs | rhs),
                    CoreValue::Uint128(lhs ^ rhs),
                ],
                0,
            )
        }
    })
}

/// Simulate u8 library functions.
fn simulate_u8_libfunc(
    libfunc: &Uint8Concrete,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    Ok(match libfunc {
        Uint8Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Uint8(*c)], 0)
        }
        Uint8Concrete::Operation(libfunc) => {
            take_inputs!(
                let [CoreValue::RangeCheck, CoreValue::Uint8(lhs), CoreValue::Uint8(rhs)] = inputs
            );
            let (value, overflow) = match libfunc.operator {
                IntOperator::OverflowingAdd => lhs.overflowing_add(rhs),
                IntOperator::OverflowingSub => lhs.overflowing_sub(rhs),
            };
            (vec![CoreValue::RangeCheck, CoreValue::Uint8(value)], usize::from(overflow))
        }
        Uint8Concrete::SquareRoot(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint8(value)] = inputs);
            let root = BigInt::from(value).sqrt();
            (vec![CoreValue::RangeCheck, CoreValue::Uint8(root.to_u8().unwrap())], 0)
        }
        Uint8Concrete::Equal(_) => {
            take_inputs!(let [CoreValue::Uint8(lhs), CoreValue::Uint8(rhs)] = inputs);
            // "False" branch (branch 0) is the case a != b.
            // "True" branch (branch 1) is the case a == b.
            (vec![], usize::from(lhs == rhs))
        }
        Uint8Concrete::ToFelt252(_) => {
            take_inputs!(let [CoreValue::Uint8(value)] = inputs);
            (vec![CoreValue::Felt252(Felt252::from(value))], 0)
        }
        Uint8Concrete::FromFelt252(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint8(value)] = inputs);
            match value.to_u8() {
                Some(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint8(value)], 0),
                None => (vec![CoreValue::RangeCheck], 1),
            }
        }
        Uint8Concrete::IsZero(_) => unimplemented!(),
        Uint8Concrete::Divmod(_) => unimplemented!(),
        Uint8Concrete::Bitwise(_) => unimplemented!(),
        Uint8Concrete::WideMul(_) => {
            take_inputs!(let [CoreValue::Uint8(lhs), CoreValue::Uint8(rhs)] = inputs);
            (vec![CoreValue::Uint16(u16::from(lhs) * u16::from(rhs))], 0)
        }
    })
}

/// Simulate u16 library functions.
fn simulate_u16_libfunc(
    libfunc: &Uint16Concrete,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    Ok(match libfunc {
        Uint16Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Uint16(*c)], 0)
        }
        Uint16Concrete::Operation(libfunc) => {
            take_inputs!(let [
                CoreValue::RangeCheck, CoreValue::Uint16(lhs), CoreValue::Uint16(rhs)
            ] = inputs);
            let (value, overflow) = match libfunc.operator {
                IntOperator::OverflowingAdd => lhs.overflowing_add(rhs),
                IntOperator::OverflowingSub => lhs.overflowing_sub(rhs),
            };
            (vec![CoreValue::RangeCheck, CoreValue::Uint16(value)], usize::from(overflow))
        }
        Uint16Concrete::SquareRoot(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint16(value)] = inputs);
            let root = BigInt::from(value).sqrt();
            (vec![CoreValue::RangeCheck, CoreValue::Uint8(root.to_u8().unwrap())], 0)
        }
        Uint16Concrete::Equal(_) => {
            take_inputs!(let [CoreValue::Uint16(lhs), CoreValue::Uint16(rhs)] = inputs);
            // "False" branch (branch 0) is the case a != b.
            // "True" branch (branch 1) is the case a == b.
            (vec![], usize::from(lhs == rhs))
        }
        Uint16Concrete::ToFelt252(_) => {
            take_inputs!(let [CoreValue::Uint16(value)] = inputs);
            (vec![CoreValue::Felt252(Felt252::from(value))], 0)
        }
        Uint16Concrete::FromFelt252(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint16(value)] = inputs);
            match value.to_u16() {
                Some(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint16(value)], 0),
                None => (vec![CoreValue::RangeCheck], 1),
            }
        }
        Uint16Concrete::IsZero(_) => unimplemented!(),
        Uint16Concrete::Divmod(_) => unimplemented!(),
        Uint16Concrete::Bitwise(_) => unimplemented!(),
        Uint16Concrete::WideMul(_) => {
            take_inputs!(let [CoreValue::Uint16(lhs), CoreValue::Uint16(rhs)] = inputs);
            (vec![CoreValue::Uint32(u32::from(lhs) * u32::from(rhs))], 0)
        }
    })
}

/// Simulate u32 library functions.
fn simulate_u32_libfunc(
    libfunc: &Uint32Concrete,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    Ok(match libfunc {
        Uint32Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Uint32(*c)], 0)
        }
        Uint32Concrete::Operation(libfunc) => {
            take_inputs!(let [
                CoreValue::RangeCheck, CoreValue::Uint32(lhs), CoreValue::Uint32(rhs)
            ] = inputs);
            let (value, overflow) = match libfunc.operator {
                IntOperator::OverflowingAdd => lhs.overflowing_add(rhs),
                IntOperator::OverflowingSub => lhs.overflowing_sub(rhs),
            };
            (vec![CoreValue::RangeCheck, CoreValue::Uint32(value)], usize::from(overflow))
        }
        Uint32Concrete::SquareRoot(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint32(value)] = inputs);
            let root = BigInt::from(value).sqrt();
            (vec![CoreValue::RangeCheck, CoreValue::Uint16(root.to_u16().unwrap())], 0)
        }
        Uint32Concrete::Equal(_) => {
            take_inputs!(let [CoreValue::Uint32(lhs), CoreValue::Uint32(rhs)] = inputs);
            // "False" branch (branch 0) is the case a != b.
            // "True" branch (branch 1) is the case a == b.
            (vec![], usize::from(lhs == rhs))
        }
        Uint32Concrete::ToFelt252(_) => {
            take_inputs!(let [CoreValue::Uint32(value)] = inputs);
            (vec![CoreValue::Felt252(Felt252::from(value))], 0)
        }
        Uint32Concrete::FromFelt252(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Felt252(value)] = inputs);
            match value.to_u32() {
                Some(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint32(value)], 0),
                None => (vec![CoreValue::RangeCheck], 1),
            }
        }
        Uint32Concrete::IsZero(_) => unimplemented!(),
        Uint32Concrete::Divmod(_) => unimplemented!(),
        Uint32Concrete::Bitwise(_) => unimplemented!(),
        Uint32Concrete::WideMul(_) => {
            take_inputs!(let [CoreValue::Uint32(lhs), CoreValue::Uint32(rhs)] = inputs);
            (vec![CoreValue::Uint64(u64::from(lhs) * u64::from(rhs))], 0)
        }
    })
}

/// Simulate u64 library functions.
fn simulate_u64_libfunc(
    libfunc: &Uint64Concrete,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    Ok(match libfunc {
        Uint64Concrete::Const(IntConstConcreteLibfunc { c, .. }) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Uint64(*c)], 0)
        }
        Uint64Concrete::Operation(libfunc) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint64(lhs), CoreValue::Uint64(rhs)] = inputs);
            let (value, overflow) = match libfunc.operator {
                IntOperator::OverflowingAdd => lhs.overflowing_add(rhs),
                IntOperator::OverflowingSub => lhs.overflowing_sub(rhs),
            };
            (vec![CoreValue::RangeCheck, CoreValue::Uint64(value)], usize::from(overflow))
        }
        Uint64Concrete::SquareRoot(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint64(value)] = inputs);
            let root = BigInt::from(value).sqrt();
            (vec![CoreValue::RangeCheck, CoreValue::Uint32(root.to_u32().unwrap())], 0)
        }
        Uint64Concrete::Equal(_) => {
            take_inputs!(let [CoreValue::Uint64(lhs), CoreValue::Uint64(rhs)] = inputs);
            // "False" branch (branch 0) is the case a != b.
            // "True" branch (branch 1) is the case a == b.
            (vec![], usize::from(lhs == rhs))
        }
        Uint64Concrete::ToFelt252(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Uint64(value)] = inputs);
            (vec![CoreValue::Felt252(Felt252::from(value))], 0)
        }
        Uint64Concrete::FromFelt252(_) => {
            take_inputs!(let [CoreValue::RangeCheck, CoreValue::Felt252(value)] = inputs);
            match value.to_u64() {
                Some(value) => (vec![CoreValue::RangeCheck, CoreValue::Uint64(value)], 0),
                None => (vec![CoreValue::RangeCheck], 1),
            }
        }
        Uint64Concrete::IsZero(_) => unimplemented!(),
        Uint64Concrete::Divmod(_) => unimplemented!(),
        Uint64Concrete::Bitwise(_) => unimplemented!(),
        Uint64Concrete::WideMul(_) => {
            take_inputs!(let [CoreValue::Uint64(lhs), CoreValue::Uint64(rhs)] = inputs);
            (vec![CoreValue::Uint128(u128::from(lhs) * u128::from(rhs))], 0)
        }
    })
}

/// Simulate felt252 library functions.
fn simulate_felt252_libfunc(
    libfunc: &Felt252Concrete,
    inputs: Vec<CoreValue>,
) -> Result<(Vec<CoreValue>, usize), LibfuncSimulationError> {
    Ok(match libfunc {
        Felt252Concrete::Const(Felt252ConstConcreteLibfunc { c, .. }) => {
            let [] = take_inputs(inputs)?;
            (vec![CoreValue::Felt252(c.into())], 0)
        }
        Felt252Concrete::BinaryOperation(Felt252BinaryOperationConcrete::WithVar(
            Felt252BinaryOpConcreteLibfunc { operator, .. },
        )) => {
            take_inputs!(let [CoreValue::Felt252(lhs), CoreValue::Felt252(rhs)] = inputs);
            (
                vec![CoreValue::Felt252(match operator {
                    Felt252BinaryOperator::Add => lhs + rhs,
                    Felt252BinaryOperator::Sub => lhs - rhs,
                    Felt252BinaryOperator::Mul => lhs * rhs,
                    Felt252BinaryOperator::Div => {
                        lhs.field_div(&NonZeroFelt252::from_felt_unchecked(rhs))
                    }
                })],
                0,
            )
        }
        Felt252Concrete::BinaryOperation(Felt252BinaryOperationConcrete::WithConst(
            Felt252OperationWithConstConcreteLibfunc { operator, c, .. },
        )) => {
            take_inputs!(let [CoreValue::Felt252(value)] = inputs);
            (
                vec![CoreValue::Felt252(match operator {
                    Felt252BinaryOperator::Add => value + Felt252::from(c),
                    Felt252BinaryOperator::Sub => value - Felt252::from(c),
                    Felt252BinaryOperator::Mul => value * Felt252::from(c),
                    Felt252BinaryOperator::Div => {
                        value.field_div(&NonZeroFelt252::from_felt_unchecked(Felt252::from(c)))
                    }
                })],
                0,
            )
        }
        Felt252Concrete::IsZero(_) => {
            take_inputs!(let [CoreValue::Felt252(value)] = inputs);
            if value.is_zero() {
                // Zero - jumping to the failure branch.
                (vec![], 0)
            } else {
                // Non-zero - jumping to the success branch and providing a NonZero wrap to the
                // given value.
                (vec![CoreValue::Felt252(value)], 1)
            }
        }
    })
}

/// Takes the inputs and returns an array of the correct size, or an error if the number of inputs
/// is wrong.
fn take_inputs<const COUNT: usize>(
    inputs: Vec<CoreValue>,
) -> Result<[CoreValue; COUNT], LibfuncSimulationError> {
    TryFrom::try_from(inputs).map_err(|_| LibfuncSimulationError::WrongNumberOfArgs)
}
