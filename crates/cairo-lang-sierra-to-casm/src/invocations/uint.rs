use cairo_felt::Felt;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::uint::{
    IntOperator, Uint64Concrete, Uint8Concrete, UintConstConcreteLibfunc, UintTraits,
};
use num_bigint::{BigInt, ToBigInt};

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};
use crate::references::ReferenceExpression;

/// Builds invocations for uint const values.
pub fn build_const<TUintTraits: UintTraits>(
    libfunc: &UintConstConcreteLibfunc<TUintTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.into()))].into_iter(),
    ))
}

/// Builds instructions for uint less than.
/// Only assumes the original uints are bound by 128 bits.
pub fn build_less_than(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
            let orig_range_check = range_check;
            tempvar a_ge_b;
            tempvar a_minus_b = a - b;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            hint TestLessThan {lhs: a_minus_b, rhs: u128_limit} into {dst: a_ge_b};
            jump False if a_ge_b != 0;
            tempvar wrapping_a_minus_b = a_minus_b + u128_limit;
            assert wrapping_a_minus_b = *(range_check++);
            jump True;
        False:
            assert a_minus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check]], None),
            ("True", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Builds instructions for uint less than or equals.
/// Only assumes the original uints are bound by 128 bits.
pub fn build_less_than_or_equal(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
            let orig_range_check = range_check;
            tempvar a_gt_b;
            tempvar b_minus_a = b - a;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            hint TestLessThanOrEqual {lhs: u128_limit, rhs: b_minus_a} into {dst: a_gt_b};
            jump False if a_gt_b != 0;
            assert b_minus_a = *(range_check++);
            jump True;
        False:
            tempvar wrapping_a_minus_b = b_minus_a + u128_limit;
            assert wrapping_a_minus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check]], None),
            ("True", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a small uint overflowing add operation.
/// All parameters values are smaller than `limit`.
fn build_small_uint_overflowing_add(
    builder: CompiledInvocationBuilder<'_>,
    limit: u128,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
            let orig_range_check = range_check;
            tempvar no_overflow;
            tempvar fixed_a_plus_b;
            tempvar a_plus_b = a + b;
            const limit_fixer = (u128::MAX - limit + 1);
            const limit = limit;
            hint TestLessThan {lhs: a_plus_b, rhs: limit} into {dst: no_overflow};
            jump NoOverflow if no_overflow != 0;
            // Overflow:
            // Here we know that `limit <= a + b < 2 * limit - 1`.
            assert fixed_a_plus_b = a_plus_b - limit;
            assert fixed_a_plus_b = *(range_check++);
            jump Target;
        NoOverflow:
            // Here we know that `0 <= a + b < limit`
            // ==> `a + b + 2**128 - limit < limit + 2**128 - limit`
            // ==> `a + b + 2**128 - limit < 2**128`.
            assert fixed_a_plus_b = a_plus_b + limit_fixer;
            assert fixed_a_plus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[a_plus_b]], None),
            ("Target", &[&[range_check], &[fixed_a_plus_b]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a small uint overflowing sub operation.
/// All parameters values are smaller than `limit`.
fn build_small_uint_overflowing_sub(
    builder: CompiledInvocationBuilder<'_>,
    limit: BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
            let orig_range_check = range_check;
            tempvar no_overflow;
            tempvar a_minus_b = a - b;
            const u128_limit = (BigInt::from(u128::MAX) + 1) as BigInt;
            const limit = limit;
            hint TestLessThan {lhs: a_minus_b, rhs: limit} into {dst: no_overflow};
            jump NoOverflow if no_overflow != 0;
            // Underflow:
            // Here we know that 0 - (limit - 1) <= a - b < 0.
            tempvar fixed_a_minus_b = a_minus_b + u128_limit;
            assert fixed_a_minus_b = *(range_check++);
            tempvar wrapping_a_minus_b = a_minus_b + limit;
            jump Target;
        NoOverflow:
            assert a_minus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[a_minus_b]], None),
            ("Target", &[&[range_check], &[wrapping_a_minus_b]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Handles a small uint conversion from felt.
fn build_small_uint_from_felt<const LIMIT: u128, const K: u8>(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const limit = LIMIT;
        tempvar is_small;
        hint TestLessThan {lhs: value, rhs: limit} into {dst: is_small};
        jump IsSmall if is_small != 0;
        tempvar shifted_value = value - limit;
    }
    match K {
        1 => {
            let auxiliary_vars: [_; 4] = std::array::from_fn(|_| casm_builder.alloc_var(false));
            validate_under_limit::<K>(
                &mut casm_builder,
                &(-Felt::from(LIMIT)).to_biguint().to_bigint().unwrap(),
                shifted_value,
                range_check,
                &auxiliary_vars,
            );
            casm_build_extend! {casm_builder, jump Done;};
        }
        2 => {
            let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
            validate_under_limit::<K>(
                &mut casm_builder,
                &(-Felt::from(LIMIT)).to_biguint().to_bigint().unwrap(),
                shifted_value,
                range_check,
                &auxiliary_vars,
            );
        }
        _ => unreachable!("Only K value of 1 or 2 are supported."),
    }
    casm_build_extend! {casm_builder,
        IsSmall:
        assert value = *(range_check++);
        // value + 2**128 - limit < 2**128 ==> value < limit
        const fixer_limit = (u128::MAX - LIMIT + 1);
        tempvar value_upper_limit = value + fixer_limit;
        assert value_upper_limit = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Done", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Builds instructions for Sierra u8 operations.
pub fn build_u8(
    libfunc: &Uint8Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint8Concrete::Const(libfunc) => build_const(libfunc, builder),
        Uint8Concrete::LessThan(_) => build_less_than(builder),
        Uint8Concrete::Equal(_) => misc::build_cell_eq(builder),
        Uint8Concrete::LessThanOrEqual(_) => build_less_than_or_equal(builder),
        Uint8Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => {
                build_small_uint_overflowing_add(builder, u8::MAX as u128 + 1)
            }
            IntOperator::OverflowingSub => {
                build_small_uint_overflowing_sub(builder, BigInt::from(u8::MAX) + 1)
            }
        },
        Uint8Concrete::ToFelt(_) => misc::build_identity(builder),
        Uint8Concrete::FromFelt(_) => build_small_uint_from_felt::<256, 2>(builder),
    }
}

/// Builds instructions for Sierra u64 operations.
pub fn build_u64(
    libfunc: &Uint64Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Uint64Concrete::Const(libfunc) => build_const(libfunc, builder),
        Uint64Concrete::LessThan(_) => build_less_than(builder),
        Uint64Concrete::Equal(_) => misc::build_cell_eq(builder),
        Uint64Concrete::LessThanOrEqual(_) => build_less_than_or_equal(builder),
        Uint64Concrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => {
                build_small_uint_overflowing_add(builder, u64::MAX as u128 + 1)
            }
            IntOperator::OverflowingSub => {
                build_small_uint_overflowing_sub(builder, BigInt::from(u64::MAX) + 1)
            }
        },
        Uint64Concrete::ToFelt(_) => misc::build_identity(builder),
        Uint64Concrete::FromFelt(_) => {
            build_small_uint_from_felt::<0x10000000000000000, 2>(builder)
        }
    }
}
