use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::casts::{CastConcreteLibfunc, DowncastConcreteLibfunc};
use num_bigint::BigInt;
use num_traits::Pow;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

/// Builds instructions for Sierra cast operations.
pub fn build(
    libfunc: &CastConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CastConcreteLibfunc::Downcast(libfunc) => build_downcast(builder, libfunc),
        CastConcreteLibfunc::Upcast(_) => build_identity(builder),
    }
}

/// Builds Casm instructions for [CastConcreteLibfunc::Downcast].
pub fn build_downcast(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &DowncastConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder,
        buffer(1) range_check;
        deref value;
    );

    // The casm code below assumes both types are at most 128 bits.
    assert!(
        libfunc.from_info.nbits <= 128 && libfunc.to_info.nbits <= 128,
        "Downcasting from types of size > 128 bit is not supported."
    );

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
    };

    let can_underflow = libfunc.from_info.signed;
    let can_overflow = libfunc.from_info.nbits > libfunc.to_info.nbits
        || (libfunc.from_info.nbits == libfunc.to_info.nbits && libfunc.to_info.signed);

    let two = BigInt::from(2u64);
    let range_size = two.clone().pow(libfunc.to_info.nbits);
    let upper_limit = if libfunc.to_info.signed {
        two.clone().pow(libfunc.to_info.nbits - 1)
    } else {
        range_size.clone()
    };
    let lower_limit: BigInt = upper_limit.clone() - range_size.clone();
    let rc_bound = two.pow(128_usize);
    let range_check = if !can_underflow {
        casm_build_extend! {casm_builder,
            // Use a hint to guess whether the result is in range (is_valid=1) or overflows
            // (is_valid=0).
            tempvar is_valid;
            const upper_limit_imm = upper_limit.clone();
            hint TestLessThan {lhs: value, rhs: upper_limit_imm} into {dst: is_valid};
            jump Success if is_valid != 0;
            // Failure.
            // value >= upper_limit  <=>  value - upper_limit >= 0.
            // Note that we know that 0 <= value < 2^128.
            tempvar shifted_value = value - upper_limit_imm;
            assert shifted_value = *(range_check++);
            jump Failure;

            // Success.
            Success:
            // Verify that the value is in range:
            // value < upper_limit  <=>  value + (rc_bound - upper_limit) < rc_bound.
            const pos_shift = rc_bound - upper_limit;
            tempvar shifted_value = value + pos_shift;
            assert shifted_value = *(range_check++);
        };
        range_check
    } else {
        casm_build_extend! {casm_builder,
            const minus_lower_limit = -lower_limit.clone();
            let canonical_value = value + minus_lower_limit;
            // Use a hint to guess whether the result is in range (is_valid=1) or overflows
            // (is_valid=0).
            tempvar is_valid;
            const range_size = range_size;
            hint TestLessThan {lhs: canonical_value, rhs: range_size} into {dst: is_valid};
            jump Success if is_valid != 0;
            // Failure.
        };
        if can_overflow {
            casm_build_extend! {casm_builder,
                const rc_bound_imm = rc_bound.clone();
                tempvar is_overflow;
                hint TestLessThan {lhs: value, rhs: rc_bound_imm} into {dst: is_overflow};
                jump Overflow if is_overflow != 0;
            }
        }
        casm_build_extend! {casm_builder,
            // Underflow.
            // value < 0  <=>  value + 2^128 < 2^128.
            // Note that we know that -2^127 < value.
            const pos_shift = rc_bound.clone() - lower_limit;
            tempvar shifted_value = value + pos_shift;
            assert shifted_value = *(range_check++);
            jump Failure;
        }
        if can_overflow {
            casm_build_extend! {casm_builder,
                Overflow:
                // value >= upper_limit  <=>  value - upper_limit >= 0.
                // Note that we know that -2^127 <= value < 2^128.
                const upper_limit = upper_limit.clone();
                tempvar shifted_value = value - upper_limit;
                assert shifted_value = *(range_check++);
                jump Failure;
            }
        }
        casm_build_extend! {casm_builder, Success:};
        // Verify that the value is in range:
        // value >= lower_limit  <=>  value + (-lower_limit) >= 0.
        if !libfunc.to_info.signed {
            casm_build_extend! {casm_builder, assert value = *(range_check++);};
        } else {
            casm_build_extend! {casm_builder,
                tempvar shifted_value = value + minus_lower_limit;
                assert shifted_value = *(range_check++);
            };
        }
        if can_overflow {
            casm_build_extend! {casm_builder,
                // Verify that the value is in range:
                // value < limit  <=>  value + (rc_bound - upper_limit) < rc_bound.
                const pos_shift = rc_bound - upper_limit;
                tempvar shifted_value = value + pos_shift;
                assert shifted_value = *(range_check++);
            };
        }
        range_check
    };

    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Failure", &[&[range_check]], Some(target_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
