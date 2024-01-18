use std::ops::Shl;

use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::utils::Range;
use num_bigint::BigInt;

use super::{
    get_non_fallthrough_statement_id, CompiledInvocation, CompiledInvocationBuilder,
    InvocationError,
};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{add_input_variables, CostValidationInfo};

/// Builds a libfunc that tries to convert a numeric value in the felt252 range to `out_range`.
/// Assumption: out_range.size() <= 2**128.
///
/// Note: Usage is preferable where: `out_range.size()` <= `prime` % u128::MAX.
/// When 2**128 >= `out_range.size()` > `prime` % u128::MAX we can use `validate_under_limit` with
/// `K=1` since for `size` == `prime` % u128::MAX we get
///     * `limit` =  `prime` - `size`
///     * A = `limit` % u128::MAX = 0
///     * B = `limit` / u128::MAX ≈ 2**123 + 17*2**64
/// So the `A <= B` condition is satisfied.
/// For smaller `out_range.size()` `A` will be almost u128::MAX and `B` will be smaller than
/// necessary.
///
/// Therefore for smaller values we should use `K=2`. It would always work since for `size` ==
/// `prime` % u128::MAX - 1 we get:
///     * `limit` =  `prime` - `size`
///     * A = `limit` % (u128::MAX - 1) = 2**123 + 17*2**64
///     * B = `limit` / (u128::MAX - 1) = 2**123 + 17*2**64 + 1
/// So the `A <= B` condition is satisfied.
/// For ranges smaller by `diff` if `diff` < `prime` % u128::MAX - 1 we get:
///     * `limit` =  `prime` - `size` + `diff`
///     * A = `limit` % (u128::MAX - 1) = 2**123 + 17*2**64 - `diff`
///     * B = `limit` / (u128::MAX - 1) = 2**123 + 17*2**64 + 1 - `diff` / (u128::MAX - 1)
///
/// `diff` is always smaller than `prime` % u128::MAX - 1 so `A <= B` is satisfied.
pub fn build_felt252_range_reduction(
    builder: CompiledInvocationBuilder<'_>,
    out_range: &Range,
    optimal_range: bool,
) -> Result<CompiledInvocation, InvocationError> {
    let prime: BigInt = Felt252::prime().into();
    if optimal_range {
        assert!(out_range.size() < (&prime % u128::MAX));
    }

    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const range_size = out_range.size();
        const minus_range_lower = -out_range.lower.clone();
        const range_upper = out_range.upper.clone();
        let canonical_value = value + minus_range_lower;
        tempvar in_range;
        hint TestLessThan {lhs: canonical_value, rhs: range_size} into {dst: in_range};
        jump InRange if in_range != 0;
        // OutOfRange:
        // Since `range_upper` may be 0.
        maybe_tempvar validated_value = value - range_upper;
    }

    // asserts that `value - out_range.upper < prime - out_range.size()`
    let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    validate_under_limit::<2>(
        &mut casm_builder,
        &(prime - out_range.size()),
        validated_value,
        range_check,
        &auxiliary_vars,
    );
    casm_build_extend! {casm_builder,
    InRange:
        // Since `minus_range_lower` may be 0.
        maybe_tempvar rc_val = value + minus_range_lower;
        assert rc_val = *(range_check++);
    }
    let rc_size = BigInt::from(1).shl(128);
    // If the out range is exactly `rc_size` the previous addition to the buffer validated this
    // case as well.
    if out_range.size() < rc_size {
        let upper_bound_fixer = rc_size - &out_range.upper;
        casm_build_extend! {casm_builder,
            const upper_bound_fixer = upper_bound_fixer;
            // Since `upper_bound_fixer` may be 0.
            maybe_tempvar rc_val = value + upper_bound_fixer;
            assert rc_val = *(range_check++);
        }
    }
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
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
