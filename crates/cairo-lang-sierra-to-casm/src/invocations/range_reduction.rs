use std::ops::Shl;

use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::utils::Range;
use num_bigint::BigInt;
use num_traits::Zero;

use super::{
    get_non_fallthrough_statement_id, CompiledInvocation, CompiledInvocationBuilder,
    InvocationError,
};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{add_input_variables, CostValidationInfo};

/// Builds a libfunc that tries to convert a numeric value in the range
/// in_range to out_range.
/// Assumption: in_range.lower==out_range.lower==0.
///             out_range.upper < in_range.upper.
///             out_range.upper <= `prime` % (2**128-1).
/// when 2**128 >=out_range.upper> `prime` % 2**128-1 we can use validate_under_limit with `K=1`
/// since for value == `prime` % 2**128-1 we get
///     * `limit` =  (`prime`-value % 2**128-1) +1
///     * `limit` % (2**128-1) = (2**128-1)-1
///     * `limit` / (2**128-1) ≈ 2**123+17*2**64
/// and for smaller value of out_range.upper limit will be larger and the module will be smaller
/// than neccasry.
///
/// Therefore for smaller values we use `K=2`
pub fn build_felt252_range_reduction(
    builder: CompiledInvocationBuilder<'_>,
    out_range: &Range,
) -> Result<CompiledInvocation, InvocationError> {
    // This also works for other values, that are bound by range check size, just suboptimal.
    // `i128` is using this non-optimal implementation.
    assert!(out_range.is_rc());

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const range_size = out_range.size();
        const range_lower = -out_range.lower.clone();
        const range_upper = out_range.upper.clone();
        let canonical_value = value + range_lower;
        tempvar in_range;
        hint TestLessThan {lhs: canonical_value, rhs: range_size} into {dst: in_range};
        jump InRange if in_range != 0;
        // OutOfRange:
    }
    let validated_value = if out_range.upper.is_zero() {
        value
    } else {
        casm_build_extend! {casm_builder,
            tempvar shifted_value = value - range_upper;
        }
        shifted_value
    };

    // asserts that `value - out_range.upper < prime - out_range.size()`
    let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    validate_under_limit::<2>(
        &mut casm_builder,
        &(-Felt252::from(out_range.size())).to_bigint(),
        validated_value,
        range_check,
        &auxiliary_vars,
    );
    casm_build_extend!(casm_builder, InRange:);
    if out_range.lower.is_zero() {
        casm_build_extend! {casm_builder,
            assert value = *(range_check++);
        };
    } else {
        casm_build_extend! {casm_builder,
            tempvar rc_val = canonical_value;
            assert rc_val = *(range_check++);
        }
    }
    let rc_size = BigInt::from(1).shl(128);
    // If the out range is exactly `rc_size` the previous addition to the buffer validated this
    // case as well.
    if out_range.size() < rc_size {
        let upper_bound_fixer = rc_size - &out_range.upper;
        if upper_bound_fixer.is_zero() {
            casm_build_extend! {casm_builder,
                assert value = *(range_check++);
            };
        } else {
            casm_build_extend! {casm_builder,
                const upper_bound_fixer = upper_bound_fixer;
                tempvar rc_val = value + upper_bound_fixer;
                assert rc_val = *(range_check++);
            }
        }
    }
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
