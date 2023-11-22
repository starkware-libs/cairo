use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::range_reduction::Range;
use num_bigint::ToBigInt;
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
///             out_range.upper < `prime` % 2**128-1.
/// when 2**128 >=out_range.upper>= `prime` % 2**128-1 we can use validate_under_limit with `K=1`
/// since for out_range.upper == `prime` % 2**128-1 we get
///     * `limit` =  (`prime`-out_range.upper % 2**128-1) +1
///     * `limit` % (2**128-1) = (2**128-1)-1
///     * `limit` / (2**128-1) ≈ 2**123+17*2**64
/// and for smaller value of out_range.upper limit will be larger and the module will be smaller
/// than neccasry.
///
/// Therefore for smaller values we use `K=2`
pub fn build_try_range_reduction(
    builder: CompiledInvocationBuilder<'_>,
    in_range: &Range,
    out_range: &Range,
) -> Result<CompiledInvocation, InvocationError> {
    assert!(in_range.lower.is_zero());
    assert!(out_range.lower.is_zero());

    assert!(out_range.upper < Felt252::prime().to_bigint().unwrap() % (u128::MAX));
    assert!(out_range.upper < in_range.upper);
    let upper_out: u128 = (&out_range.upper).try_into().unwrap();

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const num_limit = upper_out+1;
        tempvar in_range;
        hint TestLessThan {lhs: value, rhs: num_limit} into {dst: in_range};
        jump InRange if in_range != 0;
        // OutOfRange:
        tempvar shifted_value = value - num_limit;
    }

    // asserts that `value-upper_out-1<prime-upper_out-1`
    let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    validate_under_limit::<2>(
        &mut casm_builder,
        &(-Felt252::from(upper_out + 1)).to_biguint().to_bigint().unwrap(),
        shifted_value,
        range_check,
        &auxiliary_vars,
    );

    // asserts that :
    //  * value < 2**128
    //  * value + 2**128 -1 - upper_out < 2**128
    casm_build_extend! {casm_builder,
    InRange:
        assert value = *(range_check++);
        const u128_limit_minus_max = u128::MAX - upper_out;
        tempvar a = value + u128_limit_minus_max;
        assert a = *(range_check++);
        let ret_val = value;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[ret_val]], None),
            ("Done", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
