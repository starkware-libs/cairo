use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::felt252_bounded::Felt252BoundedConcreteLibfunc;
use num_bigint::{BigInt, ToBigInt};
use num_traits::Zero;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

/// Builds instructions for Sierra felt252_bounded operations.
pub fn build(
    libfunc: &Felt252BoundedConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Felt252BoundedConcreteLibfunc::FromFelt(_) => build_identity(builder),
        Felt252BoundedConcreteLibfunc::ConstrainRange(libfunc) => {
            build_felt252_bounded_constrain_range(
                builder,
                &libfunc.lower_bound_in,
                &libfunc.upper_bound_in,
                &libfunc.lower_bound_out,
                &libfunc.upper_bound_out,
            )
        }
    }
}

fn build_felt252_bounded_constrain_range(
    builder: CompiledInvocationBuilder<'_>,
    lower_bound_in: &BigInt,
    upper_bound_in: &BigInt,
    lower_bound_out: &BigInt,
    upper_bound_out: &BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    assert!(lower_bound_in.is_zero());
    assert!(lower_bound_out.is_zero());
    assert!(upper_bound_out <= &BigInt::from(u128::MAX));
    assert!(upper_bound_out <= upper_bound_in);
    let upper_bound_out: u128 = upper_bound_out.try_into().unwrap();

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const num_limit = upper_bound_out;
        tempvar in_range;
        hint TestLessThan {lhs: value, rhs: num_limit} into {dst: in_range};
        jump InRange if in_range != 0;
        // OutOfRange:
        tempvar shifted_value = value - num_limit;
    }
    let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    validate_under_limit::<2>(
        &mut casm_builder,
        &(-Felt252::from(upper_bound_out)).to_biguint().to_bigint().unwrap(),
        shifted_value,
        range_check,
        &auxiliary_vars,
    );

    casm_build_extend! {casm_builder,
    InRange:
        assert value = *(range_check++);
        const u128_limit_minus_max = u128::MAX - upper_bound_out;
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
