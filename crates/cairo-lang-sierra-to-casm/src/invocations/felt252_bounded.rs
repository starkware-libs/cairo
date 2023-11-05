use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::felt252_bounded::Felt252BoundedConcreteLibfunc;
use num_bigint::{BigInt, ToBigInt};

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
        Felt252BoundedConcreteLibfunc::FromFelt(libfunc) => {
            build_felt252_bounded_from_felt252(builder, &libfunc.min, &libfunc.max)
        }
    }
}

fn build_felt252_bounded_from_felt252(
    builder: CompiledInvocationBuilder<'_>,
    min: &BigInt,
    max: &BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    assert!(min == &0.into());
    assert!(max <= &BigInt::from(u128::MAX));
    let max: u128 = max.try_into().unwrap();

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const num_limit = max;
    }
    let ret_val = if max == 1 {
        casm_build_extend! {casm_builder,
            jump Done if value != 0;
            let ret_val = value;
        };
        ret_val
    } else if max == 2 {
        casm_build_extend! {casm_builder,
            tempvar value_squared = value*value;
            tempvar value_sqaured_minus_value= value_squared-value;
            jump Done if value_sqaured_minus_value != 0;
            let ret_val = value;
        };
        ret_val
    } else if max == 3 {
        casm_build_extend! {casm_builder,
            const one=1;
            const two=2;
            tempvar value_minus_one = value-one;
            tempvar value_minus_two = value-two;
            jump Done if value != 0;
            jump Done if value_minus_one != 0;
            jump Done if value_minus_two != 0;

            let ret_val = value;
        };
        ret_val
    } else {
        casm_build_extend! {casm_builder,
                tempvar in_range;
                hint TestLessThan {lhs: value, rhs: num_limit} into {dst: in_range};
                jump InRange if in_range != 0;
                // OutOfRange:
                const num_limit_plus_one =max+1;
                tempvar shifted_value = value - num_limit_plus_one;
        }

        // value -num_limit-1<prime-num_limit

        let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
        validate_under_limit::<2>(
            &mut casm_builder,
            &(-Felt252::from(max)).to_biguint().to_bigint().unwrap(),
            shifted_value,
            range_check,
            &auxiliary_vars,
        );

        casm_build_extend! {casm_builder,
        InRange:
            assert value = *(range_check++);
            const u128_limit_minus_num_variants = u128::MAX - max;
            tempvar a = value + u128_limit_minus_num_variants;
            assert a = *(range_check++);
            let ret_val = value;
        };
        ret_val
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
