use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::int::signed::{SintConcrete, SintTraits};
use cairo_lang_sierra::extensions::int::IntMulTraits;
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;
use num_bigint::{BigInt, ToBigInt};

use super::{add_input_variables, build_const, build_small_wide_mul};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{
    get_non_fallthrough_statement_id, misc, CompiledInvocation, CompiledInvocationBuilder,
    CostValidationInfo, InvocationError,
};

/// Handles a signed integer conversion from felt252.
pub fn build_sint_from_felt252(
    builder: CompiledInvocationBuilder<'_>,
    lower_limit: i128,
    upper_limit: i128,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref value;
    };
    let range_size: BigInt = BigInt::from(upper_limit) - BigInt::from(lower_limit) + 1;
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const positive_range_fixer = -BigInt::from(lower_limit);
        let canonical_value = value + positive_range_fixer;
        tempvar is_in_range;
        const range_size_imm = range_size.clone();
        hint TestLessThan {lhs: canonical_value, rhs: range_size_imm} into {dst: is_in_range};
        jump IsInRange if is_in_range != 0;
        const upper_limit_plus_one = (BigInt::from(upper_limit) + 1) as BigInt;
        tempvar shifted_value = value - upper_limit_plus_one;
    }
    let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    validate_under_limit::<2>(
        &mut casm_builder,
        &(-Felt252::from(range_size)).to_biguint().to_bigint().unwrap(),
        shifted_value,
        range_check,
        &auxiliary_vars,
    );
    casm_build_extend! {casm_builder,
        jump Done;
    IsInRange:
        tempvar rc_val = canonical_value;
        assert rc_val = *(range_check++);
    }
    // For i128, the previous range check already made sure the value is in range.
    if lower_limit != i128::MIN || upper_limit != i128::MAX {
        casm_build_extend! {casm_builder,
            // value + 2**128 - upper_limit - 1 < 2**128 ==> value <= upper_limit
            const fixer_limit = (BigInt::from(2).pow(128) - upper_limit - 1) as BigInt;
            tempvar rc_val = value + fixer_limit;
            assert rc_val = *(range_check++);
        };
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

/// Builds instructions for Sierra i8/i16/i32/i64 operations.
pub fn build_sint<
    TSintTraits: SintTraits + IntMulTraits + IsZeroTraits,
    const LOWER_LIMIT: i128,
    const UPPER_LIMIT: i128,
>(
    libfunc: &SintConcrete<TSintTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        SintConcrete::Const(libfunc) => build_const(libfunc, builder),
        SintConcrete::Equal(_) => misc::build_cell_eq(builder),
        SintConcrete::ToFelt252(_) => misc::build_identity(builder),
        SintConcrete::FromFelt252(_) => build_sint_from_felt252(builder, LOWER_LIMIT, UPPER_LIMIT),
        SintConcrete::IsZero(_) => misc::build_is_zero(builder),
        SintConcrete::WideMul(_) => build_small_wide_mul(builder),
    }
}
