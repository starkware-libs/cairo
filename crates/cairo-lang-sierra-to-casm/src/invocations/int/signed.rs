use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::int::signed::{SintConcrete, SintTraits};
use cairo_lang_sierra::extensions::int::IntMulTraits;
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;
use num_bigint::{BigInt, ToBigInt};
use num_traits::Zero;

use super::{add_input_variables, build_const, build_small_wide_mul};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{
    get_non_fallthrough_statement_id, misc, CompiledInvocation, CompiledInvocationBuilder,
    CostValidationInfo, InvocationError,
};

/// Handles a small signed integer conversion from felt252.
pub fn build_sint_from_felt252(
    builder: CompiledInvocationBuilder<'_>,
    lower_limit: BigInt,
    range_size: BigInt,
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
        const lower_limit_plus_range_size = lower_limit.clone() + range_size.clone();
        const positive_range_fixer = -lower_limit;
        const range_size_imm = range_size.clone();
        let canonical_value = value + positive_range_fixer;
        tempvar is_in_range;
        hint TestLessThan {lhs: canonical_value, rhs: range_size_imm} into {dst: is_in_range};
        jump IsInRange if is_in_range != 0;
        tempvar shifted_value = value - lower_limit_plus_range_size;
    }
    let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    validate_under_limit::<2>(
        &mut casm_builder,
        &(-Felt252::from(range_size.clone())).to_biguint().to_bigint().unwrap(),
        shifted_value,
        range_check,
        &auxiliary_vars,
    );
    casm_build_extend! {casm_builder,
        jump Done;
    IsInRange:
        tempvar a = canonical_value;
        assert a = *(range_check++);
    }
    // value + 2**128 - limit < 2**128 ==> value < limit
    // No need to do this check if `fixer_limit` is 0, as this was previously checked.
    let fixer_limit: BigInt = BigInt::from(u128::MAX) - range_size + 1;
    if !fixer_limit.is_zero() {
        casm_build_extend! {casm_builder,
            const fixer_limit = fixer_limit;
            tempvar b = a + fixer_limit;
            assert b = *(range_check++);
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

/// Builds instructions for Sierra s8/s16/s32/s64 operations.
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
        SintConcrete::FromFelt252(_) => build_sint_from_felt252(
            builder,
            LOWER_LIMIT.into(),
            BigInt::from(UPPER_LIMIT) - BigInt::from(LOWER_LIMIT) + 1,
        ),
        SintConcrete::IsZero(_) => misc::build_is_zero(builder),
        SintConcrete::WideMul(_) => build_small_wide_mul(builder),
    }
}
