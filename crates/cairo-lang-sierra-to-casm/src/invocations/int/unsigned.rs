use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::int::unsigned::{UintConcrete, UintTraits};
use cairo_lang_sierra::extensions::int::{IntMulTraits, IntOperator};
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;
use cairo_lang_sierra::extensions::utils::Range;
use num_bigint::BigInt;

use super::{bounded, build_const, build_small_diff, build_small_wide_mul};
use crate::invocations::range_reduction::build_felt252_range_reduction;
use crate::invocations::{
    BuiltinInfo, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError, add_input_variables, bitwise, get_non_fallthrough_statement_id, misc,
};

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
            let deferred_a_plus_b = a + b;
            const limit_fixer = (u128::MAX - limit + 1);
            const limit = limit;
            hint TestLessThan {lhs: deferred_a_plus_b, rhs: limit} into {dst: no_overflow};
            jump NoOverflow if no_overflow != 0;
            // Overflow:
            // Here we know that `limit <= a + b <= 2 * limit - 2`.
            tempvar temp_a_plus_b = deferred_a_plus_b;
            tempvar fixed_a_plus_b = temp_a_plus_b - limit;
            assert fixed_a_plus_b = *(range_check++);
            jump Target;
        NoOverflow:
            // Here we know that `0 <= a + b < limit`
            // ==> `a + b + 2**128 - limit < limit + 2**128 - limit`
            // ==> `a + b + 2**128 - limit < 2**128`.

            tempvar temp_fixed_a_plus_b;
            tempvar a_plus_b = deferred_a_plus_b;
            assert temp_fixed_a_plus_b = a_plus_b + limit_fixer;
            assert temp_fixed_a_plus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[a_plus_b]], None),
            ("Target", &[&[range_check], &[fixed_a_plus_b]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
            extra_costs: None,
        },
    ))
}

/// Handles a uint square root operation.
pub fn build_sqrt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(3) range_check;
        deref value;
    };

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar fixed_root;
        tempvar root_squared;
        tempvar value_minus_root_squared;
        tempvar root_times_two;
        tempvar diff;
        tempvar root;

        // Calculate the square root.
        hint SquareRoot { value: value} into { dst: root };

        // Assert root is in [0, 2**125) by asserting:
        // (root + (2**128-1) - (2**125-1)) is in [0, 2**128) and root is in [0, 2**128).
        // The second assertion is needed because if root is very large (e.g., P - 1) the first
        // assertion may be true.
        const u125_upper_fixer = BigInt::from(u128::MAX - (u128::pow(2, 125) - 1));
        assert fixed_root = root + u125_upper_fixer;
        assert root = *(range_check++);
        assert fixed_root = *(range_check++);

        // Assert root**2 is in [0, value] by asserting (value - root**2) is in [0, 2**128).
        // Since we know root**2 is in [0, 2**250) (because we asserted root is in [0, 2**125))
        // and that value is in [0, 2**250) this is enough.
        assert root_squared = root * root;
        assert value_minus_root_squared = value - root_squared;
        assert value_minus_root_squared = *(range_check++);

        // Assert value is in [0, (root + 1)**2 ) by asserting (2*root - (value - root**2)) is in
        // [0, 2**128). this is equivalent because
        // (root + 1)**2 - 1 - value = 2*root - (value - root**2) .
        assert root_times_two = root + root;
        assert diff = root_times_two - value_minus_root_squared;
        assert diff = *(range_check++);
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[root]], None)],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
            extra_costs: None,
        },
    ))
}

/// Builds instructions for Sierra u8/u16/u32/u64 operations.
pub fn build_uint<TUintTraits: UintTraits + IntMulTraits + IsZeroTraits, const LIMIT: u128>(
    libfunc: &UintConcrete<TUintTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        UintConcrete::Const(libfunc) => build_const(libfunc, builder),
        UintConcrete::SquareRoot(_) => build_sqrt(builder),
        UintConcrete::Equal(_) => misc::build_cell_eq(builder),
        UintConcrete::Operation(libfunc) => match libfunc.operator {
            IntOperator::OverflowingAdd => build_small_uint_overflowing_add(builder, LIMIT),
            IntOperator::OverflowingSub => build_small_diff(builder, BigInt::from(LIMIT)),
        },
        UintConcrete::ToFelt252(_) => misc::build_identity(builder),
        UintConcrete::FromFelt252(_) => {
            build_felt252_range_reduction(builder, &Range::half_open(0, LIMIT), true)
        }
        UintConcrete::IsZero(_) => misc::build_is_zero(builder),
        UintConcrete::Divmod(_) => bounded::build_div_rem(
            builder,
            &Range::half_open(0, LIMIT),
            &Range::half_open(1, LIMIT),
        ),
        UintConcrete::Bitwise(_) => bitwise::build(builder),
        UintConcrete::WideMul(_) => build_small_wide_mul(builder),
    }
}
