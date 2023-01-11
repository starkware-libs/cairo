use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::uint::{Uint8Concrete, UintConstConcreteLibfunc, UintTraits};
use num_bigint::BigInt;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};
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
    }
}
