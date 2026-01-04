use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::range::IntRangeConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::int::u128_bound;
use crate::invocations::{
    BuiltinInfo, CostValidationInfo, add_input_variables, get_non_fallthrough_statement_id,
};

/// Builds instructions for `Range` operations.
pub fn build(
    libfunc: &IntRangeConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        IntRangeConcreteLibfunc::PopFront(_) => build_pop_front(builder),
        IntRangeConcreteLibfunc::TryNew(_) => build_try_new(builder),
    }
}

/// Libfunc for constructing a range `[a, b)` if `a <= b`.
fn build_try_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_range_check, expr_start, expr_end] = builder.try_get_refs::<3>()?;
    let range_check = expr_range_check.try_unpack_single()?;
    let start = expr_start.try_unpack_single()?;
    let end = expr_end.try_unpack_single()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref start;
        deref end;
        buffer(1) range_check;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;

        tempvar diff = end - start;
        const bound = u128_bound().clone();
        tempvar is_valid_range;
        hint TestLessThan {lhs: diff, rhs: bound} into {dst: is_valid_range};
        jump Valid if is_valid_range != 0;

        // Invalid range.
        tempvar diff_fixed = diff + bound;
        assert diff_fixed = *(range_check++);
        jump Failure;

        Valid:
        assert diff = *(range_check++);
    };

    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[start, end]], None),
            ("Failure", &[&[range_check], &[end, end]], Some(failure_handle)),
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

/// Libfunc for reducing `[a, b)` to `[a + 1, b)`.
fn build_pop_front(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [start, end] = builder.try_get_refs::<1>()?[0].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref start;
        deref end;
    };
    casm_build_extend! {casm_builder,
        const one = 1;
        let new_start = start + one;
        tempvar is_non_empty = end - start;
        jump NonEmpty if is_non_empty != 0;
    };
    let non_empty_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[], None),
            ("NonEmpty", &[&[new_start, end], &[start]], Some(non_empty_handle)),
        ],
        Default::default(),
    ))
}
