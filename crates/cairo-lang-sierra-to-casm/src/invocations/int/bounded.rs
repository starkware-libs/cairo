use std::ops::Shl;

use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellOperator;
use cairo_lang_sierra::extensions::bounded_int::BoundedIntConcreteLibfunc;
use num_bigint::{BigInt, ToBigInt};

use crate::invocations::{
    add_input_variables, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError,
};

/// Builds instructions for bounded int operations.
pub fn build(
    libfunc: &BoundedIntConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoundedIntConcreteLibfunc::Add(_) => build_simple_op(builder, CellOperator::Add),
        BoundedIntConcreteLibfunc::Sub(_) => build_simple_op(builder, CellOperator::Sub),
        BoundedIntConcreteLibfunc::Mul(_) => build_simple_op(builder, CellOperator::Mul),
        BoundedIntConcreteLibfunc::DivRem(libfunc) => {
            build_div_rem(builder, &libfunc.dividend_bound)
        }
    }
}

/// Build instructions for simple operations of bounded ints.
fn build_simple_op(
    builder: CompiledInvocationBuilder<'_>,
    op: CellOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref b;
    };
    // Valid since bounded int type is currently bounded by felt252 type range.
    let result = casm_builder.bin_op(op, a, b);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[result]], None)],
        CostValidationInfo::default(),
    ))
}

/// Build div rem on bounded ints where the dividend is bounded by `dividend_bound` and the
/// `quotient` is bounded by `2**128`.
pub fn build_div_rem(
    builder: CompiledInvocationBuilder<'_>,
    dividend_bound: &BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    // Asserting we won't overwrap on prime during validation.
    assert!(dividend_bound.shl(128) < Felt252::prime().to_bigint().unwrap());

    let [range_check, a, b] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar r_plus_1;
        tempvar b_minus_r_minus_1;
        tempvar bq;
        tempvar q;
        tempvar r;
        hint DivMod { lhs: a, rhs: b } into { quotient: q, remainder: r };

        // Verify `0 <= r`.
        assert r = *(range_check++);
        // Verify `r < b` by constraining `0 <= b - (r + 1)`.
        const one = 1;
        assert r_plus_1 = r + one;
        assert b_minus_r_minus_1 = b - r_plus_1;
        assert b_minus_r_minus_1 = *(range_check++);

        // Check that `0 <= q < 2**128`.
        assert q = *(range_check++);

        // Check that `a = q * b + r`. Both hands are in the range [0, 2**128 * dividend_bound),
        // since q < 2**128 and b < dividend_bound.
        // Therefore, both hands are in the range [0, PRIME), and thus the equality
        // is an equality as integers (rather than only as field elements).
        assert bq = b * q;
        assert a = bq + r;
    }
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[q], &[r]], None)],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
