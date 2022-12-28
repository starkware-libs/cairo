use std::str::FromStr;

use cairo_casm::builder::{CasmBuilder, Var};
use cairo_casm::casm_build_extend;
use cairo_casm::operand::ResOperand;
use cairo_sierra::extensions::ec::EcConcreteLibFunc;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;

/// Returns the Beta value of the Starkware elliptic curve.
fn get_beta() -> BigInt {
    BigInt::from_str("3141592653589793238462643383279502884197169399375105820974944592307816406665")
        .unwrap()
}

/// Builds instructions for Sierra EC operations.
pub fn build(
    libfunc: &EcConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        EcConcreteLibFunc::CreatePoint(_) => build_ec_point_try_create(builder),
        EcConcreteLibFunc::CreatePointAtInfinity(_) => build_ec_create_inf_point(builder),
        EcConcreteLibFunc::InitState(_) => build_ec_init_state(builder),
    }
}

/// Extends the CASM builder to include verification that (x,y) is a point on the curve (and *not*
/// the point at infinity).
/// Either asserts the point is on the curve, or adds a jump statement to a label called NotOnCurve,
/// depending on the boolean flag passed.
fn verify_ec_point(casm_builder: &mut CasmBuilder, x: Var, y: Var, jump_if_not_on_curve: bool) {
    casm_build_extend! {casm_builder,
        const beta = (get_beta());
        tempvar y2 = y * y;
        tempvar x2 = x * x;
        tempvar x3 = x2 * x;
        tempvar alpha_x_plus_beta = x + beta; // Here we use the fact that Alpha is 1.
        tempvar expected_y2 = x3 + alpha_x_plus_beta;
    };
    if jump_if_not_on_curve {
        casm_build_extend! {casm_builder,
            tempvar diff;
            assert y2 = diff + expected_y2;
            jump NotOnCurve if diff != 0;
        };
    } else {
        casm_build_extend! {casm_builder,
            assert y2 = expected_y2;
        };
    }
}

/// Extends the CASM builder to compute the sum of two EC points and store the result in the given
/// variables.
fn add_ec_points(
    casm_builder: &mut CasmBuilder,
    x0: Var,
    y0: Var,
    x1: Var,
    y1: Var,
    result_x: Var,
    result_y: Var,
) {
    // Logic taken from ec_safe_add:
    // https://github.com/starkware-industries/starkware/blob/dev/src/starkware/python/math_utils.py#L165
    casm_build_extend! {casm_builder,
        const zero = BigInt::from(0);
        const one = BigInt::from(1);
        const two = BigInt::from(2);
        const three = BigInt::from(3);
        // If either point is the point at infinity, return the other point.
        jump Point0IsNotInfinity if x0 != 0;
        jump Point0IsNotInfinity if y0 != 0;
        assert result_x = x1;
        assert result_y = y1;
        jump Done;
        Point0IsNotInfinity:
        jump Point1IsNotInfinity if x1 != 0;
        jump Point1IsNotInfinity if y1 != 0;
        assert result_x = x0;
        assert result_y = y0;
        jump Done;
        Point1IsNotInfinity:
        // If the X coordinate is the same, either the points are equal or their sum is the point at
        // infinity.
        tempvar diff_x = x0 - x1;
        jump NotSameX if diff_x != 0;
        // X coordinate is identical. If y0 + y1 is zero, the sum is the point at infinity.
        tempvar sum_y = y0 + y1;
        jump ReturnDouble if sum_y != 0;
        // If we are here, the sum of the points is the point at infinity.
        assert result_x = zero;
        assert result_y = zero;
        jump Done;
        ReturnDouble:
        // The `X` coordinates are the same, which means `y0` is either equal to `y1` or equal to
        // `-y1`. As `y0 + y1` is not 0, we must have `y0 == y1`. In addition, we know the `Y`
        // coordinate is non-zero (so we can divide by `2 * Y`).
        // Compute the "slope": `(3 * X * X + ALPHA) / (2 * Y)`, and use the slope to compute the
        // doubled point:
        // `result_x = slope * slope - 2 * X`
        // `result_y = slope * (X - result_x) - Y`
        tempvar x2 = x0 * x0;
        tempvar x2_times_3 = three * x2;
        tempvar numerator = x2_times_3 + one; // Alpha is 1 on our curve.
        tempvar denominator = two * y0;
        tempvar slope;
        assert numerator = slope * denominator;
        tempvar slope2 = slope * slope;
        tempvar x_times_2 = two * x0;
        assert x_times_2 = result_x + slope2;
        tempvar x_minus_result_x = x0 - result_x;
        tempvar x_minus_result_x_times_slope = x_minus_result_x * slope;
        assert x_minus_result_x_times_slope = result_y + y0;
        jump Done;
        NotSameX:
        // If we are here, then `p0 != p1` and `p0 != -p1`. Compute the "slope"
        // (`(y0 - y1) / (x0 - x1)`), and use the slope to compute the sum:
        // `result_x = slope * slope - x0 - x1`
        // `result_y = slope * (x0 - result_x) - y0`
        tempvar numerator = y0 - y1;
        tempvar denominator = x0 - x1;
        tempvar slope;
        assert numerator = slope * denominator;
        tempvar slope2 = slope * slope;
        tempvar sum_x = x0 + x1;
        assert slope2 = result_x + sum_x;
        tempvar x_change = x0 - result_x;
        tempvar slope_times_x_change = slope * x_change;
        assert slope_times_x_change = result_y + y0;
        Done:
    };
}

/// Handles instruction for creating an EC point.
fn build_ec_point_try_create(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_x, expr_y] = builder.try_get_refs()?;
    let x = expr_x.try_unpack_single()?.to_deref()?;
    let y = expr_y.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let x = casm_builder.add_var(ResOperand::Deref(x));
    let y = casm_builder.add_var(ResOperand::Deref(y));

    // Assert (x,y) is on the curve.
    verify_ec_point(&mut casm_builder, x, y, true);

    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[x, y]], None), ("NotOnCurve", &[], Some(failure_handle))],
    ))
}

/// Handles instruction for creating the EC point at infinity.
fn build_ec_create_inf_point(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();

    casm_build_extend! {casm_builder,
        const zero = BigInt::from(0);
        tempvar x = zero;
        tempvar y = zero;
    };

    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[x, y]], None)]))
}

/// Handles instruction for initializing an EC state from an EC point.
fn build_ec_init_state(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_point] = builder.try_get_refs()?;
    let [input_x, input_y] = expr_point.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    let input_x = casm_builder.add_var(ResOperand::Deref(input_x.to_deref()?));
    let input_y = casm_builder.add_var(ResOperand::Deref(input_y.to_deref()?));

    // Sample a random point and verify it's on the curve.
    casm_build_extend! {casm_builder,
        tempvar random_x;
        tempvar random_y;
        hint RandomEcPoint {} into { x: random_x, y: random_y };
    }
    verify_ec_point(&mut casm_builder, random_x, random_y, false);

    casm_build_extend! {casm_builder,
        // Create a pointer to the random EC point to return as part of the state.
        tempvar random_ptr;
        hint AllocSegment {} into {dst: random_ptr};
        assert random_x = random_ptr[0];
        assert random_y = random_ptr[1];
        // Allocate cells for the point sum and compute it.
        tempvar result_x;
        tempvar result_y;
    };
    add_ec_points(&mut casm_builder, input_x, input_y, random_x, random_y, result_x, result_y);

    // The third entry in the EC state is a pointer to the originally sampled random EC point.
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[result_x, result_y, random_ptr]], None)],
    ))
}
