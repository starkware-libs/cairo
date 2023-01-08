use std::str::FromStr;

use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::operand::ResOperand;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
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
    libfunc: &EcConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        EcConcreteLibfunc::AddToState(_) => build_ec_add_to_state(builder),
        EcConcreteLibfunc::CreatePoint(_) => build_ec_point_try_create(builder),
        EcConcreteLibfunc::FinalizeState(_) => build_ec_try_finalize_state(builder),
        EcConcreteLibfunc::InitState(_) => build_ec_init_state(builder),
        EcConcreteLibfunc::UnwrapPoint(_) => build_ec_point_unwrap(builder),
    }
}

/// Extends the CASM builder to include computation of `y^2` and `x^3 + x + BETA` for the given
/// pair (x, y). Populates the two "output vars" with the computed LHS and RHS of the EC equation.
fn verify_ec_point(
    casm_builder: &mut CasmBuilder,
    x: Var,
    y: Var,
    computed_lhs: Var,
    computed_rhs: Var,
) {
    casm_build_extend! {casm_builder,
        const beta = (get_beta());
        assert computed_lhs = y * y;
        tempvar x2 = x * x;
        tempvar x3 = x2 * x;
        tempvar alpha_x_plus_beta = x + beta; // Here we use the fact that Alpha is 1.
        assert computed_rhs = x3 + alpha_x_plus_beta;
    };
}

/// Extends the CASM builder to compute the sum of two EC points and store the result in the given
/// variables.
/// In addition to the two points, accepts the numerator (difference in Y coordinate values) and the
/// denominator (difference in X coordinate values). Assumes the denominator is non zero.
fn add_ec_points(
    casm_builder: &mut CasmBuilder,
    p0: (Var, Var),
    p1: (Var, Var),
    numerator: Var,
    denominator: Var,
) -> (Var, Var) {
    let (x0, y0) = p0;
    let (x1, _) = p1;

    casm_build_extend! {casm_builder,
        // TODO(dorimedini): Once PrimeDiv is removed, instead of the next 3 lines just do
        // `tempvar slope = numerator / denominator;`.
        tempvar slope;
        hint PrimeDiv { lhs: numerator, rhs: denominator } into { result: slope };
        assert slope = numerator / denominator;
        tempvar slope2 = slope * slope;
        tempvar sum_x = x0 + x1;
        tempvar result_x = slope2 - sum_x;
        tempvar x_change = x0 - result_x;
        tempvar slope_times_x_change = slope * x_change;
        tempvar result_y = slope_times_x_change - y0;
    };

    (result_x, result_y)
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
    casm_build_extend! {casm_builder,
        tempvar y2;
        tempvar expected_y2;
    };
    verify_ec_point(&mut casm_builder, x, y, y2, expected_y2);
    casm_build_extend! {casm_builder,
        tempvar diff = y2 - expected_y2;
        jump NotOnCurve if diff != 0;
    };

    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[x, y]], None), ("NotOnCurve", &[], Some(failure_handle))],
    ))
}

/// Handles instruction for unwrapping an EC point.
fn build_ec_point_unwrap(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_point] = builder.try_get_refs()?;
    let [x, y] = expr_point.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    let x = casm_builder.add_var(ResOperand::Deref(x.to_deref()?));
    let y = casm_builder.add_var(ResOperand::Deref(y.to_deref()?));

    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[x], &[y]], None)]))
}

/// Handles instruction for initializing an EC state.
fn build_ec_init_state(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();

    // Sample a random point on the curve.
    casm_build_extend! {casm_builder,
        tempvar random_x;
        tempvar random_y;
        hint RandomEcPoint {} into { x: random_x, y: random_y };
        // Assert the random point is on the curve.
        tempvar y2;
        tempvar expected_y2;
    }
    verify_ec_point(&mut casm_builder, random_x, random_y, y2, expected_y2);
    casm_build_extend! {casm_builder,
        assert y2 = expected_y2;
        // Create a pointer to the random EC point to return as part of the state.
        tempvar random_ptr;
        hint AllocSegment {} into {dst: random_ptr};
        assert random_x = random_ptr[0];
        assert random_y = random_ptr[1];
    };

    // The third entry in the EC state is a pointer to the sampled random EC point.
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[random_x, random_y, random_ptr]], None)],
    ))
}

/// Handles instruction for adding a point to an EC state.
fn build_ec_add_to_state(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_state, expr_point] = builder.try_get_refs()?;
    let [sx, sy, random_ptr] = expr_state.try_unpack()?;
    let [px, py] = expr_point.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    let px = casm_builder.add_var(ResOperand::Deref(px.to_deref()?));
    let py = casm_builder.add_var(ResOperand::Deref(py.to_deref()?));
    let sx = casm_builder.add_var(ResOperand::Deref(sx.to_deref()?));
    let sy = casm_builder.add_var(ResOperand::Deref(sy.to_deref()?));
    let random_ptr = casm_builder.add_var(ResOperand::Deref(random_ptr.to_deref()?));

    casm_build_extend! {casm_builder,
        // If the X coordinate is the same, either the points are equal or their sum is the point at
        // infinity. Either way, we can't compute the slope in this case.
        tempvar denominator = px - sx;
        jump NotSameX if denominator != 0;
        // X coordinate is identical; either the sum of the points is the point at infinity (not
        // allowed), or the points are equal, which is also not allowed (doubling).
        InfiniteLoop:
        jump InfiniteLoop;
        NotSameX:
        tempvar numerator = py - sy;
    };

    let (result_x, result_y) =
        add_ec_points(&mut casm_builder, (px, py), (sx, sy), numerator, denominator);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[result_x, result_y, random_ptr]], None)],
    ))
}

/// Handles instruction for finalizing an EC state.
fn build_ec_try_finalize_state(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_state] = builder.try_get_refs()?;
    let [x, y, random_ptr] = expr_state.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    let x = casm_builder.add_var(ResOperand::Deref(x.to_deref()?));
    let y = casm_builder.add_var(ResOperand::Deref(y.to_deref()?));
    let random_ptr = casm_builder.add_var(ResOperand::Deref(random_ptr.to_deref()?));

    // We want to return the point `(x, y) - (random_x, random_y)`, or in other words,
    // `(x, y) + (random_x, -random_y)`.
    casm_build_extend! {casm_builder,
        tempvar random_x = random_ptr[0];
        tempvar random_y = random_ptr[1];
        // If the X coordinate is the same, either the points are equal or their sum is the point at
        // infinity. Either way, we can't compute the slope in this case.
        // The result may be the point at infinity if the user called ec_try_finalize_state
        // immediately after ec_init_state.
        tempvar denominator = x - random_x;
        jump NotSameX if denominator != 0;
        // We compute `p0 + (-p1)`, and `-(x, y)` is `(x, -y)`. As such, the sum of the Y
        // coordinates is `y - random_y`.
        tempvar sum_y = y - random_y;
        jump SumIsNotInfinity if sum_y != 0;
        jump SumIsInfinity;
        SumIsNotInfinity:
        // X coordinate is identical; either `p0 + p1` is the point at infinity (either not allowed
        // or already handled), or `p0 = p1`, which is not allowed (doubling).
        InfiniteLoop:
        jump InfiniteLoop;
        NotSameX:
        // The numerator is the difference in Y coordinate values of the summed points, and the Y
        // coordinate of the negated random point is `-random_y`.
        tempvar numerator = y + random_y;
    }

    let (result_x, result_y) =
        add_ec_points(&mut casm_builder, (x, y), (random_x, random_y), numerator, denominator);

    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[result_x, result_y]], None),
            ("SumIsInfinity", &[], Some(failure_handle)),
        ],
    ))
}
