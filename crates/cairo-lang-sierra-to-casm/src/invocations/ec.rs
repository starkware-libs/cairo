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
/// Assumes neither point is the point at infinity, and asserts their sum is not the point at
/// infinity (i.e. asserts p0 != -p1).
/// Also asserts that the points are not equal (i.e. no doubling allowed).
fn add_ec_points(casm_builder: &mut CasmBuilder, p0: (Var, Var), p1: (Var, Var)) -> (Var, Var) {
    let (x0, y0) = p0;
    let (x1, y1) = p1;

    casm_build_extend! {casm_builder,
        // If the X coordinate is the same, either the points are equal or their sum is the point at
        // infinity.
        tempvar diff_x = x0 - x1;
        jump NotSameX if diff_x != 0;
        // X coordinate is identical; either `p0 + p1` is the point at infinity (not allowed) or
        // `p0 = p1`, which is also not allowed (doubling).
        InfiniteLoop:
        jump InfiniteLoop;
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
    let (result_x, result_y) = add_ec_points(&mut casm_builder, (px, py), (sx, sy));
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[result_x, result_y, random_ptr]], None)],
    ))
}
