use std::str::FromStr;

use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::ec::EcConcreteLibfunc;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};

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
        EcConcreteLibfunc::Neg(_) => build_ec_neg(builder),
        EcConcreteLibfunc::StateAdd(_) => build_ec_state_add(builder),
        EcConcreteLibfunc::TryNew(_) => build_ec_point_try_new(builder),
        EcConcreteLibfunc::StateFinalize(_) => build_ec_state_finalize(builder),
        EcConcreteLibfunc::StateInit(_) => build_ec_state_init(builder),
        EcConcreteLibfunc::StateAddMul(_) => build_ec_state_add_mul(builder),
        EcConcreteLibfunc::PointFromX(_) => build_ec_point_from_x(builder),
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
    compute_lhs(casm_builder, y, computed_lhs);
    compute_rhs(casm_builder, x, computed_rhs);
}

/// Computes the left-hand side of the EC equation, namely `y^2`.
fn compute_lhs(casm_builder: &mut CasmBuilder, y: Var, computed_lhs: Var) {
    casm_build_extend! {casm_builder,
        assert computed_lhs = y * y;
    };
}

/// Computes the right-hand side of the EC equation, namely `x^3 + x + BETA`.
fn compute_rhs(casm_builder: &mut CasmBuilder, x: Var, computed_rhs: Var) {
    casm_build_extend! {casm_builder,
        const beta = (get_beta());
        tempvar x2 = x * x;
        tempvar x3 = x2 * x;
        tempvar alpha_x_plus_beta = x + beta; // Here we use the fact that Alpha is 1.
        assert computed_rhs = x3 + alpha_x_plus_beta;
    };
}

/// Extends the CASM builder to compute the sum - or difference - of two EC points, and store the
/// result in the given variables.
/// The inputs to the function are:
/// 1. The first point (`p0`).
/// 2. The X coordinate of the second point (`x1`).
/// 3. The "numerator", which is either `y0 - y1` (for point addition) or `y0 + y1` (for point
///    subtraction).
/// 4. The computation of `x0 - x1` (called "denominator"). Assumed to be non-zero.
fn add_ec_points(
    casm_builder: &mut CasmBuilder,
    p0: (Var, Var),
    x1: Var,
    numerator: Var,
    denominator: Var,
) -> (Var, Var) {
    let (x0, y0) = p0;

    casm_build_extend! {casm_builder,
        tempvar slope = numerator / denominator;
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
fn build_ec_point_try_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [x, y] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref x;
        deref y;
    };

    // Check if `(x, y)` is on the curve, by computing `y^2` and `x^3 + x + beta`.
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

/// Handles instruction for creating an EC point.
fn build_ec_point_from_x(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [x] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref x;
    };

    casm_build_extend! {casm_builder,
        tempvar rhs;
    };
    compute_rhs(&mut casm_builder, x, rhs);

    // Guess y, by either computing the square root of `rhs`, or of `3 * rhs`.
    casm_build_extend! {casm_builder,
        tempvar y;
        hint FieldSqrt {val: rhs} into {sqrt: y};
        tempvar lhs;
    };
    compute_lhs(&mut casm_builder, y, lhs);

    casm_build_extend! {casm_builder,
        tempvar diff = lhs - rhs;
        // If `(x, y)` is on the curve, return it.
        jump VerifyNotOnCurve if diff != 0;
        jump OnCurve;
        VerifyNotOnCurve:
        // Check that `y^2 = 3 * rhs`.
        const three = (3);
        assert lhs = rhs * three;
        // Note that `rhs != 0`: otherwise `y^2 = 3 * rhs = 0` implies `y = 0` and we would have
        // `y^2 = rhs` so this branch wouldn't have been chosen.
        // Alternatively, note that `rhs = 0` is not possible in curves of odd order (such as this
        // curve).
        // Since 3 is not a quadratic residue in the field, it will follow that `rhs` is not a
        // quadratic residue, which implies that there is no `y` such that `(x, y)` is on the curve.
        jump NotOnCurve;
        OnCurve:
        // Fallthrough - success.
    };

    let not_on_curve = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[x, y]], None), ("NotOnCurve", &[], Some(not_on_curve))],
    ))
}

/// Handles instruction for unwrapping an EC point.
fn build_ec_point_unwrap(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [x, y] = builder.try_get_refs::<1>()?[0].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref x;
        deref y;
    };

    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[x], &[y]], None)]))
}

/// Generates casm instructions for ec_neg().
fn build_ec_neg(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [x, y] = builder.try_get_refs::<1>()?[0].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref x;
        deref y;
    };
    casm_build_extend!(casm_builder,
        const neg_one = -1;
        let neg_y = y * neg_one;
    );

    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[x, neg_y]], None)]))
}

/// Handles instruction for initializing an EC state.
fn build_ec_state_init(
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
fn build_ec_state_add(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_state, expr_point] = builder.try_get_refs()?;
    let [sx, sy, random_ptr] = expr_state.try_unpack()?;
    let [px, py] = expr_point.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref px;
        deref py;
        deref sx;
        deref sy;
        deref random_ptr;
    };

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
        add_ec_points(&mut casm_builder, (px, py), sx, numerator, denominator);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[result_x, result_y, random_ptr]], None)],
    ))
}

/// Handles instruction for finalizing an EC state.
fn build_ec_state_finalize(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [x, y, random_ptr] = builder.try_get_refs::<1>()?[0].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref x;
        deref y;
        deref random_ptr;
    };

    // We want to return the point `(x, y) - (random_x, random_y)`, or in other words,
    // `(x, y) + (random_x, -random_y)`.
    casm_build_extend! {casm_builder,
        tempvar random_x = random_ptr[0];
        tempvar random_y = random_ptr[1];
        // If the X coordinate is the same, either the points are equal or their sum is the point at
        // infinity. Either way, we can't compute the slope in this case.
        // The result may be the point at infinity if the user called `ec_state_finalize`
        // immediately after ec_state_init.
        tempvar denominator = x - random_x;
        jump NotSameX if denominator != 0;
        // Assert the result is the point at infinity (the other option is the points are the same,
        // and doubling is not allowed).
        assert y = random_y;
        jump SumIsInfinity;
        NotSameX:
        // The numerator is the difference in Y coordinate values of the summed points, and the Y
        // coordinate of the negated random point is `-random_y`.
        tempvar numerator = y + random_y;
    }

    let (result_x, result_y) =
        add_ec_points(&mut casm_builder, (x, y), random_x, numerator, denominator);

    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[result_x, result_y]], None),
            ("SumIsInfinity", &[], Some(failure_handle)),
        ],
    ))
}

/// Handles instruction for computing `S + M * Q` where `S` is an EC state, `M` is a scalar (felt)
/// and `Q` is an EC point.
fn build_ec_state_add_mul(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [ec_builtin_expr, expr_state, expr_m, expr_point] = builder.try_get_refs()?;
    let ec_builtin = ec_builtin_expr.try_unpack_single()?;
    let [sx, sy, random_ptr] = expr_state.try_unpack()?;
    let [m] = expr_m.try_unpack()?;
    let [px, py] = expr_point.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(6) ec_builtin;
        deref sx;
        deref sy;
        deref random_ptr;
        deref px;
        deref py;
        deref m;
    };
    casm_build_extend! {casm_builder,
        assert sx = *(ec_builtin++);
        assert sy = *(ec_builtin++);
        assert px = *(ec_builtin++);
        assert py = *(ec_builtin++);
        assert m = *(ec_builtin++);
        let result_x = *(ec_builtin++);
        let result_y = *(ec_builtin++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[ec_builtin], &[result_x, result_y, random_ptr]], None)],
    ))
}
