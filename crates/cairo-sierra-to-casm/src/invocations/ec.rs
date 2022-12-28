use std::str::FromStr;

use cairo_casm::builder::CasmBuilder;
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
    }
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
        const beta = (get_beta());
        tempvar y2 = y * y;
        tempvar x2 = x * x;
        tempvar x3 = x2 * x;
        tempvar alpha_x_plus_beta = x + beta; // Here we use the fact that Alpha is 1.
        tempvar expected_y2 = x3 + alpha_x_plus_beta;
        tempvar diff;
        assert y2 = diff + expected_y2;
        jump NotOnCurve if diff != 0;
    };
    let failure_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[x, y]], None), ("NotOnCurve", &[], Some(failure_handle))],
    ))
}
