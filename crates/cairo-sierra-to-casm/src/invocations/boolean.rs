use cairo_casm::casm;
use cairo_casm::operand::{ap_cell_ref, DerefOrImmediate};
use cairo_sierra::extensions::boolean::BoolConcreteLibfunc;
use cairo_sierra::extensions::felt::FeltBinaryOperator;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression};

/// Builds instructions for Sierra bool operations.
pub fn build(
    libfunc: &BoolConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoolConcreteLibfunc::And(_) => build_bool_and(builder),
        BoolConcreteLibfunc::Not(_) => build_bool_not(builder),
    }
}

/// Handles instructions for boolean AND.
fn build_bool_and(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_a, expr_b] = builder.try_get_refs()?;
    let a = expr_a.try_unpack_single()?.to_deref()?;
    let b = expr_b.try_unpack_single()?.to_deref()?;
    Ok(builder.build(
        vec![],
        vec![],
        [[ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
            op: FeltBinaryOperator::Mul,
            a,
            b: DerefOrImmediate::from(b),
        }))]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles instructions for boolean NOT.
fn build_bool_not(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let a = builder.try_get_refs::<1>()?[0].try_unpack_single()?.to_deref()?;

    // We want to output `1 - a`, but a SUB expression cannot have an immediate value on the LHS.
    // Store 1 in AP first, advance AP and return `[ap - 1] - a`.
    Ok(builder.build(
        casm! { [ap + 0] = 1, ap++; }.instructions,
        vec![],
        [[ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
            op: FeltBinaryOperator::Sub,
            a: ap_cell_ref(-1),
            b: DerefOrImmediate::from(a),
        }))]
        .into_iter()]
        .into_iter(),
    ))
}
