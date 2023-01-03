use cairo_casm::operand::DerefOrImmediate;
use cairo_sierra::extensions::felt::{
    FeltBinaryOpConcreteLibfunc, FeltBinaryOperationConcreteLibfunc, FeltBinaryOperator,
    FeltConcrete, FeltOperationWithConstConcreteLibfunc,
};
use num_bigint::BigInt;

use super::misc::build_jump_nz;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression};

#[cfg(test)]
#[path = "felt_test.rs"]
mod test;

/// Builds instructions for Sierra felt operations.
pub fn build(
    libfunc: &FeltConcrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibfunc::Binary(
            FeltBinaryOpConcreteLibfunc { operator, .. },
        )) => build_felt_op(builder, *operator),
        FeltConcrete::BinaryOperation(FeltBinaryOperationConcreteLibfunc::Const(
            FeltOperationWithConstConcreteLibfunc { operator, c, .. },
        )) => build_felt_op_with_const(builder, *operator, c.clone()),
        FeltConcrete::JumpNotZero(_) => build_jump_nz(builder),
        FeltConcrete::Const(libfunc) => Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))]
                .into_iter(),
        )),
    }
}

/// Handles a felt operation with the given op.
fn build_felt_op(
    builder: CompiledInvocationBuilder<'_>,
    op: FeltBinaryOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_a, expr_b] = builder.try_get_refs()?;
    let a = expr_a.try_unpack_single()?.to_deref()?;
    let b = expr_b.try_unpack_single()?.to_deref_or_immediate()?;
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression { op, a, b }))]
            .into_iter(),
    ))
}

/// Handles a felt operation with a const.
fn build_felt_op_with_const(
    builder: CompiledInvocationBuilder<'_>,
    op: FeltBinaryOperator,
    c: BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let a = builder.try_get_refs::<1>()?[0].try_unpack_single()?.to_deref()?;
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::BinOp(BinOpExpression {
            op,
            a,
            b: DerefOrImmediate::Immediate(c),
        }))]
        .into_iter(),
    ))
}
