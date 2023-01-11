use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_sierra::extensions::felt::{
    FeltBinaryOpConcreteLibfunc, FeltBinaryOperationConcreteLibfunc, FeltBinaryOperator,
    FeltConcrete, FeltOperationWithConstConcreteLibfunc,
};
use num_bigint::BigInt;

use super::misc::build_jump_nz;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;
use crate::references::ReferenceExpression;

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
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref_or_immediate b;
    };
    let res = casm_builder.bin_op(felt_to_cell_operator(op), a, b);
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[res]], None)]))
}

/// Handles a felt operation with a const.
fn build_felt_op_with_const(
    builder: CompiledInvocationBuilder<'_>,
    op: FeltBinaryOperator,
    c: BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let [a] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder, deref a; };
    let c = casm_builder.add_var(CellExpression::Immediate(c));
    let res = casm_builder.bin_op(felt_to_cell_operator(op), a, c);
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[res]], None)]))
}

/// Converts a felt operator to the corresponding cell operator.
fn felt_to_cell_operator(op: FeltBinaryOperator) -> CellOperator {
    match op {
        FeltBinaryOperator::Add => CellOperator::Add,
        FeltBinaryOperator::Sub => CellOperator::Sub,
        FeltBinaryOperator::Mul => CellOperator::Mul,
        FeltBinaryOperator::Div => CellOperator::Div,
    }
}
