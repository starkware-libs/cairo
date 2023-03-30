use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_sierra::extensions::felt252::{
    Felt252BinaryOpConcreteLibfunc, Felt252BinaryOperationConcrete, Felt252BinaryOperator,
    Felt252Concrete, Felt252OperationWithConstConcreteLibfunc,
};
use num_bigint::BigInt;

use super::misc::build_is_zero;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, CostValidationInfo};
use crate::references::ReferenceExpression;

#[cfg(test)]
#[path = "felt252_test.rs"]
mod test;

/// Builds instructions for Sierra felt252 operations.
pub fn build(
    libfunc: &Felt252Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Felt252Concrete::BinaryOperation(Felt252BinaryOperationConcrete::Binary(
            Felt252BinaryOpConcreteLibfunc { operator, .. },
        )) => build_felt252_op(builder, *operator),
        Felt252Concrete::BinaryOperation(Felt252BinaryOperationConcrete::Const(
            Felt252OperationWithConstConcreteLibfunc { operator, c, .. },
        )) => build_felt252_op_with_const(builder, *operator, c.clone()),
        Felt252Concrete::IsZero(_) => build_is_zero(builder),
        Felt252Concrete::Const(libfunc) => Ok(builder.build_only_reference_changes(
            [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))]
                .into_iter(),
        )),
    }
}

/// Handles a felt252 operation with the given op.
fn build_felt252_op(
    builder: CompiledInvocationBuilder<'_>,
    op: Felt252BinaryOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref_or_immediate b;
    };
    let (res_var, extra_costs) = bin_op_helper(&mut casm_builder, a, b, op);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[res_var]], None)],
        CostValidationInfo { range_check_info: None, extra_costs: Some([extra_costs]) },
    ))
}

/// Handles a felt252 operation with a const.
fn build_felt252_op_with_const(
    builder: CompiledInvocationBuilder<'_>,
    op: Felt252BinaryOperator,
    c: BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let [a] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder, deref a; };
    let c = casm_builder.add_var(CellExpression::Immediate(c));
    let (res_var, extra_costs) = bin_op_helper(&mut casm_builder, a, c, op);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[res_var]], None)],
        CostValidationInfo { range_check_info: None, extra_costs: Some([extra_costs]) },
    ))
}

/// Helper for the build felt252 binary op functions: returns the res Var and the extra costs for a
/// binary operation.
fn bin_op_helper(
    casm_builder: &mut CasmBuilder,
    a: Var,
    b: Var,
    op: Felt252BinaryOperator,
) -> (Var, i32) {
    if op == Felt252BinaryOperator::Div {
        casm_build_extend! {casm_builder,
            tempvar res = a / b;
        };
        (res, 400)
    } else {
        (casm_builder.bin_op(felt252_to_cell_operator(op), a, b), 0)
    }
}

/// Converts a felt252 operator to the corresponding cell operator.
fn felt252_to_cell_operator(op: Felt252BinaryOperator) -> CellOperator {
    match op {
        Felt252BinaryOperator::Add => CellOperator::Add,
        Felt252BinaryOperator::Sub => CellOperator::Sub,
        Felt252BinaryOperator::Mul => CellOperator::Mul,
        Felt252BinaryOperator::Div => CellOperator::Div,
    }
}
