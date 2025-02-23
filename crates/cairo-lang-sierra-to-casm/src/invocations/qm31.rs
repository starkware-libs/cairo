use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_sierra::extensions::qm31::{
    QM31BinaryOpConcreteLibfunc, QM31BinaryOperator, QM31Concrete, QM31ConstConcreteLibfunc,
};
use num_bigint::BigInt;

use super::misc::build_is_zero;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{CostValidationInfo, add_input_variables};
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra qm31 operations.
pub fn build(
    libfunc: &QM31Concrete,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        QM31Concrete::BinaryOperation(QM31BinaryOpConcreteLibfunc { operator, .. }) => {
            build_qm31_op(builder, *operator)
        }
        QM31Concrete::IsZero(_) => build_is_zero(builder),
        QM31Concrete::Const(libfunc) => build_qm31_const(builder, libfunc),
    }
}

/// Handles a qm31 operation with a variable.
pub fn build_qm31_op(
    builder: CompiledInvocationBuilder<'_>,
    op: QM31BinaryOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref_or_immediate b;
    };
    let op = match op {
        QM31BinaryOperator::Add => CellOperator::Add,
        QM31BinaryOperator::Sub => CellOperator::Sub,
        QM31BinaryOperator::Mul => CellOperator::Mul,
        QM31BinaryOperator::Div => CellOperator::Div,
    };
    let res = casm_builder.bin_op(op, a, b);
    let dst = casm_builder.alloc_var(false);
    casm_builder.assert_vars_eq(dst, res, true);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[dst]], None)],
        CostValidationInfo::default(),
    ))
}

fn build_qm31_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &QM31ConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let mut value = BigInt::from(libfunc.w3);
    value <<= 36;
    value += BigInt::from(libfunc.w2);
    value <<= 36;
    value += BigInt::from(libfunc.w1);
    value <<= 36;
    value += BigInt::from(libfunc.w0);
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(value))].into_iter(),
    ))
}
