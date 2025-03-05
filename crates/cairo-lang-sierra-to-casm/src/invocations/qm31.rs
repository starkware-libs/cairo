use cairo_lang_casm::builder::{AssertEqKind, CasmBuilder};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::qm31::{
    QM31BinaryOpConcreteLibfunc, QM31BinaryOperator, QM31Concrete, QM31ConstConcreteLibfunc,
};
use num_bigint::BigInt;

use super::misc::{build_identity, build_is_zero};
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{BuiltinInfo, CostValidationInfo, add_input_variables};
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
        QM31Concrete::Pack(_) => build_qm31_pack(builder),
        QM31Concrete::Unpack(_) => build_qm31_unpack(builder),
        QM31Concrete::FromM31(_) => build_identity(builder),
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
    casm_builder.assert_vars_eq(dst, res, AssertEqKind::QM31);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[dst]], None)],
        CostValidationInfo::default(),
    ))
}

/// Each `m31` in the `felt252` representation of a `qm31` is in a `36` bit part of the word,
/// containing a 5 extra 0 bits.
const PART_SIZE: usize = 36;
/// The maximal possible value of a `m31` part of the `qm31` representation as a `felt252`.
const PART_UPPER_BOUND: u128 = 1 << PART_SIZE;

fn build_qm31_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &QM31ConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let mut value = BigInt::from(libfunc.w3);
    value <<= PART_SIZE;
    value += BigInt::from(libfunc.w2);
    value <<= PART_SIZE;
    value += BigInt::from(libfunc.w1);
    value <<= PART_SIZE;
    value += BigInt::from(libfunc.w0);
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(value))].into_iter(),
    ))
}

fn build_qm31_pack(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [w0, w1, w2, w3] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref w0;
        deref w1;
        deref w2;
        deref w3;
    };

    casm_build_extend! {casm_builder,
        const shift_value = PART_UPPER_BOUND;
        tempvar shifted_w3 = w3 * shift_value;
        tempvar unshifted_w3_w2 = shifted_w3 + w2;
        tempvar shifted_w3_w2 = unshifted_w3_w2 * shift_value;
        tempvar unshifted_w3_w2_w1 = shifted_w3_w2 + w1;
        tempvar shifted_w3_w2_w1 = unshifted_w3_w2_w1 * shift_value;
        tempvar w3_w2_w1_w0 = shifted_w3_w2_w1 + w0;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[w3_w2_w1_w0]], None)],
        CostValidationInfo::default(),
    ))
}

fn build_qm31_unpack(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, w3_w2_w1_w0] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(5) range_check;
        deref w3_w2_w1_w0;
    };

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        const shift_value = PART_UPPER_BOUND;
        // Splitting the qm31 into its 4 parts using hints.
        tempvar w0;
        tempvar w3_w2_w1;
        hint DivMod { lhs: w3_w2_w1_w0, rhs: shift_value } into { quotient: w3_w2_w1, remainder: w0 };
        tempvar w1;
        tempvar w3_w2;
        hint DivMod { lhs: w3_w2_w1, rhs: shift_value } into { quotient: w3_w2, remainder: w1 };
        tempvar w2;
        tempvar w3;
        hint DivMod { lhs: w3_w2, rhs: shift_value } into { quotient: w3, remainder: w2 };
        // Validating that the parts reconstruct the original value.
        tempvar shifted_w3 = w3 * shift_value;
        assert w3_w2 = shifted_w3 + w2;
        tempvar shifted_w3_w2 = w3_w2 * shift_value;
        assert w3_w2_w1 = shifted_w3_w2 + w1;
        tempvar shifted_w3_w2_w1 = w3_w2_w1 * shift_value;
        assert w3_w2_w1_w0 = shifted_w3_w2_w1 + w0;
        // Validating each part is within [0, 2**128).
        assert w0 = *(range_check++);
        assert w1 = *(range_check++);
        assert w2 = *(range_check++);
        assert w3 = *(range_check++);
        tempvar w0_plus_w1 = w0 + w1;
        tempvar w2_plus_w3 = w2 + w3;
        // `sum` of the values must be within [0, 2**130), and we had no overflow during its
        // calculation.
        // If we prove the sum is specifically in [0, 2**36), we'd know that all values are
        // within [0, 2**36), and the unpacking is valid.
        tempvar sum = w0_plus_w1 + w2_plus_w3;
        // Additionally validating `sum + 2**128 - 2**36` is within [0, 2**128), and therefore:
        // sum is also within [-2**128 + 2**36, 2**36) - and specifically in [0, 2**36).
        const fixer = (u128::MAX - PART_UPPER_BOUND);
        tempvar rc_value = sum + fixer;
        assert rc_value = *(range_check++);
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[w0], &[w1], &[w2], &[w3]], None)],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
            extra_costs: None,
        },
    ))
}
