use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::program::{BranchInfo, BranchTarget};
use itertools::Itertools;
use num_bigint::BigInt;

use super::{
    get_non_fallthrough_statement_id, CompiledInvocation, CompiledInvocationBuilder,
    InvocationError,
};
use crate::invocations::add_input_variables;

/// Handles a revoke ap tracking instruction.
pub fn build_revoke_ap_tracking(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    Ok(builder.build(vec![], vec![], [[].into_iter()].into_iter()))
}

/// Handles a dup instruction.
pub fn build_dup(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let expression = builder.try_get_refs::<1>()?[0].clone();
    Ok(builder.build_only_reference_changes([expression.clone(), expression].into_iter()))
}

/// Handles a drop instruction.
pub fn build_drop(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    Ok(builder.build_only_reference_changes([].into_iter()))
}

/// Handles a jump non zero statement.
/// For example, this "Sierra statement"
/// ```ignore
/// felt_is_zero(var=[ap-10]) { fallthrough() 1000(var) };
/// ```
/// translates to these casm instructions:
/// ```ignore
/// jmp rel <jump_offset_1000> if [ap-10] != 0
/// ```
pub fn build_is_zero(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [value] = builder.try_get_single_cells()?;
    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder, deref value; );
    casm_build_extend! {casm_builder,
        jump Target if value != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None), ("Target", &[&[value]], Some(target_statement_id))],
        Default::default(),
    ))
}

/// Handles a jump instruction.
pub fn build_jump(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let target_statement_id = match builder.invocation.branches.as_slice() {
        [BranchInfo { target: BranchTarget::Statement(statement_id), .. }] => statement_id,
        _ => panic!("malformed invocation"),
    };
    let mut casm_builder = CasmBuilder::default();
    casm_build_extend! {casm_builder,
        jump Target;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Target", &[], Some(*target_statement_id))],
        Default::default(),
    ))
}

/// Handles an operations that does no changes to the reference expressions.
pub fn build_identity(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let outputs = builder.refs.iter().map(|r| r.expression.clone());
    Ok(builder.build_only_reference_changes(outputs))
}

pub fn build_branch_align(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let ap_fix = builder
        .program_info
        .metadata
        .ap_change_info
        .variable_values
        .get(&builder.idx)
        .copied()
        .unwrap_or(0);
    Ok(builder.build(
        if ap_fix > 0 { casm! {ap += ap_fix;}.instructions } else { vec![] },
        vec![],
        [vec![].into_iter()].into_iter(),
    ))
}

// Handle single cell equality check, for types that have a single representation.
pub fn build_cell_eq(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();
    let [a, b] = builder.try_get_single_cells()?;

    // The target line to jump to if a != b.
    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    let diff = if matches!(a, CellExpression::Deref(_)) {
        add_input_variables! {casm_builder,
            deref a;
            deref_or_immediate b;
        };
        casm_build_extend!(casm_builder, tempvar diff = a - b;);
        diff
    } else if matches!(b, CellExpression::Deref(_)) {
        // If `a` is an immediate the previous `a - b` wouldn't be a legal command, so we do `b - a`
        // instead.
        add_input_variables! {casm_builder,
            deref b;
            deref_or_immediate a;
        };
        casm_build_extend!(casm_builder, tempvar diff = b - a;);
        diff
    } else if let (CellExpression::Immediate(a), CellExpression::Immediate(b)) = (a, b) {
        // If both `a` an `b` are immediates we do the diff calculation of code, but simulate the
        // same flow to conform on AP changes.
        casm_build_extend! {casm_builder,
            const diff_imm = a - b;
            tempvar diff = diff_imm;
        };
        diff
    } else {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    };
    casm_build_extend! {casm_builder,
        // diff = a - b => (diff == 0) <==> (a == b)
        jump NotEqual if diff != 0;
        jump Equal;
    NotEqual:
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None), ("Equal", &[], Some(target_statement_id))],
        Default::default(),
    ))
}

/// Helper to add code that validates that variable `value` is smaller than `limit`, with `K`
/// constant for checking this bound. `auxiliary_vars` are the variables already allocated used for
/// execution of the algorithm, requires different sizes for different `K`s, for 1 requires 4, for 2
/// requires 5.
///
/// We show that a number is in the range [0, `limit`) by writing it as:
///   A * x + y,
/// where:
///   * K = low positive number (the lower the better, here we support only 1 or 2).
///   * max_x = 2**128 - K.
///   * A = `limit` / max_x.
///   * B = `limit` % max_x.
///   * x is in the range [0, max_x],
///   * y is in the range [0, B):
///     * y is in the range [0, 2**128).
///     * y + 2**128 - B is in the range [0, 2**128).
///
/// Note that the minimal possible value of the expression A * x + y is min_val = 0 (where x = y
/// = 0), and the maximal value is obtained where x = max_x and y = B - 1:
///   max_val = (A * max_x + B) - 1 = `limit` - 1.
///
/// As long as A <= B, every number in the range can be represented.
/// We assert that the A and B generated by the provided `K` parameter fit the constraint.
pub fn validate_under_limit<const K: u8>(
    casm_builder: &mut CasmBuilder,
    limit: &BigInt,
    value: Var,
    range_check: Var,
    auxiliary_vars: &[Var],
) {
    let a_imm = limit / (u128::MAX - (K - 1) as u128);
    let b_imm = limit % (u128::MAX - (K - 1) as u128);
    assert!(a_imm <= b_imm, "Must choose `K` such that `{a_imm} (`A`) <= {b_imm} (`B`)");
    casm_build_extend! {casm_builder,
        const a_imm = a_imm;
        // 2**128 - B.
        const b_imm_fix = (BigInt::from(u128::MAX) - b_imm + 1) as BigInt;
        const u128_limit_minus_1 = u128::MAX;
    }
    match K {
        1 => {
            let (x, y, x_part, y_fixed) =
                auxiliary_vars.iter().cloned().collect_tuple().expect("Wrong amount of vars.");
            casm_build_extend! {casm_builder,
                hint LinearSplit {value: value, scalar: a_imm, max_x: u128_limit_minus_1} into {x: x, y: y};
                assert x_part = x * a_imm;
                assert value = x_part + y;
                // x < 2**128
                assert x = *(range_check++);
                // y < 2**128
                assert y = *(range_check++);
                // y + 2**128 - B < 2**128 ==> y < B
                assert y_fixed = y + b_imm_fix;
                assert y_fixed = *(range_check++);
            };
        }
        2 => {
            let (x, y, x_part, y_fixed, diff) =
                auxiliary_vars.iter().cloned().collect_tuple().expect("Wrong amount of vars.");
            casm_build_extend! {casm_builder,
                const u128_limit_minus_2 = u128::MAX - 1;
                hint LinearSplit {value: value, scalar: a_imm, max_x: u128_limit_minus_2} into {x: x, y: y};
                assert x_part = x * a_imm;
                assert value = x_part + y;
                // y < 2**128
                assert y = *(range_check++);
                // y + 2**128 - B < 2**128 ==> y < B
                assert y_fixed = y + b_imm_fix;
                assert y_fixed = *(range_check++);
                // x < 2**128 && x != 2**128 - 1 ==> x < 2**128 - 1
                assert x = *(range_check++);
                assert diff = x - u128_limit_minus_1;
                jump Done if diff != 0;
                InfiniteLoop:
                jump InfiniteLoop;
            };
        }
        _ => unreachable!("Only K value of 1 or 2 are supported."),
    }
}
