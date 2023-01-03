use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::operand::ResOperand;
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::program::{BranchInfo, BranchTarget};

use super::{
    get_non_fallthrough_statement_id, CompiledInvocation, CompiledInvocationBuilder,
    InvocationError,
};
use crate::references::CellExpression;

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
/// felt_jump_nz(var=[ap-10]) { fallthrough() 1000(var) };
/// ```
/// translates to these casm instructions:
/// ```ignore
/// jmp rel <jump_offset_1000> if [ap-10] != 0
/// ```
pub fn build_jump_nz(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let value = builder.try_get_refs::<1>()?[0].try_unpack_single()?.to_deref()?;
    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    let value = casm_builder.add_var(ResOperand::Deref(value));
    casm_build_extend! {casm_builder,
        jump Target if value != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None), ("Target", &[&[value]], Some(target_statement_id))],
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
    Ok(builder.build_from_casm_builder(casm_builder, [("Target", &[], Some(*target_statement_id))]))
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
    let [expr_a, expr_b] = builder.try_get_refs()?;
    let a = expr_a.try_unpack_single()?;
    let b = expr_b.try_unpack_single()?;

    // The target line to jump to if a != b.
    let target_statement_id = get_non_fallthrough_statement_id(&builder);

    let (a, b) = match (a, b) {
        (CellExpression::Deref(cell_expr_a), CellExpression::Deref(cell_expr_b)) => {
            (ResOperand::Deref(*cell_expr_a), ResOperand::Deref(*cell_expr_b))
        }
        (CellExpression::Deref(cell_expr_a), CellExpression::Immediate(big_int_b)) => {
            (ResOperand::Deref(*cell_expr_a), ResOperand::Immediate(big_int_b.clone()))
        }
        // The casm line 'tempvar diff = a - b;' won't work if a is an immediate.
        // So if a is an immediate and b is a deref: switch them.
        // If a, b are both immediates, alternative cairo code will be used.
        (CellExpression::Immediate(big_int_a), CellExpression::Deref(cell_expr_b)) => {
            (ResOperand::Deref(*cell_expr_b), ResOperand::Immediate(big_int_a.clone()))
        }
        (CellExpression::Immediate(big_int_a), CellExpression::Immediate(big_int_b)) => {
            casm_build_extend! {casm_builder,
                const difference = big_int_a - big_int_b;
                tempvar diff = difference;
                // diff = a - b => (diff == 0) <==> (a == b)
                jump NotEqual if diff != 0;
                jump Equal;
            NotEqual:
            };
            return Ok(builder.build_from_casm_builder(
                casm_builder,
                [("Fallthrough", &[], None), ("Equal", &[], Some(target_statement_id))],
            ));
        }
        _ => {
            return Err(InvocationError::InvalidReferenceExpressionForArgument);
        }
    };

    let (a, b) = (casm_builder.add_var(a), casm_builder.add_var(b));
    casm_build_extend! {casm_builder,
        // diff = a - b => (diff == 0) <==> (a == b)
        tempvar diff = a - b;
        jump NotEqual if diff != 0;
        jump Equal;
    NotEqual:
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[], None), ("Equal", &[], Some(target_statement_id))],
    ))
}
