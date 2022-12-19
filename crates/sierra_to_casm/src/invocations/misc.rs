use casm::casm;
use sierra::program::{BranchInfo, BranchTarget};
use utils::try_extract_matches;

use super::{
    get_non_fallthrough_statement_id, CompiledInvocation, CompiledInvocationBuilder,
    InvocationError,
};
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue};
use crate::relocations::{Relocation, RelocationEntry};

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
    let expression = match builder.refs {
        [ReferenceValue { expression, .. }] => expression,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    Ok(builder.build_only_reference_changes([expression.clone(), expression.clone()].into_iter()))
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
    let dst_expr = match builder.refs {
        [ReferenceValue { expression, .. }] => expression,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    let value = try_extract_matches!(
        dst_expr
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    let target_statement_id = get_non_fallthrough_statement_id(&builder);

    Ok(builder.build(
        casm! { jmp rel 0 if value != 0; }.instructions,
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(target_statement_id),
        }],
        [
            vec![].into_iter(),
            vec![ReferenceExpression::from_cell(CellExpression::Deref(value))].into_iter(),
        ]
        .into_iter(),
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

    Ok(builder.build(
        casm! { jmp rel 0; }.instructions,
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(*target_statement_id),
        }],
        [vec![].into_iter()].into_iter(),
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
