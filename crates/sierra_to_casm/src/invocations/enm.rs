use casm::ap_change::ApChange;
use casm::casm;
use casm::operand::CellRef;
use num_bigint::ToBigInt;
use sierra::extensions::enm::{EnumConcreteLibFunc, EnumInitConcreteLibFunc};
use sierra::program::{BranchInfo, BranchTarget, StatementIdx};
use utils::try_extract_matches;

use super::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError, ReferenceExpressionView,
};
use crate::references::{CellExpression, ReferenceExpression, ReferenceValue, ReferencesError};
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra enum operations.
pub fn build(
    libfunc: &EnumConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        EnumConcreteLibFunc::Init(EnumInitConcreteLibFunc { index, .. }) => {
            build_enum_init(builder, *index)
        }
        EnumConcreteLibFunc::Match(_) => build_enum_match(builder),
    }
}

/// Handles statement for initializing an enum.
/// For example, with this setup
/// ```ignore
/// type felt_ty = felt;
/// type unit_ty = Tuple;
/// type Option = Enum<felt_ty, unit_ty>;
/// libfunc init_option_some = enum_init<Option, 0>;
/// felt_const<8>() -> (felt8);
/// ````
/// this "Sierra statement"
/// ```ignore
/// init_option_some(felt8=[ap-5]) -> (some_id);
/// ```
/// translates to these casm instructions:
/// ```ignore
/// [ap] = 0; ap++
/// [ap] = [ap-5]; ap++
/// ```
fn build_enum_init(
    builder: CompiledInvocationBuilder<'_>,
    index: usize,
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
    let init_arg = try_extract_matches!(
        expression
            .try_unpack_single()
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        CellExpression::Deref
    )
    .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    let variant_selector = if builder.invocation.branches.len() <= 2 {
        // For num_branches <= 2, we use the index as the variant_selector as the `match`
        // implementation jumps to the index 0 statement on 0, and to the index 1 statement on
        // 1.
        index
    } else {
        // For num_branches > 2, the `enum_match` libfunc is implemented using a jump table. In
        // order to optimize `enum_match`, we define the variant_selector as the relevant
        // relative jump in case we match the actual variant.
        //
        // - To jump to the first variant (`index` == 0) we add "jump rel 1", as the jump
        // instruction with a deref operand is of size 1.
        // - To jump to the variant in index i, we add "jump rel (2 * i + 1)" as the rest of the
        // jump instructions are with an immediate operand, which makes them of size
        // 2.
        if index.checked_mul(2).and_then(|x| x.checked_add(1)).is_none() {
            return Err(InvocationError::IntegerOverflow);
        }
        2 * index + 1
    };

    let enum_val = EnumView {
        variant_selector: CellExpression::Immediate(variant_selector.to_bigint().unwrap()),
        inner_value: CellExpression::Deref(init_arg),
    };
    Ok(builder.build_only_reference_changes([enum_val.to_reference_expression()].into_iter()))
}

/// Handles statement for matching an enum.
fn build_enum_match(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let matched_var = match builder.refs {
        [ReferenceValue { expression, .. }] => EnumView::try_get_view(expression)
            .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?,
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    // Verify variant_selector is of type deref. This is the case with an enum_value
    // that was validly created and then stored.
    let variant_selector =
        try_extract_matches!(matched_var.variant_selector, CellExpression::Deref)
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    let target_statement_ids = builder.invocation.branches.iter().map(|b| match b {
        BranchInfo { target: BranchTarget::Statement(stmnt_id), .. } => *stmnt_id,
        _ => panic!("malformed invocation"),
    });

    let num_branches = builder.invocation.branches.len();
    if num_branches <= 2 {
        build_enum_match_short(
            builder,
            num_branches,
            matched_var,
            variant_selector,
            target_statement_ids,
        )
    } else {
        build_enum_match_long(
            builder,
            num_branches,
            matched_var,
            variant_selector,
            target_statement_ids,
        )
    }
}

/// Handles statement for matching an enum with 1 or 2 variants.
/// For example, with this setup
/// ```ignore
/// type felt_ty = felt;
/// type unit_ty = Tuple;
/// type Option = Enum<felt_ty, unit_ty>;
/// libfunc init_option_some = enum_init<Option, 0>;
/// felt_const<8>() -> (felt8);
/// init_option_some(felt8=[ap-5]) -> (enum_var);
/// ````
/// this "Sierra statement" (2-variants-enum)
/// ```ignore
/// match_option(enum_var=[ap-10]) {1000(some=[ap-9]), 2000(none=[ap-9])};
/// ```
/// translates to these casm instructions:
/// ```ignore
/// jmp rel <jump_offset_2000> if [ap-10] != 0
/// jmp rel <jump_offset_1000>
/// ```
/// Or this "Sierra statement" (single-variant-enum)
/// ```ignore
/// match_option(enum_var=[ap-10]) {1000(var=[ap-9])};
/// ```
/// translates to these casm instructions:
/// ```ignore
/// jmp rel <jump_offset_1000>
/// ```
///
/// Assumes that num_branches == builder.invocation.branches.len() ==
/// target_statement_ids_iter.len() and that num_branches <= 2.
fn build_enum_match_short(
    builder: CompiledInvocationBuilder<'_>,
    num_branches: usize,
    matched_var: EnumView,
    variant_selector: CellRef,
    mut target_statement_ids: impl Iterator<Item = StatementIdx>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut instructions = Vec::new();
    let mut relocations = Vec::new();
    // Add the jump_nz instruction if we have 2 branches.
    if num_branches == 2 {
        instructions.extend(casm! { jmp rel 0 if variant_selector != 0; }.instructions);
        // Add the first instruction of jumping to branch 1 if jmp_table_idx != 0 (1).
        relocations.push(RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(target_statement_ids.nth(1).unwrap()),
        });
    }

    // TODO(yuval): this can be avoided with fallthrough.
    // Add the jump instruction to branch 0, anyway.
    instructions.extend(casm! { jmp rel 0; }.instructions);
    relocations.push(RelocationEntry {
        instruction_idx: instructions.len() - 1,
        relocation: Relocation::RelativeStatementId(target_statement_ids.next().unwrap()),
    });

    Ok(builder.build(
        instructions,
        relocations,
        itertools::repeat_n(ApChange::Known(0), num_branches).into_iter(),
        itertools::repeat_n(
            vec![ReferenceExpression::from_cell(matched_var.inner_value)].into_iter(),
            num_branches,
        )
        .into_iter(),
    ))
}

/// Handles statement for matching an enum with 3+ variants.
/// For example, with this setup
/// ```ignore
/// type felt_ty = felt;
/// type unit_ty = Tuple;
/// type Option = Enum<felt_ty, unit_ty>;
/// libfunc init_option_some = enum_init<Option, 0>;
/// felt_const<8>() -> (felt8);
/// init_option_some(felt8=[ap-5]) -> (enum_var);
/// ````
/// this "Sierra statement" (3-variants-enum)
/// ```ignore
/// match_option(enum_var=[ap-10]) {1000(pos=[ap-9]), 2000(neg=[ap-9]), 3000(zero=[ap-9])};
/// ```
/// translates to these casm instructions:
/// ```ignore
/// jmp rel [ap-10]
/// jmp rel <jump_offset_1000>
/// jmp rel <jump_offset_2000>
/// jmp rel <jump_offset_3000>
/// ```
/// Where in the first location of the enum_var there will be the jmp_table_idx (1 for the first
/// branch, 2 for the second and so on).
///
/// Assumes that num_branches == builder.invocation.branches.len() == target_statement_ids.len()
/// and that num_branches > 2.
fn build_enum_match_long(
    builder: CompiledInvocationBuilder<'_>,
    num_branches: usize,
    matched_var: EnumView,
    variant_selector: CellRef,
    target_statement_ids: impl Iterator<Item = StatementIdx>,
) -> Result<CompiledInvocation, InvocationError> {
    // The first instruction is the jmp to the relevant index in the jmp table.
    let mut instructions = casm! { jmp rel variant_selector; }.instructions;
    let mut relocations = Vec::new();

    for (i, stmnt_id) in target_statement_ids.enumerate() {
        // Add the jump instruction to the relevant target.
        instructions.extend(casm! { jmp rel 0; }.instructions);
        relocations.push(RelocationEntry {
            instruction_idx: i + 1,
            relocation: Relocation::RelativeStatementId(stmnt_id),
        });
    }

    Ok(builder.build(
        instructions,
        relocations,
        itertools::repeat_n(ApChange::Known(0), num_branches).into_iter(),
        itertools::repeat_n(
            vec![ReferenceExpression::from_cell(matched_var.inner_value)].into_iter(),
            num_branches,
        )
        .into_iter(),
    ))
}

/// A struct representing an actual enum value in the Sierra program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumView {
    /// This would be ReferenceExpression::Immediate after enum_init, and would be
    /// ReferenceExpression::Deref after store_*.
    pub variant_selector: CellExpression,
    /// The inner value of the enum - can be any ReferenceExpression.
    pub inner_value: CellExpression,
}

impl ReferenceExpressionView for EnumView {
    type Error = ReferencesError;

    fn try_get_view(expr: &ReferenceExpression) -> Result<Self, Self::Error> {
        if expr.cells.len() != 2 {
            return Err(ReferencesError::InvalidReferenceTypeForArgument);
        }
        Ok(EnumView { variant_selector: expr.cells[0].clone(), inner_value: expr.cells[1].clone() })
    }
    fn to_reference_expression(self) -> ReferenceExpression {
        ReferenceExpression { cells: vec![self.variant_selector, self.inner_value] }
    }
}
