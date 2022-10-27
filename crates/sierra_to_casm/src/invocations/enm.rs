use casm::ap_change::ApChange;
use casm::casm;
use casm::operand::CellRef;
use itertools::{chain, repeat_n};
use num_bigint::ToBigInt;
use sierra::extensions::enm::{EnumConcreteLibFunc, EnumInitConcreteLibFunc};
use sierra::extensions::ConcreteLibFunc;
use sierra::ids::ConcreteTypeId;
use sierra::program::{BranchInfo, BranchTarget, StatementIdx};
use utils::try_extract_matches;

use super::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError, ReferenceExpressionView,
};
use crate::invocations::ProgramInfo;
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
    let init_arg_cells = &expression.cells;

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

    let variant_size = builder
        .program_info
        .type_sizes
        .get(&builder.libfunc.param_signatures()[0].ty)
        .ok_or(InvocationError::UnknownTypeData)?
        .to_owned();
    if init_arg_cells.len() != variant_size {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }

    let concrete_enum_type = &builder.libfunc.output_types()[0][0];
    let enum_val = EnumView {
        variant_selector: CellExpression::Immediate(variant_selector.to_bigint().unwrap()),
        inner_value: init_arg_cells.to_vec(),
        enum_size: get_enum_size(&builder.program_info, concrete_enum_type)
            .ok_or(InvocationError::UnknownTypeData)?,
    };
    let output_expressions = [enum_val.to_reference_expression()].into_iter();
    Ok(builder.build_only_reference_changes(output_expressions))
}

/// Handles statement for matching an enum.
fn build_enum_match(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let concrete_enum_type = &builder.libfunc.param_signatures()[0].ty;
    let matched_var = match builder.refs {
        [ReferenceValue { expression, .. }] => {
            EnumView::try_get_view(expression, &builder.program_info, concrete_enum_type)
                .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?
        }
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

    let mut branch_output_sizes: Vec<usize> = Vec::new();
    for branch_outputs in &builder.libfunc.output_types() {
        // Each branch has a single output.
        let branch_output = &branch_outputs[0];
        let branch_output_size = builder
            .program_info
            .type_sizes
            .get(branch_output)
            .ok_or(InvocationError::UnknownTypeData)?;
        branch_output_sizes.push(*branch_output_size);
    }
    let output_expressions = branch_output_sizes.into_iter().map(|size| {
        matched_var
            .inner_value
            .iter()
            .take(size)
            // TODO(yg): can this clone be removed?
            .map(|cell| ReferenceExpression { cells: vec![cell.clone()] })
    });

    let num_branches = builder.invocation.branches.len();
    if num_branches <= 2 {
        build_enum_match_short(
            builder,
            num_branches,
            variant_selector,
            target_statement_ids,
            output_expressions,
        )
    } else {
        build_enum_match_long(
            builder,
            num_branches,
            variant_selector,
            target_statement_ids,
            output_expressions,
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
/// target_statement_ids.len() == output_expressions.len() and that num_branches <= 2.
fn build_enum_match_short(
    builder: CompiledInvocationBuilder<'_>,
    num_branches: usize,
    variant_selector: CellRef,
    mut target_statement_ids: impl Iterator<Item = StatementIdx>,
    output_expressions: impl Iterator<Item = impl Iterator<Item = ReferenceExpression>>,
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
        output_expressions,
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
/// Assumes that num_branches == self.invocation.branches.len() ==
/// target_statement_ids.len() == output_expressions.len() and that num_branches > 2.
fn build_enum_match_long(
    builder: CompiledInvocationBuilder<'_>,
    num_branches: usize,
    variant_selector: CellRef,
    target_statement_ids: impl Iterator<Item = StatementIdx>,
    output_expressions: impl Iterator<Item = impl Iterator<Item = ReferenceExpression>>,
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
        output_expressions,
    ))
}

/// A struct representing an actual enum value in the Sierra program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumView {
    /// This would be ReferenceExpression::Immediate after enum_init, and would be
    /// ReferenceExpression::Deref after store_*.
    pub variant_selector: CellExpression,
    /// The inner value of the enum - a flat vector of cell expressions.
    // TODO(yg): maybe I can now change it to be a ref.
    pub inner_value: Vec<CellExpression>,
    /// The size of the enum type. Used to compute padding (as the total size of the enum must be
    /// the same regardless of the specific variant).
    pub enum_size: usize,
}

impl ReferenceExpressionView for EnumView {
    type Error = ReferencesError;

    fn try_get_view(
        expr: &ReferenceExpression,
        program_info: &ProgramInfo<'_>,
        concrete_type_id: &ConcreteTypeId,
    ) -> Result<Self, Self::Error> {
        let enum_size = get_enum_size(program_info, concrete_type_id)
            .ok_or(ReferencesError::InvalidReferenceTypeForArgument)?;
        // Verify the size.
        if expr.cells.len() != enum_size {
            return Err(ReferencesError::InvalidReferenceTypeForArgument);
        }

        let mut expr_cells_iter = expr.cells.iter();
        let variant_selector =
            expr_cells_iter.next().ok_or(ReferencesError::InvalidReferenceTypeForArgument)?.clone();

        Ok(EnumView {
            variant_selector,
            // TODO(yg): can this clone be removed?
            inner_value: expr_cells_iter.cloned().collect(),
            enum_size,
        })
    }

    fn to_reference_expression(self) -> ReferenceExpression {
        let num_padding = self.enum_size - 1 - self.inner_value.len();
        ReferenceExpression {
            cells: chain!(
                // Variant selector
                vec![self.variant_selector].into_iter(),
                // actual value's cells
                self.inner_value.into_iter(),
                // Padding
                repeat_n(CellExpression::Padding, num_padding)
            )
            .collect(),
        }
    }
}

/// Gets the size of the given concrete enum type.
fn get_enum_size(
    program_info: &ProgramInfo<'_>,
    concrete_enum_type: &ConcreteTypeId,
) -> Option<usize> {
    Some(program_info.type_sizes.get(concrete_enum_type)?.to_owned())
}
