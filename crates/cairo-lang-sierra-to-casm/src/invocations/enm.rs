use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::CellRef;
use cairo_lang_casm::{casm, casm_build_extend, casm_extend};
use cairo_lang_sierra::extensions::enm::{EnumConcreteLibfunc, EnumInitConcreteLibfunc};
use cairo_lang_sierra::extensions::ConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::{BranchInfo, BranchTarget};
use cairo_lang_utils::try_extract_matches;
use itertools::{chain, repeat_n};
use num_bigint::BigInt;

use super::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError, ReferenceExpressionView,
};
use crate::invocations::{add_input_variables, misc, CostValidationInfo, ProgramInfo};
use crate::references::{ReferenceExpression, ReferencesError};
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra enum operations.
pub fn build(
    libfunc: &EnumConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        EnumConcreteLibfunc::Init(EnumInitConcreteLibfunc { index, n_variants, .. }) => {
            build_enum_init(builder, *index, *n_variants)
        }
        EnumConcreteLibfunc::FromBoundedInt(libfunc) => {
            build_enum_from_bounded_int(builder, libfunc.n_variants)
        }
        EnumConcreteLibfunc::Match(_) | EnumConcreteLibfunc::SnapshotMatch(_) => {
            build_enum_match(builder)
        }
    }
}

/// Handles statement for initializing an enum.
/// For example, with this setup
/// ```ignore
/// type felt252_ty = felt252;
/// type unit_ty = Tuple;
/// type Option = Enum<felt252_ty, unit_ty>;
/// libfunc init_option_some = enum_init<Option, 0>;
/// felt252_const<8>() -> (felt8);
/// ````
/// this "Sierra statement"
/// ```ignore
/// init_option_some(felt8=[ap-5]) -> (some_id);
/// ```
/// translates to these casm instructions:
/// ```ignore
/// [ap] = 0; ap++
/// [ap] = 8; ap++
/// ```
fn build_enum_init(
    builder: CompiledInvocationBuilder<'_>,
    index: usize,
    n_variants: usize,
) -> Result<CompiledInvocation, InvocationError> {
    let [expression] = builder.try_get_refs()?;
    let init_arg_cells = &expression.cells;
    let variant_selector = get_variant_selector(n_variants, index)?;

    let variant_size = builder
        .program_info
        .type_sizes
        .get(&builder.libfunc.param_signatures()[0].ty)
        .ok_or(InvocationError::UnknownTypeData)?
        .to_owned();
    if init_arg_cells.len() != variant_size as usize {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }
    // Pad the variant to match the size of the largest variant
    let concrete_enum_type = &builder.libfunc.output_types()[0][0];
    let enum_size = get_enum_size(&builder.program_info, concrete_enum_type)
        .ok_or(InvocationError::UnknownTypeData)?;
    let num_padding = enum_size - 1 - variant_size;
    let inner_value = chain!(
        repeat_n(CellExpression::Immediate(BigInt::from(0)), num_padding as usize),
        init_arg_cells.clone(),
    )
    .collect();

    let enum_val = EnumView {
        variant_selector: CellExpression::Immediate(BigInt::from(variant_selector)),
        inner_value,
    };
    let output_expressions = [enum_val.to_reference_expression()].into_iter();
    Ok(builder.build_only_reference_changes(output_expressions))
}

/// Returns the variant selector for variant `index` out of `n_variants`.
pub fn get_variant_selector(n_variants: usize, index: usize) -> Result<usize, InvocationError> {
    Ok(if n_variants <= 2 {
        // For num_branches <= 2, we use the index as the variant_selector as the `match`
        // implementation jumps to the index 0 statement on 0, and to the index 1 statement on
        // 1.
        index
    } else {
        // For num_branches > 2, the `enum_match` libfunc is implemented using a jump table. In
        // order to optimize `enum_match`, we define the variant_selector as the relevant
        // relative jump in case we match the actual variant.
        //
        // - To jump to the variant in index 0, we skip the jump table and directly jump to it. Its
        //   location is (2 * n - 1) CASM steps ahead, where n is the number of variants in this
        //   enum (2 per variant but the first variant, and 1 for the first jump with a deref
        //   operand).
        // - To jump to the variant in index k, we add "jump rel (2 * (n - k) - 1)" as the first
        //   jump is of size 1 and the rest of the jump instructions are with an immediate operand,
        //   which makes them of size 2.
        (n_variants - index).checked_mul(2).ok_or(InvocationError::IntegerOverflow)? - 1
    })
}

fn build_enum_from_bounded_int(
    builder: CompiledInvocationBuilder<'_>,
    n_variants: usize,
) -> Result<CompiledInvocation, InvocationError> {
    if n_variants <= 2 {
        return misc::build_identity(builder);
    }

    let [value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref value;
    };

    // Given the number of variants, `n`, and the index of the variant `0 <= k < n`:
    // The variant selector for enums with 3 or more variants is the relative jump to the variant
    // handle which is `2 * (n - k) - 1`.
    // `2 * (n - k) - 1 = 2*n - 2*k - 1 = 2 * (2*n - 1) / 2 - 2*k = 2 * ((2*n - 1) / 2 - k)`
    // Define `(2*n - 1) / 2` as `m` - which is known in compilation time.
    // Hence the variant selector is `2 * (m - k)` or  alternatively `-2 * (k - m)`

    let m = (Felt252::from(n_variants * 2 - 1) / Felt252::from(2)).to_bigint();
    casm_build_extend! {casm_builder,
        const m = m;
        const negative_two = -2;
        tempvar value_minus_m = value - m;
        let variant_selector = value_minus_m * negative_two;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[variant_selector]], None)],
        CostValidationInfo::default(),
    ))
}

/// Handles statement for matching an enum.
fn build_enum_match(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let concrete_enum_type = &builder.libfunc.param_signatures()[0].ty;
    let [expression] = builder.try_get_refs()?;
    let matched_var = EnumView::try_get_view(expression, &builder.program_info, concrete_enum_type)
        .map_err(|_| InvocationError::InvalidReferenceExpressionForArgument)?;
    // Verify variant_selector is of type deref. This is the case with an enum_value
    // that was validly created and then stored.
    let variant_selector =
        try_extract_matches!(matched_var.variant_selector, CellExpression::Deref)
            .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;

    let mut branch_output_sizes: Vec<usize> = Vec::new();
    for branch_outputs in &builder.libfunc.output_types() {
        // Each branch has a single output.
        let branch_output = &branch_outputs[0];
        let branch_output_size = builder
            .program_info
            .type_sizes
            .get(branch_output)
            .ok_or(InvocationError::UnknownTypeData)?;
        branch_output_sizes.push(*branch_output_size as usize);
    }
    let output_expressions = branch_output_sizes.into_iter().map(|size| {
        // The size of an output must be smaller than the size of `matched_var.inner_value` as the
        // size of inner_value is fixed and is calculated as the max of the sizes of all the
        // variants (which are the outputs in all the branches). Thus it is guaranteed that the
        // iter we generate here is of size `size` (and not less).
        let padding_size = matched_var.inner_value.len() - size;
        vec![ReferenceExpression {
            cells: matched_var.inner_value.iter().skip(padding_size).cloned().collect(),
        }]
        .into_iter()
    });

    let num_branches = builder.invocation.branches.len();
    if num_branches <= 2 {
        build_enum_match_short(builder, variant_selector, output_expressions)
    } else {
        build_enum_match_long(builder, variant_selector, output_expressions)
    }
}

/// Handles statement for matching an enum with 1 or 2 variants.
/// For example, with this setup
/// ```ignore
/// type felt252_ty = felt252;
/// type unit_ty = Tuple;
/// type Option = Enum<felt252_ty, unit_ty>;
/// libfunc init_option_some = enum_init<Option, 0>;
/// libfunc match_option = enum_match<Option>;
/// felt252_const<8>() -> (felt8);
/// init_option_some(felt8=[ap-5]) -> (enum_var);
/// ````
/// this "Sierra statement" (2-variants-enum)
/// ```ignore
/// match_option(enum_var=[ap-10]) {fallthrough(some=[ap-9]), 2000(none=[ap-9])};
/// ```
/// translates to these casm instructions:
/// ```ignore
/// jmp rel <jump_offset_2000> if [ap-10] != 0
/// jmp rel <jump_offset_fallthrough>
/// ```
/// Or this "Sierra statement" (single-variant-enum)
/// ```ignore
/// match_option(enum_var=[ap-10]) {fallthrough(var=[ap-9])};
/// ```
/// translates to 0 casm instructions.
///
/// Assumes that builder.invocation.branches.len() == output_expressions.len() and that
/// builder.invocation.branches.len() <= 2.
fn build_enum_match_short(
    builder: CompiledInvocationBuilder<'_>,
    variant_selector: CellRef,
    output_expressions: impl ExactSizeIterator<
        Item = impl ExactSizeIterator<Item = ReferenceExpression>,
    >,
) -> Result<CompiledInvocation, InvocationError> {
    let mut instructions = Vec::new();
    let mut relocations = Vec::new();

    // First branch is fallthrough. If there is only one branch, this `match` statement is
    // translated to nothing in Casm.

    // If we have 2 branches, add the jump_nz instruction to branch 1 if variant_selector != 0.
    if let Some(branch) = builder.invocation.branches.get(1) {
        let statement_id = match branch {
            BranchInfo { target: BranchTarget::Statement(statement_id), .. } => *statement_id,
            _ => panic!("malformed invocation"),
        };

        instructions.extend(casm! { jmp rel 0 if variant_selector != 0; }.instructions);
        relocations.push(RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(statement_id),
        });
    }

    Ok(builder.build(instructions, relocations, output_expressions))
}

/// Handles statement for matching an enum with 3+ variants.
/// For example, with this setup
/// ```ignore
/// type felt252_ty = felt252;
/// type Positivity = Enum<felt252_ty, felt252_ty, felt252_ty>;
/// libfunc init_positive = enum_init<Positivity, 0>;
/// libfunc match_positivity = enum_match<Positivity>;
/// felt252_const<8>() -> (felt8);
/// init_positive(felt8=[ap-5]) -> (enum_var);
/// ````
/// this "Sierra statement" (3-variants-enum)
/// ```ignore
/// match_positivity(enum_var=[ap-10]) {fallthrough(pos=[ap-9]), 2000(neg=[ap-9]), 3000(zero=[ap-9])};
/// ```
/// translates to these casm instructions:
/// ```ignore
/// jmp rel [ap-10]
/// jmp rel <jump_offset_2000>
/// jmp rel <jump_offset_3000>
/// ```
/// Where in the first location of the enum_var there will be the jmp_table_idx (2*n-1 for
/// branch index 0 (where n is the number of variants of this enum), 1 for branch index 1, 3 for
/// branch index 2 and so on: (2 * k - 1) for branch index k).
///
/// Assumes that self.invocation.branches.len() == output_expressions.len() > 2.
fn build_enum_match_long(
    builder: CompiledInvocationBuilder<'_>,
    variant_selector: CellRef,
    output_expressions: impl ExactSizeIterator<
        Item = impl ExactSizeIterator<Item = ReferenceExpression>,
    >,
) -> Result<CompiledInvocation, InvocationError> {
    let target_statement_ids = builder.invocation.branches[1..].iter().map(|b| match b {
        BranchInfo { target: BranchTarget::Statement(stmnt_id), .. } => *stmnt_id,
        _ => panic!("malformed invocation"),
    });

    // The first instruction is the jmp to the relevant index in the jmp table.
    let mut ctx = casm! { jmp rel variant_selector; };
    let mut relocations = Vec::new();

    // Add a jump-table entry for all the branches but the first one (we directly jump to it from
    // the first jump above).
    for (i, stmnt_id) in target_statement_ids.rev().enumerate() {
        // Add the jump instruction to the relevant target.
        casm_extend!(ctx, jmp rel 0;);
        relocations.push(RelocationEntry {
            instruction_idx: i + 1,
            relocation: Relocation::RelativeStatementId(stmnt_id),
        });
    }

    Ok(builder.build(ctx.instructions, relocations, output_expressions))
}

/// A struct representing an actual enum value in the Sierra program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumView {
    /// This would be ReferenceExpression::Immediate after enum_init, and would be
    /// ReferenceExpression::Deref after store_*.
    pub variant_selector: CellExpression,
    /// The inner value of the enum (a flat vector of cell expressions), padded with
    /// CellExpression::Padding to match the size of the largest variant.
    pub inner_value: Vec<CellExpression>,
}

impl ReferenceExpressionView for EnumView {
    type Error = ReferencesError;

    fn try_get_view(
        expr: &ReferenceExpression,
        program_info: &ProgramInfo<'_>,
        enum_concrete_type: &ConcreteTypeId,
    ) -> Result<Self, Self::Error> {
        let enum_size = get_enum_size(program_info, enum_concrete_type)
            .ok_or(ReferencesError::InvalidReferenceTypeForArgument)?
            as usize;
        // Verify the size.
        if expr.cells.len() != enum_size {
            return Err(ReferencesError::InvalidReferenceTypeForArgument);
        }

        let mut expr_cells_iter = expr.cells.iter();
        let variant_selector =
            expr_cells_iter.next().ok_or(ReferencesError::InvalidReferenceTypeForArgument)?.clone();

        Ok(EnumView { variant_selector, inner_value: expr_cells_iter.cloned().collect() })
    }

    fn to_reference_expression(self) -> ReferenceExpression {
        ReferenceExpression {
            cells: chain!(
                // Variant selector
                vec![self.variant_selector].into_iter(),
                // actual value's cells
                self.inner_value.into_iter(),
            )
            .collect(),
        }
    }
}

/// Gets the size of the given concrete enum type.
fn get_enum_size(
    program_info: &ProgramInfo<'_>,
    concrete_enum_type: &ConcreteTypeId,
) -> Option<i16> {
    Some(program_info.type_sizes.get(concrete_enum_type)?.to_owned())
}
