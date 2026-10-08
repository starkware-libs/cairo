use cairo_lang_casm::builder::{CasmBuilder, Label, Var};
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::program::{BranchInfo, BranchTarget};
use cairo_lang_sierra_gas::objects::ConstCost;
use itertools::Itertools;
use num_bigint::{BigInt, ToBigInt};
use starknet_types_core::felt::Felt as Felt252;

use super::{
    CompiledInvocation, CompiledInvocationBuilder, InvocationError,
    get_non_fallthrough_statement_id,
};
use crate::invocations::{BuiltinInfo, CostValidationInfo, add_input_variables};
use crate::references::ReferenceExpression;
use crate::relocations::{InstructionsWithRelocations, Relocation, RelocationEntry};

/// Handles revoke/enable/disable AP-tracking instructions.
pub fn build_update_ap_tracking(
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

/// Handles a const single cell immediate value libfunc.
pub fn build_single_cell_const(
    builder: CompiledInvocationBuilder<'_>,
    value: BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(value))].into_iter(),
    ))
}

/// Handles a jump-non-zero statement.
/// For example, this "Sierra statement"
/// ```ignore
/// felt252_is_zero(var=[ap-10]) { fallthrough() 1000(var) };
/// ```
/// translates to these CASM instructions:
/// ```ignore
/// jmp rel <jump_offset_1000> if [ap-10] != 0
/// ```
pub fn build_is_zero(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::with_capacity(1, 1);
    add_input_variables!(casm_builder, deref value; );
    casm_build_extend! {casm_builder,
        label Target;
        jump Target if value != 0;
    };
    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(Label::FALLTHROUGH, &[], None), (Target, &[&[value]], Some(target_statement_id))],
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
    let mut casm_builder = CasmBuilder::with_capacity(1, 1);
    casm_build_extend! {casm_builder,
        label Target;
        jump Target;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(Target, &[], Some(*target_statement_id))],
        Default::default(),
    ))
}

/// Handles an operation that does no changes to the reference expressions.
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
    let mut casm_builder = CasmBuilder::with_capacity(3, 2);
    let [a, b] = builder.try_get_single_cells()?;

    add_input_variables! {casm_builder,
        deref a;
        deref_or_immediate b;
    };

    casm_build_extend!(casm_builder,
        label NotEqual, Equal;
        tempvar diff = a - b;
        // diff = a - b => (diff == 0) <==> (a == b)
        jump NotEqual if diff != 0;
        jump Equal;
        NotEqual:
    );

    let target_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(Label::FALLTHROUGH, &[], None), (Equal, &[], Some(target_statement_id))],
        Default::default(),
    ))
}

/// Adds code that validates that variable `value` is smaller than `limit`, using the algorithm
/// described in `add_limit_linear_split` with `K == 1`.
/// `auxiliary_vars` are 4 variables already allocated for the execution of the algorithm.
pub fn validate_under_limit_k1(
    casm_builder: &mut CasmBuilder,
    limit: &BigInt,
    value: Var,
    range_check: Var,
    auxiliary_vars: &[Var],
) {
    let (x, y, x_part, y_fixed) =
        auxiliary_vars.iter().cloned().collect_tuple().expect("Wrong amount of vars.");
    let b_imm_fix = add_limit_linear_split(casm_builder, limit, u128::MAX, value, x, y, x_part);
    casm_build_extend! {casm_builder,
        // x <= max_x = 2**128 - 1
        assert x = *(range_check++);
        // y < 2**128
        assert y = *(range_check++);
        // y + 2**128 - B < 2**128 ==> y < B
        assert y_fixed = y + b_imm_fix;
        assert y_fixed = *(range_check++);
    };
}

/// Adds code that validates that variable `value` is smaller than `limit`, using the algorithm
/// described in `add_limit_linear_split` with `K == 2`, jumping to `done` on success.
/// `auxiliary_vars` are 5 variables already allocated for the execution of the algorithm.
pub fn validate_under_limit_k2(
    casm_builder: &mut CasmBuilder,
    limit: &BigInt,
    value: Var,
    range_check: Var,
    auxiliary_vars: &[Var],
    done: Label,
) {
    let (x, y, x_part, y_fixed, diff) =
        auxiliary_vars.iter().cloned().collect_tuple().expect("Wrong amount of vars.");
    let b_imm_fix = add_limit_linear_split(casm_builder, limit, u128::MAX - 1, value, x, y, x_part);
    casm_build_extend! {casm_builder,
        const u128_limit_minus_1 = u128::MAX;
        // y < 2**128
        assert y = *(range_check++);
        // y + 2**128 - B < 2**128 ==> y < B
        assert y_fixed = y + b_imm_fix;
        assert y_fixed = *(range_check++);
        // x < 2**128 && x != 2**128 - 1 ==> x < 2**128 - 1
        assert x = *(range_check++);
        assert diff = x - u128_limit_minus_1;
        jump done if diff != 0;
        // As x cannot be 2**128 - 1, this is unreachable.
        fail;
    };
}

/// Adds the shared part of `validate_under_limit_k1` and `validate_under_limit_k2` - splitting
/// `value` into `x` and `y` and asserting that `value = A * x + y`.
/// Returns the variable holding the `2**128 - B` constant.
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
/// We assert that the A and B generated by the provided `K` fit the constraint.
fn add_limit_linear_split(
    casm_builder: &mut CasmBuilder,
    limit: &BigInt,
    max_x: u128,
    value: Var,
    x: Var,
    y: Var,
    x_part: Var,
) -> Var {
    let a_imm = limit / max_x;
    let b_imm = limit % max_x;
    assert!(a_imm <= b_imm, "Must choose `K` such that `{a_imm} (`A`) <= {b_imm} (`B`)");
    casm_build_extend! {casm_builder,
        const a_imm = a_imm;
        // 2**128 - B.
        const b_imm_fix = (BigInt::from(u128::MAX) - b_imm + 1) as BigInt;
        const max_x = max_x;
        hint LinearSplit { value, scalar: a_imm, max_x } into { x, y };
        assert x_part = x * a_imm;
        assert value = x_part + y;
    };
    b_imm_fix
}

/// Helper function to generate code that computes a pointer after the end of the program code,
/// at a given offset. This can be used for global constants.
///
/// Returns the instructions and the corresponding ap-change.
pub fn get_pointer_after_program_code(offset: i32) -> (InstructionsWithRelocations, usize) {
    let ctx = casm! {
        // The relocation table will point the `call` to the end of the program, where there will
        // be a `ret` instruction.
        call rel 0;
        // After calling an empty function, `[ap - 1]` contains the current `pc`.
        // Using the relocations below, the immediate value (`offset`) will be changed so that it
        // will compute a pointer to the second cell after the end of the program, which will
        // contain the pointer to the builtin cost array.
        [ap] = [ap - 1] + (offset), ap++;
    };
    let relocations = vec![
        RelocationEntry { instruction_idx: 0, relocation: Relocation::EndOfProgram },
        RelocationEntry { instruction_idx: 1, relocation: Relocation::EndOfProgram },
    ];

    (
        InstructionsWithRelocations {
            instructions: ctx.instructions,
            relocations,
            cost: ConstCost { steps: 3, ..Default::default() },
        },
        3,
    )
}

/// Builds a libfunc that tries to convert a felt252 to a type with values in the range
/// `[0, 2**num_bits)`.
/// Assumption: num_bits > 128.
pub fn build_unsigned_try_from_felt252(
    builder: CompiledInvocationBuilder<'_>,
    num_bits: usize,
) -> Result<CompiledInvocation, InvocationError> {
    let val_bound: BigInt = BigInt::from(1) << num_bits;
    let [range_check, value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::with_capacity(15, 2);
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref value;
    };
    let auxiliary_vars: [_; 4] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    casm_build_extend! {casm_builder,
        label IsValidValue;
        const limit = val_bound.clone();
        let orig_range_check = range_check;
        tempvar is_valid_value;
        hint TestLessThan {lhs: value, rhs: limit} into {dst: is_valid_value};
        jump IsValidValue if is_valid_value != 0;
        tempvar shifted_value = value - limit;
    }
    validate_under_limit_k1(
        &mut casm_builder,
        &(Felt252::prime().to_bigint().unwrap() - val_bound.clone()),
        shifted_value,
        range_check,
        &auxiliary_vars,
    );
    casm_build_extend! {casm_builder,
        label Failure;
        jump Failure;
        IsValidValue:
    };
    validate_under_limit_k1(&mut casm_builder, &val_bound, value, range_check, &auxiliary_vars);

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            (Label::FALLTHROUGH, &[&[range_check], &[value]], None),
            (Failure, &[&[range_check]], Some(failure_handle_statement_id)),
        ],
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
