use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::extensions::circuit::{
    CircuitConcreteLibfunc, CircuitInfo, MOD_BUILTIN_INSTANCE_SIZE, OFFSETS_PER_GATE, VALUE_SIZE,
};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_utils::casts::IntoOrPanic;
use itertools::chain;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    BuiltinInfo, CostValidationInfo, add_input_variables, get_non_fallthrough_statement_id,
};
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CircuitConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CircuitConcreteLibfunc::TryIntoCircuitModulus(_libfunc) => {
            build_try_into_circuit_modulus(builder)
        }
        CircuitConcreteLibfunc::AddInput(_libfunc) => build_add_input(builder),
        CircuitConcreteLibfunc::Eval(libfunc) => build_circuit_eval(&libfunc.ty, builder),
        CircuitConcreteLibfunc::GetDescriptor(libfunc) => {
            build_get_descriptor(&libfunc.ty, builder)
        }
        CircuitConcreteLibfunc::GetOutput(libfunc) => {
            build_get_output(&libfunc.circuit_ty, &libfunc.output_ty, builder)
        }
        CircuitConcreteLibfunc::InitCircuitData(libfunc) => {
            build_init_circuit_data(&libfunc.ty, builder)
        }
        CircuitConcreteLibfunc::FailureGuaranteeVerify(_) => {
            build_failure_guarantee_verify(builder)
        }
        CircuitConcreteLibfunc::IntoU96Guarantee(_) => build_identity(builder),
        CircuitConcreteLibfunc::U96GuaranteeVerify(_) => build_u96_guarantee_verify(builder),
        CircuitConcreteLibfunc::U96LimbsLessThanGuaranteeVerify(libfunc) => {
            build_u96_limbs_less_than_guarantee_verify(libfunc.limb_count, builder)
        }
        CircuitConcreteLibfunc::U96SingleLimbLessThanGuaranteeVerify(_) => {
            build_u96_single_limb_less_than_guarantee_verify(builder)
        }
    }
}

/// Handles a Sierra statement for initializing circuit data.
fn build_init_circuit_data(
    circuit_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_rc96] = builder.try_get_refs()?;
    let rc96 = expr_rc96.try_unpack_single()?;

    let circ_info = builder.program_info.circuits_info.circuits.get(circuit_ty).unwrap();
    let n_inputs = circ_info.n_inputs;
    let rc96_usage = circ_info.rc96_usage();

    let mut casm_builder = CasmBuilder::default();

    add_input_variables! {casm_builder,
        buffer(1) rc96;
    };
    casm_build_extend! {casm_builder,
        let rc96_start = rc96;
        // Skip the constant `1`.
        const input_start_offset = VALUE_SIZE;
        let input_start = rc96 + input_start_offset;
        // This size of all the inputs including the input 1.
        const input_end_offset = (1 + n_inputs) * VALUE_SIZE;
        let input_end = rc96 + input_end_offset;
        const rc96_usage = rc96_usage;
        let rc96 = rc96 + rc96_usage;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[rc96], &[input_start, input_end]], None)],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck96,
                start: rc96_start,
                end: rc96,
            }],
            extra_costs: None,
        },
    ))
}

/// Handles a Sierra statement for adding an input to the input accumulator.
fn build_add_input(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_handle, elem] = builder.try_get_refs()?;
    let [start, end] = expr_handle.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(elem.cells.len() as i16) start;
        deref end;
    };
    for cell in &elem.cells {
        add_input_variables!(casm_builder, deref cell;);
        casm_build_extend!(casm_builder, assert cell = *(start++););
    }

    casm_build_extend! {casm_builder,
        tempvar new_start = start;
        tempvar remaining = end - new_start;
        jump MoreInputs if remaining != 0;
        Done:
    };
    let more_inputs_handle = get_non_fallthrough_statement_id(&builder);

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[end]], None),
            ("MoreInputs", &[&[new_start, end]], Some(more_inputs_handle)),
        ],
        Default::default(),
    ))
}

/// Builds instructions for `get_circuit_descriptor` libfunc.
fn build_get_descriptor(
    circuit_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let CircuitInfo { add_offsets, mul_offsets, .. } =
        builder.program_info.circuits_info.circuits.get(circuit_ty).unwrap();

    let ctx = casm! {
        // The relocation will point the `call` to the `ret;` instruction that precedes the
        // add and mul tables.
        call rel 0;
        // The relocation table will add const offset to the `1` below, making it point to the
        // add and mul tables (the `1` is to skip the `ret` instruction).
        [ap] = [ap - 1] + 1, ap++;
        [ap] = (add_offsets.len()), ap++;
        [ap] = [ap - 2] + (add_offsets.len() * OFFSETS_PER_GATE), ap++;
        [ap] = (mul_offsets.len()), ap++;
    };

    let relocations = vec![
        RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::CircuitStart(circuit_ty.clone()),
        },
        RelocationEntry {
            instruction_idx: 1,
            relocation: Relocation::CircuitStart(circuit_ty.clone()),
        },
    ];
    Ok(builder.build(
        ctx.instructions,
        relocations,
        [vec![ReferenceExpression {
            cells: vec![
                CellExpression::Deref(CellRef { register: Register::AP, offset: -4 }),
                CellExpression::Deref(CellRef { register: Register::AP, offset: -3 }),
                CellExpression::Deref(CellRef { register: Register::AP, offset: -2 }),
                CellExpression::Deref(CellRef { register: Register::AP, offset: -1 }),
            ],
        }]
        .into_iter()]
        .into_iter(),
    ))
}

/// Builds instructions for `circuit_eval` libfunc.
fn build_circuit_eval(
    circuit_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_add_mod, expr_mul_mod, expr_desc, expr_data, modulus_expr, expr_zero, expr_one] =
        builder.try_get_refs()?;
    let add_mod = expr_add_mod.try_unpack_single()?;
    let mul_mod = expr_mul_mod.try_unpack_single()?;
    let [add_mod_offsets, n_adds, mul_mod_offsets, n_muls] = expr_desc.try_unpack()?;
    let [modulus0, modulus1, modulus2, modulus3] = modulus_expr.try_unpack()?;
    let inputs_end = expr_data.try_unpack_single()?;

    let zero = expr_zero.try_unpack_single()?;
    let one = expr_one.try_unpack_single()?;
    let mut casm_builder = CasmBuilder::default();

    let instance_size = MOD_BUILTIN_INSTANCE_SIZE.into_or_panic();
    add_input_variables! {casm_builder,
        buffer(instance_size) add_mod;
        buffer(instance_size) mul_mod;
        buffer(VALUE_SIZE.into_or_panic()) inputs_end;
        deref add_mod_offsets;
        deref n_adds;
        deref mul_mod_offsets;
        deref n_muls;

        deref modulus0;
        deref modulus1;
        deref modulus2;
        deref modulus3;

        deref zero;
        deref one;
    };

    let CircuitInfo { add_offsets, n_inputs, .. } =
        builder.program_info.circuits_info.circuits.get(circuit_ty).unwrap();

    casm_build_extend! {casm_builder,
        // input size including the input 1.
        const inputs_size = (1 + n_inputs) * VALUE_SIZE;
        tempvar values = inputs_end - inputs_size;

        // Add the constant `1` at the beginning of the values' segment.
        assert one = values[0];
        assert zero = values[1];
        assert zero = values[2];
        assert zero = values[3];
    };

    if !add_offsets.is_empty() {
        casm_build_extend! {casm_builder,
            assert modulus0 = add_mod[0];
            assert modulus1 = add_mod[1];
            assert modulus2 = add_mod[2];
            assert modulus3 = add_mod[3];
            assert values = add_mod[4];
            assert add_mod_offsets = add_mod[5];
            assert n_adds = add_mod[6];
        }
    }

    casm_build_extend! {casm_builder,
        assert modulus0 = mul_mod[0];
        assert modulus1 = mul_mod[1];
        assert modulus2 = mul_mod[2];
        assert modulus3 = mul_mod[3];
        assert values = mul_mod[4];
        assert mul_mod_offsets = mul_mod[5];
        hint EvalCircuit {
            n_add_mods: n_adds, add_mod_builtin: add_mod,
            n_mul_mods: n_muls, mul_mod_builtin: mul_mod
        };
        // Read the number of mul gates that were actually computed by the hint from the builtin.
        tempvar actual_mul_gates = mul_mod[6];

        const add_mod_usage = (MOD_BUILTIN_INSTANCE_SIZE * add_offsets.len());
        let new_add_mod = add_mod + add_mod_usage;

        const instance_size = MOD_BUILTIN_INSTANCE_SIZE;
        tempvar mul_mod_usage = actual_mul_gates * instance_size;
        let new_mul_mod = mul_mod + mul_mod_usage;

        // Compute the number of mul gates that were not evaluated.
        tempvar skipped_mul_gates = n_muls - actual_mul_gates;
        jump Failure if skipped_mul_gates != 0;
    };

    let failure_handle = get_non_fallthrough_statement_id(&builder);

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            // Success.
            (
                "Fallthrough",
                &[
                    &[new_add_mod],
                    &[new_mul_mod],
                    &[values, modulus0, modulus1, modulus2, modulus3],
                ],
                None,
            ),
            (
                "Failure",
                &[
                    &[new_add_mod],
                    &[new_mul_mod],
                    // CircuitPartialOutputs
                    &[values, modulus0, modulus1, modulus2, modulus3, actual_mul_gates],
                    // CircuitFailureGuarantee
                    &[
                        mul_mod_offsets,
                        n_muls,
                        actual_mul_gates,
                        values,
                        modulus0,
                        modulus1,
                        modulus2,
                        modulus3,
                    ],
                ],
                Some(failure_handle),
            ),
        ],
        Default::default(),
    ))
}

/// Generates casm instructions for `try_into_circuit_modulus()`.
fn build_try_into_circuit_modulus(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [l0, l1, l2, l3] = builder.try_get_refs::<1>()?[0].try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables!(casm_builder, deref l0; deref l1; deref l2; deref l3;);
    casm_build_extend! {casm_builder,
        const one = 1;
        tempvar l0_minus_one;
        jump Success if l3 != 0;
        jump Success if l2 != 0;
        jump Success if l1 != 0;
        jump CheckNotOne if l0 != 0;
        jump Failure;
        CheckNotOne:
        assert l0_minus_one = l0 - one;
        jump Success if l0_minus_one != 0;
        jump Failure;
        Success:
    };

    let failure_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[l0, l1, l2, l3]], None), ("Failure", &[], Some(failure_statement_id))],
        Default::default(),
    ))
}

/// Builds instructions for `circuit_failure_guarantee_verify` libfunc.
fn build_failure_guarantee_verify(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_rc96, expr_mul_mod, expr_guarantee, expr_zero, expr_one] = builder.try_get_refs()?;
    let rc96 = expr_rc96.try_unpack_single()?;

    let mul_mod = expr_mul_mod.try_unpack_single()?;
    let [orig_mul_mod_offsets, n_muls, fail_idx, values, modulus0, modulus1, modulus2, modulus3] =
        expr_guarantee.try_unpack()?;

    let zero = expr_zero.try_unpack_single()?;
    let one = expr_one.try_unpack_single()?;

    let mut casm_builder = CasmBuilder::default();
    let rc_usage = (2 + VALUE_SIZE).into_or_panic();

    let instance_size = MOD_BUILTIN_INSTANCE_SIZE.into_or_panic();
    add_input_variables! {casm_builder,
        buffer(rc_usage) rc96;
        buffer(instance_size) mul_mod;
        deref orig_mul_mod_offsets;

        deref modulus0;
        deref modulus1;
        deref modulus2;
        deref modulus3;

        deref values;
        deref zero;
        deref one;
        deref n_muls;
        deref fail_idx;
    };

    casm_build_extend! {casm_builder,
        let rc96_start = rc96;
        const offsets_per_gate = OFFSETS_PER_GATE;
        tempvar failing_gate_offset = fail_idx * offsets_per_gate;
        tempvar failing_gate_ptr = orig_mul_mod_offsets + failing_gate_offset;

        // The output of the failing gate points to the const one, at offset zero.
        // This implies that the failing gate is an inverse gate.
        assert zero = failing_gate_ptr[2];

        tempvar inv_input_offset = failing_gate_ptr[1];  // RHS (the input of the inverse).
        tempvar nullifier_offset = failing_gate_ptr[0];  // LHS (the output of the inverse).

        tempvar zero_offset = rc96 - values;
        // Write the value 0 to rc96.
        assert zero = *(rc96++);
        assert zero = *(rc96++);
        assert zero = *(rc96++);
        assert zero = *(rc96++);

        // Create a circuit that checks that
        //   `values[inv_input_offset] * values[nullifier_offset] = 0 (mod modulus)`.
        tempvar mul_mod_offsets;
        hint AllocSegment into {dst: mul_mod_offsets};

        assert nullifier_offset = mul_mod_offsets[0];
        assert inv_input_offset = mul_mod_offsets[1];
        assert zero_offset = mul_mod_offsets[2];

        // Check that 0 <= fail_idx <= n_muls;
        // Note that since we are in the failure case we know that fail_idx != n_muls.
        assert fail_idx = *(rc96++);
        tempvar diff = n_muls - fail_idx;
        assert diff = *(rc96++);

        assert modulus0 = mul_mod[0];
        assert modulus1 = mul_mod[1];
        assert modulus2 = mul_mod[2];
        assert modulus3 = mul_mod[3];
        assert values = mul_mod[4];
        assert mul_mod_offsets = mul_mod[5];
        assert one = mul_mod[6];

        // Verify that nullifier > 0.
        // We return a guarantee that checks that nullifier < modulus.
        // This implies:
        //   0 < nullifier < modulus,
        //   inv_input * nullifier = 0 (mod modulus).
        // Which implies that inv_input is not invertible.
        tempvar nullifier_ptr = values + nullifier_offset;
        tempvar nullifier0 = nullifier_ptr[0];
        tempvar nullifier1 = nullifier_ptr[1];
        tempvar nullifier2 = nullifier_ptr[2];
        tempvar nullifier3 = nullifier_ptr[3];
        jump Done if nullifier0 != 0;
        jump Done if nullifier1 != 0;
        jump Done if nullifier2 != 0;
        jump Done if nullifier3 != 0;

        // If the nullifier is zero, add an unsatisfiable constraint.
        unsatisfiable_assert one = zero;

        Done:
        const mul_mod_usage = MOD_BUILTIN_INSTANCE_SIZE;
        let new_mul_mod = mul_mod + mul_mod_usage;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(
            "Fallthrough",
            &[
                &[rc96],
                &[new_mul_mod],
                &[
                    nullifier0, nullifier1, nullifier2, nullifier3, modulus0, modulus1, modulus2,
                    modulus3,
                ],
            ],
            None,
        )],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck96,
                start: rc96_start,
                end: rc96,
            }],
            extra_costs: None,
        },
    ))
}

/// Builds instructions for `get_output` libfunc.
fn build_get_output(
    circuit_ty: &ConcreteTypeId,
    output_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_outputs] = builder.try_get_refs()?;
    let [values_ptr, modulus0, modulus1, modulus2, modulus3] = expr_outputs.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();

    let CircuitInfo { values, .. } =
        builder.program_info.circuits_info.circuits.get(circuit_ty).unwrap();

    let Some(output_offset) = values.get(output_ty) else {
        return Err(InvocationError::InvalidCircuitOutput {
            output_ty: output_ty.clone(),
            circuit_ty: circuit_ty.clone(),
        });
    };

    add_input_variables! {casm_builder,
        deref values_ptr;

        deref modulus0;
        deref modulus1;
        deref modulus2;
        deref modulus3;
    };

    casm_build_extend! {casm_builder,
        const output_offset = output_offset * VALUE_SIZE;
        // We compute output_ptr instead of using an offset to overcome the 15 bit offset limit.
        tempvar output_ptr = values_ptr + output_offset;

        tempvar output0 = output_ptr[0];
        tempvar output1 = output_ptr[1];
        tempvar output2 = output_ptr[2];
        tempvar output3 = output_ptr[3];
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [(
            "Fallthrough",
            &[
                &[output0, output1, output2, output3],
                &[output0, output1, output2, output3, modulus0, modulus1, modulus2, modulus3],
            ],
            None,
        )],
        Default::default(),
    ))
}

/// Builds instructions for `u96_guarantee_verify` libfunc.
fn build_u96_guarantee_verify(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [rc96, guarantee] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) rc96;
        deref guarantee;
    };
    casm_build_extend! {casm_builder,
        let rc96_start = rc96;
        assert guarantee = *(rc96++);
    }
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[rc96]], None)],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck96,
                start: rc96_start,
                end: rc96,
            }],
            extra_costs: None,
        },
    ))
}

/// Builds instructions for `u96_limbs_less_than_guarantee_verify` libfunc.
fn build_u96_limbs_less_than_guarantee_verify(
    limb_count: usize,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_guarantee] = builder.try_get_refs()?;
    let guarantee = &expr_guarantee.cells;
    assert_eq!(guarantee.len(), limb_count * 2);
    let mut casm_builder = CasmBuilder::default();
    let lhs_high_limb_idx = limb_count - 1;
    let rhs_high_limb_idx = 2 * limb_count - 1;
    let lhs_high_limb = casm_builder.add_var(guarantee[lhs_high_limb_idx].clone());
    let rhs_high_limb = casm_builder.add_var(guarantee[rhs_high_limb_idx].clone());
    let next_guarantee: Vec<_> =
        chain!(&guarantee[..lhs_high_limb_idx], &guarantee[limb_count..rhs_high_limb_idx])
            .map(|cell| casm_builder.add_var(cell.clone()))
            .collect();
    casm_build_extend! {casm_builder,
        tempvar diff = rhs_high_limb - lhs_high_limb;
        jump CheckLimb if diff != 0;
    };
    let check_limb_handle = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&next_guarantee], None),
            ("CheckLimb", &[&[diff]], Some(check_limb_handle)),
        ],
        Default::default(),
    ))
}

/// Builds instructions for `u96_single_limb_less_than_guarantee_verify` libfunc.
fn build_u96_single_limb_less_than_guarantee_verify(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_guarantee] = builder.try_get_refs()?;
    let [lhs, rhs] = expr_guarantee.try_unpack()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref lhs;
        deref rhs;
    };
    casm_build_extend!(casm_builder, let diff = rhs - lhs;);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[diff]], None)],
        Default::default(),
    ))
}
