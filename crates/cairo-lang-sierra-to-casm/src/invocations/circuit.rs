use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_casm::operand::{CellRef, Register};
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::extensions::circuit::CircuitConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::circuit::{CircuitInfo, VALUE_SIZE};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

const BUILTIN_INSTANCE_SIZE: usize = 7;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CircuitConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CircuitConcreteLibfunc::FillInput(_libfunc) => build_fill_input(builder),
        CircuitConcreteLibfunc::Eval(libfunc) => build_circuit_eval(&libfunc.ty, builder),
        CircuitConcreteLibfunc::GetDescriptor(libfunc) => {
            build_get_descriptor(&libfunc.ty, builder)
        }
        CircuitConcreteLibfunc::InitCircuitData(libfunc) => {
            build_init_circuit_data(&libfunc.ty, builder)
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

    let CircuitInfo { mut n_inputs, values, one_needed, .. } =
        builder.program_info.circuits_info.circuits.get(circuit_ty).unwrap();

    if *one_needed {
        n_inputs -= 1;
    }
    let n_values = values.len();

    let mut casm_builder = CasmBuilder::default();

    add_input_variables! {casm_builder,
        buffer(1) rc96;
    };
    casm_build_extend! {casm_builder,
        const inputs_size = n_inputs * VALUE_SIZE;
        let inputs_end = rc96 + inputs_size;
        const values_size = n_values * VALUE_SIZE;
        let vals_end = rc96 + values_size;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[vals_end], &[rc96, inputs_end]], None)],
        Default::default(),
    ))
}

/// Handles a Sierra statement for popping an element from the beginning of an array.
fn build_fill_input(
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
        jump More if remaining != 0;
        jump Failure;
        More:
    };
    let failure_handle = get_non_fallthrough_statement_id(&builder);

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[new_start, end]], None), ("Failure", &[&[end]], Some(failure_handle))],
        Default::default(),
    ))
}

/// Builds instructions for `get_circuit_descriptor` libfunc.
fn build_get_descriptor(
    circuit_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let CircuitInfo { add_offsets, .. } =
        builder.program_info.circuits_info.circuits.get(circuit_ty).unwrap();

    let ctx = casm! {
        // The relocation will point the `call` to the `ret;` instruction that precedes the
        // relevant const.
        call rel 0;
        // The relocation table will add const offset to the `1` below, making it point to the
        // constant value (the `1` is to skip the `ret` instruction).
        // TODO(Gil): Support relocatable CellExpression and return an unstored "[ap - 1] + 1".
        [ap] = [ap - 1] + 1, ap++;
        [ap] = [ap - 1] + (add_offsets.len() * VALUE_SIZE), ap++;
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
    let [expr_add_mod, expr_mul_mod, expr_desc, expr_data, modulus_expr] =
        builder.try_get_refs()?;
    let add_mod = expr_add_mod.try_unpack_single()?;
    let mul_mod = expr_mul_mod.try_unpack_single()?;
    let [add_mod_offsets, mul_mod_offsets] = expr_desc.try_unpack()?;
    let [modulus0, modulus1, modulus2, modulus3] = modulus_expr.try_unpack()?;
    let values_end = expr_data.try_unpack_single()?;
    let mut casm_builder = CasmBuilder::default();

    add_input_variables! {casm_builder,
        buffer(7) add_mod;
        buffer(7) mul_mod;
        deref values_end;
        deref add_mod_offsets;
        deref mul_mod_offsets;

        deref modulus0;
        deref modulus1;
        deref modulus2;
        deref modulus3;
    };

    let CircuitInfo { add_offsets, mul_offsets, values, .. } =
        builder.program_info.circuits_info.circuits.get(circuit_ty).unwrap();

    casm_build_extend! {casm_builder,
        const valus_size = values.len() * VALUE_SIZE;
        tempvar values = values_end - valus_size;
    }
    if !add_offsets.is_empty() {
        casm_build_extend! {casm_builder,
            assert modulus0 = add_mod[0];
            assert modulus1 = add_mod[1];
            assert modulus2 = add_mod[2];
            assert modulus3 = add_mod[3];
            assert values = add_mod[4];
            assert add_mod_offsets = add_mod[5];
        };
    }

    if !mul_offsets.is_empty() {
        casm_build_extend! {casm_builder,
            assert modulus0 = mul_mod[0];
            assert modulus1 = mul_mod[1];
            assert modulus2 = mul_mod[2];
            assert modulus3 = mul_mod[3];
            assert values = mul_mod[4];
            assert mul_mod_offsets = mul_mod[5];
        };
    }

    casm_build_extend! {casm_builder,
        const add_mod_usage = (BUILTIN_INSTANCE_SIZE * add_offsets.len());
        let new_add_mod = add_mod + add_mod_usage;
        const mul_mod_usage = (BUILTIN_INSTANCE_SIZE * mul_offsets.len());
        let new_mul_mod = mul_mod + mul_mod_usage;
        let new_data = values;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[new_add_mod], &[new_mul_mod], &[new_data]], None)],
        Default::default(),
    ))
}
