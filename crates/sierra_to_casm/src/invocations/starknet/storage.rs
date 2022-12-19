use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::operand::{BinOpOperand, DerefOrImmediate, Operation, ResOperand};
use casm::{casm, casm_build_extend, deref, deref_or_immediate};
use num_bigint::BigInt;
use num_traits::FromPrimitive;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::get_non_fallthrough_statement_id;
use crate::references::{
    try_unpack_deref, try_unpack_deref_with_offset, BinOpExpression, CellExpression,
    ReferenceExpression, ReferenceValue,
};
use crate::relocations::{Relocation, RelocationEntry};

#[cfg(test)]
#[path = "storage_test.rs"]
mod test;

/// Builds instructions for StarkNet read system call.
pub fn build_storage_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let selector = BigInt::from_bytes_le(num_bigint::Sign::Plus, "storage_read".as_bytes());

    let ((system_base, system_offset), storage_address) = match builder.refs {
        [
            ReferenceValue { expression: expr_system, .. },
            ReferenceValue { expression: expr_address, .. },
        ] => (try_unpack_deref_with_offset(expr_system)?, try_unpack_deref(expr_address)?),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };

    if system_offset > i16::MAX - 2 {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }

    let instructions = casm! {
        [ap] = selector, ap++;
        [ap - 1] = [[&system_base] + system_offset];
        storage_address = [[&system_base] + (system_offset + 1)];
        %{ syscall_handler.syscall(segments=segments, syscall_ptr=[&system_base] + system_offset) %}
        [ap] = [[&system_base] + (system_offset + 2)], ap++;
    }
    .instructions;

    let output_expressions = [vec![
        // System
        ReferenceExpression {
            cells: vec![CellExpression::BinOp(BinOpExpression {
                op: FeltBinaryOperator::Add,
                a: system_base,
                b: DerefOrImmediate::Immediate(BigInt::from_i16(system_offset).unwrap() + 3),
            })],
        },
        // Read value
        ReferenceExpression::from_cell(CellExpression::Deref(deref!([ap - 1]))),
    ]
    .into_iter()]
    .into_iter();
    Ok(builder.build(instructions, vec![], output_expressions))
}

/// Builds instructions for StarkNet write system call.
pub fn build_storage_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_le(num_bigint::Sign::Plus, "storage_write".as_bytes());

    let (gas_builtin, (system_base, system_offset), storage_address, value) = match builder.refs {
        [
            ReferenceValue { expression: expr_gas_builtin, .. },
            ReferenceValue { expression: expr_system, .. },
            ReferenceValue { expression: expr_address, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => (
            try_unpack_deref(expr_gas_builtin)?,
            try_unpack_deref_with_offset(expr_system)?,
            try_unpack_deref(expr_address)?,
            try_unpack_deref(expr_value)?,
        ),
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 4,
                actual: refs.len(),
            });
        }
    };

    if system_offset > i16::MAX - 1 {
        return Err(InvocationError::InvalidReferenceExpressionForArgument);
    }

    let mut casm_builder = CasmBuilder::default();
    let system = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: system_base,
        b: deref_or_immediate!(system_offset),
    }));
    let original_system = casm_builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: system_base,
        b: deref_or_immediate!(system_offset),
    }));
    let selector_imm = casm_builder.add_var(ResOperand::Immediate(selector_imm));
    let gas_builtin = casm_builder.add_var(ResOperand::Deref(gas_builtin));
    let storage_address = casm_builder.add_var(ResOperand::Deref(storage_address));
    let value = casm_builder.add_var(ResOperand::Deref(value));
    casm_build_extend! {casm_builder,
        alloc selector;
        assert selector = selector_imm;
        assert *(system++) = selector;
        assert *(system++) = gas_builtin;
        assert *(system++) = storage_address;
        assert *(system++) = value;
        system_call original_system;
        // TODO(yuval/orizi): Return double deref directly, without allocating a variable.
        alloc updated_gas_builtin;
        assert *(system++) = updated_gas_builtin;
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        alloc revert_reason;
        assert *(system++) = revert_reason;
        // TODO(yuval/orizi): change to "system++;" once it's supported. When supported, also change
        // the NewTempVar of revert reason output var in the signature to point to idx 0 instead of
        // None.
        alloc ignore;
        assert *(system++) = ignore;
        jump Failure if revert_reason != 0;
    };

    let CasmBuildResult { instructions, awaiting_relocations, label_state, fallthrough_state } =
        casm_builder.build();
    // TODO(orizi): Extract the assertion out of the libfunc implementation.
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [fallthrough_state.ap_change, label_state["Failure"].ap_change]
            .map(sierra_ap_change::ApChange::Known)
    );
    let [relocation_index] = &awaiting_relocations[..] else { panic!("Malformed casm builder usage.") };
    Ok(builder.build(
        instructions,
        vec![RelocationEntry {
            instruction_idx: *relocation_index,
            relocation: Relocation::RelativeStatementId(failure_handle_statement_id),
        }],
        [
            // Success branch - return (gas builtin, system)
            vec![
                ReferenceExpression::from_cell(CellExpression::Deref(
                    fallthrough_state.get_adjusted_as_cell_ref(updated_gas_builtin),
                )),
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    fallthrough_state.get_adjusted(system),
                )),
            ]
            .into_iter(),
            // Failure branch - return (gas builtin, system, revert_reason)
            vec![
                ReferenceExpression::from_cell(CellExpression::Deref(
                    label_state["Failure"].get_adjusted_as_cell_ref(updated_gas_builtin),
                )),
                ReferenceExpression::from_cell(CellExpression::from_res_operand(
                    label_state["Failure"].get_adjusted(system),
                )),
                ReferenceExpression::from_cell(CellExpression::Deref(
                    label_state["Failure"].get_adjusted_as_cell_ref(revert_reason),
                )),
            ]
            .into_iter(),
        ]
        .into_iter(),
    ))
}
