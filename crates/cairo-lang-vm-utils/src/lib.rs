#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;

use cairo_felt::Felt252;
use cairo_lang_casm::operand::{
    BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand,
};
use cairo_lang_utils::extract_matches;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use cairo_vm::vm::vm_core::VirtualMachine;

#[cfg(feature = "execute_core_hints")]
mod execute_core_hints;
#[cfg(feature = "execute_core_hints")]
pub use execute_core_hints::{execute_core_hint, execute_core_hint_base, execute_deprecated_hint};

pub fn cell_ref_to_relocatable(cell_ref: &CellRef, vm: &VirtualMachine) -> Relocatable {
    let base = match cell_ref.register {
        Register::AP => vm.get_ap(),
        Register::FP => vm.get_fp(),
    };
    (base + (cell_ref.offset as i32)).unwrap()
}

mod my_macro {
    /// Inserts a value into the vm memory cell represented by the cellref.
    #[macro_export]
    macro_rules! insert_value_to_cellref {
        ($vm:ident, $cell_ref:ident, $value:expr) => {
            $vm.insert_value($crate::cell_ref_to_relocatable($cell_ref, $vm), $value)
        };
    }
}

/// Extracts a parameter assumed to be a buffer.
pub fn extract_buffer(buffer: &ResOperand) -> (&CellRef, Felt252) {
    let (cell, base_offset) = match buffer {
        ResOperand::Deref(cell) => (cell, 0.into()),
        ResOperand::BinOp(BinOpOperand { op: Operation::Add, a, b }) => {
            (a, extract_matches!(b, DerefOrImmediate::Immediate).clone().value.into())
        }
        _ => panic!("Illegal argument for a buffer."),
    };
    (cell, base_offset)
}

/// Fetches the value of a cell from the vm.
pub fn get_cell_val(vm: &VirtualMachine, cell: &CellRef) -> Result<Felt252, VirtualMachineError> {
    Ok(vm.get_integer(cell_ref_to_relocatable(cell, vm))?.as_ref().clone())
}

/// Fetch the `MaybeRelocatable` value from an address.
pub fn get_maybe_from_addr(
    vm: &VirtualMachine,
    addr: Relocatable,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    vm.get_maybe(&addr)
        .ok_or_else(|| VirtualMachineError::InvalidMemoryValueTemporaryAddress(Box::new(addr)))
}

/// Fetches the maybe relocatable value of a cell from the vm.
pub fn get_cell_maybe(
    vm: &VirtualMachine,
    cell: &CellRef,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, cell_ref_to_relocatable(cell, vm))
}

/// Fetches the value of a cell plus an offset from the vm, useful for pointers.
pub fn get_ptr(
    vm: &VirtualMachine,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<Relocatable, VirtualMachineError> {
    Ok((vm.get_relocatable(cell_ref_to_relocatable(cell, vm))? + offset)?)
}

/// Fetches the value of a pointer described by the value at `cell` plus an offset from the vm.
pub fn get_double_deref_val(
    vm: &VirtualMachine,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<Felt252, VirtualMachineError> {
    Ok(vm.get_integer(get_ptr(vm, cell, offset)?)?.as_ref().clone())
}

/// Fetches the maybe relocatable value of a pointer described by the value at `cell` plus an offset
/// from the vm.
pub fn get_double_deref_maybe(
    vm: &VirtualMachine,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, get_ptr(vm, cell, offset)?)
}

/// Fetches the value of `res_operand` from the vm.
pub fn get_val(vm: &VirtualMachine, res_operand: &ResOperand) -> Result<Felt252, VirtualMachineError> {
    match res_operand {
        ResOperand::Deref(cell) => get_cell_val(vm, cell),
        ResOperand::DoubleDeref(cell, offset) => get_double_deref_val(vm, cell, &(*offset).into()),
        ResOperand::Immediate(x) => Ok(Felt252::from(x.value.clone())),
        ResOperand::BinOp(op) => {
            let a = get_cell_val(vm, &op.a)?;
            let b = match &op.b {
                DerefOrImmediate::Deref(cell) => get_cell_val(vm, cell)?,
                DerefOrImmediate::Immediate(x) => Felt252::from(x.value.clone()),
            };
            match op.op {
                Operation::Add => Ok(a + b),
                Operation::Mul => Ok(a * b),
            }
        }
    }
}

/// Fetches the maybe relocatable value of `res_operand` from the vm.
pub fn get_maybe(
    vm: &VirtualMachine,
    res_operand: &ResOperand,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    match res_operand {
        ResOperand::Deref(cell) => get_cell_maybe(vm, cell),
        ResOperand::DoubleDeref(cell, offset) => {
            get_double_deref_maybe(vm, cell, &(*offset).into())
        }
        ResOperand::Immediate(x) => Ok(Felt252::from(x.value.clone()).into()),
        ResOperand::BinOp(op) => {
            let a = get_cell_maybe(vm, &op.a)?;
            let b = match &op.b {
                DerefOrImmediate::Deref(cell) => get_cell_val(vm, cell)?,
                DerefOrImmediate::Immediate(x) => Felt252::from(x.value.clone()),
            };
            Ok(match op.op {
                Operation::Add => a.add_int(&b)?,
                Operation::Mul => match a {
                    MaybeRelocatable::RelocatableValue(_) => {
                        panic!("mul not implemented for relocatable values")
                    }
                    MaybeRelocatable::Int(a) => (a * b).into(),
                },
            })
        }
    }
}
