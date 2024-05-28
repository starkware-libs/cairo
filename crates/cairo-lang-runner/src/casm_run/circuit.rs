use std::ops::Shl;

use cairo_lang_sierra::extensions::circuit::VALUE_SIZE;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::vm_core::VirtualMachine;
use num_bigint::BigUint;
use num_integer::{ExtendedGcd, Integer};
use num_traits::{One, Zero};
use starknet_types_core::felt::Felt as Felt252;

struct CircuitInstance<'a> {
    vm: &'a mut VirtualMachine,
    values_ptr: Relocatable,
    add_mod_offsets: Relocatable,
    mul_mod_offsets: Relocatable,
    modulus: BigUint,
}

impl CircuitInstance<'_> {
    /// Given an address of an offset, gives the corresponding address in the values buffer.
    fn get_value_ptr(&self, offset_addr: Relocatable) -> Relocatable {
        (self.values_ptr + self.vm.get_integer(offset_addr).unwrap().as_ref()).unwrap()
    }

    /// Reads a value from the values buffer.
    fn read_circuit_value(&mut self, offset_addr: Relocatable) -> Option<BigUint> {
        let addr = self.get_value_ptr(offset_addr);
        read_circuit_value(self.vm, addr)
    }

    /// Writes a value to the values buffer.
    fn write_circuit_value(&mut self, offset_addr: Relocatable, value: BigUint) {
        let addr = self.get_value_ptr(offset_addr);
        write_circuit_value(self.vm, addr, value);
    }

    /// Reads a values from the location specified by `index` in the `add_mod_offsets` buffer.
    fn read_addmod_value(&mut self, index: usize) -> Option<BigUint> {
        self.read_circuit_value((self.add_mod_offsets + index).unwrap())
    }

    /// Writes `value` to the location specified by `index` in the `add_mod_offsets` buffer.
    fn write_addmod_value(&mut self, index: usize, value: BigUint) {
        self.write_circuit_value((self.add_mod_offsets + index).unwrap(), value)
    }

    /// Reads a values from the location specified by `index` in the `mul_mod_offsets` buffer.
    fn get_mulmod_value(&mut self, index: usize) -> Option<BigUint> {
        self.read_circuit_value((self.mul_mod_offsets + index).unwrap())
    }

    /// Writes `value` to the location specified by `index` in the `mul_mod_offsets` buffer.
    fn write_mulmod_value(&mut self, index: usize, value: BigUint) {
        self.write_circuit_value((self.mul_mod_offsets + index).unwrap(), value)
    }

    /// Fills the values for a mul gate.
    /// returns true if the values were filled successfully, false otherwise.
    fn fill_add_gate(&mut self, index: usize) -> bool {
        let lhs = self.read_addmod_value(index);
        let rhs = self.read_addmod_value(index + 1);

        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => {
                let res = (lhs + rhs).mod_floor(&self.modulus);
                self.write_addmod_value(index + 2, res);
                true
            }
            _ => false,
        }
    }

    /// Fills the values for a mul gate.
    /// returns true if the values were filled successfully, false otherwise.
    fn fill_mul_gate(&mut self, index: usize) -> bool {
        let lhs = self.get_mulmod_value(index);
        let rhs = self.get_mulmod_value(index + 1);

        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => {
                let res = (lhs * rhs).mod_floor(&self.modulus);
                self.write_mulmod_value(index + 2, res);
                true
            }
            (None, Some(rhs)) => {
                let ExtendedGcd::<_> { gcd, x, y: _ } = rhs.extended_gcd(&self.modulus);

                // TODO(ilya): is 1 invertible module 0?
                let (success, res) =
                    if gcd.is_one() { (true, x) } else { (false, BigUint::zero()) };
                self.write_mulmod_value(index, res);
                success
            }
            _ => panic!("Unexpected None value in fill_mul_gate"),
        }
    }
}

/// Reads a circuit value from the memory.
fn read_circuit_value(vm: &mut VirtualMachine, addr: Relocatable) -> Option<BigUint> {
    let mut res = BigUint::zero();
    for i in (0..VALUE_SIZE).rev() {
        let addr_i = (addr + i).unwrap();
        match vm.get_maybe(&addr_i) {
            Some(MaybeRelocatable::Int(limb)) => res = res.shl(96) + limb.to_biguint(),
            _ => return None,
        }
    }
    Some(res)
}

/// writes a circuit value from the memory.
fn write_circuit_value(vm: &mut VirtualMachine, addr: Relocatable, mut value: BigUint) {
    for i in 0..VALUE_SIZE {
        let (new_value, rem) = value.div_rem(&BigUint::from(1_u32).shl(96));
        let addr_i = (addr + i).unwrap();
        vm.insert_value(addr_i, Felt252::from(rem)).unwrap();
        value = new_value;
    }
}

/// Fills the values for a circuit.
///
/// Returns the first mul gate index that failed to fill its values or `n_mul_mods` if all gates
/// were filled successfully.
pub fn fill_values(
    vm: &mut VirtualMachine,
    values_ptr: Relocatable,
    add_mod_offsets: Relocatable,
    n_add_mods: usize,
    mul_mod_offsets: Relocatable,
    n_mul_mods: usize,
    modulus_ptr: Relocatable,
) -> usize {
    let modulus = read_circuit_value(vm, modulus_ptr).unwrap();
    let mut c = CircuitInstance { vm, values_ptr, add_mod_offsets, mul_mod_offsets, modulus };

    let mut addmod_idx = 0;
    let mut first_failure_idx = n_mul_mods;

    for mulmod_idx in 0..n_mul_mods {
        while addmod_idx < n_add_mods {
            if !c.fill_add_gate(3 * addmod_idx) {
                break;
            }
            addmod_idx += 1;
        }

        let success = c.fill_mul_gate(3 * mulmod_idx);
        if !success && first_failure_idx == n_mul_mods {
            first_failure_idx = mulmod_idx;
        }
    }

    first_failure_idx
}
