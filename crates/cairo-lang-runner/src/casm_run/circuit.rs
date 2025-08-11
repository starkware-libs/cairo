use std::array;
use std::ops::{Deref, Shl};

use cairo_lang_sierra::extensions::circuit::{
    MOD_BUILTIN_INSTANCE_SIZE, OFFSETS_PER_GATE, VALUE_SIZE,
};
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::vm_core::VirtualMachine;
use num_bigint::{BigInt, BigUint, ToBigInt};
use num_integer::{ExtendedGcd, Integer};
use num_traits::{One, Signed, Zero};
use starknet_types_core::felt::Felt as Felt252;

#[cfg(test)]
#[path = "circuit_test.rs"]
mod test;

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

    /// Reads a value from the location specified by `index` in the `add_mod_offsets` buffer.
    fn read_addmod_value(&mut self, index: usize) -> Option<BigUint> {
        self.read_circuit_value((self.add_mod_offsets + index).unwrap())
    }

    /// Writes `value` to the location specified by `index` in the `add_mod_offsets` buffer.
    fn write_addmod_value(&mut self, index: usize, value: BigUint) {
        self.write_circuit_value((self.add_mod_offsets + index).unwrap(), value)
    }

    /// Reads a value from the location specified by `index` in the `mul_mod_offsets` buffer.
    fn get_mulmod_value(&mut self, index: usize) -> Option<BigUint> {
        self.read_circuit_value((self.mul_mod_offsets + index).unwrap())
    }

    /// Writes `value` to the location specified by `index` in the `mul_mod_offsets` buffer.
    fn write_mulmod_value(&mut self, index: usize, value: BigUint) {
        self.write_circuit_value((self.mul_mod_offsets + index).unwrap(), value)
    }

    /// Fills the values for an add mod gate.
    /// Returns true if all the inputs are ready and the values were filled successfully, false
    /// otherwise.
    fn fill_add_gate(&mut self, index: usize) -> bool {
        let lhs = self.read_addmod_value(index);
        let rhs = self.read_addmod_value(index + 1);

        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => {
                let res = (lhs + rhs).mod_floor(&self.modulus);
                self.write_addmod_value(index + 2, res);
                true
            }

            (None, Some(rhs)) => {
                // lhs + rhs = res => lhs = res - rhs
                if let Some(res) = self.read_addmod_value(index + 2) {
                    self.write_addmod_value(
                        index,
                        (res + &self.modulus - rhs).mod_floor(&self.modulus),
                    );
                    true
                } else {
                    false
                }
            }

            _ => false,
        }
    }

    /// Fills the values for a mul mod gate.
    /// Assumes all the inputs are ready and the modulus is not zero or one.
    /// Returns true if the values were filled successfully, returns false if it's an inverse
    /// operation and input is not invertible.
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
                let (success, res) = invert_or_nullify(rhs, &self.modulus);
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

/// Writes a circuit value from the memory.
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

    let mut mulmod_idx = 0;
    loop {
        while addmod_idx < n_add_mods {
            if !c.fill_add_gate(3 * addmod_idx) {
                break;
            }
            addmod_idx += 1;
        }

        if mulmod_idx == n_mul_mods {
            break;
        }

        let success = c.fill_mul_gate(3 * mulmod_idx);
        if !success && first_failure_idx == n_mul_mods {
            first_failure_idx = mulmod_idx;
        }
        mulmod_idx += 1;
    }

    first_failure_idx
}

/// Returns (value % modulus).
fn positive_modulus(value: &BigInt, modulus: &BigUint) -> BigUint {
    let value_mod = value.magnitude().mod_floor(modulus);
    if value.is_negative() { modulus - value_mod } else { value_mod }
}

/// Given a value and a modulus, either finds its inverse or finds a non-zero value that nullifies
/// the value.
///
/// If the value is invertible, returns (true, inverse), otherwise returns (false, nullifier).
fn invert_or_nullify(value: BigUint, modulus: &BigUint) -> (bool, BigUint) {
    let ExtendedGcd::<_> { gcd, x, y: _ } =
        value.to_bigint().unwrap().extended_gcd(&modulus.to_bigint().unwrap());

    let gcd = gcd.to_biguint().unwrap();
    if gcd.is_one() {
        return (true, positive_modulus(&x, modulus));
    }
    let nullifier = modulus / gcd;
    // Note that gcd divides the value, so value * nullifier = value * (modulus / gcd) =
    // (value // gcd) * modulus = 0 (mod modulus)
    (false, nullifier)
}

/// Fills the instances of a mod builtin.
pub fn fill_instances(
    vm: &mut VirtualMachine,
    builtin_ptr: Relocatable,
    n_instances: usize,
    modulus: [Felt252; VALUE_SIZE],
    values_ptr: Relocatable,
    mut offsets_ptr: Relocatable,
) -> Result<(), HintError> {
    for i in 0..n_instances {
        let instance_ptr = (builtin_ptr + i * MOD_BUILTIN_INSTANCE_SIZE)?;

        for (idx, value) in modulus.iter().enumerate() {
            vm.insert_value((instance_ptr + idx)?, *value)?;
        }

        vm.insert_value((instance_ptr + 4)?, values_ptr)?;
        vm.insert_value((instance_ptr + 5)?, offsets_ptr)?;
        offsets_ptr += OFFSETS_PER_GATE;
        vm.insert_value((instance_ptr + 6)?, n_instances - i)?;
    }

    Ok(())
}

/// Evaluates a circuit and fills the builtin instances and the values buffer.
///
/// Returns the first mul gate index that failed to fill its values or `n_mul_mods` if all gates
/// were filled successfully.
pub fn eval_circuit(
    vm: &mut VirtualMachine,
    add_mod_builtin: Relocatable,
    n_add_mods: usize,
    mul_mod_builtin: Relocatable,
    n_mul_mods: usize,
) -> Result<(), HintError> {
    let modulus_ptr = mul_mod_builtin;
    // The offset of the values pointer inside an instance of the builtins.
    let values_offset = 4;
    // The offset of the offsets pointer inside an instance of the builtins.
    let offsets_offset = 5;
    let values_ptr = vm.get_relocatable((mul_mod_builtin + values_offset)?)?;

    let mul_mod_offsets = vm.get_relocatable((mul_mod_builtin + offsets_offset)?)?;
    let add_mod_offsets = if n_add_mods == 0 {
        mul_mod_offsets
    } else {
        vm.get_relocatable((add_mod_builtin + offsets_offset)?)?
    };
    let n_computed_gates = fill_values(
        vm,
        values_ptr,
        add_mod_offsets,
        n_add_mods,
        mul_mod_offsets,
        n_mul_mods,
        modulus_ptr,
    );

    let modulus: [Felt252; 4] =
        array::from_fn(|i| *vm.get_integer((modulus_ptr + i).unwrap()).unwrap().deref());
    fill_instances(vm, add_mod_builtin, n_add_mods, modulus, values_ptr, add_mod_offsets)?;
    fill_instances(vm, mul_mod_builtin, n_computed_gates, modulus, values_ptr, mul_mod_offsets)?;
    Ok(())
}
