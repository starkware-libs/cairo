use cairo_rs::types::relocatable::MaybeRelocatable;
use cairo_rs::vm::errors::vm_errors::VirtualMachineError;
use cairo_rs::vm::vm_core::VirtualMachine;
use num_bigint::BigInt;

use crate::instructions::Instruction;

#[cfg(test)]
#[path = "run_test.rs"]
mod test;

#[allow(dead_code)]
fn get_prime() -> BigInt {
    (BigInt::from(1) << 251) + 17 * (BigInt::from(1) << 192) + 1
}

// A mod function that doesn't preserve sign.
fn mod_prime(n: BigInt) -> BigInt {
    let prime = get_prime();
    ((n % &prime) + &prime) % prime
}

#[allow(clippy::result_large_err)]
pub fn run(
    program: Vec<Instruction>,
    prime: BigInt,
    n_steps: usize,
) -> Result<VirtualMachine, VirtualMachineError> {
    // Encode program instructions to integers.
    let data: Vec<BigInt> = program.iter().flat_map(|inst| inst.assemble().encode()).collect();
    let mut vm = VirtualMachine::new(prime, false);

    // Define the program and execution segments.
    let program_base = vm.add_memory_segment();
    let execution_base = vm.add_memory_segment();

    // Set the initial PC to be at the start of the program, and load the program data.
    vm.set_pc(program_base.clone());
    vm.load_data(
        &MaybeRelocatable::from(program_base),
        data.iter().map(|x| MaybeRelocatable::Int(mod_prime(x.clone()))).collect(),
    )?;

    // Define the initial stack for the program (currently a fake return address), write it at the
    // beginning of the execution segment and set AP and FP to point to the next memory cell.
    let stack = vec![MaybeRelocatable::Int(BigInt::from(0))];
    vm.set_ap(stack.len());
    vm.set_fp(stack.len());
    vm.load_data(&MaybeRelocatable::from(execution_base), stack)?;

    // Perform n instruction steps of the VM.
    for _ in 0..n_steps {
        vm.step_instruction()?;
    }
    Ok(vm)
}
