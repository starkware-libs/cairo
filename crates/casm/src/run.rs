use cairo_rs::types::relocatable::MaybeRelocatable;
use cairo_rs::vm::vm_core::VirtualMachine;
use num_bigint::BigInt;

use crate::instructions::Instruction;

#[cfg(test)]
#[path = "run_test.rs"]
mod test;

/// Returns the Starkware prime 2^251 + 17*2^192 + 1.
fn get_prime() -> BigInt {
    (BigInt::from(1) << 251) + 17 * (BigInt::from(1) << 192) + 1
}

/// A mod function that doesn't preserve sign.
fn mod_prime(n: BigInt) -> BigInt {
    let prime = get_prime();
    ((n % &prime) + &prime) % prime
}

/// Takes a vector of casm instructions and runs them on the Lambdaclass VM for n_steps.
pub fn run(program: Vec<Instruction>, prime: BigInt, n_steps: usize) -> VirtualMachine {
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
    )
    .expect("VM failed to load program data.");

    // Define the initial stack for the program (currently a fake return address), write it at the
    // beginning of the execution segment and set AP and FP to point to the next memory cell.
    let stack = vec![MaybeRelocatable::Int(BigInt::from(0))];
    vm.set_ap(stack.len());
    vm.set_fp(stack.len());
    vm.load_data(&MaybeRelocatable::from(execution_base), stack)
        .expect("VM failed to load program data.");

    // Perform n instruction steps of the VM.
    for _ in 0..n_steps {
        vm.step_instruction().expect("VM failed to run instruction.");
    }
    vm
}
