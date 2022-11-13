use std::collections::HashMap;

use cairo_rs::hint_processor::builtin_hint_processor::builtin_hint_processor_definition::BuiltinHintProcessor;
use cairo_rs::serde::deserialize_program::ReferenceManager;
use cairo_rs::types::program::Program;
use cairo_rs::types::relocatable::MaybeRelocatable;
use cairo_rs::vm::errors::vm_errors::VirtualMachineError;
use cairo_rs::vm::runners::cairo_runner::CairoRunner;
use cairo_rs::vm::vm_core::VirtualMachine;
use num_bigint::BigInt;

use crate::inline::CasmContext;
use crate::instructions::Instruction;

#[cfg(test)]
#[path = "run_test.rs"]
mod test;

/// Returns the Starkware prime 2^251 + 17*2^192 + 1.
fn get_prime() -> BigInt {
    (BigInt::from(1) << 251) + 17 * (BigInt::from(1) << 192) + 1
}

/// Runs program on layout with prime, and returns the resulting VM.
#[allow(clippy::result_large_err)]
pub fn run(
    program: Vec<Instruction>,
    layout: &str,
    prime: BigInt,
) -> Result<VirtualMachine, VirtualMachineError> {
    let data: Vec<MaybeRelocatable> = program
        .iter()
        .flat_map(|inst| inst.assemble().encode())
        .map(MaybeRelocatable::from)
        .collect();

    let hint_processor = BuiltinHintProcessor::new_empty();

    let program = Program {
        builtins: Vec::new(),
        prime: prime.clone(),
        data,
        constants: HashMap::new(),
        main: Some(0),
        start: None,
        end: None,
        hints: HashMap::new(),
        reference_manager: ReferenceManager { references: Vec::new() },
        identifiers: HashMap::new(),
    };
    let mut runner = CairoRunner::new(&program, layout, false)?;
    let mut vm = VirtualMachine::new(prime, false);

    let end = runner.initialize(&mut vm)?;

    runner.run_until_pc(end, &mut vm, &hint_processor)?;

    Ok(vm)
}

/// Runs `function` and returns `n_returns` return values.
pub fn run_function(function: CasmContext, n_returns: usize) -> Vec<BigInt> {
    run(function.instructions, "plain", get_prime())
        .expect("Virtual machine failed")
        .get_return_values(n_returns)
        .expect("Return memory cells not set.")
        .iter()
        .map(|ret_val| match ret_val {
            MaybeRelocatable::Int(value) => value.clone(),
            MaybeRelocatable::RelocatableValue(_) => panic!("Return value can't be relocatable."),
        })
        .collect()
}
