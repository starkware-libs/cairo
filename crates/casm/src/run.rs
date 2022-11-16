use std::any::Any;
use std::collections::HashMap;

use cairo_rs::hint_processor::hint_processor_definition::{HintProcessor, HintReference};
use cairo_rs::serde::deserialize_program::{
    ApTracking, FlowTrackingData, HintParams, ReferenceManager,
};
use cairo_rs::types::exec_scope::ExecutionScopes;
use cairo_rs::types::program::Program;
use cairo_rs::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_rs::vm::errors::vm_errors::VirtualMachineError;
use cairo_rs::vm::runners::cairo_runner::CairoRunner;
use cairo_rs::vm::vm_core::VirtualMachine;
use num_bigint::BigInt;

use crate::hints::Hint;
use crate::inline::CasmContext;
use crate::instructions::Instruction;
use crate::operand::{CellRef, DerefOrImmediate, Register};

#[cfg(test)]
#[path = "run_test.rs"]
mod test;

/// Returns the Starkware prime 2^251 + 17*2^192 + 1.
fn get_prime() -> BigInt {
    (BigInt::from(1) << 251) + 17 * (BigInt::from(1) << 192) + 1
}

/// Convert a Hint to the cairo-rs class HintParams by canonically serializing it to a string.
fn hint_to_hint_params(hint: &Hint) -> HintParams {
    HintParams {
        code: hint.to_string(),
        accessible_scopes: vec![],
        flow_tracking_data: FlowTrackingData {
            ap_tracking: ApTracking::new(),
            reference_ids: HashMap::new(),
        },
    }
}

/// HintProcessor for Cairo compiler hints.
struct CairoHintProcessor {
    // A dict from instruction offset to hint vector.
    pub hints_dict: HashMap<usize, Vec<HintParams>>,
    // A mapping from a string that represents a hint to the hint object.
    pub string_to_hint: HashMap<String, Hint>,
}

impl CairoHintProcessor {
    pub fn new(program: Vec<Instruction>) -> Self {
        let mut hints_dict: HashMap<usize, Vec<HintParams>> = HashMap::new();
        let mut string_to_hint: HashMap<String, Hint> = HashMap::new();

        let mut hint_offset = 0;

        for instruction in program.iter() {
            if !instruction.hints.is_empty() {
                // Register hint with string for the hint processor.
                for hint in instruction.hints.iter() {
                    string_to_hint.insert(hint.to_string(), hint.clone());
                }
                // Add hint, associated with the instruction offset.
                hints_dict.insert(
                    hint_offset,
                    instruction.hints.iter().map(hint_to_hint_params).collect(),
                );
            }
            hint_offset += instruction.body.op_size();
        }
        CairoHintProcessor { hints_dict, string_to_hint }
    }
}

fn cell_ref_to_relocatable(cell_ref: CellRef, vm: &VirtualMachine) -> Relocatable {
    let base = match cell_ref.register {
        Register::AP => vm.get_ap(),
        Register::FP => vm.get_fp(),
    };
    base + (cell_ref.offset as i32)
}

impl HintProcessor for CairoHintProcessor {
    /// Trait function to execute a given hint in the hint processor.
    fn execute_hint(
        &self,
        vm: &mut VirtualMachine,
        _exec_scopes: &mut ExecutionScopes,
        hint_data: &Box<dyn Any>,
        _constants: &HashMap<String, BigInt>,
    ) -> Result<(), VirtualMachineError> {
        let hint = hint_data.downcast_ref::<Hint>().unwrap();
        // Retrieve a value located at memory[x].
        let get_val = |x: DerefOrImmediate| -> Result<BigInt, VirtualMachineError> {
            match x {
                DerefOrImmediate::Deref(d) => {
                    Ok(vm.get_integer(&cell_ref_to_relocatable(d, vm))?.as_ref().clone())
                }
                DerefOrImmediate::Immediate(i) => Ok(i),
            }
        };
        match hint {
            Hint::AllocSegment { dst } => {
                let segment = vm.add_memory_segment();
                vm.insert_value(&cell_ref_to_relocatable(*dst, vm), segment)?;
            }
            Hint::TestLessThan { lhs, rhs, dst } => {
                let lhs_val = get_val(lhs.clone())?;
                let rhs_val = get_val(rhs.clone())?;
                vm.insert_value(
                    &cell_ref_to_relocatable(*dst, vm),
                    if lhs_val < rhs_val { BigInt::from(1) } else { BigInt::from(0) },
                )?;
            }
            Hint::DivMod { lhs, rhs, quotient, remainder } => {
                let lhs_val = get_val(lhs.clone())?;
                let rhs_val = get_val(rhs.clone())?;
                vm.insert_value(
                    &cell_ref_to_relocatable(*quotient, vm),
                    lhs_val.clone() / rhs_val.clone(),
                )?;
                vm.insert_value(&cell_ref_to_relocatable(*remainder, vm), lhs_val % rhs_val)?;
            }
            Hint::AllocDictFeltTo { .. } => todo!(),
            Hint::DictFeltToRead { .. } => todo!(),
            Hint::DictFeltToWrite { .. } => todo!(),
        };
        Ok(())
    }

    /// Trait function to store hint in the hint processor by string.
    fn compile_hint(
        &self,
        hint_code: &str,
        _ap_tracking_data: &ApTracking,
        _reference_ids: &HashMap<String, usize>,
        _references: &HashMap<usize, HintReference>,
    ) -> Result<Box<dyn Any>, VirtualMachineError> {
        Ok(Box::new(self.string_to_hint[hint_code].clone()))
    }
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

    let hint_processor = CairoHintProcessor::new(program);

    let program = Program {
        builtins: Vec::new(),
        prime: prime.clone(),
        data,
        constants: HashMap::new(),
        main: Some(0),
        start: None,
        end: None,
        hints: hint_processor.hints_dict.clone(),
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
