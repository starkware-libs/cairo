use std::any::Any;
use std::collections::HashMap;
use std::ops::Deref;

use ark_ff::fields::{Fp256, MontBackend, MontConfig};
use ark_ff::{Field, PrimeField};
use ark_std::UniformRand;
use cairo_felt::{felt_str as felt252_str, Felt252, PRIME_STR};
use cairo_lang_casm::hints::{CoreHint, Hint, StarknetHint};
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{
    BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand,
};
use cairo_lang_utils::extract_matches;
use cairo_vm::hint_processor::hint_processor_definition::{HintProcessor, HintReference};
use cairo_vm::serde::deserialize_program::{
    ApTracking, BuiltinName, FlowTrackingData, HintParams, ReferenceManager,
};
use cairo_vm::types::exec_scope::ExecutionScopes;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use cairo_vm::vm::runners::cairo_runner::CairoRunner;
use cairo_vm::vm::vm_core::VirtualMachine;
use dict_manager::DictManagerExecScope;
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::{FromPrimitive, ToPrimitive, Zero};

use self::dict_manager::DictSquashExecScope;
use crate::short_string::as_cairo_short_string;
use crate::{Arg, RunResultValue, SierraCasmRunner};

#[cfg(test)]
mod test;

mod dict_manager;

// TODO(orizi): This def is duplicated.
/// Returns the Beta value of the Starkware elliptic curve.
fn get_beta() -> Felt252 {
    felt252_str!("3141592653589793238462643383279502884197169399375105820974944592307816406665")
}

#[derive(MontConfig)]
#[modulus = "3618502788666131213697322783095070105623107215331596699973092056135872020481"]
#[generator = "3"]
struct FqConfig;
type Fq = Fp256<MontBackend<FqConfig, 4>>;

/// Convert a Hint to the cairo-vm class HintParams by canonically serializing it to a string.
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
struct CairoHintProcessor<'a> {
    /// The Cairo runner.
    #[allow(dead_code)]
    pub runner: Option<&'a SierraCasmRunner>,
    // A dict from instruction offset to hint vector.
    pub hints_dict: HashMap<usize, Vec<HintParams>>,
    // A mapping from a string that represents a hint to the hint object.
    pub string_to_hint: HashMap<String, Hint>,
    // The starknet state.
    pub starknet_state: StarknetState,
}

impl<'a> CairoHintProcessor<'a> {
    pub fn new<'b, Instructions: Iterator<Item = &'b Instruction> + Clone>(
        runner: Option<&'a SierraCasmRunner>,
        instructions: Instructions,
        starknet_state: StarknetState,
    ) -> Self {
        let mut hints_dict: HashMap<usize, Vec<HintParams>> = HashMap::new();
        let mut string_to_hint: HashMap<String, Hint> = HashMap::new();

        let mut hint_offset = 0;

        for instruction in instructions {
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
        CairoHintProcessor { runner, hints_dict, string_to_hint, starknet_state }
    }
}

fn cell_ref_to_relocatable(cell_ref: &CellRef, vm: &VirtualMachine) -> Relocatable {
    let base = match cell_ref.register {
        Register::AP => vm.get_ap(),
        Register::FP => vm.get_fp(),
    };
    (base + (cell_ref.offset as i32)).unwrap()
}

/// Inserts a value into the vm memory cell represented by the cellref.
macro_rules! insert_value_to_cellref {
    ($vm:ident, $cell_ref:ident, $value:expr) => {
        $vm.insert_value(cell_ref_to_relocatable($cell_ref, $vm), $value)
    };
}

/// Execution scope for starknet related data.
/// All values will be 0 and by default if not setup by the test.
#[derive(Clone, Default)]
pub struct StarknetState {
    /// The values of addresses in the simulated storage per contract.
    storage: HashMap<Felt252, HashMap<Felt252, Felt252>>,
    /// A mapping from contract address to class hash.
    #[allow(dead_code)]
    deployed_contracts: HashMap<Felt252, Felt252>,
    /// The simulated execution info.
    exec_info: ExecutionInfo,
    next_id: Felt252,
}
impl StarknetState {
    pub fn get_next_id(&mut self) -> Felt252 {
        self.next_id += Felt252::from(1);
        self.next_id.clone()
    }
}

/// Copy of the cairo `ExecutionInfo` struct.
#[derive(Clone, Default)]
struct ExecutionInfo {
    block_info: BlockInfo,
    tx_info: TxInfo,
    caller_address: Felt252,
    contract_address: Felt252,
}

/// Copy of the cairo `BlockInfo` struct.
#[derive(Clone, Default)]
struct BlockInfo {
    block_number: Felt252,
    block_timestamp: Felt252,
    sequencer_address: Felt252,
}

/// Copy of the cairo `TxInfo` struct.
#[derive(Clone, Default)]
struct TxInfo {
    version: Felt252,
    account_contract_address: Felt252,
    max_fee: Felt252,
    signature: Vec<Felt252>,
    transaction_hash: Felt252,
    chain_id: Felt252,
    nonce: Felt252,
}
/// Execution scope for constant memory allocation.
struct MemoryExecScope {
    /// The first free address in the segment.
    next_address: Relocatable,
}

/// Fetches the value of a cell from the vm.
fn get_cell_val(vm: &VirtualMachine, cell: &CellRef) -> Result<Felt252, VirtualMachineError> {
    Ok(vm.get_integer(cell_ref_to_relocatable(cell, vm))?.as_ref().clone())
}

/// Fetches the value of a cell plus an offset from the vm, useful for pointers.
fn get_ptr(
    vm: &VirtualMachine,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<Relocatable, VirtualMachineError> {
    Ok((vm.get_relocatable(cell_ref_to_relocatable(cell, vm))? + offset)?)
}

/// Fetches the value of a pointer described by the value at `cell` plus an offset from the vm.
fn get_double_deref_val(
    vm: &VirtualMachine,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<Felt252, VirtualMachineError> {
    Ok(vm.get_integer(get_ptr(vm, cell, offset)?)?.as_ref().clone())
}

/// Fetches the value of `res_operand` from the vm.
fn get_val(vm: &VirtualMachine, res_operand: &ResOperand) -> Result<Felt252, VirtualMachineError> {
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

impl HintProcessor for CairoHintProcessor<'_> {
    /// Trait function to execute a given hint in the hint processor.
    fn execute_hint(
        &mut self,
        vm: &mut VirtualMachine,
        exec_scopes: &mut ExecutionScopes,
        hint_data: &Box<dyn Any>,
        _constants: &HashMap<String, Felt252>,
    ) -> Result<(), HintError> {
        let hint = hint_data.downcast_ref::<Hint>().unwrap();
        let hint = match hint {
            Hint::Core(core_hint) => {
                return execute_core_hint(vm, exec_scopes, core_hint);
            }
            Hint::Starknet(hint) => hint,
        };
        match hint {
            StarknetHint::SystemCall { system } => {
                let (cell, base_offset) = extract_buffer(system);
                let selector = get_double_deref_val(vm, cell, &base_offset)?.to_bytes_be();
                // Given `res_offset` as the offset in the system ptr where the result begins,
                // `cost` as the cost of the function and a `handler` which actually implements the
                // syscall, changes the vm status and writes the system buffer in case of success
                // and may also return a revert reason if additional checks failed. Runs the
                // simulation including gas checks and revert reasons.
                let mut check_handle_oog =
                    |res_offset: u32,
                     cost: usize,
                     handler: &mut dyn FnMut(
                        &mut VirtualMachine,
                    )
                        -> Result<Option<Felt252>, HintError>|
                     -> Result<(), HintError> {
                        let gas_counter =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 1u32))?;
                        let gas_counter_updated_ptr =
                            get_ptr(vm, cell, &(base_offset.clone() + res_offset))?;
                        let failure_flag_ptr =
                            get_ptr(vm, cell, &(base_offset.clone() + (res_offset + 1)))?;
                        let revert_reason = if gas_counter < cost.into() {
                            Felt252::from_bytes_be(b"Syscall out of gas")
                        } else if let Some(revert_reason) = handler(vm)? {
                            revert_reason
                        } else {
                            vm.insert_value(gas_counter_updated_ptr, gas_counter - cost)?;
                            vm.insert_value(failure_flag_ptr, Felt252::from(0))?;
                            return Ok(());
                        };
                        vm.insert_value(gas_counter_updated_ptr, gas_counter)?;
                        vm.insert_value(failure_flag_ptr, Felt252::from(1))?;
                        let revert_reason_start = vm.add_memory_segment();
                        vm.insert_value(revert_reason_start, revert_reason)?;
                        let revert_reason_end: Relocatable = (revert_reason_start + 1)?;
                        let revert_reason_start_ptr =
                            get_ptr(vm, cell, &(base_offset.clone() + (res_offset + 2)))?;
                        vm.insert_value(revert_reason_start_ptr, revert_reason_start)?;
                        let revert_reason_end_ptr =
                            get_ptr(vm, cell, &(base_offset.clone() + (res_offset + 3)))?;
                        vm.insert_value(revert_reason_end_ptr, revert_reason_end)?;
                        Ok(())
                    };
                if selector == "StorageWrite".as_bytes() {
                    check_handle_oog(5, 1000, &mut |vm| {
                        let addr_domain =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 2u32))?;
                        if !addr_domain.is_zero() {
                            // Only address_domain 0 is currently supported.
                            return Ok(Some(Felt252::from_bytes_be(b"Unsupported address domain")));
                        }
                        let addr = get_double_deref_val(vm, cell, &(base_offset.clone() + 3u32))?;
                        let value = get_double_deref_val(vm, cell, &(base_offset.clone() + 4u32))?;
                        let contract = self.starknet_state.exec_info.contract_address.clone();
                        self.starknet_state
                            .storage
                            .entry(contract)
                            .or_default()
                            .insert(addr, value);
                        Ok(None)
                    })?;
                } else if selector == "StorageRead".as_bytes() {
                    check_handle_oog(4, 100, &mut |vm| {
                        let addr_domain =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 2u32))?;
                        if !addr_domain.is_zero() {
                            // Only address_domain 0 is currently supported.
                            return Ok(Some(Felt252::from_bytes_be(b"Unsupported address domain")));
                        }
                        let addr = get_double_deref_val(vm, cell, &(base_offset.clone() + 3u32))?;
                        let value = self
                            .starknet_state
                            .storage
                            .get(&self.starknet_state.exec_info.contract_address)
                            .and_then(|contract_storage| contract_storage.get(&addr))
                            .cloned()
                            .unwrap_or_else(|| Felt252::from(0));
                        let result_ptr = get_ptr(vm, cell, &(base_offset.clone() + 6u32))?;
                        vm.insert_value(result_ptr, value)?;
                        Ok(None)
                    })?;
                } else if selector == "GetExecutionInfo".as_bytes() {
                    check_handle_oog(2, 50, &mut |vm| {
                        let result_ptr = get_ptr(vm, cell, &(base_offset.clone() + 4u32))?;
                        let exec_info = &self.starknet_state.exec_info;
                        let block_info = &exec_info.block_info;
                        let tx_info = &exec_info.tx_info;
                        let mut res_segment = vm.add_memory_segment();
                        let signature_start = res_segment;
                        for val in &tx_info.signature {
                            vm.insert_value(res_segment, val)?;
                            res_segment.offset += 1;
                        }
                        let signature_end = res_segment;
                        let tx_info_ptr = res_segment;
                        vm.insert_value((tx_info_ptr + 0i32)?, &tx_info.version)?;
                        vm.insert_value((tx_info_ptr + 1i32)?, &tx_info.account_contract_address)?;
                        vm.insert_value((tx_info_ptr + 2i32)?, &tx_info.max_fee)?;
                        vm.insert_value((tx_info_ptr + 3i32)?, signature_start)?;
                        vm.insert_value((tx_info_ptr + 4i32)?, signature_end)?;
                        vm.insert_value((tx_info_ptr + 5i32)?, &tx_info.transaction_hash)?;
                        vm.insert_value((tx_info_ptr + 6i32)?, &tx_info.chain_id)?;
                        vm.insert_value((tx_info_ptr + 7i32)?, &tx_info.nonce)?;
                        res_segment.offset += 8;
                        let block_info_ptr = res_segment;
                        vm.insert_value((block_info_ptr + 0i32)?, &block_info.block_number)?;
                        vm.insert_value((block_info_ptr + 1i32)?, &block_info.block_timestamp)?;
                        vm.insert_value((block_info_ptr + 2i32)?, &block_info.sequencer_address)?;
                        res_segment.offset += 3;
                        let exec_info_ptr = res_segment;
                        vm.insert_value((exec_info_ptr + 0i32)?, block_info_ptr)?;
                        vm.insert_value((exec_info_ptr + 1i32)?, tx_info_ptr)?;
                        vm.insert_value((exec_info_ptr + 2i32)?, &exec_info.caller_address)?;
                        vm.insert_value((exec_info_ptr + 3i32)?, &exec_info.contract_address)?;
                        res_segment.offset += 4;
                        vm.insert_value(result_ptr, exec_info_ptr)?;
                        Ok(None)
                    })?;
                } else if selector == "EmitEvent".as_bytes() {
                    check_handle_oog(6, 50, &mut |vm| {
                        let _keys_start_ptr = get_ptr(vm, cell, &(base_offset.clone() + 2u32))?;
                        let _keys_end_ptr = get_ptr(vm, cell, &(base_offset.clone() + 3u32))?;
                        let _values_start_ptr = get_ptr(vm, cell, &(base_offset.clone() + 4u32))?;
                        let _values_end_ptr = get_ptr(vm, cell, &(base_offset.clone() + 5u32))?;
                        Ok(None)
                    })?;
                } else if selector == "Keccak".as_bytes() {
                    // TODO(orizi): Make cost dependent on length of input.
                    check_handle_oog(4, 5000, &mut |vm| {
                        let data_start_ptr = get_ptr(vm, cell, &(base_offset.clone() + 2u32))?;
                        let data_start_ptr = vm.get_relocatable(data_start_ptr)?;
                        let data_end_ptr = get_ptr(vm, cell, &(base_offset.clone() + 3u32))?;
                        let data_end_ptr = vm.get_relocatable(data_end_ptr)?;
                        if data_end_ptr <= data_start_ptr {
                            return Ok(Some(Felt252::from_bytes_be(b"Invalid data range")));
                        }
                        let size = (data_end_ptr - data_start_ptr)?;
                        if size % 17 != 0 {
                            return Ok(Some(Felt252::from_bytes_be(b"Invalid keccak input size")));
                        }
                        let data = vm.get_integer_range(data_start_ptr, size)?;
                        let mut state = [0u64; 25];
                        for chunk in data.chunks(17) {
                            for (i, val) in chunk.iter().enumerate() {
                                state[i] ^= val.to_u64().unwrap();
                            }
                            keccak::f1600(&mut state)
                        }
                        let res_low_ptr = get_ptr(vm, cell, &(base_offset.clone() + 6u32))?;
                        let res_high_ptr = get_ptr(vm, cell, &(base_offset.clone() + 7u32))?;
                        vm.insert_value(
                            res_low_ptr,
                            (Felt252::from(state[1]) << 64u32) + Felt252::from(state[0]),
                        )?;
                        vm.insert_value(
                            res_high_ptr,
                            (Felt252::from(state[3]) << 64u32) + Felt252::from(state[2]),
                        )?;

                        Ok(None)
                    })?;
                } else if selector == "Deploy".as_bytes() {
                    // TODO(spapini): Deduce the correct gas amount here.
                    check_handle_oog(7, 50, &mut |vm| {
                        // Read inputs.
                        let class_hash =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 2u32))?;
                        let _contract_address_salt =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 3u32))?;
                        let ptr = get_ptr(vm, cell, &(base_offset.clone() + 4u32))?;
                        let calldata_start_ptr = vm.get_relocatable(ptr)?;
                        let ptr = get_ptr(vm, cell, &(base_offset.clone() + 5u32))?;
                        let calldata_end_ptr = vm.get_relocatable(ptr)?;
                        let _deploy_from_zero =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 6u32))?;

                        // Assign an arbitrary address to the contract.
                        let deployed_contract_address = self.starknet_state.get_next_id();

                        // Prepare runner for running the constructor.
                        let runner = self.runner.expect("Runner is needed for starknet.");
                        let Some(contract_info) =
                            runner.starknet_contracts_info.get(&class_hash) else
                        {
                            return Ok(Some(Felt252::from_bytes_be(b"CLASS_HASH_NOT_FOUND")));
                        };

                        // Read calldata to a vector.
                        let values = vm_get_range(vm, calldata_start_ptr, calldata_end_ptr)?;

                        // Call constructor if it exists.
                        let res_data = if let Some(constructor) = &contract_info.constructor {
                            // Replace the contract address in the context.
                            let old_contract_address = std::mem::replace(
                                &mut self.starknet_state.exec_info.contract_address,
                                deployed_contract_address.clone(),
                            );

                            // Run the constructor.
                            let function = runner
                                .sierra_program_registry
                                .get_function(constructor)
                                .expect("Constructor exists, but not found.");
                            let mut res = runner
                                .run_function(
                                    function,
                                    &[Arg::Array(values)],
                                    // TODO(spapini): Assign the correct gas amount here.
                                    Some(10000000000),
                                    self.starknet_state.clone(),
                                )
                                .expect("Internal runner error.");
                            self.starknet_state = std::mem::take(&mut res.starknet_state);

                            // Restore the contract address in the context.
                            self.starknet_state.exec_info.contract_address = old_contract_address;

                            // Read the constructor return value.
                            match res.value {
                                RunResultValue::Success(value) => {
                                    read_array_result_as_vec(&res.memory, &value)
                                }
                                RunResultValue::Panic(_panic_data) => {
                                    // TODO(spapini): Add the callee panic data.
                                    return Ok(Some(Felt252::from_bytes_be(b"CONSTRUCTOR_FAILED")));
                                }
                            }
                        } else {
                            vec![]
                        };

                        // Set the class hash of the deployed contract.
                        self.starknet_state
                            .deployed_contracts
                            .insert(deployed_contract_address.clone(), class_hash);

                        // Write result.
                        let result_ptr = get_ptr(vm, cell, &(base_offset.clone() + 9u32))?;
                        vm.insert_value(result_ptr, deployed_contract_address)?;
                        let return_start = vm.add_memory_segment();
                        let return_end = vm.load_data(
                            return_start,
                            &res_data.into_iter().map(MaybeRelocatable::Int).collect(),
                        )?;
                        vm.insert_value((result_ptr + 1i32)?, return_start)?;
                        vm.insert_value((result_ptr + 2i32)?, return_end)?;

                        Ok(None)
                    })?;
                } else if selector == "CallContract".as_bytes() {
                    // TODO(spapini): Deduce the corrent gas amount here.
                    check_handle_oog(6, 50, &mut |vm| {
                        // Read inputs.
                        let contract_address =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 2u32))?;
                        let selector =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 3u32))?;
                        let ptr = get_ptr(vm, cell, &(base_offset.clone() + 4u32))?;
                        let calldata_start_ptr = vm.get_relocatable(ptr)?;
                        let ptr = get_ptr(vm, cell, &(base_offset.clone() + 5u32))?;
                        let calldata_end_ptr = vm.get_relocatable(ptr)?;

                        // Get the class hash of the contract.
                        let Some(class_hash) =
                            self.starknet_state.deployed_contracts.get(&contract_address) else
                        {
                            return Ok(Some(Felt252::from_bytes_be(b"CONTRACT_NOT_DEPLOYED")));
                        };

                        // Prepare runner for running the ctor.
                        let runner = self.runner.expect("Runner is needed for starknet.");
                        let contract_info = runner
                            .starknet_contracts_info
                            .get(class_hash)
                            .expect("Deployed contract not found in registry.");

                        // Read calldata to a vector.
                        let values = vm_get_range(vm, calldata_start_ptr, calldata_end_ptr)?;

                        // Replace the contract address in the context.
                        let old_contract_address = std::mem::replace(
                            &mut self.starknet_state.exec_info.contract_address,
                            contract_address.clone(),
                        );
                        let old_caller_address = std::mem::replace(
                            &mut self.starknet_state.exec_info.caller_address,
                            old_contract_address.clone(),
                        );

                        // Call the function.
                        let Some(entry_point) = contract_info.externals.get(&selector) else
                        {
                            return Ok(Some(Felt252::from_bytes_be(b"ENTRYPOINT_NOT_FOUND")));
                        };
                        let function = runner
                            .sierra_program_registry
                            .get_function(entry_point)
                            .expect("Entrypoint exists, but not found.");
                        let mut res = runner
                            .run_function(
                                function,
                                &[Arg::Array(values)],
                                Some(10000000000),
                                self.starknet_state.clone(),
                            )
                            .expect("Internal runner error.");

                        self.starknet_state = std::mem::take(&mut res.starknet_state);
                        // Read the constructor return value.
                        let res_data = match res.value {
                            RunResultValue::Success(value) => {
                                read_array_result_as_vec(&res.memory, &value)
                            }
                            RunResultValue::Panic(_panic_data) => {
                                // TODO(spapini): Add the callee panic data.
                                return Ok(Some(Felt252::from_bytes_be(b"ENTRYPOINT_FAILED")));
                            }
                        };

                        // Restore the contract address in the context.
                        self.starknet_state.exec_info.caller_address = old_caller_address;
                        self.starknet_state.exec_info.contract_address = old_contract_address;

                        // Write result.
                        let result_ptr = get_ptr(vm, cell, &(base_offset.clone() + 8u32))?;
                        let return_start = vm.add_memory_segment();
                        let return_end = vm.load_data(
                            return_start,
                            &res_data.into_iter().map(MaybeRelocatable::Int).collect(),
                        )?;
                        vm.insert_value(result_ptr, return_start)?;
                        vm.insert_value((result_ptr + 1i32)?, return_end)?;

                        Ok(None)
                    })?;
                } else if selector == "LibraryCall".as_bytes() {
                    // TODO(spapini): Deduce the corrent gas amount here.
                    check_handle_oog(6, 50, &mut |vm| {
                        // Read inputs.
                        let class_hash =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 2u32))?;
                        let selector =
                            get_double_deref_val(vm, cell, &(base_offset.clone() + 3u32))?;
                        let ptr = get_ptr(vm, cell, &(base_offset.clone() + 4u32))?;
                        let calldata_start_ptr = vm.get_relocatable(ptr)?;
                        let ptr = get_ptr(vm, cell, &(base_offset.clone() + 5u32))?;
                        let calldata_end_ptr = vm.get_relocatable(ptr)?;

                        // Prepare runner for running the ctor.
                        let runner = self.runner.expect("Runner is needed for starknet.");
                        let contract_info = runner
                            .starknet_contracts_info
                            .get(&class_hash)
                            .expect("Deployed contract not found in registry.");

                        // Read calldata to a vector.
                        let values = vm_get_range(vm, calldata_start_ptr, calldata_end_ptr)?;

                        // Call the function.
                        let Some(entry_point) = contract_info.externals.get(&selector) else
                        {
                            return Ok(Some(Felt252::from_bytes_be(b"ENTRYPOINT_NOT_FOUND")));
                        };
                        let function = runner
                            .sierra_program_registry
                            .get_function(entry_point)
                            .expect("Entrypoint exists, but not found.");
                        let mut res = runner
                            .run_function(
                                function,
                                &[Arg::Array(values)],
                                Some(10000000000),
                                self.starknet_state.clone(),
                            )
                            .expect("Internal runner error.");

                        self.starknet_state = std::mem::take(&mut res.starknet_state);
                        // Read the constructor return value.
                        let res_data = match res.value {
                            RunResultValue::Success(value) => {
                                read_array_result_as_vec(&res.memory, &value)
                            }
                            RunResultValue::Panic(_panic_data) => {
                                // TODO(spapini): Add the callee panic data.
                                return Ok(Some(Felt252::from_bytes_be(b"ENTRYPOINT_FAILED")));
                            }
                        };

                        // Write result.
                        let result_ptr = get_ptr(vm, cell, &(base_offset.clone() + 8u32))?;
                        let return_start = vm.add_memory_segment();
                        let return_end = vm.load_data(
                            return_start,
                            &res_data.into_iter().map(MaybeRelocatable::Int).collect(),
                        )?;
                        vm.insert_value(result_ptr, return_start)?;
                        vm.insert_value((result_ptr + 1i32)?, return_end)?;

                        Ok(None)
                    })?;
                } else {
                    panic!("Unknown selector for system call!");
                }
            }
            StarknetHint::SetBlockNumber { value } => {
                self.starknet_state.exec_info.block_info.block_number = get_val(vm, value)?;
            }
            StarknetHint::SetSequencerAddress { value } => {
                self.starknet_state.exec_info.block_info.sequencer_address = get_val(vm, value)?;
            }
            StarknetHint::SetBlockTimestamp { value } => {
                self.starknet_state.exec_info.block_info.block_timestamp = get_val(vm, value)?;
            }
            StarknetHint::SetCallerAddress { value } => {
                self.starknet_state.exec_info.caller_address = get_val(vm, value)?;
            }
            StarknetHint::SetContractAddress { value } => {
                self.starknet_state.exec_info.contract_address = get_val(vm, value)?;
            }
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

/// Executes a core hint.
pub fn execute_core_hint(
    vm: &mut VirtualMachine,
    exec_scopes: &mut ExecutionScopes,
    core_hint: &cairo_lang_casm::hints::CoreHint,
) -> Result<(), HintError> {
    match core_hint {
        CoreHint::AllocSegment { dst } => {
            let segment = vm.add_memory_segment();
            insert_value_to_cellref!(vm, dst, segment)?;
        }
        CoreHint::TestLessThan { lhs, rhs, dst } => {
            let lhs_val = get_val(vm, lhs)?;
            let rhs_val = get_val(vm, rhs)?;
            insert_value_to_cellref!(
                vm,
                dst,
                if lhs_val < rhs_val { Felt252::from(1) } else { Felt252::from(0) }
            )?;
        }
        CoreHint::TestLessThanOrEqual { lhs, rhs, dst } => {
            let lhs_val = get_val(vm, lhs)?;
            let rhs_val = get_val(vm, rhs)?;
            insert_value_to_cellref!(
                vm,
                dst,
                if lhs_val <= rhs_val { Felt252::from(1) } else { Felt252::from(0) }
            )?;
        }
        CoreHint::DivMod { lhs, rhs, quotient, remainder } => {
            let lhs_val = get_val(vm, lhs)?.to_biguint();
            let rhs_val = get_val(vm, rhs)?.to_biguint();
            insert_value_to_cellref!(
                vm,
                quotient,
                Felt252::from(lhs_val.clone() / rhs_val.clone())
            )?;
            insert_value_to_cellref!(vm, remainder, Felt252::from(lhs_val % rhs_val))?;
        }
        CoreHint::Uint256DivMod {
            dividend_low,
            dividend_high,
            divisor_low,
            divisor_high,
            quotient0,
            quotient1,
            divisor0,
            divisor1,
            extra0,
            extra1,
            remainder_low,
            remainder_high,
        } => {
            let pow_2_128 = BigUint::from(u128::MAX) + 1u32;
            let pow_2_64 = BigUint::from(u64::MAX) + 1u32;
            let dividend_low = get_val(vm, dividend_low)?.to_biguint();
            let dividend_high = get_val(vm, dividend_high)?.to_biguint();
            let divisor_low = get_val(vm, divisor_low)?.to_biguint();
            let divisor_high = get_val(vm, divisor_high)?.to_biguint();
            let dividend = dividend_low + dividend_high * pow_2_128.clone();
            let divisor = divisor_low + divisor_high.clone() * pow_2_128.clone();
            let quotient = dividend.clone() / divisor.clone();
            let remainder = dividend % divisor.clone();

            // Guess quotient limbs.
            let (quotient, limb) = quotient.div_rem(&pow_2_64);
            insert_value_to_cellref!(vm, quotient0, Felt252::from(limb))?;
            let (quotient, limb) = quotient.div_rem(&pow_2_64);
            insert_value_to_cellref!(vm, quotient1, Felt252::from(limb))?;
            let (quotient, limb) = quotient.div_rem(&pow_2_64);
            if divisor_high.is_zero() {
                insert_value_to_cellref!(vm, extra0, Felt252::from(limb))?;
                insert_value_to_cellref!(vm, extra1, Felt252::from(quotient))?;
            }

            // Guess divisor limbs.
            let (divisor, limb) = divisor.div_rem(&pow_2_64);
            insert_value_to_cellref!(vm, divisor0, Felt252::from(limb))?;
            let (divisor, limb) = divisor.div_rem(&pow_2_64);
            insert_value_to_cellref!(vm, divisor1, Felt252::from(limb))?;
            let (divisor, limb) = divisor.div_rem(&pow_2_64);
            if !divisor_high.is_zero() {
                insert_value_to_cellref!(vm, extra0, Felt252::from(limb))?;
                insert_value_to_cellref!(vm, extra1, Felt252::from(divisor))?;
            }

            // Guess remainder limbs.
            insert_value_to_cellref!(
                vm,
                remainder_low,
                Felt252::from(remainder.clone() % pow_2_128.clone())
            )?;
            insert_value_to_cellref!(vm, remainder_high, Felt252::from(remainder / pow_2_128))?;
        }
        CoreHint::SquareRoot { value, dst } => {
            let val = get_val(vm, value)?.to_biguint();
            insert_value_to_cellref!(vm, dst, Felt252::from(val.sqrt()))?;
        }
        CoreHint::Uint256SquareRoot {
            value_low,
            value_high,
            sqrt0,
            sqrt1,
            remainder_low,
            remainder_high,
            sqrt_mul_2_minus_remainder_ge_u128,
        } => {
            let pow_2_128 = BigUint::from(u128::MAX) + 1u32;
            let pow_2_64 = BigUint::from(u64::MAX) + 1u32;
            let value_low = get_val(vm, value_low)?.to_biguint();
            let value_high = get_val(vm, value_high)?.to_biguint();
            let value = value_low + value_high * pow_2_128.clone();
            let sqrt = value.sqrt();
            let remainder = value - sqrt.clone() * sqrt.clone();
            let sqrt_mul_2_minus_remainder_ge_u128_val =
                sqrt.clone() * 2u32 - remainder.clone() >= pow_2_128;

            // Guess sqrt limbs.
            let (sqrt1_val, sqrt0_val) = sqrt.div_rem(&pow_2_64);
            insert_value_to_cellref!(vm, sqrt0, Felt252::from(sqrt0_val))?;
            insert_value_to_cellref!(vm, sqrt1, Felt252::from(sqrt1_val))?;

            let (remainder_high_val, remainder_low_val) = remainder.div_rem(&pow_2_128);
            // Guess remainder limbs.
            insert_value_to_cellref!(vm, remainder_low, Felt252::from(remainder_low_val))?;
            insert_value_to_cellref!(vm, remainder_high, Felt252::from(remainder_high_val))?;
            insert_value_to_cellref!(
                vm,
                sqrt_mul_2_minus_remainder_ge_u128,
                Felt252::from(usize::from(sqrt_mul_2_minus_remainder_ge_u128_val))
            )?;
        }
        CoreHint::LinearSplit { value, scalar, max_x, x, y } => {
            let value = get_val(vm, value)?.to_biguint();
            let scalar = get_val(vm, scalar)?.to_biguint();
            let max_x = get_val(vm, max_x)?.to_biguint();
            let x_value = (value.clone() / scalar.clone()).min(max_x);
            let y_value = value - x_value.clone() * scalar;
            insert_value_to_cellref!(vm, x, Felt252::from(x_value))?;
            insert_value_to_cellref!(vm, y, Felt252::from(y_value))?;
        }
        CoreHint::RandomEcPoint { x, y } => {
            // Keep sampling a random field element `X` until `X^3 + X + beta` is a quadratic
            // residue.
            let beta = Fq::from(get_beta().to_biguint());
            let mut rng = ark_std::test_rng();
            let (random_x, random_y_squared) = loop {
                let random_x = Fq::rand(&mut rng);
                let random_y_squared = random_x * random_x * random_x + random_x + beta;
                if random_y_squared.legendre().is_qr() {
                    break (random_x, random_y_squared);
                }
            };
            let x_bigint: BigUint = random_x.into_bigint().into();
            let y_bigint: BigUint = random_y_squared.sqrt().unwrap().into_bigint().into();
            insert_value_to_cellref!(vm, x, Felt252::from(x_bigint))?;
            insert_value_to_cellref!(vm, y, Felt252::from(y_bigint))?;
        }
        CoreHint::FieldSqrt { val, sqrt } => {
            let val = Fq::from(get_val(vm, val)?.to_biguint());
            insert_value_to_cellref!(vm, sqrt, {
                let three_fq = Fq::from(BigUint::from_usize(3).unwrap());
                let res =
                    (if val.legendre().is_qr() { val } else { val * three_fq }).sqrt().unwrap();
                let root0: BigUint = res.into_bigint().into();
                let root1: BigUint = (-res).into_bigint().into();
                let res_big_uint = std::cmp::min(root0, root1);
                Felt252::from(res_big_uint)
            })?;
        }
        CoreHint::AllocFelt252Dict { segment_arena_ptr } => {
            let (cell, base_offset) = extract_buffer(segment_arena_ptr);
            let dict_manager_address = get_ptr(vm, cell, &base_offset)?;
            let n_dicts = vm
                .get_integer((dict_manager_address - 2)?)?
                .into_owned()
                .to_usize()
                .expect("Number of dictionaries too large.");
            let dict_infos_base = vm.get_relocatable((dict_manager_address - 3)?)?;

            let dict_manager_exec_scope = match exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
            {
                Ok(dict_manager_exec_scope) => dict_manager_exec_scope,
                Err(_) => {
                    exec_scopes.assign_or_update_variable(
                        "dict_manager_exec_scope",
                        Box::<DictManagerExecScope>::default(),
                    );
                    exec_scopes.get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")?
                }
            };
            let new_dict_segment = dict_manager_exec_scope.new_default_dict(vm);
            vm.insert_value((dict_infos_base + 3 * n_dicts)?, new_dict_segment)?;
        }
        CoreHint::Felt252DictRead { dict_ptr, key, value_dst } => {
            let (dict_base, dict_offset) = extract_buffer(dict_ptr);
            let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
            let key = get_val(vm, key)?;
            let dict_manager_exec_scope = exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to read from a dict while dict manager was not initialized.");
            let value = dict_manager_exec_scope
                .get_from_tracker(dict_address, &key)
                .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
            insert_value_to_cellref!(vm, value_dst, value)?;
        }
        CoreHint::Felt252DictWrite { dict_ptr, key, value } => {
            let (dict_base, dict_offset) = extract_buffer(dict_ptr);
            let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
            let key = get_val(vm, key)?;
            let value = get_val(vm, value)?;
            let dict_manager_exec_scope = exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to write to a dict while dict manager was not initialized.");
            let prev_value = dict_manager_exec_scope
                .get_from_tracker(dict_address, &key)
                .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
            vm.insert_value((dict_address + 1)?, prev_value)?;
            dict_manager_exec_scope.insert_to_tracker(dict_address, key, value);
        }
        CoreHint::Felt252DictEntryInit { dict_ptr, key } => {
            let (dict_base, dict_offset) = extract_buffer(dict_ptr);
            let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
            let key = get_val(vm, key)?;
            let dict_manager_exec_scope = exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to write to a dict while dict manager was not initialized.");
            let prev_value = dict_manager_exec_scope
                .get_from_tracker(dict_address, &key)
                .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
            vm.insert_value((dict_address + 1)?, prev_value)?;
        }
        CoreHint::Felt252DictEntryUpdate { dict_ptr, value } => {
            let (dict_base, dict_offset) = extract_buffer(dict_ptr);
            let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
            let key = get_double_deref_val(vm, dict_base, &(dict_offset + Felt252::from(-3)))?;
            let value = get_val(vm, value)?;
            let dict_manager_exec_scope = exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to write to a dict while dict manager was not initialized.");
            dict_manager_exec_scope.insert_to_tracker(dict_address, key, value);
        }
        CoreHint::GetSegmentArenaIndex { dict_end_ptr, dict_index, .. } => {
            let (dict_base, dict_offset) = extract_buffer(dict_end_ptr);
            let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
            let dict_manager_exec_scope = exec_scopes
                .get_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to read from a dict while dict manager was not initialized.");
            let dict_infos_index = dict_manager_exec_scope.get_dict_infos_index(dict_address);
            insert_value_to_cellref!(vm, dict_index, Felt252::from(dict_infos_index))?;
        }
        CoreHint::InitSquashData { dict_accesses, n_accesses, first_key, big_keys, .. } => {
            let dict_access_size = 3;
            let rangecheck_bound = Felt252::from(u128::MAX) + 1u32;

            exec_scopes.assign_or_update_variable(
                "dict_squash_exec_scope",
                Box::<DictSquashExecScope>::default(),
            );
            let dict_squash_exec_scope =
                exec_scopes.get_mut_ref::<DictSquashExecScope>("dict_squash_exec_scope")?;
            let (dict_accesses_base, dict_accesses_offset) = extract_buffer(dict_accesses);
            let dict_accesses_address = get_ptr(vm, dict_accesses_base, &dict_accesses_offset)?;
            let n_accesses = get_val(vm, n_accesses)?
                .to_usize()
                .expect("Number of accesses is too large or negative.");
            for i in 0..n_accesses {
                let current_key =
                    vm.get_integer((dict_accesses_address + i * dict_access_size)?)?;
                dict_squash_exec_scope
                    .access_indices
                    .entry(current_key.into_owned())
                    .and_modify(|indices| indices.push(Felt252::from(i)))
                    .or_insert_with(|| vec![Felt252::from(i)]);
            }
            // Reverse the accesses in order to pop them in order later.
            for (_, accesses) in dict_squash_exec_scope.access_indices.iter_mut() {
                accesses.reverse();
            }
            dict_squash_exec_scope.keys =
                dict_squash_exec_scope.access_indices.keys().cloned().collect();
            dict_squash_exec_scope.keys.sort_by(|a, b| b.cmp(a));
            // big_keys indicates if the keys are greater than rangecheck_bound. If they are not
            // a simple range check is used instead of assert_le_felt252.
            insert_value_to_cellref!(
                vm,
                big_keys,
                if dict_squash_exec_scope.keys[0] < rangecheck_bound {
                    Felt252::from(0)
                } else {
                    Felt252::from(1)
                }
            )?;
            insert_value_to_cellref!(vm, first_key, dict_squash_exec_scope.current_key().unwrap())?;
        }
        CoreHint::GetCurrentAccessIndex { range_check_ptr } => {
            let dict_squash_exec_scope: &mut DictSquashExecScope =
                exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
            let (range_check_base, range_check_offset) = extract_buffer(range_check_ptr);
            let range_check_ptr = get_ptr(vm, range_check_base, &range_check_offset)?;
            let current_access_index = dict_squash_exec_scope.current_access_index().unwrap();
            vm.insert_value(range_check_ptr, current_access_index)?;
        }
        CoreHint::ShouldSkipSquashLoop { should_skip_loop } => {
            let dict_squash_exec_scope: &mut DictSquashExecScope =
                exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
            insert_value_to_cellref!(
                vm,
                should_skip_loop,
                // The loop verifies that each two consecutive accesses are valid, thus we
                // break when there is only one remaining access.
                if dict_squash_exec_scope.current_access_indices().unwrap().len() > 1 {
                    Felt252::from(0)
                } else {
                    Felt252::from(1)
                }
            )?;
        }
        CoreHint::GetCurrentAccessDelta { index_delta_minus1 } => {
            let dict_squash_exec_scope: &mut DictSquashExecScope =
                exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
            let prev_access_index = dict_squash_exec_scope.pop_current_access_index().unwrap();
            let index_delta_minus_1_val =
                dict_squash_exec_scope.current_access_index().unwrap().clone()
                    - prev_access_index
                    - 1_u32;
            insert_value_to_cellref!(vm, index_delta_minus1, index_delta_minus_1_val)?;
        }
        CoreHint::ShouldContinueSquashLoop { should_continue } => {
            let dict_squash_exec_scope: &mut DictSquashExecScope =
                exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
            insert_value_to_cellref!(
                vm,
                should_continue,
                // The loop verifies that each two consecutive accesses are valid, thus we
                // break when there is only one remaining access.
                if dict_squash_exec_scope.current_access_indices().unwrap().len() > 1 {
                    Felt252::from(1)
                } else {
                    Felt252::from(0)
                }
            )?;
        }
        CoreHint::AssertCurrentAccessIndicesIsEmpty => {}
        CoreHint::AssertAllAccessesUsed { .. } => {}
        CoreHint::AssertAllKeysUsed => {}
        CoreHint::GetNextDictKey { next_key } => {
            let dict_squash_exec_scope: &mut DictSquashExecScope =
                exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
            dict_squash_exec_scope.pop_current_key();
            insert_value_to_cellref!(vm, next_key, dict_squash_exec_scope.current_key().unwrap())?;
        }
        CoreHint::AssertLtAssertValidInput { .. } => {}
        CoreHint::AssertLeFindSmallArcs { a, b, range_check_ptr } => {
            let a_val = get_val(vm, a)?;
            let b_val = get_val(vm, b)?;
            let mut lengths_and_indices = vec![
                (a_val.clone(), 0),
                (b_val.clone() - a_val, 1),
                (Felt252::from(-1) - b_val, 2),
            ];
            lengths_and_indices.sort();
            exec_scopes
                .assign_or_update_variable("excluded_arc", Box::new(lengths_and_indices[2].1));
            // ceil((PRIME / 3) / 2 ** 128).
            let prime_over_3_high = 3544607988759775765608368578435044694_u128;
            // ceil((PRIME / 2) / 2 ** 128).
            let prime_over_2_high = 5316911983139663648412552867652567041_u128;
            let (range_check_base, range_check_offset) = extract_buffer(range_check_ptr);
            let range_check_ptr = get_ptr(vm, range_check_base, &range_check_offset)?;
            vm.insert_value(
                range_check_ptr,
                Felt252::from(lengths_and_indices[0].0.to_biguint() % prime_over_3_high),
            )?;
            vm.insert_value(
                (range_check_ptr + 1)?,
                Felt252::from(lengths_and_indices[0].0.to_biguint() / prime_over_3_high),
            )?;
            vm.insert_value(
                (range_check_ptr + 2)?,
                Felt252::from(lengths_and_indices[1].0.to_biguint() % prime_over_2_high),
            )?;
            vm.insert_value(
                (range_check_ptr + 3)?,
                Felt252::from(lengths_and_indices[1].0.to_biguint() / prime_over_2_high),
            )?;
        }
        CoreHint::AssertLeIsFirstArcExcluded { skip_exclude_a_flag } => {
            let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
            insert_value_to_cellref!(
                vm,
                skip_exclude_a_flag,
                if excluded_arc != 0 { Felt252::from(1) } else { Felt252::from(0) }
            )?;
        }
        CoreHint::AssertLeIsSecondArcExcluded { skip_exclude_b_minus_a } => {
            let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
            insert_value_to_cellref!(
                vm,
                skip_exclude_b_minus_a,
                if excluded_arc != 1 { Felt252::from(1) } else { Felt252::from(0) }
            )?;
        }
        CoreHint::AssertLeAssertThirdArcExcluded => {}
        CoreHint::DebugPrint { start, end } => {
            let as_relocatable = |vm, value| {
                let (base, offset) = extract_buffer(value);
                get_ptr(vm, base, &offset)
            };
            let mut curr = as_relocatable(vm, start)?;
            let end = as_relocatable(vm, end)?;
            while curr != end {
                let value = vm.get_integer(curr)?;
                if let Some(shortstring) = as_cairo_short_string(&value) {
                    println!("[DEBUG]\t{shortstring: <31}\t(raw: {value: <31})");
                } else {
                    println!("[DEBUG]\t{0: <31}\t(raw: {value: <31}) ", ' ');
                }
                curr += 1;
            }
            println!();
        }
        CoreHint::AllocConstantSize { size, dst } => {
            let object_size = get_val(vm, size)?.to_usize().expect("Object size too large.");
            let memory_exec_scope =
                match exec_scopes.get_mut_ref::<MemoryExecScope>("memory_exec_scope") {
                    Ok(memory_exec_scope) => memory_exec_scope,
                    Err(_) => {
                        exec_scopes.assign_or_update_variable(
                            "memory_exec_scope",
                            Box::new(MemoryExecScope { next_address: vm.add_memory_segment() }),
                        );
                        exec_scopes.get_mut_ref::<MemoryExecScope>("memory_exec_scope")?
                    }
                };
            insert_value_to_cellref!(vm, dst, memory_exec_scope.next_address)?;
            memory_exec_scope.next_address.offset += object_size;
        }
    };
    Ok(())
}

/// Reads the result of a function call that returns `Array<felt252>`.
fn read_array_result_as_vec(memory: &[Option<Felt252>], value: &[Felt252]) -> Vec<Felt252> {
    // TODO(spapini): Handle failures.
    let [res_start, res_end] = value else {
        panic!("Unexpected return value from contract call");
    };
    let res_start: usize = res_start.clone().to_bigint().try_into().unwrap();
    let res_end: usize = res_end.clone().to_bigint().try_into().unwrap();
    (res_start..res_end).map(|i| memory[i].clone().unwrap()).collect()
}

/// Loads a range of values from the VM memory.
fn vm_get_range(
    vm: &mut VirtualMachine,
    mut calldata_start_ptr: Relocatable,
    calldata_end_ptr: Relocatable,
) -> Result<Vec<Felt252>, HintError> {
    let mut values = vec![];
    while calldata_start_ptr != calldata_end_ptr {
        let val = vm.get_integer(calldata_start_ptr)?.deref().clone();
        values.push(val);
        calldata_start_ptr.offset += 1;
    }
    Ok(values)
}

/// Extracts a parameter assumed to be a buffer.
fn extract_buffer(buffer: &ResOperand) -> (&CellRef, Felt252) {
    let (cell, base_offset) = match buffer {
        ResOperand::Deref(cell) => (cell, 0.into()),
        ResOperand::BinOp(BinOpOperand { op: Operation::Add, a, b }) => {
            (a, extract_matches!(b, DerefOrImmediate::Immediate).clone().value.into())
        }
        _ => panic!("Illegal argument for a buffer."),
    };
    (cell, base_offset)
}

/// Provides context for the `additional_initialization` callback function of [run_function].
pub struct RunFunctionContext<'a> {
    pub vm: &'a mut VirtualMachine,
    pub data_len: usize,
}

type RunFunctionRes = (Vec<Option<Felt252>>, usize, StarknetState);

/// Runs `program` on layout with prime, and returns the memory layout and ap value.
pub fn run_function<'a, 'b: 'a, Instructions: Iterator<Item = &'a Instruction> + Clone>(
    runner: Option<&'b SierraCasmRunner>,
    instructions: Instructions,
    builtins: Vec<BuiltinName>,
    additional_initialization: fn(
        context: RunFunctionContext<'_>,
    ) -> Result<(), Box<VirtualMachineError>>,
    starknet_state: StarknetState,
) -> Result<RunFunctionRes, Box<VirtualMachineError>> {
    let data: Vec<MaybeRelocatable> = instructions
        .clone()
        .flat_map(|inst| inst.assemble().encode())
        .map(Felt252::from)
        .map(MaybeRelocatable::from)
        .collect();

    let mut hint_processor = CairoHintProcessor::new(runner, instructions, starknet_state);

    let data_len = data.len();
    let program = Program {
        builtins,
        prime: PRIME_STR.to_string(),
        data,
        constants: HashMap::new(),
        main: Some(0),
        start: None,
        end: None,
        hints: hint_processor.hints_dict.clone(),
        reference_manager: ReferenceManager { references: Vec::new() },
        identifiers: HashMap::new(),
        error_message_attributes: vec![],
        instruction_locations: None,
    };
    let mut runner = CairoRunner::new(&program, "all_cairo", false)
        .map_err(VirtualMachineError::from)
        .map_err(Box::new)?;
    let mut vm = VirtualMachine::new(true);

    let end = runner.initialize(&mut vm).map_err(VirtualMachineError::from).map_err(Box::new)?;

    additional_initialization(RunFunctionContext { vm: &mut vm, data_len })?;

    runner.run_until_pc(end, &mut vm, &mut hint_processor)?;
    runner.end_run(true, false, &mut vm, &mut hint_processor).map_err(Box::new)?;
    runner.relocate(&mut vm, true).map_err(VirtualMachineError::from).map_err(Box::new)?;
    Ok((
        runner.relocated_memory,
        vm.get_relocated_trace().unwrap().last().unwrap().ap,
        hint_processor.starknet_state,
    ))
}
