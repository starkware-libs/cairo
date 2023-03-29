use std::any::Any;
use std::collections::HashMap;

use ark_ff::fields::{Fp256, MontBackend, MontConfig};
use ark_ff::{Field, PrimeField};
use ark_std::UniformRand;
use cairo_felt::{felt_str as felt252_str, Felt252, PRIME_STR};
use cairo_lang_casm::hints::Hint;
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
use num_traits::{FromPrimitive, ToPrimitive, Zero};

use self::dict_manager::DictSquashExecScope;
use crate::short_string::as_cairo_short_string;

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
struct CairoHintProcessor {
    // A dict from instruction offset to hint vector.
    pub hints_dict: HashMap<usize, Vec<HintParams>>,
    // A mapping from a string that represents a hint to the hint object.
    pub string_to_hint: HashMap<String, Hint>,
}

impl CairoHintProcessor {
    pub fn new<'a, Instructions: Iterator<Item = &'a Instruction> + Clone>(
        instructions: Instructions,
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
        CairoHintProcessor { hints_dict, string_to_hint }
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
struct StarknetExecScope {
    /// The values of addresses in the simulated storage per contract.
    storage: HashMap<Felt252, HashMap<Felt252, Felt252>>,
    /// The simulated execution info.
    exec_info: ExecutionInfo,
}

/// Copy of the cairo `ExecutionInfo` struct.
#[derive(Default)]
struct ExecutionInfo {
    block_info: BlockInfo,
    tx_info: TxInfo,
    caller_address: Felt252,
    contract_address: Felt252,
}

/// Copy of the cairo `BlockInfo` struct.
#[derive(Default)]
struct BlockInfo {
    block_number: Felt252,
    block_timestamp: Felt252,
    sequencer_address: Felt252,
}

/// Copy of the cairo `TxInfo` struct.
#[derive(Default)]
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

impl HintProcessor for CairoHintProcessor {
    /// Trait function to execute a given hint in the hint processor.
    fn execute_hint(
        &mut self,
        vm: &mut VirtualMachine,
        exec_scopes: &mut ExecutionScopes,
        hint_data: &Box<dyn Any>,
        _constants: &HashMap<String, Felt252>,
    ) -> Result<(), HintError> {
        let hint = hint_data.downcast_ref::<Hint>().unwrap();
        match hint {
            Hint::AllocSegment { dst } => {
                let segment = vm.add_memory_segment();
                insert_value_to_cellref!(vm, dst, segment)?;
            }
            Hint::TestLessThan { lhs, rhs, dst } => {
                let lhs_val = get_val(vm, lhs)?;
                let rhs_val = get_val(vm, rhs)?;
                insert_value_to_cellref!(
                    vm,
                    dst,
                    if lhs_val < rhs_val { Felt252::from(1) } else { Felt252::from(0) }
                )?;
            }
            Hint::TestLessThanOrEqual { lhs, rhs, dst } => {
                let lhs_val = get_val(vm, lhs)?;
                let rhs_val = get_val(vm, rhs)?;
                insert_value_to_cellref!(
                    vm,
                    dst,
                    if lhs_val <= rhs_val { Felt252::from(1) } else { Felt252::from(0) }
                )?;
            }
            Hint::DivMod { lhs, rhs, quotient, remainder } => {
                let lhs_val = get_val(vm, lhs)?.to_biguint();
                let rhs_val = get_val(vm, rhs)?.to_biguint();
                insert_value_to_cellref!(
                    vm,
                    quotient,
                    Felt252::from(lhs_val.clone() / rhs_val.clone())
                )?;
                insert_value_to_cellref!(vm, remainder, Felt252::from(lhs_val % rhs_val))?;
            }
            Hint::SquareRoot { value, dst } => {
                let val = get_val(vm, value)?.to_biguint();
                insert_value_to_cellref!(vm, dst, Felt252::from(val.sqrt()))?;
            }
            Hint::LinearSplit { value, scalar, max_x, x, y } => {
                let value = get_val(vm, value)?.to_biguint();
                let scalar = get_val(vm, scalar)?.to_biguint();
                let max_x = get_val(vm, max_x)?.to_biguint();
                let x_value = (value.clone() / scalar.clone()).min(max_x);
                let y_value = value - x_value.clone() * scalar;
                insert_value_to_cellref!(vm, x, Felt252::from(x_value))?;
                insert_value_to_cellref!(vm, y, Felt252::from(y_value))?;
            }
            Hint::RandomEcPoint { x, y } => {
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
            Hint::FieldSqrt { val, sqrt } => {
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
            Hint::SystemCall { system } => {
                let starknet_exec_scope = starknet_execution_scope(exec_scopes)?;
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
                        let contract = starknet_exec_scope.exec_info.contract_address.clone();
                        starknet_exec_scope
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
                        let value = starknet_exec_scope
                            .storage
                            .get(&starknet_exec_scope.exec_info.contract_address)
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
                        let exec_info = &starknet_exec_scope.exec_info;
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
                } else if selector == "CallContract".as_bytes() {
                    todo!()
                } else {
                    panic!("Unknown selector for system call!");
                }
            }
            Hint::SetBlockNumber { value } => {
                starknet_execution_scope(exec_scopes)?.exec_info.block_info.block_number =
                    get_val(vm, value)?;
            }
            Hint::SetSequencerAddress { value } => {
                starknet_execution_scope(exec_scopes)?.exec_info.block_info.sequencer_address =
                    get_val(vm, value)?;
            }
            Hint::SetBlockTimestamp { value } => {
                starknet_execution_scope(exec_scopes)?.exec_info.block_info.block_timestamp =
                    get_val(vm, value)?;
            }
            Hint::SetCallerAddress { value } => {
                starknet_execution_scope(exec_scopes)?.exec_info.caller_address =
                    get_val(vm, value)?;
            }
            Hint::SetContractAddress { value } => {
                starknet_execution_scope(exec_scopes)?.exec_info.contract_address =
                    get_val(vm, value)?;
            }
            Hint::AllocFelt252Dict { segment_arena_ptr } => {
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
                        exec_scopes
                            .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")?
                    }
                };
                let new_dict_segment = dict_manager_exec_scope.new_default_dict(vm);
                vm.insert_value((dict_infos_base + 3 * n_dicts)?, new_dict_segment)?;
            }
            Hint::Felt252DictRead { dict_ptr, key, value_dst } => {
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
            Hint::Felt252DictWrite { dict_ptr, key, value } => {
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
            Hint::GetSegmentArenaIndex { dict_end_ptr, dict_index, .. } => {
                let (dict_base, dict_offset) = extract_buffer(dict_end_ptr);
                let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
                let dict_manager_exec_scope = exec_scopes
                    .get_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                    .expect("Trying to read from a dict while dict manager was not initialized.");
                let dict_infos_index = dict_manager_exec_scope.get_dict_infos_index(dict_address);
                insert_value_to_cellref!(vm, dict_index, Felt252::from(dict_infos_index))?;
            }
            Hint::InitSquashData { dict_accesses, n_accesses, first_key, big_keys, .. } => {
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
                insert_value_to_cellref!(
                    vm,
                    first_key,
                    dict_squash_exec_scope.current_key().unwrap()
                )?;
            }
            Hint::GetCurrentAccessIndex { range_check_ptr } => {
                let dict_squash_exec_scope: &mut DictSquashExecScope =
                    exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
                let (range_check_base, range_check_offset) = extract_buffer(range_check_ptr);
                let range_check_ptr = get_ptr(vm, range_check_base, &range_check_offset)?;
                let current_access_index = dict_squash_exec_scope.current_access_index().unwrap();
                vm.insert_value(range_check_ptr, current_access_index)?;
            }
            Hint::ShouldSkipSquashLoop { should_skip_loop } => {
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
            Hint::GetCurrentAccessDelta { index_delta_minus1 } => {
                let dict_squash_exec_scope: &mut DictSquashExecScope =
                    exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
                let prev_access_index = dict_squash_exec_scope.pop_current_access_index().unwrap();
                let index_delta_minus_1_val =
                    dict_squash_exec_scope.current_access_index().unwrap().clone()
                        - prev_access_index
                        - 1_u32;
                insert_value_to_cellref!(vm, index_delta_minus1, index_delta_minus_1_val)?;
            }
            Hint::ShouldContinueSquashLoop { should_continue } => {
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
            Hint::AssertCurrentAccessIndicesIsEmpty => {}
            Hint::AssertAllAccessesUsed { .. } => {}
            Hint::AssertAllKeysUsed => {}
            Hint::GetNextDictKey { next_key } => {
                let dict_squash_exec_scope: &mut DictSquashExecScope =
                    exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
                dict_squash_exec_scope.pop_current_key();
                insert_value_to_cellref!(
                    vm,
                    next_key,
                    dict_squash_exec_scope.current_key().unwrap()
                )?;
            }
            Hint::AssertLtAssertValidInput { .. } => {}
            Hint::AssertLeFindSmallArcs { a, b, range_check_ptr } => {
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
                // ceil((PRIME / 2) / 2 ** 128).
                let prime_over_2_high = 3544607988759775765608368578435044694_u128;
                // ceil((PRIME / 3) / 2 ** 128).
                let prime_over_3_high = 5316911983139663648412552867652567041_u128;
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
            Hint::AssertLeIsFirstArcExcluded { skip_exclude_a_flag } => {
                let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
                insert_value_to_cellref!(
                    vm,
                    skip_exclude_a_flag,
                    if excluded_arc != 0 { Felt252::from(1) } else { Felt252::from(0) }
                )?;
            }
            Hint::AssertLeIsSecondArcExcluded { skip_exclude_b_minus_a } => {
                let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
                insert_value_to_cellref!(
                    vm,
                    skip_exclude_b_minus_a,
                    if excluded_arc != 1 { Felt252::from(1) } else { Felt252::from(0) }
                )?;
            }
            Hint::AssertLeAssertThirdArcExcluded => {}
            Hint::DebugPrint { start, end } => {
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
            Hint::AllocConstantSize { size, dst } => {
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

/// Returns the starknet execution scope.
fn starknet_execution_scope(
    exec_scopes: &mut ExecutionScopes,
) -> Result<&mut StarknetExecScope, HintError> {
    Ok(exec_scopes
        .get_local_variables_mut()?
        .entry("starknet_exec_scope".to_string())
        .or_insert_with(|| {
            Box::new(StarknetExecScope {
                storage: HashMap::default(),
                exec_info: ExecutionInfo::default(),
            })
        })
        .downcast_mut::<StarknetExecScope>()
        .unwrap())
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

/// Runs `program` on layout with prime, and returns the memory layout and ap value.
pub fn run_function<'a, Instructions: Iterator<Item = &'a Instruction> + Clone>(
    instructions: Instructions,
    builtins: Vec<BuiltinName>,
    additional_initialization: fn(
        context: RunFunctionContext<'_>,
    ) -> Result<(), Box<VirtualMachineError>>,
) -> Result<(Vec<Option<Felt252>>, usize), Box<VirtualMachineError>> {
    let data: Vec<MaybeRelocatable> = instructions
        .clone()
        .flat_map(|inst| inst.assemble().encode())
        .map(Felt252::from)
        .map(MaybeRelocatable::from)
        .collect();

    let mut hint_processor = CairoHintProcessor::new(instructions);

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
    let mut runner = CairoRunner::new(&program, "all", false)
        .map_err(VirtualMachineError::from)
        .map_err(Box::new)?;
    let mut vm = VirtualMachine::new(true);

    let end = runner.initialize(&mut vm).map_err(VirtualMachineError::from).map_err(Box::new)?;

    additional_initialization(RunFunctionContext { vm: &mut vm, data_len })?;

    runner.run_until_pc(end, &mut vm, &mut hint_processor)?;
    runner.end_run(true, false, &mut vm, &mut hint_processor).map_err(Box::new)?;
    runner.relocate(&mut vm).map_err(VirtualMachineError::from).map_err(Box::new)?;
    Ok((runner.relocated_memory, vm.get_relocated_trace().unwrap().last().unwrap().ap))
}
