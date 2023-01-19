use std::any::Any;
use std::collections::HashMap;

use ark_ff::fields::{Fp256, MontBackend, MontConfig};
use ark_ff::{Field, PrimeField};
use ark_std::UniformRand;
use cairo_felt::{self as felt, felt_str, Felt, FeltOps, PRIME_STR};
use cairo_lang_casm::hints::Hint;
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{
    BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand,
};
use cairo_lang_utils::extract_matches;
use cairo_vm::hint_processor::hint_processor_definition::{HintProcessor, HintReference};
use cairo_vm::serde::deserialize_program::{
    ApTracking, FlowTrackingData, HintParams, ReferenceManager,
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
fn get_beta() -> Felt {
    felt_str!("3141592653589793238462643383279502884197169399375105820974944592307816406665")
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
    base + (cell_ref.offset as i32)
}

/// Inserts a value into the vm memory cell represented by the cellref.
macro_rules! insert_value_to_cellref {
    ($vm:ident, $cell_ref:ident, $value:expr) => {
        $vm.insert_value(&cell_ref_to_relocatable($cell_ref, $vm), $value)
    };
}

/// Execution scope for starknet related data.
struct StarknetExecScope {
    /// The values of addresses in the simulated storage.
    storage: HashMap<Felt, Felt>,
}

/// Execution scope for constant memory allocation.
struct MemoryExecScope {
    /// The first free address in the segment.
    next_address: Relocatable,
}

impl HintProcessor for CairoHintProcessor {
    /// Trait function to execute a given hint in the hint processor.
    fn execute_hint(
        &mut self,
        vm: &mut VirtualMachine,
        exec_scopes: &mut ExecutionScopes,
        hint_data: &Box<dyn Any>,
        _constants: &HashMap<String, Felt>,
    ) -> Result<(), HintError> {
        let hint = hint_data.downcast_ref::<Hint>().unwrap();
        let get_cell_val = |x: &CellRef| -> Result<Felt, VirtualMachineError> {
            Ok(vm.get_integer(&cell_ref_to_relocatable(x, vm))?.as_ref().clone())
        };
        let get_ptr = |cell: &CellRef, offset: &Felt| -> Result<Relocatable, VirtualMachineError> {
            let base_ptr = vm.get_relocatable(&cell_ref_to_relocatable(cell, vm))?;
            base_ptr.add_int(offset)
        };
        let get_double_deref_val =
            |cell: &CellRef, offset: &Felt| -> Result<Felt, VirtualMachineError> {
                Ok(vm.get_integer(&get_ptr(cell, offset)?)?.as_ref().clone())
            };
        let get_val = |x: &ResOperand| -> Result<Felt, VirtualMachineError> {
            match x {
                ResOperand::Deref(cell) => get_cell_val(cell),
                ResOperand::DoubleDeref(cell, offset) => {
                    get_double_deref_val(cell, &(*offset).into())
                }
                ResOperand::Immediate(x) => Ok(Felt::from(x.clone())),
                ResOperand::BinOp(op) => {
                    let a = get_cell_val(&op.a)?;
                    let b = match &op.b {
                        DerefOrImmediate::Deref(cell) => get_cell_val(cell)?,
                        DerefOrImmediate::Immediate(x) => Felt::from(x.clone()),
                    };
                    match op.op {
                        Operation::Add => Ok(a + b),
                        Operation::Mul => Ok(a * b),
                    }
                }
            }
        };
        match hint {
            Hint::AllocSegment { dst } => {
                let segment = vm.add_memory_segment();
                insert_value_to_cellref!(vm, dst, segment)?;
            }
            Hint::TestLessThan { lhs, rhs, dst } => {
                let lhs_val = get_val(lhs)?;
                let rhs_val = get_val(rhs)?;
                insert_value_to_cellref!(
                    vm,
                    dst,
                    if lhs_val < rhs_val { Felt::from(1) } else { Felt::from(0) }
                )?;
            }
            Hint::TestLessThanOrEqual { lhs, rhs, dst } => {
                let lhs_val = get_val(lhs)?;
                let rhs_val = get_val(rhs)?;
                insert_value_to_cellref!(
                    vm,
                    dst,
                    if lhs_val <= rhs_val { Felt::from(1) } else { Felt::from(0) }
                )?;
            }
            Hint::DivMod { lhs, rhs, quotient, remainder } => {
                let lhs_val = get_val(lhs)?.to_biguint();
                let rhs_val = get_val(rhs)?.to_biguint();
                insert_value_to_cellref!(
                    vm,
                    quotient,
                    Felt::from(lhs_val.clone() / rhs_val.clone())
                )?;
                insert_value_to_cellref!(vm, remainder, Felt::from(lhs_val % rhs_val))?;
            }
            Hint::LinearSplit { value, scalar, max_x, x, y } => {
                let value = get_val(value)?.to_biguint();
                let scalar = get_val(scalar)?.to_biguint();
                let max_x = get_val(max_x)?.to_biguint();
                let x_value = (value.clone() / scalar.clone()).min(max_x);
                let y_value = value - x_value.clone() * scalar;
                insert_value_to_cellref!(vm, x, Felt::from(x_value))?;
                insert_value_to_cellref!(vm, y, Felt::from(y_value))?;
            }
            Hint::EnterScope => {}
            Hint::ExitScope => {}
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
                insert_value_to_cellref!(vm, x, Felt::from(x_bigint))?;
                insert_value_to_cellref!(vm, y, Felt::from(y_bigint))?;
            }
            Hint::FieldSqrt { val, sqrt } => {
                let val = Fq::from(get_val(val)?.to_biguint());
                insert_value_to_cellref!(vm, sqrt, {
                    let three_fq = Fq::from(BigUint::from_usize(3).unwrap());
                    let res = (if val.legendre().is_qr() { val } else { val * three_fq }).sqrt();
                    let res_big_uint: BigUint = res.unwrap().into_bigint().into();
                    Felt::from(res_big_uint)
                })?;
            }
            Hint::SystemCall { system } => {
                let starknet_exec_scope =
                    match exec_scopes.get_mut_ref::<StarknetExecScope>("starknet_exec_scope") {
                        Ok(starknet_exec_scope) => starknet_exec_scope,
                        Err(_) => {
                            exec_scopes.assign_or_update_variable(
                                "starknet_exec_scope",
                                Box::new(StarknetExecScope { storage: HashMap::default() }),
                            );
                            exec_scopes.get_mut_ref::<StarknetExecScope>("starknet_exec_scope")?
                        }
                    };
                let (cell, base_offset) = extract_buffer(system);
                let selector = get_double_deref_val(cell, &base_offset)?.to_bytes_be();
                if selector == "StorageWrite".as_bytes() {
                    let gas_counter = get_double_deref_val(cell, &(base_offset.clone() + 1u32))?;
                    const WRITE_GAS_SIM_COST: usize = 1000;
                    let gas_counter_updated_ptr = get_ptr(cell, &(base_offset.clone() + 5u32))?;
                    let revert_reason_ptr = get_ptr(cell, &(base_offset.clone() + 6u32))?;
                    let addr_domain = get_double_deref_val(cell, &(base_offset.clone() + 2u32))?;

                    // Only address_domain 0 is currently supported.
                    if addr_domain.is_zero() && gas_counter >= WRITE_GAS_SIM_COST.into() {
                        let addr = get_double_deref_val(cell, &(base_offset.clone() + 3u32))?;
                        let value = get_double_deref_val(cell, &(base_offset + 4u32))?;
                        starknet_exec_scope.storage.insert(addr, value);
                        vm.insert_value(
                            &gas_counter_updated_ptr,
                            gas_counter - WRITE_GAS_SIM_COST,
                        )?;
                        vm.insert_value(&revert_reason_ptr, Felt::from(0))?;
                    } else {
                        vm.insert_value(&gas_counter_updated_ptr, gas_counter)?;
                        vm.insert_value(&revert_reason_ptr, Felt::from(1))?;
                    }
                } else if selector == "StorageRead".as_bytes() {
                    let gas_counter = get_double_deref_val(cell, &(base_offset.clone() + 1u32))?;
                    const READ_GAS_SIM_COST: usize = 100;
                    let addr_domain = get_double_deref_val(cell, &(base_offset.clone() + 2u32))?;
                    let addr = get_double_deref_val(cell, &(base_offset.clone() + 3u32))?;

                    let gas_counter_updated_ptr = get_ptr(cell, &(base_offset.clone() + 4u32))?;
                    let revert_reason_ptr = get_ptr(cell, &(base_offset.clone() + 5u32))?;

                    // Only address_domain 0 is currently supported.
                    if addr_domain.is_zero() && gas_counter >= READ_GAS_SIM_COST.into() {
                        let value = starknet_exec_scope
                            .storage
                            .get(&addr)
                            .cloned()
                            .unwrap_or_else(|| Felt::from(0));
                        let result_ptr = get_ptr(cell, &(base_offset + 6u32))?;

                        vm.insert_value(&gas_counter_updated_ptr, gas_counter - READ_GAS_SIM_COST)?;
                        vm.insert_value(&revert_reason_ptr, Felt::from(0))?;
                        vm.insert_value(&result_ptr, value)?;
                    } else {
                        vm.insert_value(&gas_counter_updated_ptr, gas_counter)?;
                        vm.insert_value(&revert_reason_ptr, Felt::from(1))?;
                    }
                } else if selector == "call_contract".as_bytes() {
                    todo!()
                } else {
                    panic!("Unknown selector for system call!");
                }
            }
            Hint::AllocDictFeltTo { dict_manager_ptr } => {
                let (cell, base_offset) = extract_buffer(dict_manager_ptr);
                let dict_manager_address = get_ptr(cell, &base_offset)?;
                let n_dicts = vm
                    .get_integer(&(dict_manager_address + 1))?
                    .into_owned()
                    .to_usize()
                    .expect("Number of dictionaries too large.");
                let dict_infos_base = vm.get_relocatable(&(dict_manager_address))?;

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
                vm.insert_value(&(dict_infos_base + 3 * n_dicts), new_dict_segment)?;
            }
            Hint::DictFeltToRead { dict_ptr, key, value_dst } => {
                let (dict_base, dict_offset) = extract_buffer(dict_ptr);
                let dict_address = get_ptr(dict_base, &dict_offset)?;
                let key = get_val(key)?;
                let dict_manager_exec_scope = exec_scopes
                    .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                    .expect("Trying to read from a dict while dict manager was not initialized.");
                let value = dict_manager_exec_scope
                    .get_from_tracker(dict_address, &key)
                    .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
                insert_value_to_cellref!(vm, value_dst, value)?;
            }
            Hint::DictFeltToWrite { dict_ptr, key, value, prev_value_dst } => {
                let (dict_base, dict_offset) = extract_buffer(dict_ptr);
                let dict_address = get_ptr(dict_base, &dict_offset)?;
                let key = get_val(key)?;
                let value = get_val(value)?;
                let dict_manager_exec_scope = exec_scopes
                    .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                    .expect("Trying to write to a dict while dict manager was not initialized.");
                let prev_value = dict_manager_exec_scope
                    .get_from_tracker(dict_address, &key)
                    .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
                insert_value_to_cellref!(vm, prev_value_dst, prev_value)?;
                dict_manager_exec_scope.insert_to_tracker(dict_address, key, value);
            }
            Hint::GetDictIndex { dict_end_ptr, dict_index, .. } => {
                let (dict_base, dict_offset) = extract_buffer(dict_end_ptr);
                let dict_address = get_ptr(dict_base, &dict_offset)?;
                let dict_manager_exec_scope = exec_scopes
                    .get_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                    .expect("Trying to read from a dict while dict manager was not initialized.");
                let dict_infos_index = dict_manager_exec_scope.get_dict_infos_index(dict_address);
                insert_value_to_cellref!(vm, dict_index, Felt::from(dict_infos_index))?;
            }
            Hint::EnterDictSquashScope { .. } => {}
            Hint::SetDictTrackerEnd { .. } => {}
            Hint::InitSquashData { dict_accesses, n_accesses, first_key, big_keys, .. } => {
                let dict_access_size = 3;
                let rangecheck_bound = Felt::from(u128::MAX) + 1u32;

                exec_scopes.assign_or_update_variable(
                    "dict_squash_exec_scope",
                    Box::<DictSquashExecScope>::default(),
                );
                let dict_squash_exec_scope =
                    exec_scopes.get_mut_ref::<DictSquashExecScope>("dict_squash_exec_scope")?;
                let (dict_accesses_base, dict_accesses_offset) = extract_buffer(dict_accesses);
                let dict_accesses_address = get_ptr(dict_accesses_base, &dict_accesses_offset)?;
                let n_accesses = get_val(n_accesses)?
                    .to_usize()
                    .expect("Number of accesses is too large or negative.");
                for i in 0..n_accesses {
                    let current_key =
                        vm.get_integer(&(dict_accesses_address + i * dict_access_size))?;
                    dict_squash_exec_scope
                        .access_indices
                        .entry(current_key.into_owned())
                        .and_modify(|indices| indices.push(Felt::from(i)))
                        .or_insert_with(|| vec![Felt::from(i)]);
                }
                // Reverse the accesses in order to pop them in order later.
                for (_, accesses) in dict_squash_exec_scope.access_indices.iter_mut() {
                    accesses.reverse();
                }
                dict_squash_exec_scope.keys =
                    dict_squash_exec_scope.access_indices.keys().cloned().collect();
                dict_squash_exec_scope.keys.sort_by(|a, b| b.cmp(a));
                // big_keys indicates if the keys are greater than rangecheck_bound. If they are not
                // a simple range check is used instead of assert_le_felt.
                insert_value_to_cellref!(
                    vm,
                    big_keys,
                    if dict_squash_exec_scope.keys[0] < rangecheck_bound {
                        Felt::from(0)
                    } else {
                        Felt::from(1)
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
                let range_check_ptr = get_ptr(range_check_base, &range_check_offset)?;
                let current_access_index = dict_squash_exec_scope.current_access_index().unwrap();
                vm.insert_value(&range_check_ptr, current_access_index)?;
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
                        Felt::from(0)
                    } else {
                        Felt::from(1)
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
                        Felt::from(1)
                    } else {
                        Felt::from(0)
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
                let a_val = get_val(a)?;
                let b_val = get_val(b)?;
                let mut lengths_and_indices = vec![
                    (a_val.clone(), 0),
                    (b_val.clone() - a_val, 1),
                    (Felt::from(-1) - b_val, 2),
                ];
                lengths_and_indices.sort();
                exec_scopes
                    .assign_or_update_variable("excluded_arc", Box::new(lengths_and_indices[2].1));
                // ceil((PRIME / 2) / 2 ** 128).
                let prime_over_2_high = 3544607988759775765608368578435044694_u128;
                // ceil((PRIME / 3) / 2 ** 128).
                let prime_over_3_high = 5316911983139663648412552867652567041_u128;
                let (range_check_base, range_check_offset) = extract_buffer(range_check_ptr);
                let range_check_ptr = get_ptr(range_check_base, &range_check_offset)?;
                vm.insert_value(
                    &range_check_ptr,
                    Felt::from(lengths_and_indices[0].0.to_biguint() % prime_over_3_high),
                )?;
                vm.insert_value(
                    &(range_check_ptr + 1),
                    Felt::from(lengths_and_indices[0].0.to_biguint() / prime_over_3_high),
                )?;
                vm.insert_value(
                    &(range_check_ptr + 2),
                    Felt::from(lengths_and_indices[1].0.to_biguint() % prime_over_2_high),
                )?;
                vm.insert_value(
                    &(range_check_ptr + 3),
                    Felt::from(lengths_and_indices[1].0.to_biguint() / prime_over_2_high),
                )?;
            }
            Hint::AssertLeIsFirstArcExcluded { skip_exclude_a_flag } => {
                let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
                insert_value_to_cellref!(
                    vm,
                    skip_exclude_a_flag,
                    if excluded_arc != 0 { Felt::from(1) } else { Felt::from(0) }
                )?;
            }
            Hint::AssertLeIsSecondArcExcluded { skip_exclude_b_minus_a } => {
                let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
                insert_value_to_cellref!(
                    vm,
                    skip_exclude_b_minus_a,
                    if excluded_arc != 1 { Felt::from(1) } else { Felt::from(0) }
                )?;
            }
            Hint::AssertLeAssertThirdArcExcluded => {}
            Hint::DebugPrint { start, end } => {
                let as_relocatable = |value| {
                    let (base, offset) = extract_buffer(value);
                    get_ptr(base, &offset)
                };
                let mut curr = as_relocatable(start)?;
                let end = as_relocatable(end)?;
                while curr != end {
                    let value = vm.get_integer(&curr)?;
                    if let Some(shortstring) = as_cairo_short_string(&value) {
                        print!("'{shortstring}' (raw: {value}), ",);
                    } else {
                        print!("{value}, ");
                    }
                    curr = curr.add_int(&1.into())?;
                }
                println!();
            }
            Hint::AllocConstantSize { size, dst } => {
                let object_size = get_val(size)?.to_usize().expect("Object size too large.");
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

/// Extracts a parameter assumed to be a buffer.
fn extract_buffer(buffer: &ResOperand) -> (&CellRef, Felt) {
    let (cell, base_offset) = match buffer {
        ResOperand::Deref(cell) => (cell, 0.into()),
        ResOperand::BinOp(BinOpOperand { op: Operation::Add, a, b }) => {
            (a, extract_matches!(b, DerefOrImmediate::Immediate).clone().into())
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
    builtins: Vec<String>,
    additional_initialization: fn(
        context: RunFunctionContext<'_>,
    ) -> Result<(), Box<VirtualMachineError>>,
) -> Result<(Vec<Option<Felt>>, usize), Box<VirtualMachineError>> {
    let data: Vec<MaybeRelocatable> = instructions
        .clone()
        .flat_map(|inst| inst.assemble().encode())
        .map(Felt::from)
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
    let mut vm = VirtualMachine::new(true, vec![]);

    let end = runner.initialize(&mut vm).map_err(VirtualMachineError::from).map_err(Box::new)?;

    additional_initialization(RunFunctionContext { vm: &mut vm, data_len })?;

    runner.run_until_pc(end, &mut vm, &mut hint_processor)?;
    runner.end_run(true, false, &mut vm, &mut hint_processor).map_err(Box::new)?;
    runner.relocate(&mut vm).map_err(VirtualMachineError::from).map_err(Box::new)?;
    Ok((runner.relocated_memory, runner.relocated_trace.unwrap().last().unwrap().ap))
}
