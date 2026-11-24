use std::any::Any;
use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_casm::hints::{ExternalHint, Hint, StarknetHint};
use cairo_lang_casm::operand::{CellRef, ResOperand};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_utils::bigint::BigIntAsHex;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_vm::hint_processor::hint_processor_definition::{
    HintProcessor, HintProcessorLogic, HintReference,
};
use cairo_vm::serde::deserialize_program::ApTracking;
use cairo_vm::types::exec_scope::ExecutionScopes;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use cairo_vm::vm::runners::cairo_runner::{ResourceTracker, RunResources};
use cairo_vm::vm::vm_core::VirtualMachine;
use itertools::{Itertools, chain};
use lambdaworks_math::cyclic_group::IsGroup;
use lambdaworks_math::elliptic_curve::short_weierstrass::curves::secp256k1::curve::Secp256k1Curve;
use lambdaworks_math::elliptic_curve::short_weierstrass::curves::secp256r1::curve::Secp256r1Curve;
use lambdaworks_math::elliptic_curve::short_weierstrass::point::ShortWeierstrassProjectivePoint;
use lambdaworks_math::elliptic_curve::short_weierstrass::traits::IsShortWeierstrass;
use lambdaworks_math::field::element::FieldElement;
use lambdaworks_math::field::traits::{IsField, IsPrimeField};
use lambdaworks_math::unsigned_integer::element::U256;
use num_traits::{ToPrimitive, Zero};
use starknet_types_core::felt::Felt as Felt252;

use self::contract_address::calculate_contract_address;
use crate::{Arg, RunResultValue, SierraCasmRunner, StarknetExecutionResources, args_size};

#[cfg(test)]
mod test;

mod contract_address;

pub use cairo_lang_casm_run::*;

/// Helper object to allocate and track Secp256k1 elliptic curve points.
#[derive(Default)]
struct Secp256k1ExecutionScope {
    /// All elliptic curve points provided by the secp256k1 syscalls.
    /// The id of a point is the index in the vector.
    ec_points: Vec<ShortWeierstrassProjectivePoint<Secp256k1Curve>>,
}

/// Helper object to allocate and track Secp256r1 elliptic curve points.
#[derive(Default)]
struct Secp256r1ExecutionScope {
    /// All elliptic curve points provided by the secp256r1 syscalls.
    /// The id of a point is the index in the vector.
    ec_points: Vec<ShortWeierstrassProjectivePoint<Secp256r1Curve>>,
}

/// HintProcessor for Cairo compiler hints.
pub struct CairoHintProcessor<'a> {
    /// The Cairo runner.
    pub runner: Option<&'a SierraCasmRunner>,
    /// The user arguments for the run.
    ///
    /// We have a vector of arguments per parameter, as a parameter type may be composed of
    /// several user args.
    pub user_args: Vec<Vec<Arg>>,
    /// A mapping from a string that represents a hint to the hint object.
    pub string_to_hint: UnorderedHashMap<String, Hint>,
    /// The Starknet state.
    pub starknet_state: StarknetState,
    /// Maintains the resources of the run.
    pub run_resources: RunResources,
    /// Resources used during syscalls - does not include resources used during the current VM run.
    /// At the end of the run - adding both would result in the actual expected resource usage.
    pub syscalls_used_resources: StarknetExecutionResources,
    /// Avoid allocating memory segments so finalization of the segment arena may not occur.
    pub no_temporary_segments: bool,
    /// A set of markers created by the run.
    pub markers: Vec<Vec<Felt252>>,
    /// The traceback set by a panic trace hint call.
    pub panic_traceback: Vec<(Relocatable, Relocatable)>,
}

// Log type signature
type Log = (Vec<Felt252>, Vec<Felt252>);

// L2 to L1 message type signature
type L2ToL1Message = (Felt252, Vec<Felt252>);

/// Execution scope for Starknet-related data.
/// All values will be 0 by default if not set up by the test.
#[derive(Clone, Default)]
pub struct StarknetState {
    /// The values of addresses in the simulated storage per contract.
    storage: UnorderedHashMap<Felt252, UnorderedHashMap<Felt252, Felt252>>,
    /// A mapping from contract address to class hash.
    deployed_contracts: UnorderedHashMap<Felt252, Felt252>,
    /// A mapping from contract address to logs.
    logs: UnorderedHashMap<Felt252, ContractLogs>,
    /// The simulated execution info.
    exec_info: ExecutionInfo,
    /// A mock history, mapping block number to the block hash.
    block_hash: UnorderedHashMap<u64, Felt252>,
}

/// Object storing logs for a contract.
#[derive(Clone, Default)]
struct ContractLogs {
    /// Events.
    events: VecDeque<Log>,
    /// Messages sent to L1.
    l2_to_l1_messages: VecDeque<L2ToL1Message>,
}

/// Copy of the Cairo `ExecutionInfo` struct.
#[derive(Clone, Default)]
struct ExecutionInfo {
    block_info: BlockInfo,
    tx_info: TxInfo,
    call_info: CallInfo,
}

/// Originally flat in `ExecutionInfo`, separated here for convenience.
#[derive(Clone, Default)]
struct CallInfo {
    caller_address: Felt252,
    contract_address: Felt252,
    entry_point_selector: Felt252,
}

/// Copy of the Cairo `BlockInfo` struct.
#[derive(Clone, Default)]
struct BlockInfo {
    block_number: Felt252,
    block_timestamp: Felt252,
    sequencer_address: Felt252,
}

/// Copy of the Cairo `TxInfo` struct.
#[derive(Clone, Default)]
struct TxInfo {
    version: Felt252,
    account_contract_address: Felt252,
    max_fee: Felt252,
    signature: Vec<Felt252>,
    transaction_hash: Felt252,
    chain_id: Felt252,
    nonce: Felt252,
    resource_bounds: Vec<ResourceBounds>,
    tip: Felt252,
    paymaster_data: Vec<Felt252>,
    nonce_data_availability_mode: Felt252,
    fee_data_availability_mode: Felt252,
    account_deployment_data: Vec<Felt252>,
    proof_facts: Vec<Felt252>,
}

/// Copy of the Cairo `ResourceBounds` struct.
#[derive(Clone, Default)]
struct ResourceBounds {
    resource: Felt252,
    max_amount: Felt252,
    max_price_per_unit: Felt252,
}

/// Resulting options from a syscall.
enum SyscallResult {
    /// The syscall was successful.
    Success(Vec<MaybeRelocatable>),
    /// The syscall failed, with the revert reason.
    Failure(Vec<Felt252>),
}

macro_rules! fail_syscall {
    ([$reason1:expr, $reason2:expr]) => {
        return Ok(SyscallResult::Failure(vec![
            Felt252::from_bytes_be_slice($reason1),
            Felt252::from_bytes_be_slice($reason2),
        ]))
    };
    ($reason:expr) => {
        return Ok(SyscallResult::Failure(vec![Felt252::from_bytes_be_slice($reason)]))
    };
    ($existing:ident, $reason:expr) => {
        $existing.push(Felt252::from_bytes_be_slice($reason));
        return Ok(SyscallResult::Failure($existing))
    };
}

/// Gas costs for syscalls.
/// Mostly duplication of:
/// `https://github.com/starkware-libs/blockifier/blob/main/crates/blockifier/src/abi/constants.rs`.
mod gas_costs {
    const STEP: usize = 100;
    const RANGE_CHECK: usize = 70;
    const BITWISE: usize = 594;

    /// Entry point initial gas cost enforced by the compiler.
    /// Should match `ENTRY_POINT_COST` at `crates/cairo-lang-starknet/src/casm_contract_class.rs`.
    pub const ENTRY_POINT_INITIAL_BUDGET: usize = 100 * STEP;
    /// OS gas costs.
    const ENTRY_POINT: usize = ENTRY_POINT_INITIAL_BUDGET + 500 * STEP;
    // The required gas for each syscall minus the base amount that was pre-charged (by the
    // compiler).
    pub const CALL_CONTRACT: usize = 10 * STEP + ENTRY_POINT;
    pub const DEPLOY: usize = 200 * STEP + ENTRY_POINT;
    pub const EMIT_EVENT: usize = 10 * STEP;
    pub const GET_BLOCK_HASH: usize = 50 * STEP;
    pub const GET_EXECUTION_INFO: usize = 10 * STEP;
    pub const GET_CLASS_HASH_AT: usize = 50 * STEP;
    pub const KECCAK: usize = 0;
    pub const KECCAK_ROUND_COST: usize = 180000;
    pub const SHA256_PROCESS_BLOCK: usize = 1852 * STEP + 65 * RANGE_CHECK + 1115 * BITWISE;
    pub const SHA512_PROCESS_BLOCK: usize = 4733 * STEP + 65 * RANGE_CHECK + 3320 * BITWISE;
    pub const LIBRARY_CALL: usize = CALL_CONTRACT;
    pub const REPLACE_CLASS: usize = 50 * STEP;
    pub const SECP256K1_ADD: usize = 254 * STEP + 29 * RANGE_CHECK;
    pub const SECP256K1_GET_POINT_FROM_X: usize = 260 * STEP + 29 * RANGE_CHECK;
    pub const SECP256K1_GET_XY: usize = 24 * STEP + 9 * RANGE_CHECK;
    pub const SECP256K1_MUL: usize = 121810 * STEP + 10739 * RANGE_CHECK;
    pub const SECP256K1_NEW: usize = 340 * STEP + 36 * RANGE_CHECK;
    pub const SECP256R1_ADD: usize = 254 * STEP + 29 * RANGE_CHECK;
    pub const SECP256R1_GET_POINT_FROM_X: usize = 260 * STEP + 29 * RANGE_CHECK;
    pub const SECP256R1_GET_XY: usize = 24 * STEP + 9 * RANGE_CHECK;
    pub const SECP256R1_MUL: usize = 121810 * STEP + 10739 * RANGE_CHECK;
    pub const SECP256R1_NEW: usize = 340 * STEP + 36 * RANGE_CHECK;
    pub const SEND_MESSAGE_TO_L1: usize = 50 * STEP;
    pub const STORAGE_READ: usize = 50 * STEP;
    pub const STORAGE_WRITE: usize = 50 * STEP;
}

/// Deducts gas from the given gas counter or fails the syscall if there is not enough gas.
macro_rules! deduct_gas {
    ($gas:ident, $amount:ident) => {
        if *$gas < gas_costs::$amount {
            fail_syscall!(b"Syscall out of gas");
        }
        *$gas -= gas_costs::$amount;
    };
}

impl HintProcessorLogic for CairoHintProcessor<'_> {
    /// Trait function to execute a given hint in the hint processor.
    fn execute_hint(
        &mut self,
        vm: &mut VirtualMachine,
        exec_scopes: &mut ExecutionScopes,
        hint_data: &Box<dyn Any>,
    ) -> Result<(), HintError> {
        let hint = hint_data.downcast_ref::<Hint>().ok_or(HintError::WrongHintData)?;
        let hint = match hint {
            Hint::Starknet(hint) => hint,
            Hint::Core(core_hint_base) => {
                return execute_core_hint_base(
                    vm,
                    exec_scopes,
                    core_hint_base,
                    self.no_temporary_segments,
                );
            }
            Hint::External(hint) => {
                return self.execute_external_hint(vm, hint);
            }
        };
        match hint {
            StarknetHint::SystemCall { system } => {
                self.execute_syscall(system, vm, exec_scopes)?;
            }
            StarknetHint::Cheatcode {
                selector,
                input_start,
                input_end,
                output_start,
                output_end,
            } => {
                self.execute_cheatcode(
                    selector,
                    [input_start, input_end],
                    [output_start, output_end],
                    vm,
                    exec_scopes,
                )?;
            }
        };
        Ok(())
    }

    /// Trait function to store hint in the hint processor by string.
    #[expect(clippy::disallowed_types)]
    fn compile_hint(
        &self,
        hint_code: &str,
        _ap_tracking_data: &ApTracking,
        _reference_ids: &std::collections::HashMap<String, usize>,
        _references: &[HintReference],
        _accessible_scopes: &[String],
        _constants: Arc<std::collections::HashMap<String, Felt252>>,
    ) -> Result<Box<dyn Any>, VirtualMachineError> {
        Ok(Box::new(self.string_to_hint[hint_code].clone()))
    }
}

impl ResourceTracker for CairoHintProcessor<'_> {
    fn consumed(&self) -> bool {
        self.run_resources.consumed()
    }

    fn consume_step(&mut self) {
        self.run_resources.consume_step()
    }

    fn get_n_steps(&self) -> Option<usize> {
        self.run_resources.get_n_steps()
    }

    fn run_resources(&self) -> &RunResources {
        self.run_resources.run_resources()
    }
}

pub trait StarknetHintProcessor: HintProcessor {
    /// Take [`StarknetState`] out of this hint processor, resetting own state.
    fn take_starknet_state(&mut self) -> StarknetState;
    /// Take [`StarknetExecutionResources`] out of this hint processor, resetting own state.
    fn take_syscalls_used_resources(&mut self) -> StarknetExecutionResources;
}

impl StarknetHintProcessor for CairoHintProcessor<'_> {
    fn take_starknet_state(&mut self) -> StarknetState {
        std::mem::take(&mut self.starknet_state)
    }

    fn take_syscalls_used_resources(&mut self) -> StarknetExecutionResources {
        std::mem::take(&mut self.syscalls_used_resources)
    }
}

impl CairoHintProcessor<'_> {
    /// Executes a syscall.
    fn execute_syscall(
        &mut self,
        system: &ResOperand,
        vm: &mut VirtualMachine,
        exec_scopes: &mut ExecutionScopes,
    ) -> Result<(), HintError> {
        let system_ptr = extract_relocatable(vm, system)?;
        let mut system_buffer = MemBuffer::new(vm, system_ptr);
        let selector = system_buffer.next_felt252()?.to_bytes_be();
        let mut gas_counter = system_buffer.next_usize()?;
        let mut execute_handle_helper =
            |handler: &mut dyn FnMut(
                // The syscall buffer.
                &mut MemBuffer<'_>,
                // The gas counter.
                &mut usize,
            ) -> Result<SyscallResult, HintError>| {
                match handler(&mut system_buffer, &mut gas_counter)? {
                    SyscallResult::Success(values) => {
                        system_buffer.write(gas_counter)?;
                        system_buffer.write(Felt252::from(0))?;
                        system_buffer.write_data(values.into_iter())?;
                    }
                    SyscallResult::Failure(revert_reason) => {
                        system_buffer.write(gas_counter)?;
                        system_buffer.write(Felt252::from(1))?;
                        system_buffer.write_arr(revert_reason.into_iter())?;
                    }
                }
                Ok(())
            };
        let selector = std::str::from_utf8(&selector).unwrap().trim_start_matches('\0');
        *self.syscalls_used_resources.syscalls.entry(selector.into()).or_default() += 1;
        match selector {
            "StorageWrite" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.storage_write(
                    gas_counter,
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_felt252()?.into_owned(),
                )
            }),
            "StorageRead" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.storage_read(
                    gas_counter,
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_felt252()?.into_owned(),
                )
            }),
            "GetBlockHash" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.get_block_hash(gas_counter, system_buffer.next_u64()?)
            }),
            "GetExecutionInfo" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.get_execution_info(gas_counter, system_buffer)
            }),
            "EmitEvent" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.emit_event(gas_counter, system_buffer.next_arr()?, system_buffer.next_arr()?)
            }),
            "SendMessageToL1" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.send_message_to_l1(
                    gas_counter,
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_arr()?,
                )
            }),
            "Keccak" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                keccak(gas_counter, system_buffer.next_arr()?)
            }),
            "Sha256ProcessBlock" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                sha_256_process_block(
                    gas_counter,
                    system_buffer.next_fixed_size_arr_pointer(8)?,
                    system_buffer.next_fixed_size_arr_pointer(16)?,
                    exec_scopes,
                    system_buffer,
                )
            }),
            "Sha512ProcessBlock" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                sha_512_process_block(
                    gas_counter,
                    system_buffer.next_fixed_size_arr_pointer(8)?,
                    system_buffer.next_fixed_size_arr_pointer(16)?,
                    exec_scopes,
                    system_buffer,
                )
            }),
            "Secp256k1New" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256k1_new(
                    gas_counter,
                    system_buffer.next_u256()?,
                    system_buffer.next_u256()?,
                    exec_scopes,
                )
            }),
            "Secp256k1Add" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256k1_add(
                    gas_counter,
                    exec_scopes,
                    system_buffer.next_usize()?,
                    system_buffer.next_usize()?,
                )
            }),
            "Secp256k1Mul" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256k1_mul(
                    gas_counter,
                    system_buffer.next_usize()?,
                    system_buffer.next_u256()?,
                    exec_scopes,
                )
            }),
            "Secp256k1GetPointFromX" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256k1_get_point_from_x(
                    gas_counter,
                    system_buffer.next_u256()?,
                    system_buffer.next_bool()?,
                    exec_scopes,
                )
            }),
            "Secp256k1GetXy" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256k1_get_xy(gas_counter, system_buffer.next_usize()?, exec_scopes)
            }),
            "Secp256r1New" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256r1_new(
                    gas_counter,
                    system_buffer.next_u256()?,
                    system_buffer.next_u256()?,
                    exec_scopes,
                )
            }),
            "Secp256r1Add" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256r1_add(
                    gas_counter,
                    exec_scopes,
                    system_buffer.next_usize()?,
                    system_buffer.next_usize()?,
                )
            }),
            "Secp256r1Mul" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256r1_mul(
                    gas_counter,
                    system_buffer.next_usize()?,
                    system_buffer.next_u256()?,
                    exec_scopes,
                )
            }),
            "Secp256r1GetPointFromX" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256r1_get_point_from_x(
                    gas_counter,
                    system_buffer.next_u256()?,
                    system_buffer.next_bool()?,
                    exec_scopes,
                )
            }),
            "Secp256r1GetXy" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                secp256r1_get_xy(gas_counter, system_buffer.next_usize()?, exec_scopes)
            }),
            "Deploy" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.deploy(
                    gas_counter,
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_arr()?,
                    system_buffer.next_bool()?,
                    system_buffer,
                )
            }),
            "CallContract" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.call_contract(
                    gas_counter,
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_arr()?,
                    system_buffer,
                )
            }),
            "LibraryCall" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.library_call(
                    gas_counter,
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_felt252()?.into_owned(),
                    system_buffer.next_arr()?,
                    system_buffer,
                )
            }),
            "ReplaceClass" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.replace_class(gas_counter, system_buffer.next_felt252()?.into_owned())
            }),
            "GetClassHashAt" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                self.get_class_hash_at(gas_counter, system_buffer.next_felt252()?.into_owned())
            }),
            "MetaTxV0" => execute_handle_helper(&mut |_system_buffer, _gas_counter| {
                fail_syscall!(b"Meta tx is not supported.")
            }),
            _ => panic!("Unknown selector for system call!"),
        }
    }

    /// Executes the `storage_write_syscall` syscall.
    fn storage_write(
        &mut self,
        gas_counter: &mut usize,
        addr_domain: Felt252,
        addr: Felt252,
        value: Felt252,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, STORAGE_WRITE);
        if !addr_domain.is_zero() {
            // Only address_domain 0 is currently supported.
            fail_syscall!(b"Unsupported address domain");
        }
        let contract = self.starknet_state.exec_info.call_info.contract_address;
        self.starknet_state.storage.entry(contract).or_default().insert(addr, value);
        Ok(SyscallResult::Success(vec![]))
    }

    /// Executes the `storage_read_syscall` syscall.
    fn storage_read(
        &mut self,
        gas_counter: &mut usize,
        addr_domain: Felt252,
        addr: Felt252,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, STORAGE_READ);
        if !addr_domain.is_zero() {
            // Only address_domain 0 is currently supported.
            fail_syscall!(b"Unsupported address domain");
        }
        let value = self
            .starknet_state
            .storage
            .get(&self.starknet_state.exec_info.call_info.contract_address)
            .and_then(|contract_storage| contract_storage.get(&addr))
            .cloned()
            .unwrap_or_else(|| Felt252::from(0));
        Ok(SyscallResult::Success(vec![value.into()]))
    }

    /// Executes the `get_block_hash_syscall` syscall.
    fn get_block_hash(
        &mut self,
        gas_counter: &mut usize,
        block_number: u64,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, GET_BLOCK_HASH);
        if let Some(block_hash) = self.starknet_state.block_hash.get(&block_number) {
            Ok(SyscallResult::Success(vec![block_hash.into()]))
        } else {
            fail_syscall!(b"GET_BLOCK_HASH_NOT_SET");
        }
    }

    /// Executes the `get_execution_info_syscall` syscall.
    fn get_execution_info(
        &mut self,
        gas_counter: &mut usize,
        vm: &mut dyn VMWrapper,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, GET_EXECUTION_INFO);
        let exec_info = &self.starknet_state.exec_info;
        let block_info = &exec_info.block_info;
        let tx_info = &exec_info.tx_info;
        let mut res_segment = MemBuffer::new_segment(vm);
        let signature_start = res_segment.ptr;
        res_segment.write_data(tx_info.signature.iter().cloned())?;
        let signature_end = res_segment.ptr;
        let resource_bounds_start = res_segment.ptr;
        for value in &tx_info.resource_bounds {
            res_segment.write(value.resource)?;
            res_segment.write(value.max_amount)?;
            res_segment.write(value.max_price_per_unit)?;
        }
        let resource_bounds_end = res_segment.ptr;
        let paymaster_data_start = res_segment.ptr;
        res_segment.write_data(tx_info.paymaster_data.iter().cloned())?;
        let paymaster_data_end = res_segment.ptr;
        let account_deployment_data_start = res_segment.ptr;
        res_segment.write_data(tx_info.account_deployment_data.iter().cloned())?;
        let account_deployment_data_end = res_segment.ptr;
        let proof_facts_start = res_segment.ptr;
        res_segment.write_data(tx_info.proof_facts.iter().cloned())?;
        let proof_facts_end = res_segment.ptr;
        let tx_info_ptr = res_segment.ptr;
        res_segment.write(tx_info.version)?;
        res_segment.write(tx_info.account_contract_address)?;
        res_segment.write(tx_info.max_fee)?;
        res_segment.write(signature_start)?;
        res_segment.write(signature_end)?;
        res_segment.write(tx_info.transaction_hash)?;
        res_segment.write(tx_info.chain_id)?;
        res_segment.write(tx_info.nonce)?;
        res_segment.write(resource_bounds_start)?;
        res_segment.write(resource_bounds_end)?;
        res_segment.write(tx_info.tip)?;
        res_segment.write(paymaster_data_start)?;
        res_segment.write(paymaster_data_end)?;
        res_segment.write(tx_info.nonce_data_availability_mode)?;
        res_segment.write(tx_info.fee_data_availability_mode)?;
        res_segment.write(account_deployment_data_start)?;
        res_segment.write(account_deployment_data_end)?;
        res_segment.write(proof_facts_start)?;
        res_segment.write(proof_facts_end)?;
        let block_info_ptr = res_segment.ptr;
        res_segment.write(block_info.block_number)?;
        res_segment.write(block_info.block_timestamp)?;
        res_segment.write(block_info.sequencer_address)?;
        let exec_info_ptr = res_segment.ptr;
        res_segment.write(block_info_ptr)?;
        res_segment.write(tx_info_ptr)?;
        res_segment.write(exec_info.call_info.caller_address)?;
        res_segment.write(exec_info.call_info.contract_address)?;
        res_segment.write(exec_info.call_info.entry_point_selector)?;
        Ok(SyscallResult::Success(vec![exec_info_ptr.into()]))
    }

    /// Executes the `emit_event_syscall` syscall.
    fn emit_event(
        &mut self,
        gas_counter: &mut usize,
        keys: Vec<Felt252>,
        data: Vec<Felt252>,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, EMIT_EVENT);
        let contract = self.starknet_state.exec_info.call_info.contract_address;
        self.starknet_state.logs.entry(contract).or_default().events.push_back((keys, data));
        Ok(SyscallResult::Success(vec![]))
    }

    /// Executes the `send_message_to_l1_syscall` syscall.
    fn send_message_to_l1(
        &mut self,
        gas_counter: &mut usize,
        to_address: Felt252,
        payload: Vec<Felt252>,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, SEND_MESSAGE_TO_L1);
        let contract = self.starknet_state.exec_info.call_info.contract_address;
        self.starknet_state
            .logs
            .entry(contract)
            .or_default()
            .l2_to_l1_messages
            .push_back((to_address, payload));
        Ok(SyscallResult::Success(vec![]))
    }

    /// Executes the `deploy_syscall` syscall.
    fn deploy(
        &mut self,
        gas_counter: &mut usize,
        class_hash: Felt252,
        contract_address_salt: Felt252,
        calldata: Vec<Felt252>,
        deploy_from_zero: bool,
        vm: &mut dyn VMWrapper,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, DEPLOY);

        let caller_address = self.starknet_state.exec_info.call_info.contract_address;
        // Assign the Starknet address of the contract.
        let deployed_address = calculate_contract_address(
            &contract_address_salt,
            &class_hash,
            &calldata,
            &if deploy_from_zero { Felt252::zero() } else { caller_address },
        );

        // Prepare runner for running the constructor.
        let runner = self.runner.expect("Runner is needed for starknet.");
        let Some(contract_info) = runner.starknet_contracts_info.get(&class_hash) else {
            fail_syscall!(b"CLASS_HASH_NOT_FOUND");
        };

        // Set the class hash of the deployed contract before executing the constructor,
        // as the constructor could make an external call to this address.
        if self.starknet_state.deployed_contracts.insert(deployed_address, class_hash).is_some() {
            fail_syscall!(b"CONTRACT_ALREADY_DEPLOYED");
        }

        // Call constructor if it exists.
        let (res_data_start, res_data_end) = if let Some(constructor) = &contract_info.constructor {
            let call_info = CallInfo {
                contract_address: deployed_address,
                caller_address,
                // starknet_keccak of 'constructor'.
                entry_point_selector: Felt252::from_hex_unchecked(
                    "0x28ffe4ff0f226a9107253e17a904099aa4f63a02a5621de0576e5aa71bc5194",
                ),
            };
            let res =
                self.call_entry_point(gas_counter, runner, constructor, calldata, vm, call_info);
            match res {
                Ok(value) => value,
                Err(mut revert_reason) => {
                    self.starknet_state.deployed_contracts.remove(&deployed_address);
                    fail_syscall!(revert_reason, b"CONSTRUCTOR_FAILED");
                }
            }
        } else if calldata.is_empty() {
            (Relocatable::from((0, 0)), Relocatable::from((0, 0)))
        } else {
            // Remove the contract from the deployed contracts,
            // since it failed to deploy.
            self.starknet_state.deployed_contracts.remove(&deployed_address);
            fail_syscall!(b"INVALID_CALLDATA_LEN");
        };

        Ok(SyscallResult::Success(vec![
            deployed_address.into(),
            res_data_start.into(),
            res_data_end.into(),
        ]))
    }

    /// Executes the `call_contract_syscall` syscall.
    fn call_contract(
        &mut self,
        gas_counter: &mut usize,
        contract_address: Felt252,
        selector: Felt252,
        calldata: Vec<Felt252>,
        vm: &mut dyn VMWrapper,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, CALL_CONTRACT);

        // Get the class hash of the contract.
        let Some(class_hash) = self.starknet_state.deployed_contracts.get(&contract_address) else {
            fail_syscall!([b"CONTRACT_NOT_DEPLOYED", b"ENTRYPOINT_FAILED"]);
        };

        // Prepare runner for running the ctor.
        let runner = self.runner.expect("Runner is needed for starknet.");
        let contract_info = runner
            .starknet_contracts_info
            .get(class_hash)
            .expect("Deployed contract not found in registry.");

        // Call the function.
        let Some(entry_point) = contract_info.externals.get(&selector) else {
            fail_syscall!([b"ENTRYPOINT_NOT_FOUND", b"ENTRYPOINT_FAILED"]);
        };

        let call_info = CallInfo {
            contract_address,
            caller_address: self.starknet_state.exec_info.call_info.contract_address,
            entry_point_selector: selector,
        };
        let res = self.call_entry_point(gas_counter, runner, entry_point, calldata, vm, call_info);

        match res {
            Ok((res_data_start, res_data_end)) => {
                Ok(SyscallResult::Success(vec![res_data_start.into(), res_data_end.into()]))
            }
            Err(mut revert_reason) => {
                fail_syscall!(revert_reason, b"ENTRYPOINT_FAILED");
            }
        }
    }

    /// Executes the `library_call_syscall` syscall.
    fn library_call(
        &mut self,
        gas_counter: &mut usize,
        class_hash: Felt252,
        selector: Felt252,
        calldata: Vec<Felt252>,
        vm: &mut dyn VMWrapper,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, LIBRARY_CALL);
        // Prepare runner for running the call.
        let runner = self.runner.expect("Runner is needed for starknet.");
        let Some(contract_info) = runner.starknet_contracts_info.get(&class_hash) else {
            fail_syscall!(b"CLASS_HASH_NOT_DECLARED")
        };

        // Call the function.
        let Some(entry_point) = contract_info.externals.get(&selector) else {
            fail_syscall!([b"ENTRYPOINT_NOT_FOUND", b"ENTRYPOINT_FAILED"]);
        };
        // A library call runs in the caller's context; only the entry point selector changes.
        let call_info = CallInfo {
            entry_point_selector: selector,
            ..self.starknet_state.exec_info.call_info.clone()
        };
        let res = self.call_entry_point(gas_counter, runner, entry_point, calldata, vm, call_info);
        match res {
            Ok((res_data_start, res_data_end)) => {
                Ok(SyscallResult::Success(vec![res_data_start.into(), res_data_end.into()]))
            }
            Err(mut revert_reason) => {
                fail_syscall!(revert_reason, b"ENTRYPOINT_FAILED");
            }
        }
    }

    /// Executes the `replace_class_syscall` syscall.
    fn replace_class(
        &mut self,
        gas_counter: &mut usize,
        new_class: Felt252,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, REPLACE_CLASS);
        // Validating the class hash was declared as one of the Starknet contracts.
        if !self
            .runner
            .expect("Runner is needed for starknet.")
            .starknet_contracts_info
            .contains_key(&new_class)
        {
            fail_syscall!(b"CLASS_HASH_NOT_FOUND");
        };
        let address = self.starknet_state.exec_info.call_info.contract_address;
        self.starknet_state.deployed_contracts.insert(address, new_class);
        Ok(SyscallResult::Success(vec![]))
    }

    /// Executes the `get_class_hash_at_syscall` syscall.
    fn get_class_hash_at(
        &mut self,
        gas_counter: &mut usize,
        contract_address: Felt252,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, GET_CLASS_HASH_AT);
        // Look up the class hash of the deployed contract at the given address.
        let class_hash = self
            .starknet_state
            .deployed_contracts
            .get(&contract_address)
            .cloned()
            .unwrap_or_else(Felt252::zero);
        Ok(SyscallResult::Success(vec![MaybeRelocatable::Int(class_hash)]))
    }

    /// Executes the entry point with the given calldata.
    fn call_entry_point(
        &mut self,
        gas_counter: &mut usize,
        runner: &SierraCasmRunner,
        entry_point: &FunctionId,
        calldata: Vec<Felt252>,
        vm: &mut dyn VMWrapper,
        call_info: CallInfo,
    ) -> Result<(Relocatable, Relocatable), Vec<Felt252>> {
        let function = runner
            .builder
            .registry()
            .get_function(entry_point)
            .expect("Entrypoint exists, but not found.");
        let mut starknet_state = self.starknet_state.clone();
        let old_call_info = std::mem::replace(&mut starknet_state.exec_info.call_info, call_info);
        let res = runner
            .run_function_with_starknet_context(
                function,
                vec![Arg::Array(calldata.into_iter().map(Arg::Value).collect())],
                // The costs of the relevant syscall include `ENTRY_POINT_INITIAL_BUDGET` so we
                // need to refund it here before running the entry point to avoid double charging.
                Some(*gas_counter + gas_costs::ENTRY_POINT_INITIAL_BUDGET),
                starknet_state,
            )
            .expect("Internal runner error.");
        self.syscalls_used_resources += res.used_resources;
        *gas_counter = res.gas_counter.unwrap().to_usize().unwrap();
        match res.value {
            RunResultValue::Success(value) => {
                self.starknet_state = res.starknet_state;
                self.starknet_state.exec_info.call_info = old_call_info;
                Ok(segment_with_data(vm, read_array_result_as_vec(&res.memory, &value).into_iter())
                    .expect("failed to allocate segment"))
            }
            RunResultValue::Panic(panic_data) => Err(panic_data),
        }
    }

    /// Executes a cheatcode.
    fn execute_cheatcode(
        &mut self,
        selector: &BigIntAsHex,
        [input_start, input_end]: [&ResOperand; 2],
        [output_start, output_end]: [&CellRef; 2],
        vm: &mut VirtualMachine,
        _exec_scopes: &mut ExecutionScopes,
    ) -> Result<(), HintError> {
        // Parse the selector.
        let selector = &selector.value.to_bytes_be().1;
        let selector = std::str::from_utf8(selector).map_err(|_| {
            HintError::CustomHint(Box::from("failed to parse selector".to_string()))
        })?;

        // Extract the inputs.
        let input_start = extract_relocatable(vm, input_start)?;
        let input_end = extract_relocatable(vm, input_end)?;
        let inputs = vm_get_range(vm, input_start, input_end)?;

        // Helper for all the instances requiring only a single input.
        let as_single_input = |inputs| {
            vec_as_array(inputs, || {
                format!(
                    "`{selector}` cheatcode invalid args: pass span of an array with exactly one \
                     element",
                )
            })
            .map(|[value]| value)
        };

        let mut res_segment = MemBuffer::new_segment(vm);
        let res_segment_start = res_segment.ptr;
        match selector {
            "set_sequencer_address" => {
                self.starknet_state.exec_info.block_info.sequencer_address =
                    as_single_input(inputs)?;
            }
            "set_block_number" => {
                self.starknet_state.exec_info.block_info.block_number = as_single_input(inputs)?;
            }
            "set_block_timestamp" => {
                self.starknet_state.exec_info.block_info.block_timestamp = as_single_input(inputs)?;
            }
            "set_caller_address" => {
                self.starknet_state.exec_info.call_info.caller_address = as_single_input(inputs)?;
            }
            "set_contract_address" => {
                self.starknet_state.exec_info.call_info.contract_address = as_single_input(inputs)?;
            }
            "set_version" => {
                self.starknet_state.exec_info.tx_info.version = as_single_input(inputs)?;
            }
            "set_account_contract_address" => {
                self.starknet_state.exec_info.tx_info.account_contract_address =
                    as_single_input(inputs)?;
            }
            "set_max_fee" => {
                self.starknet_state.exec_info.tx_info.max_fee = as_single_input(inputs)?;
            }
            "set_transaction_hash" => {
                self.starknet_state.exec_info.tx_info.transaction_hash = as_single_input(inputs)?;
            }
            "set_chain_id" => {
                self.starknet_state.exec_info.tx_info.chain_id = as_single_input(inputs)?;
            }
            "set_nonce" => {
                self.starknet_state.exec_info.tx_info.nonce = as_single_input(inputs)?;
            }
            "set_signature" => {
                self.starknet_state.exec_info.tx_info.signature = inputs;
            }
            "set_block_hash" => {
                let [block_number, block_hash] = vec_as_array(inputs, || {
                    format!(
                        "`{selector}` cheatcode invalid args: pass span of an array with exactly \
                         two elements",
                    )
                })?;
                self.starknet_state.block_hash.insert(block_number.to_u64().unwrap(), block_hash);
            }
            "pop_log" => {
                let contract_logs = self.starknet_state.logs.get_mut(&as_single_input(inputs)?);
                if let Some((keys, data)) =
                    contract_logs.and_then(|contract_logs| contract_logs.events.pop_front())
                {
                    res_segment.write(keys.len())?;
                    res_segment.write_data(keys.iter())?;
                    res_segment.write(data.len())?;
                    res_segment.write_data(data.iter())?;
                }
            }
            "pop_l2_to_l1_message" => {
                let contract_logs = self.starknet_state.logs.get_mut(&as_single_input(inputs)?);
                if let Some((to_address, payload)) = contract_logs
                    .and_then(|contract_logs| contract_logs.l2_to_l1_messages.pop_front())
                {
                    res_segment.write(to_address)?;
                    res_segment.write(payload.len())?;
                    res_segment.write_data(payload.iter())?;
                }
            }
            _ => Err(HintError::CustomHint(Box::from(format!(
                "Unknown cheatcode selector: {selector}"
            ))))?,
        }
        let res_segment_end = res_segment.ptr;
        insert_value_to_cellref!(vm, output_start, res_segment_start)?;
        insert_value_to_cellref!(vm, output_end, res_segment_end)?;
        Ok(())
    }

    /// Executes an external hint.
    fn execute_external_hint(
        &mut self,
        vm: &mut VirtualMachine,
        core_hint: &ExternalHint,
    ) -> Result<(), HintError> {
        match core_hint {
            ExternalHint::AddRelocationRule { src, dst } => vm.add_relocation_rule(
                extract_relocatable(vm, src)?,
                // The following is needed for when the `extensive_hints` feature is used in the
                // VM, in which case `dst_ptr` is a `MaybeRelocatable` type.
                #[allow(clippy::useless_conversion)]
                {
                    extract_relocatable(vm, dst)?.into()
                },
            )?,
            ExternalHint::WriteRunParam { index, dst } => {
                let index = get_val(vm, index)?;
                let values = index
                    .to_usize()
                    .and_then(|index| self.user_args.get(index))
                    .ok_or_else(|| {
                        HintError::CustomHint(Box::from(format!(
                            "Run parameter index {index} is out of bounds."
                        )))
                    })?;
                let mut stack = vec![(cell_ref_to_relocatable(dst, vm), values)];
                while let Some((mut buffer, values)) = stack.pop() {
                    for value in values {
                        match value {
                            Arg::Value(v) => {
                                vm.insert_value(buffer, v)?;
                                buffer += 1;
                            }
                            Arg::Array(arr) => {
                                let arr_buffer = vm.add_memory_segment();
                                stack.push((arr_buffer, arr));
                                vm.insert_value(buffer, arr_buffer)?;
                                buffer += 1;
                                vm.insert_value(buffer, (arr_buffer + args_size(arr))?)?;
                                buffer += 1;
                            }
                        }
                    }
                }
            }
            ExternalHint::AddMarker { start, end } => {
                self.markers.push(read_felts(vm, start, end)?);
            }
            ExternalHint::AddTrace { flag } => {
                let flag = get_val(vm, flag)?;
                // Setting the panic backtrace if the given flag is panic.
                if flag == 0x70616e6963u64.into() {
                    let mut fp = vm.get_fp();
                    self.panic_traceback = vec![(vm.get_pc(), fp)];
                    // Fetch the fp and pc traceback entries
                    loop {
                        let ptr_at_offset = |offset: usize| {
                            (fp - offset).ok().and_then(|r| vm.get_relocatable(r).ok())
                        };
                        // Get return pc.
                        let Some(ret_pc) = ptr_at_offset(1) else {
                            break;
                        };
                        // Get fp traceback.
                        let Some(ret_fp) = ptr_at_offset(2) else {
                            break;
                        };
                        if ret_fp == fp {
                            break;
                        }
                        fp = ret_fp;

                        let call_instruction = |offset: usize| -> Option<Relocatable> {
                            let ptr = (ret_pc - offset).ok()?;
                            let inst = vm.get_integer(ptr).ok()?;
                            let inst_short = inst.to_u64()?;
                            (inst_short & 0x7000_0000_0000_0000 == 0x1000_0000_0000_0000)
                                .then_some(ptr)
                        };
                        if let Some(call_pc) = call_instruction(1).or_else(|| call_instruction(2)) {
                            self.panic_traceback.push((call_pc, fp));
                        } else {
                            break;
                        }
                    }
                    self.panic_traceback.reverse();
                }
            }
        }
        Ok(())
    }
}

/// Extracts an array of felt252s from a vector of such.
fn vec_as_array<const COUNT: usize>(
    inputs: Vec<Felt252>,
    err_msg: impl FnOnce() -> String,
) -> Result<[Felt252; COUNT], HintError> {
    inputs.try_into().map_err(|_| HintError::CustomHint(Box::from(err_msg())))
}

/// Executes the `keccak_syscall` syscall.
fn keccak(gas_counter: &mut usize, data: Vec<Felt252>) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, KECCAK);
    if !data.len().is_multiple_of(17) {
        fail_syscall!(b"Invalid keccak input size");
    }
    let mut state = [0u64; 25];
    let keccak = keccak::Keccak::new();
    for chunk in data.chunks(17) {
        deduct_gas!(gas_counter, KECCAK_ROUND_COST);
        for (i, val) in chunk.iter().enumerate() {
            state[i] ^= val.to_u64().unwrap();
        }
        keccak.with_f1600(|f1600| f1600(&mut state));
    }
    Ok(SyscallResult::Success(vec![
        ((Felt252::from((state[1] as u128) << 64u32)) + Felt252::from(state[0])).into(),
        ((Felt252::from((state[3] as u128) << 64u32)) + Felt252::from(state[2])).into(),
    ]))
}

/// Executes the `sha256_process_block` syscall.
fn sha_256_process_block(
    gas_counter: &mut usize,
    prev_state: Vec<Felt252>,
    data: Vec<Felt252>,
    exec_scopes: &mut ExecutionScopes,
    vm: &mut dyn VMWrapper,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SHA256_PROCESS_BLOCK);
    let data_as_bytes =
        data.iter().flat_map(|v| v.to_u32().unwrap().to_be_bytes()).collect_array().unwrap();
    let mut state_as_words =
        prev_state.iter().map(|v| v.to_u32().unwrap()).collect_array().unwrap();
    sha2::block_api::compress256(&mut state_as_words, &[data_as_bytes]);
    let next_state_ptr = alloc_memory(exec_scopes, vm.vm(), 8)?;
    let mut buff: MemBuffer<'_> = MemBuffer::new(vm, next_state_ptr);
    buff.write_data(state_as_words.into_iter().map(Felt252::from))?;
    Ok(SyscallResult::Success(vec![next_state_ptr.into()]))
}

/// Executes the `sha512_process_block` syscall.
fn sha_512_process_block(
    gas_counter: &mut usize,
    prev_state: Vec<Felt252>,
    data: Vec<Felt252>,
    exec_scopes: &mut ExecutionScopes,
    vm: &mut dyn VMWrapper,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SHA512_PROCESS_BLOCK);
    let data_as_bytes: [u8; 128] =
        data.iter().flat_map(|v| v.to_u64().unwrap().to_be_bytes()).collect_array().unwrap();
    let mut state_as_words: [u64; 8] =
        prev_state.iter().map(|v| v.to_u64().unwrap()).collect_array().unwrap();
    sha2::block_api::compress512(&mut state_as_words, &[data_as_bytes]);
    let next_state_ptr = alloc_memory(exec_scopes, vm.vm(), 8)?;
    let mut buff: MemBuffer<'_> = MemBuffer::new(vm, next_state_ptr);
    buff.write_data(state_as_words.into_iter().map(Felt252::from))?;
    Ok(SyscallResult::Success(vec![next_state_ptr.into()]))
}

// --- secp256k1 ---

/// Executes the `secp256k1_new_syscall` syscall.
fn secp256k1_new(
    gas_counter: &mut usize,
    x: U256,
    y: U256,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256K1_NEW);
    secp_new(x, y, &mut get_secp256k1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256k1_add_syscall` syscall.
fn secp256k1_add(
    gas_counter: &mut usize,
    exec_scopes: &mut ExecutionScopes,
    p0_id: usize,
    p1_id: usize,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256K1_ADD);
    secp_add(p0_id, p1_id, &mut get_secp256k1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256k1_mul_syscall` syscall.
fn secp256k1_mul(
    gas_counter: &mut usize,
    p_id: usize,
    scalar: U256,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256K1_MUL);
    secp_mul(p_id, scalar, &mut get_secp256k1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256k1_get_point_from_x_syscall` syscall.
fn secp256k1_get_point_from_x(
    gas_counter: &mut usize,
    x: U256,
    y_parity: bool,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256K1_GET_POINT_FROM_X);
    secp_get_point_from_x(x, y_parity, &mut get_secp256k1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256k1_get_xy_syscall` syscall.
fn secp256k1_get_xy(
    gas_counter: &mut usize,
    p_id: usize,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256K1_GET_XY);
    secp_get_xy(p_id, &get_secp256k1_exec_scope(exec_scopes)?.ec_points)
}

/// Returns the `Secp256k1ExecScope` managing the different active points.
/// The first call to this function will create the scope, and subsequent calls will return it.
/// The first call would happen from some point creation syscall.
fn get_secp256k1_exec_scope(
    exec_scopes: &mut ExecutionScopes,
) -> Result<&mut Secp256k1ExecutionScope, HintError> {
    const NAME: &str = "secp256k1_exec_scope";
    if exec_scopes.get_ref::<Secp256k1ExecutionScope>(NAME).is_err() {
        exec_scopes.assign_or_update_variable(NAME, Box::<Secp256k1ExecutionScope>::default());
    }
    exec_scopes.get_mut_ref::<Secp256k1ExecutionScope>(NAME)
}

// --- secp256r1 ---

/// Executes the `secp256r1_new_syscall` syscall.
fn secp256r1_new(
    gas_counter: &mut usize,
    x: U256,
    y: U256,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256R1_NEW);
    secp_new(x, y, &mut get_secp256r1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256r1_add_syscall` syscall.
fn secp256r1_add(
    gas_counter: &mut usize,
    exec_scopes: &mut ExecutionScopes,
    p0_id: usize,
    p1_id: usize,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256R1_ADD);
    secp_add(p0_id, p1_id, &mut get_secp256r1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256r1_mul_syscall` syscall.
fn secp256r1_mul(
    gas_counter: &mut usize,
    p_id: usize,
    scalar: U256,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256R1_MUL);
    secp_mul(p_id, scalar, &mut get_secp256r1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256r1_get_point_from_x_syscall` syscall.
fn secp256r1_get_point_from_x(
    gas_counter: &mut usize,
    x: U256,
    y_parity: bool,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256R1_GET_POINT_FROM_X);
    secp_get_point_from_x(x, y_parity, &mut get_secp256r1_exec_scope(exec_scopes)?.ec_points)
}

/// Executes the `secp256r1_get_xy_syscall` syscall.
fn secp256r1_get_xy(
    gas_counter: &mut usize,
    p_id: usize,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256R1_GET_XY);
    secp_get_xy(p_id, &get_secp256r1_exec_scope(exec_scopes)?.ec_points)
}

/// Returns the `Secp256r1ExecScope` managing the different active points.
/// The first call to this function will create the scope, and subsequent calls will return it.
/// The first call would happen from some point creation syscall.
fn get_secp256r1_exec_scope(
    exec_scopes: &mut ExecutionScopes,
) -> Result<&mut Secp256r1ExecutionScope, HintError> {
    const NAME: &str = "secp256r1_exec_scope";
    if exec_scopes.get_ref::<Secp256r1ExecutionScope>(NAME).is_err() {
        exec_scopes.assign_or_update_variable(NAME, Box::<Secp256r1ExecutionScope>::default());
    }
    exec_scopes.get_mut_ref::<Secp256r1ExecutionScope>(NAME)
}

/// Creates a new point on the curve `C` and adds it to `ec_points`.
///
/// Note that both secp curves have cofactor 1, so checking the curve equation is enough to
/// validate group membership.
fn secp_new<C>(
    x: U256,
    y: U256,
    ec_points: &mut Vec<ShortWeierstrassProjectivePoint<C>>,
) -> Result<SyscallResult, HintError>
where
    C: IsShortWeierstrass,
    C::BaseField: IsPrimeField<RepresentativeType = U256> + IsField<BaseType = U256>,
{
    let modulus_minus_one = C::BaseField::modulus_minus_one();
    if x > modulus_minus_one || y > modulus_minus_one {
        fail_syscall!(b"Coordinates out of range");
    }
    let maybe_p = if chain!(&x.limbs, &y.limbs).all(|l| *l == 0) {
        Some(ShortWeierstrassProjectivePoint::neutral_element())
    } else {
        ShortWeierstrassProjectivePoint::new([
            FieldElement::new(x),
            FieldElement::new(y),
            FieldElement::one(),
        ])
        .ok()
    };
    Ok(SyscallResult::Success(match maybe_p {
        Some(p) => {
            let id = ec_points.len();
            ec_points.push(p);
            vec![0.into(), id.into()]
        }
        None => vec![1.into(), 0.into()],
    }))
}

/// Adds the points at `p0_id` and `p1_id`, and adds the result to `ec_points`.
fn secp_add<C: IsShortWeierstrass>(
    p0_id: usize,
    p1_id: usize,
    ec_points: &mut Vec<ShortWeierstrassProjectivePoint<C>>,
) -> Result<SyscallResult, HintError> {
    let sum = ec_points[p0_id].operate_with(&ec_points[p1_id]);
    let id = ec_points.len();
    ec_points.push(sum);
    Ok(SyscallResult::Success(vec![id.into()]))
}

/// Multiplies the point at `p_id` by `scalar`, and adds the result to `ec_points`.
fn secp_mul<C: IsShortWeierstrass>(
    p_id: usize,
    scalar: U256,
    ec_points: &mut Vec<ShortWeierstrassProjectivePoint<C>>,
) -> Result<SyscallResult, HintError> {
    // No need to reduce the scalar by the curve order, as multiplying by it gives the same point.
    let product = ec_points[p_id].operate_with_self(scalar);
    let id = ec_points.len();
    ec_points.push(product);
    Ok(SyscallResult::Success(vec![id.into()]))
}

/// Finds a point on the curve `C` with the given `x` coordinate and `y` parity, and adds it to
/// `ec_points`.
fn secp_get_point_from_x<C>(
    x: U256,
    y_parity: bool,
    ec_points: &mut Vec<ShortWeierstrassProjectivePoint<C>>,
) -> Result<SyscallResult, HintError>
where
    C: IsShortWeierstrass,
    C::BaseField: IsPrimeField<RepresentativeType = U256> + IsField<BaseType = U256>,
{
    if x > C::BaseField::modulus_minus_one() {
        fail_syscall!(b"Coordinates out of range");
    }
    let x = FieldElement::new(x);
    // Solve the curve equation `y^2 = x^3 + ax + b` for `y`.
    let Some((y0, y1)) = ((x.square() + C::a()) * &x + C::b()).sqrt() else {
        // No point on the curve with the given `x`.
        return Ok(SyscallResult::Success(vec![1.into(), 0.into()]));
    };
    // The roots are `y` and `modulus - y`, and as the modulus is odd, exactly one of them is odd.
    let y0_is_odd = y0.representative().limbs[3] & 1 == 1;
    let y = if y0_is_odd == y_parity { y0 } else { y1 };
    let p = ShortWeierstrassProjectivePoint::new([x, y, FieldElement::one()]).unwrap();
    let id = ec_points.len();
    ec_points.push(p);
    Ok(SyscallResult::Success(vec![0.into(), id.into()]))
}

/// Returns the coordinates of the point at `p_id`, as pairs of 128-bit limbs.
fn secp_get_xy<C>(
    p_id: usize,
    ec_points: &[ShortWeierstrassProjectivePoint<C>],
) -> Result<SyscallResult, HintError>
where
    C: IsShortWeierstrass,
    C::BaseField: IsPrimeField<RepresentativeType = U256> + IsField<BaseType = U256>,
{
    let p = &ec_points[p_id];
    if p.is_neutral_element() {
        return Ok(SyscallResult::Success(vec![Felt252::ZERO.into(); 4]));
    }
    let p = p.to_affine();
    let merge = |l0: u64, l1: u64| Felt252::from(((l1 as u128) << 64) + l0 as u128).into();
    let split = |U256 { limbs: [l3, l2, l1, l0] }: U256| [merge(l0, l1), merge(l2, l3)];
    let [x0, x1] = split(p.x().representative());
    let [y0, y1] = split(p.y().representative());
    Ok(SyscallResult::Success(vec![x0, x1, y0, y1]))
}

/// Reads the result of a function call that returns `Array<felt252>`.
fn read_array_result_as_vec(memory: &[Option<Felt252>], value: &[Felt252]) -> Vec<Felt252> {
    // TODO(spapini): Handle failures.
    let [res_start, res_end] = value else {
        panic!("Unexpected return value from contract call");
    };
    let res_start: usize = res_start.to_bigint().try_into().unwrap();
    let res_end: usize = res_end.to_bigint().try_into().unwrap();
    (res_start..res_end).map(|i| memory[i].unwrap()).collect()
}
