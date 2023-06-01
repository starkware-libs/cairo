use std::any::Any;
use std::borrow::Cow;
use std::collections::{HashMap, VecDeque};
use std::ops::{Deref, Shl};

use ark_ff::{BigInteger, PrimeField};
use cairo_felt::Felt252;
use cairo_lang_casm::hints::{Hint, StarknetHint};
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{CellRef, ResOperand};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_utils::bigint::BigIntAsHex;
use cairo_lang_vm_utils::{execute_core_hint_base, extract_relocatable, insert_value_to_cellref};
use cairo_vm::hint_processor::hint_processor_definition::{
    HintProcessor, HintProcessorLogic, HintReference,
};
use cairo_vm::serde::deserialize_program::{
    ApTracking, BuiltinName, FlowTrackingData, HintParams, ReferenceManager,
};
use cairo_vm::types::exec_scope::ExecutionScopes;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::cairo_run_errors::CairoRunError;
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::errors::memory_errors::MemoryError;
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use cairo_vm::vm::runners::cairo_runner::{CairoRunner, ResourceTracker, RunResources};
use cairo_vm::vm::vm_core::VirtualMachine;
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::{ToPrimitive, Zero};
use {ark_secp256k1 as secp256k1, ark_secp256r1 as secp256r1};

use crate::{build_hints_dict, Arg, RunResultValue, SierraCasmRunner};

#[cfg(test)]
mod test;

/// Convert a Hint to the cairo-vm class HintParams by canonically serializing it to a string.
pub fn hint_to_hint_params(hint: &Hint) -> HintParams {
    HintParams {
        code: hint.representing_string(),
        accessible_scopes: vec![],
        flow_tracking_data: FlowTrackingData {
            ap_tracking: ApTracking::new(),
            reference_ids: HashMap::new(),
        },
    }
}

/// Helper object to allocate and track Secp256k1 elliptic curve points.
#[derive(Default)]
struct Secp256k1ExecutionScope {
    /// All elliptic curve points provided by the secp256k1 syscalls.
    /// The id of a point is the index in the vector.
    ec_points: Vec<secp256k1::Affine>,
}

/// Helper object to allocate and track Secp256r1 elliptic curve points.
#[derive(Default)]
struct Secp256r1ExecutionScope {
    /// All elliptic curve points provided by the secp256r1 syscalls.
    /// The id of a point is the index in the vector.
    ec_points: Vec<secp256r1::Affine>,
}

/// HintProcessor for Cairo compiler hints.
pub struct CairoHintProcessor<'a> {
    /// The Cairo runner.
    #[allow(dead_code)]
    pub runner: Option<&'a SierraCasmRunner>,
    // A mapping from a string that represents a hint to the hint object.
    pub string_to_hint: HashMap<String, Hint>,
    // The starknet state.
    pub starknet_state: StarknetState,
    // Maintains the resources of the run.
    pub run_resources: RunResources,
}

// Log type signature
type Log = (Vec<Felt252>, Vec<Felt252>);

/// Execution scope for starknet related data.
/// All values will be 0 and by default if not setup by the test.
#[derive(Clone, Default)]
pub struct StarknetState {
    /// The values of addresses in the simulated storage per contract.
    storage: HashMap<Felt252, HashMap<Felt252, Felt252>>,
    /// A mapping from contract address to class hash.
    #[allow(dead_code)]
    deployed_contracts: HashMap<Felt252, Felt252>,
    /// A mapping from contract address to logs.
    logs: HashMap<Felt252, VecDeque<Log>>,
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

/// Resulting options from a syscall.
enum SyscallResult {
    /// The syscall was successful.
    Success(Vec<MaybeRelocatable>),
    /// The syscall failed, with the revert reason.
    Failure(Vec<Felt252>),
}

macro_rules! fail_syscall {
    ($reason:expr) => {
        return Ok(SyscallResult::Failure(vec![Felt252::from_bytes_be($reason)]))
    };
    ($existing:ident, $reason:expr) => {
        $existing.push(Felt252::from_bytes_be($reason));
        return Ok(SyscallResult::Failure($existing))
    };
}

/// Deducts gas from the given gas counter, or fails the syscall if there is not enough gas.
macro_rules! deduct_gas {
    ($gas:ident, $amount:expr) => {
        if *$gas < $amount {
            fail_syscall!(b"Syscall out of gas");
        }
        *$gas -= $amount;
    };
}

impl HintProcessorLogic for CairoHintProcessor<'_> {
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
            Hint::Core(core_hint_base) => {
                return execute_core_hint_base(vm, exec_scopes, core_hint_base);
            }
            Hint::Starknet(hint) => hint,
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
    fn compile_hint(
        &self,
        hint_code: &str,
        _ap_tracking_data: &ApTracking,
        _reference_ids: &HashMap<String, usize>,
        _references: &[HintReference],
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

/// Wrapper trait for a VM owner.
trait VMWrapper {
    fn vm(&mut self) -> &mut VirtualMachine;
}
impl VMWrapper for VirtualMachine {
    fn vm(&mut self) -> &mut VirtualMachine {
        self
    }
}

/// Creates a new segment in the VM memory and writes data to it, returing the start and end
/// pointers of the segment.
fn segment_with_data<T: Into<MaybeRelocatable>, Data: Iterator<Item = T>>(
    vm: &mut dyn VMWrapper,
    data: Data,
) -> Result<(Relocatable, Relocatable), MemoryError> {
    let mut segment = MemBuffer::new_segment(vm);
    let start = segment.ptr;
    segment.write_data(data)?;
    Ok((start, segment.ptr))
}

/// A helper struct to continuously write and read from a buffer in the VM memory.
struct MemBuffer<'a> {
    /// The VM to write to.
    /// This is a trait so that we would borrow the actual VM only once.
    vm: &'a mut dyn VMWrapper,
    /// The current location of the buffer.
    pub ptr: Relocatable,
}
impl<'a> MemBuffer<'a> {
    /// Creates a new buffer.
    fn new(vm: &'a mut dyn VMWrapper, ptr: Relocatable) -> Self {
        Self { vm, ptr }
    }

    /// Creates a new segment and returns a buffer wrapping it.
    fn new_segment(vm: &'a mut dyn VMWrapper) -> Self {
        let ptr = vm.vm().add_memory_segment();
        Self::new(vm, ptr)
    }

    /// Returns the current position of the buffer and advances it by one.
    fn next(&mut self) -> Relocatable {
        let ptr = self.ptr;
        self.ptr += 1;
        ptr
    }

    /// Returns the felt252 value in the current position of the buffer and advances it by one.
    /// Fails if the value is not a felt252.
    /// Borrows the buffer since a reference is returned.
    fn next_felt252(&mut self) -> Result<Cow<'_, Felt252>, MemoryError> {
        let ptr = self.next();
        self.vm.vm().get_integer(ptr)
    }

    /// Returns the usize value in the current position of the buffer and advances it by one.
    /// Fails with `MemoryError` if the value is not a felt252.
    /// Panics if the value is not a usize.
    fn next_usize(&mut self) -> Result<usize, MemoryError> {
        Ok(self.next_felt252()?.to_usize().unwrap())
    }

    /// Returns the u128 value in the current position of the buffer and advances it by one.
    /// Fails with `MemoryError` if the value is not a felt252.
    /// Panics if the value is not a u128.
    fn next_u128(&mut self) -> Result<u128, MemoryError> {
        Ok(self.next_felt252()?.to_u128().unwrap())
    }

    /// Returns the u64 value in the current position of the buffer and advances it by one.
    /// Fails with `MemoryError` if the value is not a felt252.
    /// Panics if the value is not a u64.
    fn next_u64(&mut self) -> Result<u64, MemoryError> {
        Ok(self.next_felt252()?.to_u64().unwrap())
    }

    /// Returns the u256 value encoded starting from the current position of the buffer and advances
    /// it by two.
    /// Fails with `MemoryError` if any of the next two values are not felt252s.
    /// Panics if any of the next two values are not u128.
    fn next_u256(&mut self) -> Result<BigUint, MemoryError> {
        Ok(self.next_u128()? + BigUint::from(self.next_u128()?).shl(128))
    }

    /// Returns the address value in the current position of the buffer and advances it by one.
    /// Fails if the value is not an address.
    fn next_addr(&mut self) -> Result<Relocatable, MemoryError> {
        let ptr = self.next();
        self.vm.vm().get_relocatable(ptr)
    }

    /// Returns the array of integer values pointed to by the two next addresses in the buffer and
    /// advances it by two. Will fail if the two values are not addresses or if the addresses do
    /// not point to an array of integers.
    fn next_arr(&mut self) -> Result<Vec<Felt252>, HintError> {
        let start = self.next_addr()?;
        let end = self.next_addr()?;
        vm_get_range(self.vm.vm(), start, end)
    }

    /// Writes a value to the current position of the buffer and advances it by one.
    fn write<T: Into<MaybeRelocatable>>(&mut self, value: T) -> Result<(), MemoryError> {
        let ptr = self.next();
        self.vm.vm().insert_value(ptr, value)
    }
    /// Writes an iterator of values starting from the current position of the buffer and advances
    /// it to after the end of the written value.
    fn write_data<T: Into<MaybeRelocatable>, Data: Iterator<Item = T>>(
        &mut self,
        data: Data,
    ) -> Result<(), MemoryError> {
        for value in data {
            self.write(value)?;
        }
        Ok(())
    }

    /// Writes an array into a new segment and writes the start and end pointers to the current
    /// position of the buffer. Advances the buffer by two.
    fn write_arr<T: Into<MaybeRelocatable>, Data: Iterator<Item = T>>(
        &mut self,
        data: Data,
    ) -> Result<(), MemoryError> {
        let (start, end) = segment_with_data(self, data)?;
        self.write(start)?;
        self.write(end)
    }
}

impl<'a> VMWrapper for MemBuffer<'a> {
    fn vm(&mut self) -> &mut VirtualMachine {
        self.vm.vm()
    }
}

impl<'a> CairoHintProcessor<'a> {
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
        match std::str::from_utf8(&selector).unwrap() {
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
                let _to_address = system_buffer.next_felt252()?;
                let _payload = system_buffer.next_arr()?;
                deduct_gas!(gas_counter, 50);
                Ok(SyscallResult::Success(vec![]))
            }),
            "Keccak" => execute_handle_helper(&mut |system_buffer, gas_counter| {
                keccak(gas_counter, system_buffer.next_arr()?)
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
                    system_buffer.next_felt252()?.is_zero(),
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
                    system_buffer.next_felt252()?.is_zero(),
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
                    system_buffer.next_felt252()?.into_owned(),
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
        deduct_gas!(gas_counter, 1000);
        if !addr_domain.is_zero() {
            // Only address_domain 0 is currently supported.
            fail_syscall!(b"Unsupported address domain");
        }
        let contract = self.starknet_state.exec_info.contract_address.clone();
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
        deduct_gas!(gas_counter, 100);
        if !addr_domain.is_zero() {
            // Only address_domain 0 is currently supported.
            fail_syscall!(b"Unsupported address domain");
        }
        let value = self
            .starknet_state
            .storage
            .get(&self.starknet_state.exec_info.contract_address)
            .and_then(|contract_storage| contract_storage.get(&addr))
            .cloned()
            .unwrap_or_else(|| Felt252::from(0));
        Ok(SyscallResult::Success(vec![value.into()]))
    }

    /// Executes the `get_block_hash_syscall` syscall.
    fn get_block_hash(
        &mut self,
        gas_counter: &mut usize,
        _block_number: u64,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, 100);
        // TODO(Arni, 28/5/2023): Replace the temporary return value with the required value.
        //      One design suggestion - to preform a storage read. Have an arbitrary, hardcoded
        //      (For example, addr=1) contain the mapping from block number to block hash.
        fail_syscall!(b"GET_BLOCK_HASH_UNIMPLEMENTED");
    }

    /// Executes the `get_execution_info_syscall` syscall.
    fn get_execution_info(
        &mut self,
        gas_counter: &mut usize,
        vm: &mut dyn VMWrapper,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, 50);
        let exec_info = &self.starknet_state.exec_info;
        let block_info = &exec_info.block_info;
        let tx_info = &exec_info.tx_info;
        let mut res_segment = MemBuffer::new_segment(vm);
        let signature_start = res_segment.ptr;
        res_segment.write_data(tx_info.signature.iter().cloned())?;
        let signature_end = res_segment.ptr;
        let tx_info_ptr = res_segment.ptr;
        res_segment.write(tx_info.version.clone())?;
        res_segment.write(tx_info.account_contract_address.clone())?;
        res_segment.write(tx_info.max_fee.clone())?;
        res_segment.write(signature_start)?;
        res_segment.write(signature_end)?;
        res_segment.write(tx_info.transaction_hash.clone())?;
        res_segment.write(tx_info.chain_id.clone())?;
        res_segment.write(tx_info.nonce.clone())?;
        let block_info_ptr = res_segment.ptr;
        res_segment.write(block_info.block_number.clone())?;
        res_segment.write(block_info.block_timestamp.clone())?;
        res_segment.write(block_info.sequencer_address.clone())?;
        let exec_info_ptr = res_segment.ptr;
        res_segment.write(block_info_ptr)?;
        res_segment.write(tx_info_ptr)?;
        res_segment.write(exec_info.caller_address.clone())?;
        res_segment.write(exec_info.contract_address.clone())?;
        Ok(SyscallResult::Success(vec![exec_info_ptr.into()]))
    }

    /// Executes the `emit_event_syscall` syscall.
    fn emit_event(
        &mut self,
        gas_counter: &mut usize,
        keys: Vec<Felt252>,
        data: Vec<Felt252>,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, 50);
        let contract = self.starknet_state.exec_info.contract_address.clone();
        self.starknet_state.logs.entry(contract).or_default().push_back((keys, data));
        Ok(SyscallResult::Success(vec![]))
    }

    /// Executes the `deploy_syscall` syscall.
    fn deploy(
        &mut self,
        gas_counter: &mut usize,
        class_hash: Felt252,
        _contract_address_salt: Felt252,
        calldata: Vec<Felt252>,
        _deploy_from_zero: Felt252,
        vm: &mut dyn VMWrapper,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, 50);

        // Assign an arbitrary address to the contract.
        let deployed_contract_address = self.starknet_state.get_next_id();

        // Prepare runner for running the constructor.
        let runner = self.runner.expect("Runner is needed for starknet.");
        let Some(contract_info) = runner.starknet_contracts_info.get(&class_hash) else {
            fail_syscall!(b"CLASS_HASH_NOT_FOUND");
        };

        // Call constructor if it exists.
        let (res_data_start, res_data_end) = if let Some(constructor) = &contract_info.constructor {
            // Replace the contract address in the context.
            let old_contract_address = std::mem::replace(
                &mut self.starknet_state.exec_info.contract_address,
                deployed_contract_address.clone(),
            );

            // Run the constructor.
            let res = self.call_entry_point(gas_counter, runner, constructor, calldata, vm);

            // Restore the contract address in the context.
            self.starknet_state.exec_info.contract_address = old_contract_address;
            match res {
                Ok(value) => value,
                Err(mut revert_reason) => {
                    fail_syscall!(revert_reason, b"CONSTRUCTOR_FAILED");
                }
            }
        } else {
            (Relocatable::from((0, 0)), Relocatable::from((0, 0)))
        };

        // Set the class hash of the deployed contract.
        self.starknet_state
            .deployed_contracts
            .insert(deployed_contract_address.clone(), class_hash);
        Ok(SyscallResult::Success(vec![
            deployed_contract_address.into(),
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
        deduct_gas!(gas_counter, 50);

        // Get the class hash of the contract.
        let Some(class_hash) = self.starknet_state.deployed_contracts.get(&contract_address) else {
            fail_syscall!(b"CONTRACT_NOT_DEPLOYED");
        };

        // Prepare runner for running the ctor.
        let runner = self.runner.expect("Runner is needed for starknet.");
        let contract_info = runner
            .starknet_contracts_info
            .get(class_hash)
            .expect("Deployed contract not found in registry.");

        // Call the function.
        let Some(entry_point) = contract_info.externals.get(&selector) else {
            fail_syscall!(b"ENTRYPOINT_NOT_FOUND");
        };

        // Replace the contract address in the context.
        let old_contract_address = std::mem::replace(
            &mut self.starknet_state.exec_info.contract_address,
            contract_address.clone(),
        );
        let old_caller_address = std::mem::replace(
            &mut self.starknet_state.exec_info.caller_address,
            old_contract_address.clone(),
        );

        let res = self.call_entry_point(gas_counter, runner, entry_point, calldata, vm);

        // Restore the contract address in the context.
        self.starknet_state.exec_info.caller_address = old_caller_address;
        self.starknet_state.exec_info.contract_address = old_contract_address;

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
        deduct_gas!(gas_counter, 50);
        // Prepare runner for running the call.
        let runner = self.runner.expect("Runner is needed for starknet.");
        let contract_info = runner
            .starknet_contracts_info
            .get(&class_hash)
            .expect("Deployed contract not found in registry.");

        // Call the function.
        let Some(entry_point) = contract_info.externals.get(&selector) else {
            fail_syscall!(b"ENTRYPOINT_NOT_FOUND");
        };
        match self.call_entry_point(gas_counter, runner, entry_point, calldata, vm) {
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
        deduct_gas!(gas_counter, 50);
        let address = self.starknet_state.exec_info.contract_address.clone();
        self.starknet_state.deployed_contracts.insert(address, new_class);
        Ok(SyscallResult::Success(vec![]))
    }

    /// Executes the entry point with the given calldata.
    fn call_entry_point(
        &mut self,
        gas_counter: &mut usize,
        runner: &SierraCasmRunner,
        entry_point: &FunctionId,
        calldata: Vec<Felt252>,
        vm: &mut dyn VMWrapper,
    ) -> Result<(Relocatable, Relocatable), Vec<Felt252>> {
        let function = runner
            .sierra_program_registry
            .get_function(entry_point)
            .expect("Entrypoint exists, but not found.");
        let mut res = runner
            .run_function_with_starknet_context(
                function,
                &[Arg::Array(calldata)],
                Some(*gas_counter),
                self.starknet_state.clone(),
            )
            .expect("Internal runner error.");

        *gas_counter = res.gas_counter.unwrap().to_usize().unwrap();
        match res.value {
            RunResultValue::Success(value) => {
                self.starknet_state = std::mem::take(&mut res.starknet_state);
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
        let as_single_input = |inputs: Vec<Felt252>| {
            if inputs.len() != 1 {
                Err(HintError::CustomHint(Box::from(format!(
                    "`{selector}` cheatcode invalid args: pass span of an array with exactly one \
                     element",
                ))))
            } else {
                Ok(inputs[0].clone())
            }
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
                self.starknet_state.exec_info.caller_address = as_single_input(inputs)?;
            }
            "set_contract_address" => {
                self.starknet_state.exec_info.contract_address = as_single_input(inputs)?;
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
            "pop_log" => {
                let contract_logs = self.starknet_state.logs.get_mut(&as_single_input(inputs)?);
                if let Some((keys, data)) =
                    contract_logs.and_then(|contract_logs| contract_logs.pop_front())
                {
                    res_segment.write(keys.len())?;
                    res_segment.write_data(keys.iter())?;
                    res_segment.write(data.len())?;
                    res_segment.write_data(data.iter())?;
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
}

/// Executes the `keccak_syscall` syscall.
fn keccak(gas_counter: &mut usize, data: Vec<Felt252>) -> Result<SyscallResult, HintError> {
    if data.len() % 17 != 0 {
        fail_syscall!(b"Invalid keccak input size");
    }
    let mut state = [0u64; 25];
    for chunk in data.chunks(17) {
        deduct_gas!(gas_counter, 5000);
        for (i, val) in chunk.iter().enumerate() {
            state[i] ^= val.to_u64().unwrap();
        }
        keccak::f1600(&mut state)
    }
    Ok(SyscallResult::Success(vec![
        ((Felt252::from(state[1]) << 64u32) + Felt252::from(state[0])).into(),
        ((Felt252::from(state[3]) << 64u32) + Felt252::from(state[2])).into(),
    ]))
}

// --- secp256k1 ---

/// Executes the `secp256k1_new_syscall` syscall.
fn secp256k1_new(
    gas_counter: &mut usize,
    x: BigUint,
    y: BigUint,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    let modulos = <secp256k1::Fq as PrimeField>::MODULUS.into();
    if x >= modulos || y >= modulos {
        fail_syscall!(b"Coordinates out of range");
    }
    let p = if x.is_zero() && y.is_zero() {
        secp256k1::Affine::identity()
    } else {
        secp256k1::Affine::new_unchecked(x.into(), y.into())
    };
    Ok(SyscallResult::Success(
        if !(p.is_on_curve() && p.is_in_correct_subgroup_assuming_on_curve()) {
            vec![1.into(), 0.into()]
        } else {
            let ec = get_secp256k1_exec_scope(exec_scopes)?;
            let id = ec.ec_points.len();
            ec.ec_points.push(p);
            vec![0.into(), id.into()]
        },
    ))
}

/// Executes the `secp256k1_add_syscall` syscall.
fn secp256k1_add(
    gas_counter: &mut usize,
    exec_scopes: &mut ExecutionScopes,
    p0_id: usize,
    p1_id: usize,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    let ec = get_secp256k1_exec_scope(exec_scopes)?;
    let p0 = &ec.ec_points[p0_id];
    let p1 = &ec.ec_points[p1_id];
    let sum = *p0 + *p1;
    let id = ec.ec_points.len();
    ec.ec_points.push(sum.into());
    Ok(SyscallResult::Success(vec![id.into()]))
}

/// Executes the `secp256k1_mul_syscall` syscall.
fn secp256k1_mul(
    gas_counter: &mut usize,
    p_id: usize,
    m: BigUint,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    if m >= <secp256k1::Fr as PrimeField>::MODULUS.into() {
        fail_syscall!(b"Scalar out of range");
    }
    let ec = get_secp256k1_exec_scope(exec_scopes)?;
    let p = &ec.ec_points[p_id];
    let product = *p * secp256k1::Fr::from(m);
    let id = ec.ec_points.len();
    ec.ec_points.push(product.into());
    Ok(SyscallResult::Success(vec![id.into()]))
}

/// Executes the `secp256k1_get_point_from_x_syscall` syscall.
fn secp256k1_get_point_from_x(
    gas_counter: &mut usize,
    x: BigUint,
    y_parity: bool,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    if x >= <secp256k1::Fq as PrimeField>::MODULUS.into() {
        fail_syscall!(b"Coordinates out of range");
    }
    let x = x.into();
    let maybe_p = secp256k1::Affine::get_ys_from_x_unchecked(x)
        .map(|(smaller, greater)| match (smaller.0.is_even(), y_parity) {
            (true, true) | (false, false) => smaller,
            (true, false) | (false, true) => greater,
        })
        .map(|y| secp256k1::Affine::new_unchecked(x, y))
        .filter(|p| p.is_in_correct_subgroup_assuming_on_curve());
    let Some(p) = maybe_p else {
        return Ok(SyscallResult::Success(vec![1.into(), 0.into()]));
    };
    let ec = get_secp256k1_exec_scope(exec_scopes)?;
    let id = ec.ec_points.len();
    ec.ec_points.push(p);
    Ok(SyscallResult::Success(vec![0.into(), id.into()]))
}

/// Executes the `secp256k1_get_xy_syscall` syscall.
fn secp256k1_get_xy(
    gas_counter: &mut usize,
    p_id: usize,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    let ec = get_secp256k1_exec_scope(exec_scopes)?;
    let p = &ec.ec_points[p_id];
    let pow_2_128 = BigUint::from(u128::MAX) + 1u32;
    let (x1, x0) = BigUint::from(p.x).div_rem(&pow_2_128);
    let (y1, y0) = BigUint::from(p.y).div_rem(&pow_2_128);
    Ok(SyscallResult::Success(vec![
        Felt252::from(x0).into(),
        Felt252::from(x1).into(),
        Felt252::from(y0).into(),
        Felt252::from(y1).into(),
    ]))
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

/// Executes the `secp256k1_new_syscall` syscall.
fn secp256r1_new(
    gas_counter: &mut usize,
    x: BigUint,
    y: BigUint,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    let modulos = <secp256r1::Fq as PrimeField>::MODULUS.into();
    if x >= modulos || y >= modulos {
        fail_syscall!(b"Coordinates out of range");
    }
    let p = if x.is_zero() && y.is_zero() {
        secp256r1::Affine::identity()
    } else {
        secp256r1::Affine::new_unchecked(x.into(), y.into())
    };
    Ok(SyscallResult::Success(
        if !(p.is_on_curve() && p.is_in_correct_subgroup_assuming_on_curve()) {
            vec![1.into(), 0.into()]
        } else {
            let ec = get_secp256r1_exec_scope(exec_scopes)?;
            let id = ec.ec_points.len();
            ec.ec_points.push(p);
            vec![0.into(), id.into()]
        },
    ))
}

/// Executes the `secp256r1_add_syscall` syscall.
fn secp256r1_add(
    gas_counter: &mut usize,
    exec_scopes: &mut ExecutionScopes,
    p0_id: usize,
    p1_id: usize,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    let ec = get_secp256r1_exec_scope(exec_scopes)?;
    let p0 = &ec.ec_points[p0_id];
    let p1 = &ec.ec_points[p1_id];
    let sum = *p0 + *p1;
    let id = ec.ec_points.len();
    ec.ec_points.push(sum.into());
    Ok(SyscallResult::Success(vec![id.into()]))
}

/// Executes the `secp256r1_mul_syscall` syscall.
fn secp256r1_mul(
    gas_counter: &mut usize,
    p_id: usize,
    m: BigUint,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    if m >= <secp256r1::Fr as PrimeField>::MODULUS.into() {
        fail_syscall!(b"Scalar out of range");
    }
    let ec = get_secp256r1_exec_scope(exec_scopes)?;
    let p = &ec.ec_points[p_id];
    let product = *p * secp256r1::Fr::from(m);
    let id = ec.ec_points.len();
    ec.ec_points.push(product.into());
    Ok(SyscallResult::Success(vec![id.into()]))
}

/// Executes the `secp256r1_get_point_from_x_syscall` syscall.
fn secp256r1_get_point_from_x(
    gas_counter: &mut usize,
    x: BigUint,
    y_parity: bool,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    if x >= <secp256r1::Fq as PrimeField>::MODULUS.into() {
        fail_syscall!(b"Coordinates out of range");
    }
    let x = x.into();
    let maybe_p = secp256r1::Affine::get_ys_from_x_unchecked(x)
        .map(|(smaller, greater)| match (smaller.0.is_even(), y_parity) {
            (true, true) | (false, false) => smaller,
            (true, false) | (false, true) => greater,
        })
        .map(|y| secp256r1::Affine::new_unchecked(x, y))
        .filter(|p| p.is_in_correct_subgroup_assuming_on_curve());
    let Some(p) = maybe_p else {
        return Ok(SyscallResult::Success(vec![1.into(), 0.into()]));
    };
    let ec = get_secp256r1_exec_scope(exec_scopes)?;
    let id = ec.ec_points.len();
    ec.ec_points.push(p);
    Ok(SyscallResult::Success(vec![0.into(), id.into()]))
}

/// Executes the `secp256r1_get_xy_syscall` syscall.
fn secp256r1_get_xy(
    gas_counter: &mut usize,
    p_id: usize,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, 500);
    let ec = get_secp256r1_exec_scope(exec_scopes)?;
    let p = &ec.ec_points[p_id];
    let pow_2_128 = BigUint::from(u128::MAX) + 1u32;
    let (x1, x0) = BigUint::from(p.x).div_rem(&pow_2_128);
    let (y1, y0) = BigUint::from(p.y).div_rem(&pow_2_128);
    Ok(SyscallResult::Success(vec![
        Felt252::from(x0).into(),
        Felt252::from(x1).into(),
        Felt252::from(y0).into(),
        Felt252::from(y1).into(),
    ]))
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

/// Provides context for the `additional_initialization` callback function of [run_function].
pub struct RunFunctionContext<'a> {
    pub vm: &'a mut VirtualMachine,
    pub data_len: usize,
}

type RunFunctionRes = (Vec<Option<Felt252>>, usize);
type RunFunctionResStarknet = (Vec<Option<Felt252>>, usize, StarknetState);

/// Runs `program` on layout with prime, and returns the memory layout and ap value.
/// Run used CairoHintProcessor and StarknetState to emulate Starknet behaviour.
pub fn run_function_with_starknet_context<'a, 'b: 'a, Instructions>(
    instructions: Instructions,
    builtins: Vec<BuiltinName>,
    additional_initialization: fn(
        context: RunFunctionContext<'_>,
    ) -> Result<(), Box<CairoRunError>>,
) -> Result<RunFunctionResStarknet, Box<CairoRunError>>
where
    Instructions: Iterator<Item = &'a Instruction> + Clone,
{
    let (hints_dict, string_to_hint) = build_hints_dict(instructions.clone());
    let mut hint_processor = CairoHintProcessor {
        runner: None,
        string_to_hint,
        starknet_state: StarknetState::default(),
        run_resources: RunResources::default(),
    };
    run_function(instructions, builtins, additional_initialization, &mut hint_processor, hints_dict)
        .map(|(mem, val)| (mem, val, hint_processor.starknet_state))
}

/// Runs `program` on layout with prime, and returns the memory layout and ap value.
/// Allows injecting custom HintProcessor.
pub fn run_function<'a, 'b: 'a, Instructions>(
    instructions: Instructions,
    builtins: Vec<BuiltinName>,
    additional_initialization: fn(
        context: RunFunctionContext<'_>,
    ) -> Result<(), Box<CairoRunError>>,
    hint_processor: &mut dyn HintProcessor,
    hints_dict: HashMap<usize, Vec<HintParams>>,
) -> Result<RunFunctionRes, Box<CairoRunError>>
where
    Instructions: Iterator<Item = &'a Instruction> + Clone,
{
    let data: Vec<MaybeRelocatable> = instructions
        .flat_map(|inst| inst.assemble().encode())
        .map(Felt252::from)
        .map(MaybeRelocatable::from)
        .collect();

    let data_len = data.len();
    let program = Program::new(
        builtins,
        data,
        Some(0),
        hints_dict,
        ReferenceManager { references: Vec::new() },
        HashMap::new(),
        vec![],
        None,
    )
    .map_err(CairoRunError::from)?;
    let mut runner = CairoRunner::new(&program, "all_cairo", false)
        .map_err(CairoRunError::from)
        .map_err(Box::new)?;
    let mut vm = VirtualMachine::new(true);

    let end = runner.initialize(&mut vm).map_err(CairoRunError::from)?;

    additional_initialization(RunFunctionContext { vm: &mut vm, data_len })?;

    runner.run_until_pc(end, &mut vm, hint_processor).map_err(CairoRunError::from)?;
    runner.end_run(true, false, &mut vm, hint_processor).map_err(CairoRunError::from)?;
    runner.relocate(&mut vm, true).map_err(CairoRunError::from)?;
    Ok((runner.relocated_memory, vm.get_relocated_trace().unwrap().last().unwrap().ap))
}
