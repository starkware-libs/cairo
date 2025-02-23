use std::any::Any;
use std::borrow::Cow;
use std::collections::{HashMap, VecDeque};
use std::ops::{Shl, Sub};
use std::vec::IntoIter;

use ark_ff::{BigInteger, PrimeField};
use cairo_lang_casm::hints::{CoreHint, DeprecatedHint, ExternalHint, Hint, StarknetHint};
use cairo_lang_casm::operand::{
    BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand,
};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_utils::bigint::BigIntAsHex;
use cairo_lang_utils::byte_array::{BYTE_ARRAY_MAGIC, BYTES_IN_WORD};
use cairo_lang_utils::extract_matches;
use cairo_vm::hint_processor::builtin_hint_processor::blake2s_hash::blake2s_compress;
use cairo_vm::hint_processor::hint_processor_definition::{
    HintProcessor, HintProcessorLogic, HintReference,
};
use cairo_vm::serde::deserialize_program::{
    ApTracking, FlowTrackingData, HintParams, ReferenceManager,
};
use cairo_vm::types::builtin_name::BuiltinName;
use cairo_vm::types::exec_scope::ExecutionScopes;
use cairo_vm::types::layout_name::LayoutName;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::cairo_run_errors::CairoRunError;
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::errors::memory_errors::MemoryError;
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use cairo_vm::vm::runners::cairo_runner::{
    CairoRunner, ExecutionResources, ResourceTracker, RunResources,
};
use cairo_vm::vm::trace::trace_entry::RelocatedTraceEntry;
use cairo_vm::vm::vm_core::VirtualMachine;
use dict_manager::DictManagerExecScope;
use itertools::Itertools;
use num_bigint::{BigInt, BigUint};
use num_integer::{ExtendedGcd, Integer};
use num_traits::{One, Signed, ToPrimitive, Zero};
use rand::Rng;
use starknet_types_core::felt::{Felt as Felt252, NonZeroFelt};
use {ark_secp256k1 as secp256k1, ark_secp256r1 as secp256r1};

use self::contract_address::calculate_contract_address;
use self::dict_manager::DictSquashExecScope;
use crate::short_string::{as_cairo_short_string, as_cairo_short_string_ex};
use crate::{Arg, RunResultValue, SierraCasmRunner, StarknetExecutionResources, args_size};

#[cfg(test)]
mod test;

mod circuit;
mod contract_address;
mod dict_manager;

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
    pub runner: Option<&'a SierraCasmRunner>,
    /// The user arguments for the run.
    ///
    /// We have a vector of the arguments per parameter, as a parameter type may be composed of
    /// several user args.
    pub user_args: Vec<Vec<Arg>>,
    /// A mapping from a string that represents a hint to the hint object.
    pub string_to_hint: HashMap<String, Hint>,
    /// The starknet state.
    pub starknet_state: StarknetState,
    /// Maintains the resources of the run.
    pub run_resources: RunResources,
    /// Resources used during syscalls - does not include resources used during the current VM run.
    /// At the end of the run - adding both would result in the actual expected resource usage.
    pub syscalls_used_resources: StarknetExecutionResources,
    /// Avoid allocating memory segments so finalization of segment arena may not occur.
    pub no_temporary_segments: bool,
    /// A set of markers created by the run.
    pub markers: Vec<Vec<Felt252>>,
}

pub fn cell_ref_to_relocatable(cell_ref: &CellRef, vm: &VirtualMachine) -> Relocatable {
    let base = match cell_ref.register {
        Register::AP => vm.get_ap(),
        Register::FP => vm.get_fp(),
    };
    (base + (cell_ref.offset as i32)).unwrap()
}

/// Inserts a value into the vm memory cell represented by the cellref.
#[macro_export]
macro_rules! insert_value_to_cellref {
    ($vm:ident, $cell_ref:ident, $value:expr) => {
        $vm.insert_value(cell_ref_to_relocatable($cell_ref, $vm), $value)
    };
}

// Log type signature
type Log = (Vec<Felt252>, Vec<Felt252>);

// L2 to L1 message type signature
type L2ToL1Message = (Felt252, Vec<Felt252>);

/// Execution scope for starknet related data.
/// All values will be 0 and by default if not setup by the test.
#[derive(Clone, Default)]
pub struct StarknetState {
    /// The values of addresses in the simulated storage per contract.
    storage: HashMap<Felt252, HashMap<Felt252, Felt252>>,
    /// A mapping from contract address to class hash.
    deployed_contracts: HashMap<Felt252, Felt252>,
    /// A mapping from contract address to logs.
    logs: HashMap<Felt252, ContractLogs>,
    /// The simulated execution info.
    exec_info: ExecutionInfo,
    /// A mock history, mapping block number to the class hash.
    block_hash: HashMap<u64, Felt252>,
}
impl StarknetState {
    /// Replaces the addresses in the context.
    pub fn open_caller_context(
        &mut self,
        (new_contract_address, new_caller_address): (Felt252, Felt252),
    ) -> (Felt252, Felt252) {
        let old_contract_address =
            std::mem::replace(&mut self.exec_info.contract_address, new_contract_address);
        let old_caller_address =
            std::mem::replace(&mut self.exec_info.caller_address, new_caller_address);
        (old_contract_address, old_caller_address)
    }

    /// Restores the addresses in the context.
    pub fn close_caller_context(
        &mut self,
        (old_contract_address, old_caller_address): (Felt252, Felt252),
    ) {
        self.exec_info.contract_address = old_contract_address;
        self.exec_info.caller_address = old_caller_address;
    }
}

/// Object storing logs for a contract.
#[derive(Clone, Default)]
struct ContractLogs {
    /// Events.
    events: VecDeque<Log>,
    /// Messages sent to L1.
    l2_to_l1_messages: VecDeque<L2ToL1Message>,
}

/// Copy of the cairo `ExecutionInfo` struct.
#[derive(Clone, Default)]
struct ExecutionInfo {
    block_info: BlockInfo,
    tx_info: TxInfo,
    caller_address: Felt252,
    contract_address: Felt252,
    entry_point_selector: Felt252,
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
    resource_bounds: Vec<ResourceBounds>,
    tip: Felt252,
    paymaster_data: Vec<Felt252>,
    nonce_data_availability_mode: Felt252,
    fee_data_availability_mode: Felt252,
    account_deployment_data: Vec<Felt252>,
}

/// Copy of the cairo `ResourceBounds` struct.
#[derive(Clone, Default)]
struct ResourceBounds {
    resource: Felt252,
    max_amount: Felt252,
    max_price_per_unit: Felt252,
}

/// Execution scope for constant memory allocation.
struct MemoryExecScope {
    /// The first free address in the segment.
    next_address: Relocatable,
}

/// Fetches the value of a cell from the vm.
fn get_cell_val(vm: &VirtualMachine, cell: &CellRef) -> Result<Felt252, VirtualMachineError> {
    Ok(*vm.get_integer(cell_ref_to_relocatable(cell, vm))?)
}

/// Fetch the `MaybeRelocatable` value from an address.
fn get_maybe_from_addr(
    vm: &VirtualMachine,
    addr: Relocatable,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    vm.get_maybe(&addr)
        .ok_or_else(|| VirtualMachineError::InvalidMemoryValueTemporaryAddress(Box::new(addr)))
}

/// Fetches the maybe relocatable value of a cell from the vm.
fn get_cell_maybe(
    vm: &VirtualMachine,
    cell: &CellRef,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, cell_ref_to_relocatable(cell, vm))
}

/// Fetches the value of a cell plus an offset from the vm, useful for pointers.
pub fn get_ptr(
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
    Ok(*vm.get_integer(get_ptr(vm, cell, offset)?)?)
}

/// Fetches the maybe relocatable value of a pointer described by the value at `cell` plus an offset
/// from the vm.
fn get_double_deref_maybe(
    vm: &VirtualMachine,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, get_ptr(vm, cell, offset)?)
}

/// Extracts a parameter assumed to be a buffer, and converts it into a relocatable.
pub fn extract_relocatable(
    vm: &VirtualMachine,
    buffer: &ResOperand,
) -> Result<Relocatable, VirtualMachineError> {
    let (base, offset) = extract_buffer(buffer);
    get_ptr(vm, base, &offset)
}

/// Fetches the value of `res_operand` from the vm.
pub fn get_val(
    vm: &VirtualMachine,
    res_operand: &ResOperand,
) -> Result<Felt252, VirtualMachineError> {
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

/// Gas Costs for syscalls.
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

/// Deducts gas from the given gas counter, or fails the syscall if there is not enough gas.
macro_rules! deduct_gas {
    ($gas:ident, $amount:ident) => {
        if *$gas < gas_costs::$amount {
            fail_syscall!(b"Syscall out of gas");
        }
        *$gas -= gas_costs::$amount;
    };
}

/// Fetches the maybe relocatable value of `res_operand` from the vm.
fn get_maybe(
    vm: &VirtualMachine,
    res_operand: &ResOperand,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    match res_operand {
        ResOperand::Deref(cell) => get_cell_maybe(vm, cell),
        ResOperand::DoubleDeref(cell, offset) => {
            get_double_deref_maybe(vm, cell, &(*offset).into())
        }
        ResOperand::Immediate(x) => Ok(Felt252::from(x.value.clone()).into()),
        ResOperand::BinOp(op) => {
            let a = get_cell_maybe(vm, &op.a)?;
            let b = match &op.b {
                DerefOrImmediate::Deref(cell) => get_cell_val(vm, cell)?,
                DerefOrImmediate::Immediate(x) => Felt252::from(x.value.clone()),
            };
            Ok(match op.op {
                Operation::Add => a.add_int(&b)?,
                Operation::Mul => match a {
                    MaybeRelocatable::RelocatableValue(_) => {
                        panic!("mul not implemented for relocatable values")
                    }
                    MaybeRelocatable::Int(a) => (a * b).into(),
                },
            })
        }
    }
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
pub trait VMWrapper {
    fn vm(&mut self) -> &mut VirtualMachine;
}
impl VMWrapper for VirtualMachine {
    fn vm(&mut self) -> &mut VirtualMachine {
        self
    }
}

/// Creates a new segment in the VM memory and writes data to it, returning the start and end
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
pub struct MemBuffer<'a> {
    /// The VM to write to.
    /// This is a trait so that we would borrow the actual VM only once.
    vm: &'a mut dyn VMWrapper,
    /// The current location of the buffer.
    pub ptr: Relocatable,
}
impl<'a> MemBuffer<'a> {
    /// Creates a new buffer.
    pub fn new(vm: &'a mut dyn VMWrapper, ptr: Relocatable) -> Self {
        Self { vm, ptr }
    }

    /// Creates a new segment and returns a buffer wrapping it.
    pub fn new_segment(vm: &'a mut dyn VMWrapper) -> Self {
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
    pub fn next_felt252(&mut self) -> Result<Cow<'_, Felt252>, MemoryError> {
        let ptr = self.next();
        self.vm.vm().get_integer(ptr)
    }

    /// Returns the bool value in the current position of the buffer and advances it by one.
    /// Fails with `MemoryError` if the value is not a felt252.
    /// Panics if the value is not a bool.
    fn next_bool(&mut self) -> Result<bool, MemoryError> {
        let ptr = self.next();
        Ok(!(self.vm.vm().get_integer(ptr)?.is_zero()))
    }

    /// Returns the usize value in the current position of the buffer and advances it by one.
    /// Fails with `MemoryError` if the value is not a felt252.
    /// Panics if the value is not a usize.
    pub fn next_usize(&mut self) -> Result<usize, MemoryError> {
        Ok(self.next_felt252()?.to_usize().unwrap())
    }

    /// Returns the u128 value in the current position of the buffer and advances it by one.
    /// Fails with `MemoryError` if the value is not a felt252.
    /// Panics if the value is not a u128.
    pub fn next_u128(&mut self) -> Result<u128, MemoryError> {
        Ok(self.next_felt252()?.to_u128().unwrap())
    }

    /// Returns the u64 value in the current position of the buffer and advances it by one.
    /// Fails with `MemoryError` if the value is not a felt252.
    /// Panics if the value is not a u64.
    pub fn next_u64(&mut self) -> Result<u64, MemoryError> {
        Ok(self.next_felt252()?.to_u64().unwrap())
    }

    /// Returns the u256 value encoded starting from the current position of the buffer and advances
    /// it by two.
    /// Fails with `MemoryError` if any of the next two values are not felt252s.
    /// Panics if any of the next two values are not u128.
    pub fn next_u256(&mut self) -> Result<BigUint, MemoryError> {
        Ok(self.next_u128()? + BigUint::from(self.next_u128()?).shl(128))
    }

    /// Returns the address value in the current position of the buffer and advances it by one.
    /// Fails if the value is not an address.
    pub fn next_addr(&mut self) -> Result<Relocatable, MemoryError> {
        let ptr = self.next();
        self.vm.vm().get_relocatable(ptr)
    }

    /// Returns the array of integer values pointed to by the two next addresses in the buffer and
    /// advances it by two. Will fail if the two values are not addresses or if the addresses do
    /// not point to an array of integers.
    pub fn next_arr(&mut self) -> Result<Vec<Felt252>, HintError> {
        let start = self.next_addr()?;
        let end = self.next_addr()?;
        vm_get_range(self.vm.vm(), start, end)
    }

    /// Returns the array of integer values pointed to by the next address in the buffer and
    /// with a fixed size and advances the buffer by one. Will fail if the next value is not
    /// an address or if the address does not point to an array of integers.
    pub fn next_fixed_size_arr_pointer(&mut self, size: usize) -> Result<Vec<Felt252>, HintError> {
        let start = self.next_addr()?;
        let end = (start + size)?;
        vm_get_range(self.vm.vm(), start, end)
    }

    /// Writes a value to the current position of the buffer and advances it by one.
    pub fn write<T: Into<MaybeRelocatable>>(&mut self, value: T) -> Result<(), MemoryError> {
        let ptr = self.next();
        self.vm.vm().insert_value(ptr, value)
    }
    /// Writes an iterator of values starting from the current position of the buffer and advances
    /// it to after the end of the written value.
    pub fn write_data<T: Into<MaybeRelocatable>, Data: Iterator<Item = T>>(
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
    pub fn write_arr<T: Into<MaybeRelocatable>, Data: Iterator<Item = T>>(
        &mut self,
        data: Data,
    ) -> Result<(), MemoryError> {
        let (start, end) = segment_with_data(self, data)?;
        self.write(start)?;
        self.write(end)
    }
}

impl VMWrapper for MemBuffer<'_> {
    fn vm(&mut self) -> &mut VirtualMachine {
        self.vm.vm()
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
                panic!("Meta transaction is not supported.")
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
        let contract = self.starknet_state.exec_info.contract_address;
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
        let block_info_ptr = res_segment.ptr;
        res_segment.write(block_info.block_number)?;
        res_segment.write(block_info.block_timestamp)?;
        res_segment.write(block_info.sequencer_address)?;
        let exec_info_ptr = res_segment.ptr;
        res_segment.write(block_info_ptr)?;
        res_segment.write(tx_info_ptr)?;
        res_segment.write(exec_info.caller_address)?;
        res_segment.write(exec_info.contract_address)?;
        res_segment.write(exec_info.entry_point_selector)?;
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
        let contract = self.starknet_state.exec_info.contract_address;
        self.starknet_state.logs.entry(contract).or_default().events.push_back((keys, data));
        Ok(SyscallResult::Success(vec![]))
    }

    /// Executes the `send_message_to_l1_event_syscall` syscall.
    fn send_message_to_l1(
        &mut self,
        gas_counter: &mut usize,
        to_address: Felt252,
        payload: Vec<Felt252>,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, SEND_MESSAGE_TO_L1);
        let contract = self.starknet_state.exec_info.contract_address;
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
        _contract_address_salt: Felt252,
        calldata: Vec<Felt252>,
        deploy_from_zero: bool,
        vm: &mut dyn VMWrapper,
    ) -> Result<SyscallResult, HintError> {
        deduct_gas!(gas_counter, DEPLOY);

        // Assign the starknet address of the contract.
        let deployer_address = if deploy_from_zero {
            Felt252::zero()
        } else {
            self.starknet_state.exec_info.contract_address
        };
        let deployed_contract_address = calculate_contract_address(
            &_contract_address_salt,
            &class_hash,
            &calldata,
            &deployer_address,
        );

        // Prepare runner for running the constructor.
        let runner = self.runner.expect("Runner is needed for starknet.");
        let Some(contract_info) = runner.starknet_contracts_info.get(&class_hash) else {
            fail_syscall!(b"CLASS_HASH_NOT_FOUND");
        };

        // Set the class hash of the deployed contract before executing the constructor,
        // as the constructor could make an external call to this address.
        if self
            .starknet_state
            .deployed_contracts
            .insert(deployed_contract_address, class_hash)
            .is_some()
        {
            fail_syscall!(b"CONTRACT_ALREADY_DEPLOYED");
        }

        // Call constructor if it exists.
        let (res_data_start, res_data_end) = if let Some(constructor) = &contract_info.constructor {
            let old_addrs = self
                .starknet_state
                .open_caller_context((deployed_contract_address, deployer_address));
            let res = self.call_entry_point(gas_counter, runner, constructor, calldata, vm);
            self.starknet_state.close_caller_context(old_addrs);
            match res {
                Ok(value) => value,
                Err(mut revert_reason) => {
                    self.starknet_state.deployed_contracts.remove(&deployed_contract_address);
                    fail_syscall!(revert_reason, b"CONSTRUCTOR_FAILED");
                }
            }
        } else if calldata.is_empty() {
            (Relocatable::from((0, 0)), Relocatable::from((0, 0)))
        } else {
            // Remove the contract from the deployed contracts,
            // since it failed to deploy.
            self.starknet_state.deployed_contracts.remove(&deployed_contract_address);
            fail_syscall!(b"INVALID_CALLDATA_LEN");
        };

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

        let old_addrs = self.starknet_state.open_caller_context((
            contract_address,
            self.starknet_state.exec_info.contract_address,
        ));
        let res = self.call_entry_point(gas_counter, runner, entry_point, calldata, vm);
        self.starknet_state.close_caller_context(old_addrs);

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
        deduct_gas!(gas_counter, REPLACE_CLASS);
        // Validating the class hash was declared as one of the starknet contracts.
        if !self
            .runner
            .expect("Runner is needed for starknet.")
            .starknet_contracts_info
            .contains_key(&new_class)
        {
            fail_syscall!(b"CLASS_HASH_NOT_FOUND");
        };
        let address = self.starknet_state.exec_info.contract_address;
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
    ) -> Result<(Relocatable, Relocatable), Vec<Felt252>> {
        let function = runner
            .builder
            .registry()
            .get_function(entry_point)
            .expect("Entrypoint exists, but not found.");
        let mut res = runner
            .run_function_with_starknet_context(
                function,
                vec![Arg::Array(calldata.into_iter().map(Arg::Value).collect())],
                // The costs of the relevant syscall include `ENTRY_POINT_INITIAL_BUDGET` so we
                // need to refund it here before running the entry point to avoid double charging.
                Some(*gas_counter + gas_costs::ENTRY_POINT_INITIAL_BUDGET),
                self.starknet_state.clone(),
            )
            .expect("Internal runner error.");
        self.syscalls_used_resources += res.used_resources;
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
                extract_relocatable(vm, dst)?,
            )?,
            ExternalHint::WriteRunParam { index, dst } => {
                let index = get_val(vm, index)?.to_usize().expect("Got a bad index.");
                let mut stack = vec![(cell_ref_to_relocatable(dst, vm), &self.user_args[index])];
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
            ExternalHint::Blake2sCompress { state, byte_count, message, output, finalize } => {
                let state = extract_relocatable(vm, state)?;
                let byte_count = get_val(vm, byte_count)?;
                let message = extract_relocatable(vm, message)?;
                let felt_to_u32 = |value: Felt252| value.to_le_digits()[0].try_into().ok();

                let finalize = get_val(vm, finalize)?.is_one();

                let into_u32 = |opt: Option<Cow<'_, _>>| match opt {
                    Some(val) => {
                        if let MaybeRelocatable::Int(value) = *val {
                            value.to_le_digits()[0].try_into().ok()
                        } else {
                            None
                        }
                    }
                    None => None,
                };

                let state = vm
                    .get_range(state, 8)
                    .into_iter()
                    .map(into_u32)
                    .collect::<Option<Vec<u32>>>()
                    .unwrap();
                let state: [u32; 8] = state[0..8].try_into().unwrap();

                let message = vm
                    .get_range(message, 16)
                    .into_iter()
                    .map(into_u32)
                    .collect::<Option<Vec<u32>>>()
                    .unwrap();

                let new_state = blake2s_compress(
                    &state,
                    &message.try_into().unwrap(),
                    felt_to_u32(byte_count).unwrap(),
                    0,
                    if finalize { 0xffffffff } else { 0x00 },
                    0,
                );

                let output = extract_relocatable(vm, output)?;

                for (i, &val) in new_state.iter().enumerate() {
                    vm.insert_value((output + i)?, MaybeRelocatable::Int(Felt252::from(val)))?;
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
    if data.len() % 17 != 0 {
        fail_syscall!(b"Invalid keccak input size");
    }
    let mut state = [0u64; 25];
    for chunk in data.chunks(17) {
        deduct_gas!(gas_counter, KECCAK_ROUND_COST);
        for (i, val) in chunk.iter().enumerate() {
            state[i] ^= val.to_u64().unwrap();
        }
        keccak::f1600(&mut state)
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
    let data_as_bytes = sha2::digest::generic_array::GenericArray::from_exact_iter(
        data.iter().flat_map(|felt| felt.to_bigint().to_u32().unwrap().to_be_bytes()),
    )
    .unwrap();
    let mut state_as_words: [u32; 8] = prev_state
        .iter()
        .map(|felt| felt.to_bigint().to_u32().unwrap())
        .collect_vec()
        .try_into()
        .unwrap();
    sha2::compress256(&mut state_as_words, &[data_as_bytes]);
    let next_state_ptr = alloc_memory(exec_scopes, vm.vm(), 8)?;
    let mut buff: MemBuffer<'_> = MemBuffer::new(vm, next_state_ptr);
    buff.write_data(state_as_words.into_iter().map(Felt252::from))?;
    Ok(SyscallResult::Success(vec![next_state_ptr.into()]))
}

// --- secp256k1 ---

/// Executes the `secp256k1_new_syscall` syscall.
fn secp256k1_new(
    gas_counter: &mut usize,
    x: BigUint,
    y: BigUint,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256K1_NEW);
    let modulus = <secp256k1::Fq as PrimeField>::MODULUS.into();
    if x >= modulus || y >= modulus {
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
    deduct_gas!(gas_counter, SECP256K1_ADD);
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
    scalar: BigUint,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256K1_MUL);

    let ec = get_secp256k1_exec_scope(exec_scopes)?;
    let p = &ec.ec_points[p_id];
    let product = *p * secp256k1::Fr::from(scalar);
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
    deduct_gas!(gas_counter, SECP256K1_GET_POINT_FROM_X);
    if x >= <secp256k1::Fq as PrimeField>::MODULUS.into() {
        fail_syscall!(b"Coordinates out of range");
    }
    let x = x.into();
    let maybe_p = secp256k1::Affine::get_ys_from_x_unchecked(x)
        .map(
            |(smaller, greater)|
            // Return the correct y coordinate based on the parity.
            if smaller.into_bigint().is_odd() == y_parity { smaller } else { greater },
        )
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
    deduct_gas!(gas_counter, SECP256K1_GET_XY);
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
    deduct_gas!(gas_counter, SECP256R1_GET_POINT_FROM_X);
    let modulus = <secp256r1::Fq as PrimeField>::MODULUS.into();
    if x >= modulus || y >= modulus {
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
    deduct_gas!(gas_counter, SECP256R1_ADD);
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
    scalar: BigUint,
    exec_scopes: &mut ExecutionScopes,
) -> Result<SyscallResult, HintError> {
    deduct_gas!(gas_counter, SECP256R1_MUL);

    let ec = get_secp256r1_exec_scope(exec_scopes)?;
    let p = &ec.ec_points[p_id];
    let product = *p * secp256r1::Fr::from(scalar);
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
    deduct_gas!(gas_counter, SECP256R1_NEW);
    if x >= <secp256r1::Fq as PrimeField>::MODULUS.into() {
        fail_syscall!(b"Coordinates out of range");
    }
    let x = x.into();
    let maybe_p = secp256r1::Affine::get_ys_from_x_unchecked(x)
        .map(
            |(smaller, greater)|
            // Return the correct y coordinate based on the parity.
            if smaller.into_bigint().is_odd() == y_parity { smaller } else { greater },
        )
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
    deduct_gas!(gas_counter, SECP256R1_GET_XY);
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

// ---

pub fn execute_core_hint_base(
    vm: &mut VirtualMachine,
    exec_scopes: &mut ExecutionScopes,
    core_hint_base: &cairo_lang_casm::hints::CoreHintBase,
    no_temporary_segments: bool,
) -> Result<(), HintError> {
    match core_hint_base {
        cairo_lang_casm::hints::CoreHintBase::Core(core_hint) => {
            execute_core_hint(vm, exec_scopes, core_hint, no_temporary_segments)
        }
        cairo_lang_casm::hints::CoreHintBase::Deprecated(deprecated_hint) => {
            execute_deprecated_hint(vm, exec_scopes, deprecated_hint)
        }
    }
}

pub fn execute_deprecated_hint(
    vm: &mut VirtualMachine,
    exec_scopes: &mut ExecutionScopes,
    deprecated_hint: &cairo_lang_casm::hints::DeprecatedHint,
) -> Result<(), HintError> {
    match deprecated_hint {
        DeprecatedHint::Felt252DictRead { dict_ptr, key, value_dst } => {
            let dict_address = extract_relocatable(vm, dict_ptr)?;
            let key = get_val(vm, key)?;
            let dict_manager_exec_scope = exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to read from a dict while dict manager was not initialized.");
            let value = dict_manager_exec_scope
                .get_from_tracker(dict_address, &key)
                .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
            insert_value_to_cellref!(vm, value_dst, value)?;
        }
        DeprecatedHint::Felt252DictWrite { dict_ptr, key, value } => {
            let dict_address = extract_relocatable(vm, dict_ptr)?;
            let key = get_val(vm, key)?;
            let value = get_maybe(vm, value)?;
            let dict_manager_exec_scope = exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to write to a dict while dict manager was not initialized.");
            let prev_value = dict_manager_exec_scope
                .get_from_tracker(dict_address, &key)
                .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
            vm.insert_value((dict_address + 1)?, prev_value)?;
            dict_manager_exec_scope.insert_to_tracker(dict_address, key, value);
        }
        DeprecatedHint::AssertCurrentAccessIndicesIsEmpty
        | DeprecatedHint::AssertAllAccessesUsed { .. }
        | DeprecatedHint::AssertAllKeysUsed
        | DeprecatedHint::AssertLeAssertThirdArcExcluded
        | DeprecatedHint::AssertLtAssertValidInput { .. } => {}
    }
    Ok(())
}

/// Allocates a memory buffer of size `size` on a vm segment.
/// Segment will be reused between calls.
fn alloc_memory(
    exec_scopes: &mut ExecutionScopes,
    vm: &mut VirtualMachine,
    size: usize,
) -> Result<Relocatable, HintError> {
    const NAME: &str = "memory_exec_scope";
    if exec_scopes.get_ref::<MemoryExecScope>(NAME).is_err() {
        exec_scopes.assign_or_update_variable(
            NAME,
            Box::new(MemoryExecScope { next_address: vm.add_memory_segment() }),
        );
    }
    let scope = exec_scopes.get_mut_ref::<MemoryExecScope>(NAME)?;
    let updated = (scope.next_address + size)?;
    Ok(std::mem::replace(&mut scope.next_address, updated))
}

/// Executes a core hint.
pub fn execute_core_hint(
    vm: &mut VirtualMachine,
    exec_scopes: &mut ExecutionScopes,
    core_hint: &CoreHint,
    no_temporary_segments: bool,
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
        CoreHint::TestLessThanOrEqual { lhs, rhs, dst }
        | CoreHint::TestLessThanOrEqualAddress { lhs, rhs, dst } => {
            let lhs_val = get_maybe(vm, lhs)?;
            let rhs_val = get_maybe(vm, rhs)?;
            insert_value_to_cellref!(
                vm,
                dst,
                if lhs_val <= rhs_val { Felt252::from(1) } else { Felt252::from(0) }
            )?;
        }
        CoreHint::WideMul128 { lhs, rhs, high, low } => {
            let mask128 = BigUint::from(u128::MAX);
            let lhs_val = get_val(vm, lhs)?.to_biguint();
            let rhs_val = get_val(vm, rhs)?.to_biguint();
            let prod = lhs_val * rhs_val;
            insert_value_to_cellref!(vm, high, Felt252::from(prod.clone() >> 128))?;
            insert_value_to_cellref!(vm, low, Felt252::from(prod & mask128))?;
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
            dividend0,
            dividend1,
            divisor0,
            divisor1,
            quotient0,
            quotient1,
            remainder0,
            remainder1,
        } => {
            let pow_2_128 = BigUint::from(u128::MAX) + 1u32;
            let dividend0 = get_val(vm, dividend0)?.to_biguint();
            let dividend1 = get_val(vm, dividend1)?.to_biguint();
            let divisor0 = get_val(vm, divisor0)?.to_biguint();
            let divisor1 = get_val(vm, divisor1)?.to_biguint();
            let dividend: BigUint = dividend0 + dividend1.shl(128);
            let divisor = divisor0 + divisor1.shl(128);
            let (quotient, remainder) = dividend.div_rem(&divisor);
            let (limb1, limb0) = quotient.div_rem(&pow_2_128);
            insert_value_to_cellref!(vm, quotient0, Felt252::from(limb0))?;
            insert_value_to_cellref!(vm, quotient1, Felt252::from(limb1))?;
            let (limb1, limb0) = remainder.div_rem(&pow_2_128);
            insert_value_to_cellref!(vm, remainder0, Felt252::from(limb0))?;
            insert_value_to_cellref!(vm, remainder1, Felt252::from(limb1))?;
        }
        CoreHint::Uint512DivModByUint256 {
            dividend0,
            dividend1,
            dividend2,
            dividend3,
            divisor0,
            divisor1,
            quotient0,
            quotient1,
            quotient2,
            quotient3,
            remainder0,
            remainder1,
        } => {
            let pow_2_128 = BigUint::from(u128::MAX) + 1u32;
            let dividend0 = get_val(vm, dividend0)?.to_biguint();
            let dividend1 = get_val(vm, dividend1)?.to_biguint();
            let dividend2 = get_val(vm, dividend2)?.to_biguint();
            let dividend3 = get_val(vm, dividend3)?.to_biguint();
            let divisor0 = get_val(vm, divisor0)?.to_biguint();
            let divisor1 = get_val(vm, divisor1)?.to_biguint();
            let dividend: BigUint =
                dividend0 + dividend1.shl(128) + dividend2.shl(256) + dividend3.shl(384);
            let divisor = divisor0 + divisor1.shl(128);
            let (quotient, remainder) = dividend.div_rem(&divisor);
            let (quotient, limb0) = quotient.div_rem(&pow_2_128);
            insert_value_to_cellref!(vm, quotient0, Felt252::from(limb0))?;
            let (quotient, limb1) = quotient.div_rem(&pow_2_128);
            insert_value_to_cellref!(vm, quotient1, Felt252::from(limb1))?;
            let (limb3, limb2) = quotient.div_rem(&pow_2_128);
            insert_value_to_cellref!(vm, quotient2, Felt252::from(limb2))?;
            insert_value_to_cellref!(vm, quotient3, Felt252::from(limb3))?;
            let (limb1, limb0) = remainder.div_rem(&pow_2_128);
            insert_value_to_cellref!(vm, remainder0, Felt252::from(limb0))?;
            insert_value_to_cellref!(vm, remainder1, Felt252::from(limb1))?;
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
            let value = get_val(vm, value)?;
            let scalar = get_val(vm, scalar)?;
            let max_x = get_val(vm, max_x)?;
            let x_value = value.floor_div(&NonZeroFelt::from_felt_unchecked(scalar)).min(max_x);
            let y_value = value - x_value * scalar;
            insert_value_to_cellref!(vm, x, x_value)?;
            insert_value_to_cellref!(vm, y, y_value)?;
        }
        CoreHint::RandomEcPoint { x, y } => {
            // Keep sampling a random field element `X` until `X^3 + X + beta` is a quadratic
            // residue.
            let mut rng = rand::rng();
            let (random_x, random_y) = loop {
                // Randominzing 31 bytes to make sure is in range.
                // TODO(orizi): Use `Felt252` random implementation when exists.
                let x_bytes: [u8; 31] = rng.random();
                let random_x = Felt252::from_bytes_be_slice(&x_bytes);
                /// The Beta value of the Starkware elliptic curve.
                pub const BETA: Felt252 = Felt252::from_hex_unchecked(
                    "0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89",
                );
                let random_y_squared = random_x * random_x * random_x + random_x + BETA;
                if let Some(random_y) = random_y_squared.sqrt() {
                    break (random_x, random_y);
                }
            };
            insert_value_to_cellref!(vm, x, random_x)?;
            insert_value_to_cellref!(vm, y, random_y)?;
        }
        CoreHint::FieldSqrt { val, sqrt } => {
            let val = get_val(vm, val)?;
            let res = val.sqrt().unwrap_or_else(|| (val * Felt252::THREE).sqrt().unwrap());
            insert_value_to_cellref!(vm, sqrt, std::cmp::min(res, -res))?;
        }
        CoreHint::AllocFelt252Dict { segment_arena_ptr } => {
            let dict_manager_address = extract_relocatable(vm, segment_arena_ptr)?;
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
            let new_dict_segment =
                dict_manager_exec_scope.new_default_dict(vm, no_temporary_segments);
            vm.insert_value((dict_infos_base + 3 * n_dicts)?, new_dict_segment)?;
        }
        CoreHint::Felt252DictEntryInit { dict_ptr, key } => {
            let dict_address = extract_relocatable(vm, dict_ptr)?;
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
            let value = get_maybe(vm, value)?;
            let dict_manager_exec_scope = exec_scopes
                .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to write to a dict while dict manager was not initialized.");
            dict_manager_exec_scope.insert_to_tracker(dict_address, key, value);
        }
        CoreHint::GetSegmentArenaIndex { dict_end_ptr, dict_index, .. } => {
            let dict_address = extract_relocatable(vm, dict_end_ptr)?;
            let dict_manager_exec_scope = exec_scopes
                .get_ref::<DictManagerExecScope>("dict_manager_exec_scope")
                .expect("Trying to read from a dict while dict manager was not initialized.");
            let dict_infos_index = dict_manager_exec_scope.get_dict_infos_index(dict_address);
            insert_value_to_cellref!(vm, dict_index, Felt252::from(dict_infos_index))?;
        }
        CoreHint::InitSquashData { dict_accesses, n_accesses, first_key, big_keys, .. } => {
            let dict_access_size = 3;
            let rangecheck_bound = Felt252::from(BigInt::from(1).shl(128));

            exec_scopes.assign_or_update_variable(
                "dict_squash_exec_scope",
                Box::<DictSquashExecScope>::default(),
            );
            let dict_squash_exec_scope =
                exec_scopes.get_mut_ref::<DictSquashExecScope>("dict_squash_exec_scope")?;
            let dict_accesses_address = extract_relocatable(vm, dict_accesses)?;
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
            let range_check_ptr = extract_relocatable(vm, range_check_ptr)?;
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
            let index_delta_minus_1_val = (*dict_squash_exec_scope.current_access_index().unwrap()
                - prev_access_index)
                .sub(1);

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
        CoreHint::GetNextDictKey { next_key } => {
            let dict_squash_exec_scope: &mut DictSquashExecScope =
                exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
            dict_squash_exec_scope.pop_current_key();
            insert_value_to_cellref!(vm, next_key, dict_squash_exec_scope.current_key().unwrap())?;
        }
        CoreHint::AssertLeFindSmallArcs { a, b, range_check_ptr } => {
            let a_val = get_val(vm, a)?;
            let b_val = get_val(vm, b)?;
            let mut lengths_and_indices =
                [(a_val, 0), (b_val - a_val, 1), (Felt252::from(-1) - b_val, 2)];
            lengths_and_indices.sort();
            exec_scopes
                .assign_or_update_variable("excluded_arc", Box::new(lengths_and_indices[2].1));
            // ceil((PRIME / 3) / 2 ** 128).
            let prime_over_3_high = 3544607988759775765608368578435044694_u128;
            // ceil((PRIME / 2) / 2 ** 128).
            let prime_over_2_high = 5316911983139663648412552867652567041_u128;
            let range_check_ptr = extract_relocatable(vm, range_check_ptr)?;
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
        CoreHint::DebugPrint { start, end } => {
            print!("{}", format_for_debug(read_felts(vm, start, end)?.into_iter()));
        }
        CoreHint::AllocConstantSize { size, dst } => {
            let object_size = get_val(vm, size)?.to_usize().expect("Object size too large.");
            let ptr = alloc_memory(exec_scopes, vm, object_size)?;
            insert_value_to_cellref!(vm, dst, ptr)?;
        }
        CoreHint::U256InvModN {
            b0,
            b1,
            n0,
            n1,
            g0_or_no_inv,
            g1_option,
            s_or_r0,
            s_or_r1,
            t_or_k0,
            t_or_k1,
        } => {
            let pow_2_128 = BigInt::from(u128::MAX) + 1u32;
            let b0 = get_val(vm, b0)?.to_bigint();
            let b1 = get_val(vm, b1)?.to_bigint();
            let n0 = get_val(vm, n0)?.to_bigint();
            let n1 = get_val(vm, n1)?.to_bigint();
            let b: BigInt = b0.clone() + b1.clone().shl(128);
            let n: BigInt = n0 + n1.shl(128);
            let ExtendedGcd { gcd: mut g, x: _, y: mut r } = n.extended_gcd(&b);
            if n == 1.into() {
                insert_value_to_cellref!(vm, s_or_r0, Felt252::from(b0))?;
                insert_value_to_cellref!(vm, s_or_r1, Felt252::from(b1))?;
                insert_value_to_cellref!(vm, t_or_k0, Felt252::from(1))?;
                insert_value_to_cellref!(vm, t_or_k1, Felt252::from(0))?;
                insert_value_to_cellref!(vm, g0_or_no_inv, Felt252::from(1))?;
                insert_value_to_cellref!(vm, g1_option, Felt252::from(0))?;
            } else if g != 1.into() {
                // This makes sure `g0_or_no_inv` is always non-zero in the no inverse case.
                if g.is_even() {
                    g = 2u32.into();
                }
                let (limb1, limb0) = (&b / &g).div_rem(&pow_2_128);
                insert_value_to_cellref!(vm, s_or_r0, Felt252::from(limb0))?;
                insert_value_to_cellref!(vm, s_or_r1, Felt252::from(limb1))?;
                let (limb1, limb0) = (&n / &g).div_rem(&pow_2_128);
                insert_value_to_cellref!(vm, t_or_k0, Felt252::from(limb0))?;
                insert_value_to_cellref!(vm, t_or_k1, Felt252::from(limb1))?;
                let (limb1, limb0) = g.div_rem(&pow_2_128);
                insert_value_to_cellref!(vm, g0_or_no_inv, Felt252::from(limb0))?;
                insert_value_to_cellref!(vm, g1_option, Felt252::from(limb1))?;
            } else {
                r %= &n;
                if r.is_negative() {
                    r += &n;
                }
                let k: BigInt = (&r * b - 1) / n;
                let (limb1, limb0) = r.div_rem(&pow_2_128);
                insert_value_to_cellref!(vm, s_or_r0, Felt252::from(limb0))?;
                insert_value_to_cellref!(vm, s_or_r1, Felt252::from(limb1))?;
                let (limb1, limb0) = k.div_rem(&pow_2_128);
                insert_value_to_cellref!(vm, t_or_k0, Felt252::from(limb0))?;
                insert_value_to_cellref!(vm, t_or_k1, Felt252::from(limb1))?;
                insert_value_to_cellref!(vm, g0_or_no_inv, Felt252::from(0))?;
            }
        }
        CoreHint::EvalCircuit {
            n_add_mods, add_mod_builtin, n_mul_mods, mul_mod_builtin, ..
        } => {
            let add_mod_builtin = extract_relocatable(vm, add_mod_builtin)?;
            let n_add_mods = get_val(vm, n_add_mods)?.to_usize().unwrap();
            let mul_mod_builtin = extract_relocatable(vm, mul_mod_builtin)?;
            let n_mul_mods = get_val(vm, n_mul_mods)?.to_usize().unwrap();

            circuit::eval_circuit(vm, add_mod_builtin, n_add_mods, mul_mod_builtin, n_mul_mods)?;
        }
    };
    Ok(())
}

/// Reads a range of `Felt252`s from the VM.
fn read_felts(
    vm: &mut VirtualMachine,
    start: &ResOperand,
    end: &ResOperand,
) -> Result<Vec<Felt252>, HintError> {
    let mut curr = extract_relocatable(vm, start)?;
    let end = extract_relocatable(vm, end)?;

    let mut felts = Vec::new();
    while curr != end {
        let value = *vm.get_integer(curr)?;
        felts.push(value);
        curr = (curr + 1)?;
    }

    Ok(felts)
}

/// Reads the result of a function call that returns `Array<felt252>`.
fn read_array_result_as_vec(memory: &[Option<Felt252>], value: &[Felt252]) -> Vec<Felt252> {
    // TODO(spapini): Handle failures.
    let [res_start, res_end] = value else {
        panic!("Unexpected return value from contract call");
    };
    let res_start: usize = res_start.clone().to_bigint().try_into().unwrap();
    let res_end: usize = res_end.clone().to_bigint().try_into().unwrap();
    (res_start..res_end).map(|i| memory[i].unwrap()).collect()
}

/// Loads a range of values from the VM memory.
pub fn vm_get_range(
    vm: &mut VirtualMachine,
    mut calldata_start_ptr: Relocatable,
    calldata_end_ptr: Relocatable,
) -> Result<Vec<Felt252>, HintError> {
    let mut values = vec![];
    while calldata_start_ptr != calldata_end_ptr {
        let val = *vm.get_integer(calldata_start_ptr)?;
        values.push(val);
        calldata_start_ptr.offset += 1;
    }
    Ok(values)
}

/// Extracts a parameter assumed to be a buffer.
pub fn extract_buffer(buffer: &ResOperand) -> (&CellRef, Felt252) {
    let (cell, base_offset) = match buffer {
        ResOperand::Deref(cell) => (cell, 0.into()),
        ResOperand::BinOp(BinOpOperand { op: Operation::Add, a, b }) => {
            (a, extract_matches!(b, DerefOrImmediate::Immediate).clone().value.into())
        }
        _ => panic!("Illegal argument for a buffer."),
    };
    (cell, base_offset)
}

/// Runs CairoRunner on layout with prime.
/// Allows injecting custom CairoRunner.
pub fn run_function_with_runner(
    additional_initialization: impl FnOnce(&mut VirtualMachine) -> Result<(), Box<CairoRunError>>,
    hint_processor: &mut dyn HintProcessor,
    runner: &mut CairoRunner,
) -> Result<(), Box<CairoRunError>> {
    let end = runner.initialize(true).map_err(CairoRunError::from)?;

    additional_initialization(&mut runner.vm)?;

    runner.run_until_pc(end, hint_processor).map_err(CairoRunError::from)?;
    runner.end_run(true, false, hint_processor).map_err(CairoRunError::from)?;
    runner.relocate(true).map_err(CairoRunError::from)?;
    Ok(())
}

/// Creates CairoRunner for `program`.
pub fn build_cairo_runner(
    data: Vec<MaybeRelocatable>,
    builtins: Vec<BuiltinName>,
    hints_dict: HashMap<usize, Vec<HintParams>>,
) -> Result<CairoRunner, Box<CairoRunError>> {
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
    CairoRunner::new(&program, LayoutName::all_cairo, false, true)
        .map_err(CairoRunError::from)
        .map_err(Box::new)
}

/// The result of [run_function].
pub struct RunFunctionResult {
    /// The ap value after the run.
    pub ap: usize,
    /// The used resources after the run.
    pub used_resources: ExecutionResources,
    /// The relocated memory after the run.
    pub memory: Vec<Option<Felt252>>,
    /// The relocated trace.
    pub relocated_trace: Vec<RelocatedTraceEntry>,
}

/// Runs `bytecode` on layout with prime, and returns the matching [RunFunctionResult].
/// Allows injecting custom HintProcessor.
pub fn run_function<'a, 'b: 'a>(
    bytecode: impl Iterator<Item = &'a BigInt> + Clone,
    builtins: Vec<BuiltinName>,
    additional_initialization: impl FnOnce(&mut VirtualMachine) -> Result<(), Box<CairoRunError>>,
    hint_processor: &mut dyn HintProcessor,
    hints_dict: HashMap<usize, Vec<HintParams>>,
) -> Result<RunFunctionResult, Box<CairoRunError>> {
    let data: Vec<MaybeRelocatable> =
        bytecode.map(Felt252::from).map(MaybeRelocatable::from).collect();
    let mut runner = build_cairo_runner(data, builtins, hints_dict)?;

    run_function_with_runner(additional_initialization, hint_processor, &mut runner)?;

    let used_resources = runner
        .get_execution_resources()
        .expect("Failed to get execution resources, but the run was successful.");

    let relocated_trace = runner.relocated_trace.unwrap();
    let memory = runner.relocated_memory;

    Ok(RunFunctionResult {
        ap: relocated_trace.last().unwrap().ap,
        used_resources,
        memory,
        relocated_trace,
    })
}

/// Formats the given felts as a debug string.
fn format_for_debug(mut felts: IntoIter<Felt252>) -> String {
    let mut items = Vec::new();
    while let Some(item) = format_next_item(&mut felts) {
        items.push(item);
    }
    if let [item] = &items[..] {
        if item.is_string {
            return item.item.clone();
        }
    }
    items
        .into_iter()
        .map(|item| {
            if item.is_string {
                format!("{}\n", item.item)
            } else {
                format!("[DEBUG]\t{}\n", item.item)
            }
        })
        .join("")
}

/// A formatted string representation of anything formattable (e.g. ByteArray, felt, short-string).
pub struct FormattedItem {
    /// The formatted string representing the item.
    item: String,
    /// Whether the item is a string.
    is_string: bool,
}
impl FormattedItem {
    /// Returns the formatted item as is.
    pub fn get(self) -> String {
        self.item
    }
    /// Wraps the formatted item with quote, if it's a string. Otherwise returns it as is.
    pub fn quote_if_string(self) -> String {
        if self.is_string { format!("\"{}\"", self.item) } else { self.item }
    }
}

/// Formats a string or a short string / `felt252`. Returns the formatted string and a boolean
/// indicating whether it's a string. If can't format the item, returns None.
pub fn format_next_item<T>(values: &mut T) -> Option<FormattedItem>
where
    T: Iterator<Item = Felt252> + Clone,
{
    let first_felt = values.next()?;

    if first_felt == Felt252::from_hex(BYTE_ARRAY_MAGIC).unwrap() {
        if let Some(string) = try_format_string(values) {
            return Some(FormattedItem { item: string, is_string: true });
        }
    }
    Some(FormattedItem { item: format_short_string(&first_felt), is_string: false })
}

/// Formats the given felts as a panic string.
pub fn format_for_panic<T>(mut felts: T) -> String
where
    T: Iterator<Item = Felt252> + Clone,
{
    let mut items = Vec::new();
    while let Some(item) = format_next_item(&mut felts) {
        items.push(item.quote_if_string());
    }
    let panic_values_string =
        if let [item] = &items[..] { item.clone() } else { format!("({})", items.join(", ")) };
    format!("Panicked with {panic_values_string}.")
}

/// Formats a `Felt252`, as a short string if possible.
fn format_short_string(value: &Felt252) -> String {
    let hex_value = value.to_biguint();
    match as_cairo_short_string(value) {
        Some(as_string) => format!("{hex_value:#x} ('{as_string}')"),
        None => format!("{hex_value:#x}"),
    }
}

/// Tries to format a string, represented as a sequence of `Felt252`s.
/// If the sequence is not a valid serialization of a ByteArray, returns None and doesn't change the
/// given iterator (`values`).
fn try_format_string<T>(values: &mut T) -> Option<String>
where
    T: Iterator<Item = Felt252> + Clone,
{
    // Clone the iterator and work with the clone. If the extraction of the string is successful,
    // change the original iterator to the one we worked with. If not, continue with the
    // original iterator at the original point.
    let mut cloned_values_iter = values.clone();

    let num_full_words = cloned_values_iter.next()?.to_usize()?;
    let full_words = cloned_values_iter.by_ref().take(num_full_words).collect_vec();
    let pending_word = cloned_values_iter.next()?;
    let pending_word_len = cloned_values_iter.next()?.to_usize()?;

    let full_words_string = full_words
        .into_iter()
        .map(|word| as_cairo_short_string_ex(&word, BYTES_IN_WORD))
        .collect::<Option<Vec<String>>>()?
        .join("");
    let pending_word_string = as_cairo_short_string_ex(&pending_word, pending_word_len)?;

    // Extraction was successful, change the original iterator to the one we worked with.
    *values = cloned_values_iter;

    Some(format!("{full_words_string}{pending_word_string}"))
}
