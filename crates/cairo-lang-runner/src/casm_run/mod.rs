use std::any::Any;
use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::{Deref, Shl};

use ark_ff::fields::{Fp256, MontBackend, MontConfig};
use ark_ff::{BigInteger, Field, PrimeField};
use ark_std::UniformRand;
use cairo_felt::{felt_str as felt252_str, Felt252, PRIME_STR};
use cairo_lang_casm::hints::{CoreHint, DeprecatedHint, Hint, StarknetHint};
use cairo_lang_casm::instructions::Instruction;
use cairo_lang_casm::operand::{
    BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand,
};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_utils::extract_matches;
use cairo_vm::hint_processor::hint_processor_definition::{HintProcessor, HintReference};
use cairo_vm::serde::deserialize_program::{
    ApTracking, BuiltinName, FlowTrackingData, HintParams, ReferenceManager,
};
use cairo_vm::types::exec_scope::ExecutionScopes;
use cairo_vm::types::program::Program;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::errors::memory_errors::MemoryError;
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use cairo_vm::vm::runners::cairo_runner::CairoRunner;
use cairo_vm::vm::vm_core::VirtualMachine;
use dict_manager::DictManagerExecScope;
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::{FromPrimitive, ToPrimitive, Zero};
use {ark_secp256k1 as secp256k1, ark_secp256r1 as secp256r1};

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
pub fn hint_to_hint_params(hint: &Hint) -> HintParams {
    HintParams {
        code: hint.to_string(),
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

/// Fetch the `MaybeRelocatable` value from an address.
fn get_maybe_from_addr(
    vm: &VirtualMachine,
    addr: Relocatable,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    vm.get_maybe(&addr).ok_or_else(|| VirtualMachineError::InvalidMemoryValueTemporaryAddress(addr))
}

/// Fetches the maybe relocatable value of a cell from the vm.
fn get_cell_maybe(
    vm: &VirtualMachine,
    cell: &CellRef,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, cell_ref_to_relocatable(cell, vm))
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

/// Fetches the maybe relocatable value of a pointer described by the value at `cell` plus an offset
/// from the vm.
fn get_double_deref_maybe(
    vm: &VirtualMachine,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, get_ptr(vm, cell, offset)?)
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
            Hint::Core(core_hint_base) => {
                return execute_core_hint_base(vm, exec_scopes, core_hint_base);
            }
            Hint::Starknet(hint) => hint,
        };
        match hint {
            StarknetHint::SystemCall { system } => {
                self.execute_syscall(system, vm, exec_scopes)?;
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
            StarknetHint::SetVersion { value } => {
                self.starknet_state.exec_info.tx_info.version = get_val(vm, value)?;
            }
            StarknetHint::SetAccountContractAddress { value } => {
                self.starknet_state.exec_info.tx_info.account_contract_address =
                    get_val(vm, value)?;
            }
            StarknetHint::SetMaxFee { value } => {
                self.starknet_state.exec_info.tx_info.max_fee = get_val(vm, value)?;
            }
            StarknetHint::SetTransactionHash { value } => {
                self.starknet_state.exec_info.tx_info.transaction_hash = get_val(vm, value)?;
            }
            StarknetHint::SetChainId { value } => {
                self.starknet_state.exec_info.tx_info.chain_id = get_val(vm, value)?;
            }
            StarknetHint::SetNonce { value } => {
                self.starknet_state.exec_info.tx_info.nonce = get_val(vm, value)?;
            }
            StarknetHint::SetSignature { start, end } => {
                let (cell, offset) = extract_buffer(start);
                let start = get_ptr(vm, cell, &offset)?;
                let (cell, offset) = extract_buffer(end);
                let end = get_ptr(vm, cell, &offset)?;
                self.starknet_state.exec_info.tx_info.signature = vm_get_range(vm, start, end)?;
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
        let (cell, offset) = extract_buffer(system);
        let system_ptr = get_ptr(vm, cell, &offset)?;
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
                let _keys = system_buffer.next_arr()?;
                let _values = system_buffer.next_arr()?;
                deduct_gas!(gas_counter, 50);
                Ok(SyscallResult::Success(vec![]))
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
            .run_function(
                function,
                &[Arg::Array(calldata)],
                Some(*gas_counter),
                self.starknet_state.clone(),
            )
            .expect("Internal runner error.");

        *gas_counter = res.gas_counter.unwrap().to_usize().unwrap();
        self.starknet_state = std::mem::take(&mut res.starknet_state);
        match res.value {
            RunResultValue::Success(value) => {
                Ok(segment_with_data(vm, read_array_result_as_vec(&res.memory, &value).into_iter())
                    .expect("failed to allocate segment"))
            }
            RunResultValue::Panic(panic_data) => Err(panic_data),
        }
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

// ---

pub fn execute_core_hint_base(
    vm: &mut VirtualMachine,
    exec_scopes: &mut ExecutionScopes,
    core_hint_base: &cairo_lang_casm::hints::CoreHintBase,
) -> Result<(), HintError> {
    match core_hint_base {
        cairo_lang_casm::hints::CoreHintBase::Core(core_hint) => {
            execute_core_hint(vm, exec_scopes, core_hint)
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
        DeprecatedHint::Felt252DictWrite { dict_ptr, key, value } => {
            let (dict_base, dict_offset) = extract_buffer(dict_ptr);
            let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
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
            let value = get_maybe(vm, value)?;
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
        CoreHint::GetNextDictKey { next_key } => {
            let dict_squash_exec_scope: &mut DictSquashExecScope =
                exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
            dict_squash_exec_scope.pop_current_key();
            insert_value_to_cellref!(vm, next_key, dict_squash_exec_scope.current_key().unwrap())?;
        }
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
