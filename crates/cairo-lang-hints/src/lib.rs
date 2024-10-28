use std::borrow::Cow;
use std::ops::{Shl, Sub};

use cairo_lang_casm::hints::{CoreHint, DeprecatedHint};
use cairo_lang_casm::operand::{
    BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand,
};
use cairo_lang_utils::byte_array::{BYTES_IN_WORD, BYTE_ARRAY_MAGIC};
use cairo_lang_utils::extract_matches;
use cairo_vm::types::exec_scope::ExecutionScopes;
use cairo_vm::types::relocatable::{MaybeRelocatable, Relocatable};
use cairo_vm::vm::errors::vm_errors::VirtualMachineError;
use cairo_vm::vm::vm_core::VirtualMachine;
use itertools::Itertools;
use num_bigint::{BigInt, BigUint};
use num_integer::{ExtendedGcd, Integer};
use num_traits::{Signed, ToPrimitive};
use rand::Rng;
use short_string::{as_cairo_short_string, as_cairo_short_string_ex};
use starknet_types_core::felt::{Felt as Felt252, NonZeroFelt};

mod dict_manager;
use std::vec::IntoIter;

use cairo_vm::vm::errors::hint_errors::HintError;
use dict_manager::{DictManagerExecScope, DictSquashExecScope};

mod circuit;
pub mod short_string;

/// Formats the given felts as a debug string.
pub fn format_for_debug(mut felts: IntoIter<Felt252>) -> String {
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

pub fn cell_ref_to_relocatable(cell_ref: &CellRef, vm: &impl VirtualMachineTrait) -> Relocatable {
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
        $vm.insert_value($crate::cell_ref_to_relocatable($cell_ref, $vm), $value)
    };
}

/// Execution scope for constant memory allocation.
struct MemoryExecScope {
    /// The first free address in the segment.
    next_address: Relocatable,
}

/// Fetches the value of a cell from the vm.
fn get_cell_val(
    vm: &impl VirtualMachineTrait,
    cell: &CellRef,
) -> Result<Felt252, VirtualMachineTrait::VmErrorT> {
    Ok(*vm.get_integer(cell_ref_to_relocatable(cell, vm))?)
}

/// Fetch the `MaybeRelocatable` value from an address.
fn get_maybe_from_addr(
    vm: &impl VirtualMachineTrait,
    addr: Relocatable,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    vm.get_maybe(&addr)
        .ok_or_else(|| VirtualMachineError::InvalidMemoryValueTemporaryAddress(Box::new(addr)))
}

/// Fetches the maybe relocatable value of a cell from the vm.
fn get_cell_maybe(
    vm: &impl VirtualMachineTrait,
    cell: &CellRef,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, cell_ref_to_relocatable(cell, vm))
}

/// Fetches the value of a cell plus an offset from the vm, useful for pointers.
pub fn get_ptr(
    vm: impl VirtualMachineTrait,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<Relocatable, VirtualMachineError> {
    Ok((vm.get_relocatable(cell_ref_to_relocatable(cell, vm))? + offset)?)
}

/// Fetches the value of a pointer described by the value at `cell` plus an offset from the vm.
fn get_double_deref_val(
    vm: impl VirtualMachineTrait,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<Felt252, VirtualMachineError> {
    Ok(*vm.get_integer(get_ptr(vm, cell, offset)?)?)
}

/// Fetches the maybe relocatable value of a pointer described by the value at `cell` plus an offset
/// from the vm.
fn get_double_deref_maybe(
    vm: &impl VirtualMachineTrait,
    cell: &CellRef,
    offset: &Felt252,
) -> Result<MaybeRelocatable, VirtualMachineError> {
    get_maybe_from_addr(vm, get_ptr(vm, cell, offset)?)
}

/// Extracts a parameter assumed to be a buffer, and converts it into a relocatable.
pub fn extract_relocatable(
    vm: &impl VirtualMachineTrait,
    buffer: &ResOperand,
) -> Result<Relocatable, VirtualMachineError> {
    let (base, offset) = extract_buffer(buffer);
    get_ptr(vm, base, &offset)
}

/// Fetches the value of `res_operand` from the vm.
pub fn get_val(
    vm: &impl VirtualMachineTrait,
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

/// Fetches the maybe relocatable value of `res_operand` from the vm.
fn get_maybe(
    vm: &impl VirtualMachineTrait,
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

// pub fn execute_core_hint_base(
//     vm: &mut VirtualMachine,
//     exec_scopes: &mut ExecutionScopes,
//     core_hint_base: &cairo_lang_casm::hints::CoreHintBase,
// ) -> Result<(), HintError> { match core_hint_base {
//   cairo_lang_casm::hints::CoreHintBase::Core(core_hint) => { execute_core_hint(vm, exec_scopes,
//   core_hint) } cairo_lang_casm::hints::CoreHintBase::Deprecated(deprecated_hint) => {
//   execute_deprecated_hint(vm, exec_scopes, deprecated_hint) } }
// }

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
pub fn alloc_memory(
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

// /// Executes a core hint.
// pub fn execute_core_hint(
//     vm: &mut VirtualMachine,
//     exec_scopes: &mut ExecutionScopes,
//     core_hint: &cairo_lang_casm::hints::CoreHint,
// ) -> Result<(), HintError> { match core_hint { CoreHint::AllocSegment { dst } => { let segment =
//   vm.add_memory_segment(); insert_value_to_cellref!(vm, dst, segment)?; } CoreHint::TestLessThan
//   { lhs, rhs, dst } => { let lhs_val = get_val(vm, lhs)?; let rhs_val = get_val(vm, rhs)?;
//   insert_value_to_cellref!( vm, dst, if lhs_val < rhs_val { Felt252::from(1) } else {
//   Felt252::from(0) } )?; } CoreHint::TestLessThanOrEqual { lhs, rhs, dst } |
//   CoreHint::TestLessThanOrEqualAddress { lhs, rhs, dst } => { let lhs_val = get_maybe(vm, lhs)?;
//   let rhs_val = get_maybe(vm, rhs)?; insert_value_to_cellref!( vm, dst, if lhs_val <= rhs_val {
//   Felt252::from(1) } else { Felt252::from(0) } )?; } CoreHint::WideMul128 { lhs, rhs, high, low }
//   => { let mask128 = BigUint::from(u128::MAX); let lhs_val = get_val(vm, lhs)?.to_biguint(); let
//   rhs_val = get_val(vm, rhs)?.to_biguint(); let prod = lhs_val * rhs_val;
//   insert_value_to_cellref!(vm, high, Felt252::from(prod.clone() >> 128))?;
//   insert_value_to_cellref!(vm, low, Felt252::from(prod & mask128))?; } CoreHint::DivMod { lhs,
//   rhs, quotient, remainder } => { let lhs_val = get_val(vm, lhs)?.to_biguint(); let rhs_val =
//   get_val(vm, rhs)?.to_biguint(); insert_value_to_cellref!( vm, quotient,
//   Felt252::from(lhs_val.clone() / rhs_val.clone()) )?; insert_value_to_cellref!(vm, remainder,
//   Felt252::from(lhs_val % rhs_val))?; } CoreHint::Uint256DivMod { dividend0, dividend1, divisor0,
//   divisor1, quotient0, quotient1, remainder0, remainder1, } => { let pow_2_128 =
//   BigUint::from(u128::MAX) + 1u32; let dividend0 = get_val(vm, dividend0)?.to_biguint(); let
//   dividend1 = get_val(vm, dividend1)?.to_biguint(); let divisor0 = get_val(vm,
//   divisor0)?.to_biguint(); let divisor1 = get_val(vm, divisor1)?.to_biguint(); let dividend:
//   BigUint = dividend0 + dividend1.shl(128); let divisor = divisor0 + divisor1.shl(128); let
//   (quotient, remainder) = dividend.div_rem(&divisor); let (limb1, limb0) =
//   quotient.div_rem(&pow_2_128); insert_value_to_cellref!(vm, quotient0, Felt252::from(limb0))?;
//   insert_value_to_cellref!(vm, quotient1, Felt252::from(limb1))?; let (limb1, limb0) =
//   remainder.div_rem(&pow_2_128); insert_value_to_cellref!(vm, remainder0, Felt252::from(limb0))?;
//   insert_value_to_cellref!(vm, remainder1, Felt252::from(limb1))?; }
//   CoreHint::Uint512DivModByUint256 { dividend0, dividend1, dividend2, dividend3, divisor0,
//   divisor1, quotient0, quotient1, quotient2, quotient3, remainder0, remainder1, } => { let
//   pow_2_128 = BigUint::from(u128::MAX) + 1u32; let dividend0 = get_val(vm,
//   dividend0)?.to_biguint(); let dividend1 = get_val(vm, dividend1)?.to_biguint(); let dividend2 =
//   get_val(vm, dividend2)?.to_biguint(); let dividend3 = get_val(vm, dividend3)?.to_biguint(); let
//   divisor0 = get_val(vm, divisor0)?.to_biguint(); let divisor1 = get_val(vm,
//   divisor1)?.to_biguint(); let dividend: BigUint = dividend0 + dividend1.shl(128) +
//   dividend2.shl(256) + dividend3.shl(384); let divisor = divisor0 + divisor1.shl(128); let
//   (quotient, remainder) = dividend.div_rem(&divisor); let (quotient, limb0) =
//   quotient.div_rem(&pow_2_128); insert_value_to_cellref!(vm, quotient0, Felt252::from(limb0))?;
//   let (quotient, limb1) = quotient.div_rem(&pow_2_128); insert_value_to_cellref!(vm, quotient1,
//   Felt252::from(limb1))?; let (limb3, limb2) = quotient.div_rem(&pow_2_128);
//   insert_value_to_cellref!(vm, quotient2, Felt252::from(limb2))?; insert_value_to_cellref!(vm,
//   quotient3, Felt252::from(limb3))?; let (limb1, limb0) = remainder.div_rem(&pow_2_128);
//   insert_value_to_cellref!(vm, remainder0, Felt252::from(limb0))?; insert_value_to_cellref!(vm,
//   remainder1, Felt252::from(limb1))?; } CoreHint::SquareRoot { value, dst } => { let val =
//   get_val(vm, value)?.to_biguint(); insert_value_to_cellref!(vm, dst,
//   Felt252::from(val.sqrt()))?; } CoreHint::Uint256SquareRoot { value_low, value_high, sqrt0,
//   sqrt1, remainder_low, remainder_high, sqrt_mul_2_minus_remainder_ge_u128, } => { let pow_2_128
//   = BigUint::from(u128::MAX) + 1u32; let pow_2_64 = BigUint::from(u64::MAX) + 1u32; let value_low
//   = get_val(vm, value_low)?.to_biguint(); let value_high = get_val(vm, value_high)?.to_biguint();
//   let value = value_low + value_high * pow_2_128.clone(); let sqrt = value.sqrt(); let remainder
//   = value - sqrt.clone() * sqrt.clone(); let sqrt_mul_2_minus_remainder_ge_u128_val =
//   sqrt.clone() * 2u32 - remainder.clone() >= pow_2_128;

//             // Guess sqrt limbs.
//             let (sqrt1_val, sqrt0_val) = sqrt.div_rem(&pow_2_64);
//             insert_value_to_cellref!(vm, sqrt0, Felt252::from(sqrt0_val))?;
//             insert_value_to_cellref!(vm, sqrt1, Felt252::from(sqrt1_val))?;

//             let (remainder_high_val, remainder_low_val) = remainder.div_rem(&pow_2_128);
//             // Guess remainder limbs.
//             insert_value_to_cellref!(vm, remainder_low, Felt252::from(remainder_low_val))?;
//             insert_value_to_cellref!(vm, remainder_high, Felt252::from(remainder_high_val))?;
//             insert_value_to_cellref!(
//                 vm,
//                 sqrt_mul_2_minus_remainder_ge_u128,
//                 Felt252::from(usize::from(sqrt_mul_2_minus_remainder_ge_u128_val))
//             )?;
//         }
//         CoreHint::LinearSplit { value, scalar, max_x, x, y } => {
//             let value = get_val(vm, value)?;
//             let scalar = get_val(vm, scalar)?;
//             let max_x = get_val(vm, max_x)?;
//             let x_value = value.floor_div(&NonZeroFelt::from_felt_unchecked(scalar)).min(max_x);
//             let y_value = value - x_value * scalar;
//             insert_value_to_cellref!(vm, x, x_value)?;
//             insert_value_to_cellref!(vm, y, y_value)?;
//         }
//         CoreHint::RandomEcPoint { x, y } => {
//             // Keep sampling a random field element `X` until `X^3 + X + beta` is a quadratic
//             // residue.
//             let mut rng = rand::thread_rng();
//             let (random_x, random_y) = loop {
//                 // Randominzing 31 bytes to make sure is in range.
//                 // TODO(orizi): Use `Felt252` random implementation when exists.
//                 let x_bytes: [u8; 31] = rng.gen();
//                 let random_x = Felt252::from_bytes_be_slice(&x_bytes);
//                 /// The Beta value of the Starkware elliptic curve.
//                 pub const BETA: Felt252 = Felt252::from_hex_unchecked(
//                     "0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89",
//                 );
//                 let random_y_squared = random_x * random_x * random_x + random_x + BETA;
//                 if let Some(random_y) = random_y_squared.sqrt() {
//                     break (random_x, random_y);
//                 }
//             };
//             insert_value_to_cellref!(vm, x, random_x)?;
//             insert_value_to_cellref!(vm, y, random_y)?;
//         }
//         CoreHint::FieldSqrt { val, sqrt } => {
//             let val = get_val(vm, val)?;
//             let res = val.sqrt().unwrap_or_else(|| (val * Felt252::THREE).sqrt().unwrap());
//             insert_value_to_cellref!(vm, sqrt, std::cmp::min(res, -res))?;
//         }
//         CoreHint::AllocFelt252Dict { segment_arena_ptr } => {
//             let dict_manager_address = extract_relocatable(vm, segment_arena_ptr)?;
//             let n_dicts = vm
//                 .get_integer((dict_manager_address - 2)?)?
//                 .into_owned()
//                 .to_usize()
//                 .expect("Number of dictionaries too large.");
//             let dict_infos_base = vm.get_relocatable((dict_manager_address - 3)?)?;

//             let dict_manager_exec_scope = match exec_scopes
//                 .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
//             {
//                 Ok(dict_manager_exec_scope) => dict_manager_exec_scope,
//                 Err(_) => {
//                     exec_scopes.assign_or_update_variable(
//                         "dict_manager_exec_scope",
//                         Box::<DictManagerExecScope>::default(),
//                     );
//                     exec_scopes.get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")?
//                 }
//             };
//             let new_dict_segment = dict_manager_exec_scope.new_default_dict(vm);
//             vm.insert_value((dict_infos_base + 3 * n_dicts)?, new_dict_segment)?;
//         }
//         CoreHint::Felt252DictEntryInit { dict_ptr, key } => {
//             let dict_address = extract_relocatable(vm, dict_ptr)?;
//             let key = get_val(vm, key)?;
//             let dict_manager_exec_scope = exec_scopes
//                 .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
//                 .expect("Trying to write to a dict while dict manager was not initialized.");
//             let prev_value = dict_manager_exec_scope
//                 .get_from_tracker(dict_address, &key)
//                 .unwrap_or_else(|| DictManagerExecScope::DICT_DEFAULT_VALUE.into());
//             vm.insert_value((dict_address + 1)?, prev_value)?;
//         }
//         CoreHint::Felt252DictEntryUpdate { dict_ptr, value } => {
//             let (dict_base, dict_offset) = extract_buffer(dict_ptr);
//             let dict_address = get_ptr(vm, dict_base, &dict_offset)?;
//             let key = get_double_deref_val(vm, dict_base, &(dict_offset + Felt252::from(-3)))?;
//             let value = get_maybe(vm, value)?;
//             let dict_manager_exec_scope = exec_scopes
//                 .get_mut_ref::<DictManagerExecScope>("dict_manager_exec_scope")
//                 .expect("Trying to write to a dict while dict manager was not initialized.");
//             dict_manager_exec_scope.insert_to_tracker(dict_address, key, value);
//         }
//         CoreHint::GetSegmentArenaIndex { dict_end_ptr, dict_index, .. } => {
//             let dict_address = extract_relocatable(vm, dict_end_ptr)?;
//             let dict_manager_exec_scope = exec_scopes
//                 .get_ref::<DictManagerExecScope>("dict_manager_exec_scope")
//                 .expect("Trying to read from a dict while dict manager was not initialized.");
//             let dict_infos_index = dict_manager_exec_scope.get_dict_infos_index(dict_address);
//             insert_value_to_cellref!(vm, dict_index, Felt252::from(dict_infos_index))?;
//         }
//         CoreHint::InitSquashData { dict_accesses, n_accesses, first_key, big_keys, .. } => {
//             let dict_access_size = 3;
//             let rangecheck_bound = Felt252::from(BigInt::from(1).shl(128));

//             exec_scopes.assign_or_update_variable(
//                 "dict_squash_exec_scope",
//                 Box::<DictSquashExecScope>::default(),
//             );
//             let dict_squash_exec_scope =
//                 exec_scopes.get_mut_ref::<DictSquashExecScope>("dict_squash_exec_scope")?;
//             let dict_accesses_address = extract_relocatable(vm, dict_accesses)?;
//             let n_accesses = get_val(vm, n_accesses)?
//                 .to_usize()
//                 .expect("Number of accesses is too large or negative.");
//             for i in 0..n_accesses {
//                 let current_key =
//                     vm.get_integer((dict_accesses_address + i * dict_access_size)?)?;
//                 dict_squash_exec_scope
//                     .access_indices
//                     .entry(current_key.into_owned())
//                     .and_modify(|indices| indices.push(Felt252::from(i)))
//                     .or_insert_with(|| vec![Felt252::from(i)]);
//             }
//             // Reverse the accesses in order to pop them in order later.
//             for (_, accesses) in dict_squash_exec_scope.access_indices.iter_mut() {
//                 accesses.reverse();
//             }
//             dict_squash_exec_scope.keys =
//                 dict_squash_exec_scope.access_indices.keys().cloned().collect();
//             dict_squash_exec_scope.keys.sort_by(|a, b| b.cmp(a));
//             // big_keys indicates if the keys are greater than rangecheck_bound. If they are not
//             // a simple range check is used instead of assert_le_felt252.
//             insert_value_to_cellref!(
//                 vm,
//                 big_keys,
//                 if dict_squash_exec_scope.keys[0] < rangecheck_bound {
//                     Felt252::from(0)
//                 } else {
//                     Felt252::from(1)
//                 }
//             )?;
//             insert_value_to_cellref!(vm, first_key,
// dict_squash_exec_scope.current_key().unwrap())?;         }
//         CoreHint::GetCurrentAccessIndex { range_check_ptr } => {
//             let dict_squash_exec_scope: &mut DictSquashExecScope =
//                 exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
//             let range_check_ptr = extract_relocatable(vm, range_check_ptr)?;
//             let current_access_index = dict_squash_exec_scope.current_access_index().unwrap();
//             vm.insert_value(range_check_ptr, current_access_index)?;
//         }
//         CoreHint::ShouldSkipSquashLoop { should_skip_loop } => {
//             let dict_squash_exec_scope: &mut DictSquashExecScope =
//                 exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
//             insert_value_to_cellref!(
//                 vm,
//                 should_skip_loop,
//                 // The loop verifies that each two consecutive accesses are valid, thus we
//                 // break when there is only one remaining access.
//                 if dict_squash_exec_scope.current_access_indices().unwrap().len() > 1 {
//                     Felt252::from(0)
//                 } else {
//                     Felt252::from(1)
//                 }
//             )?;
//         }
//         CoreHint::GetCurrentAccessDelta { index_delta_minus1 } => {
//             let dict_squash_exec_scope: &mut DictSquashExecScope =
//                 exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
//             let prev_access_index = dict_squash_exec_scope.pop_current_access_index().unwrap();
//             let index_delta_minus_1_val =
// (*dict_squash_exec_scope.current_access_index().unwrap()
//                 - prev_access_index)
//                 .sub(1);

//             insert_value_to_cellref!(vm, index_delta_minus1, index_delta_minus_1_val)?;
//         }
//         CoreHint::ShouldContinueSquashLoop { should_continue } => {
//             let dict_squash_exec_scope: &mut DictSquashExecScope =
//                 exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
//             insert_value_to_cellref!(
//                 vm,
//                 should_continue,
//                 // The loop verifies that each two consecutive accesses are valid, thus we
//                 // break when there is only one remaining access.
//                 if dict_squash_exec_scope.current_access_indices().unwrap().len() > 1 {
//                     Felt252::from(1)
//                 } else {
//                     Felt252::from(0)
//                 }
//             )?;
//         }
//         CoreHint::GetNextDictKey { next_key } => {
//             let dict_squash_exec_scope: &mut DictSquashExecScope =
//                 exec_scopes.get_mut_ref("dict_squash_exec_scope")?;
//             dict_squash_exec_scope.pop_current_key();
//             insert_value_to_cellref!(vm, next_key,
// dict_squash_exec_scope.current_key().unwrap())?;         }
//         CoreHint::AssertLeFindSmallArcs { a, b, range_check_ptr } => {
//             let a_val = get_val(vm, a)?;
//             let b_val = get_val(vm, b)?;
//             let mut lengths_and_indices =
//                 [(a_val, 0), (b_val - a_val, 1), (Felt252::from(-1) - b_val, 2)];
//             lengths_and_indices.sort();
//             exec_scopes
//                 .assign_or_update_variable("excluded_arc", Box::new(lengths_and_indices[2].1));
//             // ceil((PRIME / 3) / 2 ** 128).
//             let prime_over_3_high = 3544607988759775765608368578435044694_u128;
//             // ceil((PRIME / 2) / 2 ** 128).
//             let prime_over_2_high = 5316911983139663648412552867652567041_u128;
//             let range_check_ptr = extract_relocatable(vm, range_check_ptr)?;
//             vm.insert_value(
//                 range_check_ptr,
//                 Felt252::from(lengths_and_indices[0].0.to_biguint() % prime_over_3_high),
//             )?;
//             vm.insert_value(
//                 (range_check_ptr + 1)?,
//                 Felt252::from(lengths_and_indices[0].0.to_biguint() / prime_over_3_high),
//             )?;
//             vm.insert_value(
//                 (range_check_ptr + 2)?,
//                 Felt252::from(lengths_and_indices[1].0.to_biguint() % prime_over_2_high),
//             )?;
//             vm.insert_value(
//                 (range_check_ptr + 3)?,
//                 Felt252::from(lengths_and_indices[1].0.to_biguint() / prime_over_2_high),
//             )?;
//         }
//         CoreHint::AssertLeIsFirstArcExcluded { skip_exclude_a_flag } => {
//             let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
//             insert_value_to_cellref!(
//                 vm,
//                 skip_exclude_a_flag,
//                 if excluded_arc != 0 { Felt252::from(1) } else { Felt252::from(0) }
//             )?;
//         }
//         CoreHint::AssertLeIsSecondArcExcluded { skip_exclude_b_minus_a } => {
//             let excluded_arc: i32 = exec_scopes.get("excluded_arc")?;
//             insert_value_to_cellref!(
//                 vm,
//                 skip_exclude_b_minus_a,
//                 if excluded_arc != 1 { Felt252::from(1) } else { Felt252::from(0) }
//             )?;
//         }
//         CoreHint::DebugPrint { start, end } => {
//             print!("{}", format_for_debug(read_felts(vm, start, end)?.into_iter()));
//         }
//         CoreHint::AllocConstantSize { size, dst } => {
//             let object_size = get_val(vm, size)?.to_usize().expect("Object size too large.");
//             let ptr = alloc_memory(exec_scopes, vm, object_size)?;
//             insert_value_to_cellref!(vm, dst, ptr)?;
//         }
//         CoreHint::U256InvModN {
//             b0,
//             b1,
//             n0,
//             n1,
//             g0_or_no_inv,
//             g1_option,
//             s_or_r0,
//             s_or_r1,
//             t_or_k0,
//             t_or_k1,
//         } => {
//             let pow_2_128 = BigInt::from(u128::MAX) + 1u32;
//             let b0 = get_val(vm, b0)?.to_bigint();
//             let b1 = get_val(vm, b1)?.to_bigint();
//             let n0 = get_val(vm, n0)?.to_bigint();
//             let n1 = get_val(vm, n1)?.to_bigint();
//             let b: BigInt = b0.clone() + b1.clone().shl(128);
//             let n: BigInt = n0 + n1.shl(128);
//             let ExtendedGcd { gcd: mut g, x: _, y: mut r } = n.extended_gcd(&b);
//             if n == 1.into() {
//                 insert_value_to_cellref!(vm, s_or_r0, Felt252::from(b0))?;
//                 insert_value_to_cellref!(vm, s_or_r1, Felt252::from(b1))?;
//                 insert_value_to_cellref!(vm, t_or_k0, Felt252::from(1))?;
//                 insert_value_to_cellref!(vm, t_or_k1, Felt252::from(0))?;
//                 insert_value_to_cellref!(vm, g0_or_no_inv, Felt252::from(1))?;
//                 insert_value_to_cellref!(vm, g1_option, Felt252::from(0))?;
//             } else if g != 1.into() {
//                 // This makes sure `g0_or_no_inv` is always non-zero in the no inverse case.
//                 if g.is_even() {
//                     g = 2u32.into();
//                 }
//                 let (limb1, limb0) = (&b / &g).div_rem(&pow_2_128);
//                 insert_value_to_cellref!(vm, s_or_r0, Felt252::from(limb0))?;
//                 insert_value_to_cellref!(vm, s_or_r1, Felt252::from(limb1))?;
//                 let (limb1, limb0) = (&n / &g).div_rem(&pow_2_128);
//                 insert_value_to_cellref!(vm, t_or_k0, Felt252::from(limb0))?;
//                 insert_value_to_cellref!(vm, t_or_k1, Felt252::from(limb1))?;
//                 let (limb1, limb0) = g.div_rem(&pow_2_128);
//                 insert_value_to_cellref!(vm, g0_or_no_inv, Felt252::from(limb0))?;
//                 insert_value_to_cellref!(vm, g1_option, Felt252::from(limb1))?;
//             } else {
//                 r %= &n;
//                 if r.is_negative() {
//                     r += &n;
//                 }
//                 let k: BigInt = (&r * b - 1) / n;
//                 let (limb1, limb0) = r.div_rem(&pow_2_128);
//                 insert_value_to_cellref!(vm, s_or_r0, Felt252::from(limb0))?;
//                 insert_value_to_cellref!(vm, s_or_r1, Felt252::from(limb1))?;
//                 let (limb1, limb0) = k.div_rem(&pow_2_128);
//                 insert_value_to_cellref!(vm, t_or_k0, Felt252::from(limb0))?;
//                 insert_value_to_cellref!(vm, t_or_k1, Felt252::from(limb1))?;
//                 insert_value_to_cellref!(vm, g0_or_no_inv, Felt252::from(0))?;
//             }
//         }
//         CoreHint::EvalCircuit {
//             n_add_mods, add_mod_builtin, n_mul_mods, mul_mod_builtin, ..
//         } => {
//             let add_mod_builtin = extract_relocatable(vm, add_mod_builtin)?;
//             let n_add_mods = get_val(vm, n_add_mods)?.to_usize().unwrap();
//             let mul_mod_builtin = extract_relocatable(vm, mul_mod_builtin)?;
//             let n_mul_mods = get_val(vm, n_mul_mods)?.to_usize().unwrap();

//             circuit::eval_circuit(vm, add_mod_builtin, n_add_mods, mul_mod_builtin, n_mul_mods)?;
//         }
//     };
//     Ok(())
// }

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

trait VirtualMachineTrait {
    type RelocatableT;
    type MaybeRelocatableT;
    type VmErrorT;

    fn get_integer(&self, key: Self::RelocatableT) -> Result<Cow<Felt252>, Self::VmErrorT>;

    fn get_relocatable(
        &self,
        key: Self::RelocatableT,
    ) -> Result<Self::RelocatableT, Self::VmErrorT>;

    fn get_ap(&self) -> Self::RelocatableT;

    fn get_fp(&self) -> Self::RelocatableT;

    fn insert_value<T: Into<Self::MaybeRelocatableT>>(
        &mut self,
        key: Self::RelocatableT,
        val: T,
    ) -> Result<(), Self::VmErrorT>;
}

impl VirtualMachineTrait for VirtualMachine {
    type RelocatableT = cairo_vm::types::relocatable::Relocatable;
    type MaybeRelocatableT = cairo_vm::types::relocatable::MaybeRelocatable;
    type VmErrorT = VirtualMachineError;

    fn get_integer(&self, key: Self::RelocatableT) -> Result<Cow<Felt252>, Self::VmErrorT> {
        Ok(self.get_integer(key)?)
    }

    fn get_relocatable(
        &self,
        key: Self::RelocatableT,
    ) -> Result<Self::RelocatableT, Self::VmErrorT> {
        Ok(self.get_relocatable(key)?)
    }

    fn get_ap(&self) -> Self::RelocatableT {
        self.get_ap()
    }

    fn get_fp(&self) -> Self::RelocatableT {
        self.get_fp()
    }

    fn insert_value<T: Into<Self::MaybeRelocatableT>>(
        &mut self,
        key: Relocatable,
        val: T,
    ) -> Result<(), Self::VmErrorT> {
        let as_felt: Self::MaybeRelocatableT = val.into();
        self.insert_value(key, as_felt).map_err(VirtualMachineError::Memory)
    }
}

pub fn execute_deprecated_hint2(
    vm: &impl VirtualMachineTrait,
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
