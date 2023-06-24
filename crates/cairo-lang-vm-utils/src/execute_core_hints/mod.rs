mod dict_manager;

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec};
use core::ops::Shl;

use ark_ff::{Field, Fp256, MontBackend, MontConfig, PrimeField};
use ark_std::UniformRand;
use cairo_felt::{felt_str as felt252_str, Felt252};
use cairo_lang_casm::hints::{CoreHint, DeprecatedHint};
use cairo_vm::types::exec_scope::ExecutionScopes;
use cairo_vm::types::relocatable::Relocatable;
use cairo_vm::vm::errors::hint_errors::HintError;
use cairo_vm::vm::vm_core::VirtualMachine;
use dict_manager::{DictManagerExecScope, DictSquashExecScope};
use num_bigint::BigUint;
use num_integer::Integer;
use num_traits::{FromPrimitive, ToPrimitive};

use crate::{
    extract_buffer, get_double_deref_val, get_maybe, get_ptr, get_val, insert_value_to_cellref,
};

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

/// Execution scope for constant memory allocation.
struct MemoryExecScope {
    /// The first free address in the segment.
    next_address: Relocatable,
}

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
                let res_big_uint = core::cmp::min(root0, root1);
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
        #[cfg(feature = "std")]
        CoreHint::DebugPrint { start, end } => {
            let as_relocatable = |vm, value| {
                let (base, offset) = extract_buffer(value);
                get_ptr(vm, base, &offset)
            };
            let mut curr = as_relocatable(vm, start)?;
            let end = as_relocatable(vm, end)?;
            while curr != end {
                let value = vm.get_integer(curr)?;
                if let Some(shortstring) =
                    cairo_lang_utils::short_string::as_cairo_short_string(&value)
                {
                    println!("[DEBUG]\t{shortstring: <31}\t(raw: {:#x}", value.to_bigint());
                } else {
                    println!("[DEBUG]\t{:<31}\t(raw: {:#x} ", ' ', value.to_bigint());
                }
                curr += 1;
            }
            println!();
        }
        #[cfg(not(feature = "std"))]
        CoreHint::DebugPrint { .. } => {}
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
