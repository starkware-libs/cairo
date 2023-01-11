use std::vec;

use cairo_lang_casm::builder::{CasmBuildResult, CasmBuilder};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::dict_felt_to::DictFeltToConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

/// Builds instructions for Sierra single cell dict operations.
pub fn build(
    libfunc: &DictFeltToConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        DictFeltToConcreteLibfunc::New(_) => build_dict_felt_to_new(builder),
        DictFeltToConcreteLibfunc::Read(_) => build_dict_felt_to_read(builder),
        DictFeltToConcreteLibfunc::Write(_) => build_dict_felt_to_write(builder),
        DictFeltToConcreteLibfunc::Squash(_) => build_dict_felt_to_squash(builder),
    }
}

/// Handles instruction for creating a new single cell dict.
fn build_dict_felt_to_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [dict_manager_ptr] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    super::add_input_variables! {casm_builder, buffer(2) dict_manager_ptr; };
    casm_build_extend! {casm_builder,
        hint AllocDictFeltTo {dict_manager_ptr: dict_manager_ptr};
        // Previous dict info
        tempvar dict_infos_start = *(dict_manager_ptr++);
        tempvar n_dicts = *(dict_manager_ptr++);
        tempvar n_destructed = *(dict_manager_ptr++);
        let new_dict_manager_ptr = dict_manager_ptr;
        // New dict info
        assert dict_infos_start = *(dict_manager_ptr++);
        const imm_1 = 1;
        tempvar new_n_dicts = n_dicts + imm_1;
        assert new_n_dicts = *(dict_manager_ptr++);
        assert n_destructed = *(dict_manager_ptr++);
        const imm_3 = 3;
        tempvar offset = n_dicts * imm_3;
        tempvar new_dict_end_ptr = dict_infos_start + offset;
        let new_dict_end = *new_dict_end_ptr;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[new_dict_manager_ptr], &[new_dict_end]], None)],
    ))
}

/// Handles instruction for reading from a single cell dict.
fn build_dict_felt_to_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [dict_ptr, key] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    super::add_input_variables! {casm_builder,
        buffer(2) dict_ptr;
        deref key;
    };
    casm_build_extend! {casm_builder,
        tempvar value;
        hint DictFeltToRead {dict_ptr: dict_ptr, key: key} into {value_dst: value};
        // Write the new dict access.
        assert key = *(dict_ptr++);
        assert value = *(dict_ptr++);
        assert value = *(dict_ptr++);
    }
    Ok(builder
        .build_from_casm_builder(casm_builder, [("Fallthrough", &[&[dict_ptr], &[value]], None)]))
}

/// Handles instruction for writing to a single cell dict.
fn build_dict_felt_to_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [dict_ptr, key, value] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    super::add_input_variables! {casm_builder,
        buffer(2) dict_ptr;
        deref key;
        deref value;
    };
    casm_build_extend! {casm_builder,
        tempvar prev_value;
        hint DictFeltToWrite {dict_ptr: dict_ptr, key: key, value: value} into {prev_value_dst: prev_value};
        // Write the new dict access.
        assert key = *(dict_ptr++);
        assert prev_value = *(dict_ptr++);
        assert value = *(dict_ptr++);
    }
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[dict_ptr]], None)]))
}

/// Handles the dict_squash instruction.
fn build_dict_felt_to_squash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check_ptr, gas_builtin, dict_manager_ptr, dict_end_address] =
        builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    super::add_input_variables! {casm_builder,
        buffer(2) dict_manager_ptr;
        buffer(0) range_check_ptr;
        deref gas_builtin;
        buffer(0) dict_end_address;
    };

    let (
        dict_access_size,
        dict_info_size,
        one,
        zero,
        gas_refund_per_access,
        dict_destruct_arg_range_check_ptr,
        dict_destruct_arg_gas_builtin,
        dict_destruct_arg_dict_manager_ptr,
        dict_destruct_arg_dict_end_address,
        final_range_check_ptr,
        final_gas_builtin,
        final_dict_manager_ptr,
        final_squashed_dict_start,
        final_squashed_dict_end,
    ) = {
        casm_build_extend! {casm_builder,
            const dict_access_size = 3;
            const dict_info_size = 3;
            const one = 1;
            const zero = 0;
            const gas_refund_per_access = 70;
            // Push DestructDict arguments.
            tempvar dict_destruct_arg_range_check_ptr = range_check_ptr;
            tempvar dict_destruct_arg_gas_builtin = gas_builtin;
            tempvar dict_destruct_arg_dict_manager_ptr = dict_manager_ptr;
            tempvar dict_destruct_arg_dict_end_address = dict_end_address;
            let (final_range_check_ptr, final_gas_builtin, final_dict_manager_ptr, final_squashed_dict_start, final_squashed_dict_end) = call DestructDict;
            jump DONE;
        };
        (
            dict_access_size,
            dict_info_size,
            one,
            zero,
            gas_refund_per_access,
            dict_destruct_arg_range_check_ptr,
            dict_destruct_arg_gas_builtin,
            dict_destruct_arg_dict_manager_ptr,
            dict_destruct_arg_dict_end_address,
            final_range_check_ptr,
            final_gas_builtin,
            final_dict_manager_ptr,
            final_squashed_dict_start,
            final_squashed_dict_end,
        )
    };

    let (
        dict_finalize_arg_range_check_ptr,
        dict_finalize_arg_dict_accesses_start,
        dict_finalize_arg_dict_accesses_end,
        dict_finalize_arg_default_value,
    ) = {
        casm_build_extend! {casm_builder,
            // Destructs a given dictionary.
            DestructDict:
            localvar dict_index;
            localvar dict_accesses_len;
            localvar local_gas_builtin = dict_destruct_arg_gas_builtin;
            // Guess the index of the dictionary.
            hint GetDictIndex {dict_manager_ptr: dict_destruct_arg_dict_manager_ptr, dict_end_ptr: dict_destruct_arg_dict_end_address} into {dict_index: dict_index};
            localvar infos = *(dict_destruct_arg_dict_manager_ptr++);
            localvar n_dicts = *(dict_destruct_arg_dict_manager_ptr++);
            localvar n_destructed = *(dict_destruct_arg_dict_manager_ptr++);
            // Add a reference the new dict manager pointer to return.
            let new_dict_manager_ptr = dict_destruct_arg_dict_manager_ptr;
            // Verify that dict_index < n_dicts.
            assert dict_index = *(dict_destruct_arg_range_check_ptr++);
            tempvar n_dicts_minus_1 = n_dicts - one;
            tempvar n_dicts_minus_1_minus_index = n_dicts_minus_1 - dict_index;
            assert n_dicts_minus_1_minus_index = *(dict_destruct_arg_range_check_ptr++);
            // Write the missing data in the dict_info (destruction index and the end of the dict_segment).
            tempvar info_offset = dict_index * dict_info_size;
            tempvar info_ptr = infos + info_offset;
            assert n_destructed = info_ptr[2];
            assert dict_destruct_arg_dict_end_address = info_ptr[1];
            // Write a new dict_manager data to the dict_manager segment (same except for the n_destructed which is incremented).
            assert infos = *(dict_destruct_arg_dict_manager_ptr++);
            assert n_dicts = *(dict_destruct_arg_dict_manager_ptr++);
            tempvar n_destructed_plus_1 = n_destructed + one;
            assert n_destructed_plus_1 = *(dict_destruct_arg_dict_manager_ptr++);
            // Find the len of the accesses segment.
            tempvar dict_accesses_start = info_ptr[0];
            assert dict_accesses_len = dict_destruct_arg_dict_end_address - dict_accesses_start;
            // Push DefaultDictFinalize arguments.
            tempvar dict_finalize_arg_range_check_ptr = dict_destruct_arg_range_check_ptr;
            tempvar dict_finalize_arg_dict_accesses_start = info_ptr[0];
            tempvar dict_finalize_arg_dict_accesses_end = dict_destruct_arg_dict_end_address;
            tempvar dict_finalize_arg_default_value = zero;
            let (returned_range_check_ptr, returned_squashed_dict_start, returned_squashed_dict_end) = call DefaultDictFinalize;
            // Find the number of keys
            tempvar squashed_dict_len = returned_squashed_dict_end - returned_squashed_dict_start;
            // The number of refunded acceesses is number_of_accesses - number_of_keys, which equals
            // to dict_accesses_len / dict_access_size - squashed_dict_len / dict_access_size.
            // Use distributivity to conserve one operation.
            tempvar accesses_len_minus_squashed_len = dict_accesses_len - squashed_dict_len;
            tempvar n_refunded_accesses = accesses_len_minus_squashed_len / dict_access_size;
            tempvar gas_to_refund = n_refunded_accesses * gas_refund_per_access;
            // Push the returned variables.
            tempvar final_range_check_ptr = returned_range_check_ptr;
            tempvar final_gas_builtin = local_gas_builtin + gas_to_refund;
            tempvar final_dict_manager_ptr = new_dict_manager_ptr;
            tempvar final_squashed_dict_start = returned_squashed_dict_start;
            tempvar final_squashed_dict_end = returned_squashed_dict_end;
            ret;
        };
        (
            dict_finalize_arg_range_check_ptr,
            dict_finalize_arg_dict_accesses_start,
            dict_finalize_arg_dict_accesses_end,
            dict_finalize_arg_default_value,
        )
    };

    let (
        dict_squash_arg_range_check_ptr,
        dict_squash_arg_dict_accesses_start,
        dict_squash_arg_dict_accesses_end,
        dict_finalize_inner_arg_dict_accesses_start,
        dict_finalize_inner_arg_n_accesses,
        dict_finalize_inner_arg_default_value,
    ) = {
        casm_build_extend! {casm_builder,
            // Finalizes the given default dictionary, and makes sure the initial values of the dictionary
            // were indeed 'default_value'.
            // Returns the squashed dictionary.
            //
            // Soundness guarantee: dict_accesses_end >= dict_accesses_start and the difference is
            // divisible by DictAccess.SIZE.
            DefaultDictFinalize:
            // Allocates function local variables.
            localvar local_range_check_ptr;
            localvar local_squashed_dict_start;
            localvar local_squashed_dict_end;
            ap += 3;
            // Push DictSquash arguments.
            tempvar dict_squash_arg_range_check_ptr = dict_finalize_arg_range_check_ptr;
            tempvar dict_squash_arg_dict_accesses_start = dict_finalize_arg_dict_accesses_start;
            tempvar dict_squash_arg_dict_accesses_end = dict_finalize_arg_dict_accesses_end;
            let (range_check_ptr, squashed_dict_start, squashed_dict_end) = call DictSquash;
            // Store the returned values as local as they are needed after DefaultDictFinalizeInner.
            assert local_range_check_ptr = range_check_ptr;
            assert local_squashed_dict_start = squashed_dict_start;
            assert local_squashed_dict_end = squashed_dict_end;
            // Push DefaultDictFinalizeInner arguments.
            tempvar n_accesses_times_access_size = squashed_dict_end - squashed_dict_start;
            tempvar dict_finalize_inner_arg_dict_accesses_start = squashed_dict_start;
            tempvar dict_finalize_inner_arg_n_accesses = n_accesses_times_access_size / dict_access_size;
            tempvar dict_finalize_inner_arg_default_value = dict_finalize_arg_default_value;
            let () = call DefaultDictFinalizeInner;
            // Push the returned variables.
            tempvar returned_range_check_ptr = local_range_check_ptr;
            tempvar returned_squashed_dict_start = local_squashed_dict_start;
            tempvar returned_squashed_dict_end = local_squashed_dict_end;
            ret;
        }
        (
            dict_squash_arg_range_check_ptr,
            dict_squash_arg_dict_accesses_start,
            dict_squash_arg_dict_accesses_end,
            dict_finalize_inner_arg_dict_accesses_start,
            dict_finalize_inner_arg_n_accesses,
            dict_finalize_inner_arg_default_value,
        )
    };

    {
        casm_build_extend! {casm_builder,
            // Recursively verifies that the initial values of the dictionary  were indeed 'default_value'.
            DefaultDictFinalizeInner:
            jump DictFinalizeInnerAssert if dict_finalize_inner_arg_n_accesses != 0;
            ret;
            DictFinalizeInnerAssert:
            assert dict_finalize_inner_arg_default_value = dict_finalize_inner_arg_dict_accesses_start[1];
            tempvar rec_arg_dict_accesses_start = dict_finalize_inner_arg_dict_accesses_start + dict_access_size;
            tempvar rec_arg_n_accesses = dict_finalize_inner_arg_n_accesses - one;
            tempvar rec_arg_default_value = dict_finalize_inner_arg_default_value;
            let () = call DefaultDictFinalizeInner;
            ret;
        }
    };

    let (
        squash_dict_arg_range_check_ptr,
        squash_dict_arg_dict_accesses_start,
        squash_dict_arg_dict_accesses_end,
        squash_dict_arg_squashed_dict_start,
    ) = {
        casm_build_extend! {casm_builder,
            // Returns a new squashed_dict with one DictAccess instance per key
            // (value before and value after) which summarizes all the changes to that key.
            //
            // Example:
            //   Input: {(key1, 0, 2), (key1, 2, 7), (key2, 4, 1), (key1, 7, 5), (key2, 1, 2)}
            //   Output: {(key1, 0, 5), (key2, 4, 2)}
            //
            // This is a wrapper of SquashDict.
            DictSquash:

            localvar squashed_dict_start;
            ap += 1;
            hint EnterDictSquashScope {dict_end_ptr: dict_squash_arg_dict_accesses_end} into {};
            let (returned_squashed_dict_start) = call SquashedDictNew;
            assert squashed_dict_start = returned_squashed_dict_start;
            hint ExitScope {} into {};
            // Push SquashDict arguments.
            tempvar squash_dict_arg_range_check_ptr = dict_squash_arg_range_check_ptr;
            tempvar squash_dict_arg_dict_accesses_start = dict_squash_arg_dict_accesses_start;
            tempvar squash_dict_arg_dict_accesses_end = dict_squash_arg_dict_accesses_end;
            tempvar squash_dict_arg_squashed_dict_start = squashed_dict_start;
            let (range_check_ptr, squashed_dict_end) = call SquashDict;
            hint SetDictTrackerEnd {squashed_dict_start: squashed_dict_start, squashed_dict_end: squashed_dict_end} into {};
            // Push the returned variables.
            tempvar returned_range_check_ptr = range_check_ptr;
            tempvar returned_squashed_dict_start = squashed_dict_start;
            tempvar returned_squashed_dict_end = squashed_dict_end;
            ret;
        };
        (
            squash_dict_arg_range_check_ptr,
            squash_dict_arg_dict_accesses_start,
            squash_dict_arg_dict_accesses_end,
            squash_dict_arg_squashed_dict_start,
        )
    };
    casm_build_extend! {casm_builder,
        // Inputs:
        // Outputs: new_dict_end_ptr
        SquashedDictNew:
        // Simply allocates a new segments.
        tempvar new_dict_end_ptr;
        hint AllocSegment {} into {dst: new_dict_end_ptr};
        ap += 1;
        ret;
    }
    let (
        squash_dict_inner_arg_range_check_ptr,
        squash_dict_inner_arg_dict_accesses_start,
        squash_dict_inner_arg_dict_accesses_end_minus1,
        squash_dict_inner_arg_key,
        squash_dict_inner_arg_remaining_accesses,
        squash_dict_inner_arg_squashed_dict_end,
        squash_dict_inner_arg_big_keys,
    ) = {
        casm_build_extend! {casm_builder,
            // Verifies that dict_accesses lists valid chronological accesses (and updates)
            // to a mutable dictionary and outputs a squashed dict with one DictAccess instance per key
            // (value before and value after) which summarizes all the changes to that key.
            SquashDict:
            localvar ptr_diff = squash_dict_arg_dict_accesses_end - squash_dict_arg_dict_accesses_start;
            localvar first_key;
            localvar big_keys;
            jump SquashDictNotEmpty if ptr_diff != 0;
            hint ExitScope {} into {};
            tempvar returned_range_check_ptr = squash_dict_arg_range_check_ptr;
            tempvar returned_squashed_dict_end = squash_dict_arg_squashed_dict_start;
            ret;
            SquashDictNotEmpty:
            tempvar n_accesses = ptr_diff / dict_access_size;
            hint InitSquashData {dict_accesses: squash_dict_arg_dict_accesses_start, ptr_diff: ptr_diff, n_accesses: n_accesses} into {big_keys: big_keys, first_key: first_key};
            let temp_range_check_ptr = squash_dict_arg_range_check_ptr;
            // Order of if branches is reversed w.r.t. the original code.
            jump SquashDictIfBigKeys if big_keys != 0;
            assert first_key = *(temp_range_check_ptr++);
            tempvar squash_dict_inner_arg_range_check_ptr = temp_range_check_ptr;
            rescope {
                squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
                squash_dict_arg_dict_accesses_start = squash_dict_arg_dict_accesses_start,
                squash_dict_arg_dict_accesses_end = squash_dict_arg_dict_accesses_end,
                squash_dict_arg_squashed_dict_start = squash_dict_arg_squashed_dict_start,
                one = one,
                first_key = first_key,
                n_accesses = n_accesses,
                big_keys = big_keys
            };
            jump SquashDictEndIfBigKeys;
            SquashDictIfBigKeys:
            tempvar squash_dict_inner_arg_range_check_ptr = temp_range_check_ptr;
            rescope {
                squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
                squash_dict_arg_dict_accesses_start = squash_dict_arg_dict_accesses_start,
                squash_dict_arg_dict_accesses_end = squash_dict_arg_dict_accesses_end,
                squash_dict_arg_squashed_dict_start = squash_dict_arg_squashed_dict_start,
                one = one,
                first_key = first_key,
                n_accesses = n_accesses,
                big_keys = big_keys
            };
            SquashDictEndIfBigKeys:
            // Push SquashDictInner arguments.
            tempvar squash_dict_inner_arg_dict_accesses_start = squash_dict_arg_dict_accesses_start;
            tempvar squash_dict_inner_arg_dict_accesses_end_minus1 = squash_dict_arg_dict_accesses_end - one;
            tempvar squash_dict_inner_arg_key = first_key;
            tempvar squash_dict_inner_arg_remaining_accesses = n_accesses;
            tempvar squash_dict_inner_arg_squashed_dict_end = squash_dict_arg_squashed_dict_start;
            tempvar squash_dict_inner_arg_big_keys = big_keys;
            let (range_check_ptr, squashed_dict_end) = call SquashDictInner;
            tempvar returned_range_check_ptr = range_check_ptr;
            tempvar returned_squashed_dict_end = squashed_dict_end;
            hint ExitScope {} into {};
            ret;
        };
        (
            squash_dict_inner_arg_range_check_ptr,
            squash_dict_inner_arg_dict_accesses_start,
            squash_dict_inner_arg_dict_accesses_end_minus1,
            squash_dict_inner_arg_key,
            squash_dict_inner_arg_remaining_accesses,
            squash_dict_inner_arg_squashed_dict_end,
            squash_dict_inner_arg_big_keys,
        )
    };
    let (assert_lt_arg_range_check_ptr, assert_lt_arg_a, assert_lt_arg_b) = {
        casm_build_extend! {casm_builder,
            // Inner tail-recursive function for squash_dict.
            // Loops over a single key accesses and verify a valid order.
            SquashDictInner:
            const dict_access_size = 3;
            const one = 1;
            localvar next_key;
            localvar new_remaining_accesses;
            let dict_diff = squash_dict_inner_arg_squashed_dict_end;
            // Prepare first loop iteration.
            hint GetCurrentAccessIndex {range_check_ptr: squash_dict_inner_arg_range_check_ptr} into {};
            tempvar current_access_index = *squash_dict_inner_arg_range_check_ptr;
            tempvar ptr_delta = current_access_index * dict_access_size;
            tempvar first_value;
            tempvar should_skip_loop;
            tempvar prev_loop_locals_access_ptr = squash_dict_inner_arg_dict_accesses_start + ptr_delta;
            let first_access = prev_loop_locals_access_ptr;
            tempvar prev_loop_locals_value = first_access[2]; // The new_value index is 2.
            tempvar prev_loop_locals_range_check_ptr = squash_dict_inner_arg_range_check_ptr + one;
            assert squash_dict_inner_arg_key = first_access[0]; // The key index is 0.
            assert squash_dict_inner_arg_key = dict_diff[0];
            assert first_value = first_access[1]; // The prev_value index is 1
            assert first_value = dict_diff[1];
            hint ShouldSkipSquashLoop {} into {should_skip_loop: should_skip_loop};
            rescope {
                squash_dict_inner_arg_dict_accesses_start = squash_dict_inner_arg_dict_accesses_start,
                prev_loop_locals_access_ptr = prev_loop_locals_access_ptr,
                prev_loop_locals_value = prev_loop_locals_value,
                prev_loop_locals_range_check_ptr = prev_loop_locals_range_check_ptr,
                should_skip_loop = should_skip_loop,
                one = one,
                dict_access_size = dict_access_size,
                squash_dict_inner_arg_key = squash_dict_inner_arg_key,
                squash_dict_inner_arg_dict_accesses_end_minus1 = squash_dict_inner_arg_dict_accesses_end_minus1,
                squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
                dict_diff = dict_diff,
                squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
                squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys,
                squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
                squash_dict_inner_arg_remaining_accesses = squash_dict_inner_arg_remaining_accesses,
                next_key = next_key,
                new_remaining_accesses = new_remaining_accesses
            };
        }
        // Split just to avoid recursion limit when the macro is parsed.
        casm_build_extend! {casm_builder,
            // Skip loop nondeterministically if necessary.
            // The verifier doesn't care if the loop is skipped or not. The only thing it checks is that
            // the function iterated over remaining_accesses accesses in total
            // with ascending keys and ascending indices for the same key.
            // This guarantees that all the entries were visited exactly once.
            jump SquashDictInnerSkipLoop if should_skip_loop != 0;
            SquashDictInnerLoop:
            tempvar loop_temps_index_delta_minus1;
            tempvar loop_temps_index_delta;
            tempvar loop_temps_ptr_delta;
            tempvar loop_temps_should_continue;
            tempvar loop_locals_access_ptr;
            tempvar loop_locals_value;
            tempvar loop_locals_range_check_ptr;
            hint GetCurrentAccessDelta {} into {index_delta_minus1: loop_temps_index_delta_minus1};
            // Check that the transition from the previous access to the current is valid.
            assert loop_temps_index_delta_minus1 = *prev_loop_locals_range_check_ptr;
            assert loop_temps_index_delta = loop_temps_index_delta_minus1 + one;
            assert loop_temps_ptr_delta = loop_temps_index_delta * dict_access_size;
            assert loop_locals_access_ptr = prev_loop_locals_access_ptr + loop_temps_ptr_delta;
            assert prev_loop_locals_value = loop_locals_access_ptr[1];
            assert loop_locals_value = loop_locals_access_ptr[2];
            assert squash_dict_inner_arg_key = loop_locals_access_ptr[0];
            assert loop_locals_range_check_ptr = prev_loop_locals_range_check_ptr + one;
            // Define references to access the values from the previous iteration,
            // the temporary variables and the values for the current iteration.
            tempvar inner_prev_loop_locals_access_ptr = loop_locals_access_ptr;
            tempvar inner_prev_loop_locals_value = loop_locals_value;
            tempvar inner_prev_loop_locals_range_check_ptr = loop_locals_range_check_ptr;
            hint ShouldContinueSquashLoop {} into {should_continue: loop_temps_should_continue};
            rescope {
                squash_dict_inner_arg_dict_accesses_start = squash_dict_inner_arg_dict_accesses_start,
                prev_loop_locals_access_ptr = inner_prev_loop_locals_access_ptr,
                prev_loop_locals_value = inner_prev_loop_locals_value,
                prev_loop_locals_range_check_ptr = inner_prev_loop_locals_range_check_ptr,
                loop_temps_should_continue = loop_temps_should_continue,
                one = one,
                dict_access_size = dict_access_size,
                squash_dict_inner_arg_key = squash_dict_inner_arg_key,
                squash_dict_inner_arg_dict_accesses_end_minus1 = squash_dict_inner_arg_dict_accesses_end_minus1,
                squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
                dict_diff = dict_diff,
                squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
                squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys,
                squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
                squash_dict_inner_arg_remaining_accesses = squash_dict_inner_arg_remaining_accesses,
                next_key = next_key,
                new_remaining_accesses = new_remaining_accesses
            };
            jump SquashDictInnerLoop if loop_temps_should_continue != 0;
            SquashDictInnerSkipLoop:
            let last_loop_locals_access_ptr = prev_loop_locals_access_ptr;
            let last_loop_locals_value = prev_loop_locals_value;
            let last_loop_locals_range_check_ptr = prev_loop_locals_range_check_ptr;
            hint AssertCurrentAccessIndicesIsEmpty {} into {};
            tempvar dict_slack = squash_dict_inner_arg_dict_accesses_end_minus1 - last_loop_locals_access_ptr;
            assert dict_slack = *last_loop_locals_range_check_ptr;
            tempvar n_used_accesses = last_loop_locals_range_check_ptr - squash_dict_inner_arg_range_check_ptr;
            hint AssertAllAccessesUsed {} into {n_used_accesses: n_used_accesses};
            assert last_loop_locals_value = dict_diff[2];
            let arg_range_check_ptr = last_loop_locals_range_check_ptr + one;
            assert new_remaining_accesses = squash_dict_inner_arg_remaining_accesses - n_used_accesses;
            jump SquashDictInnerContinueRecursion if new_remaining_accesses != 0;
            hint AssertAllKeysUsed {} into {};
            // Return from squash_dict_inner, push values to the stack and return;
            tempvar retuened_range_check_ptr = arg_range_check_ptr;
            tempvar retuened_squashed_dict = squash_dict_inner_arg_squashed_dict_end+dict_access_size;
            ret;
        }
        // Split just to avoid recursion limit when the macro is parsed.
        casm_build_extend! {casm_builder,
            SquashDictInnerContinueRecursion:
            hint GetNextDictKey {} into {next_key: next_key};
            // The if order is reversed w.r.t. the original code since the fallthrough case in the original
            // code is the big_keys != 0 case.
            jump SquashDictInnerIfBigKeys if squash_dict_inner_arg_big_keys != 0;
            tempvar key_plus1 = squash_dict_inner_arg_key + one;
            tempvar key_diff = next_key - key_plus1;
            assert key_diff = *(arg_range_check_ptr++);
            // Writing the needed invalidated variables because of the branch.
            tempvar aligned_range_check_ptr = arg_range_check_ptr;
            tempvar aligned_dict_accesses = squash_dict_inner_arg_dict_accesses_start;
            tempvar aligned_dict_accesses_end_minus1 = squash_dict_inner_arg_dict_accesses_end_minus1;
            tempvar aligned_next_key = next_key;
            tempvar aligned_remaining_accesses = new_remaining_accesses;
            rescope {
                aligned_dict_accesses = aligned_dict_accesses,
                aligned_dict_accesses_end_minus1 = aligned_dict_accesses_end_minus1,
                aligned_next_key = aligned_next_key,
                aligned_remaining_accesses = aligned_remaining_accesses,
                aligned_range_check_ptr = aligned_range_check_ptr,
                squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
                squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys,
                dict_access_size = dict_access_size
            };
            jump SquashDictInnerEndIfBigKeys;
            SquashDictInnerIfBigKeys:
            // Align the branches
            ap += 2;
            tempvar assert_lt_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr;
            tempvar assert_lt_arg_a = squash_dict_inner_arg_key;
            tempvar assert_lt_arg_b = next_key;
            let (squash_dict_inner_arg_range_check_ptr) = call AssertLtFelt;
            // Writing the needed invalidated variables because of the branch.
            tempvar aligned_range_check_ptr = squash_dict_inner_arg_range_check_ptr;
            tempvar aligned_dict_accesses = squash_dict_inner_arg_dict_accesses_start;
            tempvar aligned_dict_accesses_end_minus1 = squash_dict_inner_arg_dict_accesses_end_minus1;
            tempvar aligned_next_key = next_key;
            tempvar aligned_remaining_accesses = new_remaining_accesses;
            rescope {
                aligned_dict_accesses = aligned_dict_accesses,
                aligned_dict_accesses_end_minus1 = aligned_dict_accesses_end_minus1,
                aligned_next_key = aligned_next_key,
                aligned_remaining_accesses = aligned_remaining_accesses,
                aligned_range_check_ptr = aligned_range_check_ptr,
                squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
                squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys,
                dict_access_size = dict_access_size
            };
            SquashDictInnerEndIfBigKeys:
            tempvar rec_arg_range_check_ptr = aligned_range_check_ptr;
            tempvar rec_arg_dict_accesses = aligned_dict_accesses;
            tempvar rec_arg_dict_accesses_end_minus1 = aligned_dict_accesses_end_minus1;
            tempvar rec_arg_key = aligned_next_key;
            tempvar rec_arg_remaining_accesses = aligned_remaining_accesses;
            tempvar rec_arg_squashed_dict = squash_dict_inner_arg_squashed_dict_end + dict_access_size;
            tempvar rec_arg_big_keys = squash_dict_inner_arg_big_keys;
            let () = call SquashDictInner;
            ret;
        };
        (assert_lt_arg_range_check_ptr, assert_lt_arg_a, assert_lt_arg_b)
    };
    let (assert_le_arg_range_check_ptr, assert_le_arg_a, assert_le_arg_b) = {
        casm_build_extend! {casm_builder,
            AssertLtFelt:
            const one = 1;
            hint AssertLtAssertValidInput {a: assert_lt_arg_a, b: assert_lt_arg_b} into {};
            tempvar a_minus_b = assert_lt_arg_a - assert_lt_arg_b;
            tempvar assert_le_arg_range_check_ptr = assert_lt_arg_range_check_ptr;
            tempvar assert_le_arg_a;
            jump AssertLtFeltNEQ if a_minus_b != 0;
            assert assert_le_arg_a = assert_lt_arg_a + one;
            jump AssertLtFeltEnd;
            AssertLtFeltNEQ:
            assert assert_le_arg_a = assert_lt_arg_a;
            AssertLtFeltEnd:
            tempvar assert_le_arg_b = assert_lt_arg_b;
            let (temp_range_check_ptr) = call AssertLeFelt;
            tempvar returned_range_check_ptr = temp_range_check_ptr;
            ret;
        };
        (assert_le_arg_range_check_ptr, assert_le_arg_a, assert_le_arg_b)
    };
    {
        casm_build_extend! {casm_builder,
            // Asserts that the unsigned integer lift (as a number in the range [0, PRIME)) of a is lower than
            // or equal to that of b.
            // The numbers [0, a, b, PRIME - 1] should be ordered. To prove that, we show that two of the
            // 3 arcs {0 -> a, a -> b, b -> PRIME - 1} are small:
            //   One is less than PRIME / 3 + 2 ** 129.
            //   Another is less than PRIME / 2 + 2 ** 129.
            // Since the sum of the lengths of these two arcs is less than PRIME, there is no wrap-around.
            AssertLeFelt:
            const one = 1;
            const minus_1 = -1;
            // ceil((PRIME / 2) / 2 ** 128).
            const prime_over_2_high = 3544607988759775765608368578435044694_u128;
            // ceil((PRIME / 3) / 2 ** 128).
            const prime_over_3_high = 5316911983139663648412552867652567041_u128;
            // Guess two arc lengths.
            hint AssertLeFindSmallArcs {range_check_ptr: assert_le_arg_range_check_ptr, a: assert_le_arg_a, b: assert_le_arg_b} into {};
            // Calculate the arc lengths.
            tempvar arc_short_low = *(assert_le_arg_range_check_ptr++);
            tempvar arc_short_high_temp = *(assert_le_arg_range_check_ptr++);
            tempvar arc_short_high = arc_short_high_temp * prime_over_3_high;
            tempvar arc_short = arc_short_low+arc_short_high;
            tempvar arc_long_low = *(assert_le_arg_range_check_ptr++);
            tempvar arc_long_high_temp = *(assert_le_arg_range_check_ptr++);
            tempvar arc_long_high = arc_long_high_temp * prime_over_2_high;
            tempvar arc_long = arc_long_low+arc_long_high;
            tempvar arc_sum = arc_short + arc_long;
            tempvar arc_prod = arc_short * arc_long;
            // First, choose which arc to exclude from {0 -> a, a -> b, b -> PRIME - 1}.
            // Then, to compare the set of two arc lengths, compare their sum and product.
            tempvar skip_exclude_a_flag;
            hint AssertLeIsFirstArcExcluded {} into {skip_exclude_a_flag: skip_exclude_a_flag};
            jump AssertLeFeltSkipExcludeA if skip_exclude_a_flag != 0;
            // Exclude "0 -> a".
            tempvar minus_arg_a = assert_le_arg_a*minus_1;
            assert arc_sum = minus_arg_a + minus_1;
            tempvar a_minus_b = assert_le_arg_a - assert_le_arg_b;
            tempvar b_plus_1 = assert_le_arg_b + one;
            assert arc_prod = a_minus_b * b_plus_1;
            tempvar returned_range_check_ptr = assert_le_arg_range_check_ptr;
            ret;
            AssertLeFeltSkipExcludeA:
            tempvar skip_exclude_b_minus_a;
            hint AssertLeIsSecondArcExcluded {} into {skip_exclude_b_minus_a: skip_exclude_b_minus_a};
            jump AssertLeFeltSkipExcludeBMinusA if skip_exclude_b_minus_a != 0;
            // Exclude "a -> b".
            tempvar minus_arg_b = assert_le_arg_b*minus_1;
            tempvar minus_b_minus_1 = assert_le_arg_b + minus_1;
            assert arc_sum = assert_le_arg_a + minus_b_minus_1;
            assert arc_prod = assert_le_arg_a * minus_b_minus_1;
            tempvar returned_range_check_ptr = assert_le_arg_range_check_ptr;
            ret;
            AssertLeFeltSkipExcludeBMinusA:
            hint AssertLeAssertThirdArcExcluded {} into {};
            // Exclude "b -> PRIME - 1".
            assert arc_sum = assert_le_arg_b;
            tempvar b_minus_a = assert_le_arg_b - assert_le_arg_a;
            assert arc_prod = assert_le_arg_a * b_minus_a;
            tempvar returned_range_check_ptr = assert_le_arg_range_check_ptr;
            ret;
        };
    };
    casm_build_extend! {casm_builder,
        DONE:
    }

    let CasmBuildResult { instructions, branches: [(state, _)] } =
        casm_builder.build(["Fallthrough"]);

    Ok(builder.build(
        instructions,
        vec![],
        [[
            ReferenceExpression { cells: vec![state.get_adjusted(final_range_check_ptr)] },
            ReferenceExpression { cells: vec![state.get_adjusted(final_gas_builtin)] },
            ReferenceExpression { cells: vec![state.get_adjusted(final_dict_manager_ptr)] },
            ReferenceExpression {
                cells: vec![
                    state.get_adjusted(final_squashed_dict_start),
                    state.get_adjusted(final_squashed_dict_end),
                ],
            },
        ]
        .into_iter()]
        .into_iter(),
    ))
}
