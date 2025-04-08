use std::vec;

use cairo_lang_casm::builder::{CasmBuildResult, CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::felt252_dict::{
    Felt252DictConcreteLibfunc, Felt252DictEntryConcreteLibfunc,
};
use cairo_lang_sierra_gas::core_libfunc_cost::{
    DICT_SQUASH_FIXED_COST, DICT_SQUASH_REPEATED_ACCESS_COST, DICT_SQUASH_UNIQUE_KEY_COST,
    SEGMENT_ARENA_ALLOCATION_COST,
};
use cairo_lang_sierra_gas::objects::ConstCost;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{CostValidationInfo, add_input_variables};
use crate::references::ReferenceExpression;

const DICT_ACCESS_SIZE: i32 = 3;

/// Builds instructions for Sierra single cell dict operations.
pub fn build_dict(
    libfunc: &Felt252DictConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Felt252DictConcreteLibfunc::New(_) => build_felt252_dict_new(builder),
        Felt252DictConcreteLibfunc::Squash(_) => build_felt252_dict_squash(builder),
    }
}

/// Handles instruction for creating a new single cell dict.
fn build_felt252_dict_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [segment_arena_ptr] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder, buffer(2) segment_arena_ptr; };
    casm_build_extend! {casm_builder,
        hint AllocFelt252Dict {segment_arena_ptr: segment_arena_ptr};
        // Previous SegmentArenaBuiltin.
        tempvar infos_start = segment_arena_ptr[-3];
        tempvar n_segments = segment_arena_ptr[-2];
        tempvar n_finalized = segment_arena_ptr[-1];
        // New SegmentArenaBuiltin.
        assert infos_start = *(segment_arena_ptr++);
        const imm_1 = 1;
        tempvar new_n_segments = n_segments + imm_1;
        assert new_n_segments = *(segment_arena_ptr++);
        assert n_finalized = *(segment_arena_ptr++);
        const imm_3 = 3;
        tempvar offset = n_segments * imm_3;
        tempvar new_dict_end_ptr = infos_start + offset;
        let new_dict_end = *new_dict_end_ptr;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[segment_arena_ptr], &[new_dict_end]], None)],
        CostValidationInfo {
            builtin_infos: vec![],
            // The segment arena finalization cost.
            extra_costs: Some([SEGMENT_ARENA_ALLOCATION_COST.cost()]),
        },
    ))
}

/// Handles the dict_squash instruction.
fn build_felt252_dict_squash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check_ptr, gas_builtin, segment_arena_ptr, dict_end_address] =
        builder.try_get_single_cells()?;
    // Counters for the amount of steps in the generated code.
    let mut fixed_steps: i32 = 0;
    let mut unique_key_steps: i32 = 0;
    let mut repeated_access_steps: i32 = 0;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) segment_arena_ptr;
        buffer(0) range_check_ptr;
        deref gas_builtin;
        buffer(0) dict_end_address;
    };
    let (
        dict_access_size,
        one,
        dict_squash_arg_range_check_ptr,
        dict_squash_arg_dict_accesses_start,
        dict_squash_arg_dict_accesses_end,
        final_range_check_ptr,
        final_gas_builtin,
        final_segment_arena_ptr,
        final_squashed_dict_start,
        final_squashed_dict_end,
    ) = {
        casm_build_extend! {casm_builder,
            #{ validate steps == 0; }
            const dict_access_size = DICT_ACCESS_SIZE;
            const dict_info_size = 3;
            const one = 1;
            const gas_refund_per_access =
                DICT_SQUASH_UNIQUE_KEY_COST.cost() - DICT_SQUASH_REPEATED_ACCESS_COST.cost();
            // DestructDict is a wrapper that provides a clean scope for dict_squash where
            // local variables can be allocated.
            // Push DestructDict arguments.
            tempvar dict_destruct_arg_range_check_ptr = range_check_ptr;
            tempvar dict_destruct_arg_gas_builtin = gas_builtin;
            tempvar dict_destruct_arg_segment_arena_ptr = segment_arena_ptr;
            tempvar dict_destruct_arg_dict_end_address = dict_end_address;
            let (final_range_check_ptr,
                final_gas_builtin,
                final_segment_arena_ptr,
                final_squashed_dict_start,
                final_squashed_dict_end) = call DestructDict;
            jump DONE;
        }

        casm_build_extend! {casm_builder,
            DestructDict:
            // Allocates function local variables for data needed after the function calls.
            localvar dict_index;
            localvar dict_accesses_len;
            // Guess the index of the dictionary.
            hint GetSegmentArenaIndex {
                dict_end_ptr: dict_destruct_arg_dict_end_address
            } into { dict_index };
            localvar infos = dict_destruct_arg_segment_arena_ptr[-3];
            localvar n_dicts = dict_destruct_arg_segment_arena_ptr[-2];
            localvar n_destructed = dict_destruct_arg_segment_arena_ptr[-1];
            // Verify that dict_index < n_dicts.
            // Range check use
            assert dict_index = *(dict_destruct_arg_range_check_ptr++);
            tempvar n_dicts_minus_1 = n_dicts - one;
            tempvar n_dicts_minus_1_minus_index = n_dicts_minus_1 - dict_index;
            // Range check use
            assert n_dicts_minus_1_minus_index = *(dict_destruct_arg_range_check_ptr++);
            // Write the missing data in the dict_info (destruction index and the end of the
            // dict_segment).
            tempvar info_offset = dict_index * dict_info_size;
            tempvar info_ptr = infos + info_offset;
            assert n_destructed = info_ptr[2];
            assert dict_destruct_arg_dict_end_address = info_ptr[1];
        }
        // Split just to avoid recursion limit when the macro is parsed.
        casm_build_extend! {casm_builder,
            // Write a new dict_manager data to the dict_manager segment (same except for the
            // n_destructed which is incremented).
            assert infos = *(dict_destruct_arg_segment_arena_ptr++);
            assert n_dicts = *(dict_destruct_arg_segment_arena_ptr++);
            tempvar n_destructed_plus_1 = n_destructed + one;
            assert n_destructed_plus_1 = *(dict_destruct_arg_segment_arena_ptr++);
            // Push SquashDict arguments.
            tempvar dict_squash_arg_range_check_ptr = dict_destruct_arg_range_check_ptr;
            tempvar dict_squash_arg_dict_accesses_start = info_ptr[0];
            tempvar dict_squash_arg_dict_accesses_end = dict_destruct_arg_dict_end_address;
            // Compute the length of the accesses segment, and store it in a local variable.
            assert dict_accesses_len = dict_destruct_arg_dict_end_address -
                dict_squash_arg_dict_accesses_start;
            let (range_check_ptr, squashed_dict_end, squashed_dict_start) = call SquashDict;
            // Find the number of keys.
            tempvar squashed_dict_len = squashed_dict_end - squashed_dict_start;
            // The number of refunded accesses is number_of_accesses - number_of_keys, which equals
            // to dict_accesses_len / dict_access_size - squashed_dict_len / dict_access_size.
            // Use distributivity to conserve one operation.
            tempvar accesses_len_minus_squashed_len = dict_accesses_len - squashed_dict_len;
            tempvar n_refunded_accesses = accesses_len_minus_squashed_len / dict_access_size;
            tempvar gas_to_refund = n_refunded_accesses * gas_refund_per_access;
            // Push the returned variables.
            tempvar returned_range_check_ptr = range_check_ptr;
            tempvar returned_gas_builtin = dict_destruct_arg_gas_builtin + gas_to_refund;
            tempvar returned_segment_arena_ptr = dict_destruct_arg_segment_arena_ptr;
            tempvar returned_squashed_dict_start = squashed_dict_start;
            tempvar returned_squashed_dict_end = squashed_dict_end;
            ret;
            #{ fixed_steps += steps; steps = 0; }
        };
        (
            dict_access_size,
            one,
            dict_squash_arg_range_check_ptr,
            dict_squash_arg_dict_accesses_start,
            dict_squash_arg_dict_accesses_end,
            final_range_check_ptr,
            final_gas_builtin,
            final_segment_arena_ptr,
            final_squashed_dict_start,
            final_squashed_dict_end,
        )
    };

    let (squash_dict_inner_args, fixed_steps_) = build_squash_dict(
        &mut casm_builder,
        dict_access_size,
        one,
        SquashDictArgs {
            squash_dict_arg_range_check_ptr: dict_squash_arg_range_check_ptr,
            squash_dict_arg_dict_accesses_start: dict_squash_arg_dict_accesses_start,
            squash_dict_arg_dict_accesses_end: dict_squash_arg_dict_accesses_end,
        },
    );
    fixed_steps += fixed_steps_;

    let (fixed_steps_, unique_key_steps_, repeated_access_steps_) =
        build_squash_dict_inner(&mut casm_builder, squash_dict_inner_args);
    fixed_steps += fixed_steps_;
    unique_key_steps += unique_key_steps_;
    repeated_access_steps += repeated_access_steps_;

    casm_build_extend! {casm_builder,
        DONE:
        #{ fixed_steps += steps; steps = 0; }
    }
    // Manually counted, range check uses are marked in the builder code.
    let fixed_range_checks = 3;
    let unique_key_range_checks = 6;
    let repeated_access_range_checks = 1;
    assert_eq!(
        ConstCost {
            steps: fixed_steps,
            holes: 0,
            range_checks: fixed_range_checks,
            range_checks96: 0
        },
        DICT_SQUASH_FIXED_COST
    );
    assert_eq!(
        ConstCost {
            steps: repeated_access_steps,
            holes: 0,
            range_checks: repeated_access_range_checks,
            range_checks96: 0
        },
        DICT_SQUASH_REPEATED_ACCESS_COST
    );
    assert_eq!(
        ConstCost {
            steps: unique_key_steps,
            holes: 0,
            range_checks: unique_key_range_checks,
            range_checks96: 0
        },
        DICT_SQUASH_UNIQUE_KEY_COST
    );
    let CasmBuildResult { instructions, branches: [(state, _)] } =
        casm_builder.build(["Fallthrough"]);

    Ok(builder.build(
        instructions,
        vec![],
        [[
            ReferenceExpression { cells: vec![state.get_adjusted(final_range_check_ptr)] },
            ReferenceExpression { cells: vec![state.get_adjusted(final_gas_builtin)] },
            ReferenceExpression { cells: vec![state.get_adjusted(final_segment_arena_ptr)] },
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

struct SquashDictArgs {
    pub squash_dict_arg_range_check_ptr: Var,
    pub squash_dict_arg_dict_accesses_start: Var,
    pub squash_dict_arg_dict_accesses_end: Var,
}

struct SquashDictInnerArgs {
    pub squash_dict_inner_arg_range_check_ptr: Var,
    pub squash_dict_inner_arg_dict_accesses_start: Var,
    pub squash_dict_inner_arg_dict_accesses_end_minus1: Var,
    pub squash_dict_inner_arg_key: Var,
    pub squash_dict_inner_arg_remaining_accesses: Var,
    pub squash_dict_inner_arg_squashed_dict_end: Var,
    pub squash_dict_inner_arg_big_keys: Var,
}

fn build_squash_dict(
    casm_builder: &mut CasmBuilder,
    dict_access_size: Var,
    one: Var,
    args: SquashDictArgs,
) -> (SquashDictInnerArgs, i32) {
    let mut fixed_steps = 0;
    let SquashDictArgs {
        squash_dict_arg_range_check_ptr,
        squash_dict_arg_dict_accesses_start,
        squash_dict_arg_dict_accesses_end,
    } = args;

    casm_build_extend! {casm_builder,
        // Verifies that dict_accesses lists valid chronological accesses (and updates) to a
        // mutable dictionary and outputs a squashed dict with one DictAccess instance per key
        // (value before and value after) which summarizes all the changes to that key.
        SquashDict:
        #{ validate steps == 0; }
        localvar ptr_diff =
            squash_dict_arg_dict_accesses_end - squash_dict_arg_dict_accesses_start;
        localvar first_key;
        localvar big_keys;
        localvar squashed_dict_start;
        ap += 3;
        hint AllocSegment into {dst: squashed_dict_start};
        jump SquashDictNotEmpty if ptr_diff != 0;
        tempvar returned_range_check_ptr = squash_dict_arg_range_check_ptr;
        tempvar returned_squashed_dict_start = squashed_dict_start;
        tempvar returned_squashed_dict_end = squashed_dict_start;
        ret;
        // SquashDict on empty dict is cheaper than not empty dict. Steps disregarded.
        #{ steps = 0; }

        SquashDictNotEmpty:
        tempvar n_accesses = ptr_diff / dict_access_size;
        hint InitSquashData {
            dict_accesses: squash_dict_arg_dict_accesses_start,
            ptr_diff: ptr_diff, n_accesses: n_accesses
        } into { big_keys, first_key };
        let temp_range_check_ptr = squash_dict_arg_range_check_ptr;
        tempvar squash_dict_inner_arg_range_check_ptr;
        // Order of if branches is reversed w.r.t. the original code.
        jump SquashDictIfBigKeys if big_keys != 0;
        assert first_key = *(temp_range_check_ptr++); // Range check use
        assert squash_dict_inner_arg_range_check_ptr = temp_range_check_ptr;
        rescope {
            squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
            squash_dict_arg_dict_accesses_start = squash_dict_arg_dict_accesses_start,
            squash_dict_arg_dict_accesses_end = squash_dict_arg_dict_accesses_end,
            squashed_dict_start = squashed_dict_start,
            one = one,
            first_key = first_key,
            n_accesses = n_accesses,
            big_keys = big_keys
        };
        jump SquashDictEndIfBigKeys;
        SquashDictIfBigKeys:
        assert squash_dict_inner_arg_range_check_ptr = temp_range_check_ptr;
        rescope {
            squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
            squash_dict_arg_dict_accesses_start = squash_dict_arg_dict_accesses_start,
            squash_dict_arg_dict_accesses_end = squash_dict_arg_dict_accesses_end,
            squashed_dict_start = squashed_dict_start,
            one = one,
            first_key = first_key,
            n_accesses = n_accesses,
            big_keys = big_keys
        };
        SquashDictEndIfBigKeys:
        // Push SquashDictInner arguments.
        tempvar squash_dict_inner_arg_dict_accesses_start = squash_dict_arg_dict_accesses_start;
        tempvar squash_dict_inner_arg_dict_accesses_end_minus1 =
            squash_dict_arg_dict_accesses_end - one;
        tempvar squash_dict_inner_arg_key = first_key;
        tempvar squash_dict_inner_arg_remaining_accesses = n_accesses;
        tempvar squash_dict_inner_arg_squashed_dict_end = squashed_dict_start;
        tempvar squash_dict_inner_arg_big_keys = big_keys;
        let (_range_check_ptr, _squashed_dict_end) = call SquashDictInner;
        // For efficiency, return `squashed_dict_start` after `range_check_ptr` and
        // `squashed_dict_end` (which are always at the top of the stack).
        tempvar returned_squashed_dict_start = squashed_dict_start;
        ret;
        #{ fixed_steps += steps; steps = 0; }
    };
    (
        SquashDictInnerArgs {
            squash_dict_inner_arg_range_check_ptr,
            squash_dict_inner_arg_dict_accesses_start,
            squash_dict_inner_arg_dict_accesses_end_minus1,
            squash_dict_inner_arg_key,
            squash_dict_inner_arg_remaining_accesses,
            squash_dict_inner_arg_squashed_dict_end,
            squash_dict_inner_arg_big_keys,
        },
        fixed_steps,
    )
}

/// Generates CASM code for the `SquashDictInner` function.
fn build_squash_dict_inner(
    casm_builder: &mut CasmBuilder,
    args: SquashDictInnerArgs,
) -> (i32, i32, i32) {
    let mut fixed_steps = 0;
    let mut unique_key_steps = 0;
    let mut repeated_access_steps = 0;

    let SquashDictInnerArgs {
        squash_dict_inner_arg_range_check_ptr,
        squash_dict_inner_arg_dict_accesses_start,
        squash_dict_inner_arg_dict_accesses_end_minus1,
        squash_dict_inner_arg_key,
        squash_dict_inner_arg_remaining_accesses,
        squash_dict_inner_arg_squashed_dict_end,
        squash_dict_inner_arg_big_keys,
    } = args;

    casm_build_extend! {casm_builder,
        // Inner tail-recursive function for squash_dict.
        // Loops over a single key accesses and verify a valid order.
        SquashDictInner:
        #{ validate steps == 0; }
        const dict_access_size = DICT_ACCESS_SIZE;
        const zero = 0;
        const one = 1;
        localvar next_key;
        localvar new_remaining_accesses;
        let dict_diff = squash_dict_inner_arg_squashed_dict_end;
        // Prepare first loop iteration.
        hint GetCurrentAccessIndex {
            range_check_ptr: squash_dict_inner_arg_range_check_ptr
        };
        // Range check use, once per unique key
        tempvar current_access_index = *squash_dict_inner_arg_range_check_ptr;
        tempvar ptr_delta = current_access_index * dict_access_size;
        tempvar first_value;
        tempvar should_skip_loop;
        tempvar prev_loop_locals_access_ptr =
            squash_dict_inner_arg_dict_accesses_start + ptr_delta;
        let first_access = prev_loop_locals_access_ptr;
        tempvar prev_loop_locals_value = first_access[2]; // The new_value index is 2.
        tempvar prev_loop_locals_range_check_ptr = squash_dict_inner_arg_range_check_ptr + one;
        assert squash_dict_inner_arg_key = first_access[0]; // The key index is 0.
        assert squash_dict_inner_arg_key = dict_diff[0];
        assert first_value = first_access[1]; // The prev_value index is 1
        assert first_value = dict_diff[1];
        assert first_value = zero;
        hint ShouldSkipSquashLoop into { should_skip_loop };
        rescope {
            squash_dict_inner_arg_dict_accesses_start =
                squash_dict_inner_arg_dict_accesses_start,
            prev_loop_locals_access_ptr = prev_loop_locals_access_ptr,
            prev_loop_locals_value = prev_loop_locals_value,
            prev_loop_locals_range_check_ptr = prev_loop_locals_range_check_ptr,
            should_skip_loop = should_skip_loop,
            one = one,
            dict_access_size = dict_access_size,
            squash_dict_inner_arg_key = squash_dict_inner_arg_key,
            squash_dict_inner_arg_dict_accesses_end_minus1 =
                squash_dict_inner_arg_dict_accesses_end_minus1,
            squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
            dict_diff = dict_diff,
            squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys,
            squash_dict_inner_arg_remaining_accesses =
                squash_dict_inner_arg_remaining_accesses,
            squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
            next_key = next_key,
            new_remaining_accesses = new_remaining_accesses
        };
        #{ unique_key_steps += steps; steps = 0; }
        // Skip loop nondeterministically if necessary.
        // The verifier doesn't care if the loop is skipped or not. The only thing it checks
        // is that the function iterated over remaining_accesses accesses in total
        // with ascending keys and ascending indices for the same key.
        // This guarantees that all the entries were visited exactly once.
        jump SquashDictInnerSkipLoop if should_skip_loop != 0;
    }
    repeated_access_steps += build_squash_dict_inner_loop(
        casm_builder,
        args,
        SquashDictInnerLoopArgs {
            prev_loop_locals_range_check_ptr,
            prev_loop_locals_access_ptr,
            prev_loop_locals_value,
            dict_diff,
            next_key,
            new_remaining_accesses,
        },
    );
    casm_build_extend! {casm_builder,
        SquashDictInnerSkipLoop:
        let last_loop_locals_access_ptr = prev_loop_locals_access_ptr;
        let last_loop_locals_value = prev_loop_locals_value;
        let last_loop_locals_range_check_ptr = prev_loop_locals_range_check_ptr;
        tempvar dict_slack =
            squash_dict_inner_arg_dict_accesses_end_minus1 - last_loop_locals_access_ptr;
        // Range check use, once per unique key.
        assert dict_slack = *last_loop_locals_range_check_ptr;
        tempvar n_used_accesses =
            last_loop_locals_range_check_ptr - squash_dict_inner_arg_range_check_ptr;
        assert last_loop_locals_value = dict_diff[2];
        const one = 1;
        let arg_range_check_ptr = last_loop_locals_range_check_ptr + one;
        assert new_remaining_accesses =
            squash_dict_inner_arg_remaining_accesses - n_used_accesses;
        #{ unique_key_steps += steps; steps = 0; }
        jump SquashDictInnerContinueRecursion if new_remaining_accesses != 0;
        // Return from squash_dict_inner, push values to the stack and return;
        tempvar returned_range_check_ptr = arg_range_check_ptr;
        const dict_access_size = DICT_ACCESS_SIZE;
        tempvar returned_squashed_dict =
            squash_dict_inner_arg_squashed_dict_end + dict_access_size;
        ret;
        #{ fixed_steps += steps; steps = 0; }
    }
    // Split just to avoid recursion limit when the macro is parsed.
    casm_build_extend! {casm_builder,
        SquashDictInnerContinueRecursion:
        hint GetNextDictKey into { next_key };
        // The if order is reversed w.r.t. the original code since the fallthrough case in the
        // original code is the big_keys != 0 case.
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
            squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys
        };
        jump SquashDictInnerEndIfBigKeys;
        SquashDictInnerIfBigKeys:
    }
    validate_felt252_lt(casm_builder, arg_range_check_ptr, squash_dict_inner_arg_key, next_key);
    casm_build_extend! {casm_builder,
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
            squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys
        };
        SquashDictInnerEndIfBigKeys:
        const dict_access_size = DICT_ACCESS_SIZE;
        tempvar rec_arg_squashed_dict =
            squash_dict_inner_arg_squashed_dict_end + dict_access_size;
        tempvar rec_arg_big_keys = squash_dict_inner_arg_big_keys;
        let () = call SquashDictInner;
        ret;
        #{ unique_key_steps += steps; steps = 0; }
    };
    (fixed_steps, unique_key_steps, repeated_access_steps)
}

struct SquashDictInnerLoopArgs {
    prev_loop_locals_range_check_ptr: Var,
    prev_loop_locals_access_ptr: Var,
    prev_loop_locals_value: Var,
    dict_diff: Var,
    next_key: Var,
    new_remaining_accesses: Var,
}

fn build_squash_dict_inner_loop(
    casm_builder: &mut CasmBuilder,
    args: SquashDictInnerArgs,
    loop_args: SquashDictInnerLoopArgs,
) -> i32 {
    let mut repeated_access_steps = 0;
    let SquashDictInnerArgs {
        squash_dict_inner_arg_range_check_ptr,
        squash_dict_inner_arg_dict_accesses_start,
        squash_dict_inner_arg_dict_accesses_end_minus1,
        squash_dict_inner_arg_key,
        squash_dict_inner_arg_remaining_accesses,
        squash_dict_inner_arg_squashed_dict_end,
        squash_dict_inner_arg_big_keys,
    } = args;
    let SquashDictInnerLoopArgs {
        prev_loop_locals_range_check_ptr,
        prev_loop_locals_access_ptr,
        prev_loop_locals_value,
        dict_diff,
        next_key,
        new_remaining_accesses,
    } = loop_args;

    casm_build_extend! {casm_builder,
        const dict_access_size = DICT_ACCESS_SIZE;
        const one = 1;
        SquashDictInnerLoop:
        tempvar loop_temps_index_delta_minus1;
        tempvar loop_temps_index_delta;
        tempvar loop_temps_ptr_delta;
        tempvar loop_temps_should_continue;
        tempvar loop_locals_access_ptr;
        tempvar loop_locals_value;
        tempvar loop_locals_range_check_ptr;
        hint GetCurrentAccessDelta into {index_delta_minus1: loop_temps_index_delta_minus1};
        // Check that the transition from the previous access to the current is valid.
        // Range check use, once per access
        assert loop_temps_index_delta_minus1 = *prev_loop_locals_range_check_ptr;
        assert loop_temps_index_delta = loop_temps_index_delta_minus1 + one;
        assert loop_temps_ptr_delta = loop_temps_index_delta * dict_access_size;
        assert loop_locals_access_ptr = prev_loop_locals_access_ptr + loop_temps_ptr_delta;
        assert prev_loop_locals_value = loop_locals_access_ptr[1];
        assert loop_locals_value = loop_locals_access_ptr[2];
        assert squash_dict_inner_arg_key = loop_locals_access_ptr[0];
        assert loop_locals_range_check_ptr = prev_loop_locals_range_check_ptr + one;
        hint ShouldContinueSquashLoop into {should_continue: loop_temps_should_continue};
        rescope {
            squash_dict_inner_arg_dict_accesses_start =
                squash_dict_inner_arg_dict_accesses_start,
            prev_loop_locals_access_ptr = loop_locals_access_ptr,
            prev_loop_locals_value = loop_locals_value,
            prev_loop_locals_range_check_ptr = loop_locals_range_check_ptr,
            loop_temps_should_continue = loop_temps_should_continue,
            squash_dict_inner_arg_key = squash_dict_inner_arg_key,
            squash_dict_inner_arg_dict_accesses_end_minus1 =
                squash_dict_inner_arg_dict_accesses_end_minus1,
            squash_dict_inner_arg_range_check_ptr = squash_dict_inner_arg_range_check_ptr,
            dict_diff = dict_diff,
            squash_dict_inner_arg_squashed_dict_end = squash_dict_inner_arg_squashed_dict_end,
            squash_dict_inner_arg_big_keys = squash_dict_inner_arg_big_keys,
            squash_dict_inner_arg_remaining_accesses =
                squash_dict_inner_arg_remaining_accesses,
            next_key = next_key,
            new_remaining_accesses = new_remaining_accesses
        };
        #{ repeated_access_steps += steps; steps = 0; }
        jump SquashDictInnerLoop if loop_temps_should_continue != 0;
    }

    repeated_access_steps
}

/// Asserts that the unsigned integer lift (as a number in the range [0, PRIME)) of a is lower than
/// to that of b.
fn validate_felt252_lt(casm_builder: &mut CasmBuilder, range_check: Var, a: Var, b: Var) {
    casm_build_extend! {casm_builder,
        // Verify that a != b. Fail otherwise.
        tempvar a_minus_b = a - b;
        jump AssertLtFelt252Continue if a_minus_b != 0;
        fail;
        AssertLtFelt252Continue:
    }
    validate_felt252_le(casm_builder, range_check, a, b);
}

/// Asserts that the unsigned integer lift (as a number in the range [0, PRIME)) of a is lower than
/// or equal to that of b.
/// The numbers [0, a, b, PRIME - 1] should be ordered. To prove that, we show that two of the
/// 3 arcs {0 -> a, a -> b, b -> PRIME - 1} are small:
///   One is less than PRIME / 3 + 2 ** 129.
///   Another is less than PRIME / 2 + 2 ** 129.
/// Since the sum of the lengths of these two arcs is less than PRIME, there is no wrap-around.
fn validate_felt252_le(casm_builder: &mut CasmBuilder, range_check: Var, a: Var, b: Var) {
    casm_build_extend! {casm_builder,
        const one = 1;
        const minus_1 = -1;
        // ceil((PRIME / 3) / 2 ** 128).
        const prime_over_3_high = 3544607988759775765608368578435044694_u128;
        // ceil((PRIME / 2) / 2 ** 128).
        const prime_over_2_high = 5316911983139663648412552867652567041_u128;
        // Guess two arc lengths.
        hint AssertLeFindSmallArcs { range_check_ptr: range_check, a, b };
        // Calculate the arc lengths.
        // Range check use, 4 times, once per unique key
        tempvar arc_short_low = *(range_check++);
        tempvar arc_short_high_temp = *(range_check++);
        tempvar arc_short_high = arc_short_high_temp * prime_over_3_high;
        tempvar arc_short = arc_short_low+arc_short_high;
        tempvar arc_long_low = *(range_check++);
        tempvar arc_long_high_temp = *(range_check++);
        tempvar arc_long_high = arc_long_high_temp * prime_over_2_high;
        tempvar arc_long = arc_long_low+arc_long_high;
        tempvar arc_sum = arc_short + arc_long;
        tempvar arc_prod = arc_short * arc_long;
        // First, choose which arc to exclude from {0 -> a, a -> b, b -> PRIME - 1}.
        // Then, to compare the set of two arc lengths, compare their sum and product.
        tempvar skip_exclude_a_flag;
        hint AssertLeIsFirstArcExcluded into { skip_exclude_a_flag };
        jump AssertLeFelt252SkipExcludeA if skip_exclude_a_flag != 0;
        // Exclude "0 -> a".
        // The two arcs are (b - a) and (PRIME - 1 - b = -b - 1).
        // Thus the sum is (-a - 1) and the product is (a - b) * (b + 1).
        tempvar minus_a = a * minus_1;
        assert arc_sum = minus_a + minus_1;
        tempvar a_minus_b = a - b;
        tempvar b_plus_1 = b + one;
        assert arc_prod = a_minus_b * b_plus_1;
        jump EndOfFelt252Le;
        AssertLeFelt252SkipExcludeA:
        tempvar skip_exclude_b_minus_a;
        hint AssertLeIsSecondArcExcluded into { skip_exclude_b_minus_a };
        jump AssertLeFelt252SkipExcludeBMinusA if skip_exclude_b_minus_a != 0;
        // Exclude "a -> b".
        // The two arcs are (a - 0 = a) and (PRIME - 1 - b = -b - 1).
        // Thus the sum is a + (-b - 1) and the product is a * (-b - 1).
        tempvar minus_b = b * minus_1;
        tempvar minus_b_minus_1 = minus_b + minus_1;
        assert arc_sum = a + minus_b_minus_1;
        assert arc_prod = a * minus_b_minus_1;
        jump EndOfFelt252Le;
        AssertLeFelt252SkipExcludeBMinusA:
        tempvar _padding;
        // Exclude "b -> PRIME - 1".
        // The two arcs are (a - 0 = a) and (b - a).
        // Thus the sum is b and the product is a * (b - a).
        assert arc_sum = b;
        tempvar b_minus_a = b - a;
        assert arc_prod = a * b_minus_a;
        EndOfFelt252Le:
    };
}

/// Builds instructions for Sierra dict entry operations.
pub fn build_entry(
    libfunc: &Felt252DictEntryConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Felt252DictEntryConcreteLibfunc::Get(_) => build_felt252_dict_entry_get(builder),
        Felt252DictEntryConcreteLibfunc::Finalize(_) => build_felt252_dict_entry_finalize(builder),
    }
}

/// Builds instructions for creating a new dict entry.
fn build_felt252_dict_entry_get(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [dict_ptr, key] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder, buffer(2) dict_ptr; deref key; };
    casm_build_extend! {casm_builder,
        hint Felt252DictEntryInit { dict_ptr, key };
        assert key = *(dict_ptr++);
        let prev_value = *(dict_ptr++);
        // The new value will be written in the entry finalization.
        let _new_value = *(dict_ptr++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[dict_ptr], &[prev_value]], None)],
        CostValidationInfo {
            builtin_infos: vec![],
            extra_costs: Some([DICT_SQUASH_UNIQUE_KEY_COST.cost()]),
        },
    ))
}

/// Builds instructions for finalizing an existing dict entry.
fn build_felt252_dict_entry_finalize(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [dict_entry, new_value] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder, buffer(0) dict_entry; deref new_value; };
    casm_build_extend! {casm_builder,
        hint Felt252DictEntryUpdate { dict_ptr: dict_entry, value: new_value };
        assert new_value = dict_entry[-1];
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[dict_entry]], None)],
        Default::default(),
    ))
}
