use indoc::indoc;
use pretty_assertions;
use sierra::ProgramParser;
use test_case::test_case;

use crate::compiler::compile;
use crate::test_utils::{build_metadata, read_sierra_example_file, strip_comments_and_linebreaks};

#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;
                type BoxFelt = Box<felt>;

                libfunc finalize_locals = finalize_locals;
                libfunc felt_add = felt_add;
                libfunc felt_mul_2 = felt_mul<2>;
                libfunc felt_sub = felt_sub;
                libfunc felt_dup = dup<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc felt_into_box = into_box<felt>;
                libfunc felt_unbox = unbox<felt>;
                libfunc jump = jump;
                libfunc felt_unwrap_nz = unwrap_nz<felt>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_box_felt = store_temp<BoxFelt>;
                libfunc rename_felt = rename<felt>;
                libfunc call_foo = function_call<user@foo>;

                libfunc call_box_and_back = function_call<user@box_and_back>;

                rename_felt([1]) -> ([1]);                      // #0
                felt_dup([2]) -> ([2], [5]);                    // #1
                felt_add([1], [2]) -> ([3]);                    // #2
                store_temp_felt([3]) -> ([4]);                  // #3
                store_temp_felt([5]) -> ([5]);                  // #4
                store_temp_felt([4]) -> ([4]);                  // #5
                call_foo([5], [4]) -> ([7], [8]);               // #6
                felt_dup([8]) -> ([4], [8]);                    // #7
                store_temp_felt([4]) -> ([4]);                  // #8
                return([7], [8], [4]);                          // #9

                finalize_locals() -> ();                        // #10
                felt_jump_nz([1]) { fallthrough() 16([1]) };    // #11
                felt_dup([2]) -> ([1], [2]);                    // #12
                store_temp_felt([1]) -> ([1]);                  // #13
                store_temp_felt([2]) -> ([2]);                  // #14
                return ([1], [2]);                              // #15

                jump() { 17() };                                // #16
                felt_unwrap_nz([1]) -> ([1]);                   // #17
                felt_dup([2]) -> ([2], [3]);                    // #18
                felt_sub([1], [3]) -> ([1]);                    // #19
                store_temp_felt([1]) -> ([1]);                  // #20
                felt_mul_2([1]) -> ([1]);                       // #21
                store_temp_felt([1]) -> ([1]);                  // #22
                store_temp_felt([2]) -> ([2]);                  // #23
                call_foo([1], [2]) -> ([1], [2]);               // #24
                return ([1], [2]);                              // #25

                felt_into_box([1]) -> ([2]);                    // #26
                store_temp_box_felt([2]) -> ([2]);              // #27
                felt_unbox([2]) -> ([3]);                       // #28
                store_temp_felt([3]) -> ([3]);                  // #29
                return ([3]);                                   // #30

                store_temp_felt([1]) -> ([1]);                  // #31
                call_box_and_back([1]) -> ([1]);                // #32
                return ([1]);                                   // #33

                test_program@0([1]: felt, [2]: felt) -> (felt, felt, felt);
                foo@10([1]: felt, [2]: felt) -> (felt, felt);
                box_and_back@26([1]: felt) -> (felt);
                box_and_back_wrapper@31([1]: felt) -> (felt);
            "},
            false,
            indoc! {"
                // test_program:
                [ap + 0] = [fp + -4] + [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [ap + -2], ap++;
                // call foo
                call rel 4;
                [ap + 0] = [ap + -1], ap++;
                ret;

                // foo:
                ap += 0;
                jmp rel 5 if [fp + -4] != 0;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                ret;
                jmp rel 2;
                [fp + -4] = [ap + 0] + [fp + -3], ap++;
                [ap + 0] = [ap + -1] * 2, ap++;
                [ap + 0] = [fp + -3], ap++;
                call rel -13;
                ret;

                // box_and_back:
                %{ memory[ap + 0] = segments.add() %}
                [fp + -3] = [[ap + 0] + 0], ap++;
                [ap + 0] = [[ap + -1] + 0], ap++;
                ret;

                // box_and_back_wrapper:
                [ap + 0] = [fp + -3], ap++;
                call rel -4;
                ret;
            "};
            "good_flow")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;
                type ArrayFelt = Array<felt>;
                type UninitializedArrayFelt = Uninitialized<ArrayFelt>;

                libfunc finalize_locals = finalize_locals;
                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_local_felt = store_local<felt>;
                libfunc alloc_local_array_felt = alloc_local<ArrayFelt>;
                libfunc store_local_array_felt = store_local<ArrayFelt>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_array_felt = store_temp<ArrayFelt>;

                store_temp_felt([1]) -> ([1]);
                alloc_local_felt() -> ([4]);
                alloc_local_felt() -> ([5]);
                alloc_local_array_felt() -> ([6]);
                store_local_felt([4], [1]) -> ([4]);
                finalize_locals() -> ();
                store_local_felt([5], [2]) -> ([5]);
                store_local_array_felt([6], [3]) -> ([6]);
                store_temp_felt([4]) -> ([4]);
                store_temp_felt([5]) -> ([5]);
                store_temp_array_felt([6]) -> ([6]);
                return ([4], [5], [6]);

                test_program@0([1]: felt, [2]: felt, [3]: ArrayFelt) -> (felt, felt, ArrayFelt);
            "},
            false,
            indoc! {"
                [ap + 0] = [fp + -6], ap++;
                [fp + 1] = [ap + -1];
                ap += 4;
                [fp + 2] = [fp + -5];
                [fp + 3] = [fp + -4];
                [fp + 4] = [fp + -3];
                [ap + 0] = [fp + 1], ap++;
                [ap + 0] = [fp + 2], ap++;
                [ap + 0] = [fp + 3], ap++;
                [ap + 0] = [fp + 4], ap++;
                ret;
            "};
            "alloc_local and store_local")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc store_temp_nz_felt = store_temp<NonZeroFelt>;
                libfunc nz_felt_drop = drop<NonZeroFelt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc branch_align = branch_align;


                felt_jump_nz([1]) { fallthrough() 3([1]) };
                branch_align() -> ();
                return ();
                store_temp_nz_felt([1]) -> ([1]);
                nz_felt_drop([1]) -> ();
                return ();

                test_program@0([1]: felt) -> ();
            "},
            true,
            indoc! {"
                jmp rel 5 if [fp + -3] != 0;
                ap += 1;
                ret;
                [ap + 0] = [fp + -3], ap++;
                ret;
            "};
            "branch align")]
#[test_case(indoc!{"
                type RangeCheck = RangeCheck;
                type u128 = u128;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc u128_lt = u128_lt;
                libfunc store_u128 = store_temp<u128>;
                libfunc store_rc = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                u128_lt([1], [2], [3]) {fallthrough([1]) 2([1]) };
                store_rc([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: u128, [3]: u128) -> (RangeCheck);
            "}, false, indoc!{"
                [fp + -4] = [ap + 1] + [fp + -3], ap++;
                %{ memory[ap + -1] = memory[ap + 0] < 340282366920938463463374607431768211456 %}
                jmp rel 7 if [ap + -1] != 0, ap++;
                // a < b.
                [ap + 0] = [ap + -1] + 340282366920938463463374607431768211456, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 3;
                // a < b.
                [ap + -1] = [[fp + -5] + 0];
                // Store range_check and return.
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "}; "u128_lt")]
#[test_case(indoc! {"
                type u128 = u128;
                type RangeCheck = RangeCheck;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc u128_overflow_add = u128_overflow_add;
                libfunc drop<u128> = drop<u128>;
                libfunc store_temp<RangeCheck> = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                u128_overflow_add([1], [2], [3]) {fallthrough([1], [2]) 5([1], [2]) };
                drop<u128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);
                drop<u128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: u128, [3]: u128) -> (RangeCheck);
            "},
            false,
            indoc! {"
                [ap + 1] = [fp + -4] + [fp + -3], ap++;
                %{ memory[ap + -1] = memory[ap + 0] < 340282366920938463463374607431768211456 %}
                jmp rel 7 if [ap + -1] != 0, ap++;
                [ap + -1] = [ap + 0] + 340282366920938463463374607431768211456, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 6;
                [ap + -1] = [[fp + -5] + 0];
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "};
            "u128")]
#[test_case(read_sierra_example_file("fib_no_gas").as_str(),
            false,
            indoc! {"
                jmp rel 4 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                ret;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = [fp + -5] + [fp + -4], ap++;
                [ap + 0] = [fp + -3] + -1, ap++;
                call rel -8;
                ret;
            "};
            "fib_no_gas")]
#[test_case(read_sierra_example_file("fib_jumps").as_str(),
            true,
            indoc! {"
                jmp rel 8 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + 13, ap++;
                [ap + 0] = 1, ap++;
                ret;

                // Statement #  8 - Calculates n - 1 and tests if n - 1 == 0.
                [fp + -3] = [ap + 0] + 1, ap++;
                jmp rel 8 if [ap + -1] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + 11, ap++;
                [ap + 0] = 1, ap++;
                ret;

                // Statement # 18
                // Setting up the latest memory to be of the form [b=1, _, _, n=n-1, rc, gb, a=1].
                [ap + 0] = 1, ap++;
                [ap + 0] = [ap + -2], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = 1, ap++;
                // Statement # 27 - Getting gas for the main loop.
                %{ memory[ap + 0] = 800 <= memory[ap + -2] %}
                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap + -3] + 340282366920938463463374607431768210656, ap++;
                [ap + -1] = [[ap + -5] + 0];
                jmp rel 18;

                // Statement # 28
                // The main loop - given [b, _, _, n, rc, gb, a, _, _] - adds [n-1, updated_rc, updated_gb, a+b]
                // Memory cells form is now [b'=a, _, _, n'=n-1, rc'=updated_rc, gb'=updated_gb, a'=a+b]
                [ap + -3] = [ap + 0] + 800, ap++;
                [ap + -1] = [[ap + -5] + 0];
                [ap + -6] = [ap + 0] + 1, ap++;
                [ap + 0] = [ap + -6] + 1, ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -6] + [ap + -12], ap++;
                jmp rel -16 if [ap + -4] != 0;
                // Statement # 48 - n == 0, so we can return the latest a.
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3] + 4, ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;
                [ap + 0] = [ap + -5] + 1, ap++;
                [ap + 0] = [ap + -5], ap++;
                [ap + 0] = -1, ap++;
                ret;
            "};
            "fib_jumps")]
#[test_case(read_sierra_example_file("fib_recursive").as_str(),
            true,
            indoc! {"
                [ap + 0] = 1, ap++;
                jmp rel 7 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + 6, ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;

                // Statement #  8 - calculating n - 1, and testing if n - 1 == 0.
                [fp + -3] = [ap + 0] + 1, ap++;
                jmp rel 7 if [ap + -1] != 0;
                [ap + 0] = [fp + -5], ap++;
                // Statement # 12 - n == 1, so we return updated gb and 1.
                [ap + 0] = [fp + -4] + 4, ap++;
                [ap + 0] = [ap + -4], ap++;
                ret;

                // Statement # 17 - Get gas for the recursive calls.
                %{ memory[ap + 0] = 3600 <= memory[fp + -4] %}
                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [fp + -4] + 340282366920938463463374607431768207856, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 25;
                [fp + -4] = [ap + 0] + 3600, ap++;
                [ap + -1] = [[fp + -5] + 0];

                // Statement # 21 - Performing both recursive calculations and returning their sum.
                ap += 2;
                [ap + 0] = [fp + -5] + 1, ap++;
                [ap + 0] = [ap + -4], ap++;
                [ap + -7] = [fp + 4] + 1;
                [ap + 0] = [ap + -7], ap++;
                call rel -36;
                [fp + 5] = [ap + -1];
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [fp + 4], ap++;
                call rel -42;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [fp + 5] + [ap + -3], ap++;
                ret;

                // Statement # 41 - Ran out of gas - returning update gb and error value.
                [ap + 0] = [fp + -5] + 1, ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = -1, ap++;
                ret;
            "};
            "fib_recursive")]
#[test_case(indoc! {"
                type felt = felt;
                type DictFeltToFelt = DictFeltTo<felt>;
                type SquashedDictFeltToFelt = SquashedDictFeltTo<felt>;
                type RangeCheck = RangeCheck;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc felt_const<10> = felt_const<10>;
                libfunc felt_const<11> = felt_const<11>;
                libfunc felt_const<12> = felt_const<12>;
                libfunc felt_const<13> = felt_const<13>;
                libfunc felt_drop = drop<felt>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc dict_felt_to_new<felt> = dict_felt_to_new<felt>;
                libfunc dict_felt_to_write<felt> = dict_felt_to_write<felt>;
                libfunc dict_felt_to_read<felt> = dict_felt_to_read<felt>;
                libfunc dict_felt_to_squash<felt> = dict_felt_to_squash<felt>;
                libfunc store_temp_squashed_dict_felt_to_felt = store_temp<SquashedDictFeltToFelt>;
                libfunc store_temp_range_check = store_temp<RangeCheck>;

                felt_const<10>() -> ([1]);
                store_temp_felt([1]) -> ([1]);
                felt_const<11>() -> ([2]);
                store_temp_felt([2]) -> ([2]);
                felt_const<12>() -> ([3]);
                store_temp_felt([3]) -> ([3]);
                dict_felt_to_new<felt>([3]) -> ([4]);
                dict_felt_to_write<felt>([4], [1], [2]) -> ([5]);
                felt_const<10>() -> ([6]);
                store_temp_felt([6]) -> ([6]);
                felt_const<13>() -> ([7]);
                store_temp_felt([7]) -> ([7]);
                dict_felt_to_write<felt>([5], [6], [7]) -> ([8]);
                felt_const<10>() -> ([9]);
                store_temp_felt([9]) -> ([9]);
                dict_felt_to_read<felt>([8], [9]) -> ([10], [11]);
                felt_drop([11]) -> ();
                dict_felt_to_squash<felt>([0], [10]) -> ([12], [13]);
                store_temp_range_check([12]) -> ([12]);
                store_temp_squashed_dict_felt_to_felt([13]) -> ([13]);
                return ([12], [13]);
                test_program@0([0]: RangeCheck) -> (RangeCheck, SquashedDictFeltToFelt);
            "},
            false,
            indoc! {"
                [ap + 0] = 10, ap++;
                [ap + 0] = 11, ap++;
                [ap + 0] = 12, ap++;
                %{
                if '__dict_manager' not in globals():
                    from starkware.cairo.common.dict import DictManager
                    __dict_manager = DictManager()
                memory[ap + 0] = __dict_manager.new_default_dict(segments, memory[ap + -1])
                %}
                ap += 1;
                %{
                dict_tracker = __dict_manager.get_tracker(memory[ap + -1] + 0)
                dict_tracker.current_ptr += 3
                memory[ap + 0] = dict_tracker.data[memory[ap + -4]]
                dict_tracker.data[memory[ap + -4]] = memory[ap + -3]
                %}
                ap += 1;
                [ap + -5] = [[ap + -2] + 0];
                [ap + -1] = [[ap + -2] + 1];
                [ap + -4] = [[ap + -2] + 2];
                [ap + 0] = 10, ap++;
                [ap + 0] = 13, ap++;
                %{
                dict_tracker = __dict_manager.get_tracker(memory[ap + -4] + 3)
                dict_tracker.current_ptr += 3
                memory[ap + 0] = dict_tracker.data[memory[ap + -2]]
                dict_tracker.data[memory[ap + -2]] = memory[ap + -1]
                %}
                ap += 1;
                [ap + -3] = [[ap + -5] + 3];
                [ap + -1] = [[ap + -5] + 4];
                [ap + -2] = [[ap + -5] + 5];
                [ap + 0] = 10, ap++;
                %{
                dict_tracker = __dict_manager.get_tracker(memory[ap + -6] + 6)
                dict_tracker.current_ptr += 3
                memory[ap + 0] = dict_tracker.data[memory[ap + -1]]
                %}
                ap += 1;
                [ap + -2] = [[ap + -7] + 6];
                [ap + -1] = [[ap + -7] + 7];
                [ap + -1] = [[ap + -7] + 8];
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [ap + -8], ap++;
                [ap + 0] = [ap + -9] + 9, ap++;
                call rel 61;
                jmp rel 174;
                %{
                import itertools
                from starkware.cairo.common.math_utils import assert_integer
                assert_integer(memory[fp - 4])
                assert_integer(memory[fp - 3])
                a = memory[fp - 4] % PRIME
                b = memory[fp - 3] % PRIME
                assert a <= b, f'a = {a} is not less than or equal to b = {b}.'
                # Find an arc less than PRIME / 3, and another less than PRIME / 2.
                lengths_and_indices = [(a, 0), (b - a, 1), (PRIME - 1 - b, 2)]
                lengths_and_indices.sort()
                assert lengths_and_indices[0][0] <= PRIME
                excluded = lengths_and_indices[2][1]
                memory[memory[fp - 5] + 1], memory[memory[fp - 5] + 0] = (
                    divmod(lengths_and_indices[0][0], 3544607988759775765608368578435044694))
                memory[memory[fp - 5] + 3], memory[memory[fp - 5] + 2] = (
                    divmod(lengths_and_indices[1][0], 5316911983139663648412552867652567041))
                %}
                [ap + 0] = [[fp + -5] + 0], ap++;
                [ap + 0] = [[fp + -5] + 1], ap++;
                [ap + 0] = [ap + -1] * 3544607988759775765608368578435044694, ap++;
                [ap + 0] = [ap + -3] + [ap + -1], ap++;
                [ap + 0] = [[fp + -5] + 2], ap++;
                [ap + 0] = [[fp + -5] + 3], ap++;
                [ap + 0] = [ap + -1] * 5316911983139663648412552867652567041, ap++;
                [ap + 0] = [ap + -3] + [ap + -1], ap++;
                %{ memory[ap] = 1 if excluded != 0 else 0 %}
                jmp rel 14 if [ap + 0] != 0, ap++;
                [ap + 0] = -1, ap++;
                [ap + -1] = [ap + 0] + [fp + -4], ap++;
                [ap + -1] = [ap + -8] + [ap + -4];
                [fp + -4] = [ap + 0] + [fp + -3], ap++;
                [ap + 0] = [fp + -3] + 1, ap++;
                [ap + 0] = [ap + -2] * [ap + -1], ap++;
                [ap + -1] = [ap + -11] * [ap + -7];
                [ap + 0] = [fp + -5] + 4, ap++;
                ret;
                %{ memory[ap] = 1 if excluded != 1 else 0 %}
                [ap + 0] = -1, ap++;
                [ap + -1] = [ap + 0] + [fp + -3], ap++;
                [ap + 0] = [fp + -4] + [ap + -1], ap++;
                [ap + -1] = [ap + -10] + [ap + -6];
                [ap + 0] = [fp + -4] * [ap + -2], ap++;
                [ap + -1] = [ap + -11] * [ap + -7];
                [ap + 0] = [fp + -5] + 4, ap++;
                ret;
                %{ assert excluded == 2 %}
                [fp + -3] = [ap + -7] + [ap + -3];
                [fp + -3] = [ap + 0] + [fp + -4], ap++;
                [ap + 0] = [fp + -4] * [ap + -1], ap++;
                [ap + -1] = [ap + -9] * [ap + -5];
                ap += 2;
                [ap + 0] = [fp + -5] + 4, ap++;
                ret;
                %{
                   from starkware.cairo.common.math_utils import assert_integer
                   assert_integer(memory[fp - 4])
                   assert_integer(memory[fp - 3])
                   assert (memory[fp - 4] % PRIME) < (memory[fp - 3] % PRIME), f'a = {memory[fp - 4] % PRIME} is not less than b = {memory[fp - 3] % PRIME}.'
                %}
                [fp + -4] = [ap + 0] + [fp + -3], ap++;
                jmp rel 4 if [ap + -1] != 0;
                [fp + -4] = [fp + -4] + 1;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = [fp + -3], ap++;
                call rel -53;
                ret;
                %{
                if '__dict_manager' not in globals():
                    from starkware.cairo.common.dict import DictManager
                    __dict_manager = DictManager()
                memory[ap] = __dict_manager.new_dict(segments, initial_dict)
                del initial_dict
                %}
                ap += 1;
                ret;
                ap += 1;
                %{
                   # Prepare arguments for dict_new. In particular, the same dictionary values should be copied
                   # to the new (squashed) dictionary.
                   vm_enter_scope({
                       # Make __dict_manager accessible.
                       '__dict_manager': __dict_manager,
                       # Create a copy of the dict, in case it changes in the future.
                       'initial_dict': dict(__dict_manager.get_dict(memory[fp - 3])),
                   })
                %}
                call rel -5;
                [fp + 0] = [ap + -1];
                %{ vm_exit_scope() %}
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + 0], ap++;
                call rel 6;
                %{
                # Update the DictTracker's current_ptr to point to the end of the squashed dict.
                __dict_manager.get_tracker(memory[fp]).current_ptr = ap - 1
                %}
                [ap + 0] = [ap + -2], ap++;
                [ap + 0] = [fp + 0], ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;
                ap += 3;
                %{ vm_enter_scope() %}
                [fp + -4] = [fp + 0] + [fp + -5];
                jmp rel 5 if [fp + 0] != 0;
                %{ vm_exit_scope() %}
                [ap + 0] = [fp + -6], ap++;
                [ap + 0] = [fp + -3], ap++;
                ret;
                [ap + 0] = [fp + 0] * 1206167596222043737899107594365023368541035738443865566657697352045290673494, ap++;
                %{
                   dict_access_size = 3 # ids.DictAccess.SIZE
                   address = fp - 5
                   assert memory[fp] % dict_access_size == 0, 'Accesses array size must be divisible by DictAccess.SIZE'
                   n_accesses = memory[ap - 1]
                   if '__squash_dict_max_size' in globals():
                       assert n_accesses <= __squash_dict_max_size, f'squash_dict() can only be used with n_accesses<={__squash_dict_max_size}. ' f'Got: n_accesses={n_accesses}.'
                   # A map from key to the list of indices accessing it.
                   access_indices = {}
                   for i in range(n_accesses):
                       key = memory[memory[address] + dict_access_size * i]
                       access_indices.setdefault(key, []).append(i)
                   # Descending list of keys.
                   keys = sorted(access_indices.keys(), reverse=True)
                   # Are the keys used bigger than range_check bound.
                   memory[fp + 2] = 1 if keys[0] >= range_check_builtin.bound else 0
                   memory[fp + 1] = key = keys.pop()
                %}
                jmp rel 7 if [fp + 2] != 0;
                [fp + 1] = [[fp + -6] + 0];
                [ap + 0] = [fp + -6] + 1, ap++;
                jmp rel 3;
                [ap + 0] = [fp + -6], ap++;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + -1, ap++;
                [ap + 0] = [fp + 1], ap++;
                [ap + 0] = [ap + -5], ap++;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + 2], ap++;
                call rel 3;
                %{ vm_exit_scope() %}
                ret;
                ap += 2;
                %{
                current_access_indices = sorted(access_indices[key])[::-1]
                current_access_index = current_access_indices.pop()
                memory[memory[fp - 9]] = current_access_index
                %}
                [ap + 0] = [[fp + -9] + 0], ap++;
                [ap + 0] = [ap + -1] * 3, ap++;
                [ap + 1] = [fp + -8] + [ap + -1], ap++;
                [ap + -1] = [[ap + 0] + 2], ap++;
                [ap + 0] = [fp + -9] + 1, ap++;
                [fp + -6] = [[ap + -2] + 0];
                [fp + -6] = [[fp + -4] + 0];
                [fp + 0] = [[ap + -2] + 1];
                [fp + 0] = [[fp + -4] + 1];
                %{ memory[fp + 1] = 0 if current_access_indices else 1 %}
                jmp rel 15 if [fp + 1] != 0;
                %{
                new_access_index = current_access_indices.pop()
                memory[ap] = new_access_index - current_access_index - 1
                current_access_index = new_access_index
                %}
                [ap + 0] = [[ap + -1] + 0], ap++;
                [ap + 0] = [ap + -1] + 1, ap++;
                [ap + 0] = [ap + -1] * 3, ap++;
                [ap + 2] = [ap + -5] + [ap + -1], ap++;
                [ap + -7] = [[ap + 1] + 1];
                [ap + 0] = [[ap + 1] + 2], ap++;
                [fp + -6] = [[ap + 0] + 0];
                [ap + 1] = [ap + -6] + 1, ap++;
                %{ memory[ap - 3] = 1 if current_access_indices else 0 %}
                jmp rel -11 if [ap + -3] != 0, ap++;
                %{ assert len(current_access_indices) == 0 %}
                [fp + -7] = [ap + 0] + [ap + -2];
                [ap + 0] = [[ap + -1] + 0], ap++;
                [ap + -2] = [ap + 0] + [fp + -9], ap++;
                %{ assert memory[ap - 1] == len(access_indices[key]) %}
                [ap + -5] = [[fp + -4] + 2];
                [fp + -5] = [ap + 0] + [ap + -1], ap++;
                jmp rel 7 if [ap + -1] != 0;
                %{ assert len(keys) == 0 %}
                [ap + 0] = [ap + -4] + 1, ap++;
                [ap + 0] = [fp + -4] + 3, ap++;
                ret;
                ap += 1;
                %{
                assert len(keys) > 0, 'No keys left but remaining_accesses > 0.'
                memory[ap - 1] = key = keys.pop()
                %}
                jmp rel 14 if [fp + -3] != 0;
                [ap + 0] = [fp + -6] + 1, ap++;
                [ap + -2] = [ap + 0] + [ap + -1], ap++;
                [ap + -1] = [[ap + -7] + 1];
                [ap + 0] = [ap + -7] + 2, ap++;
                [ap + 0] = [fp + -8], ap++;
                [ap + 0] = [fp + -7], ap++;
                [ap + 0] = [ap + -6], ap++;
                [ap + 0] = [ap + -8], ap++;
                jmp rel 12;
                [ap + 0] = [ap + -5] + 1, ap++;
                [ap + 0] = [fp + -6], ap++;
                [ap + 0] = [ap + -3], ap++;
                call rel -117;
                [ap + 0] = [fp + -8], ap++;
                [ap + 0] = [fp + -7], ap++;
                [ap + 0] = [ap + -29], ap++;
                [ap + 0] = [ap + -31], ap++;
                [ap + 0] = [fp + -4] + 3, ap++;
                [ap + 0] = [fp + -3], ap++;
                call rel -69;
                ret;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -3], ap++;
                ret;
            "};
            "dict test")]

fn sierra_to_casm(sierra_code: &str, check_gas_usage: bool, expected_casm: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, &build_metadata(&program, check_gas_usage), check_gas_usage)
            .expect("Compilation failed.")
            .to_string(),
        strip_comments_and_linebreaks(expected_casm)
    );
}

// TODO(ilya, 10/10/2022): Improve error messages.
#[test_case(indoc! {"
                return([2]);

                test_program@0() -> (felt);
            "},
            "#0: [2] is undefined.";
            "Missing reference")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                felt_dup([2]) -> ([1], [2]);
                return();

                test_program@0([1]: felt) -> ();
            "},
            "#1->#2: [1] was overridden.";
            "Reference override")]
#[test_case(indoc! {"
                type felt = felt;

                return([2]);

                test_program@0([2]: felt) -> (felt);
            "},
            "#0: Return arguments are not on the stack.";
            "Invalid return reference")]
#[test_case(indoc! {"
                type felt = felt;

                store_temp_felt([1]) -> ([1]);

                test_program@0([1]: felt) -> ();
            "},
            "Error from program registry";
            "undeclared libfunc")]
#[test_case(indoc! {"
                type felt = felt;

                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_felt = store_temp<felt>;
            "},
            "Error from program registry";
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;

                felt_add([1], [2]) -> ([4]);
                felt_add([3], [4]) -> ([5]);

                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "},
            "#1: One of the arguments does not satisfy the requirements of the libfunc.";
            "Invalid reference expression for felt_add")]
#[test_case(indoc! {"
                type felt = felt;
                type u128 = u128;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3]);
                return([3]);

                test_program@0([1]: u128, [2]: u128) -> (felt);
            "},
            "One of the arguments does not match the expected type of the libfunc or return \
 statement.";
            "Types mismatch")]
#[test_case(indoc! {"
                test_program@25() -> ();
            "}, "InvalidStatementIdx";
            "Invalid entry point")]
#[test_case(indoc! {"
                return();

                foo@0([1]: felt, [1]: felt) -> ();
            "}, "Invalid function declaration.";
            "Bad Declaration")]
#[test_case(indoc! {"
            return();
            "}, "MissingAnnotationsForStatement";
            "Missing references for statement")]
#[test_case(indoc! {"
                type NonZeroFelt = NonZero<felt>;
                type felt = felt;
            "}, "Error from program registry";
            "type ordering bad for building size map")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2], [3]) -> ([4]);
                return();
                test_program@0([1]: felt, [2]: felt, [3]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "input count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) -> ([3], [4]);
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "output type mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) 1([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "branch count mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_add = felt_add;
                felt_add([1], [2]) { 0([3]) };
                test_program@0([1]: felt, [2]: felt) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "fallthrough mismatch")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
            "}, "[2] is dangling at #1.";
            "Dangling references")]
#[test_case(indoc! {"
                type felt = felt;

                return();

                foo@0([1]: felt) -> ();
                bar@0([2]: felt) -> ();
            "}, "#0: Inconsistent references annotations.";
            "Failed building type information")]
#[test_case(indoc! {"
                type felt = felt;
                libfunc felt_dup = dup<felt>;

                felt_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt) -> ();
                foo@1([1]: felt) -> (felt);
            "}, "#1: Inconsistent references annotations.";
            "Inconsistent return annotations.")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc felt_dup = dup<felt>;
                libfunc felt_drop = drop<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc store_temp_nz_felt = store_temp<NonZeroFelt>;

                felt_jump_nz([1]) { fallthrough() 3([1]) };
                store_temp_felt([2]) -> ([2]);
                return ([2]);
                felt_drop([2]) -> ();
                store_temp_nz_felt([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: felt, [2]: felt) -> (felt);
            "}, "One of the arguments does not match the expected type \
of the libfunc or return statement.";
            "Invalid return type")]
#[test_case(indoc! {"
                type felt = felt;

                libfunc felt_dup = dup<felt>;
                libfunc felt_drop = drop<felt>;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt([1]) -> ([1]);
                felt_dup([1]) -> ([1], [2]);
                call_foo([2]) -> ();
                store_temp_felt([1]) -> ([1]);
                felt_drop([1]) -> ();
                return();

                foo@0([1]: felt) -> ();
            "}, "#2->#3: Got 'Unknown ap change' error while moving [1].";
            "Ap change error")]
#[test_case(indoc! {"
                type felt = felt;
                type NonZeroFelt = NonZero<felt>;

                libfunc revoke_ap_tracking = revoke_ap_tracking;

                libfunc felt_drop = drop<felt>;
                libfunc felt_jump_nz = felt_jump_nz;
                libfunc felt_unwrap_nz = unwrap_nz<felt>;
                libfunc jump = jump;

                felt_jump_nz([1]) { fallthrough() 3([1]) };
                revoke_ap_tracking() -> ();
                jump() { 5() };
                felt_unwrap_nz([1]) -> ([1]);
                felt_drop([1]) -> ();
                return ();

                foo@0([1]: felt) -> ();
            "}, "#5: Inconsistent ap tracking.";
            "Inconsistent ap tracking.")]
#[test_case(indoc! {"
                libfunc finalize_locals = finalize_locals;

                finalize_locals () -> ();
                finalize_locals () -> ();
                return ();

                test_program@0() -> ();
            "}, "#1: finalize_locals is not allowed at this point.";
            "Invalid finalize_locals 1")]
#[test_case(indoc! {"
                type felt = felt;

                libfunc finalize_locals = finalize_locals;
                libfunc store_temp_felt = store_temp<felt>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt([1]) -> ([1]);
                call_foo([1]) -> ();
                finalize_locals() -> ();
                return ();

                foo@0([1]: felt) -> ();
            "}, "#2: finalize_locals is not allowed at this point.";
            "Invalid finalize_locals 2")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_temp_felt = store_temp<felt>;

                alloc_local_felt() -> ([2]);
                store_temp_felt([1]) -> ([1]);
                alloc_local_felt() -> ([3]);
                return ();

                foo@0([1]: felt) -> ();
            "}, "#2: alloc_local is not allowed at this point.";
            "Invalid alloc_local ")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_local_felt = store_local<felt>;
                libfunc felt_drop = drop<felt>;

                alloc_local_felt() -> ([2]);
                store_local_felt([2], [1]) -> ([2]);
                felt_drop([2]) -> ();
                return ();

                foo@0([1]: felt) -> ();
            "}, "#3: locals were allocated but finalize_locals was not called.";
            "missing finalize_locals ")]
#[test_case(indoc! {"
                type felt = felt;
                type UninitializedFelt = Uninitialized<felt>;

                libfunc alloc_local_felt = alloc_local<felt>;
                libfunc store_temp_felt = store_temp<UninitializedFelt>;

                alloc_local_felt() -> ([1]);
                store_temp_felt([1]) -> ([1]);
                return ();

                foo@0() -> ();
            "}, "#1: The functionality is supported only for sized types.";
            "store_temp<Uninitialized<felt>()")]
fn compiler_errors(sierra_code: &str, expected_result: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, &build_metadata(&program, false), false)
            .expect_err("Compilation is expected to fail.")
            .to_string(),
        expected_result
    );
}
