use cairo_lang_sierra::ProgramParser;
use indoc::indoc;
use pretty_assertions;
use test_case::test_case;

use crate::compiler::compile;
use crate::test_utils::{build_metadata, read_sierra_example_file, strip_comments_and_linebreaks};

#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;
                type BoxFelt252 = Box<felt252>;

                libfunc branch_align = branch_align;
                libfunc finalize_locals = finalize_locals;
                libfunc felt252_add = felt252_add;
                libfunc felt252_mul_2 = felt252_mul_const<2>;
                libfunc felt252_sub = felt252_sub;
                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc felt252_into_box = into_box<felt252>;
                libfunc felt252_unbox = unbox<felt252>;
                libfunc jump = jump;
                libfunc felt252_unwrap_non_zero = unwrap_non_zero<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_box_felt252 = store_temp<BoxFelt252>;
                libfunc rename_felt252 = rename<felt252>;
                libfunc call_foo = function_call<user@foo>;

                libfunc call_box_and_back = function_call<user@box_and_back>;

                rename_felt252([1]) -> ([1]);                      // #0
                felt252_dup([2]) -> ([2], [5]);                    // #1
                felt252_add([1], [2]) -> ([3]);                    // #2
                store_temp_felt252([3]) -> ([4]);                  // #3
                store_temp_felt252([5]) -> ([5]);                  // #4
                store_temp_felt252([4]) -> ([4]);                  // #5
                call_foo([5], [4]) -> ([7], [8]);                  // #6
                felt252_dup([8]) -> ([4], [8]);                    // #7
                store_temp_felt252([4]) -> ([4]);                  // #8
                return([7], [8], [4]);                             // #9

                finalize_locals() -> ();                           // #10
                felt252_is_zero([1]) { fallthrough() 17([1]) };    // #11
                branch_align() -> ();                              // #12
                felt252_dup([2]) -> ([1], [2]);                    // #13
                store_temp_felt252([1]) -> ([1]);                  // #14
                store_temp_felt252([2]) -> ([2]);                  // #15
                return ([1], [2]);                                 // #16

                branch_align() -> ();                              // #17
                jump() { 19() };                                   // #18
                felt252_unwrap_non_zero([1]) -> ([1]);                   // #19
                felt252_dup([2]) -> ([2], [3]);                    // #20
                felt252_sub([1], [3]) -> ([1]);                    // #21
                store_temp_felt252([1]) -> ([1]);                  // #22
                felt252_mul_2([1]) -> ([1]);                       // #23
                store_temp_felt252([1]) -> ([1]);                  // #24
                store_temp_felt252([2]) -> ([2]);                  // #25
                call_foo([1], [2]) -> ([1], [2]);                  // #26
                return ([1], [2]);                                 // #27

                felt252_into_box([1]) -> ([2]);                    // #28
                store_temp_box_felt252([2]) -> ([2]);              // #29
                felt252_unbox([2]) -> ([3]);                       // #30
                store_temp_felt252([3]) -> ([3]);                  // #31
                return ([3]);                                      // #32

                store_temp_felt252([1]) -> ([1]);                  // #33
                call_box_and_back([1]) -> ([1]);                   // #34
                return ([1]);                                      // #35

                test_program@0([1]: felt252, [2]: felt252) -> (felt252, felt252, felt252);
                foo@10([1]: felt252, [2]: felt252) -> (felt252, felt252);
                box_and_back@28([1]: felt252) -> (felt252);
                box_and_back_wrapper@33([1]: felt252) -> (felt252);
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
                %{
                if '__boxed_segment' not in globals():
                    __boxed_segment = segments.add()
                memory[ap + 0] = __boxed_segment
                __boxed_segment += 1
                %}
                [fp + -3] = [[ap + 0] + 0], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [[ap + -1] + 0], ap++;
                ret;

                // box_and_back_wrapper:
                [ap + 0] = [fp + -3], ap++;
                call rel -5;
                ret;
            "};
            "good_flow")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;
                type ArrayFelt252 = Array<felt252>;
                type UninitializedArrayFelt252 = Uninitialized<ArrayFelt252>;

                libfunc finalize_locals = finalize_locals;
                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_local_felt252 = store_local<felt252>;
                libfunc alloc_local_array_felt252 = alloc_local<ArrayFelt252>;
                libfunc store_local_array_felt252 = store_local<ArrayFelt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_array_felt252 = store_temp<ArrayFelt252>;

                store_temp_felt252([1]) -> ([1]);
                alloc_local_felt252() -> ([4]);
                alloc_local_felt252() -> ([5]);
                alloc_local_array_felt252() -> ([6]);
                store_local_felt252([4], [1]) -> ([4]);
                finalize_locals() -> ();
                store_local_felt252([5], [2]) -> ([5]);
                store_local_array_felt252([6], [3]) -> ([6]);
                store_temp_felt252([4]) -> ([4]);
                store_temp_felt252([5]) -> ([5]);
                store_temp_array_felt252([6]) -> ([6]);
                return ([4], [5], [6]);

                test_program@0([1]: felt252, [2]: felt252, [3]: ArrayFelt252) -> (felt252, felt252, ArrayFelt252);
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
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc jump = jump;
                libfunc store_temp_nz_felt252 = store_temp<NonZeroFelt252>;
                libfunc nz_felt252_drop = drop<NonZeroFelt252>;
                libfunc felt252_is_zero = felt252_is_zero;

                felt252_is_zero([1]) { fallthrough() 3([1]) };
                branch_align() -> ();
                return ();
                branch_align() -> ();
                store_temp_nz_felt252([1]) -> ([1]);
                nz_felt252_drop([1]) -> ();
                return ();

                test_program@0([1]: felt252) -> ();
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
                libfunc branch_align = branch_align;
                libfunc jump = jump;
                libfunc u128_lt = u128_lt;
                libfunc store_u128 = store_temp<u128>;
                libfunc store_rc = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                u128_lt([1], [2], [3]) {fallthrough([1]) 4([1]) };
                branch_align() -> ();
                jump() { 5() };
                branch_align() -> ();

                store_rc([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: u128, [3]: u128) -> (RangeCheck);
            "}, true, indoc!{"
                [fp + -4] = [ap + 1] + [fp + -3], ap++;
                %{ memory[ap + -1] = memory[ap + 0] < 340282366920938463463374607431768211456 %}
                jmp rel 7 if [ap + -1] != 0, ap++;
                // a < b.
                [ap + 0] = [ap + -1] + 340282366920938463463374607431768211456, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 5;
                // a < b.
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 2;
                // Store range_check and return.
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "}; "u128_lt")]
#[test_case(indoc! {"
                type u128 = u128;
                type RangeCheck = RangeCheck;

                libfunc branch_align = branch_align;
                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc u128_overflowing_add = u128_overflowing_add;
                libfunc drop<u128> = drop<u128>;
                libfunc store_temp<RangeCheck> = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                u128_overflowing_add([1], [2], [3]) {fallthrough([1], [2]) 6([1], [2]) };
                branch_align() -> ();
                drop<u128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);
                branch_align() -> ();
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
                jmp rel 7 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = 1, ap++;
                ret;

                // Statement # 9
                // Setting up the latest memory to be of the form [n, rc, gb, a=1, b=0].
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = 1, ap++;
                [ap + 0] = 0, ap++;

                // Statement #18, check n.
                jmp rel 6 if [ap + -5] != 0;
                // Statement # 19 - n == 0, so we can return the latest a.
                [ap + 0] = [ap + -4], ap++;
                [ap + 0] = [ap + -4], ap++;
                [ap + 0] = [ap + -4], ap++;
                ret;

                // Statement # 28 - withdrawing gas for the main loop.
                %{ memory[ap + 0] = 1070 <= memory[ap + -3] %}

                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap + -4] + 340282366920938463463374607431768210386, ap++;
                [ap + -1] = [[ap + -6] + 0];
                jmp rel 14;

                // Statement # 30
                // The main loop - given [n, rc, gb, a, b, _, _] - adds [n-1, updated_rc, updated_gb, a+b, a]
                // Memory cells form is now [n'=n-1, rc'=updated_rc, gb'=updated_gb, a'=a+b, b'=a]
                [ap + -4] = [ap + 0] + 1070, ap++;
                [ap + -1] = [[ap + -6] + 0];
                [ap + -7] = [ap + 0] + 1, ap++;
                [ap + 0] = [ap + -7] + 1, ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -7] + [ap + -6], ap++;
                [ap + 0] = [ap + -8], ap++;
                jmp rel -23;

                // Statement # 40  - Ran out of gas - returning updated gb and -1.
                [ap + 0] = [ap + -6] + 1, ap++;
                [ap + 0] = [ap + -6], ap++;
                [ap + 0] = -1, ap++;
                ret;
            "};
            "fib_jumps")]
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

                test_program@0() -> (felt252);
            "},
            "#0: [2] is undefined.";
            "Missing reference")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_dup = dup<felt252>;

                felt252_dup([1]) -> ([1], [2]);
                felt252_dup([2]) -> ([1], [2]);
                return();

                test_program@0([1]: felt252) -> ();
            "},
            "#1->#2: [1] was overridden.";
            "Reference override")]
#[test_case(indoc! {"
                type felt252 = felt252;

                return([2]);

                test_program@0([2]: felt252) -> (felt252);
            "},
            "#0: Return arguments are not on the stack.";
            "Invalid return reference")]
#[test_case(indoc! {"
                type felt252 = felt252;

                store_temp_felt252([1]) -> ([1]);

                test_program@0([1]: felt252) -> ();
            "},
            "Error from program registry";
            "undeclared libfunc")]
#[test_case(indoc! {"
                type felt252 = felt252;

                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
            "},
            "Error from program registry";
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;

                felt252_add([1], [2]) -> ([4]);
                felt252_add([3], [4]) -> ([5]);

                test_program@0([1]: felt252, [2]: felt252, [3]: felt252) -> ();
            "},
            "#1: One of the arguments does not satisfy the requirements of the libfunc.";
            "Invalid reference expression for felt252_add")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type u128 = u128;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) -> ([3]);
                return([3]);

                test_program@0([1]: u128, [2]: u128) -> (felt252);
            "},
            "#0: One of the arguments does not match the expected type of the libfunc or return \
 statement.";
            "Types mismatch")]
#[test_case(indoc! {"
                test_program@25() -> ();
            "}, "InvalidStatementIdx";
            "Invalid entry point")]
#[test_case(indoc! {"
                return();

                foo@0([1]: felt252, [1]: felt252) -> ();
            "}, "#0: Invalid function declaration.";
            "Bad Declaration")]
#[test_case(indoc! {"
            return();
            "}, "MissingAnnotationsForStatement";
            "Missing references for statement")]
#[test_case(indoc! {"
                type NonZeroFelt252 = NonZero<felt252>;
                type felt252 = felt252;
            "}, "Error from program registry";
            "type ordering bad for building size map")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2], [3]) -> ([4]);
                return();
                test_program@0([1]: felt252, [2]: felt252, [3]: felt252) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "input count mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) -> ([3], [4]);
                test_program@0([1]: felt252, [2]: felt252) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "output type mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) { 0([3]) 1([3]) };
                test_program@0([1]: felt252, [2]: felt252) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "branch count mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) { 0([3]) };
                test_program@0([1]: felt252, [2]: felt252) -> ();
            "}, "#0: Invocation mismatched to libfunc";
            "fallthrough mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_dup = dup<felt252>;

                felt252_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt252) -> ();
            "}, "[2] is dangling at #1.";
            "Dangling references")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc felt252_dup = dup<felt252>;
                libfunc jump = jump;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc drop_nz_felt252 = drop<NonZeroFelt252>;

                felt252_dup([1]) -> ([1], [2]);
                felt252_dup([1]) -> ([1], [3]);
                felt252_is_zero([1]) { fallthrough() 7([1]) };
                branch_align() -> ();
                store_temp_felt252([2]) -> ([2]);
                store_temp_felt252([3]) -> ([3]);
                jump() { 11() };
                branch_align() -> ();
                drop_nz_felt252([1]) -> ();
                store_temp_felt252([3]) -> ([3]);
                store_temp_felt252([2]) -> ([2]);
                return ([2], [3]);

                test_program@0([1]: felt252) -> (felt252, felt252);
            "}, "#11: Inconsistent references annotations.";
            "Inconsistent references - different locations on stack")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type unit = Struct<ut@unit>;
                type unit_pair = Struct<ut@unit_pair, unit, unit>;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc felt252_dup = dup<felt252>;
                libfunc jump = jump;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_unit_pair = store_temp<unit_pair>;
                libfunc drop_nz_felt252 = drop<NonZeroFelt252>;
                libfunc drop_unit = drop<unit>;
                libfunc rename_unit = rename<unit>;
                libfunc unit_pair_deconstruct = struct_deconstruct<unit_pair>;

                store_temp_unit_pair([2]) -> ([2]);
                unit_pair_deconstruct([2]) -> ([3], [4]);
                felt252_is_zero([1]) { fallthrough() 7([1]) };
                branch_align() -> ();
                drop_unit([4]) -> ();
                rename_unit([3]) -> ([4]);
                jump() { 10() };
                branch_align() -> (); // statement #7.
                drop_nz_felt252([1]) -> ();
                drop_unit([3]) -> ();
                return ([4]); // The failed merge statement #10.

                test_program@0([1]: felt252, [2]: unit_pair) -> (unit);
            "}, "#10: Inconsistent references annotations.";
            "Inconsistent references - merge on old variable not created at the same point")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_drop = drop<felt252>;
                libfunc jump = jump;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc drop_nz_felt252 = drop<NonZeroFelt252>;

                felt252_dup([1]) -> ([1], [2]);
                felt252_dup([1]) -> ([1], [3]);
                felt252_is_zero([1]) { fallthrough() 8([1]) };
                branch_align() -> ();
                store_temp_felt252([2]) -> ([2]);
                // Store and drop to break the stack so it can't be tracked.
                store_temp_felt252([3]) -> ([3]);
                felt252_drop([3]) -> ();
                jump() { 13() };
                branch_align() -> ();
                drop_nz_felt252([1]) -> ();
                store_temp_felt252([2]) -> ([2]);
                // Store and drop to break the stack so it can't be tracked.
                store_temp_felt252([3]) -> ([3]);
                felt252_drop([3]) -> ();
                return ([2]); // The failed merge statement #13.

                test_program@0([1]: felt252) -> (felt252);
            "}, "#13: Inconsistent references annotations.";
            "Inconsistent references - unaligned area")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc disable_ap_tracking = disable_ap_tracking;
                libfunc enable_ap_tracking = enable_ap_tracking;
                libfunc jump = jump;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc drop_nz_felt252 = drop<NonZeroFelt252>;

                disable_ap_tracking() -> ();
                felt252_is_zero([1]) { fallthrough() 5([1]) };
                branch_align() -> ();
                enable_ap_tracking() -> ();
                jump() { 8() };
                branch_align() -> ();
                drop_nz_felt252([1]) -> ();
                enable_ap_tracking() -> ();
                return (); // The failed merge statement #8.

                test_program@0([1]: felt252) -> ();
            "}, "#8: Inconsistent ap tracking base.";
            "Inconsistent ap tracking base.")]
#[test_case(indoc! {"
                libfunc enable_ap_tracking = enable_ap_tracking;

                enable_ap_tracking() -> ();
                return ();

                test_program@0() -> ();
            "}, "#0: Attempting to enable ap tracking when already enabled.";
            "Enabling ap tracking when already enabled.")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_drop = drop<felt252>;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_nz_felt252 = store_temp<NonZeroFelt252>;

                felt252_is_zero([1]) { fallthrough() 4([1]) };
                branch_align() -> ();
                store_temp_felt252([2]) -> ([2]);
                return ([2]);
                branch_align() -> ();
                felt252_drop([2]) -> ();
                store_temp_nz_felt252([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: felt252, [2]: felt252) -> (felt252);
            "}, "#7: One of the arguments does not match the expected type \
of the libfunc or return statement.";
            "Invalid return type")]
#[test_case(indoc! {"
                type felt252 = felt252;

                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_drop = drop<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt252([1]) -> ([1]);
                felt252_dup([1]) -> ([1], [2]);
                call_foo([2]) -> ();
                store_temp_felt252([1]) -> ([1]);
                felt252_drop([1]) -> ();
                return();

                foo@0([1]: felt252) -> ();
            "}, "#2->#3: Got 'Unknown ap change' error while moving [1].";
            "Ap change error")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc branch_align = branch_align;
                libfunc felt252_drop = drop<felt252>;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc felt252_unwrap_non_zero = unwrap_non_zero<felt252>;
                libfunc jump = jump;

                felt252_is_zero([1]) { fallthrough() 4([1]) };
                branch_align() -> ();
                revoke_ap_tracking() -> ();
                jump() { 7() };
                branch_align() -> ();
                felt252_unwrap_non_zero([1]) -> ([1]);
                felt252_drop([1]) -> ();
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#7: Inconsistent ap tracking base.";
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
                type felt252 = felt252;

                libfunc finalize_locals = finalize_locals;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt252([1]) -> ([1]);
                call_foo([1]) -> ();
                finalize_locals() -> ();
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#2: finalize_locals is not allowed at this point.";
            "Invalid finalize_locals 2")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;

                alloc_local_felt252() -> ([2]);
                store_temp_felt252([1]) -> ([1]);
                alloc_local_felt252() -> ([3]);
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#2: alloc_local is not allowed at this point.";
            "Invalid alloc_local ")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_local_felt252 = store_local<felt252>;
                libfunc felt252_drop = drop<felt252>;

                alloc_local_felt252() -> ([2]);
                store_local_felt252([2], [1]) -> ([2]);
                felt252_drop([2]) -> ();
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#3: locals were allocated but finalize_locals was not called.";
            "missing finalize_locals ")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_temp_felt252 = store_temp<UninitializedFelt252>;

                alloc_local_felt252() -> ([1]);
                store_temp_felt252([1]) -> ([1]);
                return ();

                foo@0() -> ();
            "}, "#1: The functionality is supported only for sized types.";
            "store_temp<Uninitialized<felt252>()")]
#[test_case(indoc! {"
                return ();

                foo@0() -> ();
                bar@0() -> ();
            "}, "#0: Belongs to two different functions.";
            "Statement in two functions")]
fn compiler_errors(sierra_code: &str, expected_result: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, &build_metadata(&program, false), false)
            .expect_err("Compilation is expected to fail.")
            .to_string(),
        expected_result
    );
}
